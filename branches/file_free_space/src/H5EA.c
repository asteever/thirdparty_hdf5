/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:		H5EA.c
 *			Jun 17 2008
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Implements an "extensible array" for storing elements
 *                      in an array whose high bounds can extend and shrink.
 *
 *                      Please see the documentation in:
 *                      doc/html/TechNotes/ExtensibleArray.html for a full
 *                      description of how they work, etc.
 *
 *-------------------------------------------------------------------------
 */

/**********************/
/* Module Declaration */
/**********************/

#define H5EA_MODULE


/***********************/
/* Other Packages Used */
/***********************/


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5EApkg.h"		/* Extensible Arrays			*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5Vprivate.h"         /* Vector functions			*/


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/


/*********************/
/* Package Variables */
/*********************/

/* HDF5 API Entered variable */
/* (move to H5.c when new FUNC_ENTER macros in actual use -QAK) */
hbool_t H5_api_entered_g = FALSE;


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage the H5EA_t struct */
H5FL_DEFINE_STATIC(H5EA_t);



/*-------------------------------------------------------------------------
 * Function:	H5EA_create
 *
 * Purpose:	Creates a new empty extensible array in the file.
 *
 * Return:	Pointer to earray wrapper on success
 *              NULL on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Jun 17 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PRIV, ERR,
H5EA_t *, NULL, NULL,
H5EA_create(H5F_t *f, hid_t dxpl_id, const H5EA_create_t *cparam))

    /* Local variables */
    H5EA_t *ea = NULL;          /* Pointer to new extensible array */
    H5EA_hdr_t *hdr = NULL;     /* The extensible array header information */
    haddr_t ea_addr;            /* Array header address */

#ifdef QAK
HDfprintf(stderr, "%s: Called\n", FUNC);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(cparam);

    /* Create extensible array header */
    if(HADDR_UNDEF == (ea_addr = H5EA__hdr_create(f, dxpl_id, cparam)))
	H5E_THROW(H5E_CANTINIT, "can't create extensible array header")

    /* Allocate extensible array wrapper */
    if(NULL == (ea = H5FL_MALLOC(H5EA_t)))
	H5E_THROW(H5E_CANTALLOC, "memory allocation failed for extensible array info")

    /* Lock the array header into memory */
    if(NULL == (hdr = (H5EA_hdr_t *)H5AC_protect(f, dxpl_id, H5AC_EARRAY_HDR, ea_addr, cparam->cls, NULL, H5AC_WRITE)))
	H5E_THROW(H5E_CANTPROTECT, "unable to load extensible array header")

    /* Point extensible array wrapper at header and bump it's ref count */
    ea->hdr = hdr;
    if(H5EA__hdr_incr(ea->hdr) < 0)
	H5E_THROW(H5E_CANTINC, "can't increment reference count on shared array header")

    /* Increment # of files using this array header */
    if(H5EA__hdr_fuse_incr(ea->hdr) < 0)
	H5E_THROW(H5E_CANTINC, "can't increment file reference count on shared array header")

    /* Set file pointer for this array open context */
    ea->f = f;

    /* Set the return value */
    ret_value = ea;

CATCH

    if(hdr && H5AC_unprotect(f, dxpl_id, H5AC_EARRAY_HDR, ea_addr, hdr, H5AC__NO_FLAGS_SET) < 0)
	H5E_THROW(H5E_CANTUNPROTECT, "unable to release extensible array header")
    if(!ret_value)
        if(ea && H5EA_close(ea, dxpl_id) < 0)
            H5E_THROW(H5E_CLOSEERROR, "unable to close extensible array")

END_FUNC(PRIV)  /* end H5EA_create() */


/*-------------------------------------------------------------------------
 * Function:	H5EA_open
 *
 * Purpose:	Opens an existing extensible array in the file.
 *
 * Return:	Pointer to array wrapper on success
 *              NULL on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug 28 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PRIV, ERR,
H5EA_t *, NULL, NULL,
H5EA_open(H5F_t *f, hid_t dxpl_id, haddr_t ea_addr, const H5EA_class_t *cls))

    /* Local variables */
    H5EA_t *ea = NULL;          /* Pointer to new extensible array wrapper */
    H5EA_hdr_t *hdr = NULL;     /* The extensible array header information */

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(H5F_addr_defined(ea_addr));
    HDassert(cls);

    /* Load the array header into memory */
#ifdef QAK
HDfprintf(stderr, "%s: ea_addr = %a\n", FUNC, ea_addr);
#endif /* QAK */
    if(NULL == (hdr = (H5EA_hdr_t *)H5AC_protect(f, dxpl_id, H5AC_EARRAY_HDR, ea_addr, cls, NULL, H5AC_READ)))
        H5E_THROW(H5E_CANTPROTECT, "unable to load extensible array header, address = %llu", (unsigned long long)ea_addr)

    /* Check for pending array deletion */
    if(hdr->pending_delete)
        H5E_THROW(H5E_CANTOPENOBJ, "can't open extensible array pending deletion")

    /* Create fractal heap info */
    if(NULL == (ea = H5FL_MALLOC(H5EA_t)))
        H5E_THROW(H5E_CANTALLOC, "memory allocation failed for extensible array info")

    /* Point extensible array wrapper at header */
    ea->hdr = hdr;
    if(H5EA__hdr_incr(ea->hdr) < 0)
        H5E_THROW(H5E_CANTINC, "can't increment reference count on shared array header")

    /* Increment # of files using this array header */
    if(H5EA__hdr_fuse_incr(ea->hdr) < 0)
        H5E_THROW(H5E_CANTINC, "can't increment file reference count on shared array header")

    /* Set file pointer for this array open context */
    ea->f = f;

    /* Set the return value */
    ret_value = ea;

CATCH

    if(hdr && H5AC_unprotect(f, dxpl_id, H5AC_EARRAY_HDR, ea_addr, hdr, H5AC__NO_FLAGS_SET) < 0)
        H5E_THROW(H5E_CANTUNPROTECT, "unable to release extensible array header")
    if(!ret_value)
        if(ea && H5EA_close(ea, dxpl_id) < 0)
            H5E_THROW(H5E_CLOSEERROR, "unable to close extensible array")

END_FUNC(PRIV)  /* end H5EA_open() */


/*-------------------------------------------------------------------------
 * Function:	H5EA_get_nelmts
 *
 * Purpose:	Query the current number of elements in array
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug 21 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PRIV, NOERR,
herr_t, SUCCEED, -,
H5EA_get_nelmts(const H5EA_t *ea, hsize_t *nelmts))

    /* Local variables */

#ifdef QAK
HDfprintf(stderr, "%s: Called\n", FUNC);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(ea);
    HDassert(nelmts);

    /* Retrieve the max. index set */
    *nelmts = ea->hdr->stats.max_idx_set;

END_FUNC(PRIV)  /* end H5EA_get_nelmts() */


/*-------------------------------------------------------------------------
 * Function:	H5EA_get_addr
 *
 * Purpose:	Query the address of the array
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug 21 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PRIV, NOERR,
herr_t, SUCCEED, -,
H5EA_get_addr(const H5EA_t *ea, haddr_t *addr))

    /* Local variables */

#ifdef QAK
HDfprintf(stderr, "%s: Called\n", FUNC);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(ea);
    HDassert(ea->hdr);
    HDassert(addr);

    /* Retrieve the address of the extensible array's header */
    *addr = ea->hdr->addr;

END_FUNC(PRIV)  /* end H5EA_get_addr() */


/*-------------------------------------------------------------------------
 * Function:	H5EA_set
 *
 * Purpose:	Set an element of an extensible array
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep  9 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PRIV, ERR,
herr_t, SUCCEED, FAIL,
H5EA_set(const H5EA_t *ea, hid_t dxpl_id, hsize_t idx, const void *elmt))

    /* Local variables */
    H5EA_hdr_t *hdr = ea->hdr;          /* Header for EA */
    H5EA_iblock_t *iblock = NULL;       /* Pointer to index block for EA */
    H5EA_sblock_t *sblock = NULL;       /* Pointer to super block for EA */
    H5EA_dblock_t *dblock = NULL;       /* Pointer to data block for EA */
    H5EA_dblk_page_t *dblk_page = NULL; /* Pointer to data block page for EA */
    unsigned iblock_cache_flags = H5AC__NO_FLAGS_SET;   /* Flags to unprotecting index block */
    unsigned sblock_cache_flags = H5AC__NO_FLAGS_SET;   /* Flags to unprotecting super block */
    unsigned dblock_cache_flags = H5AC__NO_FLAGS_SET;   /* Flags to unprotecting data block */
    unsigned dblk_page_cache_flags = H5AC__NO_FLAGS_SET;   /* Flags to unprotecting data block page */
    hbool_t hdr_dirty = FALSE;          /* Whether header information changed */

#ifdef QAK
HDfprintf(stderr, "%s: Called\n", FUNC);
HDfprintf(stderr, "%s: Index %Hu\n", FUNC, idx);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(ea);
    HDassert(ea->hdr);

    /* Set the shared array header's file context for this operation */
    hdr->f = ea->f;

    /* Check if we should create the index block */
    if(!H5F_addr_defined(hdr->idx_blk_addr)) {
#ifdef QAK
HDfprintf(stderr, "%s: Index block address not defined!\n", FUNC, idx);
#endif /* QAK */
        /* Create the index block */
        hdr->idx_blk_addr = H5EA__iblock_create(hdr, dxpl_id, &hdr_dirty);
        if(!H5F_addr_defined(hdr->idx_blk_addr))
            H5E_THROW(H5E_CANTCREATE, "unable to create index block")
    } /* end if */
#ifdef QAK
HDfprintf(stderr, "%s: Index block address is: %a\n", FUNC, hdr->idx_blk_addr);
#endif /* QAK */

    /* Protect index block */
    if(NULL == (iblock = H5EA__iblock_protect(hdr, dxpl_id, H5AC_WRITE)))
        H5E_THROW(H5E_CANTPROTECT, "unable to protect extensible array index block, address = %llu", (unsigned long long)hdr->idx_blk_addr)

    /* Check if element is in index block */
    if(idx < hdr->cparam.idx_blk_elmts) {
        /* Set element in index block */
        HDmemcpy(((uint8_t *)iblock->elmts) + (hdr->cparam.cls->nat_elmt_size * idx), elmt, hdr->cparam.cls->nat_elmt_size);
        iblock_cache_flags |= H5AC__DIRTIED_FLAG;
    } /* end if */
    else {
        unsigned sblk_idx;      /* Which superblock does this index fall in? */
        size_t dblk_idx;        /* Data block index */
        hsize_t elmt_idx;       /* Offset of element in super block */

        /* Get super block index where element is located */
        sblk_idx = H5EA__dblock_sblk_idx(hdr, idx);
#ifdef QAK
HDfprintf(stderr, "%s: sblk_idx = %u, iblock->nsblks = %Zu\n", FUNC, sblk_idx, iblock->nsblks);
#endif /* QAK */

        /* Adjust index to offset in super block */
        elmt_idx = idx - (hdr->cparam.idx_blk_elmts + hdr->sblk_info[sblk_idx].start_idx);
#ifdef QAK
HDfprintf(stderr, "%s: after adjusting for super block elements, elmt_idx = %Hu\n", FUNC, elmt_idx);
#endif /* QAK */

        /* Check for data block containing element address in the index block */
        if(sblk_idx < iblock->nsblks) {
#ifdef QAK
HDfprintf(stderr, "%s: Element in data block pointed to by address in index block\n", FUNC);
#endif /* QAK */
            /* Compute the data block index in index block */
            dblk_idx = (size_t)(hdr->sblk_info[sblk_idx].start_dblk + (elmt_idx / hdr->sblk_info[sblk_idx].dblk_nelmts));
#ifdef QAK
HDfprintf(stderr, "%s: dblk_idx = %u, iblock->ndblk_addrs = %Zu\n", FUNC, dblk_idx, iblock->ndblk_addrs);
#endif /* QAK */
            HDassert(dblk_idx < iblock->ndblk_addrs);

            /* Check if the data block has been allocated on disk yet */
            if(!H5F_addr_defined(iblock->dblk_addrs[dblk_idx])) {
                haddr_t dblk_addr;        /* Address of data block created */
                hsize_t dblk_off;         /* Offset of data block in array */

                /* Create data block */
                dblk_off = hdr->sblk_info[sblk_idx].start_idx + (dblk_idx * hdr->sblk_info[sblk_idx].dblk_nelmts);
                dblk_addr = H5EA__dblock_create(hdr, dxpl_id, &hdr_dirty, dblk_off, hdr->sblk_info[sblk_idx].dblk_nelmts);
                if(!H5F_addr_defined(dblk_addr))
                    H5E_THROW(H5E_CANTCREATE, "unable to create extensible array data block")

                /* Set data block address in index block */
                iblock->dblk_addrs[dblk_idx] = dblk_addr;
                iblock_cache_flags |= H5AC__DIRTIED_FLAG;
            } /* end if */

            /* Protect data block */
            if(NULL == (dblock = H5EA__dblock_protect(hdr, dxpl_id, iblock->dblk_addrs[dblk_idx], hdr->sblk_info[sblk_idx].dblk_nelmts, H5AC_WRITE)))
                H5E_THROW(H5E_CANTPROTECT, "unable to protect extensible array data block, address = %llu", (unsigned long long)iblock->dblk_addrs[dblk_idx])

            /* Adjust index to offset in data block */
            elmt_idx %= hdr->sblk_info[sblk_idx].dblk_nelmts;

            /* Set element in data block */
            HDmemcpy(((uint8_t *)dblock->elmts) + (hdr->cparam.cls->nat_elmt_size * elmt_idx), elmt, hdr->cparam.cls->nat_elmt_size);
            dblock_cache_flags |= H5AC__DIRTIED_FLAG;
        } /* end if */
        else {
            unsigned sblk_off;  /* Offset of super block in index block array of super blocks */
            void *elmts;        /* Buffer for elements */

            /* Calculate offset of super block in index block's array */
            sblk_off = sblk_idx - iblock->nsblks;

            /* Check if the super block has been allocated on disk yet */
            if(!H5F_addr_defined(iblock->sblk_addrs[sblk_off])) {
                haddr_t sblk_addr;        /* Address of data block created */

                /* Create super block */
                sblk_addr = H5EA__sblock_create(hdr, dxpl_id, &hdr_dirty, sblk_idx);
#ifdef QAK
HDfprintf(stderr, "%s: New super block address is: %a\n", FUNC, sblk_addr);
#endif /* QAK */
                if(!H5F_addr_defined(sblk_addr))
                    H5E_THROW(H5E_CANTCREATE, "unable to create extensible array super block")

                /* Set super block address in index block */
                iblock->sblk_addrs[sblk_off] = sblk_addr;
                iblock_cache_flags |= H5AC__DIRTIED_FLAG;
            } /* end if */

            /* Protect super block */
            if(NULL == (sblock = H5EA__sblock_protect(hdr, dxpl_id, iblock->sblk_addrs[sblk_off], sblk_idx, H5AC_WRITE)))
                H5E_THROW(H5E_CANTPROTECT, "unable to protect extensible array super block, address = %llu", (unsigned long long)iblock->sblk_addrs[sblk_off])

            /* Compute the data block index in super block */
            dblk_idx = (size_t)(elmt_idx / sblock->dblk_nelmts);
#ifdef QAK
HDfprintf(stderr, "%s: dblk_idx = %u, sblock->ndblks = %Zu\n", FUNC, dblk_idx, sblock->ndblks);
#endif /* QAK */
            HDassert(dblk_idx < sblock->ndblks);

            /* Check if the data block has been allocated on disk yet */
            if(!H5F_addr_defined(sblock->dblk_addrs[dblk_idx])) {
                haddr_t dblk_addr;        /* Address of data block created */
                hsize_t dblk_off;         /* Offset of data block in array */

                /* Create data block */
                dblk_off = hdr->sblk_info[sblk_idx].start_idx + (dblk_idx * hdr->sblk_info[sblk_idx].dblk_nelmts);
                dblk_addr = H5EA__dblock_create(hdr, dxpl_id, &hdr_dirty, dblk_off, sblock->dblk_nelmts);
                if(!H5F_addr_defined(dblk_addr))
                    H5E_THROW(H5E_CANTCREATE, "unable to create extensible array data block")

                /* Set data block address in index block */
                sblock->dblk_addrs[dblk_idx] = dblk_addr;
                sblock_cache_flags |= H5AC__DIRTIED_FLAG;
            } /* end if */

#ifdef QAK
if(sblock->dblk_npages)
    HDfprintf(stderr, "%s: Check 1.0: elmt_idx = %Hu\n", FUNC, elmt_idx);
#endif /* QAK */
            /* Adjust index to offset in data block */
            elmt_idx %= sblock->dblk_nelmts;
#ifdef QAK
if(sblock->dblk_npages)
    HDfprintf(stderr, "%s: Check 2.0: elmt_idx = %Hu\n", FUNC, elmt_idx);
#endif /* QAK */

            /* Check if the data block is paged */
            if(sblock->dblk_npages) {
                haddr_t dblk_page_addr;         /* Address of data block page */
                size_t page_idx;                /* Index of page within data block */
                size_t page_init_idx;           /* Index of 'page init' bit */

                /* Compute page index */
                page_idx = (size_t)elmt_idx / hdr->dblk_page_nelmts;

                /* Compute 'page init' index */
                page_init_idx = (dblk_idx * sblock->dblk_npages) + page_idx;

                /* Adjust index to offset in data block page */
                elmt_idx %= hdr->dblk_page_nelmts;

                /* Compute data block page address */
                dblk_page_addr = sblock->dblk_addrs[dblk_idx] +
                        H5EA_DBLOCK_PREFIX_SIZE(sblock) + 
                        (page_idx * sblock->dblk_page_size);
#ifdef QAK
HDfprintf(stderr, "%s: sblock->addr = %a\n", FUNC, sblock->addr);
HDfprintf(stderr, "%s: sblock->dblk_addrs[%Zu] = %a\n", FUNC, dblk_idx, sblock->dblk_addrs[dblk_idx]);
HDfprintf(stderr, "%s: H5EA_DBLOCK_PREFIX_SIZE(sblock) = %u\n", FUNC, (unsigned)H5EA_DBLOCK_PREFIX_SIZE(sblock));
HDfprintf(stderr, "%s: sblock->page_init[%Zu] = %t\n", FUNC, page_init_idx, H5V_bit_get(sblock->page_init, page_init_idx));
HDfprintf(stderr, "%s: page_idx = %Zu, elmt_idx = %Hu, dblk_page_addr = %a\n", FUNC, page_idx, elmt_idx, dblk_page_addr);
HDfprintf(stderr, "%s: sblock->dblk_page_size = %Zu\n", FUNC, sblock->dblk_page_size);
#endif /* QAK */

                /* Check if page has been initialized yet */
                if(!H5V_bit_get(sblock->page_init, page_init_idx)) {
                    /* Create the data block page */
                    if(H5EA__dblk_page_create(hdr, dxpl_id, dblk_page_addr) < 0)
                        H5E_THROW(H5E_CANTCREATE, "unable to create data block page")

                    /* Mark data block page as initialized in super block */
                    H5V_bit_set(sblock->page_init, page_init_idx, TRUE);
                    sblock_cache_flags |= H5AC__DIRTIED_FLAG;
                } /* end if */

                /* Protect data block page */
                if(NULL == (dblk_page = H5EA__dblk_page_protect(hdr, dxpl_id, dblk_page_addr, H5AC_WRITE)))
                    H5E_THROW(H5E_CANTPROTECT, "unable to protect extensible array data block page, address = %llu", (unsigned long long)dblk_page_addr)

                /* Set pointer to elements */
                elmts = dblk_page->elmts;
            } /* end if */
            else {
                /* Protect data block */
                if(NULL == (dblock = H5EA__dblock_protect(hdr, dxpl_id, sblock->dblk_addrs[dblk_idx], sblock->dblk_nelmts, H5AC_WRITE)))
                    H5E_THROW(H5E_CANTPROTECT, "unable to protect extensible array data block, address = %llu", (unsigned long long)sblock->dblk_addrs[dblk_idx])

                /* Set pointer to elements */
                elmts = dblock->elmts;
            } /* end else */

            /* Set element in data block */
            HDmemcpy(((uint8_t *)elmts) + (hdr->cparam.cls->nat_elmt_size * elmt_idx), elmt, hdr->cparam.cls->nat_elmt_size);

            /* Mark data block/data block page as dirty now */
            if(sblock->dblk_npages)
                dblk_page_cache_flags |= H5AC__DIRTIED_FLAG;
            else
                dblock_cache_flags |= H5AC__DIRTIED_FLAG;
        } /* end else */
    } /* end else */

    /* Update max. element set in array, if appropriate */
#ifdef QAK
HDfprintf(stderr, "%s: idx = %Hu, hdr->stats.max_idx_set = %Hu\n", FUNC, idx, hdr->stats.max_idx_set);
#endif /* QAK */
    if(idx >= hdr->stats.max_idx_set) {
        hdr->stats.max_idx_set = idx + 1;
        hdr_dirty = TRUE;
    } /* end if */

CATCH
    /* Check for header modified */
    if(hdr_dirty)
        if(H5EA__hdr_modified(hdr) < 0)
            H5E_THROW(H5E_CANTMARKDIRTY, "unable to mark extensible array header as modified")

    /* Release resources */
    if(iblock && H5EA__iblock_unprotect(iblock, dxpl_id, iblock_cache_flags) < 0)
        H5E_THROW(H5E_CANTUNPROTECT, "unable to release extensible array index block")
    if(sblock && H5EA__sblock_unprotect(sblock, dxpl_id, sblock_cache_flags) < 0)
        H5E_THROW(H5E_CANTUNPROTECT, "unable to release extensible array super block")
    if(dblock && H5EA__dblock_unprotect(dblock, dxpl_id, dblock_cache_flags) < 0)
        H5E_THROW(H5E_CANTUNPROTECT, "unable to release extensible array data block")
    if(dblk_page && H5EA__dblk_page_unprotect(dblk_page, dxpl_id, dblk_page_cache_flags) < 0)
        H5E_THROW(H5E_CANTUNPROTECT, "unable to release extensible array data block page")

END_FUNC(PRIV)  /* end H5EA_set() */


/*-------------------------------------------------------------------------
 * Function:	H5EA_get
 *
 * Purpose:	Get an element of an extensible array
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 11 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PRIV, ERR,
herr_t, SUCCEED, FAIL,
H5EA_get(const H5EA_t *ea, hid_t dxpl_id, hsize_t idx, void *elmt))

    /* Local variables */
    H5EA_hdr_t *hdr = ea->hdr;          /* Header for EA */
    H5EA_iblock_t *iblock = NULL;       /* Pointer to index block for EA */
    H5EA_sblock_t *sblock = NULL;       /* Pointer to super block for EA */
    H5EA_dblock_t *dblock = NULL;       /* Pointer to data block for EA */
    H5EA_dblk_page_t *dblk_page = NULL; /* Pointer to data block page for EA */

#ifdef QAK
HDfprintf(stderr, "%s: Called\n", FUNC);
HDfprintf(stderr, "%s: Index %Hu\n", FUNC, idx);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(ea);
    HDassert(ea->hdr);

    /* Set the shared array header's file context for this operation */
    hdr->f = ea->f;

    /* Check for element beyond max. element in array */
    if(idx >= hdr->stats.max_idx_set) {
#ifdef QAK
HDfprintf(stderr, "%s: Element beyond max. index set, hdr->stats.max_idx_set = %Hu, idx = %Hu\n", FUNC, hdr->stats.max_idx_set, idx);
#endif /* QAK */
        /* Call the class's 'fill' callback */
        if((hdr->cparam.cls->fill)(elmt, (size_t)1) < 0)
            H5E_THROW(H5E_CANTSET, "can't set element to class's fill value")
    } /* end if */
    else {
#ifdef QAK
HDfprintf(stderr, "%s: Index block address is: %a\n", FUNC, hdr->idx_blk_addr);
#endif /* QAK */

        /* Protect index block */
        if(NULL == (iblock = H5EA__iblock_protect(hdr, dxpl_id, H5AC_READ)))
            H5E_THROW(H5E_CANTPROTECT, "unable to protect extensible array index block, address = %llu", (unsigned long long)hdr->idx_blk_addr)

        /* Check if element is in index block */
        if(idx < hdr->cparam.idx_blk_elmts) {
            /* Get element from index block */
            HDmemcpy(elmt, ((uint8_t *)iblock->elmts) + (hdr->cparam.cls->nat_elmt_size * idx), hdr->cparam.cls->nat_elmt_size);
        } /* end if */
        else {
            unsigned sblk_idx;      /* Which superblock does this index fall in? */
            size_t dblk_idx;            /* Data block index */
            hsize_t elmt_idx;       /* Offset of element in super block */

            /* Get super block index where element is located */
            sblk_idx = H5EA__dblock_sblk_idx(hdr, idx);

            /* Adjust index to offset in super block */
            elmt_idx = idx - (hdr->cparam.idx_blk_elmts + hdr->sblk_info[sblk_idx].start_idx);
#ifdef QAK
HDfprintf(stderr, "%s: after adjusting for super block elements, elmt_idx = %Hu\n", FUNC, elmt_idx);
#endif /* QAK */

            /* Check for data block containing element address in the index block */
            if(sblk_idx < iblock->nsblks) {
#ifdef QAK
HDfprintf(stderr, "%s: Element in data block pointed to by address in index block\n", FUNC);
#endif /* QAK */
                /* Compute the data block index in index block */
                dblk_idx = (size_t)(hdr->sblk_info[sblk_idx].start_dblk + (elmt_idx / hdr->sblk_info[sblk_idx].dblk_nelmts));
#ifdef QAK
HDfprintf(stderr, "%s: dblk_idx = %u\n", FUNC, dblk_idx);
#endif /* QAK */
                HDassert(dblk_idx < iblock->ndblk_addrs);

                /* Check if the data block has been allocated on disk yet */
                if(!H5F_addr_defined(iblock->dblk_addrs[dblk_idx])) {
                    /* Call the class's 'fill' callback */
                    if((hdr->cparam.cls->fill)(elmt, (size_t)1) < 0)
                        H5E_THROW(H5E_CANTSET, "can't set element to class's fill value")
                } /* end if */
                else {
                    /* Protect data block */
                    if(NULL == (dblock = H5EA__dblock_protect(hdr, dxpl_id, iblock->dblk_addrs[dblk_idx], hdr->sblk_info[sblk_idx].dblk_nelmts, H5AC_READ)))
                        H5E_THROW(H5E_CANTPROTECT, "unable to protect extensible array data block, address = %llu", (unsigned long long)iblock->dblk_addrs[dblk_idx])

                    /* Adjust index to offset in data block */
                    elmt_idx %= hdr->sblk_info[sblk_idx].dblk_nelmts;

                    /* Retrieve element from data block */
                    HDmemcpy(elmt, ((uint8_t *)dblock->elmts) + (hdr->cparam.cls->nat_elmt_size * elmt_idx), hdr->cparam.cls->nat_elmt_size);
                } /* end else */
            } /* end if */
            else {
                unsigned sblk_off;          /* Offset of super block in index block array of super blocks */

                /* Calculate offset of super block in index block's array */
                sblk_off = sblk_idx - iblock->nsblks;

                /* Check if the super block has been allocated on disk yet */
                if(!H5F_addr_defined(iblock->sblk_addrs[sblk_off])) {
                    /* Call the class's 'fill' callback */
                    if((hdr->cparam.cls->fill)(elmt, (size_t)1) < 0)
                        H5E_THROW(H5E_CANTSET, "can't set element to class's fill value")
                } /* end if */
                else {
                    /* Protect super block */
                    if(NULL == (sblock = H5EA__sblock_protect(hdr, dxpl_id, iblock->sblk_addrs[sblk_off], sblk_idx, H5AC_READ)))
                        H5E_THROW(H5E_CANTPROTECT, "unable to protect extensible array super block, address = %llu", (unsigned long long)iblock->sblk_addrs[sblk_off])

                    /* Compute the data block index in super block */
                    dblk_idx = (size_t)(elmt_idx / sblock->dblk_nelmts);
#ifdef QAK
HDfprintf(stderr, "%s: dblk_idx = %u, sblock->ndblks = %Zu\n", FUNC, dblk_idx, sblock->ndblks);
#endif /* QAK */
                    HDassert(dblk_idx < sblock->ndblks);

                    /* Check if the data block has been allocated on disk yet */
                    if(!H5F_addr_defined(sblock->dblk_addrs[dblk_idx])) {
                        /* Call the class's 'fill' callback */
                        if((hdr->cparam.cls->fill)(elmt, (size_t)1) < 0)
                            H5E_THROW(H5E_CANTSET, "can't set element to class's fill value")
                    } /* end if */
                    else {
                        void *elmts;        /* Buffer for elements */

#ifdef QAK
if(sblock->dblk_npages)
    HDfprintf(stderr, "%s: Check 1.0: elmt_idx = %Hu\n", FUNC, elmt_idx);
#endif /* QAK */
                        /* Adjust index to offset in data block */
                        elmt_idx %= sblock->dblk_nelmts;
#ifdef QAK
if(sblock->dblk_npages)
    HDfprintf(stderr, "%s: Check 2.0: elmt_idx = %Hu\n", FUNC, elmt_idx);
#endif /* QAK */

                        /* Check if the data block is paged */
                        if(sblock->dblk_npages) {
                            size_t page_idx;                /* Index of page within data block */
                            size_t page_init_idx;           /* Index of 'page init' bit */

                            /* Compute page index */
                            page_idx = (size_t)elmt_idx / hdr->dblk_page_nelmts;

                            /* Compute 'page init' index */
                            page_init_idx = (dblk_idx * sblock->dblk_npages) + page_idx;

#ifdef QAK
HDfprintf(stderr, "%s: sblock->addr = %a\n", FUNC, sblock->addr);
HDfprintf(stderr, "%s: sblock->dblk_addrs[%Zu] = %a\n", FUNC, dblk_idx, sblock->dblk_addrs[dblk_idx]);
HDfprintf(stderr, "%s: H5EA_DBLOCK_PREFIX_SIZE(sblock) = %u\n", FUNC, (unsigned)H5EA_DBLOCK_PREFIX_SIZE(sblock));
HDfprintf(stderr, "%s: sblock->page_init[%Zu] = %t\n", FUNC, page_init_idx, H5V_bit_get(sblock->page_init, page_init_idx));
HDfprintf(stderr, "%s: page_idx = %Zu\n", FUNC, page_idx);
HDfprintf(stderr, "%s: sblock->dblk_page_size = %Zu\n", FUNC, sblock->dblk_page_size);
#endif /* QAK */

                            /* Check if page has been initialized yet */
                            if(!H5V_bit_get(sblock->page_init, page_init_idx)) {
                                /* Call the class's 'fill' callback */
                                if((hdr->cparam.cls->fill)(elmt, (size_t)1) < 0)
                                    H5E_THROW(H5E_CANTSET, "can't set element to class's fill value")

                                /* We've retrieved the value, leave now */
                                H5_LEAVE(SUCCEED)
                            } /* end if */
                            else {
                                haddr_t dblk_page_addr;     /* Address of data block page */

                                /* Adjust index to offset in data block page */
                                elmt_idx %= hdr->dblk_page_nelmts;

                                /* Compute data block page address */
                                dblk_page_addr = sblock->dblk_addrs[dblk_idx] +
                                        H5EA_DBLOCK_PREFIX_SIZE(sblock) +
                                        (page_idx * sblock->dblk_page_size);
#ifdef QAK
HDfprintf(stderr, "%s: elmt_idx = %Hu, dblk_page_addr = %a\n", FUNC, elmt_idx, dblk_page_addr);
#endif /* QAK */

                                /* Protect data block page */
                                if(NULL == (dblk_page = H5EA__dblk_page_protect(hdr, dxpl_id, dblk_page_addr, H5AC_READ)))
                                    H5E_THROW(H5E_CANTPROTECT, "unable to protect extensible array data block page, address = %llu", (unsigned long long)dblk_page_addr)

                                /* Set pointer to elements */
                                elmts = dblk_page->elmts;
                            } /* end else */
                        } /* end if */
                        else {
                            /* Protect data block */
                            if(NULL == (dblock = H5EA__dblock_protect(hdr, dxpl_id, sblock->dblk_addrs[dblk_idx], sblock->dblk_nelmts, H5AC_READ)))
                                H5E_THROW(H5E_CANTPROTECT, "unable to protect extensible array data block, address = %llu", (unsigned long long)sblock->dblk_addrs[dblk_idx])

                            /* Set pointer to elements */
                            elmts = dblock->elmts;
                        } /* end else */

                        /* Retrieve element from data block */
                        HDmemcpy(elmt, ((uint8_t *)elmts) + (hdr->cparam.cls->nat_elmt_size * elmt_idx), hdr->cparam.cls->nat_elmt_size);
                    } /* end else */
                } /* end else */
            } /* end else */
        } /* end else */
    } /* end else */

CATCH
    if(iblock && H5EA__iblock_unprotect(iblock, dxpl_id, H5AC__NO_FLAGS_SET) < 0)
        H5E_THROW(H5E_CANTUNPROTECT, "unable to release extensible array index block")
    if(sblock && H5EA__sblock_unprotect(sblock, dxpl_id, H5AC__NO_FLAGS_SET) < 0)
        H5E_THROW(H5E_CANTUNPROTECT, "unable to release extensible array super block")
    if(dblock && H5EA__dblock_unprotect(dblock, dxpl_id, H5AC__NO_FLAGS_SET) < 0)
        H5E_THROW(H5E_CANTUNPROTECT, "unable to release extensible array data block")
    if(dblk_page && H5EA__dblk_page_unprotect(dblk_page, dxpl_id, H5AC__NO_FLAGS_SET) < 0)
        H5E_THROW(H5E_CANTUNPROTECT, "unable to release extensible array data block page")

END_FUNC(PRIV)  /* end H5EA_get() */


/*-------------------------------------------------------------------------
 * Function:	H5EA_close
 *
 * Purpose:	Close an extensible array
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug 21 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PRIV, ERR,
herr_t, SUCCEED, FAIL,
H5EA_close(H5EA_t *ea, hid_t dxpl_id))

    /* Local variables */
    hbool_t pending_delete = FALSE;     /* Whether the array is pending deletion */
    haddr_t ea_addr = HADDR_UNDEF;      /* Address of array (for deletion) */

#ifdef QAK
HDfprintf(stderr, "%s: Called\n", FUNC);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(ea);

    /* Decrement file reference & check if this is the last open extensible array using the shared array header */
    if(0 == H5EA__hdr_fuse_decr(ea->hdr)) {
        /* Set the shared array header's file context for this operation */
        ea->hdr->f = ea->f;

        /* Shut down anything that can't be put in the header's 'flush' callback */

        /* Check for pending array deletion */
        if(ea->hdr->pending_delete) {
            /* Set local info, so array deletion can occur after decrementing the
             *  header's ref count
             */
            pending_delete = TRUE;
            ea_addr = ea->hdr->addr;
        } /* end if */
    } /* end if */

    /* Decrement the reference count on the array header */
    /* (don't put in H5EA_hdr_fuse_decr() as the array header may be evicted
     *  immediately -QAK)
     */
    if(H5EA__hdr_decr(ea->hdr) < 0)
        H5E_THROW(H5E_CANTDEC, "can't decrement reference count on shared array header")

    /* Check for pending array deletion */
    if(pending_delete) {
        H5EA_hdr_t *hdr;            /* Another pointer to extensible array header */

        /* Lock the array header into memory */
        if(NULL == (hdr = (H5EA_hdr_t *)H5AC_protect(ea->f, dxpl_id, H5AC_EARRAY_HDR, ea_addr, NULL, NULL, H5AC_WRITE)))
            H5E_THROW(H5E_CANTLOAD, "unable to load extensible array header")

        /* Set the shared array header's file context for this operation */
        hdr->f = ea->f;

        /* Delete array, starting with header (unprotects header) */
        if(H5EA__hdr_delete(hdr, dxpl_id) < 0)
            H5E_THROW(H5E_CANTDELETE, "unable to delete extensible array")
    } /* end if */

    /* Release the extensible array wrapper */
    ea = H5FL_FREE(H5EA_t, ea);

CATCH

END_FUNC(PRIV)  /* end H5EA_close() */


/*-------------------------------------------------------------------------
 * Function:	H5EA_delete
 *
 * Purpose:	Delete an extensible array
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug 28 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PRIV, ERR,
herr_t, SUCCEED, FAIL,
H5EA_delete(H5F_t *f, hid_t dxpl_id, haddr_t ea_addr))

    /* Local variables */
    H5EA_hdr_t *hdr = NULL;             /* The fractal heap header information */

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(H5F_addr_defined(ea_addr));

    /* Lock the array header into memory */
#ifdef QAK
HDfprintf(stderr, "%s: ea_addr = %a\n", FUNC, ea_addr);
#endif /* QAK */
    if(NULL == (hdr = (H5EA_hdr_t *)H5AC_protect(f, dxpl_id, H5AC_EARRAY_HDR, ea_addr, NULL, NULL, H5AC_WRITE)))
        H5E_THROW(H5E_CANTPROTECT, "unable to protect extensible array header, address = %llu", (unsigned long long)ea_addr)

    /* Check for files using shared array header */
    if(hdr->file_rc)
        hdr->pending_delete = TRUE;
    else {
        /* Set the shared array header's file context for this operation */
        hdr->f = f;

        /* Delete array now, starting with header (unprotects header) */
        if(H5EA__hdr_delete(hdr, dxpl_id) < 0)
            H5E_THROW(H5E_CANTDELETE, "unable to delete extensible array")
        hdr = NULL;
    } /* end if */

CATCH

    /* Unprotect the header, if an error occurred */
    if(hdr && H5AC_unprotect(f, dxpl_id, H5AC_EARRAY_HDR, ea_addr, hdr, H5AC__NO_FLAGS_SET) < 0)
        H5E_THROW(H5E_CANTUNPROTECT, "unable to release extensible array header")

END_FUNC(PRIV)  /* end H5EA_delete() */

