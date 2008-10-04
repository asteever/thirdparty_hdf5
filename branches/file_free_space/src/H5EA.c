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
        H5E_THROW(H5E_CANTPROTECT, "unable to load extensible array header, address = %llu", (unsigned long_long)ea_addr)

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
    *nelmts = ea->hdr->max_idx_set;

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
        hdr->idx_blk_addr = H5EA__iblock_create(hdr, dxpl_id);
        if(!H5F_addr_defined(hdr->idx_blk_addr))
            H5E_THROW(H5E_CANTCREATE, "unable to create index block")

        /* Mark the header dirty */
        hdr_dirty = TRUE;
    } /* end if */
#ifdef QAK
HDfprintf(stderr, "%s: Index block address is: %a\n", FUNC, hdr->idx_blk_addr);
#endif /* QAK */

    /* Protect index block */
    if(NULL == (iblock = H5EA__iblock_protect(hdr, dxpl_id, H5AC_WRITE)))
        H5E_THROW(H5E_CANTPROTECT, "unable to protect extensible array index block, address = %llu", (unsigned long_long)hdr->idx_blk_addr)

    /* Determine where the element to set falls */
    if(idx < hdr->idx_blk_elmts) {
        /* Set element in index block */
        HDmemcpy(((uint8_t *)iblock->elmts) + (hdr->cls->nat_elmt_size * idx), elmt, hdr->cls->nat_elmt_size);
    } /* end if */
    else {
HDfprintf(stderr, "%s: Index %Hu not supported yet!\n", FUNC, idx);
HDassert(0 && "Index location not supported!");
    } /* end else */

    /* Update max. element set in array, if appropriate */
    if(idx >= hdr->max_idx_set) {
        hdr->max_idx_set = idx + 1;
        hdr_dirty = TRUE;
    } /* end if */

CATCH
    /* Check for header modified */
    if(hdr_dirty)
        if(H5EA__hdr_modified(hdr) < 0)
            H5E_THROW(H5E_CANTMARKDIRTY, "unable to mark extensible array header as modified")

    if(iblock && H5EA__iblock_unprotect(iblock, dxpl_id, H5AC__DIRTIED_FLAG) < 0)
        H5E_THROW(H5E_CANTUNPROTECT, "unable to release extensible array index block")

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
    if(idx >= hdr->max_idx_set) {
#ifdef QAK
HDfprintf(stderr, "%s: Element beyond max. index set\n", FUNC, idx);
#endif /* QAK */
        /* Call the class's 'fill' callback */
        if((hdr->cls->fill)(elmt, (size_t)1) < 0)
            H5E_THROW(H5E_CANTSET, "can't set element to class's fill value")
    } /* end if */
    else {
#ifdef QAK
HDfprintf(stderr, "%s: Index block address is: %a\n", FUNC, hdr->idx_blk_addr);
#endif /* QAK */

        /* Protect index block */
        if(NULL == (iblock = H5EA__iblock_protect(hdr, dxpl_id, H5AC_READ)))
            H5E_THROW(H5E_CANTPROTECT, "unable to protect extensible array index block, address = %llu", (unsigned long_long)hdr->idx_blk_addr)

        /* Determine where the element to set falls */
        if(idx < hdr->idx_blk_elmts) {
            /* Get element from index block */
            HDmemcpy(elmt, ((uint8_t *)iblock->elmts) + (hdr->cls->nat_elmt_size * idx), hdr->cls->nat_elmt_size);
        } /* end if */
        else {
HDfprintf(stderr, "%s: Index %Hu not supported yet!\n", FUNC, idx);
HDassert(0 && "Index location not supported!");
        } /* end else */
    } /* end else */

CATCH
    if(iblock && H5EA__iblock_unprotect(iblock, dxpl_id, H5AC__NO_FLAGS_SET) < 0)
        H5E_THROW(H5E_CANTUNPROTECT, "unable to release extensible array index block")

END_FUNC(PRIV)  /* end H5EA_set() */


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
    (void)H5FL_FREE(H5EA_t, ea);

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
        H5E_THROW(H5E_CANTPROTECT, "unable to protect extensible array header, address = %llu", (unsigned long_long)ea_addr)

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

