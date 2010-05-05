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
 * Created:		H5HFcache.c
 *			Feb 24 2006
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Implement fractal heap metadata cache methods.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5HF_PACKAGE		/*suppress error about including H5HFpkg  */

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5ACprivate.h"	/* Metadata cache			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5HFpkg.h"		/* Fractal heaps			*/
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Vprivate.h"		/* Vectors and arrays 			*/
#include "H5WBprivate.h"        /* Wrapped Buffers                      */

/****************/
/* Local Macros */
/****************/

/* Fractal heap format version #'s */
#define H5HF_HDR_VERSION        0               /* Header */
#define H5HF_DBLOCK_VERSION     0               /* Direct block */
#define H5HF_IBLOCK_VERSION     0               /* Indirect block */

/* Size of stack buffer for serialized headers */
#define H5HF_HDR_BUF_SIZE       512

/* Size of stack buffer for serialized indirect blocks */
#define H5HF_IBLOCK_BUF_SIZE    4096


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* Local encode/decode routines */
static herr_t H5HF_dtable_encode(H5F_t *f, uint8_t **pp, const H5HF_dtable_t *dtable);
static herr_t H5HF_dtable_decode(H5F_t *f, const uint8_t **pp, H5HF_dtable_t *dtable);

/* Metadata cache (H5AC) callbacks */
static void *H5HF_cache_hdr_deserialize(const void *image, size_t len,
    void *udata, hbool_t *dirty);
static herr_t H5HF_cache_hdr_image_len(const void *thing, size_t *image_len_ptr);
static herr_t H5HF_cache_hdr_serialize(const H5F_t *f, hid_t dxpl_id,
    haddr_t addr, size_t len, void *image, void *thing, unsigned *flags,
    haddr_t *new_addr, size_t *new_len, void **new_image);
static herr_t H5HF_cache_hdr_free_icr(void *thing);

static void *H5HF_cache_iblock_deserialize(const void *image, size_t len,
    void *udata, hbool_t *dirty);
static herr_t H5HF_cache_iblock_serialize(const H5F_t * f, hid_t dxpl_id,
    haddr_t addr, size_t len, void *image, void *_thing, unsigned *flags,
    haddr_t *new_addr, size_t *new_len, void **new_image);
static herr_t H5HF_cache_iblock_free_icr(void *thing);

static void *H5HF_cache_dblock_deserialize(const void *image, size_t len,
    void *udata, hbool_t *dirty);
static herr_t H5HF_cache_dblock_serialize(const H5F_t * f, hid_t dxpl_id,
    haddr_t addr, size_t len, void *image, void *_thing, unsigned *flags,
    haddr_t *new_addr, size_t *new_len, void **new_image);
static herr_t H5HF_cache_dblock_free_icr(void *thing);


/*********************/
/* Package Variables */
/*********************/

/* H5HF header inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_FHEAP_HDR[1] = {{
    H5AC_FHEAP_HDR_ID,
    "fractal heap header",
    H5FD_MEM_FHEAP_HDR,
    H5HF_cache_hdr_deserialize,
    H5HF_cache_hdr_image_len,
    H5HF_cache_hdr_serialize,
    H5HF_cache_hdr_free_icr,
}};

/* H5HF indirect block inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_FHEAP_IBLOCK[1] = {{
    H5AC_FHEAP_IBLOCK_ID,
    "fractal heap indirect block",
    H5FD_MEM_FHEAP_IBLOCK,
    H5HF_cache_iblock_deserialize,
    NULL,
    H5HF_cache_iblock_serialize,
    H5HF_cache_iblock_free_icr,
}};

/* H5HF direct block inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_FHEAP_DBLOCK[1] = {{
    H5AC_FHEAP_DBLOCK_ID,
    "fractal head direct block",
    H5FD_MEM_FHEAP_DBLOCK,
    H5HF_cache_dblock_deserialize,
    NULL,
    H5HF_cache_dblock_serialize,
    H5HF_cache_dblock_free_icr,
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage heap direct block data to/from disk */
H5FL_BLK_DEFINE(direct_block);



/*-------------------------------------------------------------------------
 * Function:	H5HF_dtable_decode
 *
 * Purpose:	Decodes the metadata for a doubling table
 *
 * Return:	Success:	Pointer to a new fractal heap
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 27 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_dtable_decode(H5F_t *f, const uint8_t **pp, H5HF_dtable_t *dtable)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_dtable_decode)

    /* Check arguments */
    HDassert(f);
    HDassert(pp && *pp);
    HDassert(dtable);

    /* Table width */
    UINT16DECODE(*pp, dtable->cparam.width);

    /* Starting block size */
    H5F_DECODE_LENGTH(f, *pp, dtable->cparam.start_block_size);

    /* Maximum direct block size */
    H5F_DECODE_LENGTH(f, *pp, dtable->cparam.max_direct_size);

    /* Maximum heap size (as # of bits) */
    UINT16DECODE(*pp, dtable->cparam.max_index);

    /* Starting # of rows in root indirect block */
    UINT16DECODE(*pp, dtable->cparam.start_root_rows);

    /* Address of table */
    H5F_addr_decode(f, pp, &(dtable->table_addr));

    /* Current # of rows in root indirect block */
    UINT16DECODE(*pp, dtable->curr_root_rows);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_dtable_decode() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_dtable_encode
 *
 * Purpose:	Encodes the metadata for a doubling table
 *
 * Return:	Success:	Pointer to a new fractal heap
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 27 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_dtable_encode(H5F_t *f, uint8_t **pp, const H5HF_dtable_t *dtable)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_dtable_encode)

    /* Check arguments */
    HDassert(f);
    HDassert(pp && *pp);
    HDassert(dtable);

    /* Table width */
    UINT16ENCODE(*pp, dtable->cparam.width);

    /* Starting block size */
    H5F_ENCODE_LENGTH(f, *pp, dtable->cparam.start_block_size);

    /* Maximum direct block size */
    H5F_ENCODE_LENGTH(f, *pp, dtable->cparam.max_direct_size);

    /* Maximum heap size (as # of bits) */
    UINT16ENCODE(*pp, dtable->cparam.max_index);

    /* Starting # of rows in root indirect block */
    UINT16ENCODE(*pp, dtable->cparam.start_root_rows);

    /* Address of root direct/indirect block */
    H5F_addr_encode(f, pp, dtable->table_addr);

    /* Current # of rows in root indirect block */
    UINT16ENCODE(*pp, dtable->curr_root_rows);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_dtable_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_hdr_deserialize
 *
 * Purpose:	Deserialize the data structure from disk.
 *
 * Return:	Success:	SUCCESS
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 24 2006
 *
 *-------------------------------------------------------------------------
 */
static void *
H5HF_cache_hdr_deserialize(const void *image, size_t UNUSED len,
    void *_udata, hbool_t UNUSED *dirty)
{
    H5HF_hdr_t		*hdr = NULL;     /* Fractal heap info */
    H5HF_hdr_cache_ud_t *udata = (H5HF_hdr_cache_ud_t *)_udata;
    size_t		size;           /* Header size */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    uint32_t            stored_chksum;  /* Stored metadata checksum value */
    uint32_t            computed_chksum; /* Computed metadata checksum value */
    uint8_t             heap_flags;     /* Status flags for heap */
    H5HF_hdr_t		*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_cache_hdr_deserialize)

    /* Check arguments */
    HDassert(image);

    /* Allocate space for the fractal heap data structure */
    if(NULL == (hdr = H5HF_hdr_alloc(udata->f)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Compute the 'base' size of the fractal heap header on disk */
    size = H5HF_HEADER_SIZE(hdr);

    /* Get temporary pointer to serialized header */
    p = image;

    /* Magic number */
    if(HDmemcmp(p, H5HF_HDR_MAGIC, (size_t)H5HF_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "wrong fractal heap header signature")
    p += H5HF_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5HF_HDR_VERSION)
	HGOTO_ERROR(H5E_HEAP, H5E_VERSION, NULL, "wrong fractal heap header version")

    /* General heap information */
    UINT16DECODE(p, hdr->id_len);               /* Heap ID length */
    UINT16DECODE(p, hdr->filter_len);           /* I/O filters' encoded length */

    /* Heap status flags */
    /* (bit 0: "huge" object IDs have wrapped) */
    /* (bit 1: checksum direct blocks) */
    heap_flags = *p++;
    hdr->huge_ids_wrapped = heap_flags & H5HF_HDR_FLAGS_HUGE_ID_WRAPPED;
    hdr->checksum_dblocks = heap_flags & H5HF_HDR_FLAGS_CHECKSUM_DBLOCKS;

    /* "Huge" object information */
    UINT32DECODE(p, hdr->max_man_size);         /* Max. size of "managed" objects */
    H5F_DECODE_LENGTH(udata->f, p, hdr->huge_next_id); /* Next ID to use for "huge" object */
    H5F_addr_decode(udata->f, &p, &hdr->huge_bt2_addr); /* Address of "huge" object tracker B-tree */

    /* "Managed" object free space information */
    H5F_DECODE_LENGTH(udata->f, p, hdr->total_man_free); /* Internal free space in managed direct blocks */
    H5F_addr_decode(udata->f, &p, &hdr->fs_addr);      /* Address of free section header */

    /* Heap statistics */
    H5F_DECODE_LENGTH(udata->f, p, hdr->man_size);
    H5F_DECODE_LENGTH(udata->f, p, hdr->man_alloc_size);
    H5F_DECODE_LENGTH(udata->f, p, hdr->man_iter_off);
    H5F_DECODE_LENGTH(udata->f, p, hdr->man_nobjs);
    H5F_DECODE_LENGTH(udata->f, p, hdr->huge_size);
    H5F_DECODE_LENGTH(udata->f, p, hdr->huge_nobjs);
    H5F_DECODE_LENGTH(udata->f, p, hdr->tiny_size);
    H5F_DECODE_LENGTH(udata->f, p, hdr->tiny_nobjs);

    /* Managed objects' doubling-table info */
    if(H5HF_dtable_decode(hdr->f, &p, &(hdr->man_dtable)) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTENCODE, NULL, "unable to encode managed obj. doubling table info")

    /* Sanity check */
    /* (allow for checksum not decoded yet) */
    HDassert((size_t)(p - (const uint8_t *)image) == (size - H5HF_SIZEOF_CHKSUM));

    /* Check for I/O filter information to decode */
    if(hdr->filter_len > 0) {
        size_t filter_info_off;     /* Offset in header of filter information */
        size_t filter_info_size;    /* Size of filter information */
        H5O_pline_t *pline;         /* Pipeline information from the header on disk */

        /* Compute the offset of the filter info in the header */
        filter_info_off = p - (const uint8_t *)image;

        /* Compute the size of the extra filter information */
        filter_info_size = hdr->sizeof_size     /* Size of size for filtered root direct block */
            + 4                                 /* Size of filter mask for filtered root direct block */
            + hdr->filter_len;                  /* Size of encoded I/O filter info */

        /* Compute the heap header's size */
        hdr->heap_size = size + filter_info_size;

        /* Point at correct offset in header for the filter information */
        p = (const uint8_t *)image + filter_info_off;

        /* Decode the size of a filtered root direct block */
        H5F_DECODE_LENGTH(udata->f, p, hdr->pline_root_direct_size);

        /* Decode the filter mask for a filtered root direct block */
        UINT32DECODE(p, hdr->pline_root_direct_filter_mask);

        /* Decode I/O filter information */
        if(NULL == (pline = (H5O_pline_t *)H5O_msg_decode(hdr->f, udata->dxpl_id, H5O_PLINE_ID, p)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDECODE, NULL, "can't decode I/O pipeline filters")
        p += hdr->filter_len;

        /* Copy the information into the header's I/O pipeline structure */
        if(NULL == H5O_msg_copy(H5O_PLINE_ID, pline, &(hdr->pline)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTCOPY, NULL, "can't copy I/O filter pipeline")

        /* Release the space allocated for the I/O pipeline filters */
        H5O_msg_free(H5O_PLINE_ID, pline);
    } /* end if */
    else
        /* Set the heap header's size */
        hdr->heap_size = size;

    /* Compute checksum on entire header */
    /* (including the filter information, if present) */
    computed_chksum = H5_checksum_metadata(image, (size_t)(p - (const uint8_t *)image), 0);

    /* Metadata checksum */
    UINT32DECODE(p, stored_chksum);

    /* Sanity check */
    HDassert((size_t)(p - (const uint8_t *)image) == hdr->heap_size);

    /* Verify checksum */
    if(stored_chksum != computed_chksum)
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, NULL, "incorrect metadata checksum for fractal heap header")

    /* Finish initialization of heap header */
    if(H5HF_hdr_finish_init(hdr) < 0)
	HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, NULL, "can't finish initializing shared fractal heap header")

    /* Set return value */
    ret_value = hdr;

done:
    /* Release resources */
    if(!ret_value && hdr)
        if(H5HF_hdr_dest(hdr) < 0)
            HDONE_ERROR(H5E_HEAP, H5E_CANTFREE, NULL, "unable to destroy fractal heap header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_cache_hdr_deserialize() */ /*lint !e818 Can't make udata a pointer to const */


/*-------------------------------------------------------------------------
 * Function:    H5HF_cache_hdr_image_len
 *
 * Purpose:     Tell the metadata cache about the actual size
 *              of the fractal heap header
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Mike McGreevy
 *              mcgreevy@hdfgroup.org
 *              July 25, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_cache_hdr_image_len(const void *thing, size_t *image_len_ptr)
{
    const H5HF_hdr_t    *hdr = (const H5HF_hdr_t *)thing;    /* Fractal heap header */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_cache_hdr_image_len)

    /* Check arguments */
    HDassert(hdr);
    HDassert(image_len_ptr);

    /* Report the fractal heap header's prefix + I/O filter length */
    *image_len_ptr = H5HF_HEADER_SIZE(hdr) + hdr->filter_len;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_cache_hdr_image_len() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_hdr_serialize
 *
 * Purpose:	Serialize the data structure for writing to disk.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 24 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_cache_hdr_serialize(const H5F_t *f, hid_t UNUSED dxpl_id,
    haddr_t UNUSED addr, size_t UNUSED len, void *image, void *_thing,
    unsigned *flags, haddr_t UNUSED *new_addr, size_t UNUSED *new_len,
    void UNUSED **new_image)
{
    H5HF_hdr_t *hdr = (H5HF_hdr_t *)_thing; /* fractal heap header */
    uint8_t *p;                 /* Pointer into raw data buffer */
    size_t size;                /* Header size on disk */
    uint8_t heap_flags;         /* Status flags for heap */
    uint32_t metadata_chksum;   /* Computed metadata checksum value */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_cache_hdr_serialize)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(hdr);
    HDassert(image);
    HDassert(flags);

    /* Sanity check */
    HDassert(hdr->dirty);

    /* Set the shared heap header's file context for this operation */
    hdr->f = f;

    /* Compute the size of the heap header on disk */
    size = hdr->heap_size;

    /* Get temporary pointer to serialized header */
    p = image;

    /* Magic number */
    HDmemcpy(p, H5HF_HDR_MAGIC, (size_t)H5HF_SIZEOF_MAGIC);
    p += H5HF_SIZEOF_MAGIC;

    /* Version # */
    *p++ = H5HF_HDR_VERSION;

    /* General heap information */
    UINT16ENCODE(p, hdr->id_len);           /* Heap ID length */
    UINT16ENCODE(p, hdr->filter_len);       /* I/O filters' encoded length */

    /* Heap status flags */
    /* (bit 0: "huge" object IDs have wrapped) */
    /* (bit 1: checksum direct blocks) */
    heap_flags = 0;
    heap_flags |= (hdr->huge_ids_wrapped ?  H5HF_HDR_FLAGS_HUGE_ID_WRAPPED : 0);
    heap_flags |= (hdr->checksum_dblocks ?  H5HF_HDR_FLAGS_CHECKSUM_DBLOCKS : 0);
    *p++ = heap_flags;

    /* "Huge" object information */
    UINT32ENCODE(p, hdr->max_man_size);             /* Max. size of "managed" objects */
    H5F_ENCODE_LENGTH(f, p, hdr->huge_next_id);     /* Next ID to use for "huge" object */
    H5F_addr_encode(f, &p, hdr->huge_bt2_addr);     /* Address of "huge" object tracker B-tree */

    /* "Managed" object free space information */
    H5F_ENCODE_LENGTH(f, p, hdr->total_man_free);   /* Internal free space in managed direct blocks */
    H5F_addr_encode(f, &p, hdr->fs_addr);           /* Address of free section header */

    /* Heap statistics */
    H5F_ENCODE_LENGTH(f, p, hdr->man_size);
    H5F_ENCODE_LENGTH(f, p, hdr->man_alloc_size);
    H5F_ENCODE_LENGTH(f, p, hdr->man_iter_off);
    H5F_ENCODE_LENGTH(f, p, hdr->man_nobjs);
    H5F_ENCODE_LENGTH(f, p, hdr->huge_size);
    H5F_ENCODE_LENGTH(f, p, hdr->huge_nobjs);
    H5F_ENCODE_LENGTH(f, p, hdr->tiny_size);
    H5F_ENCODE_LENGTH(f, p, hdr->tiny_nobjs);

    /* Managed objects' doubling-table info */
    if(H5HF_dtable_encode(hdr->f, &p, &(hdr->man_dtable)) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTENCODE, FAIL, "unable to encode managed obj. doubling table info")

    /* Check for I/O filter information to encode */
    if(hdr->filter_len > 0) {
        /* Encode the size of a filtered root direct block */
        H5F_ENCODE_LENGTH(f, p, hdr->pline_root_direct_size);

        /* Encode the filter mask for a filtered root direct block */
        UINT32ENCODE(p, hdr->pline_root_direct_filter_mask);

        /* Encode I/O filter information */
        if(H5O_msg_encode(hdr->f, H5O_PLINE_ID, FALSE, p, &(hdr->pline)) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTENCODE, FAIL, "can't encode I/O pipeline fiters")
            p += hdr->filter_len;
    } /* end if */

    /* Compute metadata checksum */
    metadata_chksum = H5_checksum_metadata(image, (size_t)(p - (const uint8_t *)image), 0);

    /* Metadata checksum */
    UINT32ENCODE(p, metadata_chksum);

    /* Sanity check */
    HDassert((size_t)(p - (const uint8_t *)image) == size);

    /* Reset the cache flags for this operation (metadata not resized or renamed) */
    *flags = 0;

    /* Sanity check */
    HDassert((size_t)((const uint8_t *)p - (const uint8_t *)image) <= len);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_cache_hdr_serialize() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_hdr_free_icr
 *
 * Purpose:	Destroy/release an "in core representation" of a data structure
 *
 * Return:	Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:	Mike McGreevy
 *		mcgreevy@hdfgroup.org
 *		July 25, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_cache_hdr_free_icr(void *thing)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_cache_hdr_free_icr)

    /* Check arguments */
    HDassert(thing);

    /* Destroy fractal heap header */
    if(H5HF_hdr_dest(thing) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to destroy fractal heap header")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_cache_hdr_free_icr() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_iblock_deserialize
 *
 * Purpose:	Loads a fractal heap indirect block from the disk.
 *
 * Return:	Success:	Pointer to a new fractal heap indirect block
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 27 2006
 *
 *-------------------------------------------------------------------------
 */
static void *
H5HF_cache_iblock_deserialize(const void *image, size_t UNUSED len,
    void *_udata, hbool_t UNUSED *dirty)
{
    H5HF_hdr_t          *hdr;           /* Shared fractal heap information */
    H5HF_iblock_cache_ud_t *udata = (H5HF_iblock_cache_ud_t *)_udata; /* user data for callback */
    H5HF_indirect_t	*iblock = NULL; /* Indirect block info */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    haddr_t             heap_addr;      /* Address of heap header in the file */
    uint32_t            stored_chksum;  /* Stored metadata checksum value */
    uint32_t            computed_chksum; /* Computed metadata checksum value */
    size_t              u;              /* Local index variable */
    H5HF_indirect_t	*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_cache_iblock_deserialize)

    /* Check arguments */
    HDassert(image);

    /* Allocate space for the fractal heap indirect block */
    if(NULL == (iblock = H5FL_CALLOC(H5HF_indirect_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&iblock->cache_info, 0, sizeof(H5AC_info_t));

    /* Get the pointer to the shared heap header */
    hdr = udata->par_info->hdr;

    /* Set the shared heap header's file context for this operation */
    hdr->f = udata->f;

    /* Share common heap information */
    iblock->hdr = hdr;
    if(H5HF_hdr_incr(hdr) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't increment reference count on shared heap header")

    /* Set block's internal information */
    iblock->rc = 0;
    iblock->nrows = *udata->nrows;
    iblock->nchildren = 0;

    /* Compute size of indirect block */
    iblock->size = H5HF_MAN_INDIRECT_SIZE(hdr, iblock);
    /* Get temporary pointer to serialized indirect block */
    p = image;

    /* Magic number */
    if(HDmemcmp(p, H5HF_IBLOCK_MAGIC, (size_t)H5HF_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "wrong fractal heap indirect block signature")
    p += H5HF_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5HF_IBLOCK_VERSION)
	HGOTO_ERROR(H5E_HEAP, H5E_VERSION, NULL, "wrong fractal heap direct block version")

    /* Address of heap that owns this block */
    H5F_addr_decode(udata->f, &p, &heap_addr);
    if(H5F_addr_ne(heap_addr, hdr->heap_addr))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "incorrect heap header address for direct block")

    /* Address of parent block */
    iblock->parent = udata->par_info->iblock;
    iblock->par_entry = udata->par_info->entry;
    if(iblock->parent) {
        /* Share parent block */
        if(H5HF_iblock_incr(iblock->parent) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't increment reference count on shared indirect block")

        /* Set max. # of rows in this block */
        iblock->max_rows = iblock->nrows;
    } /* end if */
    else {
        /* Set max. # of rows in this block */
        iblock->max_rows = hdr->man_dtable.max_root_rows;
    } /* end else */

    /* Offset of heap within the heap's address space */
    UINT64DECODE_VAR(p, iblock->block_off, hdr->heap_off_size);

    /* Allocate & decode child block entry tables */
    HDassert(iblock->nrows > 0);
    if(NULL == (iblock->ents = H5FL_SEQ_MALLOC(H5HF_indirect_ent_t, (size_t)(iblock->nrows * hdr->man_dtable.cparam.width))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for direct entries")
    if(hdr->filter_len > 0) {
        unsigned dir_rows;      /* Number of direct rows in this indirect block */

        /* Compute the number of direct rows for this indirect block */
        dir_rows = MIN(iblock->nrows, hdr->man_dtable.max_direct_rows);

        /* Allocate indirect block filtered entry array */
        if(NULL == (iblock->filt_ents = H5FL_SEQ_MALLOC(H5HF_indirect_filt_ent_t, (size_t)(dir_rows * hdr->man_dtable.cparam.width))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for block entries")
    } /* end if */
    else
        iblock->filt_ents = NULL;
    for(u = 0; u < (iblock->nrows * hdr->man_dtable.cparam.width); u++) {
        /* Decode child block address */
        H5F_addr_decode(udata->f, &p, &(iblock->ents[u].addr));

        /* Check for heap with I/O filters */
        if(hdr->filter_len > 0) {
            /* Sanity check */
            HDassert(iblock->filt_ents);

            /* Decode extra information for direct blocks */
            if(u < (hdr->man_dtable.max_direct_rows * hdr->man_dtable.cparam.width)) {
                /* Size of filtered direct block */
                H5F_DECODE_LENGTH(udata->f, p, iblock->filt_ents[u].size);

                /* Sanity check */
                /* (either both the address & size are defined or both are
                 *  not defined)
                 */
                HDassert((H5F_addr_defined(iblock->ents[u].addr) && iblock->filt_ents[u].size)
                    || (!H5F_addr_defined(iblock->ents[u].addr) && iblock->filt_ents[u].size == 0));

                /* I/O filter mask for filtered direct block */
                UINT32DECODE(p, iblock->filt_ents[u].filter_mask);
            } /* end if */
        } /* end if */

        /* Count child blocks */
        if(H5F_addr_defined(iblock->ents[u].addr)) {
            iblock->nchildren++;
            iblock->max_child = u;
        } /* end if */
    } /* end for */

    /* Sanity check */
    HDassert(iblock->nchildren);        /* indirect blocks w/no children should have been deleted */

    /* Compute checksum on indirect block */
    computed_chksum = H5_checksum_metadata(image, (size_t)(p - (const uint8_t *)image), 0);

    /* Metadata checksum */
    UINT32DECODE(p, stored_chksum);

    /* Sanity check */
    HDassert((size_t)(p - (const uint8_t *)image) == iblock->size);

    /* Verify checksum */
    if(stored_chksum != computed_chksum)
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, NULL, "incorrect metadata checksum for fractal heap indirect block")

    /* Check if we have any indirect block children */
    if(iblock->nrows > hdr->man_dtable.max_direct_rows) {
        unsigned indir_rows;      /* Number of indirect rows in this indirect block */

        /* Compute the number of indirect rows for this indirect block */
        indir_rows = iblock->nrows - hdr->man_dtable.max_direct_rows;

        /* Allocate & initialize child indirect block pointer array */
        if(NULL == (iblock->child_iblocks = H5FL_SEQ_CALLOC(H5HF_indirect_ptr_t, (size_t)(indir_rows * hdr->man_dtable.cparam.width))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for block entries")
    } /* end if */
    else
        iblock->child_iblocks = NULL;

    /* Set return value */
    ret_value = iblock;

    /* Sanity check */
    HDassert((size_t)((const uint8_t *)p - (const uint8_t *)image) <= len);

done:
    /* Release resources */
    if(!ret_value && iblock)
        if(H5HF_man_iblock_dest(iblock) < 0)
            HDONE_ERROR(H5E_HEAP, H5E_CANTFREE, NULL, "unable to destroy fractal heap indirect block")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_cache_iblock_deserialize() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_iblock_serialize
 *
 * Purpose:	Flushes a dirty fractal heap indirect block to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar  6 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_cache_iblock_serialize(const H5F_t *f, hid_t UNUSED dxpl_id,
    haddr_t UNUSED addr, size_t UNUSED len, void *image, void *_thing,
    unsigned UNUSED *flags, haddr_t UNUSED *new_addr, size_t UNUSED *new_len,
    void UNUSED **new_image)
{
    H5HF_hdr_t *hdr;                /* Shared fractal heap information */
    H5HF_indirect_t *iblock = (H5HF_indirect_t *)_thing;
    uint8_t *p;                     /* Pointer into raw data buffer */
#ifndef NDEBUG
    unsigned nchildren = 0;         /* Track # of children */
    unsigned max_child = 0;         /* Track max. child entry used */
#endif /* NDEBUG */
    uint32_t metadata_chksum;       /* Computed metadata checksum value */
    size_t u;                       /* Local index variable */
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_cache_iblock_serialize)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(iblock);

    /* Get the pointer to the shared heap header */
    hdr = iblock->hdr;

    /* Set the shared heap header's file context for this operation */
    hdr->f = f;

    /* Get temporary pointer to buffer for serialized indirect block */
    p = image;

    /* Magic number */
    HDmemcpy(p, H5HF_IBLOCK_MAGIC, (size_t)H5HF_SIZEOF_MAGIC);
    p += H5HF_SIZEOF_MAGIC;

    /* Version # */
    *p++ = H5HF_IBLOCK_VERSION;

    /* Address of heap header for heap which owns this block */
    H5F_addr_encode(f, &p, hdr->heap_addr);

    /* Offset of block in heap */
    UINT64ENCODE_VAR(p, iblock->block_off, hdr->heap_off_size);

    /* Encode indirect block-specific fields */
    for(u = 0; u < (iblock->nrows * hdr->man_dtable.cparam.width); u++) {
        /* Encode child block address */
        H5F_addr_encode(f, &p, iblock->ents[u].addr);

        /* Check for heap with I/O filters */
        if(hdr->filter_len > 0) {
            /* Sanity check */
            HDassert(iblock->filt_ents);

            /* Encode extra information for direct blocks */
            if(u < (hdr->man_dtable.max_direct_rows * hdr->man_dtable.cparam.width)) {
                /* Sanity check */
                /* (either both the address & size are defined or both are
                 *  not defined)
                 */
                HDassert((H5F_addr_defined(iblock->ents[u].addr) && iblock->filt_ents[u].size)
                    || (!H5F_addr_defined(iblock->ents[u].addr) && iblock->filt_ents[u].size == 0));

                /* Size of filtered direct block */
                H5F_ENCODE_LENGTH(f, p, iblock->filt_ents[u].size);

                /* I/O filter mask for filtered direct block */
                UINT32ENCODE(p, iblock->filt_ents[u].filter_mask);
            } /* end if */
        } /* end if */

#ifndef NDEBUG
        /* Count child blocks */
        if(H5F_addr_defined(iblock->ents[u].addr)) {
            nchildren++;
            if(u > max_child)
                max_child = u;
        } /* end if */
#endif /* NDEBUG */
    } /* end for */

    /* Compute checksum */
    metadata_chksum = H5_checksum_metadata(image, (size_t)((const uint8_t *)p - (const uint8_t *)image), 0);

    /* Metadata checksum */
    UINT32ENCODE(p, metadata_chksum);

    /* Reset the cache flags for this operation (metadata not resized or renamed) */
    *flags = 0;

    /* Sanity check */
    HDassert((size_t)((const uint8_t *)p - (const uint8_t *)image) <= len);

    /* Sanity check */
    HDassert((size_t)(p - (const uint8_t *)image) == iblock->size);
#ifndef NDEBUG
    HDassert(nchildren == iblock->nchildren);
    HDassert(max_child == iblock->max_child);
#endif /* NDEBUG */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_cache_iblock_serialize() */


/*-------------------------------------------------------------------------
 * Function:    H5HF_cache_iblock_free_icr
 *
 * Purpose:     Destroy/release an "in core representation" of a data structure
 *
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 *
 * Programmer:  Mike McGreevy
 *              mcgreevy@hdfgroup.org
 *              July 25, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_cache_iblock_free_icr(void *thing)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_cache_iblock_free_icr)

    /* Check arguments */
    HDassert(thing);

    /* Destroy fractal heap indirect block */
    if(H5HF_man_iblock_dest(thing) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to destroy fractal heap indirect block")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_cache_iblock_free_icr() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_dblock_deserialize
 *
 * Purpose:	Given a direct block disk image, construct and return the
 * 		associated in core representation of the fractal heap
 * 		direct block.
 *
 * 		Note that this function is a heavily re-worked version
 * 		of the old H5HF_cache_dblock_load() routine, which had
 * 		to be replaced convert the fractal heap to use the new
 * 		journaling version of the cache.
 *
 * Return:	Success:	Pointer to a new fractal heap direct block
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 27 2006
 *
 *-------------------------------------------------------------------------
 */
static void *
H5HF_cache_dblock_deserialize(const void *image, size_t len, void *_udata,
    hbool_t UNUSED *dirty)
{
    H5HF_dblock_cache_ud_t *udata = (H5HF_dblock_cache_ud_t *)_udata; /* pointer to user data */
    H5HF_hdr_t          *hdr;           /* Shared fractal heap information */
    H5HF_parent_t       *par_info;      /* Pointer to parent information */
    H5HF_direct_t	*dblock = NULL; /* Direct block info */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    haddr_t             heap_addr;      /* Address of heap header in the file */
    H5HF_direct_t	*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_cache_dblock_deserialize)

    /* Check arguments */
    HDassert(len > 0);
    HDassert(image != NULL);
    HDassert(udata != NULL);
    HDassert(udata->f != NULL);
    HDassert(udata->dblock_size > 0);

    /* Allocate space for the fractal heap direct block */
    if(NULL == (dblock = H5FL_MALLOC(H5HF_direct_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&dblock->cache_info, 0, sizeof(H5AC_info_t));

    /* Get the pointer to the shared heap header */
    par_info = (H5HF_parent_t *)(&(udata->par_info));
    hdr = par_info->hdr;

    /* Set the shared heap header's file context for this operation */
    hdr->f = udata->f;

    /* Share common heap information */
    dblock->hdr = hdr;
    if(H5HF_hdr_incr(hdr) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't increment reference count on shared heap header")

    /* Set block's internal information */
    dblock->size = udata->dblock_size;
    dblock->blk_off_size = H5HF_SIZEOF_OFFSET_LEN(dblock->size);

    /* Allocate block buffer */
/* XXX: Change to using free-list factories */
    if(NULL == (dblock->blk = H5FL_BLK_MALLOC(direct_block, (size_t)dblock->size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* copy the disk image into the in core image, with filtering
     * if appropriate.
     */
    if(hdr->filter_len > 0) {
        H5Z_cb_t filter_cb = {NULL, NULL};  /* Filter callback structure */
        size_t nbytes;          /* Number of bytes used in buffer, after applying reverse filters */
        void *read_buf;         /* Pointer to buffer to read in */
        size_t read_size;       /* Size of filtered direct block to read */
        unsigned filter_mask;	/* Excluded filters for direct block */

        /* Allocate buffer to perform I/O filtering on, and
         * then copy the on disk image into it.
         *
         * Note that one could argue that we should just do the
         * filtering in the buffer provided by the cache, and in
         * theory there is no reason why we shouldn't.  However,
         * I can see some scenarios in which this would cause problems,
         * and in any case, we have declared it to be constant.  Thus
         * we will make a copy instead.
         */
        if(NULL == (read_buf = H5MM_malloc(len)))
            HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, NULL, "memory allocation failed for pipeline buffer")

        /* Copy disk image into read_buf */
        HDmemcpy(read_buf, image, len);

        /* Push direct block data through I/O filter pipeline */
        nbytes = len;
        read_size = len;
        filter_mask = udata->filter_mask;

        if(H5Z_pipeline(&(hdr->pline), H5Z_FLAG_REVERSE, &filter_mask, H5Z_ENABLE_EDC, filter_cb, &nbytes, &read_size, &read_buf) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTFILTER, NULL, "output pipeline failed")

        /* Sanity check */
        HDassert(nbytes == dblock->size);

        /* Copy un-filtered data into block's buffer */
        HDmemcpy(dblock->blk, read_buf, dblock->size);

        /* Release the read buffer */
        H5MM_xfree(read_buf);
    } /* end if */
    else {
        /* Sanity check */
        HDassert(len == dblock->size);

        /* just copy the disk image into dblock->blk */
        HDmemcpy(dblock->blk, image, len);
    } /* end no filtering case */

    /* Start decoding direct block */
    p = dblock->blk;

    /* Magic number */
    if(HDmemcmp(p, H5HF_DBLOCK_MAGIC, (size_t)H5HF_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "wrong fractal heap direct block signature")
    p += H5HF_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5HF_DBLOCK_VERSION)
	HGOTO_ERROR(H5E_HEAP, H5E_VERSION, NULL, "wrong fractal heap direct block version")

    /* Address of heap that owns this block (just for file integrity checks) */
    H5F_addr_decode(udata->f, &p, &heap_addr);
    if(H5F_addr_ne(heap_addr, hdr->heap_addr))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "incorrect heap header address for direct block")

    /* Address of parent block */
    dblock->parent = par_info->iblock;
    dblock->par_entry = par_info->entry;
    if(dblock->parent) {
        /* Share parent block */
        if(H5HF_iblock_incr(dblock->parent) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't increment reference count on shared indirect block")
    } /* end if */

    /* Offset of heap within the heap's address space */
    UINT64DECODE_VAR(p, dblock->block_off, hdr->heap_off_size);

    /* Decode checksum on direct block, if requested */
    if(hdr->checksum_dblocks) {
        uint32_t stored_chksum;         /* Metadata checksum value */
        uint32_t computed_chksum;       /* Computed metadata checksum value */

        /* Metadata checksum */
        UINT32DECODE(p, stored_chksum);

        /* Reset checksum field, for computing the checksum */
        /* (Casting away const OK - QAK) */
        HDmemset((uint8_t *)p - H5HF_SIZEOF_CHKSUM, 0, (size_t)H5HF_SIZEOF_CHKSUM);

        /* Compute checksum on entire direct block */
        computed_chksum = H5_checksum_metadata(dblock->blk, dblock->size, 0);

        /* Verify checksum */
        if(stored_chksum != computed_chksum)
            HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, NULL, "incorrect metadata checksum for fractal heap direct block")
    } /* end if */

    /* Sanity check */
    HDassert((size_t)(p - dblock->blk) == H5HF_MAN_ABS_DIRECT_OVERHEAD(hdr));

    /* Set return value */
    ret_value = dblock;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_cache_dblock_deserialize() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_dblock_serialize
 *
 * Purpose:	Construct the on disk image of the target direct block
 *		in preparation for writing the direct block to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 27 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_cache_dblock_serialize(const H5F_t *f, hid_t dxpl_id, haddr_t addr,
    size_t len, void * image, void * _thing, unsigned *flags, haddr_t *new_addr,
    size_t *new_len, void **new_image)
{
    H5HF_hdr_t *hdr;        /* Shared fractal heap information */
    H5HF_direct_t *dblock;
    void * write_buf;       /* Pointer to buffer to write out */
    size_t write_size;      /* Size of buffer to write out */
    uint8_t * p;            /* Pointer into raw data buffer */
#ifndef NDEBUG
    hbool_t entry_filtered = FALSE;
#endif
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_cache_dblock_serialize)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(len > 0);
    HDassert(image);
    HDassert(_thing);

    dblock = (H5HF_direct_t *)_thing;

    HDassert(dblock->cache_info.is_dirty);
    HDassert(flags);
    HDassert(new_addr);
    HDassert(new_len);
    HDassert(new_image);

    /* set *flags to 0 -- will overwrite if needed */
    *flags = 0;

    /* Get the pointer to the shared heap header */
    hdr = dblock->hdr;

    /* Set the shared heap header's file context for this operation */
    hdr->f = f;

    HDassert(dblock->blk);
    p = dblock->blk;

    /* Magic number */
    HDmemcpy(p, H5HF_DBLOCK_MAGIC, (size_t)H5HF_SIZEOF_MAGIC);
    p += H5HF_SIZEOF_MAGIC;

    /* Version # */
    *p++ = H5HF_DBLOCK_VERSION;

    /* Address of heap header for heap which owns this block */
    H5F_addr_encode(f, &p, hdr->heap_addr);

    /* Offset of block in heap */
    UINT64ENCODE_VAR(p, dblock->block_off, hdr->heap_off_size);

    /* Metadata checksum */
    if(hdr->checksum_dblocks) {
        uint32_t metadata_chksum;       /* Computed metadata checksum value */

        /* Clear the checksum field, to compute the checksum */
        HDmemset(p, 0, (size_t)H5HF_SIZEOF_CHKSUM);

        /* Compute checksum on entire direct block */
        metadata_chksum = H5_checksum_metadata(dblock->blk, dblock->size, 0);

        /* Metadata checksum */
        UINT32ENCODE(p, metadata_chksum);
    } /* end if */

    /* Sanity check */
    HDassert((size_t)(p - dblock->blk) == H5HF_MAN_ABS_DIRECT_OVERHEAD(hdr));

    /* Check for I/O filters on this heap */
    if(hdr->filter_len > 0) {
        H5Z_cb_t filter_cb = {NULL, NULL};  /* Filter callback structure */
        size_t nbytes;                      /* Number of bytes used */
        unsigned filter_mask;               /* Filter mask for block */

#ifndef NDEBUG
        entry_filtered = TRUE;
#endif /* NDEBUG */

        /* Allocate buffer to perform I/O filtering on */
        write_size = dblock->size;
        if(NULL == (write_buf = H5MM_malloc(write_size)))
            HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, FAIL, "memory allocation failed for pipeline buffer")

        HDmemcpy(write_buf, dblock->blk, write_size);

        /* Push direct block data through I/O filter pipeline */
        nbytes = write_size;
        if(H5Z_pipeline(&(hdr->pline), 0, &filter_mask, H5Z_ENABLE_EDC, filter_cb, &nbytes, &write_size, &write_buf) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_WRITEERROR, FAIL, "output pipeline failed")

        /* Use the compressed number of bytes as the size to write */
        write_size = nbytes;

        /* Check for root direct block */
        if(dblock->parent == NULL) {
            hbool_t hdr_changed = FALSE;    /* Whether the header information changed */

            /* Sanity check */
            HDassert(H5F_addr_eq(hdr->man_dtable.table_addr, addr));
            HDassert(hdr->pline_root_direct_size > 0);

            /* Check if the filter mask changed */
            if(hdr->pline_root_direct_filter_mask != filter_mask) {
                hdr->pline_root_direct_filter_mask = filter_mask;
                hdr_changed = TRUE;
            } /* end if */

            /* Check if we need to re-size the block on disk */
            if(hdr->pline_root_direct_size != write_size) {
                /* Release direct block's current disk space */
                if(H5MF_xfree(f, H5FD_MEM_FHEAP_DBLOCK, dxpl_id, addr, (hsize_t)hdr->pline_root_direct_size) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to free fractal heap direct block")

                /* Allocate space for the compressed direct block */
                if(HADDR_UNDEF == (addr = H5MF_alloc(f, H5FD_MEM_FHEAP_DBLOCK, dxpl_id, (hsize_t)write_size)))
                    HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, FAIL, "file allocation failed for fractal heap direct block")

                *flags |= H5AC__SERIALIZE_RESIZED_FLAG;
		*new_len = write_size;

                /* Let the metadata cache know, if the block moved */
                if(!H5F_addr_eq(hdr->man_dtable.table_addr, addr)) {
		    *flags |= H5AC__SERIALIZE_RENAMED_FLAG;
		    *new_addr = addr;
		} /* end if */

                /* Update information about compressed direct block's
		 * location & size
		 */
                hdr->man_dtable.table_addr = addr;
                hdr->pline_root_direct_size = write_size;

                /* Note that heap header was modified */
                hdr_changed = TRUE;
            } /* end if */

            /* Check if heap header was modified */
            if(hdr_changed)
                if(H5HF_hdr_dirty(hdr) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark heap header as dirty")
        } /* end if */
        else {
            hbool_t par_changed = FALSE;    /* Whether the parent's information changed */
            H5HF_indirect_t *par_iblock;    /* Parent indirect block */
            unsigned par_entry;             /* Entry in parent indirect block */

            /* Get parent information */
            par_iblock = dblock->parent;
            par_entry = dblock->par_entry;

            /* Sanity check */
            HDassert(H5F_addr_eq(par_iblock->ents[par_entry].addr, addr));
            HDassert(par_iblock->filt_ents[par_entry].size > 0);

            /* Check if the filter mask changed */
            if(par_iblock->filt_ents[par_entry].filter_mask != filter_mask) {
                par_iblock->filt_ents[par_entry].filter_mask = filter_mask;
                par_changed = TRUE;
            } /* end if */

            /* Check if we need to re-size the block on disk */
            if(par_iblock->filt_ents[par_entry].size != write_size) {
                /* Release direct block's current disk space */
                if(H5MF_xfree(f, H5FD_MEM_FHEAP_DBLOCK, dxpl_id, addr, (hsize_t)par_iblock->filt_ents[par_entry].size) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to free fractal heap direct block")

                /* Allocate space for the compressed direct block */
                if(HADDR_UNDEF == (addr = H5MF_alloc(f, H5FD_MEM_FHEAP_DBLOCK, dxpl_id, (hsize_t)write_size)))
                    HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, FAIL, "file allocation failed for fractal heap direct block")

                *flags |= H5AC__SERIALIZE_RESIZED_FLAG;
		*new_len = write_size;

                /* Let the metadata cache know, if the block moved */
                if(!H5F_addr_eq(par_iblock->ents[par_entry].addr, addr)) {
		    *flags |= H5AC__SERIALIZE_RENAMED_FLAG;
		    *new_addr = addr;
		} /* end if */

                /* Update information about compressed direct
		 * block's location & size
		 */
                par_iblock->ents[par_entry].addr = addr;
                par_iblock->filt_ents[par_entry].size = write_size;

                /* Note that parent was modified */
                par_changed = TRUE;
            } /* end if */

            /* Check if parent was modified */
            if(par_changed)
                if(H5HF_iblock_dirty(par_iblock) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark heap header as dirty")
        } /* end else */
    } /* end if */
    else {
        write_buf = dblock->blk;
        write_size = dblock->size;
    } /* end else */

    if(write_size == len) {
	HDassert(*flags == 0);
	HDassert(write_buf != NULL);
	HDassert(entry_filtered || (write_buf == dblock->blk));

        HDmemcpy(image, write_buf, write_size);

        if(write_buf != dblock->blk)
	    H5MM_xfree(write_buf);
    } /* end if */
    else {
	/* on disk image has been resized, and possibly renamed -- *flags,
	 * *new_len, and *new_addr should all be setup by now.
	 * Thus all we need to do here is the old image, and allocate
	 * space for the new image.
	 */
	HDassert(*flags != 0);
	HDassert(write_buf != NULL);
	HDassert(*new_len = write_size);
	HDassert(write_buf != dblock->blk);

	H5MM_xfree(image);

	*new_image = write_buf;
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_cache_dblock_serialize() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_dblock_free_icr
 *
 * Purpose:	Destroys a fractal heap direct block in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 27 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_cache_dblock_free_icr(void *thing)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_cache_dblock_free_icr)

    /*
     * Check arguments.
     */
    HDassert(thing);

    /* Destroy fractal heap direct block */
    if(H5HF_man_dblock_dest(thing) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to destroy fractal heap direct block")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_cache_dblock_free_icr() */

