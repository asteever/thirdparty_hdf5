/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:		H5HFtiny.c
 *			Aug 14 2006
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Routines for "tiny" objects in fractal heap
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
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5HFpkg.h"		/* Fractal heaps			*/


/****************/
/* Local Macros */
/****************/

/* Tiny object length information */
#define H5HF_TINY_LEN_SHORT     16              /* Max. length able to be encoded in first heap ID byte */
#define H5HF_TINY_MASK_SHORT    0x0F            /* Mask for length in first heap ID byte */
#define H5HF_TINY_MASK_EXT      0x0FFF          /* Mask for length in two heap ID bytes */
#define H5HF_TINY_MASK_EXT_1    0x0F00          /* Mask for length in first byte of two heap ID bytes */
#define H5HF_TINY_MASK_EXT_2    0x00FF          /* Mask for length in second byte of two heap ID bytes */


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


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/


/*-------------------------------------------------------------------------
 * Function:	H5HF_tiny_init
 *
 * Purpose:	Initialize information for tracking 'tiny' objects
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug 14 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_tiny_init(H5HF_hdr_t *hdr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_tiny_init)

    /*
     * Check arguments.
     */
    HDassert(hdr);

    /* Compute information about 'tiny' objects for the heap */

    /* Check if tiny objects need an extra byte for their length */
    /* (account for boundary condition when length of an object would need an
     *  extra byte, but using that byte means that the extra length byte is
     *  unneccessary)
     */
    if((hdr->id_len - 1) <= H5HF_TINY_LEN_SHORT) {
        hdr->tiny_max_len = hdr->id_len - 1;
        hdr->tiny_len_extended = FALSE;
    } /* end if */
    else if((hdr->id_len - 1) == (H5HF_TINY_LEN_SHORT + 1)) {
        hdr->tiny_max_len = H5HF_TINY_LEN_SHORT;
        hdr->tiny_len_extended = FALSE;
    } /* end if */
    else {
        hdr->tiny_max_len = hdr->id_len - 2;
        hdr->tiny_len_extended = TRUE;
    } /* end else */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_tiny_init() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_tiny_insert
 *
 * Purpose:	Pack a 'tiny' object in a heap ID
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug 14 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_tiny_insert(H5HF_hdr_t *hdr, size_t obj_size, const void *obj, void *_id)
{
    uint8_t *id = (uint8_t *)_id;       /* Pointer to ID buffer */
    size_t enc_obj_size;                /* Encoded object size */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_tiny_insert)
#ifdef QAK
HDfprintf(stderr, "%s: obj_size = %Zu\n", FUNC, obj_size);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(obj_size <= hdr->tiny_max_len);
    HDassert(obj_size <= (H5HF_TINY_MASK_EXT + 1));
    HDassert(obj);
    HDassert(id);

    /* Adjust object's size for encoding it */
    enc_obj_size = obj_size - 1;

    /* Encode object into ID */
    if(!hdr->tiny_len_extended) {
        *id++ = H5HF_ID_VERS_CURR | H5HF_ID_TYPE_TINY |
                (enc_obj_size & H5HF_TINY_MASK_SHORT);
    } /* end if */
    else {
        *id++ = H5HF_ID_VERS_CURR | H5HF_ID_TYPE_TINY |
                ((enc_obj_size & H5HF_TINY_MASK_EXT_1) >> 8);
        *id++ = enc_obj_size & H5HF_TINY_MASK_EXT_2;
    } /* end else */
    HDmemcpy(id, obj, obj_size);

    /* Update statistics about heap */
    hdr->tiny_size += obj_size;
    hdr->tiny_nobjs++;

    /* Mark heap header as modified */
    if(H5HF_hdr_dirty(hdr) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark heap header as dirty")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_tiny_insert() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_tiny_get_obj_len
 *
 * Purpose:	Get the size of a 'tiny' object in a fractal heap
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug 14 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_tiny_get_obj_len(H5HF_hdr_t *hdr, const uint8_t *id, size_t *obj_len_p)
{
    size_t enc_obj_size;                /* Encoded object size */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_tiny_get_obj_len)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(id);
    HDassert(obj_len_p);

    /* Check if 'tiny' object ID is in extended form, and retrieve encoded size */
    if(!hdr->tiny_len_extended)
        enc_obj_size = *id & H5HF_TINY_MASK_SHORT;
    else
        enc_obj_size = ((*id & H5HF_TINY_MASK_EXT_1) << 8) | (*(id + 1) & H5HF_TINY_MASK_EXT_2);

    /* Set the object's length */
    *obj_len_p = enc_obj_size + 1;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_tiny_get_obj_len() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_tiny_read
 *
 * Purpose:	Read a 'tiny' object from the heap
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug  8 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_tiny_read(H5HF_hdr_t *hdr, const uint8_t *id, void *obj)
{
    size_t enc_obj_size;                /* Encoded object size */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_tiny_read)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(id);
    HDassert(obj);

    /* Check if 'tiny' object ID is in extended form */
    if(!hdr->tiny_len_extended) {
        /* Retrieve the object's encoded length */
        enc_obj_size = *id & H5HF_TINY_MASK_SHORT;

        /* Advance past flag byte(s) */
        id++;
    } /* end if */
    else {
        /* Retrieve the object's encoded length */
        enc_obj_size = ((*id & H5HF_TINY_MASK_EXT_1) << 8) | (*(id + 1) & H5HF_TINY_MASK_EXT_2);

        /* Advance past flag byte(s) */
        id+=2;
    } /* end else */

    /* Retrieve the object's data */
    HDmemcpy(obj, id, (enc_obj_size + 1));

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_tiny_read() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_tiny_remove
 *
 * Purpose:	Remove a 'tiny' object from the heap statistics
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug 14 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_tiny_remove(H5HF_hdr_t *hdr, const uint8_t *id)
{
    size_t enc_obj_size;                /* Encoded object size */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_tiny_remove)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(id);

    /* Check if 'tiny' object ID is in extended form */
    if(!hdr->tiny_len_extended)
        enc_obj_size = *id & H5HF_TINY_MASK_SHORT;
    else
        enc_obj_size = ((*id & H5HF_TINY_MASK_EXT_1) << 8) | (*(id + 1) & H5HF_TINY_MASK_EXT_2);

    /* Update statistics about heap */
    hdr->tiny_size -= (enc_obj_size + 1);
    hdr->tiny_nobjs--;

    /* Mark heap header as modified */
    if(H5HF_hdr_dirty(hdr) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark heap header as dirty")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_tiny_remove() */

