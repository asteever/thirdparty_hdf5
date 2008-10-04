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

/* Programmer:  Quincey Koziol <koziol@hdfgroup.org>
 *              Thursday, August 28, 2008
 *
 * Purpose:	Extensible array testing functions.
 *
 */

/**********************/
/* Module Declaration */
/**********************/

#define H5EA_MODULE
#define H5EA_TESTING


/***********************/
/* Other Packages Used */
/***********************/


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5EApkg.h"		/* Extensible Arrays			*/
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

/* Extensible array class callbacks */
static herr_t H5EA__test_fill(void *nat_blk, size_t nelmts);
static herr_t H5EA__test_encode(void *raw, const void *elmt, size_t nelmts);
static herr_t H5EA__test_decode(const void *raw, void *elmt, size_t nelmts);
static herr_t H5EA__test_debug(FILE *stream, int indent, int fwidth,
    hsize_t idx, const void *elmt);


/*********************/
/* Package Variables */
/*********************/

/* Extensible array testing class information */
const H5EA_class_t H5EA_CLS_TEST[1]={{
    H5EA_CLS_TEST_ID,           /* Type of Extensible array */
    sizeof(uint64_t),           /* Size of native element */
    H5EA__test_fill,            /* Fill block of missing elements callback */
    H5EA__test_encode,          /* Element encoding callback */
    H5EA__test_decode,          /* Element decoding callback */
    H5EA__test_debug            /* Element debugging callback */
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:	H5EA__test_fill
 *
 * Purpose:	Fill "missing elements" in block of elements
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, August 28, 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, NOERR,
herr_t, SUCCEED, -,
H5EA__test_fill(void *nat_blk, size_t nelmts))

    /* Local variables */
    uint64_t fill_val = H5EA_TEST_FILL;          /* Value to fill elements with */

    /* Sanity checks */
    HDassert(nat_blk);
    HDassert(nelmts);

    H5V_array_fill(nat_blk, &fill_val, sizeof(uint64_t), nelmts);

END_FUNC(STATIC)  /* end H5EA__test_fill() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__test_encode
 *
 * Purpose:	Encode an element from "native" to "raw" form
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, August 28, 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, NOERR,
herr_t, SUCCEED, -,
H5EA__test_encode(void *raw, const void *_elmt, size_t nelmts))

    /* Local variables */
    const uint64_t *elmt = (const uint64_t *)_elmt;     /* Convenience pointer to native elements */

    /* Sanity checks */
    HDassert(raw);
    HDassert(elmt);
    HDassert(nelmts);

    /* Encode native elements into raw elements */
    while(nelmts) {
        /* Encode element */
        /* (advances 'raw' pointer */
        UINT64ENCODE(raw, *elmt);

        /* Advance native element pointer */
        elmt++;

        /* Decrement # of elements to encode */
        nelmts--;
    } /* end while */

END_FUNC(STATIC)  /* end H5EA__test_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__test_decode
 *
 * Purpose:	Decode an element from "raw" to "native" form
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, August 28, 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, NOERR,
herr_t, SUCCEED, -,
H5EA__test_decode(const void *_raw, void *_elmt, size_t nelmts))

    /* Local variables */
    uint64_t *elmt = (uint64_t *)_elmt;     /* Convenience pointer to native elements */
    const uint8_t *raw = (const uint8_t *)_raw; /* Convenience pointer to raw elements */

    /* Sanity checks */
    HDassert(raw);
    HDassert(elmt);
    HDassert(nelmts);

    /* Decode raw elements into native elements */
    while(nelmts) {
        /* Decode element */
        /* (advances 'raw' pointer */
        UINT64DECODE(raw, *elmt);

        /* Advance native element pointer */
        elmt++;

        /* Decrement # of elements to decode */
        nelmts--;
    } /* end while */

END_FUNC(STATIC)  /* end H5EA__test_decode() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__test_debug
 *
 * Purpose:	Display an element for debugging
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, August 28, 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, NOERR,
herr_t, SUCCEED, -,
H5EA__test_debug(FILE *stream, int indent, int fwidth, hsize_t idx,
    const void *elmt))

    /* Local variables */
    char temp_str[128];     /* Temporary string, for formatting */

    /* Sanity checks */
    HDassert(stream);
    HDassert(elmt);

    /* Print element */
    sprintf(temp_str, "Element #%llu:", (unsigned long_long)idx);
    HDfprintf(stream, "%*s%-*s %llu\n", indent, "", fwidth, temp_str,
        (unsigned long_long)*(const uint64_t *)elmt);

END_FUNC(STATIC)  /* end H5EA__test_debug() */


/*-------------------------------------------------------------------------
 * Function:	H5EA_get_cparam_test
 *
 * Purpose:	Retrieve the parameters used to create the extensible array
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, August 28, 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PRIV, NOERR,
herr_t, SUCCEED, -,
H5EA_get_cparam_test(const H5EA_t *ea, H5EA_create_t *cparam))

    /* Check arguments. */
    HDassert(ea);
    HDassert(cparam);

    /* Get extensible array creation parameters */
    cparam->raw_elmt_size = ea->hdr->raw_elmt_size;
    cparam->max_nelmts_bits = ea->hdr->max_nelmts_bits;
    cparam->idx_blk_elmts = ea->hdr->idx_blk_elmts;
    cparam->sup_blk_min_data_ptrs = ea->hdr->sup_blk_min_data_ptrs;
    cparam->data_blk_min_elmts = ea->hdr->data_blk_min_elmts;

END_FUNC(PRIV)  /* end H5EA_get_cparam_test() */


/*-------------------------------------------------------------------------
 * Function:	H5EA_cmp_cparam_test
 *
 * Purpose:	Compare the parameters used to create the extensible array
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, August 28, 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PRIV, ERRCATCH,
int, 0, -,
H5EA_cmp_cparam_test(const H5EA_create_t *cparam1, const H5EA_create_t *cparam2))

    /* Check arguments. */
    HDassert(cparam1);
    HDassert(cparam2);

    /* Compare creation parameters for array */
    if(cparam1->raw_elmt_size < cparam2->raw_elmt_size)
        H5_LEAVE(-1)
    else if(cparam1->raw_elmt_size > cparam2->raw_elmt_size)
        H5_LEAVE(1)
    if(cparam1->max_nelmts_bits < cparam2->max_nelmts_bits)
        H5_LEAVE(-1)
    else if(cparam1->max_nelmts_bits > cparam2->max_nelmts_bits)
        H5_LEAVE(1)
    if(cparam1->idx_blk_elmts < cparam2->idx_blk_elmts)
        H5_LEAVE(-1)
    else if(cparam1->idx_blk_elmts > cparam2->idx_blk_elmts)
        H5_LEAVE(1)
    if(cparam1->sup_blk_min_data_ptrs < cparam2->sup_blk_min_data_ptrs)
        H5_LEAVE(-1)
    else if(cparam1->sup_blk_min_data_ptrs > cparam2->sup_blk_min_data_ptrs)
        H5_LEAVE(1)
    if(cparam1->data_blk_min_elmts < cparam2->data_blk_min_elmts)
        H5_LEAVE(-1)
    else if(cparam1->data_blk_min_elmts > cparam2->data_blk_min_elmts)
        H5_LEAVE(1)

CATCH

END_FUNC(PRIV)  /* end H5EA_cmp_cparam_test() */

