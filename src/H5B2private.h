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
 * Created:		H5B2private.h
 *			Jan 31 2005
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Private header for library accessible B-tree routines.
 *
 *-------------------------------------------------------------------------
 */

#ifndef _H5B2private_H
#define _H5B2private_H

/* Include package's public header */
#include "H5B2public.h"

/* Private headers needed by this file */
#include "H5Fprivate.h"		/* File access				*/

/**************************/
/* Library Private Macros */
/**************************/

/* Define return values from operator callback function for H5B2_iterate */
/* (Actually, any positive value will cause the iterator to stop and pass back
 *      that positive value to the function that called the iterator)
 */
#define H5B2_ITER_ERROR  (-1)
#define H5B2_ITER_CONT   (0)
#define H5B2_ITER_STOP   (1)

/* Define the operator callback function pointer for H5B2_iterate() */
typedef int (*H5B2_operator_t)(const void *record, void *op_data);

/* Define the 'found' callback function pointer for H5B2_find() */
typedef herr_t (*H5B2_found_t)(const void *record, void *op_data);

     
/****************************/
/* Library Private Typedefs */
/****************************/

/* B-tree IDs for various internal things. */
typedef enum H5B2_subid_t {
    H5B2_TEST_ID	 = 0,	/* B-tree is for testing (do not use for actual data) */
    H5B2_GRP_NAME_ID,		/* B-tree is for group links, ordered by name	*/
    H5B2_NUM_BTREE_ID           /* Number of B-tree IDs (must be last)   */
} H5B2_subid_t;

/* Comparisons for H5B2_neighbor() call */
typedef enum H5B2_compare_t {
    H5B2_COMPARE_LESS,            /* Records with keys less than query value */
    H5B2_COMPARE_GREATER          /* Records with keys greater than query value */
} H5B2_compare_t;

/*
 * Each class of object that can be pointed to by a B-link tree has a
 * variable of this type that contains class variables and methods.
 */
typedef struct H5B2_class_t {
    H5B2_subid_t id;				/*id as found in file*/
    size_t	nrec_size;			/*size of native (memory) record*/

    /* Store & retrieve record from application to B-tree 'native' form */
    herr_t (*store)(void *nrecord, const void *udata);                  /*  Store record in native record table */
    herr_t (*retrieve)(void *udata, const void *nrecord);               /*  Retrieve record in native record table */

    /* Compare records, according to a key */
    herr_t (*compare)(const void *rec1, const void *rec2);              /*  Compare two native records */

    /* Encode & decode record values */
    herr_t (*encode)(const H5F_t *f, uint8_t *raw, const void *record); /*  Encode record from native form to disk storage form */
    herr_t (*decode)(const H5F_t *f, const uint8_t *raw, void *record); /*  Decode record from disk storage form to native form */

    /* Debug record values */
    herr_t (*debug) (FILE *stream, const H5F_t *f, hid_t dxpl_id,       /* Print a record for debugging */
        int indent, int fwidth, const void *record, const void *udata);

} H5B2_class_t;

/***************************************/
/* Library-private Function Prototypes */
/***************************************/
H5_DLL herr_t H5B2_create(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type,
    size_t node_size, size_t rrec_size,
    unsigned split_percent, unsigned merge_percent,
    haddr_t *addr_p);
H5_DLL herr_t H5B2_insert(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type,
    haddr_t addr, void *udata);
H5_DLL herr_t H5B2_iterate(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type,
    haddr_t addr, H5B2_operator_t op, void *op_data);
H5_DLL herr_t H5B2_find(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type,
    haddr_t addr, void *udata, H5B2_found_t op, void *op_data);
H5_DLL herr_t H5B2_index(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type,
    haddr_t addr, hsize_t idx, H5B2_found_t op, void *op_data);
H5_DLL herr_t H5B2_neighbor(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type,
    haddr_t addr, H5B2_compare_t comp, void *udata, H5B2_found_t op,
    void *op_data);
H5_DLL herr_t H5B2_remove(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type,
    haddr_t addr, void *udata);
H5_DLL herr_t H5B2_get_nrec(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type,
    haddr_t addr, hsize_t *nrec);
H5_DLL herr_t H5B2_delete(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type,
    haddr_t addr);

#endif /* _H5B2private_H */

