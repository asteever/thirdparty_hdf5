/****************************************************************************
 * NCSA HDF                                                                 *
 * Software Development Group                                               *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/

/*
 * This file contains public declarations for the H5S module.
 */
#ifndef _H5Spublic_H
#define _H5Spublic_H

/* Public headers needed by this file */
#include <H5public.h>
#include <H5Ipublic.h>

/* Define atomic datatypes */
#define H5S_ALL         (-2)
#define H5S_UNLIMITED	((hsize_t)(hssize_t)(-1))

/* Define user-level maximum number of dimensions */
/* This is not used internally to the library, see H5O_LAYOUT_NDIMS for the internal constant */
#define H5S_MAX_RANK    31

/* Different types of dataspaces */
typedef enum H5S_class_t {
    H5S_NO_CLASS         = -1,  /*error                                      */
    H5S_SCALAR           = 0,   /*scalar variable                            */
    H5S_SIMPLE           = 1,   /*simple data space                          */
    H5S_COMPLEX          = 2    /*complex data space                         */
} H5S_class_t;

/* Different ways of combining selections */
typedef enum H5S_seloper_t {
    H5S_SELECT_NOOP      = -1,  /* error                                     */
    H5S_SELECT_SET       = 0,   /* Select "set" operation */
    H5S_SELECT_OR,              /* Binary "or" operation (add new selection to existing selection) */
    H5S_SELECT_INVALID          /* Invalid upper bound on selection operations */
} H5S_seloper_t;

#ifdef __cplusplus
extern "C" {
#endif

/* Functions in H5S.c */
hid_t H5Screate(H5S_class_t type);
hid_t H5Screate_simple (int rank, const hsize_t dims[],
			const hsize_t maxdims[]);
herr_t H5Sset_extent_simple (hid_t space_id, int rank, const hsize_t dims[],
			     const hsize_t max[]);
hid_t H5Scopy (hid_t space_id);
herr_t H5Sclose (hid_t space_id);
hsize_t H5Sget_simple_extent_npoints (hid_t space_id);
int H5Sget_simple_extent_ndims (hid_t space_id);
int H5Sget_simple_extent_dims (hid_t space_id, hsize_t dims[], hsize_t maxdims[]);
hbool_t H5Sis_simple (hid_t space_id);
herr_t H5Sset_space (hid_t space_id, int rank, const hsize_t *dims);
hsize_t H5Sget_select_npoints (hid_t spaceid);
herr_t H5Sselect_hyperslab (hid_t space_id, H5S_seloper_t op,
			    const hssize_t start[], const hsize_t _stride[],
			    const hsize_t count[], const hsize_t _block[]);
herr_t H5Sselect_elements (hid_t space_id, H5S_seloper_t op, size_t num_elemn,
			   const hssize_t **coord);
H5S_class_t H5Sget_simple_extent_type (hid_t space_id);
herr_t H5Sset_extent_none (hid_t space_id);
herr_t H5Sextent_copy (hid_t dst_id,hid_t src_id);
herr_t H5Sselect_all (hid_t spaceid);
herr_t H5Sselect_none (hid_t spaceid);
herr_t H5Soffset_simple (hid_t space_id, const hssize_t *offset);
hbool_t H5Sselect_valid (hid_t spaceid);

#ifdef __cplusplus
}
#endif
#endif
