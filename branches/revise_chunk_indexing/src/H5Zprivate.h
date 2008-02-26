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

/* Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Thursday, April 16, 1998
 */

#ifndef _H5Zprivate_H
#define _H5Zprivate_H

/* Include package's public header */
#include "H5Zpublic.h"

/* Private headers needed by this file */
#include "H5Tprivate.h"		/* Datatypes				*/

/**************************/
/* Library Private Macros */
/**************************/

/* Special parameters for szip compression */
/* [These are aliases for the similar definitions in szlib.h, which we can't
 * include directly due to the duplication of various symbols with the zlib.h
 * header file] */
#define H5_SZIP_LSB_OPTION_MASK         8
#define H5_SZIP_MSB_OPTION_MASK         16
#define H5_SZIP_RAW_OPTION_MASK         128

/* Common # of 'client data values' for filters */
/* (avoids dynamic memory allocation in most cases) */
#define H5Z_COMMON_CD_VALUES    4

/* Common size of filter name */
/* (avoids dynamic memory allocation in most cases) */
#define H5Z_COMMON_NAME_LEN    12

/****************************/
/* Library Private Typedefs */
/****************************/

/* Structure to store information about each filter's parameters */
typedef struct {
    H5Z_filter_t	id;		/*filter identification number	     */
    unsigned		flags;		/*defn and invocation flags	     */
    char		_name[H5Z_COMMON_NAME_LEN];	/*internal filter name		     */
    char		*name;		/*optional filter name		     */
    size_t		cd_nelmts;	/*number of elements in cd_values[]  */
    unsigned		_cd_values[H5Z_COMMON_CD_VALUES];	/*internal client data values		     */
    unsigned		*cd_values;	/*client data values		     */
} H5Z_filter_info_t;

/*****************************/
/* Library-private Variables */
/*****************************/


/***************************************/
/* Library-private Function Prototypes */
/***************************************/
struct H5O_pline_t; /*forward decl*/

/* Internal API routines */
H5_DLL herr_t H5Z_register(const H5Z_class_t *cls);
H5_DLL herr_t H5Z_unregister(H5Z_filter_t id);
H5_DLL herr_t H5Z_append(struct H5O_pline_t *pline, H5Z_filter_t filter,
        unsigned flags, size_t cd_nelmts, const unsigned int cd_values[]);
H5_DLL herr_t H5Z_modify(const struct H5O_pline_t *pline, H5Z_filter_t filter,
        unsigned flags, size_t cd_nelmts, const unsigned int cd_values[]);
H5_DLL herr_t H5Z_pipeline(const struct H5O_pline_t *pline,
			    unsigned flags, unsigned *filter_mask/*in,out*/,
 			    H5Z_EDC_t edc_read, H5Z_cb_t cb_struct,
			    size_t *nbytes/*in,out*/, size_t *buf_size/*in,out*/,
                            void **buf/*in,out*/);
H5_DLL H5Z_class_t *H5Z_find(H5Z_filter_t id);
H5_DLL herr_t H5Z_can_apply(hid_t dcpl_id, hid_t type_id);
H5_DLL herr_t H5Z_set_local(hid_t dcpl_id, hid_t type_id);
H5_DLL H5Z_filter_info_t *H5Z_filter_info(const struct H5O_pline_t *pline,
        H5Z_filter_t filter);
H5_DLL htri_t H5Z_all_filters_avail(const struct H5O_pline_t *pline);
H5_DLL herr_t H5Z_delete(struct H5O_pline_t *pline, H5Z_filter_t filter);
H5_DLL herr_t H5Z_set_latest_version(struct H5O_pline_t *pline);

/* Data Transform Functions */
typedef struct H5Z_data_xform_t H5Z_data_xform_t; /* Defined in H5Ztrans.c */

H5_DLL H5Z_data_xform_t *H5Z_xform_create(const char *expr);
H5_DLL herr_t H5Z_xform_copy(H5Z_data_xform_t **data_xform_prop);
H5_DLL herr_t H5Z_xform_destroy(H5Z_data_xform_t *data_xform_prop);
H5_DLL herr_t H5Z_xform_eval(H5Z_data_xform_t *data_xform_prop, void* array, size_t array_size, const H5T_t *buf_type);
H5_DLL hbool_t H5Z_xform_noop(const H5Z_data_xform_t *data_xform_prop);
H5_DLL char* H5Z_xform_extract_xform_str(const H5Z_data_xform_t *data_xform_prop);

#endif
