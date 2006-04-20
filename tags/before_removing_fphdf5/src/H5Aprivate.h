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

/*
 * This file contains private information about the H5D module
 */
#ifndef _H5Aprivate_H
#define _H5Aprivate_H

/* Include package's public header */
#include "H5Apublic.h"

/* Private headers needed by this file */
#include "H5Gprivate.h"		/* Groups				*/

/* Forward references of package typedefs */
typedef struct H5A_t H5A_t;

/* Attribute creation properties */
#define H5A_CHAR_ENCODING_NAME  "character_encoding"
#define H5A_CHAR_ENCODING_SIZE  sizeof(H5T_cset_t)
#define H5A_CHAR_ENCODING_DEF   H5F_CRT_DEFAULT_CSET

/* Library private functions in package */
H5_DLL struct H5O_loc_t *H5A_oloc(H5A_t *attr);
H5_DLL H5G_name_t *H5A_nameof(H5A_t *attr);

#endif
