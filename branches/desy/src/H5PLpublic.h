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

/*
 * Programmer:  Raymond Lu <songyulu@hdfgroup.org>
 *              13 February 2013
 */
#ifndef _H5PLpublic_H
#define _H5PLpublic_H

/* Public headers needed by this file */
#include "H5public.h"

/****************************/
/* Library Public Typedefs */
/****************************/

/* Plugin type */
typedef enum H5PL_type_t {
    H5PL_TYPE_ERROR        = -1,  /*error                    */
    H5PL_TYPE_FILTER       = 0,   /*filter                   */
    H5PL_TYPE_VFD          = 1,   /*virtual file driver      */
    H5PL_TYPE_NONE         = 2    /*this must be last!       */
} H5PL_type_t;


#ifdef __cplusplus
extern "C" {
#endif

#ifdef __cplusplus
}
#endif
#endif /* _H5PLpublic_H */

