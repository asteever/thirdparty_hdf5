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

/* Programmer:  Raymond Lu <songyulu@hdfgroup.org>
 *              13 February 2013
 */

#ifndef _H5PLprivate_H
#define _H5PLprivate_H

/* Include package's public header */
#include "H5PLpublic.h"

/* Private headers needed by this file */
#include "H5private.h"          /* Generic Functions                    */

#ifdef H5_HAVE_WIN32_API
/* The following two defines must be before any windows headers are included */
#define WIN32_LEAN_AND_MEAN    /* Exclude rarely-used stuff from Windows headers */
#define NOGDI                  /* Exclude Graphic Display Interface macros */

#ifdef H5_HAVE_WINSOCK2_H
#include <winsock2.h>
#endif

#include <windows.h>
#include <direct.h>         /* For _getcwd() */
#endif /*H5_HAVE_WIN32_API*/

#ifdef H5_HAVE_DLFCN_H
#include <dlfcn.h>
#endif

#ifndef H5_HAVE_WIN32_API
#include <dirent.h>
#endif

/**************************/
/* Library Private Macros */
/**************************/


/****************************/
/* Library Private Typedefs */
/****************************/


/*****************************/
/* Library-private Variables */
/*****************************/


/***************************************/
/* Library-private Function Prototypes */
/***************************************/

/* Internal API routines */
H5_DLL void* H5PL_load(H5PL_type_t plugin_type, int type_id);

#endif
