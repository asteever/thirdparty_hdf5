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
 * This file contains private information about the H5RC module
 */
#ifndef _H5RCprivate_H
#define _H5RCprivate_H

/**************************************/
/* Public headers needed by this file */
/**************************************/
#ifdef LATER
#include "H5RCpublic.h"
#endif /* LATER */

/***************************************/
/* Private headers needed by this file */
/***************************************/
#include "H5private.h"

/************/
/* Typedefs */
/************/

/* Typedef for function to release object when reference count drops to zero */
typedef herr_t (*H5RC_free_func_t)(void *o);

/* Typedef for reference counted objects */
typedef struct H5RC_t {
    void *o;            /* Object to be reference counted */
    size_t n;           /* Reference count of number of pointers sharing object */
    H5RC_free_func_t free_func; /* Function to free object */
} H5RC_t;

/**********/
/* Macros */
/**********/
#define H5RC_INC(rc)            ((rc)->n++)
#define H5RC_DEC(rc)            (H5RC_decr(rc))
#define H5RC_GET_OBJ(rc)        ((rc)->o)

/********************/
/* Private routines */
/********************/
H5_DLL H5RC_t *H5RC_create(void *s, H5RC_free_func_t free_func);
H5_DLL herr_t H5RC_decr(H5RC_t *rc);

#endif /* _H5RSprivate_H */


