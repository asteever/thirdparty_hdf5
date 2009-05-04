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
 * Created:		H5HLint.c
 *			Apr 14 2009
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Core internal routines for High-Level interfaces.
 *
 *-------------------------------------------------------------------------
 */

/**********************/
/* Module Declaration */
/**********************/


/***********************/
/* Other Packages Used */
/***********************/


/***********/
/* Headers */
/***********/
#include "H5HLprivate2.h"       /* High-level library internal header file */


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


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/

/* HL-HDF5 API initialized variable */
hbool_t H5HL_libinit_g = FALSE;

/* HL-HDF5 API Entered variable */
hbool_t H5HL_api_entered_g = FALSE;


/*******************/
/* Local Variables */
/*******************/


/*-------------------------------------------------------------------------
 * Function:	H5HL_init_library
 *
 * Purpose:	Initialize high-level library scoped things.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 14, 2009
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HL_init_library(void)
{
    /* Perform any hfg-level library scoped initialization */
  
  char lib_str[256];
  herr_t status;

  /* open the library */
  status = H5open();
    
  /* register the error class */

  sprintf(lib_str, "%d.%d.%d",H5_VERS_MAJOR, H5_VERS_MINOR, H5_VERS_RELEASE);
    
  /* H5LT error class */

  H5HL_ERR_CLS_g = H5Eregister_class("H5LT", "HDF5:LT", lib_str);

  return SUCCEED;
} /* end H5HL_init_library() */

/*-------------------------------------------------------------------------
 * Function:	H5HL_close
 *
 * Purpose:	Free high-level library scoped things.
 *
 * Return:	
 *
 * Programmer:	M. Scot Breitenfeld
 *              May 4, 2009
 *
 *-------------------------------------------------------------------------
 */
void H5HL_close(void) {    
  H5Eunregister_class(H5HL_ERR_CLS_g);
    }
