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

#ifndef _H5IMprivate_H
#define _H5IMprivate_H

/* public hdf5 prototypes			*/
#include "H5Rpublic.h"	
#include "H5Apublic.h"	
#include "H5Spublic.h"	
#include "H5Dpublic.h"		

/* public LT prototypes			*/
#include "H5LTpublic.h"		
#include "H5IMpublic.h"


#define IMAGE_CLASS   "IMAGE"
#define PALETTE_CLASS "PALETTE"
#define IMAGE_VERSION "1.2"
#define IMAGE8_RANK    3
#define IMAGE24_RANK   3


/*-------------------------------------------------------------------------
 * Private functions
 *-------------------------------------------------------------------------
 */
H5_HLDLL herr_t  H5IM_find_palette(hid_t loc_id );


#endif
