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
 * Created:		H5Pmcpl.c
 *			January 27, 2014
 *			Mohamad Chaarawi
 *
 * Purpose:		Map creation property list class routines
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/
#define H5P_PACKAGE		/*suppress error about including H5Ppkg	  */

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Ppkg.h"		/* Property lists		  	*/


/****************/
/* Local Macros */
/****************/

/* ========= Map Creation properties ============ */


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* Property class callbacks */
static herr_t H5P__mcrt_reg_prop(H5P_genclass_t *pclass);


/*********************/
/* Package Variables */
/*********************/

/* Map creation property list class library initialization object */
const H5P_libclass_t H5P_CLS_MCRT[1] = {{
    "map create",		/* Class name for debugging     */
    H5P_TYPE_MAP_CREATE,      /* Class type                   */
    &H5P_CLS_OBJECT_CREATE_g,            /* Parent class                 */
    &H5P_CLS_MAP_CREATE_g,    /* Pointer to class             */
    &H5P_CLS_MAP_CREATE_ID_g,	/* Pointer to class ID          */
    &H5P_LST_MAP_CREATE_ID_g,	/* Pointer to default property list ID */
    H5P__mcrt_reg_prop,		/* Default property registration routine */
    NULL,		        /* Class creation callback      */
    NULL,		        /* Class creation callback info */
    NULL,			/* Class copy callback          */
    NULL,		        /* Class copy callback info     */
    NULL,			/* Class close callback         */
    NULL 		        /* Class close callback info    */
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:    H5P__mcrt_reg_prop
 *
 * Purpose:     Initialize the map creation property list class
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2014
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__mcrt_reg_prop(H5P_genclass_t *pclass)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P__mcrt_reg_prop() */
