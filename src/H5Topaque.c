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
 * Module Info: This module contains the functionality for opaque
 *      datatypes in the H5T interface.
 */

#define H5T_PACKAGE		/*suppress error about including H5Tpkg	  */

/* Pablo information */
/* (Put before include files to avoid problems with inline functions) */
#define PABLO_MASK	H5Topaque_mask

#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Tpkg.h"		/* Datatypes				*/

/* Interface initialization */
static int interface_initialize_g = 0;
#define INTERFACE_INIT H5T_init_opaque_interface
static herr_t H5T_init_opaque_interface(void);


/*--------------------------------------------------------------------------
NAME
   H5T_init_opaque_interface -- Initialize interface-specific information
USAGE
    herr_t H5T_init_opaque_interface()
   
RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5T_init_iterface currently).

--------------------------------------------------------------------------*/
static herr_t
H5T_init_opaque_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5T_init_opaque_interface)

    FUNC_LEAVE_NOAPI(H5T_init())
} /* H5T_init_opaque_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5Tset_tag
 *
 * Purpose:	Tag an opaque datatype with a unique ASCII identifier.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Thursday, May 20, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tset_tag(hid_t type_id, const char *tag)
{
    H5T_t	*dt=NULL;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Tset_tag, FAIL)
    H5TRACE2("e","is",type_id,tag);

    /* Check args */
    if (NULL == (dt = H5I_object_verify(type_id,H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type")
    if (H5T_STATE_TRANSIENT!=dt->state)
	HGOTO_ERROR(H5E_ARGS, H5E_CANTINIT, FAIL, "data type is read-only")
    while (dt->parent)
        dt = dt->parent; /*defer to parent*/
    if (H5T_OPAQUE!=dt->type)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an opaque data type")
    if (!tag)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no tag")

    /* Commit */
    H5MM_xfree(dt->u.opaque.tag);
    dt->u.opaque.tag = H5MM_strdup(tag);

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Tget_tag
 *
 * Purpose:	Get tha tag associated with an opaque datatype.
 *
 * Return:	A pointer to an allocated string. The caller should free
 *              the string. NULL is returned for errors.
 *
 * Programmer:	Robb Matzke
 *		Thursday, May 20, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
char *
H5Tget_tag(hid_t type_id)
{
    H5T_t	*dt=NULL;
    char	*ret_value;

    FUNC_ENTER_API(H5Tget_tag, NULL)

    /* Check args */
    if (NULL == (dt = H5I_object_verify(type_id,H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a data type")
    while (dt->parent)
        dt = dt->parent; /*defer to parent*/
    if (H5T_OPAQUE != dt->type)
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "operation not defined for data type class")
    
    /* result */
    if (NULL==(ret_value=H5MM_strdup(dt->u.opaque.tag)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

done:
    FUNC_LEAVE_API(ret_value)
}

