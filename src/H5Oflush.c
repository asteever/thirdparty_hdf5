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
 * Created:     H5Oflush.c
 *              Aug 19, 2010
 *              Mike McGreevy <mamcgree@hdfgroup.org>
 *
 * Purpose:     Object flush/refresh routines.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5O_PACKAGE /* suppress error about including H5Opkg */

/***********/
/* Headers */
/***********/

#include "H5Dprivate.h"     /* Datasets */
#include "H5Eprivate.h"     /* Errors   */
#include "H5Fprivate.h"     /* Files    */
#include "H5Gprivate.h"	    /* Groups	*/
#include "H5Iprivate.h"	    /* IDs	*/
#include "H5Opkg.h"         /* Objects  */

/*************/
/* Functions */
/*************/


/*-------------------------------------------------------------------------
 * Function:	H5Oflush
 *
 * Purpose:	Flushes all buffers associated with an object to disk.
 *
 * Return:	Non-negative on success, negative on failure
 *
 * Programmer:  Mike McGreevy
 *              May 19, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Oflush(hid_t obj_id)
{
    H5O_loc_t *oloc;            /* object location */
    herr_t ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", obj_id);

    /* Check args */
    if((oloc = H5O_get_loc(obj_id)) == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an object")
 
    /* Private function */
    if (H5O_flush_metadata(oloc, H5AC_dxpl_id) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to flush object")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Oflush */


/*-------------------------------------------------------------------------
 * Function:    H5O_flush_metadata
 *
 * Purpose:     Flush any metadata associated with this object.
 *
 * Return:  Success:    Non-negative
 *          Failure:    Negative
 *
 * Programmer: Mike McGreevy
 *             May 19, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_flush_metadata(const H5O_loc_t *oloc, hid_t dxpl_id)
{
    H5O_t       *oh = NULL;             /* Object header */
    herr_t      ret_value = SUCCEED;    /* Return value */
    haddr_t tag = 0;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check args */
    HDassert(oloc);

    /* Get object header for object */
    if(NULL == (oh = H5O_protect(oloc, dxpl_id, H5AC_READ)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTPROTECT, FAIL, "unable to protect object's object header")

    /* Get object header's address (i.e. the tag value for this object) */
    if (HADDR_UNDEF == (tag = H5O_OH_GET_ADDR(oh)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "unable to get address of object header")

    /* Unprotect object header before attempting to flush it */
    if(oh && H5O_unprotect(oloc, dxpl_id, oh, H5AC__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTUNPROTECT, FAIL, "unable to release object header")

    /* Reset object header pointer */
    oh = NULL;

    /* Flush metadata based on tag value of the object */
    if (H5F_flush_tagged_metadata(oloc->file, tag, dxpl_id)<0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "unable to flush tagged metadata")

done:
    /* Unprotect object header on failure */
    if(oh && H5O_unprotect(oloc, dxpl_id, oh, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_OHDR, H5E_CANTUNPROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_flush_metadata() */


/*-------------------------------------------------------------------------
 * Function:	H5Orefresh
 *
 * Purpose:	Refreshes all buffers associated with an object.
 *
 * Return:	Non-negative on success, negative on failure
 *
 * Programmer:  Mike McGreevy
 *              July 28, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Orefresh(hid_t oid)
{
    H5O_loc_t *oloc;            /* object location */
    hid_t ret_value = SUCCEED; /* return value */
    
    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", oid);

    /* Check args */
    if((oloc = H5O_get_loc(oid)) == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an object")

    /* Private function */
    if ((H5O_refresh_metadata(oid, *oloc, H5AC_dxpl_id)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTLOAD, FAIL, "unable to refresh object")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Orefresh */


/*-------------------------------------------------------------------------
 * Function:    H5O_refresh_metadata
 *
 * Purpose:	Internal routine that refreshes all buffers associated with 
 *          an object.
 *
 * Return:  Success:    Non-negative
 *          Failure:    Negative
 *
 * Programmer: Mike McGreevy
 *             July 28, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_refresh_metadata(hid_t oid, H5O_loc_t oloc, hid_t dxpl_id)
{
    void *object = NULL;        /* Dataset for this operation */
    haddr_t tag = 0;
    H5O_t *oh = NULL;
    H5G_loc_t obj_loc;
    H5G_loc_t tmp_loc;
    H5G_name_t obj_path;
    H5O_loc_t obj_oloc;
    hid_t ret_value = SUCCEED;
    H5I_type_t type;

    FUNC_ENTER_NOAPI(FAIL)
    
    /* Get the object's object header */
    if(NULL == (oh = H5O_protect(&oloc, dxpl_id, H5AC_READ)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTPROTECT, FAIL, "unable to protect object's object header")

    /* Get object's type */
    type = H5I_get_type(oid);

    /* Make deep local copy of object's location information */
    H5G_loc(oid, &tmp_loc);
    obj_loc.oloc = &obj_oloc;
    obj_loc.path = &obj_path;
    H5G_loc_reset(&obj_loc);
    H5G_loc_copy(&obj_loc, &tmp_loc, H5_COPY_DEEP);

    /* Get object header's address (i.e. the tag value for this object) */
    if(HADDR_UNDEF == (tag = H5O_OH_GET_ADDR(oh)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "unable to get address of object header")

    /* Unprotect object header before attempting to flush it */
    if(oh && H5O_unprotect(&oloc, dxpl_id, oh, H5AC__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTUNPROTECT, FAIL, "unable to release object header")
     
    /* Reset object header pointer */
    oh = NULL;

    /* Close the object */
    if(H5I_dec_ref(oid) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to close object")

    /* Flush the object's metadata before evicting it */
    if(H5O_flush_metadata(&oloc, dxpl_id) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTFLUSH, FAIL, "unable to flush object's metadata")

    /* Evict the object's tagged metadata */
    if(H5F_evict_tagged_metadata(oloc.file, tag, dxpl_id)<0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "unable to evict metadata")

    switch (type)
    {
        case(H5I_GROUP):

            /* Re-open the group */
            if(NULL == (object = H5G_open(&obj_loc, dxpl_id)))
                HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open group")
            break;

        case(H5I_DATATYPE):

            /* Re-open the named datatype */
            if(NULL == (object = H5T_open(&obj_loc, dxpl_id)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTOPENOBJ, FAIL, "unable to open named datatype")
            break;

        case(H5I_DATASET):

            /* Re-open the dataset */
            if(NULL == (object = H5D_open(&obj_loc, H5P_DATASET_ACCESS_DEFAULT, dxpl_id)))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, FAIL, "unable to open dataset")
            break;

        case(H5I_UNINIT):
        case(H5I_BADID):
        case(H5I_FILE):
        case(H5I_DATASPACE):
        case(H5I_ATTR):
        case(H5I_REFERENCE):
        case(H5I_VFL):
        case(H5I_GENPROP_CLS):
        case(H5I_GENPROP_LST):
        case(H5I_ERROR_CLASS):
        case(H5I_ERROR_MSG):
        case(H5I_ERROR_STACK):
        case(H5I_NTYPES):
        default:
            HGOTO_ERROR(H5E_ARGS, H5E_CANTRELEASE, FAIL, "not a valid file object ID (dataset, group, or datatype)")
        break;

    } /* end switch */

    /* Re-register ID for the object */
    if((H5I_register_with_id(type, object, TRUE, oid)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to re-register object atom")

done:
    /* Unprotect object header on failure */
    if(oh && H5O_unprotect(&oloc, dxpl_id, oh, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_OHDR, H5E_CANTUNPROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value);
} /* H5O_refresh_metadata */

