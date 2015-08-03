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

/****************/
/* Module Setup */
/****************/

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5V_init_interface


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5ESprivate.h"        /* Event Stacks                         */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Lprivate.h"		/* Links        		  	*/
#include "H5VMprivate.h"	/* Memory management			*/
#include "H5Vprivate.h" 	/* Views				*/
#include "H5VLprivate.h"	/* VOL plugins				*/
#include "H5VLiod_client.h"	/* IOD VOL plugin			*/

#ifdef H5_HAVE_EFF

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


/*******************/
/* Local Variables */
/*******************/


/* VIEW ID class */
static const H5I_class_t H5I_VIEW_CLS[1] = {{
    H5I_VIEW,                   /* ID class value */
    0,                          /* Class flags */
    0,                          /* # of reserved IDs for class */
    (H5I_free_t)H5V_close       /* Callback routine for closing objects of this class */
}};


/*-------------------------------------------------------------------------
 * Function:	H5V_init
 *
 * Purpose:	Initialize the interface from some other package.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              July 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5V_init(void)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)
    /* FUNC_ENTER() does all the work */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5V_init() */


/*--------------------------------------------------------------------------
NAME
   H5V_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5V_init_interface()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
static herr_t
H5V_init_interface(void)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /*
     * Create attribute ID type.
     */
    if(H5I_register_type(H5I_VIEW_CLS) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTINIT, FAIL, "unable to initialize interface")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5V_init_interface() */


/*--------------------------------------------------------------------------
 NAME
    H5V_term_interface
 PURPOSE
    Terminate various H5V objects
 USAGE
    void H5V_term_interface()
 RETURNS
 DESCRIPTION
    Release any other resources allocated.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
int
H5V_term_interface(void)
{
    int	n = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(H5_interface_initialize_g) {
	if(H5I_nmembers(H5I_VIEW) > 0) {
	    (void)H5I_clear_type(H5I_VIEW, FALSE, FALSE);
            n++; /*H5I*/
	} /* end if */
        else {
            /* Destroy the dataspace object id group */
	    (void)H5I_dec_type_ref(H5I_VIEW);
            n++; /*H5I*/

	    /* Shut down interface */
	    H5_interface_initialize_g = 0;
	} /* end else */
    } /* end if */

    FUNC_LEAVE_NOAPI(n)
} /* H5V_term_interface() */

/*-------------------------------------------------------------------------
 * Function:    H5Pset_view_elmt_scope
 *
 * Purpose:     Sets the view creation property for the scope of 
 *              constructing an element region.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              February, 2014
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_view_elmt_scope(hid_t vcpl_id, hid_t space_id)
{
    H5P_genplist_t *plist;
    H5S_t *space = NULL, *new_space = NULL;
    hid_t new_space_id;
    herr_t ret_value = SUCCEED;	/* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ii", vcpl_id, space_id);

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(vcpl_id, H5P_VIEW_CREATE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a view create property list");

    /* Get the current view create property list for the region scope */
    if(H5P_get(plist, H5V_CRT_ELMT_SCOPE_NAME, &space_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get dataspace")

    /* Close the current dataspace if set */
    if((space_id != -1) && (H5I_dec_ref(space_id) < 0))
	HGOTO_ERROR(H5E_PLIST, H5E_CANTRELEASE, FAIL, "unable to close atom for dataspace")

    if(NULL == (space = (H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
	HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dataspace")

    /* Copy */
    if(NULL == (new_space = H5S_copy(space, FALSE, TRUE)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to copy dataspace")

    /* Atomize */
    if((new_space_id = H5I_register (H5I_DATASPACE, new_space, TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register dataspace atom")

    /* Set the view create property list for the region scope */
    if(H5P_set(plist, H5V_CRT_ELMT_SCOPE_NAME, &new_space_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set dataspace scope for vcpl")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_view_elmt_scope() */


/*-------------------------------------------------------------------------
 * Function:    H5Pget_view_elmt_scope
 *
 * Purpose:     Gets the view creation property for the scope of 
 *              constructing an element region.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              February, 2014
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_view_elmt_scope(hid_t vcpl_id, hid_t *_space_id)
{
    H5P_genplist_t *plist; /* Property list pointer */
    hid_t space_id;
    herr_t ret_value=SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*i", vcpl_id, _space_id);

    if(NULL == _space_id)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid return pointer")

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(vcpl_id, H5P_VIEW_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    if(H5P_get(plist, H5V_CRT_ELMT_SCOPE_NAME, &space_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get dataspace scope for view")

    if(space_id >= 0) {
        H5S_t *space = NULL, *new_space = NULL;

        if(NULL == (space = (H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
            HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dataspace")

        /* Copy */
        if(NULL == (new_space = H5S_copy(space, FALSE, TRUE)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to copy dataspace")

        /* Atomize */
        if((space_id = H5I_register (H5I_DATASPACE, new_space, TRUE)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register dataspace atom")
    }

    *_space_id = space_id;

done:
    FUNC_LEAVE_API(ret_value);
} /* end H5Pget_view_elmt_scope() */


/*-------------------------------------------------------------------------
 * Function:	H5Vcreate_ff
 *
 * Purpose:     The H5Vcreate routine creates a new view object from a
 *              query object. The view looks under loc_id and applies 
 *              the query there.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              February 2014
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Vcreate_ff(hid_t loc_id, hid_t query_id, hid_t vcpl_id, hid_t rcxt_id, hid_t estack_id)
{
    H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
    void    **req = NULL;       /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    void    *view = NULL;       /* pointer to view object created */
    H5VL_object_t *obj = NULL;        /* object token of loc_id */
    hid_t ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE5("i", "iiiii", loc_id, query_id, vcpl_id, rcxt_id, estack_id);

    /* Get correct property list */
    if(H5P_DEFAULT == vcpl_id)
        vcpl_id = H5P_VIEW_CREATE_DEFAULT;
    else
        if(TRUE != H5P_isa_class(vcpl_id, H5P_VIEW_CREATE))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not view creation property list")

    /* get the object */
    if(NULL == (obj = H5VL_get_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid object/file identifier")

    if(obj->vol_info->vol_cls->value != H5_VOL_IOD)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "only IOD plugin supports VIEW objects for now")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = obj->vol_info->vol_cls;
    }

    /* call the IOD specific private routine to create a view object */
    if(NULL == (view = H5VL_iod_view_create(obj, query_id, vcpl_id, rcxt_id, req)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create view")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

    /* Get an atom for the attribute */
    if((ret_value = H5VL_register_id(H5I_VIEW, view, obj->vol_info, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize view handle")

done:
    if (ret_value < 0 && view) {
        if(H5V_close (view) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "unable to release view")
    }
    FUNC_LEAVE_API(ret_value)
} /* end H5Vcreate_ff */


/*-------------------------------------------------------------------------
 * Function:	H5Vget_query
 *
 * Purpose:     Returns the query used to construct the view. 
 *              Returned query must be closed with H5Qclose().
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              February 2014
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Vget_query(hid_t view_id, hid_t *query_id)
{
    H5VL_object_t *obj = NULL;
    H5VL_iod_view_t *view = NULL;
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*i", view_id, query_id);

    /* Check args */
    if(NULL == query_id)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid return pointer")
    if(NULL == (obj = (H5VL_object_t *)H5I_object_verify(view_id, H5I_VIEW)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an attribute")

    view = (H5VL_iod_view_t *)obj->vol_obj;

    if(H5I_inc_ref(view->query_id, TRUE) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, FAIL, "can't increment ID ref count");

    *query_id = view->query_id;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Vget_query */


/*-------------------------------------------------------------------------
 * Function:	H5Vget_counts
 *
 * Purpose:     Returns the query used to construct the view. 
 *              Returned query must be closed with H5Qclose().
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              February 2014
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Vget_counts(hid_t view_id, hsize_t *attr_count, hsize_t *obj_count, hsize_t *elem_region_count)
{
    H5VL_object_t *obj = NULL;
    H5VL_iod_view_t *view = NULL;
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE4("e", "i*h*h*h", view_id, attr_count, obj_count, elem_region_count);

    /* Check args */
    if(NULL == (obj = (H5VL_object_t *)H5I_object_verify(view_id, H5I_VIEW)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an attribute")

    view = (H5VL_iod_view_t *)obj->vol_obj;

    if(view->common.request) {
        if(H5VL_IOD_PENDING == view->common.request->state) {
            if(H5VL_iod_request_wait(view->common.file, view->common.request) < 0)
                HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, FAIL, "can't wait on operation")
        }
    }

    if(NULL != attr_count)
        *attr_count = view->remote_view.attr_info.count;
    if(NULL != obj_count)
        *obj_count = view->remote_view.obj_info.count;
    if(NULL != elem_region_count)
        *elem_region_count = view->remote_view.region_info.count;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Vget_counts */


/*-------------------------------------------------------------------------
 * Function:	H5Vget_location_ff
 *
 * Purpose:     Returns the root location where the view was constructed on. 
 *              Musr be closed with H5Oclose (or corresponding object close op).
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              February 2014
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Vget_location_ff(hid_t view_id, hid_t *loc_id, hid_t estack_id)
{
    H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5I_type_t opened_type;
    H5VL_object_t *obj = NULL; 
    void  *opened_obj = NULL;
    H5VL_iod_view_t *view = NULL;
    H5TR_t tr;
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "i*ii", view_id, loc_id, estack_id);

    if(NULL == loc_id)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid return pointer")
    if(NULL == (obj = (H5VL_object_t *)H5I_object_verify(view_id, H5I_VIEW)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an attribute")

    view = (H5VL_iod_view_t *)obj->vol_obj;

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = obj->vol_info->vol_cls;
    }

    tr.file = view->common.file;
    tr.trans_num = view->c_version;
    tr.req_info.request = NULL;
    tr.req_info.head = NULL;
    tr.req_info.tail = NULL;
    tr.req_info.num_req = 0;

   if(NULL == (opened_obj = H5VL_iod_obj_open_token(view->loc_info.buf, 
                                                    &tr, &opened_type, req)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to open object");

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

    /* create hid_t for opened object */
    if((*loc_id = H5VL_register_id(opened_type, opened_obj, obj->vol_info, TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize object handle")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Vget_location_ff */


/*-------------------------------------------------------------------------
 * Function:	H5Vget_elem_regions_ff
 *
 * Purpose:     Returns the root location where the view was constructed on. 
 *              Musr be closed with H5Oclose (or corresponding object close op).
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              February 2014
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Vget_elem_regions_ff(hid_t view_id, hsize_t start, hsize_t count, hid_t dataset_id[], 
                       hid_t dataspace_id[], hid_t estack_id)
{
    H5VL_object_t *obj = NULL;
    H5VL_iod_view_t *view = NULL;
    hsize_t i, k = 0;
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE6("e", "ihh*i*ii", view_id, start, count, dataset_id, dataspace_id,
             estack_id);

    /* Check args */
    if(NULL == (obj = (H5VL_object_t *)H5I_object_verify(view_id, H5I_VIEW)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an attribute")

    view = (H5VL_iod_view_t *)obj->vol_obj;

    if(view->common.request) {
        if(H5VL_IOD_PENDING == view->common.request->state) {
            if(H5VL_iod_request_wait(view->common.file, view->common.request) < 0)
                HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, FAIL, "can't wait on operation");
        }
    }

    if(start >= view->remote_view.region_info.count || start+count > view->remote_view.region_info.count)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "start/count out of range")

    for(i=start; i<count; i++) {
        H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
        void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
        H5I_type_t opened_type;
        void  *opened_obj = NULL;
        H5TR_t tr;

        if(estack_id != H5_EVENT_STACK_NULL) {
            /* create the private request */
            if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
            req = &request->req;
            request->vol_cls = obj->vol_info->vol_cls;
        }

        tr.file = view->common.file;
        tr.trans_num = view->c_version;
        tr.req_info.request = NULL;
        tr.req_info.head = NULL;
        tr.req_info.tail = NULL;
        tr.req_info.num_req = 0;

        if(NULL == (opened_obj = H5VL_iod_obj_open_token(view->remote_view.region_info.tokens[i].buf, 
                                                         &tr, &opened_type, req)))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to open object");

        HDassert(H5I_DATASET == opened_type);

        if(request && *req)
            if(H5ES_insert(estack_id, request) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

        /* create hid_t for opened object */
        if((dataset_id[k] = H5VL_register_id(opened_type, opened_obj, obj->vol_info, TRUE)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize object handle")

        if(H5I_inc_ref(view->remote_view.region_info.regions[i], TRUE) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, FAIL, "can't increment ID ref count");        
        dataspace_id[k] = view->remote_view.region_info.regions[i];

        k++;
    }
done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Vget_elem_regions_ff */

herr_t
H5Vget_objs_ff(hid_t view_id, hsize_t start, hsize_t count, hid_t obj_id[], hid_t estack_id)
{
    H5VL_object_t *obj = NULL;
    H5VL_iod_view_t *view = NULL;
    hsize_t i, k = 0;
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE5("e", "ihh*ii", view_id, start, count, obj_id, estack_id);

    /* Check args */
    if(NULL == (obj = (H5VL_object_t *)H5I_object_verify(view_id, H5I_VIEW)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an attribute")

    view = (H5VL_iod_view_t *)obj->vol_obj;

    if(view->common.request) {
        if(H5VL_IOD_PENDING == view->common.request->state) {
            if(H5VL_iod_request_wait(view->common.file, view->common.request) < 0)
                HGOTO_ERROR(H5E_SYM,  H5E_CANTGET, FAIL, "can't wait on operation");
        }
    }

    if(start >= view->remote_view.obj_info.count || start+count > view->remote_view.obj_info.count)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "start/count out of range")

    for(i=start; i<count; i++) {
        H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
        void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
        H5I_type_t opened_type;
        void  *opened_obj = NULL;
        H5TR_t tr;

        if(estack_id != H5_EVENT_STACK_NULL) {
            /* create the private request */
            if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
            req = &request->req;
            request->vol_cls = obj->vol_info->vol_cls;
        }

        tr.file = view->common.file;
        tr.trans_num = view->c_version;
        tr.req_info.request = NULL;
        tr.req_info.head = NULL;
        tr.req_info.tail = NULL;
        tr.req_info.num_req = 0;

        if(NULL == (opened_obj = H5VL_iod_obj_open_token(view->remote_view.obj_info.tokens[i].buf, 
                                                         &tr, &opened_type, req)))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to open object");

        if(request && *req)
            if(H5ES_insert(estack_id, request) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

        /* create hid_t for opened object */
        if((obj_id[k] = H5VL_register_id(opened_type, opened_obj, obj->vol_info, TRUE)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize object handle");

        k++;
    }
done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Vget_objs_ff */

herr_t
H5Vget_attrs_ff(hid_t view_id, hsize_t start, hsize_t count, hid_t attr_id[], hid_t estack_id)
{
    H5VL_object_t *obj = NULL;
    H5VL_iod_view_t *view = NULL;
    hsize_t i, k = 0;
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE5("e", "ihh*ii", view_id, start, count, attr_id, estack_id);

    /* Check args */
    if(NULL == (obj = (H5VL_object_t *)H5I_object_verify(view_id, H5I_VIEW)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an attribute")

    view = (H5VL_iod_view_t *)obj->vol_obj;

    if(view->common.request) {
        if(H5VL_IOD_PENDING == view->common.request->state) {
            if(H5VL_iod_request_wait(view->common.file, view->common.request) < 0)
                HGOTO_ERROR(H5E_SYM,  H5E_CANTGET, FAIL, "can't wait on operation");
        }
    }

    if(start >= view->remote_view.attr_info.count || start+count > view->remote_view.attr_info.count)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "start/count out of range")

    for(i=start; i<count; i++) {
        H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
        void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
        H5I_type_t opened_type;
        void  *opened_obj = NULL;
        H5TR_t tr;

        if(estack_id != H5_EVENT_STACK_NULL) {
            /* create the private request */
            if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
            req = &request->req;
            request->vol_cls = obj->vol_info->vol_cls;
        }

        tr.file = view->common.file;
        tr.trans_num = view->c_version;
        tr.req_info.request = NULL;
        tr.req_info.head = NULL;
        tr.req_info.tail = NULL;
        tr.req_info.num_req = 0;

        if(NULL == (opened_obj = H5VL_iod_obj_open_token(view->remote_view.attr_info.tokens[i].buf, 
                                                         &tr, &opened_type, req)))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to open object");

        HDassert(H5I_ATTR == opened_type);

        if(request && *req)
            if(H5ES_insert(estack_id, request) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

        /* create hid_t for opened object */
        if((attr_id[k] = H5VL_register_id(opened_type, opened_obj, obj->vol_info, TRUE)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize object handle");

        k++;
    }
done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Vget_attrs_ff */


/*-------------------------------------------------------------------------
 * Function:	H5Vclose
 *
 * Purpose: 
 *      The H5Vclose routine terminates access to a view, given by
 *      view_id.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              February 2014
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Vclose(hid_t view_id)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", view_id);

    /* Check args */
    if(NULL == H5I_object_verify(view_id,H5I_VIEW))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an view ID")

    if(H5I_dec_app_ref(view_id) < 0)
    	HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "unable to close view")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Vclose_ff */


/*-------------------------------------------------------------------------
 * Function:	H5V_close
 *
 * Purpose:	Closes the specified view.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *		February 2014
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5V_close(void *_view)
{
    H5VL_object_t *view = (H5VL_object_t *)_view;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if((ret_value = H5VL_iod_view_close((H5VL_iod_view_t *)view->vol_obj)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "unable to close view")

    /* free view */
    if(H5VL_free_object(view) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTDEC, FAIL, "unable to free VOL object")

done:
    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5V_close() */

#endif /* H5_HAVE_EFF */
