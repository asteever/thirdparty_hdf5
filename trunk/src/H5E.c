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
 * Purpose:	Provides error handling in the form of a stack.  The
 *		FUNC_ENTER() macro clears the error stack whenever an API
 *		function is entered.  When an error is detected, an entry is
 *		pushed onto the stack.  As the functions unwind additional
 *		entries are pushed onto the stack. The API function will
 *		return some indication that an error occurred and the
 *		application can print the error stack.
 *
 *		Certain API functions in the H5E package (such as H5Eprint())
 *		do not clear the error stack.  Otherwise, any function which
 *		doesn't have an underscore immediately after the package name
 *		will clear the error stack.  For instance, H5Fopen() clears
 *		the error stack while H5F_open() does not.
 *
 *		An error stack has a fixed maximum size.  If this size is
 *		exceeded then the stack will be truncated and only the
 *		inner-most functions will have entries on the stack. This is
 *		expected to be a rare condition.
 *
 *		Each thread has its own error stack, but since
 *		multi-threading has not been added to the library yet, this
 *		package maintains a single error stack. The error stack is
 *		statically allocated to reduce the complexity of handling
 *		errors within the H5E package.
 *
 */
#include "H5private.h"		/* Generic Functions			  */
#include "H5Iprivate.h"		/* IDs                                    */
#include "H5Eprivate.h"		/* Private error routines		  */
#include "H5MMprivate.h"	/* Memory management			  */

#define PABLO_MASK	H5E_mask

/* Interface initialization? */
static int interface_initialize_g = 0;
#define INTERFACE_INIT H5E_init_interface

/* HDF5 error class ID */
hid_t H5E_ERR_CLS_g			= FAIL;

/*
 * Predefined errors. These are initialized at runtime in H5E_init_interface()
 * in this source file.
 */
/* Include the automatically generated error code definitions */
#include "H5Edefin.h"

/* Amount to indent each error */
#define H5E_INDENT              2

#ifdef H5_HAVE_THREADSAFE
/*
 * The per-thread error stack. pthread_once() initializes a special
 * key that will be used by all threads to create a stack specific to
 * each thread individually. The association of stacks to threads will
 * be handled by the pthread library.
 *
 * In order for this macro to work, H5E_get_my_stack() must be preceeded
 * by "H5E_t *estack =".
 */
static H5E_t *    H5E_get_stack(void);
#define H5E_get_my_stack()  H5E_get_stack()
#else /* H5_HAVE_THREADSAFE */
/*
 * The current error stack.
 */
H5E_t		H5E_stack_g[1];
#define H5E_get_my_stack() (H5E_stack_g+0)
#endif /* H5_HAVE_THREADSAFE */


#ifdef H5_HAVE_PARALLEL
/*
 * variables used for MPI error reporting
 */
char	H5E_mpi_error_str[MPI_MAX_ERROR_STRING];
int	H5E_mpi_error_str_len;
#endif

/* Static function declarations */
static herr_t H5E_init_interface (void);
static H5E_cls_t *H5E_register_class(const char *cls_name, const char *lib_name, 
                                const char *version);
static herr_t  H5E_unregister_class(H5E_cls_t *cls);
static ssize_t H5E_get_class_name(const H5E_cls_t *cls, char *name, size_t size);
static int H5E_close_msg_cb(void *obj_ptr, hid_t obj_id, void *key);
static herr_t  H5E_close_msg(H5E_msg_t *err);
static H5E_msg_t *H5E_create_msg(H5E_cls_t *cls, H5E_type_t msg_type, const char *msg);
static ssize_t H5E_get_msg(const H5E_msg_t *msg_ptr, H5E_type_t *type, char *msg, size_t size);
static H5E_t  *H5E_get_current_stack(void);
static herr_t  H5E_set_current_stack(H5E_t *estack);
static herr_t  H5E_close_stack(H5E_t *err_stack);
static int     H5E_get_num(const H5E_t *err_stack);
static herr_t  H5E_pop(H5E_t *err_stack, size_t count);
static herr_t  H5E_clear_entries(H5E_t *estack, unsigned nentries);
static herr_t  H5E_print(const H5E_t *estack, FILE *stream);
static herr_t  H5E_walk (const H5E_t *estack, H5E_direction_t direction, H5E_walk_t func, 
                             void *client_data);
static herr_t  H5E_walk_cb(unsigned n, const H5E_error_t *err_desc, void *client_data);
static herr_t  H5E_get_auto(const H5E_t *estack, H5E_auto_t *func, void **client_data);
static herr_t  H5E_set_auto(H5E_t *estack, H5E_auto_t func, void *client_data);

/*--------------------------------------------------------------------------
 * Function:    H5E_init_interface
 *
 * Purpose:     Initialize interface-specific information
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Raymond Lu
 *              Friday, July 11, 2003
 *               
 *--------------------------------------------------------------------------
 */
static herr_t
H5E_init_interface(void)
{
    H5E_cls_t   *cls;           /* Pointer to error class */
    H5E_msg_t   *msg;           /* Pointer to new error message */
    char lib_vers[128];         /* Buffer to constructu library version within */
    herr_t      ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOINIT(H5E_init_interface)

    /* Initialize the atom group for the error class IDs */
    if(H5I_init_group(H5I_ERROR_CLASS, H5I_ERRCLS_HASHSIZE, H5E_RESERVED_ATOMS, 
                    (H5I_free_t)H5E_unregister_class)<0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTINIT, FAIL, "unable to initialize ID group")
    /* Initialize the atom group for the major error IDs */
    if(H5I_init_group(H5I_ERROR_MSG, H5I_ERRMSG_HASHSIZE, H5E_RESERVED_ATOMS, 
                    (H5I_free_t)H5E_close_msg)<0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTINIT, FAIL, "unable to initialize ID group")
    /* Initialize the atom group for the error stacks */
    if(H5I_init_group(H5I_ERROR_STACK, H5I_ERRSTK_HASHSIZE, H5E_RESERVED_ATOMS, 
                    (H5I_free_t)H5E_close_stack)<0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTINIT, FAIL, "unable to initialize ID group")

#ifndef H5_HAVE_THREADSAFE
    H5E_stack_g[0].nused = 0;
    H5E_stack_g[0].func = (H5E_auto_t)H5Eprint;
    H5E_stack_g[0].auto_data = NULL;
#endif /* H5_HAVE_THREADSAFE */

    /* Allocate the HDF5 error class */
    assert(H5E_ERR_CLS_g==(-1));
    HDsnprintf(lib_vers,sizeof(lib_vers),"%u.%u.%u%s",H5_VERS_MAJOR,H5_VERS_MINOR,H5_VERS_RELEASE,(HDstrlen(H5_VERS_SUBRELEASE)>0 ? "-"H5_VERS_SUBRELEASE : ""));
    if((cls = H5E_register_class(H5E_CLS_NAME, H5E_CLS_LIB_NAME, lib_vers))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "class initialization failed")
    if((H5E_ERR_CLS_g = H5I_register(H5I_ERROR_CLASS, cls))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error class")

    /* Include the automatically generated error code initialization */
    #include "H5Einit.h"

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5E_term_interface
 *
 * Purpose:	Terminates the H5E interface
 *
 * Return:	Success:	Positive if anything is done that might
 *				affect other interfaces; zero otherwise.
 *
 * 		Failure:	Negative.
 *
 * Programmer:	Raymond Lu
 *	        Tuesday, July 22, 2003	
 *
 * Modifications:
 *              
 *-------------------------------------------------------------------------
 */
int
H5E_term_interface(void)
{
    int	        ncls, nmsg, nstk, n=0;

    FUNC_ENTER_NOINIT(H5E_term_interface)

    if (interface_initialize_g) {
        /* Check if there are any open error stacks, classes or messages */
        ncls = H5I_nmembers(H5I_ERROR_CLASS);
        nmsg = H5I_nmembers(H5I_ERROR_MSG);
        nstk = H5I_nmembers(H5I_ERROR_STACK);

        n = ncls + nmsg + nstk;
        if(n>0) {
            /* Clear any outstanding error stacks */
            if (nstk>0)
	        H5I_clear_group(H5I_ERROR_STACK, FALSE);
        
            /* Clear all the error classes */
	    if (ncls>0) {
	        H5I_clear_group(H5I_ERROR_CLASS, FALSE);
                
                /* Reset the HDF5 error class, if its been closed */
                if(H5I_nmembers(H5I_ERROR_CLASS)==0)
                    H5E_ERR_CLS_g = -1;
            }
            
            /* Clear all the error messages */
	    if (nmsg>0) {
	        H5I_clear_group(H5I_ERROR_MSG, FALSE);
                
                /* Reset the HDF5 error messages, if they've been closed */
                if(H5I_nmembers(H5I_ERROR_MSG)==0) {
                    /* Include the automatically generated error code termination */
                    #include "H5Eterm.h"
                } /* end if */
            } /* end if */

	} else {
	    /* Destroy the error class, message, and stack id groups */
	    H5I_destroy_group(H5I_ERROR_STACK);
	    H5I_destroy_group(H5I_ERROR_CLASS);
	    H5I_destroy_group(H5I_ERROR_MSG);

	    /* Mark closed */
	    interface_initialize_g = 0;
	    n = 1; /*H5I*/
	}
    }

    FUNC_LEAVE_NOAPI(n)
}


#ifdef H5_HAVE_THREADSAFE
/*-------------------------------------------------------------------------
 * Function:	H5E_get_stack
 *
 * Purpose:	Support function for H5E_get_my_stack() to initialize and
 *              acquire per-thread error stack.
 *
 * Return:	Success:	error stack (H5E_t *)
 *
 *		Failure:	NULL
 *
 * Programmer:	Chee Wai LEE
 *              April 24, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5E_t *
H5E_get_stack(void)
{
    H5E_t *estack;
    H5E_t *ret_value;   /* Return value */

    FUNC_ENTER_NOAPI(H5E_get_stack,NULL)

    estack = pthread_getspecific(H5TS_errstk_key_g);

    if (!estack) {
        /* no associated value with current thread - create one */
        estack = (H5E_t *)H5MM_malloc(sizeof(H5E_t));
        estack->nused = 0;
        estack->func = H5Eprint;
        estack->auto_data = NULL;
        pthread_setspecific(H5TS_errstk_key_g, (void *)estack);
    }

    /* Set return value */
    ret_value=estack;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}
#endif  /* H5_HAVE_THREADSAFE */


/*-------------------------------------------------------------------------
 * Function:	H5Eregister_class
 *
 * Purpose:	Registers an error class.
 *
 * Return:	Non-negative value as class ID on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Eregister_class(const char *cls_name, const char *lib_name, const char *version)
{
    H5E_cls_t   *cls;        /* Pointer to error class */
    hid_t       ret_value;   /* Return value */

    FUNC_ENTER_API(H5Eregister_class, FAIL)
    H5TRACE3("i","sss",cls_name,lib_name,version);
    
    /* Check arguments */
    if (cls_name==NULL || lib_name==NULL || version==NULL)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid string")

    /* Create the new error class object */
    if((cls=H5E_register_class(cls_name, lib_name, version))==NULL)
	HGOTO_ERROR(H5E_ERROR, H5E_CANTCREATE, FAIL, "can't create error class")

    /* Register the new error class to get an ID for it */
    if((ret_value = H5I_register(H5I_ERROR_CLASS, cls))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error class")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5E_register_class
 *
 * Purpose:	Private function to register an error class.
 *
 * Return:	Non-negative value as class ID on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5E_cls_t *
H5E_register_class(const char *cls_name, const char *lib_name, const char *version)
{
    H5E_cls_t   *cls;        /* Pointer to error class */
    H5E_cls_t   *ret_value;  /* Return value */
    
    FUNC_ENTER_NOAPI(H5E_register_class, NULL)
    
    /* Check arguments */
    assert(cls_name);
    assert(lib_name);
    assert(version);

    /* Allocate space for new error class */
    if((cls = H5MM_malloc(sizeof(H5E_cls_t)))==NULL)
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Duplicate string information */
    if((cls->cls_name = HDstrdup(cls_name))==NULL)
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    if((cls->lib_name = HDstrdup(lib_name))==NULL)
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    if((cls->lib_vers = HDstrdup(version))==NULL)
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Set the return value */
    ret_value=cls;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Eunregister_class
 *
 * Purpose:	Closes an error class.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Eunregister_class(hid_t class_id)
{
    herr_t       ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_API(H5Eunregister_class, FAIL)
    H5TRACE1("e","i",class_id);
    
    /* Check arguments */
    if (H5I_ERROR_CLASS != H5I_get_type(class_id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an error class")

    /*
     * Decrement the counter on the dataset.  It will be freed if the count
     * reaches zero.
     */
    if(H5I_dec_ref(class_id)<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTDEC, FAIL, "unable to decrement ref count on error class")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5E_unregister_class
 *
 * Purpose:	Private function to close an error class.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5E_unregister_class(H5E_cls_t *cls)
{
    herr_t       ret_value = SUCCEED;   /* Return value */
    
    FUNC_ENTER_NOAPI(H5E_unregister_class, FAIL)

    /* Check arguments */
    assert(cls);

    /* Iterate over all the messages and delete those in this error class */
    /* (Ignore return value, since callback isn't designed to return a particular object) */
    (void)H5I_search(H5I_ERROR_MSG, H5E_close_msg_cb, cls);

    /* Free error class structure */
    if(cls->cls_name)    
        H5MM_xfree((void*)cls->cls_name);
    if(cls->lib_name)
        H5MM_xfree((void*)cls->lib_name);
    if(cls->lib_vers)
        H5MM_xfree((void*)cls->lib_vers);
    H5MM_xfree((void*)cls);
    
done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Eget_class_name
 *
 * Purpose:	Retrieves error class name.
 *
 * Return:      Non-negative for name length if succeeds(zero means no name);
 *              otherwise returns negative value.	
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Eget_class_name(hid_t class_id, char *name, size_t size)
{
    H5E_cls_t   *cls;        /* Pointer to error class */
    ssize_t     ret_value;   /* Return value */

    FUNC_ENTER_API(H5Eget_class_name, FAIL)
    H5TRACE3("Zs","isz",class_id,name,size);
    
    /* Get the error class */
    if(NULL==(cls = H5I_object_verify(class_id, H5I_ERROR_CLASS)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error class ID")

    /* Retrieve the class name */
    if((ret_value = H5E_get_class_name(cls, name, size))<0)
	HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get error class name")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5E_get_class_name
 *
 * Purpose:	Private function to retrieve error class name.
 * 
 * Return:      Non-negative for name length if succeeds(zero means no name);
 *              otherwise returns negative value.	
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static ssize_t
H5E_get_class_name(const H5E_cls_t *cls, char *name, size_t size)
{
    ssize_t       len;          /* Length of rror class's name */
    ssize_t       ret_value;    /* Return value */
    
    FUNC_ENTER_NOAPI(H5E_get_class_name, FAIL)

    /* Check arguments */
    assert(cls);

    /* Get the class's name */
    len = (ssize_t)HDstrlen(cls->cls_name);

    /* Set the user's buffer, if provided */
    if(name) {
       HDstrncpy(name, cls->cls_name, MIN((size_t)(len+1), size));
       if((size_t)len >= size)
          name[size-1]='\0';
    } 
    
    /* Return the full length */
    ret_value = len;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:    H5E_close_msg_cb
 *
 * Purpose:     H5I_search callback function to close error messages in the
 *              error class.
 *
 * Programmer:  Raymond Lu
 *              July 14, 2003
 *              
 * Return:	Non-negative value on success/Negative on failure
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
H5E_close_msg_cb(void *obj_ptr, hid_t obj_id, void *key)
{
    H5E_msg_t   *err_msg = (H5E_msg_t*)obj_ptr;
    H5E_cls_t   *cls = (H5E_cls_t*)key;
    herr_t      ret_value = SUCCEED;       /* Return value */
        
    FUNC_ENTER_NOAPI(H5_close_msg_cb, FAIL)
  
    /* Check arguments */
    assert(err_msg);

    /* Close the message if it is in the class being closed */
    if(err_msg->cls == cls)
        if(H5I_dec_ref(obj_id)<0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTDEC, FAIL, "unable to decrement ref count on error message")
    
done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Eclose_msg
 *
 * Purpose:	Closes a major or minor error.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Eclose_msg(hid_t err_id)
{
    herr_t       ret_value = SUCCEED;   /* Return value */
    
    FUNC_ENTER_API(H5Eclose_msg, FAIL)
    H5TRACE1("e","i",err_id);

    /* Check arguments */
    if (H5I_ERROR_MSG != H5I_get_type(err_id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an error class")

    /* Decrement the counter.  It will be freed if the count reaches zero. */
    if(H5I_dec_ref(err_id)<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTDEC, FAIL, "unable to decrement ref count on error message")

done:
    FUNC_LEAVE_API(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:	H5E_close_msg
 *
 * Purpose:	Private function to close an error messge.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5E_close_msg(H5E_msg_t *err)
{
    herr_t       ret_value = SUCCEED;   /* Return value */
    
    FUNC_ENTER_NOAPI(H5E_close_msg, FAIL)

    /* Check arguments */
    assert(err);

    if(err->msg)    
        H5MM_xfree((void*)err->msg);
    /* Don't free err->cls here */

    H5MM_xfree((void*)err);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:	H5Ecreate_msg
 *
 * Purpose:	Creates a major or minor error, returns an ID.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Ecreate_msg(hid_t class_id, H5E_type_t msg_type, const char *msg_str)
{
    H5E_cls_t   *cls;           /* Pointer to error class */
    H5E_msg_t   *msg;           /* Pointer to new error message */
    hid_t       ret_value;      /* Return value */
    
    FUNC_ENTER_API(H5Ecreate_msg, FAIL)
    
    /* Check arguments */
    if(msg_type!=H5E_MAJOR && msg_type!=H5E_MINOR)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a valid message type")
    if(msg_str==NULL)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "message is NULL")

    /* Get the error class */
    if(NULL==(cls = H5I_object_verify(class_id, H5I_ERROR_CLASS)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error class ID")

    /* Create the new error message object */
    if((msg = H5E_create_msg(cls, msg_type, msg_str))==NULL)
	HGOTO_ERROR(H5E_ERROR, H5E_CANTCREATE, FAIL, "can't create error message")

    /* Register the new error class to get an ID for it */
    if((ret_value = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")

done:
    FUNC_LEAVE_API(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:	H5E_create_msg
 *
 * Purpose:	Private function to create a major or minor error.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5E_msg_t *
H5E_create_msg(H5E_cls_t *cls, H5E_type_t msg_type, const char *msg_str)
{
    H5E_msg_t   *msg;           /* Pointer to new error message */
    H5E_msg_t   *ret_value;     /* Return value */
    
    FUNC_ENTER_NOAPI(H5E_create_msg, NULL)
   
    /* Check arguments */
    assert(cls);
    assert(msg_type==H5E_MAJOR || msg_type==H5E_MINOR);
    assert(msg_str);

    /* Allocate new message object */
    if((msg = H5MM_malloc(sizeof(H5E_msg_t)))==NULL)
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Fill new message object */
    msg->cls = cls;
    msg->type = msg_type;
    if((msg->msg = HDstrdup(msg_str))==NULL)
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Set return value */
    ret_value = msg;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Eget_msg
 *
 * Purpose:	Retrieves an error message.
 *
 * Return:      Non-negative for message length if succeeds(zero means no message);
 *              otherwise returns negative value.	
 *
 * Programmer:	Raymond Lu
 *              Friday, July 14, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Eget_msg(hid_t msg_id, H5E_type_t *type, char *msg_str, size_t size)
{
    H5E_msg_t   *msg;           /* Pointer to error message */
    ssize_t      ret_value;     /* Return value */

    FUNC_ENTER_API(H5Eget_msg, FAIL)
    
    /* Get the message object */
    if((msg = H5I_object_verify(msg_id, H5I_ERROR_MSG))==NULL)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error message ID")

    /* Get the message's text */
    if((ret_value = H5E_get_msg(msg, type, msg_str, size))<0)
	HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get error message text")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5E_get_msg
 *
 * Purpose:	Private function to retrieve an error message.
 * 
 * Return:      Non-negative for name length if succeeds(zero means no name);
 *              otherwise returns negative value.	
 *
 * Programmer:	Raymond Lu
 *              Friday, July 14, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static ssize_t
H5E_get_msg(const H5E_msg_t *msg, H5E_type_t *type, char *msg_str, size_t size)
{
    ssize_t       len;          /* Length of rror class's name */
    ssize_t       ret_value;   /* Return value */
    
    FUNC_ENTER_NOAPI(H5E_get_msg, FAIL)

    /* Check arguments */
    assert(msg);

    /* Get the length of the message string */
    len = (ssize_t)HDstrlen(msg->msg);

    /* Copy the message into the user's buffer, if given */
    if(msg_str) {
       HDstrncpy(msg_str, msg->msg, MIN((size_t)(len+1), size));
       if((size_t)len >= size)
          msg_str[size-1]='\0';
    } 
    
    /* Give the message type, if asked */
    if(type)
        *type = msg->type;

    /* Set the return value to the full length of the message */
    ret_value = len;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Eget_current_stack
 *
 * Purpose:	Registers current error stack, returns object handle for it,
 *              clears it.
 *
 * Return:	Non-negative value as stack ID on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 14, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Eget_current_stack(void)
{
    H5E_t	*stk;           /* Error stack */
    hid_t       ret_value;   /* Return value */

    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(H5Eget_current_stack, FAIL)
    H5TRACE0("i","");
    
    /* Get the current stack */
    if((stk=H5E_get_current_stack())==NULL)
	HGOTO_ERROR(H5E_ERROR, H5E_CANTCREATE, FAIL, "can't create error stack")

    /* Register the stack */
    if((ret_value = H5I_register(H5I_ERROR_STACK, stk))<0)
	HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't create error stack")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5E_get_current_stack
 *
 * Purpose:	Private function to register an error stack.
 *
 * Return:	Non-negative value as class ID on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5E_t *
H5E_get_current_stack(void)
{
    H5E_t	*current_stack; /* Pointer to the current error stack */
    H5E_t	*estack_copy=NULL;   /* Pointer to new error stack to return */
    unsigned    u;              /* Local index variable */
    H5E_t      *ret_value;   /* Return value */
    
    FUNC_ENTER_NOAPI(H5E_get_current_stack, NULL)

    /* Get a pointer to the current error stack */
    if((current_stack = H5E_get_my_stack ())==NULL) /*lint !e506 !e774 Make lint 'constant value Boolean' in non-threaded case */
	HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, NULL, "can't get current error stack")

    /* Allocate a new error stack */
    if((estack_copy = H5MM_malloc(sizeof(H5E_t)))==NULL)
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Make a copy of current error stack */
    estack_copy->nused = current_stack->nused; 
    for(u=0; u<current_stack->nused; u++) {
        H5E_error_t *current_error, *new_error; /* Pointers to errors on each stack */

        /* Get pointers into the current error stack location */
        current_error = &(current_stack->slot[u]);
        new_error = &(estack_copy->slot[u]);
       
        /* Increment the IDs to indicate that they are used in this stack */
        if(H5I_inc_ref(current_error->cls_id)<0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTINC, NULL, "unable to decrement ref count on error class")
        new_error->cls_id = current_error->cls_id;       
        if(H5I_inc_ref(current_error->maj_id)<0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTINC, NULL, "unable to decrement ref count on error message")
        new_error->maj_id = current_error->maj_id;       
        if(H5I_inc_ref(current_error->min_id)<0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTINC, NULL, "unable to decrement ref count on error message")
        new_error->min_id = current_error->min_id;       
        if((new_error->func_name = HDstrdup(current_error->func_name))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
        if((new_error->file_name = HDstrdup(current_error->file_name))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
        new_error->line = current_error->line;
        if((new_error->desc = HDstrdup(current_error->desc))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    } /* end for */
   
    /* Empty current error stack */ 
    H5E_clear(current_stack);
   
    /* Set the return value */
    ret_value = estack_copy;

done:
    if(ret_value==NULL) {
        if(estack_copy!=NULL)
            H5MM_xfree((void*)estack_copy);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Eset_current_stack
 *
 * Purpose:     Replaces current stack with specified stack.	
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 15, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Eset_current_stack(hid_t err_stack)
{
    H5E_t      *estack;
    herr_t         ret_value = SUCCEED;   /* Return value */
    
    FUNC_ENTER_API(H5Eset_current_stack, FAIL)
    H5TRACE1("e","i",err_stack);
    
    if(err_stack != H5E_DEFAULT) {
        if((estack = H5I_object_verify(err_stack, H5I_ERROR_STACK))==NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")

        /* Set the current error stack */
        if(H5E_set_current_stack(estack)<0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTSET, FAIL, "unable to set error stack")
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5E_set_current_stack
 *
 * Purpose:	Private function to replace an error stack.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 15, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5E_set_current_stack(H5E_t *estack)
{
    H5E_t	*current_stack;         /* Default error stack */
    unsigned     u;                     /* Local index variable */
    herr_t       ret_value = SUCCEED;   /* Return value */
    
    FUNC_ENTER_NOAPI(H5E_set_current_stack, FAIL)

    /* Sanity check */
    assert(estack);

    /* Get a pointer to the current error stack */
    if((current_stack = H5E_get_my_stack ())==NULL) /*lint !e506 !e774 Make lint 'constant value Boolean' in non-threaded case */
	HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get current error stack")

    /* Empty current error stack */ 
    H5E_clear(current_stack);

    /* Copy new stack to current error stack */
    current_stack->nused = estack->nused; 
    for(u=0; u<current_stack->nused; u++) {
        H5E_error_t *current_error, *new_error; /* Pointers to errors on each stack */

        /* Get pointers into the current error stack location */
        current_error = &(current_stack->slot[u]);
        new_error = &(estack->slot[u]);
       
        /* Increment the IDs to indicate that they are used in this stack */
        if(H5I_inc_ref(new_error->cls_id)<0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTINC, FAIL, "unable to decrement ref count on error class")
        current_error->cls_id = new_error->cls_id;
        if(H5I_inc_ref(new_error->maj_id)<0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTINC, FAIL, "unable to decrement ref count on error class")
        current_error->maj_id = new_error->maj_id;
        if(H5I_inc_ref(new_error->min_id)<0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTINC, FAIL, "unable to decrement ref count on error class")
        current_error->min_id = new_error->min_id;
        if((current_error->func_name = HDstrdup(new_error->func_name))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
        if((current_error->file_name = HDstrdup(new_error->file_name))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
        current_error->line = new_error->line;
        if((current_error->desc = HDstrdup(new_error->desc))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
    } /* end for */

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Eclose_stack
 *
 * Purpose:	Closes an error stack.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 14, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Eclose_stack(hid_t stack_id)
{
    herr_t       ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_API(H5Eclose_stack, FAIL)
    H5TRACE1("e","i",stack_id);

    if(H5E_DEFAULT != stack_id) {
        /*
         * Decrement the counter on the error stack.  It will be freed if the count
         * reaches zero.
         */
        if(H5I_dec_ref(stack_id)<0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTDEC, FAIL, "unable to decrement ref count on error stack")
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5E_close_stack
 *
 * Purpose:	Private function to close an error stack.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 14, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5E_close_stack(H5E_t *estack)
{
    herr_t       ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5E_close_stack, FAIL)
    
    /* Sanity check */
    assert(estack);

    /* Release the stack's error information */
    H5E_clear(estack);

    /* Free the stack structure */
    H5MM_xfree((void*)estack);
    
done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Eget_num
 *
 * Purpose:	Retrieves the number of error message.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 15, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5Eget_num(hid_t error_stack_id)
{
    H5E_t *estack;      /* Error stack to operate on */
    int ret_value;      /* Return value */

    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(H5Eget_num, FAIL)
    H5TRACE1("Is","i",error_stack_id);
   
    /* Need to check for errors */
    if(error_stack_id == H5E_DEFAULT) {
    	if((estack = H5E_get_my_stack())==NULL) /*lint !e506 !e774 Make lint 'constant value Boolean' in non-threaded case */
            HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get current error stack")
    } /* end if */
    else {
        /* Only clear the error stack if it's not the default stack */
        H5E_clear(NULL);

        /* Get the error stack to operate on */
        if((estack = H5I_object_verify(error_stack_id, H5I_ERROR_STACK))==NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")
    } /* end else */
    
    /* Get the number of errors on stack */
    if((ret_value=H5E_get_num(estack))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get number of errors")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5E_get_num
 *
 * Purpose:	Private function to retrieve number of errors in error stack.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 15, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
H5E_get_num(const H5E_t *estack)
{
    int      ret_value;   /* Return value */

    FUNC_ENTER_NOAPI(H5E_get_num, FAIL)
 
    assert(estack);   

    H5_ASSIGN_OVERFLOW(ret_value,estack->nused,size_t,int);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Epop
 *
 * Purpose:	Deletes some error messages from the top of error stack.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 16, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Epop(hid_t err_stack, size_t count)
{
    H5E_t *estack;
    herr_t       ret_value = SUCCEED;   /* Return value */

    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(H5Epop, FAIL)
    H5TRACE2("e","iz",err_stack,count);
   
    /* Need to check for errors */
    if(err_stack == H5E_DEFAULT) {
    	if((estack = H5E_get_my_stack())==NULL) /*lint !e506 !e774 Make lint 'constant value Boolean' in non-threaded case */
            HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get current error stack")
    } /* end if */
    else {
        /* Only clear the error stack if it's not the default stack */
        H5E_clear(NULL);

        /* Get the error stack to operate on */
        if((estack = H5I_object_verify(err_stack, H5I_ERROR_STACK))==NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")
    } /* end else */

    /* Range limit the number of errors to pop off stack */
    if(count > estack->nused)
        count = estack->nused;

    /* Pop the errors off the stack */
    if(H5E_pop(estack, count)<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTRELEASE, FAIL, "can't pop errors from stack")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5E_pop
 *
 * Purpose:	Private function to delete some error messages from the top
 *              of error stack.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 16, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5E_pop(H5E_t *estack, size_t count)
{
    herr_t      ret_value = SUCCEED;   /* Return value */
    
    FUNC_ENTER_NOAPI(H5E_pop, FAIL)

    /* Sanity check */
    assert(estack);
    assert(estack->nused>=count);

    /* Remove the entries from the error stack */
    if(H5E_clear_entries(estack, count)<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTRELEASE, FAIL, "can't remove errors from stack")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Epush
 *
 * Purpose:	Pushes a new error record onto error stack for the current
 *		thread.  The error has major and minor IDs MAJ_ID and
 *		MIN_ID, the name of a function where the error was detected,
 *		the name of the file where the error was detected, the
 *		line within that file, and an error description string.  The
 *		function name, file name, and error description strings must
 *		be statically allocated.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Monday, October 18, 1999
 *
 * Notes: 	Basically a public API wrapper around the H5E_push function.
 *
 * Modifications:
 *              Raymond Lu
 *              Tuesday, July 15, 2003
 *
 *              Added the ID of the error stack to which the error is pushed
 *              on.  The error message can be appended more message in the
 *              same control format as printf and fprintf.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Epush(hid_t err_stack, const char *file, const char *func, unsigned line, 
        hid_t cls_id, hid_t maj_id, hid_t min_id, const char *fmt, ...)
{
    va_list     ap;             /* Varargs info */
    H5E_t       *estack;        /* Pointer to error stack to modify */
    H5E_msg_t   *maj_ptr, *min_ptr;     /* Pointer to major and minor error info */
    char        tmp[H5E_LEN];   /* Buffer to place formatted description in */
    herr_t	ret_value=SUCCEED;      /* Return value */

    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(H5Epush, FAIL)
    H5TRACE7("e","issIuiis",err_stack,file,func,line,maj_id,min_id,fmt);
    
    /* Need to check for errors */
    if(err_stack == H5E_DEFAULT)
    	estack = NULL;
    else {
        /* Only clear the error stack if it's not the default stack */
        H5E_clear(NULL);

        /* Get the error stack to operate on */
        if((estack = H5I_object_verify(err_stack, H5I_ERROR_STACK))==NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")
    } /* end else */
    
    /* Check for mis-matches in major & minor error classes */
    if((maj_ptr = H5I_object_verify(maj_id, H5I_ERROR_MSG))==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error message ID")
    if((min_ptr = H5I_object_verify(min_id, H5I_ERROR_MSG))==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error message ID")
    if(maj_ptr->cls != min_ptr->cls)
        HGOTO_ERROR(H5E_ARGS, H5E_UNSUPPORTED, FAIL, "major and minor errors not from same error class")

    /* Format the description */
    va_start(ap, fmt);
    HDvsnprintf(tmp, H5E_LEN, fmt, ap);
    va_end(ap);

    /* Push the error on the stack */
    if(H5E_push(estack, file, func, line, cls_id, maj_id, min_id, tmp)<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTSET, FAIL, "can't push error on stack")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5E_push
 *
 * Purpose:	Pushes a new error record onto error stack for the current
 *		thread.  The error has major and minor IDs MAJ_ID and
 *		MIN_ID, the name of a function where the error was detected,
 *		the name of the file where the error was detected, the
 *		line within that file, and an error description string.  The
 *		function name, file name, and error description strings must
 *		be statically allocated (the FUNC_ENTER() macro takes care of
 *		the function name and file name automatically, but the
 *		programmer is responsible for the description string).
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Friday, December 12, 1997
 *
 * Modifications: 
 *              Raymond Lu
 *              Tuesday, July 15, 2003
 *               
 *              Added the ID of the error stack to which the error is pushed
 *              on.  The error message can be appended more message in the
 *              same control format as printf and fprintf.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5E_push(H5E_t *estack, const char *file, const char *func, unsigned line, 
        hid_t cls_id, hid_t maj_id, hid_t min_id, const char *desc)
{
    herr_t	ret_value=SUCCEED;      /* Return value */

    /*
     * WARNING: We cannot call HERROR() from within this function or else we
     *		could enter infinite recursion.  Furthermore, we also cannot
     *		call any other HDF5 macro or function which might call
     *		HERROR().  HERROR() is called by HRETURN_ERROR() which could
     *		be called by FUNC_ENTER().
     */
    FUNC_ENTER_NOINIT(H5E_push)

    /* Sanity check */
    assert(cls_id>0);
    assert(maj_id>0);
    assert(min_id>0);

    /* Check for 'default' error stack */
    if(estack==NULL)
    	if((estack = H5E_get_my_stack())==NULL) /*lint !e506 !e774 Make lint 'constant value Boolean' in non-threaded case */
            HGOTO_DONE(FAIL)

    /*
     * Don't fail if arguments are bad.  Instead, substitute some default
     * value.
     */
    if (!func) func = "Unknown_Function";
    if (!file) file = "Unknown_File";
    if (!desc) desc = "No description given";

    /*
     * Push the error if there's room.  Otherwise just forget it.
     */
    assert (estack);

    if (estack->nused<H5E_NSLOTS) {
        /* Increment the IDs to indicate that they are used in this stack */
        if(H5I_inc_ref(cls_id)<0)
            HGOTO_DONE(FAIL)
	estack->slot[estack->nused].cls_id = cls_id;
        if(H5I_inc_ref(maj_id)<0)
            HGOTO_DONE(FAIL)
	estack->slot[estack->nused].maj_id = maj_id;
        if(H5I_inc_ref(min_id)<0)
            HGOTO_DONE(FAIL)
	estack->slot[estack->nused].min_id = min_id;
	if((estack->slot[estack->nused].func_name = HDstrdup(func))==NULL)
            HGOTO_DONE(FAIL)
	if((estack->slot[estack->nused].file_name = HDstrdup(file))==NULL)
            HGOTO_DONE(FAIL)
	estack->slot[estack->nused].line = line;
	if((estack->slot[estack->nused].desc = HDstrdup(desc))==NULL)
            HGOTO_DONE(FAIL)
	estack->nused++;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Eclear
 *
 * Purpose:	Clears the error stack for the specified error stack.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Wednesday, July 16, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Eclear(hid_t err_stack)
{
    H5E_t   *estack;            /* Error stack to operate on */
    herr_t ret_value=SUCCEED;   /* Return value */
    
    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(H5Eclear, FAIL)
    H5TRACE1("e","i",err_stack);

    /* Need to check for errors */
    if(err_stack == H5E_DEFAULT)
    	estack = NULL;
    else {
        /* Only clear the error stack if it's not the default stack */
        H5E_clear(NULL);

        if((estack = H5I_object_verify(err_stack, H5I_ERROR_STACK))==NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")
    } /* end else */
 
    /* Clear the error stack */
    if(H5E_clear(estack)<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTSET, FAIL, "can't clear error stack")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5E_clear_entries
 *
 * Purpose:	Private function to clear the error stack entries for the
 *              specified error stack.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, August 6, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5E_clear_entries(H5E_t *estack, unsigned nentries)
{
    H5E_error_t         *error; /* Pointer to error stack entry to clear */
    unsigned u;                 /* Local index variable */
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5E_clear_entries, FAIL)

    /* Sanity check */
    assert(estack);
    assert(estack->nused>=nentries);

    /* Empty the error stack from the top down */ 
    for(u=0; nentries>0; nentries--,u++) {
        error = &(estack->slot[estack->nused-(u+1)]);

        /* Decrement the IDs to indicate that they are no longer used by this stack */
        /* (In reverse order that they were incremented, so that reference counts work well) */
        if(H5I_dec_ref(error->min_id)<0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTDEC, FAIL, "unable to decrement ref count on error message")
        if(H5I_dec_ref(error->maj_id)<0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTDEC, FAIL, "unable to decrement ref count on error message")
        if(H5I_dec_ref(error->cls_id)<0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTDEC, FAIL, "unable to decrement ref count on error class")

        /* Release strings */
        if(error->func_name)
            H5MM_xfree((void*)error->func_name);
        if(error->file_name)
            H5MM_xfree((void*)error->file_name);
        if(error->desc)
            H5MM_xfree((void*)error->desc);
    } /* end for */

    /* Decrement number of errors on stack */
    estack->nused-=u;
    
done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5E_clear
 *
 * Purpose:	Private function to clear the error stack for the
 *              specified error stack.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Wednesday, July 16, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5E_clear(H5E_t *estack)
{
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5E_clear, FAIL)

    /* Check for 'default' error stack */
    if(estack==NULL)
    	if((estack = H5E_get_my_stack())==NULL) /*lint !e506 !e774 Make lint 'constant value Boolean' in non-threaded case */
            HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get current error stack")

    /* Empty the error stack */ 
    assert(estack);
    if(estack->nused)
        if(H5E_clear_entries(estack, estack->nused)<0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTSET, FAIL, "can't clear error stack")
    
done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Eprint
 *
 * Purpose:	Prints the error stack in some default way.  This is just a
 *		convenience function for H5Ewalk() with a function that
 *		prints error messages.  Users are encouraged to write there
 *		own more specific error handlers.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Friday, February 27, 1998
 *
 * Modifications:
 *	Albert Cheng, 2000/12/02
 *	Show MPI process rank id if applicable.
 *	Albert Cheng, 2001/07/14
 *	Show HDF5 library version information string too.
 *
 *      Raymond Lu, 2003/7/16
 *      Print for specified error stack.  A line will appear before the error
 *      messages of each error class.  It states the information of library
 *      name, version number and thread ID.
 *      
 *-------------------------------------------------------------------------
 */
herr_t
H5Eprint(hid_t err_stack, FILE *stream)
{
    H5E_t   *estack;            /* Error stack to operate on */
    herr_t ret_value=SUCCEED;   /* Return value */
    
    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(H5Eprint, FAIL)
    /*NO TRACE*/

    /* Need to check for errors */
    if(err_stack == H5E_DEFAULT) {
    	if((estack = H5E_get_my_stack())==NULL) /*lint !e506 !e774 Make lint 'constant value Boolean' in non-threaded case */
            HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get current error stack")
    } /* end if */
    else {
        /* Only clear the error stack if it's not the default stack */
        H5E_clear(NULL);

        if((estack = H5I_object_verify(err_stack, H5I_ERROR_STACK))==NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")
    } /* end else */
 
    /* Print error stack */
    if(H5E_print(estack, stream)<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTLIST, FAIL, "can't display error stack")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5E_print
 *
 * Purpose:	Private function to print the error stack in some default 
 *              way.  This is just a convenience function for H5Ewalk() 
 *              with a function that prints error messages.  Users are 
 *              encouraged to write there own more specific error handlers.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Friday, February 27, 1998
 *
 * Modifications:
 *	Albert Cheng, 2000/12/02
 *	Show MPI process rank id if applicable.
 *	Albert Cheng, 2001/07/14
 *	Show HDF5 library version information string too.
 *
 *      Raymond Lu, 2003/7/16
 *      Print for specified error stack.  A line will appear before the error
 *      messages of each error class.  It states the information of library
 *      name, version number and thread ID.
 *      
 *-------------------------------------------------------------------------
 */
static herr_t
H5E_print(const H5E_t *estack, FILE *stream)
{
    H5E_print_t eprint;         /* Callback information to pass to H5E_walk_cb() */
    herr_t	ret_value = SUCCEED;

    /* Don't clear the error stack! :-) */
    FUNC_ENTER_NOAPI(H5E_print, FAIL)
    
    /* Sanity check */
    assert(estack);

    /* If no stream was given, use stderr */
    if (!stream) 
        eprint.stream = stderr;
    else
        eprint.stream = stream;

    /* Reset the original error class information */
    HDmemset(&eprint.cls,0,sizeof(H5E_cls_t));
    
    /* Walk the error stack */
    if(H5E_walk (estack, H5E_WALK_DOWNWARD, H5E_walk_cb, (void*)&eprint)<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTLIST, FAIL, "can't walk error stack")
  
done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Ewalk
 *
 * Purpose:	Walks the error stack for the current thread and calls some
 *		function for each error along the way.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Friday, February 27, 1998
 *
 * Modifications:
 *
 *              Raymond Lu
 *              Wednesday, July 16, 2003
 *              Let it walk through specified error stack.
 *              
 *-------------------------------------------------------------------------
 */
herr_t
H5Ewalk(hid_t err_stack, H5E_direction_t direction, H5E_walk_t func, void *client_data)
{
    H5E_t   *estack;            /* Error stack to operate on */
    herr_t	ret_value=SUCCEED;      /* Return value */

    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(H5Ewalk, FAIL)
    /*NO TRACE*/

    /* Need to check for errors */
    if(err_stack == H5E_DEFAULT) {
    	if((estack = H5E_get_my_stack())==NULL) /*lint !e506 !e774 Make lint 'constant value Boolean' in non-threaded case */
            HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get current error stack")
    } /* end if */
    else {
        /* Only clear the error stack if it's not the default stack */
        H5E_clear(NULL);

        if((estack = H5I_object_verify(err_stack, H5I_ERROR_STACK))==NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")
    } /* end else */

    /* Walk the error stack */
    if(H5E_walk (estack, direction, func, client_data)<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTLIST, FAIL, "can't walk error stack")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5E_walk
 *
 * Purpose:	Private function for H5Ewalk.
 *              Walks the error stack, calling the specified function for
 *		each error on the stack.  The DIRECTION argument determines
 *		whether the stack is walked from the inside out or the
 *		outside in.  The value H5E_WALK_UPWARD means begin with the
 *		most specific error and end at the API; H5E_WALK_DOWNWARD
 *		means to start at the API and end at the inner-most function
 *		where the error was first detected.
 *
 *		The function pointed to by FUNC will be called for each error
 *		in the error stack. It's arguments will include an index
 *		number (beginning at zero regardless of stack traversal
 *		direction), an error stack entry, and the CLIENT_DATA pointer
 *		passed to H5E_print.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Friday, December 12, 1997
 *
 * Modifications:
 *
 *              Raymond Lu
 *              Wednesday, July 16, 2003
 *              Let it walk through specified error stack.
 *              
 *-------------------------------------------------------------------------
 */
static herr_t
H5E_walk (const H5E_t *estack, H5E_direction_t direction, H5E_walk_t func, void *client_data)
{
    int		i;              /* Local index variable */
    herr_t	status;         /* Status from callback function */
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5E_walk, FAIL)

    /* Sanity check */
    assert (estack);

    /* check args, but rather than failing use some default value */
    if (direction!=H5E_WALK_UPWARD && direction!=H5E_WALK_DOWNWARD)
	direction = H5E_WALK_UPWARD;

    /* Walk the stack if a callback function was given */
    if(func) {
        status=SUCCEED;
        if (H5E_WALK_UPWARD==direction) {
            for (i=0; i<(int)estack->nused && status>=0; i++)
                status = (func)((unsigned)i, estack->slot+i, client_data);
        } else {
            H5_CHECK_OVERFLOW(estack->nused-1,size_t,int);
            for (i=(int)(estack->nused-1); i>=0 && status>=0; i--)
                status = (func)(estack->nused-(size_t)(i+1), estack->slot+i, client_data);
        }
        if(status<0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTLIST, FAIL, "can't walk error stack")
    } /* end if */
    
done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5E_walk_cb
 *
 * Purpose:	This is a default error stack traversal callback function
 *		that prints error messages to the specified output stream.
 *		It is not meant to be called directly but rather as an
 *		argument to the H5Ewalk() function.  This function is called
 *		also by H5Eprint().  Application writers are encouraged to
 *		use this function as a model for their own error stack
 *		walking functions.
 *
 *		N is a counter for how many times this function has been
 *		called for this particular traversal of the stack.  It always
 *		begins at zero for the first error on the stack (either the
 *		top or bottom error, or even both, depending on the traversal
 *		direction and the size of the stack).
 *
 *		ERR_DESC is an error description.  It contains all the
 *		information about a particular error.
 *
 *		CLIENT_DATA is the same pointer that was passed as the
 *		CLIENT_DATA argument of H5Ewalk().  It is expected to be a
 *		file pointer (or stderr if null).
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Friday, December 12, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5E_walk_cb(unsigned n, const H5E_error_t *err_desc, void *client_data)
{
    H5E_print_t         *eprint  = (H5E_print_t *)client_data;
    FILE		*stream;        /* I/O stream to print output to */
    H5E_cls_t           *cls_ptr;       /* Pointer to error class */
    H5E_msg_t           *maj_ptr;       /* Pointer to major error info */
    H5E_msg_t           *min_ptr;       /* Pointer to minor error info */
    const char		*maj_str = "No major description";      /* Major error description */
    const char		*min_str = "No minor description";      /* Minor error description */
    unsigned            have_desc=1;    /* Flag to indicate whether the error has a "real" description */

    FUNC_ENTER_NOINIT(H5E_walk_cb)

    /* Check arguments */
    assert (err_desc);

    /* If no client data was passed, output to stderr */
    if (!client_data) stream = stderr;
    else stream = eprint->stream;
    
    /* Get descriptions for the major and minor error numbers */
    maj_ptr = H5I_object_verify(err_desc->maj_id, H5I_ERROR_MSG);
    min_ptr = H5I_object_verify(err_desc->min_id, H5I_ERROR_MSG);
    assert(maj_ptr && min_ptr);
    if(maj_ptr->msg)
        maj_str = maj_ptr->msg;
    if(min_ptr->msg)
        min_str = min_ptr->msg;

    /* Get error class info */
    cls_ptr = maj_ptr->cls;
    
    /* Print error class header if new class */
    if(eprint->cls.lib_name==NULL || HDstrcmp(cls_ptr->lib_name, eprint->cls.lib_name)) {
        /* update to the new class information */
        if(cls_ptr->cls_name) eprint->cls.cls_name = cls_ptr->cls_name;
        if(cls_ptr->lib_name) eprint->cls.lib_name = cls_ptr->lib_name;
        if(cls_ptr->lib_vers) eprint->cls.lib_vers = cls_ptr->lib_vers;

        fprintf (stream, "%s-DIAG: Error detected in %s (%s) ", cls_ptr->cls_name, cls_ptr->lib_name, cls_ptr->lib_vers);
        
        /* try show the process or thread id in multiple processes cases*/
#ifdef H5_HAVE_PARALLEL
        {   int mpi_rank, mpi_initialized;
	    MPI_Initialized(&mpi_initialized);
	    if (mpi_initialized){
	        MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);
	        fprintf (stream, "MPI-process %d", mpi_rank);
	    }else
	        fprintf (stream, "thread 0");
        }
#elif defined(H5_HAVE_THREADSAFE)
        fprintf (stream, "thread %d", (int)pthread_self());
#else
        fprintf (stream, "thread 0");
#endif
        fprintf (stream, ":\n");
    }

    /* Check for "real" error description - used to format output more nicely */
    if(err_desc->desc==NULL || HDstrlen(err_desc->desc)==0)
        have_desc=0;

    /* Print error message */
    fprintf (stream, "%*s#%03u: %s line %u in %s()%s%s\n",
	     H5E_INDENT, "", n, err_desc->file_name, err_desc->line,
	     err_desc->func_name, (have_desc ? ": " : ""),
             (err_desc->desc ? err_desc->desc : ""));
    fprintf (stream, "%*smajor: %s\n", H5E_INDENT*2, "", maj_str);
    fprintf (stream, "%*sminor: %s\n", H5E_INDENT*2, "", min_str);

    FUNC_LEAVE_NOAPI(SUCCEED)
}


/*-------------------------------------------------------------------------
 * Function:	H5Eget_auto
 *
 * Purpose:	Returns the current settings for the automatic error stack
 *		traversal function and its data for specific error stack.  
 *		Either (or both) arguments may be null in which case the 
 *		value is not returned.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Saturday, February 28, 1998
 *
 * Modifications:
 *              Raymond Lu
 *              July 18, 2003
 *              Added error stack in the parameters.  It returns the 
 *              traversal function and data for that error stack.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Eget_auto(hid_t estack_id, H5E_auto_t *func, void **client_data)
{
    H5E_t   *estack;            /* Error stack to operate on */
    herr_t ret_value=SUCCEED;   /* Return value */
    
    FUNC_ENTER_API(H5Eget_auto, FAIL)
    H5TRACE3("e","i*x*x",estack_id,func,client_data);
    
    if(estack_id == H5E_DEFAULT) {
    	if((estack = H5E_get_my_stack())==NULL) /*lint !e506 !e774 Make lint 'constant value Boolean' in non-threaded case */
            HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get current error stack")
    } /* end if */
    else
        if((estack = H5I_object_verify(estack_id, H5I_ERROR_STACK))==NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")

    /* Get the automatic error reporting information */
    if(H5E_get_auto(estack, func, client_data)<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get automatic error info")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5E_get_auto
 *
 * Purpose:	Private function to return the current settings for the 
 *              automatic error stack traversal function and its data 
 *              for specific error stack. Either (or both) arguments may 
 *              be null in which case the value is not returned.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              July 18, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5E_get_auto(const H5E_t *estack, H5E_auto_t *func, void **client_data)
{
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5E_get_auto, FAIL)

    assert (estack);
        
    /* Retrieve the requested information */
    if(func) *func = estack->func;
    if(client_data) *client_data = estack->auto_data;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Eset_auto
 *
 * Purpose:	Turns on or off automatic printing of errors for certain 
 *              error stack.  When turned on (non-null FUNC pointer) any 
 *              API function which returns an error indication will first
 *              call FUNC passing it CLIENT_DATA as an argument.
 *
 *		The default values before this function is called are
 *		H5Eprint() with client data being the standard error stream,
 *		stderr.
 *
 *		Automatic stack traversal is always in the H5E_WALK_DOWNWARD
 *		direction.
 *		
 * See Also:	H5Ewalk()
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Friday, February 27, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Eset_auto(hid_t estack_id, H5E_auto_t func, void *client_data)
{
    H5E_t   *estack;            /* Error stack to operate on */
    herr_t ret_value=SUCCEED;   /* Return value */
    
    FUNC_ENTER_API(H5Eset_auto, FAIL)
    H5TRACE3("e","ixx",estack_id,func,client_data);

    if(estack_id == H5E_DEFAULT) {
    	if((estack = H5E_get_my_stack())==NULL) /*lint !e506 !e774 Make lint 'constant value Boolean' in non-threaded case */
            HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get current error stack")
    } /* end if */
    else
        if((estack = H5I_object_verify(estack_id, H5I_ERROR_STACK))==NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")
    
    /* Set the automatic error reporting information */
    if(H5E_set_auto(estack, func, client_data)<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTSET, FAIL, "can't set automatic error info")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5E_set_auto
 *
 * Purpose:	Private function to turn on or off automatic printing of 
 *              errors for certain error stack.  When turned on (non-null 
 *              FUNC pointer) any API function which returns an error 
 *              indication will first call FUNC passing it CLIENT_DATA 
 *              as an argument.
 *
 *		The default values before this function is called are
 *		H5Eprint() with client data being the standard error stream,
 *		stderr.
 *
 *		Automatic stack traversal is always in the H5E_WALK_DOWNWARD
 *		direction.
 *		
 * See Also:	H5Ewalk()
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Friday, February 27, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5E_set_auto(H5E_t *estack, H5E_auto_t func, void *client_data)
{
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5E_set_auto, FAIL)

    assert(estack);

    /* Set the automatic error reporting info */
    estack->func = func;
    estack->auto_data = client_data;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5E_dump_api_stack
 *
 * Purpose:	Private function to dump the error stack during an error in
 *              an API function if a callback function is defined for the
 *              current error stack.
 *		
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, August 6, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5E_dump_api_stack(int is_api)
{
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5E_dump_api_stack, FAIL)

    /* Only dump the error stack during an API call */
    if(is_api) {
        H5E_t *estack = H5E_get_my_stack();

        assert(estack);
        if (estack->func)
            (void)((estack->func)(H5E_DEFAULT, estack->auto_data));
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
}
