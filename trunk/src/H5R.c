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

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5R_init_interface


#include "H5private.h"		/* Generic Functions			*/
#include "H5Dprivate.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"         /* File access				*/
#include "H5Gprivate.h"		/* Groups				*/
#include "H5HGprivate.h"	/* Global Heaps				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Rprivate.h"		/* References				*/
#include "H5Sprivate.h"		/* Dataspaces 				*/

/* Local macro definitions */

/* Number of reserved IDs in ID group */
#define H5R_RESERVED_ATOMS  0

/* Static functions */
static herr_t H5R_create(void *ref, H5G_loc_t *loc, const char *name,
        H5R_type_t ref_type, H5S_t *space, hid_t dxpl_id);
static hid_t H5R_dereference(H5F_t *file, hid_t dxpl_id, H5R_type_t ref_type, const void *_ref);
static H5S_t * H5R_get_region(H5F_t *file, hid_t dxpl_id, const void *_ref);
static H5G_obj_t H5R_get_obj_type(H5F_t *file, hid_t dxpl_id, H5R_type_t ref_type, const void *_ref);
static ssize_t H5R_get_name(H5F_t *file, hid_t dxpl_id, hid_t id,
    H5R_type_t ref_type, const void *_ref, char *name, size_t size);


/*--------------------------------------------------------------------------
NAME
   H5R_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5R_init_interface()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
static herr_t
H5R_init_interface(void)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5R_init_interface);

    /* Initialize the atom group for the file IDs */
    if(H5I_register_type(H5I_REFERENCE, (size_t)H5I_REFID_HASHSIZE, H5R_RESERVED_ATOMS, (H5I_free_t)NULL) < 0)
	HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable to initialize interface");

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*--------------------------------------------------------------------------
 NAME
    H5R_term_interface
 PURPOSE
    Terminate various H5R objects
 USAGE
    void H5R_term_interface()
 RETURNS
    void
 DESCRIPTION
    Release the atom group and any other resources allocated.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
int
H5R_term_interface(void)
{
    int	n=0;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5R_term_interface);

    if (H5_interface_initialize_g) {
	if ((n=H5I_nmembers(H5I_REFERENCE))) {
	    H5I_clear_type(H5I_REFERENCE, FALSE);
	} else {
	    H5I_dec_type_ref(H5I_REFERENCE);
	    H5_interface_initialize_g = 0;
	    n = 1; /*H5I*/
	}
    }

    FUNC_LEAVE_NOAPI(n);
}


/*--------------------------------------------------------------------------
 NAME
    H5R_create
 PURPOSE
    Creates a particular kind of reference for the user
 USAGE
    herr_t H5R_create(ref, loc, name, ref_type, space)
        void *ref;          OUT: Reference created
        H5G_loc_t *loc;     IN: File location used to locate object pointed to
        const char *name;   IN: Name of object at location LOC_ID of object
                                    pointed to
        H5R_type_t ref_type;    IN: Type of reference to create
        H5S_t *space;       IN: Dataspace ID with selection, used for Dataset
                                    Region references.

 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Creates a particular type of reference specified with REF_TYPE, in the
    space pointed to by REF.  The LOC_ID and NAME are used to locate the object
    pointed to and the SPACE_ID is used to choose the region pointed to (for
    Dataset Region references).
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5R_create(void *_ref, H5G_loc_t *loc, const char *name, H5R_type_t ref_type, H5S_t *space, hid_t dxpl_id)
{
    H5G_loc_t	obj_loc;		/* Group hier. location of object */
    H5G_name_t  path;            	/* Object group hier. path */
    H5O_loc_t   oloc;            	/* Object object location */
    hbool_t     obj_found = FALSE;      /* Object location found */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5R_create)

    HDassert(_ref);
    HDassert(loc);
    HDassert(name);
    HDassert(ref_type > H5R_BADTYPE || ref_type < H5R_MAXTYPE);

    /* Set up object location to fill in */
    obj_loc.oloc = &oloc;
    obj_loc.path = &path;
    H5G_loc_reset(&obj_loc);

    /* Find the object */
    if(H5G_loc_find(loc, name, &obj_loc, H5P_DEFAULT, dxpl_id) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_NOTFOUND, FAIL, "object not found")
    obj_found = TRUE;

    switch(ref_type) {
        case H5R_OBJECT:
        {
            hobj_ref_t *ref = (hobj_ref_t *)_ref; /* Get pointer to correct type of reference struct */

            *ref = obj_loc.oloc->addr;
            break;
        }

        case H5R_DATASET_REGION:
        {
            H5HG_t hobjid;      /* Heap object ID */
            hdset_reg_ref_t *ref = (hdset_reg_ref_t *)_ref; /* Get pointer to correct type of reference struct */
            hssize_t buf_size;  /* Size of buffer needed to serialize selection */
            uint8_t *p;       /* Pointer to OID to store */
            uint8_t *buf;     /* Buffer to store serialized selection in */
            unsigned heapid_found;  /* Flag for non-zero heap ID found */
            unsigned u;        /* local index */

            /* Set up information for dataset region */

            /* Return any previous heap block to the free list if we are garbage collecting */
            if(H5F_GC_REF(loc->oloc->file)) {
                /* Check for an existing heap ID in the reference */
                for(u = 0, heapid_found = 0, p = (uint8_t *)ref; u < H5R_DSET_REG_REF_BUF_SIZE; u++)
                    if(p[u] != 0) {
                        heapid_found = 1;
                        break;
                    } /* end if */

                if(heapid_found != 0) {
/* Return heap block to free list */
                } /* end if */
            } /* end if */

            /* Zero the heap ID out, may leak heap space if user is re-using reference and doesn't have garbage collection on */
            HDmemset(ref, 0, H5R_DSET_REG_REF_BUF_SIZE);

            /* Get the amount of space required to serialize the selection */
            if((buf_size = H5S_SELECT_SERIAL_SIZE(space)) < 0)
                HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "Invalid amount of space for serializing selection")

            /* Increase buffer size to allow for the dataset OID */
            buf_size += sizeof(haddr_t);

            /* Allocate the space to store the serialized information */
            H5_CHECK_OVERFLOW(buf_size, hssize_t, size_t);
            if(NULL == (buf = H5MM_malloc((size_t)buf_size)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

            /* Serialize information for dataset OID into heap buffer */
            p = (uint8_t *)buf;
            H5F_addr_encode(loc->oloc->file, &p, obj_loc.oloc->addr);

            /* Serialize the selection into heap buffer */
            if(H5S_SELECT_SERIALIZE(space, p) < 0)
                HGOTO_ERROR(H5E_REFERENCE, H5E_CANTCOPY, FAIL, "Unable to serialize selection")

            /* Save the serialized buffer for later */
            H5_CHECK_OVERFLOW(buf_size, hssize_t, size_t);
            if(H5HG_insert(loc->oloc->file, dxpl_id, (size_t)buf_size, buf, &hobjid) < 0)
                HGOTO_ERROR(H5E_REFERENCE, H5E_WRITEERROR, FAIL, "Unable to serialize selection")

            /* Serialize the heap ID and index for storage in the file */
            p = (uint8_t *)ref;
            H5F_addr_encode(loc->oloc->file, &p, hobjid.addr);
            INT32ENCODE(p, hobjid.idx);

            /* Free the buffer we serialized data in */
            H5MM_xfree(buf);
            break;
        }

        case H5R_INTERNAL:
            HGOTO_ERROR(H5E_REFERENCE, H5E_UNSUPPORTED, FAIL, "Internal references are not yet supported")

        case H5R_BADTYPE:
        case H5R_MAXTYPE:
        default:
            assert("unknown reference type" && 0);
            HGOTO_ERROR(H5E_REFERENCE, H5E_UNSUPPORTED, FAIL, "internal error (unknown reference type)")
    } /* end switch */

done:
    if(obj_found)
        H5G_loc_free(&obj_loc);

    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5R_create() */


/*--------------------------------------------------------------------------
 NAME
    H5Rcreate
 PURPOSE
    Creates a particular kind of reference for the user
 USAGE
    herr_t H5Rcreate(ref, loc_id, name, ref_type, space_id)
        void *ref;          OUT: Reference created
        hid_t loc_id;       IN: Location ID used to locate object pointed to
        const char *name;   IN: Name of object at location LOC_ID of object
                                    pointed to
        H5R_type_t ref_type;    IN: Type of reference to create
        hid_t space_id;     IN: Dataspace ID with selection, used for Dataset
                                    Region references.

 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Creates a particular type of reference specified with REF_TYPE, in the
    space pointed to by REF.  The LOC_ID and NAME are used to locate the object
    pointed to and the SPACE_ID is used to choose the region pointed to (for
    Dataset Region references).
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5Rcreate(void *ref, hid_t loc_id, const char *name, H5R_type_t ref_type, hid_t space_id)
{
    H5G_loc_t   loc;            /* File location */
    H5S_t	*space = NULL;          /* Pointer to dataspace containing region */
    herr_t      ret_value;      /* Return value */

    FUNC_ENTER_API(H5Rcreate, FAIL)
    H5TRACE5("e", "*xi*sRti", ref, loc_id, name, ref_type, space_id);

    /* Check args */
    if(ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")
    if(H5G_loc(loc_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name given")
    if(ref_type <= H5R_BADTYPE || ref_type >= H5R_MAXTYPE)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type")
    if(ref_type != H5R_OBJECT && ref_type != H5R_DATASET_REGION)
        HGOTO_ERROR(H5E_ARGS, H5E_UNSUPPORTED, FAIL, "reference type not supported")
    if(space_id != (-1) && (NULL == (space = H5I_object_verify(space_id, H5I_DATASPACE))))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace")

    /* Create reference */
    if((ret_value = H5R_create(ref, &loc, name, ref_type, space, H5AC_dxpl_id)) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable to create reference")

done:
    FUNC_LEAVE_API(ret_value)
}   /* end H5Rcreate() */


/*--------------------------------------------------------------------------
 NAME
    H5R_dereference
 PURPOSE
    Opens the HDF5 object referenced.
 USAGE
    hid_t H5R_dereference(ref)
        H5F_t *file;        IN: File the object being dereferenced is within
        H5R_type_t ref_type;    IN: Type of reference
        void *ref;          IN: Reference to open.

 RETURNS
    Valid ID on success, Negative on failure
 DESCRIPTION
    Given a reference to some object, open that object and return an ID for
    that object.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    Currently only set up to work with references to datasets
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static hid_t
H5R_dereference(H5F_t *file, hid_t dxpl_id, H5R_type_t ref_type, const void *_ref)
{
    H5O_loc_t oloc;             /* Object location */
    H5G_name_t path;            /* Path of object */
    H5G_loc_t loc;              /* Group location */
    H5O_type_t obj_type;        /* Type of object */
    hid_t ret_value;

    FUNC_ENTER_NOAPI_NOINIT(H5R_dereference)

    HDassert(_ref);
    HDassert(ref_type > H5R_BADTYPE || ref_type < H5R_MAXTYPE);
    HDassert(file);

    /* Initialize the object location */
    H5O_loc_reset(&oloc);
    oloc.file = file;

    switch(ref_type) {
        case H5R_OBJECT:
            oloc.addr = *(const hobj_ref_t *)_ref; /* Only object references currently supported */
            break;

        case H5R_DATASET_REGION:
        {
            const hdset_reg_ref_t *ref = (const hdset_reg_ref_t *)_ref; /* Get pointer to correct type of reference struct */
            H5HG_t hobjid;  /* Heap object ID */
            uint8_t *buf;   /* Buffer to store serialized selection in */
            const uint8_t *p;           /* Pointer to OID to store */

            /* Get the heap ID for the dataset region */
            p = (const uint8_t *)ref;
            H5F_addr_decode(oloc.file, &p, &(hobjid.addr));
            INT32DECODE(p, hobjid.idx);

            /* Get the dataset region from the heap (allocate inside routine) */
            if((buf = H5HG_read(oloc.file, dxpl_id, &hobjid, NULL, NULL)) == NULL)
                HGOTO_ERROR(H5E_REFERENCE, H5E_READERROR, FAIL, "Unable to read dataset region information")

            /* Get the object oid for the dataset */
            p = buf;
            H5F_addr_decode(oloc.file, &p, &(oloc.addr));

            /* Free the buffer allocated in H5HG_read() */
            H5MM_xfree(buf);
        } /* end case */
        break;

        case H5R_INTERNAL:
            HGOTO_ERROR(H5E_REFERENCE, H5E_UNSUPPORTED, FAIL, "Internal references are not yet supported")

        case H5R_BADTYPE:
        case H5R_MAXTYPE:
        default:
            HDassert("unknown reference type" && 0);
            HGOTO_ERROR(H5E_REFERENCE, H5E_UNSUPPORTED, FAIL, "internal error (unknown reference type)")
    } /* end switch */

    /* Check to make certain that this object hasn't been deleted since the reference was created */
    if(H5O_link(&oloc, 0, dxpl_id) <= 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_LINKCOUNT, FAIL, "dereferencing deleted object")

    /* Construct a group location for opening the object */
    H5G_name_reset(&path);
    loc.oloc = &oloc;
    loc.path = &path;

    /* Get the type of the object */
    if(H5O_obj_type(&oloc, &obj_type, dxpl_id) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTGET, FAIL, "unable to get object type")

    /* Open the object */
    switch(obj_type) {
        case H5O_TYPE_GROUP:
            {
                H5G_t *group;               /* Pointer to group to open */

                if((group = H5G_open(&loc, dxpl_id)) == NULL)
                    HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "not found")

                /* Create an atom for the group */
                if((ret_value = H5I_register(H5I_GROUP, group)) < 0) {
                    H5G_close(group);
                    HGOTO_ERROR(H5E_SYM, H5E_CANTREGISTER, FAIL, "can't register group")
                } /* end if */
            } /* end case */
            break;

        case H5O_TYPE_NAMED_DATATYPE:
            {
                H5T_t *type;                /* Pointer to datatype to open */

                if((type = H5T_open(&loc, dxpl_id)) == NULL)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_NOTFOUND, FAIL, "not found")

                /* Create an atom for the datatype */
                if((ret_value = H5I_register(H5I_DATATYPE, type)) < 0) {
                    H5T_close(type);
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL, "can't register datatype")
                } /* end if */
            } /* end case */
            break;

        case H5O_TYPE_DATASET:
            {
                H5D_t *dset;                /* Pointer to dataset to open */

                /* Open the dataset */
                if((dset = H5D_open(&loc, dxpl_id)) == NULL)
                    HGOTO_ERROR(H5E_DATASET, H5E_NOTFOUND, FAIL, "not found")

                /* Create an atom for the dataset */
                if((ret_value = H5I_register(H5I_DATASET, dset)) < 0) {
                    H5D_close(dset);
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTREGISTER, FAIL, "can't register dataset")
                } /* end if */
            } /* end case */
            break;

        default:
            HGOTO_ERROR(H5E_REFERENCE, H5E_BADTYPE, FAIL, "can't identify type of object referenced")
     } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5R_dereference() */


/*--------------------------------------------------------------------------
 NAME
    H5Rdereference
 PURPOSE
    Opens the HDF5 object referenced.
 USAGE
    hid_t H5Rdereference(ref)
        hid_t id;       IN: Dataset reference object is in or location ID of
                            object that the dataset is located within.
        H5R_type_t ref_type;    IN: Type of reference to create
        void *ref;      IN: Reference to open.

 RETURNS
    Valid ID on success, Negative on failure
 DESCRIPTION
    Given a reference to some object, open that object and return an ID for
    that object.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hid_t
H5Rdereference(hid_t id, H5R_type_t ref_type, const void *_ref)
{
    H5G_loc_t loc;      /* Group location */
    H5F_t *file = NULL; /* File object */
    hid_t ret_value;

    FUNC_ENTER_API(H5Rdereference, FAIL)
    H5TRACE3("i", "iRt*x", id, ref_type, _ref);

    /* Check args */
    if(H5G_loc(id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(ref_type <= H5R_BADTYPE || ref_type >= H5R_MAXTYPE)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type")
    if(_ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")

    /* Get the file pointer from the entry */
    file = loc.oloc->file;

    /* Create reference */
    if((ret_value = H5R_dereference(file, H5AC_dxpl_id, ref_type, _ref)) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable dereference object")

done:
    FUNC_LEAVE_API(ret_value)
}   /* end H5Rdereference() */


/*--------------------------------------------------------------------------
 NAME
    H5R_get_region
 PURPOSE
    Retrieves a dataspace with the region pointed to selected.
 USAGE
    H5S_t *H5R_get_region(file, ref_type, ref)
        H5F_t *file;        IN: File the object being dereferenced is within
        void *ref;          IN: Reference to open.

 RETURNS
    Pointer to the dataspace on success, NULL on failure
 DESCRIPTION
    Given a reference to some object, creates a copy of the dataset pointed
    to's dataspace and defines a selection in the copy which is the region
    pointed to.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static H5S_t *
H5R_get_region(H5F_t *file, hid_t dxpl_id, const void *_ref)
{
    H5O_loc_t oloc;             /* Object location */
    const uint8_t *p;           /* Pointer to OID to store */
    const hdset_reg_ref_t *ref = (const hdset_reg_ref_t *)_ref; /* Get pointer to correct type of reference struct */
    H5HG_t hobjid;              /* Heap object ID */
    uint8_t *buf = NULL;        /* Buffer to store serialized selection in */
    H5S_t *ret_value;

    FUNC_ENTER_NOAPI_NOINIT(H5R_get_region)

    HDassert(_ref);
    HDassert(file);

    /* Initialize the object location */
    H5O_loc_reset(&oloc);
    oloc.file = file;

    /* Get the heap ID for the dataset region */
    p = (const uint8_t *)ref;
    H5F_addr_decode(oloc.file, &p, &(hobjid.addr));
    INT32DECODE(p, hobjid.idx);

    /* Get the dataset region from the heap (allocate inside routine) */
    if((buf = H5HG_read(oloc.file, dxpl_id, &hobjid, NULL, NULL)) == NULL)
        HGOTO_ERROR(H5E_REFERENCE, H5E_READERROR, NULL, "Unable to read dataset region information")

    /* Get the object oid for the dataset */
    p = buf;
    H5F_addr_decode(oloc.file, &p, &(oloc.addr));

    /* Open and copy the dataset's dataspace */
    if((ret_value = H5S_read(&oloc, dxpl_id)) == NULL)
        HGOTO_ERROR(H5E_DATASPACE, H5E_NOTFOUND, NULL, "not found")

    /* Unserialize the selection */
    if(H5S_select_deserialize(ret_value, p) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTDECODE, NULL, "can't deserialize selection")

done:
    /* Free the buffer allocated in H5HG_read() */
    if(buf)
        H5MM_xfree(buf);

    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5R_get_region() */


/*--------------------------------------------------------------------------
 NAME
    H5Rget_region
 PURPOSE
    Retrieves a dataspace with the region pointed to selected.
 USAGE
    hid_t H5Rget_region(id, ref_type, ref)
        hid_t id;       IN: Dataset reference object is in or location ID of
                            object that the dataset is located within.
        H5R_type_t ref_type;    IN: Type of reference to get region of
        void *ref;        IN: Reference to open.

 RETURNS
    Valid ID on success, Negative on failure
 DESCRIPTION
    Given a reference to some object, creates a copy of the dataset pointed
    to's dataspace and defines a selection in the copy which is the region
    pointed to.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hid_t
H5Rget_region(hid_t id, H5R_type_t ref_type, const void *ref)
{
    H5G_loc_t loc;          /* Object's group location */
    H5S_t *space = NULL;    /* Dataspace object */
    hid_t ret_value;

    FUNC_ENTER_API(H5Rget_region, FAIL)
    H5TRACE3("i", "iRt*x", id, ref_type, ref);

    /* Check args */
    if(H5G_loc(id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(ref_type != H5R_DATASET_REGION)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type")
    if(ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")

    /* Get the dataspace with the correct region selected */
    if((space = H5R_get_region(loc.oloc->file, H5AC_ind_dxpl_id, ref)) == NULL)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTCREATE, FAIL, "unable to create dataspace")

    /* Atomize */
    if((ret_value = H5I_register (H5I_DATASPACE, space)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register dataspace atom")

done:
    FUNC_LEAVE_API(ret_value)
}   /* end H5Rget_region() */


/*--------------------------------------------------------------------------
 NAME
    H5R_get_obj_type
 PURPOSE
    Retrieves the type of object that an object reference points to
 USAGE
    H5G_obj_t H5R_get_obj_type(file, ref_type, ref)
        H5F_t *file;        IN: File the object being dereferenced is within
        H5R_type_t ref_type;    IN: Type of reference to query
        void *ref;          IN: Reference to query.

 RETURNS
    Success:	An object type defined in H5Gpublic.h
    Failure:	H5G_UNKNOWN
 DESCRIPTION
    Given a reference to some object, this function returns the type of object
    pointed to.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static H5G_obj_t
H5R_get_obj_type(H5F_t *file, hid_t dxpl_id, H5R_type_t ref_type, const void *_ref)
{
    H5O_loc_t oloc;             /* Object location */
    H5O_type_t obj_type;        /* Type of object */
    const uint8_t *p;           /* Pointer to OID to store */
    H5G_obj_t ret_value;        /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5R_get_obj_type)

    HDassert(file);
    HDassert(_ref);

    /* Initialize the symbol table entry */
    H5O_loc_reset(&oloc);
    oloc.file = file;

    switch(ref_type) {
        case H5R_OBJECT:
            /* Get the object oid */
            oloc.addr = *(const hobj_ref_t *)_ref; /* Only object references currently supported */
            break;

        case H5R_DATASET_REGION:
        {
            const hdset_reg_ref_t *ref = (const hdset_reg_ref_t *)_ref; /* Get pointer to correct type of reference struct */
            H5HG_t hobjid;  /* Heap object ID */
            uint8_t *buf;   /* Buffer to store serialized selection in */

            /* Get the heap ID for the dataset region */
            p = (const uint8_t *)ref;
            H5F_addr_decode(oloc.file, &p, &(hobjid.addr));
            INT32DECODE(p, hobjid.idx);

            /* Get the dataset region from the heap (allocate inside routine) */
            if((buf = H5HG_read(oloc.file, dxpl_id, &hobjid, NULL, NULL)) == NULL)
                HGOTO_ERROR(H5E_REFERENCE, H5E_READERROR, H5G_UNKNOWN, "Unable to read dataset region information")

            /* Get the object oid for the dataset */
            p = buf;
            H5F_addr_decode(oloc.file, &p, &(oloc.addr));

            /* Free the buffer allocated in H5HG_read() */
            H5MM_xfree(buf);
        } /* end case */
        break;

        case H5R_INTERNAL:
            HGOTO_ERROR(H5E_REFERENCE, H5E_UNSUPPORTED, H5G_UNKNOWN, "Internal references are not yet supported")

        case H5R_BADTYPE:
        case H5R_MAXTYPE:
        default:
            HDassert("unknown reference type" && 0);
            HGOTO_ERROR(H5E_REFERENCE, H5E_UNSUPPORTED, H5G_UNKNOWN, "internal error (unknown reference type)")
    } /* end switch */

    /* Check to make certain that this object hasn't been deleted since the reference was created */
    if(H5O_link(&oloc, 0, dxpl_id) <= 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_LINKCOUNT, H5G_UNKNOWN, "dereferencing deleted object")

    /* Get the object type */
    if(H5O_obj_type(&oloc, &obj_type, dxpl_id) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTGET, FAIL, "unable to get object type")

    ret_value = H5G_map_obj_type(obj_type);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5R_get_obj_type() */


/*--------------------------------------------------------------------------
 NAME
    H5Rget_obj_type
 PURPOSE
    Retrieves the type of object that an object reference points to
 USAGE
    int H5Rget_obj_type(id, ref_type, ref)
        hid_t id;       IN: Dataset reference object is in or location ID of
                            object that the dataset is located within.
        H5R_type_t ref_type;    IN: Type of reference to query
        void *ref;          IN: Reference to query.

 RETURNS
    Success:	An object type defined in H5Gpublic.h
    Failure:	H5G_UNKNOWN
 DESCRIPTION
    Given a reference to some object, this function returns the type of object
    pointed to.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
H5G_obj_t
H5Rget_obj_type(hid_t id, H5R_type_t ref_type, const void *ref)
{
    H5G_loc_t loc;          /* Object location */
    H5G_obj_t ret_value;

    FUNC_ENTER_API(H5Rget_obj_type, H5G_UNKNOWN)
    H5TRACE3("Go", "iRt*x", id, ref_type, ref);

    /* Check args */
    if(H5G_loc(id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5G_UNKNOWN, "not a location")
    if(ref_type <= H5R_BADTYPE || ref_type >= H5R_MAXTYPE)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5G_UNKNOWN, "invalid reference type")
    if(ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5G_UNKNOWN, "invalid reference pointer")

    /* Get the object information */
    if((ret_value = H5R_get_obj_type(loc.oloc->file, H5AC_ind_dxpl_id, ref_type, ref)) < 0)
	HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, H5G_UNKNOWN, "unable to determine object type")

done:
    FUNC_LEAVE_API(ret_value)
}   /* end H5Rget_obj_type() */


/*--------------------------------------------------------------------------
 NAME
    H5R_get_name
 PURPOSE
    Internal routine to determine a name for the object referenced
 USAGE
    ssize_t H5R_get_name(f, dxpl_id, ref_type, ref, name, size)
        H5F_t *f;       IN: Pointer to the file that the reference is pointing
                            into
        hid_t dxpl_id;  IN: DXPL to use for operation
        hid_t id;       IN: Location ID given for reference
        H5R_type_t ref_type;    IN: Type of reference
        void *ref;      IN: Reference to query.
        char *name;     OUT: Buffer to place name of object referenced
        size_t size;    IN: Size of name buffer

 RETURNS
    Non-negative length of the path on success, Negative on failure
 DESCRIPTION
    Given a reference to some object, determine a path to the object
    referenced in the file.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    This may not be the only path to that object.
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
ssize_t
H5R_get_name(H5F_t *f, hid_t dxpl_id, hid_t id, H5R_type_t ref_type,
    const void *_ref, char *name, size_t size)
{
    hid_t file_id = (-1);       /* ID for file that the reference is in */
    H5O_loc_t oloc;             /* Object location describing object for reference */
    ssize_t ret_value;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5R_get_name)

    /* Check args */
    HDassert(f);
    HDassert(_ref);
    HDassert(name);

    /* Initialize the object location */
    H5O_loc_reset(&oloc);
    oloc.file = f;

    /* Get address for reference */
    switch(ref_type) {
        case H5R_OBJECT:
            oloc.addr = *(const hobj_ref_t *)_ref;
            break;

        case H5R_DATASET_REGION:
        {
            const hdset_reg_ref_t *ref = (const hdset_reg_ref_t *)_ref; /* Get pointer to correct type of reference struct */
            H5HG_t hobjid;  /* Heap object ID */
            uint8_t *buf;   /* Buffer to store serialized selection in */
            const uint8_t *p;           /* Pointer to OID to store */

            /* Get the heap ID for the dataset region */
            p = (const uint8_t *)ref;
            H5F_addr_decode(oloc.file, &p, &(hobjid.addr));
            INT32DECODE(p, hobjid.idx);

            /* Get the dataset region from the heap (allocate inside routine) */
            if((buf = H5HG_read(oloc.file, dxpl_id, &hobjid, NULL, NULL)) == NULL)
                HGOTO_ERROR(H5E_REFERENCE, H5E_READERROR, FAIL, "Unable to read dataset region information")

            /* Get the object oid for the dataset */
            p = buf;
            H5F_addr_decode(oloc.file, &p, &(oloc.addr));

            /* Free the buffer allocated in H5HG_read() */
            H5MM_xfree(buf);
        } /* end case */
        break;

        case H5R_INTERNAL:
            HGOTO_ERROR(H5E_REFERENCE, H5E_UNSUPPORTED, FAIL, "Internal references are not yet supported")

        case H5R_BADTYPE:
        case H5R_MAXTYPE:
        default:
            HDassert("unknown reference type" && 0);
            HGOTO_ERROR(H5E_REFERENCE, H5E_UNSUPPORTED, FAIL, "internal error (unknown reference type)")
    } /* end switch */

    /* Retrieve file ID for name search */
    if((file_id = H5I_get_file_id(id)) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTGET, FAIL, "can't retrieve file ID")

    /* Get name, length, etc. */
    if((ret_value = H5G_get_refobj_name(file_id, dxpl_id, &oloc, name, size)) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTGET, FAIL, "can't determine name")

done:
    /* Close file ID used for search */
    if(file_id > 0)
        if(H5I_dec_ref(file_id) < 0)
            HDONE_ERROR(H5E_REFERENCE, H5E_CANTCLOSEFILE, FAIL, "can't determine name")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5R_get_name() */


/*--------------------------------------------------------------------------
 NAME
    H5Rget_name
 PURPOSE
    Determines a name for the object referenced
 USAGE
    ssize_t H5Rget_name(loc_id, ref_type, ref, name, size)
        hid_t loc_id;   IN: Dataset reference object is in or location ID of
                            object that the dataset is located within.
        H5R_type_t ref_type;    IN: Type of reference
        void *ref;      IN: Reference to query.
        char *name;     OUT: Buffer to place name of object referenced
        size_t size;    IN: Size of name buffer

 RETURNS
    Non-negative length of the path on success, Negative on failure
 DESCRIPTION
    Given a reference to some object, determine a path to the object
    referenced in the file.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    This may not be the only path to that object.
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
ssize_t
H5Rget_name(hid_t id, H5R_type_t ref_type, const void *_ref, char *name,
    size_t size)
{
    H5G_loc_t loc;      /* Group location */
    H5F_t *file;        /* File object */
    ssize_t ret_value;  /* Return value */

    FUNC_ENTER_API(H5Rget_name, FAIL)
    H5TRACE5("Zs", "iRt*x*sz", id, ref_type, _ref, name, size);

    /* Check args */
    if(H5G_loc(id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(ref_type <= H5R_BADTYPE || ref_type >= H5R_MAXTYPE)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type")
    if(_ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")

    /* Get the file pointer from the entry */
    file = loc.oloc->file;

    /* Get name */
    if((ret_value = H5R_get_name(file, H5AC_dxpl_id, id, ref_type, _ref, name, size)) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable to determine object path")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Rget_name() */

