/****************************************************************************
* NCSA HDF                                                                 *
* Software Development Group                                               *
* National Center for Supercomputing Applications                          *
* University of Illinois at Urbana-Champaign                               *
* 605 E. Springfield, Champaign IL 61820                                   *
*                                                                          *
* For conditions of distribution and use, see the accompanying             *
* hdf/COPYING file.                                                        *
*                                                                          *
****************************************************************************/

#ifdef RCSID
static char             RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */

/*LINTLIBRARY */
/*+
   FILE
   hdf5meta.c
   HDF5 "Meta-Object" routines

   EXPORTED ROUTINES
   H5Maccess    -- Start access to an existing object
   H5Mcopy          -- Copy an object
   H5Mopen          -- Find an object by name
   H5Mname_len  -- Get the length of an object's name
   H5Mget_name  -- Get an object's name
   H5Mset_name  -- Set an object's name
   H5Msearch    -- Wildcard search for an object by name
   H5Mindex         -- Get an object by index
   H5Mflush         -- Flush an object out to disk
   H5Mdelete    -- Delete an object from disk
   H5Mget_file  -- Get the file ID for an object
   H5Mget_file  -- Get the parent ID for an object
   H5Mclose   -- Release access to an object

   LIBRARY-SCOPED ROUTINES

   LOCAL ROUTINES
   H5M_init_interface    -- initialize the interface
   + */

#include <H5private.h>          /* Generic functions */
#include <H5Aprivate.h>		/* Atom interface */
#include <H5Cprivate.h>         /* Template interface */
#include <H5Dprivate.h>         /* Dataset interface */
#include <H5Eprivate.h>         /*error handling */
#include <H5Pprivate.h>         /* Dataspace functions */
#include <H5Tprivate.h>         /* Datatype interface */
#include <H5Mprivate.h>         /* Meta-object interface */
#include <H5Cprivate.h>         /* Template interface */

#define PABLO_MASK      H5M_mask

/*--------------------- Locally scoped variables -----------------------------*/

static meta_func_t      meta_func_arr[] =
{
    {                           /* Template object meta-functions (defined in H5C.c) */
        H5_TEMPLATE_0,          /* File-Creation Template Type ID */
        NULL,                   /* File-Creation Template Create */
        NULL,                   /* File-Creation Template Access */
        H5Ccopy,                /* File-Creation Template Copy */
        NULL,                   /* File-Creation Template FindName */
        NULL,                   /* File-Creation Template NameLen */
        NULL,                   /* File-Creation Template GetName */
        NULL,                   /* File-Creation Template SetName */
        NULL,                   /* File-Creation Template Search */
        NULL,                   /* File-Creation Template Index */
        NULL,                   /* File-Creation Template Flush */
        NULL,                   /* File-Creation Template Delete */
        NULL,                   /* File-Creation Template GetParent */
        NULL,                   /* File-Creation Template GetFile */
        H5Cclose                /* File-Creation Template Release */
    },
    {                           /* Template object meta-functions (defined in H5C.c) */
        H5_TEMPLATE_1,          /* File-Access Template Type ID */
        NULL,                   /* File-Access Template Create */
        NULL,                   /* File-Access Template Access */
        H5Ccopy,                /* File-Access Template Copy */
        NULL,                   /* File-Access Template FindName */
        NULL,                   /* File-Access Template NameLen */
        NULL,                   /* File-Access Template GetName */
        NULL,                   /* File-Access Template SetName */
        NULL,                   /* File-Access Template Search */
        NULL,                   /* File-Access Template Index */
        NULL,                   /* File-Access Template Flush */
        NULL,                   /* File-Access Template Delete */
        NULL,                   /* File-Access Template GetParent */
        NULL,                   /* File-Access Template GetFile */
        H5Cclose                /* File-Access Template Release */
    },
    {                           /* Datatype object meta-functions (defined in H5T.c) */
        H5_DATATYPE,            /* Datatype Type ID */
        NULL,                   /* Datatype Create */
        NULL,                   /* Datatype Access */
        H5Tcopy,                /* Dataspace Copy */
        NULL,                   /* Datatype FindName */
        NULL,                   /* Datatype NameLen */
        NULL,                   /* Datatype GetName */
        NULL,                   /* Datatype SetName */
        NULL,                   /* Datatype Search */
        NULL,                   /* Datatype Index */
        NULL,                   /* Datatype Flush */
        NULL,                   /* Datatype Delete */
        NULL,                   /* Datatype GetParent */
        NULL,                   /* Datatype GetFile */
        H5Tclose                /* Datatype Release */
    },
    {                           /* Dimensionality object meta-functions (defined in H5P.c) */
        H5_DATASPACE,           /* Dimensionality Type ID */
        NULL,                   /* Dimensionality Create */
        NULL,                   /* Dimensionality Access */
        NULL,                   /* Dimensionality Copy */
        NULL,                   /* Dimensionality FindName */
        NULL,                   /* Dimensionality NameLen */
        NULL,                   /* Dimensionality GetName */
        NULL,                   /* Dimensionality SetName */
        NULL,                   /* Dimensionality Search */
        NULL,                   /* Dimensionality Index */
        NULL,                   /* Dimensionality Flush */
        NULL,                   /* Dimensionality Delete */
        NULL,                   /* Dimensionality GetParent */
        NULL,                   /* Dimensionality GetFile */
        H5Pclose                /* Dimensionality Release */
    },
    {                           /* Dataset object meta-functions (defined in H5D.c) */
        H5_DATASET,             /* Dataset Type ID */
        NULL,                   /* Dataset Create */
        NULL,                   /* Dataset Access */
        NULL,                   /* Dataset Copy */
        H5D_find_name,          /* Dataset FindName */
        NULL,                   /* Dataset NameLen */
        NULL,                   /* Dataset GetName */
        NULL,                   /* Dataset SetName */
        NULL,                   /* Dataset Search */
        NULL,                   /* Dataset Index */
        NULL,                   /* Dataset Flush */
        NULL,                   /* Dataset Delete */
        NULL,                   /* Dataset GetParent */
        NULL,                   /* Dataset GetFile */
        H5Dclose                /* Dataset Release */
    },
};

/* Interface initialization */
static intn             interface_initialize_g = FALSE;
#define INTERFACE_INIT  H5M_init_interface
static herr_t           H5M_init_interface(void);

/*--------------------------------------------------------------------------
NAME
   H5M_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5M_init_interface()
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Initializes any interface-specific data or routines.

MODIFICATIONS
    Robb Matzke, 4 Aug 1997
    Changed the FUNC variable value to H5M_init_interface.

--------------------------------------------------------------------------*/
static herr_t 
H5M_init_interface(void)
{
    herr_t                  ret_value = SUCCEED;
    FUNC_ENTER(H5M_init_interface, FAIL);

    FUNC_LEAVE(ret_value);
}                               /* H5M_init_interface */

/*--------------------------------------------------------------------------
 NAME
    H5M_find_type
 PURPOSE
    Find the type of meta-object to issue a method call on
 USAGE
    intn H5M_find_type(type)
        hobjtype_t type;        IN: Type of object to create
 RETURNS
     Returns the index of the type in the array of methods on success, or FAIL 
        on failure.
 DESCRIPTION
        This function performs a search to find the index of the type of a 
    meta-object in the array of function pointers.
--------------------------------------------------------------------------*/
static intn
H5M_find_type(group_t type)
{
    intn                    i;  /* local counting variable */
    intn                    ret_value = FAIL;

    FUNC_ENTER(H5M_find_type, FAIL);

    /*
     * Currently this uses a stright linear search, which can easily be changed
     * to a binary search when it becomes too slow.
     */
    for (i = 0; i < (sizeof(meta_func_arr) / sizeof(meta_func_t)); i++)
        if (type == meta_func_arr[i].type)
            HGOTO_DONE(i);

  done:
    if (ret_value == FAIL) {    /* Error condition cleanup */

    }                           /* end if */
    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
}                               /* end H5M_find_type() */

/*--------------------------------------------------------------------------
 NAME
    H5Maccess
 PURPOSE
    Start access to an existing HDF5 object.
 USAGE
    hid_t H5Maccess(owner_id)
        hid_t oid;       IN: OID of the object to access.
 RETURNS
    Returns ID (atom) on success, FAIL on failure
 DESCRIPTION
        This function re-directs the object's access into the appropriate
    interface, as defined by the function pointers in hdf5fptr.h
--------------------------------------------------------------------------*/
hid_t 
H5Maccess(hid_t oid)
{
    group_t                 group;
    intn                    i;  /* local counting variable */
    hid_t                   ret_value = SUCCEED;

    FUNC_ENTER(H5Maccess, FAIL);

    /* Atom group for incoming object */
    group = H5A_group(oid);

    /* Check args and all the boring stuff. */
    if (group <= BADGROUP || group >= MAXGROUP)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "bad group");

    i = H5M_find_type(group);
    if (meta_func_arr[i].access == NULL)
        HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL,
                    "no access method");
    ret_value = (meta_func_arr[i].access) (oid);

  done:
    if (ret_value == FAIL) {    /* Error condition cleanup */

    }                           /* end if */
    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
}                               /* end H5Maccess() */

/*--------------------------------------------------------------------------
 NAME
    H5Mcopy
 PURPOSE
    Copy an HDF5 object.
 USAGE
    hid_t H5Mcopy(oid)
        hid_t oid;       IN: Object to copy
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function re-directs the object's copy into the appropriate
    interface, as defined by the function pointers in hdf5fptr.h
--------------------------------------------------------------------------*/
hid_t 
H5Mcopy(hid_t oid)
{
    group_t                 group = H5A_group(oid);         /* Atom group for incoming object */
    intn                    i;  /* local counting variable */
    herr_t                  ret_value = SUCCEED;

    FUNC_ENTER(H5Mcopy, FAIL);

    /* Check args and all the boring stuff. */
    if (group <= BADGROUP || group >= MAXGROUP)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "bad group");

    i = H5M_find_type(group);
    if (meta_func_arr[i].copy == NULL)
        HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "no copy method");
    ret_value = (meta_func_arr[i].copy) (oid);

  done:
    if (ret_value == FAIL) {    /* Error condition cleanup */

    }                           /* end if */
    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
}                               /* end H5Mcopy() */

/*--------------------------------------------------------------------------
 NAME
    H5Mfind_name
 PURPOSE
    Find an HDF5 object by name.
 USAGE
    hid_t H5Mfind_name(owner_id, type, name)
        hid_t owner_id;       IN: Group/file in which to search
        hobjtype_t type;        IN: Type of object to search names of
        const char *name;       IN: Name of the object to search for
 RETURNS
    Returns ID (atom) on success, FAIL on failure
 DESCRIPTION
        This function re-directs the object's "find name" into the appropriate
    interface, as defined by the function pointers in hdf5fptr.h
--------------------------------------------------------------------------*/
hid_t
H5Mfind_name(hid_t owner_id, group_t type, const char *name)
{
#ifdef OLD_WAY
    group_t                 group = H5A_group(owner_id);    /* Atom group for incoming object */
#endif /* OLD_WAY */
    intn                    i;  /* local counting variable */
    hid_t                   ret_value = SUCCEED;

    FUNC_ENTER(H5Mfind_name, FAIL);

    /* Check args and all the boring stuff. */
#ifdef OLD_WAY
    if (group <= BADGROUP || group >= MAXGROUP)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL);

    i = H5M_find_type(group);
#else /* OLD_WAY */
    if (type <= BADGROUP || type >= MAXGROUP)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "bad group");

    i = H5M_find_type(type);
#endif /* OLD_WAY */
    if (meta_func_arr[i].find_name == NULL)
        HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "no open method");
    ret_value = (meta_func_arr[i].find_name) (owner_id, type, name);

  done:
    if (ret_value == FAIL) {    /* Error condition cleanup */

    }                           /* end if */
    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
}                               /* end H5Mfind_name() */

/*--------------------------------------------------------------------------
 NAME
    H5Mname_len
 PURPOSE
    Determine the length of the name of an HDF5 object.
 USAGE
    uint32 H5Mname_len(oid)
        hid_t oid;       IN: Object to get name's length
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function re-directs the object's "name length" into the appropriate
    interface, as defined by the function pointers in hdf5fptr.h
--------------------------------------------------------------------------*/
uint32 
H5Mname_len(hid_t oid)
{
    group_t                 group = H5A_group(oid);         /* Atom group for incoming object */
    intn                    i;  /* local counting variable */
    herr_t                  ret_value = SUCCEED;

    FUNC_ENTER(H5Mname_len, FAIL);

    /* Check args and all the boring stuff. */
    if (group <= BADGROUP || group >= MAXGROUP)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "bad group");

    i = H5M_find_type(group);
    if (meta_func_arr[i].name_len == NULL)
        HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "no name_len method");
    ret_value = (meta_func_arr[i].name_len) (oid);

  done:
    if (ret_value == FAIL) {    /* Error condition cleanup */

    }                           /* end if */
    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
}                               /* end H5Mname_len() */

/*--------------------------------------------------------------------------
 NAME
    H5Mget_name
 PURPOSE
    Get the name of an HDF5 object.
 USAGE
    herr_t H5Mget_name(oid, name)
        hid_t oid;            IN: Object to retreive name of
        char *name;             OUT: Buffer to place object's name in
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function re-directs the object's "get name" into the appropriate
    interface, as defined by the function pointers in hdf5fptr.h
--------------------------------------------------------------------------*/
herr_t 
H5Mget_name(hid_t oid, char *name)
{
    group_t                 group = H5A_group(oid);         /* Atom group for incoming object */
    intn                    i;  /* local counting variable */
    hid_t                   ret_value = SUCCEED;

    FUNC_ENTER(H5Mget_name, FAIL);

    /* Check args and all the boring stuff. */
    if (group <= BADGROUP || group >= MAXGROUP)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "bad group");

    i = H5M_find_type(group);
    if (meta_func_arr[i].get_name == NULL)
        HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL,
                    "no get_name method");
    ret_value = (meta_func_arr[i].get_name) (oid, name);

  done:
    if (ret_value == FAIL) {    /* Error condition cleanup */

    }                           /* end if */
    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
}                               /* end H5Mget_name() */

/*--------------------------------------------------------------------------
 NAME
    H5Mset_name
 PURPOSE
    Set the name of an HDF5 object.
 USAGE
    herr_t H5Mget_name(oid, name)
        hid_t oid;            IN: Object to set name of
        const char *name;       IN: Name to use for object
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function re-directs the object's "set name" into the appropriate
    interface, as defined by the function pointers in hdf5fptr.h
--------------------------------------------------------------------------*/
herr_t 
H5Mset_name(hid_t oid, const char *name)
{
    group_t                 group = H5A_group(oid);         /* Atom group for incoming object */
    intn                    i;  /* local counting variable */
    hid_t                   ret_value = SUCCEED;

    FUNC_ENTER(H5Mset_name, FAIL);

    /* Check args and all the boring stuff. */
    if (group <= BADGROUP || group >= MAXGROUP)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "bad group");

    i = H5M_find_type(group);
    if (meta_func_arr[i].set_name == NULL)
        HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL,
                    "no set_name method");
    ret_value = (meta_func_arr[i].set_name) (oid, name);

  done:
    if (ret_value == FAIL) {    /* Error condition cleanup */

    }                           /* end if */
    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
}                               /* end H5Mset_name() */

/*--------------------------------------------------------------------------
 NAME
    H5Msearch
 PURPOSE
    Wildcard search for an HDF5 object by name.
 USAGE
    hid_t H5Mfind_name(owner_id, type, name)
        hid_t owner_id;       IN: Group/file in which to search
        hobjtype_t type;        IN: Type of object to search names of
        const char *name;       IN: Name of the object to search for
 RETURNS
    Returns ID (atom) on success, FAIL on failure
 DESCRIPTION
        This function re-directs the object's "search" into the appropriate
    interface, as defined by the function pointers in hdf5fptr.h
--------------------------------------------------------------------------*/
hid_t
H5Msearch(hid_t oid, group_t type, const char *name)
{
    group_t                 group = H5A_group(oid);         /* Atom group for incoming object */
    intn                    i;  /* local counting variable */
    hid_t                   ret_value = SUCCEED;

    FUNC_ENTER(H5Msearch, FAIL);

    /* Check args and all the boring stuff. */
    if (group <= BADGROUP || group >= MAXGROUP)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "bad group");

    i = H5M_find_type(group);
    if (meta_func_arr[i].search == NULL)
        HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "no search method");
    ret_value = (meta_func_arr[i].search) (oid, type, name);

  done:
    if (ret_value == FAIL) {    /* Error condition cleanup */

    }                           /* end if */
    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
}                               /* end H5Msearch() */

/*--------------------------------------------------------------------------
 NAME
    H5Mindex
 PURPOSE
    Get an HDF5 object by index.
 USAGE
    hid_t H5Mindex(oid, type, idx)
        hid_t oid;            IN: Group/file in which to find items
        hobjtype_t type;        IN: Type of object to get
        uint32 idx;             IN: Index of the object to get
 RETURNS
    Returns ID (atom) on success, FAIL on failure
 DESCRIPTION
        This function re-directs the object's "index" into the appropriate
    interface, as defined by the function pointers in hdf5fptr.h
--------------------------------------------------------------------------*/
hid_t
H5Mindex(hid_t oid, group_t type, uint32 idx)
{
    group_t                 group = H5A_group(oid);         /* Atom group for incoming object */
    intn                    i;  /* local counting variable */
    hid_t                   ret_value = SUCCEED;

    FUNC_ENTER(H5Mindex, FAIL);

    /* Check args and all the boring stuff. */
    if (group <= BADGROUP || group >= MAXGROUP)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "bad group");

    i = H5M_find_type(group);
    if (meta_func_arr[i].index == NULL)
        HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "no index method");
    ret_value = (meta_func_arr[i].index) (oid, type, idx);

  done:
    if (ret_value == FAIL) {    /* Error condition cleanup */

    }                           /* end if */
    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
}                               /* end H5Mindex() */

/*--------------------------------------------------------------------------
 NAME
    H5Mflush
 PURPOSE
    Flush an HDF5 object out to a file.
 USAGE
    hid_t H5Mflush(oid)
        hid_t oid;       IN: Object to flush
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function re-directs the object's flush into the appropriate
    interface, as defined by the function pointers in hdf5fptr.h
--------------------------------------------------------------------------*/
hid_t 
H5Mflush(hid_t oid)
{
    group_t                 group;      /* Atom group for incoming object */
    intn                    i;  /* local counting variable */
    herr_t                  ret_value = SUCCEED;

    FUNC_ENTER(H5Mflush, FAIL); /* Insert function initialization code and variables */

    /* Check args and all the boring stuff. */
    group = H5A_group(oid); /* look up group for incoming object */
    if (group <= BADGROUP || group >= MAXGROUP)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "bad group");

    /* Find correct function pointer set from static array */
    i = H5M_find_type(group);
    if (meta_func_arr[i].flush == NULL)
        HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "no flush method");
    ret_value = (meta_func_arr[i].flush) (oid);

  done:
    if (ret_value == FAIL) {    /* Error condition cleanup */

    }                           /* end if */
    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);      /* Insert function prologue code */
}                               /* end H5Mflush() */

/*--------------------------------------------------------------------------
 NAME
    H5Mdelete
 PURPOSE
    Delete an HDF5 object from a file.
 USAGE
    herr_t H5Mdelete(oid)
        hid_t oid;       IN: Object to delete
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function re-directs the object's delete into the appropriate
    interface, as defined by the function pointers in hdf5fptr.h.  Deleting
    an object implicitly ends access to it.
--------------------------------------------------------------------------*/
herr_t 
H5Mdelete(hid_t oid)
{
    group_t                 group = H5A_group(oid);         /* Atom group for incoming object */
    intn                    i;  /* local counting variable */
    herr_t                  ret_value = SUCCEED;

    FUNC_ENTER(H5Mdelete, FAIL);

    /* Check args and all the boring stuff. */
    if (group <= BADGROUP || group >= MAXGROUP)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "bad group");

    i = H5M_find_type(group);
    if (meta_func_arr[i].delete == NULL)
        HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "no delete method");
    ret_value = (meta_func_arr[i].delete) (oid);

  done:
    if (ret_value == FAIL) {    /* Error condition cleanup */

    }                           /* end if */
    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
}                               /* end H5Mdelete() */

/*--------------------------------------------------------------------------
 NAME
    H5Mget_parent
 PURPOSE
    Get the parent ID an HDF5 object.
 USAGE
    hid_t H5Mget_parent(oid)
        hid_t oid;       IN: Object to query
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function re-directs the object's query into the appropriate
    interface, as defined by the function pointers in hdf5fptr.h
--------------------------------------------------------------------------*/
hid_t 
H5Mget_parent(hid_t oid)
{
    group_t                 group = H5A_group(oid);         /* Atom group for incoming object */
    intn                    i;  /* local counting variable */
    herr_t                  ret_value = SUCCEED;

    FUNC_ENTER(H5Mget_parent, FAIL);

    /* Check args and all the boring stuff. */
    if (group <= BADGROUP || group >= MAXGROUP)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "bad group");

    i = H5M_find_type(group);
    if (meta_func_arr[i].get_parent == NULL)
        HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL,
                    "no get_parent method");
    ret_value = (meta_func_arr[i].get_parent) (oid);

  done:
    if (ret_value == FAIL) {    /* Error condition cleanup */

    }                           /* end if */
    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
}                               /* end H5Mget_parent() */

/*--------------------------------------------------------------------------
 NAME
    H5Mget_file
 PURPOSE
    Get the file ID an HDF5 object.
 USAGE
    hid_t H5Mget_file(oid)
        hid_t oid;       IN: Object to query
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function re-directs the object's query into the appropriate
    interface, as defined by the function pointers in hdf5fptr.h
--------------------------------------------------------------------------*/
hid_t 
H5Mget_file(hid_t oid)
{
    group_t                 group = H5A_group(oid);         /* Atom group for incoming object */
    intn                    i;  /* local counting variable */
    herr_t                  ret_value = SUCCEED;

    FUNC_ENTER(H5Mget_file, FAIL);

    /* Check args and all the boring stuff. */
    if (group <= BADGROUP || group >= MAXGROUP)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "bad group");

    i = H5M_find_type(group);
    if (meta_func_arr[i].get_file == NULL)
        HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL,
                    "no get_file method");
    ret_value = (meta_func_arr[i].get_file) (oid);

  done:
    if (ret_value == FAIL) {    /* Error condition cleanup */

    }                           /* end if */
    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
}                               /* end H5Mget_file() */

/*--------------------------------------------------------------------------
 NAME
    H5Mclose
 PURPOSE
    Release access to an HDF5 object.
 USAGE
    herr_t H5Mclose(oid)
        hid_t oid;       IN: Object to release access to
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function re-directs the object's release into the appropriate
    interface, as defined by the function pointers in hdf5fptr.h
--------------------------------------------------------------------------*/
herr_t 
H5Mclose(hid_t oid)
{
    group_t                 group = H5A_group(oid);         /* Atom group for incoming object */
    intn                    i;  /* local counting variable */
    herr_t                  ret_value = SUCCEED;

    FUNC_ENTER(H5Mclose, FAIL);

    /* Check args and all the boring stuff. */
    if (group <= BADGROUP || group >= MAXGROUP)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "bad group");

    i = H5M_find_type(group);
    if (meta_func_arr[i].close == NULL)
        HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "no close method");
    ret_value = (meta_func_arr[i].close) (oid);

  done:
    if (ret_value == FAIL) {    /* Error condition cleanup */

    }                           /* end if */
    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
}
