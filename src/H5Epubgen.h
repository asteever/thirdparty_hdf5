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

/* Generated automatically by bin/make_err -- do not edit */
/* Add new errors to H5err.txt file */


#ifndef _H5Epubgen_H
#define _H5Epubgen_H

/*********************/
/* Major error codes */
/*********************/

#define H5E_DATASET          (H5OPEN H5E_DATASET_g)
#define H5E_FUNC             (H5OPEN H5E_FUNC_g)
#define H5E_STORAGE          (H5OPEN H5E_STORAGE_g)
#define H5E_FILE             (H5OPEN H5E_FILE_g)
#define H5E_FPHDF5           (H5OPEN H5E_FPHDF5_g)
#define H5E_SYM              (H5OPEN H5E_SYM_g)
#define H5E_VFL              (H5OPEN H5E_VFL_g)
#define H5E_INTERNAL         (H5OPEN H5E_INTERNAL_g)
#define H5E_BTREE            (H5OPEN H5E_BTREE_g)
#define H5E_REFERENCE        (H5OPEN H5E_REFERENCE_g)
#define H5E_DATASPACE        (H5OPEN H5E_DATASPACE_g)
#define H5E_RESOURCE         (H5OPEN H5E_RESOURCE_g)
#define H5E_PLIST            (H5OPEN H5E_PLIST_g)
#define H5E_DATATYPE         (H5OPEN H5E_DATATYPE_g)
#define H5E_RS               (H5OPEN H5E_RS_g)
#define H5E_HEAP             (H5OPEN H5E_HEAP_g)
#define H5E_OHDR             (H5OPEN H5E_OHDR_g)
#define H5E_TBBT             (H5OPEN H5E_TBBT_g)
#define H5E_ATOM             (H5OPEN H5E_ATOM_g)
#define H5E_ATTR             (H5OPEN H5E_ATTR_g)
#define H5E_IO               (H5OPEN H5E_IO_g)
#define H5E_SLIST            (H5OPEN H5E_SLIST_g)
#define H5E_EFL              (H5OPEN H5E_EFL_g)
#define H5E_TST              (H5OPEN H5E_TST_g)
#define H5E_ARGS             (H5OPEN H5E_ARGS_g)
#define H5E_ERROR            (H5OPEN H5E_ERROR_g)
#define H5E_PLINE            (H5OPEN H5E_PLINE_g)
#define H5E_CACHE            (H5OPEN H5E_CACHE_g)
H5_DLLVAR hid_t H5E_DATASET_g;       /* Dataset */
H5_DLLVAR hid_t H5E_FUNC_g;          /* Function entry/exit */
H5_DLLVAR hid_t H5E_STORAGE_g;       /* Data storage */
H5_DLLVAR hid_t H5E_FILE_g;          /* File accessability */
H5_DLLVAR hid_t H5E_FPHDF5_g;        /* Flexible Parallel HDF5 */
H5_DLLVAR hid_t H5E_SYM_g;           /* Symbol table */
H5_DLLVAR hid_t H5E_VFL_g;           /* Virtual File Layer */
H5_DLLVAR hid_t H5E_INTERNAL_g;      /* Internal error (too specific to document in detail) */
H5_DLLVAR hid_t H5E_BTREE_g;         /* B-Tree node */
H5_DLLVAR hid_t H5E_REFERENCE_g;     /* References */
H5_DLLVAR hid_t H5E_DATASPACE_g;     /* Dataspace */
H5_DLLVAR hid_t H5E_RESOURCE_g;      /* Resource unavailable */
H5_DLLVAR hid_t H5E_PLIST_g;         /* Property lists */
H5_DLLVAR hid_t H5E_DATATYPE_g;      /* Datatype */
H5_DLLVAR hid_t H5E_RS_g;            /* Reference Counted Strings */
H5_DLLVAR hid_t H5E_HEAP_g;          /* Heap */
H5_DLLVAR hid_t H5E_OHDR_g;          /* Object header */
H5_DLLVAR hid_t H5E_TBBT_g;          /* Threaded, Balanced, Binary Trees */
H5_DLLVAR hid_t H5E_ATOM_g;          /* Object atom */
H5_DLLVAR hid_t H5E_ATTR_g;          /* Attribute */
H5_DLLVAR hid_t H5E_IO_g;            /* Low-level I/O */
H5_DLLVAR hid_t H5E_SLIST_g;         /* Skip Lists */
H5_DLLVAR hid_t H5E_EFL_g;           /* External file list */
H5_DLLVAR hid_t H5E_TST_g;           /* Ternary Search Trees */
H5_DLLVAR hid_t H5E_ARGS_g;          /* Invalid arguments to routine */
H5_DLLVAR hid_t H5E_ERROR_g;         /* Error API */
H5_DLLVAR hid_t H5E_PLINE_g;         /* Data filters */
H5_DLLVAR hid_t H5E_CACHE_g;         /* Object cache */

/*********************/
/* Minor error codes */
/*********************/

/* Threaded, balanced binary tree errors */
#define H5E_CANTMAKETREE     (H5OPEN H5E_CANTMAKETREE_g)
H5_DLLVAR hid_t H5E_CANTMAKETREE_g;  /* Can't create a binary tree node */

/* Generic low-level file I/O errors */
#define H5E_SEEKERROR        (H5OPEN H5E_SEEKERROR_g)
#define H5E_READERROR        (H5OPEN H5E_READERROR_g)
#define H5E_WRITEERROR       (H5OPEN H5E_WRITEERROR_g)
#define H5E_CLOSEERROR       (H5OPEN H5E_CLOSEERROR_g)
#define H5E_OVERFLOW         (H5OPEN H5E_OVERFLOW_g)
#define H5E_FCNTL            (H5OPEN H5E_FCNTL_g)
H5_DLLVAR hid_t H5E_SEEKERROR_g;     /* Seek failed */
H5_DLLVAR hid_t H5E_READERROR_g;     /* Read failed */
H5_DLLVAR hid_t H5E_WRITEERROR_g;    /* Write failed */
H5_DLLVAR hid_t H5E_CLOSEERROR_g;    /* Close failed */
H5_DLLVAR hid_t H5E_OVERFLOW_g;      /* Address overflowed */
H5_DLLVAR hid_t H5E_FCNTL_g;         /* File control (fcntl) failed */

/* Resource errors */
#define H5E_NOSPACE          (H5OPEN H5E_NOSPACE_g)
#define H5E_CANTCOPY         (H5OPEN H5E_CANTCOPY_g)
#define H5E_CANTFREE         (H5OPEN H5E_CANTFREE_g)
#define H5E_ALREADYEXISTS    (H5OPEN H5E_ALREADYEXISTS_g)
#define H5E_CANTLOCK         (H5OPEN H5E_CANTLOCK_g)
#define H5E_CANTUNLOCK       (H5OPEN H5E_CANTUNLOCK_g)
#define H5E_CANTGC           (H5OPEN H5E_CANTGC_g)
#define H5E_CANTGETSIZE      (H5OPEN H5E_CANTGETSIZE_g)
H5_DLLVAR hid_t H5E_NOSPACE_g;       /* No space available for allocation */
H5_DLLVAR hid_t H5E_CANTCOPY_g;      /* Unable to copy object */
H5_DLLVAR hid_t H5E_CANTFREE_g;      /* Unable to free object */
H5_DLLVAR hid_t H5E_ALREADYEXISTS_g; /* Object already exists */
H5_DLLVAR hid_t H5E_CANTLOCK_g;      /* Unable to lock object */
H5_DLLVAR hid_t H5E_CANTUNLOCK_g;    /* Unable to unlock object */
H5_DLLVAR hid_t H5E_CANTGC_g;        /* Unable to garbage collect */
H5_DLLVAR hid_t H5E_CANTGETSIZE_g;   /* Unable to compute size */

/* Heap errors */
#define H5E_CANTRESTORE      (H5OPEN H5E_CANTRESTORE_g)
H5_DLLVAR hid_t H5E_CANTRESTORE_g;   /* Can't restore condition */

/* Function entry/exit interface errors */
#define H5E_CANTINIT         (H5OPEN H5E_CANTINIT_g)
#define H5E_ALREADYINIT      (H5OPEN H5E_ALREADYINIT_g)
#define H5E_CANTRELEASE      (H5OPEN H5E_CANTRELEASE_g)
H5_DLLVAR hid_t H5E_CANTINIT_g;      /* Unable to initialize object */
H5_DLLVAR hid_t H5E_ALREADYINIT_g;   /* Object already initialized */
H5_DLLVAR hid_t H5E_CANTRELEASE_g;   /* Unable to release object */

/* Property list errors */
#define H5E_CANTGET          (H5OPEN H5E_CANTGET_g)
#define H5E_CANTSET          (H5OPEN H5E_CANTSET_g)
#define H5E_DUPCLASS         (H5OPEN H5E_DUPCLASS_g)
H5_DLLVAR hid_t H5E_CANTGET_g;       /* Can't get value */
H5_DLLVAR hid_t H5E_CANTSET_g;       /* Can't set value */
H5_DLLVAR hid_t H5E_DUPCLASS_g;      /* Duplicate class name in parent class */

/* Object header related errors */
#define H5E_LINKCOUNT        (H5OPEN H5E_LINKCOUNT_g)
#define H5E_VERSION          (H5OPEN H5E_VERSION_g)
#define H5E_ALIGNMENT        (H5OPEN H5E_ALIGNMENT_g)
#define H5E_BADMESG          (H5OPEN H5E_BADMESG_g)
#define H5E_CANTDELETE       (H5OPEN H5E_CANTDELETE_g)
H5_DLLVAR hid_t H5E_LINKCOUNT_g;     /* Bad object header link count */
H5_DLLVAR hid_t H5E_VERSION_g;       /* Wrong version number */
H5_DLLVAR hid_t H5E_ALIGNMENT_g;     /* Alignment error */
H5_DLLVAR hid_t H5E_BADMESG_g;       /* Unrecognized message */
H5_DLLVAR hid_t H5E_CANTDELETE_g;    /* Can't delete message */

/* FPHDF5 errors */
#define H5E_CANTRECV         (H5OPEN H5E_CANTRECV_g)
#define H5E_CANTSENDMDATA    (H5OPEN H5E_CANTSENDMDATA_g)
#define H5E_CANTCHANGE       (H5OPEN H5E_CANTCHANGE_g)
#define H5E_CANTALLOC        (H5OPEN H5E_CANTALLOC_g)
H5_DLLVAR hid_t H5E_CANTRECV_g;      /* Can't receive messages from processes */
H5_DLLVAR hid_t H5E_CANTSENDMDATA_g; /* Can't send metadata message */
H5_DLLVAR hid_t H5E_CANTCHANGE_g;    /* Can't register change with server */
H5_DLLVAR hid_t H5E_CANTALLOC_g;     /* Can't allocate from file */

/* I/O pipeline errors */
#define H5E_NOFILTER         (H5OPEN H5E_NOFILTER_g)
#define H5E_CALLBACK         (H5OPEN H5E_CALLBACK_g)
#define H5E_CANAPPLY         (H5OPEN H5E_CANAPPLY_g)
#define H5E_SETLOCAL         (H5OPEN H5E_SETLOCAL_g)
#define H5E_NOENCODER        (H5OPEN H5E_NOENCODER_g)
H5_DLLVAR hid_t H5E_NOFILTER_g;      /* Requested filter is not available */
H5_DLLVAR hid_t H5E_CALLBACK_g;      /* Callback failed */
H5_DLLVAR hid_t H5E_CANAPPLY_g;      /* Error from filter 'can apply' callback */
H5_DLLVAR hid_t H5E_SETLOCAL_g;      /* Error from filter 'set local' callback */
H5_DLLVAR hid_t H5E_NOENCODER_g;     /* Filter present but encoding disabled */

/* Group related errors */
#define H5E_CANTOPENOBJ      (H5OPEN H5E_CANTOPENOBJ_g)
#define H5E_CANTCLOSEOBJ     (H5OPEN H5E_CANTCLOSEOBJ_g)
#define H5E_COMPLEN          (H5OPEN H5E_COMPLEN_g)
#define H5E_CWG              (H5OPEN H5E_CWG_g)
#define H5E_LINK             (H5OPEN H5E_LINK_g)
#define H5E_SLINK            (H5OPEN H5E_SLINK_g)
H5_DLLVAR hid_t H5E_CANTOPENOBJ_g;   /* Can't open object */
H5_DLLVAR hid_t H5E_CANTCLOSEOBJ_g;  /* Can't close object */
H5_DLLVAR hid_t H5E_COMPLEN_g;       /* Name component is too long */
H5_DLLVAR hid_t H5E_CWG_g;           /* Problem with current working group */
H5_DLLVAR hid_t H5E_LINK_g;          /* Link count failure */
H5_DLLVAR hid_t H5E_SLINK_g;         /* Symbolic link error */

/* File accessability errors */
#define H5E_FILEEXISTS       (H5OPEN H5E_FILEEXISTS_g)
#define H5E_FILEOPEN         (H5OPEN H5E_FILEOPEN_g)
#define H5E_CANTCREATE       (H5OPEN H5E_CANTCREATE_g)
#define H5E_CANTOPENFILE     (H5OPEN H5E_CANTOPENFILE_g)
#define H5E_CANTCLOSEFILE    (H5OPEN H5E_CANTCLOSEFILE_g)
#define H5E_NOTHDF5          (H5OPEN H5E_NOTHDF5_g)
#define H5E_BADFILE          (H5OPEN H5E_BADFILE_g)
#define H5E_TRUNCATED        (H5OPEN H5E_TRUNCATED_g)
#define H5E_MOUNT            (H5OPEN H5E_MOUNT_g)
H5_DLLVAR hid_t H5E_FILEEXISTS_g;    /* File already exists */
H5_DLLVAR hid_t H5E_FILEOPEN_g;      /* File already open */
H5_DLLVAR hid_t H5E_CANTCREATE_g;    /* Unable to create file */
H5_DLLVAR hid_t H5E_CANTOPENFILE_g;  /* Unable to open file */
H5_DLLVAR hid_t H5E_CANTCLOSEFILE_g; /* Unable to close file */
H5_DLLVAR hid_t H5E_NOTHDF5_g;       /* Not an HDF5 file */
H5_DLLVAR hid_t H5E_BADFILE_g;       /* Bad file ID accessed */
H5_DLLVAR hid_t H5E_TRUNCATED_g;     /* File has been truncated */
H5_DLLVAR hid_t H5E_MOUNT_g;         /* File mount error */

/* Object atom related errors */
#define H5E_BADATOM          (H5OPEN H5E_BADATOM_g)
#define H5E_BADGROUP         (H5OPEN H5E_BADGROUP_g)
#define H5E_CANTREGISTER     (H5OPEN H5E_CANTREGISTER_g)
#define H5E_CANTINC          (H5OPEN H5E_CANTINC_g)
#define H5E_CANTDEC          (H5OPEN H5E_CANTDEC_g)
#define H5E_NOIDS            (H5OPEN H5E_NOIDS_g)
H5_DLLVAR hid_t H5E_BADATOM_g;       /* Unable to find atom information (already closed?) */
H5_DLLVAR hid_t H5E_BADGROUP_g;      /* Unable to find ID group information */
H5_DLLVAR hid_t H5E_CANTREGISTER_g;  /* Unable to register new atom */
H5_DLLVAR hid_t H5E_CANTINC_g;       /* Unable to increment reference count */
H5_DLLVAR hid_t H5E_CANTDEC_g;       /* Unable to decrement reference count */
H5_DLLVAR hid_t H5E_NOIDS_g;         /* Out of IDs for group */

/* Cache related errors */
#define H5E_CANTFLUSH        (H5OPEN H5E_CANTFLUSH_g)
#define H5E_CANTSERIALIZE    (H5OPEN H5E_CANTSERIALIZE_g)
#define H5E_CANTLOAD         (H5OPEN H5E_CANTLOAD_g)
#define H5E_PROTECT          (H5OPEN H5E_PROTECT_g)
#define H5E_NOTCACHED        (H5OPEN H5E_NOTCACHED_g)
#define H5E_SYSTEM           (H5OPEN H5E_SYSTEM_g)
#define H5E_CANTINS          (H5OPEN H5E_CANTINS_g)
#define H5E_CANTRENAME       (H5OPEN H5E_CANTRENAME_g)
#define H5E_CANTPROTECT      (H5OPEN H5E_CANTPROTECT_g)
#define H5E_CANTUNPROTECT    (H5OPEN H5E_CANTUNPROTECT_g)
H5_DLLVAR hid_t H5E_CANTFLUSH_g;     /* Unable to flush data from cache */
H5_DLLVAR hid_t H5E_CANTSERIALIZE_g; /* Unable to serialize data from cache */
H5_DLLVAR hid_t H5E_CANTLOAD_g;      /* Unable to load metadata into cache */
H5_DLLVAR hid_t H5E_PROTECT_g;       /* Protected metadata error */
H5_DLLVAR hid_t H5E_NOTCACHED_g;     /* Metadata not currently cached */
H5_DLLVAR hid_t H5E_SYSTEM_g;        /* Internal error detected */
H5_DLLVAR hid_t H5E_CANTINS_g;       /* Unable to insert metadata into cache */
H5_DLLVAR hid_t H5E_CANTRENAME_g;    /* Unable to rename metadata */
H5_DLLVAR hid_t H5E_CANTPROTECT_g;   /* Unable to protect metadata */
H5_DLLVAR hid_t H5E_CANTUNPROTECT_g; /* Unable to unprotect metadata */

/* Parallel MPI errors */
#define H5E_MPI              (H5OPEN H5E_MPI_g)
#define H5E_MPIERRSTR        (H5OPEN H5E_MPIERRSTR_g)
H5_DLLVAR hid_t H5E_MPI_g;           /* Some MPI function failed */
H5_DLLVAR hid_t H5E_MPIERRSTR_g;     /* MPI Error String */

/* Dataspace errors */
#define H5E_CANTCLIP         (H5OPEN H5E_CANTCLIP_g)
#define H5E_CANTCOUNT        (H5OPEN H5E_CANTCOUNT_g)
#define H5E_CANTSELECT       (H5OPEN H5E_CANTSELECT_g)
#define H5E_CANTNEXT         (H5OPEN H5E_CANTNEXT_g)
#define H5E_BADSELECT        (H5OPEN H5E_BADSELECT_g)
#define H5E_CANTCOMPARE      (H5OPEN H5E_CANTCOMPARE_g)
H5_DLLVAR hid_t H5E_CANTCLIP_g;      /* Can't clip hyperslab region */
H5_DLLVAR hid_t H5E_CANTCOUNT_g;     /* Can't count elements */
H5_DLLVAR hid_t H5E_CANTSELECT_g;    /* Can't select hyperslab */
H5_DLLVAR hid_t H5E_CANTNEXT_g;      /* Can't move to next iterator location */
H5_DLLVAR hid_t H5E_BADSELECT_g;     /* Invalid selection */
H5_DLLVAR hid_t H5E_CANTCOMPARE_g;   /* Can't compare objects */

/* B-tree related errors */
#define H5E_NOTFOUND         (H5OPEN H5E_NOTFOUND_g)
#define H5E_EXISTS           (H5OPEN H5E_EXISTS_g)
#define H5E_CANTENCODE       (H5OPEN H5E_CANTENCODE_g)
#define H5E_CANTDECODE       (H5OPEN H5E_CANTDECODE_g)
#define H5E_CANTSPLIT        (H5OPEN H5E_CANTSPLIT_g)
#define H5E_CANTREDISTRIBUTE (H5OPEN H5E_CANTREDISTRIBUTE_g)
#define H5E_CANTSWAP         (H5OPEN H5E_CANTSWAP_g)
#define H5E_CANTINSERT       (H5OPEN H5E_CANTINSERT_g)
#define H5E_CANTLIST         (H5OPEN H5E_CANTLIST_g)
H5_DLLVAR hid_t H5E_NOTFOUND_g;      /* Object not found */
H5_DLLVAR hid_t H5E_EXISTS_g;        /* Object already exists */
H5_DLLVAR hid_t H5E_CANTENCODE_g;    /* Unable to encode value */
H5_DLLVAR hid_t H5E_CANTDECODE_g;    /* Unable to decode value */
H5_DLLVAR hid_t H5E_CANTSPLIT_g;     /* Unable to split node */
H5_DLLVAR hid_t H5E_CANTREDISTRIBUTE_g; /* Unable to redistribute records */
H5_DLLVAR hid_t H5E_CANTSWAP_g;      /* Unable to swap records */
H5_DLLVAR hid_t H5E_CANTINSERT_g;    /* Unable to insert object */
H5_DLLVAR hid_t H5E_CANTLIST_g;      /* Unable to list node */

/* Argument errors */
#define H5E_UNINITIALIZED    (H5OPEN H5E_UNINITIALIZED_g)
#define H5E_UNSUPPORTED      (H5OPEN H5E_UNSUPPORTED_g)
#define H5E_BADTYPE          (H5OPEN H5E_BADTYPE_g)
#define H5E_BADRANGE         (H5OPEN H5E_BADRANGE_g)
#define H5E_BADVALUE         (H5OPEN H5E_BADVALUE_g)
H5_DLLVAR hid_t H5E_UNINITIALIZED_g; /* Information is uinitialized */
H5_DLLVAR hid_t H5E_UNSUPPORTED_g;   /* Feature is unsupported */
H5_DLLVAR hid_t H5E_BADTYPE_g;       /* Inappropriate type */
H5_DLLVAR hid_t H5E_BADRANGE_g;      /* Out of range */
H5_DLLVAR hid_t H5E_BADVALUE_g;      /* Bad value */

/* Datatype conversion errors */
#define H5E_CANTCONVERT      (H5OPEN H5E_CANTCONVERT_g)
#define H5E_BADSIZE          (H5OPEN H5E_BADSIZE_g)
H5_DLLVAR hid_t H5E_CANTCONVERT_g;   /* Can't convert datatypes */
H5_DLLVAR hid_t H5E_BADSIZE_g;       /* Bad size for object */

#endif /* H5Epubgen_H */
