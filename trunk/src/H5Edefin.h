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


#ifndef _H5Edefin_H
#define _H5Edefin_H

/* Major error IDs */
hid_t H5E_DATASET_g        = FAIL;      /* Dataset */
hid_t H5E_FUNC_g           = FAIL;      /* Function entry/exit */
hid_t H5E_STORAGE_g        = FAIL;      /* Data storage */
hid_t H5E_FILE_g           = FAIL;      /* File accessability */
hid_t H5E_FPHDF5_g         = FAIL;      /* Flexible Parallel HDF5 */
hid_t H5E_SYM_g            = FAIL;      /* Symbol table */
hid_t H5E_VFL_g            = FAIL;      /* Virtual File Layer */
hid_t H5E_INTERNAL_g       = FAIL;      /* Internal error (too specific to document in detail) */
hid_t H5E_BTREE_g          = FAIL;      /* B-Tree node */
hid_t H5E_REFERENCE_g      = FAIL;      /* References */
hid_t H5E_DATASPACE_g      = FAIL;      /* Dataspace */
hid_t H5E_RESOURCE_g       = FAIL;      /* Resource unavailable */
hid_t H5E_PLIST_g          = FAIL;      /* Property lists */
hid_t H5E_DATATYPE_g       = FAIL;      /* Datatype */
hid_t H5E_RS_g             = FAIL;      /* Reference Counted Strings */
hid_t H5E_HEAP_g           = FAIL;      /* Heap */
hid_t H5E_OHDR_g           = FAIL;      /* Object header */
hid_t H5E_TBBT_g           = FAIL;      /* Threaded, Balanced, Binary Trees */
hid_t H5E_ATOM_g           = FAIL;      /* Object atom */
hid_t H5E_ATTR_g           = FAIL;      /* Attribute */
hid_t H5E_IO_g             = FAIL;      /* Low-level I/O */
hid_t H5E_SLIST_g          = FAIL;      /* Skip Lists */
hid_t H5E_EFL_g            = FAIL;      /* External file list */
hid_t H5E_TST_g            = FAIL;      /* Ternary Search Trees */
hid_t H5E_ARGS_g           = FAIL;      /* Invalid arguments to routine */
hid_t H5E_ERROR_g          = FAIL;      /* Error API */
hid_t H5E_PLINE_g          = FAIL;      /* Data filters */
hid_t H5E_CACHE_g          = FAIL;      /* Object cache */

/* Minor error IDs */

/* Threaded, balanced binary tree errors */
hid_t H5E_CANTMAKETREE_g   = FAIL;      /* Can't create a binary tree node */

/* Generic low-level file I/O errors */
hid_t H5E_SEEKERROR_g      = FAIL;      /* Seek failed */
hid_t H5E_READERROR_g      = FAIL;      /* Read failed */
hid_t H5E_WRITEERROR_g     = FAIL;      /* Write failed */
hid_t H5E_CLOSEERROR_g     = FAIL;      /* Close failed */
hid_t H5E_OVERFLOW_g       = FAIL;      /* Address overflowed */
hid_t H5E_FCNTL_g          = FAIL;      /* File control (fcntl) failed */

/* Resource errors */
hid_t H5E_NOSPACE_g        = FAIL;      /* No space available for allocation */
hid_t H5E_CANTCOPY_g       = FAIL;      /* Unable to copy object */
hid_t H5E_CANTFREE_g       = FAIL;      /* Unable to free object */
hid_t H5E_ALREADYEXISTS_g  = FAIL;      /* Object already exists */
hid_t H5E_CANTLOCK_g       = FAIL;      /* Unable to lock object */
hid_t H5E_CANTUNLOCK_g     = FAIL;      /* Unable to unlock object */
hid_t H5E_CANTGC_g         = FAIL;      /* Unable to garbage collect */
hid_t H5E_CANTGETSIZE_g    = FAIL;      /* Unable to compute size */

/* Heap errors */
hid_t H5E_CANTRESTORE_g    = FAIL;      /* Can't restore condition */

/* Function entry/exit interface errors */
hid_t H5E_CANTINIT_g       = FAIL;      /* Unable to initialize object */
hid_t H5E_ALREADYINIT_g    = FAIL;      /* Object already initialized */
hid_t H5E_CANTRELEASE_g    = FAIL;      /* Unable to release object */

/* Property list errors */
hid_t H5E_CANTGET_g        = FAIL;      /* Can't get value */
hid_t H5E_CANTSET_g        = FAIL;      /* Can't set value */
hid_t H5E_DUPCLASS_g       = FAIL;      /* Duplicate class name in parent class */

/* Object header related errors */
hid_t H5E_LINKCOUNT_g      = FAIL;      /* Bad object header link count */
hid_t H5E_VERSION_g        = FAIL;      /* Wrong version number */
hid_t H5E_ALIGNMENT_g      = FAIL;      /* Alignment error */
hid_t H5E_BADMESG_g        = FAIL;      /* Unrecognized message */
hid_t H5E_CANTDELETE_g     = FAIL;      /* Can't delete message */

/* FPHDF5 errors */
hid_t H5E_CANTRECV_g       = FAIL;      /* Can't receive messages from processes */
hid_t H5E_CANTSENDMDATA_g  = FAIL;      /* Can't send metadata message */
hid_t H5E_CANTCHANGE_g     = FAIL;      /* Can't register change with server */
hid_t H5E_CANTALLOC_g      = FAIL;      /* Can't allocate from file */

/* I/O pipeline errors */
hid_t H5E_NOFILTER_g       = FAIL;      /* Requested filter is not available */
hid_t H5E_CALLBACK_g       = FAIL;      /* Callback failed */
hid_t H5E_CANAPPLY_g       = FAIL;      /* Error from filter 'can apply' callback */
hid_t H5E_SETLOCAL_g       = FAIL;      /* Error from filter 'set local' callback */
hid_t H5E_NOENCODER_g      = FAIL;      /* Filter present but encoding disabled */

/* Group related errors */
hid_t H5E_CANTOPENOBJ_g    = FAIL;      /* Can't open object */
hid_t H5E_CANTCLOSEOBJ_g   = FAIL;      /* Can't close object */
hid_t H5E_COMPLEN_g        = FAIL;      /* Name component is too long */
hid_t H5E_CWG_g            = FAIL;      /* Problem with current working group */
hid_t H5E_LINK_g           = FAIL;      /* Link count failure */
hid_t H5E_SLINK_g          = FAIL;      /* Symbolic link error */

/* File accessability errors */
hid_t H5E_FILEEXISTS_g     = FAIL;      /* File already exists */
hid_t H5E_FILEOPEN_g       = FAIL;      /* File already open */
hid_t H5E_CANTCREATE_g     = FAIL;      /* Unable to create file */
hid_t H5E_CANTOPENFILE_g   = FAIL;      /* Unable to open file */
hid_t H5E_CANTCLOSEFILE_g  = FAIL;      /* Unable to close file */
hid_t H5E_NOTHDF5_g        = FAIL;      /* Not an HDF5 file */
hid_t H5E_BADFILE_g        = FAIL;      /* Bad file ID accessed */
hid_t H5E_TRUNCATED_g      = FAIL;      /* File has been truncated */
hid_t H5E_MOUNT_g          = FAIL;      /* File mount error */

/* Object atom related errors */
hid_t H5E_BADATOM_g        = FAIL;      /* Unable to find atom information (already closed?) */
hid_t H5E_BADGROUP_g       = FAIL;      /* Unable to find ID group information */
hid_t H5E_CANTREGISTER_g   = FAIL;      /* Unable to register new atom */
hid_t H5E_CANTINC_g        = FAIL;      /* Unable to increment reference count */
hid_t H5E_CANTDEC_g        = FAIL;      /* Unable to decrement reference count */
hid_t H5E_NOIDS_g          = FAIL;      /* Out of IDs for group */

/* Cache related errors */
hid_t H5E_CANTFLUSH_g      = FAIL;      /* Unable to flush data from cache */
hid_t H5E_CANTSERIALIZE_g  = FAIL;      /* Unable to serialize data from cache */
hid_t H5E_CANTLOAD_g       = FAIL;      /* Unable to load metadata into cache */
hid_t H5E_PROTECT_g        = FAIL;      /* Protected metadata error */
hid_t H5E_NOTCACHED_g      = FAIL;      /* Metadata not currently cached */
hid_t H5E_SYSTEM_g         = FAIL;      /* Internal error detected */
hid_t H5E_CANTINS_g        = FAIL;      /* Unable to insert metadata into cache */
hid_t H5E_CANTRENAME_g     = FAIL;      /* Unable to rename metadata */
hid_t H5E_CANTPROTECT_g    = FAIL;      /* Unable to protect metadata */
hid_t H5E_CANTUNPROTECT_g  = FAIL;      /* Unable to unprotect metadata */

/* Parallel MPI errors */
hid_t H5E_MPI_g            = FAIL;      /* Some MPI function failed */
hid_t H5E_MPIERRSTR_g      = FAIL;      /* MPI Error String */

/* Dataspace errors */
hid_t H5E_CANTCLIP_g       = FAIL;      /* Can't clip hyperslab region */
hid_t H5E_CANTCOUNT_g      = FAIL;      /* Can't count elements */
hid_t H5E_CANTSELECT_g     = FAIL;      /* Can't select hyperslab */
hid_t H5E_CANTNEXT_g       = FAIL;      /* Can't move to next iterator location */
hid_t H5E_BADSELECT_g      = FAIL;      /* Invalid selection */
hid_t H5E_CANTCOMPARE_g    = FAIL;      /* Can't compare objects */

/* B-tree related errors */
hid_t H5E_NOTFOUND_g       = FAIL;      /* Object not found */
hid_t H5E_EXISTS_g         = FAIL;      /* Object already exists */
hid_t H5E_CANTENCODE_g     = FAIL;      /* Unable to encode value */
hid_t H5E_CANTDECODE_g     = FAIL;      /* Unable to decode value */
hid_t H5E_CANTSPLIT_g      = FAIL;      /* Unable to split node */
hid_t H5E_CANTINSERT_g     = FAIL;      /* Unable to insert object */
hid_t H5E_CANTLIST_g       = FAIL;      /* Unable to list node */

/* Argument errors */
hid_t H5E_UNINITIALIZED_g  = FAIL;      /* Information is uinitialized */
hid_t H5E_UNSUPPORTED_g    = FAIL;      /* Feature is unsupported */
hid_t H5E_BADTYPE_g        = FAIL;      /* Inappropriate type */
hid_t H5E_BADRANGE_g       = FAIL;      /* Out of range */
hid_t H5E_BADVALUE_g       = FAIL;      /* Bad value */

/* Datatype conversion errors */
hid_t H5E_CANTCONVERT_g    = FAIL;      /* Can't convert datatypes */
hid_t H5E_BADSIZE_g        = FAIL;      /* Bad size for object */

#endif /* H5Edefin_H */
