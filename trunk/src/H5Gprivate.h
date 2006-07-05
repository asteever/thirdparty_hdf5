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

/*-------------------------------------------------------------------------
 *
 * Created:             H5Gprivate.h
 *                      Jul 11 1997
 *                      Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:             Library-visible declarations.
 *
 *-------------------------------------------------------------------------
 */

#ifndef _H5Gprivate_H
#define _H5Gprivate_H

/* Include package's public header */
#include "H5Gpublic.h"

/* Private headers needed by this file */
#include "H5private.h"		/* Generic Functions			*/
#include "H5Bprivate.h"		/* B-trees				*/
#include "H5Fprivate.h"		/* File access				*/
#include "H5RSprivate.h"        /* Reference-counted strings            */

/*
 * Define this to enable debugging.
 */
#ifdef NDEBUG
#  undef H5G_DEBUG
#endif

#define H5G_NODE_MAGIC  "SNOD"          /*symbol table node magic number     */
#define H5G_NODE_SIZEOF_MAGIC 4         /*sizeof symbol node magic number    */

/*
 * The disk size for a symbol table entry...
 */
#define H5G_SIZEOF_SCRATCH      16
#define H5G_SIZEOF_ENTRY(F)                                                   \
   (H5F_SIZEOF_SIZE(F) +        /*offset of name into heap              */    \
    H5F_SIZEOF_ADDR(F) +        /*address of object header              */    \
    4 +                         /*entry type                            */    \
    4 +				/*reserved				*/    \
    H5G_SIZEOF_SCRATCH)         /*scratch pad space                     */

/* ========= Group Creation properties ============ */

/* Defaults for group info values */
#define H5G_CRT_GINFO_LHEAP_SIZE_HINT           0
#define H5G_CRT_GINFO_MAX_COMPACT               8
#define H5G_CRT_GINFO_MIN_DENSE                 6
#define H5G_CRT_GINFO_EST_NUM_ENTRIES           4
#define H5G_CRT_GINFO_EST_NAME_LEN              8

/* Definitions for group info settings */
#define H5G_CRT_GROUP_INFO_NAME                 "group info"
#define H5G_CRT_GROUP_INFO_SIZE                 sizeof(H5O_ginfo_t)
#define H5G_CRT_GROUP_INFO_DEF                  {H5G_CRT_GINFO_LHEAP_SIZE_HINT, \
                                                    H5G_CRT_GINFO_MAX_COMPACT, \
                                                    H5G_CRT_GINFO_MIN_DENSE, \
                                                    H5G_CRT_GINFO_EST_NUM_ENTRIES, \
                                                    H5G_CRT_GINFO_EST_NAME_LEN}

/* definitions for copying objects */
#define H5G_CPY_OPTION_NAME 			"copy object"
#define H5G_CPY_OPTION_SIZE			sizeof(unsigned)
#define H5G_CPY_OPTION_DEF			0

/* Type of operation being performed for call to H5G_name_replace() */
typedef enum {
    H5G_NAME_MOVE = 0,          /* H5*move call    */
    H5G_NAME_UNLINK,            /* H5Lunlink call  */
    H5G_NAME_MOUNT,             /* H5Fmount call   */
    H5G_NAME_UNMOUNT            /* H5Funmount call */
} H5G_names_op_t;

/* Structure to store information about the name an object was opened with */
typedef struct {
    H5RS_str_t  *full_path_r;           /* Path to object, as seen from root of current file mounting hierarchy */
    H5RS_str_t  *user_path_r;           /* Path to object, as opened by user */
    unsigned    obj_hidden;             /* Whether the object is visible in group hier. */
} H5G_name_t;

/* Forward declarations (for prototypes & struct definitions) */
struct H5P_genplist_t;
struct H5O_loc_t;
struct H5O_t;

/*
 * The "location" of an object in a group hierarchy.  This points to an object
 * location and a group hierarchy path for the object.
 */
typedef struct {
    struct H5O_loc_t *oloc;             /* Object header location            */
    H5G_name_t *path;                   /* Group hierarchy path              */
} H5G_loc_t;

typedef struct H5G_t H5G_t;
typedef struct H5G_shared_t H5G_shared_t;

/*
 * Library prototypes...  These are the ones that other packages routinely
 * call.
 */
H5_DLL herr_t H5G_mkroot(H5F_t *f, hid_t dxpl_id, H5G_loc_t *root_loc);
H5_DLL struct H5O_loc_t *H5G_oloc(H5G_t *grp);
H5_DLL H5G_name_t * H5G_nameof(H5G_t *grp);
H5_DLL H5F_t *H5G_fileof(H5G_t *grp);
H5_DLL herr_t H5G_free(H5G_t *grp);
H5_DLL H5G_t *H5G_open(H5G_loc_t *loc, hid_t dxpl_id);
H5_DLL herr_t H5G_close(H5G_t *grp);
H5_DLL herr_t H5G_get_objinfo(const H5G_loc_t *loc, const char *name,
    hbool_t follow_link, H5G_stat_t *statbuf/*out*/, hid_t dxpl_id);
H5_DLL H5F_t *H5G_insertion_file(H5G_loc_t *loc, const char *name, hid_t dxpl_id);
H5_DLL herr_t H5G_free_grp_name(H5G_t *grp);
H5_DLL herr_t H5G_get_shared_count(H5G_t *grp);
H5_DLL herr_t H5G_mount(H5G_t *grp);
H5_DLL herr_t H5G_unmount(H5G_t *grp);

/*
 * These functions operate on symbol table nodes.
 */
H5_DLL herr_t H5G_node_close(const H5F_t *f);
H5_DLL herr_t H5G_node_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr, FILE *stream,
			      int indent, int fwidth, haddr_t heap);

/*
 * These functions operate on group object locations.
 */
/* forward reference for later use */
H5_DLL herr_t H5G_obj_ent_decode(H5F_t *f, const uint8_t **pp,
    struct H5O_loc_t *oloc);
H5_DLL herr_t H5G_obj_ent_encode(H5F_t *f, uint8_t **pp,
    const struct H5O_loc_t *oloc);

/*
 * These functions operate on group hierarchy names.
 */
H5_DLL  herr_t H5G_name_replace(H5G_obj_t type, H5G_loc_t *loc,
        H5RS_str_t *dst_name, H5G_loc_t *dst_loc, H5G_names_op_t op);
H5_DLL herr_t H5G_name_reset(H5G_name_t *name);
H5_DLL herr_t H5G_name_copy(H5G_name_t *dst, const H5G_name_t *src, H5_copy_depth_t depth);
H5_DLL herr_t H5G_name_free(H5G_name_t *name);
H5_DLL ssize_t H5G_get_name(hid_t id, char *name/*out*/, size_t size);

/*
 * These functions operate on group "locations"
 */
H5_DLL herr_t H5G_loc(hid_t loc_id, H5G_loc_t *loc);
H5_DLL herr_t H5G_loc_find(H5G_loc_t *loc, const char *name,
    H5G_loc_t *obj_loc/*out*/, hid_t dxpl_id);
H5_DLL herr_t H5G_loc_reset(H5G_loc_t *loc);
H5_DLL herr_t H5G_loc_free(H5G_loc_t *loc);

#endif

