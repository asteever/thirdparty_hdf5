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

/*
 * This file contains public declarations for the H5F module.
 */
#ifndef _H5Fpublic_H
#define _H5Fpublic_H

/* Public header files needed by this file */
#include "H5public.h"
#include "H5Ipublic.h"

/*
 * These are the bits that can be passed to the `flags' argument of
 * H5Fcreate() and H5Fopen(). Use the bit-wise OR operator (|) to combine
 * them as needed.  As a side effect, they call H5check_version() to make sure
 * that the application is compiled with a version of the hdf5 header files
 * which are compatible with the library to which the application is linked.
 * We're assuming that these constants are used rather early in the hdf5
 * session.
 */
#define H5F_ACC_RDONLY	(H5check(),0x0000u)	/*absence of rdwr => rd-only */
#define H5F_ACC_RDWR	(H5check(),0x0001u)	/*open for read and write    */
#define H5F_ACC_TRUNC	(H5check(),0x0002u)	/*overwrite existing files   */
#define H5F_ACC_EXCL	(H5check(),0x0004u)	/*fail if file already exists*/
#define H5F_ACC_DEBUG	(H5check(),0x0008u)	/*print debug info	     */
#define H5F_ACC_CREAT	(H5check(),0x0010u)	/*create non-existing files  */

#define H5F_OBJ_FILE	(0x0001u)
#define H5F_OBJ_DATASET	(0x0002u)
#define H5F_OBJ_GROUP	(0x0004u)
#define H5F_OBJ_DATATYPE (0x0008u)
#define H5F_OBJ_ALL 	(H5F_OBJ_FILE|H5F_OBJ_DATASET|H5F_OBJ_GROUP|H5F_OBJ_DATATYPE)

#ifdef H5_HAVE_PARALLEL
/*
 * Use this constant string as the MPI_Info key to set H5Fmpio debug flags.
 * To turn on H5Fmpio debug flags, set the MPI_Info value with this key to
 * have the value of a string consisting of the characters that turn on the
 * desired flags.
 */
#define H5F_MPIO_DEBUG_KEY "H5F_mpio_debug_key"
#endif /* H5_HAVE_PARALLEL */

/* The difference between a single file and a set of mounted files */
typedef enum H5F_scope_t {
    H5F_SCOPE_LOCAL	= 0,	/*specified file handle only		*/
    H5F_SCOPE_GLOBAL	= 1,	/*entire virtual file			*/
    H5F_SCOPE_DOWN      = 2	/*for internal use only			*/
} H5F_scope_t;

/* Unlimited file size for H5Pset_external() */
#define H5F_UNLIMITED	((hsize_t)(-1L))

/* How does file close behave?
 * H5F_CLOSE_DEFAULT - Use the degree pre-defined by underlining VFL
 * H5F_CLOSE_WEAK    - file closes only after all opened objects are closed 
 * H5F_CLOSE_SEMI    - if no opened objects, file is close; otherwise, file
		       close fails
 * H5F_CLOSE_STRONG  - if there are opened objects, close them first, then
		       close file
 */
typedef enum H5F_close_degree_t {
    H5F_CLOSE_DEFAULT   = 0,
    H5F_CLOSE_WEAK      = 1,
    H5F_CLOSE_SEMI      = 2,
    H5F_CLOSE_STRONG    = 3
} H5F_close_degree_t;

#ifdef __cplusplus
extern "C" {
#endif

/* Functions in H5F.c */
__DLL__ htri_t H5Fis_hdf5 (const char *filename);
__DLL__ hid_t  H5Fcreate (const char *filename, unsigned flags,
		  	  hid_t create_plist, hid_t access_plist);
__DLL__ hid_t  H5Fopen (const char *filename, unsigned flags,
		        hid_t access_plist);
__DLL__ hid_t  H5Freopen(hid_t file_id);
__DLL__ herr_t H5Fflush(hid_t object_id, H5F_scope_t scope);
__DLL__ herr_t H5Fclose (hid_t file_id);
__DLL__ hid_t  H5Fget_create_plist (hid_t file_id);
__DLL__ hid_t  H5Fget_access_plist (hid_t file_id);
__DLL__ herr_t H5Fget_obj_count(hid_t file_id, unsigned types, 
                                unsigned *obj_id_count);
__DLL__ herr_t H5Fget_obj_ids(hid_t file_id, unsigned types, hid_t *obj_id_list);
__DLL__ herr_t H5Fmount(hid_t loc, const char *name, hid_t child, hid_t plist);
__DLL__ herr_t H5Funmount(hid_t loc, const char *name);

#ifdef __cplusplus
}
#endif
#endif
