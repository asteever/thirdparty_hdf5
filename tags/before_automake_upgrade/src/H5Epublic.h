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
 * This file contains public declarations for the H5E module.
 */
#ifndef _H5Epublic_H
#define _H5Epublic_H

#include <stdio.h>              /*FILE arg of H5Eprint()                     */

/* Public headers needed by this file */
#include "H5public.h"
#include "H5Ipublic.h"

/* Value for the default error stack */
#define H5E_DEFAULT             0

/* Different kinds of error information */
typedef enum H5E_type_t {
    H5E_MAJOR,
    H5E_MINOR
} H5E_type_t;

/* For backward compatibility with v1.6 */
typedef hid_t   H5E_major_t;
typedef hid_t   H5E_minor_t;

/* Information about an error; element of error stack */
typedef struct H5E_error_t {
    hid_t       cls_id;         /*class ID                           */
    hid_t       maj_num;	/*major error ID		     */
    hid_t       min_num;	/*minor error number		     */
    const char	*func_name;   	/*function in which error occurred   */
    const char	*file_name;	/*file in which error occurred       */
    unsigned	line;		/*line in file where error occurs    */
    const char	*desc;		/*optional supplied description      */
} H5E_error_t;

/* When this header is included from a private header, don't make calls to H5open() */
#undef H5OPEN
#ifndef _H5private_H
#define H5OPEN          H5open(),
#else   /* _H5private_H */
#define H5OPEN
#endif  /* _H5private_H */

/* HDF5 error class */
#define H5E_ERR_CLS		(H5OPEN H5E_ERR_CLS_g)
H5_DLLVAR hid_t H5E_ERR_CLS_g;

/* Include the automatically generated public header information */
/* (This includes the list of major and minor error codes for the library) */
#include "H5Epubgen.h"

/*
 * One often needs to temporarily disable automatic error reporting when
 * trying something that's likely or expected to fail.  For instance, to
 * determine if an object exists one can call H5Gget_objinfo() which will fail if
 * the object doesn't exist.  The code to try can be nested between calls to
 * H5Eget_auto() and H5Eset_auto(), but it's easier just to use this macro
 * like:
 * 	H5E_BEGIN_TRY {
 *	    ...stuff here that's likely to fail...
 *      } H5E_END_TRY;
 *
 * Warning: don't break, return, or longjmp() from the body of the loop or
 *	    the error reporting won't be properly restored!
 *
 * These two macros still use the old API functions for backward compatibility
 * purpose.
 */
#define H5E_BEGIN_TRY {							      \
    unsigned H5E_saved_is_stack;					      \
    union {								      \
        H5E_auto_stack_t stack_efunc;					      \
        H5E_auto_t efunc;						      \
    } H5E_saved;							      \
    void *H5E_saved_edata;						      \
								    	      \
    (void)H5Eauto_is_stack(H5E_DEFAULT, &H5E_saved_is_stack);		      \
    if(H5E_saved_is_stack) {						      \
        (void)H5Eget_auto_stack(H5E_DEFAULT, &H5E_saved.stack_efunc, &H5E_saved_edata); \
        (void)H5Eset_auto_stack(H5E_DEFAULT, NULL, NULL);		      \
    } else {								      \
        (void)H5Eget_auto(&H5E_saved.efunc, &H5E_saved_edata);		      \
        (void)H5Eset_auto(NULL, NULL);					      \
    }

#define H5E_END_TRY							      \
    if(H5E_saved_is_stack) {						      \
        (void)H5Eset_auto_stack(H5E_DEFAULT, H5E_saved.stack_efunc, H5E_saved_edata); \
    } else {								      \
        (void)H5Eset_auto(H5E_saved.efunc, H5E_saved_edata);		      \
    }									      \
}

/*
 * Public API Convenience Macros for Error reporting - Documented
 */
/* Use the Standard C __FILE__ & __LINE__ macros instead of typing them in */
#define H5Epush_sim(func,cls,maj,min,str) H5Epush_stack(H5E_DEFAULT,__FILE__,func,__LINE__,cls,maj,min,str)

/*
 * Public API Convenience Macros for Error reporting - Undocumented
 */
/* Use the Standard C __FILE__ & __LINE__ macros instead of typing them in */
/*  And return after pushing error onto stack */
#define H5Epush_ret(func,cls,maj,min,str,ret) {         \
    H5Epush_stack(H5E_DEFAULT,__FILE__,func,__LINE__,cls,maj,min,str);    \
    return(ret);                                    \
}

/* Use the Standard C __FILE__ & __LINE__ macros instead of typing them in
 * And goto a label after pushing error onto stack.
 */
#define H5Epush_goto(func,cls,maj,min,str,label) {      \
    H5Epush_stack(H5E_DEFAULT,__FILE__,func,__LINE__,cls,maj,min,str);    \
    goto label;                                   \
}

/* Error stack traversal direction */
typedef enum H5E_direction_t {
    H5E_WALK_UPWARD	= 0,		/*begin deep, end at API function    */
    H5E_WALK_DOWNWARD	= 1		/*begin at API function, end deep    */
} H5E_direction_t;


#ifdef __cplusplus
extern "C" {
#endif

/* Error stack traversal callback function pointers */
typedef herr_t (*H5E_walk_t)(unsigned n, const H5E_error_t *err_desc, void *client_data);
typedef herr_t (*H5E_auto_t)(void *client_data);
typedef herr_t (*H5E_auto_stack_t)(hid_t estack, void *client_data);

/* Public API functions */
H5_DLL hid_t  H5Eregister_class(const char *cls_name, const char *lib_name, const char *version);
H5_DLL herr_t H5Eunregister_class(hid_t class_id);
H5_DLL herr_t H5Eclose_msg(hid_t err_id);
H5_DLL hid_t  H5Ecreate_msg(hid_t cls, H5E_type_t msg_type, const char *msg);
H5_DLL hid_t  H5Eget_current_stack(void);
H5_DLL herr_t H5Eclose_stack(hid_t stack_id);
H5_DLL ssize_t H5Eget_class_name(hid_t class_id, char *name, size_t size);
H5_DLL ssize_t H5Eget_msg(hid_t msg_id, H5E_type_t *type, char *msg, size_t size);
H5_DLL int    H5Eget_num(hid_t error_stack_id);
H5_DLL herr_t H5Eset_current_stack(hid_t err_stack_id);
H5_DLL herr_t H5Epop(hid_t err_stack, size_t count);
H5_DLL herr_t H5Eauto_is_stack(hid_t err_stack, unsigned *is_stack);

/* These old APIs are kept for backward compatibility.  They don't have
 * the error stack in the parameters. */
H5_DLL herr_t H5Epush(const char *file, const char *func, unsigned line, 
                        H5E_major_t maj, H5E_minor_t min, const char *str);
H5_DLL herr_t  H5Eprint(FILE *stream);
H5_DLL herr_t  H5Ewalk(H5E_direction_t direction, H5E_walk_t func, 
                            void *client_data);
H5_DLL herr_t  H5Eget_auto(H5E_auto_t *func, void **client_data);
H5_DLL herr_t  H5Eset_auto(H5E_auto_t func, void *client_data);
H5_DLL herr_t  H5Eclear(void);
H5_DLL const char * H5Eget_major(H5E_major_t maj);
H5_DLL const char * H5Eget_minor(H5E_minor_t min);

/* New APIs function the same as the old ones above, with the error stack 
 * in the parameters */
H5_DLL herr_t  H5Epush_stack(hid_t err_stack, const char *file, const char *func, unsigned line, 
                        hid_t cls_id, hid_t maj_id, hid_t min_id, const char *msg, ...);
H5_DLL herr_t  H5Eprint_stack(hid_t err_stack, FILE *stream);
H5_DLL herr_t  H5Ewalk_stack(hid_t err_stack, H5E_direction_t direction, H5E_walk_t func, 
                            void *client_data);
H5_DLL herr_t  H5Eget_auto_stack(hid_t estack_id, H5E_auto_stack_t *func, void **client_data);
H5_DLL herr_t  H5Eset_auto_stack(hid_t estack_id, H5E_auto_stack_t func, void *client_data);
H5_DLL herr_t  H5Eclear_stack(hid_t err_stack);
#ifdef __cplusplus
}
#endif
#endif
