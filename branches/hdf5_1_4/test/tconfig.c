/****************************************************************************
 * NCSA HDF                                                                 *
 * Scientic Data Team							    *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * COPYING file.                                                            *
 *                                                                          *
 ****************************************************************************/

/* $Id$ */

/***********************************************************
*
* Test program:  tconfig
*
* Test the definitions in the H5config.h as much as possible
*
*************************************************************/

#include "hdf5.h"
#include "H5private.h"
#include "testhdf5.h"

/* macros definitions */
/* verify C type sizes */
#define vrfy_ctype(ctype, ctype_macro) \
    if (sizeof(ctype) != ctype_macro){ \
	print_func("Error verifying %s expected: %d, got: %d\n", \
	    #ctype_macro, ctype_macro, sizeof(ctype)); \
	    num_errs++; \
    }

/* local routine prototypes */
void test_config_ctypes(void);
void test_config_malloc(void);


/*-------------------------------------------------------------------------
 * Function:	test_configure
 *
 * Purpose:	Main configure definitions testing routine
 *
 * Return:	none (error is fed back via global variable num_errs)
 *
 * Programmer:	Albert Cheng
 *              September 25, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void 
test_configure(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing configure definitions\n"));
    test_config_ctypes();
    test_config_malloc();
}


/*-------------------------------------------------------------------------
 * Function:	cleanup_configure
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Albert Cheng
 *              September 25, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_configure(void)
{
    /* no file to clean */
}


/*-------------------------------------------------------------------------
 * Function:	test_config_ctypes
 *
 * Purpose:	test C language data type sizes
 *
 * Return:	none (error is fed back via global variable num_errs)
 *
 * Programmer:	Albert Cheng
 *              September 25, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void 
test_config_ctypes(void)
{
    /* standard basic types */
    vrfy_ctype(char, SIZEOF_CHAR);
    vrfy_ctype(int, SIZEOF_INT);
    vrfy_ctype(short, SIZEOF_SHORT);
    vrfy_ctype(long, SIZEOF_LONG);
    vrfy_ctype(float, SIZEOF_FLOAT);
    vrfy_ctype(double, SIZEOF_DOUBLE);

    /* non-standard basic types */
#if SIZEOF_LONG_LONG > 0
    vrfy_ctype(long_long, SIZEOF_LONG_LONG);
#endif

#if SIZEOF_LONG_DOUBLE > 0
    vrfy_ctype(long double, SIZEOF_LONG_DOUBLE);
#endif

#if SIZEOF_UINT8_T > 0
    vrfy_ctype(uint8_t, SIZEOF_UINT8_T);
#endif

#if SIZEOF_UINT16_T > 0
    vrfy_ctype(uint16_t, SIZEOF_UINT16_T);
#endif

#if SIZEOF_UINT32_T > 0
    vrfy_ctype(uint32_t, SIZEOF_UINT32_T);
#endif

#if SIZEOF_UINT64_T > 0
    vrfy_ctype(uint64_t, SIZEOF_UINT64_T);
#endif

#if SIZEOF_UINT_FAST8_T > 0
    vrfy_ctype(uint_fast8_t, SIZEOF_UINT_FAST8_T);
#endif

#if SIZEOF_UINT_FAST16_T > 0
    vrfy_ctype(uint_fast16_t, SIZEOF_UINT_FAST16_T);
#endif

#if SIZEOF_UINT_FAST32_T > 0
    vrfy_ctype(uint_fast32_t, SIZEOF_UINT_FAST32_T);
#endif

#if SIZEOF_UINT_FAST64_T > 0
    vrfy_ctype(uint_fast64_t, SIZEOF_UINT_FAST64_T);
#endif

#if SIZEOF_UINT_LEAST8_T > 0
    vrfy_ctype(uint_least8_t, SIZEOF_UINT_LEAST8_T);
#endif

#if SIZEOF_UINT_LEAST16_T > 0
    vrfy_ctype(uint_least16_t, SIZEOF_UINT_LEAST16_T);
#endif

#if SIZEOF_UINT_LEAST32_T > 0
    vrfy_ctype(uint_least32_t, SIZEOF_UINT_LEAST32_T);
#endif

#if SIZEOF_UINT_LEAST64_T > 0
    vrfy_ctype(uint_least64_t, SIZEOF_UINT_LEAST64_T);
#endif

    /* pseudo standard basic types */
#if SIZEOF___INT64 > 0
    vrfy_ctype(__int64, SIZEOF___INT64);
#endif

#if SIZEOF_OFF_T > 0
    vrfy_ctype(off_t, SIZEOF_OFF_T);
#endif

#if SIZEOF_SIZE_T > 0
    vrfy_ctype(size_t, SIZEOF_SIZE_T);
#endif

#if SIZEOF_SSIZE_T > 0
    vrfy_ctype(ssize_t, SIZEOF_SSIZE_T);
#endif

}


/*-------------------------------------------------------------------------
 * Function:	test_config_malloc
 *
 * Purpose:	test C language malloc function
 *
 * Return:	none (error is fed back via global variable num_errs)
 *
 * Programmer:	Albert Cheng
 *              April 13, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void 
test_config_malloc(void)
{
    char	*pt;
    size_t	n;

    /* verify H5_MALLOC_WORKS (malloc zero byte) macros */
    pt = malloc(0);

#ifdef H5_MALLOC_WORKS
    if (pt==NULL){
	print_func("Error verifying H5_MALLOC_WORKS: "
	    "expected non-NULL, got NULL\n");
	num_errs++;
    }
#else
    if (pt!=NULL){
	print_func("Error verifying H5_MALLOC_WORKS: "
	    "expected NULL, got non-NULL\n");
	num_errs++;
    }
#endif
}
