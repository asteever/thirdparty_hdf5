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
 * Module Info:	Data type conversions for the H5T interface.
 */

#define H5T_PACKAGE		/*suppress error about including H5Tpkg	     */

/* Pablo information */
/* (Put before include files to avoid problems with inline functions) */
#define PABLO_MASK    H5Tconv_mask

#include "H5private.h"		/*generic functions			  */
#include "H5Eprivate.h"		/*error handling			  */
#include "H5FLprivate.h"	/*Free Lists	                          */
#include "H5Iprivate.h"		/*ID functions		   		  */
#include "H5MMprivate.h"	/*memory management			  */
#include "H5Pprivate.h"		/* Property Lists			  */
#include "H5Dprivate.h"		/* Dataset 		                  */
#include "H5Tpkg.h"		/*data-type functions			  */

/* Conversion data for H5T_conv_struct() */
typedef struct H5T_conv_struct_t {
    int	*src2dst;		/*mapping from src to dst member num */
    hid_t	*src_memb_id;		/*source member type ID's	     */
    hid_t	*dst_memb_id;		/*destination member type ID's	     */
    H5T_path_t	**memb_path;		/*conversion path for each member    */
} H5T_conv_struct_t;

/* Conversion data for H5T_conv_enum() */
typedef struct H5T_enum_struct_t {
    int	base;			/*lowest `in' value		     */
    int	length;			/*num elements in arrays	     */
    int	*src2dst;		/*map from src to dst index	     */
} H5T_enum_struct_t;

/* Conversion data for the hardware conversion functions */
typedef struct H5T_conv_hw_t {
    size_t	s_aligned;		/*number source elements aligned     */
    size_t	d_aligned;		/*number destination elements aligned*/
} H5T_conv_hw_t;

/* Interface initialization */
static int interface_initialize_g = 0;
#define INTERFACE_INIT NULL

/* Declare a free list to manage pieces of vlen data */
H5FL_BLK_DEFINE_STATIC(vlen_seq);

/* Declare a free list to manage pieces of array data */
H5FL_BLK_DEFINE_STATIC(array_seq);

/*
 * These macros are for the bodies of functions that convert buffers of one
 * atomic type to another using hardware.
 *
 * They all start with `H5T_CONV_' and end with two letters that represent the
 * source and destination types, respectively. The letters `s' and `S' refer to
 * signed integers while the letters `u' and `U' refer to unsigned integers, and
 * the letters `f' and `F' refer to floating-point values.
 *
 * The letter which is capitalized indicates that the corresponding type
 * (source or destination) is at least as large as the other type.
 *
 * Certain conversions may experience overflow conditions which arise when the
 * source value has a magnitude that cannot be represented by the destination
 * type.
 *
 * Suffix	Description
 * ------	-----------
 * sS:		Signed integers to signed integers where the destination is
 *		at least as wide as the source.	 This case cannot generate
 *		overflows.
 *
 * sU:		Signed integers to unsigned integers where the destination is
 *		at least as wide as the source.	 This case experiences
 *		overflows when the source value is negative.
 *
 * uS:		Unsigned integers to signed integers where the destination is
 *		at least as wide as the source.	 This case can experience
 *		overflows when the source and destination are the same size.
 *
 * uU:		Unsigned integers to unsigned integers where the destination
 *		is at least as wide as the source.  Overflows are not
 *		possible in this case.
 *
 * Ss:		Signed integers to signed integers where the source is at
 *		least as large as the destination.  Overflows can occur when
 *		the destination is narrower than the source.
 *
 * Su:		Signed integers to unsigned integers where the source is at
 *		least as large as the destination.  Overflows occur when the
 *		source value is negative and can also occur if the
 *		destination is narrower than the source.
 *
 * Us:		Unsigned integers to signed integers where the source is at
 *		least as large as the destination.  Overflows can occur for
 *		all sizes.
 *
 * Uu:		Unsigned integers to unsigned integers where the source is at
 *		least as large as the destination. Overflows can occur if the
 *		destination is narrower than the source.
 *
 * su:		Conversion from signed integers to unsigned integers where
 *		the source and destination are the same size. Overflow occurs
 *		when the source value is negative.
 *
 * us:		Conversion from unsigned integers to signed integers where
 *		the source and destination are the same size.  Overflow
 *		occurs when the source magnitude is too large for the
 *		destination.
 *
 * fF:		Floating-point values to floating-point values where the
 *              destination is at least as wide as the source.	 This case
 *              cannot generate overflows.
 *
 * Ff:		Floating-point values to floating-point values the source is at
 *		least as large as the destination.  Overflows can occur when
 *		the destination is narrower than the source.
 *
 * xF:          Integers to float-point values where the desination is
 *              at least as wide as the source.  This case cannot generate 
 *              overflows.
 *
 * Fx:          Float-point values to integer where the source is
 *              at least as wide as the destination.  Overflow can occur 
 *              when the source magnitude is too large for the destination.
 *              
 * The macros take a subset of these arguments in the order listed here:
 *
 * CDATA:	A pointer to the H5T_cdata_t structure that was passed to the
 *		conversion function.
 *
 * STYPE:	The hid_t value for the source data type.
 *
 * DTYPE:	The hid_t value for the destination data type.
 *
 * BUF:		A pointer to the conversion buffer.
 *
 * NELMTS:	The number of values to be converted.
 *
 * ST:		The C name for source data type (e.g., int)
 *
 * DT:		The C name for the destination data type (e.g., signed char)
 *
 * D_MIN:	The minimum possible destination value.	 For unsigned
 *		destination types this should be zero.	For signed
 *		destination types it's a negative value with a magnitude that
 *		is usually one greater than D_MAX.  Source values which are
 *		smaller than D_MIN generate overflows.
 *
 * D_MAX:	The maximum possible destination value. Source values which
 *		are larger than D_MAX generate overflows.
 * 
 * The macros are implemented with a generic programming technique, similar
 * to templates in C++.  The macro which defines the "core" part of the
 * conversion (which actually moves the data from the source to the destination)
 * is invoked inside the H5T_CONV "template" macro by "gluing" it together,
 * which allows the core conversion macro to be invoked as necessary.
 * 
 * The generic "core" macros are: (others are specific to particular conversion)
 * 
 * Suffix	Description
 * ------	-----------
 * xX:		Generic Conversion where the destination is at least as
 *              wide as the source.  This case cannot generate overflows.
 *
 * Xx:		Generic signed conversion where the source is at least as large
 *              as the destination.  Overflows can occur when the destination is
 *              narrower than the source.
 *
 * Ux:		Generic conversion for the `Us', `Uu' & `us' cases
 *		Overflow occurs when the source magnitude is too large for the
 *		destination.
 *
 */
#define H5T_CONV_xX_CORE(S,D,STYPE,DTYPE,ST,DT,D_MIN,D_MAX) {		      \
    *((DT*)D) = (DT)(*((ST*)S));					      \
}

#define H5T_CONV_Xx_CORE(S,D,STYPE,DTYPE,ST,DT,D_MIN,D_MAX) {		      \
    if (*((ST*)S) > (DT)(D_MAX)) {					      \
        if(cb_struct.func) { /*If user's exception handler is present, use it*/ \
            except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_RANGE_HI,           \
                    src_id, dst_id, S, D, cb_struct.user_data);               \
        }                                                                     \
                                                                              \
        if(except_ret == H5T_CONV_UNHANDLED) {                                \
            /*let compiler convert if case is ignored by user handler*/       \
            *((DT*)D) = (D_MAX);					      \
        } else if(except_ret == H5T_CONV_ABORT)                               \
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception") \
    } else if (*((ST*)S) < (D_MIN)) {					      \
        if(cb_struct.func) { /*If user's exception handler is present, use it*/ \
            except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_RANGE_LOW,          \
                    src_id, dst_id, S, D, cb_struct.user_data);               \
        }                                                                     \
                                                                              \
        if(except_ret == H5T_CONV_UNHANDLED) {                                \
            /*let compiler convert if case is ignored by user handler*/       \
            *((DT*)D) = (D_MIN);					      \
        } else if(except_ret == H5T_CONV_ABORT)                               \
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception") \
    } else								      \
        *((DT*)D) = (DT)(*((ST*)S));					      \
}                                                                               

#define H5T_CONV_Ux_CORE(S,D,STYPE,DTYPE,ST,DT,D_MIN,D_MAX) {		      \
    if (*((ST*)S) > (D_MAX)) {						      \
        if(cb_struct.func) { /*If user's exception handler is present, use it*/ \
            except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_RANGE_HI,           \
                    src_id, dst_id, S, D, cb_struct.user_data);               \
        }                                                                     \
                                                                              \
        if(except_ret == H5T_CONV_UNHANDLED) {                                \
            /*let compiler convert if case is ignored by user handler*/       \
            *((DT*)D) = (D_MAX);					      \
        } else if(except_ret == H5T_CONV_ABORT)                               \
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception") \
    } else								      \
        *((DT*)D) = (DT)(*((ST*)S));					      \
}

#define H5T_CONV_sS(STYPE,DTYPE,ST,DT,D_MIN,D_MAX) {			      \
    assert(sizeof(ST)<=sizeof(DT));					      \
    H5T_CONV(H5T_CONV_xX, long_long, STYPE, DTYPE, ST, DT, D_MIN, D_MAX)      \
}

#define H5T_CONV_sU_CORE(S,D,STYPE,DTYPE,ST,DT,D_MIN,D_MAX) {		      \
    if (*((ST*)S)<0) {							      \
        if(cb_struct.func) { /*If user's exception handler is present, use it*/ \
            except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_RANGE_LOW,          \
                    src_id, dst_id, S, D, cb_struct.user_data);               \
        }                                                                     \
                                                                              \
        if(except_ret == H5T_CONV_UNHANDLED) {                                \
            /*let compiler convert if case is ignored by user handler*/       \
            *((DT*)D) = 0;                                                    \
        } else if(except_ret == H5T_CONV_ABORT)                               \
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception") \
    } else								      \
        *((DT*)D) = (DT)(*((ST*)S));					      \
}

#define H5T_CONV_sU(STYPE,DTYPE,ST,DT,D_MIN,D_MAX) {			      \
    assert(sizeof(ST)<=sizeof(DT));					      \
    H5T_CONV(H5T_CONV_sU, long_long, STYPE, DTYPE, ST, DT, D_MIN, D_MAX)      \
}

#define H5T_CONV_uS_CORE(S,D,STYPE,DTYPE,ST,DT,D_MIN,D_MAX) {		      \
    if (sizeof(ST)==sizeof(DT) && *((ST*)S) > (D_MAX)) {		      \
        if(cb_struct.func) { /*If user's exception handler is present, use it*/ \
            except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_RANGE_HI,           \
                    src_id, dst_id, S, D, cb_struct.user_data);               \
        }                                                                     \
                                                                              \
        if(except_ret == H5T_CONV_UNHANDLED) {                                \
            /*let compiler convert if case is ignored by user handler*/       \
            *((DT*)D) = (D_MAX);					      \
        } else if(except_ret == H5T_CONV_ABORT)                               \
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception") \
    } else								      \
        *((DT*)D) = (DT)(*((ST*)S));					      \
}

#define H5T_CONV_uS(STYPE,DTYPE,ST,DT,D_MIN,D_MAX) {			      \
    assert(sizeof(ST)<=sizeof(DT));					      \
    H5T_CONV(H5T_CONV_uS, long_long, STYPE, DTYPE, ST, DT, D_MIN, D_MAX)      \
}

#define H5T_CONV_uU(STYPE,DTYPE,ST,DT,D_MIN,D_MAX) {			      \
    assert(sizeof(ST)<=sizeof(DT));					      \
    H5T_CONV(H5T_CONV_xX, long_long, STYPE, DTYPE, ST, DT, D_MIN, D_MAX)      \
}

#define H5T_CONV_Ss(STYPE,DTYPE,ST,DT,D_MIN,D_MAX) {			      \
    assert(sizeof(ST)>=sizeof(DT));					      \
    H5T_CONV(H5T_CONV_Xx, long_long, STYPE, DTYPE, ST, DT, D_MIN, D_MAX)      \
}

#define H5T_CONV_Su_CORE(S,D,STYPE,DTYPE,ST,DT,D_MIN,D_MAX) {		      \
    if (*((ST*)S) < 0) {						      \
        if(cb_struct.func) { /*If user's exception handler is present, use it*/ \
            except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_RANGE_LOW,          \
                    src_id, dst_id, S, D, cb_struct.user_data);               \
        }                                                                     \
                                                                              \
        if(except_ret == H5T_CONV_UNHANDLED) {                                \
            /*let compiler convert if case is ignored by user handler*/       \
            *((DT*)D) = 0;                                                    \
        } else if(except_ret == H5T_CONV_ABORT)                               \
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception") \
    } else if (sizeof(ST)>sizeof(DT) && *((ST*)S)>(ST)(D_MAX)) {	      \
        /*sign vs. unsign ok in previous line*/				      \
        if(cb_struct.func) { /*If user's exception handler is present, use it*/ \
            except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_RANGE_HI,           \
                    src_id, dst_id, S, D, cb_struct.user_data);               \
        }                                                                     \
                                                                              \
        if(except_ret == H5T_CONV_UNHANDLED) {                                \
            /*let compiler convert if case is ignored by user handler*/       \
            *((DT*)D) = (D_MAX);					      \
        } else if(except_ret == H5T_CONV_ABORT)                               \
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception") \
    } else								      \
        *((DT*)D) = (DT)(*((ST*)S));					      \
}

#define H5T_CONV_Su(STYPE,DTYPE,ST,DT,D_MIN,D_MAX) {			      \
    assert(sizeof(ST)>=sizeof(DT));					      \
    H5T_CONV(H5T_CONV_Su, long_long, STYPE, DTYPE, ST, DT, D_MIN, D_MAX)      \
}

#define H5T_CONV_Us(STYPE,DTYPE,ST,DT,D_MIN,D_MAX) {			      \
    assert(sizeof(ST)>=sizeof(DT));					      \
    H5T_CONV(H5T_CONV_Ux, long_long, STYPE, DTYPE, ST, DT, D_MIN, D_MAX)      \
}

#define H5T_CONV_Uu(STYPE,DTYPE,ST,DT,D_MIN,D_MAX) {			      \
    assert(sizeof(ST)>=sizeof(DT));					      \
    H5T_CONV(H5T_CONV_Ux, long_long, STYPE, DTYPE, ST, DT, D_MIN, D_MAX)      \
}

#define H5T_CONV_su_CORE(S,D,STYPE,DTYPE,ST,DT,D_MIN,D_MAX) {		      \
    /* Assumes memory format of unsigned & signed integers is same */	      \
    if (*((ST*)S)<0) {							      \
        if(cb_struct.func) { /*If user's exception handler is present, use it*/ \
            except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_RANGE_LOW,          \
                    src_id, dst_id, S, D, cb_struct.user_data);               \
        }                                                                     \
                                                                              \
        if(except_ret == H5T_CONV_UNHANDLED) {                                \
            /*let compiler convert if case is ignored by user handler*/       \
            *((DT*)D) = 0;                                                    \
        } else if(except_ret == H5T_CONV_ABORT)                               \
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception") \
    }									      \
}

#define H5T_CONV_su(STYPE,DTYPE,ST,DT,D_MIN,D_MAX) {			      \
    assert(sizeof(ST)==sizeof(DT));					      \
    H5T_CONV(H5T_CONV_su, long_long, STYPE, DTYPE, ST, DT, D_MIN, D_MAX)      \
}

#define H5T_CONV_us_CORE(S,D,STYPE,DTYPE,ST,DT,D_MIN,D_MAX) {		      \
    /* Assumes memory format of unsigned & signed integers is same */	      \
    if (*((ST*)S) > (D_MAX)) {						      \
        if(cb_struct.func) { /*If user's exception handler is present, use it*/ \
            except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_RANGE_HI,           \
                    src_id, dst_id, S, D, cb_struct.user_data);               \
        }                                                                     \
                                                                              \
        if(except_ret == H5T_CONV_UNHANDLED) {                                \
            /*let compiler convert if case is ignored by user handler*/       \
            *((DT*)D) = (D_MAX);					      \
        } else if(except_ret == H5T_CONV_ABORT)                               \
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception") \
    }									      \
}

#define H5T_CONV_us(STYPE,DTYPE,ST,DT,D_MIN,D_MAX) {			      \
    assert(sizeof(ST)==sizeof(DT));					      \
    H5T_CONV(H5T_CONV_us, long_long, STYPE, DTYPE, ST, DT, D_MIN, D_MAX)      \
}

#define H5T_CONV_fF(STYPE,DTYPE,ST,DT,D_MIN,D_MAX) {			      \
    assert(sizeof(ST)<=sizeof(DT));					      \
    H5T_CONV(H5T_CONV_xX, double, STYPE, DTYPE, ST, DT, D_MIN, D_MAX)	      \
}

/* Same as H5T_CONV_Xx_CORE, except that instead of using D_MAX and D_MIN
 * when an overflow occurs, use the 'float' infinity values.
 */
#define H5T_CONV_Ff_CORE(S,D,STYPE,DTYPE,ST,DT,D_MIN,D_MAX) {		      \
    if (*((ST*)S) > (DT)(D_MAX)) {					      \
        if(cb_struct.func) { /*If user's exception handler is present, use it*/ \
            except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_RANGE_HI,           \
                    src_id, dst_id, S, D, cb_struct.user_data);               \
        }                                                                     \
                                                                              \
        if(except_ret == H5T_CONV_UNHANDLED) {                                \
            /*let compiler convert if case is ignored by user handler*/       \
            *((DT*)D) = (H5T_NATIVE_FLOAT_POS_INF_g);			      \
        } else if(except_ret == H5T_CONV_ABORT)                               \
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception") \
    } else if (*((ST*)S) < (D_MIN)) {					      \
        if(cb_struct.func) { /*If user's exception handler is present, use it*/ \
            except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_RANGE_LOW,          \
                    src_id, dst_id, S, D, cb_struct.user_data);               \
        }                                                                     \
                                                                              \
        if(except_ret == H5T_CONV_UNHANDLED) {                                \
            /*let compiler convert if case is ignored by user handler*/       \
            *((DT*)D) = (H5T_NATIVE_FLOAT_NEG_INF_g);			      \
        } else if(except_ret == H5T_CONV_ABORT)                               \
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception") \
    } else								      \
        *((DT*)D) = (DT)(*((ST*)S));					      \
}

#define H5T_CONV_Ff(STYPE,DTYPE,ST,DT,D_MIN,D_MAX) {			      \
    assert(sizeof(ST)>=sizeof(DT));					      \
    H5T_CONV(H5T_CONV_Ff, double, STYPE, DTYPE, ST, DT, D_MIN, D_MAX)	      \
}

#define H5T_CONV_xF(STYPE,DTYPE,ST,DT,D_MIN,D_MAX) {			      \
    H5T_CONV(H5T_CONV_xX, double, STYPE, DTYPE, ST, DT, D_MIN, D_MAX)	      \
}

#define H5T_CONV_Fx(STYPE,DTYPE,ST,DT,D_MIN,D_MAX) {			      \
    H5T_CONV(H5T_CONV_Xx, double, STYPE, DTYPE, ST, DT, D_MIN, D_MAX)	      \
}

/* The main part of every integer hardware conversion macro */
#define H5T_CONV(GUTS,ATYPE,STYPE,DTYPE,ST,DT,D_MIN,D_MAX) {		      \
    size_t	elmtno;			/*element number		*/    \
    uint8_t	*src, *s;		/*source buffer			*/    \
    uint8_t	*dst, *d;		/*destination buffer		*/    \
    H5T_t	*st, *dt;		/*data type descriptors		*/    \
    ATYPE	aligned;		/*aligned type			*/    \
    hbool_t	s_mv, d_mv;		/*move data to align it?	*/    \
    ssize_t	s_stride, d_stride;	/*src and dst strides		*/    \
    size_t      safe;                   /*how many elements are safe to process in each pass */ \
    H5P_genplist_t      *plist;         /*Property list pointer         */    \
    H5T_conv_cb_t       cb_struct;      /*conversion callback structure */    \
    H5T_conv_ret_t      except_ret;     /*callback return*/                   \
                                                                              \
    switch (cdata->command) {						      \
    case H5T_CONV_INIT:							      \
	/* Sanity check and initialize statistics */			      \
	cdata->need_bkg = H5T_BKG_NO;					      \
	if (NULL==(st=H5I_object(src_id)) || NULL==(dt=H5I_object(dst_id)))   \
	    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL,		      \
			  "unable to dereference datatype object ID")	      \
	if (st->size!=sizeof(ST) || dt->size!=sizeof(DT))		      \
	    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL,		      \
			  "disagreement about datatype size")		      \
	CI_ALLOC_PRIV	                                                      \
	break;								      \
									      \
    case H5T_CONV_FREE:							      \
	/* Print and free statistics */					      \
	CI_PRINT_STATS(STYPE,DTYPE);					      \
	CI_FREE_PRIV	                                                      \
	break;								      \
									      \
    case H5T_CONV_CONV:							      \
	/* Initialize source & destination strides */			      \
	if (buf_stride) {						      \
            assert(buf_stride>=sizeof(ST));				      \
            assert(buf_stride>=sizeof(DT));				      \
	    s_stride = d_stride = buf_stride;				      \
	} else {							      \
            s_stride = sizeof(ST);					      \
            d_stride = sizeof(DT);					      \
        }								      \
									      \
	/* Is alignment required for source or dest? */			      \
	s_mv = H5T_NATIVE_##STYPE##_ALIGN_g>1 &&			      \
               ((size_t)buf%H5T_NATIVE_##STYPE##_ALIGN_g ||		      \
     /* Cray */ ((size_t)((ST*)buf)!=(size_t)buf) ||			      \
		s_stride%H5T_NATIVE_##STYPE##_ALIGN_g);			      \
	d_mv = H5T_NATIVE_##DTYPE##_ALIGN_g>1 &&			      \
               ((size_t)buf%H5T_NATIVE_##DTYPE##_ALIGN_g ||		      \
     /* Cray */ ((size_t)((DT*)buf)!=(size_t)buf) ||			      \
                d_stride%H5T_NATIVE_##DTYPE##_ALIGN_g);			      \
	CI_INC_SRC(s_mv)						      \
	CI_INC_DST(d_mv)						      \
	                                                                      \
        /* Get the plist structure */                                         \
        if(NULL == (plist = H5P_object_verify(dxpl_id,H5P_DATASET_XFER)))     \
            HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find property list for ID"); \
                                                                              \
        /* Get conversion exception callback property */                      \
        if (H5P_get(plist,H5D_XFER_CONV_CB_NAME,&cb_struct)<0)                \
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get conversion exception callback"); \
                                                                              \
        /* The outer loop of the type conversion macro, controlling which */  \
        /* direction the buffer is walked */				      \
        while (nelmts>0) {						      \
            /* Check if we need to go backwards through the buffer */	      \
            if(d_stride>s_stride) {					      \
                /* Compute the number of "safe" destination elements at */    \
                /* the end of the buffer (Those which don't overlap with */   \
                /* any source elements at the beginning of the buffer) */     \
                safe=nelmts-(((nelmts*s_stride)+(d_stride-1))/d_stride);      \
									      \
                /* If we're down to the last few elements, just wrap up */    \
                /* with a "real" reverse copy */			      \
                if(safe<2) {						      \
                    src = (uint8_t*)buf+(nelmts-1)*s_stride;		      \
                    dst = (uint8_t*)buf+(nelmts-1)*d_stride;		      \
                    s_stride = -s_stride;				      \
                    d_stride = -d_stride;				      \
									      \
                    safe=nelmts;					      \
                } /* end if */						      \
                else {							      \
                    src = (uint8_t*)buf+(nelmts-safe)*s_stride;		      \
                    dst = (uint8_t*)buf+(nelmts-safe)*d_stride;		      \
                } /* end else */					      \
            } /* end if */						      \
            else {							      \
                /* Single forward pass over all data */			      \
                src = dst = buf;					      \
                safe=nelmts;						      \
            } /* end else */						      \
                                                                              \
	    except_ret = H5T_CONV_UNHANDLED; 			              \
                                                                              \
            /* Perform loop over elements to convert */			      \
            if (s_mv && d_mv) {						      \
                /* Alignment is required for both source and dest */	      \
                s = (uint8_t*)&aligned;					      \
                H5T_CONV_LOOP(PRE_SALIGN,PRE_DALIGN,POST_SALIGN,POST_DALIGN,GUTS,s,d,STYPE,DTYPE,ST,DT,D_MIN,D_MAX) \
            } else if(s_mv) {						      \
                /* Alignment is required only for source */		      \
                s = (uint8_t*)&aligned;					      \
                H5T_CONV_LOOP(PRE_SALIGN,PRE_DNOALIGN,POST_SALIGN,POST_DNOALIGN,GUTS,s,dst,STYPE,DTYPE,ST,DT,D_MIN,D_MAX) \
            } else if(d_mv) {						      \
                /* Alignment is required only for destination */	      \
                H5T_CONV_LOOP(PRE_SNOALIGN,PRE_DALIGN,POST_SNOALIGN,POST_DALIGN,GUTS,src,d,STYPE,DTYPE,ST,DT,D_MIN,D_MAX) \
            } else {							      \
                /* Alignment is not required for both source and destination */ \
                H5T_CONV_LOOP(PRE_SNOALIGN,PRE_DNOALIGN,POST_SNOALIGN,POST_DNOALIGN,GUTS,src,dst,STYPE,DTYPE,ST,DT,D_MIN,D_MAX) \
            }	 	 	 	 	 	 	 	      \
									      \
            /* Decrement number of elements left to convert */		      \
            nelmts-=safe;						      \
        } /* end while */						      \
        break;								      \
									      \
    default:								      \
	HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,		      \
		      "unknown conversion command");			      \
    }									      \
}

/* Macro defining action on source data which needs to be aligned (before main action) */
#define H5T_CONV_LOOP_PRE_SALIGN(ST) {					      \
    HDmemcpy(&aligned, src, sizeof(ST));				      \
}

/* Macro defining action on source data which doesn't need to be aligned (before main action) */
#define H5T_CONV_LOOP_PRE_SNOALIGN(ST) {				      \
}

/* Macro defining action on destination data which needs to be aligned (before main action) */
#define H5T_CONV_LOOP_PRE_DALIGN(DT) {					      \
    d = (uint8_t*)&aligned;						      \
}

/* Macro defining action on destination data which doesn't need to be aligned (before main action) */
#define H5T_CONV_LOOP_PRE_DNOALIGN(DT) {				      \
}

/* Macro defining action on source data which needs to be aligned (after main action) */
#define H5T_CONV_LOOP_POST_SALIGN(ST) {					      \
}

/* Macro defining action on source data which doesn't need to be aligned (after main action) */
#define H5T_CONV_LOOP_POST_SNOALIGN(ST) {				      \
}

/* Macro defining action on destination data which needs to be aligned (after main action) */
#define H5T_CONV_LOOP_POST_DALIGN(DT) {					      \
    HDmemcpy(dst, &aligned, sizeof(DT));				      \
}

/* Macro defining action on destination data which doesn't need to be aligned (after main action) */
#define H5T_CONV_LOOP_POST_DNOALIGN(DT) {				      \
}

/* The inner loop of the type conversion macro, actually converting the elements */
#define H5T_CONV_LOOP(PRE_SALIGN_GUTS,PRE_DALIGN_GUTS,POST_SALIGN_GUTS,POST_DALIGN_GUTS,GUTS,S,D,STYPE,DTYPE,ST,DT,D_MIN,D_MAX) \
    for (elmtno=0; elmtno<safe; elmtno++) {				      \
        /* Handle source pre-alignment */				      \
        H5_GLUE(H5T_CONV_LOOP_,PRE_SALIGN_GUTS)(ST)			      \
									      \
        /* Handle destination pre-alignment */				      \
        H5_GLUE(H5T_CONV_LOOP_,PRE_DALIGN_GUTS)(DT)			      \
									      \
        /* ... user-defined stuff here -- the conversion ... */		      \
        H5_GLUE(GUTS,_CORE)(S,D,STYPE,DTYPE,ST,DT,D_MIN,D_MAX)		      \
									      \
        /* Handle source post-alignment */				      \
        H5_GLUE(H5T_CONV_LOOP_,POST_SALIGN_GUTS)(ST)			      \
									      \
        /* Handle destination post-alignment */				      \
        H5_GLUE(H5T_CONV_LOOP_,POST_DALIGN_GUTS)(DT)			      \
									      \
        /* Advance pointers */						      \
        src += s_stride;						      \
        dst += d_stride;						      \
    }


#ifdef H5T_DEBUG

/* Print alignment statistics */
#   define CI_PRINT_STATS(STYPE,DTYPE) {				      \
    if (H5DEBUG(T) && ((H5T_conv_hw_t *)cdata->priv)->s_aligned) {				      \
	HDfprintf(H5DEBUG(T),						      \
		  "      %Hu src elements aligned on %lu-byte boundaries\n",  \
		  ((H5T_conv_hw_t *)cdata->priv)->s_aligned,				\
		  (unsigned long)H5T_NATIVE_##STYPE##_ALIGN_g);		      \
    }									      \
    if (H5DEBUG(T) && ((H5T_conv_hw_t *)cdata->priv)->d_aligned) {				      \
	HDfprintf(H5DEBUG(T),						      \
		  "      %Hu dst elements aligned on %lu-byte boundaries\n",  \
		  ((H5T_conv_hw_t *)cdata->priv)->d_aligned,				\
		  (unsigned long)H5T_NATIVE_##DTYPE##_ALIGN_g);		      \
    }									      \
}

/* Allocate private alignment structure for atomic types */
#   define CI_ALLOC_PRIV \
	if (NULL==(cdata->priv=H5MM_calloc(sizeof(H5T_conv_hw_t)))) {	      \
	    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,		      \
			  "memory allocation failed");			      \
	}

/* Free private alignment structure for atomic types */
#   define CI_FREE_PRIV                                 \
    if(cdata->priv!=NULL)                               \
        cdata->priv = H5MM_xfree(cdata->priv);

/* Increment source alignment counter */
#   define CI_INC_SRC(s)   if (s) ((H5T_conv_hw_t *)cdata->priv)->s_aligned += nelmts;

/* Increment destination alignment counter */
#   define CI_INC_DST(d)   if (d) ((H5T_conv_hw_t *)cdata->priv)->d_aligned += nelmts;
#else /* H5T_DEBUG */
#   define CI_PRINT_STATS(STYPE,DTYPE) /*void*/
#   define CI_ALLOC_PRIV cdata->priv=NULL;
#   define CI_FREE_PRIV  /* void */
#   define CI_INC_SRC(s) /* void */
#   define CI_INC_DST(d) /* void */
#endif /* H5T_DEBUG */

/* Swap two elements (I & J) of an array using a temporary variable */
#define H5_SWAP_BYTES(ARRAY,I,J) {uint8_t _tmp; _tmp=ARRAY[I]; ARRAY[I]=ARRAY[J]; ARRAY[J]=_tmp;}

/* Minimum size of variable-length conversion buffer */
#define H5T_VLEN_MIN_CONF_BUF_SIZE      4096

static herr_t H5T_reverse_order(uint8_t *rev, uint8_t *s, size_t size, H5T_order_t order);


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_noop
 *
 * Purpose:	The no-op conversion.  The library knows about this
 *		conversion without it being registered.
 *
 * Return: 	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January 14, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_noop(hid_t UNUSED src_id, hid_t UNUSED dst_id, H5T_cdata_t *cdata,
	      size_t UNUSED nelmts, size_t UNUSED buf_stride,
              size_t UNUSED bkg_stride, void UNUSED *buf,
	      void UNUSED *background, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_noop, FAIL);

    switch (cdata->command) {
        case H5T_CONV_INIT:
            cdata->need_bkg = H5T_BKG_NO;
            break;

        case H5T_CONV_CONV:
            /* Nothing to convert */
            break;

        case H5T_CONV_FREE:
            break;
        
        default:
            HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_order_opt
 *
 * Purpose:	Convert one type to another when byte order is the only
 *		difference. This is the optimized version of H5T_conv_order()
 *              for a handful of different sizes.
 *
 * Note:	This is a soft conversion function.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Friday, January 25, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_order_opt(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
                   size_t nelmts, size_t buf_stride,
                   size_t UNUSED bkg_stride, void *_buf,
                   void UNUSED *background, hid_t UNUSED dxpl_id)
{
    uint8_t	*buf = (uint8_t*)_buf;
    H5T_t	*src = NULL;
    H5T_t	*dst = NULL;
    size_t      i;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_order_opt, FAIL);

    switch (cdata->command) {
    case H5T_CONV_INIT:
	/* Capability query */
	if (NULL == (src = H5I_object(src_id)) ||
                NULL == (dst = H5I_object(dst_id)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
	if (src->size != dst->size ||
                0 != src->u.atomic.offset ||
                0 != dst->u.atomic.offset)
	    HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "conversion not supported");
        if((src->type==H5T_REFERENCE && dst->type!=H5T_REFERENCE) ||
                (dst->type==H5T_REFERENCE && src->type!=H5T_REFERENCE))
	    HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "conversion not supported");
        if(src->type!=H5T_REFERENCE &&
                !((H5T_ORDER_BE == src->u.atomic.order && H5T_ORDER_LE == dst->u.atomic.order) ||
                  (H5T_ORDER_LE == src->u.atomic.order && H5T_ORDER_BE == dst->u.atomic.order)))
	    HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "conversion not supported");
        if (src->size!=1 && src->size!=2 && src->size!=4 &&
                src->size!=8 && src->size!=16)
            HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "conversion not supported");
	switch (src->type) {
            case H5T_INTEGER:
            case H5T_BITFIELD:
            case H5T_REFERENCE:
                /* nothing to check */
                break;

            case H5T_FLOAT:
                if (src->u.atomic.u.f.sign != dst->u.atomic.u.f.sign ||
                        src->u.atomic.u.f.epos != dst->u.atomic.u.f.epos ||
                        src->u.atomic.u.f.esize != dst->u.atomic.u.f.esize ||
                        src->u.atomic.u.f.ebias != dst->u.atomic.u.f.ebias ||
                        src->u.atomic.u.f.mpos != dst->u.atomic.u.f.mpos ||
                        src->u.atomic.u.f.msize != dst->u.atomic.u.f.msize ||
                        src->u.atomic.u.f.norm != dst->u.atomic.u.f.norm ||
                        src->u.atomic.u.f.pad != dst->u.atomic.u.f.pad)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "conversion not supported");
                break;

            default:
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "conversion not supported");
	}
	cdata->need_bkg = H5T_BKG_NO;
	break;

    case H5T_CONV_CONV:
	/* The conversion */
	if (NULL == (src = H5I_object(src_id)) ||
                NULL == (dst = H5I_object(dst_id)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");

        /* Check for "no op" reference conversion */
        if(src->type==H5T_REFERENCE) {
            /* Sanity check */
            assert(dst->type==H5T_REFERENCE);

            /* Check if we are on a little-endian machine (the order that
             * the addresses in the file must be) and just get out now, there
             * is no need to convert the object reference.  Yes, this is
             * icky and non-portable, but I can't think of a better way to
             * support allowing the objno in the H5G_stat_t struct and the
             * hobj_ref_t type to be compared directly without introducing a
             * "native" hobj_ref_t datatype and I think that would break a
             * lot of existing programs.  -QAK
             */
            if(H5T_native_order_g == H5T_ORDER_LE)
                break;
        } /* end if */

        buf_stride = buf_stride ? buf_stride : src->size;
        switch (src->size) {
        case 1:
            /*no-op*/
            break;
        case 2:
            for (/*void*/; nelmts>=20; nelmts-=20) {
                H5_SWAP_BYTES(buf, 0,   1); /*  0 */
                buf += buf_stride; 
                H5_SWAP_BYTES(buf, 0,   1); /*  1 */
                buf += buf_stride; 
                H5_SWAP_BYTES(buf, 0,   1); /*  2 */
                buf += buf_stride; 
                H5_SWAP_BYTES(buf, 0,   1); /*  3 */
                buf += buf_stride; 
                H5_SWAP_BYTES(buf, 0,   1); /*  4 */
                buf += buf_stride; 
                H5_SWAP_BYTES(buf, 0,   1); /*  5 */
                buf += buf_stride; 
                H5_SWAP_BYTES(buf, 0,   1); /*  6 */
                buf += buf_stride; 
                H5_SWAP_BYTES(buf, 0,   1); /*  7 */
                buf += buf_stride; 
                H5_SWAP_BYTES(buf, 0,   1); /*  8 */
                buf += buf_stride; 
                H5_SWAP_BYTES(buf, 0,   1); /*  9 */
                buf += buf_stride; 
                H5_SWAP_BYTES(buf, 0,   1); /* 10 */
                buf += buf_stride; 
                H5_SWAP_BYTES(buf, 0,   1); /* 11 */
                buf += buf_stride; 
                H5_SWAP_BYTES(buf, 0,   1); /* 12 */
                buf += buf_stride; 
                H5_SWAP_BYTES(buf, 0,   1); /* 13 */
                buf += buf_stride; 
                H5_SWAP_BYTES(buf, 0,   1); /* 14 */
                buf += buf_stride; 
                H5_SWAP_BYTES(buf, 0,   1); /* 15 */
                buf += buf_stride; 
                H5_SWAP_BYTES(buf, 0,   1); /* 16 */
                buf += buf_stride; 
                H5_SWAP_BYTES(buf, 0,   1); /* 17 */
                buf += buf_stride; 
                H5_SWAP_BYTES(buf, 0,   1); /* 18 */
                buf += buf_stride; 
                H5_SWAP_BYTES(buf, 0,   1); /* 19 */
                buf += buf_stride;
            }
            for (i=0; i<nelmts; i++, buf+=buf_stride) {
                H5_SWAP_BYTES(buf, 0, 1);
            }
            break;
        case 4:
            for (/*void*/; nelmts>=20; nelmts-=20) {
                H5_SWAP_BYTES(buf,  0,  3); /*  0 */
                H5_SWAP_BYTES(buf,  1,  2);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,  0,  3); /*  1 */
                H5_SWAP_BYTES(buf,  1,  2);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,  0,  3); /*  2 */
                H5_SWAP_BYTES(buf,  1,  2);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,  0,  3); /*  3 */
                H5_SWAP_BYTES(buf,  1,  2);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,  0,  3); /*  4 */
                H5_SWAP_BYTES(buf,  1,  2);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,  0,  3); /*  5 */
                H5_SWAP_BYTES(buf,  1,  2);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,  0,  3); /*  6 */
                H5_SWAP_BYTES(buf,  1,  2);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,  0,  3); /*  7 */
                H5_SWAP_BYTES(buf,  1,  2);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,  0,  3); /*  8 */
                H5_SWAP_BYTES(buf,  1,  2);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,  0,  3); /*  9 */
                H5_SWAP_BYTES(buf,  1,  2);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,  0,  3); /* 10 */
                H5_SWAP_BYTES(buf,  1,  2);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,  0,  3); /* 11 */
                H5_SWAP_BYTES(buf,  1,  2);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,  0,  3); /* 12 */
                H5_SWAP_BYTES(buf,  1,  2);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,  0,  3); /* 13 */
                H5_SWAP_BYTES(buf,  1,  2);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,  0,  3); /* 14 */
                H5_SWAP_BYTES(buf,  1,  2);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,  0,  3); /* 15 */
                H5_SWAP_BYTES(buf,  1,  2);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,  0,  3); /* 16 */
                H5_SWAP_BYTES(buf,  1,  2);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,  0,  3); /* 17 */
                H5_SWAP_BYTES(buf,  1,  2);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,  0,  3); /* 18 */
                H5_SWAP_BYTES(buf,  1,  2);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,  0,  3); /* 19 */
                H5_SWAP_BYTES(buf,  1,  2);
                buf += buf_stride;
            }
            for (i=0; i<nelmts; i++, buf+=buf_stride) {
                H5_SWAP_BYTES(buf, 0, 3);
                H5_SWAP_BYTES(buf, 1, 2);
            }
            break;
        case 8:
            for (/*void*/; nelmts>=10; nelmts-=10) {
                H5_SWAP_BYTES(buf,  0,  7); /*  0 */
                H5_SWAP_BYTES(buf,  1,  6);
                H5_SWAP_BYTES(buf,  2,  5);
                H5_SWAP_BYTES(buf,  3,  4);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,  0,  7); /*  1 */
                H5_SWAP_BYTES(buf,  1,  6);
                H5_SWAP_BYTES(buf,  2,  5);
                H5_SWAP_BYTES(buf,  3,  4);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,  0,  7); /*  2 */
                H5_SWAP_BYTES(buf,  1,  6);
                H5_SWAP_BYTES(buf,  2,  5);
                H5_SWAP_BYTES(buf,  3,  4);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,  0,  7); /*  3 */
                H5_SWAP_BYTES(buf,  1,  6);
                H5_SWAP_BYTES(buf,  2,  5);
                H5_SWAP_BYTES(buf,  3,  4);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,  0,  7); /*  4 */
                H5_SWAP_BYTES(buf,  1,  6);
                H5_SWAP_BYTES(buf,  2,  5);
                H5_SWAP_BYTES(buf,  3,  4);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,  0,  7); /*  5 */
                H5_SWAP_BYTES(buf,  1,  6);
                H5_SWAP_BYTES(buf,  2,  5);
                H5_SWAP_BYTES(buf,  3,  4);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,  0,  7); /*  6 */
                H5_SWAP_BYTES(buf,  1,  6);
                H5_SWAP_BYTES(buf,  2,  5);
                H5_SWAP_BYTES(buf,  3,  4);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,  0,  7); /*  7 */
                H5_SWAP_BYTES(buf,  1,  6);
                H5_SWAP_BYTES(buf,  2,  5);
                H5_SWAP_BYTES(buf,  3,  4);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,  0,  7); /*  8 */
                H5_SWAP_BYTES(buf,  1,  6);
                H5_SWAP_BYTES(buf,  2,  5);
                H5_SWAP_BYTES(buf,  3,  4);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,  0,  7); /*  9 */
                H5_SWAP_BYTES(buf,  1,  6);
                H5_SWAP_BYTES(buf,  2,  5);
                H5_SWAP_BYTES(buf,  3,  4);
                buf += buf_stride;
            }
            for (i=0; i<nelmts; i++, buf+=buf_stride) {
                H5_SWAP_BYTES(buf, 0, 7);
                H5_SWAP_BYTES(buf, 1, 6);
                H5_SWAP_BYTES(buf, 2, 5);
                H5_SWAP_BYTES(buf, 3, 4);
            }
            break;
        case 16:
            for (/*void*/; nelmts>=10; nelmts-=10) {
                H5_SWAP_BYTES(buf,   0,  15); /*  0 */
                H5_SWAP_BYTES(buf,   1,  14);
                H5_SWAP_BYTES(buf,   2,  13);
                H5_SWAP_BYTES(buf,   3,  12);
                H5_SWAP_BYTES(buf,   4,  11);
                H5_SWAP_BYTES(buf,   5,  10);
                H5_SWAP_BYTES(buf,   6,   9);
                H5_SWAP_BYTES(buf,   7,   8);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,   0,  15); /*  1 */
                H5_SWAP_BYTES(buf,   1,  14);
                H5_SWAP_BYTES(buf,   2,  13);
                H5_SWAP_BYTES(buf,   3,  12);
                H5_SWAP_BYTES(buf,   4,  11);
                H5_SWAP_BYTES(buf,   5,  10);
                H5_SWAP_BYTES(buf,   6,   9);
                H5_SWAP_BYTES(buf,   7,   8);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,   0,  15); /*  2 */
                H5_SWAP_BYTES(buf,   1,  14);
                H5_SWAP_BYTES(buf,   2,  13);
                H5_SWAP_BYTES(buf,   3,  12);
                H5_SWAP_BYTES(buf,   4,  11);
                H5_SWAP_BYTES(buf,   5,  10);
                H5_SWAP_BYTES(buf,   6,   9);
                H5_SWAP_BYTES(buf,   7,   8);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,   0,  15); /*  3 */
                H5_SWAP_BYTES(buf,   1,  14);
                H5_SWAP_BYTES(buf,   2,  13);
                H5_SWAP_BYTES(buf,   3,  12);
                H5_SWAP_BYTES(buf,   4,  11);
                H5_SWAP_BYTES(buf,   5,  10);
                H5_SWAP_BYTES(buf,   6,   9);
                H5_SWAP_BYTES(buf,   7,   8);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,   0,  15); /*  4 */
                H5_SWAP_BYTES(buf,   1,  14);
                H5_SWAP_BYTES(buf,   2,  13);
                H5_SWAP_BYTES(buf,   3,  12);
                H5_SWAP_BYTES(buf,   4,  11);
                H5_SWAP_BYTES(buf,   5,  10);
                H5_SWAP_BYTES(buf,   6,   9);
                H5_SWAP_BYTES(buf,   7,   8);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,   0,  15); /*  5 */
                H5_SWAP_BYTES(buf,   1,  14);
                H5_SWAP_BYTES(buf,   2,  13);
                H5_SWAP_BYTES(buf,   3,  12);
                H5_SWAP_BYTES(buf,   4,  11);
                H5_SWAP_BYTES(buf,   5,  10);
                H5_SWAP_BYTES(buf,   6,   9);
                H5_SWAP_BYTES(buf,   7,   8);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,   0,  15); /*  6 */
                H5_SWAP_BYTES(buf,   1,  14);
                H5_SWAP_BYTES(buf,   2,  13);
                H5_SWAP_BYTES(buf,   3,  12);
                H5_SWAP_BYTES(buf,   4,  11);
                H5_SWAP_BYTES(buf,   5,  10);
                H5_SWAP_BYTES(buf,   6,   9);
                H5_SWAP_BYTES(buf,   7,   8);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,   0,  15); /*  7 */
                H5_SWAP_BYTES(buf,   1,  14);
                H5_SWAP_BYTES(buf,   2,  13);
                H5_SWAP_BYTES(buf,   3,  12);
                H5_SWAP_BYTES(buf,   4,  11);
                H5_SWAP_BYTES(buf,   5,  10);
                H5_SWAP_BYTES(buf,   6,   9);
                H5_SWAP_BYTES(buf,   7,   8);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,   0,  15); /*  8 */
                H5_SWAP_BYTES(buf,   1,  14);
                H5_SWAP_BYTES(buf,   2,  13);
                H5_SWAP_BYTES(buf,   3,  12);
                H5_SWAP_BYTES(buf,   4,  11);
                H5_SWAP_BYTES(buf,   5,  10);
                H5_SWAP_BYTES(buf,   6,   9);
                H5_SWAP_BYTES(buf,   7,   8);
                buf += buf_stride;
                H5_SWAP_BYTES(buf,   0,  15); /*  9 */
                H5_SWAP_BYTES(buf,   1,  14);
                H5_SWAP_BYTES(buf,   2,  13);
                H5_SWAP_BYTES(buf,   3,  12);
                H5_SWAP_BYTES(buf,   4,  11);
                H5_SWAP_BYTES(buf,   5,  10);
                H5_SWAP_BYTES(buf,   6,   9);
                H5_SWAP_BYTES(buf,   7,   8);
                buf += buf_stride;
            }
            for (i=0; i<nelmts; i++, buf+=buf_stride) {
                H5_SWAP_BYTES(buf, 0, 15);
                H5_SWAP_BYTES(buf, 1, 14);
                H5_SWAP_BYTES(buf, 2, 13);
                H5_SWAP_BYTES(buf, 3, 12);
                H5_SWAP_BYTES(buf, 4, 11);
                H5_SWAP_BYTES(buf, 5, 10);
                H5_SWAP_BYTES(buf, 6,  9);
                H5_SWAP_BYTES(buf, 7,  8);
            }
            break;
        }
        break;

    case H5T_CONV_FREE:
	/* Free private data */
	break;

    default:
	HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_order
 *
 * Purpose:	Convert one type to another when byte order is the only
 *		difference.
 *
 * Note:	This is a soft conversion function.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Tuesday, January 13, 1998
 *
 * Modifications:
 *		Robb Matzke, 1999-06-16
 *		Added the `stride' argument. If its value is non-zero then we
 *		stride through memory converting one value at each location;
 *		otherwise we assume that the values should be packed.
 *
 * 		Robb Matzke, 1999-06-16
 *		Added support for bitfields.
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_order(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata, size_t nelmts,
	       size_t buf_stride, size_t UNUSED bkg_stride, void *_buf,
               void UNUSED *background, hid_t UNUSED dxpl_id)
{
    uint8_t	*buf = (uint8_t*)_buf;
    H5T_t	*src = NULL;
    H5T_t	*dst = NULL;
    size_t	i;
    size_t	j, md;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_order, FAIL);

    switch (cdata->command) {
    case H5T_CONV_INIT:
	/* Capability query */
	if (NULL == (src = H5I_object(src_id)) ||
                NULL == (dst = H5I_object(dst_id)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
	if (src->size != dst->size || 0 != src->u.atomic.offset ||
                0 != dst->u.atomic.offset ||
                !((H5T_ORDER_BE == src->u.atomic.order &&
                   H5T_ORDER_LE == dst->u.atomic.order) ||
                  (H5T_ORDER_LE == src->u.atomic.order &&
                   H5T_ORDER_BE == dst->u.atomic.order)))
	    HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "conversion not supported");
	switch (src->type) {
            case H5T_INTEGER:
            case H5T_BITFIELD:
                /* nothing to check */
                break;

            case H5T_FLOAT:
                if (src->u.atomic.u.f.sign != dst->u.atomic.u.f.sign ||
                        src->u.atomic.u.f.epos != dst->u.atomic.u.f.epos ||
                        src->u.atomic.u.f.esize != dst->u.atomic.u.f.esize ||
                        src->u.atomic.u.f.ebias != dst->u.atomic.u.f.ebias ||
                        src->u.atomic.u.f.mpos != dst->u.atomic.u.f.mpos ||
                        src->u.atomic.u.f.msize != dst->u.atomic.u.f.msize ||
                        src->u.atomic.u.f.norm != dst->u.atomic.u.f.norm ||
                        src->u.atomic.u.f.pad != dst->u.atomic.u.f.pad) {
                    HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "conversion not supported");
                }
                break;

            default:
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "conversion not supported");
	}
	cdata->need_bkg = H5T_BKG_NO;
	break;

    case H5T_CONV_CONV:
	/* The conversion */
	if (NULL == (src = H5I_object(src_id)) ||
                NULL == (dst = H5I_object(dst_id)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");

        buf_stride = buf_stride ? buf_stride : src->size;
        md = src->size / 2;
        for (i=0; i<nelmts; i++, buf+=buf_stride) {
            for (j=0; j<md; j++)
                H5_SWAP_BYTES(buf, j, src->size-(j+1));
        }
        break;

    case H5T_CONV_FREE:
	/* Free private data */
	break;

    default:
	HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_b_b
 *
 * Purpose:	Convert from one bitfield to any other bitfield.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Thursday, May 20, 1999
 *
 * Modifications:
 *		Robb Matzke, 1999-06-16
 *		Added support for non-zero strides. If BUF_STRIDE is non-zero
 *		then convert one value at each memory location advancing
 *		BUF_STRIDE bytes each time; otherwise assume both source and
 *		destination values are packed.
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_b_b(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata, size_t nelmts,
	     size_t buf_stride, size_t UNUSED bkg_stride, void *_buf,
             void UNUSED *background, hid_t dxpl_id)
{
    uint8_t	*buf = (uint8_t*)_buf;
    H5T_t	*src=NULL, *dst=NULL;	/*source and dest data types	*/
    int	direction;		/*direction of traversal	*/
    size_t	elmtno;			/*element number		*/
    size_t	olap;			/*num overlapping elements	*/
    size_t	half_size;		/*1/2 of total size for swapping*/
    uint8_t	*s, *sp, *d, *dp;	/*source and dest traversal ptrs*/
    uint8_t	dbuf[256];		/*temp destination buffer	*/
    size_t	msb_pad_offset;		/*offset for dest MSB padding	*/
    size_t	i;
    uint8_t     *src_rev;               /*order-reversed source buffer  */
    H5P_genplist_t      *plist;         /*property list pointer         */
    H5T_conv_cb_t       cb_struct={NULL, NULL};      /*conversion callback structure */
    H5T_conv_ret_t      except_ret;     /*return of callback function   */
    hbool_t             reverse;        /*if reverse the order of destination        */
    herr_t      ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_b_b, FAIL);

    switch(cdata->command) {
        case H5T_CONV_INIT:
            /* Capability query */
            if (NULL == (src = H5I_object(src_id)) ||
                    NULL == (dst = H5I_object(dst_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
            if (H5T_ORDER_LE!=src->u.atomic.order &&
                    H5T_ORDER_BE!=src->u.atomic.order)
                HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported byte order");
            if (H5T_ORDER_LE!=dst->u.atomic.order &&
                    H5T_ORDER_BE!=dst->u.atomic.order)
                HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported byte order");
            cdata->need_bkg = H5T_BKG_NO;
            break;

        case H5T_CONV_FREE:
            break;

        case H5T_CONV_CONV:
            /* Get the data types */
            if (NULL==(src=H5I_object(src_id)) ||
                    NULL==(dst=H5I_object(dst_id)))
                HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");

            /*
             * Do we process the values from beginning to end or vice versa? Also,
             * how many of the elements have the source and destination areas
             * overlapping?
             */
            if (src->size==dst->size || buf_stride) {
                sp = dp = (uint8_t*)buf;
                direction = 1;
                olap = nelmts;
            } else if (src->size>=dst->size) {
                double olap_d = HDceil((double)(dst->size)/
                                       (double)(src->size-dst->size));
                
                olap = (size_t)olap_d;
                sp = dp = (uint8_t*)buf;
                direction = 1;
            } else {
                double olap_d = HDceil((double)(src->size)/
                                       (double)(dst->size-src->size));
                olap = (size_t)olap_d;
                sp = (uint8_t*)buf + (nelmts-1) * src->size;
                dp = (uint8_t*)buf + (nelmts-1) * dst->size;
                direction = -1;
            }

            /* Get the plist structure */
            if(NULL == (plist = H5P_object_verify(dxpl_id,H5P_DATASET_XFER)))
                HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find property list for ID");

            /* Get conversion exception callback property */
            if (H5P_get(plist,H5D_XFER_CONV_CB_NAME,&cb_struct)<0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get conversion exception callback");

            /* Allocate space for order-reversed source buffer */
            src_rev = (uint8_t*)H5MM_calloc(src->size);
            
            /* The conversion loop */
            for (elmtno=0; elmtno<nelmts; elmtno++) {

                /*
                 * If the source and destination buffers overlap then use a
                 * temporary buffer for the destination.
                 */
                if (direction>0) {
                    s = sp;
                    d = elmtno<olap ? dbuf : dp;
                } else {
                    s = sp;
                    d = elmtno+olap >= nelmts ? dbuf : dp;
                }
#ifndef NDEBUG
                /* I don't quite trust the overlap calculations yet --rpm */
                if (d==dbuf) {
                    assert ((dp>=sp && dp<sp+src->size) ||
                            (sp>=dp && sp<dp+dst->size));
                } else {
                    assert ((dp<sp && dp+dst->size<=sp) ||
                            (sp<dp && sp+src->size<=dp));
                }
#endif
                
                /*
                 * Put the data in little endian order so our loops aren't so
                 * complicated.  We'll do all the conversion stuff assuming
                 * little endian and then we'll fix the order at the end.
                 */
                if (H5T_ORDER_BE==src->u.atomic.order) {
                    half_size = src->size/2;
                    for (i=0; i<half_size; i++) {
                        uint8_t tmp = s[src->size-(i+1)];
                        s[src->size-(i+1)] = s[i];
                        s[i] = tmp;
                    }
                }

                /* Initiate these variables */
                except_ret = H5T_CONV_UNHANDLED;
                reverse    = TRUE;

                /*
                 * Copy the significant part of the value. If the source is larger
                 * than the destination then invoke the overflow function or copy
                 * as many bits as possible. Zero extra bits in the destination.
                 */
                if (src->u.atomic.prec>dst->u.atomic.prec) {
                    /*overflow*/
                    if(cb_struct.func) { /*If user's exception handler is present, use it*/
                        H5T_reverse_order(src_rev, s, src->size, src->u.atomic.order); /*reverse order first*/
                        except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_RANGE_HI, src_id, dst_id, 
                                src_rev, d, cb_struct.user_data);
                    }
                    
                    if(except_ret == H5T_CONV_UNHANDLED) { 
                        H5T_bit_copy(d, dst->u.atomic.offset,
                                     s, src->u.atomic.offset, dst->u.atomic.prec);
                    } else if(except_ret == H5T_CONV_ABORT)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception")
                    else if(except_ret == H5T_CONV_HANDLED)
                        /*Don't reverse because user handles it*/
                        reverse = FALSE;
                } else {
                    H5T_bit_copy(d, dst->u.atomic.offset,
                                 s, src->u.atomic.offset,
                                 src->u.atomic.prec);
                    H5T_bit_set(d, dst->u.atomic.offset+src->u.atomic.prec,
                                dst->u.atomic.prec-src->u.atomic.prec, FALSE);
                }

                /*
                 * Fill the destination padding areas.
                 */
                switch (dst->u.atomic.lsb_pad) {
                    case H5T_PAD_ZERO:
                        H5T_bit_set(d, 0, dst->u.atomic.offset, FALSE);
                        break;
                    case H5T_PAD_ONE:
                        H5T_bit_set(d, 0, dst->u.atomic.offset, TRUE);
                        break;
                    default:
                        HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported LSB padding");
                }
                msb_pad_offset = dst->u.atomic.offset + dst->u.atomic.prec;
                switch (dst->u.atomic.msb_pad) {
                    case H5T_PAD_ZERO:
                        H5T_bit_set(d, msb_pad_offset, 8*dst->size-msb_pad_offset,
                                    FALSE);
                        break;
                    case H5T_PAD_ONE:
                        H5T_bit_set(d, msb_pad_offset, 8*dst->size-msb_pad_offset,
                                    TRUE);
                        break;
                    default:
                        HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported MSB padding");
                }

                /*
                 * Put the destination in the correct byte order.  See note at
                 * beginning of loop.
                 */
                if (H5T_ORDER_BE==dst->u.atomic.order && reverse) {
                    half_size = dst->size/2;
                    for (i=0; i<half_size; i++) {
                        uint8_t tmp = d[dst->size-(i+1)];
                        d[dst->size-(i+1)] = d[i];
                        d[i] = tmp;
                    }
                }

                /*
                 * If we had used a temporary buffer for the destination then we
                 * should copy the value to the true destination buffer.
                 */
                if (d==dbuf) HDmemcpy (dp, d, dst->size);
                if (buf_stride) {
                    sp += direction * buf_stride;
                    dp += direction * buf_stride;
                } else {
                    sp += direction * src->size;
                    dp += direction * dst->size;
                }
            }
            
            H5MM_free(src_rev);

            break;

        default:
            HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}
	    

/*-------------------------------------------------------------------------
 * Function:	H5T_conv_struct_init
 *
 * Purpose:	Initialize the `priv' field of `cdata' with conversion
 *		information that is relatively constant.  If `priv' is
 *		already initialized then the member conversion functions
 *		are recalculated.
 *
 *		Priv fields are indexed by source member number or
 *		destination member number depending on whether the field
 *		contains information about the source data type or the
 *		destination data type (fields that contains the same
 *		information for both source and destination are indexed by
 *		source member number).  The src2dst[] priv array maps source
 *		member numbers to destination member numbers, but if the
 *		source member doesn't have a corresponding destination member
 *		then the src2dst[i]=-1.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Monday, January 26, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T_conv_struct_init (H5T_t *src, H5T_t *dst, H5T_cdata_t *cdata, hid_t dxpl_id)
{
    H5T_conv_struct_t	*priv = (H5T_conv_struct_t*)(cdata->priv);
    int		*src2dst = NULL;
    unsigned		i, j;
    H5T_t		*type = NULL;
    hid_t		tid;
    herr_t      ret_value=SUCCEED;       /* Return value */
    
    FUNC_ENTER_NOAPI_NOINIT(H5T_conv_struct_init);
    
    if (!priv) {
        /*
         * Allocate private data structure and arrays.
         */
        if (NULL==(priv=cdata->priv=H5MM_calloc(sizeof(H5T_conv_struct_t))) ||
                NULL==(priv->src2dst=H5MM_malloc(src->u.compnd.nmembs *
                                 sizeof(int))) ||
                NULL==(priv->src_memb_id=H5MM_malloc(src->u.compnd.nmembs *
                                    sizeof(hid_t))) ||
                NULL==(priv->dst_memb_id=H5MM_malloc(dst->u.compnd.nmembs *
                                    sizeof(hid_t))))
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        src2dst = priv->src2dst;

        /*
         * Insure that members are sorted.
         */
        H5T_sort_value(src, NULL);
        H5T_sort_value(dst, NULL);

        /*
         * Build a mapping from source member number to destination member
         * number. If some source member is not a destination member then that
         * mapping element will be negative.  Also create atoms for each
         * source and destination member data type so we can look up the
         * member data type conversion functions later.
         */
        for (i=0; i<src->u.compnd.nmembs; i++) {
            src2dst[i] = -1;
            for (j=0; j<dst->u.compnd.nmembs; j++) {
                if (!HDstrcmp (src->u.compnd.memb[i].name,
                       dst->u.compnd.memb[j].name)) {
                    src2dst[i] = j;
                    break;
                }
            }
            if (src2dst[i]>=0) {
                type = H5T_copy (src->u.compnd.memb[i].type, H5T_COPY_ALL);
                tid = H5I_register (H5I_DATATYPE, type);
                assert (tid>=0);
                priv->src_memb_id[i] = tid;

                type = H5T_copy (dst->u.compnd.memb[src2dst[i]].type,
                     H5T_COPY_ALL);
                tid = H5I_register (H5I_DATATYPE, type);
                assert (tid>=0);
                priv->dst_memb_id[src2dst[i]] = tid;
            }
        }
    }

    /*
     * (Re)build the cache of member conversion functions and pointers to
     * their cdata entries.
     */
    src2dst = priv->src2dst;
    H5MM_xfree(priv->memb_path);
    if (NULL==(priv->memb_path=H5MM_malloc(src->u.compnd.nmembs *
					   sizeof(H5T_path_t*))))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    for (i=0; i<src->u.compnd.nmembs; i++) {
        if (src2dst[i]>=0) {
            H5T_path_t *tpath = H5T_path_find(src->u.compnd.memb[i].type,
                      dst->u.compnd.memb[src2dst[i]].type, NULL, NULL, dxpl_id);

            if (NULL==(priv->memb_path[i] = tpath)) {
                H5MM_xfree(priv->src2dst);
                H5MM_xfree(priv->src_memb_id);
                H5MM_xfree(priv->dst_memb_id);
                H5MM_xfree(priv->memb_path);
                cdata->priv = priv = H5MM_xfree (priv);
                HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unable to convert member data type");
            }
        }
    }

    /* Check if we need a background buffer */
    if (H5T_detect_class(src,H5T_COMPOUND)==TRUE || H5T_detect_class(dst,H5T_COMPOUND)==TRUE)
        cdata->need_bkg = H5T_BKG_YES;

    cdata->recalc = FALSE;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*------------------------------------------------------------------------- 
 * Function:	H5T_conv_struct
 *
 * Purpose:	Converts between compound data types.  This is a soft
 *		conversion function.  The algorithm is basically:
 *
 * 		For each element do
 *		  For I=1..NELMTS do
 *		    If sizeof detination type <= sizeof source type then
 *		      Convert member to destination type;
 *		    Move member as far left as possible;
 *		  
 *		  For I=NELMTS..1 do
 *		    If not destination type then
 *		      Convert member to destination type;
 *		    Move member to correct position in BKG
 *
 *		  Copy BKG to BUF
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Thursday, January 22, 1998
 *
 * Modifications:
 *		Robb Matzke, 1999-06-16
 *              Added support for non-zero strides. If BUF_STRIDE is
 *              non-zero then convert one value at each memory location
 *              advancing BUF_STRIDE bytes each time; otherwise assume
 *              both source and destination values are packed.
 *
 *              Robb Matzke, 2000-05-17
 *              Added the BKG_STRIDE argument to fix a design bug. If
 *              BUF_STRIDE and BKG_STRIDE are both non-zero then each
 *              data element converted will be placed temporarily at a
 *              multiple of BKG_STRIDE in the BKG buffer; otherwise the
 *              BKG buffer is assumed to be a packed array of destination
 *              datatype.
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_struct(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata, size_t nelmts,
		size_t buf_stride, size_t bkg_stride, void *_buf, void *_bkg,
                hid_t dxpl_id)
{
    uint8_t	*buf = (uint8_t *)_buf;	/*cast for pointer arithmetic	*/
    uint8_t	*bkg = (uint8_t *)_bkg;	/*background pointer arithmetic	*/
    uint8_t     *xbuf=buf, *xbkg=bkg;   /*temp pointers into buf and bkg*/
    H5T_t	*src = NULL;		/*source data type		*/
    H5T_t	*dst = NULL;		/*destination data type		*/
    int	*src2dst = NULL;	/*maps src member to dst member	*/
    H5T_cmemb_t	*src_memb = NULL;	/*source struct member descript.*/
    H5T_cmemb_t	*dst_memb = NULL;	/*destination struct memb desc.	*/
    size_t	offset;			/*byte offset wrt struct	*/
    size_t	src_delta;	    /*source stride	*/
    size_t	elmtno;
    unsigned	u;		/*counters			*/
    int	i;			/*counters			*/
    H5T_conv_struct_t *priv = (H5T_conv_struct_t *)(cdata->priv);
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_struct, FAIL);

    switch (cdata->command) {
        case H5T_CONV_INIT:
            /*
             * First, determine if this conversion function applies to the
             * conversion path SRC_ID-->DST_ID.  If not, return failure;
             * otherwise initialize the `priv' field of `cdata' with information
             * that remains (almost) constant for this conversion path.
             */
            if (NULL == (src = H5I_object(src_id)) ||
                    NULL == (dst = H5I_object(dst_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
            assert (H5T_COMPOUND==src->type);
            assert (H5T_COMPOUND==dst->type);

            if (H5T_conv_struct_init (src, dst, cdata, dxpl_id)<0)
                HGOTO_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to initialize conversion data");
            break;

        case H5T_CONV_FREE:
            /*
             * Free the private conversion data.
             */
            H5MM_xfree(priv->src2dst);
            H5MM_xfree(priv->src_memb_id);
            H5MM_xfree(priv->dst_memb_id);
            H5MM_xfree(priv->memb_path);
            cdata->priv = priv = H5MM_xfree (priv);
            break;

        case H5T_CONV_CONV:
            /*
             * Conversion.
             */
            if (NULL == (src = H5I_object(src_id)) ||
                    NULL == (dst = H5I_object(dst_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
            assert (priv);
            assert (bkg && cdata->need_bkg);

            if (cdata->recalc && H5T_conv_struct_init (src, dst, cdata, dxpl_id)<0)
                HGOTO_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to initialize conversion data");

            /*
             * Insure that members are sorted.
             */
            H5T_sort_value(src, NULL);
            H5T_sort_value(dst, NULL);
            src2dst = priv->src2dst;

            /*
             * Direction of conversion and striding through background.
             */
            if (buf_stride) {
                src_delta = buf_stride;
            if (!bkg_stride)
                bkg_stride = dst->size;
            } else if (dst->size <= src->size) {
                src_delta = src->size;
            bkg_stride = dst->size;
            } else {
                src_delta = -(int)src->size; /*overflow shouldn't be possible*/
                bkg_stride = -(int)dst->size; /*overflow shouldn't be possible*/
                xbuf += (nelmts-1) * src->size;
                xbkg += (nelmts-1) * dst->size;
            }

            /* Conversion loop... */
            for (elmtno=0; elmtno<nelmts; elmtno++) {
                /*
                 * For each source member which will be present in the
                 * destination, convert the member to the destination type unless
                 * it is larger than the source type.  Then move the member to the
                 * left-most unoccupied position in the buffer.  This makes the
                 * data point as small as possible with all the free space on the
                 * right side.
                 */
                for (u=0, offset=0; u<src->u.compnd.nmembs; u++) {
                    if (src2dst[u]<0) continue; /*subsetting*/
                    src_memb = src->u.compnd.memb + u;
                    dst_memb = dst->u.compnd.memb + src2dst[u];

                    if (dst_memb->size <= src_memb->size) {
                        if (H5T_convert(priv->memb_path[u], priv->src_memb_id[u],
                                priv->dst_memb_id[src2dst[u]],
                                1, 0, 0, /*no striding (packed array)*/
                                xbuf+src_memb->offset, xbkg+dst_memb->offset,
                                dxpl_id)<0)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to convert compound data type member");
                        HDmemmove (xbuf+offset, xbuf+src_memb->offset,
                                   dst_memb->size);
                        offset += dst_memb->size;
                    } else {
                        HDmemmove (xbuf+offset, xbuf+src_memb->offset,
                                   src_memb->size);
                        offset += src_memb->size;
                    }
                }

                /*
                 * For each source member which will be present in the
                 * destination, convert the member to the destination type if it
                 * is larger than the source type (that is, has not been converted
                 * yet).  Then copy the member to the destination offset in the
                 * background buffer.
                 */
                for (i=src->u.compnd.nmembs-1; i>=0; --i) {
                    if (src2dst[i]<0) continue; /*subsetting*/
                    src_memb = src->u.compnd.memb + i;
                    dst_memb = dst->u.compnd.memb + src2dst[i];

                    if (dst_memb->size > src_memb->size) {
                        offset -= src_memb->size;
                        if (H5T_convert(priv->memb_path[i],
                                    priv->src_memb_id[i], priv->dst_memb_id[src2dst[i]],
                                    1, 0, 0, /*no striding (packed array)*/
                                    xbuf+offset, xbkg+dst_memb->offset,
                                    dxpl_id)<0)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to convert compound data type member");
                    } else {
                        offset -= dst_memb->size;
                    }
                    HDmemmove (xbkg+dst_memb->offset, xbuf+offset, dst_memb->size);
                }
                assert (0==offset);

                /*
                 * Update pointers
                 */
                xbuf += src_delta;
                xbkg += bkg_stride;
            }

        /* If the bkg_stride was set to -(dst->size), make it positive now */
        if(buf_stride==0 && dst->size>src->size)
            bkg_stride=dst->size;

            /*
             * Copy the background buffer back into the in-place conversion
             * buffer.
             */
            for (xbuf=buf, xbkg=bkg, elmtno=0; elmtno<nelmts; elmtno++) {
                HDmemmove(xbuf, xbkg, dst->size);
                xbuf += buf_stride ? buf_stride : dst->size;
                xbkg += bkg_stride;
            }
            break;

        default:
            /* Some other command we don't know about yet.*/
            HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    }
    
done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_struct_opt
 *
 * Purpose:	Converts between compound data types in a manner more
 *		efficient than the general-purpose H5T_conv_struct()
 *		function.  This function isn't applicable if the destination
 *		is larger than the source type. This is a soft conversion
 *		function.  The algorithm is basically:
 *
 * 		For each member of the struct
 *		  If sizeof detination type <= sizeof source type then
 *		    Convert member to destination type for all elements
 *		    Move memb to BKG buffer for all elements
 *		  Else
 *		    Move member as far left as possible for all elements
 *		  
 *		For each member of the struct (in reverse order)
 *		  If not destination type then
 *		    Convert member to destination type for all elements
 *		    Move member to correct position in BKG for all elements
 *
 *		Copy BKG to BUF for all elements
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Thursday, January 22, 1998
 *
 * Modifications:
 *		Robb Matzke, 1999-06-16
 *              Added support for non-zero strides. If BUF_STRIDE is
 *              non-zero then convert one value at each memory location
 *              advancing BUF_STRIDE bytes each time; otherwise assume both
 *              source and destination values are packed.
 *
 * 		Robb Matzke, 1999-06-16
 *		If the source and destination data structs are the same size
 *		then we can convert on a field-by-field basis instead of an
 *		element by element basis. In other words, for all struct
 *		elements being converted by this function call, first convert
 *		all of the field1's, then all field2's, etc. This can
 *		drastically reduce the number of calls to H5T_convert() and
 *		thereby eliminate most of the conversion constant overhead.
 *              
 *              Robb Matzke, 2000-05-17
 *              Added the BKG_STRIDE argument to fix a design bug. If
 *              BUF_STRIDE and BKG_STRIDE are both non-zero then each
 *              data element converted will be placed temporarily at a
 *              multiple of BKG_STRIDE in the BKG buffer; otherwise the
 *              BKG buffer is assumed to be a packed array of destination
 *              datatype.
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_struct_opt(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
    size_t nelmts, size_t buf_stride, size_t bkg_stride, void *_buf,
    void *_bkg, hid_t dxpl_id)
{
    uint8_t	*buf = (uint8_t *)_buf;	/*cast for pointer arithmetic	*/
    uint8_t	*bkg = (uint8_t *)_bkg;	/*background pointer arithmetic	*/
    uint8_t	*xbuf = NULL;		/*temporary pointer into `buf'	*/
    uint8_t	*xbkg = NULL;		/*temporary pointer into `bkg'	*/
    H5T_t	*src = NULL;		/*source data type		*/
    H5T_t	*dst = NULL;		/*destination data type		*/
    int	*src2dst = NULL;	/*maps src member to dst member	*/
    H5T_cmemb_t	*src_memb = NULL;	/*source struct member descript.*/
    H5T_cmemb_t	*dst_memb = NULL;	/*destination struct memb desc.	*/
    size_t	offset;			/*byte offset wrt struct	*/
    size_t	elmtno;			/*element counter		*/
    unsigned	u;			/*counters			*/
    int	i;			    /*counters			*/
    H5T_conv_struct_t *priv = NULL;	/*private data			*/
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_struct_opt, FAIL);

    switch (cdata->command) {
        case H5T_CONV_INIT:
            /*
             * First, determine if this conversion function applies to the
             * conversion path SRC_ID-->DST_ID.  If not, return failure;
             * otherwise initialize the `priv' field of `cdata' with information
             * that remains (almost) constant for this conversion path.
             */
            if (NULL == (src = H5I_object(src_id)) ||
                    NULL == (dst = H5I_object(dst_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
            assert (H5T_COMPOUND==src->type);
            assert (H5T_COMPOUND==dst->type);

            /* Initialize data which is relatively constant */
            if (H5T_conv_struct_init (src, dst, cdata, dxpl_id)<0)
                HGOTO_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to initialize conversion data");
            priv = (H5T_conv_struct_t *)(cdata->priv);
            src2dst = priv->src2dst;
                
            /*
             * If the destination type is not larger than the source type then
             * this conversion function is guaranteed to work (provided all
             * members can be converted also). Otherwise the determination is
             * quite a bit more complicated. Essentially we have to make sure
             * that there is always room in the source buffer to do the
             * conversion of a member in place. This is basically the same pair
             * of loops as in the actual conversion except it checks that there
             * is room for each conversion instead of actually doing anything.
             */
            if (dst->size > src->size) {
                for (u=0, offset=0; u<src->u.compnd.nmembs; u++) {
                    if (src2dst[u]<0)
                        continue;
                    src_memb = src->u.compnd.memb + u;
                    dst_memb = dst->u.compnd.memb + src2dst[u];
                    if (dst_memb->size > src_memb->size)
                        offset += src_memb->size;
                }
                for (i=src->u.compnd.nmembs-1; i>=0; --i) {
                    if (src2dst[i]<0)
                        continue;
                    src_memb = src->u.compnd.memb + i;
                    dst_memb = dst->u.compnd.memb + src2dst[i];
                    if (dst_memb->size > src_memb->size) {
                        offset -= src_memb->size;
                        if (dst_memb->size > src->size-offset) {
                            H5MM_xfree(priv->src2dst);
                            H5MM_xfree(priv->src_memb_id);
                            H5MM_xfree(priv->dst_memb_id);
                            H5MM_xfree(priv->memb_path);
                            cdata->priv = priv = H5MM_xfree (priv);
                            HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "convertion is unsupported by this function");
                        }
                    }
                }
            }
            break;

        case H5T_CONV_FREE:
            /*
             * Free the private conversion data.
             */
            priv = (H5T_conv_struct_t *)(cdata->priv);
            H5MM_xfree(priv->src2dst);
            H5MM_xfree(priv->src_memb_id);
            H5MM_xfree(priv->dst_memb_id);
            H5MM_xfree(priv->memb_path);
            cdata->priv = priv = H5MM_xfree (priv);
            break;

        case H5T_CONV_CONV:
            /*
             * Conversion.
             */
            if (NULL == (src = H5I_object(src_id)) ||
                    NULL == (dst = H5I_object(dst_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");

            /* Update cached data if necessary */
            if (cdata->recalc && H5T_conv_struct_init (src, dst, cdata, dxpl_id)<0)
                HGOTO_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to initialize conversion data");
            priv = (H5T_conv_struct_t *)(cdata->priv);
            src2dst = priv->src2dst;
            assert(priv);
            assert(bkg && cdata->need_bkg);

            /*
             * Insure that members are sorted.
             */
            H5T_sort_value(src, NULL);
            H5T_sort_value(dst, NULL);

            /* 
             * Calculate strides. If BUF_STRIDE is non-zero then convert one
             * data element at every BUF_STRIDE bytes through the main buffer
             * (BUF), leaving the result of each conversion at the same
             * location; otherwise assume the source and destination data are
             * packed tightly based on src->size and dst->size.  Also, if
             * BUF_STRIDE and BKG_STRIDE are both non-zero then place
             * background data into the BKG buffer at multiples of BKG_STRIDE;
             * otherwise assume BKG buffer is the packed destination datatype.
             */
            if (!buf_stride || !bkg_stride) bkg_stride = dst->size;

            /*
             * For each member where the destination is not larger than the
             * source, stride through all the elements converting only that member
             * in each element and then copying the element to its final
             * destination in the bkg buffer. Otherwise move the element as far
             * left as possible in the buffer.
             */
            for (u=0, offset=0; u<src->u.compnd.nmembs; u++) {
                if (src2dst[u]<0) continue; /*subsetting*/
                src_memb = src->u.compnd.memb + u;
                dst_memb = dst->u.compnd.memb + src2dst[u];

                if (dst_memb->size <= src_memb->size) {
                    xbuf = buf + src_memb->offset;
                    xbkg = bkg + dst_memb->offset;
                    if (H5T_convert(priv->memb_path[u],
                            priv->src_memb_id[u],
                            priv->dst_memb_id[src2dst[u]], nelmts,
                            buf_stride ? buf_stride : src->size,
                            bkg_stride, xbuf, xbkg,
                            dxpl_id)<0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to convert compound data type member");
                    for (elmtno=0; elmtno<nelmts; elmtno++) {
                        HDmemmove(xbkg, xbuf, dst_memb->size);
                        xbuf += buf_stride ? buf_stride : src->size;
                        xbkg += bkg_stride;
                    }
                } else {
                    for (xbuf=buf, elmtno=0; elmtno<nelmts; elmtno++) {
                        HDmemmove(xbuf+offset, xbuf+src_memb->offset,
                                              src_memb->size);
                        xbuf += buf_stride ? buf_stride : src->size;
                    }
                    offset += src_memb->size;
                }
            }

            /*
             * Work from right to left, converting those members that weren't
             * converted in the previous loop (those members where the destination
             * is larger than the source) and them to their final position in the
             * bkg buffer.
             */
            for (i=src->u.compnd.nmembs-1; i>=0; --i) {
                if (src2dst[i]<0)
                    continue;
                src_memb = src->u.compnd.memb + i;
                dst_memb = dst->u.compnd.memb + src2dst[i];

                if (dst_memb->size > src_memb->size) {
                    offset -= src_memb->size;
                    xbuf = buf + offset;
                    xbkg = bkg + dst_memb->offset;
                    if (H5T_convert(priv->memb_path[i],
                                    priv->src_memb_id[i],
                                    priv->dst_memb_id[src2dst[i]], nelmts,
                                    buf_stride ? buf_stride : src->size,
                                    bkg_stride, xbuf, xbkg,
                                    dxpl_id)<0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to convert compound data type member");
                    for (elmtno=0; elmtno<nelmts; elmtno++) {
                        HDmemmove(xbkg, xbuf, dst_memb->size);
                        xbuf += buf_stride ? buf_stride : src->size;
                        xbkg += bkg_stride;
                    }
                }
            }

            /* Move background buffer into result buffer */
            for (xbuf=buf, xbkg=bkg, elmtno=0; elmtno<nelmts; elmtno++) {
                HDmemmove(xbuf, xbkg, dst->size);
                xbuf += buf_stride ? buf_stride : dst->size;
                xbkg += bkg_stride;
            }
            break;

        default:
            /* Some other command we don't know about yet.*/
            HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    }
    
done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_enum_init
 *
 * Purpose:	Initialize information for H5T_conv_enum().
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Monday, January  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T_conv_enum_init(H5T_t *src, H5T_t *dst, H5T_cdata_t *cdata)
{
    H5T_enum_struct_t	*priv=NULL;	/*private conversion data	*/
    int		n;		/*src value cast as native int	*/
    int		domain[2];	/*min and max source values	*/
    int		*map=NULL;	/*map from src value to dst idx	*/
    unsigned	length;		/*nelmts in map array		*/
    unsigned	i, j;		/*counters			*/
    herr_t      ret_value=SUCCEED;       /* Return value */
    
    FUNC_ENTER_NOAPI_NOINIT(H5T_conv_enum_init);

    cdata->need_bkg = H5T_BKG_NO;
    if (NULL==(priv=cdata->priv=H5MM_calloc(sizeof(*priv))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    if (0==src->u.enumer.nmembs)
	HGOTO_DONE(SUCCEED);

    /*
     * Check that the source symbol names are a subset of the destination
     * symbol names and build a map from source member index to destination
     * member index.
     */
    H5T_sort_name(src, NULL);
    H5T_sort_name(dst, NULL);
    if (NULL==(priv->src2dst=H5MM_malloc(src->u.enumer.nmembs*sizeof(int))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");;
    for (i=0, j=0;
             i<src->u.enumer.nmembs && j<dst->u.enumer.nmembs;
             i++, j++) {
	while (j<dst->u.enumer.nmembs &&
	       HDstrcmp(src->u.enumer.name[i], dst->u.enumer.name[j]))
            j++;
	if (j>=dst->u.enumer.nmembs)
	    HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "source type is not a subset of destination type");
	priv->src2dst[i] = j;
    }

    /*
     * The conversion function will use an O(log N) lookup method for each
     * value converted. However, if all of the following constraints are met
     * then we can build a perfect hash table and use an O(1) lookup method.
     *
     *	  A: The source data type size matches one of our native data type
     *	     sizes.
     *	     
     *	  B: After casting the source value bit pattern to a native type
     *	     the size of the range of values is less than 20% larger than
     *	     the number of values.
     *
     * If this special case is met then we use the source bit pattern cast as
     * a native integer type as an index into the `val2dst'. The values of
     * that array are the index numbers in the destination type or negative
     * if the entry is unused.
     */
    if (1==src->size || sizeof(short)==src->size || sizeof(int)==src->size) {
	for (i=0; i<src->u.enumer.nmembs; i++) {
	    if (1==src->size) {
		n = *((signed char*)(src->u.enumer.value+i));
	    } else if (sizeof(short)==src->size) {
		n = *((short*)(src->u.enumer.value+i*src->size));
	    } else {
		n = *((int*)(src->u.enumer.value+i*src->size));
	    }
	    if (0==i) {
		domain[0] = domain[1] = n;
	    } else {
		domain[0] = MIN(domain[0], n);
		domain[1] = MAX(domain[1], n);
	    }
	}

	length = (domain[1]-domain[0])+1;
	if (src->u.enumer.nmembs<2 ||
	    (double)length/src->u.enumer.nmembs<1.2) {
	    priv->base = domain[0];
	    priv->length = length;
	    if (NULL==(map=H5MM_malloc(length*sizeof(int))))
		HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
	    for (i=0; i<length; i++)
                map[i] = -1; /*entry unused*/
	    for (i=0; i<src->u.enumer.nmembs; i++) {
		if (1==src->size) {
		    n = *((signed char*)(src->u.enumer.value+i));
		} else if (sizeof(short)==src->size) {
		    n = *((short*)(src->u.enumer.value+i*src->size));
		} else {
		    n = *((int*)(src->u.enumer.value+i*src->size));
		}
		n -= priv->base;
		assert(n>=0 && n<priv->length);
		assert(map[n]<0);
		map[n] = priv->src2dst[i];
	    }

	    /*
	     * Replace original src2dst array with our new one. The original
	     * was indexed by source member number while the new one is
	     * indexed by source values.
	     */
	    H5MM_xfree(priv->src2dst);
	    priv->src2dst = map;
	    HGOTO_DONE(SUCCEED);
	}
    }

    /* Sort source type by value and adjust src2dst[] appropriately */
    H5T_sort_value(src, priv->src2dst);

done:
    if (ret_value<0 && priv) {
	H5MM_xfree(priv->src2dst);
	H5MM_xfree(priv);
	cdata->priv = NULL;
    }
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_enum
 *
 * Purpose:	Converts one type of enumerated data to another.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *              Monday, January  4, 1999
 *
 * Modifications:
 *		Robb Matzke, 1999-06-16
 *		Added support for non-zero strides. If BUF_STRIDE is non-zero
 *		then convert one value at each memory location advancing
 *		BUF_STRIDE bytes each time; otherwise assume both source and
 *		destination values are packed.
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_enum(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata, size_t nelmts,
	      size_t buf_stride, size_t UNUSED bkg_stride, void *_buf,
              void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    uint8_t	*buf = (uint8_t*)_buf;	/*cast for pointer arithmetic	*/
    H5T_t	*src=NULL, *dst=NULL;	/*src and dst data types	*/
    uint8_t	*s=NULL, *d=NULL;	/*src and dst BUF pointers	*/
    int	src_delta, dst_delta;	/*conversion strides		*/
    int	n;			/*src value cast as native int	*/
    size_t	i;			/*counters			*/
    H5T_enum_struct_t *priv = (H5T_enum_struct_t*)(cdata->priv);
    H5P_genplist_t      *plist;         /*property list pointer         */
    H5T_conv_cb_t       cb_struct;      /*conversion callback structure */
    H5T_conv_ret_t      except_ret;     /*return of callback function   */
    herr_t      ret_value=SUCCEED;      /* Return value                 */
    
    FUNC_ENTER_NOAPI(H5T_conv_enum, FAIL);

    switch (cdata->command) {
        case H5T_CONV_INIT:
            /*
             * Determine if this conversion function applies to the conversion
             * path SRC_ID->DST_ID.  If not return failure; otherwise initialize
             * the `priv' field of `cdata' with information about the underlying
             * integer conversion.
             */
            if (NULL == (src = H5I_object(src_id)) ||
                    NULL == (dst = H5I_object(dst_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
            assert (H5T_ENUM==src->type);
            assert (H5T_ENUM==dst->type);
            if (H5T_conv_enum_init(src, dst, cdata)<0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to initialize private data");
            break;

        case H5T_CONV_FREE:
#ifdef H5T_DEBUG
            if (H5DEBUG(T)) {
                fprintf(H5DEBUG(T), "      Using %s mapping function%s\n",
                        priv->length?"O(1)":"O(log N)",
                        priv->length?"":", where N is the number of enum members");
            }
#endif
            if (priv) {
                H5MM_xfree(priv->src2dst);
                H5MM_xfree(priv);
            }
            cdata->priv = NULL;
            break;

        case H5T_CONV_CONV:
            if (NULL == (src = H5I_object(src_id)) ||
                    NULL == (dst = H5I_object(dst_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
            assert (H5T_ENUM==src->type);
            assert (H5T_ENUM==dst->type);

            /* priv->src2dst map was computed for certain sort keys. Make sure those same
             * sort keys are used here during conversion. See H5T_conv_enum_init(). But
             * we actually don't care about the source type's order when doing the O(1)
             * conversion algorithm, which is turned on by non-zero priv->length */
            H5T_sort_name(dst, NULL);
            if (!priv->length) H5T_sort_value(src, NULL);
            
            /*
             * Direction of conversion.
             */
            if (buf_stride) {
                src_delta = dst_delta = (int)buf_stride;
                s = d = buf;
            } else if (dst->size <= src->size) {
                src_delta = (int)src->size; /*overflow shouldn't be possible*/
                dst_delta = (int)dst->size; /*overflow shouldn't be possible*/
                s = d = buf;
            } else {
                src_delta = -(int)src->size; /*overflow shouldn't be possible*/
                dst_delta = -(int)dst->size; /*overflow shouldn't be possible*/
                s = buf + (nelmts-1) * src->size;
                d = buf + (nelmts-1) * dst->size;
            }
            
            /* Get the plist structure */
            if(NULL == (plist = H5P_object_verify(dxpl_id,H5P_DATASET_XFER)))
                HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find property list for ID");

            /* Get conversion exception callback property */
            if (H5P_get(plist,H5D_XFER_CONV_CB_NAME,&cb_struct)<0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get conversion exception callback");
 
            for (i=0; i<nelmts; i++, s+=src_delta, d+=dst_delta) {
                if (priv->length) {
                    /* Use O(1) lookup */
                    if (1==src->size) {
                        n = *((signed char*)s);
                    } else if (sizeof(short)==src->size) {
                        n = *((short*)s);
                    } else {
                        n = *((int*)s);
                    }
                    n -= priv->base;

                    except_ret = H5T_CONV_UNHANDLED;
                    if (n<0 || n>=priv->length || priv->src2dst[n]<0) {
                        /*overflow*/
                        if(cb_struct.func) { /*If user's exception handler is present, use it*/
                            except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_RANGE_HI, src_id, dst_id, 
                                    s, d, cb_struct.user_data);
                        }
                        
                        if(except_ret == H5T_CONV_UNHANDLED) { 
                            HDmemset(d, 0xff, dst->size);
                        } else if(except_ret == H5T_CONV_ABORT)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception")
                    } else {
                        HDmemcpy(d,
                                 dst->u.enumer.value+priv->src2dst[n]*dst->size,
                                 dst->size);
                    }
                } else {
                    /* Use O(log N) lookup */
                    int lt = 0;
                    int rt = src->u.enumer.nmembs;
                    int md, cmp;
                    while (lt<rt) {
                        md = (lt+rt)/2;
                        cmp = HDmemcmp(s, src->u.enumer.value+md*src->size,
                                       src->size);
                        if (cmp<0) {
                            rt = md;
                        } else if (cmp>0) {
                            lt = md+1;
                        } else {
                            break;
                        }
                    }
                    if (lt>=rt) {
                        if(cb_struct.func) { /*If user's exception handler is present, use it*/
                            except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_RANGE_HI, src_id, dst_id, 
                                    src, d, cb_struct.user_data);
                        }
                        
                        if(except_ret == H5T_CONV_UNHANDLED) { 
                            HDmemset(d, 0xff, dst->size);
                        } else if(except_ret == H5T_CONV_ABORT)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception")
                    } else {
                        HDmemcpy(d,
                                 dst->u.enumer.value+priv->src2dst[md]*dst->size,
                                 dst->size);
                    }
                }
            }
            break;

        default:
            /* Some other command we don't know about yet.*/
            HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_vlen
 *
 * Purpose:	Converts between VL data types in memory and on disk.
 *		This is a soft conversion function.  The algorithm is
 *		basically:
 *
 *      	For every VL struct in the main buffer:
 *		  1. Allocate space for temporary dst VL data (reuse buffer
 *		     if possible) 
 *                2. Copy VL data from src buffer into dst buffer
 *                3. Convert VL data into dst representation
 *                4. Allocate buffer in dst heap
 *		  5. Free heap objects storing old data
 *                6. Write dst VL data into dst heap
 *                7. Store (heap ID or pointer) and length in main dst buffer
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, May 26, 1999
 *
 * Modifications:
 *
 *		Quincey Koziol, 2 July, 1999
 *		Enabled support for non-zero strides. If BUF_STRIDE is non-zero
 *		then convert one value at each memory location advancing
 *		BUF_STRIDE bytes each time; otherwise assume both source and
 *		destination values are packed.
 *
 *		Raymond Lu, 26 June, 2002
 *		Background buffer is used for freeing heap objects storing 
 * 		old data.  At this moment, it only frees the first level of 
 *		VL datatype.  It doesn't handle nested VL datatypes.    
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_vlen(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata, size_t nelmts,
	      size_t buf_stride, size_t bkg_stride, void *buf,
              void *bkg, hid_t dxpl_id)
{
    H5T_vlen_alloc_info_t _vl_alloc_info;       /* VL allocation info buffer */
    H5T_vlen_alloc_info_t *vl_alloc_info=&_vl_alloc_info;   /* VL allocation info */
    H5T_path_t	*tpath;			/* Type conversion path		     */
    hbool_t     noop_conv=FALSE;        /* Flag to indicate a noop conversion */
    hbool_t     write_to_file=FALSE;    /* Flag to indicate writing to file */
    hid_t   	tsrc_id = -1, tdst_id = -1;/*temporary type atoms	     */
    H5T_t	*src = NULL;		/*source data type		     */
    H5T_t	*dst = NULL;		/*destination data type		     */
    H5HG_t	bg_hobjid, parent_hobjid;
    uint8_t	*s;		        /*source buffer			*/
    uint8_t	*d;		        /*destination buffer		*/
    uint8_t	*b;		        /*background buffer		*/
    ssize_t	s_stride, d_stride;	/*src and dst strides		*/
    ssize_t	b_stride;	        /*bkg stride			*/
    size_t      safe;                   /*how many elements are safe to process in each pass */
    ssize_t 	seq_len;                /*the number of elements in the current sequence*/
    size_t	bg_seq_len=0;
    size_t	src_base_size, dst_base_size;/*source & destination base size*/
    void	*conv_buf=NULL;     	/*temporary conversion buffer 	     */
    size_t	conv_buf_size=0;  	/*size of conversion buffer in bytes */
    void	*tmp_buf=NULL;     	/*temporary background buffer 	     */
    size_t	tmp_buf_size=0;	        /*size of temporary bkg buffer	     */
    hbool_t     nested=FALSE;           /*flag of nested VL case             */
    size_t	elmtno;			/*element number counter	     */
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_vlen, FAIL);

    switch (cdata->command) {
        case H5T_CONV_INIT:
            /*
             * First, determine if this conversion function applies to the
             * conversion path SRC_ID-->DST_ID.  If not, return failure;
             * otherwise initialize the `priv' field of `cdata' with
             * information that remains (almost) constant for this
             * conversion path.
             */
            if (NULL == (src = H5I_object(src_id)) ||
                    NULL == (dst = H5I_object(dst_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
            assert (H5T_VLEN==src->type);
            assert (H5T_VLEN==dst->type);

            /* Variable-length types don't need a background buffer */
            cdata->need_bkg = H5T_BKG_NO;

            break;

        case H5T_CONV_FREE:
            /* QAK - Nothing to do currently */
            break;

        case H5T_CONV_CONV:
            /*
             * Conversion.
             */
            if (NULL == (src = H5I_object(src_id)) ||
                    NULL == (dst = H5I_object(dst_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");

            /* Initialize source & destination strides */
            if (buf_stride) {
                assert(buf_stride>=src->size);
                assert(buf_stride>=dst->size);
                s_stride = d_stride = buf_stride;
            } else {
                s_stride = src->size;
                d_stride = dst->size;
            }
            if(bkg) {
                if(bkg_stride)
                    b_stride=bkg_stride;
                else
                    b_stride=d_stride;
            } /* end if */
            else
                b_stride=0;

            /* Get the size of the base types in src & dst */
            src_base_size=H5T_get_size(src->parent);
            dst_base_size=H5T_get_size(dst->parent);

            /* Set up conversion path for base elements */
            if (NULL==(tpath=H5T_path_find(src->parent, dst->parent, NULL, NULL, dxpl_id))) {
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unable to convert between src and dest datatypes");
            } else if (!H5T_path_noop(tpath)) {
                if ((tsrc_id = H5I_register(H5I_DATATYPE, H5T_copy(src->parent, H5T_COPY_ALL)))<0 ||
                        (tdst_id = H5I_register(H5I_DATATYPE, H5T_copy(dst->parent, H5T_COPY_ALL)))<0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTREGISTER, FAIL, "unable to register types for conversion");
            } else
                noop_conv=TRUE;

            /* Check if we need a temporary buffer for this conversion */
            if(tpath->cdata.need_bkg || H5T_detect_class(dst->parent,H5T_VLEN)) {
                /* Set up initial background buffer */
                tmp_buf_size=MAX(src_base_size,dst_base_size);
                if ((tmp_buf=H5FL_BLK_MALLOC(vlen_seq,tmp_buf_size))==NULL)
                    HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for type conversion");
            } /* end if */
    
            /* Get the allocation info */
            if(H5T_vlen_get_alloc_info(dxpl_id,&vl_alloc_info)<0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "unable to retrieve VL allocation info");

            /* Set flags to indicate we are writing to or reading from the file */
            if(dst->u.vlen.f!=NULL)
                write_to_file=TRUE;

            /* Set the flag for nested VL case */
            if(write_to_file && H5T_detect_class(dst->parent,H5T_VLEN) && bkg!=NULL)
                nested=1;

            /* The outer loop of the type conversion macro, controlling which */
            /* direction the buffer is walked */
            while (nelmts>0) {
                /* Check if we need to go backwards through the buffer */
                if(d_stride>s_stride) {
                    /* Compute the number of "safe" destination elements at */
                    /* the end of the buffer (Those which don't overlap with */
                    /* any source elements at the beginning of the buffer) */
                    safe=nelmts-(((nelmts*s_stride)+(d_stride-1))/d_stride);

                    /* If we're down to the last few elements, just wrap up */
                    /* with a "real" reverse copy */
                    if(safe<2) {
                        s = (uint8_t*)buf+(nelmts-1)*s_stride;
                        d = (uint8_t*)buf+(nelmts-1)*d_stride;
                        b = (uint8_t*)bkg+(nelmts-1)*b_stride;
                        s_stride = -s_stride;
                        d_stride = -d_stride;
                        b_stride = -b_stride;

                        safe=nelmts;
                    } /* end if */
                    else {
                        s = (uint8_t*)buf+(nelmts-safe)*s_stride;
                        d = (uint8_t*)buf+(nelmts-safe)*d_stride;
                        b = (uint8_t*)bkg+(nelmts-safe)*b_stride;
                    } /* end else */
                } /* end if */
                else {
                    /* Single forward pass over all data */
                    s = d = buf;
                    b = bkg;
                    safe=nelmts;
                } /* end else */

                for (elmtno=0; elmtno<safe; elmtno++) {
                    /* Check for "nil" source sequence */
                    if((*(src->u.vlen.isnull))(src->u.vlen.f,s)) {
                        /* Write "nil" sequence to destination location */
                        if((*(dst->u.vlen.setnull))(dst->u.vlen.f,dxpl_id,d,b)<0)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_WRITEERROR, FAIL, "can't set VL data to 'nil'");
                    } /* end if */
                    else {
                        /* Get length of element sequences */
                        if((seq_len=(*(src->u.vlen.getlen))(s))<0)
                            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "incorrect length");

                        /* If we are reading from memory and there is no conversion, just get the pointer to sequence */
                        if(write_to_file && noop_conv) {
                            /* Get direct pointer to sequence */
                            if((conv_buf=(*(src->u.vlen.getptr))(s))==NULL)
                                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid source pointer");
                        } /* end if */
                        else {
                            size_t	src_size, dst_size;     /*source & destination total size in bytes*/

                            src_size=seq_len*src_base_size;
                            dst_size=seq_len*dst_base_size;

                            /* Check if conversion buffer is large enough, resize if
                             * necessary */      
                            if(conv_buf_size<MAX(src_size,dst_size)) {
                                /* Only allocate conversion buffer in H5T_VLEN_MIN_CONF_BUF_SIZE increments */
                                conv_buf_size=((MAX(src_size,dst_size)/H5T_VLEN_MIN_CONF_BUF_SIZE)+1)*H5T_VLEN_MIN_CONF_BUF_SIZE;
                                if((conv_buf=H5FL_BLK_REALLOC(vlen_seq,conv_buf, conv_buf_size))==NULL)
                                    HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for type conversion");
                            } /* end if */

                            /* Read in VL sequence */
                            if((*(src->u.vlen.read))(src->u.vlen.f,dxpl_id,s,conv_buf,src_size)<0)
                                HGOTO_ERROR(H5E_DATATYPE, H5E_READERROR, FAIL, "can't read VL data");
                        } /* end else */

                        if(!noop_conv) {
                            /* Check if temporary buffer is large enough, resize if necessary */
                            /* (Chain off the conversion buffer size) */
                            if(tmp_buf && tmp_buf_size<conv_buf_size) {
                                /* Set up initial background buffer */
                                tmp_buf_size=conv_buf_size;
                                if((tmp_buf=H5FL_BLK_REALLOC(vlen_seq,tmp_buf,tmp_buf_size))==NULL)
                                    HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for type conversion");
                            } /* end if */

                            /* If we are writing and there is a nested VL type, read 
                             * the sequence into the background buffer */
                            if(nested) {
                                uint8_t *tmp=b;
                                UINT32DECODE(tmp, bg_seq_len);

                                if(bg_seq_len>0) {
                                    if(tmp_buf_size<(bg_seq_len*MAX(src_base_size, dst_base_size))) {
                                        tmp_buf_size=(bg_seq_len*MAX(src_base_size, dst_base_size));
                                        if((tmp_buf=H5FL_BLK_REALLOC(vlen_seq,tmp_buf, tmp_buf_size))==NULL)
                                            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for type conversion");
                                    }
                                    H5F_addr_decode(dst->u.vlen.f, (const uint8_t **)&tmp, &(bg_hobjid.addr));
                                    INT32DECODE(tmp, bg_hobjid.idx);
                                    if(H5HG_read(dst->u.vlen.f,dxpl_id,&bg_hobjid,tmp_buf)==NULL)
                                        HGOTO_ERROR (H5E_DATATYPE, H5E_READERROR, FAIL, "can't read VL sequence into background buffer");
                                } /* end if */

                                /* If the sequence gets shorter, pad out the original sequence with zeros */
                                H5_CHECK_OVERFLOW(bg_seq_len,size_t,ssize_t);
                                if((ssize_t)bg_seq_len<seq_len) {
                                    HDmemset((uint8_t *)tmp_buf+dst_base_size*bg_seq_len,0,(seq_len-bg_seq_len)*dst_base_size);
                                } /* end if */
                            } /* end if */

                            /* Convert VL sequence */
                            H5_CHECK_OVERFLOW(seq_len,ssize_t,size_t);
                            if (H5T_convert(tpath, tsrc_id, tdst_id, (size_t)seq_len, 0, 0, conv_buf, tmp_buf, dxpl_id)<0)
                                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "datatype conversion failed");
                        } /* end if */

                        /* Write sequence to destination location */
                        if((*(dst->u.vlen.write))(dst->u.vlen.f,dxpl_id,vl_alloc_info,d,conv_buf, b, (size_t)seq_len, dst_base_size)<0)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_WRITEERROR, FAIL, "can't write VL data");

                        if(!noop_conv) {
                            /* For nested VL case, free leftover heap objects from the deeper level if the length of new data elements is shorter than the old data elements.*/
                            H5_CHECK_OVERFLOW(bg_seq_len,size_t,ssize_t);
                            if(nested && seq_len<(ssize_t)bg_seq_len) {
                                size_t parent_seq_len;
                                size_t     u;

                                uint8_t *tmp_p=tmp_buf;
                                tmp_p += seq_len*dst_base_size;
                                for(u=0; u<(bg_seq_len-seq_len); u++) {
                                    UINT32DECODE(tmp_p, parent_seq_len);
                                    if(parent_seq_len>0) {
                                        H5F_addr_decode(dst->u.vlen.f, (const uint8_t **)&tmp_p, &(parent_hobjid.addr));
                                        INT32DECODE(tmp_p, parent_hobjid.idx);
                                        if(H5HG_remove(dst->u.vlen.f, dxpl_id,&parent_hobjid)<0)
                                            HGOTO_ERROR(H5E_DATATYPE, H5E_WRITEERROR, FAIL, "Unable to remove heap object");
                                    }
                                }
                            } /* end if */
                        } /* end if */
                    } /* end else */

                    /* Advance pointers */
                    s += s_stride;
                    d += d_stride;
                    b += b_stride;
                } /* end for */

                /* Decrement number of elements left to convert */
                nelmts-=safe;
            } /* end while */

            /* Reset the conversion buffer pointer, so it doesn't get freed */
            if(write_to_file && noop_conv)
                conv_buf=NULL;

            /* Release the temporary datatype IDs used */
            if (tsrc_id >= 0)
                H5I_dec_ref(tsrc_id);
            if (tdst_id >= 0)
                H5I_dec_ref(tdst_id);
            break;

        default:    /* Some other command we don't know about yet.*/
            HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    }   /* end switch */
    
done:
    /* Release the conversion buffer (always allocated, except on errors) */
    if(conv_buf!=NULL)
        H5FL_BLK_FREE(vlen_seq,conv_buf);
    /* Release the background buffer, if we have one */
    if(tmp_buf!=NULL)
        H5FL_BLK_FREE(vlen_seq,tmp_buf);

    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_array
 *
 * Purpose:	Converts between array data types in memory and on disk.
 *		This is a soft conversion function.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Monday, November 6, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_array(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata, size_t nelmts,
	      size_t buf_stride, size_t bkg_stride, void *_buf,
              void UNUSED *_bkg, hid_t dxpl_id)
{
    H5T_path_t	*tpath;		        /* Type conversion path		     */
    hid_t       tsrc_id = -1, tdst_id = -1;/*temporary type atoms	     */
    H5T_t	*src = NULL;	        /*source data type		     */
    H5T_t	*dst = NULL;	        /*destination data type		     */
    uint8_t	*sp, *dp;	        /*source and dest traversal ptrs     */
    size_t	src_delta, dst_delta;	/*source & destination stride	     */
    int	        direction;		/*direction of traversal	     */
    size_t	elmtno;			/*element number counter	     */
    int         i;                      /* local index variable */
    void	*bkg_buf=NULL;     	/*temporary background buffer 	     */
    size_t	bkg_buf_size=0;	        /*size of background buffer in bytes */
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_array, FAIL);

    switch (cdata->command) {
        case H5T_CONV_INIT:
            /*
             * First, determine if this conversion function applies to the
             * conversion path SRC_ID-->DST_ID.  If not, return failure;
             * otherwise initialize the `priv' field of `cdata' with
             * information that remains (almost) constant for this
             * conversion path.
             */
            if (NULL == (src = H5I_object(src_id)) ||
                    NULL == (dst = H5I_object(dst_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
            assert (H5T_ARRAY==src->type);
            assert (H5T_ARRAY==dst->type);

            /* Check the number and sizes of the dimensions */
            if(src->u.array.ndims!=dst->u.array.ndims)
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "array datatypes do not have the same number of dimensions");
            for(i=0; i<src->u.array.ndims; i++)
                if(src->u.array.dim[i]!=dst->u.array.dim[i])
                    HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "array datatypes do not have the same sizes of dimensions");
#ifdef LATER
            for(i=0; i<src->u.array.ndims; i++)
                if(src->u.array.perm[i]!=dst->u.array.perm[i])
                    HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "array datatypes do not have the same dimension permutations");
#endif /* LATER */

            /* Array datatypes don't need a background buffer */
            cdata->need_bkg = H5T_BKG_NO;

            break;

        case H5T_CONV_FREE:
            /* QAK - Nothing to do currently */
            break;

        case H5T_CONV_CONV:
            /*
             * Conversion.
             */
            if (NULL == (src = H5I_object(src_id)) ||
                    NULL == (dst = H5I_object(dst_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");

            /*
             * Do we process the values from beginning to end or vice
             * versa? Also, how many of the elements have the source and
             * destination areas overlapping?
             */
            if (src->size>=dst->size || buf_stride>0) {
                sp = dp = (uint8_t*)_buf;
                direction = 1;
            } else {
                sp = (uint8_t*)_buf + (nelmts-1) *
                     (buf_stride ? buf_stride : src->size);
                dp = (uint8_t*)_buf + (nelmts-1) *
                     (buf_stride ? buf_stride : dst->size);
                direction = -1;
            }

            /*
             * Direction & size of buffer traversal.
             */
            src_delta = direction * (buf_stride ? buf_stride : src->size);
            dst_delta = direction * (buf_stride ? buf_stride : dst->size);

            /* Set up conversion path for base elements */
            if (NULL==(tpath=H5T_path_find(src->parent, dst->parent, NULL, NULL, dxpl_id))) {
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unable to convert between src and dest datatypes");
            } else if (!H5T_path_noop(tpath)) {
                if ((tsrc_id = H5I_register(H5I_DATATYPE, H5T_copy(src->parent, H5T_COPY_ALL)))<0 ||
                        (tdst_id = H5I_register(H5I_DATATYPE, H5T_copy(dst->parent, H5T_COPY_ALL)))<0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTREGISTER, FAIL, "unable to register types for conversion");
            }

            /* Check if we need a background buffer for this conversion */
            if(tpath->cdata.need_bkg) {
                /* Allocate background buffer */
                bkg_buf_size=src->u.array.nelem*MAX(src->size,dst->size);
                if ((bkg_buf=H5FL_BLK_MALLOC(array_seq,bkg_buf_size))==NULL)
                    HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for type conversion");
            } /* end if */

            /* Perform the actual conversion */
            for (elmtno=0; elmtno<nelmts; elmtno++) {
                /* Copy the source array into the correct location for the destination */
                HDmemmove(dp, sp, src->size);

                /* Convert array */
                if (H5T_convert(tpath, tsrc_id, tdst_id, src->u.array.nelem, 0, bkg_stride, dp, bkg_buf, dxpl_id)<0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "datatype conversion failed");

                /* Advance the source & destination pointers */
                sp += src_delta;
                dp += dst_delta;
            }

            /* Release the background buffer, if we have one */
            if(bkg_buf!=NULL)
                H5FL_BLK_FREE(array_seq,bkg_buf);

            /* Release the temporary datatype IDs used */
            if (tsrc_id >= 0)
                H5I_dec_ref(tsrc_id);
            if (tdst_id >= 0)
                H5I_dec_ref(tdst_id);
            break;

        default:    /* Some other command we don't know about yet.*/
            HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    }   /* end switch */
    
done:
    FUNC_LEAVE_NOAPI(ret_value);
}   /* end H5T_conv_array() */


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_i_i
 *
 * Purpose:	Convert one integer type to another.  This is the catch-all
 *		function for integer conversions and is probably not
 *		particularly fast.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Wednesday, June 10, 1998
 *
 * Modifications:
 *		Robb Matzke, 7 Jul 1998
 *		Added overflow handling.
 *
 *		Robb Matzke, 1999-06-16
 *		Added support for non-zero strides. If BUF_STRIDE is non-zero
 *		then convert one value at each memory location advancing
 *		BUF_STRIDE bytes each time; otherwise assume both source and
 *		destination values are packed.
 *
 *              Raymond Lu
 *              Wednesday, April 21, 2004
 *              There is a new design for exception handling like overflow,
 *              which is passed in as a transfer property.
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_i_i (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata, size_t nelmts,
	      size_t buf_stride, size_t UNUSED bkg_stride, void *buf,
              void UNUSED *bkg, hid_t dxpl_id)
{
    H5T_t	*src = NULL;		/*source data type		*/
    H5T_t	*dst = NULL;		/*destination data type		*/
    int		direction;		/*direction of traversal	*/
    size_t	elmtno;			/*element number		*/
    size_t	half_size;		/*half the type size		*/
    size_t	olap;			/*num overlapping elements	*/
    uint8_t	*s, *sp, *d, *dp;	/*source and dest traversal ptrs*/
    uint8_t     *src_rev;               /*order-reversed source buffer  */
    uint8_t	dbuf[64];		/*temp destination buffer	*/
    size_t	first;
    ssize_t	sfirst;			/*a signed version of `first'	*/
    size_t	i;                      /*Local index variables         */
    H5P_genplist_t      *plist;         /*property list pointer         */
    H5T_conv_cb_t       cb_struct={NULL, NULL};      /*conversion callback structure */
    H5T_conv_ret_t      except_ret;     /*return of callback function   */
    hbool_t             reverse;        /*if reverse the order of destination        */
    herr_t      ret_value=SUCCEED;       /* Return value */
    
    FUNC_ENTER_NOAPI(H5T_conv_i_i, FAIL);

    switch (cdata->command) {
        case H5T_CONV_INIT:
            if (NULL==(src=H5I_object(src_id)) ||
                    NULL==(dst=H5I_object(dst_id)))
                HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
            if (H5T_ORDER_LE!=src->u.atomic.order &&
                    H5T_ORDER_BE!=src->u.atomic.order)
                HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported byte order");
            if (H5T_ORDER_LE!=dst->u.atomic.order &&
                    H5T_ORDER_BE!=dst->u.atomic.order)
                HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported byte order");
            if (dst->size>sizeof dbuf)
                HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "destination size is too large");
            cdata->need_bkg = H5T_BKG_NO;
            break;
            
        case H5T_CONV_FREE:
            break;

        case H5T_CONV_CONV:
            /* Get the data types */
            if (NULL==(src=H5I_object(src_id)) ||
                    NULL==(dst=H5I_object(dst_id)))
                HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");

            /*
             * Do we process the values from beginning to end or vice versa? Also,
             * how many of the elements have the source and destination areas
             * overlapping?
             */
            if (src->size==dst->size || buf_stride) {
                sp = dp = (uint8_t*)buf;
                direction = 1;
                olap = nelmts;
            } else if (src->size>=dst->size) {
                double olap_d = HDceil((double)(dst->size)/
                                       (double)(src->size-dst->size));
                
                olap = (size_t)olap_d;
                sp = dp = (uint8_t*)buf;
                direction = 1;
            } else {
                double olap_d = HDceil((double)(src->size)/
                                       (double)(dst->size-src->size));
                olap = (size_t)olap_d;
                sp = (uint8_t*)buf + (nelmts-1) * src->size;
                dp = (uint8_t*)buf + (nelmts-1) * dst->size;
                direction = -1;
            }

            /* Get the plist structure */
            if(NULL == (plist = H5P_object_verify(dxpl_id,H5P_DATASET_XFER)))
                HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find property list for ID");

            /* Get conversion exception callback property */
            if (H5P_get(plist,H5D_XFER_CONV_CB_NAME,&cb_struct)<0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get conversion exception callback");

            /* Allocate space for order-reversed source buffer */
            src_rev = (uint8_t*)H5MM_calloc(src->size);

            /* The conversion loop */
            for (elmtno=0; elmtno<nelmts; elmtno++) {

                /*
                 * If the source and destination buffers overlap then use a
                 * temporary buffer for the destination.
                 */
                if (direction>0) {
                    s = sp;
                    d = elmtno<olap ? dbuf : dp;
                } else {
                    s = sp;
                    d = elmtno+olap >= nelmts ? dbuf : dp;
                }
#ifndef NDEBUG
                /* I don't quite trust the overlap calculations yet --rpm */
                if (d==dbuf) {
                    assert ((dp>=sp && dp<sp+src->size) || (sp>=dp && sp<dp+dst->size));
                } else {
                    assert ((dp<sp && dp+dst->size<=sp) || (sp<dp && sp+src->size<=dp));
                }
#endif
                
                /*
                 * Put the data in little endian order so our loops aren't so
                 * complicated.  We'll do all the conversion stuff assuming
                 * little endian and then we'll fix the order at the end.
                 */
                if (H5T_ORDER_BE==src->u.atomic.order) {
                    half_size = src->size/2;
                    for (i=0; i<half_size; i++) {
                        uint8_t tmp = s[src->size-(i+1)];
                        s[src->size-(i+1)] = s[i];
                        s[i] = tmp;
                    }
                }

                /*
                 * What is the bit number for the msb bit of S which is set? The
                 * bit number is relative to the significant part of the number.
                 */
                sfirst = H5T_bit_find (s, src->u.atomic.offset, src->u.atomic.prec,
                                       H5T_BIT_MSB, TRUE);
                first = (size_t)sfirst;

                /* Set these variables to default */
                except_ret = H5T_CONV_UNHANDLED;
                reverse    = TRUE;

                if (sfirst<0) {
                    /*
                     * The source has no bits set and must therefore be zero.
                     * Set the destination to zero.
                     */
                    H5T_bit_set (d, dst->u.atomic.offset, dst->u.atomic.prec, FALSE);
                        
                } else if (H5T_SGN_NONE==src->u.atomic.u.i.sign &&
                           H5T_SGN_NONE==dst->u.atomic.u.i.sign) {
                    /*
                     * Source and destination are both unsigned, but if the
                     * source has more precision bits than the destination then
                     * it's possible to overflow.  When overflow occurs the
                     * destination will be set to the maximum possible value.
                     */
                    if (src->u.atomic.prec <= dst->u.atomic.prec) {
                        H5T_bit_copy (d, dst->u.atomic.offset, s, src->u.atomic.offset,
                              src->u.atomic.prec);
                        H5T_bit_set (d, dst->u.atomic.offset+src->u.atomic.prec,
                             dst->u.atomic.prec-src->u.atomic.prec, FALSE);
                    } else if (first>=dst->u.atomic.prec) {
                        /*overflow*/
                        if(cb_struct.func) { /*If user's exception handler is present, use it*/
                            H5T_reverse_order(src_rev, s, src->size, src->u.atomic.order); /*reverse order first*/
                            except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_RANGE_HI, src_id, dst_id, 
                                    src_rev, d, cb_struct.user_data);
                        }
                        
                        if(except_ret == H5T_CONV_UNHANDLED) { 
                            H5T_bit_set (d, dst->u.atomic.offset, dst->u.atomic.prec, TRUE);
                        } else if(except_ret == H5T_CONV_ABORT)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception")
                        else if(except_ret == H5T_CONV_HANDLED)
                            /*Don't reverse because user handles it already*/
                            reverse = FALSE;
                    } else {
                        H5T_bit_copy (d, dst->u.atomic.offset, s, src->u.atomic.offset,
                              dst->u.atomic.prec);
                    }
                    
                } else if (H5T_SGN_2==src->u.atomic.u.i.sign &&
                           H5T_SGN_NONE==dst->u.atomic.u.i.sign) {
                    /*
                     * If the source is signed and the destination isn't then we
                     * can have overflow if the source contains more bits than
                     * the destination (destination is set to the maximum
                     * possible value) or overflow if the source is negative
                     * (destination is set to zero).
                     */
                    if (first+1 == src->u.atomic.prec) {
                        /*overflow - source is negative*/
                        if(cb_struct.func) { /*If user's exception handler is present, use it*/
                            H5T_reverse_order(src_rev, s, src->size, src->u.atomic.order); /*reverse order first*/
                            except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_RANGE_LOW, src_id, dst_id, 
                                    src_rev, d, cb_struct.user_data);
                        }
                        
                        if(except_ret == H5T_CONV_UNHANDLED) { 
                            H5T_bit_set (d, dst->u.atomic.offset, dst->u.atomic.prec, FALSE);
                        } else if(except_ret == H5T_CONV_ABORT)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception")
                        else if(except_ret == H5T_CONV_HANDLED)
                            /*Don't reverse because user handles it already*/
                            reverse = FALSE;
                    } else if (src->u.atomic.prec < dst->u.atomic.prec) {
                        H5T_bit_copy (d, dst->u.atomic.offset, s, src->u.atomic.offset,
                              src->u.atomic.prec-1);
                        H5T_bit_set (d, dst->u.atomic.offset+src->u.atomic.prec-1,
                             (dst->u.atomic.prec-src->u.atomic.prec)+1, FALSE);
                    } else if (first>=dst->u.atomic.prec) {
                        /*overflow - source is positive*/
                        if(cb_struct.func) { /*If user's exception handler is present, use it*/
                            H5T_reverse_order(src_rev, s, src->size, src->u.atomic.order); /*reverse order first*/
                            except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_RANGE_HI, src_id, dst_id, 
                                    src_rev, d, cb_struct.user_data);
                        }
                        
                        if(except_ret == H5T_CONV_UNHANDLED)
                            H5T_bit_set (d, dst->u.atomic.offset, dst->u.atomic.prec, TRUE);
                        else if(except_ret == H5T_CONV_ABORT)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception")
                        else if(except_ret == H5T_CONV_HANDLED)
                            /*Don't reverse because user handles it already*/
                            reverse = FALSE;
                    } else {
                        H5T_bit_copy (d, dst->u.atomic.offset, s, src->u.atomic.offset,
                              dst->u.atomic.prec);
                    }
                    
                } else if (H5T_SGN_NONE==src->u.atomic.u.i.sign &&
                           H5T_SGN_2==dst->u.atomic.u.i.sign) {
                    /*
                     * If the source is not signed but the destination is then
                     * overflow can occur in which case the destination is set to
                     * the largest possible value (all bits set except the msb).
                     */
                    if (first+1 >= dst->u.atomic.prec) {
                        /*overflow*/
                        if(cb_struct.func) { /*If user's exception handler is present, use it*/
                            H5T_reverse_order(src_rev, s, src->size, src->u.atomic.order); /*reverse order first*/
                            except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_RANGE_HI, src_id, dst_id, 
                                    src_rev, d, cb_struct.user_data);
                        }
                        
                        if(except_ret == H5T_CONV_UNHANDLED) { 
                            H5T_bit_set (d, dst->u.atomic.offset, dst->u.atomic.prec-1, TRUE);
                            H5T_bit_set (d, (dst->u.atomic.offset + dst->u.atomic.prec-1), 1, FALSE);
                        } else if(except_ret == H5T_CONV_ABORT)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception")
                        else if(except_ret == H5T_CONV_HANDLED)
                            /*Don't reverse because user handles it already*/
                            reverse = FALSE;
                    } else if (src->u.atomic.prec<dst->u.atomic.prec) {
                        H5T_bit_copy (d, dst->u.atomic.offset, s, src->u.atomic.offset,
                              src->u.atomic.prec);
                        H5T_bit_set (d, dst->u.atomic.offset+src->u.atomic.prec,
                             dst->u.atomic.prec-src->u.atomic.prec, FALSE);
                    } else {
                        H5T_bit_copy (d, dst->u.atomic.offset, s, src->u.atomic.offset,
                              dst->u.atomic.prec);
                    }
                } else if (first+1 == src->u.atomic.prec) {
                    /*
                     * Both the source and the destination are signed and the
                     * source value is negative.  We could experience overflow
                     * if the destination isn't wide enough in which case the
                     * destination is set to a negative number with the largest
                     * possible magnitude.
                     */
                    ssize_t sfz = H5T_bit_find (s, src->u.atomic.offset,
                                    src->u.atomic.prec-1, H5T_BIT_MSB, FALSE);
                    size_t fz = (size_t)sfz;
                    
                    if (sfz>=0 && fz+1>=dst->u.atomic.prec) {
                        /*overflow*/
                        if(cb_struct.func) { /*If user's exception handler is present, use it*/
                            H5T_reverse_order(src_rev, s, src->size, src->u.atomic.order); /*reverse order first*/
                            except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_RANGE_LOW, src_id, dst_id, 
                                    src_rev, d, cb_struct.user_data);
                        }
                        
                        if(except_ret == H5T_CONV_UNHANDLED) { 
                            H5T_bit_set (d, dst->u.atomic.offset, dst->u.atomic.prec-1, FALSE);
                            H5T_bit_set (d, (dst->u.atomic.offset + dst->u.atomic.prec-1), 1, TRUE);
                        } else if(except_ret == H5T_CONV_ABORT)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception")
                        else if(except_ret == H5T_CONV_HANDLED)
                            /*Don't reverse because user handles it already*/
                            reverse = FALSE;
                    } else if (src->u.atomic.prec<dst->u.atomic.prec) {
                        H5T_bit_copy (d, dst->u.atomic.offset, s, src->u.atomic.offset, src->u.atomic.prec);
                        H5T_bit_set (d, dst->u.atomic.offset+src->u.atomic.prec, dst->u.atomic.prec-src->u.atomic.prec, TRUE);
                    } else {
                        H5T_bit_copy (d, dst->u.atomic.offset, s, src->u.atomic.offset, dst->u.atomic.prec);
                    }
                    
                } else {
                    /*
                     * Source and destination are both signed but the source
                     * value is positive.  We could have an overflow in which
                     * case the destination is set to the largest possible
                     * positive value.
                     */
                    if (first+1>=dst->u.atomic.prec) {
                        /*overflow*/
                        if(cb_struct.func) { /*If user's exception handler is present, use it*/
                            H5T_reverse_order(src_rev, s, src->size, src->u.atomic.order); /*reverse order first*/
                            except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_RANGE_HI, src_id, dst_id, src_rev, d, 
                                    cb_struct.user_data);
                        }
                        
                        if(except_ret == H5T_CONV_UNHANDLED) { 
                            H5T_bit_set (d, dst->u.atomic.offset, dst->u.atomic.prec-1, TRUE);
                            H5T_bit_set (d, (dst->u.atomic.offset + dst->u.atomic.prec-1), 1, FALSE);
                        } else if(except_ret == H5T_CONV_ABORT)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception")
                        else if(except_ret == H5T_CONV_HANDLED)
                            /*Don't reverse because user handles it already*/
                            reverse = FALSE;
                    } else if (src->u.atomic.prec<dst->u.atomic.prec) {
                        H5T_bit_copy (d, dst->u.atomic.offset, s, src->u.atomic.offset,
                              src->u.atomic.prec);
                        H5T_bit_set (d, dst->u.atomic.offset+src->u.atomic.prec,
                             dst->u.atomic.prec-src->u.atomic.prec, FALSE);
                    } else {
                        H5T_bit_copy (d, dst->u.atomic.offset, s, src->u.atomic.offset,
                              dst->u.atomic.prec);
                    }
                }

                /*
                 * Set padding areas in destination.
                 */
                if (dst->u.atomic.offset>0) {
                    assert (H5T_PAD_ZERO==dst->u.atomic.lsb_pad || H5T_PAD_ONE==dst->u.atomic.lsb_pad);
                    H5T_bit_set (d, 0, dst->u.atomic.offset, (hbool_t)(H5T_PAD_ONE==dst->u.atomic.lsb_pad));
                }
                if (dst->u.atomic.offset+dst->u.atomic.prec!=8*dst->size) {
                    assert (H5T_PAD_ZERO==dst->u.atomic.msb_pad || H5T_PAD_ONE==dst->u.atomic.msb_pad);
                    H5T_bit_set (d, dst->u.atomic.offset+dst->u.atomic.prec,
                                 8*dst->size - (dst->u.atomic.offset+ dst->u.atomic.prec),
                                 (hbool_t)(H5T_PAD_ONE==dst->u.atomic.msb_pad));
                }

                /*
                 * Put the destination in the correct byte order.  See note at
                 * beginning of loop.
                 */
                if (H5T_ORDER_BE==dst->u.atomic.order && reverse) {
                    half_size = dst->size/2;
                    for (i=0; i<half_size; i++) {
                        uint8_t tmp = d[dst->size-(i+1)];
                        d[dst->size-(i+1)] = d[i];
                        d[i] = tmp;
                    }
                }

                /*
                 * If we had used a temporary buffer for the destination then we
                 * should copy the value to the true destination buffer.
                 */
                if (d==dbuf)
                    HDmemcpy (dp, d, dst->size);
                if (buf_stride) {
                    sp += direction * buf_stride;
                    dp += direction * buf_stride;
                } else {
                    sp += direction * src->size;
                    dp += direction * dst->size;
                }
            }
            
            H5MM_free(src_rev);

            break;

        default:
            HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_f_f
 *
 * Purpose:	Convert one floating point type to another.  This is a catch
 *		all for floating point conversions and is probably not
 *		particularly fast!
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Tuesday, June 23, 1998
 *
 * Modifications:
 *		Robb Matzke, 7 Jul 1998
 *		Added overflow handling.
 *
 *		Robb Matzke, 1999-06-16
 *		Added support for non-zero strides. If BUF_STRIDE is non-zero
 *		then convert one value at each memory location advancing
 *		BUF_STRIDE bytes each time; otherwise assume both source and
 *		destination values are packed.
 *
 *              Robb Matzke, 2001-02-02
 *              Oops, forgot to increment the exponent when rounding the
 *              significand resulted in a carry. Thanks to Guillaume Colin
 *              de Verdiere for finding this one!
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_f_f (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata, size_t nelmts,
    size_t buf_stride, size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
    hid_t dxpl_id)
{
    /* Traversal-related variables */
    H5T_t	*src_p;			/*source data type		*/
    H5T_t	*dst_p;			/*destination data type		*/
    H5T_atomic_t src;			/*atomic source info		*/
    H5T_atomic_t dst;			/*atomic destination info	*/
    int	direction;		        /*forward or backward traversal	*/
    size_t	elmtno;			/*element number		*/
    size_t	half_size;		/*half the type size		*/
    size_t	olap;			/*num overlapping elements	*/
    ssize_t	bitno;			/*bit number			*/
    uint8_t	*s, *sp, *d, *dp;	/*source and dest traversal ptrs*/
    uint8_t     *src_rev;               /*order-reversed source buffer  */
    uint8_t	dbuf[64];		/*temp destination buffer	*/

    /* Conversion-related variables */
    hssize_t	expo;			/*exponent			*/
    hssize_t	expo_max;		/*maximum possible dst exponent	*/
    size_t	msize=0;		/*useful size of mantissa in src*/
    size_t	mpos;			/*offset to useful mant is src	*/
    size_t	mrsh;			/*amount to right shift mantissa*/
    hbool_t	carry=0;		/*carry after rounding mantissa	*/
    size_t	i;			/*miscellaneous counters	*/
    size_t	implied;		/*destination implied bits	*/
    H5P_genplist_t      *plist;         /*property list pointer         */
    H5T_conv_cb_t       cb_struct={NULL, NULL};      /*conversion callback structure */
    H5T_conv_ret_t      except_ret;     /*return of callback function   */
    hbool_t             reverse;        /*if reverse the order of destination        */
    herr_t      ret_value=SUCCEED;      /*return value                 */
    
    FUNC_ENTER_NOAPI(H5T_conv_f_f, FAIL);

    switch (cdata->command) {
        case H5T_CONV_INIT:
            if (NULL==(src_p=H5I_object(src_id)) ||
                    NULL==(dst_p=H5I_object(dst_id)))
                HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
            src = src_p->u.atomic;
            dst = dst_p->u.atomic;
            if (H5T_ORDER_LE!=src.order && H5T_ORDER_BE!=src.order)
                HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported byte order");
            if (H5T_ORDER_LE!=dst.order && H5T_ORDER_BE!=dst.order)
                HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported byte order");
            if (dst_p->size>sizeof(dbuf))
                HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "destination size is too large");
            if (8*sizeof(expo)-1<src.u.f.esize || 8*sizeof(expo)-1<dst.u.f.esize)
                HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "exponent field is too large");
            cdata->need_bkg = H5T_BKG_NO;
            break;

        case H5T_CONV_FREE:
            break;

        case H5T_CONV_CONV:
            /* Get the data types */
            if (NULL==(src_p=H5I_object(src_id)) ||
                    NULL==(dst_p=H5I_object(dst_id)))
                HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
            src = src_p->u.atomic;
            dst = dst_p->u.atomic;
            expo_max = ((hssize_t)1 << dst.u.f.esize) - 1;

            /*
             * Do we process the values from beginning to end or vice versa? Also,
             * how many of the elements have the source and destination areas
             * overlapping?
             */
            if (src_p->size==dst_p->size || buf_stride) {
                sp = dp = (uint8_t*)buf;
                direction = 1;
                olap = nelmts;
            } else if (src_p->size>=dst_p->size) {
                double olap_d = HDceil((double)(dst_p->size)/
                                       (double)(src_p->size-dst_p->size));
                olap = (size_t)olap_d;
                sp = dp = (uint8_t*)buf;
                direction = 1;
            } else {
                double olap_d = HDceil((double)(src_p->size)/
                                       (double)(dst_p->size-src_p->size));
                olap = (size_t)olap_d;
                sp = (uint8_t*)buf + (nelmts-1) * src_p->size;
                dp = (uint8_t*)buf + (nelmts-1) * dst_p->size;
                direction = -1;
            }

            /* Get the plist structure */
            if(NULL == (plist = H5P_object_verify(dxpl_id,H5P_DATASET_XFER)))
                HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find property list for ID");

            /* Get conversion exception callback property */
            if (H5P_get(plist,H5D_XFER_CONV_CB_NAME,&cb_struct)<0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get conversion exception callback");

            /* Allocate space for order-reversed source buffer */
            src_rev = (uint8_t*)H5MM_calloc(src_p->size);

            /* The conversion loop */
            for (elmtno=0; elmtno<nelmts; elmtno++) {
                /*
                 * If the source and destination buffers overlap then use a
                 * temporary buffer for the destination.
                 */
                if (direction>0) {
                    s = sp;
                    d = elmtno<olap ? dbuf : dp;
                } else {
                    s = sp;
                    d = elmtno+olap >= nelmts ? dbuf : dp;
                }
#ifndef NDEBUG
                /* I don't quite trust the overlap calculations yet --rpm */
                if (d==dbuf) {
                    assert ((dp>=sp && dp<sp+src_p->size) ||
                            (sp>=dp && sp<dp+dst_p->size));
                } else {
                    assert ((dp<sp && dp+dst_p->size<=sp) ||
                            (sp<dp && sp+src_p->size<=dp));
                }
#endif
                
                /*
                 * Put the data in little endian order so our loops aren't so
                 * complicated.  We'll do all the conversion stuff assuming
                 * little endian and then we'll fix the order at the end.
                 */
                if (H5T_ORDER_BE==src.order) {
                    half_size = src_p->size/2;
                    for (i=0; i<half_size; i++) {
                        uint8_t tmp = s[src_p->size-(i+1)];
                        s[src_p->size-(i+1)] = s[i];
                        s[i] = tmp;
                    }
                }

                /*
                 * Check for special cases: +0, -0, +Inf, -Inf, NaN
                 */
                if (H5T_bit_find (s, src.u.f.mpos, src.u.f.msize,
                                  H5T_BIT_LSB, TRUE)<0) {
                    if (H5T_bit_find (s, src.u.f.epos, src.u.f.esize,
                                      H5T_BIT_LSB, TRUE)<0) {
                        /* +0 or -0 */
                        H5T_bit_copy (d, dst.u.f.sign, s, src.u.f.sign, 1);
                        H5T_bit_set (d, dst.u.f.epos, dst.u.f.esize, FALSE);
                        H5T_bit_set (d, dst.u.f.mpos, dst.u.f.msize, FALSE);
                        goto padding;
                    } else if (H5T_bit_find (s, src.u.f.epos, src.u.f.esize,
                                             H5T_BIT_LSB, FALSE)<0) {
                        /* +Inf or -Inf */
                        H5T_bit_copy (d, dst.u.f.sign, s, src.u.f.sign, 1);
                        H5T_bit_set (d, dst.u.f.epos, dst.u.f.esize, TRUE);
                        H5T_bit_set (d, dst.u.f.mpos, dst.u.f.msize, FALSE);
                        goto padding;
                    }
                } else if (H5T_bit_find (s, src.u.f.epos, src.u.f.esize,
                                         H5T_BIT_LSB, FALSE)<0) {
                    /*
                     * NaN. There are many NaN values, so we just set all bits of
                     * the significand.
                     */
                    H5T_bit_copy (d, dst.u.f.sign, s, src.u.f.sign, 1);
                    H5T_bit_set (d, dst.u.f.epos, dst.u.f.esize, TRUE);
                    H5T_bit_set(d, dst.u.f.mpos, dst.u.f.msize, TRUE);
                    goto padding;
                }

                /*
                 * Get the exponent as an unsigned quantity from the section of
                 * the source bit field where it's located.	 Don't worry about
                 * the exponent bias yet.
                 */
                expo = H5T_bit_get_d(s, src.u.f.epos, src.u.f.esize);
                
                /*
                 * Set markers for the source mantissa, excluding the leading `1'
                 * (might be implied).
                 */
                implied = 1;
                mpos = src.u.f.mpos;
                mrsh = 0;
                if (0==expo || H5T_NORM_NONE==src.u.f.norm) {
                    if ((bitno=H5T_bit_find(s, src.u.f.mpos, src.u.f.msize,
                                            H5T_BIT_MSB, TRUE))>0) {
                        msize = bitno;
                    } else if (0==bitno) {
                        msize = 1;
                        H5T_bit_set(s, src.u.f.mpos, 1, FALSE);
                    }
                } else if (H5T_NORM_IMPLIED==src.u.f.norm) {
                    msize = src.u.f.msize;
                } else {
                    assert("normalization method not implemented yet" && 0);
                    HDabort();
                }
                
                /*
                 * The sign for the destination is the same as the sign for the
                 * source in all cases.
                 */
                H5T_bit_copy (d, dst.u.f.sign, s, src.u.f.sign, 1);

                /*
                 * Calculate the true source exponent by adjusting according to
                 * the source exponent bias.
                 */
                if (0==expo || H5T_NORM_NONE==src.u.f.norm) {
                    bitno = H5T_bit_find(s, src.u.f.mpos, src.u.f.msize,
                                         H5T_BIT_MSB, TRUE);
                    assert(bitno>=0);
                    expo -= (src.u.f.ebias-1) + (src.u.f.msize-bitno);
                } else if (H5T_NORM_IMPLIED==src.u.f.norm) {
                    expo -= src.u.f.ebias;
                } else {
                    assert("normalization method not implemented yet" && 0);
                    HDabort();
                }

                /*
                 * If the destination is not normalized then right shift the
                 * mantissa by one.
                 */
                if (H5T_NORM_NONE==dst.u.f.norm)
                    mrsh++;
                
                /* Set these variables to default */
                except_ret = H5T_CONV_UNHANDLED;
                reverse    = TRUE;

                /*
                 * Calculate the destination exponent by adding the destination
                 * bias and clipping by the minimum and maximum possible
                 * destination exponent values.
                 */
                expo += dst.u.f.ebias;
                if (expo < -(hssize_t)(dst.u.f.msize)) {
                    /* The exponent is way too small.  Result is zero. */
                    expo = 0;
                    H5T_bit_set(d, dst.u.f.mpos, dst.u.f.msize, FALSE);
                    msize = 0;

                } else if (expo<=0) {
                    /*
                     * The exponent is too small to fit in the exponent field,
                     * but by shifting the mantissa to the right we can
                     * accomodate that value.  The mantissa of course is no
                     * longer normalized.
                     */
                    H5_ASSIGN_OVERFLOW(mrsh,(mrsh+1-expo),hssize_t,size_t);
                    /*mrsh += 1-expo;*/
                    expo = 0;
                    
                } else if (expo>=expo_max) {
                    /*
                     * The exponent is too large to fit in the available region
                     * or it results in the maximum possible value.	 Use positive
                     * or negative infinity instead unless the application
                     * specifies something else.  Before calling the overflow
                     * handler make sure the source buffer we hand it is in the
                     * original byte order.
                     */
                    if(cb_struct.func) { /*If user's exception handler is present, use it*/
                        H5T_reverse_order(src_rev, s, src_p->size, src_p->u.atomic.order); /*reverse order first*/
                        except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_RANGE_HI, src_id, dst_id, 
                                src_rev, d, cb_struct.user_data);
                    }
                    
                    if(except_ret == H5T_CONV_UNHANDLED) { 
                        expo = expo_max;
                        H5T_bit_set(d, dst.u.f.mpos, dst.u.f.msize, FALSE);
                        msize = 0;
                    } else if(except_ret == H5T_CONV_ABORT)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception")
                    else if(except_ret == H5T_CONV_HANDLED) {
                        reverse = FALSE;
                        /*goto next;*/
                        goto padding;
                    }
                }

                /*
                 * If the destination mantissa is smaller than the source
                 * mantissa then round the source mantissa.	 Rounding may cause a
                 * carry in which case the exponent has to be re-evaluated for
                 * overflow.  That is, if `carry' is clear then the implied
                 * mantissa bit is `1', else it is `10' binary.
                 */
                if (msize>0 && mrsh<=dst.u.f.msize && mrsh+msize>dst.u.f.msize) {
                    bitno = (ssize_t)(mrsh+msize - dst.u.f.msize);
                    assert(bitno>=0 && (size_t)bitno<=msize);
                    carry = H5T_bit_inc(s, mpos+bitno-1, 1+msize-bitno);
                    if (carry)
                        implied = 2;
                }
                else
                    carry=0;

                /*
                 * Write the mantissa to the destination
                 */
                if (mrsh>dst.u.f.msize+1) {
                    H5T_bit_set(d, dst.u.f.mpos, dst.u.f.msize, FALSE);
                } else if (mrsh==dst.u.f.msize+1) {
                    H5T_bit_set(d, dst.u.f.mpos+1, dst.u.f.msize-1, FALSE);
                    H5T_bit_set(d, dst.u.f.mpos, 1, TRUE);
                } else if (mrsh==dst.u.f.msize) {
                    H5T_bit_set(d, dst.u.f.mpos, dst.u.f.msize, FALSE);
                    H5T_bit_set_d(d, dst.u.f.mpos, MIN(2, dst.u.f.msize), implied);
                } else {
                    if (mrsh>0) {
                        H5T_bit_set(d, dst.u.f.mpos+dst.u.f.msize-mrsh, mrsh,
                                    FALSE);
                        H5T_bit_set_d(d, dst.u.f.mpos+dst.u.f.msize-mrsh, 2,
                                      implied);
                    }
                    if (mrsh+msize>=dst.u.f.msize) {
                        H5T_bit_copy(d, dst.u.f.mpos,
                                     s, (mpos+msize+mrsh-dst.u.f.msize), 
                                     dst.u.f.msize-mrsh);
                    } else {
                        H5T_bit_copy(d, dst.u.f.mpos+dst.u.f.msize-(mrsh+msize),
                                     s, mpos, msize);
                        H5T_bit_set(d, dst.u.f.mpos, dst.u.f.msize-(mrsh+msize),
                                    FALSE);
                    }
                }
                    
                /* Write the exponent */
#ifdef OLD_WAY  
/* It appears to be incorrect to increment the exponent when the carry is set -QAK */
                if (carry) {
                    expo++;
                    if (expo>=expo_max) {
                        /*
                         * The exponent is too large to fit in the available
                         * region or it results in the maximum possible value.
                         * Use positive or negative infinity instead unless the
                         * application specifies something else.  Before
                         * calling the overflow handler make sure the source
                         * buffer we hand it is in the original byte order.
                         */
                        if(cb_struct.func) { /*If user's exception handler is present, use it*/
                            H5T_reverse_order(src_rev, s, src_p->size, src_p->u.atomic.order); /*reverse order first*/
                            except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_RANGE_HI, src_id, dst_id, 
                                    src_rev, d, cb_struct.user_data);
                        }
                        
                        if(except_ret == H5T_CONV_UNHANDLED) { 
                            expo = expo_max;
                            H5T_bit_set(d, dst.u.f.mpos, dst.u.f.msize, FALSE);
                        } else if(except_ret == H5T_CONV_ABORT)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception")
                        else if(except_ret == H5T_CONV_HANDLED) {
                            reverse = FALSE;
                            /*goto next;*/
                            goto padding;
                        }
                    }
                }
#endif /* OLD_WAY */
                H5_CHECK_OVERFLOW(expo,hssize_t,hsize_t);
                H5T_bit_set_d(d, dst.u.f.epos, dst.u.f.esize, (hsize_t)expo);

            padding:
#ifndef LATER
                /*
                 * Set internal padding areas
                 */
#endif

                /*
                 * Set external padding areas
                 */
                if (dst.offset>0) {
                    assert (H5T_PAD_ZERO==dst.lsb_pad || H5T_PAD_ONE==dst.lsb_pad);
                    H5T_bit_set (d, 0, dst.offset, (hbool_t)(H5T_PAD_ONE==dst.lsb_pad));
                }
                if (dst.offset+dst.prec!=8*dst_p->size) {
                    assert (H5T_PAD_ZERO==dst.msb_pad || H5T_PAD_ONE==dst.msb_pad);
                    H5T_bit_set (d, dst.offset+dst.prec, 8*dst_p->size - (dst.offset+dst.prec),
                         (hbool_t)(H5T_PAD_ONE==dst.msb_pad));
                }

                /*
                 * Put the destination in the correct byte order.  See note at
                 * beginning of loop.
                 */
                if (H5T_ORDER_BE==dst.order && reverse) {
                    half_size = dst_p->size/2;
                    for (i=0; i<half_size; i++) {
                        uint8_t tmp = d[dst_p->size-(i+1)];
                        d[dst_p->size-(i+1)] = d[i];
                        d[i] = tmp;
                    }
                }

                /*
                 * If we had used a temporary buffer for the destination then we
                 * should copy the value to the true destination buffer.
                 */
            /* next: */
                if (d==dbuf)
                    HDmemcpy (dp, d, dst_p->size);
                if (buf_stride) {
                    sp += direction * buf_stride;
                    dp += direction * buf_stride;
                } else {
                    sp += direction * src_p->size;
                    dp += direction * dst_p->size;
                }
            }
            
            H5MM_free(src_rev);

            break;
                
        default:
            HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_s_s
 *
 * Purpose:	Convert one fixed-length string type to another.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Friday, August	7, 1998
 *
 * Modifications:
 *		Robb Matzke, 1999-06-16
 *		Added support for non-zero strides. If BUF_STRIDE is non-zero
 *		then convert one value at each memory location advancing
 *		BUF_STRIDE bytes each time; otherwise assume both source and
 *		destination values are packed.
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_s_s (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata, size_t nelmts,
	      size_t buf_stride, size_t UNUSED bkg_stride, void *buf,
              void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    H5T_t	*src=NULL;		/*source data type		*/
    H5T_t	*dst=NULL;		/*destination data type		*/
    int	direction;		/*direction of traversal	*/
    size_t	elmtno;			/*element number		*/
    size_t	olap;			/*num overlapping elements	*/
    size_t	nchars=0;		/*number of characters copied	*/
    uint8_t	*s, *sp, *d, *dp;	/*src and dst traversal pointers*/
    uint8_t	*dbuf=NULL;		/*temp buf for overlap convers.	*/
    herr_t      ret_value=SUCCEED;       /* Return value */
    
    FUNC_ENTER_NOAPI(H5T_conv_s_s, FAIL);

    switch (cdata->command) {
        case H5T_CONV_INIT:
            if (NULL==(src=H5I_object(src_id)) ||
                    NULL==(dst=H5I_object(dst_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
            if (8*src->size != src->u.atomic.prec || 8*dst->size != dst->u.atomic.prec)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bad precision");
            if (0 != src->u.atomic.offset || 0 != dst->u.atomic.offset)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bad offset");
            if (H5T_CSET_ASCII != src->u.atomic.u.s.cset || H5T_CSET_ASCII != dst->u.atomic.u.s.cset)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bad character set");
            if (src->u.atomic.u.s.pad<0 || src->u.atomic.u.s.pad>=H5T_NPAD ||
                    dst->u.atomic.u.s.pad<0 || dst->u.atomic.u.s.pad>=H5T_NPAD)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bad character padding");
            cdata->need_bkg = H5T_BKG_NO;
            break;

        case H5T_CONV_FREE:
            break;

        case H5T_CONV_CONV:
            /* Get the data types */
            if (NULL==(src=H5I_object(src_id)) ||
                    NULL==(dst=H5I_object(dst_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");

            /*
             * Do we process the values from beginning to end or vice versa? Also,
             * how many of the elements have the source and destination areas
             * overlapping?
             */
            if (src->size==dst->size || buf_stride) {
                /*
                 * When the source and destination are the same size we can do
                 * all the conversions in place.
                 */
                sp = dp = (uint8_t*)buf;
                direction = 1;
                olap = 0;
            } else if (src->size>=dst->size) {
                double olapd = HDceil((double)(dst->size)/
                          (double)(src->size-dst->size));
                olap = (size_t)olapd;
                sp = dp = (uint8_t*)buf;
                direction = 1;
            } else {
                double olapd = HDceil((double)(src->size)/
                          (double)(dst->size-src->size));
                olap = (size_t)olapd;
                sp = (uint8_t*)buf + (nelmts-1) * src->size;
                dp = (uint8_t*)buf + (nelmts-1) * dst->size;
                direction = -1;
            }

            /* Allocate the overlap buffer */
            if (NULL==(dbuf=H5MM_malloc(dst->size)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for string conversion");
            
            /* The conversion loop. */
            for (elmtno=0; elmtno<nelmts; elmtno++) {

                /*
                 * If the source and destination buffers overlap then use a
                 * temporary buffer for the destination.
                 */
                if (direction>0) {
                    s = sp;
                    d = elmtno<olap ? dbuf : dp;
                } else {
                    s = sp;
                    d = elmtno+olap >= nelmts ? dbuf : dp;
                }
#ifndef NDEBUG
                /* I don't quite trust the overlap calculations yet --rpm */
                if (src->size==dst->size || buf_stride) {
                    assert(s==d);
                } else if (d==dbuf) {
                    assert((dp>=sp && dp<sp+src->size) ||
                       (sp>=dp && sp<dp+dst->size));
                } else {
                    assert((dp<sp && dp+dst->size<=sp) ||
                       (sp<dp && sp+src->size<=dp));
                }
#endif
                
                /* Copy characters from source to destination */
                switch (src->u.atomic.u.s.pad) {
                    case H5T_STR_NULLTERM:
                        for (nchars=0;
                             nchars<dst->size && nchars<src->size && s[nchars];
                             nchars++) {
                            d[nchars] = s[nchars];
                        }
                        break;

                    case H5T_STR_NULLPAD:
                        for (nchars=0;
                             nchars<dst->size && nchars<src->size && s[nchars];
                             nchars++) {
                            d[nchars] = s[nchars];
                        }
                        break;

                    case H5T_STR_SPACEPAD:
                        nchars = src->size;
                        while (nchars>0 && ' '==s[nchars-1])
                            --nchars;
                        nchars = MIN(dst->size, nchars);
                        HDmemcpy(d, s, nchars);
                        break;

                    case H5T_STR_RESERVED_3:
                    case H5T_STR_RESERVED_4:
                    case H5T_STR_RESERVED_5:
                    case H5T_STR_RESERVED_6:
                    case H5T_STR_RESERVED_7:
                    case H5T_STR_RESERVED_8:
                    case H5T_STR_RESERVED_9:
                    case H5T_STR_RESERVED_10:
                    case H5T_STR_RESERVED_11:
                    case H5T_STR_RESERVED_12:
                    case H5T_STR_RESERVED_13:
                    case H5T_STR_RESERVED_14:
                    case H5T_STR_RESERVED_15:
                    case H5T_STR_ERROR:
                        HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "source string padding method not supported");
                }

                /* Terminate or pad the destination */
                switch (dst->u.atomic.u.s.pad) {
                    case H5T_STR_NULLTERM:
                        while (nchars<dst->size)
                            d[nchars++] = '\0';
                        d[dst->size-1] = '\0';
                        break;
                    
                    case H5T_STR_NULLPAD:
                        while (nchars<dst->size)
                            d[nchars++] = '\0';
                        break;
                    
                    case H5T_STR_SPACEPAD:
                        while (nchars<dst->size)
                            d[nchars++] = ' ';
                        break;

                    case H5T_STR_RESERVED_3:
                    case H5T_STR_RESERVED_4:
                    case H5T_STR_RESERVED_5:
                    case H5T_STR_RESERVED_6:
                    case H5T_STR_RESERVED_7:
                    case H5T_STR_RESERVED_8:
                    case H5T_STR_RESERVED_9:
                    case H5T_STR_RESERVED_10:
                    case H5T_STR_RESERVED_11:
                    case H5T_STR_RESERVED_12:
                    case H5T_STR_RESERVED_13:
                    case H5T_STR_RESERVED_14:
                    case H5T_STR_RESERVED_15:
                    case H5T_STR_ERROR:
                        HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "destination string padding method not supported");
                }

                /*
                 * If we used a temporary buffer for the destination then we
                 * should copy the value to the true destination buffer.
                 */
                if (d==dbuf)
                    HDmemcpy(dp, d, dst->size);
                if (buf_stride) {
                    sp += direction * buf_stride;
                    dp += direction * buf_stride;
                } else {
                    sp += direction * src->size;
                    dp += direction * dst->size;
                }
            }
            break;

        default:
            HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown converson command");
    }

done:
    H5MM_xfree(dbuf);
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_schar_uchar
 *
 * Purpose:	Converts `signed char' to `unsigned char'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Monday, November 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_schar_uchar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, size_t buf_stride,
                     size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                     hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_schar_uchar, FAIL);

    H5T_CONV_su(SCHAR, UCHAR, signed char, unsigned char, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uchar_schar
 *
 * Purpose:	Converts `unsigned char' to `signed char'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Monday, November 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uchar_schar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, size_t buf_stride,
                     size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                     hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_uchar_schar, FAIL);

    H5T_CONV_us(UCHAR, SCHAR, unsigned char, signed char, -, SCHAR_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_schar_short
 *
 * Purpose:	Converts `signed char' to `short'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_schar_short(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, size_t buf_stride,
                     size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                     hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_schar_short, FAIL);

    H5T_CONV_sS(SCHAR, SHORT, signed char, short, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_schar_ushort
 *
 * Purpose:	Converts `signed char' to `unsigned short'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_schar_ushort(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, size_t buf_stride,
                      size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                      hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_schar_ushort, FAIL);

    H5T_CONV_sU(SCHAR, USHORT, signed char, unsigned short, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uchar_short
 *
 * Purpose:	Converts `unsigned char' to `short'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uchar_short(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, size_t buf_stride,
                     size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                     hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_uchar_short, FAIL);

    H5T_CONV_uS(UCHAR, SHORT, unsigned char, short, -, SHRT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uchar_ushort
 *
 * Purpose:	Converts `unsigned char' to `unsigned short'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uchar_ushort(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, size_t buf_stride,
                      size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                      hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_uchar_ushort, FAIL);

    H5T_CONV_uU(UCHAR, USHORT, unsigned char, unsigned short, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_schar_int
 *
 * Purpose:	Converts `signed char' to `int'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_schar_int(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		   size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                   void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_schar_int, FAIL);

    H5T_CONV_sS(SCHAR, INT, signed char, int, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_schar_uint
 *
 * Purpose:	Converts `signed char' to `unsigned int'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_schar_uint(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                    void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_schar_uint, FAIL);

    H5T_CONV_sU(SCHAR, UINT, signed char, unsigned, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uchar_int
 *
 * Purpose:	Converts `unsigned char' to `int'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uchar_int(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		   size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                   void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_uchar_int, FAIL);

    H5T_CONV_uS(UCHAR, INT, unsigned char, int, -, INT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uchar_uint
 *
 * Purpose:	Converts `unsigned char' to `unsigned int'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uchar_uint(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                    void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_uchar_uint, FAIL);

    H5T_CONV_uU(UCHAR, UINT, unsigned char, unsigned, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_schar_long
 *
 * Purpose:	Converts `signed char' to `long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_schar_long(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                    void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_schar_long, FAIL);

    H5T_CONV_sS(SCHAR, LONG, signed char, long, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_schar_ulong
 *
 * Purpose:	Converts `signed char' to `unsigned long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_schar_ulong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, size_t buf_stride,
                     size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		     hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_schar_ulong, FAIL);

    H5T_CONV_sU(SCHAR, ULONG, signed char, unsigned long, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uchar_long
 *
 * Purpose:	Converts `unsigned char' to `long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uchar_long(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                    void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_uchar_long, FAIL);

    H5T_CONV_uS(UCHAR, LONG, unsigned char, long, -, LONG_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uchar_ulong
 *
 * Purpose:	Converts `unsigned char' to `unsigned long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uchar_ulong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, size_t buf_stride,
                     size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		     hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_uchar_ulong, FAIL);

    H5T_CONV_uU(UCHAR, ULONG, unsigned char, unsigned long, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_schar_llong
 *
 * Purpose:	Converts `signed char' to `long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_schar_llong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, size_t buf_stride,
                     size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		     hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_schar_llong, FAIL);

    H5T_CONV_sS(SCHAR, LLONG, signed char, long_long, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_schar_ullong
 *
 * Purpose:	Converts `signed char' to `unsigned long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_schar_ullong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, size_t buf_stride,
                      size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                      hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_schar_ullong, FAIL);

    H5T_CONV_sU(SCHAR, ULLONG, signed char, unsigned long_long, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uchar_llong
 *
 * Purpose:	Converts `unsigned char' to `long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uchar_llong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, size_t buf_stride,
                     size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		     hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_uchar_llong, FAIL);

    H5T_CONV_uS(UCHAR, LLONG, unsigned char, long_long, -, LLONG_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uchar_ullong
 *
 * Purpose:	Converts `unsigned char' to `unsigned long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uchar_ullong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, size_t buf_stride,
                      size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                      hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_uchar_ullong, FAIL);

    H5T_CONV_uU(UCHAR, ULLONG, unsigned char, unsigned long_long, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_short_schar
 *
 * Purpose:	Converts `short' to `signed char'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_short_schar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, size_t buf_stride,
                     size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		     hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_short_schar, FAIL);

    H5T_CONV_Ss(SHORT, SCHAR, short, signed char, SCHAR_MIN, SCHAR_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_short_uchar
 *
 * Purpose:	Converts `short' to `unsigned char'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_short_uchar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, size_t buf_stride,
                     size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		     hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_short_uchar, FAIL);

    H5T_CONV_Su(SHORT, UCHAR, short, unsigned char, -, UCHAR_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ushort_schar
 *
 * Purpose:	Converts `unsigned short' to `signed char'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ushort_schar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, size_t buf_stride,
                      size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                      hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_ushort_schar, FAIL);
    
    H5T_CONV_Us(USHORT, SCHAR, unsigned short, signed char, -, SCHAR_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ushort_uchar
 *
 * Purpose:	Converts `unsigned short' to `unsigned char'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ushort_uchar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, size_t buf_stride,
                      size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                      hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_ushort_uchar, FAIL);

    H5T_CONV_Uu(USHORT, UCHAR, unsigned short, unsigned char, -, UCHAR_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_short_ushort
 *
 * Purpose:	Converts `short' to `unsigned short'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Monday, November 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_short_ushort(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, size_t buf_stride,
                      size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                      hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_short_ushort, FAIL);
    
    H5T_CONV_su(SHORT, USHORT, short, unsigned short, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ushort_short
 *
 * Purpose:	Converts `unsigned short' to `short'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Monday, November 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ushort_short(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, size_t buf_stride,
                      size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                      hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_ushort_short, FAIL);

    H5T_CONV_us(USHORT, SHORT, unsigned short, short, -, SHRT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_short_int
 *
 * Purpose:	Converts `short' to `int'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_short_int(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		   size_t nelmts, size_t buf_stride,
                   size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		   hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_short_int, FAIL);

    H5T_CONV_sS(SHORT, INT, short, int, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_short_uint
 *
 * Purpose:	Converts `short' to `unsigned int'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_short_uint(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, size_t buf_stride,
                    size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		    hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_short_uint, FAIL);

    H5T_CONV_sU(SHORT, UINT, short, unsigned, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ushort_int
 *
 * Purpose:	Converts `unsigned short' to `int'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ushort_int(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, size_t buf_stride,
                    size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		    hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_ushort_int, FAIL);

    H5T_CONV_uS(USHORT, INT, unsigned short, int, -, INT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ushort_uint
 *
 * Purpose:	Converts `unsigned short' to `unsigned int'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ushort_uint(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, size_t buf_stride,
                     size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		     hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_ushort_uint, FAIL);

    H5T_CONV_uU(USHORT, UINT, unsigned short, unsigned, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_short_long
 *
 * Purpose:	Converts `short' to `long'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_short_long(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, size_t buf_stride,
                    size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		    hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_short_long, FAIL);

    H5T_CONV_sS(SHORT, LONG, short, long, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_short_ulong
 *
 * Purpose:	Converts `short' to `unsigned long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_short_ulong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, size_t buf_stride,
                     size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		     hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_short_ulong, FAIL);

    H5T_CONV_sU(SHORT, ULONG, short, unsigned long, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ushort_long
 *
 * Purpose:	Converts `unsigned short' to `long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ushort_long(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, size_t buf_stride,
                     size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		     hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_ushort_long, FAIL);

    H5T_CONV_uS(USHORT, LONG, unsigned short, long, -, LONG_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ushort_ulong
 *
 * Purpose:	Converts `unsigned short' to `unsigned long'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ushort_ulong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, size_t buf_stride,
                      size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                      hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_ushort_ulong, FAIL);

    H5T_CONV_uU(USHORT, ULONG, unsigned short, unsigned long, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_short_llong
 *
 * Purpose:	Converts `short' to `long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_short_llong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, size_t buf_stride,
                     size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		     hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_short_llong, FAIL);

    H5T_CONV_sS(SHORT, LLONG, short, long_long, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_short_ullong
 *
 * Purpose:	Converts `short' to `unsigned long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_short_ullong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, size_t buf_stride,
                      size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                      hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_short_ullong, FAIL);

    H5T_CONV_sU(SHORT, ULLONG, short, unsigned long_long, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ushort_llong
 *
 * Purpose:	Converts `unsigned short' to `long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ushort_llong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, size_t buf_stride,
                      size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                      hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_ushort_llong, FAIL);

    H5T_CONV_uS(USHORT, LLONG, unsigned short, long_long, -, LLONG_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ushort_ullong
 *
 * Purpose:	Converts `unsigned short' to `unsigned long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ushort_ullong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_ushort_ullong, FAIL);

    H5T_CONV_uU(USHORT, ULLONG, unsigned short, unsigned long_long, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_int_schar
 *
 * Purpose:	Converts `int' to `signed char'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_int_schar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		   size_t nelmts, size_t buf_stride,
                   size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		   hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_int_schar, FAIL);

    H5T_CONV_Ss(INT, SCHAR, int, signed char, SCHAR_MIN, SCHAR_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_int_uchar
 *
 * Purpose:	Converts `int' to `unsigned char'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_int_uchar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		   size_t nelmts, size_t buf_stride,
                   size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		   hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_int_uchar, FAIL);

    H5T_CONV_Su(INT, UCHAR, int, unsigned char, -, UCHAR_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uint_schar
 *
 * Purpose:	Converts `unsigned int' to `signed char'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uint_schar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, size_t buf_stride,
                    size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		    hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_uint_schar, FAIL);

    H5T_CONV_Us(UINT, SCHAR, unsigned, signed char, -, SCHAR_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uint_uchar
 *
 * Purpose:	Converts `unsigned int' to `unsigned char'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uint_uchar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, size_t buf_stride,
                    size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		    hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_uint_uchar, FAIL);
    
    H5T_CONV_Uu(UINT, UCHAR, unsigned, unsigned char, -, UCHAR_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_int_short
 *
 * Purpose:	Converts `int' to `short'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_int_short(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		   size_t nelmts, size_t buf_stride,
                   size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		   hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_int_short, FAIL);

    H5T_CONV_Ss(INT, SHORT, int, short, SHRT_MIN, SHRT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_int_ushort
 *
 * Purpose:	Converts `int' to `unsigned short'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_int_ushort(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, size_t buf_stride,
                    size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		    hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_int_ushort, FAIL);

    H5T_CONV_Su(INT, USHORT, int, unsigned short, -, USHRT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uint_short
 *
 * Purpose:	Converts `unsigned int' to `short'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uint_short(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, size_t buf_stride,
                    size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		    hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_uint_short, FAIL);

    H5T_CONV_Us(UINT, SHORT, unsigned, short, -, SHRT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uint_ushort
 *
 * Purpose:	Converts `unsigned int' to `unsigned short'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uint_ushort(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, size_t buf_stride,
                     size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		     hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_uint_ushort, FAIL);

    H5T_CONV_Uu(UINT, USHORT, unsigned, unsigned short, -, USHRT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_int_uint
 *
 * Purpose:	Converts `int' to `unsigned int'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Monday, November 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_int_uint(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		  size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                  void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_int_uint, FAIL);

    H5T_CONV_su(INT, UINT, int, unsigned, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uint_int
 *
 * Purpose:	Converts `unsigned int' to `int'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Monday, November 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uint_int(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		  size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                  void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_uint_int, FAIL);

    H5T_CONV_us(UINT, INT, unsigned, int, -, INT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_int_long
 *
 * Purpose:	Converts `int' to `long'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_int_long(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		  size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                  void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_int_long, FAIL);

    H5T_CONV_sS(INT, LONG, int, long, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_int_ulong
 *
 * Purpose:	Converts `int' to `unsigned long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_int_ulong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		   size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                   void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_int_ulong, FAIL);

    H5T_CONV_sU(INT, LONG, int, unsigned long, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uint_long
 *
 * Purpose:	Converts `unsigned int' to `long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uint_long(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		   size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                   void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_uint_long, FAIL);

    H5T_CONV_uS(UINT, LONG, unsigned, long, -, LONG_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uint_ulong
 *
 * Purpose:	Converts `unsigned int' to `unsigned long'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uint_ulong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                    void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_uint_ulong, FAIL);

    H5T_CONV_uU(UINT, ULONG, unsigned, unsigned long, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_int_llong
 *
 * Purpose:	Converts `int' to `long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_int_llong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		   size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                   void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_int_llong, FAIL);

    H5T_CONV_sS(INT, LLONG, int, long_long, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_int_ullong
 *
 * Purpose:	Converts `int' to `unsigned long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_int_ullong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                    void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_int_ullong, FAIL);

    H5T_CONV_sU(INT, ULLONG, int, unsigned long_long, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uint_llong
 *
 * Purpose:	Converts `unsigned int' to `long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uint_llong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                    void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_uint_llong, FAIL);

    H5T_CONV_uS(UINT, LLONG, unsigned, long_long, -, LLONG_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uint_ullong
 *
 * Purpose:	Converts `unsigned int' to `unsigned long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uint_ullong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, size_t buf_stride,
                     size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		     hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_uint_ullong, FAIL);

    H5T_CONV_uU(UINT, ULLONG, unsigned, unsigned long_long, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_long_schar
 *
 * Purpose:	Converts `long' to `signed char'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_long_schar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                    void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_long_schar, FAIL);

    H5T_CONV_Ss(LONG, SCHAR, long, signed char, SCHAR_MIN, SCHAR_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_long_uchar
 *
 * Purpose:	Converts `long' to `unsigned char'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_long_uchar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                    void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_long_uchar, FAIL);

    H5T_CONV_Su(LONG, UCHAR, long, unsigned char, -, UCHAR_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ulong_schar
 *
 * Purpose:	Converts `unsigned long' to `signed char'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ulong_schar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, size_t buf_stride,
                     size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		     hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_ulong_schar, FAIL);

    H5T_CONV_Us(ULONG, SCHAR, unsigned long, signed char, -, SCHAR_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ulong_uchar
 *
 * Purpose:	Converts `unsigned long' to `unsigned char'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ulong_uchar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, size_t buf_stride,
                     size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		     hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_ulong_uchar, FAIL);

    H5T_CONV_Uu(ULONG, UCHAR, unsigned long, unsigned char, -, UCHAR_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_long_short
 *
 * Purpose:	Converts `long' to `short'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_long_short(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                    void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_long_short, FAIL);

    H5T_CONV_Ss(LONG, SHORT, long, short, SHRT_MIN, SHRT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_long_ushort
 *
 * Purpose:	Converts `long' to `unsigned short'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_long_ushort(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, size_t buf_stride,
                     size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		     hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_long_ushort, FAIL);

    H5T_CONV_Su(LONG, USHORT, long, unsigned short, -, USHRT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ulong_short
 *
 * Purpose:	Converts `unsigned long' to `short'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ulong_short(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                     void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_ulong_short, FAIL);

    H5T_CONV_Us(ULONG, SHORT, unsigned long, short, -, SHRT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ulong_ushort
 *
 * Purpose:	Converts `unsigned long' to `unsigned short'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ulong_ushort(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, size_t buf_stride,
                      size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                      hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_ulong_ushort, FAIL);

    H5T_CONV_Uu(ULONG, USHORT, unsigned long, unsigned short, -, USHRT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_long_int
 *
 * Purpose:	Converts `long' to `int'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_long_int(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		  size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                  void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_long_int, FAIL);

    H5T_CONV_Ss(LONG, INT, long, int, INT_MIN, INT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_long_uint
 *
 * Purpose:	Converts `long' to `unsigned int'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_long_uint(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		   size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                   void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_long_uint, FAIL);

    H5T_CONV_Su(LONG, UINT, long, unsigned, -, UINT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ulong_int
 *
 * Purpose:	Converts `unsigned long' to `int'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ulong_int(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		   size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                   void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_ulong_int, FAIL);

    H5T_CONV_Us(ULONG, INT, unsigned long, int, -, INT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ulong_uint
 *
 * Purpose:	Converts `unsigned long' to `unsigned int'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ulong_uint(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                    void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_ulong_uint, FAIL);

    H5T_CONV_Uu(ULONG, UINT, unsigned long, unsigned, -, UINT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_long_ulong
 *
 * Purpose:	Converts `long' to `unsigned long'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Monday, November 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_long_ulong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                    void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_long_ulong, FAIL);

    H5T_CONV_su(LONG, ULONG, long, unsigned long, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ulong_long
 *
 * Purpose:	Converts `unsigned long' to `long'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Monday, November 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ulong_long(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                    void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_ulong_long, FAIL);

    H5T_CONV_us(ULONG, LONG, unsigned long, long, -, LONG_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_long_llong
 *
 * Purpose:	Converts `long' to `long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_long_llong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                    void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_long_llong, FAIL);

    H5T_CONV_sS(LONG, LLONG, long, long_long, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_long_ullong
 *
 * Purpose:	Converts `long' to `unsigned long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_long_ullong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, size_t buf_stride,
                     size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		     hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_long_ullong, FAIL);

    H5T_CONV_sU(LONG, ULLONG, long, unsigned long_long, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ulong_llong
 *
 * Purpose:	Converts `unsigned long' to `long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ulong_llong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, size_t buf_stride,
                     size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		     hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_ulong_llong, FAIL);

    H5T_CONV_uS(ULONG, LLONG, unsigned long, long_long, -, LLONG_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ulong_ullong
 *
 * Purpose:	Converts `unsigned long' to `unsigned long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ulong_ullong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, size_t buf_stride,
                      size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                      hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_ulong_ullong, FAIL);

    H5T_CONV_uU(ULONG, ULLONG, unsigned long, unsigned long_long, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_llong_schar
 *
 * Purpose:	Converts `long_long' to `signed char'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_llong_schar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, size_t buf_stride,
                     size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		     hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_llong_schar, FAIL);

    H5T_CONV_Ss(LLONG, SCHAR, long_long, signed char, SCHAR_MIN, SCHAR_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_llong_uchar
 *
 * Purpose:	Converts `long_long' to `unsigned char'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_llong_uchar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, size_t buf_stride,
                     size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		     hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_llong_uchar, FAIL);

    H5T_CONV_Su(LLONG, UCHAR, long_long, unsigned char, -, UCHAR_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ullong_schar
 *
 * Purpose:	Converts `unsigned long_long' to `signed char'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ullong_schar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, size_t buf_stride,
                      size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                      hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_ullong_schar, FAIL);

    H5T_CONV_Us(ULLONG, SCHAR, unsigned long_long, signed char, -, SCHAR_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ullong_uchar
 *
 * Purpose:	Converts `unsigned long_long' to `unsigned char'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ullong_uchar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
                      size_t nelmts, size_t buf_stride,
                      size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                      hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_ullong_uchar, FAIL);

    H5T_CONV_Uu(ULLONG, UCHAR, unsigned long_long, unsigned char, -, UCHAR_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_llong_short
 *
 * Purpose:	Converts `long_long' to `short'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_llong_short(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, size_t buf_stride,
                     size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		     hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_llong_short, FAIL);

    H5T_CONV_Ss(LLONG, SHORT, long_long, short, SHRT_MIN, SHRT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_llong_ushort
 *
 * Purpose:	Converts `long_long' to `unsigned short'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_llong_ushort(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, size_t buf_stride,
                      size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                      hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_llong_ushort, FAIL);

    H5T_CONV_Su(LLONG, USHORT, long_long, unsigned short, -, USHRT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ullong_short
 *
 * Purpose:	Converts `unsigned long_long' to `short'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ullong_short(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, size_t buf_stride,
                      size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                      hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_ullong_short, FAIL);

    H5T_CONV_Us(ULLONG, SHORT, unsigned long_long, short, -, SHRT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ullong_ushort
 *
 * Purpose:	Converts `unsigned long_long' to `unsigned short'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ullong_ushort(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_ullong_ushort, FAIL);

    H5T_CONV_Uu(ULLONG, USHORT, unsigned long_long, unsigned short, -, USHRT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_llong_int
 *
 * Purpose:	Converts `long_long' to `int'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_llong_int(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		   size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                   void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_llong_int, FAIL);

    H5T_CONV_Ss(LLONG, INT, long_long, int, INT_MIN, INT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_llong_uint
 *
 * Purpose:	Converts `long_long' to `unsigned int'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_llong_uint(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                    void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_llong_uint, FAIL);

    H5T_CONV_Su(LLONG, UINT, long_long, unsigned, -, UINT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ullong_int
 *
 * Purpose:	Converts `unsigned long_long' to `int'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ullong_int(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                    void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_ullong_int, FAIL);

    H5T_CONV_Us(ULLONG, INT, unsigned long_long, int, -, INT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ullong_uint
 *
 * Purpose:	Converts `unsigned long_long' to `unsigned int'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ullong_uint(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, size_t buf_stride,
                     size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		     hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_ullong_uint, FAIL);

    H5T_CONV_Uu(ULLONG, UINT, unsigned long_long, unsigned, -, UINT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_llong_long
 *
 * Purpose:	Converts `long_long' to `long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_llong_long(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, size_t buf_stride, size_t UNUSED bkg_stride,
                    void *buf, void UNUSED *bkg, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_llong_long, FAIL);

    H5T_CONV_Ss(LLONG, LONG, long_long, long, LONG_MIN, LONG_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_llong_ulong
 *
 * Purpose:	Converts `long_long' to `unsigned long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_llong_ulong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, size_t buf_stride,
                     size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		     hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_llong_ulong, FAIL);

    H5T_CONV_Su(LLONG, ULONG, long_long, unsigned long, -, ULONG_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ullong_long
 *
 * Purpose:	Converts `unsigned long_long' to `long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ullong_long(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, size_t buf_stride,
                     size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
		     hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_ullong_long, FAIL);

    H5T_CONV_Us(ULLONG, LONG, unsigned long_long, long, -, LONG_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ullong_ulong
 *
 * Purpose:	Converts `unsigned long_long' to `unsigned long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ullong_ulong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, size_t buf_stride,
                      size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                      hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_ullong_ulong, FAIL);

    H5T_CONV_Uu(ULLONG, ULONG, unsigned long_long, unsigned long, -, ULONG_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_llong_ullong
 *
 * Purpose:	Converts `long_long' to `unsigned long_long'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Monday, November 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_llong_ullong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, size_t buf_stride,
                      size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                      hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_llong_ullong, FAIL);

    H5T_CONV_su(LLONG, ULLONG, long_long, unsigned long_long, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ullong_llong
 *
 * Purpose:	Converts `unsigned long_long' to `long_long'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Monday, November 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ullong_llong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, size_t buf_stride,
                      size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                      hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_ullong_llong, FAIL);

    H5T_CONV_us(ULLONG, LLONG, unsigned long_long, long_long, -, LLONG_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_float_double
 *
 * Purpose:	Convert native `float' to native `double' using hardware.
 *		This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Tuesday, June 23, 1998
 *
 * Modifications:
 *		Robb Matzke, 1999-06-16
 *		Added support for non-zero strides. If BUF_STRIDE is non-zero
 *		then convert one value at each memory location advancing
 *		BUF_STRIDE bytes each time; otherwise assume both source and
 *		destination values are packed.
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_float_double (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_float_double, FAIL);

    H5T_CONV_fF(FLOAT, DOUBLE, float, double, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_double_float
 *
 * Purpose:	Convert native `double' to native `float' using hardware.
 *		This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Tuesday, June 23, 1998
 *
 * Modifications:
 *		Robb Matzke, 7 Jul 1998
 *		Added overflow handling.
 *
 *		Robb Matzke, 1999-06-16
 *		Added support for non-zero strides. If BUF_STRIDE is non-zero
 *		then convert one value at each memory location advancing
 *		BUF_STRIDE bytes each time; otherwise assume both source and
 *		destination values are packed.
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_double_float (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_double_float, FAIL);

    H5T_CONV_Ff(DOUBLE, FLOAT, double, float, -FLT_MAX, FLT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_char_float
 *
 * Purpose:	Convert native char to native float using hardware.
 *		This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_char_float (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_char_float, FAIL);

    H5T_CONV_xF(SCHAR, FLOAT, char, float, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_char_double
 *
 * Purpose:	Convert native char to native double using hardware.
 *		This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_char_double (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_char_double, FAIL);

    H5T_CONV_xF(SCHAR, DOUBLE, char, double, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uchar_float
 *
 * Purpose:	Convert native unsigned char to native float using hardware.
 *		This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uchar_float (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_uchar_float, FAIL);

    H5T_CONV_xF(UCHAR, FLOAT, unsigned char, float, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uchar_double
 *
 * Purpose:	Convert native unsigned char to native double using hardware.
 *		This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uchar_double (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_uchar_double, FAIL);

    H5T_CONV_xF(UCHAR, DOUBLE, unsigned char, double, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_short_float
 *
 * Purpose:	Convert native short to native float using hardware.
 *		This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_short_float (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_short_float, FAIL);

    H5T_CONV_xF(SHORT, FLOAT, short, float, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_short_double
 *
 * Purpose:	Convert native short to native double using hardware.
 *		This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_short_double (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_short_double, FAIL);

    H5T_CONV_xF(SHORT, DOUBLE, short, double, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ushort_float
 *
 * Purpose:	Convert native unsigned short to native float using hardware.
 *		This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ushort_float (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_ushort_float, FAIL);

    H5T_CONV_xF(USHORT, FLOAT, unsigned short, float, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ushort_double
 *
 * Purpose:	Convert native unsigned short to native double using hardware.
 *		This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ushort_double (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_ushort_double, FAIL);

    H5T_CONV_xF(USHORT, DOUBLE, unsigned short, double, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_int_float
 *
 * Purpose:	Convert native integer to native float using hardware.
 *		This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_int_float (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_int_float, FAIL);

    H5T_CONV_xF(INT, FLOAT, int, float, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_int_double
 *
 * Purpose:	Convert native integer to native double using hardware.
 *		This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_int_double (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_int_double, FAIL);

    H5T_CONV_xF(INT, DOUBLE, int, double, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uint_float
 *
 * Purpose:	Convert native unsigned integer to native float using 
 *              hardware.  This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uint_float (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_uint_float, FAIL);

    H5T_CONV_xF(UINT, FLOAT, unsigned int, float, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uint_double
 *
 * Purpose:	Convert native unsigned integer to native double using 
 *              hardware.  This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uint_double (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_uint_double, FAIL);

    H5T_CONV_xF(UINT, DOUBLE, unsigned int, double, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_long_float
 *
 * Purpose:	Convert native long to native float using hardware.
 *		This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_long_float (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_long_float, FAIL);

    H5T_CONV_xF(LONG, FLOAT, long, float, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_long_double
 *
 * Purpose:	Convert native long to native double using hardware.
 *		This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_long_double (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_long_double, FAIL);

    H5T_CONV_xF(LONG, DOUBLE, long, double, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ulong_float
 *
 * Purpose:	Convert native native long to native float using hardware.
 *		This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ulong_float (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_ulong_float, FAIL);

    H5T_CONV_xF(ULONG, FLOAT, unsigned long, float, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ulong_double
 *
 * Purpose:	Convert native native long to native double using hardware.
 *		This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ulong_double (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_ulong_double, FAIL);

    H5T_CONV_xF(ULONG, DOUBLE, unsigned long, double, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}



/*-------------------------------------------------------------------------
 * Function:	H5T_conv_llong_float
 *
 * Purpose:	Convert native long long to native float using hardware.
 *		This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_llong_float (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_llong_float, FAIL);

    H5T_CONV_xF(LLONG, FLOAT, long_long, float, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_llong_double
 *
 * Purpose:	Convert native long long to native double using hardware.
 *		This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_llong_double (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_llong_double, FAIL);

    H5T_CONV_xF(LLONG, DOUBLE, long_long, double, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}

#ifdef H5_ULLONG_TO_FP_CAST_WORKS

/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ullong_float
 *
 * Purpose:	Convert native unsigned long long to native float using 
 *              hardware.  This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ullong_float (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_ullong_float, FAIL);

    H5T_CONV_xF(ULLONG, FLOAT, unsigned long_long, float, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ullong_double
 *
 * Purpose:	Convert native unsigned long long to native double using 
 *              hardware.  This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ullong_double (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_ullong_double, FAIL);

    H5T_CONV_xF(ULLONG, DOUBLE, unsigned long_long, double, -, -);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}
#endif /* H5_ULLONG_TO_FP_CAST_WORKS */


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_float_char
 *
 * Purpose:	Convert native float to native char using 
 *              hardware.  This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_float_char (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_float_char, FAIL);

    H5T_CONV_Fx(FLOAT, SCHAR, float, char, CHAR_MIN, CHAR_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_float_uchar
 *
 * Purpose:	Convert native float to native unsigned char using 
 *              hardware.  This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_float_uchar (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_float_uchar, FAIL);

    H5T_CONV_Fx(FLOAT, UCHAR, float, unsigned char, 0, UCHAR_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_double_char
 *
 * Purpose:	Convert native float to native char using 
 *              hardware.  This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_double_char (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_double_char, FAIL);

    H5T_CONV_Fx(DOUBLE, SCHAR, double, char, CHAR_MIN, CHAR_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_double_uchar
 *
 * Purpose:	Convert native float to native unsigned char using 
 *              hardware.  This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_double_uchar (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_double_uchar, FAIL);

    H5T_CONV_Fx(DOUBLE, UCHAR, double, unsigned char, 0, UCHAR_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_float_short
 *
 * Purpose:	Convert native float to native short using 
 *              hardware.  This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_float_short (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_float_short, FAIL);

    H5T_CONV_Fx(FLOAT, SHORT, float, short, SHRT_MIN, SHRT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_float_ushort
 *
 * Purpose:	Convert native float to native unsigned short using 
 *              hardware.  This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_float_ushort (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_float_ushort, FAIL);

    H5T_CONV_Fx(FLOAT, USHORT, float, unsigned short, 0, USHRT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_double_short
 *
 * Purpose:	Convert native float to native short using 
 *              hardware.  This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_double_short (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_double_short, FAIL);

    H5T_CONV_Fx(DOUBLE, SHORT, double, short, SHRT_MIN, SHRT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_double_ushort
 *
 * Purpose:	Convert native float to native unsigned short using 
 *              hardware.  This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_double_ushort (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_double_ushort, FAIL);

    H5T_CONV_Fx(DOUBLE, USHORT, double, unsigned short, 0, USHRT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_float_int
 *
 * Purpose:	Convert native float to native int using 
 *              hardware.  This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_float_int (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_float_int, FAIL);

    H5T_CONV_Fx(FLOAT, INT, float, int, INT_MIN, INT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_float_uint
 *
 * Purpose:	Convert native float to native unsigned int using 
 *              hardware.  This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_float_uint (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_float_uint, FAIL);

    H5T_CONV_Fx(FLOAT, UINT, float, unsigned int, 0, UINT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_double_int
 *
 * Purpose:	Convert native float to native int using 
 *              hardware.  This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_double_int (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_double_int, FAIL);

    H5T_CONV_Fx(DOUBLE, INT, double, int, INT_MIN, INT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_double_uint
 *
 * Purpose:	Convert native float to native unsigned int using 
 *              hardware.  This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_double_uint (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_double_uint, FAIL);

    H5T_CONV_Fx(DOUBLE, UINT, double, unsigned int, 0, UINT_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_float_long
 *
 * Purpose:	Convert native float to native long using 
 *              hardware.  This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_float_long (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_float_long, FAIL);

    H5T_CONV_Fx(FLOAT, LONG, float, long, LONG_MIN, LONG_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_float_ulong
 *
 * Purpose:	Convert native float to native unsigned long using 
 *              hardware.  This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_float_ulong (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_float_ulong, FAIL);

    H5T_CONV_Fx(FLOAT, ULONG, float, unsigned long, 0, ULONG_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_double_long
 *
 * Purpose:	Convert native float to native long using 
 *              hardware.  This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_double_long (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_double_long, FAIL);

    H5T_CONV_Fx(DOUBLE, LONG, double, long, LONG_MIN, LONG_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_double_ulong
 *
 * Purpose:	Convert native float to native unsigned long using 
 *              hardware.  This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_double_ulong (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_double_ulong, FAIL);

    H5T_CONV_Fx(DOUBLE, ULONG, double, unsigned long, 0, ULONG_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_float_llong
 *
 * Purpose:	Convert native float to native long long using 
 *              hardware.  This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_float_llong (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_float_llong, FAIL);

    H5T_CONV_Fx(FLOAT, LLONG, float, long_long, LLONG_MIN, LLONG_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_float_ullong
 *
 * Purpose:	Convert native float to native unsigned long long using 
 *              hardware.  This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_float_ullong (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_float_ullong, FAIL);

    H5T_CONV_Fx(FLOAT, ULLONG, float, unsigned long_long, 0, ULLONG_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_double_llong
 *
 * Purpose:	Convert native float to native long long using 
 *              hardware.  This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_double_llong (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t UNUSED dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_double_llong, FAIL);

    H5T_CONV_Fx(DOUBLE, LLONG, double, long_long, LLONG_MIN, LLONG_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_double_ullong
 *
 * Purpose:	Convert native float to native unsigned long long using 
 *              hardware.  This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, November 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_double_ullong (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, size_t buf_stride,
                       size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                       hid_t dxpl_id)
{
    herr_t      ret_value=SUCCEED;      /* Return value         */
    
    FUNC_ENTER_NOAPI(H5T_conv_double_ullong, FAIL);

    H5T_CONV_Fx(DOUBLE, ULLONG, double, unsigned long_long, 0, ULLONG_MAX);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_i32le_f64le
 *
 * Purpose:	Converts 4-byte little-endian integers (signed or unsigned)
 *		to 8-byte litte-endian IEEE floating point.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *
 * Programmer:	Robb Matzke
 *		Wednesday, June 10, 1998
 *
 * Modifications:
 *		Robb Matzke, 1999-06-16
 *		Added support for non-zero strides. If BUF_STRIDE is non-zero
 *		then convert one value at each memory location advancing
 *		BUF_STRIDE bytes each time; otherwise assume both source and
 *		destination values are packed.
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_i32le_f64le (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, size_t buf_stride,
                      size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
                      hid_t UNUSED dxpl_id)
{
    uint8_t	*s=NULL, *d=NULL;	/*src and dst buf pointers	*/
    uint8_t	tmp[8];			/*temporary destination buffer	*/
    H5T_t	*src = NULL;		/*source data type		*/
    size_t	elmtno;			/*element counter		*/
    unsigned	sign;			/*sign bit			*/
    unsigned	cin, cout;		/*carry in/out			*/
    unsigned	mbits=0;		/*mantissa bits			*/
    unsigned	exponent;		/*exponent			*/
    int	i;			/*counter			*/
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5T_conv_i32le_f64le, FAIL);

    switch (cdata->command) {
        case H5T_CONV_INIT:
            assert (sizeof(int)>=4);
            cdata->need_bkg = H5T_BKG_NO;
            break;

        case H5T_CONV_FREE:
            /* Free private data */
            break;

        case H5T_CONV_CONV:
            /* The conversion */
            if (NULL==(src=H5I_object(src_id)) ||
                    NULL==H5I_object(dst_id))
                HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
            
            s = (uint8_t*)buf + (buf_stride?buf_stride:4)*(nelmts-1);
            d = (uint8_t*)buf + (buf_stride?buf_stride:8)*(nelmts-1);
            for (elmtno=0; elmtno<nelmts; elmtno++) {

                /*
                 * If this is the last element to convert (that is, the first
                 * element of the buffer) then the source and destination areas
                 * overlap so we need to use a temp buf for the destination.
                 */
                if ((void*)s==buf)
                    d = tmp;

                /* Convert the integer to a sign and magnitude */
                switch (src->u.atomic.u.i.sign) {
                    case H5T_SGN_NONE:
                        sign = 0;
                        break;

                    case H5T_SGN_2:
                        if (s[3] & 0x80) {
                            sign = 1;
                            for (i=0,cin=1; i<4; i++,cin=cout) {
                                s[i] = ~s[i];
                                cout = ((unsigned)(s[i])+cin > 0xff) ? 1 : 0;
                                s[i] += cin;
                            }
                        } else {
                            sign = 0;
                        }
                        break;

                    default:
                        HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported integer sign method");
                }
                
                /*
                 * Where is the most significant bit that is set?  We could do
                 * this in a loop, but testing it this way might be faster.
                 */
                if (s[3]) {
                    if (s[3] & 0x80) mbits = 32;
                    else if (s[3] & 0x40) mbits = 31;
                    else if (s[3] & 0x20) mbits = 30;
                    else if (s[3] & 0x10) mbits = 29;
                    else if (s[3] & 0x08) mbits = 28;
                    else if (s[3] & 0x04) mbits = 27;
                    else if (s[3] & 0x02) mbits = 26;
                    else if (s[3] & 0x01) mbits = 25;
                } else if (s[2]) {
                    if (s[2] & 0x80) mbits = 24;
                    else if (s[2] & 0x40) mbits = 23;
                    else if (s[2] & 0x20) mbits = 22;
                    else if (s[2] & 0x10) mbits = 21;
                    else if (s[2] & 0x08) mbits = 20;
                    else if (s[2] & 0x04) mbits = 19;
                    else if (s[2] & 0x02) mbits = 18;
                    else if (s[2] & 0x01) mbits = 17;
                } else if (s[1]) {
                    if (s[1] & 0x80) mbits = 16;
                    else if (s[1] & 0x40) mbits = 15;
                    else if (s[1] & 0x20) mbits = 14;
                    else if (s[1] & 0x10) mbits = 13;
                    else if (s[1] & 0x08) mbits = 12;
                    else if (s[1] & 0x04) mbits = 11;
                    else if (s[1] & 0x02) mbits = 10;
                    else if (s[1] & 0x01) mbits =  9;
                } else if (s[0]) {
                    if (s[0] & 0x80) mbits = 8;
                    else if (s[0] & 0x40) mbits =  7;
                    else if (s[0] & 0x20) mbits =  6;
                    else if (s[0] & 0x10) mbits =  5;
                    else if (s[0] & 0x08) mbits =  4;
                    else if (s[0] & 0x04) mbits =  3;
                    else if (s[0] & 0x02) mbits =  2;
                    else if (s[0] & 0x01) mbits =  1;
                } else {
                    /*zero*/
                    d[7] = d[6] = d[5] = d[4] = d[3] = d[2] = d[1] = d[0] = 0;
                    continue;
                }

                /*
                 * The sign and exponent.
                 */
                exponent = (mbits - 1) + 1023;
                d[7] = (sign<<7) | ((exponent>>4) & 0x7f);
                d[6] = (exponent & 0x0f) << 4;
              
                /*
                 * The mantissa.
                 */
                switch (mbits) {
                    case 32:
                        d[5] = d[4] = d[3] = d[1] = d[0] = 0;
                        break;

                    case 31:
                        d[6] |=	 0x0f	 & (s[3]>>2);
                        d[5] = (s[3]<<6) | (s[2]>>2);
                        d[4] = (s[2]<<6) | (s[1]>>2);
                        d[3] = (s[1]<<6) | (s[0]>>2);
                        d[2] = (s[0]<<6);
                        d[1] = d[0] = 0;
                        break;

                    case 30:
                        d[6] |=	 0x0f	 & (s[3]>>1);
                        d[5] = (s[3]<<7) | (s[2]>>1);
                        d[4] = (s[2]<<7) | (s[1]>>1);
                        d[3] = (s[1]<<7) | (s[0]>>1);
                        d[2] = (s[0]<<7);
                        d[1] = d[0] = 0;
                        break;

                    case 29:
                        d[6] |=	 0x0f	 & s[3];
                        d[5] = s[2];
                        d[4] = s[1];
                        d[3] = s[0];
                        d[2] = d[1] = d[0] = 0;
                        break;

                    case 28:
                        d[6] |= ((s[3]<<1) | (s[2]>>7)) & 0x0f;
                        d[5] =	 (s[2]<<1) | (s[1]>>7);
                        d[4] =	 (s[1]<<1) | (s[0]>>7);
                        d[3] =	 (s[0]<<1);
                        d[2] = d[1] = d[0] = 0;
                        break;

                    case 27:
                        d[6] |= ((s[3]<<2) | (s[2]>>6)) & 0x0f;
                        d[5] =	 (s[2]<<2) | (s[1]>>6);
                        d[4] =	 (s[1]<<2) | (s[0]>>6);
                        d[3] =	 (s[0]<<2);
                        d[2] = d[1] = d[0] = 0;
                        break;

                    case 26:
                        d[6] |= ((s[3]<<3) | (s[2]>>5)) & 0x0f;
                        d[5] =	 (s[2]<<3) | (s[1]>>5);
                        d[4] =	 (s[1]<<3) | (s[0]>>5);
                        d[3] =	 (s[0]<<3);
                        d[2] = d[1] = d[0] = 0;
                        break;

                    case 25:
                        d[6] |=	  0x0f	 & (s[2]>>4);
                        d[5] = (s[2]<<4) | (s[1]>>4);
                        d[4] = (s[1]<<4) | (s[0]>>4);
                        d[3] = (s[0]<<4);
                        d[2] = d[1] = d[0] = 0;
                        break;

                    case 24:
                        d[6] |=	  0x0f	 & (s[2]>>3);
                        d[5] = (s[2]<<5) | (s[1]>>3);
                        d[4] = (s[1]<<5) | (s[0]>>3);
                        d[3] = (s[0]<<5);
                        d[2] = d[1] = d[0] = 0;
                        break;

                    case 23:
                        d[6] |=	  0x0f	 & (s[2]>>2);
                        d[5] = (s[2]<<6) | (s[1]>>2);
                        d[4] = (s[1]<<6) | (s[0]>>2);
                        d[3] = (s[0]<<6);
                        d[2] = d[1] = d[0] = 0;
                        break;

                    case 22:
                        d[6] |=	  0x0f	 & (s[2]>>1);
                        d[5] = (s[2]<<7) | (s[1]>>1);
                        d[4] = (s[1]<<7) | (s[0]>>1);
                        d[3] = (s[0]<<7);
                        d[2] = d[1] = d[0] = 0;
                        break;

                    case 21:
                        d[6] |= 0x0f & s[2];
                        d[5] = s[1];
                        d[4] = s[0];
                        d[3] = d[2] = d[1] = d[0] = 0;
                        break;

                    case 20:
                        d[6] |= ((s[2]<<1) | (s[1]>>7)) & 0x0f;
                        d[5] =	 (s[1]<<1) | (s[0]>>7);
                        d[4] =	 (s[0]<<1);
                        d[3] = d[2] = d[1] = d[0] = 0;
                        break;

                    case 19:
                        d[6] |= ((s[2]<<2) | (s[1]>>6)) & 0x0f;
                        d[5] =	 (s[1]<<2) | (s[0]>>6);
                        d[4] =	 (s[0]<<2);
                        d[3] = d[2] = d[1] = d[0] = 0;
                        break;

                    case 18:
                        d[6] |= ((s[2]<<3) | (s[1]>>5)) & 0x0f;
                        d[5] =	 (s[1]<<3) | (s[0]>>5);
                        d[4] =	 (s[0]<<3);
                        d[3] = d[2] = d[1] = d[0] = 0;
                        break;

                    case 17:
                        d[6] |=	  0x0f	 & (s[1]>>4);
                        d[5] = (s[1]<<4) | (s[0]>>4);
                        d[4] = (s[0]<<4);
                        d[3] = d[2] = d[1] = d[0] = 0;
                        break;

                    case 16:
                        d[6] |=	  0x0f	 & (s[1]>>3);
                        d[5] = (s[1]<<5) | (s[0]>>3);
                        d[4] = (s[0]<<5);
                        d[3] = d[2] = d[1] = d[0] = 0;
                        break;

                    case 15:
                        d[6] |=	  0x0f	 & (s[1]>>2);
                        d[5] = (s[1]<<6) | (s[0]>>2);
                        d[4] = (s[0]<<6);
                        d[3] = d[2] = d[1] = d[0] = 0;
                        break;

                    case 14:
                        d[6] |=	  0x0f	 & (s[1]>>1);
                        d[5] = (s[1]<<7) | (s[0]>>1);
                        d[4] = (s[0]<<7);
                        d[3] = d[2] = d[1] = d[0] = 0;
                        break;

                    case 13:
                        d[6] |= 0x0f & s[1];
                        d[5] = s[0];
                        d[4] = d[3] = d[2] = d[1] = d[0] = 0;
                        break;
                        
                    case 12:
                        d[6] |= ((s[1]<<1) | (s[0]>>7)) & 0x0f;
                        d[5] =	 (s[0]<<1);
                        d[4] = d[3] = d[2] = d[1] = d[0] = 0;
                        break;

                    case 11:
                        d[6] |= ((s[1]<<2) | (s[0]>>6)) & 0x0f;
                        d[5] =	 (s[0]<<2);
                        d[4] = d[3] = d[2] = d[1] = d[0] = 0;
                        break;

                    case 10:
                        d[6] |= ((s[1]<<3) | (s[0]>>5)) & 0x0f;
                        d[5] =	 (s[0]<<3);
                        d[4] = d[3] = d[2] = d[1] = d[0] = 0;
                        break;

                    case 9:
                        d[6] |=	  0x0f	 & (s[0]>>4);
                        d[5] = (s[0]<<4);
                        d[4] = d[3] = d[2] = d[1] = d[0] = 0;
                        break;

                    case 8:
                        d[6] |=	  0x0f	 & (s[0]>>3);
                        d[5] = (s[0]<<5);
                        d[4] = d[3] = d[2] = d[1] = d[0] = 0;
                        break;

                    case 7:
                        d[6] |=	  0x0f	 & (s[0]>>2);
                        d[5] = (s[0]<<6);
                        d[4] = d[3] = d[2] = d[1] = d[0] = 0;
                        break;

                    case 6:
                        d[6] |=	  0x0f	 & (s[0]>>1);
                        d[5] = (s[0]<<7);
                        d[4] = d[3] = d[2] = d[1] = d[0] = 0;
                        break;

                    case 5:
                        d[6] |= 0x0f & s[0];
                        d[5] = d[4] = d[3] = d[2] = d[1] = d[0] = 0;
                        break;

                    case 4:
                        d[6] |= (s[0]<<1) & 0x0f;
                        d[5] = d[4] = d[3] = d[2] = d[1] = d[0] = 0;
                        break;

                    case 3:
                        d[6] |= (s[0]<<2) & 0x0f;
                        d[5] = d[4] = d[3] = d[2] = d[1] = d[0] = 0;
                        break;

                    case 2:
                        d[6] |= (s[0]<<3) & 0x0f;
                        d[5] = d[4] = d[3] = d[2] = d[1] = d[0] = 0;
                        break;

                    case 1:
                        d[5] = d[4] = d[3] = d[2] = d[1] = d[0] = 0;
                        break;
                }

                /*
                 * Copy temp buffer to the destination.  This only happens for
                 * the first value in the array, the last value processed. See
                 * beginning of loop.
                 */
                if (d==tmp)
                    HDmemcpy (s, d, 8);

                /* Advance pointers */
                if (buf_stride) {
                    s -= buf_stride;
                    d -= buf_stride;
                } else {
                    s -= 4;
                    d -= 8;
                }
            }
            break;

        default:
            /* Some other command we don't know about yet.*/
            HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    }
    
done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_f_i
 *
 * Purpose:	Convert one floating-point type to an integer.  This is 
 *              the catch-all function for float-integer conversions and 
 *              is probably not particularly fast.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Wednesday, Jan 21, 2004
 *
 * Modifications:
 *
 *              Raymond Lu
 *              Wednesday, April 21, 2004
 *              There is a new design for exception handling like overflow,
 *              which is passed in as a transfer property.
 *              		
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_f_i (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata, size_t nelmts,
    size_t buf_stride, size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
    hid_t dxpl_id)
{
    /* Traversal-related variables */
    H5T_t	*src_p;			/*source data type		*/
    H5T_t	*dst_p;			/*destination data type		*/
    H5T_atomic_t src;			/*atomic source info		*/
    H5T_atomic_t dst;			/*atomic destination info	*/
    int	direction;		        /*forward or backward traversal	*/
    size_t	elmtno;			/*element number		*/
    size_t	half_size;		/*half the type size		*/
    ssize_t	bitno;			/*bit number			*/
    size_t	olap;			/*num overlapping elements	*/
    uint8_t	*s, *sp, *d, *dp;	/*source and dest traversal ptrs*/
    uint8_t     *src_rev;               /*order-reversed source buffer  */
    uint8_t	dbuf[64];		/*temp destination buffer	*/

    /* Conversion-related variables */
    hssize_t	expo;			/*source exponent		*/
    hssize_t    sign;                   /*source sign bit value         */
    uint8_t     *int_buf;               /*buffer for temporary value    */ 
    size_t      buf_size;               /*buffer size for temporary value */
    size_t	i;			/*miscellaneous counters	*/
    size_t	first;                  /*first bit(MSB) in an integer  */
    ssize_t	sfirst;			/*a signed version of `first'	*/
    H5P_genplist_t      *plist;         /*Property list pointer         */
    H5T_conv_cb_t       cb_struct={NULL, NULL};      /*conversion callback structure */
    hbool_t     truncated;              /*if fraction value is dropped  */
    hbool_t     reverse;                /*if reverse order of destination at the end */
    H5T_conv_ret_t      except_ret;     /*return of callback function   */
    herr_t      ret_value=SUCCEED;      /* Return value                 */
    
    FUNC_ENTER_NOAPI(H5T_conv_f_i, FAIL);

    switch (cdata->command) {
        case H5T_CONV_INIT:
            if (NULL==(src_p=H5I_object(src_id)) ||
                    NULL==(dst_p=H5I_object(dst_id)))
                HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
            src = src_p->u.atomic;
            dst = dst_p->u.atomic;
            if (H5T_ORDER_LE!=src.order && H5T_ORDER_BE!=src.order)
                HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported byte order");
            if (dst_p->size>sizeof(dbuf))
                HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "destination size is too large");
            if (8*sizeof(expo)-1<src.u.f.esize)
                HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "exponent field is too large");
            cdata->need_bkg = H5T_BKG_NO;
            break;

        case H5T_CONV_FREE:
            break;

        case H5T_CONV_CONV:
            /* Get the data types */
            if (NULL==(src_p=H5I_object(src_id)) ||
                    NULL==(dst_p=H5I_object(dst_id)))
                HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
            src = src_p->u.atomic;
            dst = dst_p->u.atomic;

            /*
             * Do we process the values from beginning to end or vice versa? Also,
             * how many of the elements have the source and destination areas
             * overlapping?
             */
            if (src_p->size==dst_p->size || buf_stride) {
                sp = dp = (uint8_t*)buf;
                direction = 1;
                olap = nelmts;
            } else if (src_p->size>=dst_p->size) {
                double olap_d = HDceil((double)(dst_p->size)/
                                       (double)(src_p->size-dst_p->size));
                olap = (size_t)olap_d;
                sp = dp = (uint8_t*)buf;
                direction = 1;
            } else {
                double olap_d = HDceil((double)(src_p->size)/
                                       (double)(dst_p->size-src_p->size));
                olap = (size_t)olap_d;
                sp = (uint8_t*)buf + (nelmts-1) * src_p->size;
                dp = (uint8_t*)buf + (nelmts-1) * dst_p->size;
                direction = -1;
            }

            /* Allocate enough space for the buffer holding temporary 
             * converted value
             */ 
            buf_size = (size_t)HDpow((double)2.0, (double)src.u.f.esize) / 8 + 1;          
            int_buf = (uint8_t*)H5MM_calloc(buf_size);

            /* Get the plist structure. Do I need to close it? */
            if(NULL == (plist = H5P_object_verify(dxpl_id,H5P_DATASET_XFER)))
                HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find property list for ID");

            /* Get conversion exception callback property */
            if (H5P_get(plist,H5D_XFER_CONV_CB_NAME,&cb_struct)<0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get conversion exception callback");

            /* Allocate space for order-reversed source buffer */
            src_rev = (uint8_t*)H5MM_calloc(src_p->size);

            /* The conversion loop */
            for (elmtno=0; elmtno<nelmts; elmtno++) {
                /*
                 * If the source and destination buffers overlap then use a
                 * temporary buffer for the destination.
                 */
                if (direction>0) {
                    s = sp;
                    d = elmtno<olap ? dbuf : dp;
                } else {
                    s = sp;
                    d = elmtno+olap >= nelmts ? dbuf : dp;
                }
#ifndef NDEBUG
                /* I don't quite trust the overlap calculations yet --rpm */
                if (d==dbuf) {
                    assert ((dp>=sp && dp<sp+src_p->size) ||
                            (sp>=dp && sp<dp+dst_p->size));
                } else {
                    assert ((dp<sp && dp+dst_p->size<=sp) ||
                            (sp<dp && sp+src_p->size<=dp));
                }
#endif
                /*
                 * Put the data in little endian order so our loops aren't so
                 * complicated.  We'll do all the conversion stuff assuming
                 * little endian and then we'll fix the order at the end.
                 */
                if (H5T_ORDER_BE==src.order) {
                    half_size = src_p->size/2;
                    for (i=0; i<half_size; i++) {
                        uint8_t tmp = s[src_p->size-(i+1)];
                        s[src_p->size-(i+1)] = s[i];
                        s[i] = tmp;
                    }
                }
                
                /*zero-set all destination bits*/ 
                H5T_bit_set (d, dst.offset, dst.prec, FALSE);
                
                /*
                 * Find the sign bit value of the source.
                 */
                sign = H5T_bit_get_d(s, src.prec-1, 1);

                /*
                 * Check for special cases: +0, -0, +Inf, -Inf, NaN
                 */
                if (H5T_bit_find (s, src.u.f.mpos, src.u.f.msize,
                                  H5T_BIT_LSB, TRUE)<0) {
                    if (H5T_bit_find (s, src.u.f.epos, src.u.f.esize,
                                      H5T_BIT_LSB, TRUE)<0) {
                        /* +0 or -0 */
                        /* Set all bits to zero */
                        goto padding;
                    } else if (H5T_bit_find (s, src.u.f.epos, src.u.f.esize,
                                             H5T_BIT_LSB, FALSE)<0) {
                        /* +Infinity or -Infinity */
                        if(sign) { /* -Infinity */
                            if (H5T_SGN_2==dst.u.i.sign) 
                                H5T_bit_set (d, dst.prec-1, 1, TRUE);
                        } else { /* +Infinity */
                            if (H5T_SGN_NONE==dst.u.i.sign) 
                                H5T_bit_set (d, dst.offset, dst.prec, TRUE);
                            else if (H5T_SGN_2==dst.u.i.sign) 
                                H5T_bit_set (d, dst.offset, dst.prec-1, TRUE);
                        }        
                        goto padding;
                    }
                } else if (H5T_bit_find (s, src.u.f.epos, src.u.f.esize,
                                         H5T_BIT_LSB, FALSE)<0) {
                    /*
                     * NaN. There are many NaN values, so we just set all bits to zero. 
                     */
                    goto padding;
                }
                
                /*
                 * Get the exponent as an unsigned quantity from the section of
                 * the source bit field where it's located.   Not expecting 
                 * exponent to be greater than the maximal value of hssize_t.
                 */
                expo = H5T_bit_get_d(s, src.u.f.epos, src.u.f.esize);
                
                /*
                 * Calculate the true source exponent by adjusting according to
                 * the source exponent bias.
                 */
                if (0==expo || H5T_NORM_NONE==src.u.f.norm) {
                    bitno = H5T_bit_find(s, src.u.f.mpos, src.u.f.msize,
                                         H5T_BIT_MSB, TRUE);
                    assert(bitno>=0);
                    expo -= (src.u.f.ebias-1) + (src.u.f.msize-bitno);
                } else if (H5T_NORM_IMPLIED==src.u.f.norm) {
                    expo -= src.u.f.ebias;
                } else {
                    assert("normalization method not implemented yet" && 0);
                    HDabort();
                }

                /*
                 * Get the mantissa as bit vector from the section of
                 * the source bit field where it's located.
                 * Keep the little-endian order in the buffer.  
                 * A sequence 0x01020304 will be like in the buffer,
                 *      04      03      02      01
                 *      |       |       |       |
                 *      V       V       V       V
                 *    buf[0]  buf[1]  buf[2]  buf[3]           
                 */
                H5T_bit_copy(int_buf, 0, s, src.u.f.mpos, src.u.f.msize);
                
                /*
                 * Restore the implicit bit for mantissa if it's implied.
                 * Equivalent to mantissa |= (hsize_t)1<<src.u.f.msize.
                 */
                if (H5T_NORM_IMPLIED==src.u.f.norm)
                    H5T_bit_inc(int_buf, src.u.f.msize, 8*buf_size-src.u.f.msize);

                /* 
                 * Shift mantissa part by exponent minus mantissa size(right shift), 
                 * or by mantissa size minus exponent(left shift).  Example: Sequence
                 * 10...010111, expo=20, expo-msize=-3.  Right-shift the sequence, we get 
                 * 00010...01.  The last three ones were dropped.
                 */
                H5T_bit_shift(int_buf, (ssize_t)(expo-src.u.f.msize), 0, buf_size*8);

                /*
                 * If expo is less than mantissa size, the frantional value is dropped off
                 * during conversion.  Set exception type to be "truncate"
                 */
                if (expo < src.u.f.msize && cb_struct.func)
                    truncated = TRUE;

                /* Convert to integer representation if negative. 
                 * equivalent to ~(value - 1).
                 */
                if(sign) {
                    H5T_bit_dec(int_buf, 0, 8*buf_size);
                    H5T_bit_neg(int_buf, 0, 8*buf_size);
                }
                
                /*
                 * What is the bit position for the most significant bit(MSB) of S
                 * which is set?
                 */
                sfirst = H5T_bit_find(int_buf, 0, 8*buf_size, H5T_BIT_MSB, TRUE);
                first = (size_t)sfirst;

                /* Set these variables to default */
                except_ret = H5T_CONV_UNHANDLED;
                truncated  = FALSE;
                reverse    = TRUE;

                if(sfirst < 0) {
                    /*
                     * The source has no bits set and must therefore be zero.
                     * Set the destination to zero - nothing to do.
                     */
                } else if (H5T_SGN_NONE==dst.u.i.sign) { /*destination is unsigned*/
                    /*
                     * Destination is unsigned.  Library's default way: If the source value 
                     * is greater than the maximal destination value then it overflows, the 
                     * destination will be set to the maximum possible value.  When the
                     * source is negative, underflow happens.  Set the destination to be
                     * zero(do nothing).  If user's exception handler is set, call it and 
                     * let user handle it. 
                     */
                    if(sign) { /*source is negative*/
                        if(cb_struct.func) { /*If user's exception handler is present, use it*/
                            H5T_reverse_order(src_rev, s, src_p->size, src_p->u.atomic.order); /*reverse order first*/
                            except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_RANGE_LOW, 
                                    src_id, dst_id, src_rev, d, cb_struct.user_data);
                            if(except_ret == H5T_CONV_ABORT)
                                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception")
                            else if(except_ret == H5T_CONV_HANDLED)
                                /*No need to reverse the order of destination because user handles it*/
                                reverse = FALSE;
                        }
                    } else { /*source is positive*/
                        if (first>=dst.prec) {
                            /*overflow*/
                            if(cb_struct.func) { /*If user's exception handler is present, use it*/
                                H5T_reverse_order(src_rev, s, src_p->size, src_p->u.atomic.order); /*reverse order first*/
                                except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_RANGE_HI, 
                                        src_id, dst_id, src_rev, d, cb_struct.user_data);
                            }

                            if(except_ret == H5T_CONV_UNHANDLED)  
                                H5T_bit_set (d, dst.offset, dst.prec, TRUE);
                            else if(except_ret == H5T_CONV_HANDLED)
                                /*No need to reverse the order of destination because user handles it*/
                                reverse = FALSE;
                            else if(except_ret == H5T_CONV_ABORT)
                                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception")
                        } else if (first <dst.prec) {
                            if(truncated && cb_struct.func) { /*If user's exception handler is present, use it*/
                                H5T_reverse_order(src_rev, s, src_p->size, src_p->u.atomic.order); /*reverse order first*/
                                except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_TRUNCATE, 
                                        src_id, dst_id, src_rev, d, cb_struct.user_data);
                            }
                            
                            if(except_ret == H5T_CONV_UNHANDLED)  
                                /*copy source value into it if case is ignored by user handler*/    
                                H5T_bit_copy (d, dst.offset, int_buf, 0, first+1);
                            else if(except_ret == H5T_CONV_HANDLED)
                                /*No need to reverse the order of destination because user handles it*/
                                reverse = FALSE;
                            else if(except_ret == H5T_CONV_ABORT)
                                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception")
                        }
                    } 
                } else if (H5T_SGN_2==dst.u.i.sign) {  /*Destination is signed*/
                    if(sign) { /*source is negative*/
                        if(first < dst.prec-1) {
                            if(truncated && cb_struct.func) { /*If user's exception handler is present, use it*/
                                H5T_reverse_order(src_rev, s, src_p->size, src_p->u.atomic.order); /*reverse order first*/
                                except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_TRUNCATE, 
                                        src_id, dst_id, src_rev, d, cb_struct.user_data);
                            }
                            
                            if(except_ret == H5T_CONV_UNHANDLED) { 
                                /*copy source value into it if case is ignored by user handler*/    
                                H5T_bit_copy (d, dst.offset, int_buf, 0, first+1);
                                H5T_bit_set (d, (dst.offset + dst.prec-1), 1, TRUE);
                            } else if(except_ret == H5T_CONV_ABORT)
                                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception")
                            else if(except_ret == H5T_CONV_HANDLED)
                                /*No need to reverse the order of destination because user handles it*/
                                reverse = FALSE;
                        } else {
                            /* if underflows and no callback, do nothing except turn on 
                             * the sign bit because 0x80...00 is the biggest negative value.
                             */
                            if(cb_struct.func) { /*If user's exception handler is present, use it*/
                                H5T_reverse_order(src_rev, s, src_p->size, src_p->u.atomic.order); /*reverse order first*/
                                except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_RANGE_LOW, 
                                        src_id, dst_id, src_rev, d, cb_struct.user_data);
                            }

                            if(except_ret == H5T_CONV_UNHANDLED)
                                H5T_bit_set (d, (dst.offset + dst.prec-1), 1, TRUE);
                            else if(except_ret == H5T_CONV_ABORT)
                                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception")
                            else if(except_ret == H5T_CONV_HANDLED)
                                /*No need to reverse the order of destination because user handles it*/
                                reverse = FALSE;
                        }
                    } else { /*source is positive*/
                        if (first >= dst.prec-1) {
                            /*overflow*/
                            if(cb_struct.func) { /*If user's exception handler is present, use it*/ 
                                H5T_reverse_order(src_rev, s, src_p->size, src_p->u.atomic.order); /*reverse order first*/
                                except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_RANGE_HI, 
                                        src_id, dst_id, src_rev, d, cb_struct.user_data);
                            }

                            if(except_ret == H5T_CONV_UNHANDLED)
                                H5T_bit_set (d, dst.offset, dst.prec-1, TRUE);
                            else if(except_ret == H5T_CONV_ABORT)
                                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception")
                            else if(except_ret == H5T_CONV_HANDLED)
                                /*No need to reverse the order of destination because user handles it*/
                                reverse = FALSE;
                        } else if(first < dst.prec-1) {
                            if(truncated && cb_struct.func) { /*If user's exception handler is present, use it*/
                                H5T_reverse_order(src_rev, s, src_p->size, src_p->u.atomic.order); /*reverse order first*/
                                except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_TRUNCATE, 
                                        src_id, dst_id, src_rev, d, cb_struct.user_data);
                            }
                            
                            if(except_ret == H5T_CONV_UNHANDLED) { 
                                /*copy source value into it if case is ignored by user handler*/    
                                H5T_bit_copy (d, dst.offset, int_buf, 0, first+1);
                            } else if(except_ret == H5T_CONV_ABORT)
                                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception")
                            else if(except_ret == H5T_CONV_HANDLED)
                                /*No need to reverse the order of destination because user handles it*/
                                reverse = FALSE;
                        }
                    }
                }

            padding:
                /*
                 * Set padding areas in destination.
                 */
                if (dst.offset>0) {
                    assert (H5T_PAD_ZERO==dst.lsb_pad || H5T_PAD_ONE==dst.lsb_pad);
                    H5T_bit_set (d, 0, dst.offset, (hbool_t)(H5T_PAD_ONE==dst.lsb_pad));
                }
                if (dst.offset+dst.prec!=8*dst_p->size) {
                    assert (H5T_PAD_ZERO==dst.msb_pad || H5T_PAD_ONE==dst.msb_pad);
                    H5T_bit_set (d, dst.offset+dst.prec,
                                 8*dst_p->size - (dst.offset+ dst.prec),
                                 (hbool_t)(H5T_PAD_ONE==dst.msb_pad));
                }

                /*
                 * Put the destination in the correct byte order.  See note at
                 * beginning of loop.
                 */
                if (H5T_ORDER_BE==dst.order && reverse) {
                    half_size = dst_p->size/2;
                    for (i=0; i<half_size; i++) {
                        uint8_t tmp = d[dst_p->size-(i+1)];
                        d[dst_p->size-(i+1)] = d[i];
                        d[i] = tmp;
                    }
                }

                /*
                 * If we had used a temporary buffer for the destination then we
                 * should copy the value to the true destination buffer.
                 */
                if (d==dbuf)
                    HDmemcpy (dp, d, dst_p->size);
                if (buf_stride) {
                    sp += direction * buf_stride;
                    dp += direction * buf_stride;
                } else {
                    sp += direction * src_p->size;
                    dp += direction * dst_p->size;
                }

                HDmemset(int_buf, 0, buf_size);
            }
           
            H5MM_xfree(int_buf);
            H5MM_free(src_rev);

            break;
                
        default:
            HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_i_f
 *
 * Purpose:	Convert one integer type to a floating-point type.  This is 
 *              the catch-all function for integer-float conversions and 
 *              is probably not particularly fast.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Friday, Feb 6, 2004
 *
 * Modifications:
 *		
 *              Raymond Lu
 *              Wednesday, April 21, 2004
 *              There is a new design for exception handling like overflow,
 *              which is passed in as a transfer property.
 *              
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_i_f (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata, size_t nelmts,
    size_t buf_stride, size_t UNUSED bkg_stride, void *buf, void UNUSED *bkg,
    hid_t dxpl_id)
{
    /* Traversal-related variables */
    H5T_t	*src_p;			/*source data type		*/
    H5T_t	*dst_p;			/*destination data type		*/
    H5T_atomic_t src;			/*atomic source info		*/
    H5T_atomic_t dst;			/*atomic destination info	*/
    int	direction;		/*forward or backward traversal	*/
    size_t	elmtno;			/*element number		*/
    size_t	half_size;		/*half the type size		*/
    size_t	olap;			/*num overlapping elements	*/
    uint8_t	*s, *sp, *d, *dp;	/*source and dest traversal ptrs*/
    uint8_t     *src_rev;               /*order-reversed source buffer  */
    uint8_t	dbuf[64];		/*temp destination buffer	*/

    /* Conversion-related variables */
    hsize_t	expo;			/*destiny exponent		*/
    hsize_t	expo_max;		/*maximal possible exponent value       */
    size_t      sign;                   /*source sign bit value         */
    hbool_t     is_max_neg;             /*source is maximal negative value*/
    hbool_t     do_round;               /*whether there is roundup      */
    uint8_t     *int_buf;               /*buffer for temporary value    */ 
    size_t      buf_size;               /*buffer size for temporary value */
    size_t	i;			/*miscellaneous counters	*/
    size_t	first;                  /*first bit(MSB) in an integer  */
    ssize_t	sfirst;			/*a signed version of `first'	*/
    H5P_genplist_t      *plist;         /*Property list pointer         */
    H5T_conv_cb_t       cb_struct={NULL, NULL};      /*conversion callback structure */
    H5T_conv_ret_t      except_ret;     /*return of callback function   */
    hbool_t             reverse;        /*if reverse the order of destination   */
    herr_t      ret_value=SUCCEED;      /* Return value */
    
    FUNC_ENTER_NOAPI(H5T_conv_i_f, FAIL);

    switch (cdata->command) {
        case H5T_CONV_INIT:
            if (NULL==(src_p=H5I_object(src_id)) ||
                    NULL==(dst_p=H5I_object(dst_id)))
                HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
            src = src_p->u.atomic;
            dst = dst_p->u.atomic;
            if (H5T_ORDER_LE!=dst.order && H5T_ORDER_BE!=dst.order)
                HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported byte order");
            if (dst_p->size>sizeof(dbuf))
                HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "destination size is too large");
            if (8*sizeof(expo)-1<src.u.f.esize)
                HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "exponent field is too large");
            cdata->need_bkg = H5T_BKG_NO;
            break;

        case H5T_CONV_FREE:
            break;

        case H5T_CONV_CONV:
            /* Get the data types */
            if (NULL==(src_p=H5I_object(src_id)) ||
                    NULL==(dst_p=H5I_object(dst_id)))
                HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
            src = src_p->u.atomic;
            dst = dst_p->u.atomic;

            /*
             * Do we process the values from beginning to end or vice versa? Also,
             * how many of the elements have the source and destination areas
             * overlapping?
             */
            if (src_p->size==dst_p->size || buf_stride) {
                sp = dp = (uint8_t*)buf;
                direction = 1;
                olap = nelmts;
            } else if (src_p->size>=dst_p->size) {
                double olap_d = HDceil((double)(dst_p->size)/
                                       (double)(src_p->size-dst_p->size));
                olap = (size_t)olap_d;
                sp = dp = (uint8_t*)buf;
                direction = 1;
            } else {
                double olap_d = HDceil((double)(src_p->size)/
                                       (double)(dst_p->size-src_p->size));
                olap = (size_t)olap_d;
                sp = (uint8_t*)buf + (nelmts-1) * src_p->size;
                dp = (uint8_t*)buf + (nelmts-1) * dst_p->size;
                direction = -1;
            }

            /* Allocate enough space for the buffer holding temporary 
             * converted value
             */ 
            buf_size = (src.prec > dst.u.f.msize ? src.prec : dst.u.f.msize)/8 + 1;
            int_buf = (uint8_t*)H5MM_calloc(buf_size);

            /* Get the plist structure */
            if(NULL == (plist = H5P_object_verify(dxpl_id,H5P_DATASET_XFER)))
                HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find property list for ID");

            /* Get conversion exception callback property */
            if (H5P_get(plist,H5D_XFER_CONV_CB_NAME,&cb_struct)<0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get conversion exception callback");

            /* Allocate space for order-reversed source buffer */
            src_rev = (uint8_t*)H5MM_calloc(src_p->size);

            /* The conversion loop */
            for (elmtno=0; elmtno<nelmts; elmtno++) {
                /* Make sure these variables are reset to 0. */
                sign = 0;               /*source sign bit value         */
                is_max_neg = 0;         /*source is maximal negative value*/
                do_round = 0;           /*whether there is roundup      */

                /*
                 * If the source and destination buffers overlap then use a
                 * temporary buffer for the destination.
                 */
                if (direction>0) {
                    s = sp;
                    d = elmtno<olap ? dbuf : dp;
                } else {
                    s = sp;
                    d = elmtno+olap >= nelmts ? dbuf : dp;
                }
#ifndef NDEBUG
                /* I don't quite trust the overlap calculations yet --rpm */
                if (d==dbuf) {
                    assert ((dp>=sp && dp<sp+src_p->size) ||
                            (sp>=dp && sp<dp+dst_p->size));
                } else {
                    assert ((dp<sp && dp+dst_p->size<=sp) ||
                            (sp<dp && sp+src_p->size<=dp));
                }
#endif

                /*
                 * Put the data in little endian order so our loops aren't so
                 * complicated.  We'll do all the conversion stuff assuming
                 * little endian and then we'll fix the order at the end.
                 */
                if (H5T_ORDER_BE==src.order) {
                    half_size = src_p->size/2;
                    for (i=0; i<half_size; i++) {
                        uint8_t tmp = s[src_p->size-(i+1)];
                        s[src_p->size-(i+1)] = s[i];
                        s[i] = tmp;
                    }
                }
                
                /*zero-set all destination bits*/ 
                H5T_bit_set (d, dst.offset, dst.prec, FALSE);
                
                /* Copy source into a temporary buffer */
                H5T_bit_copy(int_buf, 0, s, src.offset, src.prec);

                /*
                 * Find the sign bit value of the source.
                 */
                if (H5T_SGN_2 == src.u.i.sign)
                    sign = H5T_bit_get_d(int_buf, src.prec-1, 1);

                /*
                 * What is the bit position(starting from 0 as first one) for the most significant 
		 * bit(MSB) of S which is set?
                 */
                if(H5T_SGN_2 == src.u.i.sign) {
                    sfirst = H5T_bit_find(int_buf, 0, src.prec-1, H5T_BIT_MSB, TRUE);
                    if(sign && sfirst < 0)
			/* The case 0x80...00, which is negative with maximal value */
                        is_max_neg = 1;    
                } else if(H5T_SGN_NONE == src.u.i.sign)
                    sfirst = H5T_bit_find(int_buf, 0, src.prec, H5T_BIT_MSB, TRUE);

                /* Handle special cases here.  Integer is zero */
                if(!sign && sfirst < 0)
                    goto padding;
                
                /*
                 * Convert source integer if it's negative
                 */
                if (H5T_SGN_2 == src.u.i.sign && sign) {
                    if(!is_max_neg) {
                        /* Equivalent to ~(i - 1) */
                        H5T_bit_dec(int_buf, 0, buf_size*8);
                        H5T_bit_neg(int_buf, 0, buf_size*8);
                        sfirst = H5T_bit_find(int_buf, 0, src.prec-1, H5T_BIT_MSB, TRUE);
                    } else {
			/* If it's maximal negative number 0x80...000, treat it as if it overflowed
			 * (create a carry) to help conversion.  i.e. a character type number 0x80
			 * is treated as 0x100.
			 */
                        sfirst = (ssize_t)(src.prec-1);
                        is_max_neg = 0;
                    }

                    /* Sign bit has been negated if bit vector isn't 0x80...00.  Set all bits in front of 
                     * sign bit to 0 in the temporary buffer because they're all negated from the previous
		     * step. */
                    H5T_bit_set(int_buf, src.prec, buf_size*8-src.prec, 0);

                    /* Set sign bit in destiny */
                    H5T_bit_set_d(d, dst.u.f.sign, 1, sign);
                }

                first = (size_t)sfirst;

                /* Set these variables to default */
                except_ret = H5T_CONV_UNHANDLED;
                reverse    = TRUE;

                /*
                 * Calculate the true destination exponent by adjusting according to
                 * the destination exponent bias.
                 */
                if (H5T_NORM_NONE==src.u.f.norm) {
                    expo = first + 1 + dst.u.f.ebias;
                } else if (H5T_NORM_IMPLIED==src.u.f.norm) {
                    expo = first + dst.u.f.ebias;
                } else {
                    assert("normalization method not implemented yet" && 0);
                    HDabort();
                }

                /* Handle mantissa part here */
                if (H5T_NORM_IMPLIED==src.u.f.norm) {
                    /* Imply first bit */
                    H5T_bit_set(int_buf, first, 1, 0);
       		} else if (H5T_NORM_NONE==src.u.f.norm) {
		    first++;
		}

                /* Roundup for mantissa */ 
                if(first > dst.u.f.msize) {
		    /* If the bit sequence is bigger than the mantissa part, there'll be some 
                     * precision loss.  Let user's handler deal with the case if it's present 
                     */
                    if(cb_struct.func) { 
                        H5T_reverse_order(src_rev, s, src_p->size, src_p->u.atomic.order); /*reverse order first*/
                        except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_PRECISION, src_id, dst_id, 
                                src_rev, d, cb_struct.user_data);
                    }
                    
                    if(except_ret == H5T_CONV_HANDLED) {
                        reverse = FALSE;
                        goto padding;
                    } else if(except_ret == H5T_CONV_ABORT)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception")

		    /* If user's exception handler does deal with it, we do it by dropping off the 
		     * extra bits at the end and do rounding.  If we have .50...0(decimal) after radix 
		     * point, we do roundup when the least significant digit before radix is odd, we do 
		     * rounddown if it's even.
		     */
			
                    /* Check 1st dropoff bit, see if it's set. */
                    if(H5T_bit_get_d(int_buf, ((first-dst.u.f.msize)-1), 1)) {
                    	/* Check all bits after 1st dropoff bit, see if any of them is set. */
                        if(((first-dst.u.f.msize)-1) > 0 && H5T_bit_get_d(int_buf, 0, ((first-dst.u.f.msize)-1)))
                            do_round = 1;
                        else {  /* The .50...0 case */
                            /* Check if the least significant bit is odd. */
                            if(H5T_bit_get_d(int_buf, (first-dst.u.f.msize), 1))
                               do_round = 1;
                        }
		    } 

                    /* Right shift to drop off extra bits */
                    H5T_bit_shift(int_buf, (ssize_t)(dst.u.f.msize-first), 0, buf_size*8);

                    if(do_round) {
                        H5T_bit_inc(int_buf, 0, buf_size*8);
                        do_round = 0;

			/* If integer is like 0x0ff...fff and we need to round up the 
			 * last f, we get 0x100...000.  Treat this special case here.
			 */  
                    	if(H5T_bit_get_d(int_buf, dst.u.f.msize, 1)) {
                	    if (H5T_NORM_IMPLIED==src.u.f.norm) {
			        /* The bit at this 1's position was impled already, so this 
			         * number should be 0x200...000.  We need to increment the 
			         * exponent in this case.  
			         */ 
			    	expo++;
       			    } else if (H5T_NORM_NONE==src.u.f.norm) {
				/* Right shift 1 bit to let the carried 1 fit in the mantissa,
				 * and increment exponent by 1.
				 */
                                H5T_bit_shift(int_buf, -1, 0, buf_size*8);
			 	expo++;    
			    }
			}
                    }
                } else {
                    /* The bit sequence can fit mantissa part.  Left shift to fit in from high-order of
		     * bit position. */
                    H5T_bit_shift(int_buf, (ssize_t)(dst.u.f.msize-first), 0, buf_size*8);
                } 
                    

                /* Check if the exponent is too big */
                expo_max = (hsize_t)HDpow((double)2.0, (double)dst.u.f.esize) - 1;         

                if(expo > expo_max) {  /*overflows*/
                    if(cb_struct.func) { /*user's exception handler.  Reverse back source order*/
                        H5T_reverse_order(src_rev, s, src_p->size, src_p->u.atomic.order); /*reverse order first*/
                        except_ret = (cb_struct.func)(H5T_CONV_EXCEPT_RANGE_HI, src_id, dst_id, 
                                src_rev, d, cb_struct.user_data);

                        if(except_ret == H5T_CONV_ABORT)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception")
                        else if(except_ret == H5T_CONV_HANDLED) {
                            reverse = FALSE;
                            goto padding;
                        }
                    } else { 
                        /*make destination infinity by setting exponent to maximal number and
                         *mantissa to zero.*/  
                        expo = expo_max;
                        HDmemset(int_buf, 0, buf_size);
                    }
                }

                if(except_ret == H5T_CONV_UNHANDLED) {
                    /* Set exponent in destiny */
                    H5T_bit_set_d(d, dst.u.f.epos, dst.u.f.esize, expo);

                    /* Copy mantissa into destiny */
                    H5T_bit_copy(d, dst.u.f.mpos, int_buf, 0, buf_size*8 > dst.u.f.msize ? dst.u.f.msize : buf_size*8);
                }

            padding:
                /*
                 * Set padding areas in destination.
                 */
                if (dst.offset>0) {
                    assert (H5T_PAD_ZERO==dst.lsb_pad || H5T_PAD_ONE==dst.lsb_pad);
                    H5T_bit_set (d, 0, dst.offset, (hbool_t)(H5T_PAD_ONE==dst.lsb_pad));
                }
                if (dst.offset+dst.prec!=8*dst_p->size) {
                    assert (H5T_PAD_ZERO==dst.msb_pad || H5T_PAD_ONE==dst.msb_pad);
                    H5T_bit_set (d, dst.offset+dst.prec,
                                 8*dst_p->size - (dst.offset+ dst.prec),
                                 (hbool_t)(H5T_PAD_ONE==dst.msb_pad));
                }

                /*
                 * Put the destination in the correct byte order.  See note at
                 * beginning of loop.
                 */
                if (H5T_ORDER_BE==dst.order && reverse) {
                    half_size = dst_p->size/2;
                    for (i=0; i<half_size; i++) {
                        uint8_t tmp = d[dst_p->size-(i+1)];
                        d[dst_p->size-(i+1)] = d[i];
                        d[i] = tmp;
                    }
                }
                
                /*
                 * If we had used a temporary buffer for the destination then we
                 * should copy the value to the true destination buffer.
                 */
                if (d==dbuf)
                    HDmemcpy (dp, d, dst_p->size);
                if (buf_stride) {
                    sp += direction * buf_stride;
                    dp += direction * buf_stride;
                } else {
                    sp += direction * src_p->size;
                    dp += direction * dst_p->size;
                }

                HDmemset(int_buf, 0, buf_size);
            }
           
            H5MM_xfree(int_buf);
            H5MM_free(src_rev);

            break;
                
        default:
            HGOTO_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_reverse_order
 *
 * Purpose:	Internal assisting function to reverse the order of 
 *              a sequence of byte when it's big endian.  The byte sequence 
 *              simulates the endian order.
 *
 * Return:      Success:        A pointer to the reversed byte sequence
 *
 *              Failure:        Null	
 *
 * Programmer:	Raymond Lu
 *		April 26, 2004
 *
 * Modifications:
 *		
 *-------------------------------------------------------------------------
 */
static herr_t
H5T_reverse_order(uint8_t *rev, uint8_t *s, size_t size, H5T_order_t order)
{
    herr_t      ret_value = SUCCEED;
    size_t      i;

    FUNC_ENTER_NOAPI(H5T_reverse_order, FAIL);

    assert(s);
    assert(size);

    if (H5T_ORDER_BE == order) {
        for (i=0; i<size; i++)
            rev[size-(i+1)] = s[i];
    } else {
        for (i=0; i<size; i++)
            rev[i] = s[i];
    }
    
done:
    FUNC_LEAVE_NOAPI(ret_value);
}
