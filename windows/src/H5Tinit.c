/* Generated automatically by H5detect -- do not edit */



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
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 * Created:             Mar 30, 2005
 *                      
 *
 * Purpose:             This machine-generated source code contains
 *                      information about the various integer and
 *                      floating point numeric formats found on this
 *                      architecture.  The parameters below should be
 *                      checked carefully and errors reported to the
 *                      HDF5 maintainer.
 *                      
 *                      Each of the numeric formats listed below are
 *                      printed from most significant bit to least
 *                      significant bit even though the actual bytes
 *                      might be stored in a different order in
 *                      memory.  The integers above each binary byte
 *                      indicate the relative order of the bytes in
 *                      memory; little-endian machines have
 *                      decreasing numbers while big-endian machines
 *                      have increasing numbers.
 *                      
 *                      The fields of the numbers are printed as
 *                      letters with `S' for the mantissa sign bit,
 *                      `M' for the mantissa magnitude, and `E' for
 *                      the exponent.  The exponent has an associated
 *                      bias which can be subtracted to find the
 *                      true exponent.  The radix point is assumed
 *                      to be before the first `M' bit.  Any bit
 *                      of a floating-point value not falling into one
 *                      of these categories is printed as a question
 *                      mark.  Bits of integer types are printed as
 *                      `I' for 2's complement and `U' for magnitude.
 *                      
 *                      If the most significant bit of the normalized
 *                      mantissa (always a `1' except for `0.0') is
 *                      not stored then an `implicit=yes' appears
 *                      under the field description.  In thie case,
 *                      the radix point is still assumed to be
 *                      before the first `M' but after the implicit
 *                      bit.
 *
 * Modifications:
 *
 *      DO NOT MAKE MODIFICATIONS TO THIS FILE!
 *      It was generated by code in `H5detect.c'.
 *
 *-------------------------------------------------------------------------
 */

#define H5T_PACKAGE /*suppress error about including H5Tpkg.h*/

#include "H5private.h"
#include "H5Iprivate.h"
#include "H5Eprivate.h"
#include "H5FLprivate.h"
#include "H5Tpkg.h"

/* Declare external the free lists for H5T_t's and H5T_shared_t's */
H5FL_EXTERN(H5T_t);
H5FL_EXTERN(H5T_shared_t);



herr_t
H5TN_init_interface(void)
{
    H5T_t       *dt = NULL;
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5TN_init_interface, FAIL);

   /*
    *    0
    * IIIIIIII
    * Alignment: none
    */
    if (NULL==(dt = H5FL_CALLOC (H5T_t)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,"memory allocation failed");
    if (NULL==(dt->shared = H5FL_CALLOC(H5T_shared_t)))
    {         H5FL_FREE(H5T_t, dt);        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    }     dt->shared->state = H5T_STATE_IMMUTABLE;
    dt->ent.header = HADDR_UNDEF;
    dt->shared->type = H5T_INTEGER;
    dt->shared->size = 1;
    dt->shared->u.atomic.order = H5T_ORDER_LE;
    dt->shared->u.atomic.offset = 0;
    dt->shared->u.atomic.prec = 8;
    dt->shared->u.atomic.lsb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.msb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.u.i.sign = H5T_SGN_2;
    if ((H5T_NATIVE_SCHAR_g = H5I_register (H5I_DATATYPE, dt))<0)
        HGOTO_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,"can't initialize type system (atom registration failure");
    H5T_NATIVE_SCHAR_ALIGN_g = 1;
    H5T_NATIVE_SCHAR_COMP_ALIGN_g = 1;

   /*
    *    0
    * UUUUUUUU
    * Alignment: none
    */
    if (NULL==(dt = H5FL_CALLOC (H5T_t)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,"memory allocation failed");
    if (NULL==(dt->shared = H5FL_CALLOC(H5T_shared_t)))
    {         H5FL_FREE(H5T_t, dt);        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    }     dt->shared->state = H5T_STATE_IMMUTABLE;
    dt->ent.header = HADDR_UNDEF;
    dt->shared->type = H5T_INTEGER;
    dt->shared->size = 1;
    dt->shared->u.atomic.order = H5T_ORDER_LE;
    dt->shared->u.atomic.offset = 0;
    dt->shared->u.atomic.prec = 8;
    dt->shared->u.atomic.lsb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.msb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.u.i.sign = H5T_SGN_NONE;
    if ((H5T_NATIVE_UCHAR_g = H5I_register (H5I_DATATYPE, dt))<0)
        HGOTO_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,"can't initialize type system (atom registration failure");
    H5T_NATIVE_UCHAR_ALIGN_g = 1;

   /*
    *    1        0
    * IIIIIIII IIIIIIII
    * Alignment: none
    */
    if (NULL==(dt = H5FL_CALLOC (H5T_t)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,"memory allocation failed");
    if (NULL==(dt->shared = H5FL_CALLOC(H5T_shared_t)))
    {         H5FL_FREE(H5T_t, dt);        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    }     dt->shared->state = H5T_STATE_IMMUTABLE;
    dt->ent.header = HADDR_UNDEF;
    dt->shared->type = H5T_INTEGER;
    dt->shared->size = 2;
    dt->shared->u.atomic.order = H5T_ORDER_LE;
    dt->shared->u.atomic.offset = 0;
    dt->shared->u.atomic.prec = 16;
    dt->shared->u.atomic.lsb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.msb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.u.i.sign = H5T_SGN_2;
    if ((H5T_NATIVE_SHORT_g = H5I_register (H5I_DATATYPE, dt))<0)
        HGOTO_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,"can't initialize type system (atom registration failure");
    H5T_NATIVE_SHORT_ALIGN_g = 1;
    H5T_NATIVE_SHORT_COMP_ALIGN_g = 2;

   /*
    *    1        0
    * UUUUUUUU UUUUUUUU
    * Alignment: none
    */
    if (NULL==(dt = H5FL_CALLOC (H5T_t)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,"memory allocation failed");
    if (NULL==(dt->shared = H5FL_CALLOC(H5T_shared_t)))
    {         H5FL_FREE(H5T_t, dt);        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    }     dt->shared->state = H5T_STATE_IMMUTABLE;
    dt->ent.header = HADDR_UNDEF;
    dt->shared->type = H5T_INTEGER;
    dt->shared->size = 2;
    dt->shared->u.atomic.order = H5T_ORDER_LE;
    dt->shared->u.atomic.offset = 0;
    dt->shared->u.atomic.prec = 16;
    dt->shared->u.atomic.lsb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.msb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.u.i.sign = H5T_SGN_NONE;
    if ((H5T_NATIVE_USHORT_g = H5I_register (H5I_DATATYPE, dt))<0)
        HGOTO_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,"can't initialize type system (atom registration failure");
    H5T_NATIVE_USHORT_ALIGN_g = 1;

   /*
    *    3        2        1        0
    * IIIIIIII IIIIIIII IIIIIIII IIIIIIII
    * Alignment: none
    */
    if (NULL==(dt = H5FL_CALLOC (H5T_t)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,"memory allocation failed");
    if (NULL==(dt->shared = H5FL_CALLOC(H5T_shared_t)))
    {         H5FL_FREE(H5T_t, dt);        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    }     dt->shared->state = H5T_STATE_IMMUTABLE;
    dt->ent.header = HADDR_UNDEF;
    dt->shared->type = H5T_INTEGER;
    dt->shared->size = 4;
    dt->shared->u.atomic.order = H5T_ORDER_LE;
    dt->shared->u.atomic.offset = 0;
    dt->shared->u.atomic.prec = 32;
    dt->shared->u.atomic.lsb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.msb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.u.i.sign = H5T_SGN_2;
    if ((H5T_NATIVE_INT_g = H5I_register (H5I_DATATYPE, dt))<0)
        HGOTO_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,"can't initialize type system (atom registration failure");
    H5T_NATIVE_INT_ALIGN_g = 1;
    H5T_NATIVE_INT_COMP_ALIGN_g = 4;

   /*
    *    3        2        1        0
    * UUUUUUUU UUUUUUUU UUUUUUUU UUUUUUUU
    * Alignment: none
    */
    if (NULL==(dt = H5FL_CALLOC (H5T_t)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,"memory allocation failed");
    if (NULL==(dt->shared = H5FL_CALLOC(H5T_shared_t)))
    {         H5FL_FREE(H5T_t, dt);        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    }     dt->shared->state = H5T_STATE_IMMUTABLE;
    dt->ent.header = HADDR_UNDEF;
    dt->shared->type = H5T_INTEGER;
    dt->shared->size = 4;
    dt->shared->u.atomic.order = H5T_ORDER_LE;
    dt->shared->u.atomic.offset = 0;
    dt->shared->u.atomic.prec = 32;
    dt->shared->u.atomic.lsb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.msb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.u.i.sign = H5T_SGN_NONE;
    if ((H5T_NATIVE_UINT_g = H5I_register (H5I_DATATYPE, dt))<0)
        HGOTO_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,"can't initialize type system (atom registration failure");
    H5T_NATIVE_UINT_ALIGN_g = 1;

   /*
    *    3        2        1        0
    * IIIIIIII IIIIIIII IIIIIIII IIIIIIII
    * Alignment: none
    */
    if (NULL==(dt = H5FL_CALLOC (H5T_t)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,"memory allocation failed");
    if (NULL==(dt->shared = H5FL_CALLOC(H5T_shared_t)))
    {         H5FL_FREE(H5T_t, dt);        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    }     dt->shared->state = H5T_STATE_IMMUTABLE;
    dt->ent.header = HADDR_UNDEF;
    dt->shared->type = H5T_INTEGER;
    dt->shared->size = 4;
    dt->shared->u.atomic.order = H5T_ORDER_LE;
    dt->shared->u.atomic.offset = 0;
    dt->shared->u.atomic.prec = 32;
    dt->shared->u.atomic.lsb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.msb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.u.i.sign = H5T_SGN_2;
    if ((H5T_NATIVE_LONG_g = H5I_register (H5I_DATATYPE, dt))<0)
        HGOTO_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,"can't initialize type system (atom registration failure");
    H5T_NATIVE_LONG_ALIGN_g = 1;
    H5T_NATIVE_LONG_COMP_ALIGN_g = 4;

   /*
    *    3        2        1        0
    * UUUUUUUU UUUUUUUU UUUUUUUU UUUUUUUU
    * Alignment: none
    */
    if (NULL==(dt = H5FL_CALLOC (H5T_t)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,"memory allocation failed");
    if (NULL==(dt->shared = H5FL_CALLOC(H5T_shared_t)))
    {         H5FL_FREE(H5T_t, dt);        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    }     dt->shared->state = H5T_STATE_IMMUTABLE;
    dt->ent.header = HADDR_UNDEF;
    dt->shared->type = H5T_INTEGER;
    dt->shared->size = 4;
    dt->shared->u.atomic.order = H5T_ORDER_LE;
    dt->shared->u.atomic.offset = 0;
    dt->shared->u.atomic.prec = 32;
    dt->shared->u.atomic.lsb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.msb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.u.i.sign = H5T_SGN_NONE;
    if ((H5T_NATIVE_ULONG_g = H5I_register (H5I_DATATYPE, dt))<0)
        HGOTO_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,"can't initialize type system (atom registration failure");
    H5T_NATIVE_ULONG_ALIGN_g = 1;

   /*
    *    0
    * IIIIIIII
    * Alignment: none
    */
    if (NULL==(dt = H5FL_CALLOC (H5T_t)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,"memory allocation failed");
    if (NULL==(dt->shared = H5FL_CALLOC(H5T_shared_t)))
    {         H5FL_FREE(H5T_t, dt);        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    }     dt->shared->state = H5T_STATE_IMMUTABLE;
    dt->ent.header = HADDR_UNDEF;
    dt->shared->type = H5T_INTEGER;
    dt->shared->size = 1;
    dt->shared->u.atomic.order = H5T_ORDER_LE;
    dt->shared->u.atomic.offset = 0;
    dt->shared->u.atomic.prec = 8;
    dt->shared->u.atomic.lsb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.msb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.u.i.sign = H5T_SGN_2;
    if ((H5T_NATIVE_INT8_g = H5I_register (H5I_DATATYPE, dt))<0)
        HGOTO_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,"can't initialize type system (atom registration failure");
    H5T_NATIVE_INT8_ALIGN_g = 1;

   /*
    *    0
    * UUUUUUUU
    * Alignment: none
    */
    if (NULL==(dt = H5FL_CALLOC (H5T_t)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,"memory allocation failed");
    if (NULL==(dt->shared = H5FL_CALLOC(H5T_shared_t)))
    {         H5FL_FREE(H5T_t, dt);        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    }     dt->shared->state = H5T_STATE_IMMUTABLE;
    dt->ent.header = HADDR_UNDEF;
    dt->shared->type = H5T_INTEGER;
    dt->shared->size = 1;
    dt->shared->u.atomic.order = H5T_ORDER_LE;
    dt->shared->u.atomic.offset = 0;
    dt->shared->u.atomic.prec = 8;
    dt->shared->u.atomic.lsb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.msb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.u.i.sign = H5T_SGN_NONE;
    if ((H5T_NATIVE_UINT8_g = H5I_register (H5I_DATATYPE, dt))<0)
        HGOTO_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,"can't initialize type system (atom registration failure");
    H5T_NATIVE_UINT8_ALIGN_g = 1;

   /*
    *    1        0
    * IIIIIIII IIIIIIII
    * Alignment: none
    */
    if (NULL==(dt = H5FL_CALLOC (H5T_t)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,"memory allocation failed");
    if (NULL==(dt->shared = H5FL_CALLOC(H5T_shared_t)))
    {         H5FL_FREE(H5T_t, dt);        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    }     dt->shared->state = H5T_STATE_IMMUTABLE;
    dt->ent.header = HADDR_UNDEF;
    dt->shared->type = H5T_INTEGER;
    dt->shared->size = 2;
    dt->shared->u.atomic.order = H5T_ORDER_LE;
    dt->shared->u.atomic.offset = 0;
    dt->shared->u.atomic.prec = 16;
    dt->shared->u.atomic.lsb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.msb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.u.i.sign = H5T_SGN_2;
    if ((H5T_NATIVE_INT16_g = H5I_register (H5I_DATATYPE, dt))<0)
        HGOTO_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,"can't initialize type system (atom registration failure");
    H5T_NATIVE_INT16_ALIGN_g = 1;

   /*
    *    1        0
    * UUUUUUUU UUUUUUUU
    * Alignment: none
    */
    if (NULL==(dt = H5FL_CALLOC (H5T_t)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,"memory allocation failed");
    if (NULL==(dt->shared = H5FL_CALLOC(H5T_shared_t)))
    {         H5FL_FREE(H5T_t, dt);        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    }     dt->shared->state = H5T_STATE_IMMUTABLE;
    dt->ent.header = HADDR_UNDEF;
    dt->shared->type = H5T_INTEGER;
    dt->shared->size = 2;
    dt->shared->u.atomic.order = H5T_ORDER_LE;
    dt->shared->u.atomic.offset = 0;
    dt->shared->u.atomic.prec = 16;
    dt->shared->u.atomic.lsb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.msb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.u.i.sign = H5T_SGN_NONE;
    if ((H5T_NATIVE_UINT16_g = H5I_register (H5I_DATATYPE, dt))<0)
        HGOTO_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,"can't initialize type system (atom registration failure");
    H5T_NATIVE_UINT16_ALIGN_g = 1;

   /*
    *    3        2        1        0
    * IIIIIIII IIIIIIII IIIIIIII IIIIIIII
    * Alignment: none
    */
    if (NULL==(dt = H5FL_CALLOC (H5T_t)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,"memory allocation failed");
    if (NULL==(dt->shared = H5FL_CALLOC(H5T_shared_t)))
    {         H5FL_FREE(H5T_t, dt);        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    }     dt->shared->state = H5T_STATE_IMMUTABLE;
    dt->ent.header = HADDR_UNDEF;
    dt->shared->type = H5T_INTEGER;
    dt->shared->size = 4;
    dt->shared->u.atomic.order = H5T_ORDER_LE;
    dt->shared->u.atomic.offset = 0;
    dt->shared->u.atomic.prec = 32;
    dt->shared->u.atomic.lsb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.msb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.u.i.sign = H5T_SGN_2;
    if ((H5T_NATIVE_INT32_g = H5I_register (H5I_DATATYPE, dt))<0)
        HGOTO_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,"can't initialize type system (atom registration failure");
    H5T_NATIVE_INT32_ALIGN_g = 1;

   /*
    *    3        2        1        0
    * UUUUUUUU UUUUUUUU UUUUUUUU UUUUUUUU
    * Alignment: none
    */
    if (NULL==(dt = H5FL_CALLOC (H5T_t)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,"memory allocation failed");
    if (NULL==(dt->shared = H5FL_CALLOC(H5T_shared_t)))
    {         H5FL_FREE(H5T_t, dt);        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    }     dt->shared->state = H5T_STATE_IMMUTABLE;
    dt->ent.header = HADDR_UNDEF;
    dt->shared->type = H5T_INTEGER;
    dt->shared->size = 4;
    dt->shared->u.atomic.order = H5T_ORDER_LE;
    dt->shared->u.atomic.offset = 0;
    dt->shared->u.atomic.prec = 32;
    dt->shared->u.atomic.lsb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.msb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.u.i.sign = H5T_SGN_NONE;
    if ((H5T_NATIVE_UINT32_g = H5I_register (H5I_DATATYPE, dt))<0)
        HGOTO_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,"can't initialize type system (atom registration failure");
    H5T_NATIVE_UINT32_ALIGN_g = 1;

   /*
    *    7        6        5        4
    * IIIIIIII IIIIIIII IIIIIIII IIIIIIII
    *    3        2        1        0
    * IIIIIIII IIIIIIII IIIIIIII IIIIIIII
    * Alignment: none
    */
    if (NULL==(dt = H5FL_CALLOC (H5T_t)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,"memory allocation failed");
    if (NULL==(dt->shared = H5FL_CALLOC(H5T_shared_t)))
    {         H5FL_FREE(H5T_t, dt);        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    }     dt->shared->state = H5T_STATE_IMMUTABLE;
    dt->ent.header = HADDR_UNDEF;
    dt->shared->type = H5T_INTEGER;
    dt->shared->size = 8;
    dt->shared->u.atomic.order = H5T_ORDER_LE;
    dt->shared->u.atomic.offset = 0;
    dt->shared->u.atomic.prec = 64;
    dt->shared->u.atomic.lsb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.msb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.u.i.sign = H5T_SGN_2;
    if ((H5T_NATIVE_INT64_g = H5I_register (H5I_DATATYPE, dt))<0)
        HGOTO_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,"can't initialize type system (atom registration failure");
    H5T_NATIVE_INT64_ALIGN_g = 1;

   /*
    *    7        6        5        4
    * UUUUUUUU UUUUUUUU UUUUUUUU UUUUUUUU
    *    3        2        1        0
    * UUUUUUUU UUUUUUUU UUUUUUUU UUUUUUUU
    * Alignment: none
    */
    if (NULL==(dt = H5FL_CALLOC (H5T_t)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,"memory allocation failed");
    if (NULL==(dt->shared = H5FL_CALLOC(H5T_shared_t)))
    {         H5FL_FREE(H5T_t, dt);        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    }     dt->shared->state = H5T_STATE_IMMUTABLE;
    dt->ent.header = HADDR_UNDEF;
    dt->shared->type = H5T_INTEGER;
    dt->shared->size = 8;
    dt->shared->u.atomic.order = H5T_ORDER_LE;
    dt->shared->u.atomic.offset = 0;
    dt->shared->u.atomic.prec = 64;
    dt->shared->u.atomic.lsb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.msb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.u.i.sign = H5T_SGN_NONE;
    if ((H5T_NATIVE_UINT64_g = H5I_register (H5I_DATATYPE, dt))<0)
        HGOTO_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,"can't initialize type system (atom registration failure");
    H5T_NATIVE_UINT64_ALIGN_g = 1;

   /*
    *    7        6        5        4
    * IIIIIIII IIIIIIII IIIIIIII IIIIIIII
    *    3        2        1        0
    * IIIIIIII IIIIIIII IIIIIIII IIIIIIII
    * Alignment: none
    */
    if (NULL==(dt = H5FL_CALLOC (H5T_t)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,"memory allocation failed");
    if (NULL==(dt->shared = H5FL_CALLOC(H5T_shared_t)))
    {         H5FL_FREE(H5T_t, dt);        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    }     dt->shared->state = H5T_STATE_IMMUTABLE;
    dt->ent.header = HADDR_UNDEF;
    dt->shared->type = H5T_INTEGER;
    dt->shared->size = 8;
    dt->shared->u.atomic.order = H5T_ORDER_LE;
    dt->shared->u.atomic.offset = 0;
    dt->shared->u.atomic.prec = 64;
    dt->shared->u.atomic.lsb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.msb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.u.i.sign = H5T_SGN_2;
    if ((H5T_NATIVE_LLONG_g = H5I_register (H5I_DATATYPE, dt))<0)
        HGOTO_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,"can't initialize type system (atom registration failure");
    H5T_NATIVE_LLONG_ALIGN_g = 1;
    H5T_NATIVE_LLONG_COMP_ALIGN_g = 8;

   /*
    *    7        6        5        4
    * UUUUUUUU UUUUUUUU UUUUUUUU UUUUUUUU
    *    3        2        1        0
    * UUUUUUUU UUUUUUUU UUUUUUUU UUUUUUUU
    * Alignment: none
    */
    if (NULL==(dt = H5FL_CALLOC (H5T_t)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,"memory allocation failed");
    if (NULL==(dt->shared = H5FL_CALLOC(H5T_shared_t)))
    {         H5FL_FREE(H5T_t, dt);        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    }     dt->shared->state = H5T_STATE_IMMUTABLE;
    dt->ent.header = HADDR_UNDEF;
    dt->shared->type = H5T_INTEGER;
    dt->shared->size = 8;
    dt->shared->u.atomic.order = H5T_ORDER_LE;
    dt->shared->u.atomic.offset = 0;
    dt->shared->u.atomic.prec = 64;
    dt->shared->u.atomic.lsb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.msb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.u.i.sign = H5T_SGN_NONE;
    if ((H5T_NATIVE_ULLONG_g = H5I_register (H5I_DATATYPE, dt))<0)
        HGOTO_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,"can't initialize type system (atom registration failure");
    H5T_NATIVE_ULLONG_ALIGN_g = 1;

   /*
    *    3        2        1        0
    * SEEEEEEE EMMMMMMM MMMMMMMM MMMMMMMM
    * Implicit bit? yes
    * Alignment: none
    */
    if (NULL==(dt = H5FL_CALLOC (H5T_t)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,"memory allocation failed");
    if (NULL==(dt->shared = H5FL_CALLOC(H5T_shared_t)))
    {         H5FL_FREE(H5T_t, dt);        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    }     dt->shared->state = H5T_STATE_IMMUTABLE;
    dt->ent.header = HADDR_UNDEF;
    dt->shared->type = H5T_FLOAT;
    dt->shared->size = 4;
    dt->shared->u.atomic.order = H5T_ORDER_LE;
    dt->shared->u.atomic.offset = 0;
    dt->shared->u.atomic.prec = 32;
    dt->shared->u.atomic.lsb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.msb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.u.f.sign = 31;
    dt->shared->u.atomic.u.f.epos = 23;
    dt->shared->u.atomic.u.f.esize = 8;
    dt->shared->u.atomic.u.f.ebias = 0x0000007f;
    dt->shared->u.atomic.u.f.mpos = 0;
    dt->shared->u.atomic.u.f.msize = 23;
    dt->shared->u.atomic.u.f.norm = H5T_NORM_IMPLIED;
    dt->shared->u.atomic.u.f.pad = H5T_PAD_ZERO;
    if ((H5T_NATIVE_FLOAT_g = H5I_register (H5I_DATATYPE, dt))<0)
        HGOTO_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,"can't initialize type system (atom registration failure");
    H5T_NATIVE_FLOAT_ALIGN_g = 1;
    H5T_NATIVE_FLOAT_COMP_ALIGN_g = 4;

   /*
    *    7        6        5        4
    * SEEEEEEE EEEEMMMM MMMMMMMM MMMMMMMM
    *    3        2        1        0
    * MMMMMMMM MMMMMMMM MMMMMMMM MMMMMMMM
    * Implicit bit? yes
    * Alignment: none
    */
    if (NULL==(dt = H5FL_CALLOC (H5T_t)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,"memory allocation failed");
    if (NULL==(dt->shared = H5FL_CALLOC(H5T_shared_t)))
    {         H5FL_FREE(H5T_t, dt);        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    }     dt->shared->state = H5T_STATE_IMMUTABLE;
    dt->ent.header = HADDR_UNDEF;
    dt->shared->type = H5T_FLOAT;
    dt->shared->size = 8;
    dt->shared->u.atomic.order = H5T_ORDER_LE;
    dt->shared->u.atomic.offset = 0;
    dt->shared->u.atomic.prec = 64;
    dt->shared->u.atomic.lsb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.msb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.u.f.sign = 63;
    dt->shared->u.atomic.u.f.epos = 52;
    dt->shared->u.atomic.u.f.esize = 11;
    dt->shared->u.atomic.u.f.ebias = 0x000003ff;
    dt->shared->u.atomic.u.f.mpos = 0;
    dt->shared->u.atomic.u.f.msize = 52;
    dt->shared->u.atomic.u.f.norm = H5T_NORM_IMPLIED;
    dt->shared->u.atomic.u.f.pad = H5T_PAD_ZERO;
    if ((H5T_NATIVE_DOUBLE_g = H5I_register (H5I_DATATYPE, dt))<0)
        HGOTO_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,"can't initialize type system (atom registration failure");
    H5T_NATIVE_DOUBLE_ALIGN_g = 1;
    H5T_NATIVE_DOUBLE_COMP_ALIGN_g = 8;

   /*
    *    7        6        5        4
    * SEEEEEEE EEEEMMMM MMMMMMMM MMMMMMMM
    *    3        2        1        0
    * MMMMMMMM MMMMMMMM MMMMMMMM MMMMMMMM
    * Implicit bit? yes
    * Alignment: none
    */
    if (NULL==(dt = H5FL_CALLOC (H5T_t)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,"memory allocation failed");
    if (NULL==(dt->shared = H5FL_CALLOC(H5T_shared_t)))
    {         H5FL_FREE(H5T_t, dt);        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    }     dt->shared->state = H5T_STATE_IMMUTABLE;
    dt->ent.header = HADDR_UNDEF;
    dt->shared->type = H5T_FLOAT;
    dt->shared->size = 8;
    dt->shared->u.atomic.order = H5T_ORDER_LE;
    dt->shared->u.atomic.offset = 0;
    dt->shared->u.atomic.prec = 64;
    dt->shared->u.atomic.lsb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.msb_pad = H5T_PAD_ZERO;
    dt->shared->u.atomic.u.f.sign = 63;
    dt->shared->u.atomic.u.f.epos = 52;
    dt->shared->u.atomic.u.f.esize = 11;
    dt->shared->u.atomic.u.f.ebias = 0x000003ff;
    dt->shared->u.atomic.u.f.mpos = 0;
    dt->shared->u.atomic.u.f.msize = 52;
    dt->shared->u.atomic.u.f.norm = H5T_NORM_IMPLIED;
    dt->shared->u.atomic.u.f.pad = H5T_PAD_ZERO;
    if ((H5T_NATIVE_LDOUBLE_g = H5I_register (H5I_DATATYPE, dt))<0)
        HGOTO_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,"can't initialize type system (atom registration failure");
    H5T_NATIVE_LDOUBLE_ALIGN_g = 1;
    H5T_NATIVE_LDOUBLE_COMP_ALIGN_g = 8;

    /* Set the native order for this machine */
    H5T_native_order_g = H5T_ORDER_LE;

    /* Structure alignment for pointers, hvl_t, hobj_ref_t, hdset_reg_ref_t */
    H5T_POINTER_COMP_ALIGN_g = 4;
    H5T_HVL_COMP_ALIGN_g = 4;
    H5T_HOBJREF_COMP_ALIGN_g = 8;
    H5T_HDSETREGREF_COMP_ALIGN_g = 1;

done:
    if(ret_value<0) {
        if(dt!=NULL)
            H5FL_FREE(H5T_t,dt);
    }

    FUNC_LEAVE_NOAPI(ret_value);
}
