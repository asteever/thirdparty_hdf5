/*keep this here -RPM*/
static const char *FileHeader = "\n\
/*-------------------------------------------------------------------------\n\
 * Copyright (C) 1997   National Center for Supercomputing Applications.   \n\
 *                      All rights reserved.                               \n\
 *                                                                         \n\
 *-------------------------------------------------------------------------";
/*
 *
 * Created:     H5detect.c
 *              10 Aug 1997
 *              Robb Matzke
 *
 * Purpose:     This code was borrowed heavily from the `detect.c'
 *              program in the AIO distribution from Lawrence
 *              Livermore National Laboratory.
 *
 *              Detects machine byte order and floating point
 *              format and generates a C source file (native.c)
 *              to describe those paramters.
 *
 * Assumptions: We have an ANSI compiler.  We're on a Unix like
 *              system or configure has detected those Unix
 *              features which aren't available.  We're not
 *              running on a Vax or other machine with mixed
 *              endianess.
 *              
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#include <assert.h>
#include <math.h>
#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#include <H5config.h>

#define MAXDETECT 16

#ifndef MIN
#  define MIN(X,Y) ((X)<(Y)?(X):(Y))
#  define MIN3(X,Y,Z) MIN(X,MIN(Y,Z))
#endif

/*
 * This structure holds information about a type that
 * was detected.
 */
typedef struct detected_t {
    const char          *varname;
    int                 size;		/*total byte size*/
    int			precision;	/*meaningful bits*/
    int			offset;		/*bit offset to meaningful bits*/
    int                 perm[32];
    int                 sign;
    int                 mpos, msize, imp;
    int                 epos, esize;
    unsigned long       bias;
} detected_t;

static void print_results(int nd, detected_t *d);
static void iprint(detected_t *);
static void print_known_formats(detected_t *);
static int byte_cmp(int, void *, void *);
static int bit_cmp(int, int *, void *, void *);
static void fix_order(int, int, int, int *, const char **);
static int imp_bit(int, int *, void *, void *);
static unsigned long find_bias(int, int, int *, void *);
static void precision (detected_t*);
static void print_header(void);


/*-------------------------------------------------------------------------
 * Function:	precision
 *
 * Purpose:	Determine the precision and offset.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Thursday, June 18, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
precision (detected_t *d)
{
    int		n;
    
    if (0==d->msize) {
	/*
	 * An integer.  The permutation can have negative values at the
	 * beginning or end which represent padding of bytes.  We must adjust
	 * the precision and offset accordingly.
	 */
	if (d->perm[0] < 0) {
	    /*
	     * Lower addresses are padded.
	     */
	    for (n=0; n<d->size && d->perm[n]<0; n++) /*void*/;
	    d->precision = 8*(d->size-n);
	    d->offset = 0;
	} else if (d->perm[d->size - 1] < 0) {
	    /*
	     * Higher addresses are padded.
	     */
	    for (n=0; n<d->size && d->perm[d->size-(n+1)]; n++) /*void*/;
	    d->precision = 8*(d->size-n);
	    d->offset = 8*n;
	} else {
	    /*
	     * No padding.
	     */
	    d->precision = 8*d->size;
	    d->offset = 0;
	}
    } else {
	/* A floating point */
	d->offset = MIN3 (d->mpos, d->epos, d->sign);
	d->precision = d->msize + d->esize + 1;
    }
}


/*-------------------------------------------------------------------------
 * For convenience, we place here in a table descriptions of all
 * architectures we've seen so far.  That way we can print a description
 * of the system on which the program is run.  We place the system name
 * in the VARNAME field.
 *-------------------------------------------------------------------------
 */
#define LE {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,                            \
     16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31}
#define LE_1 LE
#define LE_2 LE
#define LE_4 LE
#define LE_8 LE

#define BE_1 {0}
#define BE_2 {1,0}
#define BE_4 {3,2,1,0}
#define BE_8 {7,6,5,4,3,2,1,0}

#define INTEGER 0,0,0,0,0,0,0

static detected_t       Known[] =
{
   /* Single-byte quantities */
    {"Byte addressable",
     1, 8, 0, LE_1, INTEGER},

   /* Little-endian integer */
    {"Little-endian",
     2, 16, 0, LE_2, INTEGER},
    {"Little-endian",
     4, 32, 0, LE_4, INTEGER},

   /* Big-endian integer */
    {"Big-endian",
     2, 16, 0, BE_2, INTEGER},
    {"Big-endian",
     4, 32, 0, BE_4, INTEGER},

   /* Little-endian IEEE floating-point */
    {"Little-endian IEEE",
     4, 32, 0, LE_4, 31, 0, 23, 1, 23, 8, 127},
    {"Little-endian IEEE",
     8, 64, 0, LE_8, 63, 0, 52, 1, 52, 11, 1023},

   /* Big-endian IEEE floating-point */
    {"Big-endian IEEE",
     4, 32, 0, BE_4, 31, 0, 23, 1, 23, 8, 127},
    {"Big-endian IEEE",
     8, 64, 0, BE_8, 63, 0, 52, 1, 52, 11, 1023},
};

/*-------------------------------------------------------------------------
 * Function:    DETECT_I
 *
 * Purpose:     This macro takes a type like `int' and a base name like
 *              `nati' and detects the byte order.  The VAR is used to
 *              construct the names of the C variables defined.
 *
 * Return:      void
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jun 12 1996
 *
 * Modifications:
 *
 *      Robb Matzke, 4 Nov 1996
 *      The INFO.perm now contains `-1' for bytes that aren't used and
 *      are always zero.  This happens on the Cray for `short' where
 *      sizeof(short) is 8, but only the low-order 4 bytes are ever used.
 *
 *      Robb Matzke, 4 Nov 1996
 *      Added a `padding' field to indicate how many zero bytes appear to
 *      the left (N) or right (-N) of the value.
 *
 *      Robb Matzke, 5 Nov 1996
 *      Removed HFILE and CFILE arguments.
 *
 *-------------------------------------------------------------------------
 */
#define DETECT_I(TYPE,VAR,INFO) {                                             \
   TYPE _v;                                                                   \
   int _i, _j;                                                                \
   unsigned char *_x;                                                         \
   memset (&INFO, 0, sizeof(INFO));                                           \
   INFO.varname = #VAR;                                                       \
   INFO.size = sizeof(TYPE);                                                  \
   for (_i=sizeof(TYPE),_v=0; _i>0; --_i) _v = (_v<<8) + _i;                  \
   for (_i=0,_x=(unsigned char *)&_v; _i<(signed)sizeof(TYPE); _i++) {        \
      _j = (*_x++)-1;                                                         \
      assert (_j<(signed)sizeof(TYPE));                                       \
      INFO.perm[_i] = _j;                                                     \
   }                                                                          \
   INFO.sign = ('U'!=*(#VAR));                                                \
   precision (&(INFO));							      \
}

/*-------------------------------------------------------------------------
 * Function:    DETECT_F
 *
 * Purpose:     This macro takes a floating point type like `double' and
 *              a base name like `natd' and detects byte order, mantissa
 *              location, exponent location, sign bit location, presence or
 *              absence of implicit mantissa bit, and exponent bias and
 *              initializes a detected_t structure with those properties.
 *
 * Return:      void
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jun 12 1996
 *
 * Modifications:
 *
 *      Robb Matzke, 14 Aug 1996
 *      The byte order detection has been changed because on the Cray
 *      the last pass causes a rounding to occur that causes the least
 *      significant mantissa byte to change unexpectedly.
 *
 *      Robb Matzke, 5 Nov 1996
 *      Removed HFILE and CFILE arguments.
 *-------------------------------------------------------------------------
 */
#define DETECT_F(TYPE,VAR,INFO) {                                             \
   TYPE _v1, _v2, _v3;                                                        \
   int _i, _j, _first=(-1), _last=(-1);                                       \
   char *_mesg;                                                               \
                                                                              \
   memset (&INFO, 0, sizeof(INFO));                                           \
   INFO.varname = #VAR;                                                       \
   INFO.size = sizeof(TYPE);                                                  \
                                                                              \
   /* Byte Order */                                                           \
   for (_i=0,_v1=0.0,_v2=1.0; _i<(signed)sizeof(TYPE); _i++) {                \
      _v3 = _v1; _v1 += _v2; _v2 /= 256.0;                                    \
      if ((_j=byte_cmp(sizeof(TYPE), &_v3, &_v1))>=0) {                       \
         if (0==_i || INFO.perm[_i-1]!=_j) {                                  \
            INFO.perm[_i] = _j;                                               \
            _last = _i;                                                       \
            if (_first<0) _first = _i;                                        \
         }                                                                    \
      }                                                                       \
   }                                                                          \
   fix_order (sizeof(TYPE), _first, _last, INFO.perm, (const char**)&_mesg);  \
                                                                              \
   /* Implicit mantissa bit */                                                \
   _v1 = 0.5;                                                                 \
   _v2 = 1.0;                                                                 \
   INFO.imp = imp_bit (sizeof(TYPE), INFO.perm, &_v1, &_v2);                  \
                                                                              \
   /* Sign bit */                                                             \
   _v1 = 1.0;                                                                 \
   _v2 = -1.0;                                                                \
   INFO.sign = bit_cmp (sizeof(TYPE), INFO.perm, &_v1, &_v2);                 \
                                                                              \
   /* Mantissa */                                                             \
   INFO.mpos = 0;                                                             \
                                                                              \
   _v1 = 1.0;                                                                 \
   _v2 = 1.5;                                                                 \
   INFO.msize = bit_cmp (sizeof(TYPE), INFO.perm, &_v1, &_v2);                \
   INFO.msize += 1 + (INFO.imp?0:1) - INFO.mpos;                              \
                                                                              \
   /* Exponent */                                                             \
   INFO.epos = INFO.mpos + INFO.msize;                                        \
                                                                              \
   INFO.esize = INFO.sign - INFO.epos;                                        \
                                                                              \
   _v1 = 1.0;                                                                 \
   INFO.bias = find_bias (INFO.epos, INFO.esize, INFO.perm, &_v1);            \
   precision (&(INFO));							      \
}

/*-------------------------------------------------------------------------
 * Function:    print_results
 *
 * Purpose:     Prints information about the detected data types.
 *
 * Return:      void
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jun 14, 1996
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
print_results(int nd, detected_t *d)
{

    int         i;

    /* Include files */
    printf("\
#define H5T_PACKAGE /*suppress error about including H5Tpkg.h*/\n\
\n\
#include <H5private.h>\n\
#include <H5Iprivate.h>\n\
#include <H5Eprivate.h>\n\
#include <H5MMprivate.h>\n\
#include <H5Tpkg.h>\n\
\n\
static hbool_t interface_initialize_g = FALSE;\n\
#define INTERFACE_INIT NULL\n\
\n");

    /* Function declaration */
    printf("\n\
herr_t\n\
H5T_init (void)\n\
{\n\
   H5T_t        *dt = NULL;\n\
   static intn  ncalls = 0;\n\
\n\
   FUNC_ENTER (H5T_init, FAIL);\n\
\n\
   if (ncalls++) return SUCCEED; /*already initialized*/\n\
\n");

    for (i = 0; i < nd; i++) {

        /* Print a comment to describe this section of definitions. */
        printf("\n   /*\n");
        iprint(d + i);
        print_known_formats(d + i);
        printf("    */\n");

        /* The part common to fixed and floating types */
        printf("\
   if (NULL==(dt = H5MM_calloc (sizeof(H5T_t)))) {\n\
      HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,\n\
                     \"memory allocation failed\");\n\
   }\n\
   dt->state = H5T_STATE_IMMUTABLE;\n\
   H5F_addr_undef (&(dt->ent.header));\n\
   dt->type = H5T_%s;\n\
   dt->size = %d;\n\
   dt->u.atomic.order = H5T_ORDER_%s;\n\
   dt->u.atomic.offset = %d;\n\
   dt->u.atomic.prec = %d;\n\
   dt->u.atomic.lsb_pad = H5T_PAD_ZERO;\n\
   dt->u.atomic.msb_pad = H5T_PAD_ZERO;\n",
               d[i].msize ? "FLOAT" : "INTEGER",/*class 	        */
               d[i].size,   			/*size          	*/
               d[i].perm[0] ? "BE" : "LE",      /*byte order    	*/
	       d[i].offset, 			/*offset		*/
               d[i].precision);  		/*precision     	*/

        if (0 == d[i].msize) {
            /* The part unique to fixed point types */
            printf("\
   dt->u.atomic.u.i.sign = H5T_SGN_%s;\n",
                   d[i].sign ? "2" : "NONE");
        } else {
            /* The part unique to floating point types */
            printf("\
   dt->u.atomic.u.f.sign = %d;\n\
   dt->u.atomic.u.f.epos = %d;\n\
   dt->u.atomic.u.f.esize = %d;\n\
   dt->u.atomic.u.f.ebias = 0x%08lx;\n\
   dt->u.atomic.u.f.mpos = %d;\n\
   dt->u.atomic.u.f.msize = %d;\n\
   dt->u.atomic.u.f.norm = H5T_NORM_%s;\n\
   dt->u.atomic.u.f.pad = H5T_PAD_ZERO;\n",
                   d[i].sign,   /*sign location */
                   d[i].epos,   /*exponent loc  */
                   d[i].esize,  /*exponent size */
                   (unsigned long)(d[i].bias),   /*exponent bias */
                   d[i].mpos,   /*mantissa loc  */
                   d[i].msize,  /*mantissa size */
                   d[i].imp ? "IMPLIED" : "NONE");      /*normalization */
        }

        /* Atomize the type */
        printf("\
   if ((H5T_NATIVE_%s_g = H5I_register (H5_DATATYPE, dt))<0) {\n\
      HRETURN_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,\n\
                     \"can't initialize type system (atom registration \"\n\
                     \"failure\");\n\
   }\n",
               d[i].varname);
    }

    printf("   FUNC_LEAVE (SUCCEED);\n}\n");
}


/*-------------------------------------------------------------------------
 * Function:    iprint
 *
 * Purpose:     Prints information about the fields of a floating point
 *              format.
 *
 * Return:      void
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jun 13, 1996
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
iprint(detected_t *d)
{
    int         i, j, k, pass;

    for (pass=(d->size-1)/4; pass>=0; --pass) {
	/*
	 * Print the byte ordering above the bit fields.
	 */
	printf("    * ");
        for (i=MIN(pass*4+3,d->size-1); i>=pass*4; --i) {
            printf ("%4d", d->perm[i]);
            if (i>pass*4) fputs ("     ", stdout);
        }

	/*
	 * Print the bit fields
	 */
	printf("\n    * ");
        for (i=MIN(pass*4+3,d->size-1),
             k=MIN(pass*32+31,8*d->size-1);
             i>=pass*4; --i) {
            for (j=7; j>=0; --j) {
		if (k==d->sign && d->msize) {
		    putchar('S');
		} else if (k>=d->epos && k<d->epos+d->esize) {
		    putchar('E');
		} else if (k>=d->mpos && k<d->mpos+d->msize) {
		    putchar('M');
		} else if (d->msize) {
		    putchar('?');   /*unknown floating point bit */
		} else if (d->sign) {
		    putchar('I');
		} else {
		    putchar('U');
		}
		--k;
	    }
	    if (i>pass*4) putchar(' ');
	}
	putchar('\n');
    }

    /*
     * Is there an implicit bit in the mantissa.
     */
    if (d->msize) {
        printf("    * Implicit bit? %s\n", d->imp ? "yes" : "no");
    }
}


/*-------------------------------------------------------------------------
 * Function:    print_known_formats
 *
 * Purpose:     Prints archetecture names for the specified format
 *              description, if any.
 *
 * Return:      void
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jun 13, 1996
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
print_known_formats(detected_t *d)
{

    int         i, j, diff;
    int         n = sizeof(Known) / sizeof(Known[0]);

    for (i=0; i<n; i++) {
        if (d->size != Known[i].size) continue;
	if (d->precision != Known[i].precision) continue;
	if (d->offset != Known[i].offset) continue;
        for (j = diff = 0; !diff && j < d->size; j++) {
            if (d->perm[j] != Known[i].perm[j]) diff = 1;
        }
        if (diff) continue;

        /* if (d->sign  != Known[i].sign)  continue; */
        if (d->mpos != Known[i].mpos) continue;
        if (d->msize != Known[i].msize) continue;
        if (d->imp != Known[i].imp) continue;
        if (d->epos != Known[i].epos) continue;
        if (d->esize != Known[i].esize) continue;
        if (d->bias != Known[i].bias) continue;

        printf("    * %s\n", Known[i].varname);
    }
}


/*-------------------------------------------------------------------------
 * Function:    byte_cmp
 *
 * Purpose:     Compares two chunks of memory A and B and returns the
 *              byte index into those arrays of the first byte that
 *              differs between A and B.
 *
 * Return:      Success:        Index of differing byte.
 *
 *              Failure:        -1 if all bytes are the same.
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jun 12, 1996
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
byte_cmp(int n, void *_a, void *_b)
{
    register int        i;
    unsigned char       *a = (unsigned char *) _a;
    unsigned char       *b = (unsigned char *) _b;

    for (i = 0; i < n; i++) if (a[i] != b[i]) return i;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    bit_cmp
 *
 * Purpose:     Compares two bit vectors and returns the index for the
 *              first bit that differs between the two vectors.  The
 *              size of the vector is NBYTES.  PERM is a mapping from
 *              actual order to little endian.
 *
 * Return:      Success:        Index of first differing bit.
 *
 *              Failure:        -1
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jun 13, 1996
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
bit_cmp(int nbytes, int *perm, void *_a, void *_b)
{
    int                 i, j;
    unsigned char       *a = (unsigned char *) _a;
    unsigned char       *b = (unsigned char *) _b;
    unsigned char       aa, bb;

    for (i = 0; i < nbytes; i++) {
        assert(perm[i] < nbytes);
        if ((aa = a[perm[i]]) != (bb = b[perm[i]])) {
            for (j = 0; j < 8; j++, aa >>= 1, bb >>= 1) {
                if ((aa & 1) != (bb & 1)) return i * 8 + j;
            }
            assert("INTERNAL ERROR" && 0);
        }
    }
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    fix_order
 *
 * Purpose:     Given an array PERM with elements FIRST through LAST
 *              initialized with zero origin byte numbers, this function
 *              creates a permutation vector that maps the actual order
 *              of a floating point number to little-endian.
 *
 *              This function assumes that the mantissa byte ordering
 *              implies the total ordering.
 *
 * Return:      void
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jun 13, 1996
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
fix_order(int n, int first, int last, int *perm, const char **mesg)
{
    int         i;

    if (first + 1 < last) {
        /*
         * We have at least three points to consider.
         */
        if (perm[last] < perm[last - 1] && perm[last - 1] < perm[last - 2]) {
            /*
             * Little endian.
             */
            if (mesg) *mesg = "Little-endian";
            for (i = 0; i < n; i++) perm[i] = i;

        } else if (perm[last] > perm[last-1] && perm[last-1] > perm[last-2]) {
            /*
             * Big endian.
             */
            if (mesg) *mesg = "Big-endian";
            for (i = 0; i < n; i++) perm[i] = (n - 1) - i;

        } else {
            /*
             * Bi-endian machines like VAX.
             */
            assert(0 == n / 2);
            if (mesg) *mesg = "VAX";
            for (i = 0; i < n; i += 2) {
                perm[i] = (n - 2) - i;
                perm[i + 1] = (n - 1) - i;
            }
        }
    } else {
        fprintf(stderr,
             "Failed to detect byte order of %d-byte floating point.\n", n);
        exit(1);
    }
}


/*-------------------------------------------------------------------------
 * Function:    imp_bit
 *
 * Purpose:     Looks for an implicit bit in the mantissa.  The value
 *              of _A should be 1.0 and the value of _B should be 0.5.
 *              Some floating-point formats discard the most significant
 *              bit of the mantissa after normalizing since it will always
 *              be a one (except for 0.0).  If this is true for the native
 *              floating point values stored in _A and _B then the function
 *              returns non-zero.
 *
 *              This function assumes that the exponent occupies higher
 *              order bits than the mantissa and that the most significant
 *              bit of the mantissa is next to the least signficant bit
 *              of the exponent.
 *              
 *
 * Return:      Success:        Non-zero if the most significant bit
 *                              of the mantissa is discarded (ie, the
 *                              mantissa has an implicit `one' as the
 *                              most significant bit).  Otherwise,
 *                              returns zero.
 *
 *              Failure:        exit(1)
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jun 13, 1996
 *
 * Modifications:
 *
 *      Robb Matzke, 6 Nov 1996
 *      Fixed a bug that occurs with non-implicit architectures.
 *
 *-------------------------------------------------------------------------
 */
static int
imp_bit(int n, int *perm, void *_a, void *_b)
{
    unsigned char       *a = (unsigned char *) _a;
    unsigned char       *b = (unsigned char *) _b;
    int                 changed, major, minor;
    int                 msmb;   /*most significant mantissa bit */

    /*
     * Look for the least significant bit that has changed between
     * A and B.  This is the least significant bit of the exponent.
     */
    changed = bit_cmp(n, perm, a, b);
    assert(changed >= 0);

    /*
     * The bit to the right (less significant) of the changed bit should
     * be the most significant bit of the mantissa.  If it is non-zero
     * then the format does not remove the leading `1' of the mantissa.
     */
    msmb = changed - 1;
    major = msmb / 8;
    minor = msmb % 8;

    return (a[perm[major]] >> minor) & 0x01 ? 0 : 1;
}


/*-------------------------------------------------------------------------
 * Function:    find_bias
 *
 * Purpose:     Determines the bias of the exponent.  This function should
 *              be called with _A having a value of `1'.
 *
 * Return:      Success:        The exponent bias.
 *
 *              Failure:        
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jun 13, 1996
 *
 * Modifications:
 *
 *      Robb Matzke, 6 Nov 1996
 *      Fixed a bug with non-implicit architectures returning the
 *      wrong exponent bias.
 *
 *-------------------------------------------------------------------------
 */
static unsigned long
find_bias(int epos, int esize, int *perm, void *_a)
{
    unsigned char       *a = (unsigned char *) _a;
    unsigned char       mask;
    unsigned long       b, shift = 0, nbits, bias = 0;

    while (esize > 0) {
        nbits = MIN(esize, (8 - epos % 8));
        mask = (1 << nbits) - 1;
        b = (a[perm[epos / 8]] >> (epos % 8)) & mask;
        bias |= b << shift;

        shift += nbits;
        esize -= nbits;
        epos += nbits;
    }
    return bias;
}


/*-------------------------------------------------------------------------
 * Function:    print_header
 *
 * Purpose:     Prints the C file header for the generated file.
 *
 * Return:      void
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Mar 12 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
print_header(void)
{

    time_t              now = time(NULL);
    struct tm           *tm = localtime(&now);
    struct passwd       *pwd = NULL;
    char                real_name[30];
    char                host_name[256];
    int                 i;
    const char          *s;
    static const char   *month_name[] =
    {
        "Jan", "Feb", "Mar", "Apr", "May", "Jun",
        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
    static const char   *purpose = "\
This machine-generated source code contains\n\
information about the various integer and\n\
floating point numeric formats found on this\n\
architecture.  The parameters below should be\n\
checked carefully and errors reported to the\n\
HDF5 maintainer.\n\
\n\
Each of the numeric formats listed below are\n\
printed from most significant bit to least\n\
significant bit even though the actual bytes\n\
might be stored in a different order in\n\
memory.  The integers above each binary byte\n\
indicate the relative order of the bytes in\n\
memory; little-endian machines have\n\
decreasing numbers while big-endian machines\n\
have increasing numbers.\n\
\n\
The fields of the numbers are printed as\n\
letters with `S' for the mantissa sign bit,\n\
`M' for the mantissa magnitude, and `E' for\n\
the exponent.  The exponent has an associated\n\
bias which can be subtracted to find the\n\
true exponent.  The radix point is assumed\n\
to be before the first `M' bit.  Any bit\n\
of a floating-point value not falling into one\n\
of these categories is printed as a question\n\
mark.  Bits of integer types are printed as\n\
`I' for 2's complement and `U' for magnitude.\n\
\n\
If the most significant bit of the normalized\n\
mantissa (always a `1' except for `0.0') is\n\
not stored then an `implicit=yes' appears\n\
under the field description.  In thie case,\n\
the radix point is still assumed to be\n\
before the first `M' but after the implicit\n\
bit.\n";

    /*
     * The real name is the first item from the passwd gecos field.
     */
#ifdef HAVE_GETPWUID
    {
        size_t n;
        char *comma;
        if ((pwd = getpwuid(getuid()))) {
            if ((comma = strchr(pwd->pw_gecos, ','))) {
                n = MIN(sizeof(real_name)-1, (unsigned)(comma-pwd->pw_gecos));
                strncpy(real_name, pwd->pw_gecos, n);
                real_name[n] = '\0';
            } else {
                strncpy(real_name, pwd->pw_gecos, sizeof(real_name));
                real_name[sizeof(real_name) - 1] = '\0';
            }
        } else {
            real_name[0] = '\0';
        }
    }
#else
    real_name[0] = '\0';
#endif

    /*
     * The FQDM of this host or the empty string.
     */
#ifdef HAVE_GETHOSTNAME
    if (gethostname(host_name, sizeof(host_name)) < 0) {
        host_name[0] = '\0';
    }
#else
    host_name[0] = '\0';
#endif
    
    /*
     * The file header: warning, copyright notice, build information.
     */
    printf("/*\n * DO NOT EDIT OR DISTRIBUTE THIS FILE -- "
           "IT IS MACHINE GENERATED!\n */\n\n");
    puts(FileHeader);           /*the copyright notice--see top of this file */

    printf(" *\n * Created:\t\t%s %2d, %4d\n",
           month_name[tm->tm_mon], tm->tm_mday, 1900 + tm->tm_year);
    if (pwd || real_name[0] || host_name[0]) {
        printf(" *\t\t\t");
        if (real_name[0]) printf("%s <", real_name);
        if (pwd) fputs(pwd->pw_name, stdout);
        if (host_name[0]) printf("@%s", host_name);
        if (real_name[0]) printf(">");
        putchar('\n');
    }
    printf(" *\n * Purpose:\t\t");
    for (s = purpose; *s; s++) {
        putchar(*s);
        if ('\n' == *s && s[1]) printf(" *\t\t\t");
    }

    printf(" *\n * Modifications:\n *\n");
    printf(" *\tDO NOT MAKE MODIFICATIONS TO THIS FILE!\n");
    printf(" *\tIt was generated by code in `H5detect.c'.\n");

    printf(" *\n *");
    for (i = 0; i < 73; i++) putchar('-');
    printf("\n */\n\n");

}


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Main entry point.
 *
 * Return:      Success:        exit(0)
 *
 *              Failure:        exit(1)
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jun 12, 1996
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    detected_t          d[MAXDETECT];
    int                 nd = 0;

    print_header();

    DETECT_I (signed char,        CHAR,    d[nd]); nd++;
    DETECT_I (unsigned char,      UCHAR,   d[nd]); nd++;
    DETECT_I (short,              SHORT,   d[nd]); nd++;
    DETECT_I (unsigned short,     USHORT,  d[nd]); nd++;
    DETECT_I (int,                INT,     d[nd]); nd++;
    DETECT_I (unsigned int,       UINT,    d[nd]); nd++;
    DETECT_I (long,               LONG,    d[nd]); nd++;
    DETECT_I (unsigned long,      ULONG,   d[nd]); nd++;
    DETECT_I (long long,          LLONG,   d[nd]); nd++;
    DETECT_I (unsigned long long, ULLONG,  d[nd]); nd++;
    DETECT_F (float,              FLOAT,   d[nd]); nd++;
    DETECT_F (double,             DOUBLE,  d[nd]); nd++;
    DETECT_F (long double,        LDOUBLE, d[nd]); nd++;

    print_results (nd, d);
    return 0;
}
