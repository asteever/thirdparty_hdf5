/****p* Program/H5match_types
 *
 * NAME
 *  Executable: H5match_types
 *
 * FILE
 *  fortran/src/H5match_types.c
 *
 * PURPOSE
 *  C Program to match C types to Fortran types.
 *  Creates the files H5f90i_gen.h for the C code and
 *  H5fortran_types.F90 for the Fortran code.
 *
 * COPYRIGHT
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 ******
*/

#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "H5public.h"
/* Include H5Ipublic.h for hid_t type */
#include "H5Ipublic.h"

/* Definitions of which fortran type sizes exist */
#include "H5fort_type_defines.h"

/* File pointers for files */
FILE * c_header;
FILE * fort_header;

#define CFILE "H5f90i_gen.h"
#define FFILE "H5fortran_types.F90"

/* Prototypes for the write routines */
void writeTypedef(const char* c_typedef, const char* c_type, int size);
void writeTypedefDefault(const char* c_typedef, int size);
void writeToFiles(const char* c_typedef, const char* fortran_type, const char* c_type, int size, int kind);
void writeToFilesChr(const char* c_typedef, const char* fortran_type, const char* c_type, int size, char* kind);

static void
initCfile(void)
{
  fprintf(c_header,
 "/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n\
 * Copyright by The HDF Group.                                               *\n\
 * Copyright by the Board of Trustees of the University of Illinois.         *\n\
 * All rights reserved.                                                      *\n\
 *                                                                           *\n\
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *\n\
 * terms governing use, modification, and redistribution, is contained in    *\n\
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *\n\
 * of the source code distribution tree; Copyright.html can be found at the  *\n\
 * root level of an installed copy of the electronic HDF5 document set and   *\n\
 * is linked from the top-level documents page.  It can also be found at     *\n\
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *\n\
 * access to either file, you may request a copy from help@hdfgroup.org.     *\n\
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */\n\
\n\n\
#ifndef _H5f90i_gen_H\n\
#define _H5f90i_gen_H\n\
\n\
/* This file is automatically generated by H5match_types.c at build time. */\n\
\n\
#include \"H5public.h\"\n\n");
}

static void
initFfile(void)
{
  fprintf(fort_header,
    "! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * \n\
!   Copyright by The HDF Group.                                               *\n\
!   Copyright by the Board of Trustees of the University of Illinois.         *\n\
!   All rights reserved.                                                      *\n\
!                                                                             *\n\
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *\n\
!   terms governing use, modification, and redistribution, is contained in    *\n\
!   the files COPYING and Copyright.html.  COPYING can be found at the root   *\n\
!   of the source code distribution tree; Copyright.html can be found at the  *\n\
!   root level of an installed copy of the electronic HDF5 document set and   *\n\
!   is linked from the top-level documents page.  It can also be found at     *\n\
!   http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *\n\
!   access to either file, you may request a copy from help@hdfgroup.org.     *\n\
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n\
!\n!\n\
! This file is automatically generated and contains HDF5 Fortran90 type definitions.\n!\n\
       MODULE H5FORTRAN_TYPES\n\
         USE ISO_C_BINDING\n\
         !\n\
         !  HDF5 integers\n\
         !\n");

}

static void
endCfile(void)
{
  fprintf(c_header, "\n#endif /* _H5f90i_gen_H */\n");
}
static void
endFfile(void)
{
  fprintf(fort_header, "\n        INTEGER(SIZE_T), PARAMETER :: OBJECT_NAMELEN_DEFAULT_F = -1\n\n");
  fprintf(fort_header, "        END MODULE H5FORTRAN_TYPES\n");
}

/* Define a c_int_x type in the C header */
void writeTypedef(const char* c_typedef, const char* c_type, int size)
{
  fprintf(c_header, "#define c_%s_%u %s\n", c_typedef, size, c_type);
}

/* Call this function if there is no matching C type for sizes > 1 */
void writeTypedefDefault(const char* c_typedef, int size)
{
  assert(size %2 == 0);
  fprintf(c_header, "typedef struct {c_%s_%u a; c_%s_%u b;} c_%s_%u;\n", c_typedef, size / 2, c_typedef, size / 2, c_typedef, size);
}

/* Create matching Fortran and C types by writing to both files */
void writeToFiles(const char* c_typedef, const char* fortran_type, const char* c_type, int size,  int kind)
{
  fprintf(fort_header, "        INTEGER, PARAMETER :: %s = %u\n", fortran_type, kind);
  fprintf(c_header, "typedef c_%s_%d %s;\n", c_typedef, size, c_type);
}
void writeToFilesChr(const char* c_typedef, const char* fortran_type, const char* c_type, int size, char* kind)
{
  fprintf(fort_header, "        INTEGER, PARAMETER :: %s = %s\n", fortran_type, kind);
  fprintf(c_header, "typedef c_%s_%d %s;\n", c_typedef, size, c_type);
}
int main(void)
{
  int FoundIntSize[10];
  int FoundIntSizeKind[10];
  int FoundRealSize[10];
  int FoundRealSizeKind[10];
  int i, j,flag;
  char chrA[32],chrB[32];
  int H5_C_HAS_REAL_NATIVE_16;

  int IntKinds[] = H5_FORTRAN_INTEGER_KINDS;
  int IntKinds_SizeOf[] = H5_FORTRAN_INTEGER_KINDS_SIZEOF;
  int RealKinds[] = H5_FORTRAN_REAL_KINDS;
  int RealKinds_SizeOf[] = H5_FORTRAN_REAL_KINDS_SIZEOF;
  char Real_C_TYPES[10][32];
  
  int H5_FORTRAN_NUM_INTEGER_KINDS;
  int H5_FORTRAN_NUM_REAL_KINDS;

  /* Open target files */
  c_header = fopen(CFILE, "w");
  fort_header = fopen(FFILE, "w");

  /* Default is C has 16 byte float */
  H5_C_HAS_REAL_NATIVE_16 = 1;

  /* Write copyright, boilerplate to both files */
  initCfile();
  initFfile();

  /* (a) define c_int_x */

  H5_FORTRAN_NUM_INTEGER_KINDS = (int)(sizeof(IntKinds)/sizeof(IntKinds[0]));
  H5_FORTRAN_NUM_REAL_KINDS = (int)(sizeof(RealKinds)/sizeof(RealKinds[0]));

  for(i=0;i< H5_FORTRAN_NUM_INTEGER_KINDS;i++) {
    if(sizeof(long long) == IntKinds_SizeOf[i])
      writeTypedef("int", "long long", IntKinds[i]);
    else if(sizeof(long) == IntKinds[i])
      writeTypedef("int", "long", IntKinds[i]);
    else if(sizeof(int) == IntKinds_SizeOf[i])
      writeTypedef("int", "int", IntKinds[i]);
    else if(sizeof(short) == IntKinds_SizeOf[i])
      writeTypedef("int", "short", IntKinds[i]);
    else
      if(IntKinds_SizeOf[i] == 1) {
	writeTypedef("int", "char", IntKinds[i]);
	/* Actually, char is not necessarily one byte.
	 * But if char isn't, then nothing is, so this
	 * is as close as we can get. */
      } else {
	writeTypedefDefault("int",IntKinds[i]);
      }
    if(sizeof(size_t) == IntKinds_SizeOf[i])
      writeTypedef("size_t", "size_t", IntKinds[i]);
    if(sizeof(hsize_t) == IntKinds_SizeOf[i])
      writeTypedef("hsize_t", "hsize_t", IntKinds[i]);
  }

  /* (b) Define c_float_x */

  int found_long_double = 0;
  for(i=0;i< H5_FORTRAN_NUM_REAL_KINDS;i++) {

    if (sizeof(float) == RealKinds_SizeOf[i]) {
      writeTypedef("float", "float", RealKinds[i]);
      strcpy(Real_C_TYPES[i], "C_FLOAT");
    } else if(sizeof(double) == RealKinds_SizeOf[i]) {
      writeTypedef("float", "double", RealKinds[i]);
      strcpy(Real_C_TYPES[i], "C_DOUBLE");
/* may not have long double in fortran need check */ 
    } else if(sizeof(long double) == RealKinds_SizeOf[i] && found_long_double == 0) {
      writeTypedef("float", "long double", RealKinds[i]);
      strcpy(Real_C_TYPES[i], "C_LONG_DOUBLE");
      found_long_double = 1;
    }
#ifdef H5_HAVE_FLOAT128
    /* Don't select a higher precision than Fortran can support */
    else if(sizeof(__float128) == RealKinds_SizeOf[i] && found_long_double == 1 && H5_PAC_FC_MAX_REAL_PRECISION > 28) {
      writeTypedef("float", "__float128", RealKinds[i]);
      strcpy(Real_C_TYPES[i], "C_FLOAT128");
    } 
#endif
/*     else { */
  /*     /\* Did not find the real type, use the next smallest *\/ */
/*       sprintf(chrA, "%d", RealKinds[i]); */
/*       if(sizeof(float) > RealKinds_SizeOf[H5_FORTRAN_NUM_REAL_KINDS-2]) { */
/* 	writeTypedef("float", "float", RealKinds[i]); */
/* 	strcpy(Real_C_TYPES[i], chrA); */
/*       } else if(sizeof(double) > RealKinds_SizeOf[H5_FORTRAN_NUM_REAL_KINDS-2]) { */
/* 	writeTypedef("float", "double", RealKinds[i]); */
/* 	strcpy(Real_C_TYPES[i], chrA); */
/*       } else if(sizeof(long double) > RealKinds_SizeOf[H5_FORTRAN_NUM_REAL_KINDS-2]) { */
/* 	writeTypedef("float", "long double", RealKinds[i]); */
/* 	strcpy(Real_C_TYPES[i], "C_LONG_DOUBLE"); } */
      else {
	printf("                      **** HDF5 WARNING ****\n");
	printf("Fortran REAL is %d bytes, but no corresponding C floating type exists\n",RealKinds_SizeOf[i]);
	printf("Fortran Interface will create a custom datatype to store Fortran Real\n",RealKinds_SizeOf[i]);

        writeTypedef("float", "long double", RealKinds[i]);
	strcpy(Real_C_TYPES[i], "C_LONG_DOUBLE"); }
      }
/*   } */

  /* Now begin defining fortran types. */
  fprintf(c_header, "\n");

  /* haddr_t */
  for(i=0;i< H5_FORTRAN_NUM_INTEGER_KINDS;i++) {
    if(IntKinds_SizeOf[i] == H5_SIZEOF_HADDR_T) {
      writeToFiles("int","HADDR_T", "haddr_t_f", H5_SIZEOF_HADDR_T, IntKinds[i]);
      break;
    }
    if(i == (H5_FORTRAN_NUM_INTEGER_KINDS-1) )
      /* Error: couldn't find a size for haddr_t */
      return -1;
  }

  /* hsize_t */
  for(i=0;i< H5_FORTRAN_NUM_INTEGER_KINDS;i++) {
    if(IntKinds_SizeOf[i] == H5_SIZEOF_HSIZE_T) {
      writeToFiles("hsize_t","HSIZE_T", "hsize_t_f", H5_SIZEOF_HSIZE_T, IntKinds[i]);
      break;
    }
    if(i == (H5_FORTRAN_NUM_INTEGER_KINDS-1) )
      /* Error: couldn't find a size for hsize_t */
      return -1;
  }

  /* hssize_t */
  for(i=0;i< H5_FORTRAN_NUM_INTEGER_KINDS;i++) {
    if(IntKinds_SizeOf[i] == H5_SIZEOF_HSSIZE_T) {
      writeToFiles("int","HSSIZE_T", "hssize_t_f", H5_SIZEOF_HSSIZE_T, IntKinds[i]);
      break;
    }
    if(i == (H5_FORTRAN_NUM_INTEGER_KINDS-1) )
      /* Error: couldn't find a size for hssize_t */
      return -1;
  }

  /* off_t */
  for(i=0;i< H5_FORTRAN_NUM_INTEGER_KINDS;i++) {
    if(IntKinds_SizeOf[i] == H5_SIZEOF_OFF_T) {
      writeToFiles("int","OFF_T", "off_t_f", H5_SIZEOF_OFF_T, IntKinds[i]);
      break;
    }
    if(i == (H5_FORTRAN_NUM_INTEGER_KINDS-1) )
      /* Error: couldn't find a size for off_t */
      return -1;
  }

  /* size_t */
  for(i=0;i< H5_FORTRAN_NUM_INTEGER_KINDS;i++) {
    if(IntKinds_SizeOf[i] == H5_SIZEOF_SIZE_T) {
      writeToFiles("size_t","SIZE_T", "size_t_f", H5_SIZEOF_SIZE_T, IntKinds[i]);
      break;
    }
    if(i == (H5_FORTRAN_NUM_INTEGER_KINDS-1) )
      /* Error: couldn't find a size for size_t */
      return -1;
  }

  /* int */
  writeToFiles("int","Fortran_INTEGER", "int_f", H5_FORTRAN_NATIVE_INTEGER_SIZEOF, H5_FORTRAN_NATIVE_INTEGER_KIND);
  


  /* int_1, int_2, int_4, int_8 */

/* Defined different KINDs of integers:                       */
/* if the integer kind is not available then we assign        */
/* it a value of the next larger one, but if the next         */
/* higher one is not available we assigned it the next lowest */


  FoundIntSize[0] = -1;
  FoundIntSize[1] = -1;
  FoundIntSize[2] = -1;
  FoundIntSize[3] = -1;
  FoundIntSize[4] = -1;
  
  for(i=0;i<H5_FORTRAN_NUM_INTEGER_KINDS;i++) {
    FoundIntSize[i] = (int)IntKinds[i];
    FoundIntSizeKind[i] = (int)IntKinds_SizeOf[i];
/*     writeToFiles("int",chrA, chrB, FoundIntSize[i], FoundIntSizeKind[i]); */
  }

  for(i=0;i<H5_FORTRAN_NUM_INTEGER_KINDS;i++) {
      if( FoundIntSize[i] > 0) /* Found the integer type */
	{
	  sprintf(chrA, "Fortran_INTEGER_%d", FoundIntSize[i]);
	  sprintf(chrB, "int_%d_f", FoundIntSize[i]);
	  writeToFiles("int",chrA, chrB, FoundIntSize[i], FoundIntSizeKind[i]);
	}
      else  /* Did not find the integer type */
	{
	  flag = 0; /* flag indicating if found the next highest */
	  for(j=i+1;j<4;j++)  /* search for next highest */
	    {
	      if( FoundIntSize[j] > 0) /* Found the next highest */
		{
		  sprintf(chrA, "Fortran_INTEGER_%d", (-1)*FoundIntSize[i]);
		  sprintf(chrB, "int_%d_f", (-1)*FoundIntSize[i]);
		  writeToFiles("int",chrA, chrB, FoundIntSize[j], FoundIntSizeKind[j]);
		  flag = 1;
		  break;
		}
	    }
	  if(flag == 0) /* No higher one found, so find next lowest */
	    {
	      for(j=2;j>-1;j--)  /* Search for next lowest */
		{
		  if( FoundIntSize[j] > 0) /* Found the next lowest */
		    {
		      sprintf(chrA, "Fortran_INTEGER_%d", (-1)*FoundIntSize[i]);
		      sprintf(chrB, "int_%d_f", (-1)*FoundIntSize[i]);
		      writeToFiles("int",chrA, chrB, FoundIntSize[j], FoundIntSizeKind[j]);
		      flag = 1;
		      break;
		    }
		}
	    }
	  if(flag == 0) /* No higher or lower one found, indicating an error */
	     return -1;
	}
    }

  /* real_4, real_8, real_16 */

/* Defined different KINDs of reals:                          */
/* if the REAL kind is not available then we assign           */
/* it a value of the next larger one, but if the next         */
/* higher one is not available we assigned it the next lowest */

  FoundRealSize[0] = -1;
  FoundRealSize[1] = -1;
  FoundRealSize[2] = -1;
  FoundRealSize[3] = -1;
  FoundRealSize[4] = -1;
  
  for(i=0;i<H5_FORTRAN_NUM_REAL_KINDS;i++) {
    FoundRealSize[i] = (int)RealKinds[i];
    FoundRealSizeKind[i] = (int)RealKinds_SizeOf[i];	
    sprintf(chrA, "Fortran_REAL_%s", Real_C_TYPES[i]);
 /*    sprintf(chrB, "real_%d_f", FoundRealSize[i]); */
    sprintf(chrB, "real_%s_f", Real_C_TYPES[i]);
    writeToFiles("float",chrA, chrB, RealKinds[i], RealKinds_SizeOf[i]);
  }

/*   for(i=0;i<H5_FORTRAN_NUM_REAL_KINDS;i++) { */
/*     if( FoundRealSize[i] > 0) /\* Found the real type *\/ */
/* 	{ */
/* 	  sprintf(chrA, "Fortran_REAL_%d", Real_C_TYPES[i]); */
/* 	  sprintf(chrB, "real_%d_f", FoundRealSize[i]); */
/* 	  writeToFiles("float",chrA, chrB, FoundRealSize[i], FoundRealSizeKind[i]); */
/* 	} */
/*       else  /\* Did not find the real type *\/ */
/* 	{ */
/* 	  flag = 0; /\* flag indicating if found the next highest *\/ */
/* 	  for(j=i+1;j<3;j++)  /\* search for next highest *\/ */
/* 	    { */
/* 	      if( FoundRealSize[j] > 0) /\* Found the next highest *\/ */
/* 		{ */
/* 		  sprintf(chrA, "Fortran_REAL_%d", (-1)*FoundRealSize[i]); */
/* 		  sprintf(chrB, "real_%d_f", (-1)*FoundRealSize[i]); */
/* 		  if(FoundRealSize[j]>4) { */
/* 		    writeToFiles("float",chrA, chrB,  FoundRealSize[j], FoundRealSizeKind[j]); */
/* 		    flag = 1; */
/* 		  } */
/* 		/\*   else { *\/ */
/* /\* 		    writeToFiles("float", chrA, chrB, FoundRealSize[j]); *\/ */
/* /\* 		  } *\/ */
/* 		  flag = 1; */
/* 		  break; */
/* 		} */
/* 	    } */
/* 	  if(flag == 0) /\* No higher one found, so find next lowest *\/ */
/* 	    { */
/* 	      for(j=1;j>-1;j--)  /\* Search for next lowest *\/ */
/* 		{ */
/* 		  if( FoundRealSize[j] > 0) /\* Found the next lowest *\/ */
/* 		    { */
/* 		      sprintf(chrA, "Fortran_REAL_%d", (-1)*FoundRealSize[i]); */
/* 		      sprintf(chrB, "real_%d_f", (-1)*FoundRealSize[i]); */
/* 		      if(FoundRealSize[j]>4) */
/* 			writeToFiles("float",chrA, chrB,  FoundRealSize[j], FoundRealSizeKind[j]); */
/* 		     /\*  else { *\/ */
/* /\* 			writeToFiles("float", chrA, chrB, FoundRealSize[j]); *\/ */
/* /\* 		      } *\/ */
/* 		      flag = 1; */
/* 		      break; */
/* 		    } */
/* 		} */
/* 	    } */
/* 	  if(flag == 0) /\* No higher or lower one found, indicating an error *\/ */
/* 	     return -1; */
/* 	} */
/*     } */

  /* hid_t */
  for(i=0;i< H5_FORTRAN_NUM_INTEGER_KINDS;i++) {
    if(IntKinds_SizeOf[i] == H5_SIZEOF_HID_T) {
      writeToFiles("int","HID_T", "hid_t_f", H5_SIZEOF_HID_T, IntKinds[i]);
      break;
    }
    if(i == (H5_FORTRAN_NUM_INTEGER_KINDS-1) )
      /* Error: couldn't find a size for hid_t */
      return -1;
  }

  /* real_f */
  if(H5_FORTRAN_NATIVE_REAL_SIZEOF == sizeof(long double))
    writeToFilesChr("float","Fortran_REAL", "real_f", H5_FORTRAN_NATIVE_REAL_KIND, "C_LONG_DOUBLE");
  else if(H5_FORTRAN_NATIVE_REAL_SIZEOF == sizeof(double))
    writeToFilesChr("float","Fortran_REAL", "real_f", H5_FORTRAN_NATIVE_REAL_KIND, "C_DOUBLE");
  else if(H5_FORTRAN_NATIVE_REAL_SIZEOF == sizeof(float))
    writeToFilesChr("float","Fortran_REAL", "real_f", H5_FORTRAN_NATIVE_REAL_KIND, "C_FLOAT");
  else {
    /* No exact match, choose the next highest */
    if(H5_FORTRAN_NATIVE_REAL_SIZEOF > sizeof(long double))
      writeToFilesChr("float","Fortran_REAL", "real_f", H5_FORTRAN_NATIVE_REAL_KIND, "C_LONG_DOUBLE");
    else if(H5_FORTRAN_NATIVE_REAL_SIZEOF > sizeof(double))
      writeToFilesChr("float","Fortran_REAL", "real_f", H5_FORTRAN_NATIVE_REAL_KIND, "C_DOUBLE");
    else if(H5_FORTRAN_NATIVE_REAL_SIZEOF > sizeof(float))
      writeToFilesChr("float","Fortran_REAL", "real_f", H5_FORTRAN_NATIVE_REAL_KIND, "C_FLOAT");
    else
      /* Error: couldn't find a size for real_f */
      return -1;
  }

  /* double_f */
  if(H5_FORTRAN_NATIVE_DOUBLE_SIZEOF == sizeof(long double))
    writeToFilesChr("float","Fortran_DOUBLE", "double_f", H5_FORTRAN_NATIVE_DOUBLE_KIND, "C_LONG_DOUBLE");
  else if(H5_FORTRAN_NATIVE_DOUBLE_SIZEOF == sizeof(double))
    writeToFilesChr("float","Fortran_DOUBLE", "double_f", H5_FORTRAN_NATIVE_DOUBLE_KIND, "C_DOUBLE");
  else if(H5_FORTRAN_NATIVE_DOUBLE_SIZEOF == sizeof(float))
    writeToFilesChr("float","Fortran_DOUBLE", "double_f", H5_FORTRAN_NATIVE_DOUBLE_KIND, "C_FLOAT");
  else {
/*     /\* No exact match, choose the next highest *\/ */
/*     if(H5_FORTRAN_NATIVE_DOUBLE_SIZEOF > sizeof(long double)) */
/*       writeToFilesChr("float","Fortran_DOUBLE", "double_f", H5_FORTRAN_NATIVE_DOUBLE_KIND, "C_LONG_DOUBLE"); */
/*     else if(H5_FORTRAN_NATIVE_DOUBLE_SIZEOF > sizeof(double)) */
/*       writeToFilesChr("float","Fortran_DOUBLE", "double_f", sizeof(double), "C_DOUBLE"); */
/*     else if(H5_FORTRAN_NATIVE_DOUBLE_SIZEOF > sizeof(float)) */
/*       writeToFilesChr("float","Fortran_DOUBLE", "double_f", sizeof(float), "C_FLOAT"); */
/*     else */
      /* Error: couldn't find a size for double_f */
/*       printf("Error: couldn't find a size for double_f \n"); */
/*       return -1; */
  }



/*   /\* double_f *\/ */
/*   if(H5_FORTRAN_NATIVE_DOUBLE_SIZEOF == C_LONG_DOUBLE_SIZEOF) */
/*     writeToFilesChr("float","Fortran_DOUBLE", "double_f", H5_FORTRAN_NATIVE_DOUBLE_SIZEOF, "C_LONG_DOUBLE"); */
/*   else if(H5_FORTRAN_NATIVE_DOUBLE_SIZEOF == C_DOUBLE_SIZEOF) */
/*     writeToFilesChr("float","Fortran_DOUBLE", "double_f", H5_FORTRAN_NATIVE_DOUBLE_SIZEOF, "C_DOUBLE"); */
/*   else if(H5_FORTRAN_NATIVE_DOUBLE_SIZEOF == C_FLOAT_SIZEOF) */
/*     writeToFilesChr("float","Fortran_DOUBLE", "double_f", H5_FORTRAN_NATIVE_DOUBLE_SIZEOF, "C_FLOAT"); */
/*   else */
/*     /\* Error: couldn't find a size for double_f *\/ */
/*     return -1; */

/*   writeToFiles("float","Fortran_DOUBLE", "double_f", H5_FORTRAN_NATIVE_DOUBLE_SIZEOF, H5_FORTRAN_NATIVE_DOUBLE_KIND); */

/* #elif defined H5_FORTRAN_HAS_DOUBLE_NATIVE_8_KIND */
/*     writeToFilesChr("float", "Fortran_DOUBLE", "double_f", 8, "C_DOUBLE"); */
/* #else */
/*     /\* Error: couldn't find a size for real_f *\/ */
/*     return -1; */
/* #endif */

/*     /\* real_f *\/ */
/*     if(sizeof(float) == C_LONG_DOUBLE_SIZEOF) { */
/*       writeToFilesChr("float","Fortran_REAL", "real_f", (int)sizeof(float), "C_LONG_DOUBLE"); */
/*     } else if(sizeof(float) == C_DOUBLE_SIZEOF) { */
/*       writeToFilesChr("float","Fortran_REAL", "real_f", (int)sizeof(float), "C_DOUBLE"); */
/*     } else if(sizeof(float) == C_FLOAT_SIZEOF) { */
/*       writeToFilesChr("float","Fortran_REAL", "real_f", (int)sizeof(float), "C_FLOAT"); */
/*     } else { */
/*       /\* Error: couldn't find a size for real_f *\/ */
/*     return -1; */
/*     } */

/*     /\* double_f *\/ */
/*     if(sizeof(double) == C_LONG_DOUBLE_SIZEOF) { */
/*       writeToFilesChr("float","Fortran_DOUBLE", "double_f", (int)sizeof(double), "C_LONG_DOUBLE"); */
/*     } else if(sizeof(double) == C_DOUBLE_SIZEOF) { */
/*       writeToFilesChr("float","Fortran_DOUBLE", "double_f", (int)sizeof(double), "C_DOUBLE"); */
/*     } else if(sizeof(double) == C_FLOAT_SIZEOF) { */
/*       writeToFilesChr("float","Fortran_DOUBLE", "double_f", (int)sizeof(double), "C_FLOAT"); */
/*     } else { */
/*       /\* Error: couldn't find a size for double_f *\/ */
/*     return -1; */
/*     } */


  /* Need the buffer size for the fortran derive type 'hdset_reg_ref_t_f03'
   * in order to be interoperable with C's structure, the C buffer size
   * H5R_DSET_REG_REF_BUF_SIZE is (sizeof(haddr_t)+4)
   */
    
    fprintf(fort_header, "        INTEGER, PARAMETER :: H5R_DSET_REG_REF_BUF_SIZE_F = %u\n", H5_SIZEOF_HADDR_T + 4 );


  /* Close files */
  endCfile();
  endFfile();
  fclose(c_header);
  fclose(fort_header);
  return 0;
}

