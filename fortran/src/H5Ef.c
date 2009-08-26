/****h* H5Ef/H5Ef
 * PURPOSE
 *   This file contains C stubs for H5E Fortran APIs
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
#include "H5f90.h"


/****if* H5Ef/h5eclear_c
 * NAME
 *        h5eclear_c
 * PURPOSE
 *     Call H5Eclear to clear the error stack for the current thread
 * INPUTS
 *
 * OUTPUTS
 *
 * RETURNS
 *     0 on success, -1 on failure
 * AUTHOR
 *  Xiangyang Su
 *              Wednesday, March 29, 2000
 * HISTORY
 *
 * SOURCE
*/
int_f
nh5eclear_c(hid_t_f *estack_id )
/******/
{
  int ret_val = -1;
  herr_t status;
  /*
   * Call H5Eclear function.
   */
  status = H5Eclear2((hid_t)*estack_id);
  if(status < 0) return ret_val;
  ret_val = 0;
  return ret_val;
}

/****if* H5Ef/h5eprint_c1
 * NAME
 *        h5eprint_c1
 * PURPOSE
 *     Call H5Eprint to print the error stack in a default manner.
 * INPUTS
 *      name - file name
 *              namelen - length of name
 * OUTPUTS
 *
 * RETURNS
 *     0 on success, -1 on failure
 * AUTHOR
 *  Xiangyang Su
 *              Wednesday, March 29, 2000
 * HISTORY
 * Bug fix: Added call to close the file with the error messages
 *                EP 11/26/01
 * SOURCE
*/
int_f
nh5eprint_c1(_fcd name, int_f* namelen)
/******/
{
  int ret_val = -1;
  herr_t status;
  FILE * file;
  char* c_name;
  size_t c_namelen;
  c_namelen = *namelen;
  c_name = (char*)HD5f2cstring(name, c_namelen);
  if(c_name == NULL) return ret_val;
  file = fopen(c_name, "a");
       if(!file) goto DONE;
  /*
   * Call H5Eprint2 function.
   */
  status = H5Eprint2(H5E_DEFAULT, file);
  if (status >=0 ) ret_val = 0;
  fclose(file);

DONE:
  HDfree(c_name);
  return ret_val;
}


/****if* H5Ef/h5eprint_c2
 * NAME
 *        h5eprint_c2
 * PURPOSE
 *     Call H5Eprint to print the error stack to stderr
 *              in a default manner.
 * INPUTS
 *
 * OUTPUTS
 *
 * RETURNS
 *     0 on success, -1 on failure
 * AUTHOR
 *  Xiangyang Su
 *              Wednesday, March 29, 2000
 *
 * SOURCE
*/
int_f
nh5eprint_c2()
/******/
{
  int ret_val = -1;
  herr_t status;

  /*
   * Call H5Eprint2 function.
   */
  status = H5Eprint2(H5E_DEFAULT, NULL);
  if(status >= 0) ret_val = 0;
  return ret_val;
}

/****if* H5Ef/h5eget_major_c
 * NAME
 *        h5eget_major_c
 * PURPOSE
 *     Call H5Eget_major to get a character string
 *              describing an error specified by a major error number.
 * INPUTS
 *      error_no - Major error number
 * OUTPUTS
 *     name - character string describing the error
 * RETURNS
 *     0 on success, -1 on failure
 * AUTHOR
 *  Xiangyang Su
 *              Wednesday, March 29, 2000
 * HISTORY
 *
 * SOURCE
*/
int_f
nh5eget_major_c(int_f* error_no, _fcd name, size_t_f* namelen)
/******/
{
  int ret_val = -1;
  char *c_name = NULL;
  size_t c_namelen;
  hid_t c_error_no;
  c_error_no = (hid_t)*error_no;

  c_namelen = (size_t)*namelen;
  if(c_namelen) c_name = (char*) HDmalloc(c_namelen + 1);

  /*
   * Call H5Eget_major function.
   */
  H5Eget_msg(c_error_no, NULL, c_name, c_namelen);
  HD5packFstring((char*)c_name, _fcdtocp(name), c_namelen);

  if(!strcmp(c_name, "Invalid major error number")) return ret_val;
  ret_val = 0;
  return ret_val;
}

/****if* H5Ef/h5eget_minor_c
 * NAME
 *        h5eget_minor_c
 * PURPOSE
 *     Call H5Eget_minor to get a character string
 *              describing an error specified by a minor error number.
 * INPUTS
 *      error_no - Major error number
 * OUTPUTS
 *     name - character string describing the error
 * RETURNS
 *     0 on success, -1 on failure
 * AUTHOR
 *  Xiangyang Su
 *              Wednesday, March 29, 2000
 * HISTORY
 *
 * SOURCE
*/
int_f
nh5eget_minor_c(int_f* error_no, _fcd name, size_t_f* namelen)
/******/
{
  int ret_val = -1;
  char *c_name = NULL;
  size_t c_namelen;
  hid_t c_error_no;
  c_error_no = (hid_t)*error_no;

  c_namelen = (size_t)*namelen;
  if(c_namelen) c_name = (char*) HDmalloc(c_namelen + 1);

  /*
   * Call H5Eget_minor function.
   */
  H5Eget_msg(c_error_no, NULL, c_name, c_namelen);
  HD5packFstring((char*)c_name, _fcdtocp(name), c_namelen);

  if(!strcmp(c_name, "Invalid minor error number")) return ret_val;
  ret_val = 0;
  return ret_val;
}

/****if* H5Ef/h5eset_auto_c
 * NAME
 *        h5eset_auto_c
 * PURPOSE
 *     Call H5Eset_auto to turn automatic error printing on or off.
 * INPUTS
 *      printflag - flag to turn automatic error printing on or off.
 * OUTPUTS
 *
 * RETURNS
 *     0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *              Friday, November 17, 2000
 * HISTORY
 *  major bug fix. Function never disabled printing.
 * SOURCE
*/
int_f
nh5eset_auto_c(int_f* printflag)
/******/
{
  int ret_val = -1;
  herr_t status = -1;

  if (*printflag == 1)
    status = H5Eset_auto2(H5E_DEFAULT, H5Eprint2, stderr);
  else if (*printflag == 0)
    status = H5Eset_auto2(H5E_DEFAULT, NULL, NULL);
  if (status >= 0) ret_val = 0;
  return ret_val;
}


/****if* H5Ef/h5eset_auto2_c
 * NAME
 *   h5eset_auto2_c
 * PURPOSE
 *   Calls H5Eset_auto2
 * INPUTS
 *   estack_id    - Error stack identifier.
 *   func 	 - Function to be called upon an error condition.
 *   client_data - Data passed to the error function.
 *   
 * RETURNS
 *   0 on success, -1 on failure
 * AUTHOR
 *   M. Scot Breitenfeld
 *   July 22, 2009
 * SOURCE
*/
/* int_f */
/* nh5eset_auto2_c(hid_t_f *estack_id, H5E_auto2_t *func, void *client_data) */
/* /\******\/ */
/* { */
/*   int ret_val = -1; */
/*   herr_t status = -1; */

/*   status = H5Eset_auto2((hid_t)*estack_id, *func, client_data); */
/*   if (status >= 0) ret_val = 0; */
/*   return ret_val; */
/* } */

int_f
nh5eset_auto2_c(int_f *printflag, hid_t_f *estack_id, H5E_auto2_t func, void *client_data)
/******/
{
  int ret_val = -1;
  herr_t status = -1;

  if (*printflag == 1 && *estack_id == -1)
    status = H5Eset_auto2(H5E_DEFAULT, H5Eprint2, stderr);
  else if (*printflag == 1)
    status = H5Eset_auto2((hid_t)*estack_id, func, client_data);
  else if (*printflag == 0)
    status = H5Eset_auto2(H5E_DEFAULT, NULL, NULL);
  if (status >= 0) ret_val = 0;

  return ret_val;
}

/****if* H5Ef/h5eget_auto_c
 * NAME
 *  h5eget_auto_c
 * PURPOSE
 *  Calls H5Eget_auto2
 * INPUTS
 *  estack_id    - Error stack identifier
 * OUTPUTS
 *  func          - The function currently set to be called upon an error condition.
 *  client_data  - Data currently set to be passed to the error function.
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  July 10, 2009
 * NOTE
 *  if func is the default H5Eprint or H5Eprint2 then need to use the Fortran callback function
 *  associated  with H5Eprint*.
 * SOURCE
*/
int_f
nh5eget_auto_c(hid_t_f *estack_id, H5E_auto2_t *func, void **client_data, int_f *ret_func)
/******/
{
  int ret_val = -1;
  herr_t status = -1;
  void *old_data;
  H5E_auto2_t func2;
  hid_t dataset;
     
  *ret_func = -1;

/*   H5E_BEGIN_TRY { */
/*     dataset = H5Dcreate2(-1, "tmp", H5T_STD_I32BE, -1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT); */
/*   } H5E_END_TRY; */

/*    status = H5Eset_auto2(H5E_DEFAULT, (H5E_auto2_t)H5Eprint2, NULL); */

/*   printf(" %d \n", (int)status); */
  status = H5Eget_auto2((hid_t)*estack_id, &func, &client_data);

/*   printf(" %d \n", (int)status); */
  
  if(client_data != NULL)
    printf("error \n");
#ifdef H5_USE_16_API
  if (func == (H5E_auto_t)H5Eprint) 
    *ret_func = 0;
#else /* H5_USE_16_API */
  if (func == (H5E_auto2_t)H5Eprint2)
    *ret_func = 0;
#endif /* H5_USE_16_API */

/* #ifdef H5_USE_16_API */
/*   if (func == (H5E_auto_t)H5Eprint) */
/*     *ret_func = 0; */
/* #else /\* H5_USE_16_API *\/ */
/*   if (func == (H5E_auto2_t)H5Eprint2) */
/*     *ret_func = 0; */ /* printf("%p %p \n",func,(H5E_auto2_t)H5Eprint2); */
/* #endif /\* H5_USE_16_API *\/ */

/*   printf(" ret %d \n", ret_val); */
  if (status >= 0) ret_val = 0;
  return ret_val;
}
