/*
 * Copyright (C) 2000 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Raymond Lu <slu@ncsa.uiuc.edu>
 *              Feb 27, 2002 
 *
 * Purpose:     This program is run to generate a HDF5 data file with fill
 * 		value property.  A new fill value design has been put into
 *		library v1.5.  To test compatibility between v1.4 and v1.5,
 *		compile and run this program, it will generate a file called
 *		fill_old.h5.  You need to move it to the /test directory 
 *		in HDF5 source codes.  The fillval.c program will read it.
 *	 
 */

#include "h5test.h"

#define FILENAME "fill_old.h5"

int main()
{
  hid_t   file=-1, dcpl=-1, space=-1, dset1=-1, dset2=-1;
  hsize_t cur_size[2]={8, 8};
  int     fill_val = 4444;


  if((file=H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT))
	<0) goto error;
  if((space=H5Screate_simple(2, cur_size, cur_size))<0) goto error;
  if((dcpl=H5Pcreate(H5P_DATASET_CREATE))<0) goto error;

  /* Create a dataset without fill value */
  if((dset1 = H5Dcreate(file, "dset1", H5T_NATIVE_INT, space, dcpl))<0)
        goto error; 
  if(H5Dclose(dset1)<0) goto error; 

  /* Create a dataset with a fill value */
  if(H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fill_val)<0) goto error;
  if((dset2 = H5Dcreate(file, "dset2", H5T_NATIVE_INT, space, dcpl))<0)
        goto error;
  if(H5Dclose(dset2)<0) goto error;

  if(H5Sclose(space)<0) goto error;
  if(H5Pclose(dcpl)<0) goto error;
  if(H5Fclose(file)<0) goto error;

  return 0;

 error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Sclose(space);
        H5Dclose(dset1);
        H5Dclose(dset2);
        H5Fclose(file);
    } H5E_END_TRY;
    return 1;
}
