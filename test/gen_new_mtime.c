/*
 * Copyright (C) 2003 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Friday, January  3, 2003
 *
 * Purpose:	Create a dataset, which should have the newer mtime information
 *      stored in it.
 *		This program is used to create the test file `tmtimen.h5' which
 *      has the new format for mtime information.
 *		To build the test file, this program MUST be compiled and linked with
 *      the hdf5-1.5+ series of libraries and the generated test file must be
 *      put into the 'test' directory in the 1.5+ branch of the library.
 */

#include "hdf5.h"

#define TESTFILE   "tmtimen.h5"


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	
 *
 * Return:	Success:	
 *
 *		Failure:	
 *
 * Programmer:	Quincey Koziol
 *              Friday, January  3, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t	file, space, dset;

    /* Create the file */
    file = H5Fcreate(TESTFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if(file<0)
        printf("file<0!\n");

    /* Create the dataspace (for dataset) */
    space = H5Screate(H5S_SCALAR);
    if(space<0)
        printf("space<0!\n");

    /* Create the dataset with compound array fields */
    dset = H5Dcreate(file, "Dataset1", H5T_NATIVE_INT, space, H5P_DEFAULT);
    if(dset<0)
        printf("dset<0!\n");

    H5Dclose(dset);
    H5Sclose(space);
    H5Fclose(file);

    return 0;
}


