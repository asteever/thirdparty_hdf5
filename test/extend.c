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
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Friday, January 30, 1998
 *
 * Purpose:	Tests extendible datasets.
 */

#include "h5test.h"

const char *FILENAME[] = {
    "extend",
    NULL
};

#define NX	100		/* USE AN EVEN NUMBER!*/
#define NY	100		/* USE AN EVEN NUMBER!*/

/* Data buffers */
static int			buf1[NY][NX], buf2[NX/2][NY/2];


/*-------------------------------------------------------------------------
 * Function:	write_data
 *
 * Purpose:	Create extendible dataset and test extend/write/read
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, June 10, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
write_data(const char *msg, hid_t file, const char *name, hid_t cparms, hid_t mem_space)
{
    hid_t			dataset, file_space, half_space;
    static const hsize_t 	dims[2] = {NX, NY};
    static const hsize_t 	half_dims[2] = {NX/2, NY/2};
    static hsize_t		size[2];
    hssize_t			offset[2];
    int				i, j, k, m;

    TESTING(msg);

    if ((dataset = H5Dcreate (file, name, H5T_NATIVE_INT, mem_space,
			      cparms))<0) TEST_ERROR;
    /* Write the data */
    for (i=0; i<5; i++) {
	for (j=0; j<5; j++) {

	    /* Extend the dataset */
	    offset[0] = i * NX;
	    offset[1] = j * NY;
	    size[0] = offset[0] + NX;
	    size[1] = offset[1] + NY;
	    if (H5Dextend (dataset, size)<0) TEST_ERROR;

	    /* Select a hyperslab */
            if ((file_space = H5Dget_space (dataset))<0) TEST_ERROR;
	    if (H5Sselect_hyperslab (file_space, H5S_SELECT_SET, offset,
				     NULL, dims, NULL)<0) TEST_ERROR;

	    /* Write to the hyperslab */
	    if (H5Dwrite (dataset, H5T_NATIVE_INT, mem_space, file_space,
			  H5P_DEFAULT, buf1)<0) TEST_ERROR;
            if (H5Sclose (file_space)<0) TEST_ERROR;
	}
    }

    /* Read the data */
    if ((half_space = H5Screate_simple (2, half_dims, NULL))<0) TEST_ERROR;
    if ((file_space = H5Dget_space (dataset))<0) TEST_ERROR;
    for (i=0; i<10; i++) {
	for (j=0; j<10; j++) {

	    /* Select a hyperslab */
	    offset[0] = i * NX/2;
	    offset[1] = j * NY/2;
	    if (H5Sselect_hyperslab (file_space, H5S_SELECT_SET, offset,
				     NULL, half_dims, NULL)<0) TEST_ERROR;
	    
	    /* Read */
	    if (H5Dread (dataset, H5T_NATIVE_INT, half_space, file_space,
			 H5P_DEFAULT, buf2)<0) TEST_ERROR;

	    /* Compare */
	    for (k=0; k<NX/2; k++) {
		for (m=0; m<NY/2; m++) {
		    if (buf2[k][m]!=buf1[(i%2)*NX/2+k][(j%2)*NY/2+m]) {
			H5_FAILED();
			printf("    i=%d, j=%d, k=%d, m=%d\n", i, j, k, m);
			printf("    buf2[%d][%d]=%d\n",k,m,buf2[k][m]);
			printf("    buf1[%d][%d]=%d\n",(i%2)*NX/2+k,(j%2)*NY/2+m,buf1[(i%2)*NX/2+k][(j%2)*NY/2+m]);
			TEST_ERROR;
		    }
		}
	    }
	}
    }
    

    /* Cleanup */
    if (H5Dclose (dataset)<0) TEST_ERROR;
    if (H5Sclose (file_space)<0) TEST_ERROR;
    if (H5Sclose (half_space)<0) TEST_ERROR;

    PASSED();
    return 0;

error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Tests extendible datasets
 *
 * Return:	Success:	exit(0)
 *
 *		Failure:	exit(non-zero)
 *
 * Programmer:	Robb Matzke
 *              Friday, January 30, 1998
 *
 * Modifications:
 *              Took main data code out into write_data() routine, to allow
 *              different dataset creation property list settings to be tested.
 *              Quincey Koziol
 *              Tuesday, June 10, 2003
 *
 *-------------------------------------------------------------------------
 */
int
main (void)
{
    hid_t			file, mem_space, cparms;
    hid_t			fapl;
    int		nerrors = 0;
    static const hsize_t	dims[2] = {NX, NY};
    static const hsize_t 	chunk_dims[2] = {NX/2, NY/2};
    static hsize_t		maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
    char			filename[1024];
    int				i, j;

    h5_reset();
    fapl = h5_fileaccess();

    /* Initialize buffer and space */
    for (i=0; i<NX; i++) {
	for (j=0; j<NY; j++) {
	    buf1[i][j] = i*NY+j;
	}
    }
    if ((mem_space = H5Screate_simple (2, dims, maxdims))<0) TEST_ERROR;

    /* Create the file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file = H5Fcreate (filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;
    
    /* Create the dataset which is originally NX by NY */
    if((cparms = H5Pcreate(H5P_DATASET_CREATE))<0) TEST_ERROR;
    if (H5Pset_chunk (cparms, 2, chunk_dims)<0) TEST_ERROR;
    nerrors += write_data("extendible dataset with incremental allocation",file,"dataset1",cparms,mem_space)<0 	?1:0;
    if(H5Pset_alloc_time(cparms, H5D_ALLOC_TIME_EARLY)<0) TEST_ERROR;
    nerrors += write_data("extendible dataset with early allocation",file,"dataset2",cparms,mem_space)<0 	?1:0;

    if (H5Pclose (cparms)<0) TEST_ERROR;
    if (H5Sclose (mem_space)<0) TEST_ERROR;

    if (H5Fclose (file)<0) TEST_ERROR;
    if (nerrors) {
	printf("***** %d FAILURE%s! *****\n", nerrors, 1==nerrors?"":"S");
	exit(1);
    }
    printf("All extend tests passed.\n");
    h5_cleanup(FILENAME, fapl);
    return 0;

 error:
    printf("*** One or more extend tests failed ***\n");
    return 1;
}

