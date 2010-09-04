
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
   This program will test independant and collective reads and writes between
   selections of different rank that non-the-less are deemed as having the 
   same shape by H5Sselect_shape_same().
 */

#define H5S_PACKAGE             /*suppress error about including H5Spkg   */

/* Define this macro to indicate that the testing APIs should be available */
#define H5S_TESTING


#include "hdf5.h"
#include "H5private.h"
#include "testphdf5.h"
#include "H5Spkg.h"             /* Dataspaces                           */


/*-------------------------------------------------------------------------
 * Function:	contig_hyperslab_dr_pio_test__run_test()
 *
 * Purpose:	Test I/O to/from hyperslab selections of different rank in
 *		the parallel.
 *
 * Return:	void
 *
 * Programmer:	JRM -- 9/18/09
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

#define PAR_SS_DR_MAX_RANK	5
#define CONTIG_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG 0

static void
contig_hyperslab_dr_pio_test__run_test(const int test_num,
                                       const int edge_size,
                                       const int chunk_edge_size,
                                       const int small_rank,
                                       const int large_rank,
                                       const hbool_t use_collective_io,
                                       const hid_t dset_type)
{
#if CONTIG_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG 
    const char *fcnName = "contig_hyperslab_dr_pio_test__run_test()";
#endif /* CONTIG_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG */
    const char *filename;
    hbool_t	use_gpfs = FALSE;   /* Use GPFS hints */
    hbool_t	mis_match = FALSE;
    int		i, j, k, l, n;
    int         mrc;
    int		mpi_size = -1;
    int         mpi_rank = -1;
    int         start_index;
    int         stop_index;
    const int   test_max_rank = 5;  /* must update code if this changes */
    uint32_t	expected_value;
    uint32_t  * small_ds_buf_0 = NULL;
    uint32_t  * small_ds_buf_1 = NULL;
    uint32_t  * small_ds_buf_2 = NULL;
    uint32_t  * small_ds_slice_buf = NULL;
    uint32_t  * large_ds_buf_0 = NULL;
    uint32_t  * large_ds_buf_1 = NULL;
    uint32_t  * large_ds_buf_2 = NULL;
    uint32_t  * large_ds_slice_buf = NULL;
    uint32_t  * ptr_0;
    uint32_t  * ptr_1;
    uint32_t  * ptr_2;
    MPI_Comm    mpi_comm = MPI_COMM_NULL;
    MPI_Info	mpi_info = MPI_INFO_NULL;
    hid_t       fid;			/* HDF5 file ID */
    hid_t	acc_tpl;		/* File access templates */
    hid_t	xfer_plist = H5P_DEFAULT;
    hid_t       full_mem_small_ds_sid;
    hid_t       full_file_small_ds_sid;
    hid_t       mem_small_ds_sid;
    hid_t       file_small_ds_sid;
    hid_t	small_ds_slice_sid;
    hid_t       full_mem_large_ds_sid;
    hid_t       full_file_large_ds_sid;
    hid_t       mem_large_ds_sid;
    hid_t       file_large_ds_sid;
    hid_t       file_large_ds_process_slice_sid;
    hid_t       mem_large_ds_process_slice_sid;
    hid_t	large_ds_slice_sid;
    hid_t       small_ds_dcpl_id = H5P_DEFAULT;
    hid_t       large_ds_dcpl_id = H5P_DEFAULT;
    hid_t       small_dataset;     /* Dataset ID                   */
    hid_t       large_dataset;     /* Dataset ID                   */
    size_t      small_ds_size = 1;
    size_t      small_ds_slice_size = 1;
    size_t      large_ds_size = 1;
    size_t      large_ds_slice_size = 1;
    hsize_t     dims[PAR_SS_DR_MAX_RANK];
    hsize_t     chunk_dims[PAR_SS_DR_MAX_RANK];
    hsize_t     start[PAR_SS_DR_MAX_RANK];
    hsize_t     stride[PAR_SS_DR_MAX_RANK];
    hsize_t     count[PAR_SS_DR_MAX_RANK];
    hsize_t     block[PAR_SS_DR_MAX_RANK];
    hsize_t   * start_ptr = NULL;
    hsize_t   * stride_ptr = NULL;
    hsize_t   * count_ptr = NULL;
    hsize_t   * block_ptr = NULL;
    htri_t      check;          /* Shape comparison return value */
    herr_t	ret;		/* Generic return value */

    HDassert( edge_size >= 6 );
    HDassert( edge_size >= chunk_edge_size );
    HDassert( ( chunk_edge_size == 0 ) || ( chunk_edge_size >= 3 ) );
    HDassert( 1 < small_rank );
    HDassert( small_rank < large_rank );
    HDassert( large_rank <= test_max_rank );
    HDassert( test_max_rank <= PAR_SS_DR_MAX_RANK );

    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    HDassert( mpi_size >= 1 );

    mpi_comm = MPI_COMM_WORLD;
    mpi_info = MPI_INFO_NULL;

    for ( i = 0; i < small_rank - 1; i++ )
    {
        small_ds_size *= (size_t)edge_size;
        small_ds_slice_size *= (size_t)edge_size;
    }
    small_ds_size *= (size_t)(mpi_size + 1);


    for ( i = 0; i < large_rank - 1; i++ ) {

        large_ds_size *= (size_t)edge_size;
        large_ds_slice_size *= (size_t)edge_size;
    }
    large_ds_size *= (size_t)(mpi_size + 1);


    /* set up the start, stride, count, and block pointers */
    start_ptr  = &(start[PAR_SS_DR_MAX_RANK - large_rank]);
    stride_ptr = &(stride[PAR_SS_DR_MAX_RANK - large_rank]);
    count_ptr  = &(count[PAR_SS_DR_MAX_RANK - large_rank]);
    block_ptr  = &(block[PAR_SS_DR_MAX_RANK - large_rank]);


    /* Allocate buffers */
    small_ds_buf_0 = (uint32_t *)HDmalloc(sizeof(uint32_t) * small_ds_size);
    VRFY((small_ds_buf_0 != NULL), "malloc of small_ds_buf_0 succeeded");

    small_ds_buf_1 = (uint32_t *)HDmalloc(sizeof(uint32_t) * small_ds_size);
    VRFY((small_ds_buf_1 != NULL), "malloc of small_ds_buf_1 succeeded");

    small_ds_buf_2 = (uint32_t *)HDmalloc(sizeof(uint32_t) * small_ds_size);
    VRFY((small_ds_buf_2 != NULL), "malloc of small_ds_buf_2 succeeded");

    small_ds_slice_buf = 
	(uint32_t *)HDmalloc(sizeof(uint32_t) * small_ds_slice_size);
    VRFY((small_ds_slice_buf != NULL), "malloc of small_ds_slice_buf succeeded");

    large_ds_buf_0 = (uint32_t *)HDmalloc(sizeof(uint32_t) * large_ds_size);
    VRFY((large_ds_buf_0 != NULL), "malloc of large_ds_buf_0 succeeded");

    large_ds_buf_1 = (uint32_t *)HDmalloc(sizeof(uint32_t) * large_ds_size);
    VRFY((large_ds_buf_1 != NULL), "malloc of large_ds_buf_1 succeeded");

    large_ds_buf_2 = (uint32_t *)HDmalloc(sizeof(uint32_t) * large_ds_size);
    VRFY((large_ds_buf_2 != NULL), "malloc of large_ds_buf_2 succeeded");

    large_ds_slice_buf = 
	(uint32_t *)HDmalloc(sizeof(uint32_t) * large_ds_slice_size);
    VRFY((large_ds_slice_buf != NULL), "malloc of large_ds_slice_buf succeeded");

    /* initialize the buffers */

    ptr_0 = small_ds_buf_0;
    ptr_1 = small_ds_buf_1;
    ptr_2 = small_ds_buf_2;

    for ( i = 0; i < (int)small_ds_size; i++ ) {

        *ptr_0 = (uint32_t)i;
        *ptr_1 = 0;
        *ptr_2 = 0;

        ptr_0++;
        ptr_1++;
        ptr_2++;
    }

    ptr_0 = small_ds_slice_buf;

    for ( i = 0; i < (int)small_ds_slice_size; i++ ) {

	*ptr_0 = (uint32_t)0;
        ptr_0++;
    }

    ptr_0 = large_ds_buf_0;
    ptr_1 = large_ds_buf_1;
    ptr_2 = large_ds_buf_2;

    for ( i = 0; i < (int)large_ds_size; i++ ) {

        *ptr_0 = (uint32_t)i;
        *ptr_1 = 0;
        *ptr_2 = 0;

        ptr_0++;
        ptr_1++;
        ptr_2++;
    }

    ptr_0 = large_ds_slice_buf;

    for ( i = 0; i < (int)large_ds_slice_size; i++ ) {

	*ptr_0 = (uint32_t)0;
        ptr_0++;
    }

    filename = (const char *)GetTestParameters();
    HDassert( filename != NULL );
#if CONTIG_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG 
    if ( MAINPROCESS ) {

        HDfprintf(stdout, "%d: test num = %d.\n", mpi_rank, test_num);
        HDfprintf(stdout, "%d: mpi_size = %d.\n", mpi_rank, mpi_size);
        HDfprintf(stdout, 
                  "%d: small/large rank = %d/%d, use_collective_io = %d.\n",
                  mpi_rank, small_rank, large_rank, (int)use_collective_io);
        HDfprintf(stdout, "%d: edge_size = %d, chunk_edge_size = %d.\n",
                  mpi_rank, edge_size, chunk_edge_size);
        HDfprintf(stdout, "%d: small_ds_size = %d, large_ds_size = %d.\n",
                  mpi_rank, (int)small_ds_size, (int)large_ds_size);
        HDfprintf(stdout, "%d: filename = %s.\n", mpi_rank, filename);
    }
#endif
    /* ----------------------------------------
     * CREATE AN HDF5 FILE WITH PARALLEL ACCESS
     * ---------------------------------------*/
    /* setup file access template */
    acc_tpl = create_faccess_plist(mpi_comm, mpi_info, facc_type, use_gpfs);
    VRFY((acc_tpl >= 0), "create_faccess_plist() succeeded");

    /* create the file collectively */
    fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, acc_tpl);
    VRFY((fid >= 0), "H5Fcreate succeeded");

    MESG("File opened.");

    /* Release file-access template */
    ret = H5Pclose(acc_tpl);
    VRFY((ret >= 0), "H5Pclose(acc_tpl) succeeded");


    /* setup dims: */
    dims[0] = (int)(mpi_size + 1);
    dims[1] = dims[2] = dims[3] = dims[4] = edge_size;


    /* Create small ds dataspaces */
    full_mem_small_ds_sid = H5Screate_simple(small_rank, dims, NULL);
    VRFY((full_mem_small_ds_sid != 0), 
         "H5Screate_simple() full_mem_small_ds_sid succeeded");

    full_file_small_ds_sid = H5Screate_simple(small_rank, dims, NULL);
    VRFY((full_file_small_ds_sid != 0), 
         "H5Screate_simple() full_file_small_ds_sid succeeded");

    mem_small_ds_sid = H5Screate_simple(small_rank, dims, NULL);
    VRFY((mem_small_ds_sid != 0), 
	 "H5Screate_simple() mem_small_ds_sid succeeded");

    file_small_ds_sid = H5Screate_simple(small_rank, dims, NULL);
    VRFY((file_small_ds_sid != 0), 
         "H5Screate_simple() file_small_ds_sid succeeded");

    small_ds_slice_sid = H5Screate_simple(small_rank - 1, &(dims[1]), NULL);
    VRFY((small_ds_slice_sid != 0), 
         "H5Screate_simple() small_ds_slice_sid succeeded");


    /* Create large ds dataspaces */
    full_mem_large_ds_sid = H5Screate_simple(large_rank, dims, NULL);
    VRFY((full_mem_large_ds_sid != 0), 
         "H5Screate_simple() full_mem_large_ds_sid succeeded");

    full_file_large_ds_sid = H5Screate_simple(large_rank, dims, NULL);
    VRFY((full_file_large_ds_sid != FAIL), 
         "H5Screate_simple() full_file_large_ds_sid succeeded");

    mem_large_ds_sid = H5Screate_simple(large_rank, dims, NULL);
    VRFY((mem_large_ds_sid != FAIL), 
         "H5Screate_simple() mem_large_ds_sid succeeded");

    file_large_ds_sid = H5Screate_simple(large_rank, dims, NULL);
    VRFY((file_large_ds_sid != FAIL), 
         "H5Screate_simple() file_large_ds_sid succeeded");

    mem_large_ds_process_slice_sid = H5Screate_simple(large_rank, dims, NULL);
    VRFY((mem_large_ds_process_slice_sid != FAIL), 
         "H5Screate_simple() mem_large_ds_process_slice_sid succeeded");

    file_large_ds_process_slice_sid = H5Screate_simple(large_rank, dims, NULL);
    VRFY((file_large_ds_process_slice_sid != FAIL), 
         "H5Screate_simple() file_large_ds_process_slice_sid succeeded");


    large_ds_slice_sid = H5Screate_simple(large_rank - 1, &(dims[1]), NULL);
    VRFY((large_ds_slice_sid != 0), 
         "H5Screate_simple() large_ds_slice_sid succeeded");


    /* Select the entire extent of the full small ds, and ds slice dataspaces */
    ret = H5Sselect_all(full_mem_small_ds_sid);
    VRFY((ret != FAIL), "H5Sselect_all(full_mem_small_ds_sid) succeeded");

    ret = H5Sselect_all(full_file_small_ds_sid);
    VRFY((ret != FAIL), "H5Sselect_all(full_file_small_ds_sid) succeeded");

    ret = H5Sselect_all(small_ds_slice_sid);
    VRFY((ret != FAIL), "H5Sselect_all(small_ds_slice_sid) succeeded");


    /* Select the entire extent of the full large ds, and ds slice dataspaces */
    ret = H5Sselect_all(full_mem_large_ds_sid);
    VRFY((ret != FAIL), "H5Sselect_all(full_mem_large_ds_sid) succeeded");

    ret = H5Sselect_all(full_file_large_ds_sid);
    VRFY((ret != FAIL), "H5Sselect_all(full_file_large_ds_sid) succeeded");

    ret = H5Sselect_all(large_ds_slice_sid);
    VRFY((ret != FAIL), "H5Sselect_all(large_ds_slice_sid) succeeded");


    /* if chunk edge size is greater than zero, set up the small and
     * large data set creation property lists to specify chunked
     * datasets.
     */
    if ( chunk_edge_size > 0 ) {

        chunk_dims[0] = mpi_size + 1;
        chunk_dims[1] = chunk_dims[2] = 
                        chunk_dims[3] = chunk_dims[4] = chunk_edge_size;

        small_ds_dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
        VRFY((ret != FAIL), "H5Pcreate() small_ds_dcpl_id succeeded");

        ret = H5Pset_layout(small_ds_dcpl_id, H5D_CHUNKED);
        VRFY((ret != FAIL), "H5Pset_layout() small_ds_dcpl_id succeeded");

        ret = H5Pset_chunk(small_ds_dcpl_id, small_rank, chunk_dims);
        VRFY((ret != FAIL), "H5Pset_chunk() small_ds_dcpl_id succeeded");


        large_ds_dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
        VRFY((ret != FAIL), "H5Pcreate() large_ds_dcpl_id succeeded");

        ret = H5Pset_layout(large_ds_dcpl_id, H5D_CHUNKED);
        VRFY((ret != FAIL), "H5Pset_layout() large_ds_dcpl_id succeeded");

        ret = H5Pset_chunk(large_ds_dcpl_id, large_rank, chunk_dims);
        VRFY((ret != FAIL), "H5Pset_chunk() large_ds_dcpl_id succeeded");
    }

    /* create the small dataset */
    small_dataset = H5Dcreate2(fid, "small_dataset", dset_type,
                               file_small_ds_sid, H5P_DEFAULT,
                               small_ds_dcpl_id, H5P_DEFAULT);
    VRFY((ret != FAIL), "H5Dcreate2() small_dataset succeeded");

    /* create the large dataset */
    large_dataset = H5Dcreate2(fid, "large_dataset", dset_type,
                               file_large_ds_sid, H5P_DEFAULT,
                               large_ds_dcpl_id, H5P_DEFAULT);
    VRFY((ret != FAIL), "H5Dcreate2() large_dataset succeeded");



    /* setup xfer property list */
    xfer_plist = H5Pcreate(H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "H5Pcreate(H5P_DATASET_XFER) succeeded");

    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");

    if ( ! use_collective_io ) {

        ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist,
                                              H5FD_MPIO_INDIVIDUAL_IO);
        VRFY((ret>= 0), "H5Pset_dxpl_mpio_collective_opt() suceeded");
    }

    /* setup selection to write initial data to the small and large data sets */
    start[0] = mpi_rank;
    stride[0] = 2 * (mpi_size + 1);
    count[0] = 1;
    block[0] = 1;

    for ( i = 1; i < large_rank; i++ ) {

        start[i] = 0;
        stride[i] = 2 * edge_size;
        count[i] = 1;
        block[i] = edge_size;
    }

    /* setup selections for writing initial data to the small data set */
    ret = H5Sselect_hyperslab(mem_small_ds_sid,
                              H5S_SELECT_SET,
                              start,
                              stride,
                              count,
                              block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(mem_small_ds_sid, set) suceeded");

    ret = H5Sselect_hyperslab(file_small_ds_sid,
                              H5S_SELECT_SET,
                              start,
                              stride,
                              count,
                              block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(file_small_ds_sid, set) suceeded");

    if ( MAINPROCESS ) { /* add an additional slice to the selections */

        start[0] = mpi_size;

        ret = H5Sselect_hyperslab(mem_small_ds_sid,
                                  H5S_SELECT_OR,
                                  start,
                                  stride,
                                  count,
                                  block);
        VRFY((ret>= 0), "H5Sselect_hyperslab(mem_small_ds_sid, or) suceeded");

        ret = H5Sselect_hyperslab(file_small_ds_sid,
                                  H5S_SELECT_OR,
                                  start,
                                  stride,
                                  count,
                                  block);
        VRFY((ret>= 0), "H5Sselect_hyperslab(file_small_ds_sid, or) suceeded");
    }


    /* write the initial value of the small data set to file */
    ret = H5Dwrite(small_dataset, dset_type, mem_small_ds_sid, file_small_ds_sid,
                   xfer_plist, small_ds_buf_0);

    VRFY((ret >= 0), "H5Dwrite() small_dataset initial write succeeded");


    /* sync with the other processes before checking data */
    mrc = MPI_Barrier(MPI_COMM_WORLD);
    VRFY((mrc==MPI_SUCCESS), "Sync after small dataset writes");


    /* read the small data set back to verify that it contains the 
     * expected data.  Note that each process reads in the entire 
     * data set.
     */
    ret = H5Dread(small_dataset,
                  H5T_NATIVE_UINT32,
                  full_mem_small_ds_sid,
                  full_file_small_ds_sid,
                  xfer_plist,
                  small_ds_buf_1);
    VRFY((ret >= 0), "H5Dread() small_dataset initial read succeeded");


    /* verify that the correct data was written to the small data set */
    expected_value = 0;
    mis_match = FALSE;
    ptr_1 = small_ds_buf_1;

    i = 0;
    for ( i = 0; i < (int)small_ds_size; i++ ) {

        if ( *ptr_1 != expected_value ) {

            mis_match = TRUE;
        }
        ptr_1++;
        expected_value++;
    }
    VRFY( (mis_match == FALSE), "small ds init data good.");



    /* setup selections for writing initial data to the large data set */

    start[0] = mpi_rank;

    ret = H5Sselect_hyperslab(mem_large_ds_sid,
                              H5S_SELECT_SET,
                              start,
                              stride,
                              count,
                              block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(mem_large_ds_sid, set) suceeded");

    ret = H5Sselect_hyperslab(file_large_ds_sid,
                              H5S_SELECT_SET,
                              start,
                              stride,
                              count,
                              block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(file_large_ds_sid, set) suceeded");
 
    /* In passing, setup the process slice data spaces as well */

    ret = H5Sselect_hyperslab(mem_large_ds_process_slice_sid,
                              H5S_SELECT_SET,
                              start,
                              stride,
                              count,
                              block);
    VRFY((ret >= 0), 
         "H5Sselect_hyperslab(mem_large_ds_process_slice_sid, set) suceeded");

    ret = H5Sselect_hyperslab(file_large_ds_process_slice_sid,
                              H5S_SELECT_SET,
                              start,
                              stride,
                              count,
                              block);
    VRFY((ret >= 0), 
         "H5Sselect_hyperslab(file_large_ds_process_slice_sid, set) suceeded");

    if ( MAINPROCESS ) { /* add an additional slice to the selections */

        start[0] = mpi_size;

        ret = H5Sselect_hyperslab(mem_large_ds_sid,
                                  H5S_SELECT_OR,
                                  start,
                                  stride,
                                  count,
                                  block);
        VRFY((ret>= 0), "H5Sselect_hyperslab(mem_large_ds_sid, or) suceeded");

        ret = H5Sselect_hyperslab(file_large_ds_sid,
                                  H5S_SELECT_OR,
                                  start,
                                  stride,
                                  count,
                                  block);
        VRFY((ret>= 0), "H5Sselect_hyperslab(file_large_ds_sid, or) suceeded");
    }


    /* write the initial value of the large data set to file */
    ret = H5Dwrite(large_dataset, dset_type, mem_large_ds_sid, file_large_ds_sid,
                   xfer_plist, large_ds_buf_0);
    if ( ret < 0 ) H5Eprint2(H5E_DEFAULT, stderr);
    VRFY((ret >= 0), "H5Dwrite() large_dataset initial write succeeded");


    /* sync with the other processes before checking data */
    mrc = MPI_Barrier(MPI_COMM_WORLD);
    VRFY((mrc==MPI_SUCCESS), "Sync after large dataset writes");


    /* read the small data set back to verify that it contains the 
     * expected data.  Note that each process reads in the entire 
     * data set.
     */
    ret = H5Dread(large_dataset,
                  H5T_NATIVE_UINT32,
                  full_mem_large_ds_sid,
                  full_file_large_ds_sid,
                  xfer_plist,
                  large_ds_buf_1);
    VRFY((ret >= 0), "H5Dread() large_dataset initial read succeeded");


    /* verify that the correct data was written to the small data set */
    expected_value = 0;
    mis_match = FALSE;
    ptr_1 = large_ds_buf_1;

    i = 0;
    for ( i = 0; i < (int)large_ds_size; i++ ) {

        if ( *ptr_1 != expected_value ) {

            mis_match = TRUE;
        }
        ptr_1++;
        expected_value++;
    }
    VRFY( (mis_match == FALSE), "large ds init data good.");


    /* first, verify that we can read from disk correctly using selections
     * of different rank that H5S_select_shape_same() views as being of the
     * same shape.
     *
     * Start by reading small_rank-D - 1 slice from the on disk large cube, 
     * and verifying that the data read is correct.  Verify that 
     * H5S_select_shape_same() returns true on the memory and file selections.
     */

    /* We have already done a H5Sselect_all() on the data space 
     * small_ds_slice_sid, so no need to call H5Sselect_all() again.
     */

    /* set up start, stride, count, and block -- note that we will
     * change start[] so as to read slices of the large cube.
     */
    for ( i = 0; i < PAR_SS_DR_MAX_RANK; i++ ) {

        start[i] = 0;
        stride[i] = 2 * edge_size;
        count[i] = 1;
        if ( (PAR_SS_DR_MAX_RANK - i) > (small_rank - 1) ) {

            block[i] = 1;

        } else {

            block[i] = edge_size;
        }
    }

    /* zero out the buffer we will be reading into */
    ptr_0 = small_ds_slice_buf;

    for ( i = 0; i < (int)small_ds_slice_size; i++ ) {

	*ptr_0 = (uint32_t)0;
        ptr_0++;
    }

#if CONTIG_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG 
    HDfprintf(stdout, 
              "%s reading slices from big cube on disk into small cube slice.\n",
              fcnName);
#endif 
    /* in serial versions of this test, we loop through all the dimensions
     * of the large data set.  However, in the parallel version, each 
     * process only works with that slice of the large cube indicated
     * by its rank -- hence we set the most slowly changing index to 
     * mpi_rank, and don't itterate over it.
     */

    if ( PAR_SS_DR_MAX_RANK - large_rank == 0 ) {

        i = mpi_rank;

    } else {

        i = 0;
    }

    /* since large_rank is at most PAR_SS_DR_MAX_RANK, no need to 
     * loop over it -- either we are setting i to mpi_rank, or
     * we are setting it to zero.  It will not change during the 
     * test.
     */

    if ( PAR_SS_DR_MAX_RANK - large_rank == 1 ) {

        j = mpi_rank;

    } else {

        j = 0;
    }

    do {
        if ( PAR_SS_DR_MAX_RANK - large_rank == 2 ) {

            k = mpi_rank;

        } else {

            k = 0;
        }

        do {
            /* since small rank >= 2 and large_rank > small_rank, we 
             * have large_rank >= 3.  Since PAR_SS_DR_MAX_RANK == 5
             * (baring major re-orgaization), this gives us:
             *
             *     (PAR_SS_DR_MAX_RANK - large_rank) <= 2
             *
             * so no need to repeat the test in the outer loops --
             * just set l = 0.
             */

            l = 0;
            do {
                /* we know that small_rank - 1 >= 1 and that 
                 * large_rank > small_rank by the assertions at the head 
                 * of this function.  Thus no need for another inner loop.
                 */
                start[0] = i;
                start[1] = j;
                start[2] = k;
                start[3] = l;
                start[4] = 0;

                ret = H5Sselect_hyperslab(file_large_ds_sid,
                                          H5S_SELECT_SET,
                                          start_ptr,
                                          stride_ptr,
                                          count_ptr,
                                          block_ptr);
                VRFY((ret != FAIL), 
                     "H5Sselect_hyperslab(file_large_cube_sid) succeeded");


                /* verify that H5S_select_shape_same() reports the two
                 * selections as having the same shape.
                 */
                check = H5S_select_shape_same_test(small_ds_slice_sid,
                                                   file_large_ds_sid);
                VRFY((check == TRUE), "H5S_select_shape_same_test passed");


                /* Read selection from disk */
#if CONTIG_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG 
                HDfprintf(stdout, "%s:%d: start = %d %d %d %d %d.\n", 
                          fcnName, (int)mpi_rank, 
                          (int)start[0], (int)start[1], (int)start[2], 
                          (int)start[3], (int)start[4]);
                HDfprintf(stdout, "%s slice/file extent dims = %d/%d.\n",
                          fcnName,
                          H5Sget_simple_extent_ndims(small_ds_slice_sid),
                          H5Sget_simple_extent_ndims(file_large_ds_sid));
#endif 
                ret = H5Dread(large_dataset,
                              H5T_NATIVE_UINT32,
                              small_ds_slice_sid,
                              file_large_ds_sid,
                              xfer_plist,
                              small_ds_slice_buf);
                VRFY((ret >= 0), "H5Sread() slice from large ds succeeded.");


                /* verify that expected data is retrieved */

                mis_match = FALSE;
                ptr_1 = small_ds_slice_buf;
                expected_value = 
			(i * edge_size * edge_size * edge_size * edge_size) +
                        (j * edge_size * edge_size * edge_size) +
                        (k * edge_size * edge_size) +
                        (l * edge_size);

                for ( n = 0; n < (int)small_ds_slice_size; n++ ) {

                    if ( *ptr_1 != expected_value ) {

                        mis_match = TRUE;
                    }

                    *ptr_1 = 0; /* zero data for next use */

                    ptr_1++;
                    expected_value++;
                }

                VRFY((mis_match == FALSE), 
                     "small slice read from large ds data good.");
                l++;

            } while ( ( large_rank > 2 ) &&
                      ( (small_rank - 1) <= 1 ) &&
                      ( l < edge_size ) );
            k++;
        } while ( ( large_rank > 3 ) &&
                  ( (small_rank - 1) <= 2 ) &&
                  ( k < edge_size ) );
        j++;
    } while ( ( large_rank > 4 ) &&
              ( (small_rank - 1) <= 3 ) &&
              ( j < edge_size ) );


    /* similarly, read slices of the on disk small data set into slices 
     * through the in memory large data set, and verify that the correct 
     * data (and only the correct data) is read.
     */

    start[0] = mpi_rank;
    stride[0] = 2 * (mpi_size + 1);
    count[0] = 1;
    block[0] = 1;

    for ( i = 1; i < large_rank; i++ ) {

        start[i] = 0;
        stride[i] = 2 * edge_size;
        count[i] = 1;
        block[i] = edge_size;
    }

    ret = H5Sselect_hyperslab(file_small_ds_sid,
                              H5S_SELECT_SET,
                              start,
                              stride,
                              count,
                              block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(file_small_ds_sid, set) suceeded");


#if CONTIG_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG 
    HDfprintf(stdout, 
      "%s reading slices of on disk small data set into slices of big data set.\n",
              fcnName);
#endif 

    /* zero out the in memory large ds */
    ptr_1 = large_ds_buf_1;
    for ( n = 0; n < (int)large_ds_size; n++ ) {

        *ptr_1 = 0;
        ptr_1++;
    }

    /* set up start, stride, count, and block -- note that we will
     * change start[] so as to read slices of the large cube.
     */
    for ( i = 0; i < PAR_SS_DR_MAX_RANK; i++ ) {

        start[i] = 0;
        stride[i] = 2 * edge_size;
        count[i] = 1;
        if ( (PAR_SS_DR_MAX_RANK - i) > (small_rank - 1) ) {

            block[i] = 1;

        } else {

            block[i] = edge_size;
        }
    }


    /* in serial versions of this test, we loop through all the dimensions
     * of the large data set that don't appear in the small data set.  
     *
     * However, in the parallel version, each process only works with that 
     * slice of the large (and small) data set indicated by its rank -- hence 
     * we set the most slowly changing index to mpi_rank, and don't itterate 
     * over it.
     */


    if ( PAR_SS_DR_MAX_RANK - large_rank == 0 ) {

        i = mpi_rank;

    } else {

        i = 0;
    }

    /* since large_rank is at most PAR_SS_DR_MAX_RANK, no need to 
     * loop over it -- either we are setting i to mpi_rank, or
     * we are setting it to zero.  It will not change during the 
     * test.
     */

    if ( PAR_SS_DR_MAX_RANK - large_rank == 1 ) {

        j = mpi_rank;

    } else {

        j = 0;
    }

    do {
        if ( PAR_SS_DR_MAX_RANK - large_rank == 2 ) {

            k = mpi_rank;

        } else {

            k = 0;
        }

        do {
            /* since small rank >= 2 and large_rank > small_rank, we 
             * have large_rank >= 3.  Since PAR_SS_DR_MAX_RANK == 5
             * (baring major re-orgaization), this gives us:
             *
             *     (PAR_SS_DR_MAX_RANK - large_rank) <= 2
             *
             * so no need to repeat the test in the outer loops --
             * just set l = 0.
             */

            l = 0;
            do {
                /* we know that small_rank >= 1 and that large_rank > small_rank
                 * by the assertions at the head of this function.  Thus no
                 * need for another inner loop.
                 */
                start[0] = i;
                start[1] = j;
                start[2] = k;
                start[3] = l;
                start[4] = 0;

                ret = H5Sselect_hyperslab(mem_large_ds_sid,
                                          H5S_SELECT_SET,
                                          start_ptr,
                                          stride_ptr,
                                          count_ptr,
                                          block_ptr);
                VRFY((ret != FAIL), 
                     "H5Sselect_hyperslab(mem_large_ds_sid) succeeded");


                /* verify that H5S_select_shape_same() reports the two
                 * selections as having the same shape.
                 */
                check = H5S_select_shape_same_test(file_small_ds_sid,
                                                   mem_large_ds_sid);
                VRFY((check == TRUE), "H5S_select_shape_same_test passed");


                /* Read selection from disk */
#if CONTIG_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG 
                HDfprintf(stdout, "%s:%d: start = %d %d %d %d %d.\n", 
                          fcnName, (int)mpi_rank, 
                          (int)start[0], (int)start[1], (int)start[2], 
                          (int)start[3], (int)start[4]);
                HDfprintf(stdout, "%s:%d: mem/file extent dims = %d/%d.\n",
                          fcnName, mpi_rank,
                          H5Sget_simple_extent_ndims(mem_large_ds_sid),
                          H5Sget_simple_extent_ndims(file_small_ds_sid));
#endif 
                ret = H5Dread(small_dataset,
                              H5T_NATIVE_UINT32,
                              mem_large_ds_sid,
                              file_small_ds_sid,
                              xfer_plist,
                              large_ds_buf_1);
                VRFY((ret >= 0), "H5Sread() slice from small ds succeeded.");

                /* verify that the expected data and only the
                 * expected data was read.
                 */
                ptr_1 = large_ds_buf_1;
                expected_value = mpi_rank * small_ds_slice_size;
                start_index = 
                        (i * edge_size * edge_size * edge_size * edge_size) +
                        (j * edge_size * edge_size * edge_size) +
                        (k * edge_size * edge_size) +
                        (l * edge_size);
                stop_index = start_index + (int)small_ds_slice_size - 1;

                HDassert( 0 <= start_index );
                HDassert( start_index < stop_index );
                HDassert( stop_index <= (int)large_ds_size );

                for ( n = 0; n < (int)large_ds_size; n++ ) {

                    if ( ( n >= start_index ) && ( n <= stop_index ) ) {

                        if ( *ptr_1 != expected_value ) {

                            mis_match = TRUE;
                        }
                        expected_value++;

                    } else {

                        if ( *ptr_1 != 0 ) {

                            mis_match = TRUE;
                        }
                    }
                    /* zero out the value for the next pass */
                    *ptr_1 = 0;

                    ptr_1++;
                }

                VRFY((mis_match == FALSE), 
                     "small slice read from large ds data good.");

                l++;

            } while ( ( large_rank > 2 ) &&
                      ( (small_rank - 1) <= 1 ) &&
                      ( l < edge_size ) );
            k++;
        } while ( ( large_rank > 3 ) &&
                  ( (small_rank - 1) <= 2 ) &&
                  ( k < edge_size ) );
        j++;
    } while ( ( large_rank > 4 ) &&
              ( (small_rank - 1) <= 3 ) &&
              ( j < edge_size ) );


    /* now we go in the opposite direction, verifying that we can write
     * from memory to file using selections of different rank that
     * H5S_select_shape_same() views as being of the same shape.
     *
     * Start by writing small_rank - 1 D slices from the in memory large data
     * set to the on disk small cube dataset.  After each write, read the 
     * slice of the small dataset back from disk, and verify that it contains 
     * the expected data. Verify that H5S_select_shape_same() returns true on 
     * the memory and file selections.
     */

    start[0] = mpi_rank;
    stride[0] = 2 * (mpi_size + 1);
    count[0] = 1;
    block[0] = 1;

    for ( i = 1; i < large_rank; i++ ) {

        start[i] = 0;
        stride[i] = 2 * edge_size;
        count[i] = 1;
        block[i] = edge_size;
    }

    ret = H5Sselect_hyperslab(file_small_ds_sid,
                              H5S_SELECT_SET,
                              start,
                              stride,
                              count,
                              block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(file_small_ds_sid, set) suceeded");

    ret = H5Sselect_hyperslab(mem_small_ds_sid,
                              H5S_SELECT_SET,
                              start,
                              stride,
                              count,
                              block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(mem_small_ds_sid, set) suceeded");


    /* set up start, stride, count, and block -- note that we will
     * change start[] so as to read slices of the large cube.
     */
    for ( i = 0; i < PAR_SS_DR_MAX_RANK; i++ ) {

        start[i] = 0;
        stride[i] = 2 * edge_size;
        count[i] = 1;
        if ( (PAR_SS_DR_MAX_RANK - i) > (small_rank - 1) ) {

            block[i] = 1;

        } else {

            block[i] = edge_size;
        }
    }

    /* zero out the in memory small ds */
    ptr_1 = small_ds_buf_1;
    for ( n = 0; n < (int)small_ds_size; n++ ) {

        *ptr_1 = 0;
        ptr_1++;
    }


#if CONTIG_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG 
    HDfprintf(stdout, 
              "%s writing slices from big ds to slices of small ds on disk.\n",
              fcnName);
#endif 

    /* in serial versions of this test, we loop through all the dimensions
     * of the large data set that don't appear in the small data set.  
     *
     * However, in the parallel version, each process only works with that 
     * slice of the large (and small) data set indicated by its rank -- hence 
     * we set the most slowly changing index to mpi_rank, and don't itterate 
     * over it.
     */


    if ( PAR_SS_DR_MAX_RANK - large_rank == 0 ) {

        i = mpi_rank;

    } else {

        i = 0;
    }

    /* since large_rank is at most PAR_SS_DR_MAX_RANK, no need to 
     * loop over it -- either we are setting i to mpi_rank, or
     * we are setting it to zero.  It will not change during the 
     * test.
     */

    if ( PAR_SS_DR_MAX_RANK - large_rank == 1 ) {

        j = mpi_rank;

    } else {

        j = 0;
    }

    j = 0;
    do {
        if ( PAR_SS_DR_MAX_RANK - large_rank == 2 ) {

            k = mpi_rank;

        } else {

            k = 0;
        }

        do {
            /* since small rank >= 2 and large_rank > small_rank, we 
             * have large_rank >= 3.  Since PAR_SS_DR_MAX_RANK == 5
             * (baring major re-orgaization), this gives us:
             *
             *     (PAR_SS_DR_MAX_RANK - large_rank) <= 2
             *
             * so no need to repeat the test in the outer loops --
             * just set l = 0.
             */

            l = 0;
            do {
                /* we know that small_rank >= 1 and that large_rank > small_rank
                 * by the assertions at the head of this function.  Thus no
                 * need for another inner loop.
                 */

                /* zero out this rank's slice of the on disk small data set */
                ret = H5Dwrite(small_dataset,
                               H5T_NATIVE_UINT32,
                               mem_small_ds_sid,
                               file_small_ds_sid,
                               xfer_plist,
                               small_ds_buf_2);
                VRFY((ret >= 0), "H5Dwrite() zero slice to small ds succeeded.");

                /* select the portion of the in memory large cube from which we
                 * are going to write data.
                 */
                start[0] = i;
                start[1] = j;
                start[2] = k;
                start[3] = l;
                start[4] = 0;

                ret = H5Sselect_hyperslab(mem_large_ds_sid,
                                          H5S_SELECT_SET,
                                          start_ptr,
                                          stride_ptr,
                                          count_ptr,
                                          block_ptr);
                VRFY((ret >= 0), 
                     "H5Sselect_hyperslab() mem_large_ds_sid succeeded.");


                /* verify that H5S_select_shape_same() reports the in
                 * memory slice through the cube selection and the
                 * on disk full square selections as having the same shape.
                 */
                check = H5S_select_shape_same_test(file_small_ds_sid,
                                                   mem_large_ds_sid);
                VRFY((check == TRUE), "H5S_select_shape_same_test passed.");


                /* write the slice from the in memory large data set to the 
                 * slice of the on disk small dataset. */
#if CONTIG_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG 
                HDfprintf(stdout, "%s:%d: start = %d %d %d %d %d.\n", 
                          fcnName, (int)mpi_rank,
                          (int)start[0], (int)start[1], (int)start[2], 
                          (int)start[3], (int)start[4]);
                HDfprintf(stdout, "%s:%d: mem/file extent dims = %d/%d.\n",
                          fcnName, mpi_rank,
                          H5Sget_simple_extent_ndims(mem_large_ds_sid),
                          H5Sget_simple_extent_ndims(file_small_ds_sid));
#endif 
                ret = H5Dwrite(small_dataset,
                               H5T_NATIVE_UINT32,
                               mem_large_ds_sid,
                               file_small_ds_sid,
                               xfer_plist,
                               large_ds_buf_0);
                VRFY((ret >= 0), "H5Dwrite() slice to large ds succeeded.");


                /* read the on disk square into memory */
                ret = H5Dread(small_dataset,
                              H5T_NATIVE_UINT32,
                              mem_small_ds_sid,
                              file_small_ds_sid,
                              xfer_plist,
                              small_ds_buf_1);
                VRFY((ret >= 0), "H5Dread() slice from small ds succeeded.");


                /* verify that expected data is retrieved */

                mis_match = FALSE;
                ptr_1 = small_ds_buf_1;

                expected_value = 
			(i * edge_size * edge_size * edge_size * edge_size) +
                        (j * edge_size * edge_size * edge_size) +
                        (k * edge_size * edge_size) +
                        (l * edge_size);

                start_index = mpi_rank * small_ds_slice_size;
                stop_index = start_index + small_ds_slice_size - 1;

                HDassert( 0 <= start_index );
                HDassert( start_index < stop_index );
                HDassert( stop_index <= (int)small_ds_size );

                for ( n = 0; n < (int)small_ds_size; n++ ) {

                    if ( ( n >= start_index ) && ( n <= stop_index ) ) {

                        if ( *ptr_1 != expected_value ) {

                            mis_match = TRUE;
                        }
                        expected_value++;

                    } else {

                        if ( *ptr_1 != 0 ) {

                            mis_match = TRUE;
                        }
                    }
                    /* zero out the value for the next pass */
                    *ptr_1 = 0;

                    ptr_1++;
                }

                VRFY((mis_match == FALSE), 
                     "small slice write from large ds data good.");

                l++;

            } while ( ( large_rank > 2 ) &&
                      ( (small_rank - 1) <= 1 ) &&
                      ( l < edge_size ) );
            k++;
        } while ( ( large_rank > 3 ) &&
                  ( (small_rank - 1) <= 2 ) &&
                  ( k < edge_size ) );
        j++;
    } while ( ( large_rank > 4 ) &&
              ( (small_rank - 1) <= 3 ) &&
              ( j < edge_size ) );


    /* Now write the contents of the process's slice of the in memory 
     * small data set to slices of the on disk large data set.  After 
     * each write, read the process's slice of the large data set back
     * into memory, and verify that it contains the expected data. 
     * Verify that H5S_select_shape_same() returns true on the memory 
     * and file selections.
     */

    /* select the slice of the in memory small data set associated with 
     * the process's mpi rank.
     */
    start[0] = mpi_rank;
    stride[0] = 2 * (mpi_size + 1);
    count[0] = 1;
    block[0] = 1;

    for ( i = 1; i < large_rank; i++ ) {

        start[i] = 0;
        stride[i] = 2 * edge_size;
        count[i] = 1;
        block[i] = edge_size;
    }

    ret = H5Sselect_hyperslab(mem_small_ds_sid,
                              H5S_SELECT_SET,
                              start,
                              stride,
                              count,
                              block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(mem_small_ds_sid, set) suceeded");


    /* set up start, stride, count, and block -- note that we will
     * change start[] so as to write slices of the small data set to
     * slices of the large data set.
     */
    for ( i = 0; i < PAR_SS_DR_MAX_RANK; i++ ) {

        start[i] = 0;
        stride[i] = 2 * edge_size;
        count[i] = 1;
        if ( (PAR_SS_DR_MAX_RANK - i) > (small_rank - 1) ) {

            block[i] = 1;

        } else {

            block[i] = edge_size;
        }
    }

    /* zero out the in memory large ds */
    ptr_1 = large_ds_buf_1;
    for ( n = 0; n < (int)large_ds_size; n++ ) {

        *ptr_1 = 0;
        ptr_1++;
    }

#if CONTIG_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG 
    HDfprintf(stdout, 
         "%s writing process slices of small ds to slices of large ds on disk.\n",
         fcnName);
#endif 

    if ( PAR_SS_DR_MAX_RANK - large_rank == 0 ) {

        i = mpi_rank;

    } else {

        i = 0;
    }

    /* since large_rank is at most PAR_SS_DR_MAX_RANK, no need to 
     * loop over it -- either we are setting i to mpi_rank, or
     * we are setting it to zero.  It will not change during the 
     * test.
     */

    if ( PAR_SS_DR_MAX_RANK - large_rank == 1 ) {

        j = mpi_rank;

    } else {

        j = 0;
    }

    do {
        if ( PAR_SS_DR_MAX_RANK - large_rank == 2 ) {

            k = mpi_rank;

        } else {

            k = 0;
        }

        do {
            /* since small rank >= 2 and large_rank > small_rank, we 
             * have large_rank >= 3.  Since PAR_SS_DR_MAX_RANK == 5
             * (baring major re-orgaization), this gives us:
             *
             *     (PAR_SS_DR_MAX_RANK - large_rank) <= 2
             *
             * so no need to repeat the test in the outer loops --
             * just set l = 0.
             */

            l = 0;
            do {
                /* we know that small_rank >= 1 and that large_rank > small_rank
                 * by the assertions at the head of this function.  Thus no
                 * need for another inner loop.
                 */

                /* Zero out this processes slice of the on disk large data set.
                 * Note that this will leave one slice with its original data
                 * as there is one more slice than processes.
                  */
                ret = H5Dwrite(large_dataset,
                               H5T_NATIVE_UINT32,
                               large_ds_slice_sid,
                               file_large_ds_process_slice_sid,
                               xfer_plist,
                               large_ds_buf_2);
		VRFY((ret != FAIL), "H5Dwrite() to zero large ds suceeded");


                /* select the portion of the in memory large cube to which we
                 * are going to write data.
                 */
                start[0] = i;
                start[1] = j;
                start[2] = k;
                start[3] = l;
                start[4] = 0;

                ret = H5Sselect_hyperslab(file_large_ds_sid,
                                          H5S_SELECT_SET,
                                          start_ptr,
                                          stride_ptr,
                                          count_ptr,
                                          block_ptr);
		VRFY((ret != FAIL), 
                     "H5Sselect_hyperslab() target large ds slice succeeded");


                /* verify that H5S_select_shape_same() reports the in
                 * memory small data set slice selection and the
                 * on disk slice through the large data set selection
                 * as having the same shape.
                 */
                check = H5S_select_shape_same_test(mem_small_ds_sid,
                                                   file_large_ds_sid);
		VRFY((check == TRUE), "H5S_select_shape_same_test passed");


                /* write the small data set slice from memory to the 
                 * target slice of the disk data set 
                 */
#if CONTIG_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG 
                HDfprintf(stdout, "%s:%d: start = %d %d %d %d %d.\n", 
                          fcnName, (int)mpi_rank,
                          (int)start[0], (int)start[1], (int)start[2], 
                          (int)start[3], (int)start[4]);
                HDfprintf(stdout, "%s:%d: mem/file extent dims = %d/%d.\n",
                          fcnName, mpi_rank,
                          H5Sget_simple_extent_ndims(mem_small_ds_sid),
                          H5Sget_simple_extent_ndims(file_large_ds_sid));
#endif 
                ret = H5Dwrite(large_dataset,
                               H5T_NATIVE_UINT32,
                               mem_small_ds_sid,
                               file_large_ds_sid,
                               xfer_plist,
                               small_ds_buf_0);
		VRFY((ret != FAIL), 
                      "H5Dwrite of small ds slice to large ds succeeded");


                /* read this processes slice on the on disk large 
                 * data set into memory.
                 */

                ret = H5Dread(large_dataset,
                              H5T_NATIVE_UINT32,
                              mem_large_ds_process_slice_sid,
                              file_large_ds_process_slice_sid,
                              xfer_plist,
                              large_ds_buf_1);
                VRFY((ret != FAIL), 
                     "H5Dread() of process slice of large ds succeeded");


                /* verify that the expected data and only the
                 * expected data was read.
                 */
                ptr_1 = large_ds_buf_1;
                expected_value = (uint32_t)(mpi_rank) * small_ds_slice_size;


                start_index = (i * edge_size * edge_size * edge_size * edge_size) +
                              (j * edge_size * edge_size * edge_size) +
                              (k * edge_size * edge_size) +
                              (l * edge_size);
                stop_index = start_index + (int)small_ds_slice_size - 1;

                HDassert( 0 <= start_index );
                HDassert( start_index < stop_index );
                HDassert( stop_index < (int)large_ds_size );

                for ( n = 0; n < (int)large_ds_size; n++ ) {

                    if ( ( n >= start_index ) && ( n <= stop_index ) ) {

                        if ( *ptr_1 != expected_value ) {

                            mis_match = TRUE;
                        }

                        expected_value++;

                    } else {

                        if ( *ptr_1 != 0 ) {

                            mis_match = TRUE;
                        }
                    }
                    /* zero out buffer for next test */
                    *ptr_1 = 0;
                    ptr_1++;
                }

                VRFY((mis_match == FALSE), 
                     "small ds slice write to large ds slice data good.");

                l++;

            } while ( ( large_rank > 2 ) &&
                      ( (small_rank - 1) <= 1 ) &&
                      ( l < edge_size ) );
            k++;
        } while ( ( large_rank > 3 ) &&
                  ( (small_rank - 1) <= 2 ) &&
                  ( k < edge_size ) );
        j++;
    } while ( ( large_rank > 4 ) &&
              ( (small_rank - 1) <= 3 ) &&
              ( j < edge_size ) );


    /* Close dataspaces */
    ret = H5Sclose(full_mem_small_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(full_mem_small_ds_sid) succeeded");

    ret = H5Sclose(full_file_small_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(full_file_small_ds_sid) succeeded");

    ret = H5Sclose(mem_small_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(mem_small_ds_sid) succeeded");

    ret = H5Sclose(file_small_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(file_small_ds_sid) succeeded");

    ret = H5Sclose(small_ds_slice_sid);
    VRFY((ret != FAIL), "H5Sclose(small_ds_slice_sid) succeeded");

    ret = H5Sclose(full_mem_large_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(full_mem_large_ds_sid) succeeded");

    ret = H5Sclose(full_file_large_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(full_file_large_ds_sid) succeeded");

    ret = H5Sclose(mem_large_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(mem_large_ds_sid) succeeded");

    ret = H5Sclose(file_large_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(mem_large_ds_sid) succeeded");

    ret = H5Sclose(mem_large_ds_process_slice_sid);
    VRFY((ret != FAIL), "H5Sclose(mem_large_ds_process_slice_sid) succeeded");

    ret = H5Sclose(file_large_ds_process_slice_sid);
    VRFY((ret != FAIL), "H5Sclose(file_large_ds_process_slice_sid) succeeded");

    ret = H5Sclose(large_ds_slice_sid);
    VRFY((ret != FAIL), "H5Sclose(large_ds_slice_sid) succeeded");


    /* Close Datasets */
    ret = H5Dclose(small_dataset);
    VRFY((ret != FAIL), "H5Dclose(small_dataset) succeeded");

    ret = H5Dclose(large_dataset);
    VRFY((ret != FAIL), "H5Dclose(large_dataset) succeeded");


    /* close the file collectively */
    MESG("about to close file.");
    ret = H5Fclose(fid);
    VRFY((ret != FAIL), "file close succeeded");

    /* Free memory buffers */

    if ( small_ds_buf_0 != NULL ) HDfree(small_ds_buf_0);
    if ( small_ds_buf_1 != NULL ) HDfree(small_ds_buf_1);
    if ( small_ds_buf_2 != NULL ) HDfree(small_ds_buf_2);
    if ( small_ds_slice_buf != NULL ) HDfree(small_ds_slice_buf);

    if ( large_ds_buf_0 != NULL ) HDfree(large_ds_buf_0);
    if ( large_ds_buf_1 != NULL ) HDfree(large_ds_buf_1);
    if ( large_ds_buf_2 != NULL ) HDfree(large_ds_buf_2);
    if ( large_ds_slice_buf != NULL ) HDfree(large_ds_slice_buf);

    return;

} /* contig_hyperslab_dr_pio_test__run_test() */


/*-------------------------------------------------------------------------
 * Function:	contig_hyperslab_dr_pio_test()
 *
 * Purpose:	Test I/O to/from hyperslab selections of different rank in
 *		the parallel case.
 *
 * Return:	void
 *
 * Programmer:	JRM -- 9/18/09
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

void
contig_hyperslab_dr_pio_test(void)
{
    int	        test_num = 0;
    int		edge_size = 10;
    int		chunk_edge_size = 0;
    int	        small_rank;
    int	        large_rank;
    int  	use_collective_io;
    hid_t	dset_type = H5T_STD_U32LE;

    for ( large_rank = 3; large_rank <= PAR_SS_DR_MAX_RANK; large_rank++ ) {

        for ( small_rank = 2; small_rank < large_rank; small_rank++ ) {

            for ( use_collective_io = 0; 
                  use_collective_io <= 1; 
                  use_collective_io++ ) {

                chunk_edge_size = 0;
                contig_hyperslab_dr_pio_test__run_test(test_num,
                                                       edge_size,
                                                       chunk_edge_size,
                                                       small_rank,
                                                       large_rank,
                                                       (hbool_t)use_collective_io,
                                                       dset_type);
                test_num++;
                chunk_edge_size = 5;
                contig_hyperslab_dr_pio_test__run_test(test_num,
                                                       edge_size,
                                                       chunk_edge_size,
                                                       small_rank,
                                                       large_rank,
                                                       (hbool_t)use_collective_io,
                                                       dset_type);
                test_num++;
            }
        }
    }

    return;

} /* contig_hyperslab_dr_pio_test() */


/****************************************************************
**
**  checker_board_hyperslab_dr_pio_test__select_checker_board():  
**	Given a data space of tgt_rank, and dimensions:
**
**		(mpi_size + 1), edge_size, ... , edge_size
**
**	edge_size, and a checker_edge_size, select a checker
**	board selection of a sel_rank (sel_rank < tgt_rank) 
**	dimensional slice through the data space parallel to the 
**      sel_rank fastest changing indicies, with origin (in the
**	higher indicies) as indicated by the start array.
**
**	Note that this function, like all its relatives, is
**	hard coded to presume a maximum data space rank of 5.
**	While this maximum is declared as a constant, increasing
**	it will require extensive coding in addition to changing
**      the value of the constant.
**
**					JRM -- 10/8/09
**
****************************************************************/

#define CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__SELECT_CHECKER_BOARD__DEBUG 0

static void
checker_board_hyperslab_dr_pio_test__select_checker_board(
                                 const int mpi_rank,
                                 const hid_t tgt_sid,
                                 const int tgt_rank,
                                 const int edge_size,
                                 const int checker_edge_size,
                                 const int sel_rank,
                                 hsize_t sel_start[])
{
#if CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__SELECT_CHECKER_BOARD__DEBUG 
    const char *	fcnName = 
			"checker_board_hyperslab_dr_pio_test__select_checker_board():";
#endif 
    hbool_t		first_selection = TRUE;
    int                 i, j, k, l, m;
    int			n_cube_offset;
    int			sel_offset;
    const int		test_max_rank = PAR_SS_DR_MAX_RANK;  /* must update code if */
                                                             /* this changes        */
    hsize_t		base_count;
    hsize_t             offset_count;
    hsize_t     	start[PAR_SS_DR_MAX_RANK];
    hsize_t     	stride[PAR_SS_DR_MAX_RANK];
    hsize_t     	count[PAR_SS_DR_MAX_RANK];
    hsize_t     	block[PAR_SS_DR_MAX_RANK];
    herr_t      	ret;            /* Generic return value */

    HDassert( edge_size >= 6 );
    HDassert( 0 < checker_edge_size );
    HDassert( checker_edge_size <= edge_size );
    HDassert( 0 < sel_rank );
    HDassert( sel_rank <= tgt_rank );
    HDassert( tgt_rank <= test_max_rank );
    HDassert( test_max_rank <= PAR_SS_DR_MAX_RANK );

    sel_offset = test_max_rank - sel_rank;
    HDassert( sel_offset >= 0 );

    n_cube_offset = test_max_rank - tgt_rank;
    HDassert( n_cube_offset >= 0 );
    HDassert( n_cube_offset <= sel_offset );

#if CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__SELECT_CHECKER_BOARD__DEBUG 
    HDfprintf(stdout, "%s:%d: edge_size/checker_edge_size = %d/%d\n",
              fcnName, mpi_rank, edge_size, checker_edge_size);
    HDfprintf(stdout, "%s:%d: sel_rank/sel_offset = %d/%d.\n", 
              fcnName, mpi_rank, sel_rank, sel_offset);
    HDfprintf(stdout, "%s:%d: tgt_rank/n_cube_offset = %d/%d.\n", 
              fcnName, mpi_rank, tgt_rank, n_cube_offset);
#endif /* CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__SELECT_CHECKER_BOARD__DEBUG */ 

    /* First, compute the base count (which assumes start == 0
     * for the associated offset) and offset_count (which
     * assumes start == checker_edge_size for the associated
     * offset).
     *
     * Note that the following computation depends on the C99
     * requirement that integer division discard any fraction
     * (truncation towards zero) to function correctly. As we
     * now require C99, this shouldn't be a problem, but noting
     * it may save us some pain if we are ever obliged to support
     * pre-C99 compilers again.
     */

    base_count = edge_size / (checker_edge_size * 2);

    if ( (edge_size % (checker_edge_size * 2)) > 0 ) {

        base_count++;
    }

    offset_count = (edge_size - checker_edge_size) / (checker_edge_size * 2);

    if ( ((edge_size - checker_edge_size) % (checker_edge_size * 2)) > 0 ) {

        offset_count++;
    }

    /* Now set up the stride and block arrays, and portions of the start
     * and count arrays that will not be altered during the selection of 
     * the checker board.
     */
    i = 0;
    while ( i < n_cube_offset ) {

        /* these values should never be used */
        start[i] = 0;
        stride[i] = 0;
        count[i] = 0;
        block[i] = 0;

        i++;
    }

    while ( i < sel_offset ) {

        start[i] = sel_start[i];
        stride[i] = 2 * edge_size;
        count[i] = 1;
        block[i] = 1;

        i++;
    }

    while ( i < test_max_rank ) {

        stride[i] = 2 * checker_edge_size;
        block[i] = checker_edge_size;

        i++;
    }
   
    i = 0;
    do {
        if ( 0 >= sel_offset ) {

            if ( i == 0 ) {

                start[0] = 0;
                count[0] = base_count;

            } else {

                start[0] = checker_edge_size;
                count[0] = offset_count;

            }
        }

        j = 0;
        do { 
            if ( 1 >= sel_offset ) {

                if ( j == 0 ) {

                    start[1] = 0;
                    count[1] = base_count;

                } else {

                    start[1] = checker_edge_size;
                    count[1] = offset_count;

                }
            }

            k = 0;
            do {
                if ( 2 >= sel_offset ) {

                    if ( k == 0 ) {

                        start[2] = 0;
                        count[2] = base_count;

                    } else {

                        start[2] = checker_edge_size;
                        count[2] = offset_count;

                    }
                }

                l = 0;
                do {
                    if ( 3 >= sel_offset ) {

                        if ( l == 0 ) {

                            start[3] = 0;
                            count[3] = base_count;

                        } else {

                            start[3] = checker_edge_size;
                            count[3] = offset_count;

                        }
                    }

                    m = 0;
                    do {
                        if ( 4 >= sel_offset ) {

                            if ( m == 0 ) {

                                start[4] = 0;
                                count[4] = base_count;

                            } else {

                                start[4] = checker_edge_size;
                                count[4] = offset_count;

                            }
                        }

                        if ( ((i + j + k + l + m) % 2) == 0 ) {

#if CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__SELECT_CHECKER_BOARD__DEBUG 
                            HDfprintf(stdout, "%s%d: *** first_selection = %d ***\n", 
                                      fcnName, mpi_rank, (int)first_selection);
                            HDfprintf(stdout, "%s:%d: i/j/k/l/m = %d/%d/%d/%d/%d\n",
                                      fcnName, mpi_rank, i, j, k, l, m);
                            HDfprintf(stdout, 
                                      "%s:%d: start = %d %d %d %d %d.\n", 
                                      fcnName, mpi_rank, (int)start[0], (int)start[1], 
                                      (int)start[2], (int)start[3], (int)start[4]);
                            HDfprintf(stdout, 
                                      "%s:%d: stride = %d %d %d %d %d.\n", 
                                      fcnName, mpi_rank, (int)stride[0], (int)stride[1], 
                                      (int)stride[2], (int)stride[3], (int)stride[4]);
                            HDfprintf(stdout, 
                                      "%s:%d: count = %d %d %d %d %d.\n", 
                                      fcnName, mpi_rank, (int)count[0], (int)count[1], 
                                      (int)count[2], (int)count[3], (int)count[4]);
                            HDfprintf(stdout, 
                                      "%s:%d: block = %d %d %d %d %d.\n", 
                                      fcnName, mpi_rank, (int)block[0], (int)block[1], 
                                      (int)block[2], (int)block[3], (int)block[4]);
                            HDfprintf(stdout, "%s:%d: n-cube extent dims = %d.\n", 
                                      fcnName, mpi_rank,
                                      H5Sget_simple_extent_ndims(tgt_sid));
                            HDfprintf(stdout, "%s:%d: selection rank = %d.\n", 
                                      fcnName, mpi_rank, sel_rank);
#endif

                            if ( first_selection ) {

                                first_selection = FALSE; 

                                ret = H5Sselect_hyperslab
                                      (
                                        tgt_sid, 
                                        H5S_SELECT_SET,
                                        &(start[n_cube_offset]), 
                                        &(stride[n_cube_offset]), 
                                        &(count[n_cube_offset]), 
                                        &(block[n_cube_offset])
                                      );
    
                                VRFY((ret != FAIL), "H5Sselect_hyperslab(SET) succeeded");

                            } else {

                                ret = H5Sselect_hyperslab
                                      (
                                        tgt_sid, 
                                        H5S_SELECT_OR,
                                        &(start[n_cube_offset]), 
                                        &(stride[n_cube_offset]), 
                                        &(count[n_cube_offset]), 
                                        &(block[n_cube_offset])
                                      );
    
                                VRFY((ret != FAIL), "H5Sselect_hyperslab(OR) succeeded");

                            }
                        }

                        m++;

                    } while ( ( m <= 1 ) &&
                              ( 4 >= sel_offset ) );

                    l++;

                } while ( ( l <= 1 ) &&
                          ( 3 >= sel_offset ) );

                k++;

            } while ( ( k <= 1 ) &&
                      ( 2 >= sel_offset ) );

            j++;

        } while ( ( j <= 1 ) &&
                  ( 1 >= sel_offset ) );


        i++;

    } while ( ( i <= 1 ) &&
              ( 0 >= sel_offset ) );

#if CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__SELECT_CHECKER_BOARD__DEBUG 
    HDfprintf(stdout, "%s%d: H5Sget_select_npoints(tgt_sid) = %d.\n",
              fcnName, mpi_rank, (int)H5Sget_select_npoints(tgt_sid));
#endif /* CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__SELECT_CHECKER_BOARD__DEBUG */

    /* Clip the selection back to the data space proper. */

    for ( i = 0; i < test_max_rank; i++ ) {

        start[i]  = 0;
        stride[i] = edge_size;
        count[i]  = 1;
        block[i]  = edge_size;
    }

    ret = H5Sselect_hyperslab(tgt_sid, H5S_SELECT_AND,
                              start, stride, count, block);

    VRFY((ret != FAIL), "H5Sselect_hyperslab(AND) succeeded");

#if CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__SELECT_CHECKER_BOARD__DEBUG 
    HDfprintf(stdout, "%s%d: H5Sget_select_npoints(tgt_sid) = %d.\n",
              fcnName, mpi_rank, (int)H5Sget_select_npoints(tgt_sid));
    HDfprintf(stdout, "%s%d: done.\n", fcnName, mpi_rank);
#endif /* CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__SELECT_CHECKER_BOARD__DEBUG */

    return;

} /* checker_board_hyperslab_dr_pio_test__select_checker_board() */


/****************************************************************
**
**  checker_board_hyperslab_dr_pio_test__verify_data(): 
**
**	Examine the supplied buffer to see if it contains the 
**	expected data.  Return TRUE if it does, and FALSE 
**      otherwise.
**
**	The supplied buffer is presumed to this process's slice 
**	of the target data set.  Each such slice will be an
**	n-cube of rank (rank -1) and the supplied edge_size with
**	origin (mpi_rank, 0, ... , 0) in the target data set.
**
**	Further, the buffer is presumed to be the result of reading
**	or writing a checker board selection of an m (1 <= m < 
**      rank) dimensional slice through this processes slice
**	of the target data set.  Also, this slice must be parallel
**	to the fastest changing indicies.  
**
**	It is further presumed that the buffer was zeroed before
**	the read/write, and that the full target data set (i.e.
**	the buffer/data set for all processes) was initialized
**      with the natural numbers listed in order from the origin 
**	along the fastest changing axis.
**
**      Thus for a 20x10x10 dataset, the value stored in location
**	(x, y, z) (assuming that z is the fastest changing index
**	and x the slowest) is assumed to be:
**
**		(10 * 10 * x) + (10 * y) + z
**
**	Further, supposing that this is process 10, this process's 
**	slice of the dataset would be a 10 x 10 2-cube with origin
**	(10, 0, 0) in the data set, and would be initialize (prior
**	to the checkerboard selection) as follows:
**
**		1000, 1001, 1002, ... 1008, 1009
**		1010, 1011, 1012, ... 1018, 1019
**		  .     .     .         .     .
**		  .     .     .         .     .
**		  .     .     .         .     .
**		1090, 1091, 1092, ... 1098, 1099
**
**	In the case of a read from the processors slice of another
**	data set of different rank, the values expected will have
**	to be adjusted accordingly.  This is done via the 
**	first_expected_val parameter.
**
**	Finally, the function presumes that the first element 
**	of the buffer resides either at the origin of either
**	a selected or an unselected checker.  (Translation:
**	if partial checkers appear in the buffer, they will
**	intersect the edges of the n-cube oposite the origin.)
**
****************************************************************/

#define CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__VERIFY_DATA__DEBUG 0

static hbool_t
checker_board_hyperslab_dr_pio_test__verify_data(uint32_t * buf_ptr,
                                                 const int rank,
                                                 const int edge_size,
                                                 const int checker_edge_size,
                                                 uint32_t first_expected_val,
                                                 hbool_t buf_starts_in_checker)
{
#if CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__VERIFY_DATA__DEBUG
    const char *	fcnName = 
			"checker_board_hyperslab_dr_pio_test__verify_data():";
#endif
    hbool_t good_data = TRUE;
    hbool_t in_checker;
    hbool_t start_in_checker[5];
    uint32_t expected_value;
    uint32_t * val_ptr;
    int i, j, k, l, m;  /* to track position in n-cube */
    int v, w, x, y, z;  /* to track position in checker */
    const int test_max_rank = 5; /* code changes needed if this is increased */

    HDassert( buf_ptr != NULL );
    HDassert( 0 < rank );
    HDassert( rank <= test_max_rank );
    HDassert( edge_size >= 6 );
    HDassert( 0 < checker_edge_size );
    HDassert( checker_edge_size <= edge_size );
    HDassert( test_max_rank <= PAR_SS_DR_MAX_RANK );

#if CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__VERIFY_DATA__DEBUG 

    int		mpi_rank;

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    HDfprintf(stdout, "%s mpi_rank = %d.\n", fcnName, mpi_rank);
    HDfprintf(stdout, "%s rank = %d.\n", fcnName, rank);
    HDfprintf(stdout, "%s edge_size = %d.\n", fcnName, edge_size);
    HDfprintf(stdout, "%s checker_edge_size = %d.\n", fcnName, checker_edge_size);
    HDfprintf(stdout, "%s first_expected_val = %d.\n", fcnName, (int)first_expected_val);
    HDfprintf(stdout, "%s starts_in_checker = %d.\n", fcnName, (int)buf_starts_in_checker);
}
#endif

    val_ptr = buf_ptr;
    expected_value = first_expected_val;

    i = 0;
    v = 0;
    start_in_checker[0] = buf_starts_in_checker;
    do
    {
        if ( v >= checker_edge_size ) {

            start_in_checker[0] = ! start_in_checker[0];
            v = 0;
        }

        j = 0;
        w = 0;
        start_in_checker[1] = start_in_checker[0];
        do
        {
            if ( w >= checker_edge_size ) {

                start_in_checker[1] = ! start_in_checker[1];
                w = 0;
            }

            k = 0;
            x = 0;
            start_in_checker[2] = start_in_checker[1];
            do
            {
                if ( x >= checker_edge_size ) {

                    start_in_checker[2] = ! start_in_checker[2];
                    x = 0;
                }

                l = 0;
                y = 0;
                start_in_checker[3] = start_in_checker[2];
                do
                { 
                    if ( y >= checker_edge_size ) {

                        start_in_checker[3] = ! start_in_checker[3];
                        y = 0;
                    }

                    m = 0;
                    z = 0;
#if CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__VERIFY_DATA__DEBUG 
                    HDfprintf(stdout, "%d, %d, %d, %d, %d:", i, j, k, l, m);
#endif
                    in_checker = start_in_checker[3];
                    do
                    {
#if CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__VERIFY_DATA__DEBUG 
                        HDfprintf(stdout, " %d", (int)(*val_ptr));
#endif
                        if ( z >= checker_edge_size ) {

                            in_checker = ! in_checker;
                            z = 0;
                        }
         
                        if ( in_checker ) {
                   
                            if ( *val_ptr != expected_value ) {

                                good_data = FALSE;
                            }
 
                            /* zero out buffer for re-use */
                            *val_ptr = 0;

                        } else if ( *val_ptr != 0 ) {

                            good_data = FALSE;
 
                            /* zero out buffer for re-use */
                            *val_ptr = 0;

                        }

                        val_ptr++;
                        expected_value++;
                        m++;
                        z++;
 
                    } while ( ( rank >= (test_max_rank - 4) ) &&
                              ( m < edge_size ) );
#if CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__VERIFY_DATA__DEBUG 
                    HDfprintf(stdout, "\n");
#endif
                    l++;
                    y++;
                } while ( ( rank >= (test_max_rank - 3) ) &&
                          ( l < edge_size ) );
                k++;
                x++;
            } while ( ( rank >= (test_max_rank - 2) ) &&
                      ( k < edge_size ) );
            j++;
            w++;
        } while ( ( rank >= (test_max_rank - 1) ) &&
                  ( j < edge_size ) );
        i++;
        v++;
    } while ( ( rank >= test_max_rank ) &&
              ( i < edge_size ) );

    return(good_data);

} /* checker_board_hyperslab_dr_pio_test__verify_data() */


/*-------------------------------------------------------------------------
 * Function:	checker_board_hyperslab_dr_pio_test__run_test()
 *
 * Purpose:	Test I/O to/from checkerboard selections of hyperslabs of 
 *		different rank in the parallel.
 *
 * Return:	void
 *
 * Programmer:	JRM -- 10/10/09
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

#define PAR_SS_DR_MAX_RANK	5
#define CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG 0

static void
checker_board_hyperslab_dr_pio_test__run_test(const int test_num,
                                              const int edge_size,
                                              const int checker_edge_size,
                                              const int chunk_edge_size,
                                              const int small_rank,
                                              const int large_rank,
                                              const hbool_t use_collective_io,
                                              const hid_t dset_type)
{
#if CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG
    const char *fcnName = "checker_board_hyperslab_dr_pio_test__run_test()";
#endif /* CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG */
    const char *filename;
    hbool_t	use_gpfs = FALSE;   /* Use GPFS hints */
    hbool_t	data_ok = FALSE;
    hbool_t	mis_match = FALSE;
    int		i, j, k, l, n;
    int         mrc;
    int         start_index;
    int         stop_index;
    int		small_ds_offset;
    int         large_ds_offset;
    const int   test_max_rank = 5;  /* must update code if this changes */
    uint32_t	expected_value;
    uint32_t  * small_ds_buf_0 = NULL;
    uint32_t  * small_ds_buf_1 = NULL;
    uint32_t  * small_ds_buf_2 = NULL;
    uint32_t  * small_ds_slice_buf = NULL;
    uint32_t  * large_ds_buf_0 = NULL;
    uint32_t  * large_ds_buf_1 = NULL;
    uint32_t  * large_ds_buf_2 = NULL;
    uint32_t  * large_ds_slice_buf = NULL;
    uint32_t  * ptr_0;
    uint32_t  * ptr_1;
    uint32_t  * ptr_2;
    int		mpi_rank;
    int		mpi_size;
    MPI_Comm    mpi_comm = MPI_COMM_NULL;
    MPI_Info	mpi_info = MPI_INFO_NULL;
    hid_t       fid;			/* HDF5 file ID */
    hid_t	acc_tpl;		/* File access templates */
    hid_t	xfer_plist = H5P_DEFAULT;
    hid_t       full_mem_small_ds_sid;
    hid_t       full_file_small_ds_sid;
    hid_t       mem_small_ds_sid;
    hid_t       file_small_ds_sid_0;
    hid_t       file_small_ds_sid_1;
    hid_t	small_ds_slice_sid;
    hid_t       full_mem_large_ds_sid;
    hid_t       full_file_large_ds_sid;
    hid_t       mem_large_ds_sid;
    hid_t       file_large_ds_sid_0;
    hid_t       file_large_ds_sid_1;
    hid_t       file_large_ds_process_slice_sid;
    hid_t       mem_large_ds_process_slice_sid;
    hid_t	large_ds_slice_sid;
    hid_t       small_ds_dcpl_id = H5P_DEFAULT;
    hid_t       large_ds_dcpl_id = H5P_DEFAULT;
    hid_t       small_dataset;     /* Dataset ID                   */
    hid_t       large_dataset;     /* Dataset ID                   */
    size_t      small_ds_size = 1;
    size_t      small_ds_slice_size = 1;
    size_t      large_ds_size = 1;
    size_t      large_ds_slice_size = 1;
    hsize_t     dims[PAR_SS_DR_MAX_RANK];
    hsize_t     chunk_dims[PAR_SS_DR_MAX_RANK];
    hsize_t     start[PAR_SS_DR_MAX_RANK];
    hsize_t     stride[PAR_SS_DR_MAX_RANK];
    hsize_t     count[PAR_SS_DR_MAX_RANK];
    hsize_t     block[PAR_SS_DR_MAX_RANK];
    hsize_t     sel_start[PAR_SS_DR_MAX_RANK];
    htri_t      check;          /* Shape comparison return value */
    herr_t	ret;		/* Generic return value */

    HDassert( edge_size >= 6 );
    HDassert( edge_size >= chunk_edge_size );
    HDassert( ( chunk_edge_size == 0 ) || ( chunk_edge_size >= 3 ) );
    HDassert( 1 < small_rank );
    HDassert( small_rank < large_rank );
    HDassert( large_rank <= test_max_rank );
    HDassert( test_max_rank <= PAR_SS_DR_MAX_RANK );

    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    HDassert( mpi_size >= 1 );

    mpi_comm = MPI_COMM_WORLD;
    mpi_info = MPI_INFO_NULL;

    for ( i = 0; i < small_rank - 1; i++ )
    {
        small_ds_size *= (size_t)edge_size;
        small_ds_slice_size *= (size_t)edge_size;
    }
    small_ds_size *= (size_t)(mpi_size + 1);

    small_ds_offset = PAR_SS_DR_MAX_RANK - small_rank;

    HDassert( 0 < small_ds_offset );
    HDassert( small_ds_offset < PAR_SS_DR_MAX_RANK );


    for ( i = 0; i < large_rank - 1; i++ ) {

        large_ds_size *= (size_t)edge_size;
        large_ds_slice_size *= (size_t)edge_size;
    }
    large_ds_size *= (size_t)(mpi_size + 1);

    large_ds_offset = PAR_SS_DR_MAX_RANK - large_rank;

    HDassert( 0 <= large_ds_offset );
    HDassert( large_ds_offset < PAR_SS_DR_MAX_RANK );


    /* Allocate buffers */
    small_ds_buf_0 = (uint32_t *)HDmalloc(sizeof(uint32_t) * small_ds_size);
    VRFY((small_ds_buf_0 != NULL), "malloc of small_ds_buf_0 succeeded");

    small_ds_buf_1 = (uint32_t *)HDmalloc(sizeof(uint32_t) * small_ds_size);
    VRFY((small_ds_buf_1 != NULL), "malloc of small_ds_buf_1 succeeded");

    small_ds_buf_2 = (uint32_t *)HDmalloc(sizeof(uint32_t) * small_ds_size);
    VRFY((small_ds_buf_2 != NULL), "malloc of small_ds_buf_2 succeeded");

    small_ds_slice_buf = 
	(uint32_t *)HDmalloc(sizeof(uint32_t) * small_ds_slice_size);
    VRFY((small_ds_slice_buf != NULL), "malloc of small_ds_slice_buf succeeded");

    large_ds_buf_0 = (uint32_t *)HDmalloc(sizeof(uint32_t) * large_ds_size);
    VRFY((large_ds_buf_0 != NULL), "malloc of large_ds_buf_0 succeeded");

    large_ds_buf_1 = (uint32_t *)HDmalloc(sizeof(uint32_t) * large_ds_size);
    VRFY((large_ds_buf_1 != NULL), "malloc of large_ds_buf_1 succeeded");

    large_ds_buf_2 = (uint32_t *)HDmalloc(sizeof(uint32_t) * large_ds_size);
    VRFY((large_ds_buf_2 != NULL), "malloc of large_ds_buf_2 succeeded");

    large_ds_slice_buf = 
	(uint32_t *)HDmalloc(sizeof(uint32_t) * large_ds_slice_size);
    VRFY((large_ds_slice_buf != NULL), "malloc of large_ds_slice_buf succeeded");

    /* initialize the buffers */

    ptr_0 = small_ds_buf_0;
    ptr_1 = small_ds_buf_1;
    ptr_2 = small_ds_buf_2;

    for ( i = 0; i < (int)small_ds_size; i++ ) {

        *ptr_0 = (uint32_t)i;
        *ptr_1 = 0;
        *ptr_2 = 0;

        ptr_0++;
        ptr_1++;
        ptr_2++;
    }

    ptr_0 = small_ds_slice_buf;

    for ( i = 0; i < (int)small_ds_slice_size; i++ ) {

	*ptr_0 = (uint32_t)i;
        ptr_0++;
    }

    ptr_0 = large_ds_buf_0;
    ptr_1 = large_ds_buf_1;
    ptr_2 = large_ds_buf_2;

    for ( i = 0; i < (int)large_ds_size; i++ ) {

        *ptr_0 = (uint32_t)i;
        *ptr_1 = 0;
        *ptr_2 = 0;

        ptr_0++;
        ptr_1++;
        ptr_2++;
    }

    ptr_0 = large_ds_slice_buf;

    for ( i = 0; i < (int)large_ds_slice_size; i++ ) {

	*ptr_0 = (uint32_t)0;
        ptr_0++;
    }

    filename = (const char *)GetTestParameters();
    HDassert( filename != NULL );

#if CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG
    if ( MAINPROCESS ) {

        HDfprintf(stdout, "%s:%d: test num = %d.\n", fcnName, mpi_rank, test_num);
        HDfprintf(stdout, "%s:%d: mpi_size = %d.\n", fcnName, mpi_rank, mpi_size);
        HDfprintf(stdout, 
                  "%s:%d: small/large rank = %d/%d, use_collective_io = %d.\n",
                  fcnName, mpi_rank, small_rank, large_rank, (int)use_collective_io);
        HDfprintf(stdout, "%s:%d: edge_size = %d, chunk_edge_size = %d.\n",
                  fcnName, mpi_rank, edge_size, chunk_edge_size);
        HDfprintf(stdout, "%s:%d: checker_edge_size = %d.\n", 
                  fcnName, mpi_rank, checker_edge_size);
        HDfprintf(stdout, "%s:%d: small_ds_size = %d, large_ds_size = %d.\n",
                  fcnName, mpi_rank, (int)small_ds_size, (int)large_ds_size);
        HDfprintf(stdout, "%s:%d: filename = %s.\n", fcnName, mpi_rank, filename);
    }
#endif /* CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG */

    /* ----------------------------------------
     * CREATE AN HDF5 FILE WITH PARALLEL ACCESS
     * ---------------------------------------*/
    /* setup file access template */
    acc_tpl = create_faccess_plist(mpi_comm, mpi_info, facc_type, use_gpfs);
    VRFY((acc_tpl >= 0), "create_faccess_plist() succeeded");

    /* create the file collectively */
    fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, acc_tpl);
    VRFY((fid >= 0), "H5Fcreate succeeded");

    MESG("File opened.");

    /* Release file-access template */
    ret = H5Pclose(acc_tpl);
    VRFY((ret >= 0), "H5Pclose(acc_tpl) succeeded");


    /* setup dims: */
    dims[0] = (int)(mpi_size + 1);
    dims[1] = dims[2] = dims[3] = dims[4] = edge_size;


    /* Create small ds dataspaces */
    full_mem_small_ds_sid = H5Screate_simple(small_rank, dims, NULL);
    VRFY((full_mem_small_ds_sid != 0), 
         "H5Screate_simple() full_mem_small_ds_sid succeeded");

    full_file_small_ds_sid = H5Screate_simple(small_rank, dims, NULL);
    VRFY((full_file_small_ds_sid != 0), 
         "H5Screate_simple() full_file_small_ds_sid succeeded");

    mem_small_ds_sid = H5Screate_simple(small_rank, dims, NULL);
    VRFY((mem_small_ds_sid != 0), 
	 "H5Screate_simple() mem_small_ds_sid succeeded");

    file_small_ds_sid_0 = H5Screate_simple(small_rank, dims, NULL);
    VRFY((file_small_ds_sid_0 != 0), 
         "H5Screate_simple() file_small_ds_sid_0 succeeded");

    file_small_ds_sid_1 = H5Screate_simple(small_rank, dims, NULL);
    VRFY((file_small_ds_sid_1 != 0), 
         "H5Screate_simple() file_small_ds_sid_1 succeeded");

    small_ds_slice_sid = H5Screate_simple(small_rank - 1, &(dims[1]), NULL);
    VRFY((small_ds_slice_sid != 0), 
         "H5Screate_simple() small_ds_slice_sid succeeded");


    /* Create large ds dataspaces */
    full_mem_large_ds_sid = H5Screate_simple(large_rank, dims, NULL);
    VRFY((full_mem_large_ds_sid != 0), 
         "H5Screate_simple() full_mem_large_ds_sid succeeded");

    full_file_large_ds_sid = H5Screate_simple(large_rank, dims, NULL);
    VRFY((full_file_large_ds_sid != FAIL), 
         "H5Screate_simple() full_file_large_ds_sid succeeded");

    mem_large_ds_sid = H5Screate_simple(large_rank, dims, NULL);
    VRFY((mem_large_ds_sid != FAIL), 
         "H5Screate_simple() mem_large_ds_sid succeeded");

    file_large_ds_sid_0 = H5Screate_simple(large_rank, dims, NULL);
    VRFY((file_large_ds_sid_0 != FAIL), 
         "H5Screate_simple() file_large_ds_sid_0 succeeded");

    file_large_ds_sid_1 = H5Screate_simple(large_rank, dims, NULL);
    VRFY((file_large_ds_sid_1 != FAIL), 
         "H5Screate_simple() file_large_ds_sid_1 succeeded");

    mem_large_ds_process_slice_sid = H5Screate_simple(large_rank, dims, NULL);
    VRFY((mem_large_ds_process_slice_sid != FAIL), 
         "H5Screate_simple() mem_large_ds_process_slice_sid succeeded");

    file_large_ds_process_slice_sid = H5Screate_simple(large_rank, dims, NULL);
    VRFY((file_large_ds_process_slice_sid != FAIL), 
         "H5Screate_simple() file_large_ds_process_slice_sid succeeded");


    large_ds_slice_sid = H5Screate_simple(large_rank - 1, &(dims[1]), NULL);
    VRFY((large_ds_slice_sid != 0), 
         "H5Screate_simple() large_ds_slice_sid succeeded");


    /* Select the entire extent of the full small ds, and ds slice dataspaces */
    ret = H5Sselect_all(full_mem_small_ds_sid);
    VRFY((ret != FAIL), "H5Sselect_all(full_mem_small_ds_sid) succeeded");

    ret = H5Sselect_all(full_file_small_ds_sid);
    VRFY((ret != FAIL), "H5Sselect_all(full_file_small_ds_sid) succeeded");

    ret = H5Sselect_all(small_ds_slice_sid);
    VRFY((ret != FAIL), "H5Sselect_all(small_ds_slice_sid) succeeded");


    /* Select the entire extent of the full large ds, and ds slice dataspaces */
    ret = H5Sselect_all(full_mem_large_ds_sid);
    VRFY((ret != FAIL), "H5Sselect_all(full_mem_large_ds_sid) succeeded");

    ret = H5Sselect_all(full_file_large_ds_sid);
    VRFY((ret != FAIL), "H5Sselect_all(full_file_large_ds_sid) succeeded");

    ret = H5Sselect_all(large_ds_slice_sid);
    VRFY((ret != FAIL), "H5Sselect_all(large_ds_slice_sid) succeeded");


    /* if chunk edge size is greater than zero, set up the small and
     * large data set creation property lists to specify chunked
     * datasets.
     */
    if ( chunk_edge_size > 0 ) {

        chunk_dims[0] = mpi_size + 1;
        chunk_dims[1] = chunk_dims[2] = 
                        chunk_dims[3] = chunk_dims[4] = chunk_edge_size;

        small_ds_dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
        VRFY((ret != FAIL), "H5Pcreate() small_ds_dcpl_id succeeded");

        ret = H5Pset_layout(small_ds_dcpl_id, H5D_CHUNKED);
        VRFY((ret != FAIL), "H5Pset_layout() small_ds_dcpl_id succeeded");

        ret = H5Pset_chunk(small_ds_dcpl_id, small_rank, chunk_dims);
        VRFY((ret != FAIL), "H5Pset_chunk() small_ds_dcpl_id succeeded");


        large_ds_dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
        VRFY((ret != FAIL), "H5Pcreate() large_ds_dcpl_id succeeded");

        ret = H5Pset_layout(large_ds_dcpl_id, H5D_CHUNKED);
        VRFY((ret != FAIL), "H5Pset_layout() large_ds_dcpl_id succeeded");

        ret = H5Pset_chunk(large_ds_dcpl_id, large_rank, chunk_dims);
        VRFY((ret != FAIL), "H5Pset_chunk() large_ds_dcpl_id succeeded");
    }

    /* create the small dataset */
    small_dataset = H5Dcreate2(fid, "small_dataset", dset_type,
                               file_small_ds_sid_0, H5P_DEFAULT,
                               small_ds_dcpl_id, H5P_DEFAULT);
    VRFY((ret != FAIL), "H5Dcreate2() small_dataset succeeded");

    /* create the large dataset */
    large_dataset = H5Dcreate2(fid, "large_dataset", dset_type,
                               file_large_ds_sid_0, H5P_DEFAULT,
                               large_ds_dcpl_id, H5P_DEFAULT);
    VRFY((ret != FAIL), "H5Dcreate2() large_dataset succeeded");



    /* setup xfer property list */
    xfer_plist = H5Pcreate(H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "H5Pcreate(H5P_DATASET_XFER) succeeded");

    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");

    if ( ! use_collective_io ) {

        ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist,
                                              H5FD_MPIO_INDIVIDUAL_IO);
        VRFY((ret>= 0), "H5Pset_dxpl_mpio_collective_opt() suceeded");
    }

    /* setup selection to write initial data to the small and large data sets */
    start[0] = mpi_rank;
    stride[0] = 2 * (mpi_size + 1);
    count[0] = 1;
    block[0] = 1;

    for ( i = 1; i < large_rank; i++ ) {

        start[i] = 0;
        stride[i] = 2 * edge_size;
        count[i] = 1;
        block[i] = edge_size;
    }

    /* setup selections for writing initial data to the small data set */
    ret = H5Sselect_hyperslab(mem_small_ds_sid,
                              H5S_SELECT_SET,
                              start,
                              stride,
                              count,
                              block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(mem_small_ds_sid, set) suceeded");

    ret = H5Sselect_hyperslab(file_small_ds_sid_0,
                              H5S_SELECT_SET,
                              start,
                              stride,
                              count,
                              block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(file_small_ds_sid_0, set) suceeded");

    if ( MAINPROCESS ) { /* add an additional slice to the selections */

        start[0] = mpi_size;

        ret = H5Sselect_hyperslab(mem_small_ds_sid,
                                  H5S_SELECT_OR,
                                  start,
                                  stride,
                                  count,
                                  block);
        VRFY((ret>= 0), "H5Sselect_hyperslab(mem_small_ds_sid, or) suceeded");

        ret = H5Sselect_hyperslab(file_small_ds_sid_0,
                                  H5S_SELECT_OR,
                                  start,
                                  stride,
                                  count,
                                  block);
        VRFY((ret>= 0), "H5Sselect_hyperslab(file_small_ds_sid_0, or) suceeded");
    }


    /* write the initial value of the small data set to file */
    ret = H5Dwrite(small_dataset, dset_type, mem_small_ds_sid, file_small_ds_sid_0,
                   xfer_plist, small_ds_buf_0);
    VRFY((ret >= 0), "H5Dwrite() small_dataset initial write succeeded");


    /* sync with the other processes before checking data */
    mrc = MPI_Barrier(MPI_COMM_WORLD);
    VRFY((mrc==MPI_SUCCESS), "Sync after small dataset writes");


    /* read the small data set back to verify that it contains the 
     * expected data.  Note that each process reads in the entire 
     * data set and verifies it.
     */
    ret = H5Dread(small_dataset,
                  H5T_NATIVE_UINT32,
                  full_mem_small_ds_sid,
                  full_file_small_ds_sid,
                  xfer_plist,
                  small_ds_buf_1);
    VRFY((ret >= 0), "H5Dread() small_dataset initial read succeeded");


    /* verify that the correct data was written to the small data set */
    expected_value = 0;
    mis_match = FALSE;
    ptr_1 = small_ds_buf_1;

    i = 0;
    for ( i = 0; i < (int)small_ds_size; i++ ) {

        if ( *ptr_1 != expected_value ) {

            mis_match = TRUE;
        }
        ptr_1++;
        expected_value++;
    }
    VRFY( (mis_match == FALSE), "small ds init data good.");



    /* setup selections for writing initial data to the large data set */

    start[0] = mpi_rank;

    ret = H5Sselect_hyperslab(mem_large_ds_sid,
                              H5S_SELECT_SET,
                              start,
                              stride,
                              count,
                              block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(mem_large_ds_sid, set) suceeded");

    ret = H5Sselect_hyperslab(file_large_ds_sid_0,
                              H5S_SELECT_SET,
                              start,
                              stride,
                              count,
                              block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(file_large_ds_sid_0, set) suceeded");
 
    /* In passing, setup the process slice data spaces as well */

    ret = H5Sselect_hyperslab(mem_large_ds_process_slice_sid,
                              H5S_SELECT_SET,
                              start,
                              stride,
                              count,
                              block);
    VRFY((ret >= 0), 
         "H5Sselect_hyperslab(mem_large_ds_process_slice_sid, set) suceeded");

    ret = H5Sselect_hyperslab(file_large_ds_process_slice_sid,
                              H5S_SELECT_SET,
                              start,
                              stride,
                              count,
                              block);
    VRFY((ret >= 0), 
         "H5Sselect_hyperslab(file_large_ds_process_slice_sid, set) suceeded");

    if ( MAINPROCESS ) { /* add an additional slice to the selections */

        start[0] = mpi_size;

        ret = H5Sselect_hyperslab(mem_large_ds_sid,
                                  H5S_SELECT_OR,
                                  start,
                                  stride,
                                  count,
                                  block);
        VRFY((ret>= 0), "H5Sselect_hyperslab(mem_large_ds_sid, or) suceeded");

        ret = H5Sselect_hyperslab(file_large_ds_sid_0,
                                  H5S_SELECT_OR,
                                  start,
                                  stride,
                                  count,
                                  block);
        VRFY((ret>= 0), "H5Sselect_hyperslab(file_large_ds_sid_0, or) suceeded");
    }


    /* write the initial value of the large data set to file */
    ret = H5Dwrite(large_dataset, dset_type, mem_large_ds_sid, file_large_ds_sid_0,
                   xfer_plist, large_ds_buf_0);
    if ( ret < 0 ) H5Eprint2(H5E_DEFAULT, stderr);
    VRFY((ret >= 0), "H5Dwrite() large_dataset initial write succeeded");


    /* sync with the other processes before checking data */
    mrc = MPI_Barrier(MPI_COMM_WORLD);
    VRFY((mrc==MPI_SUCCESS), "Sync after large dataset writes");


    /* read the small data set back to verify that it contains the 
     * expected data.  Note that each process reads in the entire 
     * data set.
     */
    ret = H5Dread(large_dataset,
                  H5T_NATIVE_UINT32,
                  full_mem_large_ds_sid,
                  full_file_large_ds_sid,
                  xfer_plist,
                  large_ds_buf_1);
    VRFY((ret >= 0), "H5Dread() large_dataset initial read succeeded");


    /* verify that the correct data was written to the small data set */
    expected_value = 0;
    mis_match = FALSE;
    ptr_1 = large_ds_buf_1;

    i = 0;
    for ( i = 0; i < (int)large_ds_size; i++ ) {

        if ( *ptr_1 != expected_value ) {

            mis_match = TRUE;
        }
        ptr_1++;
        expected_value++;
    }
    VRFY( (mis_match == FALSE), "large ds init data good.");

    /***********************************/
    /***** INITIALIZATION COMPLETE *****/
    /***********************************/

    /* first, verify that we can read from disk correctly using selections
     * of different rank that H5S_select_shape_same() views as being of the
     * same shape.
     *
     * Start by reading a (small_rank - 1)-D slice from this processes slice 
     * of the on disk large data set, and verifying that the data read is 
     * correct.  Verify that H5S_select_shape_same() returns true on the 
     * memory and file selections.
     *
     * The first step is to set up the needed checker board selection in the
     * in memory small small cube
     */

    sel_start[0] = sel_start[1] = sel_start[2] = sel_start[3] = sel_start[4] = 0;
    sel_start[small_ds_offset] = mpi_rank;

    checker_board_hyperslab_dr_pio_test__select_checker_board(mpi_rank,
                                                              small_ds_slice_sid,
                                                              small_rank - 1,
                                                              edge_size,
                                                              checker_edge_size,
                                                              small_rank - 1,
                                                              sel_start);

    /* zero out the buffer we will be reading into */

    ptr_0 = small_ds_slice_buf;

    for ( i = 0; i < (int)small_ds_slice_size; i++ ) {

	*ptr_0 = (uint32_t)0;
        ptr_0++;
    }

#if CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG 
    HDfprintf(stdout, "%s:%d: initial small_ds_slice_buf = ",
              fcnName, mpi_rank);
    ptr_0 = small_ds_slice_buf;
    for ( i = 0; i < (int)small_ds_slice_size; i++ ) {
	HDfprintf(stdout, "%d ", (int)(*ptr_0));
        ptr_0++;
    }
    HDfprintf(stdout, "\n");
#endif /* CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG */ 

    /* set up start, stride, count, and block -- note that we will
     * change start[] so as to read slices of the large cube.
     */
    for ( i = 0; i < PAR_SS_DR_MAX_RANK; i++ ) {

        start[i] = 0;
        stride[i] = 2 * edge_size;
        count[i] = 1;
        if ( (PAR_SS_DR_MAX_RANK - i) > (small_rank - 1) ) {

            block[i] = 1;

        } else {

            block[i] = edge_size;
        }
    }

#if CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG 
    HDfprintf(stdout, 
              "%s:%d: reading slice from big ds on disk into small ds slice.\n",
              fcnName, mpi_rank);
#endif /* CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG */ 
    /* in serial versions of this test, we loop through all the dimensions
     * of the large data set.  However, in the parallel version, each 
     * process only works with that slice of the large cube indicated
     * by its rank -- hence we set the most slowly changing index to 
     * mpi_rank, and don't itterate over it.
     */

    if ( PAR_SS_DR_MAX_RANK - large_rank == 0 ) {

        i = mpi_rank;

    } else {

        i = 0;
    }

    /* since large_rank is at most PAR_SS_DR_MAX_RANK, no need to 
     * loop over it -- either we are setting i to mpi_rank, or
     * we are setting it to zero.  It will not change during the 
     * test.
     */

    if ( PAR_SS_DR_MAX_RANK - large_rank == 1 ) {

        j = mpi_rank;

    } else {

        j = 0;
    }

    do {
        if ( PAR_SS_DR_MAX_RANK - large_rank == 2 ) {

            k = mpi_rank;

        } else {

            k = 0;
        }

        do {
            /* since small rank >= 2 and large_rank > small_rank, we 
             * have large_rank >= 3.  Since PAR_SS_DR_MAX_RANK == 5
             * (baring major re-orgaization), this gives us:
             *
             *     (PAR_SS_DR_MAX_RANK - large_rank) <= 2
             *
             * so no need to repeat the test in the outer loops --
             * just set l = 0.
             */

            l = 0;
            do {
                /* we know that small_rank - 1 >= 1 and that 
                 * large_rank > small_rank by the assertions at the head 
                 * of this function.  Thus no need for another inner loop.
                 */
                start[0] = i;
                start[1] = j;
                start[2] = k;
                start[3] = l;
                start[4] = 0;

                HDassert( ( start[0] == 0 ) || ( 0 < small_ds_offset + 1 ) );
                HDassert( ( start[1] == 0 ) || ( 1 < small_ds_offset + 1 ) );
                HDassert( ( start[2] == 0 ) || ( 2 < small_ds_offset + 1 ) );
                HDassert( ( start[3] == 0 ) || ( 3 < small_ds_offset + 1 ) );
                HDassert( ( start[4] == 0 ) || ( 4 < small_ds_offset + 1 ) );

                checker_board_hyperslab_dr_pio_test__select_checker_board
                (
                  mpi_rank,
                  file_large_ds_sid_0,
                  large_rank,
                  edge_size,
                  checker_edge_size,
                  small_rank - 1,
                  start
                );

                /* verify that H5S_select_shape_same() reports the two
                 * selections as having the same shape.
                 */
                check = H5S_select_shape_same_test(small_ds_slice_sid,
                                                   file_large_ds_sid_0);
                VRFY((check == TRUE), "H5S_select_shape_same_test passed");


                /* Read selection from disk */
#if CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG 
                HDfprintf(stdout, "%s:%d: start = %d %d %d %d %d.\n", fcnName,
                          mpi_rank, start[0], start[1], start[2], start[3], 
                          start[4]);
                HDfprintf(stdout, "%s slice/file extent dims = %d/%d.\n",
                          fcnName,
                          H5Sget_simple_extent_ndims(small_ds_slice_sid),
                          H5Sget_simple_extent_ndims(file_large_ds_sid_0));
#endif /* CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG */ 

                ret = H5Dread(large_dataset,
                              H5T_NATIVE_UINT32,
                              small_ds_slice_sid,
                              file_large_ds_sid_0,
                              xfer_plist,
                              small_ds_slice_buf);
                VRFY((ret >= 0), "H5Sread() slice from large ds succeeded.");

#if CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG 
		HDfprintf(stdout, "%s:%d: H5Dread() returns.\n", 
                          fcnName, mpi_rank);
#endif 

                /* verify that expected data is retrieved */

                expected_value = (uint32_t)
			((i * edge_size * edge_size * edge_size * edge_size) +
                         (j * edge_size * edge_size * edge_size) +
                         (k * edge_size * edge_size) +
                         (l * edge_size));

                data_ok = checker_board_hyperslab_dr_pio_test__verify_data
                          (
                            small_ds_slice_buf,
                            small_rank - 1,
                            edge_size,
                            checker_edge_size,
                            expected_value,
                            (hbool_t)TRUE
                          );

                VRFY((data_ok == TRUE), 
                     "small slice read from large ds data good.");
                l++;

            } while ( ( large_rank > 2 ) &&
                      ( (small_rank - 1) <= 1 ) &&
                      ( l < edge_size ) );
            k++;
        } while ( ( large_rank > 3 ) &&
                  ( (small_rank - 1) <= 2 ) &&
                  ( k < edge_size ) );
        j++;
    } while ( ( large_rank > 4 ) &&
              ( (small_rank - 1) <= 3 ) &&
              ( j < edge_size ) );


    /* similarly, read slices of the on disk small data set into slices 
     * through the in memory large data set, and verify that the correct 
     * data (and only the correct data) is read.
     */

    sel_start[0] = sel_start[1] = sel_start[2] = sel_start[3] = sel_start[4] = 0;
    sel_start[small_ds_offset] = mpi_rank;

    checker_board_hyperslab_dr_pio_test__select_checker_board(mpi_rank,
                                                              file_small_ds_sid_0,
                                                              small_rank,
                                                              edge_size,
                                                              checker_edge_size,
                                                              small_rank - 1,
                                                              sel_start);

#if CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG 
    HDfprintf(stdout, 
      "%s reading slices of on disk small data set into slices of big data set.\n",
              fcnName);
#endif 

    /* zero out the buffer we will be reading into */
    ptr_0 = large_ds_buf_1;
    for ( i = 0; i < (int)large_ds_size; i++ ) {

	*ptr_0 = (uint32_t)0;
        ptr_0++;
    }

    /* set up start, stride, count, and block -- note that we will
     * change start[] so as to read the slice of the small data set
     * into different slices of the process slice of the large data 
     * set.
     */
    for ( i = 0; i < PAR_SS_DR_MAX_RANK; i++ ) {

        start[i] = 0;
        stride[i] = 2 * edge_size;
        count[i] = 1;
        if ( (PAR_SS_DR_MAX_RANK - i) > (small_rank - 1) ) {

            block[i] = 1;

        } else {

            block[i] = edge_size;
        }
    }


    /* in serial versions of this test, we loop through all the dimensions
     * of the large data set that don't appear in the small data set.  
     *
     * However, in the parallel version, each process only works with that 
     * slice of the large (and small) data set indicated by its rank -- hence 
     * we set the most slowly changing index to mpi_rank, and don't itterate 
     * over it.
     */


    if ( PAR_SS_DR_MAX_RANK - large_rank == 0 ) {

        i = mpi_rank;

    } else {

        i = 0;
    }

    /* since large_rank is at most PAR_SS_DR_MAX_RANK, no need to 
     * loop over it -- either we are setting i to mpi_rank, or
     * we are setting it to zero.  It will not change during the 
     * test.
     */

    if ( PAR_SS_DR_MAX_RANK - large_rank == 1 ) {

        j = mpi_rank;

    } else {

        j = 0;
    }

    do {
        if ( PAR_SS_DR_MAX_RANK - large_rank == 2 ) {

            k = mpi_rank;

        } else {

            k = 0;
        }

        do {
            /* since small rank >= 2 and large_rank > small_rank, we 
             * have large_rank >= 3.  Since PAR_SS_DR_MAX_RANK == 5
             * (baring major re-orgaization), this gives us:
             *
             *     (PAR_SS_DR_MAX_RANK - large_rank) <= 2
             *
             * so no need to repeat the test in the outer loops --
             * just set l = 0.
             */

            l = 0;
            do {
                /* we know that small_rank >= 1 and that large_rank > small_rank
                 * by the assertions at the head of this function.  Thus no
                 * need for another inner loop.
                 */
                start[0] = i;
                start[1] = j;
                start[2] = k;
                start[3] = l;
                start[4] = 0;

                HDassert( ( start[0] == 0 ) || ( 0 < small_ds_offset + 1 ) );
                HDassert( ( start[1] == 0 ) || ( 1 < small_ds_offset + 1 ) );
                HDassert( ( start[2] == 0 ) || ( 2 < small_ds_offset + 1 ) );
                HDassert( ( start[3] == 0 ) || ( 3 < small_ds_offset + 1 ) );
                HDassert( ( start[4] == 0 ) || ( 4 < small_ds_offset + 1 ) );

                checker_board_hyperslab_dr_pio_test__select_checker_board
                (
                  mpi_rank,
                  mem_large_ds_sid,
                  large_rank,
                  edge_size,
                  checker_edge_size,
                  small_rank - 1,
                  start
                );


                /* verify that H5S_select_shape_same() reports the two
                 * selections as having the same shape.
                 */
                check = H5S_select_shape_same_test(file_small_ds_sid_0,
                                                   mem_large_ds_sid);
                VRFY((check == TRUE), "H5S_select_shape_same_test passed");


                /* Read selection from disk */
#if CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG 
                HDfprintf(stdout, "%s:%d: start = %d %d %d %d %d.\n", 
                          fcnName, mpi_rank, 
                          start[0], start[1], start[2], start[3], start[4]);
                HDfprintf(stdout, "%s:%d: mem/file extent dims = %d/%d.\n",
                          fcnName, mpi_rank,
                          H5Sget_simple_extent_ndims(large_ds_slice_sid),
                          H5Sget_simple_extent_ndims(file_small_ds_sid_0));
#endif 
                ret = H5Dread(small_dataset,
                              H5T_NATIVE_UINT32,
                              mem_large_ds_sid,
                              file_small_ds_sid_0,
                              xfer_plist,
                              large_ds_buf_1);
                VRFY((ret >= 0), "H5Sread() slice from small ds succeeded.");

                /* verify that the expected data and only the
                 * expected data was read.
                 */
                data_ok = TRUE;
                ptr_1 = large_ds_buf_1;
                expected_value = mpi_rank * small_ds_slice_size;
                start_index = 
                        (i * edge_size * edge_size * edge_size * edge_size) +
                        (j * edge_size * edge_size * edge_size) +
                        (k * edge_size * edge_size) +
                        (l * edge_size);
                stop_index = start_index + (int)small_ds_slice_size - 1;

#if CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG 
{
int		m;
		HDfprintf(stdout, "%s:%d: expected_value = %d.\n", 
                          fcnName, mpi_rank, expected_value);
		HDfprintf(stdout, "%s:%d: start/stop index = %d/%d.\n",
                          fcnName, mpi_rank, start_index, stop_index);
                n = 0;
                for ( m = 0; m < large_ds_size; m ++ ) {
                    HDfprintf(stdout, "%d ", (int)(*ptr_1));
                    ptr_1++;
                    n++;
                    if ( n >= edge_size ) {
                        HDfprintf(stdout, "\n");
                        n = 0;
                    }
                }
                HDfprintf(stdout, "\n");
                fsync(stdout);
                ptr_1 = large_ds_buf_1;
}
#endif 

                HDassert( 0 <= start_index );
                HDassert( start_index < stop_index );
                HDassert( stop_index <= (int)large_ds_size );

                for ( n = 0; n < (int)start_index; n++ ) {

                    if ( *ptr_1 != 0 ) {

			data_ok = FALSE;
		    }

                    /* zero out the value for the next pass */
                    *ptr_1 = 0;

                    ptr_1++;
                }

                VRFY((data_ok == TRUE), 
                     "slice read from small to large ds data good(1).");

                data_ok = checker_board_hyperslab_dr_pio_test__verify_data
                          (
                            ptr_1,
                            small_rank - 1,
                            edge_size,
                            checker_edge_size,
                            expected_value,
                            (hbool_t)TRUE
                          );

                VRFY((data_ok == TRUE), 
                     "slice read from small to large ds data good(2).");


                ptr_1 = large_ds_buf_1 + stop_index + 1;
                for ( n = stop_index + 1; n < large_ds_size; n++ ) {

                    if ( *ptr_1 != 0 ) {

			data_ok = FALSE;
		    }

                    /* zero out the value for the next pass */
                    *ptr_1 = 0;

                    *ptr_1++;
                }

                VRFY((data_ok == TRUE), 
                     "slice read from small to large ds data good(3).");

                l++;

            } while ( ( large_rank > 2 ) &&
                      ( (small_rank - 1) <= 1 ) &&
                      ( l < edge_size ) );
            k++;
        } while ( ( large_rank > 3 ) &&
                  ( (small_rank - 1) <= 2 ) &&
                  ( k < edge_size ) );
        j++;
    } while ( ( large_rank > 4 ) &&
              ( (small_rank - 1) <= 3 ) &&
              ( j < edge_size ) );


    /* now we go in the opposite direction, verifying that we can write
     * from memory to file using selections of different rank that
     * H5S_select_shape_same() views as being of the same shape.
     *
     * Start by writing small_rank - 1 D slices from the in memory large data
     * set to the on disk small dataset.  After each write, read the slice of 
     * the small dataset back from disk, and verify that it contains the 
     * expected data. Verify that H5S_select_shape_same() returns true on 
     * the memory and file selections.
     */

    start[0] = mpi_rank;
    stride[0] = 2 * (mpi_size + 1);
    count[0] = 1;
    block[0] = 1;

    for ( i = 1; i < large_rank; i++ ) {

        start[i] = 0;
        stride[i] = 2 * edge_size;
        count[i] = 1;
        block[i] = edge_size;
    }

    ret = H5Sselect_hyperslab(file_small_ds_sid_0,
                              H5S_SELECT_SET,
                              start,
                              stride,
                              count,
                              block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(file_small_ds_sid_0, set) suceeded");

    ret = H5Sselect_hyperslab(mem_small_ds_sid,
                              H5S_SELECT_SET,
                              start,
                              stride,
                              count,
                              block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(mem_small_ds_sid, set) suceeded");


    sel_start[0] = sel_start[1] = sel_start[2] = sel_start[3] = sel_start[4] = 0;
    sel_start[small_ds_offset] = mpi_rank;

    checker_board_hyperslab_dr_pio_test__select_checker_board(mpi_rank,
                                                              file_small_ds_sid_1,
                                                              small_rank,
                                                              edge_size,
                                                              checker_edge_size,
                                                              small_rank - 1,
                                                              sel_start);


    /* set up start, stride, count, and block -- note that we will
     * change start[] so as to read slices of the large cube.
     */
    for ( i = 0; i < PAR_SS_DR_MAX_RANK; i++ ) {

        start[i] = 0;
        stride[i] = 2 * edge_size;
        count[i] = 1;
        if ( (PAR_SS_DR_MAX_RANK - i) > (small_rank - 1) ) {

            block[i] = 1;

        } else {

            block[i] = edge_size;
        }
    }

    /* zero out the in memory small ds */
    ptr_1 = small_ds_buf_1;
    for ( n = 0; n < (int)small_ds_size; n++ ) {

        *ptr_1 = 0;
        ptr_1++;
    }


#if CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG 
    HDfprintf(stdout, 
    "%s writing checker boards selections of slices from big ds to slices of small ds on disk.\n",
    fcnName);
#endif 

    /* in serial versions of this test, we loop through all the dimensions
     * of the large data set that don't appear in the small data set.  
     *
     * However, in the parallel version, each process only works with that 
     * slice of the large (and small) data set indicated by its rank -- hence 
     * we set the most slowly changing index to mpi_rank, and don't itterate 
     * over it.
     */


    if ( PAR_SS_DR_MAX_RANK - large_rank == 0 ) {

        i = mpi_rank;

    } else {

        i = 0;
    }

    /* since large_rank is at most PAR_SS_DR_MAX_RANK, no need to 
     * loop over it -- either we are setting i to mpi_rank, or
     * we are setting it to zero.  It will not change during the 
     * test.
     */

    if ( PAR_SS_DR_MAX_RANK - large_rank == 1 ) {

        j = mpi_rank;

    } else {

        j = 0;
    }

    j = 0;
    do {
        if ( PAR_SS_DR_MAX_RANK - large_rank == 2 ) {

            k = mpi_rank;

        } else {

            k = 0;
        }

        do {
            /* since small rank >= 2 and large_rank > small_rank, we 
             * have large_rank >= 3.  Since PAR_SS_DR_MAX_RANK == 5
             * (baring major re-orgaization), this gives us:
             *
             *     (PAR_SS_DR_MAX_RANK - large_rank) <= 2
             *
             * so no need to repeat the test in the outer loops --
             * just set l = 0.
             */

            l = 0;
            do {
                /* we know that small_rank >= 1 and that large_rank > small_rank
                 * by the assertions at the head of this function.  Thus no
                 * need for another inner loop.
                 */

                /* zero out this rank's slice of the on disk small data set */
                ret = H5Dwrite(small_dataset,
                               H5T_NATIVE_UINT32,
                               mem_small_ds_sid,
                               file_small_ds_sid_0,
                               xfer_plist,
                               small_ds_buf_2);
                VRFY((ret >= 0), "H5Dwrite() zero slice to small ds succeeded.");

                /* select the portion of the in memory large cube from which we
                 * are going to write data.
                 */
                start[0] = i;
                start[1] = j;
                start[2] = k;
                start[3] = l;
                start[4] = 0;

                HDassert( ( start[0] == 0 ) || ( 0 < small_ds_offset + 1 ) );
                HDassert( ( start[1] == 0 ) || ( 1 < small_ds_offset + 1 ) );
                HDassert( ( start[2] == 0 ) || ( 2 < small_ds_offset + 1 ) );
                HDassert( ( start[3] == 0 ) || ( 3 < small_ds_offset + 1 ) );
                HDassert( ( start[4] == 0 ) || ( 4 < small_ds_offset + 1 ) );

                checker_board_hyperslab_dr_pio_test__select_checker_board
                (
                  mpi_rank,
                  mem_large_ds_sid,
                  large_rank,
                  edge_size,
                  checker_edge_size,
                  small_rank - 1,
                  start
                );


                /* verify that H5S_select_shape_same() reports the in
                 * memory checkerboard selection of the slice through the 
                 * large dataset and the checkerboard selection of the process
                 * slice of the small data set as having the same shape.
                 */
                check = H5S_select_shape_same_test(file_small_ds_sid_1,
                                                   mem_large_ds_sid);
                VRFY((check == TRUE), "H5S_select_shape_same_test passed.");


                /* write the checker board selection of the slice from the in 
                 * memory large data set to the slice of the on disk small 
                 * dataset. 
                 */
#if CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG 
                HDfprintf(stdout, "%s:%d: start = %d %d %d %d %d.\n", 
                          fcnName, mpi_rank,
                          start[0], start[1], start[2], start[3], start[4]);
                HDfprintf(stdout, "%s:%d: mem/file extent dims = %d/%d.\n",
                          fcnName, mpi_rank,
                          H5Sget_simple_extent_ndims(mem_large_ds_sid),
                          H5Sget_simple_extent_ndims(file_small_ds_sid_1));
#endif 
                ret = H5Dwrite(small_dataset,
                               H5T_NATIVE_UINT32,
                               mem_large_ds_sid,
                               file_small_ds_sid_1,
                               xfer_plist,
                               large_ds_buf_0);
                VRFY((ret >= 0), "H5Dwrite() slice to large ds succeeded.");


                /* read the on disk process slice of the small dataset into memory */
                ret = H5Dread(small_dataset,
                              H5T_NATIVE_UINT32,
                              mem_small_ds_sid,
                              file_small_ds_sid_0,
                              xfer_plist,
                              small_ds_buf_1);
                VRFY((ret >= 0), "H5Dread() slice from small ds succeeded.");


                /* verify that expected data is retrieved */

                mis_match = FALSE;
                ptr_1 = small_ds_buf_1;

                expected_value = 
			(i * edge_size * edge_size * edge_size * edge_size) +
                        (j * edge_size * edge_size * edge_size) +
                        (k * edge_size * edge_size) +
                        (l * edge_size);

                start_index = mpi_rank * small_ds_slice_size;
                stop_index = start_index + small_ds_slice_size - 1;

                HDassert( 0 <= start_index );
                HDassert( start_index < stop_index );
                HDassert( stop_index <= (int)small_ds_size );

                data_ok = TRUE;

                for ( n = 0; n < start_index; n++ ) {

                    if ( *(ptr_1 + n) != 0 ) {

                        data_ok = FALSE;
                        *(ptr_1 + n) = 0;
                    }
                }

                data_ok &= checker_board_hyperslab_dr_pio_test__verify_data
                           (
                             ptr_1 + start_index,
                             small_rank - 1,
                             edge_size,
                             checker_edge_size,
                             expected_value,
                             (hbool_t)TRUE
                           );


                for ( n = stop_index; n < small_ds_size; n++ ) {

                    if ( *(ptr_1 + n) != 0 ) {

                        data_ok = FALSE;
                        *(ptr_1 + n) = 0;
                    }
                }

                VRFY((data_ok == TRUE), 
                     "large slice write slice to small slice data good.");

                l++;

            } while ( ( large_rank > 2 ) &&
                      ( (small_rank - 1) <= 1 ) &&
                      ( l < edge_size ) );
            k++;
        } while ( ( large_rank > 3 ) &&
                  ( (small_rank - 1) <= 2 ) &&
                  ( k < edge_size ) );
        j++;
    } while ( ( large_rank > 4 ) &&
              ( (small_rank - 1) <= 3 ) &&
              ( j < edge_size ) );


    /* Now write the contents of the process's slice of the in memory 
     * small data set to slices of the on disk large data set.  After 
     * each write, read the process's slice of the large data set back
     * into memory, and verify that it contains the expected data. 
     * Verify that H5S_select_shape_same() returns true on the memory 
     * and file selections.
     */

    start[0] = mpi_rank;
    stride[0] = 2 * (mpi_size + 1);
    count[0] = 1;
    block[0] = 1;

    for ( i = 1; i < large_rank; i++ ) {

        start[i] = 0;
        stride[i] = 2 * edge_size;
        count[i] = 1;
        block[i] = edge_size;
    }

    ret = H5Sselect_hyperslab(file_large_ds_sid_0,
                              H5S_SELECT_SET,
                              start,
                              stride,
                              count,
                              block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(file_large_ds_sid_0, set) suceeded");

    ret = H5Sselect_hyperslab(mem_large_ds_sid,
                              H5S_SELECT_SET,
                              start,
                              stride,
                              count,
                              block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(mem_small_ds_sid, set) suceeded");

    /* setup a checkerboard selection of the slice of the in memory small 
     * data set associated with the process's mpi rank.
     */

    sel_start[0] = sel_start[1] = sel_start[2] = sel_start[3] = sel_start[4] = 0;
    sel_start[small_ds_offset] = mpi_rank;

    checker_board_hyperslab_dr_pio_test__select_checker_board(mpi_rank,
                                                              mem_small_ds_sid,
                                                              small_rank,
                                                              edge_size,
                                                              checker_edge_size,
                                                              small_rank - 1,
                                                              sel_start);

    /* set up start, stride, count, and block -- note that we will
     * change start[] so as to write checkerboard selections of slices 
     * of the small data set to slices of the large data set.
     */
    for ( i = 0; i < PAR_SS_DR_MAX_RANK; i++ ) {

        start[i] = 0;
        stride[i] = 2 * edge_size;
        count[i] = 1;
        if ( (PAR_SS_DR_MAX_RANK - i) > (small_rank - 1) ) {

            block[i] = 1;

        } else {

            block[i] = edge_size;
        }
    }

    /* zero out the in memory large ds */
    ptr_1 = large_ds_buf_1;
    for ( n = 0; n < (int)large_ds_size; n++ ) {

        *ptr_1 = 0;
        ptr_1++;
    }

#if CONTIG_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG
    HDfprintf(stdout, 
         "%s writing process checkerboard selections of slices of small ds to process slices of large ds on disk.\n",
         fcnName);
#endif 

    if ( PAR_SS_DR_MAX_RANK - large_rank == 0 ) {

        i = mpi_rank;

    } else {

        i = 0;
    }

    /* since large_rank is at most PAR_SS_DR_MAX_RANK, no need to 
     * loop over it -- either we are setting i to mpi_rank, or
     * we are setting it to zero.  It will not change during the 
     * test.
     */

    if ( PAR_SS_DR_MAX_RANK - large_rank == 1 ) {

        j = mpi_rank;

    } else {

        j = 0;
    }

    do {
        if ( PAR_SS_DR_MAX_RANK - large_rank == 2 ) {

            k = mpi_rank;

        } else {

            k = 0;
        }

        do {
            /* since small rank >= 2 and large_rank > small_rank, we 
             * have large_rank >= 3.  Since PAR_SS_DR_MAX_RANK == 5
             * (baring major re-orgaization), this gives us:
             *
             *     (PAR_SS_DR_MAX_RANK - large_rank) <= 2
             *
             * so no need to repeat the test in the outer loops --
             * just set l = 0.
             */

            l = 0;
            do {
                /* we know that small_rank >= 1 and that large_rank > small_rank
                 * by the assertions at the head of this function.  Thus no
                 * need for another inner loop.
                 */

                /* Zero out this processes slice of the on disk large data set.
                 * Note that this will leave one slice with its original data
                 * as there is one more slice than processes.
                  */
                ret = H5Dwrite(large_dataset,
                               H5T_NATIVE_UINT32,
                               mem_large_ds_sid,
                               file_large_ds_sid_0,
                               xfer_plist,
                               large_ds_buf_2);
		VRFY((ret != FAIL), "H5Dwrite() to zero large ds suceeded");


                /* select the portion of the in memory large cube to which we
                 * are going to write data.
                 */
                start[0] = i;
                start[1] = j;
                start[2] = k;
                start[3] = l;
                start[4] = 0;

                HDassert( ( start[0] == 0 ) || ( 0 < small_ds_offset + 1 ) );
                HDassert( ( start[1] == 0 ) || ( 1 < small_ds_offset + 1 ) );
                HDassert( ( start[2] == 0 ) || ( 2 < small_ds_offset + 1 ) );
                HDassert( ( start[3] == 0 ) || ( 3 < small_ds_offset + 1 ) );
                HDassert( ( start[4] == 0 ) || ( 4 < small_ds_offset + 1 ) );

                checker_board_hyperslab_dr_pio_test__select_checker_board
                (
                  mpi_rank,
                  file_large_ds_sid_1,
                  large_rank,
                  edge_size,
                  checker_edge_size,
                  small_rank - 1,
                  start
                );


                /* verify that H5S_select_shape_same() reports the in
                 * memory small data set slice selection and the
                 * on disk slice through the large data set selection
                 * as having the same shape.
                 */
                check = H5S_select_shape_same_test(mem_small_ds_sid,
                                                   file_large_ds_sid_1);
		VRFY((check == TRUE), "H5S_select_shape_same_test passed");


                /* write the small data set slice from memory to the 
                 * target slice of the disk data set 
                 */
#if CHECKER_BOARD_HYPERSLAB_DR_PIO_TEST__RUN_TEST__DEBUG 
                HDfprintf(stdout, "%s:%d: start = %d %d %d %d %d.\n", 
                          fcnName, mpi_rank,
                          start[0], start[1], start[2], start[3], start[4]);
                HDfprintf(stdout, "%s:%d: mem/file extent dims = %d/%d.\n",
                          fcnName, mpi_rank,
                          H5Sget_simple_extent_ndims(mem_small_ds_sid),
                          H5Sget_simple_extent_ndims(file_large_ds_sid_1));
#endif 
                ret = H5Dwrite(large_dataset,
                               H5T_NATIVE_UINT32,
                               mem_small_ds_sid,
                               file_large_ds_sid_1,
                               xfer_plist,
                               small_ds_buf_0);
		VRFY((ret != FAIL), 
                      "H5Dwrite of small ds slice to large ds succeeded");


                /* read this processes slice on the on disk large 
                 * data set into memory.
                 */

                ret = H5Dread(large_dataset,
                              H5T_NATIVE_UINT32,
                              mem_large_ds_sid,
                              file_large_ds_sid_0,
                              xfer_plist,
                              large_ds_buf_1);
                VRFY((ret != FAIL), 
                     "H5Dread() of process slice of large ds succeeded");


                /* verify that the expected data and only the
                 * expected data was read.
                 */
                ptr_1 = large_ds_buf_1;
                expected_value = (uint32_t)(mpi_rank) * small_ds_slice_size;


                start_index = (i * edge_size * edge_size * edge_size * edge_size) +
                              (j * edge_size * edge_size * edge_size) +
                              (k * edge_size * edge_size) +
                              (l * edge_size);
                stop_index = start_index + (int)small_ds_slice_size - 1;

                HDassert( 0 <= start_index );
                HDassert( start_index < stop_index );
                HDassert( stop_index < (int)large_ds_size );


                mis_match = FALSE;

                data_ok = TRUE;

                for ( n = 0; n < start_index; n++ ) {

                    if ( *(ptr_1 + n) != 0 ) {

                        data_ok = FALSE;
                        *(ptr_1 + n) = 0;
                    }
                }

                data_ok &= checker_board_hyperslab_dr_pio_test__verify_data
                           (
                             ptr_1 + start_index,
                             small_rank - 1,
                             edge_size,
                             checker_edge_size,
                             expected_value,
                             (hbool_t)TRUE
                           );


                for ( n = stop_index; n < small_ds_size; n++ ) {

                    if ( *(ptr_1 + n) != 0 ) {

                        data_ok = FALSE;
                        *(ptr_1 + n) = 0;
                    }
                }

                VRFY((data_ok == TRUE), 
                     "small ds cb slice write to large ds slice data good.");

                l++;

            } while ( ( large_rank > 2 ) &&
                      ( (small_rank - 1) <= 1 ) &&
                      ( l < edge_size ) );
            k++;
        } while ( ( large_rank > 3 ) &&
                  ( (small_rank - 1) <= 2 ) &&
                  ( k < edge_size ) );
        j++;
    } while ( ( large_rank > 4 ) &&
              ( (small_rank - 1) <= 3 ) &&
              ( j < edge_size ) );


    /* Close dataspaces */
    ret = H5Sclose(full_mem_small_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(full_mem_small_ds_sid) succeeded");

    ret = H5Sclose(full_file_small_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(full_file_small_ds_sid) succeeded");

    ret = H5Sclose(mem_small_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(mem_small_ds_sid) succeeded");

    ret = H5Sclose(file_small_ds_sid_0);
    VRFY((ret != FAIL), "H5Sclose(file_small_ds_sid_0) succeeded");

    ret = H5Sclose(file_small_ds_sid_1);
    VRFY((ret != FAIL), "H5Sclose(file_small_ds_sid_1) succeeded");

    ret = H5Sclose(small_ds_slice_sid);
    VRFY((ret != FAIL), "H5Sclose(small_ds_slice_sid) succeeded");

    ret = H5Sclose(full_mem_large_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(full_mem_large_ds_sid) succeeded");

    ret = H5Sclose(full_file_large_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(full_file_large_ds_sid) succeeded");

    ret = H5Sclose(mem_large_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(mem_large_ds_sid) succeeded");

    ret = H5Sclose(file_large_ds_sid_0);
    VRFY((ret != FAIL), "H5Sclose(mem_large_ds_sid) succeeded");

    ret = H5Sclose(file_large_ds_sid_1);
    VRFY((ret != FAIL), "H5Sclose(mem_large_ds_sid) succeeded");

    ret = H5Sclose(mem_large_ds_process_slice_sid);
    VRFY((ret != FAIL), "H5Sclose(mem_large_ds_process_slice_sid) succeeded");

    ret = H5Sclose(file_large_ds_process_slice_sid);
    VRFY((ret != FAIL), "H5Sclose(file_large_ds_process_slice_sid) succeeded");

    ret = H5Sclose(large_ds_slice_sid);
    VRFY((ret != FAIL), "H5Sclose(large_ds_slice_sid) succeeded");


    /* Close Datasets */
    ret = H5Dclose(small_dataset);
    VRFY((ret != FAIL), "H5Dclose(small_dataset) succeeded");

    ret = H5Dclose(large_dataset);
    VRFY((ret != FAIL), "H5Dclose(large_dataset) succeeded");


    /* close the file collectively */
    MESG("about to close file.");
    ret = H5Fclose(fid);
    VRFY((ret != FAIL), "file close succeeded");

    /* Free memory buffers */
    if ( small_ds_buf_0 != NULL ) HDfree(small_ds_buf_0);
    if ( small_ds_buf_1 != NULL ) HDfree(small_ds_buf_1);
    if ( small_ds_buf_2 != NULL ) HDfree(small_ds_buf_2);
    if ( small_ds_slice_buf != NULL ) HDfree(small_ds_slice_buf);

    if ( large_ds_buf_0 != NULL ) HDfree(large_ds_buf_0);
    if ( large_ds_buf_1 != NULL ) HDfree(large_ds_buf_1);
    if ( large_ds_buf_2 != NULL ) HDfree(large_ds_buf_2);
    if ( large_ds_slice_buf != NULL ) HDfree(large_ds_slice_buf);

    return;

} /* checker_board_hyperslab_dr_pio_test__run_test() */


/*-------------------------------------------------------------------------
 * Function:	checker_board_hyperslab_dr_pio_test()
 *
 * Purpose:	Test I/O to/from hyperslab selections of different rank in
 *		the parallel case.
 *
 * Return:	void
 *
 * Programmer:	JRM -- 9/18/09
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

void
checker_board_hyperslab_dr_pio_test(void)
{
    int	        test_num = 0;
    int		edge_size = 10;
    int         checker_edge_size = 3;
    int		chunk_edge_size = 0;
    int	        small_rank = 3;
    int	        large_rank = 4;
    int  	use_collective_io = 1;
    hid_t	dset_type = H5T_STD_U32LE;
#if 0 
    int DebugWait = 1;
 
    while (DebugWait) ;
#endif 

    for ( large_rank = 3; large_rank <= PAR_SS_DR_MAX_RANK; large_rank++ ) {

        for ( small_rank = 2; small_rank < large_rank; small_rank++ ) {

            for ( use_collective_io = 0; 
                  use_collective_io <= 1; 
                  use_collective_io++ ) {

                chunk_edge_size = 0;
                checker_board_hyperslab_dr_pio_test__run_test(test_num,
                                                              edge_size,
                                                              checker_edge_size,
                                                              chunk_edge_size,
                                                              small_rank,
                                                              large_rank,
                                                              (hbool_t)use_collective_io,
                                                              dset_type);
                test_num++;

                chunk_edge_size = 5;
                checker_board_hyperslab_dr_pio_test__run_test(test_num,
                                                              edge_size,
                                                              checker_edge_size,
                                                              chunk_edge_size,
                                                              small_rank,
                                                              large_rank,
                                                              (hbool_t)use_collective_io,
                                                              dset_type);
                test_num++;

            }
        }
    }

    return;

} /* checker_board_hyperslab_dr_pio_test() */

