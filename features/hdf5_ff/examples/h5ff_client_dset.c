/* 
 * h5ff_client_dset.c: Client side test for Dataset routines.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "mchecksum.h"
#include "mpi.h"
#include "hdf5.h"

static uint64_t
checksum_crc64(const void *buf, size_t buf_size)
{
    const char *hash_method = "crc64";
    size_t hash_size;
    mchecksum_object_t checksum;
    uint64_t ret_value = 0;

    /* Initialize checksum */
    mchecksum_init(hash_method, &checksum);

    /* Update checksum */
    mchecksum_update(checksum, buf, buf_size);

    /* Get size of checksum */
    hash_size = mchecksum_get_size(checksum);

    assert(hash_size == sizeof(uint64_t));

    /* get checksum value */
    mchecksum_get(checksum, &ret_value, hash_size, 1);

    /* Destroy checksum */
    mchecksum_destroy(checksum);

    return ret_value;
}

int main(int argc, char **argv) {
    char file_name[50];
    hid_t file_id;
    hid_t gid1, gid2, gid3;
    hid_t sid, scalar, dtid;
    hid_t did1, did2, did3;
    hid_t tid1, tid2, tid3, rid1, rid2, rid3;
    hid_t fapl_id, trspl_id, dxpl_id;
    hid_t e_stack;
    hid_t esid;

    uint64_t version;
    uint64_t trans_num;

    int32_t *wdata1 = NULL;
    int16_t *wdata3 = NULL;
    int32_t *ex_wdata = NULL, *ex_rdata = NULL;
    int32_t *rdata1 = NULL, *rdata2 = NULL;
    int16_t *rdata3 = NULL;
    int32_t element = 0;
    const unsigned int nelem=60;
    hsize_t dims[1], max_dims[1];
    hsize_t extent;

    void *dset_token1, *dset_token2, *dset_token3;
    size_t token_size1, token_size2, token_size3;
    int my_rank, my_size;
    int provided;
    MPI_Request mpi_req, mpi_reqs[6];

    H5ES_status_t status;
    size_t num_events = 0;
    unsigned int i = 0;
    uint64_t array_cs = 0, elmt_cs = 0, read1_cs = 0, read2_cs = 0;
    uint32_t cs_scope = 0;
    herr_t ret;

    sprintf(file_name, "%s_%s", getenv("USER"), "eff_file_dset.h5");

    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
    if(MPI_THREAD_MULTIPLE != provided) {
        fprintf(stderr, "MPI does not have MPI_THREAD_MULTIPLE support\n");
        exit(1);
    }

    /* Call EFF_init to initialize the EFF stack. */
    EFF_init(MPI_COMM_WORLD, MPI_INFO_NULL);

    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &my_size);
    printf("APP processes = %d, my rank is %d\n", my_size, my_rank);

    /* Choose the IOD VOL plugin to use with this file. */
    fapl_id = H5Pcreate (H5P_FILE_ACCESS);
    H5Pset_fapl_iod(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL);

    /* allocate and initialize arrays for dataset I/O */
    wdata1 = malloc (sizeof(int32_t)*nelem);
    wdata3 = malloc (sizeof(int16_t)*nelem);
    rdata1 = calloc (nelem, sizeof(int32_t));
    rdata2 = calloc (nelem, sizeof(int32_t));
    rdata3 = calloc (nelem, sizeof(int32_t));
    for(i=0;i<nelem;++i) {
        wdata1[i]=i;
        wdata3[i]=i;
    }

    /* create an event Queue for managing asynchronous requests. */
    e_stack = H5EScreate();
    assert(e_stack);

    /* set the metada data integrity checks to happend at transfer through mercury */
    //cs_scope |= H5_CHECKSUM_TRANSFER;
    //ret = H5Pset_metadata_integrity_scope(fapl_id, cs_scope);
    //assert(ret == 0);

    /* create the file. */
    file_id = H5Fcreate_ff(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id, H5_EVENT_STACK_NULL);
    assert(file_id > 0);

    /* create 1-D dataspace with 60 elements */
    dims [0] = nelem;
    max_dims [0] = H5S_UNLIMITED;
    sid = H5Screate_simple(1, dims, max_dims);

    scalar = H5Screate(H5S_SCALAR);

    dtid = H5Tcopy(H5T_STD_I32LE);

    /* acquire container version 1 - EXACT.  
       This can be asynchronous, but here we need the acquired ID 
       right after the call to start the transaction so we make synchronous. */
    if(0 == my_rank) {
        version = 1;
        rid1 = H5RCacquire(file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL);
    }
    MPI_Bcast(&version, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD);
    assert(1 == version);
    if (my_rank != 0)
        rid1 = H5RCcreate(file_id, version);

    /* create transaction object */
    tid1 = H5TRcreate(file_id, rid1, (uint64_t)2);
    assert(tid1);

    /* start transaction 2 with default Leader/Delegate model. Leader
       which is rank 0 here starts the transaction. It can be
       asynchronous, but we make it synchronous here so that the
       Leader can tell its delegates that the transaction is
       started. */
    if(0 == my_rank) {
        hid_t dcpl_id;

        trans_num = 2;
        ret = H5TRstart(tid1, H5P_DEFAULT, H5_EVENT_STACK_NULL);
        assert(0 == ret);

        /* Leader also create some objects in transaction 1 */

        /* create group hierarchy /G1/G2/G3 */
        gid1 = H5Gcreate_ff(file_id, "G1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
        assert(gid1 > 0);
        gid2 = H5Gcreate_ff(gid1, "G2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
        assert(gid2 > 0);
        gid3 = H5Gcreate_ff(gid2, "G3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
        assert(gid3 > 0);

        dcpl_id = H5Pcreate (H5P_DATASET_CREATE);
        /* MSC - Was working in FF, but not working now. Seems like IOD regression bug. */
        H5Pset_dcpl_dim_layout(dcpl_id, H5D_COL_MAJOR);
        H5Pset_dcpl_stripe_count(dcpl_id, 4);
        H5Pset_dcpl_stripe_size(dcpl_id, 5);

        /* create datasets */
        did1 = H5Dcreate_ff(gid1, "D1", dtid, sid, H5P_DEFAULT, dcpl_id, H5P_DEFAULT, tid1, e_stack);
        assert(did1 > 0);
        H5Pclose(dcpl_id);

        did2 = H5Dcreate_ff(gid2, "D2", dtid, scalar, 
                            H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
        assert(did2 > 0);
        did3 = H5Dcreate_ff(gid3, "D3", dtid, sid, 
                            H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
        assert(did3 > 0);
    }

    /* Tell Delegates that transaction 1 is started */
    MPI_Ibcast(&trans_num, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD, &mpi_req);

    /* Do the local-to-global, global-to-local, so all delegates can
       write to the dsets created in transaction 1 */

    if(0 == my_rank) {
        /* get the token size of each dset */
        ret = H5Oget_token(did1, NULL, &token_size1);
        assert(0 == ret);
        ret = H5Oget_token(did2, NULL, &token_size2);
        assert(0 == ret);
        ret = H5Oget_token(did3, NULL, &token_size3);
        assert(0 == ret);

        /* allocate buffers for each token */
        dset_token1 = malloc(token_size1);
        dset_token2 = malloc(token_size2);
        dset_token3 = malloc(token_size3);

        /* get the token buffer */
        ret = H5Oget_token(did1, dset_token1, &token_size1);
        assert(0 == ret);
        ret = H5Oget_token(did2, dset_token2, &token_size2);
        assert(0 == ret);
        ret = H5Oget_token(did3, dset_token3, &token_size3);
        assert(0 == ret);

        /* make sure the create operations have completed before
           telling the delegates to open them */
        H5ESget_count(e_stack, &num_events);
        H5ESwait_all(e_stack, &status);
        H5ESclear(e_stack);
        printf("%d events in event stack. Completion status = %d\n", num_events, status);
        assert(status == H5ES_STATUS_SUCCEED);

        /* bcast the token sizes and the tokens */ 
        MPI_Ibcast(&token_size1, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[0]);
        MPI_Ibcast(&token_size2, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[1]);
        MPI_Ibcast(&token_size3, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[2]);
        MPI_Ibcast(dset_token1, token_size1, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[3]);
        MPI_Ibcast(dset_token2, token_size2, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[4]);
        MPI_Ibcast(dset_token3, token_size3, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[5]);
    }

    /* Leader can continue writing to transaction 2, 
       while others wait for the ibcast to complete */
    if(0 != my_rank) {
        MPI_Wait(&mpi_req, MPI_STATUS_IGNORE);
        assert(2 == trans_num);

        /* recieve the token sizes */ 
        MPI_Ibcast(&token_size1, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[0]);
        MPI_Ibcast(&token_size2, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[1]);
        MPI_Ibcast(&token_size3, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[2]);
        MPI_Waitall(3, mpi_reqs, MPI_STATUS_IGNORE);

        /* allocate buffers for each token */
        dset_token1 = malloc(token_size1);
        dset_token2 = malloc(token_size2);
        dset_token3 = malloc(token_size3);

        /* recieve the tokens */
        MPI_Ibcast(dset_token1, token_size1, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[0]);
        MPI_Ibcast(dset_token2, token_size2, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[1]);
        MPI_Ibcast(dset_token3, token_size3, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[2]);
        MPI_Waitall(3, mpi_reqs, MPI_STATUS_IGNORE);

        did1 = H5Oopen_by_token(dset_token1, tid1, e_stack);
        did2 = H5Oopen_by_token(dset_token2, tid1, e_stack);
        did3 = H5Oopen_by_token(dset_token3, tid1, e_stack);
    }

    /* write data to datasets */

    /* Attach a checksum to the dxpl which is verified all the way
       down at the server */
    dxpl_id = H5Pcreate (H5P_DATASET_XFER);
    array_cs = checksum_crc64(wdata1, sizeof(int32_t) * nelem);
    H5Pset_dxpl_checksum(dxpl_id, array_cs);
    printf("Checksum computed for raw data: %016lX\n", array_cs);
    ret = H5Dwrite_ff(did1, dtid, H5S_ALL, H5S_ALL, dxpl_id, wdata1, tid1, e_stack);
    assert(ret == 0);

    /* Raw data write on D2. same as previous, but here we indicate
       through the property list that we want to inject a
       corruption. */
    //array_cs = checksum_crc64(wdata2, sizeof(int32_t) * nelem);
    //H5Pset_dxpl_checksum(dxpl_id, array_cs);
    //H5Pset_dxpl_inject_corruption(dxpl_id, 1);

    /* tell HDF5 to disable data integrity checks stored at IOD for this write;
       The transfer checksum will still capture the corruption. */
    //cs_scope |= H5_CHECKSUM_TRANSFER;
    //ret = H5Pset_rawdata_integrity_scope(dxpl_id, cs_scope);
    //assert(ret == 0);

    element = 450;
    elmt_cs = checksum_crc64(&element, sizeof(int32_t));
    H5Pset_dxpl_checksum(dxpl_id, elmt_cs);

    ret = H5Dwrite_ff(did2, dtid, scalar, scalar, dxpl_id, &element, tid1, e_stack);
    assert(ret == 0);

    /* tell HDF5 to disable all data integrity checks for this write */
    cs_scope = 0;
    ret = H5Pset_rawdata_integrity_scope(dxpl_id, cs_scope);
    assert(ret == 0);

    /* Raw data write on D3. Same as previous; however we specify that
       the data in the buffer is in BE byte order. Type conversion will
       happen at the server when we detect that the dataset type is of
       LE order and the datatype here is in BE order. */
    ret = H5Dwrite_ff(did3, H5T_STD_I16BE, sid, sid, dxpl_id, wdata3, tid1, e_stack);
    assert(ret == 0);

    H5Pclose(dxpl_id);

    /* none leader procs have to complete operations before notifying the leader */
    if(0 != my_rank) {
        H5ESget_count(e_stack, &num_events);
        H5ESwait_all(e_stack, &status);
        H5ESclear(e_stack);
        printf("%d events in event stack. Completion status = %d\n", num_events, status);
        assert(status == H5ES_STATUS_SUCCEED);
    }

    /* Barrier to make sure all processes are done writing so Process
       0 can finish transaction 1 and acquire a read context on it. */
    MPI_Barrier(MPI_COMM_WORLD);

    /* Leader process finished the transaction after all clients
       finish their updates. Leader also asks the library to acquire
       the committed transaction, that becomes a readable version
       after the commit completes. */
    if(0 == my_rank) {
        MPI_Wait(&mpi_req, MPI_STATUS_IGNORE);

        /* make this synchronous so we know the container version has been acquired */
        ret = H5TRfinish(tid1, H5P_DEFAULT, &rid2, H5_EVENT_STACK_NULL);
        assert(0 == ret);
    }

    /* Local op */
    ret = H5TRclose(tid1);
    assert(0 == ret);

    /* release container version 1. This is async. */
    if(0 == my_rank) {
        ret = H5RCrelease(rid1, e_stack);
        assert(0 == ret);
    }

    H5ESget_count(e_stack, &num_events);
    H5ESwait_all(e_stack, &status);
    printf("%d events in event stack. H5ESwait_all Completion status = %d\n", num_events, status);
    H5ESclear(e_stack);
    assert(status == H5ES_STATUS_SUCCEED);

    /* Tell other procs that container version 2 is acquired */
    version = 2;
    MPI_Bcast(&version, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD);

    /* other processes just create a read context object; no need to
       acquire it */
    if(0 != my_rank) {
        rid2 = H5RCcreate(file_id, version);
        assert(rid2 > 0);
    }

    /* close some objects */
    if(0 == my_rank) {
        ret = H5Gclose_ff(gid1, e_stack);
        assert(ret == 0);
        ret = H5Gclose_ff(gid2, e_stack);
        assert(ret == 0);
        ret = H5Gclose_ff(gid3, e_stack);
        assert(ret == 0);
    }

    ret = H5Dclose_ff(did1, H5_EVENT_STACK_NULL);
    assert(ret == 0);

    /* Open objects closed before */
    gid1 = H5Gopen_ff(file_id, "G1", H5P_DEFAULT, rid2, e_stack);
    did1 = H5Dopen_ff(file_id, "G1/D1", H5P_DEFAULT, rid2, e_stack);

    /* read data from datasets with read version 1. */

    dxpl_id = H5Pcreate (H5P_DATASET_XFER);
    /* Give a location to the DXPL to store the checksum once the read has completed */
    H5Pset_dxpl_checksum_ptr(dxpl_id, &read1_cs);
    ret = H5Dread_ff(did1, dtid, H5S_ALL, H5S_ALL, dxpl_id, rdata1, rid2, e_stack);
    assert(ret == 0);
    H5Pclose(dxpl_id);

    /* Here we demo that we can pass hints down to the IOD server. 
       We create a new property, for demo purposes, to tell the server to inject 
       corrupted data into the received array, and hence an incorrect checksum. 
       This also detects that we are passing checksum values in both directions for 
       raw data to ensure data integrity. The read should fail when we wait on it in
       the H5Dclose(D2) later, but for the demo purposes we are not actually going to 
       fail the close, but just print a Fatal error. */
    dxpl_id = H5Pcreate (H5P_DATASET_XFER);
    H5Pset_dxpl_inject_corruption(dxpl_id, 1);
    /* Give a location to the DXPL to store the checksum once the read has completed */
    H5Pset_dxpl_checksum_ptr(dxpl_id, &read2_cs);
    ret = H5Dread_ff(did1, dtid, H5S_ALL, H5S_ALL, dxpl_id, rdata2, rid2, e_stack);
    assert(ret == 0);
    H5Pclose(dxpl_id);

    dxpl_id = H5Pcreate (H5P_DATASET_XFER);
    /* tell HDF5 to disable all data integrity checks for this read */
    cs_scope = 0;
    ret = H5Pset_rawdata_integrity_scope(dxpl_id, cs_scope);
    assert(ret == 0);
    /* Raw data read on D3. This is asynchronous.  Note that the type
       is different than the dataset type. */
    ret = H5Dread_ff(did3, H5T_STD_I16BE, H5S_ALL, H5S_ALL, dxpl_id, rdata3, rid2, e_stack);
    assert(ret == 0);
    H5Pclose(dxpl_id);

    /* Raw data read on D1. This is asynchronous.  The read is done into a 
       noncontiguous memory dataspace selection */
    {
        hid_t mem_space;
        hsize_t start = 0;
        hsize_t stride = 2;
        hsize_t count = nelem;
        hsize_t block = 1;
        int *buf = NULL;

        buf = calloc (nelem*2, sizeof(int));

        /* create a dataspace. This is a local Bookeeping operation that 
           does not touch the file */
        dims [0] = nelem*2;
        mem_space = H5Screate_simple(1, dims, NULL);
        H5Sselect_hyperslab(mem_space, H5S_SELECT_SET, &start,&stride,&count,&block);

        ret = H5Dread_ff(did1, H5T_STD_I32LE, mem_space, H5S_ALL, H5P_DEFAULT, buf, 
                         rid2, e_stack);
        assert(ret == 0);
        H5Sclose(mem_space);

        H5EStest(e_stack, 0, &status);
        printf("ESTest H5Dread Completion status = %d\n", status);

        H5ESwait(e_stack, 0, &status);
        printf("ESWait H5Dread Completion status = %d\n", status);
        assert (status);

        printf("Printing all Dataset values. We should have a 0 after each element: ");
        for(i=0;i<120;++i)
            printf("%d ", buf[i]);
        printf("\n");

        free(buf);
    }

    element = 0;
    ret = H5Dread_ff(did2, dtid, scalar, scalar, H5P_DEFAULT, &element, rid2, e_stack);
    assert(ret == 0);

    H5ESwait(e_stack, 0, &status);
    printf("ESWait H5Dread Completion status = %d\n", status);
    assert (status);

    printf("Rank %d read value %d\n", my_rank, element);
    assert(element == 450);

    /* create & start transaction 3 with num_peers = my_size. This
       means all processes are transaction leaders, and all have to
       call start and finish on the transaction. */
    tid2 = H5TRcreate(file_id, rid2, (uint64_t)3);
    assert(tid2);
    trspl_id = H5Pcreate (H5P_TR_START);
    ret = H5Pset_trspl_num_peers(trspl_id, my_size);
    assert(0 == ret);
    ret = H5TRstart(tid2, trspl_id, e_stack);
    assert(0 == ret);
    ret = H5Pclose(trspl_id);
    assert(0 == ret);

    /* Do more updates on transaction 2 */

    {
        extent = nelem+10;

        ret = H5Dset_extent_ff(did1, &extent, tid2, e_stack);
        assert(ret == 0);
    }

    H5ESget_count(e_stack, &num_events);
    H5ESwait_all(e_stack, &status);
    printf("%d events in event stack. H5ESwait_all Completion status = %d\n", num_events, status);
    H5ESclear(e_stack);
    MPI_Barrier(MPI_COMM_WORLD);

    esid = H5Dget_space(did1);
    ex_wdata = malloc (sizeof(int32_t)*(nelem+10));
    ex_rdata = malloc (sizeof(int32_t)*(nelem+10));

    for(i=0;i<nelem+10;++i) {
        ex_wdata[i] = i;
        ex_rdata[i] = 0;
    }

    ret = H5Dwrite_ff(did1, dtid, esid, esid, H5P_DEFAULT, 
                      ex_wdata, tid2, e_stack);
    assert(ret == 0);

    /* finish transaction 3 */
    ret = H5TRfinish(tid2, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL);
    assert(0 == ret);

    H5ESget_count(e_stack, &num_events);
    H5ESwait_all(e_stack, &status);
    printf("%d events in event stack. H5ESwait_all Completion status = %d\n", 
           num_events, status);
    H5ESclear(e_stack);
    MPI_Barrier(MPI_COMM_WORLD);

    if(0 == my_rank) {
        version = 3;
        rid3 = H5RCacquire(file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL);
    }
    MPI_Bcast( &version, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD );
    assert(3 == version);
    if (my_rank != 0)
        rid3 = H5RCcreate(file_id, version);

    ret = H5Dread_ff(did1, dtid, H5S_ALL, H5S_ALL, H5P_DEFAULT, ex_rdata, 
                     rid3, H5_EVENT_STACK_NULL);
    assert(ret == 0);

    printf("Printing all Extended Dataset values: ");
    for(i=0 ; i<nelem+10 ; ++i) {
        printf("%d ", ex_rdata[i]);
        assert(ex_rdata[i] == ex_wdata[i]);
    }
    printf("\n");

    H5Sclose(esid);

    MPI_Barrier(MPI_COMM_WORLD);    
    if(my_rank == 0) {
        ret = H5RCrelease(rid2, e_stack);
        assert(0 == ret);
        ret = H5RCrelease(rid3, e_stack);
        assert(0 == ret);
    }

    /* close objects */
    ret = H5Dclose_ff(did1, e_stack);
    assert(ret == 0);
    ret = H5Dclose_ff(did2, e_stack);
    assert(ret == 0);
    ret = H5Dclose_ff(did3, e_stack);
    assert(ret == 0);
    ret = H5Gclose_ff(gid1, e_stack);
    assert(ret == 0);

    H5Fclose_ff(file_id, 1, H5_EVENT_STACK_NULL);

    H5ESget_count(e_stack, &num_events);

    H5EStest_all(e_stack, &status);
    printf("%d events in event stack. H5EStest_all Completion status = %d\n", num_events, status);

    H5ESwait_all(e_stack, &status);
    printf("%d events in event stack. H5ESwait_all Completion status = %d\n", num_events, status);

    H5EStest_all(e_stack, &status);
    printf("%d events in event stack. H5EStest_all Completion status = %d\n", num_events, status);

    ret = H5Sclose(sid);
    assert(ret == 0);
    ret = H5Sclose(scalar);
    assert(ret == 0);
    ret = H5Tclose(dtid);
    assert(ret == 0);
    ret = H5Pclose(fapl_id);
    assert(ret == 0);

    ret = H5RCclose(rid1);
    assert(0 == ret);
    ret = H5RCclose(rid2);
    assert(0 == ret);

    H5ESclear(e_stack);

    printf("Read Data1: ");
    for(i=0;i<nelem;++i)
        printf("%d ",rdata1[i]);
    printf("\n");
    printf("Checksum Receieved = %016lX  Checksum Computed = %016lX (Should be Equal)\n", 
            read1_cs, array_cs);

    printf("Read Data2 (corrupted): ");
    for(i=0;i<nelem;++i)
        printf("%d ",rdata2[i]);
    printf("\n");
    printf("Checksum Receieved = %016lX  Checksum Computed = %016lX (Should NOT be Equal)\n", 
            read2_cs, array_cs);

    assert(read1_cs == array_cs);
    assert(read2_cs != array_cs);

    printf("Read Data3 with datatype conversion (expect to see same as others since it is converted twice): \n");
    for(i=0;i<nelem;++i)
        printf("%"PRId16" ",rdata3[i]);
    printf("\n");

    ret = H5ESclose(e_stack);
    assert(ret == 0);

    free(wdata1);
    free(wdata3);
    free(rdata1);
    free(rdata2);
    free(rdata3);
    free(ex_wdata);
    free(ex_rdata);

    free(dset_token1);
    free(dset_token2);
    free(dset_token3);

    MPI_Barrier(MPI_COMM_WORLD);
    EFF_finalize();
    MPI_Finalize();

    return 0;
}
