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

#define H5S_PACKAGE             /*suppress error about including H5Spkg */
#define H5G_PACKAGE             /*suppress error about including H5Gpkg */

#include "H5private.h"          /* Generic Functions                    */
#include "H5ACprivate.h"        /* Metadata Cache                       */
#include "H5Dprivate.h"         /* Dataset Functions                    */
#include "H5Eprivate.h"         /* Error Handling                       */
#include "H5Fprivate.h"         /* Files                                */
#include "H5FDprivate.h"        /* File Drivers                         */
#include "H5Gpkg.h"             /* Group Functions                      */
#include "H5Iprivate.h"         /* ID Functions                         */
#include "H5MMprivate.h"        /* Memory Allocation                    */
#include "H5Oprivate.h"         /* Object Headers                       */
#include "H5Spkg.h"             /* Dataspace Functions                  */
#include "H5TBprivate.h"        /* Threaded, Balanced, Binary Trees     */

#ifdef H5_HAVE_FPHDF5

#include "H5FDfphdf5.h"         /* File Driver for FPHDF5               */
#include "H5FPprivate.h"        /* Flexible Parallel Functions          */

/* Pablo mask */
#define PABLO_MASK      H5FPclient_mask

/* Is the interface initialized? */
static int interface_initialize_g = 0;
#define INTERFACE_INIT  NULL

/* local functions */
static unsigned H5FP_gen_request_id(void);
static herr_t H5FP_dump_to_file(H5FD_t *file, hid_t dxpl_id, H5FP_read *sap_read);

/*
 *===----------------------------------------------------------------------===
 *                    Public Library (non-API) Functions
 *===----------------------------------------------------------------------===
 */

/*
 * Function:    H5FP_request_open
 * Purpose:     Request an open of a file from the SAP. You pass in the
 *              metadata string (MDATA) (the filename), it's length in
 *              (MD_LEN), and the type of the object you're trying to
 *              open (OBJ_TYPE). The request ID is returned in a pointer
 *              supplied by the user.
 *
 *              The so-called "captain" process is in charge of telling
 *              the SAP that the processes opened a file. All processes
 *              opening the file, though, should call this function so
 *              that they can get the file ID that the SAP assigns to it.
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 28. August, 2002
 * Modifications:
 */
herr_t
H5FP_request_open(H5FP_obj_t obj_type, MPI_Offset maxaddr,
                  unsigned *file_id, unsigned *req_id)
{
    H5FP_request req;
    MPI_Status mpi_status;
    int mrc, my_rank;
    int ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5FP_request_open, FAIL);

    /* check args */
    assert(file_id);
    assert(req_id);

    HDmemset(&mpi_status, 0, sizeof(MPI_Status));

    if ((mrc = MPI_Comm_rank(H5FP_SAP_COMM, &my_rank)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Comm_rank failed", mrc);

    if ((unsigned)my_rank == H5FP_capt_rank) {
        /*
         * The captain process sends the information about the file to
         * the SAP.
         */
        HDmemset(&req, 0, sizeof(req));
        req.req_type = H5FP_REQ_OPEN;
        req.req_id = H5FP_gen_request_id();
        req.proc_rank = my_rank;
        req.md_size = 0;
        req.obj_type = obj_type;
        req.addr = maxaddr;

        if ((mrc = MPI_Send(&req, 1, H5FP_request_t, (int)H5FP_sap_rank,
                            H5FP_TAG_REQUEST, H5FP_SAP_COMM)) != MPI_SUCCESS)
            HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);
    }

    if ((mrc = MPI_Recv(file_id, 1, MPI_UNSIGNED, (int)H5FP_sap_rank,
                        H5FP_TAG_FILE_ID, H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);

done:
    *req_id = req.req_id;
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_request_lock
 * Purpose:     Request a lock on an object in a file from the SAP. The
 *              request ID is returned in a pointer supplied by the user.
 *              The status of the SAP is returned to the user in the
 *              supplied STATUS pointer.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 30. July, 2002
 * Modifications:
 */
herr_t
H5FP_request_lock(unsigned file_id, unsigned char *obj_oid,
                  H5FP_lock_t rw_lock, int last, unsigned *req_id,
                  H5FP_status_t *status)
{
    H5FP_request req;
    int mrc, my_rank;
    int ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5FP_request_lock, FAIL);

    /* check args */
    assert(obj_oid);
    assert(req_id);
    assert(status);

    HDmemset(&req, 0, sizeof(req));

    if ((mrc = MPI_Comm_rank(H5FP_SAP_COMM, &my_rank)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Comm_rank failed", mrc);

    *status = H5FP_STATUS_OK;
    req.req_type = last ? H5FP_REQ_LOCK_END : H5FP_REQ_LOCK;
    req.req_id = H5FP_gen_request_id();
    req.file_id = file_id;
    req.rw_lock = rw_lock;
    req.md_size = 0;
    req.proc_rank = my_rank;
    H5FP_COPY_OID(req.oid, obj_oid);

    if ((mrc = MPI_Send(&req, 1, H5FP_request_t, (int)H5FP_sap_rank,
                        H5FP_TAG_REQUEST, H5FP_SAP_COMM)) != MPI_SUCCESS) {
        *status = H5FP_STATUS_LOCK_FAILED;
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);
    }

    if (last) {
        /*
         * On the last lock in the lock-group to be acquired, we expect a
         * reply from the SAP
         */
        H5FP_reply sap_reply;
        MPI_Status mpi_status;

        HDmemset(&mpi_status, 0, sizeof(mpi_status));

        if ((mrc = MPI_Recv(&sap_reply, 1, H5FP_reply_t, (int)H5FP_sap_rank,
                            H5FP_TAG_REPLY, H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS)
            HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);

        *status = sap_reply.status;

        if (sap_reply.status != H5FP_STATUS_LOCK_ACQUIRED)
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTLOCK, FAIL, "can't lock object on server");
    }

done:
    *req_id = req.req_id;
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_request_release_lock
 * Purpose:     Release a lock on the file from the SAP. Request a lock
 *              on an object in a file from the SAP. The request ID is
 *              returned in a pointer supplied by the user. The status
 *              of the SAP is returned to the user in the supplied STATUS
 *              pointer.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 30. July, 2002
 * Modifications:
 */
herr_t
H5FP_request_release_lock(unsigned file_id, unsigned char *obj_oid,
                          int last, unsigned *req_id, H5FP_status_t *status)
{
    H5FP_request req;
    int mrc, my_rank;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5FP_request_release_lock, FAIL);

    /* check args */
    assert(obj_oid);
    assert(req_id);
    assert(status);

    HDmemset(&req, 0, sizeof(req));

    if ((mrc = MPI_Comm_rank(H5FP_SAP_COMM, &my_rank)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Comm_rank failed", mrc);

    *status = H5FP_STATUS_OK;
    H5FP_COPY_OID(req.oid, obj_oid);
    req.req_type = last ? H5FP_REQ_RELEASE_END : H5FP_REQ_RELEASE;
    req.req_id = H5FP_gen_request_id();
    req.file_id = file_id;
    req.md_size = 0;
    req.proc_rank = my_rank;

    if ((mrc = MPI_Send(&req, 1, H5FP_request_t, (int)H5FP_sap_rank,
                        H5FP_TAG_REQUEST, H5FP_SAP_COMM)) != MPI_SUCCESS) {
        *status = H5FP_STATUS_LOCK_RELEASE_FAILED;
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);
    }

    if (last) {
        /*
         * On the last lock released in this lock-group, we expect a
         * reply from the SAP
         */
        H5FP_reply sap_reply;
        MPI_Status mpi_status;

        HDmemset(&mpi_status, 0, sizeof(mpi_status));

        if ((mrc = MPI_Recv(&sap_reply, 1, H5FP_reply_t, (int)H5FP_sap_rank,
                            H5FP_TAG_REPLY, H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS)
            HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);

        *status = sap_reply.status;

        if (sap_reply.status != H5FP_STATUS_LOCK_RELEASED)
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTUNLOCK, FAIL,
                        "can't unlock object on server");
    }

done:
    *req_id = req.req_id;
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_request_read_metadata
 * Purpose:     Read a piece of metadata from the SAP. That is, if the
 *              SAP has access to that metadata. If not, then we'll need
 *              to read it from disk.
 *
 *              This function has the potential of causing the process to
 *              act as a dumper for the SAP's metadata. Places which call
 *              this function and check the STATUS variable should take
 *              this into account.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 02. August, 2002
 * Modifications:
 */
herr_t
H5FP_request_read_metadata(H5FD_t *file, unsigned file_id, hid_t dxpl_id,
                           H5FD_mem_t UNUSED mem_type, MPI_Offset addr,
                           size_t size, uint8_t **buf, int *bytes_read,
                           unsigned *req_id, H5FP_status_t *status)
{
    H5FP_request req;
    H5FP_read sap_read;     /* metadata info read from the SAP's cache */
    MPI_Status mpi_status;
    int mrc, my_rank;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5FP_request_read_metadata, FAIL);

    /* check args */
    assert(file);
    assert(buf);
    assert(bytes_read);
    assert(req_id);
    assert(status);

    HDmemset(&req, 0, sizeof(req));

    if ((mrc = MPI_Comm_rank(H5FP_SAP_COMM, &my_rank)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Comm_rank failed", mrc);

    *status = H5FP_STATUS_OK;
    req.req_type = H5FP_REQ_READ;
    req.req_id = H5FP_gen_request_id();
    req.file_id = file_id;
    req.proc_rank = my_rank;
    req.addr = addr;

    if ((mrc = MPI_Send(&req, 1, H5FP_request_t, (int)H5FP_sap_rank,
                        H5FP_TAG_REQUEST, H5FP_SAP_COMM)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);

    HDmemset(&mpi_status, 0, sizeof(mpi_status));

    if ((mrc = MPI_Recv(&sap_read, 1, H5FP_read_t, (int)H5FP_sap_rank, H5FP_TAG_READ,
                        H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);

    HDmemset(*buf, '\0', size);

    switch (sap_read.status) {
    case H5FP_STATUS_OK:
        /* use the info in the H5FP_read_t structure to update the metadata */
        *status = H5FP_STATUS_OK;
        HDmemset(&mpi_status, 0, sizeof(mpi_status));

        if ((mrc = MPI_Recv(*buf, (int)size, MPI_BYTE, (int)H5FP_sap_rank,
                            H5FP_TAG_METADATA, H5FP_SAP_COMM,
                            &mpi_status)) != MPI_SUCCESS)
            HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);

        *bytes_read = size;
        break;
    case H5FP_STATUS_DUMPING:
        /*
         * Collect the metadata updates from the SAP and write them to
         * the file. We fall through because at this point the metadata
         * won't be cached on the server anymore.
         */
        if (H5FP_dump_to_file(file, dxpl_id, &sap_read) == FAIL)
            HGOTO_ERROR(H5E_FPHDF5, H5E_WRITEERROR, FAIL,
                        "can't write metadata update to file");
        /* FALLTHROUGH */
    case H5FP_STATUS_MDATA_NOT_CACHED:
        /*
         * The metadata wasn't in the SAP's cache. Should read from disk
         * now.
         */
        *status = H5FP_STATUS_MDATA_NOT_CACHED;
        break;
    default:
        *status = sap_read.status;
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTCHANGE, FAIL, "can't write metadata to server");
    }

done:
    *req_id = req.req_id;
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_request_write_metadata
 * Purpose:     Tell the SAP that we want to change a piece of metadata
 *              associated with the file. The request ID is returned in a
 *              pointer supplied by the user.
 *
 *              This function has the potential of causing the process to
 *              act as a dumper for the SAP's metadata. Places which call
 *              this function and check the STATUS variable should take
 *              this into account.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 02. August, 2002
 * Modifications:
 */
herr_t
H5FP_request_write_metadata(H5FD_t *file, unsigned file_id, hid_t dxpl_id,
                            H5FD_mem_t mem_type, MPI_Offset addr,
                            int mdata_size, const char *mdata,
                            unsigned *req_id, H5FP_status_t *status)
{
    H5FP_reply sap_reply;
    H5FP_read sap_read;     /* metadata info read from the SAP's cache */
    MPI_Status mpi_status;
    H5FP_request req;
    int mrc, my_rank;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5FP_request_change, FAIL);

    /* check args */
    assert(file);
    assert(mdata);
    assert(req_id);
    assert(status);

    HDmemset(&req, 0, sizeof(req));

    if ((mrc = MPI_Comm_rank(H5FP_SAP_COMM, &my_rank)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Comm_rank failed", mrc);

    req.req_type = H5FP_REQ_WRITE;
    req.req_id = H5FP_gen_request_id();
    req.proc_rank = my_rank;
    req.file_id = file_id;
    req.mem_type = mem_type;
    req.addr = addr;
    req.md_size = mdata_size;

    if ((mrc = MPI_Send(&req, 1, H5FP_request_t, (int)H5FP_sap_rank,
                        H5FP_TAG_REQUEST, H5FP_SAP_COMM)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);

    /* The first MPI_Send will have been sent before this one will be read. */
    if (H5FP_send_metadata(mdata, mdata_size, (int)H5FP_sap_rank) != SUCCEED)
        HGOTO_ERROR(H5E_FPHDF5, H5E_CANTSENDMDATA, FAIL, "can't send metadata to server");

    HDmemset(&mpi_status, 0, sizeof(mpi_status));

    if ((mrc = MPI_Recv(&sap_reply, 1, H5FP_reply_t, (int)H5FP_sap_rank,
                        H5FP_TAG_REPLY, H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);

    switch (sap_reply.status) {
    case H5FP_STATUS_OK:
        /* Nothing to do... */
        break;
    case H5FP_STATUS_DUMPING:
        /*
         * Collect the metadata updates from the SAP and write them to
         * the file. The function which sends us the dumping data sends
         * it to us as an H5FP_read object instead of the H5FP_reply
         * object we got above. So we need this "extra" read.
         *
         * FIXME: This is probably too much of a hack and could be fixed
         * for read/write/closing instances...
         */
        if ((mrc = MPI_Recv(&sap_read, 1, H5FP_read_t, (int)H5FP_sap_rank,
                            H5FP_TAG_READ, H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS) {
            *status = H5FP_STATUS_DUMPING_FAILED;
            HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);
        }

        if (H5FP_dump_to_file(file, dxpl_id, &sap_read) == FAIL) {
            *status = H5FP_STATUS_DUMPING_FAILED;
            HGOTO_ERROR(H5E_FPHDF5, H5E_WRITEERROR, FAIL,
                        "can't write metadata update to file");
        }

        break;
    default:
        *status = sap_reply.status;
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTCHANGE, FAIL, "can't write metadata to server");
    }

    *status = H5FP_STATUS_OK;

done:
    *req_id = req.req_id;
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_request_flush_metadata
 * Purpose:     Tell the SAP that we want to change a piece of metadata
 *              associated with the file. The request ID is returned in a
 *              pointer supplied by the user.
 *
 *              This function has the potential of causing the process to
 *              act as a dumper for the SAP's metadata. Places which call
 *              this function and check the STATUS variable should take
 *              this into account.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 02. August, 2002
 * Modifications:
 */
herr_t
H5FP_request_flush_metadata(H5FD_t *file, unsigned file_id, hid_t dxpl_id,
                            unsigned *req_id, H5FP_status_t *status)
{
    H5FP_reply sap_reply;
    H5FP_read sap_read;     /* metadata info read from the SAP's cache */
    H5FP_request req;
    MPI_Status mpi_status;
    int mrc, my_rank;
    int ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5FP_request_flush_metadata, FAIL);

    /* check args */
    assert(file);
    assert(req_id);
    assert(status);

    HDmemset(&req, 0, sizeof(req));

    if ((mrc = MPI_Comm_rank(H5FP_SAP_COMM, &my_rank)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Comm_rank failed", mrc);

    req.req_type = H5FP_REQ_FLUSH;
    req.req_id = H5FP_gen_request_id();
    req.file_id = file_id;
    req.proc_rank = my_rank;

    if ((mrc = MPI_Send(&req, 1, H5FP_request_t, (int)H5FP_sap_rank,
                        H5FP_TAG_REQUEST, H5FP_SAP_COMM)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);

    HDmemset(&mpi_status, 0, sizeof(mpi_status));

    if ((mrc = MPI_Recv(&sap_reply, 1, H5FP_reply_t, (int)H5FP_sap_rank,
                        H5FP_TAG_REPLY, H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);

    switch (sap_reply.status) {
    case H5FP_STATUS_OK:
        /* Nothing to do... */
        break;
    case H5FP_STATUS_DUMPING:
        /*
         * Collect the metadata updates from the SAP and write them to
         * the file. The function which sends us the dumping data sends
         * it to us as an H5FP_read object instead of the H5FP_reply
         * object we got above. So we need this "extra" read.
         *
         * FIXME: This is probably too much of a hack and could be fixed
         * for read/write/closing instances...
         */
        if ((mrc = MPI_Recv(&sap_read, 1, H5FP_read_t, (int)H5FP_sap_rank,
                            H5FP_TAG_READ, H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS) {
            *status = H5FP_STATUS_DUMPING_FAILED;
            HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);
        }

        if (H5FP_dump_to_file(file, dxpl_id, &sap_read) == FAIL) {
            *status = H5FP_STATUS_DUMPING_FAILED;
            HGOTO_ERROR(H5E_FPHDF5, H5E_WRITEERROR, FAIL,
                        "can't write metadata update to file");
        }

        break;
    default:
        *status = sap_reply.status;
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTCHANGE, FAIL, "can't write metadata to server");
    }

    *status = H5FP_STATUS_OK;

done:
    *req_id = req.req_id;
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_request_close
 * Purpose:     Tell the SAP that we want all of the structural changes
 *              made on the file and then close the file. The request ID
 *              is returned in a pointer passed to the function by the
 *              user.
 *
 *              This function has the potential of causing the process to
 *              act as a dumper for the SAP's metadata. Places which call
 *              this function and check the STATUS variable should take
 *              this into account.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 02. August, 2002
 * Modifications:
 */
herr_t
H5FP_request_close(H5FD_t *file, unsigned file_id, unsigned *req_id,
                   H5FP_status_t *status)
{
    H5FP_reply sap_reply;
    H5FP_request req;
    MPI_Status mpi_status;
    int mrc, my_rank;
    int ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5FP_request_close, FAIL);

    /* check args */
    assert(file);
    assert(req_id);
    assert(status);

    HDmemset(&req, 0, sizeof(req));

    if ((mrc = MPI_Comm_rank(H5FP_SAP_COMM, &my_rank)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Comm_rank failed", mrc);

    req.req_type = H5FP_REQ_CLOSE;
    req.req_id = H5FP_gen_request_id();
    req.file_id = file_id;
    req.proc_rank = my_rank;

    if ((mrc = MPI_Send(&req, 1, H5FP_request_t, (int)H5FP_sap_rank,
                        H5FP_TAG_REQUEST, H5FP_SAP_COMM)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);

    HDmemset(&mpi_status, 0, sizeof(mpi_status));

    if ((mrc = MPI_Recv(&sap_reply, 1, H5FP_reply_t, (int)H5FP_sap_rank,
                        H5FP_TAG_REPLY, H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);

    if (sap_reply.status != H5FP_STATUS_OK)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTCHANGE, FAIL, "can't close file on server");

    *status = H5FP_STATUS_OK;

done:
    *req_id = req.req_id;
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 *===----------------------------------------------------------------------===
 *                    Functions Private to Client Module
 *===----------------------------------------------------------------------===
 */

/*
 * Function:    H5FP_gen_request_id
 * Purpose:     Generate a unique request ID to send along with a
 *              message.
 * Return:      Integer >= 0 - Doesn't fail.
 * Programmer:  Bill Wendling, 30. July, 2002
 * Modifications:
 */
static unsigned
H5FP_gen_request_id()
{
    static unsigned int i = 0;
    FUNC_ENTER_NOINIT(H5FP_gen_request_id);
    FUNC_LEAVE_NOAPI(i++);
}

/*
 * Function:    H5FP_dump_to_file
 * Purpose:     Dump the updated metadata to the file. 
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 03. February 2003
 * Modifications:
 */
static herr_t
H5FP_dump_to_file(H5FD_t *file, hid_t dxpl_id, H5FP_read *sap_read)
{
    hid_t new_dxpl_id = FAIL;
    H5P_genplist_t *plist = NULL, *old_plist;
    unsigned dumping = 1;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOINIT(H5FP_dump_to_file);

    /* check args */
    assert(file);
    assert(sap_read);

    if ((old_plist = H5I_object(dxpl_id)) == NULL)
        HGOTO_ERROR(H5E_PLIST, H5E_NOTFOUND, FAIL, "property object doesn't exist");

    /* Compare property lists */
    if ((new_dxpl_id = H5P_copy_plist(old_plist)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTCOPY, FAIL, "can't copy property list");

    if (new_dxpl_id == FAIL)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTCOPY, FAIL, "can't copy property list");

    if ((plist = H5P_object_verify(new_dxpl_id, H5P_DATASET_XFER)) == NULL)
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dataset transfer list");

    /* Set the fact that we're dumping metadata to the file */
    if (H5P_insert(plist, H5FD_FPHDF5_XFER_DUMPING_METADATA,
                   H5FD_FPHDF5_XFER_DUMPING_SIZE, &dumping,
                   NULL, NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't insert MPI-I/O property");

    /*
     * At this point, we've received a message saying that the SAP is
     * dumping info to us. There's a metadata read waiting for us right
     * now...
     */
    do {
        MPI_Status mpi_status;
        int mrc;
        char *mdata;

        if (H5FP_read_metadata(&mdata, (int)sap_read->md_size,
                               (int)H5FP_sap_rank) != FAIL) {
            /* FIXME: Write to the file with this metadata */
            HDfree(mdata);
        } else {
            /* FIXME: Error */
        }

        HDmemset(&mpi_status, 0, sizeof(mpi_status));

        if ((mrc = MPI_Recv(sap_read, 1, H5FP_read_t, (int)H5FP_sap_rank, H5FP_TAG_READ,
                            H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS)
            HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);
    } while (sap_read->status != H5FP_STATUS_DUMPING_FINISHED);

done:
    if (plist)
        /* FIXME: What can I do if this fail?? */
        H5P_close(plist);

    FUNC_LEAVE_NOAPI(ret_value);
}

#endif  /* H5_HAVE_FPHDF5 */
