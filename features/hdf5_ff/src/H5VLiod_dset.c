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

#define H5D_PACKAGE		/*suppress error about including H5Dpkg	  */
#include "H5Dpkg.h"		/* Datasets 				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Gprivate.h"		/* IDs			  		*/

#include "H5VLiod_server.h"

#ifdef H5_HAVE_EFF

/*
 * Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.gov>
 *              June, 2013
 *
 * Purpose:	The IOD plugin server side dataset routines.
 */

/* User data for VL traverssal */
typedef struct {
    iod_handle_t coh;
    iod_obj_id_t iod_id;
    iod_handles_t iod_oh;
    hid_t space_id;
    uint8_t *buf_ptr;
    size_t buf_size;
    size_t nelmts;
    size_t cur_seg;
    void **addrs;
    size_t *sizes;
    iod_trans_id_t wtid;
    iod_trans_id_t rtid;
} H5VL_iod_server_vl_write_t;

static herr_t 
H5VL__iod_server_vl_data_write(iod_handle_t coh, iod_obj_id_t iod_id, iod_handles_t iod_oh, 
                               hid_t space_id, hid_t mem_type_id, hid_t dset_type_id, 
                               H5VL_iod_type_info_t type_info, size_t nelmts,
                               size_t num_segments, void **addrs, size_t *sizes,
                               hid_t dxpl_id, iod_trans_id_t wtid, iod_trans_id_t rtid,
                               na_addr_t source, hg_bulk_t bulk_handle, uint32_t cs_scope,
                               na_bool_t is_coresident);

static herr_t 
H5VL__iod_server_vl_data_read(iod_handle_t coh, AXE_engine_t axe_engine, AXE_task_t axe_id, 
                              size_t nelmts, void *buf, 
                              hid_t dxpl_id, iod_trans_id_t rtid);

static herr_t 
H5VL__iod_server_vl_data_write_cb(void UNUSED *elem, hid_t type_id, unsigned ndims, 
                                  const hsize_t *point, void *_udata);

static iod_obj_id_t
H5VL__iod_get_vl_blob_oid(iod_obj_id_t dset_id, hid_t space_id, const hsize_t *point)
{
    hsize_t dims[H5S_MAX_RANK];
    uint64_t cur;
    int ndims, i;
    iod_obj_id_t blob_id = 0;

    ndims = H5Sget_simple_extent_dims(space_id, dims, NULL);

    /* set the BLOB ID algorithmically from the coordinate */
    cur = 1;
    for(i=0 ; i<ndims ; i++) {
        blob_id += cur*point[i];
        cur *= dims[i];
    }

    /* copy the last 20 bits of the dataset ID into bits 58->38 of the BLOB ID */
    for(i=0 ; i<20 ; i++) {
        (dset_id & (((uint64_t)0x1) << i)) ? 
            (blob_id |= (((uint64_t)0x1) << (38+i+1))) : 
            (blob_id &= ~(((uint64_t)0x1) << (38+i+1)));
    }

    /* set the BLOB for dset elements ID */
    blob_id |= (((uint64_t)0x1) << 59);

    /* Set IOD bit parameters for the BLOB ID */
    IOD_OBJID_SETTYPE(blob_id, IOD_OBJ_BLOB);
    IOD_OBJID_SETOWNER_APP(blob_id);

    return blob_id;
}


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_create_cb
 *
 * Purpose:	Creates a dset as a iod object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              February, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_dset_create_cb(AXE_engine_t UNUSED axe_engine, 
                               size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                               size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                               void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    dset_create_in_t *input = (dset_create_in_t *)op_data->input;
    dset_create_out_t output;
    iod_handle_t coh = input->coh; /* container handle */
    iod_handles_t loc_handle = input->loc_oh; /* location handle to start lookup */
    iod_obj_id_t loc_id = input->loc_id; /* The ID of the current location object */
    iod_obj_id_t dset_id = input->dset_id; /* The ID of the dataset that needs to be created */
    iod_obj_id_t mdkv_id = input->mdkv_id; /* The ID of the metadata KV to be created */
    iod_obj_id_t attrkv_id = input->attrkv_id; /* The ID of the attirbute KV to be created */
    iod_trans_id_t wtid = input->trans_num;
    iod_trans_id_t rtid = input->rcxt_num;
    uint32_t cs_scope = input->cs_scope;
    hid_t space_id = input->space_id;
    hid_t dcpl_id;
    iod_handles_t dset_oh, cur_oh;
    iod_handle_t mdkv_oh;
    iod_obj_id_t cur_id;
    const char *name = input->name; /* name of dset including path to create */
    char *last_comp = NULL; /* the name of the dataset obtained from the last component in the path */
    iod_array_struct_t array; /* IOD array struct describing the dataset's dimensions */
    scratch_pad sp;
    iod_ret_t ret = 0;
    int step = 0;
    hbool_t enable_checksum = FALSE;
    H5T_class_t dt_class;
    iod_hint_list_t *obj_create_hint = NULL, *md_obj_create_hint = NULL;
    iod_size_t array_dims[H5S_MAX_RANK], current_dims[H5S_MAX_RANK];
    herr_t ret_value = SUCCEED;

#if H5_EFF_DEBUG
    if(name)
        fprintf(stderr, "Start dataset create %s at %"PRIu64"\n", name, loc_handle.wr_oh.cookie);
    else
        fprintf(stderr, "Start anon dataset create at %"PRIu64"\n", loc_handle.wr_oh.cookie);
#endif

    if(H5P_DEFAULT == input->dcpl_id)
        input->dcpl_id = H5Pcopy(H5P_DATASET_CREATE_DEFAULT);
    dcpl_id = input->dcpl_id;

    /* get the scope for data integrity checks for raw data */
    if(H5Pget_ocpl_enable_checksum(dcpl_id, &enable_checksum) < 0)
        HGOTO_ERROR_FF(FAIL, "can't get scope for data integrity checks");

    if(name) {
        /* the traversal will retrieve the location where the dataset needs
           to be created. The traversal will fail if an intermediate group
           does not exist. */
        ret = H5VL_iod_server_traverse(coh, loc_id, loc_handle, name, wtid, rtid, FALSE, 
                                       cs_scope, &last_comp, &cur_id, &cur_oh);
        if(ret != SUCCEED)
            HGOTO_ERROR_FF(ret, "can't traverse path");
    }
    else {
        /* this is an anon dataset.. no link information */
        cur_oh.rd_oh.cookie = loc_handle.rd_oh.cookie;
        cur_oh.wr_oh.cookie = loc_handle.wr_oh.cookie;
        cur_id = loc_id;
    }

#if H5_EFF_DEBUG
    fprintf(stderr, "Creating Dataset ID %"PRIx64" (CV %"PRIu64", TR %"PRIu64") ", 
            dset_id, rtid, wtid);
    fprintf(stderr, "at (OH %"PRIu64" ID %"PRIx64") ", cur_oh.wr_oh.cookie, cur_id);
    if((cs_scope & H5_CHECKSUM_IOD) && enable_checksum)
        fprintf(stderr, "with Data integrity ENABLED\n");
    else
        fprintf(stderr, "with Data integrity DISABLED\n");
#endif

    if(enable_checksum) {
        obj_create_hint = (iod_hint_list_t *)malloc(sizeof(iod_hint_list_t) + sizeof(iod_hint_t));
        obj_create_hint->num_hint = 1;
        obj_create_hint->hint[0].key = "iod_hint_obj_enable_cksum";
    }

    if((cs_scope & H5_CHECKSUM_IOD)) {
        md_obj_create_hint = (iod_hint_list_t *)malloc(sizeof(iod_hint_list_t) + sizeof(iod_hint_t));
        md_obj_create_hint->num_hint = 1;
        md_obj_create_hint->hint[0].key = "iod_hint_obj_enable_cksum";
    }

    dt_class = H5Tget_class(input->type_id);
    /* Set the IOD array creation parameters */
    if(dt_class == H5T_VLEN || 
       (dt_class == H5T_STRING && H5Tis_variable_str(input->type_id)) )
        array.cell_size = sizeof(iod_obj_id_t) + sizeof(iod_size_t);
    else
        array.cell_size = (uint32_t)H5Tget_size(input->type_id);

    array.num_dims = (uint32_t)H5Sget_simple_extent_ndims(space_id);

    /* Handle Scalar Dataspaces (set rank and current dims size to 1) */
    if(0 == array.num_dims) {
        array.num_dims = 1;
        array.firstdim_max = 1;
        current_dims[0] = 1;
        array.current_dims = current_dims;
    }
    else {
        if(H5Sget_simple_extent_dims(space_id, current_dims, array_dims) < 0)
            HGOTO_ERROR_FF(FAIL, "can't get dimentions' sizes");

        if(H5S_UNLIMITED == array_dims[0]) {
            array_dims[0] = current_dims[0];
            array.firstdim_max = IOD_DIMLEN_UNLIMITED;
        }
        else {
            array.firstdim_max = array_dims[0];
        }

        array.current_dims = current_dims;
    }

    /* MSC - Add chunking support */
    array.chunk_dims = NULL;

#if H5_EFF_DEBUG 
    fprintf(stderr, "now creating the dataset with cellsize %d num dimensions %d\n",
            array.cell_size, array.num_dims);
#endif

    /* create the dataset */
    ret = iod_obj_create(coh, wtid, obj_create_hint, IOD_OBJ_ARRAY, NULL, 
                         &array, &dset_id, NULL);
    if(ret != 0)
        HGOTO_ERROR_FF(ret, "can't create Array object");

    ret = iod_obj_open_write(coh, dset_id, wtid, NULL, &dset_oh.wr_oh, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't open Dataset for Write");

    ret = iod_obj_open_read(coh, dset_id, wtid, NULL, &dset_oh.rd_oh, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't open Dataset for Read");

    step ++;

    /* create the attribute KV object for the dataset */
    ret = iod_obj_create(coh, wtid, md_obj_create_hint, IOD_OBJ_KV, NULL, NULL, &attrkv_id, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't create attribute KV object");

    /* create the metadata KV object for the dataset */
    ret = iod_obj_create(coh, wtid, md_obj_create_hint, IOD_OBJ_KV, NULL, NULL, &mdkv_id, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't create metadata KV object");

    /* set values for the scratch pad object */
    sp[0] = mdkv_id;
    sp[1] = attrkv_id;
    sp[2] = IOD_OBJ_INVALID;
    sp[3] = IOD_OBJ_INVALID;

    /* set scratch pad in dataset */
    if(cs_scope & H5_CHECKSUM_IOD) {
        iod_checksum_t sp_cs = 0;

        sp_cs = H5_checksum_crc64(&sp, sizeof(sp));

        ret = iod_obj_set_scratch(dset_oh.wr_oh, wtid, &sp, &sp_cs, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't set scratch pad");
    }
    else {
        ret = iod_obj_set_scratch(dset_oh.wr_oh, wtid, &sp, NULL, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't set scratch pad");
    }

    /* Open Metadata KV object for write */
    ret = iod_obj_open_write(coh, mdkv_id, wtid, NULL, &mdkv_oh, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't create scratch pad");

    step ++;

    /* insert plist metadata */
    ret = H5VL_iod_insert_plist(mdkv_oh, wtid, dcpl_id, cs_scope, NULL, NULL);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "can't insert KV value");

    /* insert link count metadata */
    ret = H5VL_iod_insert_link_count(mdkv_oh, wtid, (uint64_t)1, cs_scope, NULL, NULL);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "can't insert KV value");

    /* insert object type metadata */
    ret = H5VL_iod_insert_object_type(mdkv_oh, wtid, H5I_DATASET, cs_scope, NULL, NULL);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "can't insert KV value");

    /* MSC - need to check size of datatype if it fits in
       entry otherwise create a BLOB*/

    /* insert datatype metadata */
    ret = H5VL_iod_insert_datatype(mdkv_oh, wtid, input->type_id, cs_scope, NULL, NULL);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "can't insert KV value");

    /* insert dataspace metadata */
    ret = H5VL_iod_insert_dataspace(mdkv_oh, wtid, space_id, cs_scope, NULL, NULL);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "can't insert KV value");

    /* close the Metadata KV object */
    ret = iod_obj_close(mdkv_oh, NULL, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't close object");

    step --;

    /* If dataset is not anonymous, add link in parent group to current object */
    if(name) {
        ret = H5VL_iod_insert_new_link(cur_oh.wr_oh, wtid, last_comp, 
                                       H5L_TYPE_HARD, &dset_id, cs_scope, NULL, NULL);
        if(ret != SUCCEED)
            HGOTO_ERROR_FF(ret, "can't insert KV value");
    }

    output.iod_oh.rd_oh.cookie = dset_oh.rd_oh.cookie;
    output.iod_oh.wr_oh.cookie = dset_oh.wr_oh.cookie;

#if H5_EFF_DEBUG 
    fprintf(stderr, "Done with dset create, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &output);

done:
    /* close parent group if it is not the location we started the
       traversal into */
    if(loc_handle.rd_oh.cookie != cur_oh.rd_oh.cookie) {
        iod_obj_close(cur_oh.rd_oh, NULL, NULL);
    }
    if(loc_handle.wr_oh.cookie != cur_oh.wr_oh.cookie) {
        iod_obj_close(cur_oh.wr_oh, NULL, NULL);
    }

    /* return an UNDEFINED oh to the client if the operation failed */
    if(ret_value < 0) {
        fprintf(stderr, "failed to create Dataset\n");

        if(step == 2) {
            iod_obj_close(mdkv_oh, NULL, NULL);
            step --;
        }
        if(step == 1) {
            iod_obj_close(dset_oh.rd_oh, NULL, NULL);
            iod_obj_close(dset_oh.wr_oh, NULL, NULL);
        }

        output.iod_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
        output.iod_oh.wr_oh.cookie = IOD_OH_UNDEFINED;
        HG_Handler_start_output(op_data->hg_handle, &output);
    }

    if(obj_create_hint) {
        free(obj_create_hint);
        obj_create_hint = NULL;
    }

    if(md_obj_create_hint) {
        free(md_obj_create_hint);
        md_obj_create_hint = NULL;
    }

    last_comp = (char *)H5MM_xfree(last_comp);

    HG_Handler_free_input(op_data->hg_handle, input);
    HG_Handler_free(op_data->hg_handle);
    input = (dset_create_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

} /* end H5VL_iod_server_dset_create_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_open_cb
 *
 * Purpose:	Opens a dataset as a iod object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              February, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_dset_open_cb(AXE_engine_t UNUSED axe_engine, 
                             size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                             size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                             void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    dset_open_in_t *input = (dset_open_in_t *)op_data->input;
    dset_open_out_t output;
    iod_handle_t coh = input->coh; /* container handle */
    iod_handles_t loc_handle = input->loc_oh; /* location handle to start lookup */
    iod_obj_id_t loc_id = input->loc_id; /* The ID of the current location object */
    iod_trans_id_t rtid = input->rcxt_num;
    uint32_t cs_scope = input->cs_scope;
    const char *name = input->name; /* name of dset including path to open */
    iod_obj_id_t dset_id; /* ID of the dataset to open */
    iod_handles_t dset_oh;
    iod_handle_t mdkv_oh;
    scratch_pad sp;
    iod_checksum_t sp_cs = 0;
    int step = 0;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

    output.space_id = FAIL;
    output.type_id = FAIL;
    output.dcpl_id = FAIL;

#if H5_EFF_DEBUG
    fprintf(stderr, "Start dataset open %s at (OH %"PRIu64" ID %"PRIx64")\n", 
            name, loc_handle.rd_oh.cookie, loc_id);
#endif

    /* Traverse Path and open dset */
    ret = H5VL_iod_server_open_path(coh, loc_id, loc_handle, name, rtid, 
                                    cs_scope, &dset_id, &dset_oh);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "can't open object");

    /* open a write handle on the ID. */
    ret = iod_obj_open_write(coh, dset_id, rtid, NULL, &dset_oh.wr_oh, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't open current dset");
    step ++;

    /* get scratch pad of the dataset */
    ret = iod_obj_get_scratch(dset_oh.rd_oh, rtid, &sp, &sp_cs, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't get scratch pad for object");

    if(sp_cs && (cs_scope & H5_CHECKSUM_IOD)) {
        /* verify scratch pad integrity */
        if(H5VL_iod_verify_scratch_pad(&sp, sp_cs) < 0)
            HGOTO_ERROR_FF(FAIL, "Scratch Pad failed integrity check");
    }

    /* open the metadata KV */
    ret = iod_obj_open_read(coh, sp[0], rtid, NULL, &mdkv_oh, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't open metadata KV");
    step ++;

    ret = H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_PLIST, H5VL_IOD_KEY_OBJ_CPL,
                                cs_scope, NULL, &output.dcpl_id);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "failed to retrieve dcpl");

    ret = H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATATYPE, H5VL_IOD_KEY_OBJ_DATATYPE,
                                cs_scope, NULL, &output.type_id);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "failed to retrieve datatype");

    ret = H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATASPACE, H5VL_IOD_KEY_OBJ_DATASPACE,
                                cs_scope, NULL, &output.space_id);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "failed to retrieve dataspace");

    /* close the metadata KV */
    ret = iod_obj_close(mdkv_oh, NULL, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't close object");
    step --;

    output.iod_id = dset_id;
    output.mdkv_id = sp[0];
    output.attrkv_id = sp[1];
    output.iod_oh.rd_oh.cookie = dset_oh.rd_oh.cookie;
    output.iod_oh.wr_oh.cookie = dset_oh.wr_oh.cookie;

#if H5_EFF_DEBUG 
    fprintf(stderr, "Done with dset open, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &output);

done:

    if(FAIL != output.type_id)
        H5Tclose(output.type_id);
    if(FAIL != output.space_id)
        H5Sclose(output.space_id);
    if(FAIL != output.dcpl_id)
        H5Pclose(output.dcpl_id);

    if(ret_value < 0) {
        fprintf(stderr, "DSET open FAILED\n");
        output.iod_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
        output.iod_oh.wr_oh.cookie = IOD_OH_UNDEFINED;
        output.iod_id = IOD_OBJ_INVALID;
        output.space_id = FAIL;
        output.type_id = FAIL;
        output.dcpl_id = FAIL;

        if(step == 2) {
            iod_obj_close(mdkv_oh, NULL, NULL);
            step --;
        }
        if(step == 1) {
            iod_obj_close(dset_oh.rd_oh, NULL, NULL);
            iod_obj_close(dset_oh.wr_oh, NULL, NULL);
        }

        HG_Handler_start_output(op_data->hg_handle, &output);
    }

    HG_Handler_free_input(op_data->hg_handle, input);
    HG_Handler_free(op_data->hg_handle);
    input = (dset_open_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

} /* end H5VL_iod_server_dset_open_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_read_cb
 *
 * Purpose:	Reads from IOD into the function shipper BDS handle.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_dset_read_cb(AXE_engine_t axe_engine, 
                             size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                             size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                             void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    dset_io_in_t *input = (dset_io_in_t *)op_data->input;
    dset_read_out_t output;
    iod_handle_t coh = input->coh; /* container handle */
    iod_handles_t iod_oh = input->iod_oh; /* dset object handle */
    iod_obj_id_t iod_id = input->iod_id; /* dset ID */
    hg_bulk_t bulk_handle = input->bulk_handle; /* bulk handle for data */
    hid_t space_id = input->space_id; /* file space selection */
    hid_t dxpl_id;
    hid_t src_id = input->dset_type_id; /* the datatype of the dataset's element */
    hid_t dst_id = input->mem_type_id; /* the memory type of the elements */
    iod_trans_id_t rtid = input->rcxt_num;
    //uint32_t cs_scope = input->cs_scope;
    hg_bulk_t bulk_block_handle; /* HG block handle */
    hg_bulk_request_t bulk_request; /* HG request */
    size_t size, buf_size = 0;
    void *buf = NULL; /* buffer to hold outgoing data */
    iod_checksum_t cs = 0; /* checksum value */
    uint32_t raw_cs_scope;
    hbool_t is_vl_data;
    size_t nelmts; /* number of elements selected to read */
    na_addr_t dest = HG_Handler_get_addr(op_data->hg_handle); /* destination address to push data to */
    na_class_t *na_class = HG_Handler_get_na_class(op_data->hg_handle); /* NA transfer class */
    na_bool_t is_coresident = NA_Addr_is_self(na_class, dest);
    iod_ret_t ret;
    hbool_t opened_locally = FALSE; /* flag to indicate whether we opened the dset here or if it was already open */
    herr_t ret_value = SUCCEED;

    /* MSC - for now do memcpy if segment count > 1 */
    if(is_coresident && 1 != HG_Bulk_handle_get_segment_count(bulk_handle))
        is_coresident = NA_FALSE;

    /* open the dataset if we don't have the handle yet */
    if(iod_oh.rd_oh.cookie == IOD_OH_UNDEFINED) {
        ret = iod_obj_open_read(coh, iod_id, rtid, NULL, &iod_oh.rd_oh, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't open current group");
        opened_locally = TRUE;
    }

#if H5_EFF_DEBUG 
    fprintf(stderr, "Start dataset Read on OH %"PRIu64" OID %"PRIx64"\n", iod_oh.rd_oh.cookie, iod_id);
#endif

    if(H5P_DEFAULT == input->dxpl_id)
        input->dxpl_id = H5Pcopy(H5P_DATASET_XFER_DEFAULT);
    dxpl_id = input->dxpl_id;

    /* get the scope for data integrity checks for raw data */
    if(H5Pget_rawdata_integrity_scope(dxpl_id, &raw_cs_scope) < 0)
        HGOTO_ERROR_FF(FAIL, "can't get scope for data integrity checks");

    /* get the number of points selected */
    nelmts = (size_t)H5Sget_select_npoints(space_id);

    /* retrieve size of bulk data asked for to be read */
    size = HG_Bulk_handle_get_size(bulk_handle);

    /* check for vl datatypes */
    if(H5VL__iod_server_type_is_vl(src_id, &is_vl_data) < 0)
        HGOTO_ERROR_FF(FAIL, "failed to check dataype");

    if(is_coresident) {
        size_t bulk_size = 0;

        bulk_block_handle = bulk_handle;
        /* get  mercury buffer where data is */
        if(HG_SUCCESS != HG_Bulk_handle_access(bulk_block_handle, 0, size,
                                               HG_BULK_READWRITE, 1, &buf, &bulk_size, NULL))
            HGOTO_ERROR_FF(FAIL, "Could not access handle");

        assert(size == bulk_size);
    }
    else {
        /* allocate buffer to hold data */

        /* get a bigger buffer if datatype conversion is required */
        if(!is_vl_data) {
            buf_size = H5Tget_size(src_id) * nelmts;
            if(size > buf_size)
                buf_size = size;
        }
        else {
            buf_size = size;
        }

        if(NULL == (buf = malloc(buf_size)))
            HGOTO_ERROR_FF(FAIL, "can't allocate read buffer");

        /* Create bulk handle */
        if(HG_SUCCESS != HG_Bulk_handle_create(1, &buf, &size, 
                                               HG_BULK_READWRITE, &bulk_block_handle))
            HGOTO_ERROR_FF(FAIL, "can't create bulk handle");
    }
    
    /*
    if(H5VL__iod_server_adjust_buffer(dst_id, src_id, nelmts, dxpl_id, is_coresident,
                                      size, &buf, &is_vl_data, &buf_size) < 0)
        HGOTO_ERROR_FF(FAIL, "failed to setup read operation");
    */
    if(!is_vl_data) {
        size_t elmt_size;
        iod_trans_id_t read_tid;

        /* get replica ID from dxpl */
        if(H5Pget_read_replica(dxpl_id, &read_tid) < 0)
            HGOTO_ERROR_FF(FAIL, "can't get replica ID from dxpl");

        if(read_tid) {
            fprintf(stderr, "Reading from replica tag %"PRIx64"\n", read_tid);
        }
        else {
            read_tid = rtid;
        }

        /* If the data is not VL, we can read the data from the array the normal way */
        elmt_size = H5Tget_size(src_id);
        ret = H5VL__iod_server_final_io(iod_oh.rd_oh, space_id, elmt_size, FALSE, 
                                        buf, buf_size, (uint64_t)0, raw_cs_scope, read_tid);
        if(ret != SUCCEED)
            HGOTO_ERROR_FF(FAIL, "failed to read from array object");

        {
            hbool_t flag = FALSE;

            /* do data conversion */
            if(H5Tconvert(src_id, dst_id, nelmts, buf, NULL, dxpl_id) < 0)
                HGOTO_ERROR_FF(FAIL, "data type conversion failed");

            if(raw_cs_scope) {
                /* calculate a checksum for the data to be sent */
                cs = H5_checksum_crc64(buf, size);
            }
#if H5_EFF_DEBUG
            else {
                fprintf(stderr, "NO TRANSFER DATA INTEGRITY CHECKS ON RAW DATA\n");
            }
#endif
            /* MSC - check if client requested to corrupt data */
            if(H5Pget_dxpl_inject_corruption(dxpl_id, &flag) < 0)
                HGOTO_ERROR_FF(FAIL, "can't read property list");
            if(flag) {
                fprintf(stderr, "Injecting a bad data value to cause corruption \n");
                ((char *)buf)[0] = 54;
            }
        }
    }
    else {
        /* If the data is of variable length, special access is required */
        ret = H5VL__iod_server_vl_data_read(coh, axe_engine, input->axe_id, nelmts, 
                                            buf, dxpl_id, rtid);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't read from array object");
    }

    if(!is_coresident) {
        /* Push data to the client */
        if(HG_SUCCESS != HG_Bulk_transfer(HG_BULK_PUSH, dest, bulk_handle, 0, 
                                          bulk_block_handle, 0, size, &bulk_request))
            HGOTO_ERROR_FF(FAIL, "Transfer data failed");

        /* Wait for bulk data read to complete */
        if(HG_SUCCESS != HG_Bulk_wait(bulk_request, HG_MAX_IDLE_TIME, HG_STATUS_IGNORE))
            HGOTO_ERROR_FF(FAIL, "can't wait for bulk data operation");

        /* free block handle */
        if(HG_SUCCESS != HG_Bulk_handle_free(bulk_block_handle))
            HGOTO_ERROR_FF(FAIL, "can't free bds block handle");
    }

#if H5_EFF_DEBUG 
    fprintf(stderr, "Done with dset read, checksum %016lX, sending response to client\n", cs);
#endif
done:

    output.ret = ret_value;
    output.cs = cs;
    output.buf_size = buf_size;

    if(ret_value != SUCCEED)
        fprintf(stderr, "FAILED dset read, checksum %016lX \n", cs);

    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &output))
        HDONE_ERROR_FF(FAIL, "can't send result of write to client");

    HG_Handler_free_input(op_data->hg_handle, input);
    HG_Handler_free(op_data->hg_handle);
    input = (dset_io_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    if(!is_coresident && buf) {
        free(buf);
        buf = NULL;
    }

    /* close the dataset if we opened it in this routine */
    if(TRUE == opened_locally) {
        if(iod_obj_close(iod_oh.rd_oh, NULL, NULL) < 0)
            HDONE_ERROR_FF(FAIL, "can't close Array object");
    }
} /* end H5VL_iod_server_dset_read_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_get_vl_size_cb
 *
 * Purpose:	Retrieve the size required to store a selection of VL data.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              July, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_dset_get_vl_size_cb(AXE_engine_t UNUSED axe_engine, 
                                    size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                                    size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                                    void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    dset_io_in_t *input = (dset_io_in_t *)op_data->input;
    dset_read_out_t output;
    iod_handle_t coh = input->coh; /* container handle */
    iod_handles_t iod_oh = input->iod_oh; /* dset object handle */
    iod_obj_id_t iod_id = input->iod_id; /* dset ID */
    hid_t type_id = input->mem_type_id; /* the datatype of the dataset's element */
    hid_t space_id = input->space_id; /* file space selection */
    //hid_t dxpl_id = input->dxpl_id; /* transfer property list */
    iod_trans_id_t rtid = input->rcxt_num;
    uint32_t cs_scope = input->cs_scope;
    hg_bulk_t bulk_handle = input->bulk_handle; /* bulk handle for data */
    hg_bulk_t bulk_block_handle; /* HG block handle */
    hg_bulk_request_t bulk_request; /* HG request */
    size_t buf_size, elmt_size;
    void *buf = NULL; /* buffer to hold blob IDs and sizes */
    size_t nelmts; /* number of elements selected to read */
    uint8_t *buf_ptr = NULL;
    na_addr_t dest = HG_Handler_get_addr(op_data->hg_handle); /* destination address to push data to */
    na_class_t *na_class = HG_Handler_get_na_class(op_data->hg_handle); /* NA transfer class */
    na_bool_t is_coresident = NA_Addr_is_self(na_class, dest);
    hbool_t opened_locally = FALSE; /* flag to indicate whether we opened the dset here or if it was already open */
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

    /* open the dataset if we don't have the handle yet */
    if(iod_oh.rd_oh.cookie == IOD_OH_UNDEFINED) {
        ret = iod_obj_open_write(coh, iod_id, rtid, NULL, &iod_oh.rd_oh, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't open current group");
        opened_locally = TRUE;
    }

    /* get the number of points selected */
    nelmts = (size_t)H5Sget_select_npoints(space_id);
    elmt_size = sizeof(iod_obj_id_t) + sizeof(iod_size_t);

    /* allocate buffer to hold blob IDs */
    if(NULL == (buf = malloc(nelmts * elmt_size)))
        HGOTO_ERROR_FF(FAIL, "can't allocate read buffer");

    /* buffer always contains the length of each sequence, so
       initialize it to the size required to store those lengths */
    buf_size = nelmts * 8;//sizeof(size_t);

    /* read the array values containing the BLOB IDs and lengths */
    ret = H5VL__iod_server_final_io(iod_oh.rd_oh, space_id, elmt_size, FALSE, 
                                    buf, buf_size, (uint64_t)0, cs_scope, rtid);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "can't read from array object");

    /* MSC - use a temporrary buffer for now. */
    {
        size_t *temp_buf = NULL;
        uint8_t *temp_ptr;
        size_t temp_size;
        unsigned u;
        H5VL_iod_type_info_t type_info;

        if(is_coresident) {
            size_t bulk_size = 0;

            bulk_block_handle = bulk_handle;
            /* get  mercury buffer where data is */
            if(HG_SUCCESS != HG_Bulk_handle_access(bulk_block_handle, 0, buf_size,
                                                   HG_BULK_READWRITE, 1, &temp_buf, &bulk_size, NULL))
                HGOTO_ERROR_FF(FAIL, "Could not access handle");

            assert(buf_size == bulk_size);
        }
        else {
            /* allocate buffer to hold data */
            if(NULL == (temp_buf = malloc(buf_size)))
                HGOTO_ERROR_FF(FAIL, "can't allocate read buffer");

            /* Create bulk handle */
            if(HG_SUCCESS != HG_Bulk_handle_create(1, &temp_buf, &buf_size, 
                                                   HG_BULK_READWRITE, &bulk_block_handle))
                HGOTO_ERROR_FF(FAIL, "can't create bulk handle");
        }

        buf_ptr = (uint8_t *)buf;
        temp_ptr = (uint8_t *)temp_buf;

        /* Get type info */
        if(H5VL_iod_get_type_info(type_id, &type_info) < 0)
            HGOTO_ERROR_FF(FAIL, "failed to get datatype info");

        assert(1 == type_info.num_vls);

        /* copy just the size of each VL element into the temp buffer */
        for(u=0 ; u<nelmts ; u++) {
#if H5_EFF_DEBUG
            fprintf(stderr, "Element %u with BLOB ID %"PRIx64" size %zu\n", 
                    u, *((iod_obj_id_t *)buf_ptr),*((size_t *)(buf_ptr+sizeof(iod_obj_id_t))));
#endif

            temp_size = *((size_t *)(buf_ptr+sizeof(iod_obj_id_t)));

            if(type_info.vls[0].base_type) {
                /* Standard vlen */
                temp_size = temp_size / type_info.vls[0].base_type->size;
            }
            else {
                /* VL string; add space for NULL termination */
                temp_size ++;
            }

            UINT64ENCODE(temp_ptr, (uint64_t)temp_size);
            buf_ptr += elmt_size;
        }

        H5VL_iod_type_info_reset(&type_info);

        if(!is_coresident) {
            /* Push data to the client */
            if(HG_SUCCESS != HG_Bulk_transfer(HG_BULK_PUSH, dest, bulk_handle, 0, 
                                              bulk_block_handle, 0, buf_size, &bulk_request))
                HGOTO_ERROR_FF(FAIL, "Transfer data failed");

            /* Wait for bulk data read to complete */
            if(HG_SUCCESS != HG_Bulk_wait(bulk_request, HG_MAX_IDLE_TIME, HG_STATUS_IGNORE))
                HGOTO_ERROR_FF(FAIL, "can't wait for bulk data operation");

            /* free block handle */
            if(HG_SUCCESS != HG_Bulk_handle_free(bulk_block_handle))
                HGOTO_ERROR_FF(FAIL, "can't free bds block handle");

            if(temp_buf) {
                free(temp_buf);
                temp_buf = NULL;
            }
        }
    }

    op_data->output = buf;

done:

    output.ret = ret_value;
    output.cs = 0;
    output.buf_size = buf_size;

    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &output))
        HDONE_ERROR_FF(FAIL, "can't send result of write to client");

#if H5_EFF_DEBUG 
    fprintf(stderr, "Done with dset get vl size (%zu), sending response to client\n", buf_size);
#endif

    HG_Handler_free_input(op_data->hg_handle, input);
    HG_Handler_free(op_data->hg_handle);
    input = (dset_io_in_t *)H5MM_xfree(input);

    /* close the dataset if we opened it in this routine */
    if(TRUE == opened_locally) {
        if(iod_obj_close(iod_oh.rd_oh, NULL, NULL) < 0)
            HDONE_ERROR_FF(FAIL, "can't close Array object");
    }
} /* end H5VL_iod_server_dset_get_vl_size_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_write_cb
 *
 * Purpose:	Writes from IOD into the function shipper BDS handle.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_dset_write_cb(AXE_engine_t UNUSED axe_engine, 
                              size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                              size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                              void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    dset_io_in_t *input = (dset_io_in_t *)op_data->input;
    iod_handle_t coh = input->coh; /* container handle */
    iod_handles_t iod_oh = input->iod_oh; /* dset object handle */
    iod_obj_id_t iod_id = input->iod_id; /* dset ID */
    hg_bulk_t bulk_handle = input->bulk_handle; /* bulk handle for data */
    hg_bulk_t vl_len_bulk_handle = input->vl_len_bulk_handle; /* bulk handle for vlen length */
    hid_t space_id = input->space_id; /* file space selection */
    uint64_t cs = input->checksum; /* checksum recieved for data */
    hid_t src_id = input->mem_type_id; /* the memory type of the elements */
    hid_t dst_id = input->dset_type_id; /* the datatype of the dataset's element */
    iod_trans_id_t wtid = input->trans_num;
    iod_trans_id_t rtid = input->rcxt_num;
    //uint32_t cs_scope = input->cs_scope;
    hid_t dxpl_id;
    hg_bulk_t bulk_block_handle; /* HG block handle */
    hg_bulk_request_t bulk_request; /* HG request */
    size_t size, buf_size;
    hbool_t is_vl_data;
    iod_checksum_t data_cs = 0;
    uint32_t raw_cs_scope;
    H5VL_iod_type_info_t type_info;
    void *buf = NULL;
    size_t nelmts; /* number of elements selected to read */
    hbool_t flag = FALSE; /* temp flag to indicate whether corruption will be inserted */
    na_addr_t source = HG_Handler_get_addr(op_data->hg_handle); /* source address to pull data from */
    na_class_t *na_class = HG_Handler_get_na_class(op_data->hg_handle); /* NA transfer class */
    na_bool_t is_coresident = NA_Addr_is_self(na_class, source);
    hbool_t opened_locally = FALSE; /* flag to indicate whether we opened the dset here or if it was already open */
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

    /* MSC - for now do memcpy if segment count > 1 */
    if(is_coresident && 1 != HG_Bulk_handle_get_segment_count(bulk_handle))
        is_coresident = NA_FALSE;

    /* open the dataset if we don't have the handle yet */
    if(iod_oh.wr_oh.cookie == IOD_OH_UNDEFINED) {
        ret = iod_obj_open_write(coh, iod_id, wtid, NULL, &iod_oh.wr_oh, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't open dataset for write");
        opened_locally = TRUE;
    }

#if H5_EFF_DEBUG 
    fprintf(stderr, "Start dataset Write on OH %"PRIu64" OID %"PRIx64"\n", iod_oh.wr_oh.cookie, iod_id);
#endif

    if(H5P_DEFAULT == input->dxpl_id)
        input->dxpl_id = H5Pcopy(H5P_DATASET_XFER_DEFAULT);
    dxpl_id = input->dxpl_id;

    /* get the scope for data integrity checks for raw data */
    if(H5Pget_rawdata_integrity_scope(dxpl_id, &raw_cs_scope) < 0)
        HGOTO_ERROR_FF(FAIL, "can't get scope for data integrity checks");

    nelmts = (size_t)H5Sget_select_npoints(space_id);

    /* Get type info */
    if(H5VL_iod_get_type_info(src_id, &type_info) < 0)
        HGOTO_ERROR_FF(FAIL, "failed to get datatype info");

    if(type_info.vls) {
        void **addrs = NULL;
        size_t *sizes = NULL;
        size_t num_segments = 0;
        char *vl_lengths = NULL;
        size_t vl_lengths_size = 0;
        void **free_list = NULL;
        size_t free_list_len = 0;
        hg_bulk_t vl_len_handle = HG_BULK_NULL;

        /* Get size of vl_lengths array and allocate local buffer */
        vl_lengths_size = HG_Bulk_handle_get_size(vl_len_bulk_handle);
        if(vl_lengths_size == 0)
            HGOTO_ERROR_FF(FAIL, "no vlen lengths sent");

        if(is_coresident) {
            size_t bulk_size = 0;

            vl_len_handle = vl_len_bulk_handle;
            /* get  mercury buffer where data is */
            if(HG_SUCCESS != HG_Bulk_handle_access(vl_len_handle, 0, vl_lengths_size,
                                                   HG_BULK_READWRITE, 1, 
                                                   &vl_lengths, &bulk_size, NULL))
                HGOTO_ERROR_FF(FAIL, "Could not access handle");

            assert(vl_lengths_size == bulk_size);
        }
        else {
            if(NULL == (vl_lengths = (char *)malloc(vl_lengths_size)))
                HGOTO_ERROR_FF(FAIL, "can't allocate vlen lengths buffer");

            /* Create bulk handle */
            if(HG_SUCCESS != HG_Bulk_handle_create(1, &vl_lengths, &vl_lengths_size, 
                                                   HG_BULK_READWRITE, &vl_len_handle))
                HGOTO_ERROR_FF(FAIL, "create vlen bulk handle");

            /* Pull data from the client */
            if(HG_SUCCESS != HG_Bulk_transfer(HG_BULK_PULL, source, vl_len_bulk_handle, 0, 
                                              vl_len_handle, 0, vl_lengths_size, &bulk_request))
                HGOTO_ERROR_FF(FAIL, "Transfer data failed");

            /* Wait for bulk data read to complete */
            if(HG_SUCCESS != HG_Bulk_wait(bulk_request, HG_MAX_IDLE_TIME, HG_STATUS_IGNORE))
                HGOTO_ERROR_FF(FAIL, "can't wait for vlen lengths bulk data operation");
        }

        if(NULL == (buf = malloc(nelmts * type_info.size)))
            HGOTO_ERROR_FF(FAIL, "can't allocate data buffer");

        /* Create segments from vl lengths */
        if(H5VL_iod_create_segments_recv((char *)buf, &type_info, nelmts, &addrs, &sizes, &num_segments,
                                         vl_lengths, vl_lengths_size, &free_list, &free_list_len) < 0)
            HGOTO_ERROR_FF(FAIL, "can't create segments for bulk data transfer");
        assert(addrs);
        assert(sizes);

        free(buf);
        buf = NULL;

        ret = H5VL__iod_server_vl_data_write(coh, iod_id, iod_oh, space_id, src_id, dst_id, type_info, 
                                             nelmts, num_segments, addrs, sizes, dxpl_id, wtid, rtid,
                                             source, bulk_handle, raw_cs_scope, is_coresident);
        if(ret != SUCCEED)
            HGOTO_ERROR_FF(ret, "can't write VL data to array object");

        /* Free the bulk handle */
        if(!is_coresident) {
            if(HG_SUCCESS != HG_Bulk_handle_free(vl_len_handle))
                HGOTO_ERROR_FF(FAIL, "can't free vlen bulk handle");

            /* Free vl_lengths */
            if(vl_lengths) {
                free(vl_lengths);
                vl_lengths = NULL;
                vl_lengths_size = 0;
            }
        }

        /* Free segments */
        if(addrs) {
            free(addrs);
            addrs = NULL;
        } /* end if */
        if(sizes) {
            free(sizes);
            sizes = NULL;
        } /* end if */
        num_segments = 0;

        if(free_list) {
            H5VL_iod_free_list_free(free_list, free_list_len);
            free_list = NULL;
            free_list_len = 0;
        } /* end if */
    }
    else {
        size_t elmt_size;

        /* retrieve size of incoming bulk data */
        size = HG_Bulk_handle_get_size(bulk_handle);

        if(is_coresident) {
            size_t bulk_size = 0;

            bulk_block_handle = bulk_handle;
            /* get  mercury buffer where data is */
            if(HG_SUCCESS != HG_Bulk_handle_access(bulk_block_handle, 0, size,
                                                   HG_BULK_READWRITE, 1, &buf, &bulk_size, NULL))
                HGOTO_ERROR_FF(FAIL, "Could not access handle");

            assert(size == bulk_size);
        }
        else {
            /* allocate buffer to hold data */
            if(NULL == (buf = malloc(size)))
                HGOTO_ERROR_FF(FAIL, "can't allocate read buffer");

            /* Create bulk handle */
            if(HG_SUCCESS != HG_Bulk_handle_create(1, &buf, &size, 
                                                   HG_BULK_READWRITE, &bulk_block_handle))
                HGOTO_ERROR_FF(FAIL, "can't create bulk handle");

            /* Pull data from the client */
            if(HG_SUCCESS != HG_Bulk_transfer(HG_BULK_PULL, source, bulk_handle, 0, 
                                              bulk_block_handle, 0, size, &bulk_request))
                HGOTO_ERROR_FF(FAIL, "Transfer data failed");

            /* Wait for bulk data read to complete */
            if(HG_SUCCESS != HG_Bulk_wait(bulk_request, HG_MAX_IDLE_TIME, HG_STATUS_IGNORE))
                HGOTO_ERROR_FF(FAIL, "can't wait for bulk data operation");
        }

        /* MSC - check if client requested to corrupt data */
        if(H5Pget_dxpl_inject_corruption(dxpl_id, &flag) < 0)
            HGOTO_ERROR_FF(FAIL, "can't read property list");
        if(flag) {
            ((int *)buf)[0] = 10;
        }

        /* verify data if transfer flag is set */
        if(raw_cs_scope & H5_CHECKSUM_TRANSFER) {
            data_cs = H5_checksum_crc64(buf, size);
            if(cs != data_cs) {
                fprintf(stderr, 
                        "Errrr.. Network transfer Data corruption. expecting %"PRIu64", got %"PRIu64"\n",
                        cs, data_cs);
                ret_value = FAIL;
                goto done;
            }
        }
#if H5_EFF_DEBUG
        else {
            fprintf(stderr, "NO TRANSFER DATA INTEGRITY CHECKS ON RAW DATA\n");
        }
#endif

        buf_size = 0;

        /* Adjust buffer is type conversion is needed. If the data
           elements are of variable length, just return that they are in
           is_vl_data for special processing */
        if(H5VL__iod_server_adjust_buffer(src_id, dst_id, nelmts, dxpl_id, is_coresident,
                                          size, &buf, &is_vl_data, &buf_size) < 0)
            HGOTO_ERROR_FF(FAIL, "failed to setup write operation");

        /* convert data if needed */
        if(H5Tconvert(src_id, dst_id, nelmts, buf, NULL, dxpl_id) < 0)
            HGOTO_ERROR_FF(FAIL, "data type conversion failed")

        elmt_size = H5Tget_size(dst_id);
        ret = H5VL__iod_server_final_io(iod_oh.wr_oh, space_id, elmt_size, TRUE, 
                                        buf, buf_size, cs, raw_cs_scope, wtid);

        /* free the block handle */
        if(!is_coresident) {
            if(HG_SUCCESS != HG_Bulk_handle_free(bulk_block_handle))
                HGOTO_ERROR_FF(FAIL, "can't free bds block handle");

            if(buf) {
                free(buf);
                buf = NULL;
            }
        }
        if(ret != SUCCEED)
            HGOTO_ERROR_FF(ret, "can't write to array object");
    }

#if H5_EFF_DEBUG 
    fprintf(stderr, "Done with dset write, sending %d response to client \n", ret_value);
#endif

done:

#if H5_EFF_DEBUG 
    if(ret_value != SUCCEED)
        fprintf(stderr, "FAILED dset write, sending %d response to client \n", ret_value);
#endif

    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        HDONE_ERROR_FF(FAIL, "can't send result of write to client");

    HG_Handler_free_input(op_data->hg_handle, input);
    HG_Handler_free(op_data->hg_handle);
    input = (dset_io_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    H5VL_iod_type_info_reset(&type_info);

    /* close the dataset if we opened it in this routine */
    if(TRUE == opened_locally) {
        if(iod_obj_close(iod_oh.wr_oh, NULL, NULL) < 0)
            HDONE_ERROR_FF(FAIL, "can't close Array object");
    }
} /* end H5VL_iod_server_dset_write_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_set_extent_cb
 *
 * Purpose:	Set_Extents iod HDF5 dataset.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_dset_set_extent_cb(AXE_engine_t UNUSED axe_engine, 
                                   size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                                   size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                                   void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    dset_set_extent_in_t *input = (dset_set_extent_in_t *)op_data->input;
    iod_handle_t coh = input->coh;
    iod_handles_t iod_oh = input->iod_oh;
    iod_obj_id_t iod_id = input->iod_id; 
    iod_trans_id_t wtid = input->trans_num;
    iod_trans_id_t rtid = input->rcxt_num;
    uint32_t cs_scope = input->cs_scope;
    iod_obj_id_t mdkv_id = input->mdkv_id; /* The ID of the metadata KV object */
    /* int rank = input->dims.rank;  rank of dataset */
    iod_ret_t ret;
    hbool_t opened_locally = FALSE;
    herr_t ret_value = SUCCEED;

#if H5_EFF_DEBUG 
        fprintf(stderr, "Start dataset Set Extent first dim to %zu\n", 
                (iod_size_t)input->dims.size[0]);
#endif

    /* open the dataset if we don't have the handle yet */
    if(iod_oh.wr_oh.cookie == IOD_OH_UNDEFINED) {
        ret = iod_obj_open_write(coh, iod_id, wtid, NULL, &iod_oh.wr_oh, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't open current group");        
        opened_locally = TRUE;
    }

    /* extend along the first dimension only */
    ret = iod_array_extend(iod_oh.wr_oh, wtid, (iod_size_t)input->dims.size[0], NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't extend dataset");

    /* modify the dataspace of the dataset */
    {
        int rank;
        hid_t space_id;
        iod_handle_t mdkv_oh;
        iod_size_t array_dims[H5S_MAX_RANK], current_dims[H5S_MAX_RANK];

        /* open the metadata scratch pad */
        ret = iod_obj_open_write(coh, mdkv_id, wtid, NULL, &mdkv_oh, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't open scratch pad");

        /* get the stored dataset dataspace */
        ret = H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATASPACE, H5VL_IOD_KEY_OBJ_DATASPACE,
                                    cs_scope, NULL, &space_id);
        if(ret != SUCCEED)
            HGOTO_ERROR_FF(ret, "failed to retrieve dataspace");

        if((rank = H5Sget_simple_extent_dims(space_id, current_dims, array_dims)) < 0)
            HGOTO_ERROR_FF(FAIL, "can't get dimentions' sizes");

        /* Modify the size of the data space */
        if(H5Sset_extent_simple(space_id, rank, input->dims.size, array_dims) < 0)
            HGOTO_ERROR_FF(FAIL, "unable to modify size of data space");

        /* insert dataspace metadata */
        ret = H5VL_iod_insert_dataspace(mdkv_oh, wtid, space_id, 
                                        cs_scope, NULL, NULL);
        if(ret != SUCCEED)
            HGOTO_ERROR_FF(ret, "can't insert KV value");

        /* close the metadata scratch pad */
        ret = iod_obj_close(mdkv_oh, NULL, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't close object");
    }

done:
#if H5_EFF_DEBUG
    fprintf(stderr, "Done with dset set_extent, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &ret_value);

    HG_Handler_free_input(op_data->hg_handle, input);
    HG_Handler_free(op_data->hg_handle);
    input = (dset_set_extent_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    /* close the dataset if we opened it in this routine */
    if(opened_locally) {
        ret = iod_obj_close(iod_oh.wr_oh, NULL, NULL);
        if(ret < 0)
            HDONE_ERROR_FF(ret, "can't close IOD object");
    }
} /* end H5VL_iod_server_dset_set_extent_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_close_cb
 *
 * Purpose:	Closes iod HDF5 dataset.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_dset_close_cb(AXE_engine_t UNUSED axe_engine, 
                              size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                              size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                              void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    dset_close_in_t *input = (dset_close_in_t *)op_data->input;
    iod_handles_t iod_oh = input->iod_oh;
    //iod_obj_id_t iod_id = input->iod_id; 
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

#if H5_EFF_DEBUG
    fprintf(stderr, "Start dataset Close %"PRIu64" %"PRIu64"\n",
            iod_oh.rd_oh.cookie, iod_oh.wr_oh.cookie);
#endif

    ret = iod_obj_close(iod_oh.rd_oh, NULL, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't close Read Array object");
    ret = iod_obj_close(iod_oh.wr_oh, NULL, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't close Write Array object");

done:
#if H5_EFF_DEBUG
    fprintf(stderr, "Done with dset close, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &ret_value);

    HG_Handler_free_input(op_data->hg_handle, input);
    HG_Handler_free(op_data->hg_handle);
    input = (dset_close_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

} /* end H5VL_iod_server_dset_close_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__iod_server_final_io
 *
 * Read/Write to an IOD array object with an HDF5
 * selection. This is the normal way to access data given that the
 * datatype is not of variable length.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              August, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL__iod_server_final_io(iod_handle_t iod_oh, hid_t space_id, size_t elmt_size,
                          hbool_t write_op, void *buf, 
                          size_t UNUSED buf_size, iod_checksum_t UNUSED cs, 
                          uint32_t cs_scope, iod_trans_id_t tid)
{
    int ndims, i; /* dataset's rank/number of dimensions */
    hssize_t num_descriptors = 0, n; /* number of IOD file descriptors needed to describe filespace selection */
    iod_mem_desc_t *mem_desc; /* memory descriptor used for reading array */
    iod_array_iodesc_t *file_desc; /* file descriptor used to do IO */
    iod_hyperslab_t *hslabs = NULL; /* IOD hyperslab generated from HDF5 filespace */
    iod_checksum_t *cs_list = NULL;
    uint8_t *buf_ptr = NULL;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

    /* get the rank of the dataspace */
    if((ndims = H5Sget_simple_extent_ndims(space_id)) < 0)
        HGOTO_ERROR_FF(FAIL, "unable to get dataspace dimesnsion");

    /* handle scalar dataspace */
    if(0 == ndims) {
        ndims = 1;
        /* allocate the IOD hyperslab descriptors needed */
        if(NULL == (hslabs = (iod_hyperslab_t *)malloc(sizeof(iod_hyperslab_t))))
            HGOTO_ERROR_FF(FAIL, "can't allocate iod array descriptors");

        hslabs[0].start = (iod_size_t *)malloc(sizeof(iod_size_t));
        hslabs[0].stride = (iod_size_t *)malloc(sizeof(iod_size_t));
        hslabs[0].block = (iod_size_t *)malloc(sizeof(iod_size_t));
        hslabs[0].count = (iod_size_t *)malloc(sizeof(iod_size_t));

        num_descriptors = 1;
        hslabs[0].start[0] = 0;
        hslabs[0].count[0] = 1;
        hslabs[0].block[0] = 1;
        hslabs[0].stride[0] = 1;
    }
    else {
        /* get the number of decriptors required, i.e. the numbers of iod
           I/O operations needed */
        if(H5VL_iod_get_file_desc(space_id, &num_descriptors, NULL) < 0)
            HGOTO_ERROR_FF(FAIL, "unable to generate IOD file descriptor from dataspace selection");

        /* allocate the IOD hyperslab descriptors needed */
        if(NULL == (hslabs = (iod_hyperslab_t *)malloc
                    (sizeof(iod_hyperslab_t) * (size_t)num_descriptors)))
            HGOTO_ERROR_FF(FAIL, "can't allocate iod array descriptors");

        for(n=0 ; n<num_descriptors ; n++) {
            hslabs[n].start = (iod_size_t *)malloc(sizeof(iod_size_t) * (size_t)ndims);
            hslabs[n].stride = (iod_size_t *)malloc(sizeof(iod_size_t) * (size_t)ndims);
            hslabs[n].block = (iod_size_t *)malloc(sizeof(iod_size_t) * (size_t)ndims);
            hslabs[n].count = (iod_size_t *)malloc(sizeof(iod_size_t) * (size_t)ndims);
        }

        /* generate the descriptors after allocating the array */
        if(H5VL_iod_get_file_desc(space_id, &num_descriptors, hslabs) < 0)
            HGOTO_ERROR_FF(FAIL, "unable to generate IOD file descriptor from dataspace selection");
    }

    file_desc = (iod_array_iodesc_t *)hslabs;
    buf_ptr = (uint8_t *)buf;

    if(cs_scope & H5_CHECKSUM_IOD) {
        /* allocate cs array */
        if(NULL == (cs_list = (iod_checksum_t *)calloc
                    (sizeof(iod_checksum_t), (size_t)num_descriptors)))
            HGOTO_ERROR_FF(FAIL, "can't allocate checksum array");
    }
#if H5_EFF_DEBUG
    else {
        fprintf(stderr, "NO IOD DATA INTEGRITY CHECKS ON RAW DATA\n");
    }
#endif

    /* set the memory descriptor */
    mem_desc = (iod_mem_desc_t *)malloc(sizeof(iod_mem_desc_t) + 
                                        (size_t)num_descriptors * sizeof(iod_mem_frag_t));
    mem_desc->nfrag = (long unsigned)num_descriptors;
    for(n=0 ; n<num_descriptors ; n++) {
        hsize_t num_bytes = 0;
        hsize_t num_elems = 1;

        /* determine how many bytes the current descriptor holds */
        for(i=0 ; i<ndims ; i++) {
            num_elems *= (hslabs[n].count[i] * hslabs[n].block[i]);
        }
        num_bytes = num_elems * elmt_size;

        mem_desc->frag[n].addr = (void *)buf_ptr;
        mem_desc->frag[n].len = (iod_size_t)num_bytes;

        if(write_op && (cs_scope & H5_CHECKSUM_IOD))
            cs_list[n] = H5_checksum_crc64(buf_ptr, (size_t)num_bytes);

#if H5_EFF_DEBUG 
        for(i=0 ; i<ndims ; i++) {
            fprintf(stderr, "Dim %d:  start %zu   stride %zu   block %zu   count %zu\n", 
                    i, (size_t)file_desc->start[i], (size_t)file_desc->stride[i], 
                    (size_t)file_desc->block[i], (size_t)file_desc->count[i]);
        }
#endif
    }

    if(write_op) {
        /* write to array */
        ret = iod_array_write(iod_oh, tid, NULL, mem_desc, file_desc, cs_list, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't write to array object");
    }
    else {
        /* Read from array */
        ret = iod_array_read(iod_oh, tid, NULL, mem_desc, file_desc, cs_list, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't read from array object");
    }

    /* If this is a read operation, compute checksum for each IOD
       read, and compare it against checksum returned from IOD */
    if(!write_op && (cs_scope & H5_CHECKSUM_IOD)) {
        hsize_t num_bytes = 0;
        hsize_t num_elems = 1;
        iod_checksum_t checksum;

        buf_ptr = (uint8_t *)buf;

        for(n=0 ; n<num_descriptors ; n++) {
            /* determine how many bytes the current descriptor holds */
            for(i=0 ; i<ndims ; i++)
                num_elems *= (hslabs[n].count[i] * hslabs[n].block[i]);
            num_bytes = num_elems * elmt_size;

            checksum = H5_checksum_crc64(buf_ptr, (size_t)num_bytes);
#if H5_EFF_DEBUG 
            fprintf(stderr, "IOD checksum  = %016lX  Checksum Computed = %016lX\n",
                    cs_list[n], checksum);
#endif
            if(checksum != cs_list[n]) {
                fprintf(stderr, "Data Corruption detected when reading\n");
                ret_value = FAIL;
                goto done;
            }
            buf_ptr += num_bytes;
        }
    }

done:

    /* free allocated descriptors */
    for(n=0 ; n<num_descriptors ; n++) {
        free(hslabs[n].start);
        hslabs[n].start = NULL;
        free(hslabs[n].stride);
        hslabs[n].stride = NULL;
        free(hslabs[n].block);
        hslabs[n].block = NULL;
        free(hslabs[n].count);
        hslabs[n].count = NULL;
    }
    if(hslabs) {
        free(hslabs);
        hslabs = NULL;
    }
    if(cs_list) {
        free(cs_list);
        cs_list = NULL;
    }
    if(mem_desc) {
        free(mem_desc);
        mem_desc = NULL;
    }

    return ret_value;
} /* end H5VL_iod_server_final_io() */


/*-------------------------------------------------------------------------
 * Function:    H5VL__iod_server_vl_data_read
 *
 * Iterates over every (variable sized) element in the dataspace
 * selection and reads it from IOD.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              August, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5VL__iod_server_vl_data_read(iod_handle_t coh, AXE_engine_t axe_engine, AXE_task_t axe_id, 
                              size_t nelmts, void *buf, 
                              hid_t UNUSED dxpl_id, iod_trans_id_t rtid)
{
    void *vlen_buf = NULL;
    uint8_t *vlen_buf_ptr;
    uint8_t *buf_ptr = (uint8_t *)buf;
    void *get_size_op_data;
    op_data_t *op_data = NULL;
    iod_blob_io_t *io_blob = NULL; /* arary for list I/O */
    iod_checksum_t *cs_list = NULL;
    iod_ret_t *ret_list = NULL;
    iod_handle_t *blob_oh;
    size_t u, elmt_size;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

    /* retrieve the buffer that contains the blob IDs and their sizes
       that was created in the get_size operation */
    if(AXE_SUCCEED != AXEget_op_data(axe_engine, axe_id, &get_size_op_data))
        HGOTO_ERROR_FF(FAIL, "failed to get farm op_data");
    op_data = (op_data_t *)get_size_op_data;

    vlen_buf = op_data->output;
    vlen_buf_ptr = (uint8_t *)vlen_buf;

    /* allocate a blob list to read the data */
    if(NULL == (io_blob = (iod_blob_io_t *)malloc(sizeof(iod_blob_io_t) * nelmts)))
        HGOTO_ERROR_FF(FAIL, "can't allocate blob io array");

    /* allocate an array for the blob OHs */
    if(NULL == (blob_oh = (iod_handle_t *)malloc(sizeof(iod_handle_t) * nelmts)))
        HGOTO_ERROR_FF(FAIL, "can't allocate blob io array");

    /* allocate cs array */
    if(NULL == (cs_list = (iod_checksum_t *)calloc(sizeof(iod_checksum_t), nelmts)))
        HGOTO_ERROR_FF(FAIL, "can't allocate checksum array");

    /* allocate return array */
    if(NULL == (ret_list = (iod_ret_t *)calloc(sizeof(iod_ret_t), nelmts)))
        HGOTO_ERROR_FF(FAIL, "can't allocate return array");

    elmt_size = sizeof(iod_obj_id_t) + sizeof(iod_size_t);

    for(u=0 ; u<nelmts; u++) {
        size_t blob_size;
        iod_obj_id_t blob_id;
        iod_mem_desc_t *mem_desc;
        iod_blob_iodesc_t *blob_desc;

        blob_id = *((iod_obj_id_t *)vlen_buf_ptr);
        blob_size = *((size_t *)(vlen_buf_ptr+sizeof(iod_obj_id_t)));
        vlen_buf_ptr += elmt_size;

#if H5_EFF_DEBUG
        fprintf(stderr, "Element %zu with BLOB ID %"PRIx64" size %zu\n", 
                u, blob_id, blob_size);
#endif

        ret = iod_obj_open_read(coh, blob_id, rtid, NULL, &blob_oh[u], NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't open BLOB for Read");

        /* create memory descriptor for reading */
        mem_desc = (iod_mem_desc_t *)malloc(sizeof(iod_mem_desc_t) + sizeof(iod_mem_frag_t));
        mem_desc->nfrag = 1;
        mem_desc->frag[0].addr = (void *)buf_ptr;
        mem_desc->frag[0].len = blob_size;

        /* create file descriptor for writing */
        blob_desc = (iod_blob_iodesc_t *)malloc(sizeof(iod_blob_iodesc_t) + 
                                                sizeof(iod_blob_iofrag_t));
        blob_desc->nfrag = 1;
        blob_desc->frag[0].offset = 0;
        blob_desc->frag[0].len = blob_size;

        /* setup list I/O parameters */
        io_blob[u].oh = blob_oh[u];
        io_blob[u].hints = NULL;
        io_blob[u].mem_desc = mem_desc;
        io_blob[u].io_desc = blob_desc;
        io_blob[u].cs = NULL; //MSC - need IOD - &cs_list[u];
        io_blob[u].ret = &ret_list[u];

        buf_ptr += blob_size;
    }

    /* Read list IO */
    ret = iod_blob_read_list(coh, rtid, (int)nelmts, io_blob, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't read from blob objects");

    for(u=0 ; u<nelmts; u++) {
        if(ret_list[u] < 0)
            HGOTO_ERROR_FF(FAIL, "can't read from array object");

        free(io_blob[u].mem_desc);
        free(io_blob[u].io_desc);

        ret = iod_obj_close(blob_oh[u], NULL, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't close object");
    }

done:

    if(io_blob) {
        free(io_blob);
        io_blob = NULL;
    }
    if(blob_oh) {
        free(blob_oh);
        blob_oh = NULL;
    }
    if(cs_list) {
        free(cs_list);
        cs_list = NULL;
    }
    if(ret_list) {
        free(ret_list);
        ret_list = NULL;
    }

    vlen_buf = NULL;
    free(op_data->output);
    op_data->output = NULL;
    op_data = (op_data_t *)H5MM_xfree(op_data);

    return ret_value;
} /* H5VL__iod_server_vl_data_read */


/*-------------------------------------------------------------------------
 * Function:    H5VL__iod_server_vl_data_write
 *
 * Iterates over every (variable sized) element in the dataspace
 * selection and read/write it from IOD.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              August, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5VL__iod_server_vl_data_write(iod_handle_t coh, iod_obj_id_t iod_id, iod_handles_t iod_oh, 
                               hid_t space_id, hid_t mem_type_id, hid_t UNUSED dset_type_id, 
                               H5VL_iod_type_info_t type_info, size_t nelmts,
                               size_t num_segments, void **addrs, size_t *sizes,
                               hid_t UNUSED dxpl_id, iod_trans_id_t wtid, iod_trans_id_t rtid,
                               na_addr_t source, hg_bulk_t bulk_handle, uint32_t cs_scope,
                               na_bool_t is_coresident)
{
    char bogus;                 /* bogus value to pass to H5Diterate() */
    H5VL_iod_server_vl_write_t udata;
    hg_bulk_t vl_data_handle;
    hg_bulk_request_t bulk_request;
    size_t buf_size = 0, u;
    void *buf = NULL;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

    /* Print VL length DATA */
    for(u = 0; u < num_segments; u++) {
#if H5_EFF_DEBUG
        fprintf(stderr, "Element %zu  size %zu \n", u, sizes[u]);
#endif
        buf_size += sizes[u];
    } /* end for */

    if(is_coresident) {
        size_t bulk_size = 0;

        vl_data_handle = bulk_handle;

        /* get  mercury buffer where data is */
        if(HG_SUCCESS != HG_Bulk_handle_access(vl_data_handle, 0, buf_size,
                                               HG_BULK_READWRITE, 1, &buf, &bulk_size, NULL))
            HGOTO_ERROR_FF(FAIL, "Could not access handle");

        assert(buf_size == bulk_size);
    }
    else {
        if(NULL == (buf = malloc(buf_size)))
            HGOTO_ERROR_FF(FAIL, "can't allocate data buffer");

        /* Create bulk handle */
        if(HG_SUCCESS != HG_Bulk_handle_create(1, &buf, &buf_size, 
                                               HG_BULK_READWRITE, &vl_data_handle))
            HGOTO_ERROR_FF(FAIL, "can't create bulk handle");

        /* Pull data from the client */
        if(HG_SUCCESS != HG_Bulk_transfer(HG_BULK_PULL, source, bulk_handle, 0, 
                                          vl_data_handle, 0, buf_size, &bulk_request))
            HGOTO_ERROR_FF(FAIL, "Transfer data failed");

        /* Wait for bulk data read to complete */
        if(HG_SUCCESS != HG_Bulk_wait(bulk_request, HG_MAX_IDLE_TIME, HG_STATUS_IGNORE))
            HGOTO_ERROR_FF(FAIL, "can't wait for bulk data operation");
    }

    /* set other parameters needed to do IO */
    udata.coh = coh;
    udata.iod_oh = iod_oh;
    udata.nelmts = nelmts;
    udata.buf_ptr = (uint8_t *)buf;
    udata.buf_size = buf_size;
    udata.wtid = wtid;
    udata.rtid = rtid;
    udata.addrs = addrs;
    udata.sizes = sizes;
    udata.cur_seg = 0;
    udata.space_id = space_id;
    udata.iod_id = iod_id;

    /* iterate over every element and read/write it as a BLOB object */
    if(H5Diterate(&bogus, mem_type_id, space_id, H5VL__iod_server_vl_data_write_cb, &udata) < 0)
        HGOTO_ERROR_FF(FAIL, "failed to compute buffer size");

    /* Free the bulk handle */
    if(HG_SUCCESS != HG_Bulk_handle_free(vl_data_handle))
        HGOTO_ERROR_FF(FAIL, "can't free vlen bulk handle");
done:
    if(buf) {
        free(buf);
        buf = NULL;
    }
    return ret_value;
}/* end H5VL__iod_server_vl_data_write */


/*-------------------------------------------------------------------------
 * Function:    H5VL__iod_server_vl_data_write_cb
 *
 * The callback to the H5Diterate routine called in
 * H5VL__iod_server_vl_data_write. This will access every element in the
 * array object and resolves it to a BLOB object. Then the actual data
 * is read/written from/to the BLOB object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              August, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5VL__iod_server_vl_data_write_cb(void UNUSED *elem, hid_t type_id, unsigned ndims, 
                                  const hsize_t *point, void *_udata)
{
    H5VL_iod_server_vl_write_t *udata = (H5VL_iod_server_vl_write_t *)_udata;
    iod_handle_t coh = udata->coh; /* container handle */
    //size_t nelmts = udata->nelmts;
    iod_trans_id_t wtid = udata->wtid;
    //iod_trans_id_t rtid = udata->rtid;
    iod_handles_t iod_oh = udata->iod_oh;
    iod_obj_id_t iod_id = udata->iod_id;
    iod_obj_id_t blob_id = 0;
    iod_handle_t blob_oh;
    iod_hyperslab_t hslab;
    iod_mem_desc_t *mem_desc; /* memory descriptor used for reading array */
    iod_array_iodesc_t file_desc; /* file descriptor used to read array */
    iod_blob_iodesc_t *blob_desc; /* blob descriptor */
    size_t buf_size;
    unsigned u;
    hbool_t created = FALSE;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

    /* read in the point from the array object */
    hslab.start = (iod_size_t *)malloc(sizeof(iod_size_t) * ndims);
    hslab.stride = (iod_size_t *)malloc(sizeof(iod_size_t) * ndims);
    hslab.block = (iod_size_t *)malloc(sizeof(iod_size_t) * ndims);
    hslab.count = (iod_size_t *)malloc(sizeof(iod_size_t) * ndims);

#if H5_EFF_DEBUG
    fprintf(stderr, "Writing VL element # %zu\n", udata->cur_seg);
#endif

    memcpy(hslab.start, point, sizeof(size_t) * ndims);
    for(u=0 ; u<ndims ; u++) {
        hslab.stride[u] = 1;
        hslab.block[u] = 1;
        hslab.count[u] = 1;
    }
    file_desc = hslab;

    /* calculate the BLOB oid for the current coordinate */
    blob_id = H5VL__iod_get_vl_blob_oid(iod_id, udata->space_id, point);

    /* Attempt to Open the BLOB for write */
    ret = iod_obj_open_write(coh, blob_id, wtid, NULL, &blob_oh, NULL);
    /* if the open fails, try and create the BLOB */
    if(ret != 0) {
        ret = iod_obj_create(coh, wtid, NULL, IOD_OBJ_BLOB, NULL, NULL, &blob_id, NULL);
        /* if the BLOB exists now, try to open it again */
        if(0 == ret || -EEXIST == ret) {
            if(0 == ret) {
#if H5_EFF_DEBUG
                fprintf(stderr, "created BLOB with ID %"PRIx64"\n", blob_id);
#endif
                created = TRUE;
            }
            ret = iod_obj_open_write(coh, blob_id, wtid, NULL, &blob_oh, NULL);
            if(ret != 0)
                HGOTO_ERROR_FF(ret, "Failed to open BLOB object");
        }
        else 
            HGOTO_ERROR_FF(ret, "Failed to create BLOB object");
    }

    buf_size = udata->sizes[udata->cur_seg];

    /* MSC - type conversion ?? */

    /* create memory descriptor for writing */
    mem_desc = (iod_mem_desc_t *)malloc(sizeof(iod_mem_desc_t) + sizeof(iod_mem_frag_t));
    mem_desc->nfrag = 1;
    mem_desc->frag[0].addr = (void *)udata->buf_ptr;
    mem_desc->frag[0].len = (iod_size_t)buf_size;

    /* create file descriptor for writing */
    blob_desc = (iod_blob_iodesc_t *)malloc(sizeof(iod_blob_iodesc_t) + 
                                                sizeof(iod_blob_iofrag_t));
    blob_desc->nfrag = 1;
    blob_desc->frag[0].offset = 0;
    blob_desc->frag[0].len = (iod_size_t)buf_size;

    /* write the VL data to the blob */
    ret = iod_blob_write(blob_oh, wtid, NULL, mem_desc, blob_desc, NULL, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "unable to write BLOB object");

    free(mem_desc);
    mem_desc = NULL;
    free(blob_desc);
    blob_desc = NULL;

    /* close BLOB */
    ret = iod_obj_close(blob_oh, NULL, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't close object");

    if(created) {
        /* update the array element with the blob_id and sequence length */
        mem_desc = (iod_mem_desc_t *)malloc(sizeof(iod_mem_desc_t) + sizeof(iod_mem_frag_t) * 2);
        mem_desc->nfrag = 2;
        mem_desc->frag[0].addr = &blob_id;
        mem_desc->frag[0].len = sizeof(iod_obj_id_t);
        mem_desc->frag[1].addr = &buf_size;
        mem_desc->frag[1].len = sizeof(iod_size_t);

        /* MSC - no CS from IOD yet */
#if 0
        /* compute checksum of blob ID and sequence length */
        {
            void *buffers[2];
            size_t buf_sizes[2];

            buffers[0] = &blob_id;
            buf_sizes[0] = sizeof(iod_obj_id_t);
            buffers[1] = &seq_len;
            buf_sizes[1] = sizeof(iod_size_t);

            entry_cs = H5_checksum_crc64_fragments(buffers, buf_sizes, 2);
        }
#endif

        /* write the blob ID & size to the array element */
        ret = iod_array_write(iod_oh.wr_oh, wtid, NULL, 
                              mem_desc, &file_desc, NULL, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't read from array object");

        free(mem_desc);
        mem_desc = NULL;
    }

    /* advance buffer pointer */
    udata->buf_ptr += buf_size;
    udata->cur_seg ++;

done:

    free(hslab.start);
    hslab.start = NULL;
    free(hslab.stride);
    hslab.stride = NULL;
    free(hslab.block);
    hslab.block = NULL;
    free(hslab.count);
    hslab.count = NULL;

    return ret_value;
}/* end H5VL__iod_server_vl_data_write_cb */

#endif /* H5_HAVE_EFF */
