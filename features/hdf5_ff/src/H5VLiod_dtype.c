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

#include "H5VLiod_server.h"

#ifdef H5_HAVE_EFF

/*
 * Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.gov>
 *              June, 2013
 *
 * Purpose:	The IOD plugin server side datatype routines.
 */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dtype_commit_cb
 *
 * Purpose:	Commits a dtype as a iod object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_dtype_commit_cb(AXE_engine_t UNUSED axe_engine, 
                                size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                                size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                                void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    dtype_commit_in_t *input = (dtype_commit_in_t *)op_data->input;
    dtype_commit_out_t output;
    iod_handle_t coh = input->coh; /* container handle */
    iod_handles_t loc_handle = input->loc_oh; /* location handle to start lookup */
    iod_obj_id_t loc_id = input->loc_id; /* The ID of the current location object */
    iod_obj_id_t dtype_id = input->dtype_id; /* The ID of the datatype that needs to be created */
    iod_obj_id_t mdkv_id = input->mdkv_id; /* The ID of the metadata KV to be created */
    iod_obj_id_t attr_id = input->attrkv_id; /* The ID of the attirbute KV to be created */
    iod_trans_id_t wtid = input->trans_num;
    iod_trans_id_t rtid = input->rcxt_num;
    uint32_t cs_scope = input->cs_scope;
    iod_handles_t dtype_oh, cur_oh;
    iod_handle_t mdkv_oh;
    iod_obj_id_t cur_id;
    const char *name = input->name; /* name of dtype including path to commit */
    hid_t tcpl_id;
    char *last_comp; /* the name of the datatype obtained from the last component in the path */
    size_t buf_size; /* size of the serialized datatype */
    void *buf;
    iod_mem_desc_t *mem_desc = NULL; /* memory descriptor used for writing */
    iod_blob_iodesc_t *file_desc = NULL; /* file descriptor used to write */
    scratch_pad sp;
    int step = 0;
    iod_hint_list_t *obj_create_hint = NULL;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

#if H5_EFF_DEBUG
    fprintf(stderr, "Start datatype commit %s at %"PRIu64"\n", name, loc_handle.wr_oh.cookie);
#endif

    if(H5P_DEFAULT == input->tcpl_id)
        input->tcpl_id = H5Pcopy(H5P_DATATYPE_CREATE_DEFAULT);
    tcpl_id = input->tcpl_id;

   if(cs_scope & H5_CHECKSUM_IOD) {
        obj_create_hint = (iod_hint_list_t *)malloc(sizeof(iod_hint_list_t) + sizeof(iod_hint_t));
        obj_create_hint->num_hint = 1;
        obj_create_hint->hint[0].key = "iod_hint_obj_enable_cksum";
    }

    /* the traversal will retrieve the location where the datatype needs
       to be created. The traversal will fail if an intermediate group
       does not exist. */
   ret = H5VL_iod_server_traverse(coh, loc_id, loc_handle, name, wtid, rtid, FALSE, 
                                  cs_scope, &last_comp, &cur_id, &cur_oh);
   if(ret != SUCCEED)
       HGOTO_ERROR_FF(ret, "can't traverse path");

#if H5_EFF_DEBUG
    fprintf(stderr, "Creating Datatype ID %"PRIx64" (CV %"PRIu64", TR %"PRIu64") ", 
            dtype_id, rtid, wtid);
    fprintf(stderr, "at (OH %"PRIu64" ID %"PRIx64") ", cur_oh.wr_oh.cookie, cur_id);
    if(cs_scope & H5_CHECKSUM_IOD)
        fprintf(stderr, "with Data integrity ENABLED\n");
    else
        fprintf(stderr, "with Data integrity DISABLED\n");
#endif

    /* create the datatype */
    ret = iod_obj_create(coh, wtid, obj_create_hint, IOD_OBJ_BLOB, 
                         NULL, NULL, &dtype_id, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't create BLOB");

    ret = iod_obj_open_read(coh, dtype_id, wtid, NULL, &dtype_oh.rd_oh, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't open BLOB");
    ret = iod_obj_open_write(coh, dtype_id, wtid, NULL, &dtype_oh.wr_oh, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't open BLOB");

    step ++;

    /* create the metadata KV object for the datatype */
    ret = iod_obj_create(coh, wtid, obj_create_hint, IOD_OBJ_KV, 
                         NULL, NULL, &mdkv_id, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't create metadata KV object");

    /* create the attribute KV object for the datatype */
    ret = iod_obj_create(coh, wtid, obj_create_hint, IOD_OBJ_KV, 
                         NULL, NULL, &attr_id, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't create attribute KV object");

    /* set values for the scratch pad object */
    sp[0] = mdkv_id;
    sp[1] = attr_id;
    sp[2] = IOD_OBJ_INVALID;
    sp[3] = IOD_OBJ_INVALID;

    /* set scratch pad in datatype */
    if(cs_scope & H5_CHECKSUM_IOD) {
        iod_checksum_t sp_cs;

        sp_cs = H5_checksum_crc64(&sp, sizeof(sp));
        ret = iod_obj_set_scratch(dtype_oh.wr_oh, wtid, &sp, &sp_cs, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't set scratch pad");
    }
    else {
        ret = iod_obj_set_scratch(dtype_oh.wr_oh, wtid, &sp, NULL, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't set scratch pad");
    }

    /* Store Metadata in scratch pad */
    ret = iod_obj_open_write(coh, mdkv_id, wtid, NULL, &mdkv_oh, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't open metadata KV object");

    step ++;

    /* determine the buffer size needed to store the encoded type of the datatype */ 
    if(H5Tencode(input->type_id, NULL, &buf_size) < 0)
        HGOTO_ERROR_FF(FAIL, "failed to encode datatype type");
    if(NULL == (buf = malloc (buf_size)))
        HGOTO_ERROR_FF(FAIL, "can't allocate type buffer");
    /* encode datatype of the datatype */ 
    if(H5Tencode(input->type_id, buf, &buf_size) < 0)
        HGOTO_ERROR_FF(FAIL, "failed to encode datatype type");

    /* create memory descriptor for writing */
    mem_desc = (iod_mem_desc_t *)malloc(sizeof(iod_mem_desc_t) + sizeof(iod_mem_frag_t));
    mem_desc->nfrag = 1;
    mem_desc->frag[0].addr = buf;
    mem_desc->frag[0].len = (iod_size_t)buf_size;

    /* create file descriptor for writing */
    file_desc = (iod_blob_iodesc_t *)malloc(sizeof(iod_blob_iodesc_t) + 
                                            sizeof(iod_blob_iofrag_t));
    file_desc->nfrag = 1;
    file_desc->frag[0].offset = 0;
    file_desc->frag[0].len = (iod_size_t)buf_size;

    /* set scratch pad in datatype */
    if(cs_scope & H5_CHECKSUM_IOD) {
        iod_checksum_t dt_cs;

        /* calculate a checksum for the datatype */
        dt_cs = H5_checksum_crc64(buf, buf_size);

        /* write the serialized type value to the BLOB object */
        ret = iod_blob_write(dtype_oh.wr_oh, wtid, NULL, mem_desc, file_desc,
                             &dt_cs, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "unable to write BLOB object");
    }
    else {
        /* write the serialized type value to the BLOB object */
        ret = iod_blob_write(dtype_oh.wr_oh, wtid, NULL, mem_desc, file_desc, NULL, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "unable to write BLOB object");
    }

    free(mem_desc);
    free(file_desc);

    /* insert plist metadata */
    ret = H5VL_iod_insert_plist(mdkv_oh, wtid, tcpl_id, cs_scope, NULL, NULL);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "can't insert KV value");

    /* insert link count metadata */
    ret = H5VL_iod_insert_link_count(mdkv_oh, wtid, (uint64_t)1, cs_scope, NULL, NULL);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "can't insert KV value");

    /* insert object type metadata */
    ret = H5VL_iod_insert_object_type(mdkv_oh, wtid, H5I_DATATYPE, cs_scope, NULL, NULL);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "can't insert KV value");

    /* store the datatype size */
    {
        iod_kv_t kv;

        kv.key = (void *)H5VL_IOD_KEY_DTYPE_SIZE;
        kv.key_len = strlen(H5VL_IOD_KEY_DTYPE_SIZE) + 1;
        kv.value_len = sizeof(iod_size_t);
        kv.value = &buf_size;

        if(cs_scope & H5_CHECKSUM_IOD) {
            iod_checksum_t cs[2];

            cs[0] = H5_checksum_crc64(kv.key, kv.key_len);
            cs[1] = H5_checksum_crc64(kv.value, kv.value_len);
            ret = iod_kv_set(mdkv_oh, wtid, NULL, &kv, cs, NULL);
            if(ret < 0)
                HGOTO_ERROR_FF(ret, "can't set KV pair in parent");
        }
        else {
            ret = iod_kv_set(mdkv_oh, wtid, NULL, &kv, NULL, NULL);
            if(ret < 0)
                HGOTO_ERROR_FF(ret, "can't set KV pair in parent");
        }

    }

    /* close the Metadata KV object */
    ret = iod_obj_close(mdkv_oh, NULL, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't close object");

    step --;

    /* add link in parent group to current object */
    ret = H5VL_iod_insert_new_link(cur_oh.wr_oh, wtid, last_comp, 
                                   H5L_TYPE_HARD, &dtype_id, cs_scope, NULL, NULL);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "can't insert KV value");

    output.iod_oh.rd_oh.cookie = dtype_oh.rd_oh.cookie;
    output.iod_oh.wr_oh.cookie = dtype_oh.wr_oh.cookie;

#if H5_EFF_DEBUG
    fprintf(stderr, "Done with dtype commit, sending response to client\n");
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

    if(ret_value < 0) {
        if(step == 2) {
            iod_obj_close(mdkv_oh, NULL, NULL);
            step --;
        }
        if(step == 1) {
            iod_obj_close(dtype_oh.rd_oh, NULL, NULL);
            iod_obj_close(dtype_oh.wr_oh, NULL, NULL);
        }

        output.iod_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
        output.iod_oh.wr_oh.cookie = IOD_OH_UNDEFINED;
        HG_Handler_start_output(op_data->hg_handle, &output);
    }

    if(obj_create_hint) {
        free(obj_create_hint);
        obj_create_hint = NULL;
    }

    input = (dtype_commit_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);
    last_comp = (char *)H5MM_xfree(last_comp);
    free(buf);

} /* end H5VL_iod_server_dtype_commit_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dtype_open_cb
 *
 * Purpose:	Opens a datatype as a iod object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_dtype_open_cb(AXE_engine_t UNUSED axe_engine, 
                              size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                              size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                              void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    dtype_open_in_t *input = (dtype_open_in_t *)op_data->input;
    dtype_open_out_t output;
    iod_handle_t coh = input->coh; /* container handle */
    iod_handles_t loc_handle = input->loc_oh; /* location handle to start lookup */
    iod_obj_id_t loc_id = input->loc_id; /* The ID of the current location object */
    iod_obj_id_t dtype_id; /* ID of datatype to open */
    iod_handles_t dtype_oh;
    iod_handle_t mdkv_oh;
    const char *name = input->name; /* name of dtype including path to open */
    iod_trans_id_t rtid = input->rcxt_num;
    uint32_t cs_scope = input->cs_scope;
    size_t buf_size; /* size of serialized datatype */
    void *buf = NULL;
    iod_mem_desc_t *mem_desc = NULL; /* memory descriptor used for reading */
    iod_blob_iodesc_t *file_desc = NULL; /* file descriptor used to write */
    scratch_pad sp;
    iod_checksum_t sp_cs = 0;
    iod_checksum_t dt_cs = 0, blob_cs = 0;
    iod_size_t key_size=0, val_size=0;
    iod_checksum_t iod_cs[2];
    int step = 0;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

#if H5_EFF_DEBUG
    fprintf(stderr, "Start datatype open %s at (OH %"PRIu64" ID %"PRIx64")\n", 
            name, loc_handle.rd_oh.cookie, loc_id);
#endif

    /* Traverse Path and open dtype */
    ret = H5VL_iod_server_open_path(coh, loc_id, loc_handle, name, rtid, 
                                    cs_scope, &dtype_id, &dtype_oh);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "can't open object");

    /* open a write handle on the ID. */
    ret = iod_obj_open_write(coh, dtype_id, rtid, NULL, &dtype_oh.wr_oh, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't open current datatype");
    step ++;

    /* get scratch pad of the datatype */
    ret = iod_obj_get_scratch(dtype_oh.rd_oh, rtid, &sp, &sp_cs, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't get scratch pad for object");

    if(sp_cs && (cs_scope & H5_CHECKSUM_IOD)) {
        /* verify scratch pad integrity */
        if(H5VL_iod_verify_scratch_pad(&sp, sp_cs) < 0)
            HGOTO_ERROR_FF(FAIL, "Scratch Pad failed integrity check");
    }

    /* open the metadata scratch pad */
    ret = iod_obj_open_read(coh, sp[0], rtid, NULL, &mdkv_oh, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't open scratch pad");
    step ++;

    ret = H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_PLIST, H5VL_IOD_KEY_OBJ_CPL,
                                cs_scope, NULL, &output.tcpl_id);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "failed to retrieve tcpl");

    val_size = sizeof(iod_size_t);
    key_size = strlen(H5VL_IOD_KEY_DTYPE_SIZE) + 1;

    /* retrieve blob size metadata from scratch pad */
    ret = iod_kv_get_value(mdkv_oh, rtid, H5VL_IOD_KEY_DTYPE_SIZE, key_size,
                           &buf_size, &val_size, iod_cs, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "datatype size lookup failed");

    if(cs_scope & H5_CHECKSUM_IOD) {
        if(H5VL_iod_verify_kv_pair(H5VL_IOD_KEY_DTYPE_SIZE, key_size, 
                                   &buf_size, val_size, iod_cs) < 0)
            HGOTO_ERROR_FF(FAIL, "Corruption detected when reading metadata from IOD");
    }

    if(NULL == (buf = malloc(buf_size)))
        HGOTO_ERROR_FF(FAIL, "can't allocate BLOB read buffer");

    /* close the metadata scratch pad */
    ret = iod_obj_close(mdkv_oh, NULL, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't close object");
    step --;

    /* create memory descriptor for writing */
    mem_desc = (iod_mem_desc_t *)malloc(sizeof(iod_mem_desc_t) + sizeof(iod_mem_frag_t));
    mem_desc->nfrag = 1;
    mem_desc->frag[0].addr = buf;
    mem_desc->frag[0].len = (iod_size_t)buf_size;

    /* create file descriptor for writing */
    file_desc = (iod_blob_iodesc_t *)malloc(sizeof(iod_blob_iodesc_t) + 
                                            sizeof(iod_blob_iofrag_t));
    file_desc->nfrag = 1;
    file_desc->frag[0].offset = 0;
    file_desc->frag[0].len = (iod_size_t)buf_size;

    /* read the serialized type value from the BLOB object */
    ret = iod_blob_read(dtype_oh.rd_oh, rtid, NULL, mem_desc, file_desc, &blob_cs, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "unable to read from BLOB object");

    if(blob_cs && (cs_scope & H5_CHECKSUM_IOD)) {
        /* calculate a checksum for the datatype */
        dt_cs = H5_checksum_crc64(buf, buf_size);

#if H5_EFF_DEBUG 
        fprintf(stderr, "IOD BLOB checksum  = %016lX  Checksum Computed = %016lX\n",
                blob_cs, dt_cs);
#endif
        /* Verifty checksum against one given by IOD */
        if(blob_cs != dt_cs)
            HGOTO_ERROR_FF(FAIL, "Data Corruption detected when reading datatype");
    }

    /* decode the datatype */
    if((output.type_id = H5Tdecode(buf)) < 0)
        HGOTO_ERROR_FF(FAIL, "unable to decode datatype");

    free(mem_desc);
    free(file_desc);
    free(buf);

    output.iod_id = dtype_id;
    output.mdkv_id = sp[0];
    output.attrkv_id = sp[1];
    output.iod_oh.rd_oh.cookie = dtype_oh.rd_oh.cookie;
    output.iod_oh.wr_oh.cookie = dtype_oh.wr_oh.cookie;

#if H5_EFF_DEBUG
    fprintf(stderr, "Done with dtype open, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &output);

done:
    if(ret_value < 0) {
        output.iod_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
        output.iod_oh.wr_oh.cookie = IOD_OH_UNDEFINED;
        output.iod_id = IOD_OBJ_INVALID;
        output.type_id = FAIL;
        output.tcpl_id = FAIL;

        if(step == 2) {
            iod_obj_close(mdkv_oh, NULL, NULL);
            step --;
        }
        if(step == 1) {
            iod_obj_close(dtype_oh.rd_oh, NULL, NULL);
            iod_obj_close(dtype_oh.wr_oh, NULL, NULL);
        }

        HG_Handler_start_output(op_data->hg_handle, &output);
    }

    input = (dtype_open_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

} /* end H5VL_iod_server_dtype_open_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dtype_close_cb
 *
 * Purpose:	Closes iod HDF5 datatype.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_dtype_close_cb(AXE_engine_t UNUSED axe_engine, 
                               size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                               size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                               void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    dtype_close_in_t *input = (dtype_close_in_t *)op_data->input;
    iod_handles_t iod_oh = input->iod_oh;
    //iod_obj_id_t iod_id = input->iod_id; 
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

#if H5_EFF_DEBUG
    fprintf(stderr, "Start datatype Close\n");
#endif

    if(IOD_OH_UNDEFINED == iod_oh.wr_oh.cookie ||
       IOD_OH_UNDEFINED == iod_oh.rd_oh.cookie) {
        HGOTO_ERROR_FF(FAIL, "can't close object with invalid handle");
    }

    ret = iod_obj_close(iod_oh.rd_oh, NULL, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't close Read OH");
    ret = iod_obj_close(iod_oh.wr_oh, NULL, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't close Write OH");

#if H5_EFF_DEBUG
    fprintf(stderr, "Done with dtype close, sending response to client\n");
#endif
done:
    HG_Handler_start_output(op_data->hg_handle, &ret_value);

    input = (dtype_close_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

} /* end H5VL_iod_server_dtype_close_cb() */

#endif /* H5_HAVE_EFF */
