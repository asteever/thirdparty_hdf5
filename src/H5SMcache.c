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

/****************/
/* Module Setup */
/****************/

#define H5F_PACKAGE		/*suppress error about including H5Fpkg 	  */
#define H5SM_PACKAGE		/*suppress error about including H5SMpkg	  */


/***********/
/* Headers */
/***********/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"		/* File access                          */
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5SMpkg.h"            /* Shared object header messages        */
#include "H5WBprivate.h"        /* Wrapped Buffers                      */


/****************/
/* Local Macros */
/****************/

/* Size of stack buffer for serialized tables */
#define H5SM_TBL_BUF_SIZE       1024

/* Size of stack buffer for serialized list indices */
#define H5SM_LST_BUF_SIZE       1024


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Local Prototypes */
/********************/

/* Metadata cache (H5AC) callbacks */
static H5SM_master_table_t *H5SM_table_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *udata);
static herr_t H5SM_table_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5SM_master_table_t *table);
static herr_t H5SM_table_dest(H5F_t *f, H5SM_master_table_t* table);
static herr_t H5SM_table_clear(H5F_t *f, H5SM_master_table_t *table, hbool_t destroy);
static herr_t H5SM_table_size(const H5F_t *f, const H5SM_master_table_t *table, size_t *size_ptr);
static H5SM_list_t *H5SM_list_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *udata);
static herr_t H5SM_list_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5SM_list_t *list);
static herr_t H5SM_list_dest(H5F_t *f, H5SM_list_t* list);
static herr_t H5SM_list_clear(H5F_t *f, H5SM_list_t *list, hbool_t destroy);
static herr_t H5SM_list_size(const H5F_t *f, const H5SM_list_t UNUSED *list, size_t *size_ptr);


/*********************/
/* Package Variables */
/*********************/
/* H5SM inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_SOHM_TABLE[1] = {{
    H5AC_SOHM_TABLE_ID,
    (H5AC_load_func_t)H5SM_table_load,
    (H5AC_flush_func_t)H5SM_table_flush,
    (H5AC_dest_func_t)H5SM_table_dest,
    (H5AC_clear_func_t)H5SM_table_clear,
    (H5AC_size_func_t)H5SM_table_size,
}};

const H5AC_class_t H5AC_SOHM_LIST[1] = {{
    H5AC_SOHM_LIST_ID,
    (H5AC_load_func_t)H5SM_list_load,
    (H5AC_flush_func_t)H5SM_list_flush,
    (H5AC_dest_func_t)H5SM_list_dest,
    (H5AC_clear_func_t)H5SM_list_clear,
    (H5AC_size_func_t)H5SM_list_size,
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:	H5SM_table_load
 *
 * Purpose:	Loads the master table of Shared Object Header Message
 *              indexes.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *		November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static H5SM_master_table_t *
H5SM_table_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *udata)
{
    H5SM_master_table_t *table = NULL;
    size_t        size;                 /* Size of SOHM master table on disk */
    H5WB_t        *wb = NULL;           /* Wrapped buffer for table data */
    uint8_t       tbl_buf[H5SM_TBL_BUF_SIZE]; /* Buffer for table */
    uint8_t       *buf;                 /* Reading buffer */
    const uint8_t *p;                   /* Pointer into input buffer */
    uint32_t      stored_chksum;        /* Stored metadata checksum value */
    uint32_t      computed_chksum;      /* Computed metadata checksum value */
    size_t        x;                    /* Counter variable for index headers */
    H5SM_master_table_t *ret_value;

    FUNC_ENTER_NOAPI_NOINIT(H5SM_table_load)

    /* Verify that we're reading version 0 of the table; this is the only
     * version defined so far.
     */
    HDassert(f->shared->sohm_vers == HDF5_SHAREDHEADER_VERSION);

    /* Allocate space for the master table in memory */
    if(NULL == (table = H5FL_CALLOC(H5SM_master_table_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Read number of indexes and version from file superblock */
    table->num_indexes = f->shared->sohm_nindexes;

    HDassert(addr == f->shared->sohm_addr);
    HDassert(addr != HADDR_UNDEF);
    HDassert(table->num_indexes > 0);

    /* Wrap the local buffer for serialized table info */
    if(NULL == (wb = H5WB_wrap(tbl_buf, sizeof(tbl_buf))))
        HGOTO_ERROR(H5E_SOHM, H5E_CANTINIT, NULL, "can't wrap buffer")

    /* Compute the size of the SOHM table header on disk.  This is the "table" itself
     * plus each index within the table
     */
    size = H5SM_TABLE_SIZE(f) + (table->num_indexes * H5SM_INDEX_HEADER_SIZE(f));

    /* Get a pointer to a buffer that's large enough for serialized table */
    if(NULL == (buf = H5WB_actual(wb, size)))
        HGOTO_ERROR(H5E_SOHM, H5E_NOSPACE, NULL, "can't get actual buffer")

    /* Read header from disk */
    if(H5F_block_read(f, H5FD_MEM_SOHM_TABLE, addr, size, dxpl_id, buf) < 0)
	HGOTO_ERROR(H5E_SOHM, H5E_READERROR, NULL, "can't read SOHM table")

    /* Get temporary pointer to serialized table */
    p = buf;

    /* Check magic number */
    if(HDmemcmp(p, H5SM_TABLE_MAGIC, (size_t)H5SM_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_SOHM, H5E_CANTLOAD, NULL, "bad SOHM table signature")
    p += H5SM_SIZEOF_MAGIC;

    /* Don't count the checksum in the table size yet, since it comes after
     * all of the index headers
     */
    HDassert((size_t)(p - (const uint8_t *)buf) == H5SM_TABLE_SIZE(f) - H5SM_SIZEOF_CHECKSUM);

    /* Allocate space for the index headers in memory*/
    if(NULL == (table->indexes = (H5SM_index_header_t *)H5FL_ARR_MALLOC(H5SM_index_header_t, (size_t)table->num_indexes)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for SOHM indexes")

    /* Read in the index headers */
    for(x = 0; x < table->num_indexes; ++x) {
        /* Verify correct version of index list */
        if(H5SM_LIST_VERSION != *p++)
            HGOTO_ERROR(H5E_FILE, H5E_VERSION, NULL, "bad shared message list version number")

        /* Type of the index (list or B-tree) */
        table->indexes[x].index_type= *p++;

        /* Type of messages in the index */
        UINT16DECODE(p, table->indexes[x].mesg_types);

        /* Minimum size of message to share */
        UINT32DECODE(p, table->indexes[x].min_mesg_size);

        /* List cutoff; fewer than this number and index becomes a list */
        UINT16DECODE(p, table->indexes[x].list_max);

        /* B-tree cutoff; more than this number and index becomes a B-tree */
        UINT16DECODE(p, table->indexes[x].btree_min);

        /* Number of messages shared */
        UINT16DECODE(p, table->indexes[x].num_messages);

        /* Address of the actual index */
        H5F_addr_decode(f, &p, &(table->indexes[x].index_addr));

        /* Address of the index's heap */
        H5F_addr_decode(f, &p, &(table->indexes[x].heap_addr));
    } /* end for */

    /* Read in checksum */
    UINT32DECODE(p, stored_chksum);

    /* Sanity check */
    HDassert((size_t)(p - (const uint8_t *)buf) == size);

    /* Compute checksum on entire header */
    computed_chksum = H5_checksum_metadata(buf, (size - H5SM_SIZEOF_CHECKSUM), 0);

    /* Verify checksum */
    if(stored_chksum != computed_chksum)
        HGOTO_ERROR(H5E_SOHM, H5E_BADVALUE, NULL, "incorrect metadata checksum for shared message table")

    /* Set return value */
    ret_value = table;

done:
    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
        HDONE_ERROR(H5E_SOHM, H5E_CLOSEERROR, NULL, "can't close wrapped buffer")
    if(!ret_value && table)
        (void)H5SM_table_dest(f, table);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_table_load() */


/*-------------------------------------------------------------------------
 * Function:	H5SM_table_flush
 *
 * Purpose:	Flushes (and destroys) the table of Shared Object Header
 *              Message indexes.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *		November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_table_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5SM_master_table_t *table)
{
    H5WB_t *wb = NULL;                  /* Wrapped buffer for table data */
    uint8_t tbl_buf[H5SM_TBL_BUF_SIZE]; /* Buffer for table */
    herr_t ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5SM_table_flush)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(table);

    if(table->cache_info.is_dirty) {
        uint8_t  *buf;               /* Temporary buffer */
        uint8_t  *p;                 /* Pointer into raw data buffer */
        size_t	 size;               /* Header size on disk */
        uint32_t computed_chksum;    /* Computed metadata checksum value */
        size_t   x;                  /* Counter variable */

        /* Verify that we're writing version 0 of the table; this is the only
         * version defined so far.
         */
        HDassert(f->shared->sohm_vers == HDF5_SHAREDHEADER_VERSION);

        /* Wrap the local buffer for serialized header info */
        if(NULL == (wb = H5WB_wrap(tbl_buf, sizeof(tbl_buf))))
            HGOTO_ERROR(H5E_SOHM, H5E_CANTINIT, FAIL, "can't wrap buffer")

        /* Encode the master table and all of the index headers as one big blob */
        size = H5SM_TABLE_SIZE(f) + (H5SM_INDEX_HEADER_SIZE(f) * table->num_indexes);

        /* Get a pointer to a buffer that's large enough for serialized table */
        if(NULL == (buf = H5WB_actual(wb, size)))
            HGOTO_ERROR(H5E_SOHM, H5E_NOSPACE, FAIL, "can't get actual buffer")

        /* Get temporary pointer to buffer for serialized table */
        p = buf;

        /* Encode magic number */
        HDmemcpy(p, H5SM_TABLE_MAGIC, (size_t)H5SM_SIZEOF_MAGIC);
        p += H5SM_SIZEOF_MAGIC;

        /* Encode each index header */
        for(x = 0; x < table->num_indexes; ++x) {
            /* Version for this list. */
            *p++ = H5SM_LIST_VERSION;

            /* Is message index a list or a B-tree? */
            *p++ = table->indexes[x].index_type;

            /* Type of messages in the index */
            UINT16ENCODE(p, table->indexes[x].mesg_types);

            /* Minimum size of message to share */
            UINT32ENCODE(p, table->indexes[x].min_mesg_size);

            /* List cutoff; fewer than this number and index becomes a list */
            UINT16ENCODE(p, table->indexes[x].list_max);

            /* B-tree cutoff; more than this number and index becomes a B-tree */
            UINT16ENCODE(p, table->indexes[x].btree_min);

            /* Number of messages shared */
            UINT16ENCODE(p, table->indexes[x].num_messages);

            /* Address of the actual index */
            H5F_addr_encode(f, &p, table->indexes[x].index_addr);

            /* Address of the index's heap */
            H5F_addr_encode(f, &p, table->indexes[x].heap_addr);
        } /* end for */

        /* Compute checksum on buffer */
        computed_chksum = H5_checksum_metadata(buf, (size - H5SM_SIZEOF_CHECKSUM), 0);
        UINT32ENCODE(p, computed_chksum);

        /* Write the table to disk */
        HDassert((size_t)(p - buf) == size);
	if(H5F_block_write(f, H5FD_MEM_SOHM_TABLE, addr, size, dxpl_id, buf) < 0)
	    HGOTO_ERROR(H5E_SOHM, H5E_CANTFLUSH, FAIL, "unable to save sohm table to disk")

	table->cache_info.is_dirty = FALSE;
    } /* end if */

    if(destroy)
        if(H5SM_table_dest(f, table) < 0)
	    HGOTO_ERROR(H5E_SOHM, H5E_CANTFREE, FAIL, "unable to destroy sohm table")

done:
    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
        HDONE_ERROR(H5E_SOHM, H5E_CLOSEERROR, FAIL, "can't close wrapped buffer")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_table_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5SM_table_dest
 *
 * Purpose:	Frees memory used by the SOHM table.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *		November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_table_dest(H5F_t UNUSED *f, H5SM_master_table_t* table)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5SM_table_dest)

    /* Sanity check */
    HDassert(table);
    HDassert(table->indexes);

    /* Destroy Shared Object Header Message table */
    if(H5SM_table_free(table) < 0)
        HGOTO_ERROR(H5E_SOHM, H5E_CANTRELEASE, FAIL, "unable to free shared message table")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_table_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5SM_table_clear
 *
 * Purpose:	Mark this table as no longer being dirty.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *		November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_table_clear(H5F_t *f, H5SM_master_table_t *table, hbool_t destroy)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5SM_table_clear)

    /*
     * Check arguments.
     */
    HDassert(table);

    /* Reset the dirty flag.  */
    table->cache_info.is_dirty = FALSE;

    if(destroy)
        if(H5SM_table_dest(f, table) < 0)
	    HGOTO_ERROR(H5E_SOHM, H5E_CANTFREE, FAIL, "unable to delete SOHM master table")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_table_clear() */


/*-------------------------------------------------------------------------
 * Function:	H5SM_table_size
 *
 * Purpose:	Returns the size of the table encoded on disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *		November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_table_size(const H5F_t *f, const H5SM_master_table_t *table, size_t *size_ptr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_table_size)

    /* check arguments */
    HDassert(f);
    HDassert(table);
    HDassert(size_ptr);

    /* Set size value */
    *size_ptr = H5SM_TABLE_SIZE(f) + (table->num_indexes * H5SM_INDEX_HEADER_SIZE(f));

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_table_size() */


/*-------------------------------------------------------------------------
 * Function:	H5SM_list_load
 *
 * Purpose:	Loads a list of SOHM messages.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *		November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static H5SM_list_t *
H5SM_list_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *_udata)
{
    H5SM_list_t *list;          /* The SOHM list being read in */
    H5SM_list_cache_ud_t *udata = (H5SM_list_cache_ud_t *)_udata; /* User data for callback */
    size_t size;                /* Size of SOHM list on disk */
    H5WB_t *wb = NULL;          /* Wrapped buffer for list index data */
    uint8_t lst_buf[H5SM_LST_BUF_SIZE]; /* Buffer for list index */
    uint8_t *buf;               /* Reading buffer */
    uint8_t *p;                 /* Pointer into input buffer */
    uint32_t stored_chksum;     /* Stored metadata checksum value */
    uint32_t computed_chksum;   /* Computed metadata checksum value */
    size_t x;                   /* Counter variable for messages in list */
    H5SM_list_t *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5SM_list_load)

    /* Sanity check */
    HDassert(udata->header);

    /* Allocate space for the SOHM list data structure */
    if(NULL == (list = H5FL_MALLOC(H5SM_list_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&list->cache_info, 0, sizeof(H5AC_info_t));

    /* Allocate list in memory as an array*/
    if((list->messages = (H5SM_sohm_t *)H5FL_ARR_MALLOC(H5SM_sohm_t, udata->header->list_max)) == NULL)
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "file allocation failed for SOHM list")

    list->header = udata->header;

    /* Wrap the local buffer for serialized list index info */
    if(NULL == (wb = H5WB_wrap(lst_buf, sizeof(lst_buf))))
        HGOTO_ERROR(H5E_SOHM, H5E_CANTINIT, NULL, "can't wrap buffer")

    /* Compute the size of the SOHM list on disk */
    size = H5SM_LIST_SIZE(udata->f, udata->header->num_messages);

    /* Get a pointer to a buffer that's large enough for serialized list index */
    if(NULL == (buf = H5WB_actual(wb, size)))
        HGOTO_ERROR(H5E_SOHM, H5E_NOSPACE, NULL, "can't get actual buffer")

    /* Read list from disk */
    if(H5F_block_read(f, H5FD_MEM_SOHM_INDEX, addr, size, dxpl_id, buf) < 0)
	HGOTO_ERROR(H5E_SOHM, H5E_READERROR, NULL, "can't read SOHM list")

    /* Get temporary pointer to serialized list index */
    p = buf;

    /* Check magic number */
    if(HDmemcmp(p, H5SM_LIST_MAGIC, (size_t)H5SM_SIZEOF_MAGIC))
        HGOTO_ERROR(H5E_SOHM, H5E_CANTLOAD, NULL, "bad SOHM list signature")
    p += H5SM_SIZEOF_MAGIC;

    /* Read messages into the list array */
    for(x = 0; x < udata->header->num_messages; x++) {
        if(H5SM_message_decode(udata->f, p, &(list->messages[x])) < 0)
            HGOTO_ERROR(H5E_SOHM, H5E_CANTLOAD, NULL, "can't decode shared message")
        p += H5SM_SOHM_ENTRY_SIZE(udata->f);
    } /* end for */

    /* Read in checksum */
    UINT32DECODE(p, stored_chksum);

    /* Sanity check */
    HDassert((size_t)(p - buf) == size);

    /* Compute checksum on entire header */
    computed_chksum = H5_checksum_metadata(buf, (size - H5SM_SIZEOF_CHECKSUM), 0);

    /* Verify checksum */
    if(stored_chksum != computed_chksum)
        HGOTO_ERROR(H5E_SOHM, H5E_BADVALUE, NULL, "incorrect metadata checksum for shared message list")

    /* Initialize the rest of the array */
    for(x = udata->header->num_messages; x < udata->header->list_max; x++)
        list->messages[x].location = H5SM_NO_LOC;

    /* Set return value */
    ret_value = list;

done:
    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
        HDONE_ERROR(H5E_SOHM, H5E_CLOSEERROR, NULL, "can't close wrapped buffer")
    if(!ret_value && list) {
        if(list->messages)
            list->messages = H5FL_ARR_FREE(H5SM_sohm_t, list->messages);
        list = H5FL_FREE(H5SM_list_t, list);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_list_load() */


/*-------------------------------------------------------------------------
 * Function:	H5SM_list_flush
 *
 * Purpose:	Flush this list index.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *		November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_list_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5SM_list_t *list)
{
    H5WB_t *wb = NULL;                  /* Wrapped buffer for list index data */
    uint8_t lst_buf[H5SM_LST_BUF_SIZE]; /* Buffer for list index */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5SM_list_flush)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(list);
    HDassert(list->header);

    if(list->cache_info.is_dirty) {
        uint8_t *buf;               /* Temporary buffer */
        uint8_t *p;                 /* Pointer into raw data buffer */
        size_t size;                /* Header size on disk */
        uint32_t computed_chksum;   /* Computed metadata checksum value */
        size_t mesgs_written;       /* Number of messages written to list */
        size_t x;                   /* Local index variable */

        /* Wrap the local buffer for serialized list index info */
        if(NULL == (wb = H5WB_wrap(lst_buf, sizeof(lst_buf))))
            HGOTO_ERROR(H5E_SOHM, H5E_CANTINIT, FAIL, "can't wrap buffer")

        size = H5SM_LIST_SIZE(f, list->header->num_messages);

        /* Get a pointer to a buffer that's large enough for serialized list index */
        if(NULL == (buf = H5WB_actual(wb, size)))
            HGOTO_ERROR(H5E_SOHM, H5E_NOSPACE, FAIL, "can't get actual buffer")

        /* Get temporary pointer to buffer for serialized list index */
        p = buf;

        /* Encode magic number */
        HDmemcpy(p, H5SM_LIST_MAGIC, (size_t)H5SM_SIZEOF_MAGIC);
        p += H5SM_SIZEOF_MAGIC;

        /* Write messages from the messages array to disk */
        mesgs_written = 0;
        for(x = 0; x < list->header->list_max && mesgs_written < list->header->num_messages; x++) {
            if(list->messages[x].location != H5SM_NO_LOC) {
                if(H5SM_message_encode(f, p, &(list->messages[x])) < 0)
                    HGOTO_ERROR(H5E_SOHM, H5E_CANTFLUSH, FAIL, "unable to write shared message to disk")

                p+=H5SM_SOHM_ENTRY_SIZE(f);
                ++mesgs_written;
            } /* end if */
        } /* end for */
        HDassert(mesgs_written == list->header->num_messages);

        /* Compute checksum on buffer */
        computed_chksum = H5_checksum_metadata(buf, (size - H5SM_SIZEOF_CHECKSUM), 0);
        UINT32ENCODE(p, computed_chksum);

        /* Write the list to disk */
        HDassert((size_t)(p - buf) == size);
	if(H5F_block_write(f, H5FD_MEM_SOHM_INDEX, addr, size, dxpl_id, buf) < 0)
	    HGOTO_ERROR(H5E_SOHM, H5E_CANTFLUSH, FAIL, "unable to save sohm table to disk")

        list->cache_info.is_dirty = FALSE;
    } /* end if */

    if(destroy)
        if(H5SM_list_dest(f, list) < 0)
	    HGOTO_ERROR(H5E_SOHM, H5E_CANTFREE, FAIL, "unable to destroy list")

done:
    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
        HDONE_ERROR(H5E_SOHM, H5E_CLOSEERROR, FAIL, "can't close wrapped buffer")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_list_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5SM_list_dest
 *
 * Purpose:	Frees all memory used by the list.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *		November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_list_dest(H5F_t UNUSED *f, H5SM_list_t* list)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5SM_list_dest)

    HDassert(list);
    HDassert(list->messages);

    /* Destroy Shared Object Header Message list */
    if(H5SM_list_free(list) < 0)
        HGOTO_ERROR(H5E_SOHM, H5E_CANTRELEASE, FAIL, "unable to free shared message list")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_list_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5SM_list_clear
 *
 * Purpose:	Marks a list as not dirty.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *		November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_list_clear(H5F_t *f, H5SM_list_t *list, hbool_t destroy)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5SM_list_clear)

    /*
     * Check arguments.
     */
    HDassert(list);

    /* Reset the dirty flag.  */
    list->cache_info.is_dirty = FALSE;

    if(destroy)
        if(H5SM_list_dest(f, list) < 0)
	    HGOTO_ERROR(H5E_SOHM, H5E_CANTFREE, FAIL, "unable to destroy SOHM list")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end of H5SM_list_clear() */


/*-------------------------------------------------------------------------
 * Function:	H5SM_list_size
 *
 * Purpose:	Gets the size of a list on disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *		November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_list_size(const H5F_t UNUSED *f, const H5SM_list_t *list, size_t *size_ptr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_list_size)

    /* check arguments */
    HDassert(f);
    HDassert(list);
    HDassert(list->header);
    HDassert(size_ptr);

    /* Set size value */
    *size_ptr = H5SM_LIST_SIZE(f, list->header->list_max);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_list_size() */

