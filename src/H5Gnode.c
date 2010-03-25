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

/*-------------------------------------------------------------------------
 *
 * Created:		H5Gnode.c
 *			Jun 26 1997
 *			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		Functions for handling symbol table nodes.  A
 *			symbol table node is a small collection of symbol
 *			table entries.	A B-tree usually points to the
 *			symbol table nodes for any given symbol table.
 *
 *-------------------------------------------------------------------------
 */
#define H5G_PACKAGE		/*suppress error about including H5Gpkg   */
#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */


/* Packages needed by this file... */
#include "H5private.h"		/* Generic Functions			*/
#include "H5ACprivate.h"	/* Metadata cache			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"		/* File access				*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5HLprivate.h"	/* Local Heaps				*/
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5WBprivate.h"        /* Wrapped Buffers                      */

/* Private typedefs */

/*
 * Each key field of the B-link tree that points to symbol table
 * nodes consists of this structure...
 */
typedef struct H5G_node_key_t {
    size_t      offset;                 /*offset into heap for name          */
} H5G_node_key_t;

/*
 * A symbol table node is a collection of symbol table entries.  It can
 * be thought of as the lowest level of the B-link tree that points to
 * a collection of symbol table entries that belong to a specific symbol
 * table or group.
 */
typedef struct H5G_node_t {
    H5AC_info_t cache_info;    /* Information for H5AC cache functions, _must_ be */
                                /* first field in structure */
    unsigned nsyms;             /*number of symbols                  */
    H5G_entry_t *entry;         /*array of symbol table entries      */
} H5G_node_t;


/* Private macros */
#define H5G_NODE_VERS   1               /*symbol table node version number   */
#define H5G_NODE_SIZEOF_HDR(F) (H5G_NODE_SIZEOF_MAGIC + 4)

/* Size of stack buffer for serialized nodes */
#define H5G_NODE_BUF_SIZE       512

/* PRIVATE PROTOTYPES */
static herr_t H5G_node_serialize(H5F_t *f, H5G_node_t *sym, size_t size, uint8_t *buf);
static size_t H5G_node_size_real(const H5F_t *f);

/* Metadata cache callbacks */
static H5G_node_t *H5G_node_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_udata1,
				 void *_udata2);
static herr_t H5G_node_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr,
			     H5G_node_t *sym, unsigned UNUSED * flags_ptr);
static herr_t H5G_node_dest(H5F_t *f, H5G_node_t *sym);
static herr_t H5G_node_clear(H5F_t *f, H5G_node_t *sym, hbool_t destroy);
static herr_t H5G_node_size(const H5F_t *f, const H5G_node_t *sym, size_t *size_ptr);

/* B-tree callbacks */
static H5RC_t *H5G_node_get_shared(const H5F_t *f, const void *_udata);
static herr_t H5G_node_create(H5F_t *f, hid_t dxpl_id, H5B_ins_t op, void *_lt_key,
			      void *_udata, void *_rt_key,
			      haddr_t *addr_p/*out*/);
static int H5G_node_cmp2(void *_lt_key, void *_udata, void *_rt_key);
static int H5G_node_cmp3(void *_lt_key, void *_udata, void *_rt_key);
static herr_t H5G_node_found(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_lt_key,
			     void *_udata);
static H5B_ins_t H5G_node_insert(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *_lt_key,
				 hbool_t *lt_key_changed, void *_md_key,
				 void *_udata, void *_rt_key,
				 hbool_t *rt_key_changed,
				 haddr_t *new_node_p/*out*/);
static H5B_ins_t H5G_node_remove(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *lt_key,
				 hbool_t *lt_key_changed, void *udata,
				 void *rt_key, hbool_t *rt_key_changed);
static herr_t H5G_node_decode_key(const H5B_shared_t *shared, const uint8_t *raw, void *_key);
static herr_t H5G_node_encode_key(const H5B_shared_t *shared, uint8_t *raw, const void *_key);
static herr_t H5G_node_debug_key(FILE *stream, int indent, int fwidth,
    const void *key, const void *udata);

/* H5G inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_SNODE[1] = {{
    H5AC_SNODE_ID,
    (H5AC_load_func_t)H5G_node_load,
    (H5AC_flush_func_t)H5G_node_flush,
    (H5AC_dest_func_t)H5G_node_dest,
    (H5AC_clear_func_t)H5G_node_clear,
    (H5AC_size_func_t)H5G_node_size,
}};

/* H5G inherits B-tree like properties from H5B */
H5B_class_t H5B_SNODE[1] = {{
    H5B_SNODE_ID,		/*id			*/
    sizeof(H5G_node_key_t), 	/*sizeof_nkey		*/
    H5G_node_get_shared,	/*get_shared		*/
    H5G_node_create,		/*new			*/
    H5G_node_cmp2,		/*cmp2			*/
    H5G_node_cmp3,		/*cmp3			*/
    H5G_node_found,		/*found			*/
    H5G_node_insert,		/*insert		*/
    TRUE,			/*follow min branch?	*/
    TRUE,			/*follow max branch?	*/
    H5G_node_remove,		/*remove		*/
    H5G_node_decode_key,	/*decode		*/
    H5G_node_encode_key,	/*encode		*/
    H5G_node_debug_key,		/*debug			*/
}};

/* Declare a free list to manage the H5G_node_t struct */
H5FL_DEFINE_STATIC(H5G_node_t);

/* Declare a free list to manage sequences of H5G_entry_t's */
H5FL_SEQ_DEFINE_STATIC(H5G_entry_t);


/*-------------------------------------------------------------------------
 * Function:	H5G_node_get_shared
 *
 * Purpose:	Returns the shared B-tree info for the specified UDATA.
 *
 * Return:	Success:	Pointer to the raw B-tree page for this
                                file's groups
 *
 *		Failure:	Can't fail
 *
 * Programmer:	Robb Matzke
 *		Wednesday, October  8, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5RC_t *
H5G_node_get_shared(const H5F_t *f, const void UNUSED *_udata)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_node_get_shared)

    HDassert(f);

    /* Return the pointer to the ref-count object */
    FUNC_LEAVE_NOAPI(H5F_GRP_BTREE_SHARED(f))
} /* end H5G_node_get_shared() */


/*-------------------------------------------------------------------------
 * Function:	H5G_node_decode_key
 *
 * Purpose:	Decodes a raw key into a native key.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul  8 1997
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_node_decode_key(const H5B_shared_t *shared, const uint8_t *raw, void *_key)
{
    H5G_node_key_t	   *key = (H5G_node_key_t *) _key;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_node_decode_key)

    HDassert(shared);
    HDassert(raw);
    HDassert(key);

    H5F_DECODE_LENGTH_LEN(raw, key->offset, shared->sizeof_len);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5G_node_decode_key() */


/*-------------------------------------------------------------------------
 * Function:	H5G_node_encode_key
 *
 * Purpose:	Encodes a native key into a raw key.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul  8 1997
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_node_encode_key(const H5B_shared_t *shared, uint8_t *raw, const void *_key)
{
    const H5G_node_key_t *key = (const H5G_node_key_t *) _key;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_node_encode_key)

    HDassert(shared);
    HDassert(raw);
    HDassert(key);

    H5F_ENCODE_LENGTH_LEN(raw, key->offset, shared->sizeof_len);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5G_node_encode_key() */


/*-------------------------------------------------------------------------
 * Function:	H5G_node_debug_key
 *
 * Purpose:	Prints a key.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Friday, February 28, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_node_debug_key(FILE *stream, int indent, int fwidth, const void *_key,
    const void *_udata)
{
    const H5G_node_key_t   *key = (const H5G_node_key_t *) _key;
    const H5G_bt_common_t   *udata = (const H5G_bt_common_t *) _udata;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_node_debug_key)

    HDassert(key);

    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth, "Heap offset:",
        (unsigned)key->offset);

    if(udata->heap) {
        const char *s;

        HDfprintf(stream, "%*s%-*s ", indent, "", fwidth, "Name:");

        s = H5HL_offset_into(udata->heap, key->offset);
        HDfprintf(stream, "%s\n", s);
    } /* end if */
    else
        HDfprintf(stream, "%*s%-*s ", indent, "", fwidth, "Cannot get name; heap address not specified\n");

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5G_node_debug_key() */


/*-------------------------------------------------------------------------
 * Function:	H5G_node_size_real
 *
 * Purpose:	Returns the total size of a symbol table node.
 *
 * Return:	Success:	Total size of the node in bytes.
 *
 *		Failure:	Never fails.
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jun 23 1997
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5G_node_size_real(const H5F_t *f)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_node_size_real);

    FUNC_LEAVE_NOAPI(H5G_NODE_SIZEOF_HDR(f) +
                     (2 * H5F_SYM_LEAF_K(f)) * H5G_SIZEOF_ENTRY(f));
} /* end H5G_node_size_real() */


/*-------------------------------------------------------------------------
 * Function:	H5G_node_load
 *
 * Purpose:	Loads a symbol table node from the file.
 *
 * Return:	Success:	Ptr to the new table.
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jun 23 1997
 *
 *-------------------------------------------------------------------------
 */
static H5G_node_t *
H5G_node_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void UNUSED  *_udata1,
	      void UNUSED * _udata2)
{
    H5G_node_t		   *sym = NULL;
    size_t		    size;
    H5WB_t                 *wb = NULL;     /* Wrapped buffer for node data */
    uint8_t                 node_buf[H5G_NODE_BUF_SIZE]; /* Buffer for node */
    uint8_t		   *node;           /* Pointer to node buffer */
    const uint8_t	   *p;
    H5G_node_t		   *ret_value;	/*for error handling */

    FUNC_ENTER_NOAPI_NOINIT(H5G_node_load)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(!_udata1);
    HDassert(NULL == _udata2);

    /*
     * Initialize variables.
     */

    /* Wrap the local buffer for serialized node info */
    if(NULL == (wb = H5WB_wrap(node_buf, sizeof(node_buf))))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't wrap buffer")

    /* Compute the size of the serialized symbol table node on disk */
    size = H5G_node_size_real(f);

    /* Get a pointer to a buffer that's large enough for node */
    if(NULL == (node = H5WB_actual(wb, size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, NULL, "can't get actual buffer")

    /* Read the serialized symbol table node. */
    if(H5F_block_read(f, H5FD_MEM_BTREE, addr, size, dxpl_id, node) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_READERROR, NULL, "unable to read symbol table node")

    /* Get temporary pointer to serialized node */
    p = node;

    /* magic */
    if(HDmemcmp(p, H5G_NODE_MAGIC, (size_t)H5G_NODE_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_SYM, H5E_CANTLOAD, NULL, "bad symbol table node signature")
    p += 4;

    /* version */
    if(H5G_NODE_VERS != *p++)
	HGOTO_ERROR(H5E_SYM, H5E_CANTLOAD, NULL, "bad symbol table node version")

    /* reserved */
    p++;

    /* Allocate symbol table data structures */
    if(NULL == (sym = H5FL_CALLOC(H5G_node_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    if(NULL == (sym->entry = H5FL_SEQ_CALLOC(H5G_entry_t, (size_t)(2 * H5F_SYM_LEAF_K(f)))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* number of symbols */
    UINT16DECODE(p, sym->nsyms);

    /* entries */
    if(H5G_ent_decode_vec(f, &p, sym->entry, sym->nsyms) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTLOAD, NULL, "unable to decode symbol table entries")

    /* Set return value */
    ret_value = sym;

done:
    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, NULL, "can't close wrapped buffer")
    if(!ret_value)
        if(sym && H5G_node_dest(f, sym) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTFREE, NULL, "unable to destroy symbol table node")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_node_load() */


/*-------------------------------------------------------------------------
 * Function:	H5G_node_flush
 *
 * Purpose:	Flush a symbol table node to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jun 23 1997
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_node_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5G_node_t *sym, unsigned UNUSED * flags_ptr)
{
    H5WB_t     *wb = NULL;     /* Wrapped buffer for node data */
    uint8_t     node_buf[H5G_NODE_BUF_SIZE]; /* Buffer for node */
    unsigned	u;
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_node_flush)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(sym);

    /*
     * Look for dirty entries and set the node dirty flag.
     */
    for(u = 0; u < sym->nsyms; u++)
	if(sym->entry[u].dirty) {
            /* Set the node's dirty flag */
            sym->cache_info.is_dirty = TRUE;

            /* Reset the entry's dirty flag */
            sym->entry[u].dirty = FALSE;
        } /* end if */

    /*
     * Write the symbol node to disk.
     */
    if(sym->cache_info.is_dirty) {
        uint8_t	   *node;           /* Pointer to node buffer */
        size_t	size;

        /* Wrap the local buffer for serialized node info */
        if(NULL == (wb = H5WB_wrap(node_buf, sizeof(node_buf))))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't wrap buffer")

        /* Compute the size of the serialized symbol table node on disk */
        size = H5G_node_size_real(f);

        /* Get a pointer to a buffer that's large enough for node */
        if(NULL == (node = H5WB_actual(wb, size)))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't get actual buffer")

        /* Serialize symbol table node into buffer */
        if(H5G_node_serialize(f, sym, size, node) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTSERIALIZE, FAIL, "node serialization failed")

	/* Write the serialized symbol table node. */
        if(H5F_block_write(f, H5FD_MEM_BTREE, addr, size, dxpl_id, node) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "unable to write symbol table node to the file")

        /* Reset the node's dirty flag */
        sym->cache_info.is_dirty = FALSE;
    } /* end if */

    /*
     * Destroy the symbol node?	 This might happen if the node is being
     * preempted from the cache.
     */
    if(destroy)
        if(H5G_node_dest(f, sym) < 0)
	    HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to destroy symbol table node")

done:
    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "can't close wrapped buffer")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_node_flush() */


/*-------------------------------------------------------------------------
 * Function:    H5G_node_serialize
 *
 * Purpose:     Serialize the symbol table node
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Bill Wendling
 *              wendling@ncsa.uiuc.edu
 *              Sept. 16, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_node_serialize(H5F_t *f, H5G_node_t *sym, size_t size, uint8_t *buf)
{
    uint8_t    *p;
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5G_node_serialize);

    /* check args */
    assert(f);
    assert(sym);
    assert(buf);

    p = buf;

    /* magic number */
    HDmemcpy(p, H5G_NODE_MAGIC, (size_t)H5G_NODE_SIZEOF_MAGIC);
    p += 4;

    /* version number */
    *p++ = H5G_NODE_VERS;

    /* reserved */
    *p++ = 0;

    /* number of symbols */
    UINT16ENCODE(p, sym->nsyms);

    /* entries */
    if(H5G_ent_encode_vec(f, &p, sym->entry, sym->nsyms) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "can't serialize")
    HDmemset(p, 0, size - (p - buf));

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_node_serialize() */


/*-------------------------------------------------------------------------
 * Function:	H5G_node_dest
 *
 * Purpose:	Destroy a symbol table node in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Jan 15 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_node_dest(H5F_t UNUSED *f, H5G_node_t *sym)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_node_dest)

    /* Check arguments */
    HDassert(sym);

    /* Verify that node is clean */
    HDassert(sym->cache_info.is_dirty == FALSE);

    if(sym->entry)
        sym->entry = H5FL_SEQ_FREE(H5G_entry_t, sym->entry);
    H5FL_FREE(H5G_node_t, sym);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5G_node_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5G_node_clear
 *
 * Purpose:	Mark a symbol table node in memory as non-dirty.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 20 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_node_clear(H5F_t *f, H5G_node_t *sym, hbool_t destroy)
{
    unsigned u;              /* Local index variable */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5G_node_clear)

    /*
     * Check arguments.
     */
    HDassert(sym);

    /* Look for dirty entries and reset their dirty flag.  */
    for(u = 0; u < sym->nsyms; u++)
        sym->entry[u].dirty = FALSE;
    sym->cache_info.is_dirty = FALSE;

    /*
     * Destroy the symbol node?	 This might happen if the node is being
     * preempted from the cache.
     */
    if(destroy)
        if(H5G_node_dest(f, sym) < 0)
	    HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to destroy symbol table node")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_node_clear() */


/*-------------------------------------------------------------------------
 * Function:	H5G_node_size
 *
 * Purpose:	Compute the size in bytes of the specified instance of
 *		H5G_node_t on disk, and return it in *size_ptr.  On failure
 *		the value of size_ptr is undefined.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer
 *		5/13/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_node_size(const H5F_t *f, const H5G_node_t UNUSED *sym, size_t *size_ptr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_node_size)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(size_ptr);

    *size_ptr = H5G_node_size_real(f);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5G_node_size() */


/*-------------------------------------------------------------------------
 * Function:	H5G_node_create
 *
 * Purpose:	Creates a new empty symbol table node.	This function is
 *		called by the B-tree insert function for an empty tree.	 It
 *		is also called internally to split a symbol node with LT_KEY
 *		and RT_KEY null pointers.
 *
 * Return:	Success:	Non-negative.  The address of symbol table
 *				node is returned through the ADDR_P argument.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jun 23 1997
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_node_create(H5F_t *f, hid_t dxpl_id, H5B_ins_t UNUSED op, void *_lt_key,
		void UNUSED *_udata, void *_rt_key, haddr_t *addr_p/*out*/)
{
    H5G_node_key_t	   *lt_key = (H5G_node_key_t *) _lt_key;
    H5G_node_key_t	   *rt_key = (H5G_node_key_t *) _rt_key;
    H5G_node_t		   *sym = NULL;
    hsize_t		    size = 0;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_node_create)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(H5B_INS_FIRST == op);

    if(NULL == (sym = H5FL_CALLOC(H5G_node_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
    size = H5G_node_size_real(f);
    HDassert(size);
    if(HADDR_UNDEF == (*addr_p = H5MF_alloc(f, H5FD_MEM_BTREE, dxpl_id, size)))
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to allocate file space")

    if(NULL == ( sym->entry = H5FL_SEQ_CALLOC(H5G_entry_t, (size_t)(2 * H5F_SYM_LEAF_K(f)))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
    if(H5AC_set(f, dxpl_id, H5AC_SNODE, *addr_p, sym, H5AC__NO_FLAGS_SET) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to cache symbol table leaf node")
    /*
     * The left and right symbols in an empty tree are both the
     * empty string stored at offset zero by the H5G functions. This
     * allows the comparison functions to work correctly without knowing
     * that there are no symbols.
     */
    if(lt_key)
        lt_key->offset = 0;
    if(rt_key)
        rt_key->offset = 0;

done:
    if(ret_value < 0) {
        if(sym != NULL) {
            if(sym->entry != NULL)
                H5FL_SEQ_FREE(H5G_entry_t, sym->entry);
            H5FL_FREE(H5G_node_t, sym);
        } /* end if */
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_node_create() */


/*-------------------------------------------------------------------------
 * Function:	H5G_node_cmp2
 *
 * Purpose:	Compares two keys from a B-tree node (LT_KEY and RT_KEY).
 *		The UDATA pointer supplies extra data not contained in the
 *		keys (in this case, the heap address).
 *
 * Return:	Success:	negative if LT_KEY is less than RT_KEY.
 *
 *				positive if LT_KEY is greater than RT_KEY.
 *
 *				zero if LT_KEY and RT_KEY are equal.
 *
 *		Failure:	FAIL (same as LT_KEY<RT_KEY)
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jun 23 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
H5G_node_cmp2(void *_lt_key, void *_udata, void *_rt_key)
{
    H5G_bt_common_t	   *udata = (H5G_bt_common_t *) _udata;
    H5G_node_key_t	   *lt_key = (H5G_node_key_t *) _lt_key;
    H5G_node_key_t	   *rt_key = (H5G_node_key_t *) _rt_key;
    const char		   *s1, *s2;
    const char		   *base;           /* Base of heap */
    int		           ret_value;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_node_cmp2)

    /* Sanity checks */
    HDassert(udata && udata->heap);
    HDassert(lt_key);
    HDassert(rt_key);

    /* Get base address of heap */
    base = H5HL_offset_into(udata->heap, (size_t)0);
    HDassert(base);

    /* Get pointers to string names */
    s1 = base + lt_key->offset;
    s2 = base + rt_key->offset;

    /* Set return value */
    ret_value = HDstrcmp(s1, s2);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5G_node_cmp2() */


/*-------------------------------------------------------------------------
 * Function:	H5G_node_cmp3
 *
 * Purpose:	Compares two keys from a B-tree node (LT_KEY and RT_KEY)
 *		against another key (not necessarily the same type)
 *		pointed to by UDATA.
 *
 * Return:	Success:	negative if the UDATA key is less than
 *				or equal to the LT_KEY
 *
 *				positive if the UDATA key is greater
 *				than the RT_KEY.
 *
 *				zero if the UDATA key falls between
 *				the LT_KEY (exclusive) and the
 *				RT_KEY (inclusive).
 *
 *		Failure:	FAIL (same as UDATA < LT_KEY)
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jun 23 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
H5G_node_cmp3(void *_lt_key, void *_udata, void *_rt_key)
{
    H5G_bt_common_t	*udata = (H5G_bt_common_t *) _udata;
    H5G_node_key_t	*lt_key = (H5G_node_key_t *) _lt_key;
    H5G_node_key_t	*rt_key = (H5G_node_key_t *) _rt_key;
    const char		*s;
    const char          *base;              /* Base of heap */
    int                  ret_value = 0;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_node_cmp3)

    /* Sanity checks */
    HDassert(udata && udata->heap);
    HDassert(lt_key);
    HDassert(rt_key);

    /* Get base address of heap */
    base = H5HL_offset_into(udata->heap, (size_t)0);
    HDassert(base);

    /* left side */
    s = base + lt_key->offset;
    if(HDstrcmp(udata->name, s) <= 0)
	ret_value = (-1);
    else {
        /* right side */
        s = base + rt_key->offset;
        if(HDstrcmp(udata->name, s) > 0)
            ret_value = 1;
    } /* end else */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_node_cmp3() */


/*-------------------------------------------------------------------------
 * Function:	H5G_node_found
 *
 * Purpose:	The B-tree search engine has found the symbol table node
 *		which contains the requested symbol if the symbol exists.
 *		This function should examine that node for the symbol and
 *		return information about the symbol through the UDATA
 *		structure which contains the symbol name on function
 *		entry.
 *
 *		If the operation flag in UDATA is H5G_OPER_FIND, then
 *		the entry is copied from the symbol table to the UDATA
 *		entry field.  Otherwise the entry is copied from the
 *		UDATA entry field to the symbol table.
 *
 * Return:	Success:	Non-negative if found and data returned through
 *				the UDATA pointer.
 *
 *		Failure:	Negative if not found.
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jun 23 1997
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_node_found(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void UNUSED *_lt_key,
    void *_udata)
{
    H5G_bt_lkp_t	*udata = (H5G_bt_lkp_t *)_udata;
    H5G_node_t		*sn = NULL;
    unsigned		lt = 0, idx = 0, rt;
    int		        cmp = 1;
    const char		*s;
    const char          *base;           /* Base of heap */
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_node_found)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(udata && udata->common.heap);

    /*
     * Load the symbol table node for exclusive access.
     */
    if(NULL == (sn = H5AC_protect(f, dxpl_id, H5AC_SNODE, addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_SYM, H5E_CANTLOAD, FAIL, "unable to protect symbol table node")

    /* Get base address of heap */
    base = H5HL_offset_into(udata->common.heap, (size_t)0);
    HDassert(base);

    /*
     * Binary search.
     */
    rt = sn->nsyms;
    while(lt < rt && cmp) {
	idx = (lt + rt) / 2;
        s = base + sn->entry[idx].name_off;
	cmp = HDstrcmp(udata->common.name, s);

	if (cmp < 0)
	    rt = idx;
	else
	    lt = idx + 1;
    } /* end while */

    if(cmp)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "not found")

    /* Call user's callback operator */
    if((udata->op)(&sn->entry[idx], udata->op_data) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_BADITER, FAIL, "iterator callback failed")

done:
    if(sn && H5AC_unprotect(f, dxpl_id, H5AC_SNODE, addr, sn, H5AC__NO_FLAGS_SET) < 0)
	HDONE_ERROR(H5E_SYM, H5E_PROTECT, FAIL, "unable to release symbol table node")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_node_found() */


/*-------------------------------------------------------------------------
 * Function:	H5G_node_insert
 *
 * Purpose:	The B-tree insertion engine has found the symbol table node
 *		which should receive the new symbol/address pair.  This
 *		function adds it to that node unless it already existed.
 *
 *		If the node has no room for the symbol then the node is
 *		split into two nodes.  The original node contains the
 *		low values and the new node contains the high values.
 *		The new symbol table entry is added to either node as
 *		appropriate.  When a split occurs, this function will
 *		write the maximum key of the low node to the MID buffer
 *		and return the address of the new node.
 *
 *		If the new key is larger than RIGHT then update RIGHT
 *		with the new key.
 *
 * Return:	Success:	An insertion command for the caller, one of
 *				the H5B_INS_* constants.  The address of the
 *				new node, if any, is returned through the
 *				NEW_NODE_P argument.  NEW_NODE_P might not be
 *				initialized if the return value is
 *				H5B_INS_NOOP.
 *
 *		Failure:	H5B_INS_ERROR, NEW_NODE_P might not be
 *				initialized.
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jun 24 1997
 *
 *-------------------------------------------------------------------------
 */
static H5B_ins_t
H5G_node_insert(H5F_t *f, hid_t dxpl_id, haddr_t addr,
    void UNUSED *_lt_key, hbool_t UNUSED *lt_key_changed,
    void *_md_key, void *_udata,
    void *_rt_key, hbool_t *rt_key_changed,
    haddr_t *new_node_p)
{
    H5G_node_key_t	*md_key = (H5G_node_key_t *) _md_key;
    H5G_node_key_t	*rt_key = (H5G_node_key_t *) _rt_key;
    H5G_bt_ins_t	*udata = (H5G_bt_ins_t *) _udata;
    H5G_node_t		*sn = NULL, *snrt = NULL;
    unsigned		sn_flags = H5AC__NO_FLAGS_SET, snrt_flags = H5AC__NO_FLAGS_SET;
    const char		*s;
    const char          *base;                  /* Base of heap */
    unsigned		lt = 0, rt;		/* Binary search cntrs	*/
    int		        cmp = 1, idx = -1;
    H5G_node_t		*insert_into = NULL;	/*node that gets new entry*/
    H5G_entry_t         ent;                    /* Entry to insert in node */
    H5B_ins_t		ret_value = H5B_INS_ERROR;

    FUNC_ENTER_NOAPI_NOINIT(H5G_node_insert)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(md_key);
    HDassert(rt_key);
    HDassert(udata && udata->common.heap);
    HDassert(new_node_p);

    /*
     * Load the symbol node.
     */
    if(NULL == (sn = H5AC_protect(f, dxpl_id, H5AC_SNODE, addr, NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_SYM, H5E_CANTLOAD, H5B_INS_ERROR, "unable to protect symbol table node")

    /* Get base address of heap */
    base = H5HL_offset_into(udata->common.heap, (size_t)0);
    HDassert(base);

    /*
     * Where does the new symbol get inserted?	We use a binary search.
     */
    rt = sn->nsyms;
    while(lt < rt) {
	idx = (lt + rt) / 2;
        s = base + sn->entry[idx].name_off;

        /* Check if symbol is already present */
	if(0 == (cmp = HDstrcmp(udata->common.name, s)))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, H5B_INS_ERROR, "symbol is already present in symbol table")

	if (cmp < 0)
	    rt = idx;
	else
	    lt = idx + 1;
    } /* end while */
    idx += cmp > 0 ? 1 : 0;

    /* Convert link information & name to symbol table entry */
    if(H5G_ent_convert(f, dxpl_id, udata->common.heap, udata->common.name, udata->lnk, &ent) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTCONVERT, H5B_INS_ERROR, "unable to convert link")

    /* Determine where to place entry in node */
    if(sn->nsyms >= 2 * H5F_SYM_LEAF_K(f)) {
	/*
	 * The node is full.  Split it into a left and right
	 * node and return the address of the new right node (the
	 * left node is at the same address as the original node).
	 */
	ret_value = H5B_INS_RIGHT;

	/* The right node */
	if(H5G_node_create(f, dxpl_id, H5B_INS_FIRST, NULL, NULL, NULL, new_node_p/*out*/) < 0)
	    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, H5B_INS_ERROR, "unable to split symbol table node")

	if(NULL == (snrt = H5AC_protect(f, dxpl_id, H5AC_SNODE, *new_node_p, NULL, NULL, H5AC_WRITE)))
	    HGOTO_ERROR(H5E_SYM, H5E_CANTLOAD, H5B_INS_ERROR, "unable to split symbol table node")

	HDmemcpy(snrt->entry, sn->entry + H5F_SYM_LEAF_K(f),
		 H5F_SYM_LEAF_K(f) * sizeof(H5G_entry_t));
	snrt->nsyms = H5F_SYM_LEAF_K(f);
        snrt_flags |= H5AC__DIRTIED_FLAG;

	/* The left node */
	HDmemset(sn->entry + H5F_SYM_LEAF_K(f), 0,
		 H5F_SYM_LEAF_K(f) * sizeof(H5G_entry_t));
	sn->nsyms = H5F_SYM_LEAF_K(f);
        sn_flags |= H5AC__DIRTIED_FLAG;

	/* The middle key */
	md_key->offset = sn->entry[sn->nsyms - 1].name_off;

	/* Where to insert the new entry? */
	if(idx <= (int)H5F_SYM_LEAF_K(f)) {
	    insert_into = sn;
	    if(idx == (int)H5F_SYM_LEAF_K(f))
		md_key->offset = ent.name_off;
	} else {
	    idx -= H5F_SYM_LEAF_K(f);
	    insert_into = snrt;
	    if(idx == (int)H5F_SYM_LEAF_K (f)) {
		rt_key->offset = ent.name_off;
		*rt_key_changed = TRUE;
	    } /* end if */
	} /* end else */
    } else {
	/* Where to insert the new entry? */
	ret_value = H5B_INS_NOOP;
        sn_flags |= H5AC__DIRTIED_FLAG;
	insert_into = sn;
	if(idx == (int)sn->nsyms) {
	    rt_key->offset = ent.name_off;
	    *rt_key_changed = TRUE;
	} /* end if */
    } /* end else */

    /* Move entries down to make room for new entry */
    HDmemmove(insert_into->entry + idx + 1, insert_into->entry + idx,
	      (insert_into->nsyms - idx) * sizeof(H5G_entry_t));

    /* Copy new entry into table */
    H5G_ent_copy(&(insert_into->entry[idx]), &ent, H5_COPY_SHALLOW);

    /* Flag entry as dirty */
    insert_into->entry[idx].dirty = TRUE;

    /* Increment # of symbols in table */
    insert_into->nsyms += 1;

done:
    if(snrt && H5AC_unprotect(f, dxpl_id, H5AC_SNODE, *new_node_p, snrt, snrt_flags) < 0)
	HDONE_ERROR(H5E_SYM, H5E_PROTECT, H5B_INS_ERROR, "unable to release symbol table node")
    if(sn && H5AC_unprotect(f, dxpl_id, H5AC_SNODE, addr, sn, sn_flags) < 0)
	HDONE_ERROR(H5E_SYM, H5E_PROTECT, H5B_INS_ERROR, "unable to release symbol table node")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_node_insert() */


/*-------------------------------------------------------------------------
 * Function:	H5G_node_remove
 *
 * Purpose:	The B-tree removal engine has found the symbol table node
 *		which should contain the name which is being removed.  This
 *		function removes the name from the symbol table and
 *		decrements the link count on the object to which the name
 *		points.
 *
 *              If the udata->name parameter is set to NULL, then remove
 *              all entries in this symbol table node.  This only occurs
 *              during the deletion of the entire group, so don't bother
 *              freeing individual name entries in the local heap, the group's
 *              symbol table removal code will just free the entire local
 *              heap eventually.  Do reduce the link counts for each object
 *              however.
 *
 * Return:	Success:	If all names are removed from the symbol
 *				table node then H5B_INS_REMOVE is returned;
 *				otherwise H5B_INS_NOOP is returned.
 *
 *		Failure:	H5B_INS_ERROR
 *
 * Programmer:	Robb Matzke
 *              Thursday, September 24, 1998
 *
 *-------------------------------------------------------------------------
 */
static H5B_ins_t
H5G_node_remove(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *_lt_key/*in,out*/,
		hbool_t UNUSED *lt_key_changed/*out*/,
		void *_udata/*in,out*/, void *_rt_key/*in,out*/,
		hbool_t *rt_key_changed/*out*/)
{
    H5G_node_key_t	*lt_key = (H5G_node_key_t *)_lt_key;
    H5G_node_key_t	*rt_key = (H5G_node_key_t *)_rt_key;
    H5G_bt_rm_t	*udata = (H5G_bt_rm_t *)_udata;
    H5G_node_t		*sn = NULL;
    unsigned		sn_flags = H5AC__NO_FLAGS_SET;
    unsigned		lt = 0, rt, idx = 0;
    int		        cmp = 1;
    H5B_ins_t		ret_value = H5B_INS_ERROR;

    FUNC_ENTER_NOAPI_NOINIT(H5G_node_remove)

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(lt_key);
    HDassert(rt_key);
    HDassert(udata && udata->common.heap);

    /* Load the symbol table */
    if(NULL == (sn = H5AC_protect(f, dxpl_id, H5AC_SNODE, addr, NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_SYM, H5E_CANTLOAD, H5B_INS_ERROR, "unable to protect symbol table node")

    /* "Normal" removal of a single entry from the symbol table node */
    if(udata->common.name != NULL) {
        H5O_link_t lnk;         /* Constructed link for replacement */
        size_t len;             /* Length of string in local heap */
        const char *base;       /* Base of heap */

        /* Get base address of heap */
        base = H5HL_offset_into(udata->common.heap, (size_t)0);

        /* Find the name with a binary search */
        rt = sn->nsyms;
        while(lt < rt && cmp) {
            const char *s;          /* Pointer to string in local heap */

            idx = (lt + rt) / 2;
            s = base + sn->entry[idx].name_off;
            cmp = HDstrcmp(udata->common.name, s);
            if(cmp < 0)
                rt = idx;
            else
                lt = idx + 1;
        } /* end while */

        if(cmp)
            HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, H5B_INS_ERROR, "name not found")

        /* Get a pointer to the name of the link */
        if(NULL == (lnk.name = H5HL_offset_into(udata->common.heap, sn->entry[idx].name_off)))
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "unable to get link name")

        /* Set up rest of link structure */
        lnk.corder_valid = FALSE;
        lnk.corder = 0;
        lnk.cset = H5T_CSET_ASCII;
        if(sn->entry[idx].type == H5G_CACHED_SLINK) {
            lnk.type = H5L_TYPE_SOFT;
            lnk.u.soft.name = H5HL_offset_into(udata->common.heap, sn->entry[idx].cache.slink.lval_offset);
        } /* end if */
        else {
            lnk.type = H5L_TYPE_HARD;
            HDassert(H5F_addr_defined(sn->entry[idx].header));
            lnk.u.hard.addr = sn->entry[idx].header;
        } /* end else */

        /* Replace any object names */
        if(H5G_link_name_replace(f, dxpl_id, udata->grp_full_path_r, &lnk) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "unable to get object type")

        /* Decrement the ref. count for hard links */
        if(lnk.type == H5L_TYPE_HARD) {
            H5O_loc_t tmp_oloc;             /* Temporary object location */

            /* Build temporary object location */
            tmp_oloc.file = f;
            tmp_oloc.addr = lnk.u.hard.addr;

            if(H5O_link(&tmp_oloc, -1, dxpl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, H5B_INS_ERROR, "unable to decrement object link count")
        } /* end if */
        else {
            /* Remove the soft link's value from the local heap */
            if(lnk.u.soft.name) {
                len = HDstrlen(lnk.u.soft.name) + 1;
                if(H5HL_remove(f, dxpl_id, udata->common.heap, sn->entry[idx].cache.slink.lval_offset, len) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTDELETE, H5B_INS_ERROR, "unable to remove soft link from local heap")
            } /* end if */
        } /* end else */

        /* Remove the link's name from the local heap */
        len = HDstrlen(lnk.name) + 1;
        if(H5HL_remove(f, dxpl_id, udata->common.heap, sn->entry[idx].name_off, len) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTDELETE, H5B_INS_ERROR, "unable to remove link name from local heap")

        /* Remove the entry from the symbol table node */
        if(1 == sn->nsyms) {
            /*
             * We are about to remove the only symbol in this node. Copy the left
             * key to the right key and mark the right key as dirty.  Free this
             * node and indicate that the pointer to this node in the B-tree
             * should be removed also.
             */
            HDassert(0 == idx);
            *rt_key = *lt_key;
            *rt_key_changed = TRUE;
            sn->nsyms = 0;
            if(H5MF_xfree(f, H5FD_MEM_BTREE, dxpl_id, addr, (hsize_t)H5G_node_size_real(f)) < 0
                    || H5AC_unprotect(f, dxpl_id, H5AC_SNODE, addr, sn, H5AC__DIRTIED_FLAG | H5C__DELETED_FLAG) < 0) {
                sn = NULL;
                HGOTO_ERROR(H5E_SYM, H5E_PROTECT, H5B_INS_ERROR, "unable to free symbol table node")
            } /* end if */
            sn = NULL;
            ret_value = H5B_INS_REMOVE;

        } else if(0 == idx) {
            /*
             * We are about to remove the left-most entry from the symbol table
             * node but there are other entries to the right.  No key values
             * change.
             */
            sn->nsyms -= 1;
            sn_flags |= H5AC__DIRTIED_FLAG;
            HDmemmove(sn->entry + idx, sn->entry + idx + 1,
                      (sn->nsyms-idx) * sizeof(H5G_entry_t));
            ret_value = H5B_INS_NOOP;

        } else if (idx + 1 == sn->nsyms) {
            /*
             * We are about to remove the right-most entry from the symbol table
             * node but there are other entries to the left.  The right key
             * should be changed to reflect the new right-most entry.
             */
            sn->nsyms -= 1;
            sn_flags |= H5AC__DIRTIED_FLAG;
            rt_key->offset = sn->entry[sn->nsyms - 1].name_off;
            *rt_key_changed = TRUE;
            ret_value = H5B_INS_NOOP;

        } else {
            /*
             * We are about to remove an entry from the middle of a symbol table
             * node.
             */
            sn->nsyms -= 1;
            sn_flags |= H5AC__DIRTIED_FLAG;
            HDmemmove(sn->entry + idx, sn->entry + idx + 1,
                      (sn->nsyms - idx) * sizeof(H5G_entry_t));
            ret_value = H5B_INS_NOOP;
        } /* end else */
    } /* end if */
    /* Remove all entries from node, during B-tree deletion */
    else {
        H5O_loc_t tmp_oloc;             /* Temporary object location */

        /* Build temporary object location */
        tmp_oloc.file = f;

        /* Reduce the link count for all entries in this node */
        for(idx = 0; idx < sn->nsyms; idx++) {
            if(!(H5G_CACHED_SLINK == sn->entry[idx].type)) {
                /* Decrement the reference count */
                HDassert(H5F_addr_defined(sn->entry[idx].header));
                tmp_oloc.addr = sn->entry[idx].header;

                if(H5O_link(&tmp_oloc, -1, dxpl_id) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTDELETE, H5B_INS_ERROR, "unable to decrement object link count")
            } /* end if */
        } /* end for */

        /*
         * We are about to remove all the symbols in this node. Copy the left
         * key to the right key and mark the right key as dirty.  Free this
         * node and indicate that the pointer to this node in the B-tree
         * should be removed also.
         */
        *rt_key = *lt_key;
        *rt_key_changed = TRUE;
        sn->nsyms = 0;
        if(H5MF_xfree(f, H5FD_MEM_BTREE, dxpl_id, addr, (hsize_t)H5G_node_size_real(f)) < 0
                || H5AC_unprotect(f, dxpl_id, H5AC_SNODE, addr, sn, H5AC__DIRTIED_FLAG | H5C__DELETED_FLAG) < 0) {
            sn = NULL;
            HGOTO_ERROR(H5E_SYM, H5E_PROTECT, H5B_INS_ERROR, "unable to free symbol table node")
        } /* end if */
        sn = NULL;
        ret_value = H5B_INS_REMOVE;
    } /* end else */

done:
    if(sn && H5AC_unprotect(f, dxpl_id, H5AC_SNODE, addr, sn, sn_flags) < 0)
	HDONE_ERROR(H5E_SYM, H5E_CANTUNPROTECT, H5B_INS_ERROR, "unable to release symbol table node")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_node_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5G_node_iterate
 *
 * Purpose:	This function gets called during a group iterate operation.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jun 24 1997
 *
 *-------------------------------------------------------------------------
 */
int
H5G_node_iterate(H5F_t *f, hid_t dxpl_id, const void UNUSED *_lt_key, haddr_t addr,
    const void UNUSED *_rt_key, void *_udata)
{
    H5G_bt_it_it_t	*udata = (H5G_bt_it_it_t *)_udata;
    H5G_node_t		*sn = NULL;
    H5G_entry_t		*ents;                  /* Pointer to entries in this node */
    unsigned		u;                      /* Local index variable */
    int	                ret_value = H5_ITER_CONT;

    FUNC_ENTER_NOAPI(H5G_node_iterate, H5_ITER_ERROR)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(udata && udata->heap);

    /* Protect the symbol table node & local heap while we iterate over entries */
    if(NULL == (sn = H5AC_protect(f, dxpl_id, H5AC_SNODE, addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_SYM, H5E_CANTLOAD, H5_ITER_ERROR, "unable to load symbol table node")

    /*
     * Iterate over the symbol table node entries.
     */
    for(u = 0, ents = sn->entry; u < sn->nsyms && ret_value == H5_ITER_CONT; u++) {
        if(udata->skip > 0)
            --udata->skip;
        else {
            H5O_link_t lnk;     /* Link for entry */
            const char *name;   /* Pointer to link name in heap */

            /* Get the pointer to the name of the link in the heap */
            name = H5HL_offset_into(udata->heap, ents[u].name_off);
            HDassert(name);

            /* Convert the entry to a link */
            if(H5G_ent_to_link(&lnk, udata->heap, &ents[u], name) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTCONVERT, H5_ITER_ERROR, "unable to convert symbol table entry to link")

            /* Make the callback */
            ret_value = (udata->op)(&lnk, udata->op_data);

            /* Release memory for link object */
            if(H5O_msg_reset(H5O_LINK_ID, &lnk) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, H5_ITER_ERROR, "unable to release link message")
        } /* end else */

        /* Increment the number of entries passed through */
        /* (whether we skipped them or not) */
        if(udata->final_ent)
            (*udata->final_ent)++;
    } /* end for */
    if(ret_value < 0)
        HERROR(H5E_SYM, H5E_CANTNEXT, "iteration operator failed");

done:
    /* Release resources */
    if(sn && H5AC_unprotect(f, dxpl_id, H5AC_SNODE, addr, sn, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_SYM, H5E_PROTECT, H5_ITER_ERROR, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_node_iterate() */


/*-------------------------------------------------------------------------
 * Function:	H5G_node_sumup
 *
 * Purpose:	This function gets called during a group iterate operation
 *              to return total number of members in the group.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Raymond Lu
 *              Nov 20, 2002
 *
 *-------------------------------------------------------------------------
 */
int
H5G_node_sumup(H5F_t *f, hid_t dxpl_id, const void UNUSED *_lt_key, haddr_t addr,
		  const void UNUSED *_rt_key, void *_udata)
{
    hsize_t	        *num_objs = (hsize_t *)_udata;
    H5G_node_t		*sn = NULL;
    int                  ret_value = H5_ITER_CONT;

    FUNC_ENTER_NOAPI(H5G_node_sumup, H5_ITER_ERROR)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(num_objs);

    /* Find the object node and add the number of symbol entries. */
    if(NULL == (sn = H5AC_protect(f, dxpl_id, H5AC_SNODE, addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_SYM, H5E_CANTLOAD, H5_ITER_ERROR, "unable to load symbol table node")

    *num_objs += sn->nsyms;

done:
    if(sn && H5AC_unprotect(f, dxpl_id, H5AC_SNODE, addr, sn, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_SYM, H5E_PROTECT, H5_ITER_ERROR, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_node_sumup() */


/*-------------------------------------------------------------------------
 * Function:	H5G_node_name
 *
 * Purpose:	This function gets called during a group iterate operation
 *              to return object name by giving idx.
 *
 * Return:	0 if object isn't found in this node; 1 if object is found;
 *              Negative on failure
 *
 * Programmer:  Raymond Lu
 *              Nov 20, 2002
 *
 *-------------------------------------------------------------------------
 */
int
H5G_node_by_idx(H5F_t *f, hid_t dxpl_id, const void UNUSED *_lt_key, haddr_t addr,
		  const void UNUSED *_rt_key, void *_udata)
{
    H5G_bt_it_idx_common_t	*udata = (H5G_bt_it_idx_common_t *)_udata;
    H5G_node_t		*sn = NULL;
    int                 ret_value = H5_ITER_CONT;

    FUNC_ENTER_NOAPI(H5G_node_by_idx, H5_ITER_ERROR)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(udata);

    /* Get a pointer to the symbol table node */
    if(NULL == (sn = H5AC_protect(f, dxpl_id, H5AC_SNODE, addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_SYM, H5E_CANTLOAD, H5_ITER_ERROR, "unable to load symbol table node");

    /* Find the node, locate the object symbol table entry and retrieve the name */
    if(udata->idx >= udata->num_objs && udata->idx < (udata->num_objs + sn->nsyms)) {
        hsize_t ent_idx;                /* Entry index in this node */

        /* Compute index of entry */
        ent_idx = udata->idx - udata->num_objs;

        /* Call 'by index' callback */
        HDassert(udata->op);
        if((udata->op)(&sn->entry[ent_idx], udata) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, H5B_INS_ERROR, "'by index' callback failed")

        /* Indicate that we found the entry we are interested in */
        ret_value = H5_ITER_STOP;
    } /* end if */
    else
        udata->num_objs += sn->nsyms;

done:
    if(sn && H5AC_unprotect(f, dxpl_id, H5AC_SNODE, addr, sn, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_SYM, H5E_PROTECT, H5_ITER_ERROR, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_node_by_idx() */


/*-------------------------------------------------------------------------
 * Function:	H5G_node_init
 *
 * Purpose:	This function gets called during a file opening to initialize
 *              global information about group B-tree nodes for file.
 *
 * Return:	Non-negative on success
 *              Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Jul  5, 2004
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_node_init(H5F_t *f)
{
    H5B_shared_t *shared;               /* Shared B-tree node info */
    size_t	sizeof_rkey;	        /* Size of raw (disk) key	     */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5G_node_init, FAIL)

    /* Check arguments. */
    HDassert(f);

    /* Set the raw key size */
    sizeof_rkey = H5F_SIZEOF_SIZE(f);	/*name offset */

    /* Allocate & initialize global info for the shared structure */
    if(NULL == (shared = H5B_shared_new(f, H5B_SNODE, sizeof_rkey)))
	HGOTO_ERROR(H5E_BTREE, H5E_NOSPACE, FAIL, "memory allocation failed for shared B-tree info")

    /* Set up the "local" information for this file's groups */
        /* <none> */

    /* Make shared B-tree info reference counted */
    if(NULL == (f->shared->grp_btree_shared = H5RC_create(shared, H5B_shared_free)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't create ref-count wrapper for shared B-tree info")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_node_init() */


/*-------------------------------------------------------------------------
 * Function:	H5G_node_close
 *
 * Purpose:	This function gets called during a file close to shutdown
 *              global information about group B-tree nodes for file.
 *
 * Return:	Non-negative on success
 *              Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Jul  5, 2004
 *
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_node_close(const H5F_t *f)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_node_close)

    /* Check arguments. */
    HDassert(f);

    /* Free the raw B-tree node buffer */
    if(H5F_GRP_BTREE_SHARED(f))
        H5RC_DEC(H5F_GRP_BTREE_SHARED(f));

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5G_node_close */


/*-------------------------------------------------------------------------
 * Function:	H5G_node_copy
 *
 * Purpose:	This function gets called during a group iterate operation
 *              to copy objects of this node into a new location.
 *
 * Return:	0(zero) on success/Negative on failure
 *
 * Programmer:  Peter Cao
 *              Sept 10, 2005
 *
 *-------------------------------------------------------------------------
 */
int
H5G_node_copy(H5F_t *f, hid_t dxpl_id, const void UNUSED *_lt_key, haddr_t addr,
		  const void UNUSED *_rt_key, void *_udata)
{
    H5G_bt_it_cpy_t     *udata = (H5G_bt_it_cpy_t *)_udata;
    const H5O_loc_t     *src_oloc = udata->src_oloc;
    H5O_copy_t          *cpy_info = udata->cpy_info;
    H5HL_t              *heap = NULL;
    H5G_node_t	        *sn = NULL;
    unsigned int        i;                   /* Local index variable */
    int                 ret_value = H5_ITER_CONT;

    FUNC_ENTER_NOAPI(H5G_node_copy, H5_ITER_ERROR)

    /* Check arguments. */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(udata);

    /* load the symbol table into memory from the source file */
    if(NULL == (sn = H5AC_protect(f, dxpl_id, H5AC_SNODE, addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_SYM, H5E_CANTLOAD, H5_ITER_ERROR, "unable to load symbol table node")

    /* get the base address of the heap */
    if(NULL == (heap = H5HL_protect(f, dxpl_id, udata->src_heap_addr, H5AC_READ)))
       HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, H5_ITER_ERROR, "unable to protect symbol name")

    /* copy object in this node one by one */
    for(i = 0; i < sn->nsyms; i++) {
        H5G_entry_t *src_ent = &(sn->entry[i]); /* Convenience variable to refer to current source group entry */
        H5O_link_t  lnk;                /* Link to insert */
        const char  *name;              /* Name of source object */
        H5G_entry_t tmp_src_ent;        /* Temperary copy. Change will not affect the cache */

        /* expand soft link */
        if(H5G_CACHED_SLINK == src_ent->type && cpy_info->expand_soft_link) {
            H5O_info_t  oinfo;          /* Information about object pointed to by soft link */
            H5G_loc_t   grp_loc;        /* Group location holding soft link */
            H5G_name_t  grp_path;       /* Path for group holding soft link */
            char *link_name;            /* Pointer to value of soft link */

            /* Make a temporary copy, so that it will not change the info in the cache */
            HDmemcpy(&tmp_src_ent, src_ent, sizeof(H5G_entry_t));

            /* Set up group location for soft link to start in */
            H5G_name_reset(&grp_path);
            grp_loc.path = &grp_path;
            grp_loc.oloc = (H5O_loc_t *)src_oloc;

            /* Get pointer to link value in local heap */
            link_name = (char *)H5HL_offset_into(heap, tmp_src_ent.cache.slink.lval_offset);

            /* Check if the object pointed by the soft link exists in the source file */
            if(H5G_loc_info(&grp_loc, link_name, FALSE, &oinfo, H5P_DEFAULT, dxpl_id) >= 0) {
                tmp_src_ent.header = oinfo.addr;
                src_ent = &tmp_src_ent;
            } /* end if */
            else
                H5E_clear_stack(NULL); /* discard any errors from a dangling soft link */
        } /* if ((H5G_CACHED_SLINK == src_ent->type)... */

        /* Check if object in source group is a hard link */
        if(H5F_addr_defined(src_ent->header)) {
            H5O_loc_t new_dst_oloc;     /* Copied object location in destination */
            H5O_loc_t tmp_src_oloc;     /* Temporary object location for source object */

            /* Set up copied object location to fill in */
            H5O_loc_reset(&new_dst_oloc);
            new_dst_oloc.file = udata->dst_file;

            /* Build temporary object location for source */
            H5O_loc_reset(&tmp_src_oloc);
            tmp_src_oloc.file = f;
            tmp_src_oloc.addr = src_ent->header;

            /* Copy the shared object from source to destination */
            if(H5O_copy_header_map(&tmp_src_oloc, &new_dst_oloc, dxpl_id, cpy_info, TRUE) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, H5_ITER_ERROR, "unable to copy object")

            /* Construct link information for eventual insertion */
            lnk.type = H5L_TYPE_HARD;
            lnk.u.hard.addr = new_dst_oloc.addr;
        } /* ( H5F_addr_defined(src_ent->header)) */
        else if(H5G_CACHED_SLINK == src_ent->type) {
            /* it is a soft link */

            /* Construct link information for eventual insertion */
            lnk.type = H5L_TYPE_SOFT;
            lnk.u.soft.name = H5HL_offset_into(heap, src_ent->cache.slink.lval_offset);
        } /* else if */
        else
            HDassert(0 && "Unknown entry type");

        /* Set up common link data */
        lnk.cset = H5F_DEFAULT_CSET;          /* XXX: Allow user to set this */
        lnk.corder = 0;                     /* Creation order is not tracked for old-style links */
        lnk.corder_valid = FALSE;            /* Creation order is not valid */
        /* lnk.name = name; */              /* This will be set in callback */

        /* Determine name of source object */
        name = H5HL_offset_into(heap, src_ent->name_off);
	HDassert(name);

        /* Insert the new object in the destination file's group */
        /* (Don't increment the link count - that's already done above for hard links) */
        if(H5G_stab_insert_real(udata->dst_file, udata->dst_stab, name, &lnk, dxpl_id) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, H5_ITER_ERROR, "unable to insert the name")
    } /* end of for (i=0; i<sn->nsyms; i++) */

done:
    if(heap && H5HL_unprotect(heap) < 0)
        HDONE_ERROR(H5E_SYM, H5E_PROTECT, H5_ITER_ERROR, "unable to unprotect symbol name")

    if(sn && H5AC_unprotect(f, dxpl_id, H5AC_SNODE, addr, sn, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_SYM, H5E_PROTECT, H5_ITER_ERROR, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_node_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5G_node_build_table
 *
 * Purpose:	B-link tree callback for building table of links
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Nov 19 2006
 *
 *-------------------------------------------------------------------------
 */
int
H5G_node_build_table(H5F_t *f, hid_t dxpl_id, const void UNUSED *_lt_key, haddr_t addr,
    const void UNUSED *_rt_key, void *_udata)
{
    H5G_bt_it_bt_t	*udata = (H5G_bt_it_bt_t *)_udata;
    H5G_node_t		*sn = NULL;             /* Symbol table node */
    unsigned		u;                      /* Local index variable */
    int	                ret_value = H5_ITER_CONT;

    FUNC_ENTER_NOAPI(H5G_node_build_table, H5_ITER_ERROR)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(udata && udata->heap);

    /*
     * Save information about the symbol table node since we can't lock it
     * because we're about to call an application function.
     */
    if(NULL == (sn = H5AC_protect(f, dxpl_id, H5AC_SNODE, addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_SYM, H5E_CANTLOAD, H5_ITER_ERROR, "unable to load symbol table node")

    /* Check if the link table needs to be extended */
    if((udata->ltable->nlinks + sn->nsyms) >= udata->alloc_nlinks) {
        size_t na = MAX((udata->ltable->nlinks + sn->nsyms), (udata->alloc_nlinks * 2));        /* Double # of links allocated */
        H5O_link_t *x;              /* Pointer to larger array of links */

        /* Re-allocate the link table */
        if((x = H5MM_realloc(udata->ltable->lnks, sizeof(H5O_link_t) * na)) == NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, H5_ITER_ERROR, "memory allocation failed")
        udata->ltable->lnks = x;
    } /* end if */

    /* Iterate over the symbol table node entries, adding to link table */
    for(u = 0; u < sn->nsyms; u++) {
        const char	*name;          /* Pointer to link name in heap */
        unsigned        linkno;         /* Link allocated */

        /* Get pointer to link's name in the heap */
        name = H5HL_offset_into(udata->heap, sn->entry[u].name_off);
        HDassert(name);

        /* Determine the link to operate on in the table */
        linkno = udata->ltable->nlinks++;

        /* Convert the entry to a link */
        if(H5G_ent_to_link(&udata->ltable->lnks[linkno], udata->heap, &sn->entry[u], name) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTCONVERT, H5_ITER_ERROR, "unable to convert symbol table entry to link")
    } /* end for */

done:
    /* Release the locked items */
    if(sn && H5AC_unprotect(f, dxpl_id, H5AC_SNODE, addr, sn, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_SYM, H5E_PROTECT, H5_ITER_ERROR, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_node_build_table() */


/*-------------------------------------------------------------------------
 * Function:    H5G_node_iterate_size
 *
 * Purpose:     This function gets called by H5B_iterate_btree_size()
 *              to gather storage info for SNODs.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Vailin Choi
 *              Jun 19 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_node_iterate_size(H5F_t *f, hid_t UNUSED dxpl_id, const void UNUSED *_lt_key, haddr_t UNUSED addr,
    const void UNUSED *_rt_key, void *_udata)
{
    hsize_t     *stab_size = (hsize_t *)_udata;         /* User data */

    FUNC_ENTER_NOAPI_NOFUNC(H5G_node_iterate_size)

    /* Check arguments */
    HDassert(f);
    HDassert(stab_size);

    *stab_size += H5G_node_size_real(f);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5G_btree_node_iterate() */


/*-------------------------------------------------------------------------
 * Function:	H5G_node_debug
 *
 * Purpose:	Prints debugging information about a symbol table node
 *		or a B-tree node for a symbol table B-tree.
 *
 * Return:	0(zero) on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  4 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_node_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr, FILE * stream, int indent,
    int fwidth, haddr_t heap_addr)
{
    H5G_node_t		*sn = NULL;
    H5HL_t              *heap = NULL;
    unsigned		u;                      /* Local index variable */
    herr_t              ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5G_node_debug, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    /* Pin the heap down in memory */
    if(heap_addr > 0 && H5F_addr_defined(heap_addr))
        if(NULL == (heap = H5HL_protect(f, dxpl_id, heap_addr, H5AC_READ)))
            HGOTO_ERROR(H5E_SYM, H5E_CANTLOAD, FAIL, "unable to protect symbol table heap")

    /*
     * If we couldn't load the symbol table node, then try loading the
     * B-tree node.
     */
    if (NULL == (sn = H5AC_protect(f, dxpl_id, H5AC_SNODE, addr, NULL, NULL, H5AC_READ))) {
        H5G_bt_common_t	udata;		/*data to pass through B-tree	*/

        H5E_clear_stack(NULL); /* discard that error */
        udata.heap = heap;
	if(H5B_debug(f, dxpl_id, addr, stream, indent, fwidth, H5B_SNODE, &udata) < 0)
	    HGOTO_ERROR(H5E_SYM, H5E_CANTLOAD, FAIL, "unable to debug B-tree node");
    } /* end if */
    else {
        fprintf(stream, "%*sSymbol Table Node...\n", indent, "");
        fprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
                "Dirty:",
                sn->cache_info.is_dirty ? "Yes" : "No");
        fprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
                "Size of Node (in bytes):", (unsigned)H5G_node_size_real(f));
        fprintf(stream, "%*s%-*s %u of %u\n", indent, "", fwidth,
                "Number of Symbols:",
                sn->nsyms, (unsigned)(2 * H5F_SYM_LEAF_K(f)));

        indent += 3;
        fwidth = MAX(0, fwidth - 3);
        for(u = 0; u < sn->nsyms; u++) {
            fprintf(stream, "%*sSymbol %u:\n", indent - 3, "", u);

            if(heap) {
                const char *s = H5HL_offset_into(heap, sn->entry[u].name_off);

                if(s)
                    fprintf(stream, "%*s%-*s `%s'\n", indent, "", fwidth, "Name:", s);
            } /* end if */
            else
                fprintf(stream, "%*s%-*s\n", indent, "", fwidth, "Warning: Invalid heap address given, name not displayed!");

            H5G_ent_debug(sn->entry + u, stream, indent, fwidth, heap);
        } /* end for */
    } /* end if */

done:
    if(sn && H5AC_unprotect(f, dxpl_id, H5AC_SNODE, addr, sn, H5AC__NO_FLAGS_SET) < 0)
	HDONE_ERROR(H5E_SYM, H5E_PROTECT, FAIL, "unable to release symbol table node")
    if(heap && H5HL_unprotect(heap) < 0)
        HDONE_ERROR(H5E_SYM, H5E_PROTECT, FAIL, "unable to unprotect symbol table heap")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_node_debug() */

