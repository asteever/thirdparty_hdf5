/*-------------------------------------------------------------------------
 * Copyright (C) 1997-2001 National Center for Supercomputing Applications
 *			   All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		H5O.c
 *			Aug  5 1997
 *			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		Object header virtual functions.
 *
 * Modifications:	
 *
 *-------------------------------------------------------------------------
 */
#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */

#include "H5private.h"
#include "H5ACprivate.h"
#include "H5Eprivate.h"
#include "H5Fpkg.h"
#include "H5FLprivate.h"	/*Free Lists	  */
#include "H5Iprivate.h"
#include "H5MFprivate.h"
#include "H5MMprivate.h"
#include "H5Oprivate.h"
#include "H5Pprivate.h"

#ifdef H5_HAVE_GETTIMEOFDAY
#include <sys/time.h>
#endif /* H5_HAVE_GETTIMEOFDAY */

#define PABLO_MASK	H5O_mask

/* PRIVATE PROTOTYPES */
static herr_t H5O_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5O_t *oh);
static H5O_t *H5O_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_udata1,
		       void *_udata2);
static unsigned H5O_find_in_ohdr(H5F_t *f, haddr_t addr,
			     const H5O_class_t **type_p, int sequence);
static unsigned H5O_alloc(H5F_t *f, H5O_t *oh, const H5O_class_t *type,
		      size_t size);
static unsigned H5O_alloc_extend_chunk(H5O_t *oh, unsigned chunkno, size_t size);
static unsigned H5O_alloc_new_chunk(H5F_t *f, H5O_t *oh, size_t size);

/* H5O inherits cache-like properties from H5AC */
static const H5AC_class_t H5AC_OHDR[1] = {{
    H5AC_OHDR_ID,
    (H5AC_load_func_t)H5O_load,
    (H5AC_flush_func_t)H5O_flush,
}};

/* Interface initialization */
static int interface_initialize_g = 0;
#define INTERFACE_INIT	H5O_init_interface
static herr_t H5O_init_interface(void);

/* ID to type mapping */
static const H5O_class_t *const message_type_g[] = {
    H5O_NULL,		/*0x0000 Null					*/
    H5O_SDSPACE,	/*0x0001 Simple Dimensionality			*/
    NULL,		/*0x0002 Data space (fiber bundle?)		*/
    H5O_DTYPE,		/*0x0003 Data Type				*/
    H5O_FILL,       	/*0x0004 Old data storage -- fill value         */
    H5O_FILL_NEW,	/*0x0005 New Data storage -- fill value 	*/
    NULL,		/*0x0006 Data storage -- compact object		*/
    H5O_EFL,		/*0x0007 Data storage -- external data files	*/
    H5O_LAYOUT,		/*0x0008 Data Layout				*/
    NULL,		/*0x0009 Not assigned				*/
    NULL,		/*0x000A Not assigned				*/
    H5O_PLINE,		/*0x000B Data storage -- filter pipeline	*/
    H5O_ATTR,		/*0x000C Attribute list				*/
    H5O_NAME,		/*0x000D Object name				*/
    H5O_MTIME,		/*0x000E Object modification date and time	*/
    NULL,		/*0x000F Shared header message			*/
    H5O_CONT,		/*0x0010 Object header continuation		*/
    H5O_STAB,		/*0x0011 Symbol table				*/
    H5O_MTIME_NEW,	/*0x0012 New Object modification date and time  */
};

/*
 * An array of functions indexed by symbol table entry cache type
 * (H5G_type_t) that are called to retrieve constant messages cached in the
 * symbol table entry.
 */
static void *(*H5O_fast_g[H5G_NCACHED]) (const H5G_cache_t *,
					 const H5O_class_t *,
					 void *);

/* Declare a free list to manage the H5O_t struct */
H5FL_DEFINE_STATIC(H5O_t);

/* Declare a PQ free list to manage the H5O_mesg_t array information */
H5FL_ARR_DEFINE_STATIC(H5O_mesg_t,-1);

/* Declare a PQ free list to manage the H5O_chunk_t array information */
H5FL_ARR_DEFINE_STATIC(H5O_chunk_t,-1);

/* Declare a PQ free list to manage the chunk image information */
H5FL_BLK_DEFINE_STATIC(chunk_image);

/* Declare external the free list for time_t's */
H5FL_EXTERN(time_t);


/*-------------------------------------------------------------------------
 * Function:	H5O_init_interface
 *
 * Purpose:	Initialize the H5O interface.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Tuesday, January  6, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_init_interface(void)
{
    FUNC_ENTER_NOINIT(H5O_init_interface);

    /*
     * Initialize functions that decode messages from symbol table entries.
     */
    H5O_fast_g[H5G_CACHED_STAB] = H5O_stab_fast;

    FUNC_LEAVE_NOAPI(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_create
 *
 * Purpose:	Creates a new object header. Allocates space for it and
 *              then calls an initialization function. The object header
 *              is opened for write access and should eventually be
 *              closed by calling H5O_close().
 *
 * Return:	Success:	Non-negative, the ENT argument contains
 *				information about the object header,
 *				including its address.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  5 1997
 *
 * Modifications:
 *
 *      Bill Wendling, 1. November 2002
 *      Separated the create function into two different functions. One
 *      which allocates space and an initialization function which
 *      does the rest of the work (initializes, caches, and opens the
 *      object header).
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_create(H5F_t *f, size_t size_hint, H5G_entry_t *ent/*out*/)
{
    haddr_t	header;
    herr_t      ret_value = SUCCEED;    /* return value */

    FUNC_ENTER_NOAPI(H5O_create, FAIL);

    /* check args */
    assert(f);
    assert(ent);

    size_hint = H5O_ALIGN (MAX (H5O_MIN_SIZE, size_hint));

    /* allocate disk space for header and first chunk */
    if (HADDR_UNDEF == (header = H5MF_alloc(f, H5FD_MEM_OHDR,
                                            (hsize_t)H5O_SIZEOF_HDR(f) + size_hint)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                    "file allocation failed for object header header");

    /* initialize the object header */
    if (H5O_init(f, size_hint, ent, header) != SUCCEED)
	HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to initialize object header");

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_init
 *
 * Purpose:	Initialize a new object header, sets the link count to 0,
 *              and caches the header. The object header is opened for
 *              write access and should eventually be closed by calling
 *              H5O_close().
 *
 * Return:	Success:    SUCCEED, the ENT argument contains
 *                          information about the object header,
 *                          including its address.
 *		Failure:    FAIL
 *
 * Programmer:	Bill Wendling
 *		1, November 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_init(H5F_t *f, size_t size_hint, H5G_entry_t *ent/*out*/, haddr_t header)
{
    H5O_t      *oh = NULL;
    haddr_t     tmp_addr;
    herr_t      ret_value = SUCCEED;    /* return value */

    FUNC_ENTER_NOAPI(H5O_init, FAIL);

    /* check args */
    assert(f);
    assert(ent);

    size_hint = H5O_ALIGN(MAX(H5O_MIN_SIZE, size_hint));
    ent->file = f;
    ent->header = header;

    /* allocate the object header and fill in header fields */
    if (NULL == (oh = H5FL_MALLOC(H5O_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    oh->dirty = TRUE;
    oh->version = H5O_VERSION;
    oh->nlink = 0;

    /* create the chunk list and initialize the first chunk */
    oh->nchunks = 1;
    oh->alloc_nchunks = H5O_NCHUNKS;

    if (NULL == (oh->chunk = H5FL_ARR_MALLOC(H5O_chunk_t, oh->alloc_nchunks)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    tmp_addr = ent->header + (hsize_t)H5O_SIZEOF_HDR(f);
    oh->chunk[0].dirty = TRUE;
    oh->chunk[0].addr = tmp_addr;
    oh->chunk[0].size = size_hint;

    if (NULL == (oh->chunk[0].image = H5FL_BLK_CALLOC(chunk_image, size_hint)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    
    /* create the message list and initialize the first message */
    oh->nmesgs = 1;
    oh->alloc_nmesgs = H5O_NMESGS;

    if (NULL == (oh->mesg = H5FL_ARR_CALLOC(H5O_mesg_t, oh->alloc_nmesgs)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    oh->mesg[0].type = H5O_NULL;
    oh->mesg[0].dirty = TRUE;
    oh->mesg[0].native = NULL;
    oh->mesg[0].raw = oh->chunk[0].image + H5O_SIZEOF_MSGHDR(f);
    oh->mesg[0].raw_size = size_hint - H5O_SIZEOF_MSGHDR(f);
    oh->mesg[0].chunkno = 0;

    /* cache it */
    if (H5AC_set(f, H5AC_OHDR, ent->header, oh) < 0) {
        H5FL_FREE(H5O_t,oh);
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to cache object header");
    }

    /* open it */
    if (H5O_open(ent) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTOPENOBJ, FAIL, "unable to open object header");

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_open
 *
 * Purpose:	Opens an object header which is described by the symbol table
 *		entry OBJ_ENT.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Monday, January	 5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_open(H5G_entry_t *obj_ent)
{
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5O_open, FAIL);

    /* Check args */
    assert(obj_ent);
    assert(obj_ent->file);

#ifdef H5O_DEBUG
    if (H5DEBUG(O))
	HDfprintf(H5DEBUG(O), "> %a\n", obj_ent->header);
#endif

    /* Increment open-lock counters */
    obj_ent->file->nopen_objs++;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_close
 *
 * Purpose:	Closes an object header that was previously open.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Monday, January	 5, 1998
 *
 * Modifications:
 *
 *      Pedro Vicente, <pvn@ncsa.uiuc.edu> 22 Aug 2002
 *      Added `id to name' support.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_close(H5G_entry_t *obj_ent)
{
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5O_close, FAIL);

    /* Check args */
    assert(obj_ent);
    assert(obj_ent->file);
    assert(obj_ent->file->nopen_objs > 0);

    /* Decrement open-lock counters */
    --obj_ent->file->nopen_objs;

#ifdef H5O_DEBUG
    if (H5DEBUG(O)) {
	if (obj_ent->file->closing && 1==obj_ent->file->shared->nrefs) {
	    HDfprintf(H5DEBUG(O), "< %a auto %lu remaining\n",
		      obj_ent->header,
		      (unsigned long)(obj_ent->file->nopen_objs));
	} else {
	    HDfprintf(H5DEBUG(O), "< %a\n", obj_ent->header);
	}
    }
#endif
    
    /*
     * If the file open-lock count has reached zero and the file has a close
     * pending then close the file and remove it from the H5I_FILE_CLOSING ID
     * group.
     */
    if (0==obj_ent->file->nopen_objs && obj_ent->file->closing)
	H5I_dec_ref(obj_ent->file->closing);

				
    /* Free the ID to name buffers */
    H5G_free_ent_name(obj_ent);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_load
 *
 * Purpose:	Loads an object header from disk.
 *
 * Return:	Success:	Pointer to the new object header.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  5 1997
 *
 * Modifications:
 *
 *	Robb Matzke, 1997-08-30
 *	Plugged memory leaks that occur during error handling.
 *
 *	Robb Matzke, 1998-01-07
 *	Able to distinguish between constant and variable messages.
 *
 * 	Robb Matzke, 1999-07-28
 *	The ADDR argument is passed by value.
 *
 *	Quincey Koziol, 2002-7-180
 *	Added dxpl parameter to allow more control over I/O from metadata
 *      cache.
 *-------------------------------------------------------------------------
 */
static H5O_t *
H5O_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void * UNUSED _udata1,
	 void * UNUSED _udata2)
{
    H5O_t	*oh = NULL;
    H5O_t	*ret_value;
    uint8_t	buf[16], *p;
    size_t	mesg_size;
    size_t	hdr_size;
    unsigned	id;
    int	mesgno;
    unsigned	curmesg = 0, nmesgs;
    unsigned	chunkno;
    haddr_t	chunk_addr;
    size_t	chunk_size;
    H5O_cont_t	*cont = NULL;
    uint8_t	flags;

    FUNC_ENTER_NOAPI(H5O_load, NULL);

    /* check args */
    assert(f);
    assert(H5F_addr_defined(addr));
    assert(!_udata1);
    assert(!_udata2);

    /* allocate ohdr and init chunk list */
    if (NULL==(oh = H5FL_CALLOC(H5O_t)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* read fixed-lenth part of object header */
    hdr_size = H5O_SIZEOF_HDR(f);
    assert(hdr_size<=sizeof(buf));
    if (H5F_block_read(f, H5FD_MEM_OHDR, addr, hdr_size, dxpl_id, buf) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_READERROR, NULL, "unable to read object header");
    p = buf;

    /* decode version */
    oh->version = *p++;
    if (H5O_VERSION != oh->version)
	HGOTO_ERROR(H5E_OHDR, H5E_VERSION, NULL, "bad object header version number");
    
    /* reserved */
    p++;
    
    /* decode number of messages */
    UINT16DECODE(p, nmesgs);

    /* decode link count */
    UINT32DECODE(p, oh->nlink);

    /* decode first chunk info */
    chunk_addr = addr + (hsize_t)H5O_SIZEOF_HDR(f);
    UINT32DECODE(p, chunk_size);

    /* build the message array */
    oh->alloc_nmesgs = MAX(H5O_NMESGS, nmesgs);
    if (NULL==(oh->mesg=H5FL_ARR_CALLOC(H5O_mesg_t,oh->alloc_nmesgs)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* read each chunk from disk */
    while (H5F_addr_defined(chunk_addr)) {

	/* increase chunk array size */
	if (oh->nchunks >= oh->alloc_nchunks) {
	    unsigned na = oh->alloc_nchunks + H5O_NCHUNKS;
	    H5O_chunk_t *x = H5FL_ARR_REALLOC (H5O_chunk_t, oh->chunk, na);

	    if (!x)
                HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
	    oh->alloc_nchunks = na;
	    oh->chunk = x;
	}
	
	/* read the chunk raw data */
	chunkno = oh->nchunks++;
	oh->chunk[chunkno].dirty = FALSE;
	oh->chunk[chunkno].addr = chunk_addr;
	oh->chunk[chunkno].size = chunk_size;
	if (NULL==(oh->chunk[chunkno].image = H5FL_BLK_MALLOC(chunk_image,chunk_size)))
	    HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
	if (H5F_block_read(f, H5FD_MEM_OHDR, chunk_addr, chunk_size, dxpl_id,
			   oh->chunk[chunkno].image) < 0)
	    HGOTO_ERROR(H5E_OHDR, H5E_READERROR, NULL, "unable to read object header data");
	
	/* load messages from this chunk */
	for (p = oh->chunk[chunkno].image;
	     p < oh->chunk[chunkno].image + chunk_size;
	     p += mesg_size) {
	    UINT16DECODE(p, id);
	    UINT16DECODE(p, mesg_size);
	    assert (mesg_size==H5O_ALIGN (mesg_size));
	    flags = *p++;
	    p += 3; /*reserved*/

	    if (id >= NELMTS(message_type_g) || NULL == message_type_g[id])
		HGOTO_ERROR(H5E_OHDR, H5E_BADMESG, NULL, "corrupt object header");
	    if (p + mesg_size > oh->chunk[chunkno].image + chunk_size)
		HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, NULL, "corrupt object header");
	    if (H5O_NULL_ID == id && oh->nmesgs > 0 &&
		H5O_NULL_ID == oh->mesg[oh->nmesgs - 1].type->id &&
		oh->mesg[oh->nmesgs - 1].chunkno == chunkno) {
		/* combine adjacent null messages */
		mesgno = oh->nmesgs - 1;
		oh->mesg[mesgno].raw_size += H5O_SIZEOF_MSGHDR(f) + mesg_size;
	    } else {
		/* new message */
		if (oh->nmesgs >= nmesgs)
		    HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "corrupt object header");
		mesgno = oh->nmesgs++;
		oh->mesg[mesgno].type = message_type_g[id];
		oh->mesg[mesgno].dirty = FALSE;
		oh->mesg[mesgno].flags = flags;
		oh->mesg[mesgno].native = NULL;
		oh->mesg[mesgno].raw = p;
		oh->mesg[mesgno].raw_size = mesg_size;
		oh->mesg[mesgno].chunkno = chunkno;
	    }
	}
	assert(p == oh->chunk[chunkno].image + chunk_size);

	/* decode next object header continuation message */
	for (chunk_addr=HADDR_UNDEF;
	     !H5F_addr_defined(chunk_addr) && curmesg < oh->nmesgs;
	     curmesg++) {
	    if (H5O_CONT_ID == oh->mesg[curmesg].type->id) {
		uint8_t *p2 = oh->mesg[curmesg].raw;

		cont = (H5O_CONT->decode) (f, p2, NULL);
		oh->mesg[curmesg].native = cont;
		chunk_addr = cont->addr;
		chunk_size = cont->size;
		cont->chunkno = oh->nchunks;	/*the next chunk to allocate */
	    }
	}
    }

    /* Set return value */
    ret_value = oh;

done:
    if (!ret_value && oh) {
        /*
         * Free resources.
         */
        unsigned u;

        for (u = 0; u < oh->nchunks; u++)
            oh->chunk[u].image = H5FL_BLK_FREE(chunk_image,oh->chunk[u].image);
        oh->chunk = H5FL_ARR_FREE(H5O_chunk_t,oh->chunk);
        oh->mesg = H5FL_ARR_FREE(H5O_mesg_t,oh->mesg);
        H5FL_FREE(H5O_t,oh);
    }

    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_flush
 *
 * Purpose:	Flushes (and destroys) an object header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  5 1997
 *
 * Modifications:
 *
 *	Robb Matzke, 1998-01-07
 *	Handles constant vs non-constant messages.
 *
 *      rky, 1998-08-28
 *	Only p0 writes metadata to disk.
 *
 * 	Robb Matzke, 1999-07-28
 *	The ADDR argument is passed by value.
 *
 *	Quincey Koziol, 2002-7-180
 *	Added dxpl parameter to allow more control over I/O from metadata
 *      cache.
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5O_t *oh)
{
    uint8_t	buf[16], *p;
    int	id;
    unsigned	u;
    H5O_mesg_t *curr_msg;       /* Pointer to current message being operated on */
    H5O_cont_t	*cont = NULL;
    herr_t	(*encode)(H5F_t*, uint8_t*, const void*) = NULL;
    unsigned combine=0;        /* Whether to combine the object header prefix & the first chunk */
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5O_flush, FAIL);

    /* check args */
    assert(f);
    assert(H5F_addr_defined(addr));
    assert(oh);

    /* flush */
    if (oh->dirty) {
	p = buf;

	/* encode version */
	*p++ = oh->version;

	/* reserved */
	*p++ = 0;

	/* encode number of messages */
	UINT16ENCODE(p, oh->nmesgs);

	/* encode link count */
	UINT32ENCODE(p, oh->nlink);

	/* encode body size */
	UINT32ENCODE(p, oh->chunk[0].size);

	/* zero to alignment */
	HDmemset (p, 0, H5O_SIZEOF_HDR(f)-12);

	/* write the object header prefix */

        /* Check if we can combine the object header prefix & the first chunk into one I/O operation */
        if(oh->chunk[0].dirty && (addr+H5O_SIZEOF_HDR(f))==oh->chunk[0].addr) {
            combine=1;
        } /* end if */
        else {
            if (H5F_block_write(f, H5FD_MEM_OHDR, addr, H5O_SIZEOF_HDR(f), 
                        dxpl_id, buf) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "unable to write object header hdr to disk");
        } /* end else */
            
	/* encode messages */
	for (u = 0, curr_msg=&oh->mesg[0]; u < oh->nmesgs; u++,curr_msg++) {
	    if (curr_msg->dirty) {
                p = curr_msg->raw - H5O_SIZEOF_MSGHDR(f);

                id = curr_msg->type->id;
                UINT16ENCODE(p, id);
                assert (curr_msg->raw_size<H5O_MAX_SIZE);
                UINT16ENCODE(p, curr_msg->raw_size);
                *p++ = curr_msg->flags;
                *p++ = 0; /*reserved*/
                *p++ = 0; /*reserved*/
                *p++ = 0; /*reserved*/
                
                if (curr_msg->native) {
                    assert(curr_msg->type->encode);

                    /* allocate file space for chunks that have none yet */
                    if (H5O_CONT_ID == curr_msg->type->id &&
                            !H5F_addr_defined(((H5O_cont_t *)(curr_msg->native))->addr)) {
                        cont = (H5O_cont_t *) (curr_msg->native);
                        assert(cont->chunkno < oh->nchunks);
                        assert(!H5F_addr_defined(oh->chunk[cont->chunkno].addr));
                        cont->size = oh->chunk[cont->chunkno].size;
                        if (HADDR_UNDEF==(cont->addr=H5MF_alloc(f,
                                            H5FD_MEM_OHDR, (hsize_t)cont->size)))
                            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to allocate space for object header data");
                        oh->chunk[cont->chunkno].addr = cont->addr;
                    }
                    
                    /*
                     * Encode the message.  If the message is shared then we
                     * encode a Shared Object message instead of the object
                     * which is being shared.
                     */
                    assert(curr_msg->raw >=
                       oh->chunk[curr_msg->chunkno].image);
                    assert (curr_msg->raw_size ==
                        H5O_ALIGN (curr_msg->raw_size));
                    assert(curr_msg->raw + curr_msg->raw_size <=
                       oh->chunk[curr_msg->chunkno].image +
                       oh->chunk[curr_msg->chunkno].size);
                    if (curr_msg->flags & H5O_FLAG_SHARED) {
                        encode = H5O_SHARED->encode;
                    } else {
                        encode = curr_msg->type->encode;
                    }
                    if ((encode)(f, curr_msg->raw, curr_msg->native)<0)
                        HGOTO_ERROR(H5E_OHDR, H5E_CANTENCODE, FAIL, "unable to encode object header message");
                }
                curr_msg->dirty = FALSE;
                oh->chunk[curr_msg->chunkno].dirty = TRUE;
	    }
	}

	/* write each chunk to disk */
	for (u = 0; u < oh->nchunks; u++) {
	    if (oh->chunk[u].dirty) {
                assert(H5F_addr_defined(oh->chunk[u].addr));
                if(u==0 && combine) {
                    /* Allocate space for the combined prefix and first chunk */
                    if((p=H5FL_BLK_MALLOC(chunk_image,(H5O_SIZEOF_HDR(f)+oh->chunk[u].size)))==NULL)
                        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                    /* Copy in the prefix */
                    HDmemcpy(p,buf,H5O_SIZEOF_HDR(f));

                    /* Copy in the first chunk */
                    HDmemcpy(p+H5O_SIZEOF_HDR(f),oh->chunk[u].image,oh->chunk[u].size);

                    /* Write the combined prefix/chunk out */
                    if (H5F_block_write(f, H5FD_MEM_OHDR, addr,
                                (H5O_SIZEOF_HDR(f)+oh->chunk[u].size),
                                dxpl_id, p) < 0)
                        HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "unable to write object header data to disk");

                    /* Release the memory for the combined prefix/chunk */
                    p = H5FL_BLK_FREE(chunk_image,p);
                } /* end if */
                else {
                    if (H5F_block_write(f, H5FD_MEM_OHDR, oh->chunk[u].addr,
                                (oh->chunk[u].size),
                                dxpl_id, oh->chunk[u].image) < 0)
                        HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "unable to write object header data to disk");
                } /* end else */
                oh->chunk[u].dirty = FALSE;
	    } /* end if */
	} /* end for */
	oh->dirty = FALSE;
    }

    if (destroy) {
	/* destroy chunks */
	for (u = 0; u < oh->nchunks; u++)
	    oh->chunk[u].image = H5FL_BLK_FREE(chunk_image,oh->chunk[u].image);
	oh->chunk = H5FL_ARR_FREE(H5O_chunk_t,oh->chunk);

	/* destroy messages */
	for (u = 0, curr_msg=&oh->mesg[0]; u < oh->nmesgs; u++,curr_msg++) {
	    if (curr_msg->flags & H5O_FLAG_SHARED) {
                H5O_free(H5O_SHARED, curr_msg->native);
	    } else {
                H5O_free(curr_msg->type, curr_msg->native);
	    }
	}
	oh->mesg = H5FL_ARR_FREE(H5O_mesg_t,oh->mesg);

	/* destroy object header */
	H5FL_FREE(H5O_t,oh);
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_reset
 *
 * Purpose:	Some message data structures have internal fields that
 *		need to be freed.  This function does that if appropriate
 *		but doesn't free NATIVE.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug 12 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_reset(const H5O_class_t *type, void *native)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5O_reset, FAIL);

    if (native) {
	if (type->reset) {
	    if ((type->reset) (native) < 0)
		HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "reset method failed");
	} else {
	    HDmemset(native, 0, type->native_size);
	}
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_free
 *
 * Purpose:	Similar to H5O_reset() except it also frees the message
 *		pointer.
 *
 * Return:	Success:	NULL
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Thursday, May 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void *
H5O_free (const H5O_class_t *type, void *mesg)
{
    void * ret_value=NULL;      /* Return value */

    FUNC_ENTER_NOAPI(H5O_free, NULL);
    
    if (mesg) {
        H5O_reset (type, mesg);
        if (NULL!=(type->free))
            (type->free)(mesg);
        else
            H5MM_xfree (mesg);
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_copy
 *
 * Purpose:	Copies a message.  If MESG is is the null pointer then a null
 *		pointer is returned with no error.
 *
 * Return:	Success:	Ptr to the new message
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Thursday, May 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void *
H5O_copy (const H5O_class_t *type, const void *mesg, void *dst)
{
    void	*ret_value = NULL;
    
    FUNC_ENTER_NOAPI(H5O_copy, NULL);

    assert (type);
    assert (type->copy);

    if (mesg) {
	if (NULL==(ret_value=(type->copy)(mesg, dst)))
	    HGOTO_ERROR (H5E_OHDR, H5E_CANTINIT, NULL, "unable to copy object header message");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}



/*-------------------------------------------------------------------------
 * Function:	H5O_link
 *
 * Purpose:	Adjust the link count for an object header by adding
 *		ADJUST to the link count.
 *
 * Return:	Success:	New link count
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  5 1997
 *
 * Modifications:
 *
 * 	Robb Matzke, 1998-08-27
 *	This function can also be used to obtain the current number of links
 *	if zero is passed for ADJUST.  If that's the case then we don't check
 *	for write access on the file.
 *
 *-------------------------------------------------------------------------
 */
int
H5O_link(H5G_entry_t *ent, int adjust)
{
    H5O_t	*oh = NULL;
    int	ret_value = FAIL;

    FUNC_ENTER_NOAPI(H5O_link, FAIL);

    /* check args */
    assert(ent);
    assert(ent->file);
    assert(H5F_addr_defined(ent->header));
    if (adjust!=0 && 0==(ent->file->intent & H5F_ACC_RDWR))
	HGOTO_ERROR (H5E_OHDR, H5E_WRITEERROR, FAIL, "no write intent on file");

    /* get header */
    if (NULL == (oh = H5AC_protect(ent->file, H5AC_OHDR, ent->header,
				   NULL, NULL)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header");

    /* adjust link count */
    if (adjust<0) {
	if (oh->nlink + adjust < 0)
	    HGOTO_ERROR(H5E_OHDR, H5E_LINKCOUNT, FAIL, "link count would be negative");
	oh->nlink += adjust;
	oh->dirty = TRUE;
    } else if (adjust>0) {
	oh->nlink += adjust;
	oh->dirty = TRUE;
    }

    /* Set return value */
    ret_value = oh->nlink;

done:
    if (oh && H5AC_unprotect(ent->file, H5AC_OHDR, ent->header, oh) < 0 && ret_value>=0)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header");

    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_count
 *
 * Purpose:	Counts the number of messages in an object header which are a
 *		certain type.
 *
 * Return:	Success:	Number of messages of specified type.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Tuesday, April 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5O_count (H5G_entry_t *ent, const H5O_class_t *type)
{
    H5O_t	*oh = NULL;
    int	acc;
    unsigned	u;
    int	ret_value;
    
    FUNC_ENTER_NOAPI(H5O_count, FAIL);

    /* Check args */
    assert (ent);
    assert (ent->file);
    assert (H5F_addr_defined(ent->header));
    assert (type);

    /* Load the object header */
    if (NULL==(oh=H5AC_find_f (ent->file, H5AC_OHDR, ent->header, NULL, NULL)))
	HGOTO_ERROR (H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header");

    for (u=acc=0; u<oh->nmesgs; u++) {
	if (oh->mesg[u].type==type)
            acc++;
    }

    /* Set return value */
    ret_value=acc;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_exists
 *
 * Purpose:	Determines if a particular message exists in an object
 *		header without trying to decode the message.
 *
 * Return:	Success:	FALSE if the message does not exist; TRUE if
 *				th message exists.
 *
 *		Failure:	FAIL if the existence of the message could
 *				not be determined due to some error such as
 *				not being able to read the object header.
 *
 * Programmer:	Robb Matzke
 *              Monday, November  2, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5O_exists(H5G_entry_t *ent, const H5O_class_t *type, int sequence)
{
    H5O_t	*oh=NULL;
    unsigned	u;
    htri_t      ret_value;       /* Return value */
    
    FUNC_ENTER_NOAPI(H5O_exists, FAIL);

    assert(ent);
    assert(ent->file);
    assert(type);
    assert(sequence>=0);

    /* Load the object header */
    if (NULL==(oh=H5AC_find_f(ent->file, H5AC_OHDR, ent->header, NULL, NULL)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header");

    /* Scan through the messages looking for the right one */
    for (u=0; u<oh->nmesgs; u++) {
	if (type->id!=oh->mesg[u].type->id)
            continue;
	if (--sequence<0)
            break;
    }

    /* Set return value */
    ret_value=(sequence<0);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_read
 *
 * Purpose:	Reads a message from an object header and returns a pointer
 *		to it.	The caller will usually supply the memory through
 *		MESG and the return value will be MESG.	 But if MESG is
 *		the null pointer, then this function will malloc() memory
 *		to hold the result and return its pointer instead.
 *
 * Return:	Success:	Ptr to message in native format.  The message
 *				should be freed by calling H5O_reset().  If
 *				MESG is a null pointer then the caller should
 *				also call H5MM_xfree() on the return value
 *				after calling H5O_reset().
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  6 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void *
H5O_read(H5G_entry_t *ent, const H5O_class_t *type, int sequence, void *mesg)
{
    H5O_t		*oh = NULL;
    void		*ret_value = NULL;
    int		idx;
    H5G_cache_t		*cache = NULL;
    H5G_type_t		cache_type;

    FUNC_ENTER_NOAPI(H5O_read, NULL);

    /* check args */
    assert(ent);
    assert(ent->file);
    assert(H5F_addr_defined(ent->header));
    assert(type);
    assert(sequence >= 0);

    /* can we get it from the symbol table entry? */
    cache = H5G_ent_cache(ent, &cache_type);
    if (H5O_fast_g[cache_type]) {
	ret_value = (H5O_fast_g[cache_type]) (cache, type, mesg);
	if (ret_value)
	    HGOTO_DONE(ret_value);
	H5E_clear(); /*don't care, try reading from header */
    }

    /* can we get it from the object header? */
    if ((idx = H5O_find_in_ohdr(ent->file, ent->header, &type, sequence)) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, NULL, "unable to find message in object header");

    /* copy the message to the user-supplied buffer */
    if (NULL == (oh = H5AC_protect(ent->file, H5AC_OHDR, ent->header, NULL, NULL)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "unable to load object header");
    if (oh->mesg[idx].flags & H5O_FLAG_SHARED) {
	/*
	 * If the message is shared then then the native pointer points to an
	 * H5O_SHARED message.  We use that information to look up the real
	 * message in the global heap or some other object header.
	 */
	H5O_shared_t *shared;
	void *tmp_buf, *tmp_mesg;

	shared = (H5O_shared_t *)(oh->mesg[idx].native);
	if (shared->in_gh) {
	    if (NULL==(tmp_buf = H5HG_read (ent->file, &(shared->u.gh), NULL)))
		HGOTO_ERROR (H5E_OHDR, H5E_CANTLOAD, NULL, "unable to read shared message from global heap");
	    tmp_mesg = (type->decode)(ent->file, tmp_buf, shared);
	    tmp_buf = H5MM_xfree (tmp_buf);
	    if (!tmp_mesg)
		HGOTO_ERROR (H5E_OHDR, H5E_CANTLOAD, NULL, "unable to decode object header shared message");
	    if (mesg) {
		HDmemcpy (mesg, tmp_mesg, type->native_size);
		H5MM_xfree (tmp_mesg);
	    } else {
		ret_value = tmp_mesg;
	    }
	} else {
	    ret_value = H5O_read (&(shared->u.ent), type, 0, mesg);
	    if (type->set_share &&
                    (type->set_share)(ent->file, ret_value, shared)<0)
		HGOTO_ERROR (H5E_OHDR, H5E_CANTINIT, NULL, "unable to set sharing information");
	}
    } else {
	/*
	 * The message is not shared, but rather exists in the object
	 * header.  The object header caches the native message (along with
	 * the raw message) so we must copy the native message before
	 * returning.
	 */
	if (NULL==(ret_value = (type->copy) (oh->mesg[idx].native, mesg)))
	    HGOTO_ERROR (H5E_OHDR, H5E_CANTINIT, NULL, "unable to copy message to user space");
    }

done:
    if (oh && H5AC_unprotect(ent->file, H5AC_OHDR, ent->header, oh) < 0 && ret_value!=NULL)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, NULL, "unable to release object header");

    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_find_in_ohdr
 *
 * Purpose:	Find a message in the object header without consulting
 *		a symbol table entry.
 *
 * Return:	Success:	Index number of message.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  6 1997
 *
 * Modifications:
 *		Robb Matzke, 1999-07-28
 *		The ADDR argument is passed by value.
 *-------------------------------------------------------------------------
 */
static unsigned
H5O_find_in_ohdr(H5F_t *f, haddr_t addr, const H5O_class_t **type_p,
		 int sequence)
{
    H5O_t		*oh = NULL;
    unsigned		u;
    const H5O_class_t	*type = NULL;
    unsigned		ret_value;

    FUNC_ENTER_NOINIT(H5O_find_in_ohdr);

    /* Check args */
    assert(f);
    assert(H5F_addr_defined(addr));
    assert(type_p);

    /* Load the object header */
    if (NULL == (oh = H5AC_find_f(f, H5AC_OHDR, addr, NULL, NULL)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, UFAIL, "unable to load object header");

    /* Scan through the messages looking for the right one */
    for (u = 0; u < oh->nmesgs; u++) {
	if (*type_p && (*type_p)->id != oh->mesg[u].type->id)
            continue;
	if (--sequence < 0)
            break;
    }
    if (sequence >= 0)
	HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, UFAIL, "unable to find object header message");

    /*
     * Decode the message if necessary.  If the message is shared then decode
     * a shared message, ignoring the message type.
     */
    if (oh->mesg[u].flags & H5O_FLAG_SHARED) {
	type = H5O_SHARED;
    } else {
	type = oh->mesg[u].type;
    }
    if (NULL == oh->mesg[u].native) {
	assert(type->decode);
	oh->mesg[u].native = (type->decode) (f, oh->mesg[u].raw, NULL);
	if (NULL == oh->mesg[u].native)
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTDECODE, UFAIL, "unable to decode message");
    }

    /*
     * Return the message type. If this is a shared message then return the
     * pointed-to type.
     */
    *type_p = oh->mesg[u].type;

    /* Set return value */
    ret_value=u;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_modify
 *
 * Purpose:	Modifies an existing message or creates a new message.
 *		The cache fields in that symbol table entry ENT are *not*
 *		updated, you must do that separately because they often
 *		depend on multiple object header messages.  Besides, we
 *		don't know which messages will be constant and which will
 *		not.
 *
 *		The OVERWRITE argument is either a sequence number of a
 *		message to overwrite (usually zero) or the constant
 *		H5O_NEW_MESG (-1) to indicate that a new message is to
 *		be created.  If the message to overwrite doesn't exist then
 *		it is created (but only if it can be inserted so its sequence
 *		number is OVERWRITE; that is, you can create a message with
 *		the sequence number 5 if there is no message with sequence
 *		number 4).
 *
 *              The UPDATE_TIME argument is a boolean that allows the caller
 *              to skip updating the modification time.  This is useful when
 *              several calls to H5O_modify will be made in a sequence.
 *
 * Return:	Success:	The sequence number of the message that
 *				was modified or created.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  6 1997
 *
 * Modifications:
 *
 *	Robb Matzke, 7 Jan 1998
 *	Handles constant vs non-constant messages.  Once a message is made
 *	constant it can never become non-constant.  Constant messages cannot
 *	be modified.
 *
 *-------------------------------------------------------------------------
 */
int
H5O_modify(H5G_entry_t *ent, const H5O_class_t *type, int overwrite,
   unsigned flags, unsigned update_time, const void *mesg)
{
    H5O_t		*oh=NULL;
    int		        sequence;
    unsigned		idx;            /* Index of message to modify */
    H5O_mesg_t         *idx_msg;        /* Pointer to message to modify */
    int		        ret_value = FAIL;
    size_t		size=0;
    H5O_shared_t	sh_mesg;

    FUNC_ENTER_NOAPI(H5O_modify, FAIL);

    /* check args */
    assert(ent);
    assert(ent->file);
    assert(H5F_addr_defined(ent->header));
    assert(type);
    assert(mesg);
    assert (0==(flags & ~H5O_FLAG_BITS));

    if (0==(ent->file->intent & H5F_ACC_RDWR))
	HGOTO_ERROR (H5E_OHDR, H5E_WRITEERROR, FAIL, "no write intent on file");

    if (NULL == (oh = H5AC_protect(ent->file, H5AC_OHDR, ent->header, NULL, NULL)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header");

    /* Count similar messages */
    for (idx = 0, sequence = -1, idx_msg=&oh->mesg[0]; idx < oh->nmesgs; idx++, idx_msg++) {
	if (type->id != idx_msg->type->id)
            continue;
	if (++sequence == overwrite)
            break;
    }

    /* Was the right message found? */
    if (overwrite >= 0 &&
            (idx >= oh->nmesgs || sequence != overwrite)) {

	/* But can we insert a new one with this sequence number? */
	if (overwrite == sequence + 1) {
	    overwrite = -1;
	} else {
	    HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, FAIL, "message not found");
	}
    }
    if (overwrite < 0) {
	/* Allocate space for the new message */
	if (flags & H5O_FLAG_SHARED) {
	    HDmemset(&sh_mesg,0,sizeof(H5O_shared_t));

	    if (NULL==type->get_share)
		HGOTO_ERROR (H5E_OHDR, H5E_UNSUPPORTED, FAIL, "message class is not sharable");
	    if ((type->get_share)(ent->file, mesg, &sh_mesg/*out*/)<0) {
		/*
		 * If the message isn't shared then turn off the shared bit
		 * and treat it as an unshared message.
		 */
		H5E_clear ();
		flags &= ~H5O_FLAG_SHARED;
	    } else if (sh_mesg.in_gh) {
		/*
		 * The shared message is stored in the global heap.
		 * Increment the reference count on the global heap message.
		 */
		if (H5HG_link (ent->file, &(sh_mesg.u.gh), 1)<0)
		    HGOTO_ERROR (H5E_OHDR, H5E_LINK, FAIL, "unable to adjust shared object link count");
		size = (H5O_SHARED->raw_size)(ent->file, &sh_mesg);
	    } else {
		/*
		 * The shared message is stored in some other object header.
		 * The other object header must be in the same file as the
		 * new object header. Increment the reference count on that
		 * object header.
		 */
		if (sh_mesg.u.ent.file->shared != ent->file->shared)
		    HGOTO_ERROR(H5E_OHDR, H5E_LINK, FAIL, "interfile hard links are not allowed");
		if (H5O_link (&(sh_mesg.u.ent), 1)<0)
		    HGOTO_ERROR (H5E_OHDR, H5E_LINK, FAIL, "unable to adjust shared object link count");
		size = (H5O_SHARED->raw_size)(ent->file, &sh_mesg);
	    }
	}
        /* This can't be an 'else' statement due to the possibility of the shared bit getting turned off above */
	if (0== (flags & H5O_FLAG_SHARED)) {
	    size = (type->raw_size) (ent->file, mesg);
	    if (size>=H5O_MAX_SIZE)
		HGOTO_ERROR (H5E_OHDR, H5E_CANTINIT, FAIL, "object header message is too large (16k max)");
	}
	idx = H5O_alloc(ent->file, oh, type, size);
	if (idx == UFAIL)
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to allocate space for message");
	sequence++;
	    
    } else if (oh->mesg[idx].flags & H5O_FLAG_CONSTANT) {
	HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "unable to modify constant message");
    } else if (oh->mesg[idx].flags & H5O_FLAG_SHARED) {
	HGOTO_ERROR (H5E_OHDR, H5E_WRITEERROR, FAIL, "unable to modify shared (constant) message");
    }
    
    /* Set pointer to the correct message */
    idx_msg=&oh->mesg[idx];

    /* Copy the native value into the object header */
    if (flags & H5O_FLAG_SHARED) {
        if (NULL==(idx_msg->native = H5MM_malloc (sizeof (H5O_shared_t))))
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        HDmemcpy(idx_msg->native,&sh_mesg,sizeof(H5O_shared_t));
    } else {
	if (idx_msg->native)
	    H5O_reset (idx_msg->type, idx_msg->native);
	idx_msg->native = (type->copy) (mesg, idx_msg->native);
	if (NULL == idx_msg->native)
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to copy message to object header");
    }

    /* Update the modification time message if any */
    if(update_time)
        H5O_touch_oh(ent->file, oh, FALSE);
    
    idx_msg->flags = flags;
    idx_msg->dirty = TRUE;
    oh->dirty = TRUE;

    /* Set return value */
    ret_value = sequence;

done:
    if (oh && H5AC_unprotect(ent->file, H5AC_OHDR, ent->header, oh) < 0 && ret_value!=FAIL)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header");
    
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_protect
 *
 * Purpose:	Wrapper around H5AC_protect for use during a H5O_protect->
 *              H5O_append->...->H5O_append->H5O_unprotect sequence of calls
 *              during an object's creation.
 *
 * Return:	Success:	Pointer to the object header structure for the
 *                              object.
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Dec 31 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5O_t *
H5O_protect(H5G_entry_t *ent)
{
    H5O_t	       *ret_value;      /* Return value */

    FUNC_ENTER_NOAPI(H5O_protect, NULL);

    /* check args */
    assert(ent);
    assert(ent->file);
    assert(H5F_addr_defined(ent->header));

    if (0==(ent->file->intent & H5F_ACC_RDWR))
	HGOTO_ERROR (H5E_OHDR, H5E_WRITEERROR, NULL, "no write intent on file");

    if (NULL == (ret_value = H5AC_protect(ent->file, H5AC_OHDR, ent->header, NULL, NULL)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "unable to load object header");

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5O_protect() */


/*-------------------------------------------------------------------------
 * Function:	H5O_unprotect
 *
 * Purpose:	Wrapper around H5AC_unprotect for use during a H5O_protect->
 *              H5O_append->...->H5O_append->H5O_unprotect sequence of calls
 *              during an object's creation.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Dec 31 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_unprotect(H5G_entry_t *ent, H5O_t *oh)
{
    herr_t ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5O_unprotect, FAIL);

    /* check args */
    assert(ent);
    assert(ent->file);
    assert(H5F_addr_defined(ent->header));
    assert(oh);

    if (H5AC_unprotect(ent->file, H5AC_OHDR, ent->header, oh) < 0)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header");

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5O_unprotect() */


/*-------------------------------------------------------------------------
 * Function:	H5O_append
 *
 * Purpose:	Simplified version of H5O_modify, used when creating a new
 *              object header message (usually during object creation)
 *
 *              Modifies an existing message or creates a new message.
 *		The cache fields in that symbol table entry ENT are *not*
 *		updated, you must do that separately because they often
 *		depend on multiple object header messages.  Besides, we
 *		don't know which messages will be constant and which will
 *		not.
 *
 * Return:	Success:	The sequence number of the message that
 *				was created.
 *
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Dec 31 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5O_append(H5F_t *f, H5O_t *oh, const H5O_class_t *type, 
    unsigned flags, const void *mesg)
{
    unsigned		idx;            /* Index of message to modify */
    H5O_mesg_t         *idx_msg;        /* Pointer to message to modify */
    int		        ret_value = FAIL;
    size_t		size=0;
    H5O_shared_t	sh_mesg;

    FUNC_ENTER_NOAPI(H5O_append, FAIL);

    /* check args */
    assert(f);
    assert(oh);
    assert(type);
    assert(0==(flags & ~H5O_FLAG_BITS));
    assert(mesg);

    /* Allocate space for the new message */
    if (flags & H5O_FLAG_SHARED) {
        HDmemset(&sh_mesg,0,sizeof(H5O_shared_t));

        if (NULL==type->get_share)
            HGOTO_ERROR (H5E_OHDR, H5E_UNSUPPORTED, FAIL, "message class is not sharable");
        if ((type->get_share)(f, mesg, &sh_mesg/*out*/)<0) {
            /*
             * If the message isn't shared then turn off the shared bit
             * and treat it as an unshared message.
             */
            H5E_clear ();
            flags &= ~H5O_FLAG_SHARED;
        } else if (sh_mesg.in_gh) {
            /*
             * The shared message is stored in the global heap.
             * Increment the reference count on the global heap message.
             */
            if (H5HG_link (f, &(sh_mesg.u.gh), 1)<0)
                HGOTO_ERROR (H5E_OHDR, H5E_LINK, FAIL, "unable to adjust shared object link count");
            size = (H5O_SHARED->raw_size)(f, &sh_mesg);
        } else {
            /*
             * The shared message is stored in some other object header.
             * The other object header must be in the same file as the
             * new object header. Increment the reference count on that
             * object header.
             */
            if (sh_mesg.u.ent.file->shared != f->shared)
                HGOTO_ERROR(H5E_OHDR, H5E_LINK, FAIL, "interfile hard links are not allowed");
            if (H5O_link (&(sh_mesg.u.ent), 1)<0)
                HGOTO_ERROR (H5E_OHDR, H5E_LINK, FAIL, "unable to adjust shared object link count");
            size = (H5O_SHARED->raw_size)(f, &sh_mesg);
        }
    }
    /* This can't be an 'else' statement due to the possibility of the shared bit getting turned off above */
    if (0== (flags & H5O_FLAG_SHARED)) {
        size = (type->raw_size) (f, mesg);
        if (size>=H5O_MAX_SIZE)
            HGOTO_ERROR (H5E_OHDR, H5E_CANTINIT, FAIL, "object header message is too large (16k max)");
    }
    if ((idx = H5O_alloc(f, oh, type, size)) == UFAIL)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to allocate space for message");
        
    /* Set pointer to the correct message */
    idx_msg=&oh->mesg[idx];

    /* Copy the native value into the object header */
    if (flags & H5O_FLAG_SHARED) {
        if (NULL==(idx_msg->native = H5MM_malloc (sizeof (H5O_shared_t))))
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        HDmemcpy(idx_msg->native,&sh_mesg,sizeof(H5O_shared_t));
    } else {
	if (idx_msg->native)
	    H5O_reset (idx_msg->type, idx_msg->native);
	idx_msg->native = (type->copy) (mesg, idx_msg->native);
	if (NULL == idx_msg->native)
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to copy message to object header");
    }
    
    idx_msg->flags = flags;
    idx_msg->dirty = TRUE;
    oh->dirty = TRUE;

    /* Set return value */
    ret_value = idx;

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5O_append() */


/*-------------------------------------------------------------------------
 * Function:	H5O_touch_oh
 *
 * Purpose:	If FORCE is non-zero then create a modification time message
 *		unless one already exists.  Then update any existing
 *		modification time message with the current time.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Monday, July 27, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_touch_oh(H5F_t *f, H5O_t *oh, hbool_t force)
{
    unsigned	idx;
#ifdef H5_HAVE_GETTIMEOFDAY
    struct timeval now_tv;
#endif /* H5_HAVE_GETTIMEOFDAY */
    time_t	now;
    size_t	size;
    herr_t      ret_value=SUCCEED;       /* Return value */
    
    FUNC_ENTER_NOINIT(H5O_touch_oh);

    assert(oh);

    /* Look for existing message */
    for (idx=0; idx<oh->nmesgs; idx++) {
	if (H5O_MTIME==oh->mesg[idx].type || H5O_MTIME_NEW==oh->mesg[idx].type)
            break;
    }

#ifdef H5_HAVE_GETTIMEOFDAY
    HDgettimeofday(&now_tv,NULL);
    now=now_tv.tv_sec;
#else /* H5_HAVE_GETTIMEOFDAY */
    now = HDtime(NULL);
#endif /* H5_HAVE_GETTIMEOFDAY */

    /* Create a new message */
    if (idx==oh->nmesgs) {
	if (!force)
            HGOTO_DONE(SUCCEED); /*nothing to do*/
	size = (H5O_MTIME_NEW->raw_size)(f, &now);
	if ((idx=H5O_alloc(f, oh, H5O_MTIME_NEW, size))==UFAIL)
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to allocate space for modification time message");
    }

    /* Update the native part */
    if (NULL==oh->mesg[idx].native) {
	if (NULL==(oh->mesg[idx].native = H5FL_MALLOC(time_t)))
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "memory allocation failed for modification time message");
    }
    *((time_t*)(oh->mesg[idx].native)) = now;
    oh->mesg[idx].dirty = TRUE;
    oh->dirty = TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_touch
 *
 * Purpose:	Touch an object by setting the modification time to the
 *		current time and marking the object as dirty.  Unless FORCE
 *		is non-zero, nothing happens if there is no MTIME message in
 *		the object header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Monday, July 27, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_touch(H5G_entry_t *ent, hbool_t force)
{
    H5O_t	*oh = NULL;
    herr_t      ret_value=SUCCEED;       /* Return value */
    
    FUNC_ENTER_NOAPI(H5O_touch, FAIL);

    /* check args */
    assert(ent);
    assert(ent->file);
    assert(H5F_addr_defined(ent->header));
    if (0==(ent->file->intent & H5F_ACC_RDWR))
	HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "no write intent on file");

    /* Get the object header */
    if (NULL==(oh=H5AC_protect(ent->file, H5AC_OHDR, ent->header, NULL, NULL)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header");

    /* Create/Update the modification time message */
    if (H5O_touch_oh(ent->file, oh, force)<0)
	HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to update object modificaton time");

done:
    if (oh && H5AC_unprotect(ent->file, H5AC_OHDR, ent->header, oh)<0 && ret_value>=0)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header");

    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_remove
 *
 * Purpose:	Removes the specified message from the object header.
 *		If sequence is H5O_ALL (-1) then all messages of the
 *		specified type are removed.  Removing a message causes
 *		the sequence numbers to change for subsequent messages of
 *		the same type.
 *
 *		No attempt is made to join adjacent free areas of the
 *		object header into a single larger free area.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug 28 1997
 *
 * Modifications:
 *
 *	Robb Matzke, 7 Jan 1998
 *	Does not remove constant messages.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_remove(H5G_entry_t *ent, const H5O_class_t *type, int sequence)
{
    H5O_t		*oh = NULL;
    int		seq, nfailed = 0;
    unsigned		u;
    herr_t      ret_value=SUCCEED;       /* Return value */
    H5O_shared_t	*sh_mesg = NULL;

    FUNC_ENTER_NOAPI(H5O_remove, FAIL);

    /* check args */
    assert(ent);
    assert(ent->file);
    assert(H5F_addr_defined(ent->header));
    assert(type);

    if (0==(ent->file->intent & H5F_ACC_RDWR))
	HGOTO_ERROR (H5E_HEAP, H5E_WRITEERROR, FAIL, "no write intent on file");

    /* load the object header */
    if (NULL == (oh = H5AC_protect(ent->file, H5AC_OHDR, ent->header, NULL, NULL)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header");
    
    for (u = seq = 0; u < oh->nmesgs; u++) {
	if (type->id != oh->mesg[u].type->id)
            continue;
	if (seq++ == sequence || H5O_ALL == sequence) {
	    /*
	     * Keep track of how many times we failed trying to remove constant
	     * messages.
	     */
	    if (oh->mesg[u].flags & H5O_FLAG_CONSTANT) {
		nfailed++;
		continue;
	    }

	    if (oh->mesg[u].flags & H5O_FLAG_SHARED) {
		if (NULL==oh->mesg[u].native) {
		    sh_mesg = (H5O_SHARED->decode)(ent->file, oh->mesg[u].raw,
						   NULL);
		    if (NULL==(oh->mesg[u].native = sh_mesg))
			HGOTO_ERROR (H5E_OHDR, H5E_CANTDECODE, FAIL, "unable to decode shared message info");
		}
		if (sh_mesg->in_gh) {
		    if (H5HG_link (ent->file, &(sh_mesg->u.gh), -1)<0)
			HGOTO_ERROR (H5E_OHDR, H5E_LINK, FAIL, "unable to decrement link count on shared message");
		} else {
		    if (H5O_link (&(sh_mesg->u.ent), -1)<0)
			HGOTO_ERROR (H5E_OHDR, H5E_LINK, FAIL, "unable to decrement link count on shared message");
		}
	    }

	    /* change message type to nil and zero it */
	    oh->mesg[u].type = H5O_NULL;
	    HDmemset(oh->mesg[u].raw, 0, oh->mesg[u].raw_size);
	    oh->mesg[u].native = H5O_free (type, oh->mesg[u].native);
	    oh->mesg[u].dirty = TRUE;
	    oh->dirty = TRUE;
	    H5O_touch_oh(ent->file, oh, FALSE);
	}
    }

    /* Fail if we tried to remove any constant messages */
    if (nfailed)
	HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to remove constant message(s)");

done:
    if (oh && H5AC_unprotect(ent->file, H5AC_OHDR, ent->header, oh) < 0 && ret_value>=0)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header");

    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_alloc_extend_chunk
 *
 * Purpose:	Extends a chunk which hasn't been allocated on disk yet
 *		to make the chunk large enough to contain a message whose
 *		data size is exactly SIZE bytes (SIZE need not be aligned).
 *
 *		If the last message of the chunk is the null message, then
 *		that message will be extended with the chunk.  Otherwise a
 *		new null message is created.
 *
 * Return:	Success:	Message index for null message which
 *				is large enough to hold SIZE bytes.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  7 1997
 *
 * Modifications:
 *		Robb Matzke, 1999-08-26
 *		If new memory is allocated as a multiple of some alignment
 *		then we're careful to initialize the part of the new memory
 *		from the end of the expected message to the end of the new
 *		memory.
 *-------------------------------------------------------------------------
 */
static unsigned
H5O_alloc_extend_chunk(H5O_t *oh, unsigned chunkno, size_t size)
{
    unsigned	u;
    unsigned	idx;
    size_t	delta, old_size;
    size_t	aligned_size = H5O_ALIGN(size);
    uint8_t	*old_addr;
    unsigned	ret_value;

    FUNC_ENTER_NOINIT(H5O_alloc_extend_chunk);

    /* check args */
    assert(oh);
    assert(chunkno < oh->nchunks);
    assert(size > 0);

    if (H5F_addr_defined(oh->chunk[chunkno].addr))
	HGOTO_ERROR(H5E_OHDR, H5E_NOSPACE, UFAIL, "chunk is on disk");

    /* try to extend a null message */
    for (idx=0; idx<oh->nmesgs; idx++) {
	if (H5O_NULL_ID == oh->mesg[idx].type->id &&
	    (oh->mesg[idx].raw + oh->mesg[idx].raw_size ==
	     oh->chunk[chunkno].image + oh->chunk[chunkno].size)) {

	    delta = MAX (H5O_MIN_SIZE, aligned_size - oh->mesg[idx].raw_size);
	    assert (delta=H5O_ALIGN (delta));
	    oh->mesg[idx].dirty = TRUE;
	    oh->mesg[idx].raw_size += delta;

	    old_addr = oh->chunk[chunkno].image;

	    /* Be careful not to indroduce garbage */
	    oh->chunk[chunkno].image = H5FL_BLK_REALLOC(chunk_image,old_addr,
						    (oh->chunk[chunkno].size + delta));
	    if (NULL==oh->chunk[chunkno].image)
		HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, UFAIL, "memory allocation failed");
	    HDmemset(oh->chunk[chunkno].image + oh->chunk[chunkno].size,
		     0, delta);
	    oh->chunk[chunkno].size += delta;

	    /* adjust raw addresses for messages of this chunk */
	    if (old_addr != oh->chunk[chunkno].image) {
		for (u = 0; u < oh->nmesgs; u++) {
		    if (oh->mesg[u].chunkno == chunkno)
			oh->mesg[u].raw = oh->chunk[chunkno].image +
					  (oh->mesg[u].raw - old_addr);
		}
	    }
	    HGOTO_DONE(idx);
	}
    }

    /* create a new null message */
    if (oh->nmesgs >= oh->alloc_nmesgs) {
        unsigned na = oh->alloc_nmesgs + H5O_NMESGS;
        H5O_mesg_t *x = H5FL_ARR_REALLOC (H5O_mesg_t, oh->mesg, na);

        if (NULL==x)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, UFAIL, "memory allocation failed");
        oh->alloc_nmesgs = na;
        oh->mesg = x;
    }
    delta = MAX(H5O_MIN_SIZE, aligned_size+H5O_SIZEOF_MSGHDR(f));
    delta = H5O_ALIGN(delta);
    idx = oh->nmesgs++;
    oh->mesg[idx].type = H5O_NULL;
    oh->mesg[idx].dirty = TRUE;
    oh->mesg[idx].native = NULL;
    oh->mesg[idx].raw = oh->chunk[chunkno].image +
			oh->chunk[chunkno].size +
			H5O_SIZEOF_MSGHDR(f);
    oh->mesg[idx].raw_size = delta - H5O_SIZEOF_MSGHDR(f);
    oh->mesg[idx].chunkno = chunkno;

    old_addr = oh->chunk[chunkno].image;
    old_size = oh->chunk[chunkno].size;
    oh->chunk[chunkno].size += delta;
    oh->chunk[chunkno].image = H5FL_BLK_REALLOC(chunk_image,old_addr,
					    oh->chunk[chunkno].size);
    if (NULL==oh->chunk[chunkno].image)
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, UFAIL, "memory allocation failed");
    HDmemset(oh->chunk[chunkno].image+old_size, 0,
	     oh->chunk[chunkno].size - old_size);
    
    /* adjust raw addresses for messages of this chunk */
    if (old_addr != oh->chunk[chunkno].image) {
	for (u = 0; u < oh->nmesgs; u++) {
	    if (oh->mesg[u].chunkno == chunkno)
		oh->mesg[u].raw = oh->chunk[chunkno].image +
		    (oh->mesg[u].raw - old_addr);
	}
    }

    /* Set return value */
    ret_value=idx;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_alloc_new_chunk
 *
 * Purpose:	Allocates a new chunk for the object header but doen't
 *		give the new chunk a file address yet.	One of the other
 *		chunks will get an object continuation message.	 If there
 *		isn't room in any other chunk for the object continuation
 *		message, then some message from another chunk is moved into
 *		this chunk to make room.
 *
 * 		SIZE need not be aligned.
 *
 * Return:	Success:	Index number of the null message for the
 *				new chunk.  The null message will be at
 *				least SIZE bytes not counting the message
 *				ID or size fields.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  7 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static unsigned
H5O_alloc_new_chunk(H5F_t *f, H5O_t *oh, size_t size)
{
    size_t	cont_size;		/*continuation message size	*/
    int	found_null = (-1);	/*best fit null message		*/
    int	found_other = (-1);	/*best fit other message	*/
    unsigned	idx;		        /*message number */
    uint8_t	*p = NULL;		/*ptr into new chunk		*/
    H5O_cont_t	*cont = NULL;		/*native continuation message	*/
    int	chunkno;
    unsigned	u;
    unsigned	ret_value;		/*return value	*/

    FUNC_ENTER_NOINIT(H5O_alloc_new_chunk);

    /* check args */
    assert (oh);
    assert (size > 0);
    size = H5O_ALIGN(size);

    /*
     * Find the smallest null message that will hold an object
     * continuation message.  Failing that, find the smallest message
     * that could be moved to make room for the continuation message.
     * Don't ever move continuation message from one chunk to another.
     */
    cont_size = H5O_ALIGN (H5F_SIZEOF_ADDR(f) + H5F_SIZEOF_SIZE(f));
    for (u=0; u<oh->nmesgs; u++) {
	if (H5O_NULL_ID == oh->mesg[u].type->id) {
	    if (cont_size == oh->mesg[u].raw_size) {
		found_null = u;
		break;
	    } else if (oh->mesg[u].raw_size >= cont_size &&
		       (found_null < 0 ||
			(oh->mesg[u].raw_size <
			 oh->mesg[found_null].raw_size))) {
		found_null = u;
	    }
	} else if (H5O_CONT_ID == oh->mesg[u].type->id) {
	    /*don't consider continuation messages */
	} else if (oh->mesg[u].raw_size >= cont_size &&
		   (found_other < 0 ||
		    oh->mesg[u].raw_size < oh->mesg[found_other].raw_size)) {
	    found_other = u;
	}
    }
    assert(found_null >= 0 || found_other >= 0);

    /*
     * If we must move some other message to make room for the null
     * message, then make sure the new chunk has enough room for that
     * other message.
     */
    if (found_null < 0)
	size += H5O_SIZEOF_MSGHDR(f) + oh->mesg[found_other].raw_size;

    /*
     * The total chunk size must include the requested space plus enough
     * for the message header.	This must be at least some minimum and a
     * multiple of the alignment size.
     */
    size = MAX(H5O_MIN_SIZE, size + H5O_SIZEOF_MSGHDR(f));
    assert (size == H5O_ALIGN (size));

    /*
     * Create the new chunk without giving it a file address.
     */
    if (oh->nchunks >= oh->alloc_nchunks) {
        unsigned na = oh->alloc_nchunks + H5O_NCHUNKS;
        H5O_chunk_t *x = H5FL_ARR_REALLOC (H5O_chunk_t, oh->chunk, na);

        if (!x)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, UFAIL, "memory allocation failed");
        oh->alloc_nchunks = na;
        oh->chunk = x;
    }
    chunkno = oh->nchunks++;
    oh->chunk[chunkno].dirty = TRUE;
    oh->chunk[chunkno].addr = HADDR_UNDEF;
    oh->chunk[chunkno].size = size;
    if (NULL==(oh->chunk[chunkno].image = p = H5FL_BLK_CALLOC(chunk_image,size)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, UFAIL, "memory allocation failed");
    
    /*
     * Make sure we have enough space for all possible new messages
     * that could be generated below.
     */
    if (oh->nmesgs + 3 > oh->alloc_nmesgs) {
        int old_alloc=oh->alloc_nmesgs;
        unsigned na = oh->alloc_nmesgs + MAX (H5O_NMESGS, 3);
        H5O_mesg_t *x = H5FL_ARR_REALLOC (H5O_mesg_t, oh->mesg, na);

        if (!x)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, UFAIL, "memory allocation failed");
        oh->alloc_nmesgs = na;
        oh->mesg = x;

        /* Set new object header info to zeros */
        HDmemset(&oh->mesg[old_alloc], 0,
		 (oh->alloc_nmesgs-old_alloc)*sizeof(H5O_mesg_t));
    }

    /*
     * Describe the messages of the new chunk.
     */
    if (found_null < 0) {
	found_null = u = oh->nmesgs++;
	oh->mesg[u].type = H5O_NULL;
	oh->mesg[u].dirty = TRUE;
	oh->mesg[u].native = NULL;
	oh->mesg[u].raw = oh->mesg[found_other].raw;
	oh->mesg[u].raw_size = oh->mesg[found_other].raw_size;
	oh->mesg[u].chunkno = oh->mesg[found_other].chunkno;

	oh->mesg[found_other].dirty = TRUE;
        /* Copy the message to the new location */
        HDmemcpy(p+H5O_SIZEOF_MSGHDR(f),oh->mesg[found_other].raw,oh->mesg[found_other].raw_size);
	oh->mesg[found_other].raw = p + H5O_SIZEOF_MSGHDR(f);
	oh->mesg[found_other].chunkno = chunkno;
	p += H5O_SIZEOF_MSGHDR(f) + oh->mesg[found_other].raw_size;
	size -= H5O_SIZEOF_MSGHDR(f) + oh->mesg[found_other].raw_size;
    }
    idx = oh->nmesgs++;
    oh->mesg[idx].type = H5O_NULL;
    oh->mesg[idx].dirty = TRUE;
    oh->mesg[idx].native = NULL;
    oh->mesg[idx].raw = p + H5O_SIZEOF_MSGHDR(f);
    oh->mesg[idx].raw_size = size - H5O_SIZEOF_MSGHDR(f);
    oh->mesg[idx].chunkno = chunkno;

    /*
     * If the null message that will receive the continuation message
     * is larger than the continuation message, then split it into
     * two null messages.
     */
    if (oh->mesg[found_null].raw_size > cont_size) {
	u = oh->nmesgs++;
	oh->mesg[u].type = H5O_NULL;
	oh->mesg[u].dirty = TRUE;
	oh->mesg[u].native = NULL;
	oh->mesg[u].raw = oh->mesg[found_null].raw +
			  cont_size +
			  H5O_SIZEOF_MSGHDR(f);
	oh->mesg[u].raw_size = oh->mesg[found_null].raw_size -
			       (cont_size + H5O_SIZEOF_MSGHDR(f));
	oh->mesg[u].chunkno = oh->mesg[found_null].chunkno;

	oh->mesg[found_null].dirty = TRUE;
	oh->mesg[found_null].raw_size = cont_size;
    }

    /*
     * Initialize the continuation message.
     */
    oh->mesg[found_null].type = H5O_CONT;
    oh->mesg[found_null].dirty = TRUE;
    if (NULL==(cont = H5MM_calloc(sizeof(H5O_cont_t))))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, UFAIL, "memory allocation failed");
    cont->addr = HADDR_UNDEF;
    cont->size = 0;
    cont->chunkno = chunkno;
    oh->mesg[found_null].native = cont;

    /* Set return value */
    ret_value=idx;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_alloc
 *
 * Purpose:	Allocate enough space in the object header for this message.
 *
 * Return:	Success:	Index of message
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  6 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static unsigned
H5O_alloc(H5F_t *f, H5O_t *oh, const H5O_class_t *type, size_t size)
{
    unsigned	idx;
    H5O_mesg_t *msg;            /* Pointer to newly allocated message */
    size_t	aligned_size = H5O_ALIGN(size);
    unsigned	ret_value;      /* Return value */

    FUNC_ENTER_NOINIT(H5O_alloc);

    /* check args */
    assert (oh);
    assert (type);

    /* look for a null message which is large enough */
    for (idx = 0; idx < oh->nmesgs; idx++) {
	if (H5O_NULL_ID == oh->mesg[idx].type->id &&
                oh->mesg[idx].raw_size >= aligned_size)
	    break;
    }

#ifdef LATER
    /*
     * Perhaps if we join adjacent null messages we could make one
     * large enough... we leave this as an exercise for future
     * programmers :-)	This isn't a high priority because when an
     * object header is read from disk the null messages are combined
     * anyway.
     */
#endif

    /* if we didn't find one, then allocate more header space */
    if (idx >= oh->nmesgs) {
        unsigned	chunkno;

	/*
	 * Look for a chunk which hasn't had disk space allocated yet
	 * since we can just increase the size of that chunk.
	 */
	for (chunkno = 0; chunkno < oh->nchunks; chunkno++) {
	    if ((idx = H5O_alloc_extend_chunk(oh, chunkno, size)) != UFAIL) {
		break;
	    }
	    H5E_clear();
	}

	/*
	 * Create a new chunk
	 */
	if (idx == UFAIL) {
	    if ((idx = H5O_alloc_new_chunk(f, oh, size)) == UFAIL)
		HGOTO_ERROR(H5E_OHDR, H5E_NOSPACE, UFAIL, "unable to create a new object header data chunk");
	}
    }

    /* Set pointer to newly allocated message */
    msg=&oh->mesg[idx];

    /* do we need to split the null message? */
    if (msg->raw_size > aligned_size) {
        H5O_mesg_t *null_msg;       /* Pointer to null message */
        size_t	mesg_size = aligned_size+ H5O_SIZEOF_MSGHDR(f); /* Total size of newly allocated message */

	assert(msg->raw_size - aligned_size >= H5O_SIZEOF_MSGHDR(f));

	if (oh->nmesgs >= oh->alloc_nmesgs) {
	    int old_alloc=oh->alloc_nmesgs;
	    unsigned na = oh->alloc_nmesgs + H5O_NMESGS;
	    H5O_mesg_t *x = H5FL_ARR_REALLOC (H5O_mesg_t, oh->mesg, na);

	    if (!x)
                HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, UFAIL, "memory allocation failed");
	    oh->alloc_nmesgs = na;
	    oh->mesg = x;

	    /* Set new object header info to zeros */
	    HDmemset(&oh->mesg[old_alloc],0,
		     (oh->alloc_nmesgs-old_alloc)*sizeof(H5O_mesg_t));
	}
        null_msg=&oh->mesg[oh->nmesgs++];
	null_msg->type = H5O_NULL;
	null_msg->dirty = TRUE;
	null_msg->native = NULL;
	null_msg->raw = msg->raw + mesg_size;
	null_msg->raw_size = msg->raw_size - mesg_size;
	null_msg->chunkno = msg->chunkno;
	msg->raw_size = aligned_size;
    }

    /* initialize the new message */
    msg->type = type;
    msg->dirty = TRUE;
    msg->native = NULL;

    oh->dirty = TRUE;

    /* Set return value */
    ret_value=idx;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_share
 *
 * Purpose:	Writes a message to the global heap.
 *
 * Return:	Success:	Non-negative, and HOBJ describes the global heap
 *				object.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Thursday, April  2, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_share (H5F_t *f, const H5O_class_t *type, const void *mesg,
	   H5HG_t *hobj/*out*/)
{
    size_t	size;
    void	*buf = NULL;
    herr_t      ret_value=SUCCEED;       /* Return value */
    
    FUNC_ENTER_NOAPI(H5O_share, FAIL);

    /* Check args */
    assert (f);
    assert (type);
    assert (mesg);
    assert (hobj);

    /* Encode the message put it in the global heap */
    if ((size = (type->raw_size)(f, mesg))>0) {
	if (NULL==(buf = H5MM_malloc (size)))
	    HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
	if ((type->encode)(f, buf, mesg)<0)
	    HGOTO_ERROR (H5E_OHDR, H5E_CANTENCODE, FAIL, "unable to encode message");
	if (H5HG_insert (f, size, buf, hobj)<0)
	    HGOTO_ERROR (H5E_OHDR, H5E_CANTINIT, FAIL, "unable to store message in global heap");
    }

done:
    if(buf)
        H5MM_xfree (buf);

    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_debug
 *
 * Purpose:	Prints debugging info about an object header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  6 1997
 *
 * Modifications:
 *		Robb Matzke, 1999-07-28
 *		The ADDR argument is passed by value.
 *-------------------------------------------------------------------------
 */
herr_t
H5O_debug(H5F_t *f, haddr_t addr, FILE *stream, int indent, int fwidth)
{
    H5O_t	*oh = NULL;
    unsigned	i, chunkno;
    size_t	mesg_total = 0, chunk_total = 0;
    int		*sequence;
    haddr_t	tmp_addr;
    herr_t	ret_value = FAIL;
    void	*(*decode)(H5F_t*, const uint8_t*, H5O_shared_t*);
    herr_t      (*debug)(H5F_t*, const void*, FILE*, int, int)=NULL;

    FUNC_ENTER_NOAPI(H5O_debug, FAIL);

    /* check args */
    assert(f);
    assert(H5F_addr_defined(addr));
    assert(stream);
    assert(indent >= 0);
    assert(fwidth >= 0);

    if (NULL == (oh = H5AC_protect(f, H5AC_OHDR, addr, NULL, NULL)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header");

    /* debug */
    HDfprintf(stream, "%*sObject Header...\n", indent, "");

    HDfprintf(stream, "%*s%-*s %d\n", indent, "", fwidth,
	      "Dirty:",
	      (int) (oh->dirty));
    HDfprintf(stream, "%*s%-*s %d\n", indent, "", fwidth,
	      "Version:",
	      (int) (oh->version));
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Header size (in bytes):",
	      (unsigned) H5O_SIZEOF_HDR(f));
    HDfprintf(stream, "%*s%-*s %d\n", indent, "", fwidth,
	      "Number of links:",
	      (int) (oh->nlink));
    HDfprintf(stream, "%*s%-*s %u (%u)\n", indent, "", fwidth,
	      "Number of messages (allocated):",
	      (unsigned) (oh->nmesgs), (unsigned) (oh->alloc_nmesgs));
    HDfprintf(stream, "%*s%-*s %u (%u)\n", indent, "", fwidth,
	      "Number of chunks (allocated):",
	      (unsigned) (oh->nchunks), (unsigned) (oh->alloc_nchunks));

    /* debug each chunk */
    for (i=0, chunk_total=0; i<oh->nchunks; i++) {
	chunk_total += oh->chunk[i].size;
	HDfprintf(stream, "%*sChunk %d...\n", indent, "", i);

	HDfprintf(stream, "%*s%-*s %d\n", indent + 3, "", MAX(0, fwidth - 3),
		  "Dirty:",
		  (int) (oh->chunk[i].dirty));

	HDfprintf(stream, "%*s%-*s %a\n", indent + 3, "", MAX(0, fwidth - 3),
		  "Address:", oh->chunk[i].addr);

	tmp_addr = addr + (hsize_t)H5O_SIZEOF_HDR(f);
	if (0 == i && H5F_addr_ne(oh->chunk[i].addr, tmp_addr))
	    HDfprintf(stream, "*** WRONG ADDRESS!\n");
	HDfprintf(stream, "%*s%-*s %lu\n", indent + 3, "", MAX(0, fwidth - 3),
		  "Size in bytes:",
		  (unsigned long) (oh->chunk[i].size));
    }

    /* debug each message */
    if (NULL==(sequence = H5MM_calloc(NELMTS(message_type_g)*sizeof(int))))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    for (i=0, mesg_total=0; i<oh->nmesgs; i++) {
	mesg_total += H5O_SIZEOF_MSGHDR(f) + oh->mesg[i].raw_size;
	HDfprintf(stream, "%*sMessage %d...\n", indent, "", i);

	/* check for bad message id */
	if (oh->mesg[i].type->id < 0 ||
	    oh->mesg[i].type->id >= (int)NELMTS(message_type_g)) {
	    HDfprintf(stream, "*** BAD MESSAGE ID 0x%04x\n",
		      oh->mesg[i].type->id);
	    continue;
	}

	/* message name and size */
	HDfprintf(stream, "%*s%-*s 0x%04x %s(%d)\n",
		  indent + 3, "", MAX(0, fwidth - 3),
		  "Message ID:",
		  (unsigned) (oh->mesg[i].type->id),
		  oh->mesg[i].type->name,
		  sequence[oh->mesg[i].type->id]++);
	HDfprintf (stream, "%*s%-*s %s\n", indent+3, "", MAX (0, fwidth-3),
		   "Shared message:",
		   (oh->mesg[i].flags & H5O_FLAG_SHARED) ? "Yes" : "No");
	HDfprintf(stream, "%*s%-*s %s\n", indent + 3, "", MAX(0, fwidth - 3),
		  "Constant:",
		  (oh->mesg[i].flags & H5O_FLAG_CONSTANT) ? "Yes" : "No");
	if (oh->mesg[i].flags & ~H5O_FLAG_BITS) {
	    HDfprintf (stream, "%*s%-*s 0x%02x\n", indent+3,"",MAX(0,fwidth-3), 
		       "*** ADDITIONAL UNKNOWN FLAGS --->",
		       oh->mesg[i].flags & ~H5O_FLAG_BITS);
	}
	HDfprintf(stream, "%*s%-*s %lu bytes\n", indent+3, "", MAX(0,fwidth-3),
		  "Raw size in obj header:",
		  (unsigned long) (oh->mesg[i].raw_size));
	HDfprintf(stream, "%*s%-*s %d\n", indent + 3, "", MAX(0, fwidth - 3),
		  "Chunk number:",
		  (int) (oh->mesg[i].chunkno));
	chunkno = oh->mesg[i].chunkno;
	if (chunkno >= oh->nchunks)
	    HDfprintf(stream, "*** BAD CHUNK NUMBER\n");
	
	/* check the size */
	if ((oh->mesg[i].raw + oh->mesg[i].raw_size >
                 oh->chunk[chunkno].image + oh->chunk[chunkno].size) ||
                (oh->mesg[i].raw < oh->chunk[chunkno].image)) {
	    HDfprintf(stream, "*** BAD MESSAGE RAW ADDRESS\n");
	}
	
	/* decode the message */
	if (oh->mesg[i].flags & H5O_FLAG_SHARED) {
	    decode = H5O_SHARED->decode;
	    debug = H5O_SHARED->debug;
	} else {
	    decode = oh->mesg[i].type->decode;
	    debug = oh->mesg[i].type->debug;
	}
	if (NULL==oh->mesg[i].native && oh->mesg[i].type->decode)
	    oh->mesg[i].native = (decode)(f, oh->mesg[i].raw, NULL);
	if (NULL==oh->mesg[i].native)
	    debug = NULL;
	
	/* print the message */
	if (debug)
	    (debug)(f, oh->mesg[i].native, stream, indent+3, MAX(0, fwidth-3));
	else
	    HDfprintf(stream, "%*sNo info for this message.\n", indent + 3, "");

	/* If the message is shared then also print the pointed-to message */
	if (oh->mesg[i].flags & H5O_FLAG_SHARED) {
	    H5O_shared_t *shared = (H5O_shared_t*)(oh->mesg[i].native);
	    void *mesg = NULL;
	    if (shared->in_gh) {
		void *p = H5HG_read (f, oh->mesg[i].native, NULL);
		mesg = (oh->mesg[i].type->decode)(f, p, oh->mesg[i].native);
		H5MM_xfree (p);
	    } else {
		mesg = H5O_read (&(shared->u.ent), oh->mesg[i].type, 0, NULL);
	    }
	    if (oh->mesg[i].type->debug) {
		(oh->mesg[i].type->debug)(f, mesg, stream, indent+3,
					  MAX (0, fwidth-3));
	    }
	    H5O_free (oh->mesg[i].type, mesg);
	}
    }
    sequence = H5MM_xfree(sequence);

    if (mesg_total != chunk_total)
	HDfprintf(stream, "*** TOTAL SIZE DOES NOT MATCH ALLOCATED SIZE!\n");

done:
    if (oh && H5AC_unprotect(f, H5AC_OHDR, addr, oh) < 0 && ret_value>=0)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header");

    FUNC_LEAVE_NOAPI(ret_value);
}
