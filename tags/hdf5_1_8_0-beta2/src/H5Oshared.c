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
 * Programmer:	Robb Matzke <matzke@llnl.gov>
 *		Wednesday, April  1, 1998
 *
 * Purpose:	Functions that operate on a shared message.  The shared
 *		message doesn't ever actually appear in the object header as
 *		a normal message.  Instead, if a message is shared, the
 *		H5O_FLAG_SHARED bit is set and the message body is that
 *		defined here for H5O_SHARED.  The message ID is the ID of the
 *		pointed-to message and the pointed-to message is stored in
 *		the global heap.
 */

#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */
#define H5O_PACKAGE		/*suppress error about including H5Opkg	  */


#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"             /* File access				*/
#include "H5FLprivate.h"	/* Free lists                           */
#include "H5Gprivate.h"		/* Groups				*/
#include "H5HFprivate.h"        /* Fractal heap				*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Opkg.h"             /* Object headers			*/
#include "H5SMprivate.h"        /* Shared object header messages        */

/* First version, with full symbol table entry as link for object header sharing */
#define H5O_SHARED_VERSION_1	1

/* Older version, with just address of object as link for object header sharing */
#define H5O_SHARED_VERSION_2	2

/* Newest version, which recognizes messages that are stored in the heap */
#define H5O_SHARED_VERSION_3	3
#define H5O_SHARED_VERSION_LATEST	H5O_SHARED_VERSION_3

/* Size of stack buffer for serialized messages */
#define H5O_MESG_BUF_SIZE               128

/* Declare a free list to manage the serialized message information */
H5FL_BLK_DEFINE(ser_mesg);


/*-------------------------------------------------------------------------
 * Function:	H5O_shared_read
 *
 * Purpose:	Reads a message referred to by a shared message.
 *
 * Return:	Success:	Ptr to message in native format.  The message
 *				should be freed by calling H5O_msg_reset().  If
 *				MESG is a null pointer then the caller should
 *				also call H5MM_xfree() on the return value
 *				after calling H5O_msg_reset().
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Sep 24 2003
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_shared_read(H5F_t *f, hid_t dxpl_id, const H5O_shared_t *shared,
    const H5O_msg_class_t *type)
{
    H5HF_t *fheap = NULL;
    uint8_t mesg_buf[H5O_MESG_BUF_SIZE]; /* Buffer for deserializing messages */
    uint8_t *buf = NULL;        /* Pointer to raw message in heap */
    void *ret_value = NULL;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_shared_read)

    /* check args */
    HDassert(f);
    HDassert(shared);
    HDassert(type);
    HDassert(type->share_flags & H5O_SHARE_IS_SHARABLE);

    /* This message could have a heap ID (SOHM) or the address of an object
     * header on disk (named datatype)
     */
    HDassert(H5O_IS_STORED_SHARED(shared->type));

    /* Check for implicit shared object header message */
    if(shared->type == H5O_SHARE_TYPE_SOHM) {
        haddr_t fheap_addr;
        size_t buf_size;

        /* Retrieve the fractal heap address for shared messages */
        if(H5SM_get_fheap_addr(f, dxpl_id, type->id, &fheap_addr) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, NULL, "can't get fheap address for shared messages")

        /* Open the fractal heap */
        if(NULL == (fheap = H5HF_open(f, dxpl_id, fheap_addr)))
            HGOTO_ERROR(H5E_OHDR, H5E_CANTOPENOBJ, NULL, "unable to open fractal heap")

        /* Get the size of the message in the heap */
        if(H5HF_get_obj_len(fheap, dxpl_id, &(shared->u.heap_id), &buf_size) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, NULL, "can't get message size from fractal heap.")

        /* Allocate space for serialized message, if necessary */
        if(buf_size > sizeof(mesg_buf)) {
            if(NULL == (buf = H5FL_BLK_MALLOC(ser_mesg, buf_size)))
                HGOTO_ERROR(H5E_OHDR, H5E_NOSPACE, NULL, "memory allocation failed")
        } /* end if */
        else
            buf = mesg_buf;

        /* Retrieve the message from the heap */
        if(H5HF_read(fheap, dxpl_id, &(shared->u.heap_id), buf) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "can't read message from fractal heap.")

        /* Decode the message */
        if(NULL == (ret_value = (type->decode)(f, dxpl_id, 0, buf)))
            HGOTO_ERROR(H5E_OHDR, H5E_CANTDECODE, NULL, "can't decode shared message.")
    } /* end if */
    else {
        H5O_loc_t oloc;         /* Location for object header where message is stored */

        HDassert(shared->type == H5O_SHARE_TYPE_COMMITTED);

        /* Get the shared message from an object header */
        oloc.file = f;
        oloc.addr = shared->u.loc.oh_addr;
        oloc.holding_file = FALSE;
        if(NULL == (ret_value = H5O_msg_read(&oloc, type->id, NULL, dxpl_id)))
            HGOTO_ERROR(H5E_OHDR, H5E_READERROR, NULL, "unable to read message")
    } /* end else */

    /* Mark the message as shared */
    if(H5O_msg_set_share(type->id, shared, ret_value) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, NULL, "unable to set sharing information")

done:
    /* Release resources */
    if(buf && buf != mesg_buf)
        buf = H5FL_BLK_FREE(ser_mesg, buf);
    if(fheap && H5HF_close(fheap, dxpl_id) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_CANTFREE, NULL, "can't close fractal heap")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_shared_read() */


/*-------------------------------------------------------------------------
 * Function:	H5O_shared_link_adj
 *
 * Purpose:	Changes the link count for the object referenced by a shared
 *              message.
 *
 *              This function changes the object header link count and is
 *              only relevant for committed messages.  Messages shared in
 *              the heap are re-shared each time they're written, so their
 *              reference count is stored in the file-wide shared message
 *              index and is changed in a different place in the code.
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Sep 26 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_shared_link_adj(H5F_t *f, hid_t dxpl_id, H5O_t *open_oh,
    const H5O_msg_class_t *type, H5O_shared_t *shared, int adjust)
{
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_shared_link_adj)

    /* check args */
    HDassert(f);
    HDassert(shared);

    /* Check for type of shared message */
    if(shared->type == H5O_SHARE_TYPE_COMMITTED) {
        H5O_loc_t oloc;         /* Location for object header where message is stored */

        /*
         * The shared message is stored in some other object header.
         * The other object header must be in the same file as the
         * new object header. Adjust the reference count on that
         * object header.
         */
        if(shared->file->shared != f->shared)
            HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "interfile hard links are not allowed")

        /* Get the shared message from an object header */
        oloc.file = f;
        oloc.addr = shared->u.loc.oh_addr;
        oloc.holding_file = FALSE;

        if(H5O_link(&oloc, adjust, dxpl_id) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_LINKCOUNT, FAIL, "unable to adjust shared object link count")
    } /* end if */
    else {
        HDassert(shared->type == H5O_SHARE_TYPE_SOHM || shared->type == H5O_SHARE_TYPE_HERE);

        /* Check for decrementing reference count on shared message */
        if(adjust < 0) {
            if(H5SM_delete(f, dxpl_id, open_oh, shared) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTDEC, FAIL, "unable to delete message from SOHM table")
        } /* end if */
        /* Check for incrementing reference count on message */
        else if(adjust > 0) {
            if(H5SM_try_share(f, dxpl_id, open_oh, type->id, shared, NULL) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTINC, FAIL, "error trying to share message")
        } /* end if */
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_shared_link_adj() */


/*-------------------------------------------------------------------------
 * Function:	H5O_shared_decode
 *
 * Purpose:	Decodes a shared object message
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Monday, January 22, 2007
 *
 *-------------------------------------------------------------------------
 */
void *
H5O_shared_decode(H5F_t *f, hid_t dxpl_id, const uint8_t *buf, const H5O_msg_class_t *type)
{
    H5O_shared_t sh_mesg;       /* Shared message info */
    unsigned version;           /* Shared message version */
    void *ret_value;            /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_shared_decode)

    /* Check args */
    HDassert(f);
    HDassert(buf);
    HDassert(type);

    /* Version */
    version = *buf++;
    if(version < H5O_SHARED_VERSION_1 || version > H5O_SHARED_VERSION_LATEST)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "bad version number for shared object message")

    /* Get the shared information type
     * Flags are unused before version 3.
     */
    if(version >= H5O_SHARED_VERSION_2)
        sh_mesg.type = *buf++;
    else {
        sh_mesg.type = H5O_SHARE_TYPE_COMMITTED;
        buf++;
    } /* end else */

    /* Skip reserved bytes (for version 1) */
    if(version == H5O_SHARED_VERSION_1)
        buf += 6;

    /* Body */
    if(version == H5O_SHARED_VERSION_1) {
        /* Initialize other location fields */
        sh_mesg.u.loc.index = 0;

        /* Decode stored "symbol table entry" into message location */
        buf += H5F_SIZEOF_SIZE(f);          /* Skip over local heap address */
        H5F_addr_decode(f, &buf, &(sh_mesg.u.loc.oh_addr));
    } /* end if */
    else if (version >= H5O_SHARED_VERSION_2) {
        /* If this message is in the heap, copy a heap ID.
         * Otherwise, it is a named datatype, so copy an H5O_loc_t.
         */
        if(sh_mesg.type == H5O_SHARE_TYPE_SOHM) {
            HDassert(version >= H5O_SHARED_VERSION_3);
            HDmemcpy(&sh_mesg.u.heap_id, buf, sizeof(sh_mesg.u.heap_id));
        } /* end if */
        else {
            /* The H5O_COMMITTED_FLAG should be set if this message
             * is from an older version before the flag existed.
             */
            if(version < H5O_SHARED_VERSION_3)
                sh_mesg.type = H5O_SHARE_TYPE_COMMITTED;

            sh_mesg.u.loc.index = 0;
            H5F_addr_decode(f, &buf, &sh_mesg.u.loc.oh_addr);
        } /* end else */
    } /* end else if */

    /* Set file pointer & message type for all types of shared messages */
    sh_mesg.file = f;
    sh_mesg.msg_type_id = type->id;

    /* Retrieve actual message, through decoded shared message info */
    if(NULL == (ret_value = H5O_shared_read(f, dxpl_id, &sh_mesg, type)))
        HGOTO_ERROR(H5E_OHDR, H5E_READERROR, NULL, "unable to retrieve native message")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_shared_decode() */


/*-------------------------------------------------------------------------
 * Function:	H5O_shared_encode
 *
 * Purpose:	Encodes message _MESG into buffer BUF.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Thursday, April  2, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_shared_encode(const H5F_t *f, uint8_t *buf/*out*/, const H5O_shared_t *sh_mesg)
{
    unsigned    version;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_shared_encode)

    /* Check args */
    HDassert(f);
    HDassert(buf);
    HDassert(sh_mesg);

    /* If this message is shared in the heap, we need to use version 3 of the
     * encoding and encode the SHARED_IN_HEAP flag.
     */
    if(sh_mesg->type == H5O_SHARE_TYPE_SOHM || H5F_USE_LATEST_FORMAT(f))
        version = H5O_SHARED_VERSION_LATEST;
    else {
        HDassert(sh_mesg->type == H5O_SHARE_TYPE_COMMITTED);
        version = H5O_SHARED_VERSION_2; /* version 1 is no longer used */
    } /* end else */

    *buf++ = version;
    *buf++ = (unsigned)sh_mesg->type;

    /* Encode either the heap ID of the message or the address of the
     * object header that holds it.
     */
    if(sh_mesg->type == H5O_SHARE_TYPE_SOHM)
        HDmemcpy(buf, &(sh_mesg->u.heap_id), sizeof(sh_mesg->u.heap_id));
    else
        H5F_addr_encode(f, &buf, sh_mesg->u.loc.oh_addr);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_shared_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5O_set_shared
 *
 * Purpose:	Sets the shared component for a message.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Sep 26 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_set_shared(H5O_shared_t *dst, const H5O_shared_t *src)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_set_shared)

    /* check args */
    HDassert(dst);
    HDassert(src);

    /* copy */
    *dst = *src;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_set_shared() */


/*-------------------------------------------------------------------------
 * Function:	H5O_shared_size
 *
 * Purpose:	Returns the length of a shared object message.
 *
 * Return:	Success:	Length
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *              Thursday, April  2, 1998
 *
 *-------------------------------------------------------------------------
 */
size_t
H5O_shared_size(const H5F_t *f, const H5O_shared_t *sh_mesg)
{
    size_t ret_value;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_shared_size)

    if(sh_mesg->type == H5O_SHARE_TYPE_COMMITTED) {
        ret_value = 1 +			/*version			*/
            1 +				/*the type field		*/
            H5F_SIZEOF_ADDR(f);		/*sharing by another obj hdr	*/
    } /* end if */
    else {
        HDassert(sh_mesg->type == H5O_SHARE_TYPE_SOHM);
        ret_value = 1 +			/*version			*/
            1 +				/*the type field		*/
            H5O_FHEAP_ID_LEN;		/* Shared in the heap		*/
    } /* end else */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_shared_size() */


/*-------------------------------------------------------------------------
 * Function:    H5O_shared_delete
 *
 * Purpose:     Free file space referenced by message
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Friday, September 26, 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_shared_delete(H5F_t *f, hid_t dxpl_id, H5O_t *open_oh,
    const H5O_msg_class_t *type, H5O_shared_t *sh_mesg)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_shared_delete)

    /* check args */
    HDassert(f);
    HDassert(sh_mesg);

    /* 
     * Committed datatypes increment the OH of the original message when they
     * are written (in H5O_shared_link) and decrement it here.
     * SOHMs in the heap behave differently; their refcount is incremented
     * during H5SM_share when they are going to be written (in H5O_msg_append
     * or H5O_msg_write). Their refcount in the SOHM indexes still needs to
     * be decremented when they're deleted (in H5O_shared_link_adj).
     */

    /* Decrement the reference count on the shared object */
    if(H5O_shared_link_adj(f, dxpl_id, open_oh, type, sh_mesg, -1) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_LINKCOUNT, FAIL, "unable to adjust shared object link count")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_shared_delete() */


/*-------------------------------------------------------------------------
 * Function:    H5O_shared_link
 *
 * Purpose:     Increment reference count on any objects referenced by
 *              message
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Friday, September 26, 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_shared_link(H5F_t *f, hid_t dxpl_id, H5O_t *open_oh,
    const H5O_msg_class_t *type, H5O_shared_t *sh_mesg)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_shared_link)

    /* check args */
    HDassert(f);
    HDassert(sh_mesg);

    /* Increment the reference count on the shared object */
    if(H5O_shared_link_adj(f, dxpl_id, open_oh, type, sh_mesg, 1) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_LINKCOUNT, FAIL, "unable to adjust shared object link count")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_shared_link() */


/*-------------------------------------------------------------------------
 * Function:    H5O_shared_copy_file
 *
 * Purpose:     Copies a message from _MESG to _DEST in file
 *
 * Return:      Success:        Non-negative
 *              Failure:        Negative
 *
 * Programmer:  Quincey Koziol
 *              January 22, 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_shared_copy_file(H5F_t *file_src, H5F_t *file_dst, 
    const H5O_msg_class_t *mesg_type, const void *_native_src, void *_native_dst,
    hbool_t *recompute_size, H5O_copy_t *cpy_info, void UNUSED *udata,
    hid_t dxpl_id)
{
    const H5O_shared_t  *shared_src = (const H5O_shared_t *)_native_src; /* Alias to shared info in native source */
    H5O_shared_t        *shared_dst = (H5O_shared_t *)_native_dst; /* Alias to shared info in native destination message */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_shared_copy_file)

    /* check args */
    HDassert(file_src);
    HDassert(file_dst);
    HDassert(mesg_type);
    HDassert(shared_src);
    HDassert(shared_dst);
    HDassert(recompute_size);
    HDassert(cpy_info);

    /* Committed shared messages create a shared message at the destination
     * and also copy the committed object that they point to.
     *
     * SOHMs try to share the destination message.
     */
    if(shared_src->type == H5O_SHARE_TYPE_COMMITTED) {
        H5O_loc_t dst_oloc;
        H5O_loc_t src_oloc;

        /* Copy the shared object from source to destination */
        dst_oloc.file = file_dst;
        src_oloc.file = shared_src->file;
        src_oloc.addr = shared_src->u.loc.oh_addr;
        if(H5O_copy_header_map(&src_oloc, &dst_oloc, dxpl_id, cpy_info, FALSE) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, FAIL, "unable to copy object")

        /* Set up destination message's shared info */
        H5O_UPDATE_SHARED(shared_dst, H5O_SHARE_TYPE_COMMITTED, file_dst, mesg_type->id, 0, dst_oloc.addr)
    } /* end if */
    else {
        /* Try to share new message in the destination file. */
        /* Message is always shared in heap in dest. file because the dest.
         *      object header doesn't quite exist yet - JML
         */
        if(H5SM_try_share(file_dst, dxpl_id, NULL, mesg_type->id, _native_dst, NULL) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "unable to determine if message should be shared")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O_shared_copy_file() */


/*-------------------------------------------------------------------------
 * Function:    H5O_shared_post_copy_file
 *
 * Purpose:     Delate a shared message and replace with a new one.
 *              The function is needed at cases such as coping a shared reg_ref attribute.
 *              When a shared reg_ref attribute is copied from one file to
 *              another, the values in file need to be replaced. The only way 
 *              to complish that is to delete the old message and write the
 *              new message with the correct values.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Peter Cao
 *              xcao@hdfgroup.org
 *              May 24 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_shared_post_copy_file(H5F_t *f, hid_t dxpl_id, H5O_t *oh, void *mesg)
{
    H5O_shared_t *old_sh_mesg;
    htri_t shared_mesg;                 /* Whether the message should be shared */
    unsigned msg_type_id;               /* Message's type ID */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_shared_post_copy_file)

    /* check args */
    HDassert(f);
    HDassert(mesg);

    /* the old shared message */
    old_sh_mesg = (H5O_shared_t *) mesg;

    /* save the type id for later use */
    msg_type_id = old_sh_mesg->msg_type_id;

    /* Remove the old message from the SOHM storage */
    if(H5SM_delete(f, dxpl_id, oh, old_sh_mesg) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTFREE, FAIL, "unable to find attribute information for object")

    /* Add the new message */
    if((shared_mesg = H5SM_try_share(f, dxpl_id, oh, msg_type_id, mesg, NULL)) == 0)
        HGOTO_ERROR(H5E_OHDR, H5E_BADMESG, FAIL, "message changed sharing status")
    else if(shared_mesg < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_BADMESG, FAIL, "can't share message")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_shared_post_copy_file() */



/*-------------------------------------------------------------------------
 * Function:	H5O_shared_debug
 *
 * Purpose:	Prints debugging info for the message
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Thursday, April  2, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_shared_debug(const H5O_shared_t *mesg, FILE *stream, int indent, int fwidth)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_shared_debug)

    /* Check args */
    HDassert(mesg);
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    switch(mesg->type) {
        case H5O_SHARE_TYPE_UNSHARED:
            HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
                    "Shared Message type:",
                    "Unshared");
            break;

        case H5O_SHARE_TYPE_COMMITTED:
            HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
                    "Shared Message type:",
                    "Obj Hdr");
            HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
                    "Object address:",
                    mesg->u.loc.oh_addr);
            break;

        case H5O_SHARE_TYPE_SOHM:
            HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
                    "Shared Message type:",
                    "SOHM");
            HDfprintf(stream, "%*s%-*s %016llx\n", indent, "", fwidth,
                    "Heap ID:",
                    (unsigned long_long)mesg->u.heap_id);
            break;

        case H5O_SHARE_TYPE_HERE:
            HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
                    "Shared Message type:",
                    "Here");
            break;

        default:
            HDfprintf(stream, "%*s%-*s %s (%u)\n", indent, "", fwidth,
                    "Shared Message type:",
                    "Unknown", (unsigned)mesg->type);
    } /* end switch */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_shared_debug() */

