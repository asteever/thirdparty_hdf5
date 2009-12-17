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
 * Programmer: Robb Matzke <matzke@llnl.gov>
 *             Friday, September 19, 1997
 */
#define H5G_PACKAGE		/*suppress error about including H5Gpkg	  */
#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */


/* Packages needed by this file... */
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"             /* File access				*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5HLprivate.h"	/* Local Heaps				*/

/* Private macros */

/* Private prototypes */

/* Declare extern the PQ free list for the wrapped strings */
H5FL_BLK_EXTERN(str_buf);


/*-------------------------------------------------------------------------
 * Function:    H5G_ent_decode_vec
 *
 * Purpose:     Same as H5G_ent_decode() except it does it for an array of
 *              symbol table entries.
 *
 * Return:      Success:        Non-negative, with *pp pointing to the first byte
 *                              after the last symbol.
 *
 *              Failure:        Negative
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul 18 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_ent_decode_vec(const H5F_t *f, const uint8_t **pp, H5G_entry_t *ent, unsigned n)
{
    unsigned    u;                      /* Local index variable */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5G_ent_decode_vec, FAIL)

    /* check arguments */
    HDassert(f);
    HDassert(pp);
    HDassert(ent);

    /* decode entries */
    for(u = 0; u < n; u++)
        if(H5G_ent_decode(f, pp, ent + u) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_ent_decode_vec() */


/*-------------------------------------------------------------------------
 * Function:    H5G_ent_decode
 *
 * Purpose:     Decodes a symbol table entry pointed to by `*pp'.
 *
 * Return:      Success:        Non-negative with *pp pointing to the first byte
 *                              following the symbol table entry.
 *
 *              Failure:        Negative
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul 18 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_ent_decode(const H5F_t *f, const uint8_t **pp, H5G_entry_t *ent)
{
    const uint8_t	*p_ret = *pp;
    uint32_t		tmp;
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5G_ent_decode, FAIL)

    /* check arguments */
    HDassert(f);
    HDassert(pp);
    HDassert(ent);

    /* Set the entry's file pointer */
    ent->file = f;

    /* decode header */
    H5F_DECODE_LENGTH(f, *pp, ent->name_off);
    H5F_addr_decode(f, pp, &(ent->header));
    UINT32DECODE(*pp, tmp);
    *pp += 4; /*reserved*/
    ent->type = (H5G_cache_type_t)tmp;

    /* decode scratch-pad */
    switch(ent->type) {
        case H5G_NOTHING_CACHED:
            break;

        case H5G_CACHED_STAB:
            HDassert(2 * H5F_SIZEOF_ADDR(f) <= H5G_SIZEOF_SCRATCH);
            H5F_addr_decode(f, pp, &(ent->cache.stab.btree_addr));
            H5F_addr_decode(f, pp, &(ent->cache.stab.heap_addr));
            break;

        case H5G_CACHED_SLINK:
            UINT32DECODE(*pp, ent->cache.slink.lval_offset);
            break;

        default:
            HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "unknown symbol table entry cache type")
    } /* end switch */

    *pp = p_ret + H5G_SIZEOF_ENTRY(f);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_ent_decode() */


/*-------------------------------------------------------------------------
 * Function:    H5G_ent_encode_vec
 *
 * Purpose:     Same as H5G_ent_encode() except it does it for an array of
 *              symbol table entries.
 *
 * Return:      Success:        Non-negative, with *pp pointing to the first byte
 *                              after the last symbol.
 *
 *              Failure:        Negative
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul 18 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_ent_encode_vec(const H5F_t *f, uint8_t **pp, const H5G_entry_t *ent, unsigned n)
{
    unsigned    u;                      /* Local index variable */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5G_ent_encode_vec, FAIL)

    /* check arguments */
    HDassert(f);
    HDassert(pp);
    HDassert(ent);

    /* encode entries */
    for(u = 0; u < n; u++)
        if(H5G_ent_encode(f, pp, ent + u) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "can't encode")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5G_ent_encode_vec() */


/*-------------------------------------------------------------------------
 * Function:    H5G_ent_encode
 *
 * Purpose:     Encodes the specified symbol table entry into the buffer
 *              pointed to by *pp.
 *
 * Return:      Success:        Non-negative, with *pp pointing to the first byte
 *                              after the symbol table entry.
 *
 *              Failure:        Negative
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul 18 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_ent_encode(const H5F_t *f, uint8_t **pp, const H5G_entry_t *ent)
{
    uint8_t	*p_ret = *pp + H5G_SIZEOF_ENTRY(f);
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5G_ent_encode, FAIL)

    /* check arguments */
    HDassert(f);
    HDassert(pp);

    /* Check for actual entry to encode */
    if(ent) {
        /* encode header */
        H5F_ENCODE_LENGTH(f, *pp, ent->name_off);
        H5F_addr_encode(f, pp, ent->header);
        UINT32ENCODE(*pp, ent->type);
	UINT32ENCODE(*pp, 0); /*reserved*/

        /* encode scratch-pad */
        switch(ent->type) {
            case H5G_NOTHING_CACHED:
                break;

            case H5G_CACHED_STAB:
                HDassert(2 * H5F_SIZEOF_ADDR(f) <= H5G_SIZEOF_SCRATCH);
                H5F_addr_encode(f, pp, ent->cache.stab.btree_addr);
                H5F_addr_encode(f, pp, ent->cache.stab.heap_addr);
                break;

            case H5G_CACHED_SLINK:
                UINT32ENCODE(*pp, ent->cache.slink.lval_offset);
                break;

            default:
                HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "unknown symbol table entry cache type")
        } /* end switch */
    } /* end if */
    else {
        H5F_ENCODE_LENGTH(f, *pp, 0);
        H5F_addr_encode(f, pp, HADDR_UNDEF);
        UINT32ENCODE(*pp, H5G_NOTHING_CACHED);
	UINT32ENCODE(*pp, 0); /*reserved*/
    } /* end else */

    /* fill with zero */
    while(*pp < p_ret)
        *(*pp)++ = 0;
    *pp = p_ret;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_ent_encode() */


/*-------------------------------------------------------------------------
 * Function:    H5G_ent_copy
 *
 * Purpose:     Do a deep copy of symbol table entries
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Pedro Vicente
 *              pvn@ncsa.uiuc.edu
 *              ???day, August ??, 2002
 *
 * Notes:       'depth' parameter determines how much of the group entry
 *              structure we want to copy.  The values are:
 *                  H5_COPY_SHALLOW - Copy all the fields from the source
 *                      to the destination, including the user path and
 *                      canonical path. (Destination "takes ownership" of
 *                      user and canonical paths)
 *                  H5_COPY_DEEP - Copy all the fields from the source to
 *                      the destination, deep copying the user and canonical
 *                      paths.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_ent_copy(H5G_entry_t *dst, const H5G_entry_t *src, H5_copy_depth_t depth)
{
    FUNC_ENTER_NOAPI_NOFUNC(H5G_ent_copy)

    /* Check arguments */
    HDassert(src);
    HDassert(dst);
    HDassert(depth == H5_COPY_SHALLOW || depth == H5_COPY_DEEP);

    /* Copy the top level information */
    HDmemcpy(dst, src, sizeof(H5G_entry_t));

    /* Deep copy the names */
    if(depth == H5_COPY_DEEP) {
        /* Nothing currently */
        ;
    } else if(depth == H5_COPY_SHALLOW) {
        /* Discarding 'const' qualifier OK - QAK */
        H5G_ent_reset((H5G_entry_t *)src);
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5G_ent_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5G_ent_reset
 *
 * Purpose:	Reset a symbol table entry to an empty state
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              ?day, August ??, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_ent_reset(H5G_entry_t *ent)
{
    FUNC_ENTER_NOAPI_NOFUNC(H5G_ent_reset)

    /* Check arguments */
    HDassert(ent);

    /* Clear the symbol table entry to an empty state */
    HDmemset(ent, 0, sizeof(H5G_entry_t));
    ent->header = HADDR_UNDEF;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5G_ent_reset() */


/*-------------------------------------------------------------------------
 * Function:    H5G_ent_convert
 *
 * Purpose:     Convert a link to a symbol table entry
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:  Quincey Koziol
 *              koziol@ncsa.uiuc.edu
 *              Sep 20 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_ent_convert(H5F_t *f, hid_t dxpl_id, H5HL_t *heap, const char *name,
    const H5O_link_t *lnk, H5G_entry_t *ent)
{
    size_t	name_offset;            /* Offset of name in heap */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5G_ent_convert, FAIL)

    /* check arguments */
    HDassert(f);
    HDassert(heap);
    HDassert(name);
    HDassert(lnk);

    /* Reset the new entry */
    H5G_ent_reset(ent);

    /*
     * Add the new name to the heap.
     */
    name_offset = H5HL_insert(f, dxpl_id, heap, HDstrlen(name) + 1, name);
    if(0 == name_offset || (size_t)(-1) == name_offset)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, H5B_INS_ERROR, "unable to insert symbol name into heap")
    ent->name_off = name_offset;

    /* Build correct information for symbol table entry based on link type */
    switch(lnk->type) {
        case H5L_TYPE_HARD:
            ent->type = H5G_NOTHING_CACHED;
            ent->header = lnk->u.hard.addr;
            break;

        case H5L_TYPE_SOFT:
            {
                size_t	lnk_offset;		/* Offset to sym-link value	*/

                /* Insert link value into local heap */
                if((size_t)(-1) == (lnk_offset = H5HL_insert(f, dxpl_id, heap,
                        HDstrlen(lnk->u.soft.name) + 1, lnk->u.soft.name)))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to write link value to local heap")

                ent->type = H5G_CACHED_SLINK;
                ent->cache.slink.lval_offset = lnk_offset;
            } /* end case */
            break;

        default:
          HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "unrecognized link type")
    } /* end switch */

    /* Set the file for the entry */
    ent->file = f;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_ent_convert() */


/*-------------------------------------------------------------------------
 * Function:    H5G_ent_debug
 *
 * Purpose:     Prints debugging information about a symbol table entry.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Aug 29 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_ent_debug(const H5G_entry_t *ent, FILE *stream, int indent, int fwidth,
    H5HL_t *heap)
{
    const char		*lval = NULL;
    int nested_indent, nested_fwidth;

    FUNC_ENTER_NOAPI_NOFUNC(H5G_ent_debug)

    /* Calculate the indent & field width values for nested information */
    nested_indent = indent + 3;
    nested_fwidth = MAX(0, fwidth - 3);

    HDfprintf(stream, "%*s%-*s %lu\n", indent, "", fwidth,
	      "Name offset into private heap:",
	      (unsigned long) (ent->name_off));

    HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
	      "Object header address:", ent->header);

    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
	      "Dirty:",
	      ent->dirty ? "Yes" : "No");
    HDfprintf(stream, "%*s%-*s ", indent, "", fwidth,
	      "Cache info type:");
    switch(ent->type) {
        case H5G_NOTHING_CACHED:
            HDfprintf(stream, "Nothing Cached\n");
            break;

        case H5G_CACHED_STAB:
            HDfprintf(stream, "Symbol Table\n");

            HDfprintf(stream, "%*s%-*s\n", indent, "", fwidth,
                      "Cached entry information:");
            HDfprintf(stream, "%*s%-*s %a\n", nested_indent, "", nested_fwidth,
                      "B-tree address:", ent->cache.stab.btree_addr);

            HDfprintf(stream, "%*s%-*s %a\n", nested_indent, "", nested_fwidth,
                      "Heap address:", ent->cache.stab.heap_addr);
            break;

        case H5G_CACHED_SLINK:
            HDfprintf(stream, "Symbolic Link\n");
            HDfprintf(stream, "%*s%-*s\n", indent, "", fwidth,
                      "Cached information:");
            HDfprintf(stream, "%*s%-*s %lu\n", nested_indent, "", nested_fwidth,
                       "Link value offset:",
                       (unsigned long)(ent->cache.slink.lval_offset));
            if(heap) {
                lval = (const char *)H5HL_offset_into(ent->file, heap, ent->cache.slink.lval_offset);
                HDfprintf(stream, "%*s%-*s %s\n", nested_indent, "", nested_fwidth,
                           "Link value:",
                           lval);
            } /* end if */
            else
                HDfprintf(stream, "%*s%-*s\n", nested_indent, "", nested_fwidth, "Warning: Invalid heap address given, name not displayed!");
            break;

        default:
            HDfprintf(stream, "*** Unknown symbol type %d\n", ent->type);
            break;
    } /* end switch */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5G_ent_debug() */

