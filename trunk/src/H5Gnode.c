/*-------------------------------------------------------------------------
 * Copyright (C) 1997	National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		snode.c
 * 			Jun 26 1997
 * 			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		Functions for handling symbol table nodes.  A
 *			symbol table node is a small collection of symbol
 *			table entries.  A B-tree usually points to the
 *			symbol table nodes for any given symbol table.
 *
 * Modifications:
 *
 * 	Robb Matzke, 5 Aug 1997
 *	Added calls to H5E.
 *
 *-------------------------------------------------------------------------
 */
#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "hdf5.h"

/* Packages needed by this file... */
#include "H5private.h"			/*library			*/
#include "H5ACprivate.h"		/*cache				*/
#include "H5Bprivate.h"			/*B-link trees			*/
#include "H5Gprivate.h"			/*me				*/
#include "H5Hprivate.h"			/*heap				*/
#include "H5MFprivate.h"		/*file memory management	*/
#include "H5MMprivate.h"		/*core memory management	*/

#define PABLO_MASK	H5G_node_mask


/* PRIVATE PROTOTYPES */
static herr_t H5G_node_decode_key (hdf5_file_t *f, uint8 *raw, void *_key);
static herr_t H5G_node_encode_key (hdf5_file_t *f, uint8 *raw, void *_key);
static size_t H5G_node_size (hdf5_file_t *f);
static haddr_t H5G_node_new (hdf5_file_t *f, void *_lt_key, void *_udata,
			     void *_rt_key);
static herr_t H5G_node_flush (hdf5_file_t *f, hbool_t destroy, haddr_t addr,
			      H5G_node_t *sym);
static H5G_node_t *H5G_node_load (hdf5_file_t *f, haddr_t addr,
				  const void *_data);
static intn H5G_node_cmp (hdf5_file_t *f, void *_lt_key, void *_udata,
			  void *_rt_key);
static herr_t H5G_node_found (hdf5_file_t *f, haddr_t addr, void *_lt_key,
			      void *_udata, void *_rt_key);
static haddr_t H5G_node_insert (hdf5_file_t *f, haddr_t addr, intn *anchor,
				void *_lt_key, hbool_t *lt_key_changed,
				void *_md_key, void *_udata,
				void *_rt_key, hbool_t *rt_key_changed);
static herr_t H5G_node_list (hdf5_file_t *f, haddr_t addr, void *_udata);
static size_t H5G_node_sizeof_rkey (hdf5_file_t *f);

/* H5G inherits cache-like properties from H5AC */
static const H5AC_class_t H5AC_SNODE[1] = {{
      (void*(*)(hdf5_file_t*,haddr_t,const void*))H5G_node_load,
      (herr_t(*)(hdf5_file_t*,hbool_t,haddr_t,void*))H5G_node_flush,
}};

/* H5G inherits B-tree like properties from H5B */
const H5B_class_t H5B_SNODE[1] = {{
   H5B_SUBTYPE_SNODE,				/*id			*/
   16,						/*k			*/
   sizeof (H5G_node_key_t),			/*sizeof_nkey		*/
   H5G_node_sizeof_rkey,			/*get_sizeof_rkey	*/
   H5G_node_new,				/*new			*/
   H5G_node_cmp,				/*cmp			*/
   H5G_node_found,				/*found			*/
   H5G_node_insert,				/*insert		*/
   H5G_node_list,				/*list			*/
   H5G_node_decode_key,				/*decode		*/
   H5G_node_encode_key,				/*encode		*/
}};

/* Has the interface been initialized? */
static intn interface_initialize_g = FALSE;


/*-------------------------------------------------------------------------
 * Function:	H5G_node_sizeof_rkey
 *
 * Purpose:	Returns the size of a raw B-link tree key for the specified
 *		file.
 *
 * Return:	Success:	Size of the key.
 *
 *		Failure:	never fails
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 14 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5G_node_sizeof_rkey (hdf5_file_t *f)
{
   return H5F_SIZEOF_OFFSET(f);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_decode_key
 *
 * Purpose:	Decodes a raw key into a native key.
 *
 * Return:	Success:	SUCCEED
 *
 * 		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul  8 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_node_decode_key (hdf5_file_t *f, uint8 *raw, void *_key)
{
   H5G_node_key_t	*key = (H5G_node_key_t *)_key;

   FUNC_ENTER (H5G_node_decode_key, NULL, FAIL);

   assert (f);
   assert (raw);
   assert (key);

   H5F_decode_offset (f, raw, key->offset);

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_encode_key
 *
 * Purpose:	Encodes a native key into a raw key.
 *
 * Return:	Success:	SUCCEED
 *
 * 		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul  8 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_node_encode_key (hdf5_file_t *f, uint8 *raw, void *_key)
{
   H5G_node_key_t	*key = (H5G_node_key_t *)_key;

   FUNC_ENTER (H5G_node_encode_key, NULL, FAIL);

   assert (f);
   assert (raw);
   assert (key);

   H5F_encode_offset (f, raw, key->offset);

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_size
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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5G_node_size (hdf5_file_t *f)
{
   return H5G_NODE_SIZEOF_HDR(f) +
      (2*H5G_NODE_K) * H5G_SIZEOF_ENTRY(f);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_new
 *
 * Purpose:	Creates a new empty symbol table.  This function is called
 *		by the B-tree insert function for an empty tree.  It is
 *		also called internally to split a symbol node with
 *		LT_KEY and RT_KEY null pointers.
 *
 * Return:	Success:	Address of symbol table node.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jun 23 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5G_node_new (hdf5_file_t *f, void *_lt_key, void *_udata, void *_rt_key)
{
   H5G_node_key_t	*lt_key = (H5G_node_key_t*)_lt_key;
   H5G_node_key_t	*rt_key = (H5G_node_key_t*)_rt_key;
   H5G_node_t		*sym = NULL;
   size_t		size = 0;
   haddr_t		addr;

   FUNC_ENTER (H5G_node_new, NULL, FAIL);

   /*
    * Check arguments.
    */
   assert (f);

   sym = H5MM_xcalloc (1, sizeof(H5G_node_t));
   size = H5G_node_size (f);
   if ((addr = H5MF_alloc (f, size))<0) {
      H5MM_xfree (sym);
      HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, FAIL);
   }

   sym->dirty = 1;
   sym->entry = H5MM_xcalloc (2 * H5G_NODE_K, sizeof(H5G_entry_t));
   if (H5AC_set (f, H5AC_SNODE, addr, sym)<0) {
      H5MM_xfree (sym->entry);
      H5MM_xfree (sym);
      HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, FAIL);
   }

   /*
    * The left and right symbols in an empty tree are both the
    * empty string stored at offset zero by the H5G functions. This
    * allows the comparison functions to work correctly without knowing
    * that there are no symbols.
    */
   if (lt_key) lt_key->offset = 0;
   if (rt_key) rt_key->offset = 0;

   FUNC_LEAVE (addr);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_flush
 *
 * Purpose:	Flush a symbol table node to disk.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jun 23 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_node_flush (hdf5_file_t *f, hbool_t destroy, haddr_t addr, H5G_node_t *sym)
{
   uint8	*buf=NULL, *p=NULL;
   size_t	size;
   herr_t	status;

   FUNC_ENTER (H5G_node_flush, NULL, FAIL);

   /*
    * Check arguments.
    */
   assert (f);
   assert (addr>=0);
   assert (sym);

   if (sym->dirty) {
      size = H5G_node_size (f);
      buf = p = H5MM_xmalloc (size);

      /* magic number */
      HDmemcpy (p, H5G_NODE_MAGIC, H5G_NODE_SIZEOF_MAGIC);
      p += 4;

      /* version number */
      *p++ = H5G_NODE_VERS;

      /* reserved */
      *p++ = 0;

      /* number of symbols */
      UINT16ENCODE (p, sym->nsyms);

      /* entries */
      H5G_encode_vec (f, &p, sym->entry, sym->nsyms);
      memset (p, 0, size - (p-buf));
      
      status = H5F_block_write (f, addr, size, buf);
      buf = H5MM_xfree (buf);
      if (status<0) HRETURN_ERROR (H5E_SYM, H5E_WRITEERROR, FAIL);
   }

   if (destroy) {
      sym->entry = H5MM_xfree (sym->entry);
      H5MM_xfree (sym);
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_load
 *
 * Purpose:	Loads a symbol table from the file.
 *
 * Return:	Success:	Ptr to the new table.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jun 23 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5G_node_t *
H5G_node_load (hdf5_file_t *f, haddr_t addr, const void *_udata)
{
   H5G_node_t	*sym = NULL;
   size_t	size = 0;
   uint8	*buf = NULL, *p = NULL;

   FUNC_ENTER (H5G_node_load, NULL, NULL);

   /*
    * Check arguments.
    */
   assert (f);
   assert (addr>=0);
   assert (NULL==_udata);

   /*
    * Initialize variables.
    */
   size = H5G_node_size (f);
   buf = p = H5MM_xmalloc (size);
   sym = H5MM_xcalloc (1, sizeof(H5G_node_t));
   sym->entry = H5MM_xcalloc (2*H5G_NODE_K, sizeof(H5G_entry_t));
   
   if (H5F_block_read (f, addr, size, buf)<0) {
      H5MM_xfree (sym->entry);
      H5MM_xfree (sym);
      HRETURN_ERROR (H5E_SYM, H5E_READERROR, NULL);
   }

   /* magic */
   if (HDmemcmp (p, H5G_NODE_MAGIC, H5G_NODE_SIZEOF_MAGIC)) goto error;
   p += 4;

   /* version */
   if (H5G_NODE_VERS!=*p++) goto error;

   /* reserved */
   p++;

   /* number of symbols */
   UINT16DECODE (p, sym->nsyms);

   /* entries */
   if (H5G_decode_vec (f, &p, sym->entry, sym->nsyms)<0) goto error;

   H5MM_xfree (buf);
   FUNC_LEAVE (sym);

error:
   H5MM_xfree (buf);
   H5MM_xfree (sym);
   HRETURN_ERROR (H5E_SYM, H5E_CANTLOAD, NULL);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_cmp
 *
 * Purpose:	Compares two keys from a B-tree node (LEFT and RIGHT)
 *		against another key (not necessarily the same type)
 *		pointed to by UDATA.
 *
 * Return:	Success:	negative if the UDATA key is less than
 *				or equal to the LEFT key.
 *
 * 				positive if the UDATA key is greater
 *				than the RIGHT key.
 *
 * 				zero if the UDATA key falls between
 *				the LEFT key (exclusive) and the
 *				RIGHT key (inclusive).
 *
 *		Failure:	FAIL (same as LT < RT)
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jun 23 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static intn
H5G_node_cmp (hdf5_file_t *f, void *_lt_key, void *_udata, void *_rt_key)
{
   H5G_node_ud1_t	*udata = (H5G_node_ud1_t *)_udata;
   H5G_node_key_t	*lt_key = (H5G_node_key_t *)_lt_key;
   H5G_node_key_t	*rt_key = (H5G_node_key_t *)_rt_key;
   const char		*s;

   FUNC_ENTER (H5G_node_cmp, NULL, FAIL);

   /* left side */
   if (NULL==(s=H5H_peek (f, udata->heap, lt_key->offset))) {
      HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL);
   }
   if (HDstrcmp (udata->name, s)<=0) HRETURN (-1);

   /* right side */
   if (NULL==(s=H5H_peek (f, udata->heap, rt_key->offset))) {
      HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL);
   }
   if (HDstrcmp (udata->name, s)>0) HRETURN(1);

   FUNC_LEAVE (0);
}


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
 * 		If the operation flag in UDATA is H5G_OPER_FIND, then
 *		the entry is copied from the symbol table to the UDATA
 *		entry field.  Otherwise the entry is copied from the
 *		UDATA entry field to the symbol table.
 *
 * Return:	Success:	SUCCEED if found and data returned through the
 *				UDATA pointer.
 *
 *		Failure:	FAIL if not found.
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jun 23 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_node_found (hdf5_file_t *f, haddr_t addr, void *_lt_key, void *_udata,
		void *_rt_key)
{
   H5G_node_ud1_t	*udata = (H5G_node_ud1_t *)_udata;
   H5G_node_t		*sn;
   intn			lt=0, idx=0, rt, cmp=1;
   const char		*s;

   FUNC_ENTER (H5G_node_found, NULL, FAIL);

   /*
    * Check arguments.
    */
   assert (f);
   assert (addr>=0);
   assert (udata);

   if (NULL==(sn=H5AC_find (f, H5AC_SNODE, addr, NULL))) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTLOAD, FAIL);
   }
   rt = sn->nsyms;

   /*
    * Binary search.
    */
   while (lt<rt && cmp) {
      idx = (lt + rt) / 2;
      if (NULL==(sn=H5AC_find (f, H5AC_SNODE, addr, NULL))) {
	 HRETURN_ERROR (H5E_SYM, H5E_CANTLOAD, FAIL);
      }
      if (NULL==(s=H5H_peek (f, udata->heap, sn->entry[idx].name_off))) {
	 HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL);
      }
      cmp = HDstrcmp (udata->name, s);
      
      if (cmp<0) {
	 rt = idx;
      } else {
	 lt = idx+1;
      }
   }
   if (cmp) HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL);

   switch (udata->operation) {
   case H5G_OPER_FIND:
      /*
       * The caller is querying the symbol entry.
       */
      udata->entry = sn->entry[idx];
      break;

   case H5G_OPER_MODIFY:
      /*
       * The caller is modifying the symbol entry. 
       */
      sn->entry[idx] = udata->entry;
      sn->dirty += 1;
      break;
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_insert
 *
 * Purpose:	The B-tree insertion engine has found the symbol table node
 *		which should receive the new symbol/address pair.  This
 *		function adds it to that node unless it already existed.
 *
 * 		If the node has no room for the symbol then the node is
 *		split into two nodes.  The original node contains the
 *		low values and the new node contains the high values.
 *		The new symbol table entry is added to either node as
 *		appropriate.  When a split occurs, this function will
 *	        write the maximum key of the low node to the MID buffer
 *		and return the address of the new node.
 *
 * 		If the new key is larger than RIGHT then update RIGHT
 *		with the new key.
 *
 * Return:	Success:	Address of new node if the node was
 *				split.  MID has been initialized with
 *				the high key of the left node, RIGHT
 *				has the high key of the right node.
 *
 * 				Zero if the node didn't split.  RIGHT has the
 *				high key of the right node.
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jun 24 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5G_node_insert (hdf5_file_t *f, haddr_t addr, intn *anchor,
		 void *_lt_key, hbool_t *lt_key_changed,
		 void *_md_key, void *_udata,
		 void *_rt_key, hbool_t *rt_key_changed)
{
   H5G_node_key_t	*md_key = (H5G_node_key_t *)_md_key;
   H5G_node_key_t	*rt_key = (H5G_node_key_t *)_rt_key;
   H5G_node_ud1_t 	*udata = (H5G_node_ud1_t *)_udata;
   
   H5G_node_t		*sn;
   H5G_entry_t 		ent[2*H5G_NODE_K];
   haddr_t		new_node=0, offset;
   const char		*s;
   intn			idx=-1, nsyms, cmp=1;
   intn			lt=0, rt;		/*binary search cntrs	*/

   FUNC_ENTER (H5G_node_insert, NULL, FAIL);

   /*
    * Check arguments.
    */
   assert (f);
   assert (addr>=0);
   assert (anchor);
   assert (md_key);
   assert (rt_key);
   assert (udata);

   /*
    * Symbol tables are always split so the new symbol table node is
    * to the right of the old one.
    */
   *anchor = H5B_ANCHOR_LT;
   *lt_key_changed = FALSE;
   *rt_key_changed = FALSE;

   /*
    * Load the symbol node and buffer the entries so we don't have to
    * worry about the cached value disappearing.
    */
   if (NULL==(sn=H5AC_find (f, H5AC_SNODE, addr, NULL))) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTLOAD, FAIL);
   }
   HDmemcpy (ent, sn->entry, sn->nsyms * sizeof(H5G_entry_t));
   rt = nsyms = sn->nsyms;
   sn = NULL;

   /*
    * Where does the new symbol get inserted?  We use a binary search.
    */
   while (lt<rt) {
      idx = (lt + rt) / 2;
      if (NULL==(s=H5H_peek (f, udata->heap, ent[idx].name_off))) {
	 HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL);
      }
      if (0==(cmp=HDstrcmp (udata->name, s))) {
	 HRETURN_ERROR (H5E_SYM, H5E_CANTINSERT, FAIL); /*already present*/
      }
      if (cmp<0) {
	 rt = idx;
      } else {
	 lt = idx+1;
      }
   }
   idx += cmp>0 ? 1 : 0;

   /*
    * Add the new name to the heap.  The caller will check if the
    * heap address changed and update the symbol table object header
    * with the new heap address.
    */
   offset = H5H_insert (f, udata->heap, strlen(udata->name)+1, udata->name);
   if (offset<0) HRETURN_ERROR (H5E_SYM, H5E_CANTINSERT, FAIL);

   if (nsyms>=2*H5G_NODE_K) {
      /*
       * The node is full.  Split it into a left and right
       * node and return the address of the new right node (the
       * left node is at the same address as the original node).
       */
      
      /* The left node */
      if (NULL==(sn=H5AC_find (f, H5AC_SNODE, addr, NULL))) {
	 HRETURN_ERROR (H5E_SYM, H5E_CANTLOAD, FAIL);
      }
      HDmemset (sn->entry+H5G_NODE_K, 0, H5G_NODE_K*sizeof(H5G_entry_t));
      sn->nsyms = H5G_NODE_K;
      sn->dirty += 1;
      
      if (idx<=H5G_NODE_K) {
	 memmove (sn->entry+idx+1, sn->entry+idx,
		  (H5G_NODE_K-idx) * sizeof(H5G_entry_t));
	 sn->entry[idx] = udata->entry;
	 sn->entry[idx].name_off = offset;
	 sn->nsyms += 1;
      }
      
      /* The middle key */
      md_key->offset = sn->entry[sn->nsyms-1].name_off;

      /* The right node */
      if ((new_node = H5G_node_new (f, NULL, NULL, NULL))<0) {
	 HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, FAIL);
      }
      if (NULL==(sn=H5AC_find (f, H5AC_SNODE, new_node, NULL))) {
	 HRETURN_ERROR (H5E_SYM, H5E_CANTLOAD, FAIL);
      }
      HDmemcpy (sn->entry, ent+H5G_NODE_K,
		H5G_NODE_K*sizeof(H5G_entry_t));
      sn->nsyms = H5G_NODE_K;
      sn->dirty += 1;

      if (idx>H5G_NODE_K) {
	 idx -= H5G_NODE_K;
	 HDmemmove (sn->entry+idx+1, sn->entry+idx,
		    (H5G_NODE_K-idx) * sizeof (H5G_entry_t));
	 sn->entry[idx] = udata->entry;
	 sn->entry[idx].name_off = offset;
	 sn->nsyms += 1;

	 if (idx+1==sn->nsyms) {
	    rt_key->offset = offset;
	    *rt_key_changed = TRUE;
	 }
      }
      
   } else {
      /*
       * Add the new symbol to the node.
       */
      if (NULL==(sn=H5AC_find (f, H5AC_SNODE, addr, NULL))) {
	 HRETURN_ERROR (H5E_SYM, H5E_CANTLOAD, FAIL);
      }
      sn->dirty += 1;
      HDmemmove (sn->entry+idx+1, sn->entry+idx,
		 (sn->nsyms-idx) * sizeof (H5G_entry_t));
      sn->nsyms += 1;
      sn->entry[idx] = udata->entry;
      sn->entry[idx].name_off = offset;

      if (idx+1==sn->nsyms) {
	 rt_key->offset = offset;
	 *rt_key_changed = TRUE;
      }
   }

   FUNC_LEAVE (new_node);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_list
 *
 * Purpose:	This function gets called during a directory list operation.
 *		It should fill in data in the UDATA struct.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jun 24 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_node_list (hdf5_file_t *f, haddr_t addr, void *_udata)
{
   H5G_node_list_t	*udata = (H5G_node_list_t *)_udata;
   H5G_node_t		*sn = NULL;
   off_t		*offsets = NULL;
   intn			nsyms, i;
   const char		*s;

   FUNC_ENTER (H5G_node_list, NULL, FAIL);

   /*
    * Check arguments.
    */
   assert (f);
   assert (addr>=0);
   assert (udata);

   if (NULL==(sn=H5AC_find (f, H5AC_SNODE, addr, NULL))) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTLOAD, FAIL);
   }
   nsyms = sn->nsyms;

   /*
    * If we've already overflowed the user-supplied buffer, then just
    * keep track of how many names we've seen and don't bother doing
    * anything else.
    */
   if (udata->nsyms >= udata->maxentries) {
      udata->nsyms += nsyms;
      HRETURN (SUCCEED);
   }

   /*
    * Save the name offsets because looking up the name may cause the
    * symbol table node to be preempted from the cache.
    */
   if (udata->entry) {
      for (i=0; i<nsyms && udata->nsyms+i<udata->maxentries; i++) {
	 udata->entry[udata->nsyms+i] = sn->entry[i];
      }
   } else if (udata->name) {
      offsets = H5MM_xmalloc (nsyms * sizeof(off_t));
      for (i=0; i<nsyms && udata->nsyms+i<udata->maxentries; i++) {
	 offsets[i] = sn->entry[i].name_off;
      }
   }

   /*
    * Now strdup() the names.
    */
   if (udata->name && udata->entry) {
      for (i=0; i<nsyms && udata->nsyms+i<udata->maxentries; i++) {
	 s = H5H_peek (f, udata->heap, udata->entry[udata->nsyms+i].name_off);
	 if (!s) HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL);
	 udata->name[udata->nsyms+i] = H5MM_xstrdup (s);
      }
   } else if (udata->name) {
      for (i=0; i<nsyms && udata->nsyms+i<udata->maxentries; i++) {
	 if (NULL==(s=H5H_peek (f, udata->heap, offsets[i]))) {
	    HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL);
	 }
	 udata->name[udata->nsyms+i] = H5MM_xstrdup (s);
      }
      offsets = H5MM_xfree (offsets);
   }

   /*
    * Update the number of symbols.
    */
   udata->nsyms += nsyms;
   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_debug
 *
 * Purpose:	Prints debugging information about a symbol table node
 *		or a B-tree node for a symbol table B-tree.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  4 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_node_debug (hdf5_file_t *f, haddr_t addr, FILE *stream, intn indent,
		intn fwidth, haddr_t heap)
{
   int		i, j;
   char		buf[64];
   H5G_node_t	*sn = NULL;
   herr_t	status;
   const char	*s;

   FUNC_ENTER (H5G_node_debug, NULL, FAIL);

   /*
    * Check arguments.
    */
   assert (f);
   assert (addr>=0);
   assert (stream);
   assert (indent>=0);
   assert (fwidth>=0);

   /*
    * If we couldn't load the symbol table node, then try loading the
    * B-tree node.
    */
   if (NULL==(sn=H5AC_find(f, H5AC_SNODE, addr, NULL))) {
      H5ECLEAR; /*discard that error*/
      status = H5B_debug (f, addr, stream, indent, fwidth, H5B_SNODE);
      if (status<0) HRETURN_ERROR (H5E_SYM, H5E_CANTLOAD, FAIL);
      HRETURN (SUCCEED);
   }


   fprintf (stream, "%*sSymbol Table Node...\n", indent, "");
   fprintf (stream, "%*s%-*s %d\n", indent, "", fwidth,
	    "Dirty:",
	    sn->dirty);
   fprintf (stream, "%*s%-*s %d of %d\n", indent, "", fwidth,
	    "Number of Symbols:",
	    sn->nsyms, 2*H5G_NODE_K);

   indent += 3;
   fwidth = MAX (0, fwidth-3);
   for (i=0; i<sn->nsyms; i++) {
      fprintf (stream, "%*sSymbol %d:\n", indent-3, "", i);
      if (heap>0 && (s = H5H_peek (f, heap, sn->entry[i].name_off))) {
	 fprintf (stream, "%*s%-*s `%s'\n", indent, "", fwidth,
		  "Name:",
		  s);
      }
      fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
	       "Name offset into private heap:",
	       (unsigned long)(sn->entry[i].name_off));
      fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
	       "Object header address:",
	       (unsigned long)(sn->entry[i].header));
      
      fprintf (stream, "%*s%-*s ", indent, "", fwidth,
	       "Symbol type:");
      switch (sn->entry[i].type) {
      case H5G_NOTHING_CACHED:
	 fprintf (stream, "Nothing Cached\n");
	 break;
	 
      case H5G_CACHED_SDATA:
	 fprintf (stream, "S-data\n");
	 fprintf (stream, "%*s%-*s %u\n", indent, "", fwidth,
		  "Number type:",
		  (unsigned)(sn->entry[i].cache.sdata.nt));
	 fprintf (stream, "%*s%-*s %u\n", indent, "", fwidth,
		  "Dimensionality:",
		  (unsigned)(sn->entry[i].cache.sdata.ndim));
	 for (j=0; j<sn->entry[i].cache.sdata.ndim && j<4; j++) {
	    sprintf (buf, "Dimension %d", j);
	    fprintf (stream, "%*s%-*s %u\n", indent, "", fwidth,
		     buf,
		     (unsigned)(sn->entry[i].cache.sdata.dim[j]));
	 }
	 break;
	 
      case H5G_CACHED_STAB:
	 fprintf (stream, "Symbol Table\n");
	 fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
		  "B-tree address:",
		  (unsigned long)(sn->entry[i].cache.stab.btree));
	 fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
		  "Heap address:",
		  (unsigned long)(sn->entry[i].cache.stab.heap));
	 break;

      default:
	 fprintf (stream, "*** Unknown symbol type %d\n", sn->entry[i].type);
	 break;
      }
   }

   FUNC_LEAVE (SUCCEED);
}
   
