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

/*-------------------------------------------------------------------------
 *
 * Created:     H5C.c
 *              June 1 2004
 *              John Mainzer
 *
 * Purpose:     Functions in this file implement a generic cache for
 *              things which exist on disk, and which may be 
 *	 	unambiguously referenced by their disk addresses.
 *
 *              The code in this module was initially written in 
 *		support of a complete re-write of the metadata cache
 *		in H5AC.c  However, other uses for the cache code
 *		suggested themselves, and thus this file was created 
 *		in an attempt to support re-use.
 *
 *		For a detailed overview of the cache, please see the
 *		header comment for H5C_t in this file.
 *
 * Modifications:
 *
 *              QAK - 11/27/2004
 *              Switched over to using skip list routines instead of TBBT
 *              routines.
 *
 *-------------------------------------------------------------------------
 */

/**************************************************************************
 *
 *				To Do:
 *
 *	Code Changes:
 *
 *	 - Remove extra functionality in H5C_flush_single_entry()?
 *
 *	 - Change protect/unprotect to lock/unlock.
 *
 *	 - Change the way the dirty flag is set.  Probably pass it in
 *	   as a parameter in unprotect & insert.
 *
 *	 - Size should also be passed in as a parameter in insert and
 *	   unprotect -- or some other way should be found to advise the
 *	   cache of changes in entry size.
 *
 *	 - Flush entries in increasing address order in 
 *	   H5C_make_space_in_cache().
 *
 *	 - Also in H5C_make_space_in_cache(), use high and low water marks
 *	   to reduce the number of I/O calls.
 *
 *	 - When flushing, attempt to combine contiguous entries to reduce
 *	   I/O overhead.  Can't do this just yet as some entries are not
 *	   contiguous.  Do this in parallel only or in serial as well?
 *
 *	 - Create MPI type for dirty objects when flushing in parallel.
 *
 *	 - Now that TBBT routines aren't used, fix nodes in memory to
 *         point directly to the skip list node from the LRU list, eliminating
 *         skip list lookups when evicting objects from the cache.
 *
 *	Tests:
 *
 *	 - Trim execution time.  (This is no longer a major issue with the 
 *	   shift from the TBBT to a hash table for indexing.)
 *
 *	 - Add random tests.
 *
 **************************************************************************/

#define H5C_PACKAGE		/*suppress error about including H5Cpkg	  */
#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */

/* Pablo information */
/* (Put before include files to avoid problems with inline functions) */
#define PABLO_MASK	H5C_mask

#include "H5private.h"		/* Generic Functions			*/
#include "H5Cpkg.h"		/* Cache				*/
#include "H5Dprivate.h"		/* Dataset functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"		/* Files				*/
#include "H5FDprivate.h"	/* File drivers				*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"         /* Property lists                       */
#include "H5SLprivate.h"	/* Skip lists				*/


/****************************************************************************
 *
 * We maintain doubly linked lists of instances of H5C_cache_entry_t for a 
 * variety of reasons -- protected list, LRU list, and the clean and dirty 
 * LRU lists at present.  The following macros support linking and unlinking 
 * of instances of H5C_cache_entry_t by both their regular and auxilary next 
 * and previous pointers.  
 *
 * The size and length fields are also maintained.
 *
 * Note that the relevant pair of prev and next pointers are presumed to be
 * NULL on entry in the insertion macros.
 *
 * Finally, observe that the sanity checking macros evaluate to the empty
 * string when H5C_DO_SANITY_CHECKS is FALSE.  They also contain calls
 * to the HGOTO_ERROR macro, which may not be appropriate in all cases.
 * If so, we will need versions of the insertion and deletion macros which
 * do not reference the sanity checking macros.
 *							JRM - 5/5/04
 *
 * Changes:
 *
 *  - Removed the line:
 *
 *        ( ( (Size) == (entry_ptr)->size ) && ( (len) != 1 ) ) ||
 *
 *    from the H5C__DLL_PRE_REMOVE_SC macro.  With the addition of the
 *    epoch markers used in the age out based cache size reduction algorithm,
 *    this invarient need not hold, as the epoch markers are of size 0.
 *
 *    One could argue that I should have given the epoch markers a positive
 *    size, but this would break the index_size = LRU_list_size + pl_size
 *    invarient.
 *
 *    Alternatively, I could pass the current decr_mode in to the macro, 
 *    and just skip the check whenever epoch markers may be in use.
 *
 *    However, any size errors should be caught when the cache is flushed
 *    and destroyed.  Until we are tracking such an error, this should be
 *    good enough.
 *                                                     JRM - 12/9/04
 *
 ****************************************************************************/

#if H5C_DO_SANITY_CHECKS

#define H5C__DLL_PRE_REMOVE_SC(entry_ptr, head_ptr, tail_ptr, len, Size, fv) \
if ( ( (head_ptr) == NULL ) ||                                               \
     ( (tail_ptr) == NULL ) ||                                               \
     ( (entry_ptr) == NULL ) ||                                              \
     ( (len) <= 0 ) ||                                                       \
     ( (Size) < (entry_ptr)->size ) ||                                       \
     ( ( (entry_ptr)->prev == NULL ) && ( (head_ptr) != (entry_ptr) ) ) ||   \
     ( ( (entry_ptr)->next == NULL ) && ( (tail_ptr) != (entry_ptr) ) ) ||   \
     ( ( (len) == 1 ) &&                                                     \
       ( ! ( ( (head_ptr) == (entry_ptr) ) &&                                \
             ( (tail_ptr) == (entry_ptr) ) &&                                \
             ( (entry_ptr)->next == NULL ) &&                                \
             ( (entry_ptr)->prev == NULL ) &&                                \
             ( (Size) == (entry_ptr)->size )                                 \
           )                                                                 \
       )                                                                     \
     )                                                                       \
   ) {                                                                       \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, (fv), "DLL pre remove SC failed")     \
}

#define H5C__DLL_SC(head_ptr, tail_ptr, len, Size, fv)                   \
if ( ( ( ( (head_ptr) == NULL ) || ( (tail_ptr) == NULL ) ) &&           \
       ( (head_ptr) != (tail_ptr) )                                      \
     ) ||                                                                \
     ( (len) < 0 ) ||                                                    \
     ( (Size) < 0 ) ||                                                   \
     ( ( (len) == 1 ) &&                                                 \
       ( ( (head_ptr) != (tail_ptr) ) || ( (cache_ptr)->size <= 0 ) ||   \
         ( (head_ptr) == NULL ) || ( (head_ptr)->size != (Size) )        \
       )                                                                 \
     ) ||                                                                \
     ( ( (len) >= 1 ) &&                                                 \
       ( ( (head_ptr) == NULL ) || ( (head_ptr)->prev != NULL ) ||       \
         ( (tail_ptr) == NULL ) || ( (tail_ptr)->next != NULL )          \
       )                                                                 \
     )                                                                   \
   ) {                                                                   \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, (fv), "DLL sanity check failed")  \
}

#define H5C__DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size, fv) \
if ( ( (entry_ptr) == NULL ) ||                                              \
     ( (entry_ptr)->next != NULL ) ||                                        \
     ( (entry_ptr)->prev != NULL ) ||                                        \
     ( ( ( (head_ptr) == NULL ) || ( (tail_ptr) == NULL ) ) &&               \
       ( (head_ptr) != (tail_ptr) )                                          \
     ) ||                                                                    \
     ( (len) < 0 ) ||                                                        \
     ( ( (len) == 1 ) &&                                                     \
       ( ( (head_ptr) != (tail_ptr) ) || ( (Size) <= 0 ) ||                  \
         ( (head_ptr) == NULL ) || ( (head_ptr)->size != (Size) )            \
       )                                                                     \
     ) ||                                                                    \
     ( ( (len) >= 1 ) &&                                                     \
       ( ( (head_ptr) == NULL ) || ( (head_ptr)->prev != NULL ) ||           \
         ( (tail_ptr) == NULL ) || ( (tail_ptr)->next != NULL )              \
       )                                                                     \
     )                                                                       \
   ) {                                                                       \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, (fv), "DLL pre insert SC failed")     \
}

#else /* H5C_DO_SANITY_CHECKS */

#define H5C__DLL_PRE_REMOVE_SC(entry_ptr, head_ptr, tail_ptr, len, Size, fv)
#define H5C__DLL_SC(head_ptr, tail_ptr, len, Size, fv)
#define H5C__DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size, fv)

#endif /* H5C_DO_SANITY_CHECKS */


#define H5C__DLL_APPEND(entry_ptr, head_ptr, tail_ptr, len, Size, fail_val) \
        H5C__DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size,    \
                               fail_val)                                    \
        if ( (head_ptr) == NULL )                                           \
        {                                                                   \
           (head_ptr) = (entry_ptr);                                        \
           (tail_ptr) = (entry_ptr);                                        \
        }                                                                   \
        else                                                                \
        {                                                                   \
           (tail_ptr)->next = (entry_ptr);                                  \
           (entry_ptr)->prev = (tail_ptr);                                  \
           (tail_ptr) = (entry_ptr);                                        \
        }                                                                   \
        (len)++;                                                            \
        (Size) += (entry_ptr)->size;

#define H5C__DLL_PREPEND(entry_ptr, head_ptr, tail_ptr, len, Size, fail_val) \
        H5C__DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size,     \
                               fail_val)                                     \
        if ( (head_ptr) == NULL )                                            \
        {                                                                    \
           (head_ptr) = (entry_ptr);                                         \
           (tail_ptr) = (entry_ptr);                                         \
        }                                                                    \
        else                                                                 \
        {                                                                    \
           (head_ptr)->prev = (entry_ptr);                                   \
           (entry_ptr)->next = (head_ptr);                                   \
           (head_ptr) = (entry_ptr);                                         \
        }                                                                    \
        (len)++;                                                             \
        (Size) += entry_ptr->size;

#define H5C__DLL_REMOVE(entry_ptr, head_ptr, tail_ptr, len, Size, fail_val) \
        H5C__DLL_PRE_REMOVE_SC(entry_ptr, head_ptr, tail_ptr, len, Size,    \
                               fail_val)                                    \
        {                                                                   \
           if ( (head_ptr) == (entry_ptr) )                                 \
           {                                                                \
              (head_ptr) = (entry_ptr)->next;                               \
              if ( (head_ptr) != NULL )                                     \
              {                                                             \
                 (head_ptr)->prev = NULL;                                   \
              }                                                             \
           }                                                                \
           else                                                             \
           {                                                                \
              (entry_ptr)->prev->next = (entry_ptr)->next;                  \
           }                                                                \
           if ( (tail_ptr) == (entry_ptr) )                                 \
           {                                                                \
              (tail_ptr) = (entry_ptr)->prev;                               \
              if ( (tail_ptr) != NULL )                                     \
              {                                                             \
                 (tail_ptr)->next = NULL;                                   \
              }                                                             \
           }                                                                \
           else                                                             \
           {                                                                \
              (entry_ptr)->next->prev = (entry_ptr)->prev;                  \
           }                                                                \
           entry_ptr->next = NULL;                                          \
           entry_ptr->prev = NULL;                                          \
           (len)--;                                                         \
           (Size) -= entry_ptr->size;                                       \
        }


#if H5C_DO_SANITY_CHECKS

#define H5C__AUX_DLL_PRE_REMOVE_SC(entry_ptr, hd_ptr, tail_ptr, len, Size, fv) \
if ( ( (hd_ptr) == NULL ) ||                                                   \
     ( (tail_ptr) == NULL ) ||                                                 \
     ( (entry_ptr) == NULL ) ||                                                \
     ( (len) <= 0 ) ||                                                         \
     ( (Size) < (entry_ptr)->size ) ||                                         \
     ( ( (Size) == (entry_ptr)->size ) && ( ! ( (len) == 1 ) ) ) ||            \
     ( ( (entry_ptr)->aux_prev == NULL ) && ( (hd_ptr) != (entry_ptr) ) ) ||   \
     ( ( (entry_ptr)->aux_next == NULL ) && ( (tail_ptr) != (entry_ptr) ) ) || \
     ( ( (len) == 1 ) &&                                                       \
       ( ! ( ( (hd_ptr) == (entry_ptr) ) && ( (tail_ptr) == (entry_ptr) ) &&   \
             ( (entry_ptr)->aux_next == NULL ) &&                              \
             ( (entry_ptr)->aux_prev == NULL ) &&                              \
             ( (Size) == (entry_ptr)->size )                                   \
           )                                                                   \
       )                                                                       \
     )                                                                         \
   ) {                                                                         \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, (fv), "aux DLL pre remove SC failed")   \
}

#define H5C__AUX_DLL_SC(head_ptr, tail_ptr, len, Size, fv)                  \
if ( ( ( ( (head_ptr) == NULL ) || ( (tail_ptr) == NULL ) ) &&              \
       ( (head_ptr) != (tail_ptr) )                                         \
     ) ||                                                                   \
     ( (len) < 0 ) ||                                                       \
     ( (Size) < 0 ) ||                                                      \
     ( ( (len) == 1 ) &&                                                    \
       ( ( (head_ptr) != (tail_ptr) ) || ( (Size) <= 0 ) ||                 \
         ( (head_ptr) == NULL ) || ( (head_ptr)->size != (Size) )           \
       )                                                                    \
     ) ||                                                                   \
     ( ( (len) >= 1 ) &&                                                    \
       ( ( (head_ptr) == NULL ) || ( (head_ptr)->aux_prev != NULL ) ||      \
         ( (tail_ptr) == NULL ) || ( (tail_ptr)->aux_next != NULL )         \
       )                                                                    \
     )                                                                      \
   ) {                                                                      \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, (fv), "AUX DLL sanity check failed") \
}

#define H5C__AUX_DLL_PRE_INSERT_SC(entry_ptr, hd_ptr, tail_ptr, len, Size, fv) \
if ( ( (entry_ptr) == NULL ) ||                                                \
     ( (entry_ptr)->aux_next != NULL ) ||                                      \
     ( (entry_ptr)->aux_prev != NULL ) ||                                      \
     ( ( ( (hd_ptr) == NULL ) || ( (tail_ptr) == NULL ) ) &&                   \
       ( (hd_ptr) != (tail_ptr) )                                              \
     ) ||                                                                      \
     ( (len) < 0 ) ||                                                          \
     ( ( (len) == 1 ) &&                                                       \
       ( ( (hd_ptr) != (tail_ptr) ) || ( (Size) <= 0 ) ||                      \
         ( (hd_ptr) == NULL ) || ( (hd_ptr)->size != (Size) )                  \
       )                                                                       \
     ) ||                                                                      \
     ( ( (len) >= 1 ) &&                                                       \
       ( ( (hd_ptr) == NULL ) || ( (hd_ptr)->aux_prev != NULL ) ||             \
         ( (tail_ptr) == NULL ) || ( (tail_ptr)->aux_next != NULL )            \
       )                                                                       \
     )                                                                         \
   ) {                                                                         \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, (fv), "AUX DLL pre insert SC failed")   \
}

#else /* H5C_DO_SANITY_CHECKS */

#define H5C__AUX_DLL_PRE_REMOVE_SC(entry_ptr, hd_ptr, tail_ptr, len, Size, fv)
#define H5C__AUX_DLL_SC(head_ptr, tail_ptr, len, Size, fv)
#define H5C__AUX_DLL_PRE_INSERT_SC(entry_ptr, hd_ptr, tail_ptr, len, Size, fv)

#endif /* H5C_DO_SANITY_CHECKS */


#define H5C__AUX_DLL_APPEND(entry_ptr, head_ptr, tail_ptr, len, Size, fail_val)\
        H5C__AUX_DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size,   \
                                   fail_val)                                   \
        if ( (head_ptr) == NULL )                                              \
        {                                                                      \
           (head_ptr) = (entry_ptr);                                           \
           (tail_ptr) = (entry_ptr);                                           \
        }                                                                      \
        else                                                                   \
        {                                                                      \
           (tail_ptr)->aux_next = (entry_ptr);                                 \
           (entry_ptr)->aux_prev = (tail_ptr);                                 \
           (tail_ptr) = (entry_ptr);                                           \
        }                                                                      \
        (len)++;                                                               \
        (Size) += entry_ptr->size;

#define H5C__AUX_DLL_PREPEND(entry_ptr, head_ptr, tail_ptr, len, Size, fv)   \
        H5C__AUX_DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size, \
                                   fv)                                       \
        if ( (head_ptr) == NULL )                                            \
        {                                                                    \
           (head_ptr) = (entry_ptr);                                         \
           (tail_ptr) = (entry_ptr);                                         \
        }                                                                    \
        else                                                                 \
        {                                                                    \
           (head_ptr)->aux_prev = (entry_ptr);                               \
           (entry_ptr)->aux_next = (head_ptr);                               \
           (head_ptr) = (entry_ptr);                                         \
        }                                                                    \
        (len)++;                                                             \
        (Size) += entry_ptr->size;

#define H5C__AUX_DLL_REMOVE(entry_ptr, head_ptr, tail_ptr, len, Size, fv)    \
        H5C__AUX_DLL_PRE_REMOVE_SC(entry_ptr, head_ptr, tail_ptr, len, Size, \
                                   fv)                                       \
        {                                                                    \
           if ( (head_ptr) == (entry_ptr) )                                  \
           {                                                                 \
              (head_ptr) = (entry_ptr)->aux_next;                            \
              if ( (head_ptr) != NULL )                                      \
              {                                                              \
                 (head_ptr)->aux_prev = NULL;                                \
              }                                                              \
           }                                                                 \
           else                                                              \
           {                                                                 \
              (entry_ptr)->aux_prev->aux_next = (entry_ptr)->aux_next;       \
           }                                                                 \
           if ( (tail_ptr) == (entry_ptr) )                                  \
           {                                                                 \
              (tail_ptr) = (entry_ptr)->aux_prev;                            \
              if ( (tail_ptr) != NULL )                                      \
              {                                                              \
                 (tail_ptr)->aux_next = NULL;                                \
              }                                                              \
           }                                                                 \
           else                                                              \
           {                                                                 \
              (entry_ptr)->aux_next->aux_prev = (entry_ptr)->aux_prev;       \
           }                                                                 \
           entry_ptr->aux_next = NULL;                                       \
           entry_ptr->aux_prev = NULL;                                       \
           (len)--;                                                          \
           (Size) -= entry_ptr->size;                                        \
        }


/***********************************************************************
 *
 * Stats collection macros
 *
 * The following macros must handle stats collection when this collection
 * is enabled, and evaluate to the empty string when it is not.
 *
 * The sole exception to this rule is 
 * H5C__UPDATE_CACHE_HIT_RATE_STATS(), which is always active as
 * the cache hit rate stats are always collected and available.
 *
 ***********************************************************************/

#define H5C__UPDATE_CACHE_HIT_RATE_STATS(cache_ptr, hit) \
        (cache_ptr->cache_accesses)++;                   \
        if ( hit ) {                                     \
            (cache_ptr->cache_hits)++;                   \
        }                                                \

#if H5C_COLLECT_CACHE_STATS

#define H5C__UPDATE_STATS_FOR_INSERTION(cache_ptr, entry_ptr)        \
	(((cache_ptr)->insertions)[(entry_ptr)->type->id])++;        \
        if ( (cache_ptr)->index_len > (cache_ptr)->max_index_len )   \
	    (cache_ptr)->max_index_len = (cache_ptr)->index_len;     \
        if ( (cache_ptr)->index_size > (cache_ptr)->max_index_size ) \
	    (cache_ptr)->max_index_size = (cache_ptr)->index_size;   \
        if ( (cache_ptr)->slist_len > (cache_ptr)->max_slist_len )   \
	    (cache_ptr)->max_slist_len = (cache_ptr)->slist_len;     \
        if ( (cache_ptr)->slist_size > (cache_ptr)->max_slist_size ) \
	    (cache_ptr)->max_slist_size = (cache_ptr)->slist_size;   \
        if ( (entry_ptr)->size >                                     \
             ((cache_ptr)->max_size)[(entry_ptr)->type->id] ) {      \
            ((cache_ptr)->max_size)[(entry_ptr)->type->id]           \
                 = (entry_ptr)->size;                                \
        }

#define H5C__UPDATE_STATS_FOR_UNPROTECT(cache_ptr)                   \
        if ( (cache_ptr)->slist_len > (cache_ptr)->max_slist_len )   \
	    (cache_ptr)->max_slist_len = (cache_ptr)->slist_len;     \
        if ( (cache_ptr)->slist_size > (cache_ptr)->max_slist_size ) \
	    (cache_ptr)->max_slist_size = (cache_ptr)->slist_size; 

#define H5C__UPDATE_STATS_FOR_RENAME(cache_ptr, entry_ptr) \
	(((cache_ptr)->renames)[(entry_ptr)->type->id])++;

#define H5C__UPDATE_STATS_FOR_HT_INSERTION(cache_ptr) \
	(cache_ptr)->total_ht_insertions++;

#define H5C__UPDATE_STATS_FOR_HT_DELETION(cache_ptr) \
	(cache_ptr)->total_ht_deletions++;

#define H5C__UPDATE_STATS_FOR_HT_SEARCH(cache_ptr, success, depth)  \
	if ( success ) {                                            \
	    (cache_ptr)->successful_ht_searches++;                  \
	    (cache_ptr)->total_successful_ht_search_depth += depth; \
	} else {                                                    \
	    (cache_ptr)->failed_ht_searches++;                      \
	    (cache_ptr)->total_failed_ht_search_depth += depth;     \
	}

#if H5C_COLLECT_CACHE_ENTRY_STATS

#define H5C__RESET_CACHE_ENTRY_STATS(entry_ptr) \
        (entry_ptr)->accesses = 0;              \
        (entry_ptr)->clears   = 0;              \
        (entry_ptr)->flushes  = 0;

#define H5C__UPDATE_STATS_FOR_CLEAR(cache_ptr, entry_ptr) \
	(((cache_ptr)->clears)[(entry_ptr)->type->id])++; \
        ((entry_ptr)->clears)++;

#define H5C__UPDATE_STATS_FOR_FLUSH(cache_ptr, entry_ptr)  \
	(((cache_ptr)->flushes)[(entry_ptr)->type->id])++; \
        ((entry_ptr)->flushes)++;

#define H5C__UPDATE_STATS_FOR_EVICTION(cache_ptr, entry_ptr)        \
	(((cache_ptr)->evictions)[(entry_ptr)->type->id])++;        \
        if ( (entry_ptr)->accesses >                                \
             ((cache_ptr)->max_accesses)[(entry_ptr)->type->id] ) { \
            ((cache_ptr)->max_accesses)[(entry_ptr)->type->id]      \
                = (entry_ptr)->accesses;                            \
        }                                                           \
        if ( (entry_ptr)->accesses <                                \
             ((cache_ptr)->min_accesses)[(entry_ptr)->type->id] ) { \
            ((cache_ptr)->min_accesses)[(entry_ptr)->type->id]      \
                = (entry_ptr)->accesses;                            \
        }                                                           \
        if ( (entry_ptr)->clears >                                  \
             ((cache_ptr)->max_clears)[(entry_ptr)->type->id] ) {   \
            ((cache_ptr)->max_clears)[(entry_ptr)->type->id]        \
                 = (entry_ptr)->clears;                             \
        }                                                           \
        if ( (entry_ptr)->flushes >                                 \
             ((cache_ptr)->max_flushes)[(entry_ptr)->type->id] ) {  \
            ((cache_ptr)->max_flushes)[(entry_ptr)->type->id]       \
                 = (entry_ptr)->flushes;                            \
        }                                                           \
        if ( (entry_ptr)->size >                                    \
             ((cache_ptr)->max_size)[(entry_ptr)->type->id] ) {     \
            ((cache_ptr)->max_size)[(entry_ptr)->type->id]          \
                 = (entry_ptr)->size;                               \
        }                                                           \

#define H5C__UPDATE_STATS_FOR_PROTECT(cache_ptr, entry_ptr, hit)     \
	if ( hit )                                                   \
            ((cache_ptr)->hits)[(entry_ptr)->type->id]++;            \
	else                                                         \
            ((cache_ptr)->misses)[(entry_ptr)->type->id]++;          \
        if ( (cache_ptr)->index_len > (cache_ptr)->max_index_len )   \
            (cache_ptr)->max_index_len = (cache_ptr)->index_len;     \
        if ( (cache_ptr)->index_size > (cache_ptr)->max_index_size ) \
            (cache_ptr)->max_index_size = (cache_ptr)->index_size;   \
        if ( (cache_ptr)->pl_len > (cache_ptr)->max_pl_len )         \
            (cache_ptr)->max_pl_len = (cache_ptr)->pl_len;           \
        if ( (cache_ptr)->pl_size > (cache_ptr)->max_pl_size )       \
            (cache_ptr)->max_pl_size = (cache_ptr)->pl_size;         \
        if ( (entry_ptr)->size >                                     \
             ((cache_ptr)->max_size)[(entry_ptr)->type->id] ) {      \
            ((cache_ptr)->max_size)[(entry_ptr)->type->id]           \
                 = (entry_ptr)->size;                                \
        }                                                            \
        ((entry_ptr)->accesses)++;

#else /* H5C_COLLECT_CACHE_ENTRY_STATS */

#define H5C__RESET_CACHE_ENTRY_STATS(entry_ptr) 

#define H5C__UPDATE_STATS_FOR_CLEAR(cache_ptr, entry_ptr) \
	(((cache_ptr)->clears)[(entry_ptr)->type->id])++; 

#define H5C__UPDATE_STATS_FOR_FLUSH(cache_ptr, entry_ptr) \
	(((cache_ptr)->flushes)[(entry_ptr)->type->id])++; 

#define H5C__UPDATE_STATS_FOR_EVICTION(cache_ptr, entry_ptr) \
	(((cache_ptr)->evictions)[(entry_ptr)->type->id])++; 

#define H5C__UPDATE_STATS_FOR_PROTECT(cache_ptr, entry_ptr, hit)     \
	if ( hit )                                                   \
            ((cache_ptr)->hits)[(entry_ptr)->type->id]++;            \
	else                                                         \
            ((cache_ptr)->misses)[(entry_ptr)->type->id]++;          \
        if ( (cache_ptr)->index_len > (cache_ptr)->max_index_len )   \
            (cache_ptr)->max_index_len = (cache_ptr)->index_len;     \
        if ( (cache_ptr)->index_size > (cache_ptr)->max_index_size ) \
            (cache_ptr)->max_index_size = (cache_ptr)->index_size;   \
        if ( (cache_ptr)->pl_len > (cache_ptr)->max_pl_len )         \
            (cache_ptr)->max_pl_len = (cache_ptr)->pl_len;           \
        if ( (cache_ptr)->pl_size > (cache_ptr)->max_pl_size )       \
            (cache_ptr)->max_pl_size = (cache_ptr)->pl_size;

#endif /* H5C_COLLECT_CACHE_ENTRY_STATS */

#else /* H5C_COLLECT_CACHE_STATS */

#define H5C__RESET_CACHE_ENTRY_STATS(entry_ptr) 
#define H5C__UPDATE_STATS_FOR_UNPROTECT(cache_ptr)
#define H5C__UPDATE_STATS_FOR_RENAME(cache_ptr, entry_ptr)
#define H5C__UPDATE_STATS_FOR_HT_INSERTION(cache_ptr)
#define H5C__UPDATE_STATS_FOR_HT_DELETION(cache_ptr)
#define H5C__UPDATE_STATS_FOR_HT_SEARCH(cache_ptr, success, depth)
#define H5C__UPDATE_STATS_FOR_INSERTION(cache_ptr, entry_ptr)
#define H5C__UPDATE_STATS_FOR_CLEAR(cache_ptr, entry_ptr)
#define H5C__UPDATE_STATS_FOR_FLUSH(cache_ptr, entry_ptr)
#define H5C__UPDATE_STATS_FOR_EVICTION(cache_ptr, entry_ptr)
#define H5C__UPDATE_STATS_FOR_PROTECT(cache_ptr, entry_ptr, hit)

#endif /* H5C_COLLECT_CACHE_STATS */


/***********************************************************************
 *
 * Hash table access and manipulation macros:
 *
 * The following macros handle searches, insertions, and deletion in
 * the hash table.
 *
 * When modifying these macros, remember to modify the similar macros
 * in tst/cache.c
 *
 ***********************************************************************/

/* H5C__HASH_TABLE_LEN is defined in H5Cpkg.h.  It mut be a power of two. */

#define H5C__HASH_MASK		((size_t)(H5C__HASH_TABLE_LEN - 1) << 3)

#define H5C__HASH_FCN(x)	(int)(((x) & H5C__HASH_MASK) >> 3)

#if H5C_DO_SANITY_CHECKS

#define H5C__PRE_HT_INSERT_SC(cache_ptr, entry_ptr, fail_val) \
if ( ( (cache_ptr) == NULL ) ||                               \
     ( (cache_ptr)->magic != H5C__H5C_T_MAGIC ) ||            \
     ( (entry_ptr) == NULL ) ||                               \
     ( ! H5F_addr_defined((entry_ptr)->addr) ) ||             \
     ( (entry_ptr)->ht_next != NULL ) ||                      \
     ( (entry_ptr)->ht_prev != NULL ) ||                      \
     ( (entry_ptr)->size <= 0 ) ||                            \
     ( (k = H5C__HASH_FCN((entry_ptr)->addr)) < 0 ) ||        \
     ( k >= H5C__HASH_TABLE_LEN ) ) {                         \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, fail_val,              \
               "Pre HT insert SC failed")                     \
}

#define H5C__PRE_HT_REMOVE_SC(cache_ptr, entry_ptr)                     \
if ( ( (cache_ptr) == NULL ) ||                                         \
     ( (cache_ptr)->magic != H5C__H5C_T_MAGIC ) ||                      \
     ( (cache_ptr)->index_len < 1 ) ||                                  \
     ( (entry_ptr) == NULL ) ||                                         \
     ( (cache_ptr)->index_size < (entry_ptr)->size ) ||                 \
     ( ! H5F_addr_defined((entry_ptr)->addr) ) ||                       \
     ( (entry_ptr)->size <= 0 ) ||                                      \
     ( H5C__HASH_FCN((entry_ptr)->addr) < 0 ) ||                        \
     ( H5C__HASH_FCN((entry_ptr)->addr) >= H5C__HASH_TABLE_LEN ) ||     \
     ( ((cache_ptr)->index)[(H5C__HASH_FCN((entry_ptr)->addr))]         \
       == NULL ) ||                                                     \
     ( ( ((cache_ptr)->index)[(H5C__HASH_FCN((entry_ptr)->addr))]       \
       != (entry_ptr) ) &&                                              \
       ( (entry_ptr)->ht_prev == NULL ) ) ||                            \
     ( ( ((cache_ptr)->index)[(H5C__HASH_FCN((entry_ptr)->addr))] ==    \
         (entry_ptr) ) &&                                               \
       ( (entry_ptr)->ht_prev != NULL ) ) ) {                           \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Pre HT remove SC failed") \
}

#define H5C__PRE_HT_SEARCH_SC(cache_ptr, Addr, fail_val)                    \
if ( ( (cache_ptr) == NULL ) ||                                             \
     ( (cache_ptr)->magic != H5C__H5C_T_MAGIC ) ||                          \
     ( ! H5F_addr_defined(Addr) ) ||                                        \
     ( H5C__HASH_FCN(Addr) < 0 ) ||                                         \
     ( H5C__HASH_FCN(Addr) >= H5C__HASH_TABLE_LEN ) ) {                     \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, fail_val, "Pre HT search SC failed") \
}

#define H5C__POST_SUC_HT_SEARCH_SC(cache_ptr, entry_ptr, Addr, k, fail_val) \
if ( ( (cache_ptr) == NULL ) ||                                             \
     ( (cache_ptr)->magic != H5C__H5C_T_MAGIC ) ||                          \
     ( (cache_ptr)->index_len < 1 ) ||                                      \
     ( (entry_ptr) == NULL ) ||                                             \
     ( (cache_ptr)->index_size < (entry_ptr)->size ) ||                     \
     ( H5F_addr_ne((entry_ptr)->addr, (Addr)) ) ||                          \
     ( (entry_ptr)->size <= 0 ) ||                                          \
     ( ((cache_ptr)->index)[k] == NULL ) ||                                 \
     ( ( ((cache_ptr)->index)[k] != (entry_ptr) ) &&                        \
       ( (entry_ptr)->ht_prev == NULL ) ) ||                                \
     ( ( ((cache_ptr)->index)[k] == (entry_ptr) ) &&                        \
       ( (entry_ptr)->ht_prev != NULL ) ) ||                                \
     ( ( (entry_ptr)->ht_prev != NULL ) &&                                  \
       ( (entry_ptr)->ht_prev->ht_next != (entry_ptr) ) ) ||                \
     ( ( (entry_ptr)->ht_next != NULL ) &&                                  \
       ( (entry_ptr)->ht_next->ht_prev != (entry_ptr) ) ) ) {               \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, fail_val,                            \
                "Post successful HT search SC failed")                      \
}

#define H5C__POST_HT_SHIFT_TO_FRONT(cache_ptr, entry_ptr, k, fail_val) \
if ( ( (cache_ptr) == NULL ) ||                                        \
     ( ((cache_ptr)->index)[k] != (entry_ptr) ) ||                     \
     ( (entry_ptr)->ht_prev != NULL ) ) {                              \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, fail_val,                       \
               "Post HT shift to front SC failed")                     \
}


#else /* H5C_DO_SANITY_CHECKS */

#define H5C__PRE_HT_INSERT_SC(cache_ptr, entry_ptr, fail_val)
#define H5C__PRE_HT_REMOVE_SC(cache_ptr, entry_ptr)
#define H5C__PRE_HT_SEARCH_SC(cache_ptr, Addr, fail_val)
#define H5C__POST_SUC_HT_SEARCH_SC(cache_ptr, entry_ptr, Addr, k, fail_val)
#define H5C__POST_HT_SHIFT_TO_FRONT(cache_ptr, entry_ptr, k, fail_val)

#endif /* H5C_DO_SANITY_CHECKS */


#define H5C__INSERT_IN_INDEX(cache_ptr, entry_ptr, fail_val) \
{                                                            \
    int k;                                                   \
    H5C__PRE_HT_INSERT_SC(cache_ptr, entry_ptr, fail_val)    \
    k = H5C__HASH_FCN((entry_ptr)->addr);                    \
    if ( ((cache_ptr)->index)[k] == NULL )                   \
    {                                                        \
        ((cache_ptr)->index)[k] = (entry_ptr);               \
    }                                                        \
    else                                                     \
    {                                                        \
        (entry_ptr)->ht_next = ((cache_ptr)->index)[k];      \
        (entry_ptr)->ht_next->ht_prev = (entry_ptr);         \
        ((cache_ptr)->index)[k] = (entry_ptr);               \
    }                                                        \
    (cache_ptr)->index_len++;                                \
    (cache_ptr)->index_size += (entry_ptr)->size;            \
    H5C__UPDATE_STATS_FOR_HT_INSERTION(cache_ptr)            \
}

#define H5C__DELETE_FROM_INDEX(cache_ptr, entry_ptr)          \
{                                                             \
    int k;                                                    \
    H5C__PRE_HT_REMOVE_SC(cache_ptr, entry_ptr)               \
    k = H5C__HASH_FCN((entry_ptr)->addr);                     \
    if ( (entry_ptr)->ht_next )                               \
    {                                                         \
        (entry_ptr)->ht_next->ht_prev = (entry_ptr)->ht_prev; \
    }                                                         \
    if ( (entry_ptr)->ht_prev )                               \
    {                                                         \
        (entry_ptr)->ht_prev->ht_next = (entry_ptr)->ht_next; \
    }                                                         \
    if ( ((cache_ptr)->index)[k] == (entry_ptr) )             \
    {                                                         \
        ((cache_ptr)->index)[k] = (entry_ptr)->ht_next;       \
    }                                                         \
    (entry_ptr)->ht_next = NULL;                              \
    (entry_ptr)->ht_prev = NULL;                              \
    (cache_ptr)->index_len--;                                 \
    (cache_ptr)->index_size -= (entry_ptr)->size;             \
    H5C__UPDATE_STATS_FOR_HT_DELETION(cache_ptr)              \
}

#define H5C__SEARCH_INDEX(cache_ptr, Addr, entry_ptr, fail_val)             \
{                                                                           \
    int k;                                                                  \
    int depth = 0;                                                          \
    H5C__PRE_HT_SEARCH_SC(cache_ptr, Addr, fail_val)                        \
    k = H5C__HASH_FCN(Addr);                                                \
    entry_ptr = ((cache_ptr)->index)[k];                                    \
    while ( ( entry_ptr ) && ( H5F_addr_ne(Addr, (entry_ptr)->addr) ) )     \
    {                                                                       \
        (entry_ptr) = (entry_ptr)->ht_next;                                 \
        (depth)++;                                                          \
    }                                                                       \
    if ( entry_ptr )                                                        \
    {                                                                       \
        H5C__POST_SUC_HT_SEARCH_SC(cache_ptr, entry_ptr, Addr, k, fail_val) \
        if ( entry_ptr != ((cache_ptr)->index)[k] )                         \
        {                                                                   \
            if ( (entry_ptr)->ht_next )                                     \
            {                                                               \
                (entry_ptr)->ht_next->ht_prev = (entry_ptr)->ht_prev;       \
            }                                                               \
            HDassert( (entry_ptr)->ht_prev != NULL );                       \
            (entry_ptr)->ht_prev->ht_next = (entry_ptr)->ht_next;           \
            ((cache_ptr)->index)[k]->ht_prev = (entry_ptr);                 \
            (entry_ptr)->ht_next = ((cache_ptr)->index)[k];                 \
            (entry_ptr)->ht_prev = NULL;                                    \
            ((cache_ptr)->index)[k] = (entry_ptr);                          \
            H5C__POST_HT_SHIFT_TO_FRONT(cache_ptr, entry_ptr, k, fail_val)  \
        }                                                                   \
    }                                                                       \
    H5C__UPDATE_STATS_FOR_HT_SEARCH(cache_ptr, (entry_ptr != NULL), depth)  \
} 


/**************************************************************************
 *
 * Skip list insertion and deletion macros:  
 *
 * These used to be functions, but I converted them to macros to avoid some
 * function call overhead.
 *
 **************************************************************************/

/*-------------------------------------------------------------------------
 *
 * Macro:	H5C__INSERT_ENTRY_IN_SLIST
 *
 * Purpose:     Insert the specified instance of H5C_cache_entry_t into 
 *		the skip list in the specified instance of H5C_t.  Update
 *		the associated length and size fields.
 *
 * Return:      N/A
 *
 * Programmer:  John Mainzer, 5/10/04
 *
 * Modifications:
 *
 *		JRM -- 7/21/04
 *		Updated function to set the in_tree flag when inserting
 *		an entry into the tree.  Also modified the function to 
 *		update the tree size and len fields instead of the similar 
 *		index fields.
 *
 *		All of this is part of the modifications to support the
 *		hash table.
 *
 *		JRM -- 7/27/04
 *		Converted the function H5C_insert_entry_in_tree() into
 *		the macro H5C__INSERT_ENTRY_IN_TREE in the hopes of 
 *		wringing a little more speed out of the cache.
 *
 *		Note that we don't bother to check if the entry is already
 *		in the tree -- if it is, H5SL_insert() will fail.
 *
 *		QAK -- 11/27/04
 *		Switched over to using skip list routines.
 *
 *-------------------------------------------------------------------------
 */

#define H5C__INSERT_ENTRY_IN_SLIST(cache_ptr, entry_ptr)                       \
{                                                                              \
    HDassert( (cache_ptr) );                                                   \
    HDassert( (cache_ptr)->magic == H5C__H5C_T_MAGIC );                        \
    HDassert( (entry_ptr) );                                                   \
    HDassert( (entry_ptr)->size > 0 );                                         \
    HDassert( H5F_addr_defined((entry_ptr)->addr) );                           \
    HDassert( !((entry_ptr)->in_slist) );                                      \
                                                                               \
    if ( H5SL_insert((cache_ptr)->slist_ptr, &(entry_ptr)->addr, entry_ptr)    \
                                                                         < 0 ) \
        HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL,                             \
                    "Can't insert entry in skip list")                         \
                                                                               \
    (entry_ptr)->in_slist = TRUE;                                              \
    (cache_ptr)->slist_len++;                                                  \
    (cache_ptr)->slist_size += (entry_ptr)->size;                              \
                                                                               \
    HDassert( (cache_ptr)->slist_len > 0 );                                    \
    HDassert( (cache_ptr)->slist_size > 0 );                                   \
                                                                               \
} /* H5C__INSERT_ENTRY_IN_SLIST */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__REMOVE_ENTRY_FROM_SLIST
 *
 * Purpose:     Remove the specified instance of H5C_cache_entry_t from the 
 *		index skip list in the specified instance of H5C_t.  Update
 *		the associated length and size fields.
 *
 * Return:      N/A
 *
 * Programmer:  John Mainzer, 5/10/04
 *
 * Modifications:
 *
 *		JRM -- 7/21/04
 *		Updated function for the addition of the hash table.
 *
 *		JRM - 7/27/04
 *		Converted from the function H5C_remove_entry_from_tree()
 *		to the macro H5C__REMOVE_ENTRY_FROM_TREE in the hopes of
 *		wringing a little more performance out of the cache.
 *
 *		QAK -- 11/27/04
 *		Switched over to using skip list routines.
 *
 *-------------------------------------------------------------------------
 */

#define H5C__REMOVE_ENTRY_FROM_SLIST(cache_ptr, entry_ptr)          \
{                                                                   \
    HDassert( (cache_ptr) );                                        \
    HDassert( (cache_ptr)->magic == H5C__H5C_T_MAGIC );             \
    HDassert( (entry_ptr) );                                        \
    HDassert( !((entry_ptr)->is_protected) );                       \
    HDassert( (entry_ptr)->size > 0 );                              \
    HDassert( (entry_ptr)->in_slist );                              \
    HDassert( (cache_ptr)->slist_ptr );                             \
                                                                    \
    if ( H5SL_remove((cache_ptr)->slist_ptr, &(entry_ptr)->addr)    \
         != (entry_ptr) )                                           \
                                                                    \
        HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL,                  \
                    "Can't delete entry from skip list.")           \
                                                                    \
    HDassert( (cache_ptr)->slist_len > 0 );                         \
    (cache_ptr)->slist_len--;                                       \
    HDassert( (cache_ptr)->slist_size >= (entry_ptr)->size );       \
    (cache_ptr)->slist_size -= (entry_ptr)->size;                   \
    (entry_ptr)->in_slist = FALSE;                                  \
} /* H5C__REMOVE_ENTRY_FROM_SLIST */


/**************************************************************************
 *
 * Replacement policy update macros:  
 *
 * These used to be functions, but I converted them to macros to avoid some
 * function call overhead.
 *
 **************************************************************************/

/*-------------------------------------------------------------------------
 *
 * Macro:	H5C__UPDATE_RP_FOR_EVICTION
 *
 * Purpose:     Update the replacement policy data structures for an 
 *		eviction of the specified cache entry.
 *
 *		At present, we only support the modified LRU policy, so 
 *		this function deals with that case unconditionally.  If
 *		we ever support other replacement policies, the function
 *		should switch on the current policy and act accordingly.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  John Mainzer, 5/10/04
 *
 * Modifications:
 *
 *		JRM - 7/27/04
 *		Converted the function H5C_update_rp_for_eviction() to the
 *		macro H5C__UPDATE_RP_FOR_EVICTION in an effort to squeeze
 *		a bit more performance out of the cache.
 *
 *		At least for the first cut, I am leaving the comments and
 *		white space in the macro.  If they cause dificulties with
 *		the pre-processor, I'll have to remove them.
 *
 *		JRM - 7/28/04
 *		Split macro into two version, one supporting the clean and
 *		dirty LRU lists, and the other not.  Yet another attempt
 *		at optimization.
 *
 *-------------------------------------------------------------------------
 */

#if H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS

#define H5C__UPDATE_RP_FOR_EVICTION(cache_ptr, entry_ptr, fail_val)          \
{                                                                            \
    HDassert( (cache_ptr) );                                                 \
    HDassert( (cache_ptr)->magic == H5C__H5C_T_MAGIC );                      \
    HDassert( (entry_ptr) );                                                 \
    HDassert( !((entry_ptr)->is_protected) );                                \
    HDassert( (entry_ptr)->size > 0 );                                       \
                                                                             \
    /* modified LRU specific code */                                         \
                                                                             \
    /* remove the entry from the LRU list. */                                \
                                                                             \
    H5C__DLL_REMOVE((entry_ptr), (cache_ptr)->LRU_head_ptr,                  \
                    (cache_ptr)->LRU_tail_ptr, (cache_ptr)->LRU_list_len,    \
                    (cache_ptr)->LRU_list_size, (fail_val))                  \
                                                                             \
    /* If the entry is clean when it is evicted, it should be on the         \
     * clean LRU list, if it was dirty, it should be on the dirty LRU list.  \
     * Remove it from the appropriate list according to the value of the     \
     * dirty flag.                                                           \
     */                                                                      \
                                                                             \
    if ( (entry_ptr)->is_dirty ) {                                           \
                                                                             \
        H5C__AUX_DLL_REMOVE((entry_ptr), (cache_ptr)->dLRU_head_ptr,         \
                            (cache_ptr)->dLRU_tail_ptr,                      \
                            (cache_ptr)->dLRU_list_len,                      \
                            (cache_ptr)->dLRU_list_size, (fail_val))         \
    } else {                                                                 \
        H5C__AUX_DLL_REMOVE((entry_ptr), (cache_ptr)->cLRU_head_ptr,         \
                            (cache_ptr)->cLRU_tail_ptr,                      \
                            (cache_ptr)->cLRU_list_len,                      \
                            (cache_ptr)->cLRU_list_size, (fail_val))         \
    }                                                                        \
                                                                             \
} /* H5C__UPDATE_RP_FOR_EVICTION */

#else /* H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

#define H5C__UPDATE_RP_FOR_EVICTION(cache_ptr, entry_ptr, fail_val)          \
{                                                                            \
    HDassert( (cache_ptr) );                                                 \
    HDassert( (cache_ptr)->magic == H5C__H5C_T_MAGIC );                      \
    HDassert( (entry_ptr) );                                                 \
    HDassert( !((entry_ptr)->is_protected) );                                \
    HDassert( (entry_ptr)->size > 0 );                                       \
                                                                             \
    /* modified LRU specific code */                                         \
                                                                             \
    /* remove the entry from the LRU list. */                                \
                                                                             \
    H5C__DLL_REMOVE((entry_ptr), (cache_ptr)->LRU_head_ptr,                  \
                    (cache_ptr)->LRU_tail_ptr, (cache_ptr)->LRU_list_len,    \
                    (cache_ptr)->LRU_list_size, (fail_val))                  \
                                                                             \
} /* H5C__UPDATE_RP_FOR_EVICTION */

#endif /* H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */


/*-------------------------------------------------------------------------
 *
 * Macro:	H5C__UPDATE_RP_FOR_FLUSH
 *
 * Purpose:     Update the replacement policy data structures for a flush 
 *		of the specified cache entry.
 *
 *		At present, we only support the modified LRU policy, so 
 *		this function deals with that case unconditionally.  If
 *		we ever support other replacement policies, the function
 *		should switch on the current policy and act accordingly.
 *
 * Return:      N/A
 *
 * Programmer:  John Mainzer, 5/6/04
 *
 * Modifications:
 *
 *		JRM - 7/27/04
 *		Converted the function H5C_update_rp_for_flush() to the
 *		macro H5C__UPDATE_RP_FOR_FLUSH in an effort to squeeze
 *		a bit more performance out of the cache.
 *
 *		At least for the first cut, I am leaving the comments and
 *		white space in the macro.  If they cause dificulties with
 *		pre-processor, I'll have to remove them.
 *
 *		JRM - 7/28/04
 *		Split macro into two version, one supporting the clean and
 *		dirty LRU lists, and the other not.  Yet another attempt
 *		at optimization.
 *
 *-------------------------------------------------------------------------
 */

#if H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS

#define H5C__UPDATE_RP_FOR_FLUSH(cache_ptr, entry_ptr, fail_val)            \
{                                                                           \
    HDassert( (cache_ptr) );                                                \
    HDassert( (cache_ptr)->magic == H5C__H5C_T_MAGIC );                     \
    HDassert( (entry_ptr) );                                                \
    HDassert( !((entry_ptr)->is_protected) );                               \
    HDassert( (entry_ptr)->size > 0 );                                      \
                                                                            \
    /* modified LRU specific code */                                        \
                                                                            \
    /* remove the entry from the LRU list, and re-insert it at the head. */ \
                                                                            \
    H5C__DLL_REMOVE((entry_ptr), (cache_ptr)->LRU_head_ptr,                 \
                    (cache_ptr)->LRU_tail_ptr, (cache_ptr)->LRU_list_len,   \
                    (cache_ptr)->LRU_list_size, (fail_val))                 \
                                                                            \
    H5C__DLL_PREPEND((entry_ptr), (cache_ptr)->LRU_head_ptr,                \
                     (cache_ptr)->LRU_tail_ptr, (cache_ptr)->LRU_list_len,  \
                     (cache_ptr)->LRU_list_size, (fail_val))                \
                                                                            \
    /* since the entry is being flushed or cleared, one would think that it \
     * must be dirty -- but that need not be the case.  Use the dirty flag  \
     * to infer whether the entry is on the clean or dirty LRU list, and    \
     * remove it.  Then insert it at the head of the clean LRU list.        \
     *                                                                      \
     * The function presumes that a dirty entry will be either cleared or   \
     * flushed shortly, so it is OK if we put a dirty entry on the clean    \
     * LRU list.                                                            \
     */                                                                     \
                                                                            \
    if ( (entry_ptr)->is_dirty ) {                                          \
        H5C__AUX_DLL_REMOVE((entry_ptr), (cache_ptr)->dLRU_head_ptr,        \
                            (cache_ptr)->dLRU_tail_ptr,                     \
                            (cache_ptr)->dLRU_list_len,                     \
                            (cache_ptr)->dLRU_list_size, (fail_val))        \
    } else {                                                                \
        H5C__AUX_DLL_REMOVE((entry_ptr), (cache_ptr)->cLRU_head_ptr,        \
                            (cache_ptr)->cLRU_tail_ptr,                     \
                            (cache_ptr)->cLRU_list_len,                     \
                            (cache_ptr)->cLRU_list_size, (fail_val))        \
    }                                                                       \
                                                                            \
    H5C__AUX_DLL_PREPEND((entry_ptr), (cache_ptr)->cLRU_head_ptr,           \
                         (cache_ptr)->cLRU_tail_ptr,                        \
                         (cache_ptr)->cLRU_list_len,                        \
                         (cache_ptr)->cLRU_list_size, (fail_val))           \
                                                                            \
    /* End modified LRU specific code. */                                   \
                                                                            \
} /* H5C__UPDATE_RP_FOR_FLUSH */

#else /* H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

#define H5C__UPDATE_RP_FOR_FLUSH(cache_ptr, entry_ptr, fail_val)            \
{                                                                           \
    HDassert( (cache_ptr) );                                                \
    HDassert( (cache_ptr)->magic == H5C__H5C_T_MAGIC );                     \
    HDassert( (entry_ptr) );                                                \
    HDassert( !((entry_ptr)->is_protected) );                               \
    HDassert( (entry_ptr)->size > 0 );                                      \
                                                                            \
    /* modified LRU specific code */                                        \
                                                                            \
    /* remove the entry from the LRU list, and re-insert it at the head. */ \
                                                                            \
    H5C__DLL_REMOVE((entry_ptr), (cache_ptr)->LRU_head_ptr,                 \
                    (cache_ptr)->LRU_tail_ptr, (cache_ptr)->LRU_list_len,   \
                    (cache_ptr)->LRU_list_size, (fail_val))                 \
                                                                            \
    H5C__DLL_PREPEND((entry_ptr), (cache_ptr)->LRU_head_ptr,                \
                     (cache_ptr)->LRU_tail_ptr, (cache_ptr)->LRU_list_len,  \
                     (cache_ptr)->LRU_list_size, (fail_val))                \
                                                                            \
    /* End modified LRU specific code. */                                   \
                                                                            \
} /* H5C__UPDATE_RP_FOR_FLUSH */

#endif /* H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */


/*-------------------------------------------------------------------------
 *
 * Macro:	H5C__UPDATE_RP_FOR_INSERTION
 *
 * Purpose:     Update the replacement policy data structures for an
 *		insertion of the specified cache entry.
 *
 *		At present, we only support the modified LRU policy, so 
 *		this function deals with that case unconditionally.  If
 *		we ever support other replacement policies, the function
 *		should switch on the current policy and act accordingly.
 *
 * Return:      N/A
 *
 * Programmer:  John Mainzer, 5/17/04
 *
 * Modifications:
 *
 *		JRM - 7/27/04
 *		Converted the function H5C_update_rp_for_insertion() to the
 *		macro H5C__UPDATE_RP_FOR_INSERTION in an effort to squeeze
 *		a bit more performance out of the cache.
 *
 *		At least for the first cut, I am leaving the comments and
 *		white space in the macro.  If they cause dificulties with
 *		pre-processor, I'll have to remove them.
 *
 *		JRM - 7/28/04
 *		Split macro into two version, one supporting the clean and
 *		dirty LRU lists, and the other not.  Yet another attempt
 *		at optimization.
 *
 *-------------------------------------------------------------------------
 */

#if H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS

#define H5C__UPDATE_RP_FOR_INSERTION(cache_ptr, entry_ptr, fail_val)       \
{                                                                          \
    HDassert( (cache_ptr) );                                               \
    HDassert( (cache_ptr)->magic == H5C__H5C_T_MAGIC );                    \
    HDassert( (entry_ptr) );                                               \
    HDassert( !((entry_ptr)->is_protected) );                              \
    HDassert( (entry_ptr)->size > 0 );                                     \
                                                                           \
    /* modified LRU specific code */                                       \
                                                                           \
    /* insert the entry at the head of the LRU list. */                    \
                                                                           \
    H5C__DLL_PREPEND((entry_ptr), (cache_ptr)->LRU_head_ptr,               \
                     (cache_ptr)->LRU_tail_ptr, (cache_ptr)->LRU_list_len, \
                     (cache_ptr)->LRU_list_size, (fail_val))               \
                                                                           \
    /* insert the entry at the head of the clean or dirty LRU list as      \
     * appropriate.                                                        \
     */                                                                    \
                                                                           \
    if ( entry_ptr->is_dirty ) {                                           \
        H5C__AUX_DLL_PREPEND((entry_ptr), (cache_ptr)->dLRU_head_ptr,      \
                             (cache_ptr)->dLRU_tail_ptr,                   \
                             (cache_ptr)->dLRU_list_len,                   \
                             (cache_ptr)->dLRU_list_size, (fail_val))      \
    } else {                                                               \
        H5C__AUX_DLL_PREPEND((entry_ptr), (cache_ptr)->cLRU_head_ptr,      \
                             (cache_ptr)->cLRU_tail_ptr,                   \
                             (cache_ptr)->cLRU_list_len,                   \
                             (cache_ptr)->cLRU_list_size, (fail_val))      \
    }                                                                      \
                                                                           \
    /* End modified LRU specific code. */                                  \
                                                                           \
}

#else /* H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

#define H5C__UPDATE_RP_FOR_INSERTION(cache_ptr, entry_ptr, fail_val)       \
{                                                                          \
    HDassert( (cache_ptr) );                                               \
    HDassert( (cache_ptr)->magic == H5C__H5C_T_MAGIC );                    \
    HDassert( (entry_ptr) );                                               \
    HDassert( !((entry_ptr)->is_protected) );                              \
    HDassert( (entry_ptr)->size > 0 );                                     \
                                                                           \
    /* modified LRU specific code */                                       \
                                                                           \
    /* insert the entry at the head of the LRU list. */                    \
                                                                           \
    H5C__DLL_PREPEND((entry_ptr), (cache_ptr)->LRU_head_ptr,               \
                     (cache_ptr)->LRU_tail_ptr, (cache_ptr)->LRU_list_len, \
                     (cache_ptr)->LRU_list_size, (fail_val))               \
                                                                           \
    /* End modified LRU specific code. */                                  \
                                                                           \
}

#endif /* H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */


/*-------------------------------------------------------------------------
 *
 * Macro:	H5C__UPDATE_RP_FOR_PROTECT
 *
 * Purpose:     Update the replacement policy data structures for a 
 *		protect of the specified cache entry.
 *
 *		To do this, unlink the specified entry from any data 
 *		structures used by the replacement policy, and add the
 *		entry to the protected list.
 *
 *		At present, we only support the modified LRU policy, so 
 *		this function deals with that case unconditionally.  If
 *		we ever support other replacement policies, the function
 *		should switch on the current policy and act accordingly.
 *
 * Return:      N/A
 *
 * Programmer:  John Mainzer, 5/17/04
 *
 * Modifications:
 *
 *		JRM - 7/27/04
 *		Converted the function H5C_update_rp_for_protect() to the
 *		macro H5C__UPDATE_RP_FOR_PROTECT in an effort to squeeze
 *		a bit more performance out of the cache.
 *
 *		At least for the first cut, I am leaving the comments and
 *		white space in the macro.  If they cause dificulties with
 *		pre-processor, I'll have to remove them.
 *
 *		JRM - 7/28/04
 *		Split macro into two version, one supporting the clean and
 *		dirty LRU lists, and the other not.  Yet another attempt
 *		at optimization.
 *
 *-------------------------------------------------------------------------
 */

#if H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS

#define H5C__UPDATE_RP_FOR_PROTECT(cache_ptr, entry_ptr, fail_val)        \
{                                                                         \
    HDassert( (cache_ptr) );                                              \
    HDassert( (cache_ptr)->magic == H5C__H5C_T_MAGIC );                   \
    HDassert( (entry_ptr) );                                              \
    HDassert( !((entry_ptr)->is_protected) );                             \
    HDassert( (entry_ptr)->size > 0 );                                    \
                                                                          \
    /* modified LRU specific code */                                      \
                                                                          \
    /* remove the entry from the LRU list. */                             \
                                                                          \
    H5C__DLL_REMOVE((entry_ptr), (cache_ptr)->LRU_head_ptr,               \
                    (cache_ptr)->LRU_tail_ptr, (cache_ptr)->LRU_list_len, \
                    (cache_ptr)->LRU_list_size, (fail_val))               \
                                                                          \
    /* Similarly, remove the entry from the clean or dirty LRU list       \
     * as appropriate.                                                    \
     */                                                                   \
                                                                          \
    if ( (entry_ptr)->is_dirty ) {                                        \
                                                                          \
        H5C__AUX_DLL_REMOVE((entry_ptr), (cache_ptr)->dLRU_head_ptr,      \
                            (cache_ptr)->dLRU_tail_ptr,                   \
                            (cache_ptr)->dLRU_list_len,                   \
                            (cache_ptr)->dLRU_list_size, (fail_val))      \
                                                                          \
    } else {                                                              \
                                                                          \
        H5C__AUX_DLL_REMOVE((entry_ptr), (cache_ptr)->cLRU_head_ptr,      \
                            (cache_ptr)->cLRU_tail_ptr,                   \
                            (cache_ptr)->cLRU_list_len,                   \
                            (cache_ptr)->cLRU_list_size, (fail_val))      \
    }                                                                     \
                                                                          \
    /* End modified LRU specific code. */                                 \
                                                                          \
    /* Regardless of the replacement policy, now add the entry to the     \
     * protected list.                                                    \
     */                                                                   \
                                                                          \
    H5C__DLL_APPEND((entry_ptr), (cache_ptr)->pl_head_ptr,                \
                    (cache_ptr)->pl_tail_ptr,                             \
                    (cache_ptr)->pl_len,                                  \
                    (cache_ptr)->pl_size, (fail_val))                     \
} /* H5C__UPDATE_RP_FOR_PROTECT */

#else /* H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

#define H5C__UPDATE_RP_FOR_PROTECT(cache_ptr, entry_ptr, fail_val)        \
{                                                                         \
    HDassert( (cache_ptr) );                                              \
    HDassert( (cache_ptr)->magic == H5C__H5C_T_MAGIC );                   \
    HDassert( (entry_ptr) );                                              \
    HDassert( !((entry_ptr)->is_protected) );                             \
    HDassert( (entry_ptr)->size > 0 );                                    \
                                                                          \
    /* modified LRU specific code */                                      \
                                                                          \
    /* remove the entry from the LRU list. */                             \
                                                                          \
    H5C__DLL_REMOVE((entry_ptr), (cache_ptr)->LRU_head_ptr,               \
                    (cache_ptr)->LRU_tail_ptr, (cache_ptr)->LRU_list_len, \
                    (cache_ptr)->LRU_list_size, (fail_val))               \
                                                                          \
    /* End modified LRU specific code. */                                 \
                                                                          \
    /* Regardless of the replacement policy, now add the entry to the     \
     * protected list.                                                    \
     */                                                                   \
                                                                          \
    H5C__DLL_APPEND((entry_ptr), (cache_ptr)->pl_head_ptr,                \
                    (cache_ptr)->pl_tail_ptr,                             \
                    (cache_ptr)->pl_len,                                  \
                    (cache_ptr)->pl_size, (fail_val))                     \
} /* H5C__UPDATE_RP_FOR_PROTECT */

#endif /* H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */


/*-------------------------------------------------------------------------
 *
 * Macro:	H5C__UPDATE_RP_FOR_RENAME
 *
 * Purpose:     Update the replacement policy data structures for a 
 *		rename of the specified cache entry.
 *
 *		At present, we only support the modified LRU policy, so 
 *		this function deals with that case unconditionally.  If
 *		we ever support other replacement policies, the function
 *		should switch on the current policy and act accordingly.
 *
 * Return:      N/A
 *
 * Programmer:  John Mainzer, 5/17/04
 *
 * Modifications:
 *
 *		JRM - 7/27/04
 *		Converted the function H5C_update_rp_for_rename() to the
 *		macro H5C__UPDATE_RP_FOR_RENAME in an effort to squeeze
 *		a bit more performance out of the cache.
 *
 *		At least for the first cut, I am leaving the comments and
 *		white space in the macro.  If they cause dificulties with
 *		pre-processor, I'll have to remove them.
 *
 *		JRM - 7/28/04
 *		Split macro into two version, one supporting the clean and
 *		dirty LRU lists, and the other not.  Yet another attempt
 *		at optimization.
 *
 *-------------------------------------------------------------------------
 */

#if H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS

#define H5C__UPDATE_RP_FOR_RENAME(cache_ptr, entry_ptr, fail_val)           \
{                                                                           \
    HDassert( (cache_ptr) );                                                \
    HDassert( (cache_ptr)->magic == H5C__H5C_T_MAGIC );                     \
    HDassert( (entry_ptr) );                                                \
    HDassert( !((entry_ptr)->is_protected) );                               \
    HDassert( (entry_ptr)->size > 0 );                                      \
                                                                            \
    /* modified LRU specific code */                                        \
                                                                            \
    /* remove the entry from the LRU list, and re-insert it at the head. */ \
                                                                            \
    H5C__DLL_REMOVE((entry_ptr), (cache_ptr)->LRU_head_ptr,                 \
                    (cache_ptr)->LRU_tail_ptr, (cache_ptr)->LRU_list_len,   \
                    (cache_ptr)->LRU_list_size, (fail_val))                 \
                                                                            \
    H5C__DLL_PREPEND((entry_ptr), (cache_ptr)->LRU_head_ptr,                \
                     (cache_ptr)->LRU_tail_ptr, (cache_ptr)->LRU_list_len,  \
                     (cache_ptr)->LRU_list_size, (fail_val))                \
                                                                            \
    /* move the entry to the head of either the clean or dirty LRU list     \
     * as appropriate.                                                      \
     */                                                                     \
                                                                            \
    if ( (entry_ptr)->is_dirty ) {                                          \
                                                                            \
        H5C__AUX_DLL_REMOVE((entry_ptr), (cache_ptr)->dLRU_head_ptr,        \
                            (cache_ptr)->dLRU_tail_ptr,                     \
                            (cache_ptr)->dLRU_list_len,                     \
                            (cache_ptr)->dLRU_list_size, (fail_val))        \
                                                                            \
        H5C__AUX_DLL_PREPEND((entry_ptr), (cache_ptr)->dLRU_head_ptr,       \
                             (cache_ptr)->dLRU_tail_ptr,                    \
                             (cache_ptr)->dLRU_list_len,                    \
                             (cache_ptr)->dLRU_list_size, (fail_val))       \
                                                                            \
    } else {                                                                \
                                                                            \
        H5C__AUX_DLL_REMOVE((entry_ptr), (cache_ptr)->cLRU_head_ptr,        \
                            (cache_ptr)->cLRU_tail_ptr,                     \
                            (cache_ptr)->cLRU_list_len,                     \
                            (cache_ptr)->cLRU_list_size, (fail_val))        \
                                                                            \
        H5C__AUX_DLL_PREPEND((entry_ptr), (cache_ptr)->cLRU_head_ptr,       \
                             (cache_ptr)->cLRU_tail_ptr,                    \
                             (cache_ptr)->cLRU_list_len,                    \
                             (cache_ptr)->cLRU_list_size, (fail_val))       \
    }                                                                       \
                                                                            \
    /* End modified LRU specific code. */                                   \
                                                                            \
} /* H5C__UPDATE_RP_FOR_RENAME */

#else /* H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

#define H5C__UPDATE_RP_FOR_RENAME(cache_ptr, entry_ptr, fail_val)           \
{                                                                           \
    HDassert( (cache_ptr) );                                                \
    HDassert( (cache_ptr)->magic == H5C__H5C_T_MAGIC );                     \
    HDassert( (entry_ptr) );                                                \
    HDassert( !((entry_ptr)->is_protected) );                               \
    HDassert( (entry_ptr)->size > 0 );                                      \
                                                                            \
    /* modified LRU specific code */                                        \
                                                                            \
    /* remove the entry from the LRU list, and re-insert it at the head. */ \
                                                                            \
    H5C__DLL_REMOVE((entry_ptr), (cache_ptr)->LRU_head_ptr,                 \
                    (cache_ptr)->LRU_tail_ptr, (cache_ptr)->LRU_list_len,   \
                    (cache_ptr)->LRU_list_size, (fail_val))                 \
                                                                            \
    H5C__DLL_PREPEND((entry_ptr), (cache_ptr)->LRU_head_ptr,                \
                     (cache_ptr)->LRU_tail_ptr, (cache_ptr)->LRU_list_len,  \
                     (cache_ptr)->LRU_list_size, (fail_val))                \
                                                                            \
    /* End modified LRU specific code. */                                   \
                                                                            \
} /* H5C__UPDATE_RP_FOR_RENAME */

#endif /* H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */


/*-------------------------------------------------------------------------
 *
 * Macro:	H5C__UPDATE_RP_FOR_UNPROTECT
 *
 * Purpose:     Update the replacement policy data structures for an
 *		unprotect of the specified cache entry.
 *
 *		To do this, unlink the specified entry from the protected
 *		list, and re-insert it in the data structures used by the
 *		current replacement policy.
 *
 *		At present, we only support the modified LRU policy, so 
 *		this function deals with that case unconditionally.  If
 *		we ever support other replacement policies, the function
 *		should switch on the current policy and act accordingly.
 *
 * Return:      N/A
 *
 * Programmer:  John Mainzer, 5/19/04
 *
 * Modifications:
 *
 *		JRM - 7/27/04
 *		Converted the function H5C_update_rp_for_unprotect() to 
 *		the macro H5C__UPDATE_RP_FOR_UNPROTECT in an effort to 
 *		squeeze a bit more performance out of the cache.
 *
 *		At least for the first cut, I am leaving the comments and
 *		white space in the macro.  If they cause dificulties with
 *		pre-processor, I'll have to remove them.
 *
 *		JRM - 7/28/04
 *		Split macro into two version, one supporting the clean and
 *		dirty LRU lists, and the other not.  Yet another attempt
 *		at optimization.
 *
 *-------------------------------------------------------------------------
 */

#if H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS

#define H5C__UPDATE_RP_FOR_UNPROTECT(cache_ptr, entry_ptr, fail_val)   \
{                                                                      \
    HDassert( (cache_ptr) );                                           \
    HDassert( (cache_ptr)->magic == H5C__H5C_T_MAGIC );                \
    HDassert( (entry_ptr) );                                           \
    HDassert( (entry_ptr)->is_protected);                              \
    HDassert( (entry_ptr)->size > 0 );                                 \
                                                                       \
    /* Regardless of the replacement policy, remove the entry from the \
     * protected list.                                                 \
     */                                                                \
    H5C__DLL_REMOVE((entry_ptr), (cache_ptr)->pl_head_ptr,             \
                    (cache_ptr)->pl_tail_ptr, (cache_ptr)->pl_len,     \
                    (cache_ptr)->pl_size, (fail_val))                  \
                                                                       \
    /* modified LRU specific code */                                   \
                                                                       \
    /* insert the entry at the head of the LRU list. */                \
                                                                       \
    H5C__DLL_PREPEND((entry_ptr), (cache_ptr)->LRU_head_ptr,           \
                     (cache_ptr)->LRU_tail_ptr,                        \
                     (cache_ptr)->LRU_list_len,                        \
                     (cache_ptr)->LRU_list_size, (fail_val))           \
                                                                       \
    /* Similarly, insert the entry at the head of either the clean or  \
     * dirty LRU list as appropriate.                                  \
     */                                                                \
                                                                       \
    if ( (entry_ptr)->is_dirty ) {                                     \
                                                                       \
        H5C__AUX_DLL_PREPEND((entry_ptr), (cache_ptr)->dLRU_head_ptr,  \
                             (cache_ptr)->dLRU_tail_ptr,               \
                             (cache_ptr)->dLRU_list_len,               \
                             (cache_ptr)->dLRU_list_size, (fail_val))  \
                                                                       \
    } else {                                                           \
                                                                       \
        H5C__AUX_DLL_PREPEND((entry_ptr), (cache_ptr)->cLRU_head_ptr,  \
                             (cache_ptr)->cLRU_tail_ptr,               \
                             (cache_ptr)->cLRU_list_len,               \
                             (cache_ptr)->cLRU_list_size, (fail_val))  \
    }                                                                  \
                                                                       \
    /* End modified LRU specific code. */                              \
                                                                       \
} /* H5C__UPDATE_RP_FOR_UNPROTECT */

#else /* H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

#define H5C__UPDATE_RP_FOR_UNPROTECT(cache_ptr, entry_ptr, fail_val)   \
{                                                                      \
    HDassert( (cache_ptr) );                                           \
    HDassert( (cache_ptr)->magic == H5C__H5C_T_MAGIC );                \
    HDassert( (entry_ptr) );                                           \
    HDassert( (entry_ptr)->is_protected);                              \
    HDassert( (entry_ptr)->size > 0 );                                 \
                                                                       \
    /* Regardless of the replacement policy, remove the entry from the \
     * protected list.                                                 \
     */                                                                \
    H5C__DLL_REMOVE((entry_ptr), (cache_ptr)->pl_head_ptr,             \
                    (cache_ptr)->pl_tail_ptr, (cache_ptr)->pl_len,     \
                    (cache_ptr)->pl_size, (fail_val))                  \
                                                                       \
    /* modified LRU specific code */                                   \
                                                                       \
    /* insert the entry at the head of the LRU list. */                \
                                                                       \
    H5C__DLL_PREPEND((entry_ptr), (cache_ptr)->LRU_head_ptr,           \
                     (cache_ptr)->LRU_tail_ptr,                        \
                     (cache_ptr)->LRU_list_len,                        \
                     (cache_ptr)->LRU_list_size, (fail_val))           \
                                                                       \
    /* End modified LRU specific code. */                              \
                                                                       \
} /* H5C__UPDATE_RP_FOR_UNPROTECT */

#endif /* H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */


/*
 * Private file-scope variables.
 */

/* Declare a free list to manage the H5C_t struct */
H5FL_DEFINE_STATIC(H5C_t);

/*
 * Private file-scope function declarations:
 */

static herr_t H5C__auto_adjust_cache_size(H5C_t * cache_ptr,
                                          H5F_t * f,
                                          hid_t primary_dxpl_id,
                                          hid_t secondary_dxpl_id,
                                          hbool_t write_permitted,
                                          hbool_t * first_flush_ptr);

static herr_t H5C__autoadjust__ageout(H5C_t * cache_ptr,
                                      double hit_rate,
                                      enum H5C_resize_status * status_ptr,
                                      size_t * new_max_cache_size_ptr,
                                      H5F_t * f,
                                      hid_t primary_dxpl_id,
                                      hid_t secondary_dxpl_id,
                                      hbool_t write_permitted,
                                      hbool_t * first_flush_ptr);

static herr_t H5C__autoadjust__ageout__cycle_epoch_marker(H5C_t * cache_ptr);

static herr_t H5C__autoadjust__ageout__evict_aged_out_entries(H5F_t * f,
                                                    hid_t primary_dxpl_id,
                                                    hid_t secondary_dxpl_id,
                                                    H5C_t * cache_ptr,
                                                    hbool_t write_permitted,
                                                    hbool_t * first_flush_ptr);

static herr_t H5C__autoadjust__ageout__insert_new_marker(H5C_t * cache_ptr);

static herr_t H5C__autoadjust__ageout__remove_all_markers(H5C_t * cache_ptr);

static herr_t H5C__autoadjust__ageout__remove_excess_markers(H5C_t * cache_ptr);

static herr_t H5C_flush_single_entry(H5F_t *             f,
                                     hid_t               primary_dxpl_id,
                                     hid_t               secondary_dxpl_id,
                                     H5C_t *             cache_ptr,
                                     const H5C_class_t * type_ptr,
                                     haddr_t             addr,
                                     unsigned            flags,
                                     hbool_t *           first_flush_ptr,
                                     hbool_t    del_entry_from_slist_on_destroy);

static void * H5C_load_entry(H5F_t *             f,
                             hid_t               dxpl_id,
                             const H5C_class_t * type,
                             haddr_t             addr,
                             const void *        udata1,
                             void *              udata2,
                             hbool_t             skip_file_checks);

static herr_t H5C_make_space_in_cache(H5F_t * f,
                                      hid_t   primary_dxpl_id,
                                      hid_t   secondary_dxpl_id,
                                      H5C_t * cache_ptr,
                                      size_t  space_needed,
                                      hbool_t write_permitted,
                                      hbool_t * first_flush_ptr);


/****************************************************************************
 *
 * #defines and declarations for epoch marker cache entries.
 *
 * As a strategy for automatic cache size reduction, the cache may insert
 * marker entries in the LRU list at the end of each epoch.  These markers
 * are then used to identify entries that have not been accessed for n
 * epochs so that they can be evicted from the cache.
 *
 ****************************************************************************/

/* Note that H5C__MAX_EPOCH_MARKERS is defined in H5Cpkg.h, not here because
 * it is needed to dimension an array in H5C_t.
 */

#define H5C__EPOCH_MARKER_TYPE	H5C__MAX_NUM_TYPE_IDS

static void *H5C_epoch_marker_load(H5F_t *f, hid_t dxpl_id, haddr_t addr,
                                   const void *udata1, void *udata2);
static herr_t H5C_epoch_marker_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest,
                                     haddr_t addr, void *thing);
static herr_t H5C_epoch_marker_dest(H5F_t *f, void *thing);
static herr_t H5C_epoch_marker_clear(H5F_t *f, void *thing, hbool_t dest);
static herr_t H5C_epoch_marker_size(H5F_t *f, void *thing, size_t *size_ptr);

const H5C_class_t epoch_marker_class =
{
    /* id    = */ H5C__EPOCH_MARKER_TYPE,
    /* load  = */ &H5C_epoch_marker_load,
    /* flush = */ &H5C_epoch_marker_flush,
    /* dest  = */ &H5C_epoch_marker_dest,
    /* clear = */ &H5C_epoch_marker_clear,
    /* size  = */ &H5C_epoch_marker_size
};

/***************************************************************************
 * Class functions for H5C__EPOCH_MAKER_TYPE:
 *
 * None of these functions should ever be called, so there is no point in
 * documenting them separately.
 *                                                     JRM - 11/16/04
 *
 ***************************************************************************/

static void *
H5C_epoch_marker_load(UNUSED H5F_t *f, 
                      UNUSED hid_t dxpl_id, 
                      UNUSED haddr_t addr,
                      UNUSED const void *udata1, 
                      UNUSED void *udata2)
{
    void * ret_value = NULL;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_epoch_marker_load, NULL)

    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, NULL, "called unreachable fcn.")

done:

    FUNC_LEAVE_NOAPI(ret_value)
}

static herr_t 
H5C_epoch_marker_flush(UNUSED H5F_t *f, 
                       UNUSED hid_t dxpl_id, 
                       UNUSED hbool_t dest,
                       UNUSED haddr_t addr, 
                       UNUSED void *thing)
{
    herr_t ret_value = FAIL;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_epoch_marker_flush, FAIL)

    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "called unreachable fcn.")

done:

    FUNC_LEAVE_NOAPI(ret_value)
}

static herr_t 
H5C_epoch_marker_dest(UNUSED H5F_t *f, 
                      UNUSED void *thing)
{
    herr_t ret_value = FAIL;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_epoch_marker_dest, FAIL)

    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "called unreachable fcn.")

done:

    FUNC_LEAVE_NOAPI(ret_value)
}

static herr_t 
H5C_epoch_marker_clear(UNUSED H5F_t *f, 
                       UNUSED void *thing, 
                       UNUSED hbool_t dest)
{
    herr_t ret_value = FAIL;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_epoch_marker_clear, FAIL)

    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "called unreachable fcn.")

done:

    FUNC_LEAVE_NOAPI(ret_value)
}

static herr_t 
H5C_epoch_marker_size(UNUSED H5F_t *f, 
                      UNUSED void *thing, 
                      UNUSED size_t *size_ptr)
{
    herr_t ret_value = FAIL;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_epoch_marker_size, FAIL)

    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "called unreachable fcn.")

done:

    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:    H5C_create
 *
 * Purpose:     Allocate, initialize, and return the address of a new 
 *		instance of H5C_t.
 *
 *		In general, the max_cache_size parameter must be positive, 
 *		and the min_clean_size parameter must lie in the closed 
 *		interval [0, max_cache_size]. 
 *
 *		The check_write_permitted parameter must either be NULL,
 *		or point to a function of type H5C_write_permitted_func_t.
 *		If it is NULL, the cache will presume that writes are 
 *		always permitted.
 *
 * Return:      Success:        Pointer to the new instance.
 *
 *              Failure:        NULL
 *
 * Programmer:  John Mainzer
 *              6/2/04
 *
 * Modifications:
 *
 *		JRM -- 7/20/04
 *		Updated for the addition of the hash table.
 *
 *		JRM -- 10/5/04
 *		Added call to H5C_reset_cache_hit_rate_stats().  Also
 *		added initialization for cache_is_full flag and for 
 *		resize_ctl.
 *
 *		JRM -- 11/12/04
 *		Added initialization for the new size_decreased field.
 *
 *		JRM -- 11/17/04
 *		Added/updated initialization for the automatic cache
 *		size control data structures.
 *
 *-------------------------------------------------------------------------
 */

H5C_t *
H5C_create(size_t		      max_cache_size,
           size_t		      min_clean_size,
           int			      max_type_id,
           const char *		      (* type_name_table_ptr),
           H5C_write_permitted_func_t check_write_permitted)
{
    int i;
    H5C_t * cache_ptr = NULL;
    H5C_t * ret_value = NULL;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_create, NULL)

    HDassert( max_cache_size > 0 );
    HDassert( min_clean_size <= max_cache_size );

    HDassert( max_type_id >= 0 );
    HDassert( max_type_id < H5C__MAX_NUM_TYPE_IDS );
    HDassert( type_name_table_ptr );

    for ( i = 0; i <= max_type_id; i++ ) {

        HDassert( (type_name_table_ptr)[i] );
        HDassert( HDstrlen(( type_name_table_ptr)[i]) > 0 );
    }


    if ( NULL == (cache_ptr = H5FL_CALLOC(H5C_t)) ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, \
                    "memory allocation failed")
    }

    if ( (cache_ptr->slist_ptr = H5SL_create(H5SL_TYPE_HADDR,0.5,16)) 
         == NULL ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTCREATE, NULL, "can't create skip list.")
    }

    /* If we get this far, we should succeed.  Go ahead and initialize all
     * the fields.
     */

    cache_ptr->magic 				= H5C__H5C_T_MAGIC;

    cache_ptr->max_type_id			= max_type_id;
    cache_ptr->type_name_table_ptr		= type_name_table_ptr;

    cache_ptr->max_cache_size			= max_cache_size;
    cache_ptr->min_clean_size			= min_clean_size;

    cache_ptr->check_write_permitted		= check_write_permitted;

    cache_ptr->index_len			= 0;
    cache_ptr->index_size			= (size_t)0;

    cache_ptr->slist_len			= 0;
    cache_ptr->slist_size			= (size_t)0;

    for ( i = 0; i < H5C__HASH_TABLE_LEN; i++ )
    {
        (cache_ptr->index)[i] = NULL;
    }

    cache_ptr->pl_len				= 0;
    cache_ptr->pl_size				= (size_t)0;
    cache_ptr->pl_head_ptr			= NULL;
    cache_ptr->pl_tail_ptr			= NULL;

    cache_ptr->LRU_list_len			= 0;
    cache_ptr->LRU_list_size			= (size_t)0;
    cache_ptr->LRU_head_ptr			= NULL;
    cache_ptr->LRU_tail_ptr			= NULL;

    cache_ptr->cLRU_list_len			= 0;
    cache_ptr->cLRU_list_size			= (size_t)0;
    cache_ptr->cLRU_head_ptr			= NULL;
    cache_ptr->cLRU_tail_ptr			= NULL;

    cache_ptr->dLRU_list_len			= 0;
    cache_ptr->dLRU_list_size			= (size_t)0;
    cache_ptr->dLRU_head_ptr			= NULL;
    cache_ptr->dLRU_tail_ptr			= NULL;

    cache_ptr->size_increase_possible		= FALSE;
    cache_ptr->size_decrease_possible		= FALSE;
    cache_ptr->resize_enabled			= FALSE;
    cache_ptr->cache_full			= FALSE;
    cache_ptr->size_decreased			= FALSE;

    (cache_ptr->resize_ctl).version		= H5C__CURR_AUTO_SIZE_CTL_VER;
    (cache_ptr->resize_ctl).rpt_fcn		= NULL;
    (cache_ptr->resize_ctl).set_initial_size	= FALSE;
    (cache_ptr->resize_ctl).initial_size	= H5C__DEF_AR_INIT_SIZE;
    (cache_ptr->resize_ctl).min_clean_fraction	= H5C__DEF_AR_MIN_CLEAN_FRAC;
    (cache_ptr->resize_ctl).max_size		= H5C__DEF_AR_MAX_SIZE;
    (cache_ptr->resize_ctl).min_size		= H5C__DEF_AR_MIN_SIZE;
    (cache_ptr->resize_ctl).epoch_length	= H5C__DEF_AR_EPOCH_LENGTH;

    (cache_ptr->resize_ctl).incr_mode		= H5C_incr__off;
    (cache_ptr->resize_ctl).lower_hr_threshold	= H5C__DEF_AR_LOWER_THRESHHOLD;
    (cache_ptr->resize_ctl).increment	        = H5C__DEF_AR_INCREMENT;
    (cache_ptr->resize_ctl).apply_max_increment	= TRUE;
    (cache_ptr->resize_ctl).max_increment	= H5C__DEF_AR_MAX_INCREMENT;

    (cache_ptr->resize_ctl).decr_mode		= H5C_decr__off;
    (cache_ptr->resize_ctl).upper_hr_threshold	= H5C__DEF_AR_UPPER_THRESHHOLD;
    (cache_ptr->resize_ctl).decrement	        = H5C__DEF_AR_DECREMENT;
    (cache_ptr->resize_ctl).apply_max_decrement	= TRUE;
    (cache_ptr->resize_ctl).max_decrement	= H5C__DEF_AR_MAX_DECREMENT;
    (cache_ptr->resize_ctl).epochs_before_eviction = H5C__DEF_AR_EPCHS_B4_EVICT;
    (cache_ptr->resize_ctl).apply_empty_reserve = TRUE;
    (cache_ptr->resize_ctl).empty_reserve	= H5C__DEF_AR_EMPTY_RESERVE;

    cache_ptr->epoch_markers_active		= 0;

    /* no need to initialize the ring buffer itself */
    cache_ptr->epoch_marker_ringbuf_first	= 1;
    cache_ptr->epoch_marker_ringbuf_last	= 0;
    cache_ptr->epoch_marker_ringbuf_size	= 0;

    for ( i = 0; i < H5C__MAX_EPOCH_MARKERS; i++ )
    {
        (cache_ptr->epoch_marker_active)[i]		= FALSE;

        ((cache_ptr->epoch_markers)[i]).addr		= (haddr_t)i;
        ((cache_ptr->epoch_markers)[i]).size		= (size_t)0;
        ((cache_ptr->epoch_markers)[i]).type		= &epoch_marker_class;
        ((cache_ptr->epoch_markers)[i]).is_dirty	= FALSE;
        ((cache_ptr->epoch_markers)[i]).is_protected	= FALSE;
        ((cache_ptr->epoch_markers)[i]).in_slist	= FALSE;
        ((cache_ptr->epoch_markers)[i]).ht_next		= NULL;
        ((cache_ptr->epoch_markers)[i]).ht_prev		= NULL;
        ((cache_ptr->epoch_markers)[i]).next		= NULL;
        ((cache_ptr->epoch_markers)[i]).prev		= NULL;
        ((cache_ptr->epoch_markers)[i]).aux_next	= NULL;
        ((cache_ptr->epoch_markers)[i]).aux_prev	= NULL;
#if H5C_COLLECT_CACHE_ENTRY_STATS
        ((cache_ptr->epoch_markers)[i]).accesses	= 0;
        ((cache_ptr->epoch_markers)[i]).clears		= 0;
        ((cache_ptr->epoch_markers)[i]).flushes		= 0;
#endif /* H5C_COLLECT_CACHE_ENTRY_STATS */
    }

    if ( H5C_reset_cache_hit_rate_stats(cache_ptr) != SUCCEED ) {

        /* this should be impossible... */
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, NULL, \
                    "H5C_reset_cache_hit_rate_stats failed.")
    }

    H5C_stats__reset(cache_ptr);

    cache_ptr->skip_file_checks			= FALSE;
    cache_ptr->skip_dxpl_id_checks		= FALSE;

    /* Set return value */
    ret_value = cache_ptr;

done:

    if ( ret_value == 0 ) {

        if ( cache_ptr != NULL ) {

            if ( cache_ptr->slist_ptr != NULL )
                H5SL_close(cache_ptr->slist_ptr);

            cache_ptr->magic = 0;
            H5FL_FREE(H5C_t, cache_ptr);
            cache_ptr = NULL;

        } /* end if */

    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_create() */


/*-------------------------------------------------------------------------
 * Function:    H5C_def_auto_resize_rpt_fcn
 *
 * Purpose:     Print results of a automatic cache resize.
 *
 *		This function should only be used where HDprintf() behaves
 *		well -- i.e. not on Windows.
 *		
 * Return:      void
 *
 * Programmer:  John Mainzer
 *		10/27/04
 *
 * Modifications:
 *
 *		JRM -- 11/22/04
 *		Reworked function to adapt it to the addition of the 
 *		ageout method of cache size reduction.
 *
 *-------------------------------------------------------------------------
 */
void 
H5C_def_auto_resize_rpt_fcn(H5C_t * cache_ptr,
                            int32_t version,
                            double hit_rate,
                            enum H5C_resize_status status,
                            size_t old_max_cache_size,
                            size_t new_max_cache_size,
                            size_t old_min_clean_size,
                            size_t new_min_clean_size)
{
    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( version == H5C__CURR_AUTO_RESIZE_RPT_FCN_VER );

    switch ( status )
    {
        case in_spec:
            HDfprintf(stdout, "Auto cache resize -- no change. ");
            HDfprintf(stdout, "(hit rate = %lf)\n", hit_rate);
            break;

        case increase:
            HDassert( hit_rate < (cache_ptr->resize_ctl).lower_hr_threshold );
            HDassert( old_max_cache_size < new_max_cache_size );

            HDfprintf(stdout, "Auto cache resize -- hit rate (%lf) ", hit_rate);

            HDfprintf(stdout, "out of bounds low (%6.5lf).\n",
                      (cache_ptr->resize_ctl).lower_hr_threshold);

            HDfprintf(stdout, 
                      "	cache size increased from (%Zu/%Zu) to (%Zu/%Zu).\n",
                      old_max_cache_size,
                      old_min_clean_size,
                      new_max_cache_size,
                      new_min_clean_size);
            break;

        case decrease:
            HDassert( old_max_cache_size > new_max_cache_size );

            switch ( (cache_ptr->resize_ctl).decr_mode )
            {
                case H5C_decr__threshold:
                    HDassert( hit_rate >
                              (cache_ptr->resize_ctl).upper_hr_threshold );

                    HDfprintf(stdout, 
                              "Auto cache resize -- decrease by threshold.  ");

                    HDfprintf(stdout, "HR = %lf > %6.5lf\n", 
                              hit_rate, 
                              (cache_ptr->resize_ctl).upper_hr_threshold);

                    HDfprintf(stdout, "out of bounds high (%6.5lf).\n",
                              (cache_ptr->resize_ctl).upper_hr_threshold);
                    break;

                case H5C_decr__age_out:
                    HDfprintf(stdout, 
                              "Auto cache resize -- decrease by ageout. ");
                    HDfprintf(stdout, "HR = %lf\n", hit_rate);
                    break;

                case H5C_decr__age_out_with_threshold:
                    HDassert( hit_rate >
                              (cache_ptr->resize_ctl).upper_hr_threshold );

                    HDfprintf(stdout, 
                              "Auto cache resize -- decrease by ageout with ");
                    HDfprintf(stdout, "threshold. HR = %lf > %6.5lf\n",
                              hit_rate, 
                              (cache_ptr->resize_ctl).upper_hr_threshold);
                    break;

                default:
                    HDfprintf(stdout, 
                              "Auto cache resize -- decrease by unknown mode.");
                    HDfprintf(stdout, " HR = %lf\n", hit_rate);
            }

            HDfprintf(stdout, 
                      "	cache size decreased from (%Zu/%Zu) to (%Zu/%Zu).\n",
                      old_max_cache_size,
                      old_min_clean_size,
                      new_max_cache_size,
                      new_min_clean_size);
            break;

        case at_max_size:
            HDfprintf(stdout, "Auto cache resize -- hit rate (%lf) ", hit_rate);
            HDfprintf(stdout, "out of bounds low (%6.5lf).\n",
                      (cache_ptr->resize_ctl).lower_hr_threshold);
            HDfprintf(stdout, "	cache already at maximum size so no change.\n");
            break;

        case at_min_size:
            HDfprintf(stdout, "Auto cache resize -- hit rate (%lf) ", hit_rate);
            HDfprintf(stdout, "-- can't decrease.\n");
            HDfprintf(stdout, "	cache already at minimum size.\n");
            break;

        case increase_disabled:
            HDfprintf(stdout, "Auto cache resize -- increase disabled -- ");
            HDfprintf(stdout, "HR = %lf.", hit_rate);
            break;

        case decrease_disabled:
            HDfprintf(stdout, "Auto cache resize -- decrease disabled -- ");
            HDfprintf(stdout, "HR = %lf.\n", hit_rate);
            break;

        case not_full:
            HDassert( hit_rate < (cache_ptr->resize_ctl).lower_hr_threshold );

            HDfprintf(stdout, "Auto cache resize -- hit rate (%lf) ", hit_rate);
            HDfprintf(stdout, "out of bounds low (%6.5lf).\n",
                      (cache_ptr->resize_ctl).lower_hr_threshold);
            HDfprintf(stdout, "	cache not full so no increase in size.\n");
            break;

        default:
            HDfprintf(stdout, "Auto cache resize -- unknown status code.\n");
            break;
    }

    return;

} /* H5C_def_auto_resize_rpt_fcn() */


/*-------------------------------------------------------------------------
 * Function:    H5C_dest
 *
 * Purpose:     Flush all data to disk and destroy the cache.
 *
 *              This function fails if any object are protected since the
 *              resulting file might not be consistent.
 *
 *		The primary_dxpl_id and secondary_dxpl_id parameters 
 *		specify the dxpl_ids used on the first write occasioned
 *		by the destroy (primary_dxpl_id), and on all subsequent 
 *		writes (secondary_dxpl_id).  This is useful in the metadata 
 *		cache, but may not be needed elsewhere.  If so, just use the 
 *		same dxpl_id for both parameters.
 *
 *		Note that *cache_ptr has been freed upon successful return.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *		6/2/04
 *
 * Modifications:
 *
 *              None.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_dest(H5F_t * f, 
         hid_t	 primary_dxpl_id, 
         hid_t	 secondary_dxpl_id, 
         H5C_t * cache_ptr)
{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_dest, FAIL)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->skip_file_checks || f );

    if ( H5C_flush_cache(f, primary_dxpl_id, secondary_dxpl_id, 
                         cache_ptr, H5F_FLUSH_INVALIDATE) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush cache")
    }

    if ( cache_ptr->slist_ptr != NULL ) {

        H5SL_close(cache_ptr->slist_ptr);
        cache_ptr->slist_ptr = NULL;
    }

    cache_ptr->magic = 0;

    H5FL_FREE(H5C_t, cache_ptr);

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_dest() */


/*-------------------------------------------------------------------------
 * Function:    H5C_dest_empty
 *
 * Purpose:     Destroy an empty cache.
 *
 *              This function fails if the cache is not empty on entry.
 *
 *		Note that *cache_ptr has been freed upon successful return.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *		6/2/04
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_dest_empty(H5C_t * cache_ptr)
{
    herr_t ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_dest_empty, FAIL)

    /* This would normally be an assert, but we need to use an HGOTO_ERROR
     * call to shut up the compiler.
     */
    if ( ( ! cache_ptr ) || 
         ( cache_ptr->magic != H5C__H5C_T_MAGIC ) || 
         ( cache_ptr->index_len != 0 ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Bad cache_ptr or non-empty cache on entry.")
    }


    if ( cache_ptr->slist_ptr != NULL ) {

        H5SL_close(cache_ptr->slist_ptr);
        cache_ptr->slist_ptr = NULL;
    }

    cache_ptr->magic = 0;

    H5FL_FREE(H5C_t, cache_ptr);

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_dest_empty() */


/*-------------------------------------------------------------------------
 * Function:    H5C_flush_cache
 *
 * Purpose:	Flush (and possibly destroy) the entries contained in the
 *		specified cache.
 *
 *		If the cache contains protected entries, the function will
 *		fail, as protected entries cannot be flushed.  However 
 *		all unprotected entries should be flushed before the 
 *		function returns failure.
 *
 *		The primary_dxpl_id and secondary_dxpl_id parameters 
 *		specify the dxpl_ids used on the first write occasioned
 *		by the flush (primary_dxpl_id), and on all subsequent 
 *		writes (secondary_dxpl_id).  This is useful in the metadata 
 *		cache, but may not be needed elsewhere.  If so, just use the 
 *		same dxpl_id for both parameters.
 *		
 * Return:      Non-negative on success/Negative on failure or if there was 
 *		a request to flush all items and something was protected.
 *
 * Programmer:  John Mainzer
 *		6/2/04
 *
 * Modifications:
 *
 *		JRM -- 7/20/04
 *		Modified the function for the addition of the hash table.
 *
 *		JRM -- 11/22/04
 *		Added code to remove all epoch markers (if any) from the
 *		LRU list before a destroy.  Strictly speaking, this isn't
 *		necessary, as the marker entries reside only in the LRU
 *		list, never in the index or in the tree.  However, it 
 *		never hurts to tidy up.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_flush_cache(H5F_t *  f, 
                hid_t    primary_dxpl_id, 
                hid_t    secondary_dxpl_id, 
                H5C_t *  cache_ptr,
                unsigned flags)
{
    herr_t              status;
    herr_t		ret_value = SUCCEED;
    hbool_t             destroy = ( (flags & H5F_FLUSH_INVALIDATE) != 0 );
    hbool_t		first_flush = TRUE;
    int32_t		protected_entries = 0;
    int32_t		i;
    H5SL_node_t * 	node_ptr;
    H5C_cache_entry_t *	entry_ptr;
#if H5C_DO_SANITY_CHECKS
    int32_t		actual_slist_len = 0;
    size_t              actual_slist_size = 0;
#endif /* H5C_DO_SANITY_CHECKS */

    FUNC_ENTER_NOAPI(H5C_flush_cache, FAIL)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->skip_file_checks || f );
    HDassert( cache_ptr->slist_ptr );

    if ( ( destroy ) && ( cache_ptr->epoch_markers_active > 0 ) ) {

        status = H5C__autoadjust__ageout__remove_all_markers(cache_ptr);

        if ( status != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "error removing all epoch markers.")
        }
    }


    if ( cache_ptr->slist_len == 0 ) {

        node_ptr = NULL;
        HDassert( cache_ptr->slist_size == 0 );

    } else {

        node_ptr = H5SL_first(cache_ptr->slist_ptr);
    }

    while ( node_ptr != NULL )
    {
        entry_ptr = (H5C_cache_entry_t *)H5SL_item(node_ptr);

        HDassert( entry_ptr != NULL );
        HDassert( entry_ptr->in_slist );

#if H5C_DO_SANITY_CHECKS
        actual_slist_len++;
        actual_slist_size += entry_ptr->size;
#endif /* H5C_DO_SANITY_CHECKS */

        if ( entry_ptr->is_protected ) {

            /* we have major problems -- but lets flush everything 
             * we can before we flag an error.
             */
	    protected_entries++;

        } else { 

            status = H5C_flush_single_entry(f, 
                                            primary_dxpl_id, 
                                            secondary_dxpl_id, 
                                            cache_ptr,
                                            NULL, 
                                            entry_ptr->addr, 
                                            flags, 
                                            &first_flush, 
                                            FALSE);
            if ( status < 0 ) {

                /* This shouldn't happen -- if it does, we are toast so
                 * just scream and die.
                 */
                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, \
                            "Can't flush entry.")
            }
        }

        node_ptr = H5SL_next(node_ptr);

    } /* while */

#if H5C_DO_SANITY_CHECKS
    HDassert( actual_slist_len == cache_ptr->slist_len );
    HDassert( actual_slist_size == cache_ptr->slist_size );
#endif /* H5C_DO_SANITY_CHECKS */

    if ( destroy ) {

        if(cache_ptr->slist_ptr) {

            /* Release all nodes from skip list, but keep list active */
            H5SL_release(cache_ptr->slist_ptr);

        }
        cache_ptr->slist_len = 0;
        cache_ptr->slist_size = 0;

        /* Since we are doing a destroy, we must make a pass through
         * the hash table and flush all entries that remain.  Note that
         * all remaining entries entries must be clean, so this will
         * not result in any writes to disk.
         */
        for ( i = 0; i < H5C__HASH_TABLE_LEN; i++ )
        {
            while ( cache_ptr->index[i] )
            {
                entry_ptr = cache_ptr->index[i];

                if ( entry_ptr->is_protected ) {

                    /* we have major problems -- but lets flush and destroy
                     * everything we can before we flag an error.
                     */
                    
                    H5C__DELETE_FROM_INDEX(cache_ptr, entry_ptr)

                    if ( !entry_ptr->in_slist ) {

	                protected_entries++;
                        HDassert( !(entry_ptr->is_dirty) );
                    }
                } else {

                    HDassert( !(entry_ptr->is_dirty) );
                    HDassert( !(entry_ptr->in_slist) );

                    status = H5C_flush_single_entry(f, 
                                                    primary_dxpl_id, 
                                                    secondary_dxpl_id, 
                                                    cache_ptr,
                                                    NULL, 
                                                    entry_ptr->addr, 
                                                    flags, 
                                                    &first_flush, 
                                                    FALSE);
                    if ( status < 0 ) {

                        /* This shouldn't happen -- if it does, we are toast so
                         * just scream and die.
                         */
                        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, \
                                    "Can't flush entry.")
                    }
                }
            }
        }

        HDassert( protected_entries == cache_ptr->pl_len );

        if ( protected_entries > 0 )
        {
            /* the caller asked us to flush and destroy a cache that 
             * contains one or more protected entries.  Since we can't
             * flush protected entries, we haven't destroyed them either.
             * Since they are all on the protected list, just re-insert 
             * them into the cache before we flag an error.
             */
            entry_ptr = cache_ptr->pl_head_ptr;

            while ( entry_ptr != NULL )
            {
                entry_ptr->in_slist = FALSE;

                H5C__INSERT_IN_INDEX(cache_ptr, entry_ptr, FAIL)

                if ( entry_ptr->is_dirty ) {

                    H5C__INSERT_ENTRY_IN_SLIST(cache_ptr, entry_ptr)
                }
                entry_ptr = entry_ptr->next;
            }
        }
    }

    HDassert( protected_entries <= cache_ptr->pl_len );

    if ( cache_ptr->pl_len > 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_PROTECT, FAIL, "cache has protected items")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_flush_cache() */


/*-------------------------------------------------------------------------
 * Function:    H5C_get_cache_auto_resize_config
 *
 * Purpose:	Copy the current configuration of the cache automatic 
 *		re-sizing function into the instance of H5C_auto_size_ctl_t
 *		pointed to by config_ptr.
 *
 * Return:      SUCCEED on success, and FAIL on failure.
 *
 * Programmer:  John Mainzer
 *		10/8/04
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C_get_cache_auto_resize_config(H5C_t * cache_ptr, 
                                 H5C_auto_size_ctl_t *config_ptr)
{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_get_cache_auto_resize_config, FAIL)

    if ( ( cache_ptr == NULL ) || ( cache_ptr->magic != H5C__H5C_T_MAGIC ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad cache_ptr on entry.")
    }

    if ( config_ptr == NULL ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad config_ptr on entry.")
    }

    *config_ptr = cache_ptr->resize_ctl;

    config_ptr->set_initial_size = FALSE;
    config_ptr->initial_size = cache_ptr->max_cache_size;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_get_cache_auto_resize_config() */


/*-------------------------------------------------------------------------
 * Function:    H5C_get_cache_size
 *
 * Purpose:	Return the cache maximum size, the minimum clean size, the 
 *		current size, and the current number of entries in 
 *              *max_size_ptr, *min_clean_size_ptr, *cur_size_ptr, and
 *		*cur_num_entries_ptr respectively.  If any of these 
 *		parameters are NULL, skip that value.
 *
 * Return:      SUCCEED on success, and FAIL on failure.
 *
 * Programmer:  John Mainzer
 *		10/8/04
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C_get_cache_size(H5C_t * cache_ptr, 
                   size_t * max_size_ptr,
                   size_t * min_clean_size_ptr,
                   size_t * cur_size_ptr,
                   int32_t * cur_num_entries_ptr)
{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_get_cache_size, FAIL)

    if ( ( cache_ptr == NULL ) || ( cache_ptr->magic != H5C__H5C_T_MAGIC ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad cache_ptr on entry.")
    }

    if ( max_size_ptr != NULL ) {

        *max_size_ptr = cache_ptr->max_cache_size;
    }

    if ( min_clean_size_ptr != NULL ) {

        *min_clean_size_ptr = cache_ptr->min_clean_size;
    }

    if ( cur_size_ptr != NULL ) {

        *cur_size_ptr = cache_ptr->index_size;
    }

    if ( cur_num_entries_ptr != NULL ) {

        *cur_num_entries_ptr = cache_ptr->index_len;
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_get_cache_size() */


/*-------------------------------------------------------------------------
 * Function:    H5C_get_cache_hit_rate
 *
 * Purpose:	Compute and return the current cache hit rate in 
 *              *hit_rate_ptr.  If there have been no accesses since the 
 *              last time the cache hit rate stats were reset, set
 *		*hit_rate_ptr to 0.0.  On error, *hit_rate_ptr is 
 *		undefined.
 *
 * Return:      SUCCEED on success, and FAIL on failure.
 *
 * Programmer:  John Mainzer
 *		10/7/04
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C_get_cache_hit_rate(H5C_t * cache_ptr,
                       double * hit_rate_ptr)

{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_get_cache_hit_rate, FAIL)

    if ( ( cache_ptr == NULL ) || ( cache_ptr->magic != H5C__H5C_T_MAGIC ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad cache_ptr on entry.")
    }

    if ( hit_rate_ptr == NULL ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad hit_rate_ptr on entry.")
    }

    HDassert( cache_ptr->cache_hits >= 0 );
    HDassert( cache_ptr->cache_accesses >= cache_ptr->cache_hits );

    if ( cache_ptr->cache_accesses > 0 ) {

        *hit_rate_ptr = ((double)(cache_ptr->cache_hits)) /
                         ((double)(cache_ptr->cache_accesses));

    } else {

        *hit_rate_ptr = 0.0;
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_get_cache_hit_rate() */


/*-------------------------------------------------------------------------
 * Function:    H5C_insert_entry
 *
 * Purpose:     Adds the specified thing to the cache.  The thing need not
 *              exist on disk yet, but it must have an address and disk
 *              space reserved.
 *
 *		The primary_dxpl_id and secondary_dxpl_id parameters 
 *		specify the dxpl_ids used on the first write occasioned
 *		by the insertion (primary_dxpl_id), and on all subsequent 
 *		writes (secondary_dxpl_id).  This is useful in the 
 *		metadata cache, but may not be needed elsewhere.  If so, 
 *		just use the same dxpl_id for both parameters.
 *
 *		The primary_dxpl_id is the dxpl_id passed to the 
 *		check_write_permitted function if such a function has been 
 *		provided.
 *
 *		Observe that this function cannot occasion a read.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *		6/2/04
 *
 * Modifications:
 *
 *		JRM -- 7/21/04
 *		Updated function for the addition of the hash table.
 *
 *		JRM -- 10/28/04
 *		Added code to set the cache_full flag to TRUE when ever
 *		we need to make space in the cache.
 *
 *		JRM --11/22/04
 *		Updated function for the addition of the first_flush_ptr
 *		parameter to H5C_make_space_in_cache().
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C_insert_entry(H5F_t * 	     f, 
                 hid_t		     primary_dxpl_id, 
                 hid_t		     secondary_dxpl_id, 
                 H5C_t *	     cache_ptr,
                 const H5C_class_t * type, 
                 haddr_t 	     addr, 
                 void *		     thing)
{
    herr_t		result;
    herr_t		ret_value = SUCCEED;    /* Return value */
    hbool_t		first_flush = TRUE;
    hbool_t		write_permitted = TRUE;
    H5C_cache_entry_t *	entry_ptr;
    H5C_cache_entry_t *	test_entry_ptr;

    FUNC_ENTER_NOAPI(H5C_insert_entry, FAIL)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->skip_file_checks || f );
    HDassert( type );
    HDassert( type->flush );
    HDassert( type->size );
    HDassert( H5F_addr_defined(addr) );
    HDassert( thing );

    entry_ptr = (H5C_cache_entry_t *)thing;

    entry_ptr->addr = addr;
    entry_ptr->type = type;

    if ( (type->size)(f, thing, &(entry_ptr->size)) < 0 ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGETSIZE, FAIL, \
                    "Can't get size of thing")
    }

    HDassert( entry_ptr->size < H5C_MAX_ENTRY_SIZE ); 

    entry_ptr->in_slist = FALSE;

    entry_ptr->ht_next = NULL;
    entry_ptr->ht_prev = NULL;

    entry_ptr->next = NULL;
    entry_ptr->prev = NULL; 

    entry_ptr->aux_next = NULL;
    entry_ptr->aux_prev = NULL;

    H5C__RESET_CACHE_ENTRY_STATS(entry_ptr) 

    if ((cache_ptr->index_size + entry_ptr->size) > cache_ptr->max_cache_size) {

        size_t space_needed;

        cache_ptr->cache_full = TRUE;

        if ( cache_ptr->check_write_permitted != NULL ) {

            result = (cache_ptr->check_write_permitted)(f, 
                                                        primary_dxpl_id, 
                                                        &write_permitted);

            if ( result < 0 ) {

                HGOTO_ERROR(H5E_CACHE, H5E_CANTINS, FAIL, \
                            "Can't get write_permitted")
            }
        }

        HDassert( entry_ptr->size <= H5C_MAX_ENTRY_SIZE );

        space_needed = (cache_ptr->index_size + entry_ptr->size) -
                       cache_ptr->max_cache_size;

        /* It would be nice to be able to do a tight sanity check on
         * space_needed here, but it is hard to assign an upper bound on
         * its value other than then value assigned to it.
         *
         * This fact springs from several features of the cache:
         *
         * First, it is possible for the cache to grow without
         * bound as long as entries are protected and not unprotected.
         *
         * Second, when writes are not permitted it is also possible
         * for the cache to grow without bound.
         *
         * Finally, we don't check to see if the cache is oversized
         * at the end of an unprotect.  As a result, it is possible
         * to have a vastly oversized cache with no protected entries
         * as long as all the protects preceed the unprotects.
         *
         * Since items 1 and 2 are not changing any time soon, I see
         * no point in worrying about the third.
         *
         * In any case, I hope this explains why there is no sanity
         * check on space_needed here.
         */

        result = H5C_make_space_in_cache(f, 
                                         primary_dxpl_id, 
                                         secondary_dxpl_id,
                                         cache_ptr, 
                                         space_needed, 
                                         write_permitted,
                                         &first_flush);

        if ( result < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTINS, FAIL, \
                        "H5C_make_space_in_cache failed.")
        }
    }

    /* verify that the new entry isn't already in the hash table -- scream
     * and die if it is.
     */

    H5C__SEARCH_INDEX(cache_ptr, addr, test_entry_ptr, FAIL)

    if ( test_entry_ptr != NULL ) {

        if ( test_entry_ptr == entry_ptr ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTINS, FAIL, \
                        "entry already in cache.")

        } else {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTINS, FAIL, \
                        "duplicate entry in cache.")
        }
    }

    /* we don't initialize the protected field until here as it is
     * possible that the entry is already in the cache, and already 
     * protected.  If it is, we don't want to make things worse by
     * marking it unprotected.
     */

    entry_ptr->is_protected = FALSE;

    H5C__INSERT_IN_INDEX(cache_ptr, entry_ptr, FAIL)

    if ( entry_ptr->is_dirty ) {

        H5C__INSERT_ENTRY_IN_SLIST(cache_ptr, entry_ptr)
    }

    H5C__UPDATE_RP_FOR_INSERTION(cache_ptr, entry_ptr, FAIL)

    H5C__UPDATE_STATS_FOR_INSERTION(cache_ptr, entry_ptr)

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_insert_entry() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_rename_entry
 *
 * Purpose:     Use this function to notify the cache that an entry's
 *              file address changed.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              6/2/04
 *
 * Modifications:
 *
 *		JRM -- 7/21/04
 *		Updated function for the addition of the hash table.
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C_rename_entry(H5F_t *	     f, 
                 H5C_t *	     cache_ptr,
                 const H5C_class_t * type, 
                 haddr_t 	     old_addr,
	         haddr_t 	     new_addr)
{
    herr_t		ret_value = SUCCEED;      /* Return value */
    H5C_cache_entry_t *	entry_ptr = NULL;
    H5C_cache_entry_t *	test_entry_ptr = NULL;

    FUNC_ENTER_NOAPI(H5C_rename_entry, FAIL)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->skip_file_checks || f );
    HDassert( type );
    HDassert( H5F_addr_defined(old_addr) );
    HDassert( H5F_addr_defined(new_addr) );
    HDassert( H5F_addr_ne(old_addr, new_addr) );

    H5C__SEARCH_INDEX(cache_ptr, old_addr, entry_ptr, FAIL)

    if ( ( entry_ptr == NULL ) || ( entry_ptr->type != type ) )

        /* the old item doesn't exist in the cache, so we are done. */
        HGOTO_DONE(SUCCEED)

    HDassert( entry_ptr->addr == old_addr );
    HDassert( entry_ptr->type == type );
    HDassert( !(entry_ptr->is_protected) );

    H5C__SEARCH_INDEX(cache_ptr, new_addr, test_entry_ptr, FAIL)

    if ( test_entry_ptr != NULL ) { /* we are hosed */
        
        if ( test_entry_ptr->type == type ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTRENAME, FAIL, \
                        "Target already renamed & reinserted???.")

        } else {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTRENAME, FAIL, \
                        "New address already in use?.")
        }
    }

    /* If we get this far, we have work to do.  Remove *entry_ptr from
     * the hash table (and skip list if necessary), change its address to the 
     * new address, and then re-insert.
     *
     * Update the replacement policy for a hit to avoid an eviction before
     * the renamed entry is touched.  Update stats for a rename.
     *
     * Note that we do not check the size of the cache, or evict anything.
     * Since this is a simple re-name, cache size should be unaffected.
     */
    H5C__DELETE_FROM_INDEX(cache_ptr, entry_ptr)

    if ( entry_ptr->in_slist ) {

        HDassert( cache_ptr->slist_ptr );

        H5C__REMOVE_ENTRY_FROM_SLIST(cache_ptr, entry_ptr)
    }

    entry_ptr->addr = new_addr;

    H5C__INSERT_IN_INDEX(cache_ptr, entry_ptr, FAIL)

    if ( entry_ptr->is_dirty ) {

        H5C__INSERT_ENTRY_IN_SLIST(cache_ptr, entry_ptr)
    }

    H5C__UPDATE_RP_FOR_RENAME(cache_ptr, entry_ptr, FAIL)

    H5C__UPDATE_STATS_FOR_RENAME(cache_ptr, entry_ptr)

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_rename_entry() */


/*-------------------------------------------------------------------------
 * Function:    H5C_protect
 *
 * Purpose:     If the target entry is not in the cache, load it.  If 
 *		necessary, attempt to evict one or more entries to keep
 *		the cache within its maximum size.
 *
 *		Mark the target entry as protected, and return its address
 *		to the caller.  The caller must call H5C_unprotect() when
 *		finished with the entry.
 *
 *		While it is protected, the entry may not be either evicted 
 *		or flushed -- nor may it be accessed by another call to 
 *		H5C_protect.  Any attempt to do so will result in a failure.
 *
 *		The primary_dxpl_id and secondary_dxpl_id parameters 
 *		specify the dxpl_ids used on the first write occasioned
 *		by the insertion (primary_dxpl_id), and on all subsequent 
 *		writes (secondary_dxpl_id).  This is useful in the 
 *		metadata cache, but may not be needed elsewhere.  If so, 
 *		just use the same dxpl_id for both parameters.
 *
 *		All reads are performed with the primary_dxpl_id.
 *
 *		Similarly, the primary_dxpl_id is passed to the 
 *		check_write_permitted function if it is called.
 *
 * Return:      Success:        Ptr to the desired entry
 *
 *              Failure:        NULL
 *
 * Programmer:  John Mainzer -  6/2/04
 *
 * Modifications:
 *
 *		JRM -- 7/21/04
 *		Updated for the addition of the hash table.
 *
 *		JRM -- 10/28/04
 *		Added code to set cache_full to TRUE whenever we try to 
 *		make space in the cache.
 *
 *		JRM -- 11/12/04
 *		Added code to call to H5C_make_space_in_cache()	after the 
 *		call to H5C__auto_adjust_cache_size() if that function 
 *		sets the size_decreased flag is TRUE.
 *
 *-------------------------------------------------------------------------
 */

void *
H5C_protect(H5F_t *	       f, 
            hid_t	       primary_dxpl_id, 
            hid_t	       secondary_dxpl_id, 
            H5C_t *	       cache_ptr,
            const H5C_class_t * type, 
            haddr_t 	       addr,
            const void *       udata1, 
            void *	       udata2)
{
    hbool_t		hit = FALSE;
    hbool_t		first_flush = TRUE;
    hbool_t		have_write_permitted = FALSE;
    hbool_t		write_permitted = TRUE;
    herr_t		result;
    void *		thing = NULL;
    H5C_cache_entry_t *	entry_ptr;
    void *		ret_value = NULL;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_protect, NULL)

    /* check args */
    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->skip_file_checks || f );
    HDassert( type );
    HDassert( type->flush );
    HDassert( type->load );
    HDassert( H5F_addr_defined(addr) );

    /* first check to see if the target is in cache */
    H5C__SEARCH_INDEX(cache_ptr, addr, entry_ptr, NULL)

    if ( entry_ptr != NULL ) {

        hit = TRUE;
        thing = (void *)entry_ptr;

    } else { /* must try to load the entry from disk. */

        hit = FALSE;

        thing = H5C_load_entry(f, primary_dxpl_id, type, addr, udata1, udata2,
                               cache_ptr->skip_file_checks);

        if ( thing == NULL ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTLOAD, NULL, "can't load entry")
        }

        entry_ptr = (H5C_cache_entry_t *)thing;

        /* try to free up some space if necessay */
        if ( (cache_ptr->index_size + entry_ptr->size) > 
              cache_ptr->max_cache_size ) {

            size_t space_needed;

            cache_ptr->cache_full = TRUE;

            if ( cache_ptr->check_write_permitted != NULL ) {

                result = (cache_ptr->check_write_permitted)(f, 
                                                            primary_dxpl_id, 
                                                            &write_permitted);

                if ( result < 0 ) {

                    HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, NULL, \
                               "Can't get write_permitted 1")

                } else {

                    have_write_permitted = TRUE;
                }
            } else {

                have_write_permitted = TRUE;
            }

            HDassert( entry_ptr->size <= H5C_MAX_ENTRY_SIZE );

            space_needed = (cache_ptr->index_size + entry_ptr->size) -
                           cache_ptr->max_cache_size;

            /* It would be nice to be able to do a tight sanity check on 
             * space_needed here, but it is hard to assign an upper bound on 
             * its value other than then value assigned to it.
             *
             * This fact springs from several features of the cache:
             *
             * First, it is possible for the cache to grow without
             * bound as long as entries are protected and not unprotected.
             *
             * Second, when writes are not permitted it is also possible
             * for the cache to grow without bound.
             *
             * Finally, we don't check to see if the cache is oversized 
             * at the end of an unprotect.  As a result, it is possible 
             * to have a vastly oversized cache with no protected entries
             * as long as all the protects preceed the unprotects.
             *
             * Since items 1 and 2 are not changing any time soon, I see
             * no point in worrying about the third.
             *
             * In any case, I hope this explains why there is no sanity
             * check on space_needed here.
             */

            result = H5C_make_space_in_cache(f, primary_dxpl_id, 
                                             secondary_dxpl_id, cache_ptr, 
                                             space_needed, write_permitted,
                                             &first_flush);

            if ( result < 0 ) {

                HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, NULL, \
                            "H5C_make_space_in_cache failed 1.")
            }
        }

        /* Insert the entry in the hash table.  It can't be dirty yet, so 
         * we don't even check to see if it should go in the skip list.
         */
        H5C__INSERT_IN_INDEX(cache_ptr, entry_ptr, NULL)

        /* insert the entry in the data structures used by the replacement
         * policy.  We are just going to take it out again when we update
         * the replacement policy for a protect, but this simplifies the
         * code.  If we do this often enough, we may want to optimize this.
         */
        H5C__UPDATE_RP_FOR_INSERTION(cache_ptr, entry_ptr, NULL)
    }

    HDassert( entry_ptr->addr == addr );
    HDassert( entry_ptr->type == type );

    if ( entry_ptr->is_protected ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, NULL, \
                    "Target already protected?!?.")
    }

    H5C__UPDATE_RP_FOR_PROTECT(cache_ptr, entry_ptr, NULL)

    entry_ptr->is_protected = TRUE;

    ret_value = thing;

    H5C__UPDATE_CACHE_HIT_RATE_STATS(cache_ptr, hit)

    H5C__UPDATE_STATS_FOR_PROTECT(cache_ptr, entry_ptr, hit)

    if ( ( cache_ptr->resize_enabled ) &&
         ( cache_ptr->cache_accesses >= 
           (cache_ptr->resize_ctl).epoch_length ) ) {

        if ( ! have_write_permitted ) {

            if ( cache_ptr->check_write_permitted != NULL ) {

                result = (cache_ptr->check_write_permitted)(f, 
                                                            primary_dxpl_id, 
                                                            &write_permitted);

                if ( result < 0 ) {

                    HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, NULL, \
                               "Can't get write_permitted 2")

                } else {

                    have_write_permitted = TRUE;
                }
            } else {

                have_write_permitted = TRUE;
            }
        }

        result = H5C__auto_adjust_cache_size(cache_ptr,
                                             f,
                                             primary_dxpl_id,
                                             secondary_dxpl_id,
                                             write_permitted,
                                             &first_flush);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, NULL, \
                        "Cache auto-resize failed.")
        
        } else if ( cache_ptr->size_decreased  ) {

            cache_ptr->size_decreased = FALSE;
        
            /* check to see if the cache is now oversized due to the cache
             * size reduction.  If it is, try to evict enough entries to 
             * bring the cache size down to the current maximum cache size.
             */
            if ( cache_ptr->index_size > cache_ptr->max_cache_size ) {

                size_t space_needed;

                cache_ptr->cache_full = TRUE;

                space_needed = cache_ptr->index_size - 
                               cache_ptr->max_cache_size + 1;

                result = H5C_make_space_in_cache(f, primary_dxpl_id, 
                                                 secondary_dxpl_id, cache_ptr, 
                                                 space_needed, write_permitted,
                                                 &first_flush);

                if ( result < 0 ) {

                    HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, NULL, \
                                "H5C_make_space_in_cache failed 2.")
                }
            }
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_protect() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_reset_cache_hit_rate_stats()
 *
 * Purpose:     Reset the cache hit rate computation fields.
 *
 * Return:      SUCCEED on success, and FAIL on failure.
 *
 * Programmer:  John Mainzer, 10/5/04
 *
 * Modifications:
 *
 *		JRM - 7/21/04
 *		Updated the function for the addition of the hash table.
 *		In particular, we now add dirty entries to the skip list if
 *		they aren't in the list already.
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C_reset_cache_hit_rate_stats(H5C_t * cache_ptr)
{
    herr_t	ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_reset_cache_hit_rate_stats, FAIL)

    if ( ( cache_ptr == NULL ) || ( cache_ptr->magic != H5C__H5C_T_MAGIC ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad cache_ptr on entry.")
    }

    cache_ptr->cache_hits		= 0;
    cache_ptr->cache_accesses		= 0;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_reset_cache_hit_rate_stats() */


/*-------------------------------------------------------------------------
 * Function:    H5C_set_cache_auto_resize_config
 *
 * Purpose:	Set the cache automatic resize configuration to the 
 *		provided values if they are in range, and fail if they
 *		are not.
 *
 *		If the new configuration enables automatic cache resizing,
 *		coerce the cache max size and min clean size into agreement
 *		with the new policy and re-set the full cache hit rate
 *		stats.
 *
 * Return:      SUCCEED on success, and FAIL on failure.
 *
 * Programmer:  John Mainzer
 *		10/8/04
 *
 * Modifications:
 *
 *		JRM -- 11/18/04
 *		Reworked function to match major changes in 
 *		H5C_auto_size_ctl_t.  
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C_set_cache_auto_resize_config(H5C_t * cache_ptr, 
                                 H5C_auto_size_ctl_t *config_ptr)
{
    herr_t	ret_value = SUCCEED;      /* Return value */
    herr_t	result;
    size_t      new_max_cache_size;
    size_t      new_min_clean_size;

    FUNC_ENTER_NOAPI(H5C_set_cache_auto_resize_config, FAIL)

    if ( ( cache_ptr == NULL ) || ( cache_ptr->magic != H5C__H5C_T_MAGIC ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad cache_ptr on entry.")
    }

    if ( config_ptr == NULL ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "NULL config_ptr on entry.")
    }

    if ( config_ptr->version != H5C__CURR_AUTO_SIZE_CTL_VER ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Unknown config version.")
    }

    /* check general configuration section of the config: */
    if ( ( config_ptr->max_size > H5C__MAX_MAX_CACHE_SIZE ) 
         ||
         ( config_ptr->max_size < config_ptr->min_size )
         ||
         ( config_ptr->min_size < H5C__MIN_MAX_CACHE_SIZE )
         ||
         ( ( config_ptr->set_initial_size ) &&
           ( config_ptr->initial_size > config_ptr->max_size )
         )
         ||
         ( ( config_ptr->set_initial_size ) &&
           ( config_ptr->initial_size < config_ptr->min_size )
         )
         ||
         ( config_ptr->min_clean_fraction > 1.0 )
         ||
         ( config_ptr->min_clean_fraction < 0.0 )
         ||
         ( config_ptr->epoch_length < H5C__MIN_AR_EPOCH_LENGTH )
         ||
         ( config_ptr->epoch_length > H5C__MAX_AR_EPOCH_LENGTH )
       ) {

        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, \
                    "error in general configuration fields of new config.")
    }

    /* check size increase control fields of the config: */
    if ( ( ( config_ptr->incr_mode != H5C_incr__off ) 
           &&
           ( config_ptr->incr_mode != H5C_incr__threshold ) 
         )
         ||
         ( ( config_ptr->incr_mode == H5C_incr__threshold )
           &&
           ( ( config_ptr->lower_hr_threshold < 0.0 )
             ||
             ( config_ptr->lower_hr_threshold > 1.0 )
             ||
             ( config_ptr->increment < 1.0 )
             /* no need to check max_increment, as it is a size_t,
              * and thus must be non-negative.
              */
           ) 
         )
       ) {

        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, \
                    "error in the size increase control fields of new config.")
    }

    /* check size decrease control fields of the config: */
    if ( ( ( config_ptr->decr_mode != H5C_decr__off ) 
           &&
           ( config_ptr->decr_mode != H5C_decr__threshold ) 
           &&
           ( config_ptr->decr_mode != H5C_decr__age_out ) 
           &&
           ( config_ptr->decr_mode != H5C_decr__age_out_with_threshold ) 
         )
         ||
         ( ( config_ptr->decr_mode == H5C_decr__threshold )
           &&
           ( ( config_ptr->upper_hr_threshold > 1.0 )
             ||
             ( config_ptr->decrement > 1.0 )
             ||
             ( config_ptr->decrement < 0.0 )
             /* no need to check max_decrement as it is a size_t 
              * and thus must be non-negative.
              */
           )
         )
         ||
         ( ( ( config_ptr->decr_mode == H5C_decr__age_out )
             ||
             ( config_ptr->decr_mode == H5C_decr__age_out_with_threshold )
           )
           &&
           (
             ( config_ptr->epochs_before_eviction < 1 )
             ||
             ( config_ptr->epochs_before_eviction > H5C__MAX_EPOCH_MARKERS )
             ||
             ( ( config_ptr->apply_empty_reserve )
               &&
               ( config_ptr->empty_reserve < 0.0 )
             )
             ||
             ( ( config_ptr->apply_empty_reserve )
               &&
               ( config_ptr->empty_reserve > 1.0 )
             )
             /* no need to check max_decrement as it is a size_t 
              * and thus must be non-negative.
              */
           )
         )
         ||
         ( ( config_ptr->decr_mode == H5C_decr__age_out_with_threshold )
           &&
           ( config_ptr->upper_hr_threshold > 1.0 )
         )
       ) {

        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, \
                    "error in the size decrease control fields of new config.")
    }

    /* check for conflicts between size increase and size decrease controls: */
    if ( ( config_ptr->incr_mode == H5C_incr__threshold )
         &&
         ( ( config_ptr->decr_mode == H5C_decr__threshold ) 
           ||
           ( config_ptr->decr_mode == H5C_decr__age_out_with_threshold )
         )
         &&
         ( config_ptr->lower_hr_threshold >= config_ptr->upper_hr_threshold )
       ) {

        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, \
                    "conflicting threshold fields in new config.")
    }

    cache_ptr->size_increase_possible = TRUE; /* will set to FALSE if needed */
    cache_ptr->size_decrease_possible = TRUE; /* will set to FALSE if needed */

    switch ( config_ptr->incr_mode )
    {
        case H5C_incr__off:
            cache_ptr->size_increase_possible = FALSE;
            break;

        case H5C_incr__threshold:
            if ( ( config_ptr->lower_hr_threshold <= 0.0 ) ||
                 ( config_ptr->increment <= 1.0 ) ||
                 ( ( config_ptr->apply_max_increment ) &&
                   ( config_ptr->max_increment <= 0 ) ) ) {

                 cache_ptr->size_increase_possible = FALSE;
            }    
            break;

        default: /* should be unreachable */
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Unknown incr_mode?!?!?.")
            break;
    }

    switch ( config_ptr->decr_mode )
    {
        case H5C_decr__off:
            cache_ptr->size_decrease_possible = FALSE;
            break;

        case H5C_decr__threshold:
            if ( ( config_ptr->upper_hr_threshold >= 1.0 ) ||
                 ( config_ptr->decrement >= 1.0 ) ||
                 ( ( config_ptr->apply_max_decrement ) &&
                   ( config_ptr->max_decrement <= 0 ) ) ) {

                cache_ptr->size_decrease_possible = FALSE;
            }
            break;

        case H5C_decr__age_out:
            if ( ( ( config_ptr->apply_empty_reserve ) &&
                   ( config_ptr->empty_reserve >= 1.0 ) ) ||
                 ( ( config_ptr->apply_max_decrement ) &&
                   ( config_ptr->max_decrement <= 0 ) ) ) {

                cache_ptr->size_decrease_possible = FALSE;
            }
            break;

        case H5C_decr__age_out_with_threshold:
            if ( ( ( config_ptr->apply_empty_reserve ) &&
                   ( config_ptr->empty_reserve >= 1.0 ) ) ||
                 ( ( config_ptr->apply_max_decrement ) &&
                   ( config_ptr->max_decrement <= 0 ) ) ||
                 ( config_ptr->upper_hr_threshold >= 1.0 ) ) {

                cache_ptr->size_decrease_possible = FALSE;
            }
            break;

        default: /* should be unreachable */
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Unknown decr_mode?!?!?.")
            break;
    }

    if ( config_ptr->max_size == config_ptr->min_size ) {

        cache_ptr->size_increase_possible = FALSE;
        cache_ptr->size_decrease_possible = FALSE;
    }

    cache_ptr->resize_enabled = cache_ptr->size_increase_possible ||
                                cache_ptr->size_decrease_possible;

    cache_ptr->resize_ctl = *config_ptr;

    /* Resize the cache to the supplied initial value if requested, or as 
     * necessary to force it within the bounds of the current automatic 
     * cache resizing configuration.
     *
     * Note that the min_clean_fraction may have changed, so we 
     * go through the exercise even if the current size is within
     * range and an initial size has not been provided.
     */
    if ( (cache_ptr->resize_ctl).set_initial_size ) {
        
        new_max_cache_size = (cache_ptr->resize_ctl).initial_size;
    } 
    else if ( cache_ptr->max_cache_size > (cache_ptr->resize_ctl).max_size ) {

        new_max_cache_size = (cache_ptr->resize_ctl).max_size;
    }
    else if ( cache_ptr->max_cache_size < (cache_ptr->resize_ctl).min_size ) {

        new_max_cache_size = (cache_ptr->resize_ctl).min_size;

    } else {

        new_max_cache_size = cache_ptr->max_cache_size;
    }

    new_min_clean_size = (size_t)
                         ((double)new_max_cache_size *
                          ((cache_ptr->resize_ctl).min_clean_fraction));


    /* since new_min_clean_size is of type size_t, we have
     *
     * 	( 0 <= new_min_clean_size )
     *
     * by definition.
     */
    HDassert( new_min_clean_size <= new_max_cache_size );
    HDassert( (cache_ptr->resize_ctl).min_size <= new_max_cache_size );
    HDassert( new_max_cache_size <= (cache_ptr->resize_ctl).max_size );

    cache_ptr->max_cache_size = new_max_cache_size;
    cache_ptr->min_clean_size = new_min_clean_size;

    if ( H5C_reset_cache_hit_rate_stats(cache_ptr) != SUCCEED ) {

        /* this should be impossible... */
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "H5C_reset_cache_hit_rate_stats failed.")
    }

    /* remove excess epoch markers if any */
    if ( ( config_ptr->decr_mode == H5C_decr__age_out_with_threshold ) ||
         ( config_ptr->decr_mode == H5C_decr__age_out ) ) {

        if ( cache_ptr->epoch_markers_active >
             (cache_ptr->resize_ctl).epochs_before_eviction ) {

            result =
                H5C__autoadjust__ageout__remove_excess_markers(cache_ptr);

            if ( result != SUCCEED ) {

                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                            "can't remove excess epoch markers.")
            }
        }
    } else if ( cache_ptr->epoch_markers_active > 0 ) {

        result = H5C__autoadjust__ageout__remove_all_markers(cache_ptr);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "error removing all epoch markers.")
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_set_cache_auto_resize_config() */


/*-------------------------------------------------------------------------
 * Function:    H5C_set_skip_flags
 *
 * Purpose:     Set the values of the skip sanity check flags.
 *		
 *		This function and the skip sanity check flags were created
 *		for the convenience of the test bed.  However it is 
 *		possible that there may be other uses for the flags.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              6/11/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C_set_skip_flags(H5C_t * cache_ptr,
                   hbool_t skip_file_checks,
                   hbool_t skip_dxpl_id_checks)
{
    herr_t		ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5C_set_skip_flags, FAIL)

    /* This would normally be an assert, but we need to use an HGOTO_ERROR
     * call to shut up the compiler.
     */
    if ( ( ! cache_ptr ) || ( cache_ptr->magic != H5C__H5C_T_MAGIC ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad cache_ptr")
    }

    cache_ptr->skip_file_checks    = skip_file_checks;
    cache_ptr->skip_dxpl_id_checks = skip_dxpl_id_checks;

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_set_skip_flags() */


/*-------------------------------------------------------------------------
 * Function:    H5C_stats
 *
 * Purpose:     Prints statistics about the cache.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              6/2/04
 *
 * Modifications:
 *
 *		JRM -- 7/21/04
 *		Updated function for the addition of the hash table.
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C_stats(H5C_t * cache_ptr,
          const char *  cache_name,
          hbool_t
#if !H5C_COLLECT_CACHE_STATS
          UNUSED
#endif /* H5C_COLLECT_CACHE_STATS */
          display_detailed_stats)
{
    herr_t	ret_value = SUCCEED;   /* Return value */

#if H5C_COLLECT_CACHE_STATS
    int		i;
    int64_t     total_hits = 0;
    int64_t     total_misses = 0;
    int64_t     total_insertions = 0;
    int64_t     total_clears = 0;
    int64_t     total_flushes = 0;
    int64_t     total_evictions = 0;
    int64_t     total_renames = 0;
    int32_t     aggregate_max_accesses = 0;
    int32_t     aggregate_min_accesses = 1000000;
    int32_t     aggregate_max_clears = 0;
    int32_t     aggregate_max_flushes = 0;
    size_t      aggregate_max_size = 0;
    double      hit_rate;
    double	average_successful_search_depth = 0.0;
    double	average_failed_search_depth = 0.0;
#endif /* H5C_COLLECT_CACHE_STATS */

    FUNC_ENTER_NOAPI(H5C_stats, FAIL)

    /* This would normally be an assert, but we need to use an HGOTO_ERROR
     * call to shut up the compiler.
     */
    if ( ( ! cache_ptr ) || 
         ( cache_ptr->magic != H5C__H5C_T_MAGIC ) || 
         ( !cache_name ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad cache_ptr or cache_name")
    }

#if H5C_COLLECT_CACHE_STATS

    for ( i = 0; i <= cache_ptr->max_type_id; i++ ) {

        total_hits       += cache_ptr->hits[i];
        total_misses     += cache_ptr->misses[i];
        total_insertions += cache_ptr->insertions[i];
        total_clears     += cache_ptr->clears[i];
        total_flushes    += cache_ptr->flushes[i];
        total_evictions  += cache_ptr->evictions[i];
        total_renames    += cache_ptr->renames[i];
#if H5C_COLLECT_CACHE_ENTRY_STATS
    if ( aggregate_max_accesses < cache_ptr->max_accesses[i] )
        aggregate_max_accesses = cache_ptr->max_accesses[i];
    if ( aggregate_min_accesses > aggregate_max_accesses )
        aggregate_min_accesses = aggregate_max_accesses;
    if ( aggregate_min_accesses > cache_ptr->min_accesses[i] )
        aggregate_min_accesses = cache_ptr->min_accesses[i]; 
    if ( aggregate_max_clears < cache_ptr->max_clears[i] )
        aggregate_max_clears = cache_ptr->max_clears[i];
    if ( aggregate_max_flushes < cache_ptr->max_flushes[i] )
        aggregate_max_flushes = cache_ptr->max_flushes[i];
    if ( aggregate_max_size < cache_ptr->max_size[i] )
        aggregate_max_size = cache_ptr->max_size[i];
#endif /* H5C_COLLECT_CACHE_ENTRY_STATS */
    }

    if ( ( total_hits > 0 ) || ( total_misses > 0 ) ) {

        hit_rate = 100.0 * ((double)(total_hits)) /
                   ((double)(total_hits + total_misses));
    } else {
        hit_rate = 0.0;
    }

    if ( cache_ptr->successful_ht_searches > 0 ) {

        average_successful_search_depth = 
            ((double)(cache_ptr->total_successful_ht_search_depth)) /
            ((double)(cache_ptr->successful_ht_searches));
    }

    if ( cache_ptr->failed_ht_searches > 0 ) {

        average_failed_search_depth = 
            ((double)(cache_ptr->total_failed_ht_search_depth)) /
            ((double)(cache_ptr->failed_ht_searches));
    }
        

    HDfprintf(stdout, "\nH5C: cache statistics for %s\n",
              cache_name);

    HDfprintf(stdout, "\n");

    HDfprintf(stdout, 
              "  hash table insertion / deletions   = %ld / %ld\n",
              (long)(cache_ptr->total_ht_insertions),
              (long)(cache_ptr->total_ht_deletions));

    HDfprintf(stdout, 
              "  HT successful / failed searches    = %ld / %ld\n",
              (long)(cache_ptr->successful_ht_searches),
              (long)(cache_ptr->failed_ht_searches));

    HDfprintf(stdout, 
              "  Av. HT suc / failed search depth   = %f / %f\n",
              average_successful_search_depth,
              average_failed_search_depth);

    HDfprintf(stdout, 
              "  current (max) index size / length  = %ld (%ld) / %ld (%ld)\n",
              (long)(cache_ptr->index_size),
              (long)(cache_ptr->max_index_size),
              (long)(cache_ptr->index_len),
              (long)(cache_ptr->max_index_len));

    HDfprintf(stdout, 
              "  current (max) slist size / length  = %ld (%ld) / %ld (%ld)\n",
              (long)(cache_ptr->slist_size),
              (long)(cache_ptr->max_slist_size),
              (long)(cache_ptr->slist_len),
              (long)(cache_ptr->max_slist_len));

    HDfprintf(stdout, 
              "  current (max) PL size / length     = %ld (%ld) / %ld (%ld)\n",
              (long)(cache_ptr->pl_size),
              (long)(cache_ptr->max_pl_size),
              (long)(cache_ptr->pl_len),
              (long)(cache_ptr->max_pl_len));

    HDfprintf(stdout,
              "  current LRU list size / length     = %ld / %ld\n",
              (long)(cache_ptr->LRU_list_size),
              (long)(cache_ptr->LRU_list_len));

    HDfprintf(stdout,
              "  current clean LRU size / length    = %ld / %ld\n",
              (long)(cache_ptr->cLRU_list_size),
              (long)(cache_ptr->cLRU_list_len));

    HDfprintf(stdout,
              "  current dirty LRU size / length    = %ld / %ld\n",
              (long)(cache_ptr->dLRU_list_size),
              (long)(cache_ptr->dLRU_list_len));

    HDfprintf(stdout, 
              "  Total hits / misses / hit_rate     = %ld / %ld / %f\n",
              (long)total_hits, 
              (long)total_misses, 
              hit_rate);

    HDfprintf(stdout, 
              "  Total clears / flushes / evictions = %ld / %ld / %ld\n",
              (long)total_clears,
              (long)total_flushes,
              (long)total_evictions);

    HDfprintf(stdout, "  Total insertions / renames         = %ld / %ld\n",
              (long)total_insertions,
              (long)total_renames);

#if H5C_COLLECT_CACHE_ENTRY_STATS

    HDfprintf(stdout, "  aggregate max / min accesses       = %d / %d\n",
              (int)aggregate_max_accesses,
              (int)aggregate_min_accesses);

    HDfprintf(stdout, "  aggregate max_clears / max_flushes = %d / %d\n",
              (int)aggregate_max_clears,
              (int)aggregate_max_flushes);

    HDfprintf(stdout, "  aggregate max_size                 = %d\n",
              (int)aggregate_max_size);


#endif /* H5C_COLLECT_CACHE_ENTRY_STATS */

    if ( display_detailed_stats )
    {

        for ( i = 0; i <= cache_ptr->max_type_id; i++ ) {

            HDfprintf(stdout, "\n");

            HDfprintf(stdout, "  Stats on %s:\n", 
                      ((cache_ptr->type_name_table_ptr))[i]);

            if ( ( cache_ptr->hits[i] > 0 ) || ( cache_ptr->misses[i] > 0 ) ) {

                hit_rate = 100.0 * ((double)(cache_ptr->hits[i])) /
                          ((double)(cache_ptr->hits[i] + cache_ptr->misses[i]));
            } else {
                hit_rate = 0.0;
            }

            HDfprintf(stdout, 
                      "    hits / misses / hit_rate       = %ld / %ld / %f\n", 
                      (long)(cache_ptr->hits[i]),
                      (long)(cache_ptr->misses[i]),
                      hit_rate);

            HDfprintf(stdout, 
                      "    clears / flushes / evictions   = %ld / %ld / %ld\n",
                      (long)(cache_ptr->clears[i]),
                      (long)(cache_ptr->flushes[i]),
                      (long)(cache_ptr->evictions[i]));

            HDfprintf(stdout, 
                      "    insertions / renames           = %ld / %ld\n",
                      (long)(cache_ptr->insertions[i]),
                      (long)(cache_ptr->renames[i]));

#if H5C_COLLECT_CACHE_ENTRY_STATS

            HDfprintf(stdout, 
                      "    entry max / min accesses       = %d / %d\n",
                      cache_ptr->max_accesses[i],
                      cache_ptr->min_accesses[i]);

            HDfprintf(stdout, 
                      "    entry max_clears / max_flushes = %d / %d\n",
                      cache_ptr->max_clears[i],
                      cache_ptr->max_flushes[i]);

            HDfprintf(stdout, 
                      "    entry max_size                 = %d\n",
                      (int)(cache_ptr->max_size[i]));


#endif /* H5C_COLLECT_CACHE_ENTRY_STATS */

        }
    }

    HDfprintf(stdout, "\n");

#endif /* H5C_COLLECT_CACHE_STATS */

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_stats() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_stats__reset
 *
 * Purpose:     Reset the stats fields to their initial values.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer, 4/28/04
 *
 * Modifications:
 *
 *		JRM - 7/21/04
 *		Updated for hash table related statistics.
 *
 *-------------------------------------------------------------------------
 */

void
H5C_stats__reset(H5C_t * cache_ptr)
{
#if H5C_COLLECT_CACHE_STATS
    int i;
#endif /* H5C_COLLECT_CACHE_STATS */

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );

#if H5C_COLLECT_CACHE_STATS
    for ( i = 0; i <= cache_ptr->max_type_id; i++ )
    {
        cache_ptr->hits[i]			= 0;
        cache_ptr->misses[i]			= 0;
        cache_ptr->insertions[i]		= 0;
        cache_ptr->clears[i]			= 0;
        cache_ptr->flushes[i]			= 0;
        cache_ptr->evictions[i]	 		= 0;
        cache_ptr->renames[i]	 		= 0;
    }

    cache_ptr->total_ht_insertions		= 0;
    cache_ptr->total_ht_deletions		= 0;
    cache_ptr->successful_ht_searches		= 0;
    cache_ptr->total_successful_ht_search_depth	= 0;
    cache_ptr->failed_ht_searches		= 0;
    cache_ptr->total_failed_ht_search_depth	= 0;

    cache_ptr->max_index_len			= 0;
    cache_ptr->max_index_size			= (size_t)0;

    cache_ptr->max_slist_len			= 0;
    cache_ptr->max_slist_size			= (size_t)0;

    cache_ptr->max_pl_len			= 0;
    cache_ptr->max_pl_size			= (size_t)0;

#if H5C_COLLECT_CACHE_ENTRY_STATS

    for ( i = 0; i <= cache_ptr->max_type_id; i++ )
    {
        cache_ptr->max_accesses[i]		= 0;
        cache_ptr->min_accesses[i]		= 1000000; 
        cache_ptr->max_clears[i]		= 0;
        cache_ptr->max_flushes[i]		= 0;
        cache_ptr->max_size[i]			= (size_t)0;
    }

#endif /* H5C_COLLECT_CACHE_ENTRY_STATS */
#endif /* H5C_COLLECT_CACHE_STATS */

    return;

} /* H5C_stats__reset() */


/*-------------------------------------------------------------------------
 * Function:    H5C_unprotect
 *
 * Purpose:	Undo an H5C_protect() call -- specifically, mark the
 *		entry as unprotected, remove it from the protected list,
 *		and give it back to the replacement policy.
 *
 *		The TYPE and ADDR arguments must be the same as those in
 *		the corresponding call to H5C_protect() and the THING 
 *		argument must be the value returned by that call to 
 *		H5C_protect().
 *
 *		The primary_dxpl_id and secondary_dxpl_id parameters 
 *		specify the dxpl_ids used on the first write occasioned
 *		by the unprotect (primary_dxpl_id), and on all subsequent 
 *		writes (secondary_dxpl_id).  Since an uprotect cannot 
 *		occasion a write at present, all this is moot for now.  
 *		However, things change, and in any case, 
 *		H5C_flush_single_entry() needs primary_dxpl_id and 
 *		secondary_dxpl_id in its parameter list.
 *
 *		The function can't cause a read either, so the dxpl_id
 *		parameters are moot in this case as well.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *		If the deleted flag is TRUE, simply remove the target entry
 *		from the cache, clear it, and free it without writing it to 
 *		disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              6/2/04
 *
 * Modifications:
 *
 *		JRM -- 7/21/04
 *		Updated the function for the addition of the hash table.
 *		In particular, we now add dirty entries to the tree if
 *		they aren't in the tree already.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_unprotect(H5F_t *		  f, 
              hid_t		  primary_dxpl_id, 
              hid_t		  secondary_dxpl_id, 
              H5C_t *		  cache_ptr,
              const H5C_class_t * type, 
              haddr_t		  addr,
              void *		  thing, 
              hbool_t		  deleted)
{
    herr_t              ret_value = SUCCEED;    /* Return value */
    H5C_cache_entry_t *	entry_ptr;
    H5C_cache_entry_t *	test_entry_ptr;

    FUNC_ENTER_NOAPI(H5C_unprotect, FAIL)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->skip_file_checks || f );
    HDassert( type );
    HDassert( type->clear );
    HDassert( type->flush );
    HDassert( H5F_addr_defined(addr) );
    HDassert( thing );

    entry_ptr = (H5C_cache_entry_t *)thing;

    HDassert( entry_ptr->addr == addr );
    HDassert( entry_ptr->type == type );

    if ( ! (entry_ptr->is_protected) ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, \
                        "Entry already unprotected??")
    }

    H5C__UPDATE_RP_FOR_UNPROTECT(cache_ptr, entry_ptr, FAIL)

    entry_ptr->is_protected = FALSE;    

    /* add the entry to the tree if it is dirty, and it isn't already in
     * the tree.
     */

    if ( ( entry_ptr->is_dirty ) && ( ! (entry_ptr->in_slist) ) ) {

        H5C__INSERT_ENTRY_IN_SLIST(cache_ptr, entry_ptr)
    }

    /* this implementation of the "deleted" option is a bit inefficient, as
     * we re-insert the entry to be deleted into the replacement policy
     * data structures, only to remove them again.  Depending on how often
     * we do this, we may want to optimize a bit.  
     *
     * On the other hand, this implementation is reasonably clean, and 
     * makes good use of existing code.
     *                                             JRM - 5/19/04
     */
    if ( deleted ) {

        /* the following first flush flag will never be used as we are
         * calling H5C_flush_single_entry with both the H5F_FLUSH_CLEAR_ONLY
         * and H5F_FLUSH_INVALIDATE flags.  However, it is needed for the
         * function call.
         */
        hbool_t		dummy_first_flush = TRUE;

        /* verify that the target entry is in the cache. */

        H5C__SEARCH_INDEX(cache_ptr, addr, test_entry_ptr, FAIL)

        if ( test_entry_ptr == NULL ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, \
                        "entry not in hash table?!?.")
        }
        else if ( test_entry_ptr != entry_ptr ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, \
                        "hash table contains multiple entries for addr?!?.")
        }

        if ( H5C_flush_single_entry(f, 
                                    primary_dxpl_id, 
                                    secondary_dxpl_id, 
                                    cache_ptr,
                                    type, 
                                    addr, 
                                    (H5F_FLUSH_CLEAR_ONLY|H5F_FLUSH_INVALIDATE),
                                    &dummy_first_flush, 
                                    TRUE) < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, "Can't flush.")
        }
    }

    H5C__UPDATE_STATS_FOR_UNPROTECT(cache_ptr)

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_unprotect() */


/*************************************************************************/
/**************************** Private Functions: *************************/
/*************************************************************************/

/*-------------------------------------------------------------------------
 *
 * Function:	H5C__auto_adjust_cache_size
 *
 * Purpose:    	Obtain the current full cache hit rate, and compare it 
 *		with the hit rate thresholds for modifying cache size.  
 *		If one of the thresholds has been crossed, adjusts the 
 *		size of the cache accordingly.
 *
 *		The function then resets the full cache hit rate 
 *		statistics, and exits.
 *
 * Return:      Non-negative on success/Negative on failure or if there was 
 *		an attempt to flush a protected item.
 *
 *
 * Programmer:  John Mainzer, 10/7/04
 *
 * Modifications:
 *
 *		JRM -- 11/18/04
 *		Major re-write to support ageout method of cache size
 *		reduction, and to adjust to changes in the 
 *		H5C_auto_size_ctl_t structure.
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5C__auto_adjust_cache_size(H5C_t * cache_ptr,
                            H5F_t * f,
                            hid_t primary_dxpl_id,
                            hid_t secondary_dxpl_id,
                            hbool_t write_permitted,
                            hbool_t * first_flush_ptr)
{
    herr_t			ret_value = SUCCEED;      /* Return value */
    herr_t			result;
    hbool_t			inserted_epoch_marker = FALSE;
    size_t			new_max_cache_size = 0;
    size_t			old_max_cache_size = 0;
    size_t			new_min_clean_size = 0;
    size_t			old_min_clean_size = 0;
    double			hit_rate;
    enum H5C_resize_status	status = in_spec; /* will change if needed */

    FUNC_ENTER_NOAPI_NOINIT(H5C__auto_adjust_cache_size)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->cache_accesses >= 
              (cache_ptr->resize_ctl).epoch_length );
    HDassert( 0.0 <= (cache_ptr->resize_ctl).min_clean_fraction );
    HDassert( (cache_ptr->resize_ctl).min_clean_fraction <= 100.0 );

    if ( !cache_ptr->resize_enabled ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Auto cache resize disabled.")
    }

    HDassert( ( (cache_ptr->resize_ctl).incr_mode != H5C_incr__off ) || \
              ( (cache_ptr->resize_ctl).decr_mode != H5C_decr__off ) );

    if ( H5C_get_cache_hit_rate(cache_ptr, &hit_rate) != SUCCEED ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Can't get hit rate.")
    }

    HDassert( ( 0.0 <= hit_rate ) && ( hit_rate <= 1.0 ) );

    switch ( (cache_ptr->resize_ctl).incr_mode )
    {
        case H5C_incr__off:
            if ( cache_ptr->size_increase_possible ) {

                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                           "size_increase_possible but H5C_incr__off?!?!?")
            }
            break;

        case H5C_incr__threshold:
            if ( hit_rate < (cache_ptr->resize_ctl).lower_hr_threshold ) {

                if ( ! cache_ptr->size_increase_possible ) {

                    status = increase_disabled;
                    
                } else if ( cache_ptr->max_cache_size >= 
                            (cache_ptr->resize_ctl).max_size ) {

                    HDassert( cache_ptr->max_cache_size == \
                              (cache_ptr->resize_ctl).max_size );
                    status = at_max_size;

                } else if ( ! cache_ptr->cache_full ) {

                    status = not_full;

                } else {

                    new_max_cache_size = (size_t)
                                     (((double)(cache_ptr->max_cache_size)) *
                                      (cache_ptr->resize_ctl).increment);

                    /* clip to max size if necessary */
                    if ( new_max_cache_size > 
                         (cache_ptr->resize_ctl).max_size ) {

                        new_max_cache_size = (cache_ptr->resize_ctl).max_size;
                    }

                    /* clip to max increment if necessary */
                    if ( ( (cache_ptr->resize_ctl).apply_max_increment ) &&
                         ( (cache_ptr->max_cache_size + 
                            (cache_ptr->resize_ctl).max_increment) <
                           new_max_cache_size ) ) {

                        new_max_cache_size = cache_ptr->max_cache_size +
                                         (cache_ptr->resize_ctl).max_increment;
                    }

                    status = increase;
                }
            }
            break;

        default:
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unknown incr_mode.")
            break;
    }

    /* If the decr_mode is either age out or age out with threshold, we 
     * must run the marker maintenance code, whether we run the size
     * reduction code or not.  We do this in two places -- here we 
     * insert a new marker if the number of active epoch markers is
     * is less than the the current epochs before eviction, and after
     * the ageout call, we cycle the markers.
     *
     * However, we can't call the ageout code or cycle the markers
     * unless there was a full complement of markers in place on 
     * entry.  The inserted_epoch_marker flag is used to track this.
     */

    if ( ( ( (cache_ptr->resize_ctl).decr_mode == H5C_decr__age_out ) 
           ||
           ( (cache_ptr->resize_ctl).decr_mode == 
              H5C_decr__age_out_with_threshold
           )
         )
         &&
         ( cache_ptr->epoch_markers_active < 
           (cache_ptr->resize_ctl).epochs_before_eviction 
         ) 
       ) {

        result = H5C__autoadjust__ageout__insert_new_marker(cache_ptr);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "can't insert new epoch marker.")

        } else {

            inserted_epoch_marker = TRUE;
        }
    }

    /* don't run the cache size decrease code unless the cache size
     * increase code is disabled, or the size increase code sees no need
     * for action.  In either case, status == in_spec at this point.
     */

    if ( status == in_spec ) {

        switch ( (cache_ptr->resize_ctl).decr_mode )
        {
            case H5C_decr__off:
                break;

            case H5C_decr__threshold:
                if ( hit_rate > (cache_ptr->resize_ctl).upper_hr_threshold ) {

                    if ( ! cache_ptr->size_decrease_possible ) {

                        status = decrease_disabled;
                    
                    } else if ( cache_ptr->max_cache_size <= 
                                (cache_ptr->resize_ctl).min_size ) {

                        HDassert( cache_ptr->max_cache_size ==
                                  (cache_ptr->resize_ctl).min_size );
                        status = at_min_size;

                    } else {

                        new_max_cache_size = (size_t)
                                 (((double)(cache_ptr->max_cache_size)) *
                                  (cache_ptr->resize_ctl).decrement);

                        /* clip to min size if necessary */
                        if ( new_max_cache_size < 
                             (cache_ptr->resize_ctl).min_size ) {

                            new_max_cache_size = 
                                (cache_ptr->resize_ctl).min_size;
                        }

                        /* clip to max decrement if necessary */
                        if ( ( (cache_ptr->resize_ctl).apply_max_decrement ) &&
                             ( ((cache_ptr->resize_ctl).max_decrement +
                                new_max_cache_size) < 
                               cache_ptr->max_cache_size ) ) {

                            new_max_cache_size = cache_ptr->max_cache_size - 
                                         (cache_ptr->resize_ctl).max_decrement;
                        }

                        status = decrease;
                    }
                }
                break;

            case H5C_decr__age_out_with_threshold:
            case H5C_decr__age_out:
                if ( ! inserted_epoch_marker ) {

                    if ( ! cache_ptr->size_decrease_possible ) {

                        status = decrease_disabled;

                    } else {

                        result = H5C__autoadjust__ageout(cache_ptr,
                                                         hit_rate,
                                                         &status,
                                                         &new_max_cache_size,
                                                         f,
                                                         primary_dxpl_id,
                                                         secondary_dxpl_id,
                                                         write_permitted,
                                                         first_flush_ptr);

                        if ( result != SUCCEED ) {

                            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                                        "ageout code failed.")
                        }
                    }
                }
                break;

            default:
                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unknown incr_mode.")
                break;
        }
    }

    /* cycle the epoch markers here if appropriate */
    if ( ( ( (cache_ptr->resize_ctl).decr_mode == H5C_decr__age_out ) 
           ||
           ( (cache_ptr->resize_ctl).decr_mode == 
              H5C_decr__age_out_with_threshold
           )
         )
         &&
         ( ! inserted_epoch_marker )
       ) {

        /* move last epoch marker to the head of the LRU list */
        result = H5C__autoadjust__ageout__cycle_epoch_marker(cache_ptr);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "error cycling epoch marker.")
        }
    }

    if ( ( status == increase ) || ( status == decrease ) ) {

        old_max_cache_size = cache_ptr->max_cache_size;
        old_min_clean_size = cache_ptr->min_clean_size;

        new_min_clean_size = (size_t)
                             ((double)new_max_cache_size * 
                              ((cache_ptr->resize_ctl).min_clean_fraction));

        /* new_min_clean_size is of size_t, and thus must be non-negative.
         * Hence we have
         *
         * 	( 0 <= new_min_clean_size ).
         *
 	 * by definition.
         */
        HDassert( new_min_clean_size <= new_max_cache_size );
        HDassert( (cache_ptr->resize_ctl).min_size <= new_max_cache_size );
        HDassert( new_max_cache_size <= (cache_ptr->resize_ctl).max_size );

        cache_ptr->max_cache_size = new_max_cache_size;
        cache_ptr->min_clean_size = new_min_clean_size;

        if ( status == increase ) {

            cache_ptr->cache_full = FALSE;

        } else if ( status == decrease ) {

            cache_ptr->size_decreased = TRUE;
        }
    }

    if ( (cache_ptr->resize_ctl).rpt_fcn != NULL ) {

        (*((cache_ptr->resize_ctl).rpt_fcn))
            (cache_ptr,
             H5C__CURR_AUTO_RESIZE_RPT_FCN_VER,
             hit_rate,
             status,
             old_max_cache_size,
             new_max_cache_size,
             old_min_clean_size,
             new_min_clean_size);
    }

    if ( H5C_reset_cache_hit_rate_stats(cache_ptr) != SUCCEED ) {

        /* this should be impossible... */
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "H5C_reset_cache_hit_rate_stats failed.")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C__auto_adjust_cache_size() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__autoadjust__ageout
 *
 * Purpose:     Implement the ageout automatic cache size decrement 
 *		algorithm.  Note that while this code evicts aged out
 *		entries, the code does not change the maximum cache size.
 *		Instead, the function simply computes the new value (if
 *		any change is indicated) and reports this value in 
 *		*new_max_cache_size_ptr.
 *
 * Return:      Non-negative on success/Negative on failure or if there was
 *              an attempt to flush a protected item.
 *
 *
 * Programmer:  John Mainzer, 11/18/04
 *
 * Modifications:
 *
 *              None.
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5C__autoadjust__ageout(H5C_t * cache_ptr,
                        double hit_rate,
                        enum H5C_resize_status * status_ptr,
                        size_t * new_max_cache_size_ptr,
                        H5F_t * f,
                        hid_t primary_dxpl_id,
                        hid_t secondary_dxpl_id,
                        hbool_t write_permitted,
                        hbool_t * first_flush_ptr)
{
    herr_t	ret_value = SUCCEED;      /* Return value */
    herr_t	result;
    size_t	test_size;

    FUNC_ENTER_NOAPI_NOINIT(H5C__autoadjust__ageout)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( ( status_ptr ) && ( *status_ptr == in_spec ) );
    HDassert( ( new_max_cache_size_ptr ) && ( *new_max_cache_size_ptr == 0 ) );

    /* remove excess epoch markers if any */
    if ( cache_ptr->epoch_markers_active >
         (cache_ptr->resize_ctl).epochs_before_eviction ) {

        result = H5C__autoadjust__ageout__remove_excess_markers(cache_ptr);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "can't remove excess epoch markers.")
        }
    }

    if ( ( (cache_ptr->resize_ctl).decr_mode == H5C_decr__age_out ) 
         ||
         ( ( (cache_ptr->resize_ctl).decr_mode == 
              H5C_decr__age_out_with_threshold 
               )
           &&
           ( hit_rate >= (cache_ptr->resize_ctl).upper_hr_threshold )
         )
       ) {

        if ( cache_ptr->max_cache_size > (cache_ptr->resize_ctl).min_size ){

            /* evict aged out cache entries if appropriate... */
            result = H5C__autoadjust__ageout__evict_aged_out_entries
                     (
                       f,
                       primary_dxpl_id,
                       secondary_dxpl_id,
                       cache_ptr,
                       write_permitted,
                       first_flush_ptr
                     );

            if ( result != SUCCEED ) {

                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                            "error flushing aged out entries.")
            }

            /* ... and then reduce cache size if appropriate */
            if ( cache_ptr->index_size < cache_ptr->max_cache_size ) {

                if ( (cache_ptr->resize_ctl).apply_empty_reserve ) {

                    test_size = (size_t)(((double)cache_ptr->index_size) /
                                (1 - (cache_ptr->resize_ctl).empty_reserve));

                    if ( test_size < cache_ptr->max_cache_size ) {

                        *status_ptr = decrease;
                        *new_max_cache_size_ptr = test_size;
                    }
                } else {

                    *status_ptr = decrease;
                    *new_max_cache_size_ptr = cache_ptr->index_size;
                }

                if ( *status_ptr == decrease ) {

                    /* clip to min size if necessary */
                    if ( *new_max_cache_size_ptr <
                         (cache_ptr->resize_ctl).min_size ) {

                        *new_max_cache_size_ptr = 
                                (cache_ptr->resize_ctl).min_size;
                    }

                    /* clip to max decrement if necessary */
                    if ( ( (cache_ptr->resize_ctl).apply_max_decrement ) &&
                         ( ((cache_ptr->resize_ctl).max_decrement +
                            *new_max_cache_size_ptr) < 
                           cache_ptr->max_cache_size ) ) {

                        *new_max_cache_size_ptr = cache_ptr->max_cache_size - 
                                         (cache_ptr->resize_ctl).max_decrement;
                    }
                }
            }
        } else {

            *status_ptr = at_min_size;
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C__autoadjust__ageout() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__autoadjust__ageout__cycle_epoch_marker
 *
 * Purpose:     Remove the oldest epoch marker from the LRU list,
 *		and reinsert it at the head of the LRU list.  Also
 *		remove the epoch marker's index from the head of the
 *		ring buffer, and re-insert it at the tail of the ring
 *		buffer.
 *
 * Return:      SUCCEED on success/FAIL on failure.
 *
 * Programmer:  John Mainzer, 11/22/04
 *
 * Modifications:
 *
 *              None.
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5C__autoadjust__ageout__cycle_epoch_marker(H5C_t * cache_ptr)
{
    herr_t                      ret_value = SUCCEED;      /* Return value */
    int i;

    FUNC_ENTER_NOAPI_NOINIT(H5C__autoadjust__ageout__cycle_epoch_marker)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );

    if ( cache_ptr->epoch_markers_active <= 0 ) { 

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "No active epoch markers on entry?!?!?.")
    }

    /* remove the last marker from both the ring buffer and the LRU list */

    i = cache_ptr->epoch_marker_ringbuf[cache_ptr->epoch_marker_ringbuf_first];

    cache_ptr->epoch_marker_ringbuf_first = 
            (cache_ptr->epoch_marker_ringbuf_first + 1) % 
            (H5C__MAX_EPOCH_MARKERS + 1);

    cache_ptr->epoch_marker_ringbuf_size -= 1;

    if ( cache_ptr->epoch_marker_ringbuf_size < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "ring buffer underflow.")
    }

    if ( (cache_ptr->epoch_marker_active)[i] != TRUE ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unused marker in LRU?!?")
    }

    H5C__DLL_REMOVE((&((cache_ptr->epoch_markers)[i])), \
                    (cache_ptr)->LRU_head_ptr, \
                    (cache_ptr)->LRU_tail_ptr, \
                    (cache_ptr)->LRU_list_len, \
                    (cache_ptr)->LRU_list_size, \
                    (FAIL))

    /* now, re-insert it at the head of the LRU list, and at the tail of
     * the ring buffer.
     */ 

    HDassert( ((cache_ptr->epoch_markers)[i]).addr == (haddr_t)i );
    HDassert( ((cache_ptr->epoch_markers)[i]).next == NULL );
    HDassert( ((cache_ptr->epoch_markers)[i]).prev == NULL );

    cache_ptr->epoch_marker_ringbuf_last = 
        (cache_ptr->epoch_marker_ringbuf_last + 1) % 
        (H5C__MAX_EPOCH_MARKERS + 1);

    (cache_ptr->epoch_marker_ringbuf)[cache_ptr->epoch_marker_ringbuf_last] = i;

    cache_ptr->epoch_marker_ringbuf_size += 1;

    if ( cache_ptr->epoch_marker_ringbuf_size > H5C__MAX_EPOCH_MARKERS ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "ring buffer overflow.")
    }

    H5C__DLL_PREPEND((&((cache_ptr->epoch_markers)[i])), \
                     (cache_ptr)->LRU_head_ptr, \
                     (cache_ptr)->LRU_tail_ptr, \
                     (cache_ptr)->LRU_list_len, \
                     (cache_ptr)->LRU_list_size, \
                     (FAIL))
done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C__autoadjust__ageout__cycle_epoch_marker() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__autoadjust__ageout__evict_aged_out_entries
 *
 * Purpose:     Evict clean entries in the cache that haven't
 *		been accessed for at least 
 *              (cache_ptr->resize_ctl).epochs_before_eviction epochs,
 *      	and flush dirty entries that haven't been accessed for
 *		that amount of time.
 *
 *		Depending on configuration, the function will either 
 *		flush or evict all such entries, or all such entries it
 *		encounters until it has freed the maximum amount of space
 *		allowed under the maximum decrement.
 *
 *		If we are running in parallel mode, writes may not be
 *		permitted.  If so, the function simply skips any dirty 
 *		entries it may encounter.
 *
 *		The function makes no attempt to maintain the minimum 
 *		clean size, as there is no guarantee that the cache size
 *		will be changed.  
 *
 *		If there is no cache size change, the minimum clean size 
 *		constraint will be met through a combination of clean 
 *		entries and free space in the cache.
 *
 *		If there is a cache size reduction, the minimum clean size
 *		will be re-calculated, and will be enforced the next time
 *		we have to make space in the cache.
 *
 *              The primary_dxpl_id and secondary_dxpl_id parameters
 *              specify the dxpl_ids used depending on the value of 
 *		*first_flush_ptr.  The idea is to use the primary_dxpl_id
 *		on the first write in a sequence of writes, and to use
 *		the secondary_dxpl_id on all subsequent writes.
 *
 *              This is useful in the metadata cache, but may not be 
 *		needed elsewhere.  If so, just use the same dxpl_id for 
 *		both parameters.
 *
 *              Observe that this function cannot occasion a read.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  John Mainzer, 11/22/04
 *
 * Modifications:
 *
 *              None.
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5C__autoadjust__ageout__evict_aged_out_entries(H5F_t * f,
                                                hid_t   primary_dxpl_id,
                                                hid_t   secondary_dxpl_id,
                                                H5C_t * cache_ptr,
                                                hbool_t write_permitted,
                                                hbool_t * first_flush_ptr)
{
    herr_t              ret_value = SUCCEED;      /* Return value */
    herr_t              result;
    size_t		eviction_size_limit;
    size_t		bytes_evicted = 0;
    H5C_cache_entry_t * entry_ptr;
    H5C_cache_entry_t * prev_ptr;

    FUNC_ENTER_NOAPI_NOINIT(H5C__autoadjust__ageout__evict_aged_out_entries)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );

    /* if there is a limit on the amount that the cache size can be decrease
     * in any one round of the cache size reduction algorithm, load that 
     * limit into eviction_size_limit.  Otherwise, set eviction_size_limit
     * to the equivalent of infinity.  The current size of the index will
     * do nicely.
     */
    if ( (cache_ptr->resize_ctl).apply_max_decrement ) {

        eviction_size_limit = (cache_ptr->resize_ctl).max_decrement;

    } else {

        eviction_size_limit = cache_ptr->index_size; /* i.e. infinity */
    }

    if ( write_permitted ) {

        entry_ptr = cache_ptr->LRU_tail_ptr;

        while ( ( entry_ptr != NULL ) &&
                ( (entry_ptr->type)->id != H5C__EPOCH_MARKER_TYPE ) &&
                ( bytes_evicted < eviction_size_limit ) )
        {
            HDassert( ! (entry_ptr->is_protected) );

            prev_ptr = entry_ptr->prev;

            if ( entry_ptr->is_dirty ) {

                result = H5C_flush_single_entry(f,
                                                primary_dxpl_id,
                                                secondary_dxpl_id,
                                                cache_ptr,
                                                entry_ptr->type,
                                                entry_ptr->addr,
                                                (unsigned)0,
                                                first_flush_ptr,
                                                FALSE);
            } else {

                bytes_evicted += entry_ptr->size;

                result = H5C_flush_single_entry(f,
                                                primary_dxpl_id,
                                                secondary_dxpl_id,
                                                cache_ptr,
                                                entry_ptr->type,
                                                entry_ptr->addr,
                                                H5F_FLUSH_INVALIDATE,
                                                first_flush_ptr,
                                                TRUE);
            }

            if ( result < 0 ) {

                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, \
                            "unable to flush entry")
            }

            entry_ptr = prev_ptr;

        } /* end while */

        /* for now at least, don't bother to maintain the minimum clean size,
         * as the cache should now be less than its maximum size.  Due to 
         * the vaguries of the cache size reduction algorthim, we may not
         * reduce the size of the cache.
         *
         * If we do, we will calculate a new minimum clean size, which will
         * be enforced the next time we try to make space in the cache.
         *
         * If we don't, no action is necessary, as we have just evicted and/or
         * or flushed a bunch of entries and therefore the sum of the clean
         * and free space in the cache must be greater than or equal to the 
         * min clean space requirement (assuming that requirement was met on
         * entry).
         */

    } else /* ! write_permitted */  {

        /* since we are not allowed to write, all we can do is evict
         * any clean entries that we may encounter before we either 
         * hit the eviction size limit, or encounter the epoch marker.
         *
         * If we are operating read only, this isn't an issue, as there
         * will not be any dirty entries.
         *
         * If we are operating in R/W mode, all the dirty entries we 
         * skip will be flushed the next time we attempt to make space
         * when writes are permitted.  This may have some local 
         * performance implications, but it shouldn't cause any net 
         * slowdown.
         */

        HDassert( H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS );

        entry_ptr = cache_ptr->LRU_tail_ptr;

        while ( ( entry_ptr != NULL ) &&
                ( (entry_ptr->type)->id != H5C__EPOCH_MARKER_TYPE ) &&
                ( bytes_evicted < eviction_size_limit ) )
        {
            HDassert( ! (entry_ptr->is_protected) );

            prev_ptr = entry_ptr->prev;

            if ( ! (entry_ptr->is_dirty) ) {

                result = H5C_flush_single_entry(f,
                                                primary_dxpl_id,
                                                secondary_dxpl_id,
                                                cache_ptr,
                                                entry_ptr->type,
                                                entry_ptr->addr,
                                                H5F_FLUSH_INVALIDATE,
                                                first_flush_ptr,
                                                TRUE);

                if ( result < 0 ) {

                    HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, \
                                "unable to flush clean entry")
                }
            }
            /* just skip the entry if it is dirty, as we can't do 
             * anything with it now since we can't write.
             */

            entry_ptr = prev_ptr;

        } /* end while */
    }

    if ( cache_ptr->index_size < cache_ptr->max_cache_size ) {

        cache_ptr->cache_full = FALSE;
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C__autoadjust__ageout__evict_aged_out_entries() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__autoadjust__ageout__insert_new_marker
 *
 * Purpose:     Find an unused marker cache entry, mark it as used, and 
 *		insert it at the head of the LRU list.  Also add the 
 *		marker's index in the epoch_markers array.
 *
 * Return:      SUCCEED on success/FAIL on failure.
 *
 * Programmer:  John Mainzer, 11/19/04
 *
 * Modifications:
 *
 *              None.
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5C__autoadjust__ageout__insert_new_marker(H5C_t * cache_ptr)
{
    herr_t                      ret_value = SUCCEED;      /* Return value */
    int i;

    FUNC_ENTER_NOAPI_NOINIT(H5C__autoadjust__ageout__insert_new_marker)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );

    if ( cache_ptr->epoch_markers_active >=
         (cache_ptr->resize_ctl).epochs_before_eviction ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Already have a full complement of markers.")
    }

    /* find an unused marker */
    i = 0;
    while ( ( (cache_ptr->epoch_marker_active)[i] ) && 
            ( i < H5C__MAX_EPOCH_MARKERS ) ) 
    {
        i++;
    }

    HDassert( i < H5C__MAX_EPOCH_MARKERS );

    if ( (cache_ptr->epoch_marker_active)[i] != FALSE ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Can't find unused marker.")
    }

    HDassert( ((cache_ptr->epoch_markers)[i]).addr == (haddr_t)i );
    HDassert( ((cache_ptr->epoch_markers)[i]).next == NULL );
    HDassert( ((cache_ptr->epoch_markers)[i]).prev == NULL );

    (cache_ptr->epoch_marker_active)[i] = TRUE;

    cache_ptr->epoch_marker_ringbuf_last = 
        (cache_ptr->epoch_marker_ringbuf_last + 1) % 
        (H5C__MAX_EPOCH_MARKERS + 1);

    (cache_ptr->epoch_marker_ringbuf)[cache_ptr->epoch_marker_ringbuf_last] = i;

    cache_ptr->epoch_marker_ringbuf_size += 1;

    if ( cache_ptr->epoch_marker_ringbuf_size > H5C__MAX_EPOCH_MARKERS ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "ring buffer overflow.")
    }

    H5C__DLL_PREPEND((&((cache_ptr->epoch_markers)[i])), \
                     (cache_ptr)->LRU_head_ptr, \
                     (cache_ptr)->LRU_tail_ptr, \
                     (cache_ptr)->LRU_list_len, \
                     (cache_ptr)->LRU_list_size, \
                     (FAIL))

    cache_ptr->epoch_markers_active += 1;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C__autoadjust__ageout__insert_new_marker() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__autoadjust__ageout__remove_all_markers
 *
 * Purpose:     Remove all epoch markers from the LRU list and mark them 
 *		as inactive.
 *
 * Return:      SUCCEED on success/FAIL on failure.
 *
 * Programmer:  John Mainzer, 11/22/04
 *
 * Modifications:
 *
 *              None.
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5C__autoadjust__ageout__remove_all_markers(H5C_t * cache_ptr)
{
    herr_t                      ret_value = SUCCEED;      /* Return value */
    int i;
    int ring_buf_index;

    FUNC_ENTER_NOAPI_NOINIT(H5C__autoadjust__ageout__remove_all_markers)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );

    while ( cache_ptr->epoch_markers_active > 0 )
    {
        /* get the index of the last epoch marker in the LRU list 
         * and remove it from the ring buffer.
         */

        ring_buf_index = cache_ptr->epoch_marker_ringbuf_first;
        i = (cache_ptr->epoch_marker_ringbuf)[ring_buf_index];

        cache_ptr->epoch_marker_ringbuf_first = 
            (cache_ptr->epoch_marker_ringbuf_first + 1) % 
            (H5C__MAX_EPOCH_MARKERS + 1);

        cache_ptr->epoch_marker_ringbuf_size -= 1;

        if ( cache_ptr->epoch_marker_ringbuf_size < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "ring buffer underflow.")
        }

        if ( (cache_ptr->epoch_marker_active)[i] != TRUE ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unused marker in LRU?!?")
        }

        /* remove the epoch marker from the LRU list */
        H5C__DLL_REMOVE((&((cache_ptr->epoch_markers)[i])), \
                        (cache_ptr)->LRU_head_ptr, \
                        (cache_ptr)->LRU_tail_ptr, \
                        (cache_ptr)->LRU_list_len, \
                        (cache_ptr)->LRU_list_size, \
                        (FAIL))

        /* mark the epoch marker as unused. */
        (cache_ptr->epoch_marker_active)[i] = FALSE;

        HDassert( ((cache_ptr->epoch_markers)[i]).addr == (haddr_t)i );
        HDassert( ((cache_ptr->epoch_markers)[i]).next == NULL );
        HDassert( ((cache_ptr->epoch_markers)[i]).prev == NULL );

        /* decrement the number of active epoch markers */
        cache_ptr->epoch_markers_active -= 1;

        HDassert( cache_ptr->epoch_markers_active == \
                  cache_ptr->epoch_marker_ringbuf_size );
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C__autoadjust__ageout__remove_all_markers() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__autoadjust__ageout__remove_excess_markers
 *
 * Purpose:     Remove epoch markers from the end of the LRU list and 
 *		mark them as inactive until the number of active markers
 *		equals the the current value of 
 *		(cache_ptr->resize_ctl).epochs_before_eviction.
 *
 * Return:      SUCCEED on success/FAIL on failure.
 *
 * Programmer:  John Mainzer, 11/19/04
 *
 * Modifications:
 *
 *              None.
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5C__autoadjust__ageout__remove_excess_markers(H5C_t * cache_ptr)
{
    herr_t	ret_value = SUCCEED;      /* Return value */
    int		i;
    int		ring_buf_index;

    FUNC_ENTER_NOAPI_NOINIT(H5C__autoadjust__ageout__remove_excess_markers)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );

    if ( cache_ptr->epoch_markers_active <=
         (cache_ptr->resize_ctl).epochs_before_eviction ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "no excess markers on entry.")
    }

    while ( cache_ptr->epoch_markers_active > 
            (cache_ptr->resize_ctl).epochs_before_eviction )
    {
        /* get the index of the last epoch marker in the LRU list 
         * and remove it from the ring buffer.
         */

        ring_buf_index = cache_ptr->epoch_marker_ringbuf_first;
        i = (cache_ptr->epoch_marker_ringbuf)[ring_buf_index];

        cache_ptr->epoch_marker_ringbuf_first = 
            (cache_ptr->epoch_marker_ringbuf_first + 1) % 
            (H5C__MAX_EPOCH_MARKERS + 1);

        cache_ptr->epoch_marker_ringbuf_size -= 1;

        if ( cache_ptr->epoch_marker_ringbuf_size < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "ring buffer underflow.")
        }

        if ( (cache_ptr->epoch_marker_active)[i] != TRUE ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unused marker in LRU?!?")
        }

        /* remove the epoch marker from the LRU list */
        H5C__DLL_REMOVE((&((cache_ptr->epoch_markers)[i])), \
                        (cache_ptr)->LRU_head_ptr, \
                        (cache_ptr)->LRU_tail_ptr, \
                        (cache_ptr)->LRU_list_len, \
                        (cache_ptr)->LRU_list_size, \
                        (FAIL))

        /* mark the epoch marker as unused. */
        (cache_ptr->epoch_marker_active)[i] = FALSE;

        HDassert( ((cache_ptr->epoch_markers)[i]).addr == (haddr_t)i );
        HDassert( ((cache_ptr->epoch_markers)[i]).next == NULL );
        HDassert( ((cache_ptr->epoch_markers)[i]).prev == NULL );

        /* decrement the number of active epoch markers */
        cache_ptr->epoch_markers_active -= 1;

        HDassert( cache_ptr->epoch_markers_active == \
                  cache_ptr->epoch_marker_ringbuf_size );
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C__autoadjust__ageout__remove_excess_markers() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_flush_single_entry
 *
 * Purpose:     Flush or clear (and evict if requested) the cache entry 
 *		with the specified address and type.  If the type is NULL,
 *		any unprotected entry at the specified address will be 
 *		flushed (and possibly evicted).
 *
 *		Attempts to flush a protected entry will result in an 
 *		error.
 *
 *		*first_flush_ptr should be true if only one 
 *		flush is contemplated before the next load, or if this
 *		is the first of a sequence of flushes that will be 
 *		completed before the next load.  *first_flush_ptr is set
 *		to false if a flush actually takes place, and should be 
 *		left false until the end of the sequence.
 *
 *		The primary_dxpl_id is used if *first_flush_ptr is TRUE
 *		on entry, and a flush actually takes place.  The 
 *		secondary_dxpl_id is used in any subsequent flush where 
 *		*first_flush_ptr is FALSE on entry.
 *
 *		If the H5F_FLUSH_CLEAR_ONLY flag is set, the entry will
 *		be cleared and not flushed -- in the case *first_flush_ptr,
 *		primary_dxpl_id, and secondary_dxpl_id are all irrelevent,
 *		and the call can't be part of a sequence of flushes.
 *
 *		If the caller knows the address of the TBBT node at 
 *		which the target entry resides, it can avoid a lookup 
 *		by supplying that address in the tgt_node_ptr parameter.
 *		If this parameter is NULL, the function will do a TBBT
 *		search for the entry instead.
 *
 *		The function does nothing silently if there is no entry
 *		at the supplied address, or if the entry found has the 
 *		wrong type.
 *
 * Return:      Non-negative on success/Negative on failure or if there was 
 *		an attempt to flush a protected item.
 *
 * Programmer:  John Mainzer, 5/5/04
 *
 * Modifications:
 *
 *		JRM -- 7/21/04
 *		Updated function for the addition of the hash table.
 *
 *		QAK -- 11/26/04
 *		Updated function for the switch from TBBTs to skip lists.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5C_flush_single_entry(H5F_t *		   f, 
                       hid_t 		   primary_dxpl_id, 
                       hid_t 		   secondary_dxpl_id, 
                       H5C_t *		   cache_ptr,
                       const H5C_class_t * type_ptr, 
                       haddr_t		   addr, 
                       unsigned		   flags,
                       hbool_t *	   first_flush_ptr,
                       hbool_t		   del_entry_from_slist_on_destroy)
{
    hbool_t		destroy = ( (flags & H5F_FLUSH_INVALIDATE) != 0 );
    hbool_t		clear_only = ( (flags & H5F_FLUSH_CLEAR_ONLY) != 0);  
    herr_t		ret_value = SUCCEED;      /* Return value */
    herr_t		status;
    H5C_cache_entry_t *	entry_ptr = NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5C_flush_single_entry)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->skip_file_checks || f );
    HDassert( H5F_addr_defined(addr) );
    HDassert( first_flush_ptr );

    /* attempt to find the target entry in the hash table */
    H5C__SEARCH_INDEX(cache_ptr, addr, entry_ptr, FAIL)

#if H5C_DO_SANITY_CHECKS
    if ( entry_ptr != NULL ) {

        if ( entry_ptr->in_slist ) {

            if ( entry_ptr->addr != addr ) {

                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                            "entry in slist failed sanity checks.")
            }
        } else {

            if ( ( entry_ptr->is_dirty ) || ( entry_ptr->addr != addr ) ) {

                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                            "entry failed sanity checks.")
            }
        }
    }
#if 0 
    /* this should be useful for debugging from time to time.
     * lets leave it in for now.       -- JRM 12/15/04
     */
    else {
        HDfprintf(stdout, 
                  "H5C_flush_single_entry(): non-existant entry. addr = %a\n",
                  addr);
        HDfflush(stdout);
    }
#endif
#endif /* H5C_DO_SANITY_CHECKS */

    if ( ( entry_ptr != NULL ) && ( entry_ptr->is_protected ) )
    {

        /* Attempt to flush a protected entry -- scream and die. */
        HGOTO_ERROR(H5E_CACHE, H5E_PROTECT, FAIL, \
                    "Attempt to flush a protected entry.")
    }

    if ( ( entry_ptr != NULL ) &&
         ( ( type_ptr == NULL ) || ( type_ptr->id == entry_ptr->type->id ) ) )
    {
        /* we have work to do */

#ifdef H5_HAVE_PARALLEL  
#ifndef NDEBUG

        /* If MPI based VFD is used, do special parallel I/O sanity checks.
         * Note that we only do these sanity checks when the clear_only flag
         * is not set, and the entry to be flushed is dirty.  Don't bother 
         * otherwise as no file I/O can result.
         *
         * There are also cases (testing for instance) where it is convenient
         * to pass in dummy dxpl_ids.  Since we don't use the dxpl_ids directly,
         * this isn't a problem -- but we do have to turn off sanity checks
         * involving them.  We use cache_ptr->skip_dxpl_id_checks to do this.
         */
        if ( ( ! cache_ptr->skip_dxpl_id_checks ) && 
             ( ! clear_only ) && 
             ( entry_ptr->is_dirty ) && 
             ( IS_H5FD_MPI(f) ) ) {

            H5P_genplist_t *dxpl;           /* Dataset transfer property list */
            H5FD_mpio_xfer_t xfer_mode;     /* I/O xfer mode property value */

            /* Get the dataset transfer property list */
            if ( NULL == (dxpl = H5I_object(primary_dxpl_id)) ) {

                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, \
                            "not a dataset creation property list")
            }

            /* Get the transfer mode property */
            if( H5P_get(dxpl, H5D_XFER_IO_XFER_MODE_NAME, &xfer_mode) < 0 ) {

                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, \
                            "can't retrieve xfer mode")
            }

            /* Sanity check transfer mode */
            HDassert( xfer_mode == H5FD_MPIO_COLLECTIVE || IS_H5FD_FPHDF5(f) );
        }

#endif /* NDEBUG */
#endif /* H5_HAVE_PARALLEL */

        if ( clear_only ) {
            H5C__UPDATE_STATS_FOR_CLEAR(cache_ptr, entry_ptr)
        } else {
            H5C__UPDATE_STATS_FOR_FLUSH(cache_ptr, entry_ptr)
        }

        if ( destroy ) {
            H5C__UPDATE_STATS_FOR_EVICTION(cache_ptr, entry_ptr)
        }

        /* Always remove the entry from the hash table on a destroy.  On a 
         * flush with destroy, it is cheaper to discard the skip list all at 
         * once rather than remove the entries one by one, so we only delete 
         * from the list if requested.
         *
         * We must do deletions now as the callback routines will free the 
         * entry if destroy is true.
         */
        if ( destroy ) {

            H5C__DELETE_FROM_INDEX(cache_ptr, entry_ptr)

            if ( ( entry_ptr->in_slist ) && 
                 ( del_entry_from_slist_on_destroy ) ) {

                H5C__REMOVE_ENTRY_FROM_SLIST(cache_ptr, entry_ptr)
            }
        }

        /* Update the replacement policy for the flush or eviction.
         * Again, do this now so we don't have to reference freed 
         * memory in the destroy case.
         */
        if ( destroy ) { /* AKA eviction */

            H5C__UPDATE_RP_FOR_EVICTION(cache_ptr, entry_ptr, FAIL)

        } else {

            H5C__UPDATE_RP_FOR_FLUSH(cache_ptr, entry_ptr, FAIL)
        }

        /* Clear the dirty flag only, if requested */
        if ( clear_only ) {

            /* Call the callback routine to clear all dirty flags for object */
            if ( (entry_ptr->type->clear)(f, entry_ptr, destroy) < 0 ) {

                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "can't clear entry")
            }
        } else {

            /* Only block for all the processes on the first piece of metadata 
             */

            if ( *first_flush_ptr && entry_ptr->is_dirty ) {

                status = (entry_ptr->type->flush)(f, primary_dxpl_id, destroy, 
                                                 entry_ptr->addr, entry_ptr);
                *first_flush_ptr = FALSE;

            } else {

                status = (entry_ptr->type->flush)(f, secondary_dxpl_id, 
                                                 destroy, entry_ptr->addr, 
                                                 entry_ptr);
            }

            if ( status < 0 ) {

                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, \
                            "unable to flush entry")
            }
        }

        if ( ! destroy ) {
        
            HDassert( !(entry_ptr->is_dirty) );
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_flush_single_entry() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_load_entry
 *
 * Purpose:     Attempt to load the entry at the specified disk address
 *		and with the specified type into memory.  If successful.
 *		return the in memory address of the entry.  Return NULL
 *		on failure.
 *
 *		Note that this function simply loads the entry into 
 *		core.  It does not insert it into the cache.
 *
 * Return:      Non-NULL on success / NULL on failure.
 *
 * Programmer:  John Mainzer, 5/18/04
 *
 * Modifications:
 *
 *		JRM - 7/21/04
 *		Updated function for the addition of the hash table.
 *
 *-------------------------------------------------------------------------
 */

static void *
H5C_load_entry(H5F_t *             f, 
               hid_t               dxpl_id, 
               const H5C_class_t * type, 
               haddr_t             addr,
               const void *        udata1, 
               void *              udata2,
               hbool_t		   skip_file_checks)
{
    void *		thing = NULL;
    void *		ret_value = NULL;
    H5C_cache_entry_t *	entry_ptr = NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5C_load_entry)

    HDassert( skip_file_checks || f );
    HDassert( type );
    HDassert( type->load );
    HDassert( type->size );
    HDassert( H5F_addr_defined(addr) );

    if ( NULL == (thing = (type->load)(f, dxpl_id, addr, udata1, udata2)) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTLOAD, NULL, "unable to load entry")

    }

    entry_ptr = (H5C_cache_entry_t *)thing;

    HDassert( entry_ptr->is_dirty == FALSE );

    entry_ptr->addr = addr;
    entry_ptr->type = type;
    entry_ptr->is_protected = FALSE;
    entry_ptr->in_slist = FALSE;

    if ( (type->size)(f, thing, &(entry_ptr->size)) < 0 ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGETSIZE, NULL, \
                    "Can't get size of thing")
    }

    HDassert( entry_ptr->size < H5C_MAX_ENTRY_SIZE ); 

    entry_ptr->ht_next = NULL;
    entry_ptr->ht_prev = NULL;

    entry_ptr->next = NULL;
    entry_ptr->prev = NULL;

    entry_ptr->aux_next = NULL;
    entry_ptr->aux_prev = NULL;

    H5C__RESET_CACHE_ENTRY_STATS(entry_ptr);

    ret_value = thing;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_load_entry() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_make_space_in_cache
 *
 * Purpose:     Attempt to evict cache entries until the index_size
 *		is at least needed_space below max_cache_size.
 *
 *		In passing, also attempt to bring cLRU_list_size to a 
 *		value greater than min_clean_size.
 *
 *		Depending on circumstances, both of these goals may
 *		be impossible, as in parallel mode, we must avoid generating
 *		a write as part of a read (to avoid deadlock in collective
 *		I/O), and in all cases, it is possible (though hopefully
 *		highly unlikely) that the protected list may exceed the 
 *		maximum size of the cache.
 *		
 *		Thus the function simply does its best, returning success
 *		unless an error is encountered.
 *
 *		The primary_dxpl_id and secondary_dxpl_id parameters 
 *		specify the dxpl_ids used on the first write occasioned
 *		by the call (primary_dxpl_id), and on all subsequent 
 *		writes (secondary_dxpl_id).  This is useful in the metadata 
 *		cache, but may not be needed elsewhere.  If so, just use the 
 *		same dxpl_id for both parameters.
 *
 *		Observe that this function cannot occasion a read.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  John Mainzer, 5/14/04
 *
 * Modifications:
 *
 *		JRM --7/21/04
 *		Minor modifications in support of the addition of a hash
 *		table to facilitate lookups.
 *
 *		JRM -- 11/22/04
 *		Added the first_flush_ptr parameter, which replaces the 
 *		old first_flush local variable.  This allows the function
 *		to coordinate on the first flush issue with other functions.
 *
 *		JRM -- 12/13/04
 *		Added code to skip over epoch markers if present.
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5C_make_space_in_cache(H5F_t *	f,
                        hid_t	primary_dxpl_id,
                        hid_t	secondary_dxpl_id,
                        H5C_t *	cache_ptr,
		        size_t	space_needed,
                        hbool_t	write_permitted,
                        hbool_t * first_flush_ptr)
{
    herr_t		ret_value = SUCCEED;      /* Return value */
    herr_t		result;
    int32_t		entries_examined = 0;
    int32_t		initial_list_len;
    H5C_cache_entry_t *	entry_ptr;
    H5C_cache_entry_t *	prev_ptr;

    FUNC_ENTER_NOAPI_NOINIT(H5C_make_space_in_cache)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( first_flush_ptr != NULL );
    HDassert( ( *first_flush_ptr == TRUE ) || ( *first_flush_ptr == FALSE ) );

    if ( write_permitted ) {

        initial_list_len = cache_ptr->LRU_list_len;
        entry_ptr = cache_ptr->LRU_tail_ptr;

        while ( ( (cache_ptr->index_size + space_needed) 
                  > 
                  cache_ptr->max_cache_size
                )
                &&
                ( entries_examined <= (2 * initial_list_len) )
                &&
                ( entry_ptr != NULL )
              )
        {
            HDassert( ! (entry_ptr->is_protected) );

            prev_ptr = entry_ptr->prev;

            if ( (entry_ptr->type)->id != H5C__EPOCH_MARKER_TYPE ) {

                if ( entry_ptr->is_dirty ) {

                    result = H5C_flush_single_entry(f, 
                                                    primary_dxpl_id, 
                                                    secondary_dxpl_id,
                                                    cache_ptr,
                                                    entry_ptr->type, 
                                                    entry_ptr->addr, 
                                                    (unsigned)0, 
                                                    first_flush_ptr, 
                                                    FALSE);
                } else {

                    result = H5C_flush_single_entry(f, 
                                                    primary_dxpl_id, 
                                                    secondary_dxpl_id,
                                                    cache_ptr,
                                                    entry_ptr->type, 
                                                    entry_ptr->addr, 
                                                    H5F_FLUSH_INVALIDATE,
                                                    first_flush_ptr, 
                                                    TRUE);
                }
            } else {

                /* Skip epoch markers.  Set result to SUCCEED to avoid
                 * triggering the error code below.
                 */
                result = SUCCEED;
            }

            if ( result < 0 ) {

                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, \
                            "unable to flush entry")
            }

            entry_ptr = prev_ptr;
        }

#if H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS

        initial_list_len = cache_ptr->dLRU_list_len;
        entry_ptr = cache_ptr->dLRU_tail_ptr;

        while ( ( cache_ptr->cLRU_list_size < cache_ptr->min_clean_size ) &&
                ( entries_examined <= initial_list_len ) &&
                ( entry_ptr != NULL )
              )
        {
            HDassert( ! (entry_ptr->is_protected) );
            HDassert( entry_ptr->is_dirty );
            HDassert( entry_ptr->in_slist );

            prev_ptr = entry_ptr->aux_prev;

            result = H5C_flush_single_entry(f, 
                                            primary_dxpl_id, 
                                            secondary_dxpl_id,
                                            cache_ptr,
                                            entry_ptr->type, 
                                            entry_ptr->addr, 
                                            (unsigned)0, 
                                            first_flush_ptr, 
                                            FALSE);

            if ( result < 0 ) {

                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, \
                            "unable to flush entry")
            }

            entry_ptr = prev_ptr;
        }

#endif /* H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

    } else {

        HDassert( H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS );

        initial_list_len = cache_ptr->cLRU_list_len;
        entry_ptr = cache_ptr->cLRU_tail_ptr;

        while ( ( (cache_ptr->index_size + space_needed) 
                  > 
                  cache_ptr->max_cache_size
                )
                &&
                ( entries_examined <= initial_list_len ) 
                &&
                ( entry_ptr != NULL )
              )
        {
            HDassert( ! (entry_ptr->is_protected) );
            HDassert( ! (entry_ptr->is_dirty) );

            prev_ptr = entry_ptr->aux_prev;

            result = H5C_flush_single_entry(f, 
                                            primary_dxpl_id, 
                                            secondary_dxpl_id,
                                            cache_ptr,
                                            entry_ptr->type, 
                                            entry_ptr->addr, 
                                            H5F_FLUSH_INVALIDATE,
                                            first_flush_ptr, 
                                            TRUE);

            if ( result < 0 ) {

                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, \
                            "unable to flush entry")
            }

            entry_ptr = prev_ptr;
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_make_space_in_cache() */
