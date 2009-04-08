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
 * Created:     H5C2journal.c
 *              Dec 6 2007
 *              John Mainzer
 *
 * Purpose:     This file is a general catchall for functions supporting
 *              metadata journaling.  Note that journaling must be tighly
 *              integrated with the metadata cache, and thus this file only
 *              contains only that code that can be easily separated from 
 *              the rest of the cache code.
 *
 *              Observe also that to minimize overhead, it is quite possible
 *              that many of the functions in this file will be converted
 *              into macros at some point in the future.  
 *
 * Modifications:
 *
 *              None.
 *
 *-------------------------------------------------------------------------
 */

#define H5F_PACKAGE             /* suppress error about including H5Fpkg  */
#define H5C2_PACKAGE            /* suppress error about including H5C2pkg */

#include "H5private.h"          /* Generic Functions                    */
#include "H5Eprivate.h"         /* Error handling                       */
#include "H5MMprivate.h"        /* Memory management                    */
#include "H5MFprivate.h"        /* File memory management               */
#include "H5Fpkg.h"		/* File access                          */
#include "H5C2pkg.h"            /* Cache                                */
#include "H5Pprivate.h"		/* Property lists			*/

/**************************************************************************/
/***************************** global variables ***************************/
/**************************************************************************/

/* In the test code, it is sometimes useful to skip the check for journaling
 * in progress on open.  The check_for_journaling global is used to support
 * this.  Note that we can't tuck this variable into H5C2_t, as the test
 * takes place before H5Fopen() returns.
 */

hbool_t H5C2__check_for_journaling = TRUE;

/**************************************************************************/
/***************************** local prototypes ***************************/
/**************************************************************************/

static herr_t H5C2_call_mdjsc_callbacks(H5C2_t * cache_ptr,
                                        hid_t dxpl_id,
                                        H5C2_mdj_config_t * config_ptr);

static herr_t H5C2_grow_mdjsc_callback_table(H5C2_t * cache_ptr);

static herr_t H5C2_shrink_mdjsc_callback_table(H5C2_t * cache_ptr);

static herr_t H5C2_get_journaling_in_progress(const H5F_t * f,
					      H5C2_t * cache_ptr);


/**************************************************************************/
/************************* journaling code proper *************************/
/**************************************************************************/

/*-------------------------------------------------------------------------
 * Function:    H5C2_begin_journaling
 *
 * Purpose:     Setup the metadata cache to begin journaling.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              March 26, 2008
 *
 * Changes:	JRM -- 8/14/08
 *              Reworked the function to use the H5C2_mdj_config_t
 *              structure.
 *
 *		JRM -- 8/18/08
 *		Added code to flush the cache before journaling 
 *		starts, and to call the metadata journaling status
 *		change callbacks after journaling has been started.
 *
 *		JRM -- 2/10/09
 *		Added journal_magic variable and supporting code.  
 *
 *		The idea is to assign a random magic number to both the 
 *		journal file, and to the journal configuration information
 *		information in the super block so that it will be hard to
 *		apply the wrong journal file to a corrupted hdf5 file.
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C2_begin_journaling(H5F_t * f,
		      hid_t dxpl_id,
		      H5C2_t * cache_ptr,
		      H5C2_mdj_config_t * config_ptr)
{
    herr_t result;
    herr_t ret_value = SUCCEED;      /* Return value */
    int32_t journal_magic;
    H5C2_mdj_config_t config;

    FUNC_ENTER_NOAPI(H5C2_begin_journaling, FAIL)
#if 0 /* JRM */
    HDfprintf(stdout, "%s entering.\n", FUNC);
#endif /* JRM */ 
    HDassert( f != NULL );
    HDassert( f->name != NULL );
    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C2__H5C2_T_MAGIC );
    HDassert( cache_ptr->mdj_enabled == FALSE );
    HDassert( cache_ptr->trans_in_progress == FALSE );
    HDassert( cache_ptr->trans_num == 0 );
    HDassert( cache_ptr->last_trans_on_disk == 0 );
    HDassert( cache_ptr->tl_len == 0 );
    HDassert( cache_ptr->tl_size == 0 );
    HDassert( cache_ptr->tl_head_ptr == NULL );
    HDassert( cache_ptr->tl_tail_ptr == NULL );
    HDassert( cache_ptr->jwipl_len == 0 );
    HDassert( cache_ptr->jwipl_size == 0 );
    HDassert( cache_ptr->jwipl_head_ptr == NULL );
    HDassert( cache_ptr->jwipl_tail_ptr == NULL );
    HDassert( config_ptr != NULL );
    HDassert( config_ptr->jbrb_buf_size > 0 );
    HDassert( config_ptr->jbrb_num_bufs > 0 );
    HDassert( HDstrlen(config_ptr->journal_file_path) > 0 );

    if ( cache_ptr->mdj_enabled ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "metadata journaling already enabled on entry.")
    }

#if 0 /* JRM */
    HDfprintf(stdout, "%s Finished initial sanity checks.\n", FUNC);
#endif /* JRM */ 

    result = H5C2_flush_cache(f, dxpl_id, H5C2__NO_FLAGS_SET);

    if ( result < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_flush_cache() failed.") 
    }

#if 0 /* JRM */
    HDfprintf(stdout, "%s Finished flushing the cache.\n", FUNC);
#endif /* JRM */ 

    journal_magic = (int32_t)HDrand();

#if 0 /* JRM */
    HDfprintf(stdout, "%s journal_magic = %d.\n", FUNC, (int)journal_magic);
#endif /* JRM */ 

#if 0 /* JRM */
    HDfprintf(stdout, "%s calling H5C2_jb__init().\n", FUNC);
#endif /* JRM */ 

    result = H5C2_jb__init(&(cache_ptr->mdj_jbrb),
                           journal_magic,
		           f->name,
			   config_ptr->journal_file_path,
			   config_ptr->jbrb_buf_size,
			   config_ptr->jbrb_num_bufs,
			   config_ptr->jbrb_use_aio,
			   config_ptr->jbrb_human_readable);

    if ( result != SUCCEED ) {
#if 0 /* JRM */
        HDfprintf(stdout, "%s: H5C2_jb__init() failed.\n", FUNC);
#endif /* JRM */
        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_jb__init() failed.")
    }

#if 0 /* JRM */
        HDfprintf(stdout, "%s calling H5C2_mark_journaling_in_progress().\n", 
		  FUNC);
#endif /* JRM */ 

    /* Note that this call flushes the HDF5 file in passing */
    result = H5C2_mark_journaling_in_progress(f, dxpl_id, journal_magic,
		                              config_ptr->journal_file_path);

    if ( result != SUCCEED ) {
#if 0 /* JRM */
        HDfprintf(stdout, 
                  "%s: H5C2_mark_journaling_in_progress() failed.\n", FUNC);
#endif /* JRM */
        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_mark_journaling_in_progress() failed.")
    }

    cache_ptr->mdj_enabled = TRUE;

    result = H5C2_get_journal_config(cache_ptr, &config);

    if ( result < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_get_journal_config() failed.")
    }

    result = H5C2_call_mdjsc_callbacks(cache_ptr, dxpl_id, &config);

    if ( result < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_call_mdjsc_callbacks() failed.")
    }

#if 0 /* JRM */
        HDfprintf(stdout, "%s exiting.\n", FUNC);
#endif /* JRM */ 

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C2_begin_journaling() */


/*-------------------------------------------------------------------------
 * Function:    H5C2_begin_transaction
 *
 * Purpose:     Handle book keeping for the beginning of a transaction, and
 *              return the transaction ID assigned to the transaction in 
 *              *trans_num_ptr.  
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              March 18, 2008
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C2_begin_transaction(H5C2_t * cache_ptr,
                       uint64_t * trans_num_ptr,
                       const char * api_call_name)
{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C2_begin_transaction, FAIL)
#if 0 /* JRM */
    HDfprintf(stdout, "%s entering -- call name = \"%s\".\n", 
              FUNC, api_call_name);
    HDfprintf(stdout, "%s cache_ptr->mdj_enabled = %d.\n", 
              FUNC, (int)(cache_ptr->mdj_enabled));
#endif /* JRM */ 
    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C2__H5C2_T_MAGIC );
    HDassert( cache_ptr->tl_len == 0 );
    HDassert( cache_ptr->tl_size == 0 );
    HDassert( cache_ptr->tl_head_ptr == NULL );
    HDassert( cache_ptr->tl_tail_ptr == NULL );
    HDassert( trans_num_ptr != NULL );
    HDassert( api_call_name != NULL );
    HDassert( HDstrlen(api_call_name) <= H5C2__MAX_API_NAME_LEN );

    if ( cache_ptr->mdj_enabled ) {

        if ( cache_ptr->trans_in_progress ) {

	    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "transaction already in progress?.")
        }

	HDstrncpy(cache_ptr->trans_api_name, api_call_name,
                  (size_t)H5C2__MAX_API_NAME_LEN);

        (cache_ptr->trans_num)++;

        *trans_num_ptr = cache_ptr->trans_num;

        cache_ptr->trans_in_progress = TRUE;
    }

done:
#if 0 /* JRM */
    HDfprintf(stdout, "%s exiting -- cache_ptr->trans_num = %lld.\n", 
              FUNC, cache_ptr->trans_num);
#endif /* JRM */
    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C2_begin_transaction() */


/*-------------------------------------------------------------------------
 * Function:    H5C2_end_journaling
 *
 * Purpose:     Shutdown metadata journaling.
 *
 *              To do this we must:
 *
 *              1) Flush the cache.  This will also flush and truncate the
 *                 journal file.
 *
 *              2) Mark the superblock to indicate that we are no longer
 *                 journaling.  Note that this will flush the HDF5 file 
 *                 again in passing.
 *
 *              3) Tell the journal file write code to shutdown.  This will
 *                 also cause the journal file to be deleted.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              April 12, 2008
 *
 * Changes:	Added code to call the metadata journaling status change
 *		callback function.
 *						JR -- 8/18/08
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C2_end_journaling(H5F_t * f,
                    hid_t dxpl_id,
		    H5C2_t * cache_ptr)
{
    herr_t result;
    herr_t ret_value = SUCCEED;      /* Return value */
    H5C2_mdj_config_t config;

    FUNC_ENTER_NOAPI(H5C2_end_journaling, FAIL)
#if 0 /* JRM */
    HDfprintf(stdout, "%s: entering.\n", FUNC);
#endif /* JRM */    
    HDassert( f != NULL );
    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C2__H5C2_T_MAGIC );

    if ( cache_ptr->mdj_enabled ) {

#if 0 /* JRM */
        HDfprintf(stdout, "%s: taking down journaling.\n", FUNC);
#endif /* JRM */    

	HDassert( cache_ptr->mdj_enabled );
        HDassert( cache_ptr->trans_in_progress == FALSE );
        HDassert( cache_ptr->tl_len == 0 );
        HDassert( cache_ptr->tl_size == 0 );
        HDassert( cache_ptr->tl_head_ptr == NULL );
        HDassert( cache_ptr->tl_tail_ptr == NULL );

        result = H5C2_flush_cache(f, dxpl_id, H5C2__NO_FLAGS_SET);

        if ( result < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                             "H5C2_flush_cache() failed.") 
	}

	HDassert( cache_ptr->mdj_enabled );

        /* Turn off journaling now, before attempting to modify the superblock
         *      extension (which is really an object header) and having the
         *      object header code call into the cache, which gets confused
         *      because there's no transaction in progress. -QAK
         */
        cache_ptr->mdj_enabled = FALSE;

        /* Remove the journal configuration information from the superblock
         * extension.  In passing, also discard the cache's copies of the 
         * metadata journaling magic, and the journal file name.
         */
        result = H5C2_unmark_journaling_in_progress(f, dxpl_id, cache_ptr);

        if ( result < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C2_unmark_journaling_in_progress() failed.")
        }

        result = H5C2_jb__takedown(&(cache_ptr->mdj_jbrb));

        if ( result < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C2_jb__takedown() failed.")
        }

        result = H5C2_get_journal_config(cache_ptr, &config);

        if ( result < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C2_get_journal_config() failed.")
        }

        result = H5C2_call_mdjsc_callbacks(cache_ptr, dxpl_id, &config);

        if ( result < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C2_call_mdjsc_callbacks() failed.")
        }
    }

done:

#if 0 /* JRM */
    HDfprintf(stdout, "%s: exiting.\n", FUNC);
#endif /* JRM */    

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C2_end_journaling() */



/*-------------------------------------------------------------------------
 * Function:    H5C2_end_transaction
 *
 * Purpose:     Handle book keeping for the end of a transaction.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              March 18, 2008
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C2_end_transaction(H5F_t * f,
		     hid_t dxpl_id,
                     H5C2_t * cache_ptr,
                     uint64_t trans_num,
                     const char * api_call_name)
{
    uint64_t new_last_trans_on_disk = 0;
    herr_t result;
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C2_end_transaction, FAIL)
#if 0 /* JRM */
    HDfprintf(stdout, "%s call = \"%s\", trans_num = %lld, tl_len = %d.\n", 
              FUNC, api_call_name, trans_num, (int)(cache_ptr->tl_len));
#endif /* JRM */ 
    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C2__H5C2_T_MAGIC );
    HDassert( api_call_name != NULL );
    HDassert( HDstrlen(api_call_name) <= H5C2__MAX_API_NAME_LEN );
    HDassert( ( ! ( cache_ptr->mdj_enabled ) ) ||
              ( HDstrcmp(api_call_name, cache_ptr->trans_api_name) == 0 ) );

    if ( cache_ptr->mdj_enabled ) {

        if ( ! ( cache_ptr->trans_in_progress ) ) {
#if 0 /* JRM */
            HDfprintf(stdout, "%s: transaction not in progress?!?!\n", FUNC);
#endif /* JRM */
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "transaction not in progress?!?!")
        }

        if ( cache_ptr->trans_num != trans_num ) {

#if 0 /* JRM */
            HDfprintf(stdout, "%s: trans_num mis-match (%lld/%lld)\n", 
                      FUNC, (long long)(trans_num), 
		      (long long)(cache_ptr->trans_num));
#endif /* JRM */
	    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "trans_num mis-match?!?!")
        }

        /* if the transaction list is not empty, generate journal messages,
         * and remove all entries from the transaction list.
         */
        if ( cache_ptr->tl_len > 0 ) {

            result = H5C2_journal_transaction(f, dxpl_id, cache_ptr);

            if ( result != SUCCEED ) {

#if 0 /* JRM */
                HDfprintf(stdout, "%s: H5C2_journal_transaction() failed.\n", 
                          FUNC);
#endif /* JRM */
                HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                            "H5C2_journal_transaction() failed.")
            }
        }
#if 0 /* JRM */
	HDfprintf(stdout, "%s: setting cache_ptr->trans_in_progress = FALSE.\n",
		  FUNC);
#endif /* JRM */
        cache_ptr->trans_in_progress = FALSE;

        /* Get the last transaction on disk.  If it has changed, remove
         * all entries with completed journal writes from the journal write
         * in progress list.
         */

        result = H5C2_jb__get_last_transaction_on_disk(&(cache_ptr->mdj_jbrb),
                                                       &new_last_trans_on_disk);
        if ( result != SUCCEED ) {

#if 0 /* JRM */
            HDfprintf(stdout, 
                      "%s: H5C2_jb__get_last_transaction_on_disk() failed.\n", 
                      FUNC);
#endif /* JRM */
            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C2_jb__get_last_transaction_on_disk() failed.")
        }

        if ( cache_ptr->last_trans_on_disk < new_last_trans_on_disk ) {

            result = H5C2_update_for_new_last_trans_on_disk(cache_ptr,
		                                        new_last_trans_on_disk);

	    if ( result != SUCCEED ) {

#if 0 /* JRM */
                HDfprintf(stdout, 
                      "%s: H5C2_update_for_new_last_trans_on_disk() failed.\n", 
                      FUNC);
#endif /* JRM */
                HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                            "H5C2_update_for_new_last_trans_on_disk() failed.")
            }
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C2_end_transaction() */


/*-------------------------------------------------------------------------
 * Function:    H5C2_get_journal_config
 *
 * Purpose:     Return the current metadata journaling status in an
 *              instance of H5C2_mdj_config_t.
 *
 *              If journaling is enabled, config_ptr->enable_journaling 
 *              is set to TRUE, and the remaining fields in *config_ptr
 *              will be set to reflect current journaling status.
 *
 *              If journaling is disabled, config_ptr->enable_journaling
 *              is set to FALSE, and the remaining fields of *config_ptr
 *              are undefined.
 * 		
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              April 13, 2008
 *
 * Changes:
 *
 *              JRM -- 8/14/08
 *              Reworked function to use H5C2_mdj_config_t.
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C2_get_journal_config(H5C2_t * cache_ptr,
		        H5C2_mdj_config_t * config_ptr)
{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C2_get_journal_config, FAIL)

    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C2__H5C2_T_MAGIC );

    if ( config_ptr == NULL ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "config_ptr NULL on entry!?!.")
    }

    if ( cache_ptr->mdj_enabled ) {

        config_ptr->enable_journaling = TRUE;

	HDstrncpy(&(config_ptr->journal_file_path[0]), 
		  cache_ptr->jnl_file_name,
		  H5C2__MAX_JOURNAL_FILE_NAME_LEN);

	config_ptr->journal_file_path[H5C2__MAX_JOURNAL_FILE_NAME_LEN] = '\0';

	config_ptr->jbrb_buf_size = (cache_ptr->mdj_jbrb).buf_size;

	config_ptr->jbrb_num_bufs = (cache_ptr->mdj_jbrb).num_bufs;

	config_ptr->jbrb_use_aio = (cache_ptr->mdj_jbrb).use_aio;

	config_ptr->jbrb_human_readable = (cache_ptr->mdj_jbrb).human_readable;
	
    } else {

        config_ptr->enable_journaling = FALSE;
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C2_get_journal_config() */


/*-------------------------------------------------------------------------
 * Function:    H5C2_journal_post_flush()
 *
 * Purpose:     Handle any journaling activities that are necessary
 * 		after we flush the metadata cache.
 *
 * 		At present this means:
 *
 * 		1) Verify that a transaction is still not in progress.
 *
 * 		2) Verify that the journal write in progress list
 * 		   is still empty.
 *
 * 		3) If the cache_is_clean parameter is true:
 *
 * 		   a) Flush the HDF5 file
 *
 * 		   b) Truncate the journal file
 *
 * 		   c) Reset cache_ptr->trans_num and 
 * 		      cache_ptr->last_trans_on_disk to zero.
 * 		
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              April 10, 2008
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C2_journal_post_flush(const H5F_t * f,
                        hid_t dxpl_id,
                        H5C2_t * cache_ptr,
		        hbool_t cache_is_clean)
{
    herr_t result;
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C2_journal_post_flush, FAIL)

    HDassert( f != NULL );
    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C2__H5C2_T_MAGIC );
    HDassert( cache_ptr->mdj_enabled );

    if ( cache_ptr->trans_in_progress ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Transaction in progress during flush?!?!?.")
    }

    if ( cache_ptr->jwipl_len != 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "journal write in progress list isn't empty?!?!.")
    }

    if ( cache_is_clean ) {

        /* Write the superblock to disk */

        result = H5F_super_write(f, dxpl_id);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_WRITEERROR, FAIL, \
                        "unable to write superblock to file")
        }

	result = H5FD_flush(f->shared->lf, dxpl_id, (unsigned)0);

	if ( result > 0 ) {

	    HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		        "low level flush failed")
	}

        result = H5C2_jb__trunc(&(cache_ptr->mdj_jbrb));

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C2_jb__trunc() failed.")
        }
        
	cache_ptr->trans_num = (uint64_t)0;
	cache_ptr->last_trans_on_disk = (uint64_t)0;
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C2_journal_post_flush() */


/*-------------------------------------------------------------------------
 * Function:    H5C2_journal_pre_flush()
 *
 * Purpose:     Handle any journaling activities that are necessary
 * 		before we flush the metadata cache.
 *
 * 		At present this means:
 *
 * 		1) Verify that a transaction is not in progress.
 *
 * 		2) Flush the journal to disk.
 *
 * 		3) Get the ID of the last transaction on disk.
 *
 * 		4) If the value obtained in 3) above has changed,
 * 		   remove all entries whose last transaction has 
 * 		   made it to disk from the journal write in progress
 * 		   list.
 *
 * 		5) Verify that the journal write in progress list is
 * 		   empty.
 * 		
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              April 10, 2008
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C2_journal_pre_flush(H5C2_t * cache_ptr)
{
    herr_t result;
    uint64_t new_last_trans_on_disk;
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C2_journal_pre_flush, FAIL)

    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C2__H5C2_T_MAGIC );
    HDassert( cache_ptr->mdj_enabled );

    if ( cache_ptr->trans_in_progress ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Transaction in progress during flush?!?!?.")
    }

    result = H5C2_jb__flush(&(cache_ptr->mdj_jbrb));

    if ( result != SUCCEED ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_jb__flush() failed.")
    }

    result = H5C2_jb__get_last_transaction_on_disk(&(cache_ptr->mdj_jbrb),
		                                   &new_last_trans_on_disk);
    if ( result != SUCCEED ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_jb__get_last_transaction_on_disk() failed.")
    }

    if ( cache_ptr->last_trans_on_disk < new_last_trans_on_disk ) {

        result = H5C2_update_for_new_last_trans_on_disk(cache_ptr,
		                                        new_last_trans_on_disk);

	if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C2_update_for_new_last_trans_on_disk() failed.")
        }
    }    

    if ( cache_ptr->jwipl_len != 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "journal write in progress list isn't empty?!?!.")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C2_journal_pre_flush() */


/*-------------------------------------------------------------------------
 * Function:    H5C2_journal_transaction()
 *
 * Purpose:     Generate journal messages for the current transaction.
 * 		In passing, remove all entries from the transaction list.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              April 3, 2008
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C2_journal_transaction(H5F_t * f,
		         hid_t dxpl_id,
		         H5C2_t * cache_ptr)

{
    char buf[H5C2__MAX_API_NAME_LEN + 128];
    hbool_t resized;
    hbool_t renamed;
    H5C2_cache_entry_t * entry_ptr = NULL;
    unsigned serialize_flags = 0;
    haddr_t new_addr;
    size_t new_len;
    void * new_image_ptr;
    void * thing;
    herr_t result;    
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C2_journal_transaction, FAIL)
    
    HDassert( f != NULL );
    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C2__H5C2_T_MAGIC );
    HDassert( cache_ptr->trans_in_progress );
    HDassert( cache_ptr->tl_len > 0 );

    HDsnprintf(buf, H5C2__MAX_API_NAME_LEN + 128, "Begin transaction on %s.",
               cache_ptr->trans_api_name);

    result = H5C2_jb__comment(&(cache_ptr->mdj_jbrb), buf);

    if ( result != SUCCEED ) {

#if 0 /* JRM */
        HDfprintf(stdout, "%s: H5C2_jb__comment() failed.\n", FUNC);
#endif /* JRM */
        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_jb__comment() failed.")
    }

    result = H5C2_jb__start_transaction(&(cache_ptr->mdj_jbrb), 
		                        cache_ptr->trans_num);

    if ( result != SUCCEED ) {

#if 0 /* JRM */
        HDfprintf(stdout, "%s: H5C2_jb__start_transaction() failed.\n", FUNC);
#endif /* JRM */
        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_jb__start_transaction() failed.")
    }

    entry_ptr = cache_ptr->tl_tail_ptr;
    while ( entry_ptr != NULL )
    {
        HDassert( entry_ptr->is_dirty );
	HDassert( entry_ptr->last_trans == cache_ptr->trans_num );

	resized = FALSE;
	renamed = FALSE;

	if ( entry_ptr->is_protected ) 
        {

#if 0 /* JRM */
            HDfprintf(stdout, 
		"%s: Protected entry in TL at transaction close.\n", FUNC);
#endif /* JRM */
            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "Protected entry in TL at transaction close.")
	}

        if ( entry_ptr->image_ptr == NULL )
        {
            entry_ptr->image_ptr = H5MM_malloc(entry_ptr->size);

            if ( entry_ptr->image_ptr == NULL )
            {

#if 0 /* JRM */
               HDfprintf(stdout, 
		    "%s: memory allocation failed for on disk image buffer.\n", 
		    FUNC);
#endif /* JRM */
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                           "memory allocation failed for on disk image buffer.")
            }
        }

	/* This should always be true, unless the entry has already been 
	 * serialized in this function, and that serialization caused the
	 * entry to be resized (and possibly renamed as well).
	 */
	if ( ! ( entry_ptr->image_up_to_date ) ) {

            result = entry_ptr->type->serialize(f,
			                        dxpl_id,
                                                entry_ptr->addr,
                                                entry_ptr->size,
                                                entry_ptr->image_ptr,
                                                (void *)entry_ptr,
                                                &serialize_flags,
                                                &new_addr,
                                                &new_len,
                                                &new_image_ptr);
            if ( result != SUCCEED )
            {
#if 0 /* JRM */
                HDfprintf(stdout, "%s: unable to serialize entry.\n", FUNC);
#endif /* JRM */
                HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                            "unable to serialize entry")
            }

            if ( serialize_flags != 0 )
            {
	        /* if the serialize_flags are not zero, the entry has been 
	         * modified as a result of the serialize.  Pass these changes
	         * on to the cache, and don't bother to write a journal entry 
	         * at this time -- the protect/unprotect/rename will move the 
	         * entry to the head of the transaction list, where we will 
	         * handle it later.
	         */

	        resized = (serialize_flags & H5C2__SERIALIZE_RESIZED_FLAG) != 0;
	        renamed = (serialize_flags & H5C2__SERIALIZE_RENAMED_FLAG) != 0;

	        if ( ( renamed ) && ( ! resized ) )
                {
#if 0 /* JRM */
                    HDfprintf(stdout, "%s: entry renamed but not resized.\n", 
			      FUNC);
#endif /* JRM */
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                                "entry renamed but not resized?!?!")
                }

	        if ( resized ) 
                {
                    /* in the following protect/unprotect, the dxpl_id
		     * is irrelement, as we know that the entry is in cache,
	             * and thus no I/O will take place.
	             */
	            thing = H5C2_protect(f, dxpl_id,
	                                 entry_ptr->type, entry_ptr->addr,
				         entry_ptr->size, NULL, 
				         H5C2__NO_FLAGS_SET);

                    if ( thing == NULL ) 
                    {
#if 0 /* JRM */
                        HDfprintf(stdout, "%s: H5C2_protect() failed.\n", 
                                  FUNC);
#endif /* JRM */
                        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                                    "H5C2_protect() failed.")
                    }

                    result = H5C2_unprotect(f, dxpl_id,
                                            entry_ptr->type, entry_ptr->addr,
                                            thing, H5C2__SIZE_CHANGED_FLAG, 
					    new_len);

                    if ( result < 0 )
                    {
#if 0 /* JRM */
                        HDfprintf(stdout, "%s: H5C2_unprotect() failed.\n", 
                                  FUNC);
#endif /* JRM */
                        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                                    "H5C2_unprotect() failed.")
                    }

		    entry_ptr->image_ptr = new_image_ptr;
                }

	        if ( renamed )
                {
                    result = H5C2_rename_entry(cache_ptr, entry_ptr->type,
				               entry_ptr->addr, new_addr);

                    if ( result < 0 )
                    {
#if 0 /* JRM */
                        HDfprintf(stdout, "%s: H5C2_rename_entr() failed.\n", 
                                  FUNC);
#endif /* JRM */
                        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                                    "H5C2_rename_entr() failed.")
                    }
                }
            }

	    entry_ptr->image_up_to_date = TRUE;
        }

	/* if the entry hasn't been either resized or renamed, generate
	 * the journal entry, & remove from the transaction list.
	 */
	if ( ( ! resized ) && ( ! renamed ) ) {
                
            result = H5C2_jb__journal_entry(&(cache_ptr->mdj_jbrb),
                                            cache_ptr->trans_num,
					    entry_ptr->addr,
					    entry_ptr->size,
					    entry_ptr->image_ptr);

            if ( result != SUCCEED ) {

#if 0 /* JRM */
                HDfprintf(stdout, "%s: H5C2_jb__journal_entry() failed.\n", 
                          FUNC);
#endif /* JRM */
                HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                            "H5C2_jb__journal_entry() failed.")
            }

	    H5C2__TRANS_DLL_REMOVE(entry_ptr, cache_ptr->tl_head_ptr, \
                                   cache_ptr->tl_tail_ptr, cache_ptr->tl_len, \
				   cache_ptr->tl_size, FAIL);
        }
        entry_ptr = cache_ptr->tl_tail_ptr;
    }

    result = H5C2_jb__end_transaction(&(cache_ptr->mdj_jbrb),
		                      cache_ptr->trans_num);

    if ( result != SUCCEED ) {

#if 0 /* JRM */
        HDfprintf(stdout, "%s: H5C2_jb__end_transaction() failed.\n", FUNC);
#endif /* JRM */
        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_jb__end_transaction() failed.")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C2_journal_transaction() */


/*-------------------------------------------------------------------------
 * Function:    H5C2_update_for_new_last_trans_on_disk()
 *
 * Purpose:     Update the journal write in progress list for a change in
 * 		the last transaction on disk.
 *
 * 		Specifically, update the last_trans_on_disk field of 
 * 		*cache_ptr, and then scan the journal write in progress
 * 		list for entries whose last_trans field is now less than
 * 		or equal to cache_ptr->last_trans_on_disk.  Remove all
 * 		these entries from the journal write in progress list,
 * 		set their last_trans fields to zero, and insert then into
 * 		the eviction policy data structures.
 *
 * 		Similarly, scan the pinned entry list for entries whose
 * 		last_trans field is now less than or equal to 
 * 		cache_ptr->last_trans_on_disk.  In this case, just set
 * 		the last trans field to 0.  Note that here we assume that
 * 		the pinned entry list will always be small -- if this
 * 		ceases to be the case, we will have re-visit this case.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              April 3, 2008
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C2_update_for_new_last_trans_on_disk(H5C2_t * cache_ptr,
		                       uint64_t new_last_trans_on_disk)
{
    H5C2_cache_entry_t * entry_ptr = NULL;
    H5C2_cache_entry_t * prev_entry_ptr = NULL;
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C2_update_for_new_last_trans_on_disk, FAIL)

    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C2__H5C2_T_MAGIC );
    HDassert( cache_ptr->mdj_enabled );
    HDassert( cache_ptr->last_trans_on_disk <= new_last_trans_on_disk );

    if ( cache_ptr->last_trans_on_disk < new_last_trans_on_disk ) {

        cache_ptr->last_trans_on_disk = new_last_trans_on_disk;

	entry_ptr = cache_ptr->jwipl_tail_ptr;

	while ( entry_ptr != NULL )
        {
            prev_entry_ptr = entry_ptr->prev;

	    HDassert( entry_ptr->last_trans > 0 );
	    HDassert( entry_ptr->is_dirty );

	    if ( entry_ptr->last_trans <= cache_ptr->last_trans_on_disk ) {

                entry_ptr->last_trans = 0;
                H5C2__UPDATE_RP_FOR_JOURNAL_WRITE_COMPLETE(cache_ptr, \
				                           entry_ptr, \
		                                           FAIL)
            }

	    entry_ptr = prev_entry_ptr;
        }

	/* now scan the pinned entry list */

	entry_ptr = cache_ptr->pel_head_ptr;

	while ( entry_ptr != NULL ) {

	    if ( entry_ptr->last_trans > 0 ) {

	        HDassert( entry_ptr->is_dirty );

		if ( entry_ptr->last_trans <= cache_ptr->last_trans_on_disk ) {

		    entry_ptr->last_trans = 0;
		}
	    }
	    entry_ptr = entry_ptr->next;
	}
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C2_update_for_new_last_trans_on_disk() */


/**************************************************************************/
/************* superblock journaling message management code **************/
/**************************************************************************/

/*-------------------------------------------------------------------------
 * Function:	H5C2_check_for_journaling()
 *
 * Purpose:	If the superblock extension of a newly opened HDF5 file
 * 		indicates that journaling is in progress, the process
 * 		that created the file failed to close it properly, and 
 * 		thus the file is almost certainly corrupted.
 *
 * 		The purpose of this function is to detect this condition,
 * 		and either throw an error telling the user to run the 
 * 		recovery tool, or if so directed (presumably by the 
 * 		recovery tool) simply delete the metadata journaling 
 * 		configuration block and any reference to journaling in the 
 * 		superblock extension.
 *
 * 							JRM -- 3/26/08
 *
 * Return:	Success:	SUCCEED
 * 		Failure:	FAIL
 *
 * Programmer:	John Mainzer
 * 		March 26, 2008
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C2_check_for_journaling(H5F_t * f,
                          hid_t dxpl_id,
			  H5C2_t * cache_ptr,
		          hbool_t journal_recovered)
{
    const char * l0 =
        "This file was last written with metadata journaling enabled and was \n";
    const char * l1 =
        "not closed cleanly.  To allow HDF5 to read this file, please run the \n";
    const char * l2 = 
	"journal recovery tool on this file.  The journal was written \n";
    const char * l3 = "to \"";
    const char * l4 = "\".\n";
    herr_t result;
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C2_check_for_journaling, FAIL)

#if 0 /* JRM */
    HDfprintf(stdout, "%s: entering.\n", FUNC);
#endif /* JRM */

    HDassert( f );
    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C2__H5C2_T_MAGIC );
    HDassert( cache_ptr->jnl_magic == 0 );
    HDassert( cache_ptr->jnl_file_name_len == 0 );

    if ( H5C2__check_for_journaling ) {

        result = H5C2_get_journaling_in_progress(f, cache_ptr);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C2_get_journaling_in_progress() failed.")
        }

        if ( cache_ptr->jnl_file_name_len > 0 ) /* journaling was in */
					        /* progress          */
        {
#if 0 /* JRM */
            HDfprintf(stdout, "%s: journaling was in progress.\n", FUNC);
#endif /* JRM */

            if ( journal_recovered ) {

	        /* Just forget that we were journaling.  Do this by
                 * deleting the superblock extension message that says
                 * we were.
	         */

                result = H5C2_unmark_journaling_in_progress(f, dxpl_id, cache_ptr);

	        if ( result != SUCCEED ) {

                    HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                                "H5C2_unmark_journaling_in_progress() failed.")
                }
	    } else {

                /* we have to play some games here to set up an error message 
		 * that contains the journal file path.  In essence, what 
		 * follows is a somewhat modified version of the HGOTO_ERROR() 
		 * macro.
	         */
                (void)H5Epush2(H5E_DEFAULT, __FILE__, FUNC, __LINE__, 
			       H5E_ERR_CLS_g, H5E_CACHE, H5E_CANTJOURNAL, 
			       "%s%s%s%s%s%s", l0, l1, l2, l3, 
			       cache_ptr->jnl_file_name, l4);
	        (void)H5E_dump_api_stack((int)H5_IS_API(FUNC));
	        HGOTO_DONE(FAIL)

	    }
        }
    }

#if 0 /* JRM */
    HDfprintf(stdout, "%s: done.\n", FUNC);
#endif /* JRM */

done:

#if 0 /* JRM */
    HDfprintf(stdout, "%s: exiting.\n", FUNC);
#endif /* JRM */

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C2_check_for_journaling() */


/*-------------------------------------------------------------------------
 * Function:    H5C2_get_journaling_in_progress()
 *
 * Purpose:     Query the HDF5 file to see if it is marked as having
 * 		journaling in progress.  Update the journaling 
 * 		configuration fields in the cache structure accordingly.
 *
 * 		At least initially, the purpose of this function is
 * 		to examine a newly opened HDF5 file, and determine
 * 		whether journaling was enabled.  If so, we can presume
 * 		that the application crashed while journaling, and that
 * 		we must refuse to open the file until the user runs the
 * 		recovery utility on it.
 *
 * 		Hwever, this logic will be handled at a higher level.
 * 		In this function, we just get the journaling configuration
 * 		(if any) that has been saved in the file, and load it
 * 		into *cache_ptr.
 *
 * 		Note that this function assumes that *cache_ptr has
 * 		no journaling configuration set before the function
 * 		is called.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              March 11, 2008
 *
 * Changes:	JRM -- 2/20/09
 *		Reworked to reflect the move of the journal file name 
 *		and magic from the journaling configuration block to 
 *		the metadata journaling superblock extension message.
 *		Note that the journaling configuration block no longer
 *		exists.
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C2_get_journaling_in_progress(const H5F_t * f,
				H5C2_t * cache_ptr)
{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5C2_get_journaling_in_progress)

#if 0 /* JRM */
    HDfprintf(stdout, "%s: entering.\n", FUNC);
#endif /* JRM */

    HDassert( f );
    HDassert( f->shared != NULL );
    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C2__H5C2_T_MAGIC );
    HDassert( cache_ptr->jnl_file_name_len == 0 );

#if 0 /* JRM */
        HDfprintf(stdout, "%s: f->shared->mdc_jnl_enabled = %d.\n", FUNC,
		  (int)(f->shared->mdc_jnl_enabled));
#endif /* JRM */

    if ( f->shared->mdc_jnl_enabled == TRUE ) {

        if ( f->shared->mdc_jnl_file_name_len <= 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "journaling enabled but jnl file name empty?!?.")
        }

        if ( f->shared->mdc_jnl_file_name_len > 
             H5C2__MAX_JOURNAL_FILE_NAME_LEN ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "journal file name too long?!?.")
        }

        cache_ptr->jnl_magic         = f->shared->mdc_jnl_magic;
        cache_ptr->jnl_file_name_len = f->shared->mdc_jnl_file_name_len;
        HDstrncpy(cache_ptr->jnl_file_name,
                  f->shared->mdc_jnl_file_name,
                  f->shared->mdc_jnl_file_name_len + 1);

        if ( ( (cache_ptr->jnl_file_name)[cache_ptr->jnl_file_name_len]
               != '\0' ) ||
             ( HDstrlen(cache_ptr->jnl_file_name) != 
               (size_t)(cache_ptr->jnl_file_name_len) ) ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "bad jnl file name or name len?!?.")
        }
    }

#if 0 /* JRM */
    HDfprintf(stdout, "%s: done.\n");
#endif /* JRM */

done:

#if 0 /* JRM */
    HDfprintf(stdout, "%s: exiting.\n");
#endif /* JRM */

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C2_get_journaling_in_progress() */


/*-------------------------------------------------------------------------
 * Function:    H5C2_mark_journaling_in_progress()
 *
 * Purpose:     Modify the HDF5 file to indicate that journaling is 
 * 		in progress, and flush the file to disk.  
 *
 * 		The objective here is to allow us to detect the fact 
 * 		the file was being journaled if we crash before we 
 * 		close the file properly.
 *
 * 		Note that the function assumes that the file is not 
 * 		currently marked as having journaling in progress.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              March 11, 2008
 *
 * Changes:	JRM -- 2/10/09
 *		Added the journal_magic parameter and related code.
 *
 *		JRM -- 2/20/09
 *		Reworked function to reflect the move of the journal
 *		file name and magic to the super block extension message
 *		and out of the metadata journaling configuration block
 *		which no longer exists.
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C2_mark_journaling_in_progress(H5F_t * f,
                                 hid_t dxpl_id,
				 const int32_t journal_magic,
                                 const char * journal_file_name_ptr)
{
    H5C2_t * cache_ptr;
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C2_mark_journaling_in_progress, FAIL)
#if 0 /* JRM */
    HDfprintf(stdout, "%s: Entering.\n", FUNC);
#endif /* JRM */
    HDassert( f != NULL );
    HDassert( f->shared != NULL );
    HDassert( ! f->shared->mdc_jnl_enabled );

    cache_ptr = f->shared->cache2;

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C2__H5C2_T_MAGIC );
    HDassert( cache_ptr->jnl_file_name_len == 0 );
    HDassert( journal_file_name_ptr != NULL );

    /* Can't journal a read only file, so verify that we are
     * opened read/write and fail if we are not.
     */
    if ( (f->shared->flags & H5F_ACC_RDWR) == 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "File is opened read only.")
    }

    cache_ptr->jnl_magic = journal_magic;
    cache_ptr->jnl_file_name_len = HDstrlen(journal_file_name_ptr);

    if ( cache_ptr->jnl_file_name_len <= 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "length of journal file name is zero.")
    }

    if ( cache_ptr->jnl_file_name_len > H5C2__MAX_JOURNAL_FILE_NAME_LEN ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "journal file name too long.")
    }

    HDstrncpy(cache_ptr->jnl_file_name,
              journal_file_name_ptr,
              (size_t)(cache_ptr->jnl_file_name_len + 1));

    /* now, load the journaling information into shared, and then call
     * H5F_super_write_mdj_msg() to write the metadata journaling 
     * superblock extension message to file.  
     */
    f->shared->mdc_jnl_enabled       = TRUE;
    f->shared->mdc_jnl_magic         = journal_magic;
    f->shared->mdc_jnl_file_name_len = (size_t)(cache_ptr->jnl_file_name_len);
    HDstrncpy(f->shared->mdc_jnl_file_name,
              journal_file_name_ptr,
              (size_t)(cache_ptr->jnl_file_name_len + 1));

#if 0 /* JRM */
    HDfprintf(stdout, "%s: writing superblock extension.\n", FUNC);
#endif /* JRM */

    if ( H5F_super_write_mdj_msg(f, dxpl_id) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5F_super_write_mdj_msg() failed.")
    }

#if 0 /* JRM */
    HDfprintf(stdout, "%s: finished writing superblock extension.\n", FUNC);
#endif /* JRM */

    /* Finally, flush the file to ensure that changes made it to disk. */

#if 0 /* JRM */
    HDfprintf(stdout, "%s: calling H5F_flush().\n", FUNC);
#endif /* JRM */

    if ( H5F_flush(f, dxpl_id, H5F_SCOPE_GLOBAL, H5F_FLUSH_NONE) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, "H5F_flush() failed.")
    }

#if 0 /* JRM */
    HDfprintf(stdout, "%s: H5F_flush() returns.\n", FUNC);
#endif /* JRM */

#if 0 /* JRM */
    HDfprintf(stdout, "%s: Exiting.\n", FUNC);
#endif /* JRM */

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C2_mark_journaling_in_progress() */


/*-------------------------------------------------------------------------
 * Function:    H5C2_unmark_journaling_in_progress()
 *
 * Purpose:     Modify the HDF5 file to indicate that journaling is 
 * 		not in progress, and flush the file to disk.  
 *
 * 		The objective here is to remove the messages indicating
 * 		that the file is being journaled.  We will typically do 
 * 		this either on file close, or if directed to cease 
 * 		journaling.  Once these messages are removed, we will
 * 		be able to open the file without triggering a "journaling
 * 		in progress" failure.
 *
 * 		Note that the function assumes that the file is
 * 		currently marked as having journaling in progress.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              March 11, 2008
 *
 * Changes:	JRM -- 2/20/09
 *		Reworked function to reflect the move of the journal 
 *		file name and magic from the metadata journaling config
 *		block and into a superblock extension message.  Note that 
 *		the metadata journaling configuration block no longer 
 *		exists.
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C2_unmark_journaling_in_progress(H5F_t * f,
                                   hid_t dxpl_id,
#ifndef NDEBUG
				   H5C2_t * cache_ptr)
#else /* NDEBUG */
				   H5C2_t UNUSED * cache_ptr)
#endif /* NDEBUG */
{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C2_unmark_journaling_in_progress, FAIL)
#if 0 /* JRM */
    HDfprintf(stdout, "%s: entering.\n", FUNC);
    HDfflush(stdout);
#endif /* JRM */
    HDassert( f != NULL );
    HDassert( f->shared != NULL );
    HDassert( f->shared->mdc_jnl_enabled );
    HDassert( f->shared->cache2 == cache_ptr );
    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C2__H5C2_T_MAGIC );
    HDassert( cache_ptr->jnl_file_name_len > 0 );


    /* Can't journal a read only file, so verify that we are
     * opened read/write and fail if we are not.
     */
    if ( (f->shared->flags & H5F_ACC_RDWR) == 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "File is opened read only.")
    }

    /* Discard the journal file name and magic in *cache_ptr */
    cache_ptr->jnl_magic          = 0;
    cache_ptr->jnl_file_name_len  = 0;
    (cache_ptr->jnl_file_name)[0] = '\0';

    /* now, mark f->shared to indicate that journaling is not in 
     * progress, and then call H5F_super_write_mdj_msg() to write
     * the changes to disk.
     */
    f->shared->mdc_jnl_enabled        = FALSE;
    f->shared->mdc_jnl_magic          = 0;
    f->shared->mdc_jnl_file_name_len  = 0;
    (f->shared->mdc_jnl_file_name)[0] = '\0';

    if ( H5F_super_write_mdj_msg(f, dxpl_id) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5F_super_write_mdj_msg() failed.")
    }

    /* Finally, flush the file to ensure that changes made it to disk. */

#if 0 /* JRM */
    HDfprintf(stdout, "%s: calling H5F_flush().\n", FUNC);
    HDfflush(stdout);
#endif /* JRM */

    if ( H5F_flush(f, dxpl_id, H5F_SCOPE_GLOBAL, H5F_FLUSH_NONE) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, "H5F_flush() failed.")
    }

#if 0 /* JRM */
    HDfprintf(stdout, "%s: done.\n", FUNC);
    HDfflush(stdout);
#endif /* JRM */

done:

#if 0 /* JRM */
    HDfprintf(stdout, "%s: exiting.\n", FUNC);
    HDfflush(stdout);
#endif /* JRM */

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C2_unmark_journaling_in_progress() */


/**************************************************************************/
/****** metadata journaling status change callback management code ********/
/**************************************************************************/

/*-------------------------------------------------------------------------
 * Function:    H5C2_call_mdjsc_callbacks()
 *
 * Purpose:     Call the metadata journaling status change callbacks.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              August 15, 2008
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5C2_call_mdjsc_callbacks(H5C2_t * cache_ptr, 
		          hid_t dxpl_id,
		          H5C2_mdj_config_t * config_ptr)
{
    herr_t ret_value = SUCCEED;      /* Return value */
    int32_t i;
    int32_t funcs_called = 0;
    H5C2_mdj_status_change_func_t func_ptr;
    void * data_ptr;

    FUNC_ENTER_NOAPI_NOINIT(H5C2_call_mdjsc_callbacks)

    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C2__H5C2_T_MAGIC );
    HDassert( cache_ptr->mdjsc_cb_tbl != NULL );
    HDassert( cache_ptr->mdjsc_cb_tbl_len >= H5C2__MIN_MDJSC_CB_TBL_LEN );
    HDassert( ( cache_ptr->mdjsc_cb_tbl_fl_head == -1 ) ||
	      ( cache_ptr->num_mdjsc_cbs < cache_ptr->mdjsc_cb_tbl_len ) );

    if ( ( cache_ptr->num_mdjsc_cbs < 0 ) 
         ||
	 ( cache_ptr->num_mdjsc_cbs > cache_ptr->mdjsc_cb_tbl_len ) 
	 ||
	 ( cache_ptr->mdjsc_cb_tbl_fl_head < -1 ) 
	 ||
	 ( cache_ptr->mdjsc_cb_tbl_fl_head > cache_ptr->mdjsc_cb_tbl_len ) 
	 ||
	 ( cache_ptr->mdjsc_cb_tbl_max_idx_in_use < -1 ) 
	 ||
	 ( cache_ptr->mdjsc_cb_tbl_max_idx_in_use >= 
	   cache_ptr->mdjsc_cb_tbl_len ) 
	 ||
	 ( cache_ptr->mdjsc_cb_tbl_len < H5C2__MIN_MDJSC_CB_TBL_LEN ) 
	 ||
         ( ( cache_ptr->num_mdjsc_cbs == cache_ptr->mdjsc_cb_tbl_len )
	   &&
	   ( ( cache_ptr->mdjsc_cb_tbl_fl_head != -1 ) 
	     ||
	     ( cache_ptr->mdjsc_cb_tbl_max_idx_in_use !=
	       cache_ptr->mdjsc_cb_tbl_len - 1 ) 
	   )
	 )
	 ||
         ( ( cache_ptr->num_mdjsc_cbs < cache_ptr->mdjsc_cb_tbl_len )
	   &&
	   ( cache_ptr->mdjsc_cb_tbl_fl_head < 0 )
	 )
       ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "mdjsc_cb_tbl corrupt(1)?!?!");
    }

    for ( i = 0; i <= cache_ptr->mdjsc_cb_tbl_max_idx_in_use; i++ )
    {
        if ( ((cache_ptr->mdjsc_cb_tbl)[i]).fcn_ptr != NULL ) {

	    func_ptr = ((cache_ptr->mdjsc_cb_tbl)[i]).fcn_ptr;
	    data_ptr = ((cache_ptr->mdjsc_cb_tbl)[i]).data_ptr;

            /* Try the callback */
	    if(func_ptr(config_ptr, dxpl_id, data_ptr) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "metadata journaling status change callback failed!");

	    funcs_called++;
	}
    }

    if ( funcs_called != cache_ptr->num_mdjsc_cbs ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
		    "funcs_called != cache_ptr->num_mdjsc_cbs.");
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C2_call_mdjsc_callbacks() */


/*-------------------------------------------------------------------------
 * Function:    H5C2_deregister_mdjsc_callback()
 *
 * Purpose:     Deregister a metadata journaling status change callback,
 * 		shrinking the metadata journaling status callback table 
 * 		as necessary.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              August 15, 2008
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C2_deregister_mdjsc_callback(H5C2_t * cache_ptr,
			       int32_t idx)
{
    herr_t ret_value = SUCCEED;      /* Return value */
    int32_t i;
    double fraction_in_use;

    FUNC_ENTER_NOAPI(H5C2_deregister_mdjsc_callback, FAIL)
#if 0 /* JRM */
    HDfprintf(stdout, "entering %s: idx = %d.\n", FUNC, idx);
#endif /* JRM */

    if ( ( cache_ptr == NULL ) ||
         ( cache_ptr->magic != H5C2__H5C2_T_MAGIC ) ) {

#if 0 /* JRM */
        HDfprintf(stdout, "%s: cache_ptr corrupt?!?\n", FUNC);
#endif /* JRM */
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
		    "cache_ptr corrupt?!?");
    }

    if ( ( cache_ptr->mdjsc_cb_tbl == NULL ) ||
         ( ( cache_ptr->num_mdjsc_cbs == cache_ptr->mdjsc_cb_tbl_len ) 
	   &&
	   ( cache_ptr->mdjsc_cb_tbl_fl_head != -1 ) ) ||
	 ( ( cache_ptr->mdjsc_cb_tbl_fl_head < 0 ) 
	   &&
	   ( cache_ptr->num_mdjsc_cbs != cache_ptr->mdjsc_cb_tbl_len ) ) ||
         ( cache_ptr->mdjsc_cb_tbl_len < H5C2__MIN_MDJSC_CB_TBL_LEN ) ||
         ( cache_ptr->mdjsc_cb_tbl_fl_head >= cache_ptr->mdjsc_cb_tbl_len ) ||
	 ( cache_ptr->num_mdjsc_cbs > cache_ptr->mdjsc_cb_tbl_len ) ||
	 ( cache_ptr->num_mdjsc_cbs < 0 ) ||
	 ( ( cache_ptr->mdjsc_cb_tbl_max_idx_in_use < 0 ) &&
	   ( cache_ptr->num_mdjsc_cbs > 0 ) ) ) {
	    
#if 0 /* JRM */
        HDfprintf(stdout, "%s: mdjsc_cb_tbl corrupt(1)?!?!\n", FUNC);
	HDfprintf(stdout, "mdjsc_cb_tbl_len = %d\n", 
		  cache_ptr->mdjsc_cb_tbl_len);
	HDfprintf(stdout, "num_mdjsc_cbs = %d\n", 
		  cache_ptr->num_mdjsc_cbs);
	HDfprintf(stdout, "mdjsc_cb_tbl_fl_head = %d\n", 
		  cache_ptr->mdjsc_cb_tbl_fl_head);
	HDfprintf(stdout, "mdjsc_cb_tbl_max_idx_in_use = %d\n", 
		  cache_ptr->mdjsc_cb_tbl_max_idx_in_use);
#endif /* JRM */
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "mdjsc_cb_tbl corrupt(1)?!?!");
    }

    if ( cache_ptr->num_mdjsc_cbs <= 0 ) {

#if 0 /* JRM */
        HDfprintf(stdout, "%s: mdjsc_cb_tbl empty(1)?!?!\n", FUNC);
#endif /* JRM */
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "mdjsc_cb_tbl empty(1)!?!");
    }

    if ( ( idx < 0 ) ||
	 ( idx >= cache_ptr->mdjsc_cb_tbl_len ) ||
	 ( idx > cache_ptr->mdjsc_cb_tbl_max_idx_in_use ) ) {

#if 0 /* JRM */
        HDfprintf(stdout, "%s: bad fcn_ptr/data_ptr/idx?!?\n", FUNC);
	HDfprintf(stdout, "%s: idx = %d.\n", FUNC, idx);
	HDfprintf(stdout, "%s: cache_ptr->mdjsc_cb_tbl_len = %d.\n", 
		  FUNC, cache_ptr->mdjsc_cb_tbl_len);
	HDfprintf(stdout, "%s: cache_ptr->mdjsc_cb_tbl_max_idx_in_use = %d.\n",
		  FUNC, cache_ptr->mdjsc_cb_tbl_max_idx_in_use);
#endif /* JRM */
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "bad idx?!?");

    } else if ( ((cache_ptr->mdjsc_cb_tbl)[idx]).fcn_ptr == NULL ) {
	
#if 0 /* JRM */
        HDfprintf(stdout, "%s: callback already deregistered\n", FUNC);
#endif /* JRM */
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
		    "callback already deregistered");
   
    } else if ( ((cache_ptr->mdjsc_cb_tbl)[idx]).fl_next != -1 ) {
	
#if 0 /* JRM */
        HDfprintf(stdout, "%s: free list corrupted\n", FUNC);
#endif /* JRM */
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "free list corrupted.");

    }

    ((cache_ptr->mdjsc_cb_tbl)[idx]).fcn_ptr = NULL;
    ((cache_ptr->mdjsc_cb_tbl)[idx]).data_ptr = NULL;
    ((cache_ptr->mdjsc_cb_tbl)[idx]).fl_next = 
	    cache_ptr->mdjsc_cb_tbl_fl_head;
    cache_ptr->mdjsc_cb_tbl_fl_head = idx;
    (cache_ptr->num_mdjsc_cbs)--;

    if ( cache_ptr->num_mdjsc_cbs == 0 ) {

        cache_ptr->mdjsc_cb_tbl_max_idx_in_use = -1;

    } else if ( idx == cache_ptr->mdjsc_cb_tbl_max_idx_in_use ) {

        i = idx;

        while ( ( i >= 0 ) &&
		( ((cache_ptr->mdjsc_cb_tbl)[i]).fcn_ptr == NULL ) ) {

	    i--;
	}

	if ( i < 0 ) {

#if 0 /* JRM */
            HDfprintf(stdout, "%s: mdjsc_cb_tbl empty(2)!?!\n", FUNC);
#endif /* JRM */
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
			"mdjsc_cb_tbl empty(2)!?!");
	}

	cache_ptr->mdjsc_cb_tbl_max_idx_in_use = i;
    }

    if ( ( cache_ptr->num_mdjsc_cbs >= cache_ptr->mdjsc_cb_tbl_len )
	 ||
	 ( ( cache_ptr->num_mdjsc_cbs < cache_ptr->mdjsc_cb_tbl_len ) 
	   &&
	   ( cache_ptr->num_mdjsc_cbs > 0 ) 
	   &&
	   ( ( cache_ptr->mdjsc_cb_tbl_fl_head < 0 ) 
	     ||
	     ( cache_ptr->mdjsc_cb_tbl_fl_head >= cache_ptr->mdjsc_cb_tbl_len )
	   ) 
	 ) 
	 ||
	 ( ( cache_ptr->num_mdjsc_cbs == 0 ) 
	   &&
	   ( cache_ptr->mdjsc_cb_tbl_max_idx_in_use != -1 ) 
	 )
       ) {

#if 0 /* JRM */
        HDfprintf(stdout, "%s: mdjsc_cb_tbl corrupt(2)?!?!\n", FUNC);
#endif /* JRM */
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "mdjsc_cb_tbl corrupt(2)?!?!");
    }

    fraction_in_use = ((double)(cache_ptr->num_mdjsc_cbs)) /
	              ((double)(cache_ptr->mdjsc_cb_tbl_len));

    if ( ( fraction_in_use < H5C2__MDJSC_CB_TBL_MIN_ACTIVE_RATIO ) &&
         ( cache_ptr->mdjsc_cb_tbl_len > H5C2__MIN_MDJSC_CB_TBL_LEN ) &&
         ( cache_ptr->mdjsc_cb_tbl_max_idx_in_use < 
	   (cache_ptr->mdjsc_cb_tbl_len / 2) ) ) {
        herr_t result;

        result = H5C2_shrink_mdjsc_callback_table(cache_ptr);

	if ( result != SUCCEED ) {

#if 0 /* JRM */
            HDfprintf(stdout, 
	              "%s: H5C2_shrink_mdjsc_callback_table() failed.\n", FUNC);
#endif /* JRM */
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
			"H5C2_shrink_mdjsc_callback_table() failed.");
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C2_deregister_mdjsc_callback() */


/*-------------------------------------------------------------------------
 * Function:    H5C2_grow_mdjsc_callback_table()
 *
 * Purpose:     Double the size of the the metadata journaling status
 * 		change callback table.  Note that the table is assumed
 * 		to be full on entry.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              August 15, 2008
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5C2_grow_mdjsc_callback_table(H5C2_t * cache_ptr)
{
    herr_t ret_value = SUCCEED;      /* Return value */
    int32_t i;
    int32_t old_mdjsc_cb_tbl_len;
    int64_t new_mdjsc_cb_tbl_len;
    H5C2_mdjsc_record_t * old_mdjsc_cb_tbl = NULL;
    H5C2_mdjsc_record_t * new_mdjsc_cb_tbl = NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5C2_grow_mdjsc_callback_table)

    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C2__H5C2_T_MAGIC );
    HDassert( cache_ptr->mdjsc_cb_tbl != NULL );
    HDassert( cache_ptr->mdjsc_cb_tbl_len >= H5C2__MIN_MDJSC_CB_TBL_LEN );
    HDassert( cache_ptr->mdjsc_cb_tbl_fl_head == -1 );
    HDassert( cache_ptr->num_mdjsc_cbs == cache_ptr->mdjsc_cb_tbl_len );

    if ( ( cache_ptr->num_mdjsc_cbs != cache_ptr->mdjsc_cb_tbl_len ) ||
	 ( cache_ptr->mdjsc_cb_tbl_fl_head != -1 ) ||
	 ( cache_ptr->mdjsc_cb_tbl_max_idx_in_use != 
	   cache_ptr->mdjsc_cb_tbl_len - 1 ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
	            "unexpected mdjsc_cb_tbl status.");
    }

    old_mdjsc_cb_tbl = cache_ptr->mdjsc_cb_tbl;
    old_mdjsc_cb_tbl_len = cache_ptr->mdjsc_cb_tbl_len;

    new_mdjsc_cb_tbl_len = 2 * old_mdjsc_cb_tbl_len;
    new_mdjsc_cb_tbl = H5MM_malloc((size_t)new_mdjsc_cb_tbl_len *
		                   sizeof(H5C2_mdjsc_record_t));
    if ( new_mdjsc_cb_tbl == NULL ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "can't alloc new mdjsc_cb_tbl.")
    }

    for ( i = 0; i < old_mdjsc_cb_tbl_len; i++ )
    {
        new_mdjsc_cb_tbl[i] = old_mdjsc_cb_tbl[i];
    }

    for ( i = old_mdjsc_cb_tbl_len; i < new_mdjsc_cb_tbl_len; i++ )
    {
	new_mdjsc_cb_tbl[i].fcn_ptr = NULL;
	new_mdjsc_cb_tbl[i].data_ptr = NULL;
	new_mdjsc_cb_tbl[i].fl_next = i + 1;
    }
    new_mdjsc_cb_tbl[new_mdjsc_cb_tbl_len - 1].fl_next = -1;

    cache_ptr->mdjsc_cb_tbl = new_mdjsc_cb_tbl;
    cache_ptr->mdjsc_cb_tbl_len = new_mdjsc_cb_tbl_len;
    cache_ptr->mdjsc_cb_tbl_fl_head = old_mdjsc_cb_tbl_len;

    old_mdjsc_cb_tbl = H5MM_xfree(old_mdjsc_cb_tbl);

    if ( old_mdjsc_cb_tbl != NULL ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                    "free of old_mdjsc_cb_tbl failed.");
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C2_grow_mdjsc_callback_table() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C2_register_mdjsc_callback()
 *
 * Purpose:     Register a metadata journaling status change callback,
 * 		growing the metadata journaling status callback table 
 * 		as necessary.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              August 15, 2008
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C2_register_mdjsc_callback(H5C2_t * cache_ptr,
		             H5C2_mdj_status_change_func_t fcn_ptr,
			     void * data_ptr,
			     int32_t * idx_ptr)
{
    herr_t result;
    herr_t ret_value = SUCCEED;      /* Return value */
    int32_t i;

    FUNC_ENTER_NOAPI(H5C2_register_mdjsc_callback, FAIL)

    if ( ( cache_ptr == NULL ) ||
         ( cache_ptr->magic != H5C2__H5C2_T_MAGIC ) )
    {
#if 0 /* JRM */
	HDfprintf(stdout, "%s: bad cache_ptr on entry.\n", FUNC);
#endif /* JRM */
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "bad cache_ptr on entry");
    }

    if ( cache_ptr->mdjsc_cb_tbl == NULL ) 
    {
#if 0 /* JRM */
	HDfprintf(stdout, "%s: cache_ptr->mdjsc_cb_tbl == NULL.\n", FUNC);
#endif /* JRM */
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
		    "cache_ptr->mdjsc_cb_tbl == NULL")
    }

    if ( cache_ptr->mdjsc_cb_tbl_len < H5C2__MIN_MDJSC_CB_TBL_LEN )
    {
#if 0 /* JRM */
	HDfprintf(stdout, "%s: cache_ptr->mdjsc_cb_tbl_len too small.\n", FUNC);
#endif /* JRM */
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
		    "cache_ptr->mdjsc_cb_tbl_len too small")
    }

    if ( ( cache_ptr->mdjsc_cb_tbl_fl_head == -1 ) &&
	 ( cache_ptr->num_mdjsc_cbs < cache_ptr->mdjsc_cb_tbl_len ) )
    {
#if 0 /* JRM */
	HDfprintf(stdout, "%s: mdjsc callback table corrupt?\n", FUNC);
#endif /* JRM */
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
		    "mdjsc callback table corrupt?")
    }

    if ( fcn_ptr == NULL )
    {
#if 0 /* JRM */
	HDfprintf(stdout, "%s: fcn_ptr NULL on entry\n", FUNC);
#endif /* JRM */
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "fcn_ptr NULL on entry")
    }

    if ( idx_ptr == NULL )
    {
#if 0 /* JRM */
	HDfprintf(stdout, "%s: idx_ptr NULL on entry\n", FUNC);
#endif /* JRM */
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "idx_ptr NULL on entry")
    }

    if ( cache_ptr->mdjsc_cb_tbl_len <= cache_ptr->num_mdjsc_cbs ) {

        result = H5C2_grow_mdjsc_callback_table(cache_ptr);

	if ( result != SUCCEED ) {
#if 0 /* JRM */
	    HDfprintf(stdout, "%s: H5C2_grow_mdjsc_callback_table() failed.\n",
		      FUNC);
#endif /* JRM */
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
			"H5C2_grow_mdjsc_callback_table() failed.");
        }
    }

    if ( ( cache_ptr->mdjsc_cb_tbl_fl_head < 0 ) ||
         ( cache_ptr->mdjsc_cb_tbl_fl_head >= cache_ptr->mdjsc_cb_tbl_len ) ||
	 ( cache_ptr->num_mdjsc_cbs >= cache_ptr->mdjsc_cb_tbl_len ) ) {

#if 0 /* JRM */
	    HDfprintf(stdout, "%s: mdjsc_cb_tbl corrupt(1)?!?!.\n",
		      FUNC);
	    HDfprintf(stdout, "%s: cache_ptr->mdjsc_cb_tbl_fl_head = %d.\n",
		      FUNC, cache_ptr->mdjsc_cb_tbl_fl_head);
	    HDfprintf(stdout, "%s: cache_ptr->num_mdjsc_cbs = %d.\n",
		      FUNC, cache_ptr->num_mdjsc_cbs);
	    HDfprintf(stdout, "%s: cache_ptr->mdjsc_cb_tbl_len = %d.\n",
		      FUNC, cache_ptr->mdjsc_cb_tbl_len);
#endif /* JRM */
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "mdjsc_cb_tbl corrupt(1)?!?!");
    }

    i = cache_ptr->mdjsc_cb_tbl_fl_head;

    cache_ptr->mdjsc_cb_tbl_fl_head = ((cache_ptr->mdjsc_cb_tbl)[i]).fl_next;
    (cache_ptr->num_mdjsc_cbs)++;

    if ( ( ( cache_ptr->num_mdjsc_cbs == cache_ptr->mdjsc_cb_tbl_len ) &&
	   ( cache_ptr->mdjsc_cb_tbl_fl_head != -1 ) 
	 ) 
         ||
	 ( cache_ptr->num_mdjsc_cbs > cache_ptr->mdjsc_cb_tbl_len ) 
	 ||
	 ( ( cache_ptr->num_mdjsc_cbs < cache_ptr->mdjsc_cb_tbl_len ) 
	   &&
	   ( ( cache_ptr->mdjsc_cb_tbl_fl_head < 0 ) 
	     ||
	     ( cache_ptr->mdjsc_cb_tbl_fl_head >= cache_ptr->mdjsc_cb_tbl_len )
	   ) 
	 ) 
       ) {

#if 0 /* JRM */
	    HDfprintf(stdout, "%s: mdjsc_cb_tbl corrupt(2)?!?!.",
		      FUNC);
#endif /* JRM */
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "mdjsc_cb_tbl corrupt(2)?!?!");
    }

    ((cache_ptr->mdjsc_cb_tbl)[i]).fcn_ptr  = fcn_ptr;
    ((cache_ptr->mdjsc_cb_tbl)[i]).data_ptr = data_ptr;
    ((cache_ptr->mdjsc_cb_tbl)[i]).fl_next  = -1;

    if ( i > cache_ptr->mdjsc_cb_tbl_max_idx_in_use ) {

        cache_ptr->mdjsc_cb_tbl_max_idx_in_use = i;
    }

    *idx_ptr = i;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C2_register_mdjsc_callback() */


/*-------------------------------------------------------------------------
 * Function:    H5C2_shrink_mdjsc_callback_table()
 *
 * Purpose:     Half the size of the the metadata journaling status
 * 		change callback table.  Note that the table is assumed
 * 		to be:
 *
 * 		1) Not more than H5C2__MDJSC_CB_TBL_MIN_ACTIVE_RATIO * 100
 *                 percent full.
 *
 *              2) Of size H5C2__MIN_MDJSC_CB_TBL_LEN * 2 ** n, where
 *                 n is a positive integer.
 *
 *              3) Contain no entries at index greater than or equal to
 *                 cache_ptr->mdjsc_cb_tbl_len / 2.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              August 15, 2008
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5C2_shrink_mdjsc_callback_table(H5C2_t * cache_ptr)
{
    herr_t ret_value = SUCCEED;      /* Return value */
    int32_t i;
    int32_t old_mdjsc_cb_tbl_len;
    int32_t new_mdjsc_cb_tbl_len;
    int32_t new_fl_head = -1;
    int32_t last_free_entry = -1;
    double fraction_in_use;
    H5C2_mdjsc_record_t * old_mdjsc_cb_tbl = NULL;
    H5C2_mdjsc_record_t * new_mdjsc_cb_tbl = NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5C2_shrink_mdjsc_callback_table)

    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C2__H5C2_T_MAGIC );
    HDassert( cache_ptr->mdjsc_cb_tbl != NULL );
    HDassert( cache_ptr->mdjsc_cb_tbl_len > H5C2__MIN_MDJSC_CB_TBL_LEN );
    HDassert( cache_ptr->mdjsc_cb_tbl_fl_head >= 0);
    HDassert( cache_ptr->num_mdjsc_cbs < cache_ptr->mdjsc_cb_tbl_len / 2 );

    fraction_in_use = ((double)(cache_ptr->num_mdjsc_cbs)) /
	              ((double)(cache_ptr->mdjsc_cb_tbl_len));

    if ( ( cache_ptr->num_mdjsc_cbs >= cache_ptr->mdjsc_cb_tbl_len ) ||
	 ( (cache_ptr->mdjsc_cb_tbl_len / 2) < H5C2__MIN_MDJSC_CB_TBL_LEN ) ||
	 ( cache_ptr->mdjsc_cb_tbl_fl_head == -1 ) ||
	 ( cache_ptr->mdjsc_cb_tbl_max_idx_in_use >= 
	   cache_ptr->mdjsc_cb_tbl_len / 2 ) ||
	 ( fraction_in_use >= H5C2__MDJSC_CB_TBL_MIN_ACTIVE_RATIO ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
	            "unexpected mdjsc_cb_tbl status.");
    }

    old_mdjsc_cb_tbl = cache_ptr->mdjsc_cb_tbl;
    old_mdjsc_cb_tbl_len = cache_ptr->mdjsc_cb_tbl_len;

    new_mdjsc_cb_tbl_len = old_mdjsc_cb_tbl_len / 2;
#if 0 /* JRM */
    HDfprintf(stdout, "new_mdjsc_cb_tbl_len = %d.\n", new_mdjsc_cb_tbl_len);
#endif /* JRM */

    while ( ( (new_mdjsc_cb_tbl_len / 2) >= H5C2__MIN_MDJSC_CB_TBL_LEN ) &&
	    ( (((double)(cache_ptr->num_mdjsc_cbs)) / 
	       ((double)new_mdjsc_cb_tbl_len)) <= 
	      H5C2__MDJSC_CB_TBL_MIN_ACTIVE_RATIO ) &&
	    ( (new_mdjsc_cb_tbl_len / 2) > 
	      cache_ptr->mdjsc_cb_tbl_max_idx_in_use ) )
    {
	new_mdjsc_cb_tbl_len /= 2;
#if 0 /* JRM */
        HDfprintf(stdout, "new_mdjsc_cb_tbl_len = %d.\n", new_mdjsc_cb_tbl_len);
#endif /* JRM */
    }

    if ( ( new_mdjsc_cb_tbl_len < H5C2__MIN_MDJSC_CB_TBL_LEN ) ||
         ( new_mdjsc_cb_tbl_len < cache_ptr->mdjsc_cb_tbl_max_idx_in_use ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
	            "error in computation of new_mdjsc_cb_tbl_len?!?!");
    }

    new_mdjsc_cb_tbl = H5MM_malloc(new_mdjsc_cb_tbl_len *
		                   sizeof(H5C2_mdjsc_record_t));
    if ( new_mdjsc_cb_tbl == NULL ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "can't alloc new mdjsc_cb_tbl.")
    }

    /* now copy over the table, constructing the free list as we go */

    for ( i = 0; i < new_mdjsc_cb_tbl_len; i++ )
    {
        if ( old_mdjsc_cb_tbl[i].fcn_ptr == NULL ) {

	    new_mdjsc_cb_tbl[i].fcn_ptr = NULL;
	    new_mdjsc_cb_tbl[i].data_ptr = NULL;
	    new_mdjsc_cb_tbl[i].fl_next = -1;
	
	    if ( new_fl_head == -1 ) {

	        new_fl_head = i;
		last_free_entry = i;

	    } else {

		new_mdjsc_cb_tbl[last_free_entry].fl_next = i;
	        last_free_entry = i;
	    }
	} else {

            new_mdjsc_cb_tbl[i] = old_mdjsc_cb_tbl[i];

	}
    }

    if ( new_fl_head == -1 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "table full after shrink!?!.");

    }

    cache_ptr->mdjsc_cb_tbl = new_mdjsc_cb_tbl;
    cache_ptr->mdjsc_cb_tbl_fl_head = new_fl_head;
    cache_ptr->mdjsc_cb_tbl_len = new_mdjsc_cb_tbl_len;

    old_mdjsc_cb_tbl = H5MM_xfree(old_mdjsc_cb_tbl);

    if ( old_mdjsc_cb_tbl != NULL ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                    "free of old_mdjsc_cb_tbl failed.");
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C2_shrink_mdjsc_callback_table() */


/**************************************************************************/
/********************** journal file management code **********************/
/**************************************************************************/

/******************************************************************************
 *
 * Function:		H5C2_jb__bin2hex
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Tuesday, March 4, 2008
 *
 * Purpose:		Convert binary data into hexadecimal.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C2_jb__bin2hex(const uint8_t * buf, 
                 char * hexdata,
                 size_t * hexlength,
                 size_t buf_size)

{
    size_t         v;                   /* Local index variable */
    uint8_t        c;
    char *         t;
	
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5C2_jb__bin2hex)

    t = hexdata;
    t[0] = ' ';
    for (v = 0; v < buf_size; v++) {

        t = &hexdata[v * 3 + 1];
        c = buf[v];
        HDsnprintf(t, (size_t)3, "%02x ", c);
        t[2] = ' ';

    } /* end for */
    t[3] = '\n';
    t[4] = 0;

    * hexlength = v * 3 + 2;

    FUNC_LEAVE_NOAPI(SUCCEED)

} /* end H5C2_jb__bin2hex*/


/******************************************************************************
 *
 * Function:		H5C2_jb__comment
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Insert the supplied comment in the journal file. This 
 * 			call may be ignored if the journal file is machine 
 *			readable.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C2_jb__comment(H5C2_jbrb_t * struct_ptr,
		 const char * comment_ptr)
{
    char * temp = NULL;
    size_t temp_len;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C2_jb__comment, FAIL)
	
    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(comment_ptr);
    HDassert(struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC);
    HDassert(struct_ptr->hdf5_file_name);

    /* Verify that header message is present in journal file or ring buffer. 
     * If not, write it. 
     */
    if ( struct_ptr->header_present == FALSE ) {

        if ( H5C2_jb__write_header_entry(struct_ptr) != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C2_jb__write_header_entry() failed.\n")
        }

    } /* end if */

    temp_len = HDstrlen(comment_ptr) + 11;
    if(NULL == (temp = H5MM_malloc(temp_len + 1)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "allocation of temp buffer failed.")

    /* Write comment message */
    HDsnprintf(temp, (temp_len + 1), "C comment %s\n", comment_ptr);
    HDassert(temp_len == HDstrlen(temp));

    if(H5C2_jb__write_to_buffer(struct_ptr, temp_len, temp, FALSE, struct_ptr->cur_trans) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, "H5C2_jb__write_to_buffer() failed.\n")

done:
    if(NULL != temp) {
        temp = H5MM_xfree(temp);
        if(NULL != temp)
            HDONE_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, "free of assembly buffer failed.")
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5C2_jb__comment */


/*****************************************************************************
 *
 * Function:		H5C2_jb__end_transaction
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Verify that the supplied transaction is in progress,
 *			and that at least one journal entry has been written 
 *			under it. Then construct an end transaction message,
 *			and write it to the current journal buffer. Make note
 *			that the supplied transaction is closed, and that no
 *			transaction is in progress.
 *
 * Returns:		SUCCEED on success.
 *
 *****************************************************************************/
herr_t
H5C2_jb__end_transaction(H5C2_jbrb_t * struct_ptr,
			 uint64_t trans_num)
{
    char temp[25];
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C2_jb__end_transaction, FAIL)
#if 0 /* JRM */
    HDfprintf(stdout, "%s trans_num = %lld.\n", FUNC, trans_num);
#endif /* JRM */	
    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC);
	
    /* Verify that the supplied transaction is in progress */
    if ( ( struct_ptr->trans_in_prog != TRUE ) ||
         ( struct_ptr->cur_trans != trans_num ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Transaction not in progress or bad transaction number.")
    } /* end if */
	
    /* Verify that at least one journal entry has been written under 
     * the current transaction 
     */
    if ( struct_ptr->jentry_written != TRUE ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Empty transaction -- at least one journal entry required.")
    } /* end if */


    /* Prepare end transaction message */
    HDsnprintf(temp, (size_t)25, "3 end_trans %llu\n", trans_num);

    /* Write end transaction message */
    if ( H5C2_jb__write_to_buffer(struct_ptr, HDstrlen(temp), temp, 
			          TRUE, trans_num ) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_jb__write_to_buffer() failed.\n")
    } /* end if */

    /* reset boolean flag indicating if at least one journal entry has 
     * been written under transaction 
     */
    struct_ptr->jentry_written = FALSE;

    /* Close current transaction */
    struct_ptr->trans_in_prog = FALSE;

done:
	
    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__end_transaction */


/******************************************************************************
 *
 * Function:		H5C2_jb__eoa
 *
 * Programmer:		Mike McGreevy <mamcgree@hdfgroup.org>
 *			July 29, 2008
 *
 * Purpose:		Insert the supplied EOA into the journal file.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C2_jb__eoa(H5C2_jbrb_t * struct_ptr,
		 haddr_t eoa)
{
    char temp[41];
    size_t temp_len = 41;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C2_jb__eoa, FAIL)
	
    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC);
    HDassert(struct_ptr->hdf5_file_name);

    /* Verify that header message is present in journal file or ring buffer. 
     * If not, write it. 
     */
    if(struct_ptr->header_present == FALSE ) {
        if(H5C2_jb__write_header_entry(struct_ptr) != SUCCEED)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, "H5C2_jb__write_header_entry() failed.\n")
    } /* end if */

    /* Write EOA message */
    HDsnprintf(temp, temp_len, "E eoa_value 0x%llx\n", eoa);
    HDassert(HDstrlen(temp) < temp_len);

    if(H5C2_jb__write_to_buffer(struct_ptr, HDstrlen(temp), temp, FALSE, struct_ptr->cur_trans) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, "H5C2_jb__write_to_buffer() failed.\n")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5C2_jb__eoa */


/******************************************************************************
 *
 * Function:		H5C2_jb__flush
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Verify that there is no transaction in progress. Then
 * 			flush all journal entries in the journal buffers to the
 * 			journal file. Do not return until all entries are on
 * 			disk.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C2_jb__flush(H5C2_jbrb_t * struct_ptr)
{
    int result;
    int i;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C2_jb__flush, FAIL)

    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC);
	
    /* Check if transaction is in progress */

    if (struct_ptr->trans_in_prog != FALSE) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Attempt to flush buffers with transaction in progress.")
    } /* end if */

    if (struct_ptr->get > struct_ptr->put) {

	/* write from get through end of buffer */
	result = HDwrite(struct_ptr->journal_file_fd, 
	      (*struct_ptr->buf)[struct_ptr->get], 
	      (struct_ptr->num_bufs - struct_ptr->get) * struct_ptr->buf_size);

	if ( result == -1 ) {

            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		        "Journal file write failed(1).")
        }
        
	struct_ptr->bufs_in_use -= (struct_ptr->num_bufs - struct_ptr->get);
        struct_ptr->rb_free_space += (struct_ptr->num_bufs - struct_ptr->get) * struct_ptr->buf_size;
        struct_ptr->get = 0;
        
    } /* end if */

    if (struct_ptr->get < struct_ptr->put) {

	/* write from get up to, but not including, put */
	result = HDwrite(struct_ptr->journal_file_fd, 
	            (*struct_ptr->buf)[struct_ptr->get], 
	            (struct_ptr->put - struct_ptr->get) * struct_ptr->buf_size);

	if ( result == -1 ) {

            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		        "Journal file write failed (2).")
        }

	struct_ptr->bufs_in_use -= (struct_ptr->put - struct_ptr->get);
        struct_ptr->rb_free_space += (struct_ptr->put - struct_ptr->get) * struct_ptr->buf_size;
        struct_ptr->get = struct_ptr->put;

    } /* end if */

    if ( struct_ptr->cur_buf_free_space != struct_ptr->buf_size ) {

        /* flush partially filled portion of current journal buffer to disk */
	result = HDwrite(struct_ptr->journal_file_fd, 
	               (*struct_ptr->buf)[struct_ptr->put], 
	               struct_ptr->buf_size - struct_ptr->cur_buf_free_space);

	if ( result == -1 ) {

            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		        "Journal file write failed (3).")
        }

	struct_ptr->bufs_in_use--;
        struct_ptr->rb_free_space += (struct_ptr->buf_size - struct_ptr->cur_buf_free_space);

    } /* end if */

    HDassert(struct_ptr->bufs_in_use == 0);
    HDassert(struct_ptr->rb_free_space == struct_ptr->num_bufs * struct_ptr->buf_size);

    /* perform sync to ensure everything gets to disk before returning */
    /* Note: there is no HDfsync function, so for now, the standard
       fsync is being used. */
    if(fsync(struct_ptr->journal_file_fd) < 0)
        HGOTO_ERROR(H5E_IO, H5E_SYNCFAIL, FAIL, "Journal file sync failed.")

    /* record last transaction number that made it to disk */
    struct_ptr->last_trans_on_disk = 
		(*struct_ptr->trans_tracking)[struct_ptr->put];

    /* MIKE: optimization note: don't reset to top of ring buffer. 
     * instead, keep filling out current buffer so we can keep writes 
     * on block boundaries. 
     */
    struct_ptr->cur_buf_free_space = struct_ptr->buf_size;
    struct_ptr->rb_space_to_rollover = struct_ptr->num_bufs * struct_ptr->buf_size;
    struct_ptr->head = (*struct_ptr->buf)[0];
    struct_ptr->put = 0;

    /* Propogate the last transaction on in the buffers throughout the 
     * transaction tracking array. */
    for(i = 0; i < struct_ptr->num_bufs; i++)
	(*struct_ptr->trans_tracking)[i] = struct_ptr->last_trans_on_disk;

    /* update get index */
    struct_ptr->get = struct_ptr->put;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5C2_jb__flush */


/******************************************************************************
 *
 * Function:		H5C2_jb__flush_full_buffers
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Flush all the dirtied buffers in the ring buffer 
 *                      starting with the buffer referenced by struct_ptr->get 
 *                      and ending with the buffer right before the one
 *                      referenced by struct_ptr->put. 
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C2_jb__flush_full_buffers(H5C2_jbrb_t * struct_ptr)	
{
    int result;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C2_jb__flush_full_buffers, FAIL)

    /* this asserts that at least one buffer is in use */
    HDassert(struct_ptr->bufs_in_use > 0);
    /* write an assert to verify that at least one buffer is full */
    HDassert( (struct_ptr->put != struct_ptr->get) ||
              (struct_ptr->rb_free_space == 0)      );

    /* flush all full, dirtied journal buffers to disk */
    if (struct_ptr->get < struct_ptr->put) {

	/* can write solid chunk from get up to, but not 
	 * including, put 
	 */
	result = HDwrite(struct_ptr->journal_file_fd, 
	                 (*struct_ptr->buf)[struct_ptr->get], 
	                 (struct_ptr->put - struct_ptr->get) * 
                           struct_ptr->buf_size);

	if ( result == -1 ) {

            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		        "Journal file write failed (1).")
        }

	struct_ptr->bufs_in_use -= (struct_ptr->put - struct_ptr->get);
        struct_ptr->rb_free_space += (struct_ptr->put - struct_ptr->get) * struct_ptr->buf_size;

    } /* end if */

    else {

	/* write from get through end of buffer */
	result = HDwrite(struct_ptr->journal_file_fd, 
	      (*struct_ptr->buf)[struct_ptr->get], 
	      (struct_ptr->num_bufs - struct_ptr->get) * struct_ptr->buf_size);

	if ( result == -1 ) {

            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		        "Journal file write failed (2).")
        }

	struct_ptr->bufs_in_use -= (struct_ptr->num_bufs - struct_ptr->get);
        struct_ptr->rb_free_space += (struct_ptr->num_bufs - struct_ptr->get) *
                                     struct_ptr->buf_size;

        /* if put = 0, then everything that needs to be flushed will have been
         * flushed, so we can stop here. Otherwise, need to flush all buffers
         * from the start of the ring buffer's allocated space up to, but not
         * including, the buffer indexed by put. 
         */
        if (struct_ptr->put != 0) {

            result = HDwrite(struct_ptr->journal_file_fd, 
                             (*struct_ptr->buf)[0], 
                             (struct_ptr->put) * struct_ptr->buf_size);

	    if ( result == -1 ) {

                HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		            "Journal file write failed(3).")
            } /* end if */

        struct_ptr->rb_free_space += (struct_ptr->put * struct_ptr->buf_size);

        } /* end if */

	struct_ptr->bufs_in_use -= struct_ptr->put;

    } /* end else */
	
    HDassert(struct_ptr->bufs_in_use <= 1);

    /* update get index */
    struct_ptr->get = struct_ptr->put;
	
    /* record last transaction number that made it to disk */
    if (struct_ptr->put == 0) {

	struct_ptr->last_trans_on_disk = 
		(*struct_ptr->trans_tracking)[struct_ptr->num_bufs - 1];

    } else {

	struct_ptr->last_trans_on_disk = 
		(*struct_ptr->trans_tracking)[struct_ptr->put - 1];
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__flush_full_buffers */


/******************************************************************************
 *
 * Function:		H5C2_jb__get_last_transaction_on_disk
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Lookup the number of the last transaction to have been
 *			fully written to disk, and place its transaction
 *			number in *trans_num_ptr. If no transaction has made
 *			it to disk, load zero into *trans_num_ptr.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C2_jb__get_last_transaction_on_disk(H5C2_jbrb_t * struct_ptr,
				      uint64_t * trans_num_ptr)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C2_jb__get_last_transaction_on_disk, FAIL)
	
    /* Check Arguments */
    HDassert( struct_ptr != NULL );
    HDassert( trans_num_ptr != NULL );
    HDassert( struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC );

    /* JRM: In machine readable version, lets check to see if a sync is 
     *      necessary, and call it only if it is.
     */
    /* perform a sync to ensure everything gets to disk before continuing */
    /* Note: there is no HDfsync function, so for now, the standard
       fsync is being used. */
    if(fsync(struct_ptr->journal_file_fd) < 0 )
        HGOTO_ERROR(H5E_IO, H5E_SYNCFAIL, FAIL, "Jounal file sync failed.")

    * trans_num_ptr = struct_ptr->last_trans_on_disk;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5C2_jb__get_last_transaction_on_disk */


/******************************************************************************
 *
 * Function:		H5C2_jb__init
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Tuesday, February 5, 2008
 *
 * Purpose:		Initialize the supplied instance of H5C2_jbrb_t as
 *			specified by the buf_size and num_bufs fields. Open the
 *			journal file whose name is supplied in journal_file_name
 *			for either synchronous or asynchronous I/O as specified
 * 			by use_aio.
 *
 * Returns:		SUCCEED on success.
 *
 * Changes:		JRM -- 2/10/09
 *			Added the journal_magic parameter and related code.
 *
 *			Also deleted code to write the header message.
 *			Since the base address of the journal magic in 
 *			the HDF5 file isn't available at this time, wait
 *			until our first real entry to write the header.
 *
 ******************************************************************************/

herr_t 
H5C2_jb__init(H5C2_jbrb_t * struct_ptr,  	
              const int32_t journal_magic,
	      const char * HDF5_file_name,	 	
	      const char * journal_file_name, 	
	      size_t buf_size,		
	      int num_bufs,		 	
	      hbool_t use_aio,		
 	      hbool_t human_readable)
{
    int 	i;
    herr_t 	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C2_jb__init, FAIL)

    /* Check Arguments */
    HDassert( struct_ptr );
    HDassert( HDF5_file_name );
    HDassert( journal_file_name );
    HDassert( buf_size > 0 );
    HDassert( num_bufs > 0 );
    HDassert( struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC );

    /* Initialize Fields of H5C2_jbrb_t structure.  Note that we will
     * overwrite some of these initializations almost immediately.
     */
    struct_ptr->journal_magic = journal_magic;
    struct_ptr->journal_file_fd = -1;
    struct_ptr->num_bufs = num_bufs;
    struct_ptr->buf_size = buf_size;
    struct_ptr->bufs_in_use = 0;
    struct_ptr->jvers = H5C2__JOURNAL_VERSION;
    struct_ptr->get = 0;
    struct_ptr->put = 0;
    struct_ptr->jentry_written = FALSE;
    struct_ptr->use_aio = use_aio;
    struct_ptr->human_readable = human_readable;
    struct_ptr->journal_is_empty = TRUE;
    struct_ptr->cur_trans = 0;
    struct_ptr->last_trans_on_disk = 0;
    struct_ptr->trans_in_prog = FALSE;
    struct_ptr->jname = HDstrdup(journal_file_name);

    if ( struct_ptr->jname == NULL ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                "allocation of space for copy of journal_file_name failed.");
    } 

    struct_ptr->hdf5_file_name = HDstrdup(HDF5_file_name);

    if ( struct_ptr->jname == NULL ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                "allocation of space for copy of HDF5_file_name failed.");
    } 

    struct_ptr->header_present = FALSE;
    struct_ptr->cur_buf_free_space = buf_size;
    struct_ptr->rb_space_to_rollover = num_bufs * buf_size;
    struct_ptr->rb_free_space = num_bufs * buf_size;
    struct_ptr->head = NULL;
    struct_ptr->trans_tracking = NULL;
    struct_ptr->buf = NULL;

	
    /* Open journal file */
    struct_ptr->journal_file_fd = 
	    HDopen(journal_file_name, O_WRONLY|O_CREAT|O_EXCL, 0777);

    if ( struct_ptr->journal_file_fd  == -1) {

        HGOTO_ERROR(H5E_FILE, H5E_CANTCREATE, FAIL, \
                    "Can't create journal file.  Does it already exist?")
    } /* end if */

	
    /* Allocate space for the ring buffer's journal buffer pointers */
    struct_ptr->buf = H5MM_malloc(struct_ptr->num_bufs * sizeof(char *));

    if ( struct_ptr->buf == NULL ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "allocation of buf pointer array failed.");
    } /* end if */
	
    /* Allocate space for journal buffers */
    (*struct_ptr->buf)[0] = 
            H5MM_malloc(struct_ptr->buf_size * struct_ptr->num_bufs);

    if ( (*struct_ptr->buf)[0] == NULL ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "allocation of buffers failed.");
    } /* end if */

    /* Allocate space for the purposes of tracking the last 
     * transaction on disk 
     */
    struct_ptr->trans_tracking = 
    	H5MM_malloc(struct_ptr->num_bufs * sizeof(unsigned long));

    if ( struct_ptr->trans_tracking == NULL ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "allocation of trans_tracking failed.");
    } /* end if */
	
    /* Initialize the transaction tracking array */
    for (i=0; i<struct_ptr->num_bufs; i++)
    {
	(*struct_ptr->trans_tracking)[i] = 0;
    }
	
    /* Make journal buffer pointers point to the right location in 
     * chunk of allocated memory above 
     */
    for ( i = 1; i < struct_ptr->num_bufs; i++ )
    {
	(*struct_ptr->buf)[i] = 
		&((*struct_ptr->buf)[0])[i * struct_ptr->buf_size];
    }

    /* Define head pointer to point at where we are writing to in the buffer */
    struct_ptr->head = (*struct_ptr->buf)[struct_ptr->put];
#if 0 /* JRM */
    if ( H5C2_jb__write_header_entry(struct_ptr) != SUCCEED ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_jb__write_header_entry() failed.\n")
    }
#endif /* JRM */
done:
	
    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__init */


/******************************************************************************
 *
 * Function:		H5C2_jb__journal_entry
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Verify that the specified transaction is open. Then
 *			construct a journal entry recording the supplied base
 *			address, length, and body, and write it to the current
 *			journal buffer.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C2_jb__journal_entry(H5C2_jbrb_t * struct_ptr,
			uint64_t trans_num,
			haddr_t base_addr,
			size_t length,
			const uint8_t * body)
{

    char * temp = NULL;
    char * hexdata = NULL;
    size_t hexlength;
    herr_t ret_value = SUCCEED;
    uint8_t * bodydata;

    FUNC_ENTER_NOAPI(H5C2_jb__journal_entry, FAIL)
#if 0 /* JRM */
    HDfprintf(stdout, "%s trans_num = %lld, base_addr = %d, length = %ld.\n",
              FUNC, trans_num, (int)base_addr, (int)length);
#endif /* JRM */	
    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC);

    /* Make a copy of body data */
    if ( (bodydata = H5MM_malloc(length)) == NULL ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                   "allocation of assembly buffer failed.");
    }

    HDmemcpy(bodydata, body, length);
	
    /* Verify that the supplied transaction is in progress */
    if ( ( struct_ptr->trans_in_prog != TRUE ) ||
         ( struct_ptr->cur_trans != trans_num ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Transaction not in progress or bad transaction number.")
    } /* end if */

    if ( (temp = H5MM_malloc(length + 100)) == NULL ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                   "allocation of assembly buffer failed.");
    }

    if ( (hexdata = H5MM_malloc(length * 40)) == NULL ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                   "allocation of assembly buffer failed.");
    }

    /* Write journal entry */
    HDsnprintf(temp, 
               (size_t)(length + 100),
               "2 trans_num %llu length %zu base_addr 0x%lx body ", 
 	       trans_num, 
	       length, 
	       (unsigned long)base_addr);

    if ( H5C2_jb__write_to_buffer(struct_ptr, HDstrlen(temp), temp, FALSE, trans_num) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_jb__write_to_buffer() failed.\n")
    } /* end if */

    /* Convert data from binary to hex */
    H5C2_jb__bin2hex(bodydata, hexdata, &hexlength, length);

    if ( H5C2_jb__write_to_buffer(struct_ptr, hexlength, hexdata, FALSE, trans_num) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_jb__write_to_buffer() failed.\n")
    } /* end if */

    /* Indicate that at least one journal entry has been written under 
     * this transaction 
     */
    if ( struct_ptr->jentry_written == FALSE ) {

	struct_ptr->jentry_written = TRUE;
    }

done:
    if(bodydata != NULL) {
        bodydata = H5MM_xfree(bodydata);
        if(bodydata != NULL)
            HDONE_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, "free of assembly buffer failed.")
    } /* end if */

    if(temp != NULL) {
        temp = H5MM_xfree(temp);
        if(temp != NULL)
            HDONE_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, "free of assembly buffer failed.")
    } /* end if */

    if(hexdata != NULL) {
        hexdata = H5MM_xfree(hexdata);
        if(hexdata != NULL)
            HDONE_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, "free of assembly buffer failed.")
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5C2_jb__journal_entry */


/******************************************************************************
 *
 * Function:		H5C2_jb__start_transaction
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Verify that there is no transaction in progress, and
 *			that the supplied transaction number greater than 
 *			the last.  Then construct a start transaction message, 
 *			and write it to the current journal buffer. Make note
 *			of the fact that the supplied transaction is in
 *			progress.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C2_jb__start_transaction(H5C2_jbrb_t * struct_ptr,
			   uint64_t trans_num)

{
    char temp[150];
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C2_jb__start_transaction, FAIL)
#if 0 /* JRM */
    HDfprintf(stdout, "%s trans_num = %lld.\n", FUNC, trans_num);
#endif /* JRM */	
    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC);
	
    /* Verify that there is no transaction in progress */
    if ( struct_ptr->trans_in_prog != FALSE ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Transaction already in progress.")
    } /* end if */

    /* JRM: Heads up:  we may relax this constraint to rquire that the 
     *      new transaction number is greater than the old, but possibly
     *      not the next integer in sequence.  Will this cause problems
     *      with testing?
     */

    /* Verify that the supplied transaction number greater than the last */
    if ( (struct_ptr->cur_trans) >= trans_num ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "New transaction out of sequence.")
    } /* end if */

    /* Verify that header message is present in journal file or ring buffer. 
     * If not, write it. 
     */
    if ( struct_ptr->header_present == FALSE ) {

        if ( H5C2_jb__write_header_entry(struct_ptr) != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C2_jb__write_header_entry() failed.\n")
        }

    } /* end if */

    /* Write start transaction message */
    HDsnprintf(temp, (size_t)150, "1 bgn_trans %llu\n", trans_num);
    if ( H5C2_jb__write_to_buffer(struct_ptr, HDstrlen(temp), temp, 
			          FALSE, trans_num) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_jb__write_to_buffer() failed.\n")
    } /* end if */
		
    /* Make note of the fact that supplied transaction is in progress */
    struct_ptr->trans_in_prog = TRUE;
    struct_ptr->cur_trans = trans_num;

done:
	
    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__start_transaction */


/******************************************************************************
 *
 * Function:		H5C2_jb__takedown
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Thursday, February 7, 2008
 *
 * Purpose:		Verify that the journal buffers are empty, and that the
 *			journal file has been truncated. Then close and delete
 *			the journal file associated with *struct_ptr, and free
 *			all dynamically allocated memory associated with 
 *			*struct_ptr.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C2_jb__takedown(H5C2_jbrb_t * struct_ptr)

{
    herr_t ret_value = SUCCEED;
	
    FUNC_ENTER_NOAPI(H5C2_jb__takedown, FAIL)

    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC);
	
    /* Verify that the journal buffers are empty */
    if ( struct_ptr->bufs_in_use != 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
	            "Attempt to takedown with non-empty buffers.")
    } /* end if */	

    /* Verify that the journal file has been truncated */
    if (struct_ptr->journal_is_empty != TRUE) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
	            "Attempt to takedown with journal file not truncated.")
    } /* end if */

    /* Close and delete the journal file associated with struct_ptr */
    if ( HDclose(struct_ptr->journal_file_fd) < 0 ) {

        HGOTO_ERROR(H5E_IO, H5E_CLOSEERROR, FAIL, "Jounal file close failed.")
    } /* end if */

    if ( HDremove(struct_ptr->jname) < 0) {

        HGOTO_ERROR(H5E_IO, H5E_REMOVEFAIL, FAIL, "Jounal file close failed.")
    } /* end if */

    /* Free all memory associated with struct_ptr */

    if ( struct_ptr->jname != NULL ) {

        struct_ptr->jname = H5MM_xfree(struct_ptr->jname);

        if ( struct_ptr->jname != NULL ) {

            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "free of jname failed.");
        }
    }

    if ( struct_ptr->hdf5_file_name != NULL ) {

        struct_ptr->hdf5_file_name = H5MM_xfree(struct_ptr->hdf5_file_name);

        if ( struct_ptr->hdf5_file_name != NULL ) {

            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "free of hdf5_file_name failed.");
        }
    }

    if ( (*struct_ptr->buf)[0] != NULL ) {

        (*struct_ptr->buf)[0] = H5MM_xfree((*struct_ptr->buf)[0]);
        if ( (*struct_ptr->buf)[0] != NULL ) {

            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "free of buffers failed.");
        }
    }

    if ( struct_ptr->buf != NULL ) {

        struct_ptr->buf = H5MM_xfree(struct_ptr->buf);
        if ( struct_ptr->buf != NULL ) {

            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "free of buffer pointer array failed.");
        }
    }

    if ( struct_ptr->trans_tracking != NULL ) {

        struct_ptr->trans_tracking = H5MM_xfree(struct_ptr->trans_tracking);

        if ( struct_ptr->trans_tracking != NULL ) {

            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "free of transaction tracking array failed.");
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__takedown */


/******************************************************************************
 *
 * Function:		H5C2_jb__trunc
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Thursday, February 7, 2008
 *
 * Purpose:		Verify that there is no transaction in progress, and 
 *			that the journal entry buffers are empty. Truncate
 *			the journal file. Does not return until the file
 *			is truncated on disk.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C2_jb__trunc(H5C2_jbrb_t * struct_ptr)

{
    int i;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C2_jb__trunc, FAIL)

    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC);
	
    /* Verify that there is no transaction in progress */
    if ( struct_ptr->trans_in_prog != FALSE ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
	     "Attempt to truncate journal file while transaction in progress.")
    } /* end if */

    /* Verify that the journal buffers are empty */
    if ( struct_ptr->bufs_in_use != 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
	            "Attempt to truncate with non-empty buffers.")
    } /* end if */	

    /* Truncate the journal file */
    if ( HDftruncate(struct_ptr->journal_file_fd, (off_t)0) < 0 ) {

        HGOTO_ERROR(H5E_IO, H5E_SYNCFAIL, FAIL, "Jounal file truncate failed.")
    } /* end if */

    /* Start back to top of journal buffer and journal file */
    struct_ptr->header_present = FALSE;
    struct_ptr->journal_is_empty = TRUE;

    /* reset the transaction number fields */
    struct_ptr->cur_trans = 0;
    struct_ptr->last_trans_on_disk = 0;
	
    /* reset the transaction tracking array */
    for (i=0; i<struct_ptr->num_bufs; i++)
    {
	(*struct_ptr->trans_tracking)[i] = 0;
    }

    if ( HDlseek(struct_ptr->journal_file_fd, (off_t)0, SEEK_SET) == (off_t)-1 )
    {
        HGOTO_ERROR(H5E_IO, H5E_SEEKERROR, FAIL, "Jounal file seek failed.")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__trunc */


/******************************************************************************
 *
 * Function:		H5C2_jb__write_header_entry
 *
 * Programmer:		John Mainzer
 *			2/12/09
 *
 * Purpose:		Write the header message to the journal file.
 * 
 *			This message appear exactly once in every journal
 *			file, and is always the first message in the file.
 *			It identifies the journal file, and contains 
 *			information required to run the journal, should 
 *			that be necessary.
 *
 *			It is always in human readable format.
 *			
 * Returns:		SUCCEED on success.
 *			FAIL on failure.
 *
 * Changes:		JRM -- 3/21/09
 *                      Moved the entry tag strings into #defines.  
 *			Replaced all white space in the creation date 
 *			string with underscores.
 *
 ******************************************************************************/

herr_t 
H5C2_jb__write_header_entry(H5C2_jbrb_t * struct_ptr)

{
    herr_t      ret_value = SUCCEED;
    char 	*buf;
    char      * p;
    char	time_buf[32];
    int		chars_written;
    int         i;
    size_t      file_name_len;
    size_t	buf_len;
    time_t      current_date;
	
    FUNC_ENTER_NOAPI(H5C2_jb__write_header_entry, FAIL)

    /* Check Arguments */
    HDassert( struct_ptr );
    HDassert( struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC );
    HDassert( struct_ptr->hdf5_file_name != NULL );
    HDassert( struct_ptr->header_present == FALSE );
    HDassert( struct_ptr->journal_is_empty == TRUE );

    file_name_len = HDstrlen(struct_ptr->hdf5_file_name);

    HDassert( file_name_len > 0 );

    buf_len = file_name_len + 256;
	
    /* Allocate space for journal buffers */
    buf = H5MM_malloc(buf_len);

    if ( buf == NULL ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "buffer allocation failed.");
    } /* end if */
	
    /* Get the current date */
    current_date = HDtime(NULL);

    /* load ascii representation of current_date into time_buf[],
     * replacing white space with underscores.
     */
    time_buf[31] = '\0'; /* just to be safe */

    if ( (p = HDctime(&current_date)) == NULL ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "Can't get ascii representation of current date.")

    } else {

        /* copy the string into time_buf, replacing white space with 
         * underscores.
         *
         * Do this to make parsing the header easier.
         */
        i = 0;

        while ( ( i < 31 ) && ( *p != '\0' ) ) {

            if ( isspace(*p) ) {

                time_buf[i] = '_';

            } else {

                time_buf[i] = *p;
            }

            i++;
            p++;
        }

        time_buf[i] = '\0';
    }

    /* Format the header message in the temporary buffer */

    chars_written = 
        HDsnprintf(buf, 
                   buf_len - 1,
                   "0 %s %ld %s %s %s %d %s %10.10s %s %d\n",
                   H5C2_JNL__VER_NUM_TAG,
	           struct_ptr->jvers, 
                   H5C2_JNL__TGT_FILE_NAME_TAG,
	           struct_ptr->hdf5_file_name, 
                   H5C2_JNL__JNL_MAGIC_TAG,
                   (int)(struct_ptr->journal_magic),
                   H5C2_JNL__CREATION_DATE_TAG,
	           time_buf,
                   H5C2_JNL__HUMAN_READABLE_TAG,
	           struct_ptr->human_readable);

    if ( chars_written >= buf_len - 1 ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_CANTCOPY, FAIL, \
                    "tried to overwrite buffer.");
    }

    HDassert( HDstrlen(buf) < buf_len );

    /* Write the header message into the ring buffer */
    if ( H5C2_jb__write_to_buffer(struct_ptr, HDstrlen(buf), buf, FALSE, 
			          (uint64_t)0) < 0) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_jb__write_to_buffer() failed.\n")
    } /* end if */

    /* Update boolean flags */
    struct_ptr->header_present = TRUE;
    struct_ptr->journal_is_empty = FALSE;

done:
    if(buf != NULL) {
        buf = H5MM_xfree(buf);
        if(buf != NULL)
            HDONE_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, "free of buf failed.")
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5C2_jb__write_header_entry() */


/******************************************************************************
 *
 * Function:		H5C2_jb__write_to_buffer
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Put the contents of data into the journal buffers. This
 * 			is done as follows: While the data to be written is 
 * 			larger than the amount of space left in the ring buffer,
 * 			the ring buffer is filled to capacity with data and
 *			flushed. This repeats until the unwritten data remaining
 * 			can fit in the ring buffer without having to loop around
 *			to the top.
 *
 *			At this point, the rest of the data can just be written
 *			without having to break it up further. In the event
 *			the data covers more than one journal buffer, the get 
 *			and put indices are updated to state this fact. Any 
 *			journal buffers that were filled during the write are 
 *			flushed.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C2_jb__write_to_buffer(H5C2_jbrb_t * struct_ptr,	
			size_t size,			
			const char * data,
                        hbool_t is_end_trans,
                        uint64_t trans_num)
{
    herr_t ret_value = SUCCEED;
    unsigned long track_last_trans = 0;
    int oldput = 0;
    int i;
	
    FUNC_ENTER_NOAPI(H5C2_jb__write_to_buffer, FAIL)

    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(data);
    HDassert(struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC);
    HDassert(HDstrlen(data) == size);
    HDassert(struct_ptr->rb_space_to_rollover <= 
		    struct_ptr->num_bufs * struct_ptr->buf_size);
    HDassert(struct_ptr->rb_space_to_rollover > 0); 

    /* If the data size exceeds the bounds of the ring buffer's allocated 
     * memory, loop around to top 
     */
    if (size >= struct_ptr->rb_space_to_rollover) {

	while (size >= struct_ptr->rb_space_to_rollover) {
			
	    /* Assertions */
	    HDassert(size != 0);
	    HDassert(HDstrlen(data) >= struct_ptr->rb_space_to_rollover);

	    /* fill up remaining space in the ring buffer */
	    HDmemcpy(struct_ptr->head, data, struct_ptr->rb_space_to_rollover);
			
	    /* move head to point to start of ring buffer */
	    struct_ptr->head = (*struct_ptr->buf)[0];

            /* make note of last transaction on disk */
            track_last_trans = (*struct_ptr->trans_tracking)[struct_ptr->put];
            
            /* update rb_free_space */
            struct_ptr->rb_free_space -= struct_ptr->rb_space_to_rollover;

            /* Fill out the remainder of the trans_tracking array with
               the most recent transaction in the array.*/
	    (*struct_ptr->trans_tracking)[0] = track_last_trans;
            for (i=struct_ptr->put; i<struct_ptr->num_bufs; i++)
            {
	        (*struct_ptr->trans_tracking)[i] = track_last_trans;
            }

	    /* reset put index */
	    struct_ptr->put = 0;

	    /* update bufs_in_use as necessary */
	    struct_ptr->bufs_in_use = struct_ptr->num_bufs - struct_ptr->get;

            /* check to see if trans_tracking needs to be updated. If so,
               then update it */
            if ((size == struct_ptr->rb_space_to_rollover) &&
                (is_end_trans == TRUE)) {
                
                (*struct_ptr->trans_tracking)[struct_ptr->num_bufs - 1] 
                                                    = trans_num;
                (*struct_ptr->trans_tracking)[0] = trans_num;
            }

	    /* flush buffers */
	    if ( H5C2_jb__flush_full_buffers(struct_ptr) < 0 ) {

		 HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                             "H5C2_jb__flush_full_buffers() failed.\n")
            }

	    /* update remaining size of data to be written */
	    size = size - struct_ptr->rb_space_to_rollover;

	    /* update the data pointer to point to the remaining data to be 
	     * written 
	     */
	    data = &data[struct_ptr->rb_space_to_rollover];

	    /* update the amount of space left at end of ring buffer */
	    struct_ptr->rb_space_to_rollover = 
		    struct_ptr->buf_size * struct_ptr->num_bufs;

	    /* update the amount of space in the current buffer */
	    struct_ptr->cur_buf_free_space = struct_ptr->buf_size;

	} /* end while */
    } /* end if */
	
    /* If the size of the data exceeds the bounds of a single journal 
     * buffer, will write into multiple 
     */
    if (size > struct_ptr->cur_buf_free_space) {

	HDassert(struct_ptr->cur_buf_free_space != 0);

	/* write data into journal buffers */
	HDmemcpy(struct_ptr->head, data, size);

	/* update head pointer */
	struct_ptr->head = &struct_ptr->head[size];

        /* make note of last transaction on disk */
        track_last_trans = (*struct_ptr->trans_tracking)[struct_ptr->put];
        oldput = struct_ptr->put;

        /* update rb_free_space */
        struct_ptr->rb_free_space -= size;

	/* update put index */
	struct_ptr->put += 
            (size-struct_ptr->cur_buf_free_space)/(struct_ptr->buf_size) + 1; 

        /* Drag the last transaction in a filled buffer value residing in the 
           old put location through the trans_tracking array to the new 
           corresponding put position. */
        for (i=oldput; i<struct_ptr->put+1; i++)
        {
            (*struct_ptr->trans_tracking)[i] = track_last_trans;
        }

	/* update current buffer usage */
	struct_ptr->cur_buf_free_space = 
            struct_ptr->rb_space_to_rollover - size - 
            (struct_ptr->num_bufs - (struct_ptr->put + 1)) * 
            (struct_ptr->buf_size );

	/* update bufs_in_use as necessary */
	struct_ptr->bufs_in_use = struct_ptr->put - struct_ptr->get;
	if (struct_ptr->cur_buf_free_space < struct_ptr->buf_size) {

	    struct_ptr->bufs_in_use++;
        }

        /* check to see if trans_tracking needs to be updated. If so,
           then update it */
        if (is_end_trans == TRUE) {
                
            if (struct_ptr->cur_buf_free_space == struct_ptr->buf_size) {
                (*struct_ptr->trans_tracking)[struct_ptr->put - 1] = trans_num;
            }
            else {
                (*struct_ptr->trans_tracking)[struct_ptr->put] = trans_num;
            }

        } /* end if */

	/* flush buffers */
	if ( H5C2_jb__flush_full_buffers(struct_ptr) < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C2_jb__flush_full_buffers() failed.\n")
        }

	/* update space left at end of ring buffer */
	struct_ptr->rb_space_to_rollover -= size;

    } /* end if */

    /* if the data can fit in the remaining space in the current journal 
     * buffer indexed by put 
     */
    else if (size > 0)  {

	HDassert(size <= struct_ptr->cur_buf_free_space);
		
	/* write data into journal buffer */
	HDmemcpy(struct_ptr->head, data, size);

	/* increment bufs_in_use as necessary */
	if ( ( struct_ptr->bufs_in_use == 0 ) ) {

	    struct_ptr->bufs_in_use++;
        }

	/* update head pointer */
	struct_ptr->head = &struct_ptr->head[size];

        /* update rb_free_space */
        struct_ptr->rb_free_space -= size;

	/* update current buffer usage */
	struct_ptr->cur_buf_free_space -= size;

	/* update end of buffer space */
	struct_ptr->rb_space_to_rollover -= size;
		
        /* check to see if trans_tracking needs to be updated. If so,
           then update it */
        if (is_end_trans == TRUE) {
                
            (*struct_ptr->trans_tracking)[struct_ptr->put] 
                                            = trans_num;
        } /* end if */

	/* if buffer is full, flush it, and loop to the top of the 
	 * ring buffer if at the end. 
	 */
	if (struct_ptr->cur_buf_free_space == 0) {

	    if ( struct_ptr->put != (struct_ptr->num_bufs - 1) ) {
		struct_ptr->put += 1;

                /* Drag trans_tracking value into next buffer */
                (*struct_ptr->trans_tracking)[struct_ptr->put] =
                        (*struct_ptr->trans_tracking)[struct_ptr->put - 1];

	    } /* end if */
                
            else {

		struct_ptr->put = 0;

                /* Drag trans_tracking value into next buffer */
                (*struct_ptr->trans_tracking)[0] 
                 = (*struct_ptr->trans_tracking)[struct_ptr->num_bufs - 1];

                /* reset head pointer and free space values */
		struct_ptr->head = (*struct_ptr->buf)[0];
		struct_ptr->rb_space_to_rollover = 
			struct_ptr->buf_size * struct_ptr->num_bufs;

	    } /* end else */

	    if ( H5C2_jb__flush_full_buffers(struct_ptr) < 0 ) {

                HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                            "H5C2_jb__flush_full_buffers() failed.\n")
            } /* end if */

	    struct_ptr->cur_buf_free_space = struct_ptr->buf_size;

	} /* end if */

    } /* end else */
	
    HDassert(struct_ptr->bufs_in_use <= struct_ptr->num_bufs);

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__write_to_buffer */

