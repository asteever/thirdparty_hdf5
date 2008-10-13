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
 * Programmer: John Mainzer -- 10/12/04
 *
 * Purpose:     This file contains declarations which are normally visible
 *              only within the H5C2 package (just H5C2.c at present).
 *
 *		Source files outside the H5C2 package should include
 *		H5C2private.h instead.
 *
 *		The one exception to this rule is test/cache2.c.  The test
 *		code is easier to write if it can look at the cache's
 *		internal data structures.  Indeed, this is the main
 *		reason why this file was created.
 */

#ifndef H5C2_PACKAGE
#error "Do not include this file outside the H5C2 package!"
#endif

#ifndef _H5C2pkg_H
#define _H5C2pkg_H


/* Get package's private header */
#include "H5C2private.h"


/* Get needed headers */
#include "H5SLprivate.h"        /* Skip lists */


/******************************************************************************
 *
 * Structure: 		H5C2_jbrb_t
 *
 * Programmer: 		Mike McGreevy <mcgreevy@hdfgroup.org>
 * 			Tuesday, February 5, 2008
 *
 * Purpose:		Instances of the H5C2_jbrb_t structure are used to
 * 			implement a ring buffer of journal buffers. This
 * 			structure is used in association with HDF5 File
 * 			Recovery. It is used to journal metadata cache
 * 			changes in an effort to be able to reproduce
 * 			actions in the event of a crash during data writing.
 *
 *			The fields of this structure are discussed below:
 *
 *
 * magic:		Unsigned 32-bit integer always set to 
 * 			H5C2__H5C2_JBRB_T_MAGIC.  This field is used to validate
 * 			pointers to instances of H5C_jbrb_t.
 *			
 * journal_file_fd:	File Descriptor of the journal file that is being 
 * 			written to from this ring buffer.
 *
 * num_bufs:		The number of journal buffers in the ring buffer. This
 * 			must be at least 2 in the asynchronous case (one for
 * 			storing journal entries as they are accumulated, and
 * 			one for holding the last set of journal entries while
 *			they are being written to disk).
 *
 * buf_size:		The size of each journal buffer in the ring buffer. This
 * 			value is user specified, and will determine how much
 * 			data each journal buffer can hold before a move to
 * 			another journal buffer in the ring buffer is necessary.
 *			Typically, this will be a multiple of the block size of
 *			the underlying file system.
 *
 * bufs_in_use:		This is the current number of dirty journal buffers
 * 			in the ring buffer.
 *
 * jvers:		The journal version number. This is used to keep track
 * 			of the formatting changes of the journal file.
 *
 * get:			Number of the journal buffer that is next in line to
 * 			be written to disk. (i.e. the least recently dirtied 
 * 			journal buffer).
 *
 * put:			Number of the journal buffer that is currently being
 *		 	written to.
 *
 * jentry_written:	Boolean flag that indiciates if a journal entry has
 *			been written under the current transaction.
 *
 * use_aio:		Boolean flag that indicates whether synchronous or
 *			asynchronous writes will be used.
 *
 * human_readable:	Boolean flag that indicates whether the journal file
 *			is to be human readable or machine readable.
 *	
 * journal_is_empty:	Boolean flag that indicates if the journal file
 *			associated with the ring buffer is currently
 * 			empty.
 *
 * cur_trans:		Current transaction number, used to differentiate
 *			between differing journal entries in the journal file.
 *
 * last_trans_on_disk:	Number of the last transaction that has successfully
 * 			made it to disk.
 *
 * trans_in_prog:	Boolean flag that indicates if a transaction is in
 *			progress or not.
 *
 * jname: 		Character array containing the name of the journal file.
 *
 * hdf5_file_name: 	Character array containing the name of the HDF5 file
 *			associated with this journal file.
 *	
 * header_present:	Boolean flag that indicates if the header message has
 *			been written into the current journal file or journal
 *			buffer.
 *
 * cur_buf_free_space: 	The amount of space remaining in the currently active
 *			journal buffer. This is used to determine when the
 * 			ring buffer needs to switch to writing to the next
 *			journal buffer.
 *
 * rb_space_to_rollover: The amount of space left at the end of the ring 
 *                      buffer, starting at the head pointer, and ending at
 *                      the end of the ring buffer's allocate space. This
 *                      is used to keep track of when a rollover to the start
 *                      of the ring buffer must occur.
 *
 * rb_free_space:       The amount of unused space in the ring buffer.
 * 
 * head: 		A pointer to the location in the active journal buffer
 *			that is to be written to.
 *
 * trans_tracking:      An array of size num_bufs that reports the last
 *                      transaction successfully written into each buffer. This
 *                      is used when the buffers are flushed to determine which
 *                      is the last transaction successfully on disk.
 *
 * buf:			Array of char pointers to each journal buffer in the
 *			ring buffer. This is allocated as a single chunk of 
 * 			memory, and thus data can be written past a buffer
 * 			boundary provided it will not extend past the end
 * 			of the total area allocated for the ring buffer.
 *
 ******************************************************************************/

#define H5C2__H5C2_JBRB_T_MAGIC   	(unsigned)0x00D0A03
#define H5C2__JOURNAL_VERSION		1

struct H5C2_jbrb_t 
{
	uint32_t	magic;
	int 		journal_file_fd;
	int 		num_bufs;
	size_t		buf_size;
	int		bufs_in_use;
	unsigned long 	jvers;
	int 		get;
	int 		put;
	hbool_t 	jentry_written;
	hbool_t		use_aio;
	hbool_t		human_readable;
	hbool_t		journal_is_empty;
	uint64_t	cur_trans;
	uint64_t	last_trans_on_disk;
	hbool_t		trans_in_prog;
	const char *	jname;
	const char *	hdf5_file_name;
	hbool_t 	header_present;
	size_t 		cur_buf_free_space;
	size_t		rb_space_to_rollover;
	size_t		rb_free_space;
	char * 		head;
	unsigned long	(*trans_tracking)[];
	char 		*((*buf)[]);
};


/* With the introduction of the fractal heap, it is now possible for 
 * entries to be dirtied, resized, and/or renamed in the flush callbacks.
 * As a result, on flushes, it may be necessary to make multiple passes
 * through the slist before it is empty.  The H5C2__MAX_PASSES_ON_FLUSH
 * #define is used to set an upper limit on the number of passes.
 * The current value was obtained via personal communication with 
 * Quincey.  I have applied a fudge factor of 2.
 */

#define H5C2__MAX_PASSES_ON_FLUSH	4


/****************************************************************************
 *
 * structure H5C2_mdjsc_record_t
 *
 * A dynamically allocate array of instances of H5C2_mdjsc_record_t is 
 * used to record metadata journaling status change callbacks -- of which 
 * there can be an arbitrary number.
 *
 * The fields in the structure are discussed individually below:
 *
 * fcn_ptr:	Pointer to the instance of H5C2_mdj_status_change_func_t
 * 		to be called on metadata journaling start or stop.  NULL
 * 		if this record is not in use.
 *
 * 		Note that the cache must be clean when this callback 
 * 		is called.
 *
 * data_ptr:	Pointer to void.  This value is supplied on registration,
 * 		and is passed to *fcn_ptr.  NULL if this record is not 
 * 		in use.
 *
 * fl_next:	Index of the next free entry in the metadata status change
 * 		callback table, or -1 if there is no next free entry or 
 * 		if the entry is in use.
 *
 ****************************************************************************/

typedef struct H5C2_mdjsc_record_t 
{
    H5C2_mdj_status_change_func_t	fcn_ptr;
    void *				data_ptr;
    int32_t				fl_next;
} H5C2_mdjsc_record_t;


/****************************************************************************
 *
 * structure H5C2_t
 *
 * Catchall structure for all variables specific to an instance of the cache.
 *
 * While the individual fields of the structure are discussed below, the
 * following overview may be helpful.
 *
 * Entries in the cache are stored in an instance of H5TB_TREE, indexed on
 * the entry's disk address.  While the H5TB_TREE is less efficient than
 * hash table, it keeps the entries in address sorted order.  As flushes
 * in parallel mode are more efficient if they are issued in increasing
 * address order, this is a significant benefit.  Also the H5TB_TREE code
 * was readily available, which reduced development time.
 *
 * While the cache was designed with multiple replacement policies in mind,
 * at present only a modified form of LRU is supported.
 *
 *                                              JRM - 4/26/04
 *
 * Profiling has indicated that searches in the instance of H5TB_TREE are
 * too expensive.  To deal with this issue, I have augmented the cache
 * with a hash table in which all entries will be stored.  Given the
 * advantages of flushing entries in increasing address order, the TBBT
 * is retained, but only dirty entries are stored in it.  At least for
 * now, we will leave entries in the TBBT after they are flushed.
 *
 * Note that index_size and index_len now refer to the total size of
 * and number of entries in the hash table.
 *
 *						JRM - 7/19/04
 *
 * The TBBT has since been replaced with a skip list.  This change
 * greatly predates this note.
 *
 *						JRM - 9/26/05
 *
 * magic:	Unsigned 32 bit integer always set to H5C2__H5C2_T_MAGIC.  
 * 		This field is used to validate pointers to instances of 
 * 		H5C2_t.
 *
 * flush_in_progress: Boolean flag indicating whether a flush is in 
 * 		progress.
 *
 * trace_file_ptr:  File pointer pointing to the trace file, which is used
 *              to record cache operations for use in simulations and design
 *              studies.  This field will usually be NULL, indicating that
 *              no trace file should be recorded.
 *
 *              Since much of the code supporting the parallel metadata
 *              cache is in H5AC, we don't write the trace file from 
 *              H5C2.  Instead, H5AC reads the trace_file_ptr as needed.
 *
 *              When we get to using H5C2 in other places, we may add
 *              code to write trace file data at the H5C2 level as well.
 *
 * aux_ptr:	Pointer to void used to allow wrapper code to associate
 *		its data with an instance of H5C2_t.  The H5C2 cache code
 *		sets this field to NULL, and otherwise leaves it alone.
 *
 * max_type_id:	Integer field containing the maximum type id number assigned
 *		to a type of entry in the cache.  All type ids from 0 to
 *		max_type_id inclusive must be defined.  The names of the
 *		types are stored in the type_name_table discussed below, and
 *		indexed by the ids.
 *
 * type_name_table_ptr: Pointer to an array of pointer to char of length
 *              max_type_id + 1.  The strings pointed to by the entries
 *              in the array are the names of the entry types associated
 *              with the indexing type IDs.
 *
 * max_cache_size:  Nominal maximum number of bytes that may be stored in the
 *              cache.  This value should be viewed as a soft limit, as the
 *              cache can exceed this value under the following circumstances:
 *
 *              a) All entries in the cache are protected, and the cache is
 *                 asked to insert a new entry.  In this case the new entry
 *                 will be created.  If this causes the cache to exceed
 *                 max_cache_size, it will do so.  The cache will attempt
 *                 to reduce its size as entries are unprotected.
 *
 *              b) When running in parallel mode, the cache may not be
 *		   permitted to flush a dirty entry in response to a read.
 *		   If there are no clean entries available to evict, the
 *		   cache will exceed its maximum size.  Again the cache
 *                 will attempt to reduce its size to the max_cache_size
 *                 limit on the next cache write.
 *
 *		c) When an entry increases in size, the cache may exceed
 *		   the max_cache_size limit until the next time the cache
 *		   attempts to load or insert an entry.
 *
 * min_clean_size: Nominal minimum number of clean bytes in the cache.
 *              The cache attempts to maintain this number of bytes of
 *              clean data so as to avoid case b) above.  Again, this is
 *              a soft limit.
 *
 *
 * In addition to the call back functions required for each entry, the
 * cache requires the following call back functions for this instance of
 * the cache as a whole:
 *
 * check_write_permitted:  In certain applications, the cache may not
 *		be allowed to write to disk at certain time.  If specified,
 *		the check_write_permitted function is used to determine if
 *		a write is permissible at any given point in time.
 *
 *		If no such function is specified (i.e. this field is NULL),
 *		the cache uses the following write_permitted field to
 *		determine whether writes are permitted.
 *
 * write_permitted: If check_write_permitted is NULL, this boolean flag
 *		indicates whether writes are permitted.
 *
 * log_flush:	If provided, this function is called whenever a dirty
 *		entry is flushed to disk.
 *
 *
 * In cases where memory is plentiful, and performance is an issue, it
 * is useful to disable all cache evictions, and thereby postpone metadata
 * writes.  The following field is used to implement this.
 *
 * evictions_enabled:  Boolean flag that is initialized to TRUE.  When
 * 		this flag is set to FALSE, the metadata cache will not 
 * 		attempt to evict entries to make space for newly protected
 * 		entries, and instead the will grow without limit.
 * 		
 * 		Needless to say, this feature must be used with care.
 *
 *
 * The cache requires an index to facilitate searching for entries.  The
 * following fields support that index.
 *
 * index_len:   Number of entries currently in the hash table used to index
 *		the cache.
 *
 * index_size:  Number of bytes of cache entries currently stored in the
 *              hash table used to index the cache.
 *
 *              This value should not be mistaken for footprint of the
 *              cache in memory.  The average cache entry is small, and
 *              the cache has a considerable overhead.  Multiplying the
 *              index_size by two should yield a conservative estimate
 *              of the cache's memory footprint.
 *
 * index:	Array of pointer to H5C2_cache_entry_t of size
 *		H5C2__HASH_TABLE_LEN.  At present, this value is a power
 *		of two, not the usual prime number.
 *
 *		I hope that the variable size of cache elements, the large
 *		hash table size, and the way in which HDF5 allocates space
 *		will combine to avoid problems with periodicity.  If so, we
 *		can use a trivial hash function (a bit-and and a 3 bit left
 *		shift) with some small savings.
 *
 *		If not, it will become evident in the statistics. Changing
 *		to the usual prime number length hash table will require
 *		changing the H5C2__HASH_FCN macro and the deletion of the
 *		H5C2__HASH_MASK #define.  No other changes should be required.
 *
 *
 * When we flush the cache, we need to write entries out in increasing
 * address order.  An instance of a skip list is used to store dirty entries in
 * sorted order.  Whether it is cheaper to sort the dirty entries as needed,
 * or to maintain the list is an open question.  At a guess, it depends
 * on how frequently the cache is flushed.  We will see how it goes.
 *
 * For now at least, I will not remove dirty entries from the list as they
 * are flushed. (this has been changed -- dirty entries are now removed from
 * the skip list as they are flushed.  JRM - 10/25/05)
 *
 * slist_len:   Number of entries currently in the skip list
 *              used to maintain a sorted list of dirty entries in the
 *              cache.
 *
 * slist_size:  Number of bytes of cache entries currently stored in the
 *              skip list used to maintain a sorted list of
 *              dirty entries in the cache.
 *
 * slist_ptr:   pointer to the instance of H5SL_t used maintain a sorted
 *              list of dirty entries in the cache.  This sorted list has
 *              two uses:
 *
 *              a) It allows us to flush dirty entries in increasing address
 *                 order, which results in significant savings.
 *
 *              b) It facilitates checking for adjacent dirty entries when
 *                 attempting to evict entries from the cache.  While we
 *                 don't use this at present, I hope that this will allow
 *                 some optimizations when I get to it.
 *
 * With the addition of the fractal heap, the cache must now deal with
 * the case in which entries may be dirtied, renamed, or have their sizes
 * changed during a flush.  To allow sanity checks in this situation, the
 * following two fields have been added.  They are only compiled in when
 * H5C2_DO_SANITY_CHECKS is TRUE.
 *
 * slist_len_increase: Number of entries that have been added to the 
 * 		slist since the last time this field was set to zero.
 *
 * slist_size_increase: Total size of all entries that have been added
 * 		to the slist since the last time this field was set to
 * 		zero.
 *
 *
 * When a cache entry is protected, it must be removed from the LRU
 * list(s) as it cannot be either flushed or evicted until it is unprotected.
 * The following fields are used to implement the protected list (pl).
 *
 * pl_len:      Number of entries currently residing on the protected list.
 *
 * pl_size:     Number of bytes of cache entries currently residing on the
 *              protected list.
 *
 * pl_head_ptr: Pointer to the head of the doubly linked list of protected
 *              entries.  Note that cache entries on this list are linked
 *              by their next and prev fields.
 *
 *              This field is NULL if the list is empty.
 *
 * pl_tail_ptr: Pointer to the tail of the doubly linked list of protected
 *              entries.  Note that cache entries on this list are linked
 *              by their next and prev fields.
 *
 *              This field is NULL if the list is empty.
 *
 *
 * For very frequently used entries, the protect/unprotect overhead can
 * become burdensome.  To avoid this overhead, I have modified the cache
 * to allow entries to be "pinned".  A pinned entry is similar to a
 * protected entry, in the sense that it cannot be evicted, and that
 * the entry can be modified at any time.
 *
 * Pinning an entry has the following implications:
 *
 *	1) A pinned entry cannot be evicted.  Thus unprotected
 *         pinned entries reside in the pinned entry list, instead
 *         of the LRU list(s) (or other lists maintained by the current
 *         replacement policy code).
 *
 *      2) A pinned entry can be accessed or modified at any time.
 *         Therefore, the cache must check with the entry owner
 *         before flushing it.  If permission is denied, the
 *         cache just skips the entry in the flush.
 *
 *      3) A pinned entry can be marked as dirty (and possibly
 *         change size) while it is unprotected.
 *
 *      4) The flush-destroy code must allow pinned entries to
 *         be unpinned (and possibly unprotected) during the
 *         flush.
 *
 * Since pinned entries cannot be evicted, they must be kept on a pinned
 * entry list, instead of being entrusted to the replacement policy code.
 *
 * Maintaining the pinned entry list requires the following fields:
 *
 * pel_len:	Number of entries currently residing on the pinned
 * 		entry list.
 *
 * pel_size:	Number of bytes of cache entries currently residing on
 * 		the pinned entry list.
 *
 * pel_head_ptr: Pointer to the head of the doubly linked list of pinned
 * 		but not protected entries.  Note that cache entries on
 * 		this list are linked by their next and prev fields.
 *
 *              This field is NULL if the list is empty.
 *
 * pel_tail_ptr: Pointer to the tail of the doubly linked list of pinned
 * 		but not protected entries.  Note that cache entries on
 * 		this list are linked by their next and prev fields.
 *
 *              This field is NULL if the list is empty.
 *
 *
 * The cache must have a replacement policy, and the fields supporting this
 * policy must be accessible from this structure.
 *
 * While there has been interest in several replacement policies for
 * this cache, the initial development schedule is tight.  Thus I have
 * elected to support only a modified LRU policy for the first cut.
 *
 * To further simplify matters, I have simply included the fields needed
 * by the modified LRU in this structure.  When and if we add support for
 * other policies, it will probably be easiest to just add the necessary
 * fields to this structure as well -- we only create one instance of this
 * structure per file, so the overhead is not excessive.
 *
 *
 * Fields supporting the modified LRU policy:
 *
 * See most any OS text for a discussion of the LRU replacement policy.
 *
 * When operating in parallel mode, we must ensure that a read does not
 * cause a write.  If it does, the process will hang, as the write will
 * be collective and the other processes will not know to participate.
 *
 * To deal with this issue, I have modified the usual LRU policy by adding
 * clean and dirty LRU lists to the usual LRU list.
 *
 * The clean LRU list is simply the regular LRU list with all dirty cache
 * entries removed.
 *
 * Similarly, the dirty LRU list is the regular LRU list with all the clean
 * cache entries removed.
 *
 * When reading in parallel mode, we evict from the clean LRU list only.
 * This implies that we must try to ensure that the clean LRU list is
 * reasonably well stocked at all times.
 *
 * We attempt to do this by trying to flush enough entries on each write
 * to keep the cLRU_list_size >= min_clean_size.
 *
 * Even if we start with a completely clean cache, a sequence of protects
 * without unprotects can empty the clean LRU list.  In this case, the
 * cache must grow temporarily.  At the next write, we will attempt to
 * evict enough entries to reduce index_size to less than max_cache_size.
 * While this will usually be possible, all bets are off if enough entries
 * are protected.
 *
 * Discussions of the individual fields used by the modified LRU replacement
 * policy follow:
 *
 * LRU_list_len:  Number of cache entries currently on the LRU list.
 *
 *              Observe that LRU_list_len + pl_len must always equal
 *              index_len.
 *
 * LRU_list_size:  Number of bytes of cache entries currently residing on the
 *              LRU list.
 *
 *              Observe that LRU_list_size + pl_size must always equal
 *              index_size.
 *
 * LRU_head_ptr:  Pointer to the head of the doubly linked LRU list.  Cache
 *              entries on this list are linked by their next and prev fields.
 *
 *              This field is NULL if the list is empty.
 *
 * LRU_tail_ptr:  Pointer to the tail of the doubly linked LRU list.  Cache
 *              entries on this list are linked by their next and prev fields.
 *
 *              This field is NULL if the list is empty.
 *
 * cLRU_list_len: Number of cache entries currently on the clean LRU list.
 *
 *              Observe that cLRU_list_len + dLRU_list_len must always
 *              equal LRU_list_len.
 *
 * cLRU_list_size:  Number of bytes of cache entries currently residing on
 *              the clean LRU list.
 *
 *              Observe that cLRU_list_size + dLRU_list_size must always
 *              equal LRU_list_size.
 *
 * cLRU_head_ptr:  Pointer to the head of the doubly linked clean LRU list.
 *              Cache entries on this list are linked by their aux_next and
 *              aux_prev fields.
 *
 *              This field is NULL if the list is empty.
 *
 * cLRU_tail_ptr:  Pointer to the tail of the doubly linked clean LRU list.
 *              Cache entries on this list are linked by their aux_next and
 *              aux_prev fields.
 *
 *              This field is NULL if the list is empty.
 *
 * dLRU_list_len: Number of cache entries currently on the dirty LRU list.
 *
 *              Observe that cLRU_list_len + dLRU_list_len must always
 *              equal LRU_list_len.
 *
 * dLRU_list_size:  Number of cache entries currently on the dirty LRU list.
 *
 *              Observe that cLRU_list_len + dLRU_list_len must always
 *              equal LRU_list_len.
 *
 * dLRU_head_ptr:  Pointer to the head of the doubly linked dirty LRU list.
 *              Cache entries on this list are linked by their aux_next and
 *              aux_prev fields.
 *
 *              This field is NULL if the list is empty.
 *
 * dLRU_tail_ptr:  Pointer to the tail of the doubly linked dirty LRU list.
 *              Cache entries on this list are linked by their aux_next and
 *              aux_prev fields.
 *
 *              This field is NULL if the list is empty.
 *
 *
 * Automatic cache size adjustment:
 *
 * While the default cache size is adequate for most cases, we can run into
 * cases where the default is too small.  Ideally, we will let the user
 * adjust the cache size as required.  However, this is not possible in all
 * cases.  Thus I have added automatic cache size adjustment code.
 *
 * The configuration for the automatic cache size adjustment is stored in
 * the structure described below:
 *
 * size_increase_possible:  Depending on the configuration data given
 *		in the resize_ctl field, it may or may not be possible
 *		to increase the size of the cache.  Rather than test for
 *		all the ways this can happen, we simply set this flag when
 *		we receive a new configuration.
 *
 * flash_size_increase_possible: Depending on the configuration data given
 *              in the resize_ctl field, it may or may not be possible
 *              for a flash size increase to occur.  We set this flag
 *              whenever we receive a new configuration so as to avoid
 *              repeated calculations.
 *
 * flash_size_increase_threshold: If a flash cache size increase is possible,
 *              this field is used to store the minimum size of a new entry
 *              or size increase needed to trigger a flash cache size
 *              increase.  Note that this field must be updated whenever
 *              the size of the cache is changed.
 *
 * size_decrease_possible:  Depending on the configuration data given
 *              in the resize_ctl field, it may or may not be possible
 *              to decrease the size of the cache.  Rather than test for
 *              all the ways this can happen, we simply set this flag when
 *              we receive a new configuration.
 *
 * cache_full:	Boolean flag used to keep track of whether the cache is
 *		full, so we can refrain from increasing the size of a
 *		cache which hasn't used up the space alotted to it.
 *
 *		The field is initialized to FALSE, and then set to TRUE
 *		whenever we attempt to make space in the cache.
 *
 * resize_enabled:  This is another convenience flag which is set whenever
 *		a new set of values for resize_ctl are provided.  Very
 *		simply,
 *
 *		    resize_enabled = size_increase_possible ||
 *                                   size_decrease_possible;
 *
 * size_decreased:  Boolean flag set to TRUE whenever the maximun cache
 *		size is decreased.  The flag triggers a call to
 *		H5C2_make_space_in_cache() on the next call to H5C2_protect().
 *
 * resize_ctl:	Instance of H5C2_auto_size_ctl_t containing configuration
 * 		data for automatic cache resizing.
 *
 * epoch_markers_active:  Integer field containing the number of epoch
 *		markers currently in use in the LRU list.  This value
 *		must be in the range [0, H5C2__MAX_EPOCH_MARKERS - 1].
 *
 * epoch_marker_active:  Array of boolean of length H5C2__MAX_EPOCH_MARKERS.
 *		This array is used to track which epoch markers are currently
 *		in use.
 *
 * epoch_marker_ringbuf:  Array of int of length H5C2__MAX_EPOCH_MARKERS + 1.
 *
 *		To manage the epoch marker cache entries, it is necessary
 *		to track their order in the LRU list.  This is done with
 *		epoch_marker_ringbuf.  When markers are inserted at the
 *		head of the LRU list, the index of the marker in the
 *		epoch_markers array is inserted at the tail of the ring
 *		buffer.  When it becomes the epoch_marker_active'th marker
 *		in the LRU list, it will have worked its way to the head
 *		of the ring buffer as well.  This allows us to remove it
 *		without scanning the LRU list if such is required.
 *
 * epoch_marker_ringbuf_first: Integer field containing the index of the
 *		first entry in the ring buffer.
 *
 * epoch_marker_ringbuf_last: Integer field containing the index of the
 *		last entry in the ring buffer.
 *
 * epoch_marker_ringbuf_size: Integer field containing the number of entries
 *		in the ring buffer.
 *
 * epoch_markers:  Array of instances of H5C2_cache_entry_t of length
 *		H5C2__MAX_EPOCH_MARKERS.  The entries are used as markers
 *		in the LRU list to identify cache entries that haven't
 *		been accessed for some (small) specified number of
 *		epochs.  These entries (if any) can then be evicted and
 *		the cache size reduced -- ideally without evicting any
 *		of the current working set.  Needless to say, the epoch
 *		length and the number of epochs before an unused entry
 *		must be chosen so that all, or almost all, the working
 *		set will be accessed before the limit.
 *
 *		Epoch markers only appear in the LRU list, never in
 *		the index or slist.  While they are of type
 *		H5C2__EPOCH_MARKER_TYPE, and have associated class
 *		functions, these functions should never be called.
 *
 *		The addr fields of these instances of H5C2_cache_entry_t
 *		are set to the index of the instance in the epoch_markers
 *		array, the size is set to 0, and the type field points
 *		to the constant structure epoch_marker_class defined
 *		in H5C2.c.  The next and prev fields are used as usual
 *		to link the entry into the LRU list.
 *
 *		All other fields are unused.
 *
 *
 * Cache hit rate collection fields:
 *
 * We supply the current cache hit rate on request, so we must keep a
 * simple cache hit rate computation regardless of whether statistics
 * collection is enabled.  The following fields support this capability.
 *
 * cache_hits: Number of cache hits since the last time the cache hit
 *	rate statistics were reset.  Note that when automatic cache
 *	re-sizing is enabled, this field will be reset every automatic
 *	resize epoch.
 *
 * cache_accesses: Number of times the cache has been accessed while
 *	since the last since the last time the cache hit rate statistics
 *	were reset.  Note that when automatic cache re-sizing is enabled,
 *	this field will be reset every automatic resize epoch.
 *
 *
 * Metadata journaling fields:
 *
 * The following fields are used to support metadata journaling.  The 
 * objective here is to journal all changes in metadata, so that we will
 * be able to re-construct a HDF5 file with a consistent set of metadata
 * in the event of a crash.
 *
 * mdj_enabled: Boolean flag used to indicate whether journaling is 
 * 		currently enabled.  In general, the values of the 
 * 		remaining fields in this section are undefined if 
 * 		mdj_enabled is FALSE.
 *
 * trans_in_progress Boolean flag used to indicate whether a metadata
 * 		transaction is in progress.  
 *
 * 		For purposes of metadata journaling, a transaction is a 
 * 		sequence of operations on metadata selected such that 
 * 		the HDF5 file metadata is in a consistent state both at 
 * 		the beginning and at the end of the sequence.  
 *
 * 		At least to begin with, transactions will be closely tied
 * 		to user level API calls.
 *
 * trans_api_name: Array of char of length H5C2__MAX_API_NAME_LEN + 1. Used 
 * 		to store the name of the API call associated with the 
 * 		current transaction.
 *
 * trans_num:	uint64_t containing the id assigned to the current 
 * 		transaction (if trans_in_progress is TRUE), or of the 
 * 		last transaction completed (if trans_in_progress is FALSE),
 * 		or zero if no transaction has been initiated yet.
 *
 * last_trans_on_disk:  uint64_t containing the id assigned to the 
 * 		last transaction all of whose associated journal entries
 * 		are on disk in the journal file.  
 *
 * 		We must track this value, as to avoid messages from the
 * 		future, we must not write a cache entry to file until
 * 		the journal entries of all transactions in which it has
 * 		been modified have been written to disk in the journal
 * 		file.
 *
 * mdj_file_name_ptr:  Pointer to a string containing the path of the 
 * 		journal file, or NULL if this path is undefined.
 * 		At present, the journal will always be stored in an 
 * 		external file, so this field must be defined if 
 * 		journaling is enabled.
 *
 * 		To avoid allocating extra memory, mdj_file_name_ptr
 * 		points into the approprite location in the in core
 * 		image of the metadata journaling configuration block.
 *
 * mdj_conf_block_addr:  Address of the metadata journaling configuration
 * 		block on disk, or HADDR_UNDEF if that block is undefined.
 *
 * mdj_conf_block_len: Length (in bytes) of the metadata journaling 
 * 		configuration block, or 0 if that block is undefined.
 *
 * mdj_conf_block_ptr: Pointer to a dynamically allocated chunk of 
 * 		memory of size mdj_conf_block_len used to construct 
 * 		an image of the on disk metadata journaling configuration
 * 		block.  This block is used to record the path of the 
 * 		journal file in the HDF5 file, so that it can be 
 * 		found in the event of a crash.
 *
 * 		The metadata journaling configuration block has the 
 * 		following format:
 *
 * 		4 bytes		signature
 *
 * 		1 byte		version
 *
 * 		4 bytes		path length
 *
 * 		variable	string containing path of journal file
 *
 * 		4 bytes		checksum
 *
 * 		The base address and lenth of the metadata journaling 
 * 		configuration block is stored in the mdj_msg superblock
 * 		extension message.
 *
 * mdj_jbrb:    Instance of H5C2_jbrb_t used to manage logging of journal
 * 		entries to the journal file.
 *
 * While a transaction is in progress, we must maintain a list of the 
 * entries that have been modified during the transaction so we can
 * generate the appropriate journal entries.  The following fields are
 * used to maintain this list:
 *
 * tl_len:      Number of entries currently residing on the transaction list.
 *
 * tl_size:     Number of bytes of cache entries currently residing on the
 *              transaction list.
 *
 * tl_head_ptr: Pointer to the head of the doubly linked list of entries
 * 		dirtied in the current transaction.  Note that cache entries 
 * 		on this list are linked by their trans_next and trans_prev 
 * 		fields.
 *
 *              This field is NULL if the list is empty.
 *
 * tl_tail_ptr: Pointer to the tail of the doubly linked list of entries
 *              dirtied in the current transaction.  Note that cache entries 
 *              on this list are linked by their trans_next and trans_prev 
 *              fields.
 *
 *              This field is NULL if the list is empty.
 *
 * When an entry is dirtied in a transaction, we must not flush it until 
 * all the journal entries generated by the transaction have reached disk 
 * in the journal file.
 *
 * We could just leave these entries in the LRU and skip over them when
 * we scan the list for candidates for eviction.  However, this will be 
 * costly, so we store them on the journal write in progress list instead
 * until all the journal entries for the specified transaction reaches 
 * disk.
 *
 * jwipl_len:	Number of entries currently residing on the journal 
 * 		entry write in progress list.
 *
 * jwipl_size:  Number of bytes of cache entries currently residing on the
 *              journal entry write in progress list.
 *
 * jwipl_head_ptr:  Pointer to the head of the doubly linked list of entries
 * 		dirtied in some transaction n, where at least some of the 
 * 		journal entries generated in transaction n have not yet
 * 		made it to disk in the journal file.
 *
 * 		Entries on this list are linked by their next and prev 
 * 		fields.
 *
 *              This field is NULL if the list is empty.
 *
 * jwipl_tail_ptr:  Pointer to the tail of the doubly linked list of entries
 * 		dirtied in some transaction n, where at least some of the 
 * 		journal entries generated in transaction n have not yet
 * 		made it to disk in the journal file.
 *
 * 		Entries on this list are linked by their next and prev 
 * 		fields.
 *
 *              This field is NULL if the list is empty.
 *
 * It is necessary to turn off some optimization while journaling is 
 * in progress, so as to avoid generating dirty metadata during a flush.
 * The following fields are used to maintain a list of functions to be
 * called when journaling is enabled or disabled.  Note that the metadata
 * cache must be clean when these function are called.
 *
 * The metadata journaling status change callback table is initaly allocated
 * with H5C2__MIN_MDJSC_CB_TBL_LEN entries.  The table size is doubled
 * whenever an entry is added to a full table, and halved whenever the 
 * active entries to total entries ratio drops below 
 * H5C2__MDJSC_CB_TBL_MIN_ACTIVE_RATIO and the upper half of the table is 
 * empty (Since entries are removed from the table by specifying the 
 * index of the entry, we can't compress the table).
 *
 * mdjsc_cb_tbl: Base address of a dynamically allocated array of instances
 * 		of H5C2_mdjsc_record_t used to record an arbitrarily long 
 * 		list of functions to call whenever journaling is enabled or 
 * 		disabled.
 *
 * mdjsc_cb_tbl_len: Number of entries currently allocated in *mdjsc_cb_tbl.
 *
 * num_mdjsc_cbs: Number of callbacks currently registered in the metadata
 * 		journaling status change callback table.
 *
 * mdjsc_cb_tbl_fl_head:  Index of the first free entry in the mdjsc_cb_tbl,
 * 		or -1 if the table is full.
 *
 * mdjsc_cb_tbl_max_idx_in_use: Maximum of the indicies of metadata journaling
 * 		status change callback table entries in use, or -1 if the 
 * 		table is empty;
 *
 * Statistics collection fields:
 *
 * When enabled, these fields are used to collect statistics as described
 * below.  The first set are collected only when H5C2_COLLECT_CACHE_STATS
 * is true.
 *
 * hits:        Array of int64 of length H5C2__MAX_NUM_TYPE_IDS + 1.  The cells
 *		are used to record the number of times an entry with type id
 *		equal to the array index has been in cache when requested in
 *		the current epoch.
 *
 * misses:      Array of int64 of length H5C2__MAX_NUM_TYPE_IDS + 1.  The cells
 *		are used to record the number of times an entry with type id
 *		equal to the array index has not been in cache when
 *		requested in the current epoch.
 *
 * write_protects:  Array of int64 of length H5C2__MAX_NUM_TYPE_IDS + 1.  The 
 * 		cells are used to record the number of times an entry with 
 * 		type id equal to the array index has been write protected 
 * 		in the current epoch.
 *
 * 		Observe that (hits + misses) = (write_protects + read_protects).
 *
 * read_protects: Array of int64 of length H5C2__MAX_NUM_TYPE_IDS + 1.  The 
 * 		cells are used to record the number of times an entry with 
 * 		type id equal to the array index has been read protected in 
 * 		the current epoch.
 *
 *              Observe that (hits + misses) = (write_protects + read_protects).
 *
 * max_read_protects:  Array of int32 of length H5C2__MAX_NUM_TYPE_IDS + 1. 
 * 		The cells are used to maximum number of simultaneous read 
 * 		protects on any entry with type id equal to the array index 
 * 		in the current epoch.
 *
 * insertions:  Array of int64 of length H5C2__MAX_NUM_TYPE_IDS + 1.  The cells
 *		are used to record the number of times an entry with type
 *		id equal to the array index has been inserted into the
 *		cache in the current epoch.
 *
 * pinned_insertions:  Array of int64 of length H5C2__MAX_NUM_TYPE_IDS + 1.  
 * 		The cells are used to record the number of times an entry 
 * 		with type id equal to the array index has been inserted 
 * 		pinned into the cache in the current epoch.
 *
 * clears:      Array of int64 of length H5C2__MAX_NUM_TYPE_IDS + 1.  The cells
 *		are used to record the number of times an entry with type
 *		id equal to the array index has been cleared in the current
 *		epoch.
 *
 * flushes:     Array of int64 of length H5C2__MAX_NUM_TYPE_IDS + 1.  The cells
 *		are used to record the number of times an entry with type id
 *		equal to the array index has been written to disk in the
 *              current epoch.
 *
 * evictions:   Array of int64 of length H5C2__MAX_NUM_TYPE_IDS + 1.  The cells
 *		are used to record the number of times an entry with type id
 *		equal to the array index has been evicted from the cache in
 *		the current epoch.
 *
 * renames:     Array of int64 of length H5C2__MAX_NUM_TYPE_IDS + 1.  The cells
 *		are used to record the number of times an entry with type
 *		id equal to the array index has been renamed in the current
 *		epoch.
 *
 * entry_flush_renames: Array of int64 of length H5C2__MAX_NUM_TYPE_IDS + 1.  
 * 		The cells are used to record the number of times an entry 
 * 		with type id equal to the array index has been renamed
 * 		during its flush callback in the current epoch.
 *
 * cache_flush_renames: Array of int64 of length H5C2__MAX_NUM_TYPE_IDS + 1.  
 * 		The cells are used to record the number of times an entry 
 * 		with type id equal to the array index has been renamed
 * 		during a cache flush in the current epoch.
 *
 * pins:        Array of int64 of length H5C2__MAX_NUM_TYPE_IDS + 1.  The cells
 *		are used to record the number of times an entry with type
 *		id equal to the array index has been pinned in the current
 *		epoch.
 *
 * unpins:      Array of int64 of length H5C2__MAX_NUM_TYPE_IDS + 1.  The cells
 *		are used to record the number of times an entry with type
 *		id equal to the array index has been unpinned in the current
 *		epoch.
 *
 * dirty_pins:	Array of int64 of length H5C2__MAX_NUM_TYPE_IDS + 1.  The cells
 *		are used to record the number of times an entry with type
 *		id equal to the array index has been marked dirty while pinned
 *		in the current epoch.
 *
 * pinned_flushes:  Array of int64 of length H5C2__MAX_NUM_TYPE_IDS + 1.  The
 * 		cells are used to record the number of times an  entry
 * 		with type id equal to the array index has been flushed while
 * 		pinned in the current epoch.
 *
 * pinned_cleared:  Array of int64 of length H5C2__MAX_NUM_TYPE_IDS + 1.  The
 * 		cells are used to record the number of times an  entry
 * 		with type id equal to the array index has been cleared while
 * 		pinned in the current epoch.
 *
 * size_increases:  Array of int64 of length H5C2__MAX_NUM_TYPE_IDS + 1.
 *		The cells are used to record the number of times an entry
 *		with type id equal to the array index has increased in
 *		size in the current epoch.
 *
 * size_decreases:  Array of int64 of length H5C2__MAX_NUM_TYPE_IDS + 1.
 *		The cells are used to record the number of times an entry
 *		with type id equal to the array index has decreased in
 *		size in the current epoch.
 *
 * entry_flush_size_changes:  Array of int64 of length 
 * 		H5C2__MAX_NUM_TYPE_IDS + 1.  The cells are used to record 
 * 		the number of times an entry with type id equal to the 
 * 		array index has changed size while in its flush callback.
 *
 * cache_flush_size_changes:  Array of int64 of length 
 * 		H5C2__MAX_NUM_TYPE_IDS + 1.  The cells are used to record 
 * 		the number of times an entry with type id equal to the 
 * 		array index has changed size during a cache flush
 *
 * total_ht_insertions: Number of times entries have been inserted into the
 *		hash table in the current epoch.
 *
 * total_ht_deletions: Number of times entries have been deleted from the
 *              hash table in the current epoch.
 *
 * successful_ht_searches: int64 containing the total number of successful
 *		searches of the hash table in the current epoch.
 *
 * total_successful_ht_search_depth: int64 containing the total number of
 *		entries other than the targets examined in successful
 *		searches of the hash table in the current epoch.
 *
 * failed_ht_searches: int64 containing the total number of unsuccessful
 *              searches of the hash table in the current epoch.
 *
 * total_failed_ht_search_depth: int64 containing the total number of
 *              entries examined in unsuccessful searches of the hash
 *		table in the current epoch.
 *
 * max_index_len:  Largest value attained by the index_len field in the
 *              current epoch.
 *
 * max_index_size:  Largest value attained by the index_size field in the
 *              current epoch.
 *
 * max_slist_len:  Largest value attained by the slist_len field in the
 *              current epoch.
 *
 * max_slist_size:  Largest value attained by the slist_size field in the
 *              current epoch.
 *
 * max_pl_len:  Largest value attained by the pl_len field in the
 *              current epoch.
 *
 * max_pl_size: Largest value attained by the pl_size field in the
 *              current epoch.
 *
 * max_pel_len: Largest value attained by the pel_len field in the
 *              current epoch.
 *
 * max_pel_size: Largest value attained by the pel_size field in the
 *              current epoch.
 *
 * The remaining stats are collected only when both H5C2_COLLECT_CACHE_STATS
 * and H5C2_COLLECT_CACHE_ENTRY_STATS are true.
 *
 * max_accesses: Array of int32 of length H5C2__MAX_NUM_TYPE_IDS + 1.  The cells
 *		are used to record the maximum number of times any single
 *		entry with type id equal to the array index has been
 *		accessed in the current epoch.
 *
 * min_accesses: Array of int32 of length H5C2__MAX_NUM_TYPE_IDS + 1.  The cells
 *		are used to record the minimum number of times any single
 *		entry with type id equal to the array index has been
 *		accessed in the current epoch.
 *
 * max_clears:  Array of int32 of length H5C2__MAX_NUM_TYPE_IDS + 1.  The cells
 *		are used to record the maximum number of times any single
 *		entry with type id equal to the array index has been cleared
 *		in the current epoch.
 *
 * max_flushes: Array of int32 of length H5C2__MAX_NUM_TYPE_IDS + 1.  The cells
 *		are used to record the maximum number of times any single
 *		entry with type id equal to the array index has been
 *		flushed in the current epoch.
 *
 * max_size:	Array of size_t of length H5C2__MAX_NUM_TYPE_IDS + 1.  The cells
 *              are used to record the maximum size of any single entry
 *		with type id equal to the array index that has resided in
 *		the cache in the current epoch.
 *
 * max_pins:	Array of size_t of length H5C2__MAX_NUM_TYPE_IDS + 1.  The cells
 *              are used to record the maximum number of times that any single
 *              entry with type id equal to the array index that has been
 *              marked as pinned in the cache in the current epoch.
 *
 *
 * Fields supporting testing:
 *
 * For test purposes, it is useful to turn off some asserts and sanity
 * checks.  The following flags support this.
 *
 * skip_file_checks:  Boolean flag used to skip sanity checks on file
 *		parameters passed to the cache.  In the test bed, there
 *		is no reason to have a file open, as the cache proper
 *		just passes these parameters through without using them.
 *
 *		When this flag is set, all sanity checks on the file
 *		parameters are skipped.  The field defaults to FALSE.
 *
 * skip_dxpl_id_checks:  Boolean flag used to skip sanity checks on the
 *		dxpl_id parameters passed to the cache.  These are not
 *		used directly by the cache, so skipping the checks
 *		simplifies the test bed.
 *
 *		When this flag is set, all sanity checks on the dxpl_id
 *		parameters are skipped.  The field defaults to FALSE.
 *
 * prefix	Array of char used to prefix debugging output.  The
 *		field is intended to allow marking of output of with
 *		the processes mpi rank.
 *
 ****************************************************************************/

#define H5C2__HASH_TABLE_LEN     (64 * 1024) /* must be a power of 2 */

#define H5C2__H5C2_T_MAGIC		0x005CAC0F
#define H5C2__MAX_NUM_TYPE_IDS		18
#define H5C2__PREFIX_LEN		32
#define H5C2__MAX_API_NAME_LEN		128

#define H5C2__JOURNAL_MAGIC_LEN		(size_t)4
#define H5C2__JOURNAL_CONF_MAGIC	"MDJC"
#define H5C2__JOURNAL_CONF_VERSION	((uint8_t)1)
#define H5C2__JOURNAL_BLOCK_LEN(pathLen, f)	\
	( H5C2__JOURNAL_MAGIC_LEN +		\
	  1 + /* version */			\
          H5F_SIZEOF_SIZE(f) +			\
	  ((pathLen) + 1) +             	\
	  4 /* checksum */ )

#define H5C2__MIN_MDJSC_CB_TBL_LEN		16
#define H5C2__MDJSC_CB_TBL_MIN_ACTIVE_RATIO	0.25

struct H5C2_t
{
    uint32_t			magic;

    hbool_t			flush_in_progress;

    FILE *			trace_file_ptr;

    void *			aux_ptr;

    int32_t			max_type_id;
    const char *                (* type_name_table_ptr);

    size_t                      max_cache_size;
    size_t                      min_clean_size;

    H5C2_write_permitted_func_t	check_write_permitted;
    hbool_t			write_permitted;

    H5C2_log_flush_func_t	log_flush;

    hbool_t			evictions_enabled;

    int32_t                     index_len;
    size_t                      index_size;
    H5C2_cache_entry_t *	(index[H5C2__HASH_TABLE_LEN]);


    int32_t                     slist_len;
    size_t                      slist_size;
    H5SL_t *                    slist_ptr;
#if H5C2_DO_SANITY_CHECKS
    int64_t			slist_len_increase;
    int64_t			slist_size_increase;
#endif /* H5C2_DO_SANITY_CHECKS */

    int32_t                     pl_len;
    size_t                      pl_size;
    H5C2_cache_entry_t *	pl_head_ptr;
    H5C2_cache_entry_t *  	pl_tail_ptr;

    int32_t                     pel_len;
    size_t                      pel_size;
    H5C2_cache_entry_t *	        pel_head_ptr;
    H5C2_cache_entry_t *  	pel_tail_ptr;

    int32_t                     LRU_list_len;
    size_t                      LRU_list_size;
    H5C2_cache_entry_t *	LRU_head_ptr;
    H5C2_cache_entry_t *	LRU_tail_ptr;

    int32_t                     cLRU_list_len;
    size_t                      cLRU_list_size;
    H5C2_cache_entry_t *	cLRU_head_ptr;
    H5C2_cache_entry_t *	cLRU_tail_ptr;

    int32_t                     dLRU_list_len;
    size_t                      dLRU_list_size;
    H5C2_cache_entry_t *	dLRU_head_ptr;
    H5C2_cache_entry_t *	dLRU_tail_ptr;

    hbool_t			size_increase_possible;
    hbool_t                     flash_size_increase_possible;
    size_t                      flash_size_increase_threshold;
    hbool_t			size_decrease_possible;
    hbool_t			resize_enabled;
    hbool_t			cache_full;
    hbool_t			size_decreased;
    H5C2_auto_size_ctl_t	resize_ctl;

    int32_t			epoch_markers_active;
    hbool_t			epoch_marker_active[H5C2__MAX_EPOCH_MARKERS];
    int32_t			epoch_marker_ringbuf[H5C2__MAX_EPOCH_MARKERS+1];
    int32_t			epoch_marker_ringbuf_first;
    int32_t			epoch_marker_ringbuf_last;
    int32_t			epoch_marker_ringbuf_size;
    H5C2_cache_entry_t		epoch_markers[H5C2__MAX_EPOCH_MARKERS];

    int64_t			cache_hits;
    int64_t			cache_accesses;
 
    hbool_t			mdj_enabled;
    hbool_t			trans_in_progress;
    char			trans_api_name[H5C2__MAX_API_NAME_LEN];
    uint64_t			trans_num;
    uint64_t			last_trans_on_disk;
    char *			mdj_file_name_ptr;
    haddr_t			mdj_conf_block_addr;
    hsize_t			mdj_conf_block_len;
    void *			mdj_conf_block_ptr;
    struct H5C2_jbrb_t		mdj_jbrb;
    int32_t			tl_len;
    size_t			tl_size;
    H5C2_cache_entry_t *	tl_head_ptr;
    H5C2_cache_entry_t *	tl_tail_ptr;
    int32_t			jwipl_len;
    size_t			jwipl_size;
    H5C2_cache_entry_t *	jwipl_head_ptr;
    H5C2_cache_entry_t *	jwipl_tail_ptr;
    H5C2_mdjsc_record_t *	mdjsc_cb_tbl;
    int32_t			mdjsc_cb_tbl_len;
    int32_t			num_mdjsc_cbs;
    int32_t			mdjsc_cb_tbl_fl_head;
    int32_t			mdjsc_cb_tbl_max_idx_in_use;
    
#if H5C2_COLLECT_CACHE_STATS

    /* stats fields */
    int64_t                     hits[H5C2__MAX_NUM_TYPE_IDS + 1];
    int64_t                     misses[H5C2__MAX_NUM_TYPE_IDS + 1];
    int64_t                     write_protects[H5C2__MAX_NUM_TYPE_IDS + 1];
    int64_t                     read_protects[H5C2__MAX_NUM_TYPE_IDS + 1];
    int32_t                     max_read_protects[H5C2__MAX_NUM_TYPE_IDS + 1];
    int64_t                     insertions[H5C2__MAX_NUM_TYPE_IDS + 1];
    int64_t                     pinned_insertions[H5C2__MAX_NUM_TYPE_IDS + 1];
    int64_t                     clears[H5C2__MAX_NUM_TYPE_IDS + 1];
    int64_t                     flushes[H5C2__MAX_NUM_TYPE_IDS + 1];
    int64_t                     evictions[H5C2__MAX_NUM_TYPE_IDS + 1];
    int64_t                     renames[H5C2__MAX_NUM_TYPE_IDS + 1];
    int64_t                     entry_flush_renames[H5C2__MAX_NUM_TYPE_IDS + 1];
    int64_t                     cache_flush_renames[H5C2__MAX_NUM_TYPE_IDS + 1];
    int64_t                     pins[H5C2__MAX_NUM_TYPE_IDS + 1];
    int64_t                     unpins[H5C2__MAX_NUM_TYPE_IDS + 1];
    int64_t                     dirty_pins[H5C2__MAX_NUM_TYPE_IDS + 1];
    int64_t                     pinned_flushes[H5C2__MAX_NUM_TYPE_IDS + 1];
    int64_t                     pinned_clears[H5C2__MAX_NUM_TYPE_IDS + 1];
    int64_t                     size_increases[H5C2__MAX_NUM_TYPE_IDS + 1];
    int64_t                     size_decreases[H5C2__MAX_NUM_TYPE_IDS + 1];
    int64_t                     entry_flush_size_changes
	    				[H5C2__MAX_NUM_TYPE_IDS + 1];
    int64_t                     cache_flush_size_changes
	    				[H5C2__MAX_NUM_TYPE_IDS + 1];

    int64_t			total_ht_insertions;
    int64_t			total_ht_deletions;
    int64_t			successful_ht_searches;
    int64_t			total_successful_ht_search_depth;
    int64_t			failed_ht_searches;
    int64_t			total_failed_ht_search_depth;

    int32_t                     max_index_len;
    size_t                      max_index_size;

    int32_t                     max_slist_len;
    size_t                      max_slist_size;

    int32_t                     max_pl_len;
    size_t                      max_pl_size;

    int32_t                     max_pel_len;
    size_t                      max_pel_size;

#if H5C2_COLLECT_CACHE_ENTRY_STATS

    int32_t                     max_accesses[H5C2__MAX_NUM_TYPE_IDS + 1];
    int32_t                     min_accesses[H5C2__MAX_NUM_TYPE_IDS + 1];
    int32_t                     max_clears[H5C2__MAX_NUM_TYPE_IDS + 1];
    int32_t                     max_flushes[H5C2__MAX_NUM_TYPE_IDS + 1];
    size_t                      max_size[H5C2__MAX_NUM_TYPE_IDS + 1];
    int32_t                     max_pins[H5C2__MAX_NUM_TYPE_IDS + 1];

#endif /* H5C2_COLLECT_CACHE_ENTRY_STATS */

#endif /* H5C2_COLLECT_CACHE_STATS */

    hbool_t			skip_file_checks;
    hbool_t			skip_dxpl_id_checks;
    char			prefix[H5C2__PREFIX_LEN];
};


/****************************************************************************/
/***************************** Macro Definitions ****************************/
/****************************************************************************/

/****************************************************************************
 *
 * We maintain doubly linked lists of instances of H5C2_cache_entry_t for a
 * variety of reasons -- protected list, LRU list, and the clean and dirty
 * LRU lists at present.  The following macros support linking and unlinking
 * of instances of H5C2_cache_entry_t by both their regular and auxilary next
 * and previous pointers.
 *
 * The size and length fields are also maintained.
 *
 * Note that the relevant pair of prev and next pointers are presumed to be
 * NULL on entry in the insertion macros.
 *
 * Finally, observe that the sanity checking macros evaluate to the empty
 * string when H5C2_DO_SANITY_CHECKS is FALSE.  They also contain calls
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
 *    from the H5C2__DLL_PRE_REMOVE_SC macro.  With the addition of the
 *    epoch markers used in the age out based cache size reduction algorithm,
 *    this invarient need not hold, as the epoch markers are of size 0.
 *
 *    One could argue that I should have given the epoch markers a positive
 *    size, but this would break the index_size = LRU_list_size + pl_size
 *    + pel_size invarient.
 *
 *    Alternatively, I could pass the current decr_mode in to the macro,
 *    and just skip the check whenever epoch markers may be in use.
 *
 *    However, any size errors should be caught when the cache is flushed
 *    and destroyed.  Until we are tracking such an error, this should be
 *    good enough.
 *                                                     JRM - 12/9/04
 *
 *
 *  - In the H5C2__DLL_PRE_INSERT_SC macro, replaced the lines:
 *
 *    ( ( (len) == 1 ) &&
 *      ( ( (head_ptr) != (tail_ptr) ) || ( (Size) <= 0 ) ||
 *        ( (head_ptr) == NULL ) || ( (head_ptr)->size != (Size) )
 *      )
 *    ) ||
 *
 *    with:
 *
 *    ( ( (len) == 1 ) &&
 *      ( ( (head_ptr) != (tail_ptr) ) ||
 *        ( (head_ptr) == NULL ) || ( (head_ptr)->size != (Size) )
 *      )
 *    ) ||
 *
 *    Epoch markers have size 0, so we can now have a non-empty list with
 *    zero size.  Hence the "( (Size) <= 0 )" clause cause false failures
 *    in the sanity check.  Since "Size" is typically a size_t, it can't
 *    take on negative values, and thus the revised clause "( (Size) < 0 )"
 *    caused compiler warnings.
 *                                                     JRM - 12/22/04
 *
 *  - In the H5C2__DLL_SC macro, replaced the lines:
 *
 *    ( ( (len) == 1 ) &&
 *      ( ( (head_ptr) != (tail_ptr) ) || ( (cache_ptr)->size <= 0 ) ||
 *        ( (head_ptr) == NULL ) || ( (head_ptr)->size != (Size) )
 *      )
 *    ) ||
 *
 *    with
 *
 *    ( ( (len) == 1 ) &&
 *      ( ( (head_ptr) != (tail_ptr) ) ||
 *        ( (head_ptr) == NULL ) || ( (head_ptr)->size != (Size) )
 *      )
 *    ) ||
 *
 *    Epoch markers have size 0, so we can now have a non-empty list with
 *    zero size.  Hence the "( (Size) <= 0 )" clause cause false failures
 *    in the sanity check.  Since "Size" is typically a size_t, it can't
 *    take on negative values, and thus the revised clause "( (Size) < 0 )"
 *    caused compiler warnings.
 *                                                     JRM - 1/10/05
 *
 *  - Added the H5C2__DLL_UPDATE_FOR_SIZE_CHANGE macro and the associated
 *    sanity checking macros.  These macro are used to update the size of
 *    a DLL when one of its entries changes size.
 *
 *							JRM - 9/8/05
 *
 *  - Added a set of macros supporting doubly linked lists using the new 
 *    trans_next and trans_prev fields in H5C2_cache_entry_t.  These 
 *    fields are used to maintain a list of entries that have been dirtied
 *    in the current transaction.  At the end of the transaction, this 
 *    list is used to generate the needed journal entries.
 *
 *    							JRM -- 3/27/08
 *
 ****************************************************************************/

#if H5C2_DO_SANITY_CHECKS

#define H5C2__DLL_PRE_REMOVE_SC(entry_ptr, head_ptr, tail_ptr, len, Size, fv) \
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

#define H5C2__DLL_SC(head_ptr, tail_ptr, len, Size, fv)                   \
if ( ( ( ( (head_ptr) == NULL ) || ( (tail_ptr) == NULL ) ) &&           \
       ( (head_ptr) != (tail_ptr) )                                      \
     ) ||                                                                \
     ( (len) < 0 ) ||                                                    \
     ( (Size) < 0 ) ||                                                   \
     ( ( (len) == 1 ) &&                                                 \
       ( ( (head_ptr) != (tail_ptr) ) ||                                 \
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

#define H5C2__DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size, fv) \
if ( ( (entry_ptr) == NULL ) ||                                              \
     ( (entry_ptr)->next != NULL ) ||                                        \
     ( (entry_ptr)->prev != NULL ) ||                                        \
     ( ( ( (head_ptr) == NULL ) || ( (tail_ptr) == NULL ) ) &&               \
       ( (head_ptr) != (tail_ptr) )                                          \
     ) ||                                                                    \
     ( (len) < 0 ) ||                                                        \
     ( ( (len) == 1 ) &&                                                     \
       ( ( (head_ptr) != (tail_ptr) ) ||                                     \
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

#define H5C2__DLL_PRE_SIZE_UPDATE_SC(dll_len, dll_size, old_size, new_size)    \
if ( ( (dll_len) <= 0 ) ||                                                    \
     ( (dll_size) <= 0 ) ||                                                   \
     ( (old_size) <= 0 ) ||                                                   \
     ( (old_size) > (dll_size) ) ||                                           \
     ( (new_size) <= 0 ) ||                                                   \
     ( ( (dll_len) == 1 ) && ( (old_size) != (dll_size) ) ) ) {               \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "DLL pre size update SC failed") \
}

#define H5C2__DLL_POST_SIZE_UPDATE_SC(dll_len, dll_size, old_size, new_size)    \
if ( ( (new_size) > (dll_size) ) ||                                            \
     ( ( (dll_len) == 1 ) && ( (new_size) != (dll_size) ) ) ) {                \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "DLL post size update SC failed") \
}

#else /* H5C2_DO_SANITY_CHECKS */

#define H5C2__DLL_PRE_REMOVE_SC(entry_ptr, head_ptr, tail_ptr, len, Size, fv)
#define H5C2__DLL_SC(head_ptr, tail_ptr, len, Size, fv)
#define H5C2__DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size, fv)
#define H5C2__DLL_PRE_SIZE_UPDATE_SC(dll_len, dll_size, old_size, new_size)
#define H5C2__DLL_POST_SIZE_UPDATE_SC(dll_len, dll_size, old_size, new_size)

#endif /* H5C2_DO_SANITY_CHECKS */


#define H5C2__DLL_APPEND(entry_ptr, head_ptr, tail_ptr, len, Size, fail_val) \
        H5C2__DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size,    \
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

#define H5C2__DLL_PREPEND(entry_ptr, head_ptr, tail_ptr, len, Size, fail_val) \
        H5C2__DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size,     \
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

#define H5C2__DLL_REMOVE(entry_ptr, head_ptr, tail_ptr, len, Size, fail_val) \
        H5C2__DLL_PRE_REMOVE_SC(entry_ptr, head_ptr, tail_ptr, len, Size,    \
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

#define H5C2__DLL_UPDATE_FOR_SIZE_CHANGE(dll_len, dll_size, old_size, new_size) \
        H5C2__DLL_PRE_SIZE_UPDATE_SC(dll_len, dll_size, old_size, new_size)     \
	(dll_size) -= (old_size);                                              \
	(dll_size) += (new_size);                                              \
        H5C2__DLL_POST_SIZE_UPDATE_SC(dll_len, dll_size, old_size, new_size)

#if H5C2_DO_SANITY_CHECKS

#define H5C2__AUX_DLL_PRE_REMOVE_SC(entry_ptr, hd_ptr, tail_ptr, len, Size, fv) \
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

#define H5C2__AUX_DLL_SC(head_ptr, tail_ptr, len, Size, fv)                 \
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

#define H5C2__AUX_DLL_PRE_INSERT_SC(entry_ptr, hd_ptr, tail_ptr, len, Size, fv)\
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

#else /* H5C2_DO_SANITY_CHECKS */

#define H5C2__AUX_DLL_PRE_REMOVE_SC(entry_ptr, hd_ptr, tail_ptr, len, Size, fv)
#define H5C2__AUX_DLL_SC(head_ptr, tail_ptr, len, Size, fv)
#define H5C2__AUX_DLL_PRE_INSERT_SC(entry_ptr, hd_ptr, tail_ptr, len, Size, fv)

#endif /* H5C2_DO_SANITY_CHECKS */


#define H5C2__AUX_DLL_APPEND(entry_ptr, head_ptr, tail_ptr, len, Size, fail_val)\
        H5C2__AUX_DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size,   \
                                   fail_val)                                    \
        if ( (head_ptr) == NULL )                                               \
        {                                                                       \
           (head_ptr) = (entry_ptr);                                            \
           (tail_ptr) = (entry_ptr);                                            \
        }                                                                       \
        else                                                                    \
        {                                                                       \
           (tail_ptr)->aux_next = (entry_ptr);                                  \
           (entry_ptr)->aux_prev = (tail_ptr);                                  \
           (tail_ptr) = (entry_ptr);                                            \
        }                                                                       \
        (len)++;                                                                \
        (Size) += entry_ptr->size;

#define H5C2__AUX_DLL_PREPEND(entry_ptr, head_ptr, tail_ptr, len, Size, fv)   \
        H5C2__AUX_DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size, \
                                   fv)                                        \
        if ( (head_ptr) == NULL )                                             \
        {                                                                     \
           (head_ptr) = (entry_ptr);                                          \
           (tail_ptr) = (entry_ptr);                                          \
        }                                                                     \
        else                                                                  \
        {                                                                     \
           (head_ptr)->aux_prev = (entry_ptr);                                \
           (entry_ptr)->aux_next = (head_ptr);                                \
           (head_ptr) = (entry_ptr);                                          \
        }                                                                     \
        (len)++;                                                              \
        (Size) += entry_ptr->size;

#define H5C2__AUX_DLL_REMOVE(entry_ptr, head_ptr, tail_ptr, len, Size, fv)    \
        H5C2__AUX_DLL_PRE_REMOVE_SC(entry_ptr, head_ptr, tail_ptr, len, Size, \
                                   fv)                                        \
        {                                                                     \
           if ( (head_ptr) == (entry_ptr) )                                   \
           {                                                                  \
              (head_ptr) = (entry_ptr)->aux_next;                             \
              if ( (head_ptr) != NULL )                                       \
              {                                                               \
                 (head_ptr)->aux_prev = NULL;                                 \
              }                                                               \
           }                                                                  \
           else                                                               \
           {                                                                  \
              (entry_ptr)->aux_prev->aux_next = (entry_ptr)->aux_next;        \
           }                                                                  \
           if ( (tail_ptr) == (entry_ptr) )                                   \
           {                                                                  \
              (tail_ptr) = (entry_ptr)->aux_prev;                             \
              if ( (tail_ptr) != NULL )                                       \
              {                                                               \
                 (tail_ptr)->aux_next = NULL;                                 \
              }                                                               \
           }                                                                  \
           else                                                               \
           {                                                                  \
              (entry_ptr)->aux_next->aux_prev = (entry_ptr)->aux_prev;        \
           }                                                                  \
           entry_ptr->aux_next = NULL;                                        \
           entry_ptr->aux_prev = NULL;                                        \
           (len)--;                                                           \
           (Size) -= entry_ptr->size;                                         \
        }

#if H5C2_DO_SANITY_CHECKS

#define H5C2__TRANS_DLL_PRE_REMOVE_SC(entry_ptr, hd_ptr, tail_ptr,             \
		                      len, Size, fv)                           \
if ( ( (hd_ptr) == NULL ) ||                                                   \
     ( (tail_ptr) == NULL ) ||                                                 \
     ( (entry_ptr) == NULL ) ||                                                \
     ( (len) <= 0 ) ||                                                         \
     ( (Size) < (entry_ptr)->size ) ||                                         \
     ( ( (Size) == (entry_ptr)->size ) && ( ! ( (len) == 1 ) ) ) ||            \
     ( ( (entry_ptr)->trans_prev == NULL ) && ( (hd_ptr) != (entry_ptr) ) ) || \
     ( ( (entry_ptr)->trans_next == NULL ) &&                                  \
       ( (tail_ptr) != (entry_ptr) ) ) ||                                      \
     ( ( (len) == 1 ) &&                                                       \
       ( ! ( ( (hd_ptr) == (entry_ptr) ) && ( (tail_ptr) == (entry_ptr) ) &&   \
             ( (entry_ptr)->trans_next == NULL ) &&                            \
             ( (entry_ptr)->trans_prev == NULL ) &&                            \
             ( (Size) == (entry_ptr)->size )                                   \
           )                                                                   \
       )                                                                       \
     )                                                                         \
   ) {                                                                         \
    HDfprintf(stdout, "%s: TRANS DLL pre remove SC failed.\n", FUNC); \
    /* JRM */ /* Debugging code -- remove eventually */                  \
    if ( (hd_ptr) == NULL ) HDfprintf(stdout, "( (hd_ptr) == NULL ).\n" );\
    if ( (tail_ptr) == NULL ) HDfprintf(stdout, "(tail_ptr) == NULL\n");\
    if ( (entry_ptr) == NULL ) HDfprintf(stdout, "(entry_ptr) == NULL\n");\
    if ( (len) <= 0 ) HDfprintf(stdout, "( (len) <= 0 )\n");\
    if ( (Size) < (entry_ptr)->size ) \
	HDfprintf(stdout,"( (Size) < (entry_ptr)->size )\n");\
    if ( ( (Size) == (entry_ptr)->size ) && ( ! ( (len) == 1 ) ) ) \
	HDfprintf(stdout, "(((Size)==(entry_ptr)->size)&&(!((len) == 1)))\n");\
    if ( ( (entry_ptr)->trans_prev == NULL ) && ( (hd_ptr) != (entry_ptr) ) ) \
	HDfprintf(stdout,"(((entry_ptr)->trans_prev==NULL)&&((hd_ptr)!=(entry_ptr)))\n");\
    if ( ((entry_ptr)->trans_next == NULL) && ((tail_ptr) != (entry_ptr)) ) \
	HDfprintf(stdout,"(((entry_ptr)->trans_next==NULL)&&((tail_ptr)!=(entry_ptr)))\n"); \
    if ( ( (len) == 1 ) &&  \
         ( ! ( ( (hd_ptr) == (entry_ptr) ) && ( (tail_ptr) == (entry_ptr) ) && \
              ( (entry_ptr)->trans_next == NULL ) && \
              ( (entry_ptr)->trans_prev == NULL ) && \
              ( (Size) == (entry_ptr)->size ) ) ) )  \
	HDfprintf(stdout,"long condition failed\n"); \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, (fv), "TRANS DLL pre remove SC failed") \
}

#define H5C2__TRANS_DLL_SC(head_ptr, tail_ptr, len, Size, fv)                 \
if ( ( ( ( (head_ptr) == NULL ) || ( (tail_ptr) == NULL ) ) &&                \
       ( (head_ptr) != (tail_ptr) )                                           \
     ) ||                                                                     \
     ( (len) < 0 ) ||                                                         \
     ( (Size) < 0 ) ||                                                        \
     ( ( (len) == 1 ) &&                                                      \
       ( ( (head_ptr) != (tail_ptr) ) || ( (Size) <= 0 ) ||                   \
         ( (head_ptr) == NULL ) || ( (head_ptr)->size != (Size) )             \
       )                                                                      \
     ) ||                                                                     \
     ( ( (len) >= 1 ) &&                                                      \
       ( ( (head_ptr) == NULL ) || ( (head_ptr)->trans_prev != NULL ) ||      \
         ( (tail_ptr) == NULL ) || ( (tail_ptr)->trans_next != NULL )         \
       )                                                                      \
     )                                                                        \
   ) {                                                                        \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, (fv), "TRANS DLL sanity check failed") \
}

#define H5C2__TRANS_DLL_PRE_INSERT_SC(entry_ptr, hd_ptr, tail_ptr,             \
		                      len, Size, fv)                           \
if ( ( (entry_ptr) == NULL ) ||                                                \
     ( ! ((entry_ptr)->is_dirty) ) ||                                          \
     ( (entry_ptr)->trans_next != NULL ) ||                                    \
     ( (entry_ptr)->trans_prev != NULL ) ||                                    \
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
       ( ( (hd_ptr) == NULL ) || ( (hd_ptr)->trans_prev != NULL ) ||           \
         ( (tail_ptr) == NULL ) || ( (tail_ptr)->trans_next != NULL )          \
       )                                                                       \
     )                                                                         \
   ) {                                                                         \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, (fv), "TRANS DLL pre insert SC failed") \
}

#else /* H5C2_DO_SANITY_CHECKS */

#define H5C2__TRANS_DLL_PRE_REMOVE_SC(entry_ptr, hd_ptr, tail_ptr, len, Size, fv)
#define H5C2__TRANS_DLL_SC(head_ptr, tail_ptr, len, Size, fv)
#define H5C2__TRANS_DLL_PRE_INSERT_SC(entry_ptr, hd_ptr, tail_ptr, len, Size, fv)

#endif /* H5C2_DO_SANITY_CHECKS */


#define H5C2__TRANS_DLL_APPEND(entry_ptr, head_ptr, tail_ptr, len,             \
		               Size, fail_val)                                 \
        H5C2__TRANS_DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len,      \
			              Size, fail_val)                          \
        if ( (head_ptr) == NULL )                                              \
        {                                                                      \
           (head_ptr) = (entry_ptr);                                           \
           (tail_ptr) = (entry_ptr);                                           \
        }                                                                      \
        else                                                                   \
        {                                                                      \
           (tail_ptr)->trans_next = (entry_ptr);                               \
           (entry_ptr)->trans_prev = (tail_ptr);                               \
           (tail_ptr) = (entry_ptr);                                           \
        }                                                                      \
        (len)++;                                                               \
        (Size) += entry_ptr->size;

#define H5C2__TRANS_DLL_PREPEND(entry_ptr, head_ptr, tail_ptr, len, Size, fv)  \
        H5C2__TRANS_DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len,      \
			              Size, fv)                                \
        if ( (head_ptr) == NULL )                                              \
        {                                                                      \
           (head_ptr) = (entry_ptr);                                           \
           (tail_ptr) = (entry_ptr);                                           \
        }                                                                      \
        else                                                                   \
        {                                                                      \
           (head_ptr)->trans_prev = (entry_ptr);                               \
           (entry_ptr)->trans_next = (head_ptr);                               \
           (head_ptr) = (entry_ptr);                                           \
        }                                                                      \
        (len)++;                                                               \
        (Size) += entry_ptr->size;

#define H5C2__TRANS_DLL_REMOVE(entry_ptr, head_ptr, tail_ptr, len, Size, fv) \
        H5C2__TRANS_DLL_PRE_REMOVE_SC(entry_ptr, head_ptr, tail_ptr, len,    \
			              Size, fv)                              \
        {                                                                    \
           if ( (head_ptr) == (entry_ptr) )                                  \
           {                                                                 \
              (head_ptr) = (entry_ptr)->trans_next;                          \
              if ( (head_ptr) != NULL )                                      \
              {                                                              \
                 (head_ptr)->trans_prev = NULL;                              \
              }                                                              \
           }                                                                 \
           else                                                              \
           {                                                                 \
              (entry_ptr)->trans_prev->trans_next = (entry_ptr)->trans_next; \
           }                                                                 \
           if ( (tail_ptr) == (entry_ptr) )                                  \
           {                                                                 \
              (tail_ptr) = (entry_ptr)->trans_prev;                          \
              if ( (tail_ptr) != NULL )                                      \
              {                                                              \
                 (tail_ptr)->trans_next = NULL;                              \
              }                                                              \
           }                                                                 \
           else                                                              \
           {                                                                 \
              (entry_ptr)->trans_next->trans_prev = (entry_ptr)->trans_prev; \
           }                                                                 \
           entry_ptr->trans_next = NULL;                                     \
           entry_ptr->trans_prev = NULL;                                     \
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
 * H5C2__UPDATE_CACHE_HIT_RATE_STATS(), which is always active as
 * the cache hit rate stats are always collected and available.
 *
 * Changes:
 *
 * 	JRM -- 3/21/06
 * 	Added / updated macros for pinned entry related stats.
 *
 * 	JRM -- 8/9/06
 * 	More pinned entry stats related updates.
 *
 * 	JRM -- 3/31/07
 * 	Updated H5C2__UPDATE_STATS_FOR_PROTECT() to keep stats on 
 * 	read and write protects.
 *
 ***********************************************************************/

#define H5C2__UPDATE_CACHE_HIT_RATE_STATS(cache_ptr, hit) \
        (cache_ptr->cache_accesses)++;                   \
        if ( hit ) {                                     \
            (cache_ptr->cache_hits)++;                   \
        }                                                \

#if H5C2_COLLECT_CACHE_STATS

#define H5C2__UPDATE_STATS_FOR_DIRTY_PIN(cache_ptr, entry_ptr) \
	(((cache_ptr)->dirty_pins)[(entry_ptr)->type->id])++;

#define H5C2__UPDATE_STATS_FOR_UNPROTECT(cache_ptr)                   \
        if ( (cache_ptr)->slist_len > (cache_ptr)->max_slist_len )   \
	    (cache_ptr)->max_slist_len = (cache_ptr)->slist_len;     \
        if ( (cache_ptr)->slist_size > (cache_ptr)->max_slist_size ) \
	    (cache_ptr)->max_slist_size = (cache_ptr)->slist_size;   \
	if ( (cache_ptr)->pel_len > (cache_ptr)->max_pel_len )       \
	    (cache_ptr)->max_pel_len = (cache_ptr)->pel_len;         \
	if ( (cache_ptr)->pel_size > (cache_ptr)->max_pel_size )     \
	    (cache_ptr)->max_pel_size = (cache_ptr)->pel_size;

#define H5C2__UPDATE_STATS_FOR_RENAME(cache_ptr, entry_ptr)               \
	if ( cache_ptr->flush_in_progress ) {                            \
            ((cache_ptr)->cache_flush_renames[(entry_ptr)->type->id])++; \
	}                                                                \
        if ( entry_ptr->flush_in_progress ) {                            \
            ((cache_ptr)->entry_flush_renames[(entry_ptr)->type->id])++; \
	}                                                                \
	(((cache_ptr)->renames)[(entry_ptr)->type->id])++;

#define H5C2__UPDATE_STATS_FOR_ENTRY_SIZE_CHANGE(cache_ptr, entry_ptr, new_size)\
	if ( cache_ptr->flush_in_progress ) {                                   \
            ((cache_ptr)->cache_flush_size_changes[(entry_ptr)->type->id])++;   \
	}                                                                       \
        if ( entry_ptr->flush_in_progress ) {                                   \
            ((cache_ptr)->entry_flush_size_changes[(entry_ptr)->type->id])++;   \
	}                                                                       \
	if ( (entry_ptr)->size < (new_size) ) {                                 \
	    ((cache_ptr)->size_increases[(entry_ptr)->type->id])++;             \
            if ( (cache_ptr)->index_size > (cache_ptr)->max_index_size )        \
                (cache_ptr)->max_index_size = (cache_ptr)->index_size;          \
            if ( (cache_ptr)->slist_size > (cache_ptr)->max_slist_size )        \
                (cache_ptr)->max_slist_size = (cache_ptr)->slist_size;          \
            if ( (cache_ptr)->pl_size > (cache_ptr)->max_pl_size )              \
                (cache_ptr)->max_pl_size = (cache_ptr)->pl_size;                \
	} else if ( (entry_ptr)->size > (new_size) ) {                          \
	    ((cache_ptr)->size_decreases[(entry_ptr)->type->id])++;             \
	}

#define H5C2__UPDATE_STATS_FOR_HT_INSERTION(cache_ptr) \
	(cache_ptr)->total_ht_insertions++;

#define H5C2__UPDATE_STATS_FOR_HT_DELETION(cache_ptr) \
	(cache_ptr)->total_ht_deletions++;

#define H5C2__UPDATE_STATS_FOR_HT_SEARCH(cache_ptr, success, depth) \
	if ( success ) {                                            \
	    (cache_ptr)->successful_ht_searches++;                  \
	    (cache_ptr)->total_successful_ht_search_depth += depth; \
	} else {                                                    \
	    (cache_ptr)->failed_ht_searches++;                      \
	    (cache_ptr)->total_failed_ht_search_depth += depth;     \
	}

#define H5C2__UPDATE_STATS_FOR_UNPIN(cache_ptr, entry_ptr) \
	((cache_ptr)->unpins)[(entry_ptr)->type->id]++;

#if H5C2_COLLECT_CACHE_ENTRY_STATS

#define H5C2__RESET_CACHE_ENTRY_STATS(entry_ptr) \
        (entry_ptr)->accesses = 0;               \
        (entry_ptr)->clears   = 0;               \
        (entry_ptr)->flushes  = 0;               \
	(entry_ptr)->pins     = 0;

#define H5C2__UPDATE_STATS_FOR_CLEAR(cache_ptr, entry_ptr)           \
	(((cache_ptr)->clears)[(entry_ptr)->type->id])++;            \
        if ( (entry_ptr)->is_pinned ) {                              \
	    (((cache_ptr)->pinned_clears)[(entry_ptr)->type->id])++; \
	}                                                            \
        ((entry_ptr)->clears)++;

#define H5C2__UPDATE_STATS_FOR_FLUSH(cache_ptr, entry_ptr)            \
	(((cache_ptr)->flushes)[(entry_ptr)->type->id])++;            \
        if ( (entry_ptr)->is_pinned ) {                               \
	    (((cache_ptr)->pinned_flushes)[(entry_ptr)->type->id])++; \
	}                                                             \
        ((entry_ptr)->flushes)++;

#define H5C2__UPDATE_STATS_FOR_EVICTION(cache_ptr, entry_ptr)       \
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
        if ( (entry_ptr)->pins >                                    \
             ((cache_ptr)->max_pins)[(entry_ptr)->type->id] ) {     \
            ((cache_ptr)->max_pins)[(entry_ptr)->type->id]          \
                 = (entry_ptr)->pins;                               \
        }

#define H5C2__UPDATE_STATS_FOR_INSERTION(cache_ptr, entry_ptr)           \
	(((cache_ptr)->insertions)[(entry_ptr)->type->id])++;            \
	if ( (entry_ptr)->is_pinned ) {                                  \
	    (((cache_ptr)->pinned_insertions)[(entry_ptr)->type->id])++; \
	    ((cache_ptr)->pins)[(entry_ptr)->type->id]++;                \
            (entry_ptr)->pins++;                                         \
	    if ( (cache_ptr)->pel_len > (cache_ptr)->max_pel_len )       \
	        (cache_ptr)->max_pel_len = (cache_ptr)->pel_len;         \
	    if ( (cache_ptr)->pel_size > (cache_ptr)->max_pel_size )     \
	        (cache_ptr)->max_pel_size = (cache_ptr)->pel_size;       \
	}                                                                \
        if ( (cache_ptr)->index_len > (cache_ptr)->max_index_len )       \
	    (cache_ptr)->max_index_len = (cache_ptr)->index_len;         \
        if ( (cache_ptr)->index_size > (cache_ptr)->max_index_size )     \
	    (cache_ptr)->max_index_size = (cache_ptr)->index_size;       \
        if ( (cache_ptr)->slist_len > (cache_ptr)->max_slist_len )       \
	    (cache_ptr)->max_slist_len = (cache_ptr)->slist_len;         \
        if ( (cache_ptr)->slist_size > (cache_ptr)->max_slist_size )     \
	    (cache_ptr)->max_slist_size = (cache_ptr)->slist_size;       \
        if ( (entry_ptr)->size >                                         \
             ((cache_ptr)->max_size)[(entry_ptr)->type->id] ) {          \
            ((cache_ptr)->max_size)[(entry_ptr)->type->id]               \
                 = (entry_ptr)->size;                                    \
        }

#define H5C2__UPDATE_STATS_FOR_PROTECT(cache_ptr, entry_ptr, hit)            \
	if ( hit )                                                           \
            ((cache_ptr)->hits)[(entry_ptr)->type->id]++;                    \
	else                                                                 \
            ((cache_ptr)->misses)[(entry_ptr)->type->id]++;                  \
        if ( ! ((entry_ptr)->is_read_only) ) {                               \
	    ((cache_ptr)->write_protects)[(entry_ptr)->type->id]++;          \
	} else {                                                             \
	    ((cache_ptr)->read_protects)[(entry_ptr)->type->id]++;           \
	    if ( ((entry_ptr)->ro_ref_count) >                               \
		 ((cache_ptr)->max_read_protects)[(entry_ptr)->type->id] ) { \
	        ((cache_ptr)->max_read_protects)[(entry_ptr)->type->id] =    \
			((entry_ptr)->ro_ref_count);                         \
	    }                                                                \
	}                                                                    \
        if ( (cache_ptr)->index_len > (cache_ptr)->max_index_len )           \
            (cache_ptr)->max_index_len = (cache_ptr)->index_len;             \
        if ( (cache_ptr)->index_size > (cache_ptr)->max_index_size )         \
            (cache_ptr)->max_index_size = (cache_ptr)->index_size;           \
        if ( (cache_ptr)->pl_len > (cache_ptr)->max_pl_len )                 \
            (cache_ptr)->max_pl_len = (cache_ptr)->pl_len;                   \
        if ( (cache_ptr)->pl_size > (cache_ptr)->max_pl_size )               \
            (cache_ptr)->max_pl_size = (cache_ptr)->pl_size;                 \
        if ( (entry_ptr)->size >                                             \
             ((cache_ptr)->max_size)[(entry_ptr)->type->id] ) {              \
            ((cache_ptr)->max_size)[(entry_ptr)->type->id]                   \
                 = (entry_ptr)->size;                                        \
        }                                                                    \
        ((entry_ptr)->accesses)++;

#define H5C2__UPDATE_STATS_FOR_PIN(cache_ptr, entry_ptr)         \
	((cache_ptr)->pins)[(entry_ptr)->type->id]++;            \
        (entry_ptr)->pins++;                                     \
	if ( (cache_ptr)->pel_len > (cache_ptr)->max_pel_len )   \
	    (cache_ptr)->max_pel_len = (cache_ptr)->pel_len;     \
	if ( (cache_ptr)->pel_size > (cache_ptr)->max_pel_size ) \
	    (cache_ptr)->max_pel_size = (cache_ptr)->pel_size;

#else /* H5C2_COLLECT_CACHE_ENTRY_STATS */

#define H5C2__RESET_CACHE_ENTRY_STATS(entry_ptr)

#define H5C2__UPDATE_STATS_FOR_CLEAR(cache_ptr, entry_ptr)            \
        if ( (entry_ptr)->is_pinned ) {                               \
	    (((cache_ptr)->pinned_clears)[(entry_ptr)->type->id])++;  \
	}                                                             \
	(((cache_ptr)->clears)[(entry_ptr)->type->id])++;

#define H5C2__UPDATE_STATS_FOR_FLUSH(cache_ptr, entry_ptr)            \
	(((cache_ptr)->flushes)[(entry_ptr)->type->id])++;            \
        if ( (entry_ptr)->is_pinned ) {                               \
	    (((cache_ptr)->pinned_flushes)[(entry_ptr)->type->id])++; \
	}

#define H5C2__UPDATE_STATS_FOR_EVICTION(cache_ptr, entry_ptr) \
	(((cache_ptr)->evictions)[(entry_ptr)->type->id])++;

#define H5C2__UPDATE_STATS_FOR_INSERTION(cache_ptr, entry_ptr)           \
	(((cache_ptr)->insertions)[(entry_ptr)->type->id])++;            \
	if ( (entry_ptr)->is_pinned ) {                                  \
	    (((cache_ptr)->pinned_insertions)[(entry_ptr)->type->id])++; \
	    ((cache_ptr)->pins)[(entry_ptr)->type->id]++;                \
	    if ( (cache_ptr)->pel_len > (cache_ptr)->max_pel_len )       \
	        (cache_ptr)->max_pel_len = (cache_ptr)->pel_len;         \
	    if ( (cache_ptr)->pel_size > (cache_ptr)->max_pel_size )     \
	        (cache_ptr)->max_pel_size = (cache_ptr)->pel_size;       \
	}                                                                \
        if ( (cache_ptr)->index_len > (cache_ptr)->max_index_len )       \
	    (cache_ptr)->max_index_len = (cache_ptr)->index_len;         \
        if ( (cache_ptr)->index_size > (cache_ptr)->max_index_size )     \
	    (cache_ptr)->max_index_size = (cache_ptr)->index_size;       \
        if ( (cache_ptr)->slist_len > (cache_ptr)->max_slist_len )       \
	    (cache_ptr)->max_slist_len = (cache_ptr)->slist_len;         \
        if ( (cache_ptr)->slist_size > (cache_ptr)->max_slist_size )     \
	    (cache_ptr)->max_slist_size = (cache_ptr)->slist_size;

#define H5C2__UPDATE_STATS_FOR_PROTECT(cache_ptr, entry_ptr, hit)            \
	if ( hit )                                                           \
            ((cache_ptr)->hits)[(entry_ptr)->type->id]++;                    \
	else                                                                 \
            ((cache_ptr)->misses)[(entry_ptr)->type->id]++;                  \
        if ( ! ((entry_ptr)->is_read_only) ) {                               \
	    ((cache_ptr)->write_protects)[(entry_ptr)->type->id]++;          \
	} else {                                                             \
	    ((cache_ptr)->read_protects)[(entry_ptr)->type->id]++;           \
	    if ( ((entry_ptr)->ro_ref_count) >                               \
		 ((cache_ptr)->max_read_protects)[(entry_ptr)->type->id] ) { \
	        ((cache_ptr)->max_read_protects)[(entry_ptr)->type->id] =    \
			((entry_ptr)->ro_ref_count);                         \
	    }                                                                \
	}                                                                    \
        if ( (cache_ptr)->index_len > (cache_ptr)->max_index_len )           \
            (cache_ptr)->max_index_len = (cache_ptr)->index_len;             \
        if ( (cache_ptr)->index_size > (cache_ptr)->max_index_size )         \
            (cache_ptr)->max_index_size = (cache_ptr)->index_size;           \
        if ( (cache_ptr)->pl_len > (cache_ptr)->max_pl_len )                 \
            (cache_ptr)->max_pl_len = (cache_ptr)->pl_len;                   \
        if ( (cache_ptr)->pl_size > (cache_ptr)->max_pl_size )               \
            (cache_ptr)->max_pl_size = (cache_ptr)->pl_size;

#define H5C2__UPDATE_STATS_FOR_PIN(cache_ptr, entry_ptr)         \
	((cache_ptr)->pins)[(entry_ptr)->type->id]++;            \
	if ( (cache_ptr)->pel_len > (cache_ptr)->max_pel_len )   \
	    (cache_ptr)->max_pel_len = (cache_ptr)->pel_len;     \
	if ( (cache_ptr)->pel_size > (cache_ptr)->max_pel_size ) \
	    (cache_ptr)->max_pel_size = (cache_ptr)->pel_size;

#endif /* H5C2_COLLECT_CACHE_ENTRY_STATS */

#else /* H5C2_COLLECT_CACHE_STATS */

#define H5C2__RESET_CACHE_ENTRY_STATS(entry_ptr)
#define H5C2__UPDATE_STATS_FOR_DIRTY_PIN(cache_ptr, entry_ptr)
#define H5C2__UPDATE_STATS_FOR_UNPROTECT(cache_ptr)
#define H5C2__UPDATE_STATS_FOR_RENAME(cache_ptr, entry_ptr)
#define H5C2__UPDATE_STATS_FOR_ENTRY_SIZE_CHANGE(cache_ptr, entry_ptr, new_size)
#define H5C2__UPDATE_STATS_FOR_HT_INSERTION(cache_ptr)
#define H5C2__UPDATE_STATS_FOR_HT_DELETION(cache_ptr)
#define H5C2__UPDATE_STATS_FOR_HT_SEARCH(cache_ptr, success, depth)
#define H5C2__UPDATE_STATS_FOR_INSERTION(cache_ptr, entry_ptr)
#define H5C2__UPDATE_STATS_FOR_CLEAR(cache_ptr, entry_ptr)
#define H5C2__UPDATE_STATS_FOR_FLUSH(cache_ptr, entry_ptr)
#define H5C2__UPDATE_STATS_FOR_EVICTION(cache_ptr, entry_ptr)
#define H5C2__UPDATE_STATS_FOR_PROTECT(cache_ptr, entry_ptr, hit)
#define H5C2__UPDATE_STATS_FOR_PIN(cache_ptr, entry_ptr)
#define H5C2__UPDATE_STATS_FOR_UNPIN(cache_ptr, entry_ptr)

#endif /* H5C2_COLLECT_CACHE_STATS */


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

/* H5C2__HASH_TABLE_LEN is defined in H5C2pkg.h.  It mut be a power of two. */

#define H5C2__HASH_MASK		((size_t)(H5C2__HASH_TABLE_LEN - 1) << 3)

#define H5C2__HASH_FCN(x)	(int)(((x) & H5C2__HASH_MASK) >> 3)

#if H5C2_DO_SANITY_CHECKS

#define H5C2__PRE_HT_INSERT_SC(cache_ptr, entry_ptr, fail_val) \
if ( ( (cache_ptr) == NULL ) ||                                \
     ( (cache_ptr)->magic != H5C2__H5C2_T_MAGIC ) ||           \
     ( (entry_ptr) == NULL ) ||                                \
     ( ! H5F_addr_defined((entry_ptr)->addr) ) ||              \
     ( (entry_ptr)->ht_next != NULL ) ||                       \
     ( (entry_ptr)->ht_prev != NULL ) ||                       \
     ( (entry_ptr)->size <= 0 ) ||                             \
     ( (k = H5C2__HASH_FCN((entry_ptr)->addr)) < 0 ) ||        \
     ( k >= H5C2__HASH_TABLE_LEN ) ) {                         \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, fail_val,               \
               "Pre HT insert SC failed")                      \
}

#define H5C2__PRE_HT_REMOVE_SC(cache_ptr, entry_ptr)                    \
if ( ( (cache_ptr) == NULL ) ||                                         \
     ( (cache_ptr)->magic != H5C2__H5C2_T_MAGIC ) ||                    \
     ( (cache_ptr)->index_len < 1 ) ||                                  \
     ( (entry_ptr) == NULL ) ||                                         \
     ( (cache_ptr)->index_size < (entry_ptr)->size ) ||                 \
     ( ! H5F_addr_defined((entry_ptr)->addr) ) ||                       \
     ( (entry_ptr)->size <= 0 ) ||                                      \
     ( H5C2__HASH_FCN((entry_ptr)->addr) < 0 ) ||                       \
     ( H5C2__HASH_FCN((entry_ptr)->addr) >= H5C2__HASH_TABLE_LEN ) ||   \
     ( ((cache_ptr)->index)[(H5C2__HASH_FCN((entry_ptr)->addr))]        \
       == NULL ) ||                                                     \
     ( ( ((cache_ptr)->index)[(H5C2__HASH_FCN((entry_ptr)->addr))]      \
       != (entry_ptr) ) &&                                              \
       ( (entry_ptr)->ht_prev == NULL ) ) ||                            \
     ( ( ((cache_ptr)->index)[(H5C2__HASH_FCN((entry_ptr)->addr))] ==   \
         (entry_ptr) ) &&                                               \
       ( (entry_ptr)->ht_prev != NULL ) ) ) {                           \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Pre HT remove SC failed") \
}

#define H5C2__PRE_HT_SEARCH_SC(cache_ptr, Addr, fail_val)                    \
if ( ( (cache_ptr) == NULL ) ||                                              \
     ( (cache_ptr)->magic != H5C2__H5C2_T_MAGIC ) ||                         \
     ( ! H5F_addr_defined(Addr) ) ||                                         \
     ( H5C2__HASH_FCN(Addr) < 0 ) ||                                         \
     ( H5C2__HASH_FCN(Addr) >= H5C2__HASH_TABLE_LEN ) ) {                    \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, fail_val, "Pre HT search SC failed")  \
}

#define H5C2__POST_SUC_HT_SEARCH_SC(cache_ptr, entry_ptr, Addr, k, fail_val) \
if ( ( (cache_ptr) == NULL ) ||                                              \
     ( (cache_ptr)->magic != H5C2__H5C2_T_MAGIC ) ||                         \
     ( (cache_ptr)->index_len < 1 ) ||                                       \
     ( (entry_ptr) == NULL ) ||                                              \
     ( (cache_ptr)->index_size < (entry_ptr)->size ) ||                      \
     ( H5F_addr_ne((entry_ptr)->addr, (Addr)) ) ||                           \
     ( (entry_ptr)->size <= 0 ) ||                                           \
     ( ((cache_ptr)->index)[k] == NULL ) ||                                  \
     ( ( ((cache_ptr)->index)[k] != (entry_ptr) ) &&                         \
       ( (entry_ptr)->ht_prev == NULL ) ) ||                                 \
     ( ( ((cache_ptr)->index)[k] == (entry_ptr) ) &&                         \
       ( (entry_ptr)->ht_prev != NULL ) ) ||                                 \
     ( ( (entry_ptr)->ht_prev != NULL ) &&                                   \
       ( (entry_ptr)->ht_prev->ht_next != (entry_ptr) ) ) ||                 \
     ( ( (entry_ptr)->ht_next != NULL ) &&                                   \
       ( (entry_ptr)->ht_next->ht_prev != (entry_ptr) ) ) ) {                \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, fail_val,                             \
                "Post successful HT search SC failed")                       \
}

#define H5C2__POST_HT_SHIFT_TO_FRONT(cache_ptr, entry_ptr, k, fail_val) \
if ( ( (cache_ptr) == NULL ) ||                                         \
     ( ((cache_ptr)->index)[k] != (entry_ptr) ) ||                      \
     ( (entry_ptr)->ht_prev != NULL ) ) {                               \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, fail_val,                        \
                "Post HT shift to front SC failed")                     \
}

#define H5C2__PRE_HT_ENTRY_SIZE_CHANGE_SC(cache_ptr, old_size, new_size) \
if ( ( (cache_ptr) == NULL ) ||                                          \
     ( (cache_ptr)->index_len <= 0 ) ||                                  \
     ( (cache_ptr)->index_size <= 0 ) ||                                 \
     ( (new_size) <= 0 ) ||                                              \
     ( (old_size) > (cache_ptr)->index_size ) ||                         \
     ( (new_size) <= 0 ) ||                                              \
     ( ( (cache_ptr)->index_len == 1 ) &&                                \
       ( (cache_ptr)->index_size != (old_size) ) ) ) {                   \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL,                             \
                "Pre HT entry size change SC failed")                    \
}

#define H5C2__POST_HT_ENTRY_SIZE_CHANGE_SC(cache_ptr, old_size, new_size) \
if ( ( (cache_ptr) == NULL ) ||                                           \
     ( (cache_ptr)->index_len <= 0 ) ||                                   \
     ( (cache_ptr)->index_size <= 0 ) ||                                  \
     ( (new_size) > (cache_ptr)->index_size ) ||                          \
     ( ( (cache_ptr)->index_len == 1 ) &&                                 \
       ( (cache_ptr)->index_size != (new_size) ) ) ) {                    \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL,                              \
                "Post HT entry size change SC failed")                    \
}

#else /* H5C2_DO_SANITY_CHECKS */

#define H5C2__PRE_HT_INSERT_SC(cache_ptr, entry_ptr, fail_val)
#define H5C2__PRE_HT_REMOVE_SC(cache_ptr, entry_ptr)
#define H5C2__PRE_HT_SEARCH_SC(cache_ptr, Addr, fail_val)
#define H5C2__POST_SUC_HT_SEARCH_SC(cache_ptr, entry_ptr, Addr, k, fail_val)
#define H5C2__POST_HT_SHIFT_TO_FRONT(cache_ptr, entry_ptr, k, fail_val)
#define H5C2__PRE_HT_ENTRY_SIZE_CHANGE_SC(cache_ptr, old_size, new_size)
#define H5C2__POST_HT_ENTRY_SIZE_CHANGE_SC(cache_ptr, old_size, new_size)

#endif /* H5C2_DO_SANITY_CHECKS */


#define H5C2__INSERT_IN_INDEX(cache_ptr, entry_ptr, fail_val) \
{                                                             \
    int k;                                                    \
    H5C2__PRE_HT_INSERT_SC(cache_ptr, entry_ptr, fail_val)    \
    k = H5C2__HASH_FCN((entry_ptr)->addr);                    \
    if ( ((cache_ptr)->index)[k] == NULL )                    \
    {                                                         \
        ((cache_ptr)->index)[k] = (entry_ptr);                \
    }                                                         \
    else                                                      \
    {                                                         \
        (entry_ptr)->ht_next = ((cache_ptr)->index)[k];       \
        (entry_ptr)->ht_next->ht_prev = (entry_ptr);          \
        ((cache_ptr)->index)[k] = (entry_ptr);                \
    }                                                         \
    (cache_ptr)->index_len++;                                 \
    (cache_ptr)->index_size += (entry_ptr)->size;             \
    H5C2__UPDATE_STATS_FOR_HT_INSERTION(cache_ptr)            \
}

#define H5C2__DELETE_FROM_INDEX(cache_ptr, entry_ptr)         \
{                                                             \
    int k;                                                    \
    H5C2__PRE_HT_REMOVE_SC(cache_ptr, entry_ptr)              \
    k = H5C2__HASH_FCN((entry_ptr)->addr);                    \
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
    H5C2__UPDATE_STATS_FOR_HT_DELETION(cache_ptr)             \
}

#define H5C2__SEARCH_INDEX(cache_ptr, Addr, entry_ptr, fail_val)             \
{                                                                            \
    int k;                                                                   \
    int depth = 0;                                                           \
    H5C2__PRE_HT_SEARCH_SC(cache_ptr, Addr, fail_val)                        \
    k = H5C2__HASH_FCN(Addr);                                                \
    entry_ptr = ((cache_ptr)->index)[k];                                     \
    while ( ( entry_ptr ) && ( H5F_addr_ne(Addr, (entry_ptr)->addr) ) )      \
    {                                                                        \
        (entry_ptr) = (entry_ptr)->ht_next;                                  \
        (depth)++;                                                           \
    }                                                                        \
    if ( entry_ptr )                                                         \
    {                                                                        \
        H5C2__POST_SUC_HT_SEARCH_SC(cache_ptr, entry_ptr, Addr, k, fail_val) \
        if ( entry_ptr != ((cache_ptr)->index)[k] )                          \
        {                                                                    \
            if ( (entry_ptr)->ht_next )                                      \
            {                                                                \
                (entry_ptr)->ht_next->ht_prev = (entry_ptr)->ht_prev;        \
            }                                                                \
            HDassert( (entry_ptr)->ht_prev != NULL );                        \
            (entry_ptr)->ht_prev->ht_next = (entry_ptr)->ht_next;            \
            ((cache_ptr)->index)[k]->ht_prev = (entry_ptr);                  \
            (entry_ptr)->ht_next = ((cache_ptr)->index)[k];                  \
            (entry_ptr)->ht_prev = NULL;                                     \
            ((cache_ptr)->index)[k] = (entry_ptr);                           \
            H5C2__POST_HT_SHIFT_TO_FRONT(cache_ptr, entry_ptr, k, fail_val)  \
        }                                                                    \
    }                                                                        \
    H5C2__UPDATE_STATS_FOR_HT_SEARCH(cache_ptr, (entry_ptr != NULL), depth)  \
}

#define H5C2__SEARCH_INDEX_NO_STATS(cache_ptr, Addr, entry_ptr, fail_val)    \
{                                                                            \
    int k;                                                                   \
    int depth = 0;                                                           \
    H5C2__PRE_HT_SEARCH_SC(cache_ptr, Addr, fail_val)                        \
    k = H5C2__HASH_FCN(Addr);                                                \
    entry_ptr = ((cache_ptr)->index)[k];                                     \
    while ( ( entry_ptr ) && ( H5F_addr_ne(Addr, (entry_ptr)->addr) ) )      \
    {                                                                        \
        (entry_ptr) = (entry_ptr)->ht_next;                                  \
        (depth)++;                                                           \
    }                                                                        \
    if ( entry_ptr )                                                         \
    {                                                                        \
        H5C2__POST_SUC_HT_SEARCH_SC(cache_ptr, entry_ptr, Addr, k, fail_val) \
        if ( entry_ptr != ((cache_ptr)->index)[k] )                          \
        {                                                                    \
            if ( (entry_ptr)->ht_next )                                      \
            {                                                                \
                (entry_ptr)->ht_next->ht_prev = (entry_ptr)->ht_prev;        \
            }                                                                \
            HDassert( (entry_ptr)->ht_prev != NULL );                        \
            (entry_ptr)->ht_prev->ht_next = (entry_ptr)->ht_next;            \
            ((cache_ptr)->index)[k]->ht_prev = (entry_ptr);                  \
            (entry_ptr)->ht_next = ((cache_ptr)->index)[k];                  \
            (entry_ptr)->ht_prev = NULL;                                     \
            ((cache_ptr)->index)[k] = (entry_ptr);                           \
            H5C2__POST_HT_SHIFT_TO_FRONT(cache_ptr, entry_ptr, k, fail_val)  \
        }                                                                    \
    }                                                                        \
}

#define H5C2__UPDATE_INDEX_FOR_SIZE_CHANGE(cache_ptr, old_size, new_size) \
{                                                                         \
    H5C2__PRE_HT_ENTRY_SIZE_CHANGE_SC(cache_ptr, old_size, new_size)      \
    (cache_ptr)->index_size -= old_size;                                  \
    (cache_ptr)->index_size += new_size;                                  \
    H5C2__POST_HT_ENTRY_SIZE_CHANGE_SC(cache_ptr, old_size, new_size)     \
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
 * Macro:	H5C2__INSERT_ENTRY_IN_SLIST
 *
 * Purpose:     Insert the specified instance of H5C2_cache_entry_t into
 *		the skip list in the specified instance of H5C2_t.  Update
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
 *		Converted the function H5C2_insert_entry_in_tree() into
 *		the macro H5C2__INSERT_ENTRY_IN_TREE in the hopes of
 *		wringing a little more speed out of the cache.
 *
 *		Note that we don't bother to check if the entry is already
 *		in the tree -- if it is, H5SL_insert() will fail.
 *
 *		QAK -- 11/27/04
 *		Switched over to using skip list routines.
 *
 *		JRM -- 6/27/06
 *		Added fail_val parameter.
 *
 *		JRM -- 8/25/06
 *		Added the H5C2_DO_SANITY_CHECKS version of the macro.
 *
 *		This version maintains the slist_len_increase and 
 *		slist_size_increase fields that are used in sanity
 *		checks in the flush routines.
 *
 *		All this is needed as the fractal heap needs to be 
 *		able to dirty, resize and/or rename entries during the 
 *		flush.
 *
 *-------------------------------------------------------------------------
 */

#if H5C2_DO_SANITY_CHECKS

#define H5C2__INSERT_ENTRY_IN_SLIST(cache_ptr, entry_ptr, fail_val)            \
{                                                                              \
    HDassert( (cache_ptr) );                                                   \
    HDassert( (cache_ptr)->magic == H5C2__H5C2_T_MAGIC );                      \
    HDassert( (entry_ptr) );                                                   \
    HDassert( (entry_ptr)->size > 0 );                                         \
    HDassert( H5F_addr_defined((entry_ptr)->addr) );                           \
    HDassert( !((entry_ptr)->in_slist) );                                      \
                                                                               \
    if ( H5SL_insert((cache_ptr)->slist_ptr, entry_ptr, &(entry_ptr)->addr)    \
                                                                         < 0 ) \
        HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, (fail_val),                       \
                    "Can't insert entry in skip list")                         \
                                                                               \
    (entry_ptr)->in_slist = TRUE;                                              \
    (cache_ptr)->slist_len++;                                                  \
    (cache_ptr)->slist_size += (entry_ptr)->size;                              \
    (cache_ptr)->slist_len_increase++;                                         \
    (cache_ptr)->slist_size_increase += (entry_ptr)->size;                     \
                                                                               \
    HDassert( (cache_ptr)->slist_len > 0 );                                    \
    HDassert( (cache_ptr)->slist_size > 0 );                                   \
                                                                               \
} /* H5C2__INSERT_ENTRY_IN_SLIST */

#else /* H5C2_DO_SANITY_CHECKS */

#define H5C2__INSERT_ENTRY_IN_SLIST(cache_ptr, entry_ptr, fail_val)            \
{                                                                              \
    HDassert( (cache_ptr) );                                                   \
    HDassert( (cache_ptr)->magic == H5C2__H5C2_T_MAGIC );                      \
    HDassert( (entry_ptr) );                                                   \
    HDassert( (entry_ptr)->size > 0 );                                         \
    HDassert( H5F_addr_defined((entry_ptr)->addr) );                           \
    HDassert( !((entry_ptr)->in_slist) );                                      \
                                                                               \
    if ( H5SL_insert((cache_ptr)->slist_ptr, entry_ptr, &(entry_ptr)->addr)    \
                                                                         < 0 ) \
        HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, (fail_val),                       \
                    "Can't insert entry in skip list")                         \
                                                                               \
    (entry_ptr)->in_slist = TRUE;                                              \
    (cache_ptr)->slist_len++;                                                  \
    (cache_ptr)->slist_size += (entry_ptr)->size;                              \
                                                                               \
    HDassert( (cache_ptr)->slist_len > 0 );                                    \
    HDassert( (cache_ptr)->slist_size > 0 );                                   \
                                                                               \
} /* H5C2__INSERT_ENTRY_IN_SLIST */

#endif /* H5C2_DO_SANITY_CHECKS */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C2__REMOVE_ENTRY_FROM_SLIST
 *
 * Purpose:     Remove the specified instance of H5C2_cache_entry_t from the
 *		index skip list in the specified instance of H5C2_t.  Update
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
 *		Converted from the function H5C2_remove_entry_from_tree()
 *		to the macro H5C2__REMOVE_ENTRY_FROM_TREE in the hopes of
 *		wringing a little more performance out of the cache.
 *
 *		QAK -- 11/27/04
 *		Switched over to using skip list routines.
 *
 *		JRM -- 3/28/07
 *		Updated sanity checks for the new is_read_only and 
 *		ro_ref_count fields in H5C2_cache_entry_t.
 *
 *-------------------------------------------------------------------------
 */

#define H5C2__REMOVE_ENTRY_FROM_SLIST(cache_ptr, entry_ptr)         \
{                                                                   \
    HDassert( (cache_ptr) );                                        \
    HDassert( (cache_ptr)->magic == H5C2__H5C2_T_MAGIC );           \
    HDassert( (entry_ptr) );                                        \
    HDassert( !((entry_ptr)->is_protected) );                       \
    HDassert( !((entry_ptr)->is_read_only) );                       \
    HDassert( ((entry_ptr)->ro_ref_count) == 0 );                   \
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
} /* H5C2__REMOVE_ENTRY_FROM_SLIST */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C2__UPDATE_SLIST_FOR_SIZE_CHANGE
 *
 * Purpose:     Update cache_ptr->slist_size for a change in the size of
 *		and entry in the slist.
 *
 * Return:      N/A
 *
 * Programmer:  John Mainzer, 9/07/05
 *
 * Modifications:
 *
 *		JRM -- 8/27/06
 *		Added the H5C2_DO_SANITY_CHECKS version of the macro.
 *
 *		This version maintains the slist_size_increase field 
 *		that are used in sanity checks in the flush routines.
 *
 *		All this is needed as the fractal heap needs to be 
 *		able to dirty, resize and/or rename entries during the 
 *		flush.
 *
 *-------------------------------------------------------------------------
 */

#if H5C2_DO_SANITY_CHECKS

#define H5C2__UPDATE_SLIST_FOR_SIZE_CHANGE(cache_ptr, old_size, new_size) \
{                                                                         \
    HDassert( (cache_ptr) );                                              \
    HDassert( (cache_ptr)->magic == H5C2__H5C2_T_MAGIC );                 \
    HDassert( (old_size) > 0 );                                           \
    HDassert( (new_size) > 0 );                                           \
    HDassert( (old_size) <= (cache_ptr)->slist_size );                    \
    HDassert( (cache_ptr)->slist_len > 0 );                               \
    HDassert( ((cache_ptr)->slist_len > 1) ||                             \
              ( (cache_ptr)->slist_size == (old_size) ) );                \
                                                                          \
    (cache_ptr)->slist_size -= (old_size);                                \
    (cache_ptr)->slist_size += (new_size);                                \
                                                                          \
    (cache_ptr)->slist_size_increase -= (int64_t)(old_size);              \
    (cache_ptr)->slist_size_increase += (int64_t)(new_size);              \
                                                                          \
    HDassert( (new_size) <= (cache_ptr)->slist_size );                    \
    HDassert( ( (cache_ptr)->slist_len > 1 ) ||                           \
              ( (cache_ptr)->slist_size == (new_size) ) );                \
} /* H5C2__REMOVE_ENTRY_FROM_SLIST */

#else /* H5C2_DO_SANITY_CHECKS */

#define H5C2__UPDATE_SLIST_FOR_SIZE_CHANGE(cache_ptr, old_size, new_size) \
{                                                                         \
    HDassert( (cache_ptr) );                                              \
    HDassert( (cache_ptr)->magic == H5C2__H5C2_T_MAGIC );                 \
    HDassert( (old_size) > 0 );                                           \
    HDassert( (new_size) > 0 );                                           \
    HDassert( (old_size) <= (cache_ptr)->slist_size );                    \
    HDassert( (cache_ptr)->slist_len > 0 );                               \
    HDassert( ((cache_ptr)->slist_len > 1) ||                             \
              ( (cache_ptr)->slist_size == (old_size) ) );                \
                                                                          \
    (cache_ptr)->slist_size -= (old_size);                                \
    (cache_ptr)->slist_size += (new_size);                                \
                                                                          \
    HDassert( (new_size) <= (cache_ptr)->slist_size );                    \
    HDassert( ( (cache_ptr)->slist_len > 1 ) ||                           \
              ( (cache_ptr)->slist_size == (new_size) ) );                \
} /* H5C2__REMOVE_ENTRY_FROM_SLIST */

#endif /* H5C2_DO_SANITY_CHECKS */


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
 * Macro:	H5C2__FAKE_RP_FOR_MOST_RECENT_ACCESS
 *
 * Purpose:     For efficiency, we sometimes change the order of flushes --
 *		but doing so can confuse the replacement policy.  This
 *		macro exists to allow us to specify an entry as the
 *		most recently touched so we can repair any such
 *		confusion.
 *
 *		At present, we only support the modified LRU policy, so
 *		this function deals with that case unconditionally.  If
 *		we ever support other replacement policies, the macro
 *		should switch on the current policy and act accordingly.
 *
 * Return:      N/A
 *
 * Programmer:  John Mainzer, 10/13/05
 *
 * Modifications:
 *
 *		JRM -- 3/20/06
 *		Modified macro to ignore pinned entries.  Pinned entries
 *		do not appear in the data structures maintained by the
 *		replacement policy code, and thus this macro has nothing
 *		to do if called for such an entry.
 *
 *		JRM -- 3/28/07
 *		Added sanity checks using the new is_read_only and 
 *		ro_ref_count fields of struct H5C2_cache_entry_t.
 *
 *		JRM -- 3/29/08
 *		Added a sanity check to verify that the target entry
 *		does not have a pending journal entry write.
 *
 *-------------------------------------------------------------------------
 */

#if H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS

#define H5C2__FAKE_RP_FOR_MOST_RECENT_ACCESS(cache_ptr, entry_ptr, fail_val) \
{                                                                            \
    HDassert( (cache_ptr) );                                                 \
    HDassert( (cache_ptr)->magic == H5C2__H5C2_T_MAGIC );                    \
    HDassert( (entry_ptr) );                                                 \
    HDassert( !((entry_ptr)->is_protected) );                                \
    HDassert( !((entry_ptr)->is_read_only) );                                \
    HDassert( ((entry_ptr)->ro_ref_count) == 0 );                            \
    HDassert( (entry_ptr)->size > 0 );                                       \
    HDassert( (entry_ptr)->last_trans == 0 );                                \
                                                                             \
    if ( ! ((entry_ptr)->is_pinned) ) {                                      \
                                                                             \
        /* modified LRU specific code */                                     \
                                                                             \
        /* remove the entry from the LRU list, and re-insert it at the head. \
	 */                                                                  \
                                                                             \
        H5C2__DLL_REMOVE((entry_ptr), (cache_ptr)->LRU_head_ptr,             \
                        (cache_ptr)->LRU_tail_ptr,                           \
			(cache_ptr)->LRU_list_len,                           \
                        (cache_ptr)->LRU_list_size, (fail_val))              \
                                                                             \
        H5C2__DLL_PREPEND((entry_ptr), (cache_ptr)->LRU_head_ptr,            \
                         (cache_ptr)->LRU_tail_ptr,                          \
			 (cache_ptr)->LRU_list_len,                          \
                         (cache_ptr)->LRU_list_size, (fail_val))             \
                                                                             \
        /* Use the dirty flag to infer whether the entry is on the clean or  \
         * dirty LRU list, and remove it.  Then insert it at the head of     \
         * the same LRU list.                                                \
         *                                                                   \
         * At least initially, all entries should be clean.  That may        \
         * change, so we may as well deal with both cases now.               \
         */                                                                  \
                                                                             \
        if ( (entry_ptr)->is_dirty ) {                                       \
            H5C2__AUX_DLL_REMOVE((entry_ptr), (cache_ptr)->dLRU_head_ptr,    \
                                (cache_ptr)->dLRU_tail_ptr,                  \
                                (cache_ptr)->dLRU_list_len,                  \
                                (cache_ptr)->dLRU_list_size, (fail_val))     \
                                                                             \
            H5C2__AUX_DLL_PREPEND((entry_ptr), (cache_ptr)->dLRU_head_ptr,   \
                                 (cache_ptr)->dLRU_tail_ptr,                 \
                                 (cache_ptr)->dLRU_list_len,                 \
                                 (cache_ptr)->dLRU_list_size, (fail_val))    \
        } else {                                                             \
            H5C2__AUX_DLL_REMOVE((entry_ptr), (cache_ptr)->cLRU_head_ptr,    \
                                (cache_ptr)->cLRU_tail_ptr,                  \
                                (cache_ptr)->cLRU_list_len,                  \
                                (cache_ptr)->cLRU_list_size, (fail_val))     \
                                                                             \
            H5C2__AUX_DLL_PREPEND((entry_ptr), (cache_ptr)->cLRU_head_ptr,   \
                                 (cache_ptr)->cLRU_tail_ptr,                 \
                                 (cache_ptr)->cLRU_list_len,                 \
                                 (cache_ptr)->cLRU_list_size, (fail_val))    \
        }                                                                    \
                                                                             \
        /* End modified LRU specific code. */                                \
    }                                                                        \
} /* H5C2__FAKE_RP_FOR_MOST_RECENT_ACCESS */

#else /* H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

#define H5C2__FAKE_RP_FOR_MOST_RECENT_ACCESS(cache_ptr, entry_ptr, fail_val) \
{                                                                            \
    HDassert( (cache_ptr) );                                                 \
    HDassert( (cache_ptr)->magic == H5C2__H5C2_T_MAGIC );                    \
    HDassert( (entry_ptr) );                                                 \
    HDassert( !((entry_ptr)->is_protected) );                                \
    HDassert( !((entry_ptr)->is_read_only) );                                \
    HDassert( ((entry_ptr)->ro_ref_count) == 0 );                            \
    HDassert( (entry_ptr)->size > 0 );                                       \
    HDassert( (entry_ptr)->last_trans == 0 );                                \
                                                                             \
    if ( ! ((entry_ptr)->is_pinned) ) {                                      \
                                                                             \
        /* modified LRU specific code */                                     \
                                                                             \
        /* remove the entry from the LRU list, and re-insert it at the head  \
	 */                                                                  \
                                                                             \
        H5C2__DLL_REMOVE((entry_ptr), (cache_ptr)->LRU_head_ptr,             \
                        (cache_ptr)->LRU_tail_ptr,                           \
			(cache_ptr)->LRU_list_len,                           \
                        (cache_ptr)->LRU_list_size, (fail_val))              \
                                                                             \
        H5C2__DLL_PREPEND((entry_ptr), (cache_ptr)->LRU_head_ptr,            \
                         (cache_ptr)->LRU_tail_ptr,                          \
			 (cache_ptr)->LRU_list_len,                          \
                         (cache_ptr)->LRU_list_size, (fail_val))             \
                                                                             \
        /* End modified LRU specific code. */                                \
    }                                                                        \
} /* H5C2__FAKE_RP_FOR_MOST_RECENT_ACCESS */

#endif /* H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */


/*-------------------------------------------------------------------------
 *
 * Macro:	H5C2__UPDATE_RP_FOR_EVICTION
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
 *		Converted the function H5C2_update_rp_for_eviction() to the
 *		macro H5C2__UPDATE_RP_FOR_EVICTION in an effort to squeeze
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
 *		JRM - 3/20/06
 *		Pinned entries can't be evicted, so this entry should never
 *		be called on a pinned entry.  Added assert to verify this.
 *
 *		JRM -- 3/28/07
 *		Added sanity checks for the new is_read_only and 
 *		ro_ref_count fields of struct H5C2_cache_entry_t.
 *
 *		JRM -- 3/29/08
 *		Added sanity check to verify that the evicted entry
 *		does not have a pending journal entry write.
 *
 *-------------------------------------------------------------------------
 */

#if H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS

#define H5C2__UPDATE_RP_FOR_EVICTION(cache_ptr, entry_ptr, fail_val)         \
{                                                                            \
    HDassert( (cache_ptr) );                                                 \
    HDassert( (cache_ptr)->magic == H5C2__H5C2_T_MAGIC );                    \
    HDassert( (entry_ptr) );                                                 \
    HDassert( !((entry_ptr)->is_protected) );                                \
    HDassert( !((entry_ptr)->is_read_only) );                                \
    HDassert( ((entry_ptr)->ro_ref_count) == 0 );                            \
    HDassert( !((entry_ptr)->is_pinned) );                                   \
    HDassert( (entry_ptr)->size > 0 );                                       \
    HDassert( (entry_ptr)->last_trans == 0 );                                \
                                                                             \
    /* modified LRU specific code */                                         \
                                                                             \
    /* remove the entry from the LRU list. */                                \
                                                                             \
    H5C2__DLL_REMOVE((entry_ptr), (cache_ptr)->LRU_head_ptr,                 \
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
        H5C2__AUX_DLL_REMOVE((entry_ptr), (cache_ptr)->dLRU_head_ptr,        \
                            (cache_ptr)->dLRU_tail_ptr,                      \
                            (cache_ptr)->dLRU_list_len,                      \
                            (cache_ptr)->dLRU_list_size, (fail_val))         \
    } else {                                                                 \
        H5C2__AUX_DLL_REMOVE((entry_ptr), (cache_ptr)->cLRU_head_ptr,        \
                            (cache_ptr)->cLRU_tail_ptr,                      \
                            (cache_ptr)->cLRU_list_len,                      \
                            (cache_ptr)->cLRU_list_size, (fail_val))         \
    }                                                                        \
                                                                             \
} /* H5C2__UPDATE_RP_FOR_EVICTION */

#else /* H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

#define H5C2__UPDATE_RP_FOR_EVICTION(cache_ptr, entry_ptr, fail_val)         \
{                                                                            \
    HDassert( (cache_ptr) );                                                 \
    HDassert( (cache_ptr)->magic == H5C2__H5C2_T_MAGIC );                    \
    HDassert( (entry_ptr) );                                                 \
    HDassert( !((entry_ptr)->is_protected) );                                \
    HDassert( !((entry_ptr)->is_read_only) );                                \
    HDassert( ((entry_ptr)->ro_ref_count) == 0 );                            \
    HDassert( !((entry_ptr)->is_pinned) );                                   \
    HDassert( (entry_ptr)->size > 0 );                                       \
    HDassert( (entry_ptr)->last_trans == 0 );                                \
                                                                             \
    /* modified LRU specific code */                                         \
                                                                             \
    /* remove the entry from the LRU list. */                                \
                                                                             \
    H5C2__DLL_REMOVE((entry_ptr), (cache_ptr)->LRU_head_ptr,                 \
                    (cache_ptr)->LRU_tail_ptr, (cache_ptr)->LRU_list_len,    \
                    (cache_ptr)->LRU_list_size, (fail_val))                  \
                                                                             \
} /* H5C2__UPDATE_RP_FOR_EVICTION */

#endif /* H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */


/*-------------------------------------------------------------------------
 *
 * Macro:	H5C2__UPDATE_RP_FOR_FLUSH
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
 *		Converted the function H5C2_update_rp_for_flush() to the
 *		macro H5C2__UPDATE_RP_FOR_FLUSH in an effort to squeeze
 *		a bit more performance out of the cache.
 *
 *		At least for the first cut, I am leaving the comments and
 *		white space in the macro.  If they cause dificulties with
 *		pre-processor, I'll have to remove them.
 *
 *		JRM - 7/28/04
 *		Split macro into two versions, one supporting the clean and
 *		dirty LRU lists, and the other not.  Yet another attempt
 *		at optimization.
 *
 *		JRM - 3/20/06
 *		While pinned entries can be flushed, they don't reside in
 *		the replacement policy data structures when unprotected.
 *		Thus I modified this macro to do nothing if the entry is
 *		pinned.
 *
 *		JRM - 3/28/07
 *		Added sanity checks based on the new is_read_only and
 *		ro_ref_count fields of struct H5C2_cache_entry_t.
 *
 *		JRM -- 3/29/08
 *		Added sanity check to verify that the flushed entry
 *		does not have a pending journal entry write.
 *
 *-------------------------------------------------------------------------
 */

#if H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS

#define H5C2__UPDATE_RP_FOR_FLUSH(cache_ptr, entry_ptr, fail_val)           \
{                                                                           \
    HDassert( (cache_ptr) );                                                \
    HDassert( (cache_ptr)->magic == H5C2__H5C2_T_MAGIC );                   \
    HDassert( (entry_ptr) );                                                \
    HDassert( !((entry_ptr)->is_protected) );                               \
    HDassert( !((entry_ptr)->is_read_only) );                               \
    HDassert( ((entry_ptr)->ro_ref_count) == 0 );                           \
    HDassert( (entry_ptr)->size > 0 );                                      \
    HDassert( (entry_ptr)->last_trans == 0 );                               \
                                                                            \
    if ( ! ((entry_ptr)->is_pinned) ) {                                     \
                                                                            \
        /* modified LRU specific code */                                    \
                                                                            \
        /* remove the entry from the LRU list, and re-insert it at the      \
	 * head.                                                            \
	 */                                                                 \
                                                                            \
        H5C2__DLL_REMOVE((entry_ptr), (cache_ptr)->LRU_head_ptr,            \
                        (cache_ptr)->LRU_tail_ptr,                          \
			(cache_ptr)->LRU_list_len,                          \
                        (cache_ptr)->LRU_list_size, (fail_val))             \
                                                                            \
        H5C2__DLL_PREPEND((entry_ptr), (cache_ptr)->LRU_head_ptr,           \
                         (cache_ptr)->LRU_tail_ptr,                         \
			 (cache_ptr)->LRU_list_len,                         \
                         (cache_ptr)->LRU_list_size, (fail_val))            \
                                                                            \
        /* since the entry is being flushed or cleared, one would think     \
	 * that it must be dirty -- but that need not be the case.  Use the \
	 * dirty flag to infer whether the entry is on the clean or dirty   \
	 * LRU list, and remove it.  Then insert it at the head of the      \
	 * clean LRU list.                                                  \
         *                                                                  \
         * The function presumes that a dirty entry will be either cleared  \
	 * or flushed shortly, so it is OK if we put a dirty entry on the   \
	 * clean LRU list.                                                  \
         */                                                                 \
                                                                            \
        if ( (entry_ptr)->is_dirty ) {                                      \
            H5C2__AUX_DLL_REMOVE((entry_ptr), (cache_ptr)->dLRU_head_ptr,   \
                                (cache_ptr)->dLRU_tail_ptr,                 \
                                (cache_ptr)->dLRU_list_len,                 \
                                (cache_ptr)->dLRU_list_size, (fail_val))    \
        } else {                                                            \
            H5C2__AUX_DLL_REMOVE((entry_ptr), (cache_ptr)->cLRU_head_ptr,   \
                                (cache_ptr)->cLRU_tail_ptr,                 \
                                (cache_ptr)->cLRU_list_len,                 \
                                (cache_ptr)->cLRU_list_size, (fail_val))    \
        }                                                                   \
                                                                            \
        H5C2__AUX_DLL_PREPEND((entry_ptr), (cache_ptr)->cLRU_head_ptr,      \
                             (cache_ptr)->cLRU_tail_ptr,                    \
                             (cache_ptr)->cLRU_list_len,                    \
                             (cache_ptr)->cLRU_list_size, (fail_val))       \
                                                                            \
        /* End modified LRU specific code. */                               \
    }                                                                       \
} /* H5C2__UPDATE_RP_FOR_FLUSH */

#else /* H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

#define H5C2__UPDATE_RP_FOR_FLUSH(cache_ptr, entry_ptr, fail_val)           \
{                                                                           \
    HDassert( (cache_ptr) );                                                \
    HDassert( (cache_ptr)->magic == H5C2__H5C2_T_MAGIC );                   \
    HDassert( (entry_ptr) );                                                \
    HDassert( !((entry_ptr)->is_protected) );                               \
    HDassert( !((entry_ptr)->is_read_only) );                               \
    HDassert( ((entry_ptr)->ro_ref_count) == 0 );                           \
    HDassert( (entry_ptr)->size > 0 );                                      \
    HDassert( (entry_ptr)->last_trans == 0 );                               \
                                                                            \
    if ( ! ((entry_ptr)->is_pinned) ) {                                     \
                                                                            \
        /* modified LRU specific code */                                    \
                                                                            \
        /* remove the entry from the LRU list, and re-insert it at the      \
	 * head.                                                            \
	 */                                                                 \
                                                                            \
        H5C2__DLL_REMOVE((entry_ptr), (cache_ptr)->LRU_head_ptr,            \
                        (cache_ptr)->LRU_tail_ptr,                          \
			(cache_ptr)->LRU_list_len,                          \
                        (cache_ptr)->LRU_list_size, (fail_val))             \
                                                                            \
        H5C2__DLL_PREPEND((entry_ptr), (cache_ptr)->LRU_head_ptr,           \
                         (cache_ptr)->LRU_tail_ptr,                         \
			 (cache_ptr)->LRU_list_len,                         \
                         (cache_ptr)->LRU_list_size, (fail_val))            \
                                                                            \
        /* End modified LRU specific code. */                               \
    }                                                                       \
} /* H5C2__UPDATE_RP_FOR_FLUSH */

#endif /* H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */


/*-------------------------------------------------------------------------
 *
 * Macro:	H5C2__UPDATE_RP_FOR_INSERTION
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
 *		Converted the function H5C2_update_rp_for_insertion() to the
 *		macro H5C2__UPDATE_RP_FOR_INSERTION in an effort to squeeze
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
 *		JRM - 3/10/06
 *		This macro should never be called on a pinned entry.
 *		Inserted an assert to verify this.
 *
 *		JRM - 8/9/06
 *		Not any more.  We must now allow insertion of pinned 
 *		entries.  Updated macro to support this.
 *
 *		JRM - 3/28/07
 *		Added sanity checks using the new is_read_only and
 *		ro_ref_count fields of struct H5C2_cache_entry_t.
 *
 *		JRM - 3/29/30
 *		Added sanity check that verifies that the last_trans field 
 *		of the entry matches the trans_num field of the cache.  
 *		Note that when journaling is disabled, both of these 
 *		fields should contain zero.  Also verify that either 
 *		journaling is disabled or a transaction is in progress.
 *
 *		Added code to put the entry in the journal write in 
 *		progress list if entries last_trans field is non-
 *		zero and the entry is not pinned.
 *
 *-------------------------------------------------------------------------
 */

#if H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS

#define H5C2__UPDATE_RP_FOR_INSERTION(cache_ptr, entry_ptr, fail_val)      \
{                                                                          \
    HDassert( (cache_ptr) );                                               \
    HDassert( (cache_ptr)->magic == H5C2__H5C2_T_MAGIC );                  \
    HDassert( (entry_ptr) );                                               \
    HDassert( !((entry_ptr)->is_protected) );                              \
    HDassert( !((entry_ptr)->is_read_only) );                              \
    HDassert( ((entry_ptr)->ro_ref_count) == 0 );                          \
    HDassert( (entry_ptr)->size > 0 );                                     \
    HDassert( (entry_ptr)->last_trans == (cache_ptr)->trans_num );         \
    HDassert( ( ! ((cache_ptr)->mdj_enabled) ) ||                          \
              ( (cache_ptr)->trans_in_progress ) );                        \
                                                                           \
    if ( (entry_ptr)->is_pinned ) {                                        \
                                                                           \
        H5C2__DLL_PREPEND((entry_ptr), (cache_ptr)->pel_head_ptr,          \
                         (cache_ptr)->pel_tail_ptr,                        \
                         (cache_ptr)->pel_len,                             \
                         (cache_ptr)->pel_size, (fail_val))                \
                                                                           \
    } else if ( (entry_ptr)->last_trans != 0 ) {                           \
	                                                                   \
        HDassert( (cache_ptr)->mdj_enabled );                              \
	HDassert( (cache_ptr)->trans_in_progress );                        \
        H5C2__DLL_PREPEND((entry_ptr),                                     \
		          ((cache_ptr)->jwipl_head_ptr),                   \
		          ((cache_ptr)->jwipl_tail_ptr),                   \
			  ((cache_ptr)->jwipl_len),                        \
			  ((cache_ptr)->jwipl_size), fail_val)             \
	                                                                   \
    } else {                                                               \
                                                                           \
        /* modified LRU specific code */                                   \
                                                                           \
        /* insert the entry at the head of the LRU list. */                \
                                                                           \
        H5C2__DLL_PREPEND((entry_ptr), (cache_ptr)->LRU_head_ptr,          \
                         (cache_ptr)->LRU_tail_ptr,                        \
			 (cache_ptr)->LRU_list_len,                        \
                         (cache_ptr)->LRU_list_size, (fail_val))           \
                                                                           \
        /* insert the entry at the head of the clean or dirty LRU list as  \
         * appropriate.                                                    \
         */                                                                \
                                                                           \
        if ( entry_ptr->is_dirty ) {                                       \
            H5C2__AUX_DLL_PREPEND((entry_ptr), (cache_ptr)->dLRU_head_ptr, \
                                 (cache_ptr)->dLRU_tail_ptr,               \
                                 (cache_ptr)->dLRU_list_len,               \
                                 (cache_ptr)->dLRU_list_size, (fail_val))  \
        } else {                                                           \
            H5C2__AUX_DLL_PREPEND((entry_ptr), (cache_ptr)->cLRU_head_ptr, \
                                 (cache_ptr)->cLRU_tail_ptr,               \
                                 (cache_ptr)->cLRU_list_len,               \
                                 (cache_ptr)->cLRU_list_size, (fail_val))  \
        }                                                                  \
                                                                           \
        /* End modified LRU specific code. */                              \
    }                                                                      \
}

#else /* H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

#define H5C2__UPDATE_RP_FOR_INSERTION(cache_ptr, entry_ptr, fail_val)      \
{                                                                          \
    HDassert( (cache_ptr) );                                               \
    HDassert( (cache_ptr)->magic == H5C2__H5C2_T_MAGIC );                  \
    HDassert( (entry_ptr) );                                               \
    HDassert( !((entry_ptr)->is_protected) );                              \
    HDassert( !((entry_ptr)->is_read_only) );                              \
    HDassert( ((entry_ptr)->ro_ref_count) == 0 );                          \
    HDassert( (entry_ptr)->size > 0 );                                     \
    HDassert( (entry_ptr)->last_trans == (cache_ptr)->trans_num );         \
    HDassert( ( ! ((cache_ptr)->mdj_enabled) ) ||                          \
              ( (cache_ptr)->trans_in_progress ) );                        \
                                                                           \
    if ( (entry_ptr)->is_pinned ) {                                        \
                                                                           \
        H5C2__DLL_PREPEND((entry_ptr), (cache_ptr)->pel_head_ptr,          \
                         (cache_ptr)->pel_tail_ptr,                        \
                         (cache_ptr)->pel_len,                             \
                         (cache_ptr)->pel_size, (fail_val))                \
	                                                                   \
    } else if ( (entry_ptr)->last_trans != 0 ) {                           \
	                                                                   \
        HDassert( (cache_ptr)->mdj_enabled );                              \
	HDassert( (cache_ptr)->trans_in_progress );                        \
        H5C2__DLL_PREPEND((entry_ptr),                                     \
		          ((cache_ptr)->jwipl_head_ptr),                   \
		          ((cache_ptr)->jwipl_tail_ptr),                   \
			  ((cache_ptr)->jwipl_len),                        \
			  ((cache_ptr)->jwipl_size), (fail_val))           \
                                                                           \
    } else {                                                               \
                                                                           \
        /* modified LRU specific code */                                   \
                                                                           \
        /* insert the entry at the head of the LRU list. */                \
                                                                           \
        H5C2__DLL_PREPEND((entry_ptr), (cache_ptr)->LRU_head_ptr,          \
                         (cache_ptr)->LRU_tail_ptr,                        \
			 (cache_ptr)->LRU_list_len,                        \
                         (cache_ptr)->LRU_list_size, (fail_val))           \
                                                                           \
        /* End modified LRU specific code. */                              \
    }                                                                      \
}

#endif /* H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */


/*-------------------------------------------------------------------------
 *
 * Macro:	H5C2__UPDATE_RP_FOR_JOURNAL_WRITE_COMPLETE
 *
 * Purpose:     Update the replacement policy data structures for the 
 *              completion of the last pending journal write for the 
 *              specified un-pinned and un-protected cache entry.
 *
 *		If an entry with a pending journal write is not protected
 *		and is not pinned, it must be on the journal write in 
 *		progress list.  Unlink it from that list, and add it to 
 *		the data structures used by the current replacement policy.
 *
 *		At present, we only support the modified LRU policy, so
 *		this function deals with that case unconditionally.  If
 *		we ever support other replacement policies, the function
 *		should switch on the current policy and act accordingly.
 *
 *		Note that the macro presumes that the entry's last_trans
 *		field is zero on entry.
 *
 * Return:      N/A
 *
 * Programmer:  John Mainzer, 3/31/08
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

#if H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS

#define H5C2__UPDATE_RP_FOR_JOURNAL_WRITE_COMPLETE(cache_ptr, entry_ptr,  \
		                                   fail_val)              \
{                                                                         \
    HDassert( (cache_ptr) );                                              \
    HDassert( (cache_ptr)->magic == H5C2__H5C2_T_MAGIC );                 \
    HDassert( (cache_ptr)->mdj_enabled );                                 \
    HDassert( (entry_ptr) );                                              \
    HDassert( !((entry_ptr)->is_protected) );                             \
    HDassert( !((entry_ptr)->is_pinned) );                                \
    HDassert( !((entry_ptr)->is_read_only) );                             \
    HDassert( ((entry_ptr)->ro_ref_count) == 0 );                         \
    HDassert( (entry_ptr)->size > 0 );                                    \
    HDassert( (entry_ptr)->is_dirty );                                    \
    HDassert( (entry_ptr)->last_trans == 0 );                             \
									  \
    H5C2__DLL_REMOVE((entry_ptr),                                         \
                     ((cache_ptr)->jwipl_head_ptr),                       \
                     ((cache_ptr)->jwipl_tail_ptr),                       \
                     ((cache_ptr)->jwipl_len),                            \
                     ((cache_ptr)->jwipl_size),                           \
                     (fail_val))                                          \
                                                                          \
    /* modified LRU specific code */                                      \
                                                                          \
    /* insert the entry at the head of the LRU list. */                   \
                                                                          \
    H5C2__DLL_PREPEND((entry_ptr), (cache_ptr)->LRU_head_ptr,             \
                      (cache_ptr)->LRU_tail_ptr,                          \
                      (cache_ptr)->LRU_list_len,                          \
                      (cache_ptr)->LRU_list_size, (fail_val))             \
                                                                          \
    /* insert the entry at the head of the clean or dirty LRU list as     \
     * appropriate.                                                       \
     */                                                                   \
                                                                          \
    if ( entry_ptr->is_dirty ) {                                          \
        H5C2__AUX_DLL_PREPEND((entry_ptr), (cache_ptr)->dLRU_head_ptr,    \
                              (cache_ptr)->dLRU_tail_ptr,                 \
                              (cache_ptr)->dLRU_list_len,                 \
                              (cache_ptr)->dLRU_list_size, (fail_val))    \
    } else {                                                              \
        H5C2__AUX_DLL_PREPEND((entry_ptr), (cache_ptr)->cLRU_head_ptr,    \
                              (cache_ptr)->cLRU_tail_ptr,                 \
                              (cache_ptr)->cLRU_list_len,                 \
                              (cache_ptr)->cLRU_list_size, (fail_val))    \
    }                                                                     \
                                                                          \
    /* End modified LRU specific code. */                                 \
                                                                          \
} /* H5C2__UPDATE_RP_FOR_JOURNAL_WRITE_COMPLETE */

#else /* H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

#define H5C2__UPDATE_RP_FOR_JOURNAL_WRITE_COMPLETE(cache_ptr, entry_ptr,  \
		                                   fail_val)              \
{                                                                         \
    HDassert( (cache_ptr) );                                              \
    HDassert( (cache_ptr)->magic == H5C2__H5C2_T_MAGIC );                 \
    HDassert( (cache_ptr)->mdj_enabled );                                 \
    HDassert( (entry_ptr) );                                              \
    HDassert( !((entry_ptr)->is_protected) );                             \
    HDassert( !((entry_ptr)->is_pinned) );                                \
    HDassert( !((entry_ptr)->is_read_only) );                             \
    HDassert( ((entry_ptr)->ro_ref_count) == 0 );                         \
    HDassert( (entry_ptr)->size > 0 );                                    \
    HDassert( (entry_ptr)->is_dirty );                                    \
    HDassert( (entry_ptr)->last_trans == 0 );                             \
									  \
    H5C2__DLL_REMOVE((entry_ptr),                                         \
                     ((cache_ptr)->jwipl_head_ptr),                       \
                     ((cache_ptr)->jwipl_tail_ptr),                       \
                     ((cache_ptr)->jwipl_len),                            \
                     ((cache_ptr)->jwipl_size),                           \
                     (fail_val))                                          \
                                                                          \
    /* modified LRU specific code */                                      \
                                                                          \
    /* insert the entry at the head of the LRU list. */                   \
                                                                          \
    H5C2__DLL_PREPEND((entry_ptr), (cache_ptr)->LRU_head_ptr,             \
                      (cache_ptr)->LRU_tail_ptr,                          \
                      (cache_ptr)->LRU_list_len,                          \
                      (cache_ptr)->LRU_list_size, (fail_val))             \
                                                                          \
    /* End modified LRU specific code. */                                 \
                                                                          \
} /* H5C2__UPDATE_RP_FOR_JOURNAL_WRITE_COMPLETE */

#endif /* H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */


/*-------------------------------------------------------------------------
 *
 * Macro:	H5C2__UPDATE_RP_FOR_LOAD
 *
 * Purpose:     Update the replacement policy data structures for a
 *		load from disk of the specified cache entry.
 *
 *		Note that we update the replacement policy for load only
 *		as a convenience -- the newly loaded entry will be 
 *		protected immediately.  If this starts to eat up a 
 *		significant number of cycles, we will have to re-work
 *		the code to avoid this step.
 *
 *		At present, we only support the modified LRU policy, so
 *		this function deals with that case unconditionally.  If
 *		we ever support other replacement policies, the function
 *		should switch on the current policy and act accordingly.
 *
 *		In theory, the loaded entry should be clean, but due
 *		to a bug fix, it is possible that we will dirty it during
 *		the load.
 *
 * Return:      N/A
 *
 * Programmer:  John Mainzer, 5/02/08
 *
 * Modifications:
 *
 * 		None.
 *
 *-------------------------------------------------------------------------
 */

#if H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS

#define H5C2__UPDATE_RP_FOR_LOAD(cache_ptr, entry_ptr, fail_val)           \
{                                                                          \
    HDassert( (cache_ptr) );                                               \
    HDassert( (cache_ptr)->magic == H5C2__H5C2_T_MAGIC );                  \
    HDassert( (entry_ptr) );                                               \
    HDassert( !((entry_ptr)->is_protected) );                              \
    HDassert( !((entry_ptr)->is_pinned) );                                 \
    HDassert( !((entry_ptr)->is_read_only) );                              \
    HDassert( ((entry_ptr)->ro_ref_count) == 0 );                          \
    HDassert( (entry_ptr)->size > 0 );                                     \
    HDassert( (entry_ptr)->last_trans == 0 );                              \
                                                                           \
    /* modified LRU specific code */                                       \
                                                                           \
    /* insert the entry at the head of the LRU list. */                    \
                                                                           \
    H5C2__DLL_PREPEND((entry_ptr), (cache_ptr)->LRU_head_ptr,              \
                      (cache_ptr)->LRU_tail_ptr,                           \
                      (cache_ptr)->LRU_list_len,                           \
                      (cache_ptr)->LRU_list_size, (fail_val))              \
                                                                           \
    /* insert the entry at the head of the clean or dirty LRU list as      \
     * appropriate.                                                        \
     */                                                                    \
                                                                           \
    if ( entry_ptr->is_dirty ) {                                           \
        H5C2__AUX_DLL_PREPEND((entry_ptr), (cache_ptr)->dLRU_head_ptr,     \
                              (cache_ptr)->dLRU_tail_ptr,                  \
                              (cache_ptr)->dLRU_list_len,                  \
                              (cache_ptr)->dLRU_list_size, (fail_val))     \
    } else {                                                               \
        H5C2__AUX_DLL_PREPEND((entry_ptr), (cache_ptr)->cLRU_head_ptr,     \
                              (cache_ptr)->cLRU_tail_ptr,                  \
                              (cache_ptr)->cLRU_list_len,                  \
                              (cache_ptr)->cLRU_list_size, (fail_val))     \
    }                                                                      \
                                                                           \
    /* End modified LRU specific code. */                                  \
}

#else /* H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

#define H5C2__UPDATE_RP_FOR_LOAD(cache_ptr, entry_ptr, fail_val)           \
{                                                                          \
    HDassert( (cache_ptr) );                                               \
    HDassert( (cache_ptr)->magic == H5C2__H5C2_T_MAGIC );                  \
    HDassert( (entry_ptr) );                                               \
    HDassert( !((entry_ptr)->is_protected) );                              \
    HDassert( !((entry_ptr)->is_pinned) );                                 \
    HDassert( !((entry_ptr)->is_read_only) );                              \
    HDassert( ((entry_ptr)->ro_ref_count) == 0 );                          \
    HDassert( (entry_ptr)->size > 0 );                                     \
    HDassert( (entry_ptr)->last_trans == 0 );                              \
                                                                           \
    /* modified LRU specific code */                                       \
                                                                           \
    /* insert the entry at the head of the LRU list. */                    \
                                                                           \
    H5C2__DLL_PREPEND((entry_ptr), (cache_ptr)->LRU_head_ptr,              \
                      (cache_ptr)->LRU_tail_ptr,                           \
                      (cache_ptr)->LRU_list_len,                           \
                      (cache_ptr)->LRU_list_size, (fail_val))              \
                                                                           \
    /* End modified LRU specific code. */                                  \
}

#endif /* H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */


/*-------------------------------------------------------------------------
 *
 * Macro:	H5C2__UPDATE_RP_FOR_PROTECT
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
 *		Converted the function H5C2_update_rp_for_protect() to the
 *		macro H5C2__UPDATE_RP_FOR_PROTECT in an effort to squeeze
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
 *		JRM - 3/17/06
 *		Modified macro to attempt to remove pinned entriese from
 *		the pinned entry list instead of from the data structures
 *		maintained by the replacement policy.
 *
 *		JRM - 3/28/07
 *		Added sanity checks based on the new is_read_only and 
 *		ro_ref_count fields of struct H5C2_cache_entry_t.
 *
 *		JRM - 3/29/08
 *		Added code to remove the entry from the journal write in
 *		progress list if appropriate.  Also related sanity checks.
 *
 *-------------------------------------------------------------------------
 */

#if H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS

#define H5C2__UPDATE_RP_FOR_PROTECT(cache_ptr, entry_ptr, fail_val)       \
{                                                                         \
    HDassert( (cache_ptr) );                                              \
    HDassert( (cache_ptr)->magic == H5C2__H5C2_T_MAGIC );                 \
    HDassert( (entry_ptr) );                                              \
    HDassert( !((entry_ptr)->is_protected) );                             \
    HDassert( !((entry_ptr)->is_read_only) );                             \
    HDassert( ((entry_ptr)->ro_ref_count) == 0 );                         \
    HDassert( (entry_ptr)->size > 0 );                                    \
									  \
    if ( (entry_ptr)->is_pinned ) {                                       \
                                                                          \
        H5C2__DLL_REMOVE((entry_ptr), (cache_ptr)->pel_head_ptr,          \
                        (cache_ptr)->pel_tail_ptr, 			  \
			(cache_ptr)->pel_len,                             \
                        (cache_ptr)->pel_size, (fail_val))                \
	                                                                  \
    } else if ( (entry_ptr)->last_trans != 0 ) {                          \
	                                                                  \
        HDassert( (cache_ptr)->mdj_enabled );                             \
        HDassert( (entry_ptr)->is_dirty );                                \
        H5C2__DLL_REMOVE((entry_ptr),                                     \
                         ((cache_ptr)->jwipl_head_ptr),                   \
                         ((cache_ptr)->jwipl_tail_ptr),                   \
                         ((cache_ptr)->jwipl_len),                        \
                         ((cache_ptr)->jwipl_size),                       \
                         (fail_val))                                      \
                                                                          \
    } else {                                                              \
                                                                          \
        /* modified LRU specific code */                                  \
                                                                          \
        /* remove the entry from the LRU list. */                         \
                                                                          \
        H5C2__DLL_REMOVE((entry_ptr), (cache_ptr)->LRU_head_ptr,          \
                        (cache_ptr)->LRU_tail_ptr,                        \
			(cache_ptr)->LRU_list_len,                        \
                        (cache_ptr)->LRU_list_size, (fail_val))           \
                                                                          \
        /* Similarly, remove the entry from the clean or dirty LRU list   \
         * as appropriate.                                                \
         */                                                               \
                                                                          \
        if ( (entry_ptr)->is_dirty ) {                                    \
                                                                          \
            H5C2__AUX_DLL_REMOVE((entry_ptr), (cache_ptr)->dLRU_head_ptr, \
                                (cache_ptr)->dLRU_tail_ptr,               \
                                (cache_ptr)->dLRU_list_len,               \
                                (cache_ptr)->dLRU_list_size, (fail_val))  \
                                                                          \
        } else {                                                          \
                                                                          \
            H5C2__AUX_DLL_REMOVE((entry_ptr), (cache_ptr)->cLRU_head_ptr, \
                                (cache_ptr)->cLRU_tail_ptr,               \
                                (cache_ptr)->cLRU_list_len,               \
                                (cache_ptr)->cLRU_list_size, (fail_val))  \
        }                                                                 \
                                                                          \
        /* End modified LRU specific code. */                             \
    }                                                                     \
                                                                          \
    /* Regardless of the replacement policy, or whether the entry is      \
     * pinned, now add the entry to the protected list.                   \
     */                                                                   \
                                                                          \
    H5C2__DLL_APPEND((entry_ptr), (cache_ptr)->pl_head_ptr,               \
                    (cache_ptr)->pl_tail_ptr,                             \
                    (cache_ptr)->pl_len,                                  \
                    (cache_ptr)->pl_size, (fail_val))                     \
} /* H5C2__UPDATE_RP_FOR_PROTECT */

#else /* H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

#define H5C2__UPDATE_RP_FOR_PROTECT(cache_ptr, entry_ptr, fail_val)       \
{                                                                         \
    HDassert( (cache_ptr) );                                              \
    HDassert( (cache_ptr)->magic == H5C2__H5C2_T_MAGIC );                 \
    HDassert( (entry_ptr) );                                              \
    HDassert( !((entry_ptr)->is_protected) );                             \
    HDassert( !((entry_ptr)->is_read_only) );                             \
    HDassert( ((entry_ptr)->ro_ref_count) == 0 );                         \
    HDassert( (entry_ptr)->size > 0 );                                    \
									  \
    if ( (entry_ptr)->is_pinned ) {                                       \
                                                                          \
        H5C2__DLL_REMOVE((entry_ptr), (cache_ptr)->pel_head_ptr,          \
                        (cache_ptr)->pel_tail_ptr, 			  \
			(cache_ptr)->pel_len,                             \
                        (cache_ptr)->pel_size, (fail_val))                \
                                                                          \
    } else if ( (entry_ptr)->last_trans != 0 ) {                          \
	                                                                  \
        HDassert( (cache_ptr)->mdj_enabled );                             \
        HDassert( (entry_ptr)->is_dirty );                                \
        H5C2__DLL_REMOVE((entry_ptr),                                     \
                         ((cache_ptr)->jwipl_head_ptr),                   \
                         ((cache_ptr)->jwipl_tail_ptr),                   \
                         ((cache_ptr)->jwipl_len),                        \
                         ((cache_ptr)->jwipl_size),                       \
                         (fail_val))                                      \
                                                                          \
    } else {                                                              \
                                                                          \
        /* modified LRU specific code */                                  \
                                                                          \
        /* remove the entry from the LRU list. */                         \
                                                                          \
        H5C2__DLL_REMOVE((entry_ptr), (cache_ptr)->LRU_head_ptr,          \
                        (cache_ptr)->LRU_tail_ptr,                        \
			(cache_ptr)->LRU_list_len,                        \
                        (cache_ptr)->LRU_list_size, (fail_val))           \
                                                                          \
        /* End modified LRU specific code. */                             \
    }                                                                     \
                                                                          \
    /* Regardless of the replacement policy, or whether the entry is      \
     * pinned, now add the entry to the protected list.                   \
     */                                                                   \
                                                                          \
    H5C2__DLL_APPEND((entry_ptr), (cache_ptr)->pl_head_ptr,               \
                     (cache_ptr)->pl_tail_ptr,                            \
                     (cache_ptr)->pl_len,                                 \
                     (cache_ptr)->pl_size, (fail_val))                    \
} /* H5C2__UPDATE_RP_FOR_PROTECT */

#endif /* H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */


/*-------------------------------------------------------------------------
 *
 * Macro:	H5C2__UPDATE_RP_FOR_RENAME
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
 *		Converted the function H5C2_update_rp_for_rename() to the
 *		macro H5C2__UPDATE_RP_FOR_RENAME in an effort to squeeze
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
 *		JRM - 6/23/05
 *		Added the was_dirty parameter.  It is possible that
 *		the entry was clean when it was renamed -- if so it
 *		it is in the clean LRU regardless of the current
 *		value of the is_dirty field.
 *
 *		At present, all renamed entries are forced to be
 *		dirty.  This macro is a bit more general that that,
 *		to allow it to function correctly should that policy
 *		be relaxed in the future.
 *
 *		JRM - 3/17/06
 *		Modified macro to do nothing if the entry is pinned.
 *		In this case, the entry is on the pinned entry list, not
 *		in the replacement policy data structures, so there is
 *		nothing to be done.
 *
 *		JRM - 3/28/07
 *		Added sanity checks using the new is_read_only and 
 *		ro_ref_count fields of struct H5C2_cache_entry_t.
 *
 *		JRM - 3/29/08
 *		Reworked macro to handle the case in which the renamed
 *		entry has a journal write pending -- this required the
 *		addition of the had_jwip parameter.  Also added some 
 *		related sanity checks.  
 *
 *-------------------------------------------------------------------------
 */

#if H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS

#define H5C2__UPDATE_RP_FOR_RENAME(cache_ptr, entry_ptr, was_dirty,          \
		                   had_jwip, fail_val)                       \
{                                                                            \
    HDassert( (cache_ptr) );                                                 \
    HDassert( (cache_ptr)->magic == H5C2__H5C2_T_MAGIC );                    \
    HDassert( (entry_ptr) );                                                 \
    HDassert( !((entry_ptr)->is_protected) );                                \
    HDassert( !((entry_ptr)->is_read_only) );                                \
    HDassert( ((entry_ptr)->ro_ref_count) == 0 );                            \
    HDassert( (entry_ptr)->size > 0 );                                       \
    /* read following as: had_jwip => (entry_ptr)->last_trans != 0 */        \
    HDassert( ( (entry_ptr)->last_trans != 0 ) || ( ! (had_jwip) ) );        \
                                                                             \
    if ( ! ( (entry_ptr)->is_pinned ) ) {                                    \
	                                                                     \
	/* remove the entry from either the jwip list, or from the current   \
	 * replacement policy data structures, as appropriate.               \
	 */                                                                  \
	if ( had_jwip ) { /* must be on jwip list */                         \
	                                                                     \
            HDassert( (cache_ptr)->mdj_enabled );                            \
	    HDassert( (cache_ptr)->trans_in_progress );                      \
	    HDassert( (entry_ptr)->last_trans != 0 );                        \
	    HDassert( was_dirty );                                           \
	    HDassert( (entry_ptr)->is_dirty );                               \
            H5C2__DLL_REMOVE((entry_ptr),                                    \
	  		     ((cache_ptr)->jwipl_head_ptr),                  \
                             ((cache_ptr)->jwipl_tail_ptr),                  \
                             ((cache_ptr)->jwipl_len),                       \
                             ((cache_ptr)->jwipl_size),                      \
			     (fail_val))                                     \
                                                                             \
	} else { /* must be in replacement policy data structures */         \
		                                                             \
            /* begin modified LRU specific code */                           \
                                                                             \
            /* remove the entry from the LRU list */                         \
                                                                             \
            H5C2__DLL_REMOVE((entry_ptr), (cache_ptr)->LRU_head_ptr,         \
                             (cache_ptr)->LRU_tail_ptr,                      \
			     (cache_ptr)->LRU_list_len,                      \
                             (cache_ptr)->LRU_list_size, (fail_val))         \
                                                                             \
            /* remove the entry from either the clean or dirty LUR list as   \
             * indicated by the was_dirty parameter                          \
             */                                                              \
            if ( was_dirty ) {                                               \
                                                                             \
                H5C2__AUX_DLL_REMOVE((entry_ptr),                            \
				     (cache_ptr)->dLRU_head_ptr,             \
                                     (cache_ptr)->dLRU_tail_ptr,             \
                                     (cache_ptr)->dLRU_list_len,             \
                                     (cache_ptr)->dLRU_list_size,            \
				     (fail_val))                             \
                                                                             \
            } else {                                                         \
                                                                             \
                H5C2__AUX_DLL_REMOVE((entry_ptr),                            \
				     (cache_ptr)->cLRU_head_ptr,             \
                                     (cache_ptr)->cLRU_tail_ptr,             \
                                     (cache_ptr)->cLRU_list_len,             \
                                     (cache_ptr)->cLRU_list_size,            \
				     (fail_val))                             \
            }                                                                \
            /* end modified LRU specific code */                             \
	}                                                                    \
	/* insert in either jwip list, or replacement policy data structures \
	 * as appropriate.                                                   \
	 */                                                                  \
	if ( (entry_ptr)->last_trans != 0 ) { /* goes in jwip list */        \
	                                                                     \
            HDassert( (cache_ptr)->mdj_enabled );                            \
	    HDassert( (cache_ptr)->trans_in_progress );                      \
	    HDassert( (entry_ptr)->is_dirty );                               \
            H5C2__DLL_PREPEND((entry_ptr),                                   \
		              ((cache_ptr)->jwipl_head_ptr),                 \
		              ((cache_ptr)->jwipl_tail_ptr),                 \
                              ((cache_ptr)->jwipl_len),                      \
			      ((cache_ptr)->jwipl_size), fail_val)           \
	                                                                     \
        } else { /* goes back in the replacement policy data structures */   \
		                                                             \
            /* begin modified LRU specific code */                           \
	                                                                     \
            H5C2__DLL_PREPEND((entry_ptr), (cache_ptr)->LRU_head_ptr,        \
                              (cache_ptr)->LRU_tail_ptr,                     \
			      (cache_ptr)->LRU_list_len,                     \
                              (cache_ptr)->LRU_list_size, (fail_val))        \
                                                                             \
            /* insert the entry at the head of either the clean or dirty     \
	     * LRU list as appropriate.                                      \
             */                                                              \
                                                                             \
            if ( (entry_ptr)->is_dirty ) {                                   \
                                                                             \
                H5C2__AUX_DLL_PREPEND((entry_ptr),                           \
				      (cache_ptr)->dLRU_head_ptr,            \
                                      (cache_ptr)->dLRU_tail_ptr,            \
                                      (cache_ptr)->dLRU_list_len,            \
                                      (cache_ptr)->dLRU_list_size,           \
				      (fail_val))                            \
                                                                             \
            } else {                                                         \
                                                                             \
                H5C2__AUX_DLL_PREPEND((entry_ptr),                           \
				      (cache_ptr)->cLRU_head_ptr,            \
                                      (cache_ptr)->cLRU_tail_ptr,            \
                                      (cache_ptr)->cLRU_list_len,            \
                                      (cache_ptr)->cLRU_list_size,           \
				      (fail_val))                            \
            }                                                                \
                                                                             \
            /* End modified LRU specific code. */                            \
        }                                                                    \
    }                                                                        \
} /* H5C2__UPDATE_RP_FOR_RENAME */

#else /* H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

#define H5C2__UPDATE_RP_FOR_RENAME(cache_ptr, entry_ptr, was_dirty,          \
		                   had_jwip, fail_val)                       \
{                                                                            \
    HDassert( (cache_ptr) );                                                 \
    HDassert( (cache_ptr)->magic == H5C2__H5C2_T_MAGIC );                    \
    HDassert( (entry_ptr) );                                                 \
    HDassert( !((entry_ptr)->is_protected) );                                \
    HDassert( !((entry_ptr)->is_read_only) );                                \
    HDassert( ((entry_ptr)->ro_ref_count) == 0 );                            \
    HDassert( (entry_ptr)->size > 0 );                                       \
    /* read following as: had_jwip => (entry_ptr)->last_trans != 0 */        \
    HDassert( ( (entry_ptr)->last_trans != 0 ) || ( ! (had_jwip) ) );        \
                                                                             \
    if ( ! ( (entry_ptr)->is_pinned ) ) {                                    \
	                                                                     \
	/* remove the entry from either the jwip list, or from the current   \
	 * replacement policy data structures, as appropriate.               \
	 */                                                                  \
	if ( had_jwip ) { /* must be on jwip list */                         \
	                                                                     \
            HDassert( (cache_ptr)->mdj_enabled );                            \
	    HDassert( (cache_ptr)->trans_in_progress );                      \
	    HDassert( (entry_ptr)->last_trans != 0 );                        \
	    HDassert( was_dirty );                                           \
	    HDassert( (entry_ptr)->is_dirty );                               \
            H5C2__DLL_REMOVE((entry_ptr),                                    \
	  	             ((cache_ptr)->jwipl_head_ptr),                  \
                             ((cache_ptr)->jwipl_tail_ptr),                  \
                             ((cache_ptr)->jwipl_len),                       \
                             ((cache_ptr)->jwipl_size),                      \
			     (fail_val))                                     \
                                                                             \
	} else { /* must be in replacement policy data structures */         \
		                                                             \
            /* begin modified LRU specific code */                           \
                                                                             \
            /* remove the entry from the LRU list */                         \
                                                                             \
            H5C2__DLL_REMOVE((entry_ptr), (cache_ptr)->LRU_head_ptr,         \
                             (cache_ptr)->LRU_tail_ptr,                      \
			     (cache_ptr)->LRU_list_len,                      \
                             (cache_ptr)->LRU_list_size, (fail_val))         \
                                                                             \
            /* end modified LRU specific code */                             \
	}                                                                    \
	/* insert in either jwip list, or replacement policy data            \
	 * structures as appropriate.                                        \
	 */                                                                  \
	if ( (entry_ptr)->last_trans != 0 ) { /* goes in jwip list */        \
	                                                                     \
            HDassert( (cache_ptr)->mdj_enabled );                            \
	    HDassert( (cache_ptr)->trans_in_progress );                      \
	    HDassert( (entry_ptr)->is_dirty );                               \
            H5C2__DLL_PREPEND((entry_ptr),                                   \
                              ((cache_ptr)->jwipl_head_ptr),                 \
                              ((cache_ptr)->jwipl_tail_ptr),                 \
                              ((cache_ptr)->jwipl_len),                      \
                              ((cache_ptr)->jwipl_size), fail_val)           \
	                                                                     \
        } else { /* goes back in the replacement policy data structures */   \
		                                                             \
            /* begin modified LRU specific code */                           \
	                                                                     \
            H5C2__DLL_PREPEND((entry_ptr), (cache_ptr)->LRU_head_ptr,        \
                              (cache_ptr)->LRU_tail_ptr,                     \
			      (cache_ptr)->LRU_list_len,                     \
                              (cache_ptr)->LRU_list_size, (fail_val))        \
                                                                             \
            /* End modified LRU specific code. */                            \
        }                                                                    \
    }                                                                        \
} /* H5C2__UPDATE_RP_FOR_RENAME */

#endif /* H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */


/*-------------------------------------------------------------------------
 *
 * Macro:	H5C2__UPDATE_RP_FOR_SIZE_CHANGE
 *
 * Purpose:     Update the replacement policy data structures for a
 *		size change of the specified cache entry.
 *
 *		To do this, determine if the entry is pinned.  If it is,
 *		update the size of the pinned entry list.
 *
 *		If it isn't pinned, the entry must handled by the 
 *		replacement policy.  Update the appropriate replacement
 *		policy data structures.
 *
 *		At present, we only support the modified LRU policy, so
 *		this function deals with that case unconditionally.  If
 *		we ever support other replacement policies, the function
 *		should switch on the current policy and act accordingly.
 *
 * Return:      N/A
 *
 * Programmer:  John Mainzer, 8/23/06
 *
 * Modifications:
 *
 * 		JRM -- 3/28/07
 *		Added sanity checks based on the new is_read_only and 
 *		ro_ref_count fields of struct H5C2_cache_entry_t.
 *
 *		JRM -- 3/29/08
 *		Added code to deal with the journal write in progress
 *		list -- in essence, after checking to see if the entry is
 *		pinned, check to see if it is on the jwip list.  If it 
 *		is, update the size of that list.  If not, proceed as
 *		before.
 *
 *-------------------------------------------------------------------------
 */

#if H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS

#define H5C2__UPDATE_RP_FOR_SIZE_CHANGE(cache_ptr, entry_ptr, new_size)   \
{                                                                         \
    HDassert( (cache_ptr) );                                              \
    HDassert( (cache_ptr)->magic == H5C2__H5C2_T_MAGIC );                 \
    HDassert( (entry_ptr) );                                              \
    HDassert( !((entry_ptr)->is_protected) );                             \
    HDassert( !((entry_ptr)->is_read_only) );                             \
    HDassert( ((entry_ptr)->ro_ref_count) == 0 );                         \
    HDassert( (entry_ptr)->size > 0 );                                    \
    HDassert( new_size > 0 );                                             \
				  					  \
    if ( (entry_ptr)->is_pinned ) {                                       \
                                                                          \
	H5C2__DLL_UPDATE_FOR_SIZE_CHANGE((cache_ptr)->pel_len,            \
			                (cache_ptr)->pel_size,            \
			                (entry_ptr)->size,                \
					(new_size));                      \
	                                                                  \
    } else if ( (entry_ptr)->last_trans != 0 ) {                          \
                                                                          \
        HDassert( (cache_ptr)->mdj_enabled );                             \
	HDassert( (entry_ptr)->is_dirty );                                \
	H5C2__DLL_UPDATE_FOR_SIZE_CHANGE((cache_ptr)->jwipl_len,          \
			                (cache_ptr)->jwipl_size,          \
			                (entry_ptr)->size,                \
					(new_size));                      \
                                                                          \
    } else {                                                              \
                                                                          \
        /* modified LRU specific code */                                  \
                                                                          \
	/* Update the size of the LRU list */                             \
                                                                          \
	H5C2__DLL_UPDATE_FOR_SIZE_CHANGE((cache_ptr)->LRU_list_len,       \
			                (cache_ptr)->LRU_list_size,       \
			                (entry_ptr)->size,                \
					(new_size));                      \
                                                                          \
        /* Similarly, update the size of the clean or dirty LRU list as   \
	 * appropriate.  At present, the entry must be clean, but that    \
	 * could change.                                                  \
         */                                                               \
                                                                          \
        if ( (entry_ptr)->is_dirty ) {                                    \
                                                                          \
	    H5C2__DLL_UPDATE_FOR_SIZE_CHANGE((cache_ptr)->dLRU_list_len,  \
			                    (cache_ptr)->dLRU_list_size,  \
			                    (entry_ptr)->size,            \
					    (new_size));                  \
                                                                          \
        } else {                                                          \
                                                                          \
	    H5C2__DLL_UPDATE_FOR_SIZE_CHANGE((cache_ptr)->cLRU_list_len,  \
			                    (cache_ptr)->cLRU_list_size,  \
			                    (entry_ptr)->size,            \
					    (new_size));                  \
        }                                                                 \
                                                                          \
        /* End modified LRU specific code. */                             \
    }                                                                     \
                                                                          \
} /* H5C2__UPDATE_RP_FOR_SIZE_CHANGE */

#else /* H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

#define H5C2__UPDATE_RP_FOR_SIZE_CHANGE(cache_ptr, entry_ptr, new_size)   \
{                                                                         \
    HDassert( (cache_ptr) );                                              \
    HDassert( (cache_ptr)->magic == H5C2__H5C2_T_MAGIC );                 \
    HDassert( (entry_ptr) );                                              \
    HDassert( !((entry_ptr)->is_protected) );                             \
    HDassert( !((entry_ptr)->is_read_only) );                             \
    HDassert( ((entry_ptr)->ro_ref_count) == 0 );                         \
    HDassert( (entry_ptr)->size > 0 );                                    \
    HDassert( new_size > 0 );                                             \
				  					  \
    if ( (entry_ptr)->is_pinned ) {                                       \
                                                                          \
	H5C2__DLL_UPDATE_FOR_SIZE_CHANGE((cache_ptr)->pel_len,            \
			                (cache_ptr)->pel_size,            \
			                (entry_ptr)->size,                \
					(new_size));                      \
                                                                          \
    } else if ( (entry_ptr)->last_trans != 0 ) {                          \
                                                                          \
        HDassert( (cache_ptr)->mdj_enabled );                             \
	HDassert( (entry_ptr)->is_dirty );                                \
	H5C2__DLL_UPDATE_FOR_SIZE_CHANGE((cache_ptr)->jwipl_len,          \
			                (cache_ptr)->jwipl_size,          \
			                (entry_ptr)->size,                \
					(new_size));                      \
                                                                          \
    } else {                                                              \
                                                                          \
        /* modified LRU specific code */                                  \
                                                                          \
	/* Update the size of the LRU list */                             \
                                                                          \
	H5C2__DLL_UPDATE_FOR_SIZE_CHANGE((cache_ptr)->LRU_list_len,       \
			                (cache_ptr)->LRU_list_size,       \
			                (entry_ptr)->size,                \
					(new_size));                      \
                                                                          \
        /* End modified LRU specific code. */                             \
    }                                                                     \
                                                                          \
} /* H5C2__UPDATE_RP_FOR_SIZE_CHANGE */

#endif /* H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */


/*-------------------------------------------------------------------------
 *
 * Macro:	H5C2__UPDATE_RP_FOR_UNPIN
 *
 * Purpose:     Update the replacement policy data structures for an
 *		unpin of the specified cache entry.
 *
 *		To do this, unlink the specified entry from the protected
 *		entry list, and re-insert it in the data structures used
 *		by the current replacement policy.
 *
 *		At present, we only support the modified LRU policy, so
 *		this function deals with that case unconditionally.  If
 *		we ever support other replacement policies, the macro
 *		should switch on the current policy and act accordingly.
 *
 * Return:      N/A
 *
 * Programmer:  John Mainzer, 3/22/06
 *
 * Modifications:
 *
 *		JRM -- 3/28/07
 *		Added sanity checks based on the new is_read_only and 
 *		ro_ref_count fields of struct H5C2_cache_entry_t.
 *
 *		JRM -- 3/30/08
 *		Added code to place the newly unpinned entry on the 
 *		journal write pending list if appropriate.
 *
 *-------------------------------------------------------------------------
 */

#if H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS

#define H5C2__UPDATE_RP_FOR_UNPIN(cache_ptr, entry_ptr, fail_val)      \
{                                                                      \
    HDassert( (cache_ptr) );                                           \
    HDassert( (cache_ptr)->magic == H5C2__H5C2_T_MAGIC );              \
    HDassert( (entry_ptr) );                                           \
    HDassert( !((entry_ptr)->is_protected) );                          \
    HDassert( !((entry_ptr)->is_read_only) );                          \
    HDassert( ((entry_ptr)->ro_ref_count) == 0 );                      \
    HDassert( (entry_ptr)->is_pinned);                                 \
    HDassert( (entry_ptr)->size > 0 );                                 \
                                                                       \
    /* Regardless of the replacement policy, remove the entry from the \
     * pinned entry list.                                              \
     */                                                                \
    H5C2__DLL_REMOVE((entry_ptr), (cache_ptr)->pel_head_ptr,           \
                    (cache_ptr)->pel_tail_ptr, (cache_ptr)->pel_len,   \
                    (cache_ptr)->pel_size, (fail_val))                 \
                                                                       \
    if ( (entry_ptr)->last_trans != 0 ) {                              \
                                                                       \
        /* put the entry in the jwip list */                           \
        HDassert( (cache_ptr)->mdj_enabled );                          \
        HDassert( (entry_ptr)->is_dirty );                             \
        H5C2__DLL_PREPEND((entry_ptr),                                 \
                          ((cache_ptr)->jwipl_head_ptr),               \
                          ((cache_ptr)->jwipl_tail_ptr),               \
                          ((cache_ptr)->jwipl_len),                    \
                          ((cache_ptr)->jwipl_size), fail_val)         \
                                                                       \
    } else { /* put entry in the replacement policy data structures */ \
                                                                       \
        /* modified LRU specific code */                               \
                                                                       \
        /* insert the entry at the head of the LRU list. */            \
                                                                       \
        H5C2__DLL_PREPEND((entry_ptr), (cache_ptr)->LRU_head_ptr,      \
                         (cache_ptr)->LRU_tail_ptr,                    \
                         (cache_ptr)->LRU_list_len,                    \
                         (cache_ptr)->LRU_list_size, (fail_val))       \
                                                                       \
        /* Similarly, insert the entry at the head of either the clean \
         * or dirty LRU list as appropriate.                           \
         */                                                            \
                                                                       \
        if ( (entry_ptr)->is_dirty ) {                                 \
                                                                       \
            H5C2__AUX_DLL_PREPEND((entry_ptr),                         \
			          (cache_ptr)->dLRU_head_ptr,          \
                                  (cache_ptr)->dLRU_tail_ptr,          \
                                  (cache_ptr)->dLRU_list_len,          \
                                  (cache_ptr)->dLRU_list_size,         \
			          (fail_val))                          \
                                                                       \
        } else {                                                       \
                                                                       \
            H5C2__AUX_DLL_PREPEND((entry_ptr),                         \
			          (cache_ptr)->cLRU_head_ptr,          \
                                  (cache_ptr)->cLRU_tail_ptr,          \
                                  (cache_ptr)->cLRU_list_len,          \
                                  (cache_ptr)->cLRU_list_size,         \
			          (fail_val))                          \
         }                                                             \
                                                                       \
        /* End modified LRU specific code. */                          \
    }                                                                  \
                                                                       \
} /* H5C2__UPDATE_RP_FOR_UNPIN */

#else /* H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

#define H5C2__UPDATE_RP_FOR_UNPIN(cache_ptr, entry_ptr, fail_val)      \
{                                                                      \
    HDassert( (cache_ptr) );                                           \
    HDassert( (cache_ptr)->magic == H5C2__H5C2_T_MAGIC );              \
    HDassert( (entry_ptr) );                                           \
    HDassert( !((entry_ptr)->is_protected) );                          \
    HDassert( !((entry_ptr)->is_read_only) );                          \
    HDassert( ((entry_ptr)->ro_ref_count) == 0 );                      \
    HDassert( (entry_ptr)->is_pinned);                                 \
    HDassert( (entry_ptr)->size > 0 );                                 \
                                                                       \
    /* Regardless of the replacement policy, remove the entry from the \
     * pinned entry list.                                              \
     */                                                                \
    H5C2__DLL_REMOVE((entry_ptr), (cache_ptr)->pel_head_ptr,           \
                    (cache_ptr)->pel_tail_ptr, (cache_ptr)->pel_len,   \
                    (cache_ptr)->pel_size, (fail_val))                 \
                                                                       \
    if ( (entry_ptr)->last_trans != 0 ) {                              \
                                                                       \
        /* put the entry in the jwip list */                           \
        HDassert( (cache_ptr)->mdj_enabled );                          \
        HDassert( (entry_ptr)->is_dirty );                             \
        H5C2__DLL_PREPEND((entry_ptr),                                 \
                          ((cache_ptr)->jwipl_head_ptr),               \
                          ((cache_ptr)->jwipl_tail_ptr),               \
                          ((cache_ptr)->jwipl_len),                    \
                          ((cache_ptr)->jwipl_size), fail_val)         \
                                                                       \
    } else { /* put entry in the replacement policy data structures */ \
                                                                       \
        /* modified LRU specific code */                               \
                                                                       \
        /* insert the entry at the head of the LRU list. */            \
                                                                       \
        H5C2__DLL_PREPEND((entry_ptr), (cache_ptr)->LRU_head_ptr,      \
                         (cache_ptr)->LRU_tail_ptr,                    \
                         (cache_ptr)->LRU_list_len,                    \
                         (cache_ptr)->LRU_list_size, (fail_val))       \
                                                                       \
        /* End modified LRU specific code. */                          \
    }                                                                  \
                                                                       \
} /* H5C2__UPDATE_RP_FOR_UNPIN */

#endif /* H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */


/*-------------------------------------------------------------------------
 *
 * Macro:	H5C2__UPDATE_RP_FOR_UNPROTECT
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
 *		Converted the function H5C2_update_rp_for_unprotect() to
 *		the macro H5C2__UPDATE_RP_FOR_UNPROTECT in an effort to
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
 *		JRM - 3/17/06
 *		Modified macro to put pinned entries on the pinned entry
 *		list instead of inserting them in the data structures
 *		maintained by the replacement policy.
 *
 *		JRM - 3/30/08
 *		Modified macro to put un-pinned entries with pending 
 *		journal writes on the journal write in progress list.
 *
 *-------------------------------------------------------------------------
 */

#if H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS

#define H5C2__UPDATE_RP_FOR_UNPROTECT(cache_ptr, entry_ptr, fail_val)      \
{                                                                          \
    HDassert( (cache_ptr) );                                               \
    HDassert( (cache_ptr)->magic == H5C2__H5C2_T_MAGIC );                  \
    HDassert( (entry_ptr) );                                               \
    HDassert( (entry_ptr)->is_protected);                                  \
    HDassert( (entry_ptr)->size > 0 );                                     \
                                                                           \
    /* Regardless of the replacement policy, remove the entry from the     \
     * protected list.                                                     \
     */                                                                    \
    H5C2__DLL_REMOVE((entry_ptr), (cache_ptr)->pl_head_ptr,                \
                    (cache_ptr)->pl_tail_ptr, (cache_ptr)->pl_len,         \
                    (cache_ptr)->pl_size, (fail_val))                      \
                                                                           \
    if ( (entry_ptr)->is_pinned ) {                                        \
                                                                           \
        H5C2__DLL_PREPEND((entry_ptr), (cache_ptr)->pel_head_ptr,          \
                          (cache_ptr)->pel_tail_ptr,                       \
                          (cache_ptr)->pel_len,                            \
                          (cache_ptr)->pel_size, (fail_val))               \
                                                                           \
    } else if ( (entry_ptr)->last_trans != 0 ) {                           \
                                                                           \
        /* put the entry in the jwip list */                               \
        HDassert( (cache_ptr)->mdj_enabled );                              \
        HDassert( (entry_ptr)->is_dirty );                                 \
        H5C2__DLL_PREPEND((entry_ptr),                                     \
                          ((cache_ptr)->jwipl_head_ptr),                   \
                          ((cache_ptr)->jwipl_tail_ptr),                   \
                          ((cache_ptr)->jwipl_len),                        \
                          ((cache_ptr)->jwipl_size), fail_val)             \
                                                                           \
    } else { /* put the entry in the replacement policy data structures */ \
                                                                           \
        /* modified LRU specific code */                                   \
                                                                           \
        /* insert the entry at the head of the LRU list. */                \
                                                                           \
        H5C2__DLL_PREPEND((entry_ptr), (cache_ptr)->LRU_head_ptr,          \
                          (cache_ptr)->LRU_tail_ptr,                       \
                          (cache_ptr)->LRU_list_len,                       \
                          (cache_ptr)->LRU_list_size, (fail_val))          \
                                                                           \
        /* Similarly, insert the entry at the head of either the clean or  \
         * dirty LRU list as appropriate.                                  \
         */                                                                \
                                                                           \
        if ( (entry_ptr)->is_dirty ) {                                     \
                                                                           \
            H5C2__AUX_DLL_PREPEND((entry_ptr), (cache_ptr)->dLRU_head_ptr, \
                                  (cache_ptr)->dLRU_tail_ptr,              \
                                  (cache_ptr)->dLRU_list_len,              \
                                  (cache_ptr)->dLRU_list_size, (fail_val)) \
                                                                           \
        } else {                                                           \
                                                                           \
            H5C2__AUX_DLL_PREPEND((entry_ptr), (cache_ptr)->cLRU_head_ptr, \
                                  (cache_ptr)->cLRU_tail_ptr,              \
                                  (cache_ptr)->cLRU_list_len,              \
                                  (cache_ptr)->cLRU_list_size, (fail_val)) \
        }                                                                  \
                                                                           \
        /* End modified LRU specific code. */                              \
    }                                                                      \
                                                                           \
} /* H5C2__UPDATE_RP_FOR_UNPROTECT */

#else /* H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

#define H5C2__UPDATE_RP_FOR_UNPROTECT(cache_ptr, entry_ptr, fail_val)      \
{                                                                          \
    HDassert( (cache_ptr) );                                               \
    HDassert( (cache_ptr)->magic == H5C2__H5C2_T_MAGIC );                  \
    HDassert( (entry_ptr) );                                               \
    HDassert( (entry_ptr)->is_protected);                                  \
    HDassert( (entry_ptr)->size > 0 );                                     \
                                                                           \
    /* Regardless of the replacement policy, remove the entry from the     \
     * protected list.                                                     \
     */                                                                    \
    H5C2__DLL_REMOVE((entry_ptr), (cache_ptr)->pl_head_ptr,                \
                    (cache_ptr)->pl_tail_ptr, (cache_ptr)->pl_len,         \
                    (cache_ptr)->pl_size, (fail_val))                      \
                                                                           \
    if ( (entry_ptr)->is_pinned ) {                                        \
                                                                           \
        H5C2__DLL_PREPEND((entry_ptr), (cache_ptr)->pel_head_ptr,          \
                         (cache_ptr)->pel_tail_ptr,                        \
                         (cache_ptr)->pel_len,                             \
                         (cache_ptr)->pel_size, (fail_val))                \
                                                                           \
    } else if ( (entry_ptr)->last_trans != 0 ) {                           \
                                                                           \
        /* put the entry in the jwip list */                               \
        HDassert( (cache_ptr)->mdj_enabled );                              \
        HDassert( (entry_ptr)->is_dirty );                                 \
        H5C2__DLL_PREPEND((entry_ptr),                                     \
                          ((cache_ptr)->jwipl_head_ptr),                   \
                          ((cache_ptr)->jwipl_tail_ptr),                   \
                          ((cache_ptr)->jwipl_len),                        \
                          ((cache_ptr)->jwipl_size), fail_val)             \
                                                                           \
    } else {                                                               \
                                                                           \
        /* modified LRU specific code */                                   \
                                                                           \
        /* insert the entry at the head of the LRU list. */                \
                                                                           \
        H5C2__DLL_PREPEND((entry_ptr), (cache_ptr)->LRU_head_ptr,          \
                          (cache_ptr)->LRU_tail_ptr,                       \
                          (cache_ptr)->LRU_list_len,                       \
                          (cache_ptr)->LRU_list_size, (fail_val))          \
                                                                           \
        /* End modified LRU specific code. */                              \
    }                                                                      \
} /* H5C2__UPDATE_RP_FOR_UNPROTECT */

#endif /* H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */


/**************************************************************************
 *
 * Transaction list update macros:
 *
 * When journaling is enabled, we must maintain the transaction list -- the
 * list of all entries that have been dirtied during the current
 * transaction.
 *
 * The following macros exist to support this task.
 *
 **************************************************************************/

/*-------------------------------------------------------------------------
 *
 * Macro:       H5C2__INSERT_ENTRY_IN_TL()
 *
 * Purpose:     Check to see if journaling is enabled.
 *
 *              If it is, set the last_trans field of the target entry
 *              to the current transaction number, and insert the entry
 *              in the transaction list.
 *
 * Return:      N/A
 *
 * Programmer:  John Mainzer, 3/31/08
 *
 * Modifications:
 *
 *              None.
 *
 *-------------------------------------------------------------------------
 */

#define H5C2__INSERT_ENTRY_IN_TL(cache_ptr, entry_ptr, fail_val)       \
if ( cache_ptr->mdj_enabled )                                          \
{                                                                      \
    HDassert( cache_ptr->trans_in_progress );                          \
    HDassert( entry_ptr->last_trans == 0 );                            \
                                                                       \
    entry_ptr->last_trans = cache_ptr->trans_num;                      \
                                                                       \
    H5C2__TRANS_DLL_PREPEND((entry_ptr), (cache_ptr->tl_head_ptr),     \
                            (cache_ptr->tl_tail_ptr),                  \
                            (cache_ptr->tl_len), (cache_ptr->tl_size), \
                            (fail_val));                               \
} /* H5C2__INSERT_ENTRY_IN_TL */


/*-------------------------------------------------------------------------
 *
 * Macro:	H5C2__UPDATE_TL_FOR_ENTRY_CLEAR
 *
 * Purpose:     Check to see if journaling is enabled.
 *
 *              If it is, see if the target entry is in the transaction
 *              list.  If it is, remove it from the list, and set its 
 *              last_trans field to zero.
 *
 * Return:      N/A
 *
 * Programmer:  John Mainzer, 3/31/08
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

#define H5C2__UPDATE_TL_FOR_ENTRY_CLEAR(cache_ptr, entry_ptr, fail_val) \
if ( cache_ptr->mdj_enabled )                                           \
{                                                                       \
    HDassert( cache_ptr->trans_in_progress );                           \
    HDassert( entry_ptr->last_trans <= cache_ptr->trans_num );          \
                                                                        \
    if ( entry_ptr->last_trans == cache_ptr->trans_num ) {              \
                                                                        \
        H5C2__TRANS_DLL_REMOVE((entry_ptr), (cache_ptr->tl_head_ptr),   \
                               (cache_ptr->tl_tail_ptr),                \
                               (cache_ptr->tl_len),                     \
                               (cache_ptr->tl_size), (fail_val));       \
        entry_ptr->last_trans = 0;                                      \
    }                                                                   \
} /* H5C2__UPDATE_TL_FOR_ENTRY_CLEAR */


/*-------------------------------------------------------------------------
 *
 * Macro:	H5C2__UPDATE_TL_FOR_ENTRY_DIRTY
 *
 * Purpose:     Check to see if journaling is enabled.
 *
 *              If it is, see if the target entry is in the transaction
 *              list.  If it is, remove it from the list.  If it isn't,
 *              set the entries last_trans field to the id of the current
 *              transaction.
 *
 *              In either case, then insert the entry at the head of the
 *              transaction list.
 *
 * Return:      N/A
 *
 * Programmer:  John Mainzer, 3/31/08
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

#define H5C2__UPDATE_TL_FOR_ENTRY_DIRTY(cache_ptr, entry_ptr, fail_val) \
if ( cache_ptr->mdj_enabled )                                           \
{                                                                       \
    HDassert( cache_ptr->trans_in_progress );                           \
    HDassert( entry_ptr->last_trans <= cache_ptr->trans_num );          \
                                                                        \
    if ( entry_ptr->last_trans == cache_ptr->trans_num ) {              \
                                                                        \
        H5C2__TRANS_DLL_REMOVE((entry_ptr), (cache_ptr->tl_head_ptr),   \
                               (cache_ptr->tl_tail_ptr),                \
                               (cache_ptr->tl_len),                     \
                               (cache_ptr->tl_size), (fail_val));       \
    } else {                                                            \
        entry_ptr->last_trans = cache_ptr->trans_num;                   \
    }                                                                   \
                                                                        \
    H5C2__TRANS_DLL_PREPEND((entry_ptr), (cache_ptr->tl_head_ptr),      \
                            (cache_ptr->tl_tail_ptr),                   \
		            (cache_ptr->tl_len), (cache_ptr->tl_size),  \
                            (fail_val));                                \
} /* H5C2__UPDATE_TL_FOR_ENTRY_DIRTY */


/*-------------------------------------------------------------------------
 *
 * Macro:	H5C2__UPDATE_TL_FOR_ENTRY_SIZE_CHANGE
 *
 * Purpose:     Update the transaction list for a change in the size of 
 *              one of its constituents.  Note that it is the callers
 *              responsibility to verify that the entry is in the 
 *              transaction list if it should be.
 *
 * Return:      N/A
 *
 * Programmer:  John Mainzer, 3/31/08
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

#define H5C2__UPDATE_TL_FOR_ENTRY_SIZE_CHANGE(cache_ptr, entry_ptr, \
                                              old_size, new_size)   \
if ( ( (cache_ptr)->mdj_enabled ) &&                                \
     ( (entry_ptr)->last_trans == (cache_ptr)->trans_num ) ) {      \
    HDassert( (cache_ptr)->trans_in_progress );                     \
    H5C2__DLL_UPDATE_FOR_SIZE_CHANGE(((cache_ptr)->tl_len),         \
                                     ((cache_ptr)->tl_size),        \
                                     (old_size), (new_size));       \
} /* H5C2__UPDATE_TL_FOR_ENTRY_SIZE_CHANGE() */

#endif /* _H5C2pkg_H */

