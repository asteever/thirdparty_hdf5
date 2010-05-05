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
 * Created:		H5Cprivate.h
 *			6/3/04
 *			John Mainzer
 *
 * Purpose:		Constants and typedefs available to the rest of the
 *			library.
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

#ifndef _H5Cprivate_H
#define _H5Cprivate_H

#include "H5Cpublic.h"		/* public prototypes		        */

/* Pivate headers needed by this header */
#include "H5private.h"		/* Generic Functions			*/
#include "H5Fprivate.h"		/* File access				*/


#define H5C_DO_SANITY_CHECKS		1
#define H5C_DO_EXTREME_SANITY_CHECKS	0

/* This sanity checking constant was picked out of the air.  Increase
 * or decrease it if appropriate.  Its purposes is to detect corrupt
 * object sizes, so it probably doesn't matter if it is a bit big.
 *
 *					JRM - 5/17/04
 */
#define H5C_MAX_ENTRY_SIZE		((size_t)(10 * 1024 * 1024))

/* H5C_COLLECT_CACHE_STATS controls overall collection of statistics
 * on cache activity.  In general, this #define should be set to 0.
 */
#define H5C_COLLECT_CACHE_STATS	1

/* H5C_COLLECT_CACHE_ENTRY_STATS controls collection of statistics
 * in individual cache entries.
 *
 * H5C_COLLECT_CACHE_ENTRY_STATS should only be defined to true if
 * H5C_COLLECT_CACHE_STATS is also defined to true.
 */
#if H5C_COLLECT_CACHE_STATS

#define H5C_COLLECT_CACHE_ENTRY_STATS	1

#else

#define H5C_COLLECT_CACHE_ENTRY_STATS	0

#endif /* H5C_COLLECT_CACHE_STATS */


#ifdef H5_HAVE_PARALLEL

/* we must maintain the clean and dirty LRU lists when we are compiled
 * with parallel support.
 */
#define H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS  1

#else /* H5_HAVE_PARALLEL */

/* The clean and dirty LRU lists don't buy us anything here -- we may
 * want them on for testing on occasion, but in general they should be
 * off.
 */
#define H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS  0

#endif /* H5_HAVE_PARALLEL */


/* Typedef for the main structure for the cache (defined in H5Cpkg.h) */

typedef struct H5C_t H5C_t;


/***************************************************************************
 *
 * Struct H5C_class_t
 *
 * Instances of H5C_class_t are used to specify the callback functions
 * used by the metadata cache for each class of metadata cache entry.
 * The fields of the structure are discussed below:
 *
 * id:	Integer field containing the unique ID of the class of metadata
 * 	cache entries.
 *
 * name: Pointer to a string containing the name of the class of metadata
 * 	cache entries.
 *
 * mem_type:  Instance of H5FD_mem_t, that is used to supply the
 * 	mem type passed into H5F_block_read().
 *
 * deserialize: Pointer to the deserialize function.
 *
 * 	This function must be able to read an on disk image of a metadata
 * 	cache entry, allocate and load the equivalent in core representation,
 * 	and return a pointer to that representation.
 *
 *	The typedef for the deserialize callback is as follows:
 *
 * 	   typedef void *(*H5C_deserialize_func_t)(haddr_t addr,
 * 	                                           size_t len,
 *                                                 const void * image_ptr,
 *                                                 void * udata_ptr,
 *                                                 boolean * dirty_ptr);
 *
 *	The parameters of the deserialize callback are as follows:
 *
 *	addr:   Base address in file of the image to be deserialized.
 *
 * 		This parameter is supplied mainly for sanity checking.
 * 	        Sanity checks should be performed when compiled in debug
 * 	        mode, but the parameter may be unused when compiled in
 * 	        production mode.
 *
 *	len:    Length in bytes of the in file image to be deserialized.
 *
 *              This parameter is supplied mainly for sanity checking.
 *              Sanity checks should be performed when compiled in debug
 *              mode, but the parameter may be unused when compiled in
 *              production mode.
 *
 *	image_ptr: Pointer to a buffer of length len containing the
 *		contents of the file starting at addr and continuing
 *		for len bytes.
 *
 *	udata_ptr: Pointer to user data provided in the protect call, which
 *         	must be passed through to the deserialize callback.
 *
 *      dirty_ptr:  Pointer to boolean which the deserialize function
 *      	must use to mark the entry dirty if it has to modify
 *      	the entry to clean up file corruption left over from
 *      	an old bug in the HDF5 library.
 *
 *	Processing in the deserialize function should proceed as follows:
 *
 *      If the image contains valid data, and is of the correct length,
 *      the deserialize function must allocate space for an in core
 *      representation of that data, load the contents of the image into
 *      the space allocated for the in core representation, and return
 *      a pointer to the in core representation.  Observe that an
 *      instance of H5C_cache_entry_t must be the first item in this
 *      representation.  It will have to be initialized appropriately
 *      after the callback returns.
 *
 *      Note that the structure of the in core representation is otherwise
 *      up to the cache client.  All that is required is that the pointer
 *      returned be sufficient for the clients purposes when it is returned
 *      on a protect call.
 *
 *      If the deserialize function has to clean up file corruption
 *      left over from an old bug in the HDF5 library, it must set
 *      *dirty_ptr to TRUE.  If it doesn't, no action is needed as
 *      *dirty_ptr will be set to FALSE before the deserialize call.
 *
 *      If the operation fails for any reason (i.e. bad data in buffer, bad
 *      buffer length, malloc failure, etc.) the function must return NULL and
 *      push error information on the error stack with the error API routines.
 *
 *      If the protect call which occasioned the call to the deserialize
 *      callback had the check length flag set, after the deserialize call
 *      returns, the cache must call the image_len callback (see below) and
 *      update its on disk image length accordingly.
 *
 *
 * image_len: Pointer to the image length callback.
 *
 *	In the best of all possible worlds, we would not have this callback.
 *	It exists to allow clients to reduce the size of the on disk image of
 *	an entry in the deserialize callback.
 *
 *      The typedef for the image_len callback is as follows:
 *
 *      typedef herr_t (*H5C_image_len_func_t)(void *thing,
 *                                             size_t *image_len_ptr);
 *
 * 	The parameters of the image_len callback are as follows:
 *
 *	thing:  Pointer to the in core representation of the entry.
 *
 *	image_len_ptr: Pointer to size_t in which the callback will return
 *		the length of the on disk image of the cache entry.
 *
 *	Processing in the image_len function should proceed as follows:
 *
 *	If successful, the function will place the length of the on disk
 *	image associated with the in core representation provided in the
 *	thing parameter in *image_len_ptr, and then return SUCCEED.
 *
 *	On failure, the function must return FAIL and push error information
 *	onto the error stack with the error API routines.
 *
 *
 * serialize: Pointer to the serialize callback.
 *
 *	The serialize callback is invoked by the metadata cache whenever
 *	it needs a current on disk image of the metadata entry for purposes
 *	either constructing a journal or flushing the entry to disk.
 *
 *	At this point, one would think that the base address and length of
 *	the length of the entry's image on disk would be well known. 
 *	However, that need not be the case as fractal heap blocks can
 *	change size (and therefor possible location as well) on
 *	serialization if compression is enabled.  In the old H5C code,
 *	this happened on a flush, and occasioned a rename in the midst
 *	of the flush.  To avoid this in H5C, the serialize callback
 *	will return the new base address, length, and image pointer to
 *	the caller when necessary.  The caller must then update the
 *	metadata cache's internal structures accordingly.
 *
 *	The typedef for the serialize callback is as follows:
 *
 *	typedef herr_t (*H5C_serialize_func_t)(const H5F_t *f,
 *                                             hid_t dxpl_id,
 *                                             haddr_t addr,
 *                                             size_t len,
 *                                             void * image_ptr,
 *                                             void * thing,
 *                                             unsigned * flags_ptr,
 *                                             haddr_t * new_addr_ptr,
 *                                             size_t * new_len_ptr,
 *                                             void ** new_image_ptr_ptr);
 *
 *	The parameters of the serialize callback are as follows:
 *
 *	f:	File pointer -- needed if other metadata cache entries
 *		must be modified in the process of serializing the
 *		target entry.
 *
 *	dxpl_id: dxpl_id passed with the file pointer to the cache, and
 *	        passed on to the callback.  Necessary as some callbacks
 *	        revise the size and location of the target entry, or
 *	        possibly other entries on serialize.
 *
 *	addr:   Base address in file of the entry to be serialized.
 *
 *		This parameter is supplied mainly for sanity checking.
 *		Sanity checks should be performed when compiled in debug
 *		mode, but the parameter may be unused when compiled in
 *		production mode.
 *
 *	len:    Length in bytes of the in file image of the entry to be
 *		serialized.  Also the size of *image_ptr (below).
 *
 *		This parameter is supplied mainly for sanity checking.
 *		Sanity checks should be performed when compiled in debug
 *		mode, but the parameter may be unused when compiled in
 *		production mode.
 *
 *	image_ptr: Pointer to a buffer of length len bytes into which a
 *		serialized image of the target metadata cache entry is
 *		to be written.
 *
 * 		Note that this buffer will not in general be initialized
 * 		to any particular value.  Thus the serialize function may
 * 		not assume any initial value and must set each byte in
 * 		the buffer.
 *
 *	thing:  Pointer to void containing the address of the in core
 *		representation of the target metadata cache entry. 
 *		This is the same pointer returned by a protect of the
 *		addr and len given above.
 *
 *	flags_ptr:  Pointer to an unsigned integer used to return flags
 *		indicating whether the resize function resized or renamed
 *		the entry.  If the entry was neither resized or renamed,
 *		the serialize function must set *flags_ptr to zero. 
 *		H5C__SERIALIZE_RESIZED_FLAG and H5C__SERIALIZE_RENAMED_FLAG
 *		must be set to indicate a resize and a rename respectively.
 *
 *	        If the H5C__SERIALIZE_RESIZED_FLAG is set, the new length
 *	        and image pointer must be stored in *new_len_ptr and
 *	        *new_image_ptr_ptr respectively. 
 *
 *	        If the H5C__SERIALIZE_RENAMED_FLAG flag is also set, the
 *	        new image base address must be stored in *new_addr_ptr. 
 *	        Observe that the H5C__SERIALIZE_RENAMED_FLAG must not
 *	        appear without the H5C__SERIALIZE_RESIZED_FLAG. 
 *
 *	        Except as noted above, the locations pointed to by the
 *	        remaining parameters are undefined, and should be ignored
 *	        by the caller. 
 *
 *	new_addr_ptr:  Pointer to haddr_t.  If the entry is renamed by
 *		the serialize function, the new on disk base address must
 *		be stored in *new_addr_ptr.  If the entry is not renamed
 *		by the serialize function, *new_addr_ptr is undefined. 
 *
 *	new_len_ptr:  Pointer to size_t.  If the entry is resized by the
 *		serialize function, the new length of the on disk image
 *		must be stored in *new_len_ptr.  If the entry is not
 *		resized by the serialize function, *new_len_ptr is
 *		undefined. 
 *
 *	new_image_ptr_ptr:  Pointer to pointer to void.  If the entry is
 *		resized by the serialize function, the pointer to the
 *		new buffer containing the on disk image must be stored
 *		in *new_image_ptr_ptr.  If the entry is not resized by
 *		the serialize function, *new_image_ptr_ptr is undefined.
 *
 *	Processing in the serialize function should proceed as follows:
 *
 *	The serialize function must examine the in core representation
 *	indicated by the thing parameter, and write a serialized image
 *	of its contents into the provided buffer. 
 *
 *	If the serialize function does not change the size or location
 *	of the on disk image, it must set *flags_ptr to zero. 
 *
 *	If the size of the on disk image must be changed, the serialize
 *	function must free the old image buffer (base address in image_ptr),
 *	allocate a new one, load the image into the new buffer, load the
 *	base address of the new buffer into *new_image_ptr_ptr, load the
 *	length of the new image into *new_len_ptr, and set the
 *	H5C__SERIALIZE_RESIZED_FLAG in *flags_ptr. 
 *
 *	If in addition, the base address of the on disk image must
 *	be changed, the serialize function must also set *new_addr_ptr
 *	to the new base address, and set the H5C__SERIALIZE_RENAMED_FLAG
 *	in *flags_ptr.
 *
 *	If it is successful, the function must return SUCCEED. 
 *
 *	If it fails for any reason, the function must return FAIL and
 *	push error information on the error stack with the error API
 *	routines.
 *
 *
 * free_icr: Pointer to the free ICR Callback.
 *
 *	The free ICR callback is invoked by the metadata cache when it
 *	wishes to evict an entry, and needs the client to free the memory
 *	allocated for the in core representation. 
 *
 *	The typedef for the free ICR callback is as follows:
 *
 *	typedef herr_t (*N5C_free_icr_func_t)(void * thing);
 *
 * 	The parameters of the free ICR callback are as follows:
 *
 *	thing:  Pointer to void containing the address of the in core
 *		representation of the target metadata cache entry.  This
 *		is the same pointer that would be returned by a protect
 *		of the addr and len above. 
 *
 *	Processing in the free ICR function should proceed as follows:
 *
 *	The free ICR function must free all memory allocated to the
 *	in core representation. 
 *
 *	If the function is successful, it must return SUCCEED. 
 *
 *	If it fails for any reason, the function must return FAIL and
 *	push error information on the error stack with the error API
 *	routines. 
 *
 *	At least when compiled with debug, it would be useful if the
 *	free ICR call would fail if the in core representation has been
 *	modified since the last serialize of clear callback.
 *
 ***************************************************************************/
typedef void *(*H5C_deserialize_func_t)(haddr_t addr,
                                         size_t len,
                                         const void * image_ptr,
                                         void * udata_ptr,
					 hbool_t * dirty_ptr);

typedef herr_t (*H5C_image_len_func_t)(const void *thing,
                                        size_t *image_len_ptr);

#define H5C__SERIALIZE_RESIZED_FLAG	0x1
#define H5C__SERIALIZE_RENAMED_FLAG	0x2

typedef herr_t (*H5C_serialize_func_t)(const H5F_t *f,
		                        hid_t dxpl_id,
                                        haddr_t addr,
                                        size_t len,
                                        void * image_ptr,
                                        void * thing,
                                        unsigned * flags_ptr,
				        haddr_t * new_addr_ptr,
				        size_t * new_len_ptr,
				        void ** new_image_ptr_ptr);

typedef herr_t (*H5C_free_icr_func_t)(void * thing);

typedef herr_t (*H5C_clear_dirty_bits_func_t)(haddr_t addr,
                                               size_t len,
                                               void * thing);

typedef struct H5C_class_t {
    int					id;
    const char *			name;
    H5FD_mem_t				mem_type;
    H5C_deserialize_func_t 		deserialize;
    H5C_image_len_func_t		image_len;
    H5C_serialize_func_t		serialize;
    H5C_free_icr_func_t		        free_icr;
} H5C_class_t;


/* Type defintions of call back functions used by the cache as a whole */

typedef herr_t (*H5C_write_permitted_func_t)(const H5F_t *f,
                                             hid_t dxpl_id,
                                             hbool_t * write_permitted_ptr);

typedef herr_t (*H5C_log_flush_func_t)(H5C_t * cache_ptr,
                                       haddr_t addr,
                                       hbool_t was_dirty,
                                       unsigned flags,
                                       int type_id);

/* Upper and lower limits on cache size.  These limits are picked
 * out of a hat -- you should be able to change them as necessary.
 *
 * However, if you need a very big cache, you should also increase the
 * size of the hash table (H5C__HASH_TABLE_LEN in H5Cpkg.h).  The current
 * upper bound on cache size is rather large for the current hash table
 * size.
 */

#define H5C__MAX_MAX_CACHE_SIZE		((size_t)(128 * 1024 * 1024))
#define H5C__MIN_MAX_CACHE_SIZE		((size_t)(1024))


/* Default max cache size and min clean size are give here to make
 * them generally accessable.
 */

#define H5C__DEFAULT_MAX_CACHE_SIZE     ((size_t)(4 * 1024 * 1024))
#define H5C__DEFAULT_MIN_CLEAN_SIZE     ((size_t)(2 * 1024 * 1024))


/****************************************************************************
 *
 * structure H5C_cache_entry_t
 *
 * Instances of the H5C_cache_entry_t structure are used to store cache
 * entries in a hash table and sometimes in a skip list.
 * See H5SL.c for the particulars of the skip list.
 *
 * In typical application, this structure is the first field in a
 * structure to be cached.  For historical reasons, the external module
 * is responsible for managing the is_dirty field (this is no longer
 * completely true.  See the comment on the is_dirty field for details).
 * All other fields are managed by the cache.
 *
 * The fields of this structure are discussed individually below:
 *
 *						JRM - 4/26/04
 *
 * magic:	Unsigned 32 bit integer that must always be set to
 *              H5C__H5C_CACHE_ENTRY_T_MAGIC when the entry is valid.
 *              The field must be set to H5C__H5C_CACHE_ENTRY_T_BAD_MAGIC
 *              just before the entry is freed.
 *
 *              This is necessary, as the LRU list can be changed out
 *              from under H5C_make_space_in_cache() by the serialize
 *              callback which may change the size of an existing entry,
 *              and/or load a new entry while serializing the target entry.
 *
 *              This in turn can cause a recursive call to
 *              H5C_make_space_in_cache() which may either flush or evict
 *              the next entry that the first invocation of that function
 *              was about to examine.
 *
 *              The magic field allows H5C_make_space_in_cache() to
 *              detect this case, and re-start its scan from the bottom
 *              of the LRU when this situation occurs.
 *
 * cache_ptr:	Pointer to the cache that this entry is contained within.
 *
 * addr:	Base address of the cache entry on disk.
 *
 * size:	Length of the cache entry on disk.  Note that unlike normal
 *		caches, the entries in this cache are of variable length.
 *		The entries should never overlap, and when we do writebacks,
 *		we will want to writeback adjacent entries where possible.
 *
 *		NB: At present, entries need not be contiguous on disk.  Until
 *		    we fix this, we can't do much with writing back adjacent
 *		    entries.
 *
 *		Update: This has now been changed -- all metadata cache
 *		entries must now be associated with a single contiguous
 *		block of memory on disk.  The image of this block (i.e.
 *		the on disk image) is stored in *image_ptr (discussed below).
 *
 * image_ptr:	Pointer to void.  When not NULL, this field points to a
 * 		dynamically allocated block of size bytes in which the
 * 		on disk image of the metadata cache entry is stored.
 *
 * 		If the entry is dirty, the serialize callback must be used
 * 		to update this image before it is written to disk
 *
 * image_up_to_date:  Boolean flag that is set to TRUE when *image_ptr
 * 		is up to date, and set to false when the entry is dirtied.
 *
 * type:	Pointer to the instance of H5C_class_t containing pointers
 *		to the methods for cache entries of the current type.  This
 *		field should be NULL when the instance of H5C_cache_entry_t
 *		is not in use.
 *
 *		The name is not particularly descriptive, but is retained
 *		to avoid changes in existing code.
 *
 * is_dirty:	Boolean flag indicating whether the contents of the cache
 *		entry has been modified since the last time it was written
 *		to disk.
 *
 *		NOTE: For historical reasons, this field is not maintained
 *		      by the cache.  Instead, the module using the cache
 *		      sets this flag when it modifies the entry, and the
 *		      flush and clear functions supplied by that module
 *		      reset the dirty when appropriate.
 *
 *		      This is a bit quirky, so we may want to change this
 *		      someday.  However it will require a change in the
 *		      cache interface.
 *
 *		Update: Management of the is_dirty field has been largely
 *		      moved into the cache.  The only remaining exceptions
 *		      are the flush and clear functions supplied by the
 *		      modules using the cache.  These still clear the
 *		      is_dirty field as before.  -- JRM 7/5/05
 *
 *		Update: Management of the is_dirty field is now entirely
 *		      in the cache.		 -- JRM 7/5/07
 *
 * dirtied:	Boolean flag used to indicate that the entry has been
 * 		dirtied while protected.
 *
 * 		This field is set to FALSE in the protect call, and may
 * 		be set to TRUE by the
 * 		H5C_mark_pinned_or_protected_entry_dirty()
 * 		call at an time prior to the unprotect call.
 *
 * 		The H5C_mark_pinned_or_protected_entry_dirty() call exists
 * 		as a convenience function for the fractal heap code which
 * 		may not know if an entry is protected or pinned, but knows
 * 		that is either protected or pinned.  The dirtied field was
 * 		added as in the parallel case, it is necessary to know
 * 		whether a protected entry was dirty prior to the protect call.
 *
 * is_protected: Boolean flag indicating whether this entry is protected
 *		(or locked, to use more conventional terms).  When it is
 *		protected, the entry cannot be flushed or accessed until
 *		it is unprotected (or unlocked -- again to use more
 *		conventional terms).
 *
 *		Note that protected entries are removed from the LRU lists
 *		and inserted on the protected list.
 *
 * is_read_only: Boolean flag that is only meaningful if is_protected is
 * 		TRUE.  In this circumstance, it indicates whether the
 * 		entry has been protected read only, or read/write.
 *
 * 		If the entry has been protected read only (i.e. is_protected
 * 		and is_read_only are both TRUE), we allow the entry to be
 * 		protected more than once.
 *
 *		In this case, the number of readers is maintained in the
 *		ro_ref_count field (see below), and unprotect calls simply
 *		decrement that field until it drops to zero, at which point
 *		the entry is actually unprotected.
 *
 * ro_ref_count: Integer field used to maintain a count of the number of
 * 		outstanding read only protects on this entry.  This field
 * 		must be zero whenever either is_protected or is_read_only
 * 		are TRUE.
 *
 * is_pinned:	Boolean flag indicating whether the entry has been pinned
 * 		in the cache.
 *
 * 		For very hot entries, the protect / unprotect overhead
 * 		can become excessive.  Thus the cache has been extended
 * 		to allow an entry to be "pinned" in the cache.
 *
 * 		Pinning an entry in the cache has several implications:
 *
 * 		1) A pinned entry cannot be evicted.  Thus unprotected
 * 		   pinned entries must be stored in the pinned entry
 * 		   list, instead of being managed by the replacement
 * 		   policy code (LRU at present).
 *
 * 		2) A pinned entry can be accessed or modified at any time.
 * 		   Therefore, the cache must check with the entry owner
 * 		   before flushing it.  If permission is denied, the
 * 		   cache does not flush the entry.
 *
 * 		3) A pinned entry can be marked as dirty (and possibly
 *		   change size) while it is unprotected.
 *
 *		4) The flush-destroy code must allow pinned entries to
 *		   be unpinned (and possibly unprotected) during the
 *		   flush.
 *
 *		   					JRM -- 3/16/06
 *
 * in_slist:	Boolean flag indicating whether the entry is in the skip list
 *		As a general rule, entries are placed in the list when they
 *              are marked dirty.  However they may remain in the list after
 *              being flushed.
 *
 *              Update: Dirty entries are now removed from the skip list
 *			when they are flushed.
 *
 * flush_marker:  Boolean flag indicating that the entry is to be flushed
 *		the next time H5C_flush_cache() is called with the
 *		H5C__FLUSH_MARKED_ENTRIES_FLAG.  The flag is reset when
 *		the entry is flushed for whatever reason.
 *
 * clear_on_unprotect:  Boolean flag used only in PHDF5.  When H5C is used
 *		to implement the metadata cache In the parallel case, only
 *		the cache with mpi rank 0 is allowed to actually write to
 *		file -- all other caches must retain dirty entries until they
 *		are advised that the entry is clean.
 *
 *		This flag is used in the case that such an advisory is
 *		received when the entry is protected.  If it is set when an
 *		entry is unprotected, and the dirtied flag is not set in
 *		the unprotect, the entry's is_dirty flag is reset by flushing
 *		it with the H5C__FLUSH_CLEAR_ONLY_FLAG.
 *
 * flush_in_progress:  Boolean flag that is set to true iff the entry
 * 		is in the process of being flushed.  This allows the cache
 * 		to detect when a call is the result of a flush callback.
 *
 * destroy_in_progress:  Boolean flag that is set to true iff the entry
 * 		is in the process of being flushed and destroyed.
 *
 *
 * Fields supporting the hash table:
 *
 * Fields in the cache are indexed by a more or less conventional hash table.
 * If there are multiple entries in any hash bin, they are stored in a doubly
 * linked list.
 *
 * ht_next:	Next pointer used by the hash table to store multiple
 *		entries in a single hash bin.  This field points to the
 *		next entry in the doubly linked list of entries in the
 *		hash bin, or NULL if there is no next entry.
 *
 * ht_prev:     Prev pointer used by the hash table to store multiple
 *              entries in a single hash bin.  This field points to the
 *              previous entry in the doubly linked list of entries in
 *		the hash bin, or NULL if there is no previuos entry.
 *
 *
 * Fields supporting replacement policies:
 *
 * The cache must have a replacement policy, and it will usually be
 * necessary for this structure to contain fields supporting that policy.
 *
 * While there has been interest in several replacement policies for
 * this cache, the initial development schedule is tight.  Thus I have
 * elected to support only a modified LRU policy for the first cut.
 *
 * When additional replacement policies are added, the fields in this
 * section will be used in different ways or not at all.  Thus the
 * documentation of these fields is repeated for each replacement policy.
 *
 * Modified LRU:
 *
 * When operating in parallel mode, we must ensure that a read does not
 * cause a write.  If it does, the process will hang, as the write will
 * be collective and the other processes will not know to participate.
 *
 * To deal with this issue, I have modified the usual LRU policy by adding
 * clean and dirty LRU lists to the usual LRU list.  When reading in
 * parallel mode, we evict from the clean LRU list only.  This implies
 * that we must try to ensure that the clean LRU list is reasonably well
 * stocked.  See the comments on H5C_t in H5Cpkg.h for more details.
 *
 * Note that even if we start with a completely clean cache, a sequence
 * of protects without unprotects can empty the clean LRU list.  In this
 * case, the cache must grow temporarily.  At the next write, we will
 * attempt to evict enough entries to get the cache down to its nominal
 * maximum size.
 *
 * The use of the replacement policy fields under the Modified LRU policy
 * is discussed below:
 *
 * next:	Next pointer in either the LRU or the protected list,
 *		depending on the current value of protected.  If there
 *		is no next entry on the list, this field should be set
 *		to NULL.
 *
 * prev:	Prev pointer in either the LRU or the protected list,
 *		depending on the current value of protected.  If there
 *		is no previous entry on the list, this field should be
 *		set to NULL.
 *
 * aux_next:	Next pointer on either the clean or dirty LRU lists.
 *		This entry should be NULL when protected is true.  When
 *		protected is false, and dirty is true, it should point
 *		to the next item on the dirty LRU list.  When protected
 *		is false, and dirty is false, it should point to the
 *		next item on the clean LRU list.  In either case, when
 *		there is no next item, it should be NULL.
 *
 * aux_prev:	Previous pointer on either the clean or dirty LRU lists.
 *		This entry should be NULL when protected is true.  When
 *		protected is false, and dirty is true, it should point
 *		to the previous item on the dirty LRU list.  When protected
 *		is false, and dirty is false, it should point to the
 *		previous item on the clean LRU list.  In either case, when
 *		there is no previous item, it should be NULL.
 *
 *
 * Fields supporting metadata journaling:
 *
 * last_trans:	unit64_t containing the ID of the last transaction in
 * 		which this entry was dirtied.  If journaling is disabled,
 * 		or if the entry has never been dirtied in a transaction,
 * 		this field should be set to zero.  Once we notice that
 * 		the specified transaction has made it to disk, we will
 * 		reset this field to zero as well.
 *
 * 		We must maintain this field, as to avoid messages from
 * 		the future, we must not flush a dirty entry to disk
 * 		until the last transaction in which it was dirtied
 * 		has made it to disk in the journal file.
 *
 * trans_next:  Next pointer in the entries modified in the current
 * 		transaction list.  This field should always be null
 * 		unless journaling is enabled, the entry is dirty,
 * 		and last_trans field contains the current transaction
 * 		number.  Even if all these conditions are fulfilled,
 * 		the field will still be NULL if this is the last
 * 		entry on the list.
 *
 * trans_prev:  Previous pointer in the entries modified in the current
 * 		transaction list.  This field should always be null
 * 		unless journaling is enabled, the entry is dirty,
 * 		and last_trans field contains the current transaction
 * 		number.  Even if all these conditions are fulfilled,
 * 		the field will still be NULL if this is the first
 * 		entry on the list.
 *
 *
 * Cache entry stats collection fields:
 *
 * These fields should only be compiled in when both H5C_COLLECT_CACHE_STATS
 * and H5C_COLLECT_CACHE_ENTRY_STATS are true.  When present, they allow
 * collection of statistics on individual cache entries.
 *
 * accesses:	int32_t containing the number of times this cache entry has
 *		been referenced in its lifetime.
 *
 * clears:	int32_t containing the number of times this cache entry has
 *              been cleared in its life time.
 *
 * flushes:	int32_t containing the number of times this cache entry has
 *              been flushed to file in its life time.
 *
 * pins:	int32_t containing the number of times this cache entry has
 * 		been pinned in cache in its life time.
 *
 ****************************************************************************/

#ifndef NDEBUG
#define H5C__H5C_CACHE_ENTRY_T_MAGIC		0x005CAC0A
#define H5C__H5C_CACHE_ENTRY_T_BAD_MAGIC	0xDeadBeef
#endif /* NDEBUG */

typedef struct H5C_cache_entry_t
{
#ifndef NDEBUG
    uint32_t			magic;
#endif /* NDEBUG */
    H5C_t *                     cache_ptr;
    haddr_t			addr;
    size_t			size;
    void *			image_ptr;
    hbool_t			image_up_to_date;
    const H5C_class_t *		type;
    hbool_t			is_dirty;
    hbool_t			dirtied;
    hbool_t			is_protected;
    hbool_t			is_read_only;
    int				ro_ref_count;
    hbool_t			is_pinned;
    hbool_t			in_slist;
    hbool_t			flush_marker;
#ifdef H5_HAVE_PARALLEL
    hbool_t			clear_on_unprotect;
#endif /* H5_HAVE_PARALLEL */
    hbool_t			flush_in_progress;
    hbool_t			destroy_in_progress;

    /* fields supporting the hash table: */

    struct H5C_cache_entry_t *	ht_next;
    struct H5C_cache_entry_t *	ht_prev;

    /* fields supporting replacement policies: */

    struct H5C_cache_entry_t *	next;
    struct H5C_cache_entry_t *	prev;
    struct H5C_cache_entry_t *	aux_next;
    struct H5C_cache_entry_t *	aux_prev;


    /* fields supporting journaling */

    uint64_t			last_trans;
    struct H5C_cache_entry_t * trans_next;
    struct H5C_cache_entry_t * trans_prev;

#if H5C_COLLECT_CACHE_ENTRY_STATS

    /* cache entry stats fields */

    int32_t			accesses;
    int32_t			clears;
    int32_t			flushes;
    int32_t			pins;

#endif /* H5C_COLLECT_CACHE_ENTRY_STATS */

} H5C_cache_entry_t;


/****************************************************************************
 *
 * structure H5C_auto_size_ctl_t
 *
 * Instances of H5C_auto_size_ctl_t are used to get and set the control
 * fields for automatic cache re-sizing.
 *
 * The fields of the structure are discussed individually below:
 *
 * version: Integer field containing the version number of this version
 *	of the H5C_auto_size_ctl_t structure.  Any instance of
 *	H5C_auto_size_ctl_t passed to the cache must have a known
 *	version number, or an error will be flagged.
 *
 * report_fcn:  Pointer to the function that is to be called to report
 *      activities each time the auto cache resize code is executed.  If the
 *	field is NULL, no call is made.
 *
 *	If the field is not NULL, it must contain the address of a function
 *	of type H5C_auto_resize_report_fcn.
 *
 * set_initial_size: Boolean flag indicating whether the size of the
 *	initial size of the cache is to be set to the value given in
 *	the initial_size field.  If set_initial_size is FALSE, the
 *	initial_size field is ignored.
 *
 * initial_size: If enabled, this field contain the size the cache is
 *	to be set to upon receipt of this structure.  Needless to say,
 *	initial_size must lie in the closed interval [min_size, max_size].
 *
 * min_clean_fraction: double in the range 0 to 1 indicating the fraction
 *	of the cache that is to be kept clean.  This field is only used
 *	in parallel mode.  Typical values are 0.1 to 0.5.
 *
 * max_size: Maximum size to which the cache can be adjusted.  The
 *	supplied value must fall in the closed interval
 *	[MIN_MAX_CACHE_SIZE, MAX_MAX_CACHE_SIZE].  Also, max_size must
 *	be greater than or equal to min_size.
 *
 * min_size: Minimum size to which the cache can be adjusted.  The
 *      supplied value must fall in the closed interval
 *      [MIN_MAX_CACHE_SIZE, MAX_MAX_CACHE_SIZE].  Also, min_size must
 *	be less than or equal to max_size.
 *
 * epoch_length: Number of accesses on the cache over which to collect
 *	hit rate stats before running the automatic cache resize code,
 *      if it is enabled.
 *
 *	At the end of an epoch, we discard prior hit rate data and start
 * 	collecting afresh.  The epoch_length must lie in the closed
 *	interval [H5C__MIN_AR_EPOCH_LENGTH, H5C__MAX_AR_EPOCH_LENGTH].
 *
 *
 * Cache size increase control fields:
 *
 * incr_mode: Instance of the H5C_cache_incr_mode enumerated type whose
 *	value indicates how we determine whether the cache size should be
 *	increased.  At present there are two possible values:
 *
 *	H5C_incr__off:	Don't attempt to increase the size of the cache
 *		automatically.
 *
 *		When this increment mode is selected, the remaining fields
 *		in the cache size increase section ar ignored.
 *
 *	H5C_incr__threshold: Attempt to increase the size of the cache
 *		whenever the average hit rate over the last epoch drops
 *		below the value supplied in the lower_hr_threshold
 *		field.
 *
 *		Note that this attempt will fail if the cache is already
 *		at its maximum size, or if the cache is not already using
 *		all available space.
 *
 * lower_hr_threshold: Lower hit rate threshold.  If the increment mode
 *	(incr_mode) is H5C_incr__threshold and the hit rate drops below the
 *	value supplied in this field in an epoch, increment the cache size by
 *	size_increment.  Note that cache size may not be incremented above
 *	max_size, and that the increment may be further restricted by the
 *	max_increment field if it is enabled.
 *
 *	When enabled, this field must contain a value in the range [0.0, 1.0].
 *	Depending on the incr_mode selected, it may also have to be less than
 *	upper_hr_threshold.
 *
 * increment:  Double containing the multiplier used to derive the new
 *	cache size from the old if a cache size increment is triggered.
 *      The increment must be greater than 1.0, and should not exceed 2.0.
 *
 *	The new cache size is obtained by multiplying the current max cache
 *	size by the increment, and then clamping to max_size and to stay
 *	within the max_increment as necessary.
 *
 * apply_max_increment:  Boolean flag indicating whether the max_increment
 *	field should be used to limit the maximum cache size increment.
 *
 * max_increment: If enabled by the apply_max_increment field described
 *	above, this field contains the maximum number of bytes by which the
 *	cache size can be increased in a single re-size.
 *
 * flash_incr_mode:  Instance of the H5C_cache_flash_incr_mode enumerated
 *      type whose value indicates whether and by what algorithm we should
 *      make flash increases in the size of the cache to accomodate insertion
 *      of large entries and large increases in the size of a single entry.
 *
 *      The addition of the flash increment mode was occasioned by performance
 *      problems that appear when a local heap is increased to a size in excess
 *      of the current cache size.  While the existing re-size code dealt with
 *      this eventually, performance was very bad for the remainder of the
 *      epoch.
 *
 *      At present, there are two possible values for the flash_incr_mode:
 *
 *      H5C_flash_incr__off:  Don't perform flash increases in the size of
 *              the cache.
 *
 *      H5C_flash_incr__add_space:  Let x be either the size of a newly
 *              newly inserted entry, or the number of bytes by which the
 *              size of an existing entry has been increased.
 *
 *              If
 *                   x > flash_threshold * current max cache size,
 *
 *              increase the current maximum cache size by x * flash_multiple
 *              less any free space in the cache, and start a new epoch.  For
 *              now at least, pay no attention to the maximum increment.
 *
 *
 *      With a little thought, it should be obvious that the above flash
 *      cache size increase algorithm is not sufficient for all
 *      circumstances -- for example, suppose the user round robins through
 *      (1/flash_threshold) +1 groups, adding one data set to each on each
 *      pass.  Then all will increase in size at about the same time, requiring
 *      the max cache size to at least double to maintain acceptable
 *      performance, however the above flash increment algorithm will not be
 *      triggered.
 *
 *      Hopefully, the add space algorithm detailed above will be sufficient
 *      for the performance problems encountered to date.  However, we should
 *      expect to revisit the issue.
 *
 * flash_multiple: Double containing the multiple described above in the
 *      H5C_flash_incr__add_space section of the discussion of the
 *      flash_incr_mode section.  This field is ignored unless flash_incr_mode
 *      is H5C_flash_incr__add_space.
 *
 * flash_threshold: Double containing the factor by which current max cache
 * 	size is multiplied to obtain the size threshold for the add_space
 * 	flash increment algorithm.  The field is ignored unless
 * 	flash_incr_mode is H5C_flash_incr__add_space.
 *
 *
 * Cache size decrease control fields:
 *
 * decr_mode: Instance of the H5C_cache_decr_mode enumerated type whose
 *	value indicates how we determine whether the cache size should be
 *	decreased.  At present there are four possibilities.
 *
 *	H5C_decr__off:	Don't attempt to decrease the size of the cache
 *		automatically.
 *
 *		When this increment mode is selected, the remaining fields
 *		in the cache size decrease section are ignored.
 *
 *	H5C_decr__threshold: Attempt to decrease the size of the cache
 *		whenever the average hit rate over the last epoch rises
 *		above the value	supplied in the upper_hr_threshold
 *		field.
 *
 *	H5C_decr__age_out:  At the end of each epoch, search the cache for
 *		entries that have not been accessed for at least the number
 *		of epochs specified in the epochs_before_eviction field, and
 *		evict these entries.  Conceptually, the maximum cache size
 *		is then decreased to match the new actual cache size.  However,
 *		this reduction may be modified by the min_size, the
 *		max_decrement, and/or the empty_reserve.
 *
 *	H5C_decr__age_out_with_threshold:  Same as age_out, but we only
 *		attempt to reduce the cache size when the hit rate observed
 *		over the last epoch exceeds the value provided in the
 *		upper_hr_threshold field.
 *
 * upper_hr_threshold: Upper hit rate threshold.  The use of this field
 *	varies according to the current decr_mode:
 *
 *	H5C_decr__off or H5C_decr__age_out:  The value of this field is
 *		ignored.
 *
 *	H5C_decr__threshold:  If the hit rate exceeds this threshold in any
 *		epoch, attempt to decrement the cache size by size_decrement.
 *
 *		Note that cache size may not be decremented below min_size.
 *
 *		Note also that if the upper_threshold is 1.0, the cache size
 *		will never be reduced.
 *
 *	H5C_decr__age_out_with_threshold:  If the hit rate exceeds this
 *		threshold in any epoch, attempt to reduce the cache size
 *		by evicting entries that have not been accessed for more
 *		than the specified number of epochs.
 *
 * decrement: This field is only used when the decr_mode is
 *	H5C_decr__threshold.
 *
 *	The field is a double containing the multiplier used to derive the
 *	new cache size from the old if a cache size decrement is triggered.
 *	The decrement must be in the range 0.0 (in which case the cache will
 *      try to contract to its minimum size) to 1.0 (in which case the
 *      cache will never shrink).
 *
 * apply_max_decrement:  Boolean flag used to determine whether decrements
 *	in cache size are to be limited by the max_decrement field.
 *
 * max_decrement: Maximum number of bytes by which the cache size can be
 *	decreased in a single re-size.  Note that decrements may also be
 *	restricted by the min_size of the cache, and (in age out modes) by
 *	the empty_reserve field.
 *
 * epochs_before_eviction:  Integer field used in H5C_decr__age_out and
 *	H5C_decr__age_out_with_threshold decrement modes.
 *
 *	This field contains the number of epochs an entry must remain
 *	unaccessed before it is evicted in an attempt to reduce the
 *	cache size.  If applicable, this field must lie in the range
 *	[1, H5C__MAX_EPOCH_MARKERS].
 *
 * apply_empty_reserve:  Boolean field controlling whether the empty_reserve
 *	field is to be used in computing the new cache size when the
 *	decr_mode is H5C_decr__age_out or H5C_decr__age_out_with_threshold.
 *
 * empty_reserve:  To avoid a constant racheting down of cache size by small
 *	amounts in the H5C_decr__age_out and H5C_decr__age_out_with_threshold
 *	modes, this field allows one to require that any cache size
 *	reductions leave the specified fraction of unused space in the cache.
 *
 *	The value of this field must be in the range [0.0, 1.0].  I would
 *	expect typical values to be in the range of 0.01 to 0.1.
 *
 ****************************************************************************/

#define H5C_RESIZE_CFG__VALIDATE_GENERAL        0x1
#define H5C_RESIZE_CFG__VALIDATE_INCREMENT      0x2
#define H5C_RESIZE_CFG__VALIDATE_DECREMENT      0x4
#define H5C_RESIZE_CFG__VALIDATE_INTERACTIONS   0x8
#define H5C_RESIZE_CFG__VALIDATE_ALL      \
(                                         \
    H5C_RESIZE_CFG__VALIDATE_GENERAL |    \
    H5C_RESIZE_CFG__VALIDATE_INCREMENT |  \
    H5C_RESIZE_CFG__VALIDATE_DECREMENT |  \
    H5C_RESIZE_CFG__VALIDATE_INTERACTIONS \
)

#define H5C__CURR_AUTO_SIZE_CTL_VER		1
#define H5C__CURR_AUTO_RESIZE_RPT_FCN_VER	1

#define H5C__MAX_EPOCH_MARKERS  		10

#define H5C__DEF_AR_UPPER_THRESHHOLD		0.9999
#define H5C__DEF_AR_LOWER_THRESHHOLD		0.9
#define H5C__DEF_AR_MAX_SIZE			((size_t)(16 * 1024 * 1024))
#define H5C__DEF_AR_INIT_SIZE			((size_t)( 1 * 1024 * 1024))
#define H5C__DEF_AR_MIN_SIZE			((size_t)( 1 * 1024 * 1024))
#define H5C__DEF_AR_MIN_CLEAN_FRAC		0.5
#define H5C__DEF_AR_INCREMENT			2.0
#define H5C__DEF_AR_MAX_INCREMENT		((size_t)( 2 * 1024 * 1024))
#define H5C__DEF_AR_FLASH_MULTIPLE              1.0
#define H5C__DEV_AR_FLASH_THRESHOLD             0.25
#define H5C__DEF_AR_DECREMENT			0.9
#define H5C__DEF_AR_MAX_DECREMENT		((size_t)( 1 * 1024 * 1024))
#define H5C__DEF_AR_EPCHS_B4_EVICT		3
#define H5C__DEF_AR_EMPTY_RESERVE		0.05
#define H5C__MIN_AR_EPOCH_LENGTH		100
#define H5C__DEF_AR_EPOCH_LENGTH		50000
#define H5C__MAX_AR_EPOCH_LENGTH		1000000

#define H5C__MIN_JBRB_BUF_SIZE			((size_t)(1))
#define H5C__MAX_JBRB_BUF_SIZE			((size_t)(2 * 1024 * 1024))

#define H5C__MIN_JBRB_NUM_BUFS			1
#define H5C__MAX_JBRB_NUM_BUFS			256

enum H5C_resize_status
{
    in_spec,
    increase,
    flash_increase,
    decrease,
    at_max_size,
    at_min_size,
    increase_disabled,
    decrease_disabled,
    not_full
}; /* enum H5C_resize_conditions */

typedef void (*H5C_auto_resize_rpt_fcn)(H5C_t * cache_ptr,
                                        int32_t version,
                                        double hit_rate,
                                        enum H5C_resize_status status,
                                        size_t old_max_cache_size,
                                        size_t new_max_cache_size,
                                        size_t old_min_clean_size,
                                        size_t new_min_clean_size);

typedef struct H5C_auto_size_ctl_t
{
    /* general configuration fields: */
    int32_t				version;
    H5C_auto_resize_rpt_fcn		rpt_fcn;

    hbool_t				set_initial_size;
    size_t				initial_size;

    double				min_clean_fraction;

    size_t				max_size;
    size_t				min_size;

    int64_t				epoch_length;


    /* size increase control fields: */
    enum H5C_cache_incr_mode		incr_mode;

    double				lower_hr_threshold;

    double				increment;

    hbool_t				apply_max_increment;
    size_t				max_increment;

    enum H5C_cache_flash_incr_mode      flash_incr_mode;
    double                              flash_multiple;
    double                              flash_threshold;


    /* size decrease control fields: */
    enum H5C_cache_decr_mode		decr_mode;

    double				upper_hr_threshold;

    double				decrement;

    hbool_t				apply_max_decrement;
    size_t				max_decrement;

    int32_t				epochs_before_eviction;

    hbool_t				apply_empty_reserve;
    double				empty_reserve;

} H5C_auto_size_ctl_t;


/****************************************************************************
 *
 * structure H5C_mdj_config_t
 *
 * H5C_mdj_config_t is a structure intended for use in comminicating
 * metadata journaling configuration data to and from the metadata
 * cache.
 *
 * In its initial incarnation, it is identical to the H5AC_jnl_config_t
 * structure less ther version, and journal_recovered fields.  However,
 * in the future we may support other types of journaling -- which will
 * likely require modification or replacement of the H5AC_jnl_config_t
 * structure.
 *
 * The fields of the structure are discussed individually below.  Note
 * that the fields with the "jbrb_" prefix are used to configure the
 * journal buffer ring buffer -- a ring buffer of buffers used to buffer
 * output of journal messages.
 *
 * enable_journaling:  Boolean flag that is set to TRUE if journaling is
 *      to be enabled, and to FALSE otherwise.
 *
 *      When the cache configuration is reported, this field is TRUE iff
 *      journaling is enabled.
 *
 * journal_file_path:  Full path of the file to be used to store the
 *      metadata journal.  This field is only defined if enable_journaling
 *      is TRUE.
 *
 *      At present, the length of the journal file path is restricted to
 *      no more than H5C__MAX_JOURNAL_FILE_NAME_LEN (which must equal
 *      H5AC__MAX_JOURNAL_FILE_NAME_LEN).
 *
 * jbrb_buf_size: size_t containing the size of each individual buffer
 *      in the journal buffer ring buffer.  This size should be chosen
 *      to be some multiple of the block size used by the file system
 *      on which the journal file will be written.
 *
 * jbrb_num_bufs: Integer containing the number of buffers in the journal
 *      buffer ring buffer.  If synchronous I/O is used, one or two buffers
 *      is sufficient.  If asynchronous I/O is used, the number of buffers
 *      should be sufficiently large that a write on buffer is likely to
 *      complete before that buffer is needed again.
 *
 * jbrb_use_aio:  Boolean flag indicating whether we should use
 *      asynchronous I/O for journal entry writes.
 *
 * jbrb_human_readable: Boolean flag which determines whether the journal
 *      file will be written in human readable form.  In general, this
 *      field should be set to false, as the human readable journal
 *      file is at least twice a large as the machine readable version.
 *
 ****************************************************************************/

#define H5C__MAX_JOURNAL_FILE_NAME_LEN        1024

typedef struct H5C_mdj_config_t
{
    hbool_t     enable_journaling;
    char        journal_file_path[H5C__MAX_JOURNAL_FILE_NAME_LEN + 1];
    size_t      jbrb_buf_size;
    int         jbrb_num_bufs;
    hbool_t     jbrb_use_aio;
    hbool_t     jbrb_human_readable;

} H5C_mdj_config_t;

/* The following typedef is used for callbacks on metadata journaling
 * configuration changes.
 */

typedef herr_t (*H5C_mdj_status_change_func_t)(const H5C_mdj_config_t * config_ptr,
                                              hid_t dxpl_id,
		                              void * data_ptr);


/*
 * Library prototypes.
 */

/* #defines of flags used in the flags parameters in some of the
 * following function calls.  Note that not all flags are applicable
 * to all function calls.  Flags that don't apply to a particular
 * function are ignored in that function.
 *
 * These flags apply to all function calls:
 *
 * 	H5C__NO_FLAGS_SET (generic "no flags set" for all fcn calls)
 *
 *
 * These flags apply to H5C_insert_entry():
 *
 * 	H5C__SET_FLUSH_MARKER_FLAG
 * 	H5C__PIN_ENTRY_FLAG
 *
 * These flags apply to H5C_protect()
 *
 * 	H5C__READ_ONLY_FLAG
 *
 * These flags apply to H5C_unprotect():
 *
 * 	H5C__SET_FLUSH_MARKER_FLAG
 * 	H5C__DELETED_FLAG
 * 	H5C__DIRTIED_FLAG
 * 	H5C__SIZE_CHANGED_FLAG
 * 	H5C__PIN_ENTRY_FLAG
 * 	H5C__UNPIN_ENTRY_FLAG
 *
 *
 * These flags apply to H5C_flush_cache():
 *
 * 	H5C__FLUSH_INVALIDATE_FLAG
 * 	H5C__FLUSH_CLEAR_ONLY_FLAG
 * 	H5C__FLUSH_MARKED_ENTRIES_FLAG
 *	H5C__FLUSH_IGNORE_PROTECTED_FLAG (can't use this flag in combination
 *					  with H5C__FLUSH_INVALIDATE_FLAG)
 *
 * These flags apply to H5C_flush_single_entry():
 *
 * 	H5C__FLUSH_INVALIDATE_FLAG
 * 	H5C__FLUSH_CLEAR_ONLY_FLAG
 * 	H5C__FLUSH_MARKED_ENTRIES_FLAG
 */

#define H5C__NO_FLAGS_SET			0x0000
#define H5C__SET_FLUSH_MARKER_FLAG		0x0001
#define H5C__DELETED_FLAG			0x0002
#define H5C__DIRTIED_FLAG			0x0004
#define H5C__SIZE_CHANGED_FLAG			0x0008
#define H5C__PIN_ENTRY_FLAG			0x0010
#define H5C__UNPIN_ENTRY_FLAG			0x0020
#define H5C__FLUSH_INVALIDATE_FLAG		0x0040
#define H5C__FLUSH_CLEAR_ONLY_FLAG		0x0080
#define H5C__FLUSH_MARKED_ENTRIES_FLAG		0x0100
#define H5C__FLUSH_IGNORE_PROTECTED_FLAG	0x0200
#define H5C__READ_ONLY_FLAG			0x0400

H5_DLL H5C_t * H5C_create(size_t                     max_cache_size,
                          size_t                     min_clean_size,
                          int                        max_type_id,
			  const char *               (* type_name_table_ptr),
                          H5C_write_permitted_func_t check_write_permitted,
                          hbool_t                    write_permitted,
                          H5C_log_flush_func_t       log_flush,
                          void *                     aux_ptr);

H5_DLL void H5C_def_auto_resize_rpt_fcn(H5C_t * cache_ptr,
                                        int32_t version,
                                        double hit_rate,
                                        enum H5C_resize_status status,
                                        size_t old_max_cache_size,
                                        size_t new_max_cache_size,
                                        size_t old_min_clean_size,
                                        size_t new_min_clean_size);

H5_DLL herr_t H5C_dest(H5F_t * f,
		       hid_t  dxpl_id);

H5_DLL herr_t H5C_dest_empty(H5C_t * cache_ptr);

H5_DLL herr_t H5C_expunge_entry(H5F_t *             f,
		                hid_t               dxpl_id,
                                const H5C_class_t * type,
                                haddr_t             addr);

H5_DLL herr_t H5C_flush_cache(H5F_t *f, hid_t dxpl_id, unsigned flags);


H5_DLL herr_t H5C_flush_to_min_clean(H5F_t * f,
		                     hid_t    dxpl_id);

H5_DLL herr_t H5C_get_cache_auto_resize_config(const H5C_t * cache_ptr,
                                               H5C_auto_size_ctl_t *config_ptr);

H5_DLL herr_t H5C_get_cache_size(H5C_t * cache_ptr,
                                 size_t * max_size_ptr,
                                 size_t * min_clean_size_ptr,
                                 size_t * cur_size_ptr,
                                 int32_t * cur_num_entries_ptr);

H5_DLL herr_t H5C_get_cache_hit_rate(H5C_t * cache_ptr,
                                     double * hit_rate_ptr);

H5_DLL herr_t H5C_get_entry_status(const H5F_t *f,
                                   haddr_t   addr,
                                   size_t *  size_ptr,
                                   hbool_t * in_cache_ptr,
                                   hbool_t * is_dirty_ptr,
                                   hbool_t * is_protected_ptr,
                                   hbool_t * is_pinned_ptr);

H5_DLL herr_t H5C_get_evictions_enabled(const H5C_t * cache_ptr,
                                        hbool_t * evictions_enabled_ptr);

H5_DLL herr_t H5C_get_trace_file_ptr(const H5C_t *cache_ptr,
    FILE **trace_file_ptr_ptr);
H5_DLL herr_t H5C_get_trace_file_ptr_from_entry(const H5C_cache_entry_t *entry_ptr,
    FILE **trace_file_ptr_ptr);

H5_DLL herr_t H5C_insert_entry(H5F_t *             f,
                               hid_t               dxpl_id,
                               const H5C_class_t * type,
                               haddr_t             addr,
			       size_t              len,
                               void *              thing,
                               unsigned int        flags);

H5_DLL herr_t H5C_mark_entries_as_clean(H5F_t *  f,
                                        hid_t    dxpl_id,
                                        int32_t  ce_array_len,
                                        haddr_t *ce_array_ptr);

H5_DLL herr_t H5C_mark_pinned_entry_dirty(void *  thing,
					  hbool_t size_changed,
					  size_t  new_size);

H5_DLL herr_t H5C_mark_pinned_or_protected_entry_dirty(void *thing);

H5_DLL herr_t H5C_rename_entry(H5C_t *             cache_ptr,
                               const H5C_class_t * type,
                               haddr_t             old_addr,
                               haddr_t             new_addr);

H5_DLL herr_t H5C_pin_protected_entry(void *thing);

H5_DLL void * H5C_protect(H5F_t *             f,
		          hid_t               dxpl_id,
			  const H5C_class_t * type,
                          haddr_t             addr,
			  size_t	      len,
                          void *              udata,
                          unsigned            flags);

H5_DLL herr_t H5C_reset_cache_hit_rate_stats(H5C_t * cache_ptr);

H5_DLL herr_t H5C_resize_pinned_entry(void *thing, size_t new_size);

H5_DLL herr_t H5C_set_cache_auto_resize_config(H5C_t *cache_ptr,
                                               H5C_auto_size_ctl_t *config_ptr);

H5_DLL herr_t H5C_set_evictions_enabled(H5C_t *cache_ptr,
                                        hbool_t evictions_enabled);

H5_DLL herr_t H5C_set_prefix(H5C_t * cache_ptr, char * prefix);

H5_DLL herr_t H5C_set_skip_flags(H5C_t * cache_ptr,
                                 hbool_t skip_file_checks,
                                 hbool_t skip_dxpl_id_checks);

H5_DLL herr_t H5C_set_trace_file_ptr(H5C_t * cache_ptr,
		                     FILE * trace_file_ptr);

H5_DLL herr_t H5C_stats(H5C_t * cache_ptr,
                        const char * cache_name,
                        hbool_t display_detailed_stats);

H5_DLL void H5C_stats__reset(H5C_t * cache_ptr);

H5_DLL herr_t H5C_unpin_entry(void *thing);

H5_DLL herr_t H5C_unprotect(H5F_t *             f,
		            hid_t               dxpl_id,
                            const H5C_class_t * type,
                            haddr_t             addr,
                            void *              thing,
                            unsigned int        flags,
                            size_t              new_size);

H5_DLL herr_t H5C_validate_resize_config(H5C_auto_size_ctl_t * config_ptr,
                                         unsigned int tests);


/**************************************************************************/
/***************** journaling function definitions proper: ****************/
/**************************************************************************/

H5_DLL herr_t H5C_begin_journaling(H5F_t * f,
                                   hid_t dxpl_id,
				   H5C_t * cache_ptr,
				   H5C_mdj_config_t * config_ptr);

H5_DLL herr_t H5C_begin_transaction(H5C_t * cache_ptr,
		                    uint64_t * trans_num_ptr,
                                    const char * api_call_name);

H5_DLL herr_t H5C_end_journaling(H5F_t * f,
                                 hid_t dxpl_id,
				 H5C_t * cache_ptr);

H5_DLL herr_t H5C_end_transaction(H5F_t * f,
		                  hid_t dxpl_id,
                                  H5C_t * cache_ptr,
                                  uint64_t trans_num,
                                  const char * api_call_name);

H5_DLL herr_t H5C_get_journal_config(H5C_t * cache_ptr,
		                     H5C_mdj_config_t * config_ptr);

H5_DLL herr_t H5C_journal_post_flush(H5F_t * f,
                                     hid_t dxpl_id,
				     H5C_t * cache_ptr,
                                     hbool_t cache_is_clean);

H5_DLL herr_t H5C_journal_pre_flush(H5C_t * cache_ptr);

H5_DLL herr_t H5C_journal_transaction(H5F_t * f,
		                      hid_t dxpl_id,
                                      H5C_t * cache_ptr);

H5_DLL herr_t H5C_update_for_new_last_trans_on_disk(H5C_t * cache_ptr,
                                             uint64_t new_last_trans_on_disk);


/*****************************************************************************/
/****************** journal buffer function definitions: *********************/
/*****************************************************************************/

typedef struct H5C_jbrb_t H5C_jbrb_t;

/* Note that H5C_jb_aio_await_completion_of_all_async_fsyncs() and
 * H5C_jb_aio__await_completion_of_all_pending_writes() are declared
 * in this header file purely for testing purposes -- they should not be
 * called outside test/cache_journal.c
 */
H5_DLL herr_t H5C_jb_aio__await_completion_of_all_async_fsyncs(
						H5C_jbrb_t * struct_ptr);

H5_DLL herr_t H5C_jb_aio__await_completion_of_all_pending_writes(
						H5C_jbrb_t * struct_ptr);


H5_DLL herr_t H5C_jb__bin2hex(const uint8_t * buf,
                              char * hexdata,
                              size_t * hexlength,
                              size_t buf_size);

H5_DLL herr_t H5C_jb__comment(H5C_jbrb_t * struct_ptr,
                              const char * comment_ptr);

H5_DLL herr_t H5C_jb__end_transaction(H5C_jbrb_t * struct_ptr,
                                      uint64_t trans_num);

H5_DLL herr_t H5C_jb__eoa(H5C_jbrb_t * struct_ptr,
		          haddr_t eoa);

H5_DLL herr_t H5C_jb__get_last_transaction_on_disk(H5C_jbrb_t * struct_ptr,
                                                   uint64_t * trans_num_ptr);

H5_DLL herr_t H5C_jb__flush(H5C_jbrb_t * struct_ptr);


H5_DLL herr_t H5C_jb__journal_entry(H5C_jbrb_t * struct_ptr,
                                    uint64_t trans_num,
                                    haddr_t base_addr,
                                    size_t length,
                                    const uint8_t * body);

H5_DLL herr_t H5C_jb__init(H5C_jbrb_t * struct_ptr,
                           const int32_t journal_magic,
                           const char * HDF5_file_name,
                           const char * journal_file_name,
                           size_t buf_size,
                           int num_bufs,
                           hbool_t use_aio,
                           hbool_t human_readable,
                           size_t sizeof_addr,
                           size_t sizeof_size);

H5_DLL herr_t H5C_jb__start_transaction(H5C_jbrb_t * struct_ptr,
                                        uint64_t trans_num);

H5_DLL herr_t H5C_jb__takedown(H5C_jbrb_t * struct_ptr);

H5_DLL herr_t H5C_jb__trunc(H5C_jbrb_t * struct_ptr);

H5_DLL herr_t H5C_jb__write_header_entry(H5C_jbrb_t * struct_ptr);

H5_DLL herr_t H5C_jb__write_to_buffer(H5C_jbrb_t * struct_ptr,
		                      size_t size,
                                      const char * data,
                                      hbool_t is_end_trans,
                                      uint64_t trans_num);

/*****************************************************************************/
/***** superblock journaling message management function definitions: ********/
/*****************************************************************************/

H5_DLL herr_t H5C_check_for_journaling(H5F_t * f,
                                       hid_t dxpl_id,
		                       H5C_t * cache_ptr,
                                       hbool_t journal_recovered);

H5_DLL herr_t H5C_mark_journaling_in_progress(H5F_t * f,
                                            hid_t dxpl_id,
					    const int32_t journal_magic,
                                            const char * journal_file_name_ptr);

H5_DLL herr_t H5C_unmark_journaling_in_progress(H5F_t * f,
                                                hid_t dxpl_id,
						H5C_t * cache_ptr);


/**************************************************************************/
/****** metadata journaling status change callback management code ********/
/**************************************************************************/

H5_DLL herr_t H5C_deregister_mdjsc_callback(H5C_t * cache_ptr,
                                            int32_t idx);

H5_DLL herr_t H5C_register_mdjsc_callback(H5C_t * cache_ptr,
                                          H5C_mdj_status_change_func_t fcn_ptr,
                                          void * data_ptr,
                                          int32_t * idx_ptr);

#endif /* !_H5Cprivate_H */

