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

/* Programmer:  John Mainzer
 *              6/9/04
 *
 *		This file contains tests for the cache implemented in 
 *		H5C.c
 */
#include "h5test.h"
#include "H5Iprivate.h"

const char *FILENAME[] = {
    "cache",
    NULL
};

#define H5C_PACKAGE             /*suppress error about including H5Cpkg   */

#include "H5Cpkg.h"

/* with apologies for the abuse of terminology... */

#define PICO_ENTRY_TYPE		0
#define NANO_ENTRY_TYPE		1
#define MICRO_ENTRY_TYPE	2
#define TINY_ENTRY_TYPE		3
#define SMALL_ENTRY_TYPE	4
#define MEDIUM_ENTRY_TYPE	5
#define LARGE_ENTRY_TYPE	6
#define HUGE_ENTRY_TYPE		7
#define MONSTER_ENTRY_TYPE	8

#define NUMBER_OF_ENTRY_TYPES	9

#define PICO_ENTRY_SIZE		(size_t)1
#define NANO_ENTRY_SIZE		(size_t)4
#define MICRO_ENTRY_SIZE	(size_t)16
#define TINY_ENTRY_SIZE		(size_t)64
#define SMALL_ENTRY_SIZE	(size_t)256
#define MEDIUM_ENTRY_SIZE	(size_t)1024
#define LARGE_ENTRY_SIZE	(size_t)(4 * 1024)
#define HUGE_ENTRY_SIZE		(size_t)(16 * 1024)
#define MONSTER_ENTRY_SIZE	(size_t)(64 * 1024)

#define NUM_PICO_ENTRIES	(10 * 1024)
#define NUM_NANO_ENTRIES	(10 * 1024)
#define NUM_MICRO_ENTRIES	(10 * 1024)
#define NUM_TINY_ENTRIES	(10 * 1024)
#define NUM_SMALL_ENTRIES	(10 * 1024)
#define NUM_MEDIUM_ENTRIES	(10 * 1024)
#define NUM_LARGE_ENTRIES	(10 * 1024)
#define NUM_HUGE_ENTRIES	(10 * 1024)
#define NUM_MONSTER_ENTRIES	(10 * 1024)

#define MAX_ENTRIES		(10 * 1024)

#define PICO_BASE_ADDR		(haddr_t)0
#define NANO_BASE_ADDR		(haddr_t)(PICO_BASE_ADDR + \
                                      (PICO_ENTRY_SIZE * NUM_PICO_ENTRIES))
#define MICRO_BASE_ADDR		(haddr_t)(NANO_BASE_ADDR + \
                                      (NANO_ENTRY_SIZE * NUM_NANO_ENTRIES))
#define TINY_BASE_ADDR		(haddr_t)(MICRO_BASE_ADDR + \
			              (MICRO_ENTRY_SIZE * NUM_MICRO_ENTRIES))
#define SMALL_BASE_ADDR		(haddr_t)(TINY_BASE_ADDR + \
                                      (TINY_ENTRY_SIZE * NUM_TINY_ENTRIES))
#define MEDIUM_BASE_ADDR	(haddr_t)(SMALL_BASE_ADDR + \
                                      (SMALL_ENTRY_SIZE * NUM_SMALL_ENTRIES))
#define LARGE_BASE_ADDR		(haddr_t)(MEDIUM_BASE_ADDR + \
                                      (MEDIUM_ENTRY_SIZE * NUM_MEDIUM_ENTRIES))
#define HUGE_BASE_ADDR		(haddr_t)(LARGE_BASE_ADDR + \
                                      (LARGE_ENTRY_SIZE * NUM_LARGE_ENTRIES))
#define MONSTER_BASE_ADDR	(haddr_t)(HUGE_BASE_ADDR + \
                                      (HUGE_ENTRY_SIZE * NUM_HUGE_ENTRIES))

#define PICO_ALT_BASE_ADDR	(haddr_t)(MONSTER_BASE_ADDR + \
				     (MONSTER_ENTRY_SIZE * NUM_MONSTER_ENTRIES))
#define NANO_ALT_BASE_ADDR	(haddr_t)(PICO_ALT_BASE_ADDR + \
                                      (PICO_ENTRY_SIZE * NUM_PICO_ENTRIES))
#define MICRO_ALT_BASE_ADDR	(haddr_t)(NANO_ALT_BASE_ADDR + \
                                      (NANO_ENTRY_SIZE * NUM_NANO_ENTRIES))
#define TINY_ALT_BASE_ADDR	(haddr_t)(MICRO_ALT_BASE_ADDR + \
			              (MICRO_ENTRY_SIZE * NUM_MICRO_ENTRIES))
#define SMALL_ALT_BASE_ADDR	(haddr_t)(TINY_ALT_BASE_ADDR + \
                                      (TINY_ENTRY_SIZE * NUM_TINY_ENTRIES))
#define MEDIUM_ALT_BASE_ADDR	(haddr_t)(SMALL_ALT_BASE_ADDR + \
                                      (SMALL_ENTRY_SIZE * NUM_SMALL_ENTRIES))
#define LARGE_ALT_BASE_ADDR	(haddr_t)(MEDIUM_ALT_BASE_ADDR + \
                                      (MEDIUM_ENTRY_SIZE * NUM_MEDIUM_ENTRIES))
#define HUGE_ALT_BASE_ADDR	(haddr_t)(LARGE_ALT_BASE_ADDR + \
                                      (LARGE_ENTRY_SIZE * NUM_LARGE_ENTRIES))
#define MONSTER_ALT_BASE_ADDR	(haddr_t)(HUGE_ALT_BASE_ADDR + \
                                      (HUGE_ENTRY_SIZE * NUM_HUGE_ENTRIES))

typedef struct test_entry_t 
{
    H5C_cache_entry_t	  header;	/* entry data used by the cache 
					 * -- must be first 
                               		 */
    struct test_entry_t * self; 	/* pointer to this entry -- used for
					 * sanity checking.
                                         */
    haddr_t		  addr;         /* where the cache thinks this entry 
                                         * is located 
                                         */
    hbool_t		  at_main_addr;	/* boolean flag indicating whether
					 * the entry is supposed to be at
					 * either its main or alternate 
					 * address.
     					 */
    haddr_t		  main_addr;    /* initial location of the entry
                                         */
    haddr_t		  alt_addr;	/* location to which the entry
					 * can be relocated or "renamed"
                                         */
    size_t		  size;         /* how big the cache thinks this 
                                         * entry is 
                                         */
    int32_t		  type;		/* indicates which entry array this 
					 * entry is in 
                                         */
    int32_t		  index;	/* index in its entry array 
                                         */
    int32_t		  reads;	/* number of times this entry has
					 * been loaded.
                                         */
    int32_t		  writes;	/* number of times this entry has 
                                         * been written 
                                         */
    hbool_t		  is_dirty;	/* entry has been modified since 
                                         * last write 
                                         */
    hbool_t		  is_protected;	/* entry should currently be on 
					 * the cache's protected list.
                                         */
    hbool_t		  loaded;       /* entry has been loaded since the 
                                         * last time it was reset.
                                         */
    hbool_t		  cleared;      /* entry has been cleared since the 
                                         * last time it was reset.
                                         */
    hbool_t		  flushed;      /* entry has been flushed since the 
                                         * last time it was reset.
                                         */
    hbool_t               destroyed;    /* entry has been destroyed since the
                                         * last time it was reset.
                                         */
} test_entry_t;

/* The following is a cut down copy of the hash table manipulation 
 * macros from H5C.c, which have been further modified to avoid references
 * to the error reporting macros.  Needless to say, these macros must be
 * updated as necessary.
 */

#define H5C__HASH_MASK          ((size_t)(H5C__HASH_TABLE_LEN - 1) << 3)
#define H5C__HASH_FCN(x)        (int)(((x) & H5C__HASH_MASK) >> 3)

#define H5C__PRE_HT_SEARCH_SC(cache_ptr, Addr)          \
if ( ( (cache_ptr) == NULL ) ||                         \
     ( (cache_ptr)->magic != H5C__H5C_T_MAGIC ) ||      \
     ( ! H5F_addr_defined(Addr) ) ||                    \
     ( H5C__HASH_FCN(Addr) < 0 ) ||                     \
     ( H5C__HASH_FCN(Addr) >= H5C__HASH_TABLE_LEN ) ) { \
    HDfprintf(stdout, "Pre HT search SC failed.\n");    \
}

#define H5C__POST_SUC_HT_SEARCH_SC(cache_ptr, entry_ptr, Addr, k) \
if ( ( (cache_ptr) == NULL ) ||                                   \
     ( (cache_ptr)->magic != H5C__H5C_T_MAGIC ) ||                \
     ( (cache_ptr)->index_len < 1 ) ||                            \
     ( (entry_ptr) == NULL ) ||                                   \
     ( (cache_ptr)->index_size < (entry_ptr)->size ) ||           \
     ( H5F_addr_ne((entry_ptr)->addr, (Addr)) ) ||                \
     ( (entry_ptr)->size <= 0 ) ||                                \
     ( ((cache_ptr)->index)[k] == NULL ) ||                       \
     ( ( ((cache_ptr)->index)[k] != (entry_ptr) ) &&              \
       ( (entry_ptr)->ht_prev == NULL ) ) ||                      \
     ( ( ((cache_ptr)->index)[k] == (entry_ptr) ) &&              \
       ( (entry_ptr)->ht_prev != NULL ) ) ||                      \
     ( ( (entry_ptr)->ht_prev != NULL ) &&                        \
       ( (entry_ptr)->ht_prev->ht_next != (entry_ptr) ) ) ||      \
     ( ( (entry_ptr)->ht_next != NULL ) &&                        \
       ( (entry_ptr)->ht_next->ht_prev != (entry_ptr) ) ) ) {     \
    HDfprintf(stdout, "Post successful HT search SC failed.\n");  \
}


#define H5C__SEARCH_INDEX(cache_ptr, Addr, entry_ptr)                   \
{                                                                       \
    int k;                                                              \
    int depth = 0;                                                      \
    H5C__PRE_HT_SEARCH_SC(cache_ptr, Addr)                              \
    k = H5C__HASH_FCN(Addr);                                            \
    entry_ptr = ((cache_ptr)->index)[k];                                \
    while ( ( entry_ptr ) && ( H5F_addr_ne(Addr, (entry_ptr)->addr) ) ) \
    {                                                                   \
        (entry_ptr) = (entry_ptr)->ht_next;                             \
        (depth)++;                                                      \
    }                                                                   \
    if ( entry_ptr )                                                    \
    {                                                                   \
        H5C__POST_SUC_HT_SEARCH_SC(cache_ptr, entry_ptr, Addr, k)       \
        if ( entry_ptr != ((cache_ptr)->index)[k] )                     \
        {                                                               \
            if ( (entry_ptr)->ht_next )                                 \
            {                                                           \
                (entry_ptr)->ht_next->ht_prev = (entry_ptr)->ht_prev;   \
            }                                                           \
            HDassert( (entry_ptr)->ht_prev != NULL );                   \
            (entry_ptr)->ht_prev->ht_next = (entry_ptr)->ht_next;       \
            ((cache_ptr)->index)[k]->ht_prev = (entry_ptr);             \
            (entry_ptr)->ht_next = ((cache_ptr)->index)[k];             \
            (entry_ptr)->ht_prev = NULL;                                \
            ((cache_ptr)->index)[k] = (entry_ptr);                      \
        }                                                               \
    }                                                                   \
}


/* misc type definitions */

struct flush_cache_test_spec
{
    int			entry_num;
    int			entry_type;
    int			entry_index;
    hbool_t		insert_flag;
    hbool_t		dirty_flag;
    unsigned int	flags;
    hbool_t		expected_loaded;
    hbool_t		expected_cleared;
    hbool_t		expected_flushed;
    hbool_t		expected_destroyed;
};


/* global variable declarations: */

static hbool_t write_permitted = TRUE;
static hbool_t pass = TRUE; /* set to false on error */
static hbool_t skip_long_tests = TRUE;
const char *failure_mssg = NULL;

test_entry_t pico_entries[NUM_PICO_ENTRIES];
test_entry_t nano_entries[NUM_NANO_ENTRIES];
test_entry_t micro_entries[NUM_MICRO_ENTRIES];
test_entry_t tiny_entries[NUM_TINY_ENTRIES];
test_entry_t small_entries[NUM_SMALL_ENTRIES];
test_entry_t medium_entries[NUM_MEDIUM_ENTRIES];
test_entry_t large_entries[NUM_LARGE_ENTRIES];
test_entry_t huge_entries[NUM_HUGE_ENTRIES];
test_entry_t monster_entries[NUM_MONSTER_ENTRIES];

test_entry_t * entries[NUMBER_OF_ENTRY_TYPES] =
{
    pico_entries,
    nano_entries,
    micro_entries,
    tiny_entries,
    small_entries,
    medium_entries,
    large_entries,
    huge_entries,
    monster_entries
};

const int32_t max_indices[NUMBER_OF_ENTRY_TYPES] =
{
    NUM_PICO_ENTRIES - 1,
    NUM_NANO_ENTRIES - 1,
    NUM_MICRO_ENTRIES - 1,
    NUM_TINY_ENTRIES - 1,
    NUM_SMALL_ENTRIES - 1,
    NUM_MEDIUM_ENTRIES - 1,
    NUM_LARGE_ENTRIES - 1,
    NUM_HUGE_ENTRIES - 1,
    NUM_MONSTER_ENTRIES - 1
};

const size_t entry_sizes[NUMBER_OF_ENTRY_TYPES] =
{
    PICO_ENTRY_SIZE,
    NANO_ENTRY_SIZE,
    MICRO_ENTRY_SIZE,
    TINY_ENTRY_SIZE, 
    SMALL_ENTRY_SIZE,
    MEDIUM_ENTRY_SIZE,
    LARGE_ENTRY_SIZE,
    HUGE_ENTRY_SIZE, 
    MONSTER_ENTRY_SIZE
};

const haddr_t base_addrs[NUMBER_OF_ENTRY_TYPES] =
{
    PICO_BASE_ADDR,
    NANO_BASE_ADDR,
    MICRO_BASE_ADDR,
    TINY_BASE_ADDR,
    SMALL_BASE_ADDR,
    MEDIUM_BASE_ADDR,
    LARGE_BASE_ADDR,
    HUGE_BASE_ADDR,
    MONSTER_BASE_ADDR
};

const haddr_t alt_base_addrs[NUMBER_OF_ENTRY_TYPES] =
{
    PICO_ALT_BASE_ADDR,
    NANO_ALT_BASE_ADDR,
    MICRO_ALT_BASE_ADDR,
    TINY_ALT_BASE_ADDR,
    SMALL_ALT_BASE_ADDR,
    MEDIUM_ALT_BASE_ADDR,
    LARGE_ALT_BASE_ADDR,
    HUGE_ALT_BASE_ADDR,
    MONSTER_ALT_BASE_ADDR
};

const char * entry_type_names[NUMBER_OF_ENTRY_TYPES] =
{
    "pico entries -- 1 B",
    "nano entries -- 4 B",
    "micro entries -- 16 B",
    "tiny entries -- 64 B",
    "small entries -- 256 B",
    "medium entries -- 1 KB",
    "large entries -- 4 KB",
    "huge entries -- 16 KB",
    "monster entries -- 64 KB"
};


/* call back function declarations: */

static herr_t check_write_permitted(const H5F_t UNUSED * f,
                                    hid_t UNUSED dxpl_id,
                                    hbool_t * write_permitted_ptr);

static herr_t clear(H5F_t * f, void * thing, hbool_t dest);

herr_t pico_clear(H5F_t * f, void *  thing, hbool_t dest);
herr_t nano_clear(H5F_t * f, void *  thing, hbool_t dest);
herr_t micro_clear(H5F_t * f, void *  thing, hbool_t dest);
herr_t tiny_clear(H5F_t * f, void *  thing, hbool_t dest);
herr_t small_clear(H5F_t * f, void *  thing, hbool_t dest);
herr_t medium_clear(H5F_t * f, void *  thing, hbool_t dest);
herr_t large_clear(H5F_t * f, void *  thing, hbool_t dest);
herr_t huge_clear(H5F_t * f, void *  thing, hbool_t dest);
herr_t monster_clear(H5F_t * f, void *  thing, hbool_t dest);


static herr_t destroy(H5F_t UNUSED * f, void * thing);

herr_t pico_dest(H5F_t * f, void * thing);
herr_t nano_dest(H5F_t * f, void * thing);
herr_t micro_dest(H5F_t * f, void * thing);
herr_t tiny_dest(H5F_t * f, void * thing);
herr_t small_dest(H5F_t * f, void * thing);
herr_t medium_dest(H5F_t * f, void * thing);
herr_t large_dest(H5F_t * f, void * thing);
herr_t huge_dest(H5F_t * f, void *  thing);
herr_t monster_dest(H5F_t * f, void *  thing);


static herr_t flush(H5F_t *f, hid_t UNUSED dxpl_id, hbool_t dest, 
                    haddr_t addr, void *thing);

herr_t pico_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, 
                  haddr_t addr, void *thing);
herr_t nano_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, 
                  haddr_t addr, void *thing);
herr_t micro_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, 
                   haddr_t addr, void *thing);
herr_t tiny_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, 
                  haddr_t addr, void *thing);
herr_t small_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, 
                   haddr_t addr, void *thing);
herr_t medium_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, 
                    haddr_t addr, void *thing);
herr_t large_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, 
                   haddr_t addr, void *thing);
herr_t huge_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, 
                  haddr_t addr, void *thing);
herr_t monster_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, 
                     haddr_t addr, void *thing);


static void * load(H5F_t UNUSED *f, hid_t UNUSED dxpl_id, haddr_t addr,
                   const void UNUSED *udata1, void UNUSED *udata2);

void * pico_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, 
                 const void *udata1, void *udata2);
void * nano_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, 
                 const void *udata1, void *udata2);
void * micro_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, 
                  const void *udata1, void *udata2);
void * tiny_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, 
                 const void *udata1, void *udata2);
void * small_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, 
                  const void *udata1, void *udata2);
void * medium_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, 
                   const void *udata1, void *udata2);
void * large_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, 
                  const void *udata1, void *udata2);
void * huge_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, 
                 const void *udata1, void *udata2);
void * monster_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, 
                    const void *udata1, void *udata2);


static herr_t size(H5F_t UNUSED * f, void * thing, size_t * size_ptr);

herr_t pico_size(H5F_t * f, void * thing, size_t * size_ptr);
herr_t nano_size(H5F_t * f, void * thing, size_t * size_ptr);
herr_t micro_size(H5F_t * f, void * thing, size_t * size_ptr);
herr_t tiny_size(H5F_t * f, void * thing, size_t * size_ptr);
herr_t small_size(H5F_t * f, void * thing, size_t * size_ptr);
herr_t medium_size(H5F_t * f, void * thing, size_t * size_ptr);
herr_t large_size(H5F_t * f, void * thing, size_t * size_ptr);
herr_t huge_size(H5F_t * f, void * thing, size_t * size_ptr);
herr_t monster_size(H5F_t * f, void * thing, size_t * size_ptr);


/* callback table declaration */

static const H5C_class_t types[NUMBER_OF_ENTRY_TYPES] =
{
  {
    PICO_ENTRY_TYPE,
    (H5C_load_func_t)pico_load,
    (H5C_flush_func_t)pico_flush,
    (H5C_dest_func_t)pico_dest,
    (H5C_clear_func_t)pico_clear,
    (H5C_size_func_t)pico_size
  },
  {
    NANO_ENTRY_TYPE,
    (H5C_load_func_t)nano_load,
    (H5C_flush_func_t)nano_flush,
    (H5C_dest_func_t)nano_dest,
    (H5C_clear_func_t)nano_clear,
    (H5C_size_func_t)nano_size
  },
  {
    MICRO_ENTRY_TYPE,
    (H5C_load_func_t)micro_load,
    (H5C_flush_func_t)micro_flush,
    (H5C_dest_func_t)micro_dest,
    (H5C_clear_func_t)micro_clear,
    (H5C_size_func_t)micro_size
  },
  {
    TINY_ENTRY_TYPE,
    (H5C_load_func_t)tiny_load,
    (H5C_flush_func_t)tiny_flush,
    (H5C_dest_func_t)tiny_dest,
    (H5C_clear_func_t)tiny_clear,
    (H5C_size_func_t)tiny_size
  },
  {
    SMALL_ENTRY_TYPE,
    (H5C_load_func_t)small_load,
    (H5C_flush_func_t)small_flush,
    (H5C_dest_func_t)small_dest,
    (H5C_clear_func_t)small_clear,
    (H5C_size_func_t)small_size
  },
  {
    MEDIUM_ENTRY_TYPE,
    (H5C_load_func_t)medium_load,
    (H5C_flush_func_t)medium_flush,
    (H5C_dest_func_t)medium_dest,
    (H5C_clear_func_t)medium_clear,
    (H5C_size_func_t)medium_size
  },
  {
    LARGE_ENTRY_TYPE,
    (H5C_load_func_t)large_load,
    (H5C_flush_func_t)large_flush,
    (H5C_dest_func_t)large_dest,
    (H5C_clear_func_t)large_clear,
    (H5C_size_func_t)large_size
  },
  {
    HUGE_ENTRY_TYPE,
    (H5C_load_func_t)huge_load,
    (H5C_flush_func_t)huge_flush,
    (H5C_dest_func_t)huge_dest,
    (H5C_clear_func_t)huge_clear,
    (H5C_size_func_t)huge_size
  },
  {
    MONSTER_ENTRY_TYPE,
    (H5C_load_func_t)monster_load,
    (H5C_flush_func_t)monster_flush,
    (H5C_dest_func_t)monster_dest,
    (H5C_clear_func_t)monster_clear,
    (H5C_size_func_t)monster_size
  }
};


/* private function declarations: */

static void addr_to_type_and_index(haddr_t addr, 
                                   int32_t * type_ptr, 
                                   int32_t * index_ptr);

#if 0 /* keep this for a while -- it may be useful */
static haddr_t type_and_index_to_addr(int32_t type, 
                                      int32_t idx);
#endif

static void insert_entry(H5C_t * cache_ptr,
                         int32_t type,
                         int32_t idx,
                         hbool_t dirty, 
                         unsigned int flags);

static void rename_entry(H5C_t * cache_ptr,
                         int32_t type,
                         int32_t idx,
                         hbool_t main_addr);

static void protect_entry(H5C_t * cache_ptr,
                          int32_t type,
                          int32_t idx);

hbool_t entry_in_cache(H5C_t * cache_ptr,
                       int32_t type,
                       int32_t idx);

static void reset_entries(void);

static H5C_t * setup_cache(size_t max_cache_size, size_t min_clean_size);

static void row_major_scan_forward(H5C_t * cache_ptr,
                                   int32_t lag,
                                   hbool_t verbose,
                                   hbool_t reset_stats,
                                   hbool_t display_stats,
                                   hbool_t display_detailed_stats,
                                   hbool_t do_inserts,
                                   hbool_t dirty_inserts,
                                   hbool_t do_renames,
                                   hbool_t rename_to_main_addr,
                                   hbool_t do_destroys,
                                   int dirty_destroys,
                                   int dirty_unprotects);

static void hl_row_major_scan_forward(H5C_t * cache_ptr,
                                      hbool_t verbose,
                                      hbool_t reset_stats,
                                      hbool_t display_stats,
                                      hbool_t display_detailed_stats,
                                      hbool_t do_inserts,
                                      hbool_t dirty_inserts);

static void row_major_scan_backward(H5C_t * cache_ptr,
                                    int32_t lag,
                                    hbool_t verbose,
                                    hbool_t reset_stats,
                                    hbool_t display_stats,
                                    hbool_t display_detailed_stats,
                                    hbool_t do_inserts,
                                    hbool_t dirty_inserts,
                                    hbool_t do_renames,
                                    hbool_t rename_to_main_addr,
                                    hbool_t do_destroys,
                                    int dirty_destroys,
                                    int dirty_unprotects);

static void hl_row_major_scan_backward(H5C_t * cache_ptr,
                                       hbool_t verbose,
                                       hbool_t reset_stats,
                                       hbool_t display_stats,
                                       hbool_t display_detailed_stats,
                                       hbool_t do_inserts,
                                       hbool_t dirty_inserts);

static void col_major_scan_forward(H5C_t * cache_ptr,
                                   int32_t lag,
                                   hbool_t verbose,
                                   hbool_t reset_stats,
                                   hbool_t display_stats,
                                   hbool_t display_detailed_stats,
                                   hbool_t do_inserts,
                                   hbool_t dirty_inserts,
                                   int dirty_unprotects);

static void hl_col_major_scan_forward(H5C_t * cache_ptr,
                                      hbool_t verbose,
                                      hbool_t reset_stats,
                                      hbool_t display_stats,
                                      hbool_t display_detailed_stats,
                                      hbool_t do_inserts,
                                      hbool_t dirty_inserts,
                                      int dirty_unprotects);

static void col_major_scan_backward(H5C_t * cache_ptr,
                                    int32_t lag,
                                    hbool_t verbose,
                                    hbool_t reset_stats,
                                    hbool_t display_stats,
                                    hbool_t display_detailed_stats,
                                    hbool_t do_inserts,
                                    hbool_t dirty_inserts,
                                    int dirty_unprotects);

static void hl_col_major_scan_backward(H5C_t * cache_ptr,
                                       hbool_t verbose,
                                       hbool_t reset_stats,
                                       hbool_t display_stats,
                                       hbool_t display_detailed_stats,
                                       hbool_t do_inserts,
                                       hbool_t dirty_inserts,
                                       int dirty_unprotects);

static void smoke_check_1(void);
static void smoke_check_2(void);
static void smoke_check_3(void);
static void smoke_check_4(void);
static void smoke_check_5(void);
static void smoke_check_6(void);
static void smoke_check_7(void);
static void smoke_check_8(void);
static void write_permitted_check(void);
static void check_flush_cache(void);
static void check_flush_cache__empty_cache(H5C_t * cache_ptr);
static void check_flush_cache__multi_entry(H5C_t * cache_ptr);
static void check_flush_cache__multi_entry_test(H5C_t * cache_ptr,
                                          int test_num,
                                          unsigned int flush_flags,
                                          int spec_size,
                                          struct flush_cache_test_spec spec[]);
static void check_flush_cache__single_entry(H5C_t * cache_ptr);
static void check_flush_cache__single_entry_test(H5C_t * cache_ptr,
                                                 int test_num,
                                                 int entry_type,
                                                 int entry_idx,
                                                 hbool_t insert_flag,
                                                 hbool_t dirty_flag,
                                                 unsigned int flags,
                                                 unsigned int flush_flags,
                                                 hbool_t expected_loaded,
                                                 hbool_t expected_cleared,
                                                 hbool_t expected_flushed,
                                                 hbool_t expected_destroyed);
static void check_flush_protected_err(void);
static void check_destroy_protected_err(void);
static void check_duplicate_insert_err(void);
static void check_rename_err(void);
static void check_double_protect_err(void);
static void check_double_unprotect_err(void);
static void check_auto_cache_resize(void);
static void check_auto_cache_resize_disable(void);
static void check_auto_cache_resize_epoch_markers(void);
static void check_auto_cache_resize_input_errs(void);
static void check_auto_cache_resize_aux_fcns(void);

static void takedown_cache(H5C_t * cache_ptr, 
                           hbool_t dump_stats, 
                           hbool_t dump_detailed_stats);

static void flush_cache(H5C_t * cache_ptr,
                        hbool_t destroy_entries,
                        hbool_t dump_stats,
                        hbool_t dump_detailed_stats);

static void unprotect_entry(H5C_t * cache_ptr,
                            int32_t type,
                            int32_t idx,
                            int dirty,
                            unsigned int flags);

static void verify_clean(void);

static void verify_unprotected(void);



/* address translation funtions: */

/*-------------------------------------------------------------------------
 * Function:	addr_to_type_and_index
 *
 * Purpose:	Given an address, compute the type and index of the 
 *		associated entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/10/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
addr_to_type_and_index(haddr_t addr,
                       int32_t * type_ptr,
                       int32_t * index_ptr)
{
    int i;
    int32_t type;
    int32_t idx;

    HDassert( type_ptr );
    HDassert( index_ptr );

    /* we only have a small number of entry types, so just do a 
     * linear search.  If NUMBER_OF_ENTRY_TYPES grows, we may want
     * to do a binary search instead.
     */
    i = 1;
    if ( addr >= PICO_ALT_BASE_ADDR ) {

        while ( ( i < NUMBER_OF_ENTRY_TYPES ) &&
                ( addr >= alt_base_addrs[i] ) )
        {
            i++;
        }

    } else {

        while ( ( i < NUMBER_OF_ENTRY_TYPES ) &&
                ( addr >= base_addrs[i] ) )
        {
            i++;
        }
    }

    type = i - 1;

    HDassert( ( type >= 0 ) && ( type < NUMBER_OF_ENTRY_TYPES ) );

    if ( addr >= PICO_ALT_BASE_ADDR ) {

        idx = (addr - alt_base_addrs[type]) / entry_sizes[type];
        HDassert( !((entries[type])[idx].at_main_addr) );
        HDassert( addr == (entries[type])[idx].alt_addr );

    } else {

        idx = (addr - base_addrs[type]) / entry_sizes[type];
        HDassert( (entries[type])[idx].at_main_addr );
        HDassert( addr == (entries[type])[idx].main_addr );
    }

    HDassert( ( idx >= 0 ) && ( idx <= max_indices[type] ) );

    HDassert( addr == (entries[type])[idx].addr );

    *type_ptr = type;
    *index_ptr = idx;

    return;

} /* addr_to_type_and_index() */


#if 0 /* This function has never been used, but we may want it 
       * some time.  Lets keep it for now.
       */
/*-------------------------------------------------------------------------
 * Function:	type_and_index_to_addr
 *
 * Purpose:	Given a type and index of an entry, compute the associated 
 *		addr and return that value.
 *
 * Return:	computed addr
 *
 * Programmer:	John Mainzer
 *              6/10/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
type_and_index_to_addr(int32_t type,
                       int32_t idx)
{
    haddr_t addr;

    HDassert( ( type >= 0 ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
    HDassert( ( idx >= 0 ) && ( idx <= max_indices[type] ) );

    addr = base_addrs[type] + (((haddr_t)idx) * entry_sizes[type]);

    HDassert( addr == (entries[type])[idx].addr );

    if ( (entries[type])[idx].at_main_addr ) {

        HDassert( addr == (entries[type])[idx].main_addr );

    } else {

        HDassert( addr == (entries[type])[idx].alt_addr );
    }

    return(addr);

} /* type_and_index_to_addr() */

#endif


/* Call back functions: */

/*-------------------------------------------------------------------------
 *
 * Function:    H5AC_check_if_write_permitted
 *
 * Purpose:     Determine if a write is permitted under the current
 *              circumstances, and set *write_permitted_ptr accordingly.
 *              As a general rule it is, but when we are running in parallel
 *              mode with collective I/O, we must ensure that a read cannot
 *              cause a write.
 *
 *              In the event of failure, the value of *write_permitted_ptr
 *              is undefined.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  John Mainzer, 5/15/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static herr_t
check_write_permitted(const H5F_t UNUSED * f,
                      hid_t UNUSED dxpl_id,
                      hbool_t * write_permitted_ptr)
{

    HDassert( write_permitted_ptr );
    *write_permitted_ptr = write_permitted;

    return(SUCCEED);

} /* check_write_permitted() */


/*-------------------------------------------------------------------------
 * Function:	clear & friends
 *
 * Purpose:	clear the entry.  The helper functions verify that the 
 *		correct version of clear is being called, and then call
 *		clear proper.
 *
 * Return:	SUCCEED
 *
 * Programmer:	John Mainzer
 *              6/10/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static herr_t 
clear(H5F_t * f,
      void *  thing,
      hbool_t dest)
{
    test_entry_t * entry_ptr;
    test_entry_t * base_addr;

    HDassert( thing );

    entry_ptr = (test_entry_t *)thing;
    base_addr = entries[entry_ptr->type];

    HDassert( entry_ptr->index >= 0 );
    HDassert( entry_ptr->index <= max_indices[entry_ptr->type] );
    HDassert( entry_ptr == &(base_addr[entry_ptr->index]) );
    HDassert( entry_ptr == entry_ptr->self );
    HDassert( entry_ptr->header.addr == entry_ptr->addr );
    HDassert( entry_ptr->header.size == entry_ptr->size );
    HDassert( entry_ptr->size == entry_sizes[entry_ptr->type] );

    entry_ptr->header.is_dirty = FALSE;
    entry_ptr->is_dirty = FALSE;

    entry_ptr->cleared = TRUE;

    if ( dest ) {

        destroy(f, thing);

    }

    return(SUCCEED);

} /* clear() */

herr_t 
pico_clear(H5F_t * f, void *  thing, hbool_t dest)
{
    HDassert ( ((test_entry_t *)thing)->type == PICO_ENTRY_TYPE );
    return(clear(f, thing, dest));
} 

herr_t 
nano_clear(H5F_t * f, void *  thing, hbool_t dest)
{
    HDassert ( ((test_entry_t *)thing)->type == NANO_ENTRY_TYPE );
    return(clear(f, thing, dest));
} 

herr_t 
micro_clear(H5F_t * f, void *  thing, hbool_t dest)
{
    HDassert ( ((test_entry_t *)thing)->type == MICRO_ENTRY_TYPE );
    return(clear(f, thing, dest));
} 

herr_t 
tiny_clear(H5F_t * f, void *  thing, hbool_t dest)
{
    HDassert ( ((test_entry_t *)thing)->type == TINY_ENTRY_TYPE );
    return(clear(f, thing, dest));
} 

herr_t 
small_clear(H5F_t * f, void *  thing, hbool_t dest)
{
    HDassert ( ((test_entry_t *)thing)->type == SMALL_ENTRY_TYPE );
    return(clear(f, thing, dest));
} 

herr_t 
medium_clear(H5F_t * f, void *  thing, hbool_t dest)
{
    HDassert ( ((test_entry_t *)thing)->type == MEDIUM_ENTRY_TYPE );
    return(clear(f, thing, dest));
} 

herr_t 
large_clear(H5F_t * f, void *  thing, hbool_t dest)
{
    HDassert ( ((test_entry_t *)thing)->type == LARGE_ENTRY_TYPE );
    return(clear(f, thing, dest));
} 

herr_t 
huge_clear(H5F_t * f, void *  thing, hbool_t dest)
{
    HDassert ( ((test_entry_t *)thing)->type == HUGE_ENTRY_TYPE );
    return(clear(f, thing, dest));
} 

herr_t 
monster_clear(H5F_t * f, void *  thing, hbool_t dest)
{
    HDassert ( ((test_entry_t *)thing)->type == MONSTER_ENTRY_TYPE );
    return(clear(f, thing, dest));
} 


/*-------------------------------------------------------------------------
 * Function:	dest & friends
 *
 * Purpose:	Destroy the entry.  The helper functions verify that the 
 *		correct version of dest is being called, and then call
 *		dest proper.
 *
 * Return:	SUCCEED
 *
 * Programmer:	John Mainzer
 *              6/10/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static herr_t 
destroy(H5F_t UNUSED * f,
        void *         thing)
{
    test_entry_t * entry_ptr;
    test_entry_t * base_addr;

    HDassert( thing );

    entry_ptr = (test_entry_t *)thing;
    base_addr = entries[entry_ptr->type];

    HDassert ( entry_ptr->index >= 0 );
    HDassert ( entry_ptr->index <= max_indices[entry_ptr->type] );
    HDassert( entry_ptr == &(base_addr[entry_ptr->index]) );
    HDassert( entry_ptr == entry_ptr->self );
    HDassert( entry_ptr->header.addr == entry_ptr->addr );
    HDassert( entry_ptr->header.size == entry_ptr->size );
    HDassert( entry_ptr->size == entry_sizes[entry_ptr->type] );

    HDassert( !(entry_ptr->is_dirty) );
    HDassert( !(entry_ptr->header.is_dirty) );

    entry_ptr->destroyed = TRUE;

    return(SUCCEED);

} /* dest() */

herr_t 
pico_dest(H5F_t * f, void * thing)
{
    HDassert ( ((test_entry_t *)thing)->type == PICO_ENTRY_TYPE );
    return(destroy(f, thing));
} 

herr_t 
nano_dest(H5F_t * f, void * thing)
{
    HDassert ( ((test_entry_t *)thing)->type == NANO_ENTRY_TYPE );
    return(destroy(f, thing));
} 

herr_t 
micro_dest(H5F_t * f, void * thing)
{
    HDassert ( ((test_entry_t *)thing)->type == MICRO_ENTRY_TYPE );
    return(destroy(f, thing));
} 

herr_t 
tiny_dest(H5F_t * f, void * thing)
{
    HDassert ( ((test_entry_t *)thing)->type == TINY_ENTRY_TYPE );
    return(destroy(f, thing));
} 

herr_t 
small_dest(H5F_t * f, void * thing)
{
    HDassert ( ((test_entry_t *)thing)->type == SMALL_ENTRY_TYPE );
    return(destroy(f, thing));
} 

herr_t 
medium_dest(H5F_t * f, void * thing)
{
    HDassert ( ((test_entry_t *)thing)->type == MEDIUM_ENTRY_TYPE );
    return(destroy(f, thing));
} 

herr_t 
large_dest(H5F_t * f, void * thing)
{
    HDassert ( ((test_entry_t *)thing)->type == LARGE_ENTRY_TYPE );
    return(destroy(f, thing));
} 

herr_t 
huge_dest(H5F_t * f, void *  thing)
{
    HDassert ( ((test_entry_t *)thing)->type == HUGE_ENTRY_TYPE );
    return(destroy(f, thing));
} 

herr_t 
monster_dest(H5F_t * f, void *  thing)
{
    HDassert ( ((test_entry_t *)thing)->type == MONSTER_ENTRY_TYPE );
    return(destroy(f, thing));
} 


/*-------------------------------------------------------------------------
 * Function:	flush & friends
 *
 * Purpose:	flush the entry and mark it as clean.  The helper functions 
 *              verify that the correct version of flush is being called, 
 *		and then call flush proper.
 *
 * Return:	SUCCEED
 *
 * Programmer:	John Mainzer
 *              6/10/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static herr_t 
flush(H5F_t *f,
      hid_t UNUSED dxpl_id,
      hbool_t dest,
      haddr_t addr,
      void *thing)
{
    test_entry_t * entry_ptr;
    test_entry_t * base_addr;

    HDassert( thing );

    entry_ptr = (test_entry_t *)thing;
    base_addr = entries[entry_ptr->type];

    HDassert( entry_ptr->index >= 0 );
    HDassert( entry_ptr->index <= max_indices[entry_ptr->type] );
    HDassert( entry_ptr == &(base_addr[entry_ptr->index]) );
    HDassert( entry_ptr == entry_ptr->self );
    HDassert( entry_ptr->header.addr == entry_ptr->addr );
    HDassert( entry_ptr->addr == addr );
    HDassert( entry_ptr->header.size == entry_ptr->size );
    HDassert( entry_ptr->size == entry_sizes[entry_ptr->type] );

    entry_ptr->flushed = TRUE;

    if ( ( ! write_permitted ) && ( entry_ptr->is_dirty ) ) {

        pass = FALSE;
        failure_mssg = "called flush when write_permitted is FALSE.";
    }

    if ( entry_ptr->is_dirty ) {

        (entry_ptr->writes)++;
        entry_ptr->is_dirty = FALSE;
        entry_ptr->header.is_dirty = FALSE;
    }

    if ( dest ) {

        destroy(f, thing);

    }

    return(SUCCEED);

} /* flush() */

herr_t 
pico_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, haddr_t addr, void *thing)
{
    HDassert ( ((test_entry_t *)thing)->type == PICO_ENTRY_TYPE );
    return(flush(f, dxpl_id, dest, addr, thing));
} 

herr_t 
nano_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, haddr_t addr, void *thing)
{
    HDassert ( ((test_entry_t *)thing)->type == NANO_ENTRY_TYPE );
    return(flush(f, dxpl_id, dest, addr, thing));
} 

herr_t 
micro_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, haddr_t addr, void *thing)
{
    HDassert ( ((test_entry_t *)thing)->type == MICRO_ENTRY_TYPE );
    return(flush(f, dxpl_id, dest, addr, thing));
} 

herr_t 
tiny_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, haddr_t addr, void *thing)
{
    HDassert ( ((test_entry_t *)thing)->type == TINY_ENTRY_TYPE );
    return(flush(f, dxpl_id, dest, addr, thing));
} 

herr_t 
small_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, haddr_t addr, void *thing)
{
    HDassert ( ((test_entry_t *)thing)->type == SMALL_ENTRY_TYPE );
    return(flush(f, dxpl_id, dest, addr, thing));
} 

herr_t 
medium_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, haddr_t addr, void *thing)
{
    HDassert ( ((test_entry_t *)thing)->type == MEDIUM_ENTRY_TYPE );
    return(flush(f, dxpl_id, dest, addr, thing));
} 

herr_t 
large_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, haddr_t addr, void *thing)
{
    HDassert ( ((test_entry_t *)thing)->type == LARGE_ENTRY_TYPE );
    return(flush(f, dxpl_id, dest, addr, thing));
} 

herr_t 
huge_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, haddr_t addr, void *thing)
{
    HDassert ( ((test_entry_t *)thing)->type == HUGE_ENTRY_TYPE );
    return(flush(f, dxpl_id, dest, addr, thing));
} 

herr_t 
monster_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, haddr_t addr, void *thing)
{
    HDassert ( ((test_entry_t *)thing)->type == MONSTER_ENTRY_TYPE );
    return(flush(f, dxpl_id, dest, addr, thing));
} 


/*-------------------------------------------------------------------------
 * Function:	load & friends
 *
 * Purpose:	"load" the requested entry and mark it as clean.  The 
 *		helper functions verify that the correct version of load
 *		 is being called, and then call load proper.
 *
 * Return:	SUCCEED
 *
 * Programmer:	John Mainzer
 *              6/10/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void *
load(H5F_t UNUSED *f,
     hid_t UNUSED dxpl_id,
     haddr_t addr,
     const void UNUSED *udata1,
     void UNUSED *udata2)
{
    int32_t type;
    int32_t idx;
    test_entry_t * entry_ptr;
    test_entry_t * base_addr;

    addr_to_type_and_index(addr, &type, &idx);

    base_addr = entries[type];
    entry_ptr = &(base_addr[idx]);

    HDassert( entry_ptr->type == type );
    HDassert( entry_ptr->type >= 0 );
    HDassert( entry_ptr->type < NUMBER_OF_ENTRY_TYPES );
    HDassert( entry_ptr->index == idx );
    HDassert( entry_ptr->index >= 0 );
    HDassert( entry_ptr->index <= max_indices[type] );
    HDassert( entry_ptr == entry_ptr->self );
    HDassert( entry_ptr->addr == addr );
    HDassert( entry_ptr->size == entry_sizes[type] );

    entry_ptr->loaded = TRUE;

    entry_ptr->is_dirty = FALSE;

    (entry_ptr->reads)++;

    return(entry_ptr);

} /* load() */

void * 
pico_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, 
          const void *udata1, void *udata2)
{
    return(load(f, dxpl_id, addr, udata1, udata2));
} 

void *
nano_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, 
          const void *udata1, void *udata2)
{
    return(load(f, dxpl_id, addr, udata1, udata2));
} 

void *
micro_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, 
           const void *udata1, void *udata2)
{
    return(load(f, dxpl_id, addr, udata1, udata2));
} 

void *
tiny_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, 
          const void *udata1, void *udata2)
{
    return(load(f, dxpl_id, addr, udata1, udata2));
} 

void *
small_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, 
           const void *udata1, void *udata2)
{
    return(load(f, dxpl_id, addr, udata1, udata2));
} 

void *
medium_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, 
            const void *udata1, void *udata2)
{
    return(load(f, dxpl_id, addr, udata1, udata2));
} 

void *
large_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, 
           const void *udata1, void *udata2)
{
    return(load(f, dxpl_id, addr, udata1, udata2));
} 

void *
huge_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, 
          const void *udata1, void *udata2)
{
    return(load(f, dxpl_id, addr, udata1, udata2));
} 

void *
monster_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, 
             const void *udata1, void *udata2)
{
    return(load(f, dxpl_id, addr, udata1, udata2));
} 


/*-------------------------------------------------------------------------
 * Function:	size & friends
 *
 * Purpose:	Get the size of the specified entry.  The helper functions 
 *		verify that the correct version of size is being called, 
 *		and then call size proper.
 *
 * Return:	SUCCEED
 *
 * Programmer:	John Mainzer
 *              6/10/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static herr_t 
size(H5F_t UNUSED *  f,
     void *   thing,
     size_t * size_ptr)
{
    test_entry_t * entry_ptr;
    test_entry_t * base_addr;

    HDassert( size_ptr );
    HDassert( thing );

    entry_ptr = (test_entry_t *)thing;
    base_addr = entries[entry_ptr->type];

    HDassert( entry_ptr->index >= 0 );
    HDassert( entry_ptr->index <= max_indices[entry_ptr->type] );
    HDassert( entry_ptr == &(base_addr[entry_ptr->index]) );
    HDassert( entry_ptr == entry_ptr->self );
    HDassert( entry_ptr->header.addr == entry_ptr->addr );
    HDassert( entry_ptr->size == entry_sizes[entry_ptr->type] );

    *size_ptr = entry_ptr->size;

    return(SUCCEED);

} /* size() */

herr_t 
pico_size(H5F_t * f, void * thing, size_t * size_ptr)
{
    HDassert ( ((test_entry_t *)thing)->type == PICO_ENTRY_TYPE );
    return(size(f, thing, size_ptr));
} 

herr_t 
nano_size(H5F_t * f, void * thing, size_t * size_ptr)
{
    HDassert ( ((test_entry_t *)thing)->type == NANO_ENTRY_TYPE );
    return(size(f, thing, size_ptr));
} 

herr_t 
micro_size(H5F_t * f, void * thing, size_t * size_ptr)
{
    HDassert ( ((test_entry_t *)thing)->type == MICRO_ENTRY_TYPE );
    return(size(f, thing, size_ptr));
} 

herr_t 
tiny_size(H5F_t * f, void * thing, size_t * size_ptr)
{
    HDassert ( ((test_entry_t *)thing)->type == TINY_ENTRY_TYPE );
    return(size(f, thing, size_ptr));
} 

herr_t 
small_size(H5F_t * f, void * thing, size_t * size_ptr)
{
    HDassert ( ((test_entry_t *)thing)->type == SMALL_ENTRY_TYPE );
    return(size(f, thing, size_ptr));
} 

herr_t 
medium_size(H5F_t * f, void * thing, size_t * size_ptr)
{
    HDassert ( ((test_entry_t *)thing)->type == MEDIUM_ENTRY_TYPE );
    return(size(f, thing, size_ptr));
} 

herr_t 
large_size(H5F_t * f, void * thing, size_t * size_ptr)
{
    HDassert ( ((test_entry_t *)thing)->type == LARGE_ENTRY_TYPE );
    return(size(f, thing, size_ptr));
} 

herr_t 
huge_size(H5F_t * f, void * thing, size_t * size_ptr)
{
    HDassert ( ((test_entry_t *)thing)->type == HUGE_ENTRY_TYPE );
    return(size(f, thing, size_ptr));
} 

herr_t 
monster_size(H5F_t * f, void * thing, size_t * size_ptr)
{
    HDassert ( ((test_entry_t *)thing)->type == MONSTER_ENTRY_TYPE );
    return(size(f, thing, size_ptr));
} 


/**************************************************************************/
/**************************************************************************/
/************************** test utility functions: ***********************/
/**************************************************************************/
/**************************************************************************/

/*-------------------------------------------------------------------------
 * Function:	entry_in_cache
 *
 * Purpose:	Given a pointer to a cache, an entry type, and an index, 
 *		determine if the entry is currently in the cache.
 *
 * Return:	TRUE if the entry is in the cache, and FALSE otherwise.
 *
 * Programmer:	John Mainzer
 *              6/10/04
 *
 * Modifications:
 *
 *		JRM - 10/12/04
 *		Removed references to local_H5C_t, as we now get direct
 *		access to the definition of H5C_t via H5Cpkg.h.
 *
 *-------------------------------------------------------------------------
 */

hbool_t
entry_in_cache(H5C_t * cache_ptr,
               int32_t type,
               int32_t idx)
{
    hbool_t in_cache = FALSE; /* will set to TRUE if necessary */
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;
    H5C_cache_entry_t * test_ptr = NULL;

    HDassert( cache_ptr );
    HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
    HDassert( ( 0 <= idx ) && ( idx <= max_indices[type] ) );

    base_addr = entries[type];
    entry_ptr = &(base_addr[idx]);

    HDassert( entry_ptr->index == idx );
    HDassert( entry_ptr->type == type );
    HDassert( entry_ptr == entry_ptr->self );

    H5C__SEARCH_INDEX(cache_ptr, entry_ptr->addr, test_ptr)

    if ( test_ptr != NULL ) {

        in_cache = TRUE;
        HDassert( test_ptr == (H5C_cache_entry_t *)entry_ptr );
        HDassert( entry_ptr->addr == entry_ptr->header.addr );
    }

    return(in_cache);

} /* entry_in_cache() */


/*-------------------------------------------------------------------------
 * Function:	reset_entries
 *
 * Purpose:	reset the contents of the entries arrays to know values.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/10/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
reset_entries(void)

{
    int i;
    int j;
    int32_t max_index;
    haddr_t addr = 0;
    haddr_t alt_addr = PICO_ALT_BASE_ADDR;
    size_t entry_size;
    test_entry_t * base_addr;

    for ( i = 0; i < NUMBER_OF_ENTRY_TYPES; i++ )
    {
        entry_size = entry_sizes[i];
        max_index = max_indices[i];
        base_addr = entries[i];

        HDassert( base_addr );

        for ( j = 0; j <= max_index; j++ )
        {
            /* one can argue that we should fill the header with garbage.
             * If this is desired, we can simply comment out the header
             * initialization - the headers will be full of garbage soon
             * enough.
             */

            base_addr[j].header.addr = (haddr_t)0;
            base_addr[j].header.size = (size_t)0;
            base_addr[j].header.type = NULL;
            base_addr[j].header.is_dirty = FALSE;
            base_addr[j].header.is_protected = FALSE;
            base_addr[j].header.next = NULL;
            base_addr[j].header.prev = NULL;
            base_addr[j].header.aux_next = NULL;
            base_addr[j].header.aux_prev = NULL;

            base_addr[j].self = &(base_addr[j]);
            base_addr[j].addr = addr;
            base_addr[j].at_main_addr = TRUE;
            base_addr[j].main_addr = addr;
            base_addr[j].alt_addr = alt_addr;
            base_addr[j].size = entry_size;
            base_addr[j].type = i;
            base_addr[j].index = j;
            base_addr[j].reads = 0;
            base_addr[j].writes = 0;
            base_addr[j].is_dirty = FALSE;
            base_addr[j].is_protected = FALSE;

            base_addr[j].loaded = FALSE;
            base_addr[j].cleared = FALSE;
            base_addr[j].flushed = FALSE;
            base_addr[j].destroyed = FALSE;

            addr += (haddr_t)entry_size;
            alt_addr += (haddr_t)entry_size;
        }
    }

    return;

} /* reset_entries() */


/*-------------------------------------------------------------------------
 * Function:	verify_clean
 *
 * Purpose:	Verify that all cache entries are marked as clean.  If any
 *		are not, set pass to FALSE.
 *
 *		Do nothing if pass is FALSE on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/10/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
verify_clean(void)

{
    int i;
    int j;
    int dirty_count = 0;
    int32_t max_index;
    test_entry_t * base_addr;

    if ( pass ) {

        for ( i = 0; i < NUMBER_OF_ENTRY_TYPES; i++ )
        {
            max_index = max_indices[i];
            base_addr = entries[i];

            HDassert( base_addr );

            for ( j = 0; j <= max_index; j++ )
            {
                if ( ( base_addr[j].header.is_dirty ) || ( base_addr[j].is_dirty ) ) {
            
                    dirty_count++;
                }
            }
        }

        if ( dirty_count > 0 ) {

            pass = FALSE;
            failure_mssg = "verify_clean() found dirty entry(s).";
        }
    }

    return;

} /* verify_clean() */


/*-------------------------------------------------------------------------
 * Function:	verify_unprotected
 *
 * Purpose:	Verify that no cache entries are marked as protected.  If 
 *		any are, set pass to FALSE.
 *
 *		Do nothing if pass is FALSE on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/10/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
verify_unprotected(void)

{
    int i;
    int j;
    int protected_count = 0;
    int32_t max_index;
    test_entry_t * base_addr;

    if ( pass ) {

        for ( i = 0; i < NUMBER_OF_ENTRY_TYPES; i++ )
        {
            max_index = max_indices[i];
            base_addr = entries[i];

            HDassert( base_addr );

            for ( j = 0; j <= max_index; j++ )
            {
                HDassert( base_addr[j].header.is_protected == 
                          base_addr[j].is_protected );

                if ( ( base_addr[j].header.is_protected ) || 
                     ( base_addr[j].is_protected ) ) {
            
                    protected_count++;
                }
            }
        }

        if ( protected_count > 0 ) {

            pass = FALSE;
            failure_mssg = "verify_unprotected() found protected entry(s).";
        }
    }

    return;

} /* verify_unprotected() */


/*-------------------------------------------------------------------------
 * Function:	setup_cache()
 *
 * Purpose:	Allocate a cache of the desired size and configure it for
 *		use in the test bed.  Return a pointer to the new cache
 *		structure.
 *
 * Return:	Pointer to new cache, or NULL on failure.
 *
 * Programmer:	John Mainzer
 *              6/11/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static H5C_t *
setup_cache(size_t max_cache_size,
            size_t min_clean_size)
{
    H5C_t * cache_ptr = NULL;

    cache_ptr = H5C_create(max_cache_size, 
                           min_clean_size,
                           (NUMBER_OF_ENTRY_TYPES - 1),
			   (const char **)entry_type_names,
                           check_write_permitted);

    if ( cache_ptr == NULL ) {

        pass = FALSE;
        failure_mssg = "H5C_create() returned NULL.";

    } else {

        H5C_set_skip_flags(cache_ptr, TRUE, TRUE);
    }

    return(cache_ptr);

} /* setup_cache() */


/*-------------------------------------------------------------------------
 * Function:	takedown_cache()
 *
 * Purpose:	Flush the specified cache and disable it.  If requested,
 *		dump stats first.  If pass is FALSE, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/11/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
takedown_cache(H5C_t * cache_ptr,
               hbool_t dump_stats,
               hbool_t dump_detailed_stats)
{
    HDassert(cache_ptr);

    if ( pass ) {

        if ( dump_stats ) {

            H5C_stats(cache_ptr, "test cache", dump_detailed_stats);
        }

        H5C_dest(NULL, -1, -1, cache_ptr);
    }

    return;

} /* takedown_cache() */


/*-------------------------------------------------------------------------
 * Function:	flush_cache()
 *
 * Purpose:	Flush the specified cache, destroying all entries if 
                requested.  If requested, dump stats first.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/23/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
flush_cache(H5C_t * cache_ptr,
            hbool_t destroy_entries,
            hbool_t dump_stats,
            hbool_t dump_detailed_stats)
{
    herr_t result = 0;

    HDassert(cache_ptr);

    verify_unprotected();

    if ( pass ) {

        if ( destroy_entries ) {

            result = H5C_flush_cache(NULL, -1, -1, cache_ptr, 
                                     H5C__FLUSH_INVALIDATE_FLAG);

        } else {

            result = H5C_flush_cache(NULL, -1, -1, cache_ptr, 
                                     H5C__NO_FLAGS_SET);
        }
    }

    if ( dump_stats ) {

        H5C_stats(cache_ptr, "test cache", dump_detailed_stats);
    }

    if ( result < 0 ) {

        pass = FALSE;
        failure_mssg = "error in H5C_flush_cache().";
    }

    return;

} /* flush_cache() */


/*-------------------------------------------------------------------------
 * Function:	insert_entry()
 *
 * Purpose:	Insert the entry indicated by the type and index.  Mark
 *		it clean or dirty as indicated.
 *
 *		Note that I don't see much practical use for inserting
 *		a clean entry, but the interface permits it so we should
 *		test it.
 *
 *		Do nothing if pass is false.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/16/04
 *
 * Modifications:
 *
 *		JRM -- 1/13/05
 *		Updated function for the flags parameter in 
 *		H5C_insert_entry(), and to allow access to this parameter.
 *
 *-------------------------------------------------------------------------
 */

static void
insert_entry(H5C_t * cache_ptr,
             int32_t type,
             int32_t idx,
             hbool_t dirty, 
             unsigned int flags)
{
    herr_t result;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    if ( pass ) {

        HDassert( cache_ptr );
        HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= idx ) && ( idx <= max_indices[type] ) );

        base_addr = entries[type];
        entry_ptr = &(base_addr[idx]);

        HDassert( entry_ptr->index == idx );
        HDassert( entry_ptr->type == type );
        HDassert( entry_ptr == entry_ptr->self );
        HDassert( !(entry_ptr->is_protected) );

        if ( dirty ) {

            (entry_ptr->header).is_dirty = dirty;
            entry_ptr->is_dirty = dirty;
        }

        result = H5C_insert_entry(NULL, -1, -1, cache_ptr, &(types[type]),
                                  entry_ptr->addr, (void *)entry_ptr, flags);

        if ( ( result < 0 ) ||
             ( entry_ptr->header.is_protected ) ||
             ( entry_ptr->header.type != &(types[type]) ) ||
             ( entry_ptr->size != entry_ptr->header.size ) ||
             ( entry_ptr->addr != entry_ptr->header.addr ) ) {

            pass = FALSE;
            failure_mssg = "error in H5C_insert().";

#if 0 
            /* This is useful debugging code.  Lets keep it around. */

            HDfprintf(stdout, "result = %d\n", (int)result);
            HDfprintf(stdout, "entry_ptr->header.is_protected = %d\n",
                      (int)(entry_ptr->header.is_protected));
            HDfprintf(stdout, "entry_ptr->header.type != &(types[type]) = %d\n",
                      (int)(entry_ptr->header.type != &(types[type])));
            HDfprintf(stdout, 
                      "entry_ptr->size != entry_ptr->header.size = %d\n",
                      (int)(entry_ptr->size != entry_ptr->header.size));
            HDfprintf(stdout, 
                      "entry_ptr->addr != entry_ptr->header.addr = %d\n",
                       (int)(entry_ptr->addr != entry_ptr->header.addr));
#endif
        }

        HDassert( ((entry_ptr->header).type)->id == type );
    }

    return;

} /* insert_entry() */


/*-------------------------------------------------------------------------
 * Function:	rename_entry()
 *
 * Purpose:	Rename the entry indicated by the type and index to its
 *		main or alternate address as indicated.  If the entry is
 *		already at the desired entry, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/21/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
rename_entry(H5C_t * cache_ptr,
             int32_t type,
             int32_t idx,
             hbool_t main_addr)
{
    herr_t         result;
    hbool_t	   done = TRUE; /* will set to FALSE if we have work to do */
    haddr_t        old_addr = HADDR_UNDEF;
    haddr_t        new_addr = HADDR_UNDEF;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    HDassert( cache_ptr );
    HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
    HDassert( ( 0 <= idx ) && ( idx <= max_indices[type] ) );

    base_addr = entries[type];
    entry_ptr = &(base_addr[idx]);

    HDassert( entry_ptr->index == idx );
    HDassert( entry_ptr->type == type );
    HDassert( entry_ptr == entry_ptr->self );
    HDassert( !(entry_ptr->is_protected) );
    HDassert( !(entry_ptr->header.is_protected) );

    if ( entry_ptr->at_main_addr && !main_addr ) { 

        /* rename to alt addr */

        HDassert( entry_ptr->addr == entry_ptr->main_addr );

        done = FALSE;
        old_addr = entry_ptr->addr;
        new_addr = entry_ptr->alt_addr;

    } else if ( !(entry_ptr->at_main_addr) && main_addr ) { 

        /* rename to main addr */

        HDassert( entry_ptr->addr == entry_ptr->alt_addr );

        done = FALSE;
        old_addr = entry_ptr->addr;
        new_addr = entry_ptr->main_addr;
    }

    if ( ! done ) {

        result = H5C_rename_entry(cache_ptr, &(types[type]),
                                  old_addr, new_addr);
    }

    if ( ! done ) {

        if ( ( result < 0 ) || ( entry_ptr->header.addr != new_addr ) ) {

            pass = FALSE;
            failure_mssg = "error in H5C_rename_entry().";

        } else {

            entry_ptr->addr = new_addr;
            entry_ptr->at_main_addr = main_addr;
        }
    }

    HDassert( ((entry_ptr->header).type)->id == type );

    return;

} /* rename_entry() */


/*-------------------------------------------------------------------------
 * Function:	protect_entry()
 *
 * Purpose:	Protect the entry indicated by the type and index.
 *
 *		Do nothing if pass is FALSE on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/11/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
protect_entry(H5C_t * cache_ptr,
              int32_t type,
              int32_t idx)
{
    /* const char * fcn_name = "protect_entry()"; */
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;
    H5C_cache_entry_t * cache_entry_ptr;

    if ( pass ) {

        HDassert( cache_ptr );
        HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= idx ) && ( idx <= max_indices[type] ) );

        base_addr = entries[type];
        entry_ptr = &(base_addr[idx]);

        HDassert( entry_ptr->index == idx );
        HDassert( entry_ptr->type == type );
        HDassert( entry_ptr == entry_ptr->self );
        HDassert( !(entry_ptr->is_protected) );

        cache_entry_ptr = H5C_protect(NULL, -1, -1, cache_ptr, &(types[type]),
                                      entry_ptr->addr, NULL, NULL);

        if ( ( cache_entry_ptr != (void *)entry_ptr ) ||
             ( !(entry_ptr->header.is_protected) ) ||
             ( entry_ptr->header.type != &(types[type]) ) ||
             ( entry_ptr->size != entry_ptr->header.size ) ||
             ( entry_ptr->addr != entry_ptr->header.addr ) ) {

#if 0 
            /* I've written the following debugging code several times 
             * now.  Lets keep it around so I don't have to write it 
             * again.
             *                              - JRM
             */
            HDfprintf(stdout, "( cache_entry_ptr != (void *)entry_ptr ) = %d\n",
                      (int)( cache_entry_ptr != (void *)entry_ptr ));
            HDfprintf(stdout, "cache_entry_ptr = 0x%lx, entry_ptr = 0x%lx\n",
                      (long)cache_entry_ptr, (long)entry_ptr);
            HDfprintf(stdout, "entry_ptr->header.is_protected = %d\n",
                      (int)(entry_ptr->header.is_protected));
            HDfprintf(stdout, 
                      "( entry_ptr->header.type != &(types[type]) ) = %d\n",
                      (int)( entry_ptr->header.type != &(types[type]) ));
            HDfprintf(stdout, 
                      "entry_ptr->size = %d, entry_ptr->header.size = %d\n",
                      (int)(entry_ptr->size), (int)(entry_ptr->header.size));
            HDfprintf(stdout,
                      "entry_ptr->addr = %d, entry_ptr->header.addr = %d\n",
                      (int)(entry_ptr->addr), (int)(entry_ptr->header.addr));
#endif
            pass = FALSE;
            failure_mssg = "error in H5C_protect().";

        } else {

            entry_ptr->is_protected = TRUE;

        }

        HDassert( ((entry_ptr->header).type)->id == type );
    }

    return;

} /* protect_entry() */


/*-------------------------------------------------------------------------
 * Function:	unprotect_entry()
 *
 * Purpose:	Unprotect the entry indicated by the type and index.
 *
 *		Do nothing if pass is FALSE on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/12/04
 *
 * Modifications:
 *
 *		JRM -- 1/7/05
 *		Updated for the replacement of the deleted parameter in 
 *		H5C_unprotect() with the new flags parameter.
 *
 *-------------------------------------------------------------------------
 */

#define NO_CHANGE	-1

static void
unprotect_entry(H5C_t * cache_ptr,
                int32_t type,
                int32_t idx,
                int dirty,
                unsigned int flags)
{
    /* const char * fcn_name = "unprotect_entry()"; */
    herr_t result;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    if ( pass ) {

        HDassert( cache_ptr );
        HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= idx ) && ( idx <= max_indices[type] ) );

        base_addr = entries[type];
        entry_ptr = &(base_addr[idx]);

        HDassert( entry_ptr->index == idx );
        HDassert( entry_ptr->type == type );
        HDassert( entry_ptr == entry_ptr->self );
        HDassert( entry_ptr->header.is_protected );
        HDassert( entry_ptr->is_protected );

        if ( ( dirty == TRUE ) || ( dirty == FALSE ) ) {

            entry_ptr->header.is_dirty = dirty;
            entry_ptr->is_dirty = dirty;
        }

        result = H5C_unprotect(NULL, -1, -1, cache_ptr, &(types[type]),
                               entry_ptr->addr, (void *)entry_ptr, flags);

        if ( ( result < 0 ) ||
             ( entry_ptr->header.is_protected ) ||
             ( entry_ptr->header.type != &(types[type]) ) ||
             ( entry_ptr->size != entry_ptr->header.size ) ||
             ( entry_ptr->addr != entry_ptr->header.addr ) ) {

            pass = FALSE;
            failure_mssg = "error in H5C_unprotect().";

        }
        else
        {
            entry_ptr->is_protected = FALSE;
        }

        HDassert( ((entry_ptr->header).type)->id == type );
    }

    return;

} /* unprotect_entry() */


/*-------------------------------------------------------------------------
 * Function:	row_major_scan_forward()
 *
 * Purpose:	Do a sequence of inserts, protects, unprotects, renames,
 *		destroys while scanning through the set of entries.  If
 *		pass is false on entry, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/12/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
row_major_scan_forward(H5C_t * cache_ptr,
                       int32_t lag,
                       hbool_t verbose,
                       hbool_t reset_stats,
                       hbool_t display_stats,
                       hbool_t display_detailed_stats,
                       hbool_t do_inserts,
                       hbool_t dirty_inserts,
                       hbool_t do_renames,
                       hbool_t rename_to_main_addr,
                       hbool_t do_destroys,
                       int dirty_destroys,
                       int dirty_unprotects)
{
    const char * fcn_name = "row_major_scan_forward";
    int32_t type;
    int32_t idx;

    if ( verbose )
        HDfprintf(stdout, "%s(): entering.\n", fcn_name);

    HDassert( lag > 5 );

    type = 0;

    if ( ( pass ) && ( reset_stats ) ) {

        H5C_stats__reset(cache_ptr);
    }

    while ( ( pass ) && ( type < NUMBER_OF_ENTRY_TYPES ) )
    {
        idx = -lag;

        while ( ( pass ) && ( idx <= (max_indices[type] + lag) ) )
        {
            if ( ( pass ) && ( do_inserts ) && ( (idx + lag) >= 0 ) &&
                 ( (idx + lag) <= max_indices[type] ) &&
                 ( ((idx + lag) % 2) == 0 ) &&
                 ( ! entry_in_cache(cache_ptr, type, (idx + lag)) ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(i, %d, %d) ", type, (idx + lag));

                insert_entry(cache_ptr, type, (idx + lag), dirty_inserts,
                             H5C__NO_FLAGS_SET);
            }


            if ( ( pass ) && ( (idx + lag - 1) >= 0 ) &&
                 ( (idx + lag - 1) <= max_indices[type] ) &&
                 ( ( (idx + lag - 1) % 3 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, (idx + lag - 1));

                protect_entry(cache_ptr, type, (idx + lag - 1));
            }

            if ( ( pass ) && ( (idx + lag - 2) >= 0 ) &&
                 ( (idx + lag - 2) <= max_indices[type] ) &&
                 ( ( (idx + lag - 2) % 3 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(u, %d, %d) ", type, (idx + lag - 2)); 

                unprotect_entry(cache_ptr, type, idx+lag-2, NO_CHANGE, 
                                H5C__NO_FLAGS_SET);
            }


            if ( ( pass ) && ( do_renames ) && ( (idx + lag - 2) >= 0 ) &&
                 ( (idx + lag - 2) <= max_indices[type] ) &&
                 ( ( (idx + lag - 2) % 3 ) == 0 ) ) {

                rename_entry(cache_ptr, type, (idx + lag - 2), 
                             rename_to_main_addr);
            }


            if ( ( pass ) && ( (idx + lag - 3) >= 0 ) &&
                 ( (idx + lag - 3) <= max_indices[type] ) &&
                 ( ( (idx + lag - 3) % 5 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, (idx + lag - 3));

                protect_entry(cache_ptr, type, (idx + lag - 3));
            }

            if ( ( pass ) && ( (idx + lag - 5) >= 0 ) &&
                 ( (idx + lag - 5) <= max_indices[type] ) &&
                 ( ( (idx + lag - 5) % 5 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(u, %d, %d) ", type, (idx + lag - 5));

                unprotect_entry(cache_ptr, type, idx+lag-5, NO_CHANGE, 
                                H5C__NO_FLAGS_SET);
            }

            if ( ( pass ) && ( idx >= 0 ) && ( idx <= max_indices[type] ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, idx); 

                protect_entry(cache_ptr, type, idx);
            }


            if ( ( pass ) && ( (idx - lag + 2) >= 0 ) &&
                 ( (idx - lag + 2) <= max_indices[type] ) &&
                 ( ( (idx - lag + 2) % 7 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(u, %d, %d) ", type, (idx - lag + 2));

                unprotect_entry(cache_ptr, type, idx-lag+2, NO_CHANGE, 
                                H5C__NO_FLAGS_SET);
            }

            if ( ( pass ) && ( (idx - lag + 1) >= 0 ) &&
                 ( (idx - lag + 1) <= max_indices[type] ) &&
                 ( ( (idx - lag + 1) % 7 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, (idx - lag + 1));

                protect_entry(cache_ptr, type, (idx - lag + 1));
            }


            if ( do_destroys ) {

                if ( ( pass ) && ( (idx - lag) >= 0 ) &&
                     ( ( idx - lag) <= max_indices[type] ) ) {

                    switch ( (idx - lag) %4 ) {

                        case 0: /* we just did an insert */
                            unprotect_entry(cache_ptr, type, idx - lag,
                                            NO_CHANGE, H5C__NO_FLAGS_SET);
                            break;

                        case 1:
                            if ( (entries[type])[idx-lag].is_dirty ) {

                                unprotect_entry(cache_ptr, type, idx - lag,
                                                NO_CHANGE, H5C__NO_FLAGS_SET);
                            } else {

                                unprotect_entry(cache_ptr, type, idx - lag,
                                                dirty_unprotects, 
                                                H5C__NO_FLAGS_SET);
                            }
                            break;

                        case 2: /* we just did an insrt */
                            unprotect_entry(cache_ptr, type, idx - lag,
                                            NO_CHANGE, H5C__DELETED_FLAG);
                            break;

                        case 3:
                            if ( (entries[type])[idx-lag].is_dirty ) {

                                unprotect_entry(cache_ptr, type, idx - lag,
                                                NO_CHANGE, H5C__DELETED_FLAG);
                            } else {

                                unprotect_entry(cache_ptr, type, idx - lag,
                                                dirty_destroys, 
                                                H5C__DELETED_FLAG);
                            }
                            break;

                        default:
                            HDassert(0); /* this can't happen... */
                            break;
                    }
                }

            } else {

                if ( ( pass ) && ( (idx - lag) >= 0 ) &&
                     ( ( idx - lag) <= max_indices[type] ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(u, %d, %d) ", type, (idx - lag));

                    unprotect_entry(cache_ptr, type, idx - lag, 
                                    dirty_unprotects, H5C__NO_FLAGS_SET);
                }
            }
            
            if ( verbose )
                HDfprintf(stdout, "\n");

            idx++;
        }
        type++;
    }

    if ( ( pass ) && ( display_stats ) ) {

        H5C_stats(cache_ptr, "test cache", display_detailed_stats);
    }

    return;

} /* row_major_scan_forward() */


/*-------------------------------------------------------------------------
 * Function:	hl_row_major_scan_forward()
 *
 * Purpose:	Do a high locality sequence of inserts, protects, and
 *		unprotects while scanning through the set of entries.  
 *		If pass is false on entry, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              10/21/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
hl_row_major_scan_forward(H5C_t * cache_ptr,
                          hbool_t verbose,
                          hbool_t reset_stats,
                          hbool_t display_stats,
                          hbool_t display_detailed_stats,
                          hbool_t do_inserts,
                          hbool_t dirty_inserts)
{
    const char * fcn_name = "hl_row_major_scan_forward";
    int32_t type;
    int32_t idx;
    int32_t i;
    int32_t lag = 100;

    if ( verbose )
        HDfprintf(stdout, "%s(): entering.\n", fcn_name);

    HDassert( lag > 5 );

    type = 0;

    if ( ( pass ) && ( reset_stats ) ) {

        H5C_stats__reset(cache_ptr);
    }

    while ( ( pass ) && ( type < NUMBER_OF_ENTRY_TYPES ) )
    {
        idx = -lag;

        while ( ( pass ) && ( idx <= (max_indices[type] + lag) ) )
        {
            if ( ( pass ) && ( do_inserts ) && ( (idx + lag) >= 0 ) &&
                 ( (idx + lag) <= max_indices[type] ) &&
                 ( ((idx + lag) % 2) == 0 ) &&
                 ( ! entry_in_cache(cache_ptr, type, (idx + lag)) ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(i, %d, %d) ", type, (idx + lag));

                insert_entry(cache_ptr, type, (idx + lag), dirty_inserts,
                             H5C__NO_FLAGS_SET);
            }

            i = idx;

            while ( ( pass ) && ( i >= idx - lag ) && ( i >= 0 ) )
            {
                if ( ( pass ) && ( i >= 0 ) && ( i <= max_indices[type] ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(p, %d, %d) ", type, i);

                    protect_entry(cache_ptr, type, i);

                    if ( verbose )
                        HDfprintf(stdout, "(u, %d, %d) ", type, i); 

                    unprotect_entry(cache_ptr, type, i, NO_CHANGE,
                                    H5C__NO_FLAGS_SET);
                }
                i--;
            }

            if ( verbose )
                HDfprintf(stdout, "\n");

            idx++;
        }
        type++;
    }

    if ( ( pass ) && ( display_stats ) ) {

        H5C_stats(cache_ptr, "test cache", display_detailed_stats);
    }

    return;

} /* hl_row_major_scan_forward() */


/*-------------------------------------------------------------------------
 * Function:	row_major_scan_backward()
 *
 * Purpose:	Do a sequence of inserts, protects, unprotects, renames,
 *		destroys while scanning backwards through the set of 
 *		entries.  If pass is false on entry, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/12/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
row_major_scan_backward(H5C_t * cache_ptr,
                        int32_t lag,
                        hbool_t verbose,
                        hbool_t reset_stats,
                        hbool_t display_stats,
                        hbool_t display_detailed_stats,
                        hbool_t do_inserts,
                        hbool_t dirty_inserts,
                        hbool_t do_renames,
                        hbool_t rename_to_main_addr,
                        hbool_t do_destroys,
                        int dirty_destroys,
                        int dirty_unprotects)
{
    const char * fcn_name = "row_major_scan_backward"; 
    int32_t type;
    int32_t idx;

    if ( verbose )
        HDfprintf(stdout, "%s(): Entering.\n", fcn_name);

    HDassert( lag > 5 );

    type = NUMBER_OF_ENTRY_TYPES - 1;

    if ( ( pass ) && ( reset_stats ) ) {

        H5C_stats__reset(cache_ptr);
    }

    while ( ( pass ) && ( type >= 0 ) )
    {
        idx = max_indices[type] + lag;

        while ( ( pass ) && ( idx >= -lag ) )
        {
            if ( ( pass ) && ( do_inserts ) && ( (idx - lag) >= 0 ) &&
                 ( (idx - lag) <= max_indices[type] ) &&
                 ( ((idx - lag) % 2) == 1 ) &&
                 ( ! entry_in_cache(cache_ptr, type, (idx - lag)) ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(i, %d, %d) ", type, (idx - lag));

                insert_entry(cache_ptr, type, (idx - lag), dirty_inserts,
                             H5C__NO_FLAGS_SET);
            }


            if ( ( pass ) && ( (idx - lag + 1) >= 0 ) &&
                 ( (idx - lag + 1) <= max_indices[type] ) &&
                 ( ( (idx - lag + 1) % 3 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, (idx - lag + 1));

                protect_entry(cache_ptr, type, (idx - lag + 1));
            }

            if ( ( pass ) && ( (idx - lag + 2) >= 0 ) &&
                 ( (idx - lag + 2) <= max_indices[type] ) &&
                 ( ( (idx - lag + 2) % 3 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(u, %d, %d) ", type, (idx - lag + 2)); 

                unprotect_entry(cache_ptr, type, idx-lag+2, NO_CHANGE, 
                                H5C__NO_FLAGS_SET);
            }


            if ( ( pass ) && ( do_renames ) && ( (idx - lag + 2) >= 0 ) &&
                 ( (idx - lag + 2) <= max_indices[type] ) &&
                 ( ( (idx - lag + 2) % 3 ) == 0 ) ) {

                rename_entry(cache_ptr, type, (idx - lag + 2), 
                             rename_to_main_addr);
            }


            if ( ( pass ) && ( (idx - lag + 3) >= 0 ) &&
                 ( (idx - lag + 3) <= max_indices[type] ) &&
                 ( ( (idx - lag + 3) % 5 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, (idx - lag + 3));

                protect_entry(cache_ptr, type, (idx - lag + 3));
            }

            if ( ( pass ) && ( (idx - lag + 5) >= 0 ) &&
                 ( (idx - lag + 5) <= max_indices[type] ) &&
                 ( ( (idx - lag + 5) % 5 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(u, %d, %d) ", type, (idx - lag + 5));

                unprotect_entry(cache_ptr, type, idx-lag+5, NO_CHANGE, 
                                H5C__NO_FLAGS_SET);
            }

            if ( ( pass ) && ( idx >= 0 ) && ( idx <= max_indices[type] ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, idx); 

                protect_entry(cache_ptr, type, idx);
            }


            if ( ( pass ) && ( (idx + lag - 2) >= 0 ) &&
                 ( (idx + lag - 2) <= max_indices[type] ) &&
                 ( ( (idx + lag - 2) % 7 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(u, %d, %d) ", type, (idx + lag - 2));

                unprotect_entry(cache_ptr, type, idx+lag-2, NO_CHANGE, 
                                H5C__NO_FLAGS_SET);
            }

            if ( ( pass ) && ( (idx + lag - 1) >= 0 ) &&
                 ( (idx + lag - 1) <= max_indices[type] ) &&
                 ( ( (idx + lag - 1) % 7 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, (idx + lag - 1));

                protect_entry(cache_ptr, type, (idx + lag - 1));
            }


            if ( do_destroys ) {

                if ( ( pass ) && ( (idx + lag) >= 0 ) &&
                     ( ( idx + lag) <= max_indices[type] ) ) {

                    switch ( (idx + lag) %4 ) {

                        case 0:
                            if ( (entries[type])[idx+lag].is_dirty ) {

                                unprotect_entry(cache_ptr, type, idx + lag,
                                                NO_CHANGE, H5C__NO_FLAGS_SET);
                            } else {

                                unprotect_entry(cache_ptr, type, idx + lag,
                                                dirty_unprotects, 
                                                H5C__NO_FLAGS_SET);
                            }
                            break;

                        case 1: /* we just did an insert */
                            unprotect_entry(cache_ptr, type, idx + lag,
                                            NO_CHANGE, H5C__NO_FLAGS_SET);
                            break;

                        case 2:
                            if ( (entries[type])[idx + lag].is_dirty ) {

                                unprotect_entry(cache_ptr, type, idx + lag,
                                                NO_CHANGE, H5C__DELETED_FLAG);
                            } else {

                                unprotect_entry(cache_ptr, type, idx + lag,
                                                dirty_destroys, 
                                                H5C__DELETED_FLAG);
                            }
                            break;

                        case 3: /* we just did an insrt */
                            unprotect_entry(cache_ptr, type, idx + lag,
                                            NO_CHANGE, H5C__DELETED_FLAG);
                            break;

                        default:
                            HDassert(0); /* this can't happen... */
                            break;
                    }
                }
            } else {

                if ( ( pass ) && ( (idx + lag) >= 0 ) &&
                     ( ( idx + lag) <= max_indices[type] ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(u, %d, %d) ", type, (idx - lag));

                    unprotect_entry(cache_ptr, type, idx + lag, 
                                    dirty_unprotects, H5C__NO_FLAGS_SET);
                }
            }
            
            if ( verbose )
                HDfprintf(stdout, "\n");

            idx--;
        }
        type--;
    }

    if ( ( pass ) && ( display_stats ) ) {

        H5C_stats(cache_ptr, "test cache", display_detailed_stats);
    }

    return;

} /* row_major_scan_backward() */


/*-------------------------------------------------------------------------
 * Function:	hl_row_major_scan_backward()
 *
 * Purpose:	Do a high locality sequence of inserts, protects, and
 *		unprotects while scanning through the set of entries.  
 *		If pass is false on entry, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              10/21/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
hl_row_major_scan_backward(H5C_t * cache_ptr,
                           hbool_t verbose,
                           hbool_t reset_stats,
                           hbool_t display_stats,
                           hbool_t display_detailed_stats,
                           hbool_t do_inserts,
                           hbool_t dirty_inserts)
{
    const char * fcn_name = "hl_row_major_scan_backward";
    int32_t type;
    int32_t idx;
    int32_t i;
    int32_t lag = 100;

    if ( verbose )
        HDfprintf(stdout, "%s(): entering.\n", fcn_name);

    HDassert( lag > 5 );

    type = NUMBER_OF_ENTRY_TYPES - 1;

    if ( ( pass ) && ( reset_stats ) ) {

        H5C_stats__reset(cache_ptr);
    }

    while ( ( pass ) && ( type >= 0 ) )
    {
        idx = max_indices[type] + lag;

        while ( ( pass ) && ( idx >= -lag ) )
        {
            if ( ( pass ) && ( do_inserts ) && ( (idx + lag) >= 0 ) &&
                 ( (idx + lag) <= max_indices[type] ) &&
                 ( ((idx + lag) % 2) == 0 ) &&
                 ( ! entry_in_cache(cache_ptr, type, (idx + lag)) ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(i, %d, %d) ", type, (idx + lag));

                insert_entry(cache_ptr, type, (idx + lag), dirty_inserts,
                             H5C__NO_FLAGS_SET);
            }

            i = idx;

            while ( ( pass ) && ( i >= idx - lag ) && ( i >= 0 ) )
            {
                if ( ( pass ) && ( i >= 0 ) && ( i <= max_indices[type] ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(p, %d, %d) ", type, i);

                    protect_entry(cache_ptr, type, i);

                    if ( verbose )
                        HDfprintf(stdout, "(u, %d, %d) ", type, i); 

                    unprotect_entry(cache_ptr, type, i, NO_CHANGE, 
                                    H5C__NO_FLAGS_SET);
                }
                i--;
            }

            if ( verbose )
                HDfprintf(stdout, "\n");

            idx--;
        }
        type--;
    }

    if ( ( pass ) && ( display_stats ) ) {

        H5C_stats(cache_ptr, "test cache", display_detailed_stats);
    }

    return;

} /* hl_row_major_scan_backward() */


/*-------------------------------------------------------------------------
 * Function:	col_major_scan_forward()
 *
 * Purpose:	Do a sequence of inserts, protects, and unprotects
 *		while scanning through the set of entries.  If
 *		pass is false on entry, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/23/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
col_major_scan_forward(H5C_t * cache_ptr,
                       int32_t lag,
                       hbool_t verbose,
                       hbool_t reset_stats,
                       hbool_t display_stats,
                       hbool_t display_detailed_stats,
                       hbool_t do_inserts,
                       hbool_t dirty_inserts,
                       int dirty_unprotects)
{
    const char * fcn_name = "col_major_scan_forward()";
    int32_t type;
    int32_t idx;

    if ( verbose )
        HDfprintf(stdout, "%s: entering.\n", fcn_name);

    HDassert( lag > 5 );

    type = 0;

    if ( ( pass ) && ( reset_stats ) ) {

        H5C_stats__reset(cache_ptr);
    }

    idx = -lag;

    while ( ( pass ) && ( (idx - lag) <= MAX_ENTRIES ) )
    {
        type = 0;

        while ( ( pass ) && ( type < NUMBER_OF_ENTRY_TYPES ) )
        {
            if ( ( pass ) && ( do_inserts ) && ( (idx + lag) >= 0 ) &&
                 ( (idx + lag) <= max_indices[type] ) &&
                 ( ((idx + lag) % 3) == 0 ) &&
                 ( ! entry_in_cache(cache_ptr, type, (idx + lag)) ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(i, %d, %d) ", type, (idx + lag));

                insert_entry(cache_ptr, type, (idx + lag), dirty_inserts,
                             H5C__NO_FLAGS_SET);
            }

            if ( ( pass ) && ( idx >= 0 ) && ( idx <= max_indices[type] ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, idx);

                protect_entry(cache_ptr, type, idx);
            }

            if ( ( pass ) && ( (idx - lag) >= 0 ) &&
                 ( (idx - lag) <= max_indices[type] ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(u, %d, %d) ", type, (idx - lag));

                unprotect_entry(cache_ptr, type, idx - lag, 
                                dirty_unprotects, H5C__NO_FLAGS_SET);
            }
            
            if ( verbose )
                HDfprintf(stdout, "\n");

            type++;
        }

        idx++;
    }

    if ( ( pass ) && ( display_stats ) ) {

        H5C_stats(cache_ptr, "test cache", display_detailed_stats);
    }

    return;

} /* col_major_scan_forward() */


/*-------------------------------------------------------------------------
 * Function:	hl_col_major_scan_forward()
 *
 * Purpose:	Do a high locality sequence of inserts, protects, and 
 *		unprotects while scanning through the set of entries.  If
 *		pass is false on entry, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              19/25/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
hl_col_major_scan_forward(H5C_t * cache_ptr,
                          hbool_t verbose,
                          hbool_t reset_stats,
                          hbool_t display_stats,
                          hbool_t display_detailed_stats,
                          hbool_t do_inserts,
                          hbool_t dirty_inserts,
                          int dirty_unprotects)
{
    const char * fcn_name = "hl_col_major_scan_forward()";
    int32_t type;
    int32_t idx;
    int32_t lag = 200;
    int32_t i;

    if ( verbose )
        HDfprintf(stdout, "%s: entering.\n", fcn_name);

    HDassert( lag > 5 );

    type = 0;

    if ( ( pass ) && ( reset_stats ) ) {

        H5C_stats__reset(cache_ptr);
    }

    idx = 0;

    while ( ( pass ) && ( idx <= MAX_ENTRIES ) )
    {

        i = idx;

        while ( ( pass ) && ( i >= 0 ) && ( i >= (idx - lag) ) ) {

            type = 0;

            while ( ( pass ) && ( type < NUMBER_OF_ENTRY_TYPES ) )
            {
                if ( ( pass ) && ( do_inserts ) && ( i == idx ) &&
                     ( i <= max_indices[type] ) &&
                     ( (i % 3) == 0 ) &&
                     ( ! entry_in_cache(cache_ptr, type, i) ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(i, %d, %d) ", type, i);

                    insert_entry(cache_ptr, type, i, dirty_inserts,
                                 H5C__NO_FLAGS_SET);
                }

                if ( ( pass ) && ( i >= 0 ) && ( i <= max_indices[type] ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(p, %d, %d) ", type, i);

                    protect_entry(cache_ptr, type, i);
                }

                if ( ( pass ) && ( i >= 0 ) &&
                     ( i <= max_indices[type] ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(u, %d, %d) ", type, i);

                    unprotect_entry(cache_ptr, type, i, 
                                    dirty_unprotects, H5C__NO_FLAGS_SET);
                }
            
                if ( verbose )
                    HDfprintf(stdout, "\n");

                type++;
            }

            i--;
        }

        idx++;
    }

    if ( ( pass ) && ( display_stats ) ) {

        H5C_stats(cache_ptr, "test cache", display_detailed_stats);
    }

    return;

} /* hl_col_major_scan_forward() */


/*-------------------------------------------------------------------------
 * Function:	col_major_scan_backward()
 *
 * Purpose:	Do a sequence of inserts, protects, and unprotects
 *		while scanning backwards through the set of 
 *		entries.  If pass is false on entry, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/23/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
col_major_scan_backward(H5C_t * cache_ptr,
                        int32_t lag,
                        hbool_t verbose,
                        hbool_t reset_stats,
                        hbool_t display_stats,
                        hbool_t display_detailed_stats,
                        hbool_t do_inserts,
                        hbool_t dirty_inserts,
                        int dirty_unprotects)
{
    const char * fcn_name = "col_major_scan_backward()";
    int mile_stone = 1;
    int32_t type;
    int32_t idx;

    if ( verbose )
        HDfprintf(stdout, "%s: entering.\n", fcn_name);

    HDassert( lag > 5 );

    if ( ( pass ) && ( reset_stats ) ) {

        H5C_stats__reset(cache_ptr);
    }

    idx = MAX_ENTRIES + lag;

    if ( verbose ) /* 1 */
        HDfprintf(stdout, "%s: point %d.\n", fcn_name, mile_stone++);


    while ( ( pass ) && ( (idx + lag) >= 0 ) )
    {
        type = NUMBER_OF_ENTRY_TYPES - 1;

        while ( ( pass ) && ( type >= 0 ) )
        {
            if ( ( pass ) && ( do_inserts) && ( (idx - lag) >= 0 ) &&
                 ( (idx - lag) <= max_indices[type] ) &&
                 ( ((idx - lag) % 3) == 0 ) &&
                 ( ! entry_in_cache(cache_ptr, type, (idx - lag)) ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(i, %d, %d) ", type, (idx - lag));

                insert_entry(cache_ptr, type, (idx - lag), dirty_inserts,
                             H5C__NO_FLAGS_SET);
            }

            if ( ( pass ) && ( idx >= 0 ) && ( idx <= max_indices[type] ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, idx);

                protect_entry(cache_ptr, type, idx);
            }

            if ( ( pass ) && ( (idx + lag) >= 0 ) &&
                 ( (idx + lag) <= max_indices[type] ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(u, %d, %d) ", type, (idx + lag));

                unprotect_entry(cache_ptr, type, idx + lag, 
                                dirty_unprotects, H5C__NO_FLAGS_SET);
            }
            
            if ( verbose )
                HDfprintf(stdout, "\n");

            type--;
        }

        idx--;
    }

    if ( verbose ) /* 2 */
        HDfprintf(stdout, "%s: point %d.\n", fcn_name, mile_stone++);

    if ( ( pass ) && ( display_stats ) ) {

        H5C_stats(cache_ptr, "test cache", display_detailed_stats);
    }

    if ( verbose )
        HDfprintf(stdout, "%s: exiting.\n", fcn_name);

    return;

} /* col_major_scan_backward() */


/*-------------------------------------------------------------------------
 * Function:	hl_col_major_scan_backward()
 *
 * Purpose:	Do a high locality sequence of inserts, protects, and 
 *		unprotects while scanning backwards through the set of 
 *		entries.  If pass is false on entry, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              10/25/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
hl_col_major_scan_backward(H5C_t * cache_ptr,
                           hbool_t verbose,
                           hbool_t reset_stats,
                           hbool_t display_stats,
                           hbool_t display_detailed_stats,
                           hbool_t do_inserts,
                           hbool_t dirty_inserts,
                           int dirty_unprotects)
{
    const char * fcn_name = "hl_col_major_scan_backward()";
    int32_t type;
    int32_t idx;
    int32_t lag = 50;
    int32_t i;

    if ( verbose )
        HDfprintf(stdout, "%s: entering.\n", fcn_name);

    type = 0;

    if ( ( pass ) && ( reset_stats ) ) {

        H5C_stats__reset(cache_ptr);
    }

    idx = MAX_ENTRIES;

    while ( ( pass ) && ( idx >= 0 ) )
    {

        i = idx;

        while ( ( pass ) && ( i <= MAX_ENTRIES ) && ( i <= (idx + lag) ) ) {

            type = 0;

            while ( ( pass ) && ( type < NUMBER_OF_ENTRY_TYPES ) )
            {
                if ( ( pass ) && ( do_inserts ) && ( i == idx ) &&
                     ( i <= max_indices[type] ) &&
                     ( ! entry_in_cache(cache_ptr, type, i) ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(i, %d, %d) ", type, i);

                    insert_entry(cache_ptr, type, i, dirty_inserts,
                                 H5C__NO_FLAGS_SET);
                }

                if ( ( pass ) && ( i >= 0 ) && ( i <= max_indices[type] ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(p, %d, %d) ", type, i);

                    protect_entry(cache_ptr, type, i);
                }

                if ( ( pass ) && ( i >= 0 ) &&
                     ( i <= max_indices[type] ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(u, %d, %d) ", type, i);

                    unprotect_entry(cache_ptr, type, i, 
                                    dirty_unprotects, H5C__NO_FLAGS_SET);
                }
            
                if ( verbose )
                    HDfprintf(stdout, "\n");

                type++;
            }

            i++;
        }

        idx--;
    }

    if ( ( pass ) && ( display_stats ) ) {

        H5C_stats(cache_ptr, "test cache", display_detailed_stats);
    }

    return;

} /* hl_col_major_scan_backward() */


/**************************************************************************/
/**************************************************************************/
/********************************* tests: *********************************/
/**************************************************************************/
/**************************************************************************/

/*-------------------------------------------------------------------------
 * Function:	smoke_check_1()
 *
 * Purpose:	A basic functional test, inserts, destroys, and renames in 
 *              the mix, along with repeated protects and unprotects.  
 *		All entries are marked as clean.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/16/04
 *
 * Modifications:
 *
 *		JRM -- 1/18/05
 *		Added code to skip this test if the skip_long_tests global
 *		is true.
 *
 *-------------------------------------------------------------------------
 */

static void
smoke_check_1(void)
{
    const char * fcn_name = "smoke_check_1";
    hbool_t show_progress = FALSE;
    hbool_t dirty_inserts = FALSE;
    int dirty_unprotects = FALSE;
    int dirty_destroys = FALSE;
    hbool_t display_stats = FALSE;
    int32_t lag = 10;
    int mile_stone = 1;
    H5C_t * cache_ptr = NULL;

    TESTING("smoke check #1 -- all clean, ins, dest, ren, 4/2 MB cache");

    if ( skip_long_tests ) {

        SKIPPED();

        HDfprintf(stdout, "	Long tests disabled.\n");

        return;
    }

    pass = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    reset_entries();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    cache_ptr = setup_cache((size_t)(4 * 1024 * 1024),
                            (size_t)(2 * 1024 * 1024));

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ TRUE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ FALSE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* do_renames             */ TRUE,
                            /* rename_to_main_addr    */ TRUE,
                            /* do_destroys            */ FALSE,
                            /* dirty_destroys         */ dirty_destroys,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ FALSE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    /* flush and destroy all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr, 
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    col_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    /* flush all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr, 
                /* destroy_entries     */ FALSE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    col_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ TRUE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    takedown_cache(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    verify_clean();
    verify_unprotected();

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n", 
                  fcn_name, failure_mssg);

} /* smoke_check_1() */


/*-------------------------------------------------------------------------
 * Function:	smoke_check_2()
 *
 * Purpose:	A basic functional test, with inserts, destroys, and 
 *		renames in the mix, along with some repeated protects 
 *		and unprotects.  About half the entries are marked as 
 *		dirty.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *		JRM -- 1/18/05
 *		Added code to skip this test if the skip_long_tests global
 *		is true.
 *
 *-------------------------------------------------------------------------
 */

static void
smoke_check_2(void)
{
    const char * fcn_name = "smoke_check_2";
    hbool_t show_progress = FALSE;
    hbool_t dirty_inserts = TRUE;
    int dirty_unprotects = TRUE;
    int dirty_destroys = TRUE;
    hbool_t display_stats = FALSE;
    int32_t lag = 10;
    int mile_stone = 1;
    H5C_t * cache_ptr = NULL;

    TESTING("smoke check #2 -- ~1/2 dirty, ins, dest, ren, 4/2 MB cache");

    if ( skip_long_tests ) {

        SKIPPED();

        HDfprintf(stdout, "	Long tests disabled.\n");

        return;
    }

    pass = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    reset_entries();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    cache_ptr = setup_cache((size_t)(4 * 1024 * 1024),
                            (size_t)(2 * 1024 * 1024));

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ TRUE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ FALSE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* do_renames             */ TRUE,
                            /* rename_to_main_addr    */ TRUE,
                            /* do_destroys            */ FALSE,
                            /* dirty_destroys         */ dirty_destroys,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ FALSE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    /* flush and destroy all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr, 
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    col_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    /* flush all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr, 
                /* destroy_entries     */ FALSE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    col_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ TRUE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    takedown_cache(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    verify_clean();
    verify_unprotected();

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n", 
                  fcn_name, failure_mssg);

} /* smoke_check_2() */


/*-------------------------------------------------------------------------
 * Function:	smoke_check_3()
 *
 * Purpose:	A basic functional test on a tiny cache, with inserts, 
 *		destroys, and renames in the mix, along with repeated 
 *		protects and unprotects.  All entries are marked as clean.  
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/16/04
 *
 * Modifications:
 *
 *		JRM -- 1/18/05
 *		Added code to skip this test if the skip_long_tests global
 *		is true.
 *
 *-------------------------------------------------------------------------
 */

static void
smoke_check_3(void)
{
    const char * fcn_name = "smoke_check_3";
    hbool_t show_progress = FALSE;
    hbool_t dirty_inserts = FALSE;
    int dirty_unprotects = FALSE;
    int dirty_destroys = FALSE;
    hbool_t display_stats = FALSE;
    int32_t lag = 10;
    int mile_stone = 1;
    H5C_t * cache_ptr = NULL;

    TESTING("smoke check #3 -- all clean, ins, dest, ren, 2/1 KB cache");

    if ( skip_long_tests ) {

        SKIPPED();

        HDfprintf(stdout, "	Long tests disabled.\n");

        return;
    }

    pass = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    reset_entries();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    cache_ptr = setup_cache((size_t)(2 * 1024),
                            (size_t)(1 * 1024));

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ TRUE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ FALSE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* do_renames             */ TRUE,
                            /* rename_to_main_addr    */ TRUE,
                            /* do_destroys            */ FALSE,
                            /* dirty_destroys         */ dirty_destroys,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ FALSE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    /* flush and destroy all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr, 
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    col_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    /* flush all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr, 
                /* destroy_entries     */ FALSE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    col_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ TRUE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    takedown_cache(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    verify_clean();
    verify_unprotected();

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n", 
                  fcn_name, failure_mssg);

} /* smoke_check_3() */


/*-------------------------------------------------------------------------
 * Function:	smoke_check_4()
 *
 * Purpose:	A basic functional test on a tiny cache, with inserts,
 *	 	destroys, and renames in the mix, along with repeated 
 *		protects and unprotects.  About half the entries are 
 *		marked as dirty.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *		JRM -- 1/18/05
 *		Added code to skip this test if the skip_long_tests global
 *		is true.
 *
 *-------------------------------------------------------------------------
 */

static void
smoke_check_4(void)
{
    const char * fcn_name = "smoke_check_4";
    hbool_t show_progress = FALSE;
    hbool_t dirty_inserts = TRUE;
    int dirty_unprotects = TRUE;
    int dirty_destroys = TRUE;
    hbool_t display_stats = FALSE;
    int32_t lag = 10;
    int mile_stone = 1;
    H5C_t * cache_ptr = NULL;

    TESTING("smoke check #4 -- ~1/2 dirty, ins, dest, ren, 2/1 KB cache");

    if ( skip_long_tests ) {

        SKIPPED();

        HDfprintf(stdout, "	Long tests disabled.\n");

        return;
    }

    pass = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    reset_entries();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    cache_ptr = setup_cache((size_t)(2 * 1024),
                            (size_t)(1 * 1024));

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ TRUE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ FALSE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* do_renames             */ TRUE,
                            /* rename_to_main_addr    */ TRUE,
                            /* do_destroys            */ FALSE,
                            /* dirty_destroys         */ dirty_destroys,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ FALSE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    /* flush and destroy all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr, 
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    col_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    /* flush all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr, 
                /* destroy_entries     */ FALSE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    col_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ TRUE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    takedown_cache(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    verify_clean();
    verify_unprotected();

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n", 
                  fcn_name, failure_mssg);

} /* smoke_check_4() */


/*-------------------------------------------------------------------------
 * Function:	smoke_check_5()
 *
 * Purpose:	A basic functional test on a cache with automatic cache 
 *		resizing enabled, with inserts in the mix, along with 
 *		repeated protects and unprotects.  All entries are marked 
 *		as clean.  
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              10/14/04
 *
 * Modifications:
 *
 *		JRM -- 1/18/05
 *		Added code to skip this test if the skip_long_tests global
 *		is true.
 *
 *-------------------------------------------------------------------------
 */

static void
smoke_check_5(void)
{
    const char * fcn_name = "smoke_check_5";
    herr_t result;
    hbool_t show_progress = FALSE;
    hbool_t dirty_inserts = FALSE;
    int dirty_unprotects = FALSE;
    hbool_t display_stats = FALSE;
    int mile_stone = 1;
    H5C_t * cache_ptr = NULL;
    H5C_auto_size_ctl_t auto_size_ctl =
    {
        /* int32_t     version                = */ H5C__CURR_AUTO_SIZE_CTL_VER,
#if 1
        /* H5C_auto_resize_report_fcn rpt_fcn = */ NULL,
#else
        /* H5C_auto_resize_report_fcn rpt_fcn = */ H5C_def_auto_resize_rpt_fcn,
#endif
        /* hbool_t     set_initial_size       = */ TRUE,
        /* size_t      initial_size           = */ (2 * 1024 * 1024),

        /* double      min_clean_fraction     = */ 0.1,

        /* size_t      max_size               = */ (32 * 1024 * 1025),
        /* size_t      min_size               = */ (512 * 1024),

        /* int64_t     epoch_length           = */ 50000,


        /* enum H5C_cache_incr_mode incr_mode = */ H5C_incr__threshold,

        /* double     lower_hr_threshold      = */ 0.75,

        /* double      increment              = */ 2.0,

        /* hbool_t     apply_max_increment    = */ TRUE,
        /* size_t      max_increment          = */ (4 * 1024 * 1024),


        /* enum H5C_cache_decr_mode decr_mode = */ H5C_decr__threshold,

        /* double      upper_hr_threshold     = */ 0.995,

        /* double      decrement              = */ 0.9,

        /* hbool_t     apply_max_decrement    = */ TRUE,
        /* size_t      max_decrement          = */ (1 * 1024 * 1024),

        /* int32_t     epochs_before_eviction = */ 3,

        /* hbool_t     apply_empty_reserve    = */ TRUE,
        /* double      empty_reserve          = */ 0.5
    };

    TESTING("smoke check #5 -- all clean, ins, prot, unprot, AR cache 1");

    if ( skip_long_tests ) {

        SKIPPED();

        HDfprintf(stdout, "	Long tests disabled.\n");

        return;
    }

    pass = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    reset_entries();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    cache_ptr = setup_cache((size_t)(2 * 1024),
                            (size_t)(1 * 1024));

    if ( pass ) {

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 1.\n";
        }
    }

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    hl_row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ FALSE,
                              /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    hl_row_major_scan_backward(/* cache_ptr              */ cache_ptr,
                               /* verbose                */ FALSE,
                               /* reset_stats            */ TRUE,
                               /* display_stats          */ display_stats,
                               /* display_detailed_stats */ FALSE,
                               /* do_inserts             */ FALSE,
                               /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    hl_row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ TRUE,
                              /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    /* flush and destroy all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr, 
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    hl_col_major_scan_forward(/* cache_ptr              */ cache_ptr,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ TRUE,
                              /* dirty_inserts          */ dirty_inserts,
                              /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    /* flush all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr, 
                /* destroy_entries     */ FALSE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    hl_col_major_scan_backward(/* cache_ptr              */ cache_ptr,
                               /* verbose                */ FALSE,
                               /* reset_stats            */ TRUE,
                               /* display_stats          */ display_stats,
                               /* display_detailed_stats */ FALSE,
                               /* do_inserts             */ TRUE,
                               /* dirty_inserts          */ dirty_inserts,
                               /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    takedown_cache(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    verify_clean();
    verify_unprotected();

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n", 
                  fcn_name, failure_mssg);

} /* smoke_check_5() */


/*-------------------------------------------------------------------------
 * Function:	smoke_check_6()
 *
 * Purpose:	A basic functional test on a cache with automatic cache 
 *		resizing enabled, with inserts in the mix, along with 
 *              repeated protects and unprotects.  About one half of all
 *		entries are marked as dirty.  
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              10/25/04
 *
 * Modifications:
 *
 *		JRM -- 1/18/05
 *		Added code to skip this test if the skip_long_tests global
 *		is true.
 *
 *-------------------------------------------------------------------------
 */

static void
smoke_check_6(void)
{
    const char * fcn_name = "smoke_check_6";
    herr_t result;
    hbool_t show_progress = FALSE;
    hbool_t dirty_inserts = TRUE;
    int dirty_unprotects = FALSE;
    hbool_t display_stats = FALSE;
    int mile_stone = 1;
    H5C_t * cache_ptr = NULL;
    H5C_auto_size_ctl_t auto_size_ctl =
    {
        /* int32_t     version                = */ H5C__CURR_AUTO_SIZE_CTL_VER,
#if 1
        /* H5C_auto_resize_report_fcn rpt_fcn = */ NULL,
#else
        /* H5C_auto_resize_report_fcn rpt_fcn = */ H5C_def_auto_resize_rpt_fcn,
#endif
        /* hbool_t     set_initial_size       = */ TRUE,
        /* size_t      initial_size           = */ (2 * 1024 * 1024),

        /* double      min_clean_fraction     = */ 0.1,

        /* size_t      max_size               = */ (32 * 1024 * 1025),
        /* size_t      min_size               = */ (512 * 1024),

        /* int64_t     epoch_length           = */ 50000,


        /* enum H5C_cache_incr_mode incr_mode = */ H5C_incr__threshold,

        /* double     lower_hr_threshold      = */ 0.75,

        /* double      increment              = */ 2.0,

        /* hbool_t     apply_max_increment    = */ TRUE,
        /* size_t      max_increment          = */ (4 * 1024 * 1024),


        /* enum H5C_cache_decr_mode decr_mode = */ H5C_decr__threshold,

        /* double      upper_hr_threshold     = */ 0.995,

        /* double      decrement              = */ 0.9,

        /* hbool_t     apply_max_decrement    = */ TRUE,
        /* size_t      max_decrement          = */ (1 * 1024 * 1024),

        /* int32_t     epochs_before_eviction = */ 3,

        /* hbool_t     apply_empty_reserve    = */ TRUE,
        /* double      empty_reserve          = */ 0.05
    };

    TESTING("smoke check #6 -- ~1/2 dirty, ins, prot, unprot, AR cache 1");

    pass = TRUE;

    if ( skip_long_tests ) {

        SKIPPED();

        HDfprintf(stdout, "	Long tests disabled.\n");

        return;
    }

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    reset_entries();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    cache_ptr = setup_cache((size_t)(2 * 1024),
                            (size_t)(1 * 1024));

    if ( pass ) {

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 1.\n";
        }
    }

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    hl_row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ FALSE,
                              /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    hl_row_major_scan_backward(/* cache_ptr              */ cache_ptr,
                               /* verbose                */ FALSE,
                               /* reset_stats            */ TRUE,
                               /* display_stats          */ display_stats,
                               /* display_detailed_stats */ FALSE,
                               /* do_inserts             */ FALSE,
                               /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    hl_row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ TRUE,
                              /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    /* flush and destroy all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr, 
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    hl_col_major_scan_forward(/* cache_ptr              */ cache_ptr,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ TRUE,
                              /* dirty_inserts          */ dirty_inserts,
                              /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    /* flush all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr, 
                /* destroy_entries     */ FALSE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    hl_col_major_scan_backward(/* cache_ptr              */ cache_ptr,
                               /* verbose                */ FALSE,
                               /* reset_stats            */ TRUE,
                               /* display_stats          */ display_stats,
                               /* display_detailed_stats */ FALSE,
                               /* do_inserts             */ TRUE,
                               /* dirty_inserts          */ dirty_inserts,
                               /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    takedown_cache(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    verify_clean();
    verify_unprotected();

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n", 
                  fcn_name, failure_mssg);

} /* smoke_check_6() */


/*-------------------------------------------------------------------------
 * Function:	smoke_check_7()
 *
 * Purpose:	A basic functional test on a cache with automatic cache 
 *		resizing enabled, with inserts in the mix, along with 
 *		repeated protects and unprotects.  All entries are marked 
 *		as clean.  
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              12/2/04
 *
 * Modifications:
 *
 *		JRM -- 1/18/05
 *		Added code to skip this test if the skip_long_tests global
 *		is true.
 *
 *-------------------------------------------------------------------------
 */

static void
smoke_check_7(void)
{
    const char * fcn_name = "smoke_check_7";
    herr_t result;
    hbool_t show_progress = FALSE;
    hbool_t dirty_inserts = FALSE;
    int dirty_unprotects = FALSE;
    hbool_t display_stats = FALSE;
    int mile_stone = 1;
    H5C_t * cache_ptr = NULL;
    H5C_auto_size_ctl_t auto_size_ctl =
    {
        /* int32_t     version                = */ H5C__CURR_AUTO_SIZE_CTL_VER,
#if 1
        /* H5C_auto_resize_report_fcn rpt_fcn = */ NULL,
#else
        /* H5C_auto_resize_report_fcn rpt_fcn = */ H5C_def_auto_resize_rpt_fcn,
#endif
        /* hbool_t     set_initial_size       = */ TRUE,
        /* size_t      initial_size           = */ (2 * 1024 * 1024),

        /* double      min_clean_fraction     = */ 0.1,

        /* size_t      max_size               = */ (32 * 1024 * 1025),
        /* size_t      min_size               = */ (512 * 1024),

        /* int64_t     epoch_length           = */ 100000,


        /* enum H5C_cache_incr_mode incr_mode = */ H5C_incr__threshold,

        /* double     lower_hr_threshold      = */ 0.75,

        /* double      increment              = */ 2.0,

        /* hbool_t     apply_max_increment    = */ TRUE,
        /* size_t      max_increment          = */ (8 * 1024 * 1024),


        /* enum H5C_cache_decr_mode decr_mode = */ 
                                             H5C_decr__age_out_with_threshold,

        /* double      upper_hr_threshold     = */ 0.995,

        /* double      decrement              = */ 0.9,

        /* hbool_t     apply_max_decrement    = */ TRUE,
        /* size_t      max_decrement          = */ (1 * 1024 * 1024),

        /* int32_t     epochs_before_eviction = */ 3,

        /* hbool_t     apply_empty_reserve    = */ TRUE,
        /* double      empty_reserve          = */ 0.1
    };

    TESTING("smoke check #7 -- all clean, ins, prot, unprot, AR cache 2");

    if ( skip_long_tests ) {

        SKIPPED();

        HDfprintf(stdout, "	Long tests disabled.\n");

        return;
    }

    pass = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    reset_entries();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    cache_ptr = setup_cache((size_t)(2 * 1024),
                            (size_t)(1 * 1024));

    if ( pass ) {

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 1.\n";
        }
    }

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    hl_row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ FALSE,
                              /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    hl_row_major_scan_backward(/* cache_ptr              */ cache_ptr,
                               /* verbose                */ FALSE,
                               /* reset_stats            */ TRUE,
                               /* display_stats          */ display_stats,
                               /* display_detailed_stats */ FALSE,
                               /* do_inserts             */ FALSE,
                               /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    hl_row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ TRUE,
                              /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    /* flush and destroy all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr, 
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    hl_col_major_scan_forward(/* cache_ptr              */ cache_ptr,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ TRUE,
                              /* dirty_inserts          */ dirty_inserts,
                              /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    /* flush all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr, 
                /* destroy_entries     */ FALSE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    hl_col_major_scan_backward(/* cache_ptr              */ cache_ptr,
                               /* verbose                */ FALSE,
                               /* reset_stats            */ TRUE,
                               /* display_stats          */ display_stats,
                               /* display_detailed_stats */ FALSE,
                               /* do_inserts             */ TRUE,
                               /* dirty_inserts          */ dirty_inserts,
                               /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    takedown_cache(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    verify_clean();
    verify_unprotected();

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n", 
                  fcn_name, failure_mssg);

} /* smoke_check_7() */


/*-------------------------------------------------------------------------
 * Function:	smoke_check_8()
 *
 * Purpose:	A basic functional test on a cache with automatic cache 
 *		resizing enabled, with inserts in the mix, along with 
 *              repeated protects and unprotects.  About one half of all
 *		entries are marked as dirty.  
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              10/25/04
 *
 * Modifications:
 *
 *		JRM -- 1/18/05
 *		Added code to skip this test if the skip_long_tests global
 *		is true.
 *
 *-------------------------------------------------------------------------
 */

static void
smoke_check_8(void)
{
    const char * fcn_name = "smoke_check_8";
    herr_t result;
    hbool_t show_progress = FALSE;
    hbool_t dirty_inserts = TRUE;
    int dirty_unprotects = FALSE;
    hbool_t display_stats = FALSE;
    int mile_stone = 1;
    H5C_t * cache_ptr = NULL;
    H5C_auto_size_ctl_t auto_size_ctl =
    {
        /* int32_t     version                = */ H5C__CURR_AUTO_SIZE_CTL_VER,
#if 1
        /* H5C_auto_resize_report_fcn rpt_fcn = */ NULL,
#else
        /* H5C_auto_resize_report_fcn rpt_fcn = */ H5C_def_auto_resize_rpt_fcn,
#endif
        /* hbool_t     set_initial_size       = */ TRUE,
        /* size_t      initial_size           = */ (2 * 1024 * 1024),

        /* double      min_clean_fraction     = */ 0.1,

        /* size_t      max_size               = */ (32 * 1024 * 1025),
        /* size_t      min_size               = */ (512 * 1024),

        /* int64_t     epoch_length           = */ 100000,


        /* enum H5C_cache_incr_mode incr_mode = */ H5C_incr__threshold,

        /* double     lower_hr_threshold      = */ 0.75,

        /* double      increment              = */ 2.0,

        /* hbool_t     apply_max_increment    = */ TRUE,
        /* size_t      max_increment          = */ (4 * 1024 * 1024),


        /* enum H5C_cache_decr_mode decr_mode = */ 
                                             H5C_decr__age_out_with_threshold,

        /* double      upper_hr_threshold     = */ 0.995,

        /* double      decrement              = */ 0.9,

        /* hbool_t     apply_max_decrement    = */ TRUE,
        /* size_t      max_decrement          = */ (1 * 1024 * 1024),

        /* int32_t     epochs_before_eviction = */ 3,

        /* hbool_t     apply_empty_reserve    = */ TRUE,
        /* double      empty_reserve          = */ 0.1
    };

    TESTING("smoke check #8 -- ~1/2 dirty, ins, prot, unprot, AR cache 2");

    if ( skip_long_tests ) {

        SKIPPED();

        HDfprintf(stdout, "	Long tests disabled.\n");

        return;
    }

    pass = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    reset_entries();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    cache_ptr = setup_cache((size_t)(2 * 1024),
                            (size_t)(1 * 1024));

    if ( pass ) {

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 1.\n";
        }
    }

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    hl_row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ FALSE,
                              /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    hl_row_major_scan_backward(/* cache_ptr              */ cache_ptr,
                               /* verbose                */ FALSE,
                               /* reset_stats            */ TRUE,
                               /* display_stats          */ display_stats,
                               /* display_detailed_stats */ FALSE,
                               /* do_inserts             */ FALSE,
                               /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    hl_row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ TRUE,
                              /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    /* flush and destroy all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr, 
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    hl_col_major_scan_forward(/* cache_ptr              */ cache_ptr,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ TRUE,
                              /* dirty_inserts          */ dirty_inserts,
                              /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    /* flush all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr, 
                /* destroy_entries     */ FALSE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    hl_col_major_scan_backward(/* cache_ptr              */ cache_ptr,
                               /* verbose                */ FALSE,
                               /* reset_stats            */ TRUE,
                               /* display_stats          */ display_stats,
                               /* display_detailed_stats */ FALSE,
                               /* do_inserts             */ TRUE,
                               /* dirty_inserts          */ dirty_inserts,
                               /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    takedown_cache(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    verify_clean();
    verify_unprotected();

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n", 
                  fcn_name, failure_mssg);

} /* smoke_check_8() */


/*-------------------------------------------------------------------------
 * Function:	write_permitted_check()
 *
 * Purpose:	A basic test of the write permitted function.  In essence,
 *		we load the cache up with dirty entryies, set 
 *		write_permitted to FALSE, and then protect a bunch of 
 *		entries.  If there are any writes while write_permitted is
 *		FALSE, the test will fail.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
write_permitted_check(void)
{

#if H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS

    const char * fcn_name = "write_permitted_check";
    hbool_t show_progress = FALSE;
    hbool_t display_stats = FALSE;
    int32_t lag = 10;
    int mile_stone = 1;
    H5C_t * cache_ptr = NULL;

#endif /* H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

    TESTING("write permitted check -- 1/0 MB cache");

#if H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS

    pass = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    reset_entries();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    cache_ptr = setup_cache((size_t)(1 * 1024 * 1024),
                            (size_t)(0));

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ TRUE,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ TRUE,
                           /* dirty_destroys         */ TRUE,
                           /* dirty_unprotects       */ TRUE);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    write_permitted = FALSE;

    row_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ FALSE,
                            /* dirty_inserts          */ FALSE,
                            /* do_renames             */ TRUE,
                            /* rename_to_main_addr    */ TRUE,
                            /* do_destroys            */ FALSE,
                            /* dirty_destroys         */ FALSE,
                            /* dirty_unprotects       */ NO_CHANGE);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    write_permitted = TRUE;

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ TRUE,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ FALSE,
                           /* dirty_destroys         */ TRUE,
                           /* dirty_unprotects       */ TRUE);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    /* flush and destroy all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr, 
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    col_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ TRUE,
                           /* dirty_unprotects       */ TRUE);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    write_permitted = FALSE;

    col_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ FALSE,
                            /* dirty_inserts          */ FALSE,
                            /* dirty_unprotects       */ NO_CHANGE);

    write_permitted = TRUE;

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    takedown_cache(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n", 
                  fcn_name, mile_stone++, (int)pass);

    verify_clean();
    verify_unprotected();

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n", 
                  fcn_name, failure_mssg);

#else /* H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

    SKIPPED();

    HDfprintf(stdout, "	Clean and dirty LRU lists disabled.\n");

#endif /* H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

} /* write_permitted_check() */


/*-------------------------------------------------------------------------
 * Function:	check_flush_cache()
 *
 * Purpose:	Verify that flush_cache behaves as expected.  In particular,
 *		test the behaviour with different flags.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              1/10/05
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_flush_cache(void)
{
    const char * fcn_name = "check_flush_cache";
    H5C_t *      cache_ptr = NULL;

    TESTING("H5C_flush_cache() functionality");

    pass = TRUE;

    /* allocate a cache, and flush it under various circumstances.
     * To the extent possible, verify that the desired actions took 
     * place.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024 * 1024),
                                (size_t)(1 * 1024 * 1024));
    }

    /* first test behaviour on an empty cache.  Can't do much sanity
     * checking in this case, so simply check the return values.
     */

    if ( pass ) {

        check_flush_cache__empty_cache(cache_ptr);
    }

    /* now do a series of similar tests with a cache with a single entry.
     * Start with a clean entry, with no flags set.
     */

    if ( pass ) {

        check_flush_cache__single_entry(cache_ptr);
    }

    if ( pass ) {

        check_flush_cache__multi_entry(cache_ptr);
    }

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n", 
                  fcn_name, failure_mssg);

} /* check_flush_cache() */


/*-------------------------------------------------------------------------
 * Function:	check_flush_cache__empty_cache()
 *
 * Purpose:	Verify that flush_cache behaves as expected with an empty
 *              cache.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              1/12/05
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_flush_cache__empty_cache(H5C_t * cache_ptr)
{
    /* const char * fcn_name = "check_flush_cache__empty_cache"; */
    herr_t	 result;

    if ( cache_ptr == NULL ) {

        pass = FALSE;
        failure_mssg = "cache_ptr NULL on entry to empty cache case.";
    }
    else if ( ( cache_ptr->index_len != 0 ) ||
              ( cache_ptr->index_size != 0 ) ) {

        pass = FALSE;
        failure_mssg = "cache not empty at beginning of empty cache case.";
    }


    /* Test behaviour on an empty cache.  Can't do much sanity
     * checking in this case, so simply check the return values.
     */

    if ( pass ) {

        result = H5C_flush_cache(NULL, -1, -1, cache_ptr, H5C__NO_FLAGS_SET);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "flush with flags = 0x00 failed on empty cache.\n";
        }
    }

    if ( pass ) {

        result = H5C_flush_cache(NULL, -1, -1, cache_ptr, 
                                 H5C__FLUSH_INVALIDATE_FLAG);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "flush with flags = 0x04 failed on empty cache.\n";
        }
    }

    if ( pass ) {

        result = H5C_flush_cache(NULL, -1, -1, cache_ptr, 
                                 H5C__FLUSH_CLEAR_ONLY_FLAG);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "flush with flags = 0x08 failed on empty cache.\n";
        }
    }


    if ( pass ) {

        result = H5C_flush_cache(NULL, -1, -1, cache_ptr, 
                                 H5C__FLUSH_MARKED_ENTRIES_FLAG);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "flush with flags = 0x10 failed on empty cache.\n";
        }
    }

} /* check_flush_cache__empty_cache() */


/*-------------------------------------------------------------------------
 * Function:	check_flush_cache__multi_entry()
 *
 * Purpose:	Verify that flush_cache behaves as expected when the cache
 *		contains multiple elements.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              1/14/05
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_flush_cache__multi_entry(H5C_t * cache_ptr)
{
    /* const char * fcn_name = "check_flush_cache__multi_entry"; */

    if ( cache_ptr == NULL ) {

        pass = FALSE;
        failure_mssg = "cache_ptr NULL on entry to multi entry case.";
    }
    else if ( ( cache_ptr->index_len != 0 ) ||
              ( cache_ptr->index_size != 0 ) ) {

        pass = FALSE;
        failure_mssg = "cache not empty at beginning of multi entry case.";
    }

    {
        int test_num                         = 1;
        unsigned int flush_flags             = H5C__NO_FLAGS_SET;
        int spec_size                        = 8;
        struct flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num          = */ 0,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 100,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 1,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 75,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 2,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 25,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 3,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 50,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 4,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 10,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 5,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 20,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 6,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 30,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 7,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 40,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          }
        };

        check_flush_cache__multi_entry_test(cache_ptr, test_num,
                                            flush_flags, spec_size, spec);
    }


    {
        int test_num                         = 2;
        unsigned int flush_flags             = H5C__FLUSH_INVALIDATE_FLAG;
        int spec_size                        = 8;
        struct flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num          = */ 0,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 100,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 1,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 75,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 2,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 25,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 3,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 50,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 4,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 10,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 5,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 20,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 6,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 30,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 7,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 40,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          }
        };

        check_flush_cache__multi_entry_test(cache_ptr, test_num,
                                            flush_flags, spec_size, spec);
    }


    {
        int test_num                         = 3;
        unsigned int flush_flags             = H5C__FLUSH_CLEAR_ONLY_FLAG;
        int spec_size                        = 8;
        struct flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num          = */ 0,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 100,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 1,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 75,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 2,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 25,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 3,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 50,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 4,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 10,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 5,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 20,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 6,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 30,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 7,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 40,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          }
        };

        check_flush_cache__multi_entry_test(cache_ptr, test_num,
                                            flush_flags, spec_size, spec);
    }


    {
        int test_num                         = 4;
        unsigned int flush_flags             = H5C__FLUSH_MARKED_ENTRIES_FLAG;
        int spec_size                        = 8;
        struct flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num          = */ 0,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 100,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 1,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 75,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 2,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 25,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 3,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 50,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 4,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 10,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 5,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 20,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 6,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 30,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 7,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 40,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          }
        };

        check_flush_cache__multi_entry_test(cache_ptr, test_num,
                                            flush_flags, spec_size, spec);
    }


    {
        int test_num                         = 5;
        unsigned int flush_flags             = H5C__FLUSH_INVALIDATE_FLAG |
                                               H5C__FLUSH_CLEAR_ONLY_FLAG;
        int spec_size                        = 8;
        struct flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num          = */ 0,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 100,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 1,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 75,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 2,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 25,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 3,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 50,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 4,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 10,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 5,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 20,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 6,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 30,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 7,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 40,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          }
        };

        check_flush_cache__multi_entry_test(cache_ptr, test_num,
                                            flush_flags, spec_size, spec);
    }


    {
        int test_num                         = 6;
        unsigned int flush_flags             = H5C__FLUSH_INVALIDATE_FLAG |
                                               H5C__FLUSH_MARKED_ENTRIES_FLAG;
        int spec_size                        = 8;
        struct flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num          = */ 0,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 100,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 1,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 75,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 2,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 25,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 3,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 50,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 4,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 10,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 5,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 20,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 6,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 30,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 7,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 40,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          }
        };

        check_flush_cache__multi_entry_test(cache_ptr, test_num,
                                            flush_flags, spec_size, spec);
    }


    {
        int test_num                         = 7;
        unsigned int flush_flags             = H5C__FLUSH_CLEAR_ONLY_FLAG |
                                               H5C__FLUSH_MARKED_ENTRIES_FLAG;
        int spec_size                        = 8;
        struct flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num          = */ 0,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 100,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 1,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 75,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 2,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 25,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 3,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 50,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 4,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 10,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 5,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 20,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 6,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 30,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 7,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 40,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          }
        };

        check_flush_cache__multi_entry_test(cache_ptr, test_num,
                                            flush_flags, spec_size, spec);
    }


    {
        int test_num                         = 8;
        unsigned int flush_flags             = H5C__FLUSH_INVALIDATE_FLAG |
                                               H5C__FLUSH_CLEAR_ONLY_FLAG |
                                               H5C__FLUSH_MARKED_ENTRIES_FLAG;
        int spec_size                        = 8;
        struct flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num          = */ 0,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 100,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 1,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 75,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 2,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 25,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 3,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 50,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 4,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 10,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 5,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 20,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 6,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 30,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 7,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 40,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          }
        };

        check_flush_cache__multi_entry_test(cache_ptr, test_num,
                                            flush_flags, spec_size, spec);
    }


    /* verify that all other flags are ignored */
    {
        int test_num                         = 9;
        unsigned int flush_flags             = (unsigned)
                                               ~(H5C__FLUSH_INVALIDATE_FLAG |
                                                H5C__FLUSH_CLEAR_ONLY_FLAG |
                                                H5C__FLUSH_MARKED_ENTRIES_FLAG);
        int spec_size                        = 8;
        struct flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num          = */ 0,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 100,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 1,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 75,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 2,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 25,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 3,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 50,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 4,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 10,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 5,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 20,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 6,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 30,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 7,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 40,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          }
        };

        check_flush_cache__multi_entry_test(cache_ptr, test_num,
                                            flush_flags, spec_size, spec);
    }

} /* check_flush_cache__multi_entry() */


/*-------------------------------------------------------------------------
 * Function:	check_flush_cache__multi_entry_test()
 *
 * Purpose:	Run a multi entry flush cache test.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              1/13/05
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_flush_cache__multi_entry_test(H5C_t * cache_ptr,
                                    int test_num,
                                    unsigned int flush_flags,
                                    int spec_size,
                                    struct flush_cache_test_spec spec[])
{
    /* const char *   fcn_name = "check_flush_cache__multi_entry_test"; */
    static char    msg[128];
    herr_t	   result;
    int            i;
    size_t	   total_entry_size = 0;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    if ( cache_ptr == NULL ) {

        pass = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "cache_ptr NULL on entry to single entry test #%d.",
                   test_num);
        failure_mssg = msg;
    }
    else if ( ( cache_ptr->index_len != 0 ) ||
              ( cache_ptr->index_size != 0 ) ) {

        pass = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "cache not empty at beginning of multi entry test #%d.",
                   test_num);
        failure_mssg = msg;
    }
    else if ( ( spec_size < 1 ) || ( spec == NULL ) ) {

        pass = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "missing/bad test spec on entry to multi entry test #%d.",
                   test_num);
        failure_mssg = msg;
    }

    i = 0;
    while ( ( pass ) && ( i < spec_size ) )
    {
        if ( ( spec[i].entry_num != i ) ||
             ( spec[i].entry_type < 0 ) ||
             ( spec[i].entry_type >= NUMBER_OF_ENTRY_TYPES ) ||
             ( spec[i].entry_index < 0 ) ||
             ( spec[i].entry_index > max_indices[spec[i].entry_type] ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "bad data in spec[%d] on entry to multi entry test #%d.",
                       i, test_num);
            failure_mssg = msg;
        }
        i++;
    }

    i = 0;
    while ( ( pass ) && ( i < spec_size ) )
    {
        if ( spec[i].insert_flag ) {

            insert_entry(cache_ptr, spec[i].entry_type, spec[i].entry_index, 
                         spec[i].dirty_flag, spec[i].flags);

        } else {

            protect_entry(cache_ptr, spec[i].entry_type, spec[i].entry_index);

            unprotect_entry(cache_ptr, spec[i].entry_type, spec[i].entry_index,
                            (int)(spec[i].dirty_flag), spec[i].flags);
        }

        total_entry_size += entry_sizes[spec[i].entry_type];
    
        i++;
    }

    if ( pass ) {

        result = H5C_flush_cache(NULL, -1, -1, cache_ptr, flush_flags);

        if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "flush with flags 0x%x failed in multi entry test #%d.",
                       flush_flags, test_num);
            failure_mssg = msg;
        }
    }

    i = 0;
    while ( ( pass ) && ( i < spec_size ) )
    {
        base_addr = entries[spec[i].entry_type];
        entry_ptr = &(base_addr[spec[i].entry_index]);

        if ( ( entry_ptr->loaded != spec[i].expected_loaded ) ||
             ( entry_ptr->cleared != spec[i].expected_cleared ) ||
             ( entry_ptr->flushed != spec[i].expected_flushed ) ||
             ( entry_ptr->destroyed != spec[i].expected_destroyed ) ) {

#if 0 /* This is useful debugging code.  Lets keep it around. */

            HDfprintf(stdout, 
              "loaded = %d(%d), clrd = %d(%d), flshd = %d(%d), dest = %d(%d)\n",
              (int)(entry_ptr->loaded),
              (int)(spec[i].expected_loaded),
              (int)(entry_ptr->cleared),
              (int)(spec[i].expected_cleared),
              (int)(entry_ptr->flushed),
              (int)(spec[i].expected_flushed),
              (int)(entry_ptr->destroyed),
              (int)(spec[i].expected_destroyed));

#endif

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                "Bad status on entry %d after flush in multi entry test #%d.",
                i, test_num);
            failure_mssg = msg;
        }
        i++;
    }

    if ( pass ) {

        if ( ( ( (flush_flags & H5C__FLUSH_INVALIDATE_FLAG) == 0 ) 
               &&
               ( ( cache_ptr->index_len != spec_size ) 
                 ||
                 ( cache_ptr->index_size != total_entry_size ) 
               )
             )
             || 
             ( ( (flush_flags & H5C__FLUSH_INVALIDATE_FLAG) != 0 )
               &&
               ( ( cache_ptr->index_len != 0 )
                 ||
                 ( cache_ptr->index_size != 0 )
               )
             )
           ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
              "Unexpected cache len/size after flush in multi entry test #%d.",
              test_num);
            failure_mssg = msg;
        }
    }

    /* clean up the cache to prep for the next test */ 
    if ( pass ) {

        result = H5C_flush_cache(NULL, -1, -1, cache_ptr, 
                                 H5C__FLUSH_INVALIDATE_FLAG);

        if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "Flush failed on cleanup in multi entry test #%d.",
                       test_num);
            failure_mssg = msg;
        }
        else if ( ( cache_ptr->index_len != 0 ) ||
                  ( cache_ptr->index_size != 0 ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
            "Unexpected cache len/size after cleanup in multi entry test #%d.",
            test_num);
            failure_mssg = msg;

        } 
    }

    i = 0;
    while ( ( pass ) && ( i < spec_size ) )
    {
        base_addr = entries[spec[i].entry_type];
        entry_ptr = &(base_addr[spec[i].entry_index]);

        entry_ptr->loaded    = FALSE;
        entry_ptr->cleared   = FALSE;
        entry_ptr->flushed   = FALSE;
        entry_ptr->destroyed = FALSE;

        i++;
    }

} /* check_flush_cache__multi_entry_test() */


/*-------------------------------------------------------------------------
 * Function:	check_flush_cache__single_entry()
 *
 * Purpose:	Verify that flush_cache behaves as expected when the cache
 *		contains only one element.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              1/12/05
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_flush_cache__single_entry(H5C_t * cache_ptr)
{
    /* const char * fcn_name = "check_flush_cache__single_entry"; */

    if ( cache_ptr == NULL ) {

        pass = FALSE;
        failure_mssg = "cache_ptr NULL on entry to single entry case.";
    }
    else if ( ( cache_ptr->index_len != 0 ) ||
              ( cache_ptr->index_size != 0 ) ) {

        pass = FALSE;
        failure_mssg = "cache not empty at beginning of single entry case.";
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 1,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__NO_FLAGS_SET,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 2,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__NO_FLAGS_SET,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 3,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 4,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 5,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 6,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 7,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 8,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 9,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG | 
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 10,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 11,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 12,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 13,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 14,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 15,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG | 
                                       H5C__FLUSH_CLEAR_ONLY_FLAG |
                                       H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 16,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG |
                                       H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 17,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__NO_FLAGS_SET,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 18,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__NO_FLAGS_SET,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 19,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 20,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 21,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 22,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 23,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 24,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 25,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG | 
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 26,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 27,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 28,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 29,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 30,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 31,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG | 
                                       H5C__FLUSH_CLEAR_ONLY_FLAG |
                                       H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 32,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG |
                                       H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 33,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__NO_FLAGS_SET,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 34,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__NO_FLAGS_SET,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 35,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 36,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 37,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 38,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 39,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 40,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 41,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG | 
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 42,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 43,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 44,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 45,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 46,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 47,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG | 
                                       H5C__FLUSH_CLEAR_ONLY_FLAG |
                                       H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 48,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG |
                                       H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 49,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__NO_FLAGS_SET,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 50,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__NO_FLAGS_SET,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 51,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 52,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 53,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 54,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 55,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 56,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 57,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG | 
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 58,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 59,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 60,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 61,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 62,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 63,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG | 
                                       H5C__FLUSH_CLEAR_ONLY_FLAG |
                                       H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 64,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG |
                                       H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

} /* check_flush_cache__single_entry() */


/*-------------------------------------------------------------------------
 * Function:	check_flush_cache__single_entry_test()
 *
 * Purpose:	Run a single entry flush cache test.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              1/12/05
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_flush_cache__single_entry_test(H5C_t * cache_ptr,
                                     int test_num,
                                     int entry_type,
                                     int entry_idx,
                                     hbool_t insert_flag,
                                     hbool_t dirty_flag,
                                     unsigned int flags,
                                     unsigned int flush_flags,
                                     hbool_t expected_loaded,
                                     hbool_t expected_cleared,
                                     hbool_t expected_flushed,
                                     hbool_t expected_destroyed)
{
    /* const char *   fcn_name = "check_flush_cache__single_entry_test"; */
    static char    msg[128];
    herr_t	   result;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    if ( cache_ptr == NULL ) {

        pass = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "cache_ptr NULL on entry to single entry test #%d.",
                   test_num);
        failure_mssg = msg;
    }
    else if ( ( cache_ptr->index_len != 0 ) ||
              ( cache_ptr->index_size != 0 ) ) {

        pass = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "cache not empty at beginning of single entry test #%d.",
                   test_num);
        failure_mssg = msg;
    }
    else if ( ( entry_type < 0 ) || ( entry_type >= NUMBER_OF_ENTRY_TYPES ) ||
              ( entry_idx < 0 ) || ( entry_idx > max_indices[entry_type] ) ) {

        pass = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "Bad parameters on entry to single entry test #%d.",
                   test_num);
        failure_mssg = msg;
    }

    if ( pass ) {

        base_addr = entries[entry_type];
        entry_ptr = &(base_addr[entry_idx]);

        if ( insert_flag ) {

            insert_entry(cache_ptr, entry_type, entry_idx, dirty_flag, flags);

        } else {

            protect_entry(cache_ptr, entry_type, entry_idx);

            unprotect_entry(cache_ptr, entry_type, entry_idx, 
                            (int)dirty_flag, flags);
        }
    }

    if ( pass ) {

        result = H5C_flush_cache(NULL, -1, -1, cache_ptr, flush_flags);

        if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "flush with flags 0x%x failed in single entry test #%d.",
                       flush_flags, test_num);
            failure_mssg = msg;
        }
        else if ( ( entry_ptr->loaded != expected_loaded ) ||
                  ( entry_ptr->cleared != expected_cleared ) ||
                  ( entry_ptr->flushed != expected_flushed ) ||
                  ( entry_ptr->destroyed != expected_destroyed ) ) {

            HDfprintf(stdout, 
              "loaded = %d(%d), clrd = %d(%d), flshd = %d(%d), dest = %d(%d)\n",
              (int)(entry_ptr->loaded),
              (int)expected_loaded,
              (int)(entry_ptr->cleared),
              (int)expected_cleared,
              (int)(entry_ptr->flushed),
              (int)expected_flushed,
              (int)(entry_ptr->destroyed),
              (int)expected_destroyed);

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                "Unexpected entry status after flush in single entry test #%d.",
                test_num);
            failure_mssg = msg;
        }
        else if ( ( ( (flush_flags & H5C__FLUSH_INVALIDATE_FLAG) == 0 ) 
                    &&
                    ( ( cache_ptr->index_len != 1 ) 
                      ||
                      ( cache_ptr->index_size != entry_sizes[entry_type] ) 
                    ) 
                  )
                  || 
                  ( ( (flush_flags & H5C__FLUSH_INVALIDATE_FLAG) != 0 )
                    &&
                    ( ( cache_ptr->index_len != 0 )
                      ||
                      ( cache_ptr->index_size != 0 )
                    )
                  )
                ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
              "Unexpected cache len/size after flush in single entry test #%d.",
              test_num);
            failure_mssg = msg;
        }
    }
  
  
    /* clean up the cache to prep for the next test */ 
    if ( pass ) {

        result = H5C_flush_cache(NULL, -1, -1, cache_ptr, 
                                 H5C__FLUSH_INVALIDATE_FLAG);

        if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "Flush failed on cleanup in single entry test #%d.",
                       test_num);
            failure_mssg = msg;
        }
        else if ( ( cache_ptr->index_len != 0 ) ||
                  ( cache_ptr->index_size != 0 ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
            "Unexpected cache len/size after cleanup in single entry test #%d.",
            test_num);
            failure_mssg = msg;

        } else {

            entry_ptr->loaded    = FALSE;
            entry_ptr->cleared   = FALSE;
            entry_ptr->flushed   = FALSE;
            entry_ptr->destroyed = FALSE;
        }
    }
} /* check_flush_cache__single_entry_test() */


/*-------------------------------------------------------------------------
 * Function:	check_flush_protected_err()
 *
 * Purpose:	Verify that an attempt to flush the cache when it contains
 *		a protected entry will generate an error.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_flush_protected_err(void)
{
    const char * fcn_name = "check_flush_protected_err";
    H5C_t * cache_ptr = NULL;

    TESTING("flush cache with protected entry error");

    pass = TRUE;

    /* allocate a cache, protect an entry, and try to flush.  This
     * should fail.  Unprotect the entry and flush again -- should
     * succeed.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry(cache_ptr, 0, 0);

        if ( H5C_flush_cache(NULL, -1, -1, cache_ptr, H5C__NO_FLAGS_SET)
             >= 0 ) {

            pass = FALSE;
            failure_mssg = "flush succeeded on cache with protected entry.\n";

        } else {

            unprotect_entry(cache_ptr, 0, 0, TRUE, H5C__NO_FLAGS_SET);

            if ( H5C_flush_cache(NULL, -1, -1, cache_ptr, H5C__NO_FLAGS_SET) 
                 < 0 ) {

                pass = FALSE;
                failure_mssg = "flush failed after unprotect.\n";

            } else {

                takedown_cache(cache_ptr, FALSE, FALSE);
            }
        }
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n", 
                  fcn_name, failure_mssg);

} /* check_flush_protected_err() */


/*-------------------------------------------------------------------------
 * Function:	check_destroy_protected_err()
 *
 * Purpose:	Verify that an attempt to destroy the cache when it contains
 *		a protected entry will generate an error.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_destroy_protected_err(void)
{
    const char * fcn_name = "check_destroy_protected_err";
    H5C_t * cache_ptr = NULL;

    TESTING("destroy cache with protected entry error");

    pass = TRUE;

    /* allocate a cache, protect an entry, and try to flush.  This
     * should fail.  Unprotect the entry and flush again -- should
     * succeed.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry(cache_ptr, 0, 0);

        if ( H5C_dest(NULL, -1, -1, cache_ptr) >= 0 ) {

            pass = FALSE;
            failure_mssg = "destroy succeeded on cache with protected entry.\n";

        } else {

            unprotect_entry(cache_ptr, 0, 0, TRUE, H5C__NO_FLAGS_SET);

            if ( H5C_dest(NULL, -1, -1, cache_ptr) < 0 ) {

                pass = FALSE;
                failure_mssg = "destroy failed after unprotect.\n";

            } 
        }
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n", 
                  fcn_name, failure_mssg);

} /* check_destroy_protected_err() */


/*-------------------------------------------------------------------------
 * Function:	check_duplicate_insert_err()
 *
 * Purpose:	Verify that an attempt to insert and entry that is 
 *		alread in the cache will generate an error.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_duplicate_insert_err(void)
{
    const char * fcn_name = "check_duplicate_insert_err";
    herr_t result;
    H5C_t * cache_ptr = NULL;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    TESTING("duplicate entry insertion error");

    pass = TRUE;

    /* allocate a cache, protect an entry, and then try to insert
     * the entry again.  This should fail.  Unprotect the entry and 
     * destroy the cache -- should succeed.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry(cache_ptr, 0, 0);

        if ( pass ) {

            base_addr = entries[0];
            entry_ptr = &(base_addr[0]);

            result = H5C_insert_entry(NULL, -1, -1, cache_ptr, 
                                      &(types[0]), entry_ptr->addr,
                                      (void *)entry_ptr, H5C__NO_FLAGS_SET);

            if ( result >= 0 ) {

                pass = FALSE;
                failure_mssg = "insert of duplicate entry succeeded.\n";

            } else {

                unprotect_entry(cache_ptr, 0, 0, TRUE, H5C__NO_FLAGS_SET);

                takedown_cache(cache_ptr, FALSE, FALSE);
            }
        }
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n", 
                  fcn_name, failure_mssg);

} /* check_duplicate_insert_err() */


/*-------------------------------------------------------------------------
 * Function:	check_rename_err()
 *
 * Purpose:	Verify that an attempt to rename an entry to the address
 *		of an existing entry will generate an error.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_rename_err(void)
{
    const char * fcn_name = "check_rename_err()";
    herr_t result;
    H5C_t * cache_ptr = NULL;
    test_entry_t * entry_0_0_ptr;
    test_entry_t * entry_0_1_ptr;
    test_entry_t * entry_1_0_ptr;

    TESTING("rename to existing entry errors");

    pass = TRUE;

    /* allocate a cache, and insert several entries.  Try to rename
     * entries to other entries resident in the cache.  This should
     * fail.  Destroy the cache -- should succeed.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        insert_entry(cache_ptr, 0, 0, TRUE, H5C__NO_FLAGS_SET);
        insert_entry(cache_ptr, 0, 1, TRUE, H5C__NO_FLAGS_SET);
        insert_entry(cache_ptr, 1, 0, TRUE, H5C__NO_FLAGS_SET);

        entry_0_0_ptr = &((entries[0])[0]);
        entry_0_1_ptr = &((entries[0])[1]);
        entry_1_0_ptr = &((entries[1])[0]);
    }

    if ( pass ) {

        result = H5C_rename_entry(cache_ptr, &(types[0]),
                                  entry_0_0_ptr->addr, entry_0_1_ptr->addr);

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "rename to addr of same type succeeded.\n";
        }
    }

    if ( pass ) {

        result = H5C_rename_entry(cache_ptr, &(types[0]),
                                  entry_0_0_ptr->addr, entry_1_0_ptr->addr);

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "rename to addr of different type succeeded.\n";
        }
    }

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n", 
                  fcn_name, failure_mssg);

} /* check_rename_err() */


/*-------------------------------------------------------------------------
 * Function:	check_double_protect_err()
 *
 * Purpose:	Verify that an attempt to protect an entry that is already
 *		protected will generate an error.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_double_protect_err(void)
{
    const char * fcn_name = "check_double_protect_err()";
    H5C_t * cache_ptr = NULL;
    test_entry_t * entry_ptr;
    H5C_cache_entry_t * cache_entry_ptr;

    TESTING("protect a protected entry error");

    pass = TRUE;

    /* allocate a cache, protect an entry, and then try to protect
     * the entry again.  This should fail.  Unprotect the entry and 
     * destroy the cache -- should succeed.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry(cache_ptr, 0, 0);

        entry_ptr = &((entries[0])[0]);
    }

    if ( pass ) {

        cache_entry_ptr = H5C_protect(NULL, -1, -1, cache_ptr, &(types[0]),
                                      entry_ptr->addr, NULL, NULL);

        if ( cache_entry_ptr != NULL ) {

            pass = FALSE;
            failure_mssg = "attempt to protect a protected entry succeeded.\n";
        }
    }

    if ( pass ) {

        unprotect_entry(cache_ptr, 0, 0, FALSE, H5C__NO_FLAGS_SET);
    }

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n", 
                  fcn_name, failure_mssg);

} /* check_double_protect_err() */


/*-------------------------------------------------------------------------
 * Function:	check_double_unprotect_err()
 *
 * Purpose:	Verify that an attempt to unprotect an entry that is already
 *		unprotected will generate an error.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_double_unprotect_err(void)
{
    const char * fcn_name = "check_double_unprotect_err()";
    herr_t result;
    H5C_t * cache_ptr = NULL;
    test_entry_t * entry_ptr;

    TESTING("unprotect an unprotected entry error");

    pass = TRUE;

    /* allocate a cache, protect an entry, unprotect it, and then try to 
     * unprotect the entry again.  This should fail.  Destroy the cache 
     * -- should succeed.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry(cache_ptr, 0, 0);

        unprotect_entry(cache_ptr, 0, 0, FALSE, H5C__NO_FLAGS_SET);

        entry_ptr = &((entries[0])[0]);
    }

    if ( pass ) {

        result = H5C_unprotect(NULL, -1, -1, cache_ptr, &(types[0]),
                               entry_ptr->addr, (void *)entry_ptr, 
                               H5C__NO_FLAGS_SET);

        if ( result > 0 ) {

            pass = FALSE;
            failure_mssg = 
                "attempt to unprotect an unprotected entry succeeded 1.\n";
        }
    }

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n", 
                  fcn_name, failure_mssg);

} /* check_double_unprotect_err() */


/*-------------------------------------------------------------------------
 * Function:	check_auto_cache_resize()
 *
 * Purpose:	Exercise the automatic cache resizing functionality.
 *		The objective is to operate the auto-resize code in 
 *		all possible modes.  Unfortunately, there are quite 
 *		a few of them.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              10/29/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

hbool_t rpt_fcn_called = FALSE;
enum H5C_resize_status rpt_status;

void test_rpt_fcn(UNUSED H5C_t * cache_ptr,
                  UNUSED int32_t version,
                  UNUSED double hit_rate,
                  UNUSED enum H5C_resize_status status,
                  UNUSED size_t old_max_cache_size,
                  UNUSED size_t new_max_cache_size,
                  UNUSED size_t old_min_clean_size,
                  UNUSED size_t new_min_clean_size);

void test_rpt_fcn(UNUSED H5C_t * cache_ptr,
                  UNUSED int32_t version,
                  UNUSED double hit_rate,
                  UNUSED enum H5C_resize_status status,
                  UNUSED size_t old_max_cache_size,
                  UNUSED size_t new_max_cache_size,
                  UNUSED size_t old_min_clean_size,
                  UNUSED size_t new_min_clean_size)
{
    rpt_fcn_called = TRUE;
    rpt_status = status;
}

static void
check_auto_cache_resize(void)
{
    const char * fcn_name = "check_auto_cache_resize()";
    hbool_t show_progress = FALSE;
    herr_t result;
    int32_t i;
    int32_t checkpoint = 0;
    H5C_t * cache_ptr = NULL;
    H5C_auto_size_ctl_t auto_size_ctl =
    {
        /* int32_t     version                = */ H5C__CURR_AUTO_SIZE_CTL_VER,
        /* H5C_auto_resize_report_fcn rpt_fcn = */ test_rpt_fcn,

        /* hbool_t     set_initial_size       = */ TRUE,
        /* size_t      initial_size           = */ (512 * 1024),

        /* double      min_clean_fraction     = */ 0.5,

        /* size_t      max_size               = */ (14 * 1024 * 1024),
        /* size_t      min_size               = */ (512 * 1024),

        /* int64_t     epoch_length           = */ 1000,


        /* enum H5C_cache_incr_mode incr_mode = */ H5C_incr__threshold,

        /* double     lower_hr_threshold      = */ 0.75,

        /* double      increment              = */ 2.0,

        /* hbool_t     apply_max_increment    = */ TRUE,
        /* size_t      max_increment          = */ (4 * 1024 * 1024),


        /* enum H5C_cache_decr_mode decr_mode = */ H5C_decr__threshold,

        /* double      upper_hr_threshold     = */ 0.995,

        /* double      decrement              = */ 0.1,

        /* hbool_t     apply_max_decrement    = */ TRUE,
        /* size_t      max_decrement          = */ (1 * 1024 * 1024),

        /* int32_t     epochs_before_eviction = */ 3,

        /* hbool_t     apply_empty_reserve    = */ TRUE,
        /* double      empty_reserve          = */ 0.05
    };

    TESTING("automatic cache resizing");

    pass = TRUE;

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* allocate a cache, enable automatic cache resizing, and then force 
     * the cache through all its operational modes.  Verify that all
     * performs as expected.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));
    }

    if ( pass ) {

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 1.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after initialization.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache not full -- should result in not 
     * full status.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, PICO_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, PICO_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != not_full ) ||
             ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 1.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full -- should result in increase
     * of cache size from .5 to 1 meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (1 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (512 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 2.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache not full -- should result in not 
     * full status.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, PICO_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, PICO_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != not_full ) ||
             ( cache_ptr->max_cache_size != (1 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (512 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 3.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full again -- should result in increase
     * of cache size from 1 to 2 meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 4.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full again -- should result in increase
     * of cache size from 2 to 4 meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 5.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full again -- should result in increase
     * of cache size from 4 to 8 meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 6.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full again -- should result in increase
     * of cache size from 8 to 12 meg.  Note that max increase reduced the
     * size of the increase.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (12 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (6 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 7.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full again -- should result in increase
     * of cache size from 12 to 14 meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (14 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (7 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 8.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full and at maximum size -- should 
     * in no change in size and a result of at_max_size.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (14 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (7 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 9.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate with cache full and at maximum size -- should 
     * result in a decrease from 14 to 13 Meg -- note that max decrease
     * reduced the size of the reduction
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (13 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (6 * 1024 * 1024 + 512 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 10.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* the current cache configuration is unconvenient for testing cache
     * size reduction, so lets change it some something easier to work 
     * with.
     */

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1000 * 1000 + 10;

        auto_size_ctl.min_clean_fraction     = 0.1;

        auto_size_ctl.max_size               = 8 * 1000 * 1000;
        auto_size_ctl.min_size               = 500 * 1000;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1000 * 1000);


        auto_size_ctl.decr_mode              = H5C_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1000 * 1000);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 2.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1000 * 1000 + 10) ) ||
             ( cache_ptr->min_clean_size != (400 * 1000 + 1) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 1.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate  -- should result in a decrease from ~4 to ~3 
     * M -- note that max decrease reduces the size of the reduction
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (3 * 1000 * 1000 + 10) ) ||
             ( cache_ptr->min_clean_size != (300 * 1000 + 1) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 11.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate again  -- should result in a decrease from ~3 
     * to ~2 M -- again note that max decrease reduces the size of the 
     * reduction.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2 * 1000 * 1000 + 10) ) ||
             ( cache_ptr->min_clean_size != (200 * 1000 + 1) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 12.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate again  -- should result in a decrease from ~2 
     * to ~1 M -- again note that max decrease reduces the size of the 
     * reduction, but only by five bites.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (1 * 1000 * 1000 + 10) ) ||
             ( cache_ptr->min_clean_size != (100 * 1000 + 1) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 13.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate again  -- should result in a decrease from ~1 
     * to ~0.5 M -- max decrease is no longer a factor.  New size is five
     * bytes above the minimum.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (500 * 1000 + 5) ) ||
             ( cache_ptr->min_clean_size != (50 * 1000) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 14.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate again  -- should result in a decrease of five
     * bytes to the minimum cache size.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (500 * 1000) ) ||
             ( cache_ptr->min_clean_size != (50 * 1000) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 15.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate again -- Already at minimum size so no change in
     * cache size and result should be at_min_size.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_min_size ) ||
             ( cache_ptr->max_cache_size != (500 * 1000) ) ||
             ( cache_ptr->min_clean_size != (50 * 1000) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 16.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force in range hit rate  -- should be no change in cache size, 
     * and result should be in_spec.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 900 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        while ( ( pass ) && ( i < 1000 ) ) 
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i + 1000);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i + 1000, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (500 * 1000) ) ||
             ( cache_ptr->min_clean_size != (50 * 1000) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 17.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full -- should 
     * increase cache size from .5 to 1 M.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (1 * 1000 * 1000) ) ||
             ( cache_ptr->min_clean_size != (100 * 1000) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 18.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should result in a decrease to the 
     * minimum cache size.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (500 * 1000) ) ||
             ( cache_ptr->min_clean_size != (50 * 1000) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 19.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /******************************************************************
     * now do some tests with the maximum increase and decrease sizes
     * disabled.
     ******************************************************************/

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 4.0;

        auto_size_ctl.apply_max_increment    = FALSE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.25;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 3.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 2.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should result in a decrease to the 
     * minimum cache size.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (1 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (512 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 20.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full -- should increase cache size 
     * from 1 to 4 Meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 21.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate again with cache full -- should increase cache 
     * size from 4 to 16 Meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (16 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != ( 8 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 22.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should result in a decrease cache size from
     * 16 to 4 Meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 23.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /******************************************************************
     * We have tested the threshold increment and decrement modes.  
     * must now test the ageout decrement mode.
     *
     * Reconfigure the cache for this testing.
     ******************************************************************/

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1024 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__age_out;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = FALSE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 4.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 3.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /* fill the cache with 1024 byte entries -- nothing should happen
     * for three epochs while the markers are inserted into the cache
     *
     * Note that hit rate will be zero, so the cache will attempt to
     * increase its size. Since we are already at max size, it will 
     * not be able to.
     */
    if ( pass ) { /* first epoch */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 24.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* second epoch */

        rpt_fcn_called = FALSE;
        i = 1000;
        while ( ( pass ) && ( i < 2000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 25.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* third epoch */

        rpt_fcn_called = FALSE;
        i = 2000;
        while ( ( pass ) && ( i < 3000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 26.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fourth epoch -- If the hit rate were above the lower threshold,
     * we would see cache size reduction now.  However, nothing will 
     * happen until we get the hit rate above the lower threshold.
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 27.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fifth epoch -- force the hit rate to 100%.  We should see cache size 
     * reduction now. 
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2001 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(2001 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 28.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* sixth epoch -- force the hit rate to 100% again.  
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (1001 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(1001 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 29.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* seventh epoch -- force the hit rate to 100% again.  
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(1000 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 30.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* eigth epoch -- force the hit rate to 100% again -- should be steady 
     * state.  
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(1000 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 31.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "*check point %d\n", checkpoint++);

    /* now just bang on one entry -- after three epochs, this should 
     * get all entries other than the one evicted, and the cache size
     * should be decreased to the minimum.
     */
    if ( pass ) { /* ninth epoch */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(1000 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 32.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* tenth epoch */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(1000 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 33.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* eleventh epoch -- cache size reduction */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ||
             ( cache_ptr->index_len != 2 ) ||
             ( cache_ptr->index_size != 
               MONSTER_ENTRY_SIZE + MEDIUM_ENTRY_SIZE ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 34.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* twelth epoch -- at minimum size so no more ageouts */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_min_size ) ||
             ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ||
             ( cache_ptr->index_len != 2 ) ||
             ( cache_ptr->index_size != 
               MONSTER_ENTRY_SIZE + MEDIUM_ENTRY_SIZE ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 35.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /* repeat the above test, but with max_decrement enabled to see
     * if that features works as it should.  Note that this will change
     * the structure of the test a bit.
     */

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1024 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__age_out;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = FALSE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 5.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 4.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /* fill the cache with 1024 byte entries -- nothing should happen
     * for three epochs while the markers are inserted into the cache
     *
     * Note that hit rate will be zero, so the cache will attempt to
     * increase its size. Since we are already at max size, it will 
     * not be able to.
     */
    if ( pass ) { /* first epoch */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 36.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* second epoch */

        rpt_fcn_called = FALSE;
        i = 1000;
        while ( ( pass ) && ( i < 2000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 37.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* third epoch */

        rpt_fcn_called = FALSE;
        i = 2000;
        while ( ( pass ) && ( i < 3000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 38.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fourth epoch -- If the hit rate were above the lower threshold,
     * we would see cache size reduction now.  However, nothing will 
     * happen until we get the hit rate above the lower threshold.
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 39.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fifth epoch -- force the hit rate to 100%.  We should see cache size 
     * reduction now. 
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (7 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (7 * 512 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 40.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* sixth epoch -- force the hit rate to 100% again.  
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 2000;
        while ( ( pass ) && ( i < 3000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (6 * 512 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 41.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* seventh epoch -- keep hit rate at 100%, and keep 2K entries active.
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (5 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (5 * 512 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 42.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* eigth epoch -- still 100% hit rate
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 2000;
        while ( ( pass ) && ( i < 3000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 512 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 43.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* ninth epoch --hit rate at 100%.
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (3 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 512 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 44.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* tenth epoch -- still 100% hit rate
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 2000;
        while ( ( pass ) && ( i < 3000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 512 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 45.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* eleventh epoch -- hit rate at 100% -- starting to stableize
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(2000 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 46.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* twelth epoch -- force the hit rate to 100% again -- should be steady 
     * state.  
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 2000;
        while ( ( pass ) && ( i < 3000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (2000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(2000 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 47.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* now just bang on one entry -- after three epochs, this should 
     * get all entries other than the one evicted, and the cache size
     * should be decreased to the minimum.
     */
    if ( pass ) { /* thirteenth epoch */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (2000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(2000 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 48.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* fourteenth epoch */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != 
               (1001 * 1024 + MONSTER_ENTRY_SIZE) ) ||
             ( cache_ptr->min_clean_size != 
               (1001 * 512 + MONSTER_ENTRY_SIZE / 2) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 49.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* fifteenth epoch -- cache size reduction */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ||
             ( cache_ptr->index_len != 2 ) ||
             ( cache_ptr->index_size != 
               MONSTER_ENTRY_SIZE + MEDIUM_ENTRY_SIZE ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 50.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* sixteenth epoch -- at minimum size so no more ageouts */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_min_size ) ||
             ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ||
             ( cache_ptr->index_len != 2 ) ||
             ( cache_ptr->index_size != 
               MONSTER_ENTRY_SIZE + MEDIUM_ENTRY_SIZE ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 51.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /* repeat the test yet again, this time with empty reserve enabled.
     * Again, some structural changes in the test are necessary.
     */

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1024 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__age_out;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.5; /* for ease of testing */

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 6.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 5.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /* fill the cache with 1024 byte entries -- nothing should happen
     * for three epochs while the markers are inserted into the cache
     *
     * Note that hit rate will be zero, so the cache will attempt to
     * increase its size. Since we are already at max size, it will 
     * not be able to.
     */
    if ( pass ) { /* first epoch */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 52.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* second epoch */

        rpt_fcn_called = FALSE;
        i = 1000;
        while ( ( pass ) && ( i < 2000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 53.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* third epoch */

        rpt_fcn_called = FALSE;
        i = 2000;
        while ( ( pass ) && ( i < 3000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 54.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fourth epoch -- If the hit rate were above the lower threshold,
     * we would see cache size reduction now.  However, nothing will 
     * happen until we get the hit rate above the lower threshold.
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 55.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fifth epoch -- force the hit rate to 100%.  We should see cache size 
     * reduction now. 
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (4002 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(4002 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 56.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* sixth epoch -- force the hit rate to 100% again.  
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2002 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(2002 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 57.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* seventh epoch -- force the hit rate to 100% again.  
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(2000 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 58.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* eigth epoch -- force the hit rate to 100% again -- should be steady 
     * state.  
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (2000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(2000 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 59.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* now just bang on one entry -- after three epochs, this should 
     * get all entries other than the one evicted, and the cache size
     * should be decreased to the minimum.
     */
    if ( pass ) { /* ninth epoch */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (2000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(2000 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 60.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* tenth epoch */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (2000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2000 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 61.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* eleventh epoch -- cache size reduction */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ||
             ( cache_ptr->index_len != 2 ) ||
             ( cache_ptr->index_size != 
               MONSTER_ENTRY_SIZE + MEDIUM_ENTRY_SIZE ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 62.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* twelth epoch -- at minimum size so no more ageouts */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_min_size ) ||
             ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ||
             ( cache_ptr->index_len != 2 ) ||
             ( cache_ptr->index_size != 
               MONSTER_ENTRY_SIZE + MEDIUM_ENTRY_SIZE ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 63.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /* Repeat the test again, this time using the age out with threshold
     * mode.  To simplify the testing, set epochs to eviction to 1.
     *
     * Again, there are some minor structural changes in the test.
     */

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1024 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__off;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__age_out_with_threshold;

        auto_size_ctl.upper_hr_threshold     = 0.999; /* for ease of testing */

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 1; /* for ease of testing */

        auto_size_ctl.apply_empty_reserve    = FALSE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 7.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 6.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /* fill the cache with 4K byte entries -- increment mode is off,
     * so cache size reduction should kick in as soon as we get the 
     * hit rate above .999.
     */
    if ( pass ) { /* first epoch -- hit rate 0 */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 64.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* second epoch -- hit rate 0 */

        rpt_fcn_called = FALSE;
        i = 1000;
        while ( ( pass ) && ( i < 2000 ) )
        {
            protect_entry(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 65.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* third epoch -- hit rate 1.0 -- should see decrease */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (1001 * LARGE_ENTRY_SIZE) ) ||
             ( cache_ptr->min_clean_size != (1001 * LARGE_ENTRY_SIZE / 2) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 66.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fourth epoch -- load up the cache again -- hit rate 0 */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (1001 * LARGE_ENTRY_SIZE) ) ||
             ( cache_ptr->min_clean_size != (1001 * LARGE_ENTRY_SIZE / 2) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 67.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fifth epoch -- still loading up the cache -- hit rate 0 */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 1000;
        while ( ( pass ) && ( i < 2000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (1001 * LARGE_ENTRY_SIZE) ) ||
             ( cache_ptr->min_clean_size != (1001 * LARGE_ENTRY_SIZE / 2) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 68.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* sixth epoch -- force hit rate to .998 -- should be no reduction */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 1002;
        while ( ( pass ) && ( i < 2002 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (1001 * LARGE_ENTRY_SIZE) ) ||
             ( cache_ptr->min_clean_size != (1001 * LARGE_ENTRY_SIZE / 2) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 69.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* seventh epoch -- force hit rate to .999 -- should see reduction 
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 1003;
        while ( ( pass ) && ( i < 2003 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (1000 * MEDIUM_ENTRY_SIZE) ) ||
             ( cache_ptr->min_clean_size != (1000 * MEDIUM_ENTRY_SIZE / 2) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 70.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /* We have now tested all the major ageout modes individually.
     * Lets try them all together to look for unexpected interactions
     * and/or bugs.
     */

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1000 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1000 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__age_out_with_threshold;

        auto_size_ctl.upper_hr_threshold     = 0.999; /* for ease of testing */

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1000 * 1024);

        auto_size_ctl.epochs_before_eviction = 1; /* for ease of testing */

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.5; /* for ease of testing */

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 8.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 7.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fill the cache with 4K byte entries -- increment mode is threshold,
     * so the decrease code will not be executed until the hit rate exceeds
     * .75.
     */
    if ( pass ) { /* first epoch -- hit rate 0 */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 71.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* second epoch -- hit rate 0 */

        rpt_fcn_called = FALSE;
        i = 1000;
        while ( ( pass ) && ( i < 2000 ) )
        {
            protect_entry(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 72.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* third epoch -- force the hit rate to 1.0.  Should be no change
     * in the cache size due to the combination of the empty reserve
     * and the max decrease.  Max decrease will limit the evictions
     * in any one epoch, and the empty reserve will not permit cache
     * size reduction unless the specified empty reserve is maintained.
     *
     * In this epoch, all we should see is a reduction in the index size.
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (7 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 73.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fourth epoch -- hit rate still 1.0.  Index size should decrease,
     * but otherwise no change expected.
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (6 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 74.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fifth epoch -- hit rate still 1.0.  Index size should decrease,
     * but otherwise no change expected.
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (5 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 75.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* sixth epoch -- hit rate still 1.0.  Index size should decrease,
     * but otherwise no change expected.  Note that the cache size is
     * now just on the edge of meeting the clean reserve.
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (4 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 76.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* seventh epoch -- hit rate still 1.0.  No change in index size expected.
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (4 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 77.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* eighth epoch -- start loading 1 KB entries.  Hit rate 0 so 
     * decrease code shouldn't be called.
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (5 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 78.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* ninth epoch -- access the 1 KB entries again, driving the hit rate
     * to 1.0.  Decrease code should be triggered, but the max decrease
     * should prevent the empty reserve from being met in this epoch.
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (4 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 79.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* tenth epoch -- access the 1 KB entries yet again, forcing hit rate
     * to 1.0.  Decrease code should be triggered, and the empty reserve
     * should finally be met.
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (7 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (7 * 1000 * 1024 / 2) ) ||
             ( cache_ptr->index_size != (3 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 80.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* eleventh epoch -- access the 1 KB entries yet again, forcing hit rate
     * to 1.0.  Decrease code should be triggered, and the empty reserve
     * should be met again.
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (6 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (2 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 81.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* twelth  epoch -- hit rate 1.0 -- decrease as before.
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (5 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (5 * 1000 * 1024 / 2) ) ||
             ( cache_ptr->index_size != (1 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 82.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* thirteenth  epoch -- hit rate 1.0 -- decrease as before.
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (4 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (1 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 83.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fourteenth  epoch -- hit rate 1.0 -- decrease as before.
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (3 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1000 * 1024 / 2) ) ||
             ( cache_ptr->index_size != (1 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 84.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fifteenth  epoch -- hit rate 1.0 -- decrease as before.
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (1 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 85.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* sixteenth  epoch -- hit rate 1.0 -- should be stable now
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (2 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (1 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 86.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n", 
                  fcn_name, failure_mssg);

} /* check_auto_cache_resize() */


/*-------------------------------------------------------------------------
 * Function:	check_auto_cache_resize_disable()
 *
 * Purpose:	Test the various ways in which the resize code can
 *		be disabled.  Unfortunately, there are quite a few of them.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              12/16/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_auto_cache_resize_disable(void)
{
    const char * fcn_name = "check_auto_cache_resize_disable()";
    hbool_t show_progress = FALSE;
    herr_t result;
    int32_t i;
    int32_t checkpoint = 0;
    H5C_t * cache_ptr = NULL;
    H5C_auto_size_ctl_t auto_size_ctl =
    {
        /* int32_t     version                = */ H5C__CURR_AUTO_SIZE_CTL_VER,
        /* H5C_auto_resize_report_fcn rpt_fcn = */ test_rpt_fcn,

        /* hbool_t     set_initial_size       = */ TRUE,
        /* size_t      initial_size           = */ (512 * 1024),

        /* double      min_clean_fraction     = */ 0.5,

        /* size_t      max_size               = */ (14 * 1024 * 1024),
        /* size_t      min_size               = */ (512 * 1024),

        /* int64_t     epoch_length           = */ 1000,


        /* enum H5C_cache_incr_mode incr_mode = */ H5C_incr__threshold,

        /* double     lower_hr_threshold      = */ 0.75,

        /* double      increment              = */ 2.0,

        /* hbool_t     apply_max_increment    = */ TRUE,
        /* size_t      max_increment          = */ (4 * 1024 * 1024),


        /* enum H5C_cache_decr_mode decr_mode = */ H5C_decr__threshold,

        /* double      upper_hr_threshold     = */ 0.995,

        /* double      decrement              = */ 0.1,

        /* hbool_t     apply_max_decrement    = */ TRUE,
        /* size_t      max_decrement          = */ (1 * 1024 * 1024),

        /* int32_t     epochs_before_eviction = */ 3,

        /* hbool_t     apply_empty_reserve    = */ TRUE,
        /* double      empty_reserve          = */ 0.05
    };

    TESTING("automatic cache resize disable");

    pass = TRUE;

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* allocate a cache, enable automatic cache resizing, and then force 
     * the cache through all its operational modes.  Verify that all
     * performs as expected.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));
    }

    if ( pass ) {

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 1.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after initialization.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /******************************************************************
     * So far, we have forced the auto cache resize through all modes 
     * other than increase_disabled and decrease_disabled.  Force these
     * modes now.  Note that there are several ways we can reach these
     * modes.
     ******************************************************************/

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 1.0; /* disable size increases */

        auto_size_ctl.apply_max_increment    = FALSE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 2.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 1.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full -- increase disabled so should 
     * be no change in cache size, and result should be increase_disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( rpt_status != increase_disabled ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 1.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- make sure that we haven't disabled decreases.
     * should result in a decrease cache size from 4 to 2 Meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 2.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate again -- increase disabled so should 
     * be no change in cache size, and result should be increase_disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( rpt_status != increase_disabled ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 3.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Repeat the above tests, disabling increase through the lower
     * threshold instead of the increment.
     */

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.0; /* disable size increases */

        auto_size_ctl.increment              = 2.0; 

        auto_size_ctl.apply_max_increment    = FALSE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 3.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 2.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full -- increase disabled so should 
     * be no change in cache size, and result should be in_spec.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 4.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- make sure that we haven't disabled decreases.
     * should result in a decrease cache size from 4 to 2 Meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 5.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate again -- increase disabled so should 
     * be no change in cache size, and result should be increase_disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 6.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Repeat the above tests yet again, disabling increase through the 
     * incr_mode.
     */

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__off;

        auto_size_ctl.lower_hr_threshold     = 0.75; 

        auto_size_ctl.increment              = 2.0; 

        auto_size_ctl.apply_max_increment    = FALSE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 4.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 3.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full -- increase disabled so should 
     * be no change in cache size, and result should be in_spec.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 7.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- make sure that we haven't disabled decreases.
     * should result in a decrease cache size from 4 to 2 Meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 8.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate again -- increase disabled so should 
     * be no change in cache size, and result should be increase_disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 9.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Now, disable size decreases, and repeat the above tests.
     */

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75; 

        auto_size_ctl.increment              = 2.0; 

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 1.0; /* disable size decreases */

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 5.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 4.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should be no change in cache size,
     * and result should be decrease_disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease_disabled ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 10.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- cache size should increase from 4 to 6 Meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 11.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate again -- should be no change in cache size,
     * and result should be decrease_disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease_disabled ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 12.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Repeat the above tests, disabling decrease through the upper
     * threshold instead of the decrement.
     */

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75; 

        auto_size_ctl.increment              = 2.0; 

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 1.0; /* disable size decreases */

        auto_size_ctl.decrement              = 0.5; 

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 6.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 5.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should be no change in cache size,
     * and result should be in_spec.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 13.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- cache size should increase from 4 to 6 Meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 14.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate again -- should be no change in cache size,
     * and result should be in_spec.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 15.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Repeat the above tests, disabling decrease through the decr_mode.
     */

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75; 

        auto_size_ctl.increment              = 2.0; 

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__off;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5; 

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 7.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 6.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should be no change in cache size,
     * and result should be in_spec.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 16.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- cache size should increase from 4 to 6 Meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 17.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate again -- should be no change in cache size,
     * and result should be in_spec.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 18.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Now do tests disabling size decrement in age out mode.
     *
     * Start by disabling size decrement by setting max_decrement to zero.
     */

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75; 

        auto_size_ctl.increment              = 2.0; 

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__age_out;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5; 

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = 0; /* disable decrement */

        auto_size_ctl.epochs_before_eviction = 1;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 8.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 7.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* flush the cache and destroy all entries so we start from a known point */
    flush_cache(cache_ptr, TRUE, FALSE, FALSE);

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* load up the cache with small entries.  Note that it will take an
     * epoch for the ageout code to initialize itself if it is enabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, SMALL_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != not_full ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 19.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Load up some more small entries.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 1000;
        while ( ( pass ) && ( i < 2000 ) )
        {
            protect_entry(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, SMALL_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != not_full ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 20.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Now force a high hit rate so that the size increase code is 
     * is satisfied.  We would see a decrease here if decrease were
     * possible.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, SMALL_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != decrease_disabled ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 21.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- cache size should increase from 4 to 6 Meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 22.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* just bang on a single entry.  This will see to it that there are
     * many entries that could be aged out were decreases enabled.
     * Should be no change in cache size, and result should be 
     * decrease_disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != decrease_disabled ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 23.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Now disable size decrement in age out mode via the empty reserve.
     */

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75; 

        auto_size_ctl.increment              = 2.0; 

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__age_out;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5; 

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 1;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 1.0; /* disable decrement */

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 9.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 8.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* flush the cache and destroy all entries so we start from a known point */
    flush_cache(cache_ptr, TRUE, FALSE, FALSE);

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* load up the cache with small entries.  Note that it will take an
     * epoch for the ageout code to initialize itself if it is enabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, SMALL_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != not_full ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 24.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Load up some more small entries.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 1000;
        while ( ( pass ) && ( i < 2000 ) )
        {
            protect_entry(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, SMALL_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != not_full ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 25.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Now force a high hit rate so that the size increase code is 
     * is satisfied.  We would see a decrease here if decrease were
     * possible.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, SMALL_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != decrease_disabled ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 26.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- cache size should increase from 4 to 6 Meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 27.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* just bang on a single entry.  This will see to it that there are
     * many entries that could be aged out were decreases enabled.
     * Should be no change in cache size, and result should be 
     * decrease_disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != decrease_disabled ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 28.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Now work with age out with threshold.  One can argue that we should 
     * repeat the above age out tests with age out with threshold, but the
     * same code is executed in both cases so I don't see the point.  If 
     * that ever changes, this test should be updated.
     *
     * There is only one way of disabling decrements that is peculiar
     * to age out with threshold, which is to set the upper threshold 
     * to 1.0.  Test this now.
     */

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75; 

        auto_size_ctl.increment              = 2.0; 

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__age_out_with_threshold;

        auto_size_ctl.upper_hr_threshold     = 1.0;

        auto_size_ctl.decrement              = 0.5; 

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 1;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 10.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 9.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* flush the cache and destroy all entries so we start from a known point */
    flush_cache(cache_ptr, TRUE, FALSE, FALSE);

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* load up the cache with small entries.  Note that it will take an
     * epoch for the ageout code to initialize itself if it is enabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, SMALL_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != not_full ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 29.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Load up some more small entries.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 1000;
        while ( ( pass ) && ( i < 2000 ) )
        {
            protect_entry(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, SMALL_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != not_full ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 30.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Now force a high hit rate so that the size increase code is 
     * is satisfied.  We would see a decrease here if decrease were
     * possible, but the upper threshold cannot be met, so no decrease.
     *
     * rpt_status should be decrease_disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, SMALL_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != decrease_disabled ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->index_len != 2000 ) ||
             ( cache_ptr->index_size != 2000 * SMALL_ENTRY_SIZE ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 31.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- cache size should increase from 4 to 6 Meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 32.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* just bang on a single entry.  This keeps the hit rate high, and sees
     * to it that there are many entries that could be aged out were 
     * decreases enabled.
     *
     * Should be no change in cache size, and result should be 
     * decrease_disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 999);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 999, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != decrease_disabled ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 33.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /*********************************************************************
     * Finally, use the auto cache resize code to set the size of the 
     * cache and keep it there.  Again, due to the complexity of the 
     * interface, there are lots of ways of doing this.  We have to 
     * check them all.
     *********************************************************************/

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 2 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               =  1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.0; /* disable size increases */

        auto_size_ctl.increment              = 2.0; 

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 1.0; /* disable size decreases */

        auto_size_ctl.decrement              = 0.5; 

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 11.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 10.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- should be no response as the auto-resize 
     * code should be disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 34.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 35.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.25;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               =  1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 1.0; /* disable size increment */

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 1.0; /* disable size decrement */

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 12.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 11.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- should be no response as the auto-resize 
     * code should be disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 36.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 37.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = FALSE;
        auto_size_ctl.initial_size           = 2 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 6 * 1024 * 1024; /* no resize */
        auto_size_ctl.min_size               = 6 * 1024 * 1024; /* no resize */

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 13.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 12.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- should be no response as the auto-resize 
     * code should be disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 38.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 39.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.25;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               =  1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 1.0; /* disable size increment */

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 1.0; /* disable size decrement */

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 14.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 13.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- should be no response as the auto-resize 
     * code should be disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 40.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 41.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               =  1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.0; /* disable size increment */

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 1.0; /* disable size decrement */

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;


        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 15.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 14.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- should be no response as the auto-resize 
     * code should be disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 42.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 43.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               =  1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__off;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__off;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;


        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 16.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 15.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- should be no response as the auto-resize 
     * code should be disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 44.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 45.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n", 
                  fcn_name, failure_mssg);

} /* check_auto_cache_resize_disable() */


/*-------------------------------------------------------------------------
 * Function:	check_auto_cache_resize_epoch_markers()
 *
 * Purpose:	Verify that the auto-resize code manages epoch markers
 *		correctly.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              12/16/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_auto_cache_resize_epoch_markers(void)
{
    const char * fcn_name = "check_auto_cache_resize_epoch_markers()";
    hbool_t show_progress = FALSE;
    herr_t result;
    int32_t i;
    int32_t j;
    int32_t checkpoint = 0;
    H5C_t * cache_ptr = NULL;
    H5C_auto_size_ctl_t auto_size_ctl =
    {
        /* int32_t     version                = */ H5C__CURR_AUTO_SIZE_CTL_VER,
        /* H5C_auto_resize_report_fcn rpt_fcn = */ test_rpt_fcn,

        /* hbool_t     set_initial_size       = */ TRUE,
        /* size_t      initial_size           = */ (512 * 1024),

        /* double      min_clean_fraction     = */ 0.5,

        /* size_t      max_size               = */ (14 * 1024 * 1024),
        /* size_t      min_size               = */ (512 * 1024),

        /* int64_t     epoch_length           = */ 1000,


        /* enum H5C_cache_incr_mode incr_mode = */ H5C_incr__threshold,

        /* double     lower_hr_threshold      = */ 0.75,

        /* double      increment              = */ 2.0,

        /* hbool_t     apply_max_increment    = */ TRUE,
        /* size_t      max_increment          = */ (4 * 1024 * 1024),


        /* enum H5C_cache_decr_mode decr_mode = */ H5C_decr__threshold,

        /* double      upper_hr_threshold     = */ 0.995,

        /* double      decrement              = */ 0.1,

        /* hbool_t     apply_max_decrement    = */ TRUE,
        /* size_t      max_decrement          = */ (1 * 1024 * 1024),

        /* int32_t     epochs_before_eviction = */ 3,

        /* hbool_t     apply_empty_reserve    = */ TRUE,
        /* double      empty_reserve          = */ 0.05
    };

    TESTING("automatic cache resize epoch marker management");

    pass = TRUE;

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));
    }

    if ( pass ) {

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 1.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after initialization.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /* Now make sure that we are managing the epoch markers correctly.
     */

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1024 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__off;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__age_out;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 10; 

        auto_size_ctl.apply_empty_reserve    = FALSE;
        auto_size_ctl.empty_reserve          = 0.05; 

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 2.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 1.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Since we just created the cache, there should be no epoch markers
     * active.  Verify that this is true.
     */

    if ( pass ) {

        if ( cache_ptr->epoch_markers_active != 0 ) {

            pass = FALSE;
            failure_mssg = "Unexpected # of epoch markers 1.\n";
        }
    }

    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->index_size != (1 * 1000 * MEDIUM_ENTRY_SIZE) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 0.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    if ( pass ) { 

        j = 2;
        while ( ( pass ) && ( j <= 10 ) )
        {

            rpt_fcn_called = FALSE;
            i = (j - 2) * 1000;
            while ( ( pass ) && ( i < (j - 1) * 1000 ) )
            {
                protect_entry(cache_ptr, SMALL_ENTRY_TYPE, i);

                if ( pass ) {
                    unprotect_entry(cache_ptr, SMALL_ENTRY_TYPE, i,
                                    NO_CHANGE, H5C__NO_FLAGS_SET);
                }
                i++;
            }

            if ( ( ! rpt_fcn_called ) ||
                 ( rpt_status != in_spec ) ||
                 ( cache_ptr->epoch_markers_active != j ) ) {

                pass = FALSE;
                failure_mssg = "Unexpected # of epoch markers 2.\n";
            }

            j++;
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* we now have a full complement of epoch markers -- see if 
     * we get the expected reduction.
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 9000;
        while ( ( pass ) && ( i < 10000 ) )
        {
            protect_entry(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, SMALL_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != 
               (10 * 1000 * SMALL_ENTRY_SIZE + MEDIUM_ENTRY_SIZE) ) ||
             ( cache_ptr->min_clean_size != 
               ((10 * 1000 * SMALL_ENTRY_SIZE + MEDIUM_ENTRY_SIZE) / 2) ) ||
             ( cache_ptr->index_size != 
               (10 * 1000 * SMALL_ENTRY_SIZE + MEDIUM_ENTRY_SIZE) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 1.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* now reduce the epochs before eviction, and see if the cache 
     * deletes the extra markers
     */
    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1024 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__off;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__age_out;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 1; 

        auto_size_ctl.apply_empty_reserve    = FALSE;
        auto_size_ctl.empty_reserve          = 0.05; 

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 3.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 2.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* There should be exactly one active epoch marker at present.
     */
    if ( pass ) {

        if ( cache_ptr->epoch_markers_active != 1 ) {

            pass = FALSE;
            failure_mssg = "Unexpected # of epoch markers 3.\n";
        }
    }

    /* Now do an epochs worth of accesses, and verify that everything
     * not accessed in this epoch gets evicted, and the cache size 
     * is reduced.
     */
    if ( pass ) { 

        rpt_fcn_called = FALSE;
        i = 9000;
        while ( ( pass ) && ( i < 10000 ) )
        {
            protect_entry(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, SMALL_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ||
             ( cache_ptr->index_size != (1 * 1000 * SMALL_ENTRY_SIZE) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 2.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* There should be exactly one active epoch marker at present...
     */
    if ( pass ) {

        if ( cache_ptr->epoch_markers_active != 1 ) {

            pass = FALSE;
            failure_mssg = "Unexpected # of epoch markers 4.\n";
        }
    }

    /* shift the decrement mode to threshold, and verify that we remove
     * all epoch markers.
     */
    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1024 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__off;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 1; 

        auto_size_ctl.apply_empty_reserve    = FALSE;
        auto_size_ctl.empty_reserve          = 0.05; 

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 4.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 3.\n";
        }
    }

    /* ... and now there should be none.
     */
    if ( pass ) {

        if ( cache_ptr->epoch_markers_active != 0 ) {

            pass = FALSE;
            failure_mssg = "Unexpected # of epoch markers 5.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* shift the decrement mode to age out with threshold.  Set epochs 
     * before eviction to 10 again.
     */
    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1024 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__off;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__age_out_with_threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 10; 

        auto_size_ctl.apply_empty_reserve    = FALSE;
        auto_size_ctl.empty_reserve          = 0.05; 

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 5.\n";
        }
    }

    /* Verify that there are no active epoch markers.
     */
    if ( pass ) {

        if ( cache_ptr->epoch_markers_active != 0 ) {

            pass = FALSE;
            failure_mssg = "Unexpected # of epoch markers 6.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* build up a full set of epoch markers. */
    if ( pass ) { 

        j = 1;
        while ( ( pass ) && ( j <= 10 ) )
        {

            rpt_fcn_called = FALSE;
            i = (j - 1) * 1000;
            while ( ( pass ) && ( i < j * 1000 ) )
            {
                protect_entry(cache_ptr, SMALL_ENTRY_TYPE, i);

                if ( pass ) {
                    unprotect_entry(cache_ptr, SMALL_ENTRY_TYPE, i,
                                    NO_CHANGE, H5C__NO_FLAGS_SET);
                }
                i++;
            }

            if ( ( ! rpt_fcn_called ) ||
                 ( rpt_status != in_spec ) ||
                 ( cache_ptr->epoch_markers_active != j ) ) {

                pass = FALSE;
                failure_mssg = "Unexpected # of epoch markers 7.\n";
            }

            j++;
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Verify that there are now 10 active epoch markers.
     */
    if ( pass ) {

        if ( cache_ptr->epoch_markers_active != 10 ) {

            pass = FALSE;
            failure_mssg = "Unexpected # of epoch markers 8.\n";
        }
    }

    /* shift the decrement mode to off.  This should cause all epoch
     * markers to be removed.
     */
    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1024 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__off;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__off;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 10; 

        auto_size_ctl.apply_empty_reserve    = FALSE;
        auto_size_ctl.empty_reserve          = 0.05; 

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 6.\n";
        }
    }

    /* Verify that there are now no active epoch markers.
     */
    if ( pass ) {

        if ( cache_ptr->epoch_markers_active != 0 ) {

            pass = FALSE;
            failure_mssg = "Unexpected # of epoch markers 9.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* verify that we still have the expected number of entries in the cache,
     * and that the cache is of the expected size.
     */
    if ( pass ) { 

        if ( ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) )||
             ( cache_ptr->index_size != (10 * 1000 * SMALL_ENTRY_SIZE) ) ||
             ( cache_ptr->index_len != 10000 ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 3.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n", 
                  fcn_name, failure_mssg);

} /* check_auto_cache_resize_epoch_markers() */


/*-------------------------------------------------------------------------
 * Function:	check_auto_cache_resize_input_errs()
 *
 * Purpose:	Verify that H5C_set_cache_auto_resize_config() detects
 *		and rejects invalid input.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              10/29/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

#define RESIZE_CONFIGS_ARE_EQUAL(a, b, compare_init)              \
( ( (a).version                == (b).version ) &&                \
  ( (a).rpt_fcn                == (b).rpt_fcn ) &&                \
  ( ( ! compare_init ) ||                                         \
    ( (a).set_initial_size     == (b).set_initial_size ) ) &&     \
  ( ( ! compare_init ) ||                                         \
    ( (a).initial_size         == (b).initial_size ) ) &&         \
  ( (a).min_clean_fraction     == (b).min_clean_fraction ) &&     \
  ( (a).max_size               == (b).max_size ) &&               \
  ( (a).min_size               == (b).min_size ) &&               \
  ( (a).epoch_length           == (b).epoch_length ) &&           \
  ( (a).incr_mode              == (b).incr_mode ) &&              \
  ( (a).lower_hr_threshold     == (b).lower_hr_threshold ) &&     \
  ( (a).increment              == (b).increment ) &&              \
  ( (a).apply_max_increment    == (b).apply_max_increment ) &&    \
  ( (a).max_increment          == (b).max_increment ) &&          \
  ( (a).decr_mode              == (b).decr_mode ) &&              \
  ( (a).upper_hr_threshold     == (b).upper_hr_threshold ) &&     \
  ( (a).decrement              == (b).decrement ) &&              \
  ( (a).apply_max_decrement    == (b).apply_max_decrement ) &&    \
  ( (a).max_decrement          == (b).max_decrement ) &&          \
  ( (a).epochs_before_eviction == (b).epochs_before_eviction ) && \
  ( (a).apply_empty_reserve    == (b).apply_empty_reserve ) &&    \
  ( (a).empty_reserve          == (b).empty_reserve ) )

static void
check_auto_cache_resize_input_errs(void)
{
    const char * fcn_name = "check_auto_cache_resize_input_errs()";
    herr_t result;
    H5C_t * cache_ptr = NULL;
    H5C_auto_size_ctl_t ref_auto_size_ctl =
    {
        /* int32_t     version                = */ H5C__CURR_AUTO_SIZE_CTL_VER,
        /* H5C_auto_resize_report_fcn rpt_fcn = */ test_rpt_fcn,

        /* hbool_t     set_initial_size       = */ TRUE,
        /* size_t      initial_size           = */ (512 * 1024),

        /* double      min_clean_fraction     = */ 0.5,

        /* size_t      max_size               = */ (16 * 1024 * 1024),
        /* size_t      min_size               = */ (512 * 1024),

        /* int64_t     epoch_length           = */ 1000,


        /* enum H5C_cache_incr_mode incr_mode = */ H5C_incr__threshold,

        /* double     lower_hr_threshold      = */ 0.75,

        /* double      increment              = */ 2.0,

        /* hbool_t     apply_max_increment    = */ TRUE,
        /* size_t      max_increment          = */ (4 * 1024 * 1024),


        /* enum H5C_cache_decr_mode decr_mode = */ H5C_decr__threshold,

        /* double      upper_hr_threshold     = */ 0.995,

        /* double      decrement              = */ 0.1,

        /* hbool_t     apply_max_decrement    = */ TRUE,
        /* size_t      max_decrement          = */ (1 * 1024 * 1024),

        /* int32_t     epochs_before_eviction = */ 3,

        /* hbool_t     apply_empty_reserve    = */ TRUE,
        /* double      empty_reserve          = */ 0.05
    };

    H5C_auto_size_ctl_t invalid_auto_size_ctl;
    H5C_auto_size_ctl_t test_auto_size_ctl;

    TESTING("automatic cache resize input errors");

    pass = TRUE;

    /* allocate a cache, and set a reference automatic cache control 
     * configuration.  Then feed H5C_set_cache_auto_resize_config()
     * invalid input, and verify that the correct error is returned,
     * and that the configuration is not modified.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));
    }

    if ( pass ) {

        result = H5C_set_cache_auto_resize_config(cache_ptr, 
                                                  &ref_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 1.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after initialization.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr, 
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 1.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 1.";
        }
    }

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.7;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(NULL, 
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_set_cache_auto_resize_config accepted NULL cache_ptr.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr, 
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 2.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 2.";
        }
    }


    /* check bad version rejection. */

    if ( pass ) {

        invalid_auto_size_ctl.version                = -1; /* INVALID */
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.7;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, 
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_set_cache_auto_resize_config accepted bad version.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr, 
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 3.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 3.";
        }
    }


    /* check bad initial size rejection */

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 16 * 1024 * 1024 + 1;
                                                       /* INVALID */

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;
 
        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, 
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_set_cache_auto_resize_config accepted bad init size 1.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr, 
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 4.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 4.";
        }
    }

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 1 * 1024 * 1024 - 1;
                                                       /* INVALID */

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;
 
        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, 
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_set_cache_auto_resize_config accepted bad init size 2.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr, 
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 5.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 5.";
        }
    }


    /* test for invalid min clean fraction rejection. */

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 1.00001; /* INVALID */

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;
 
        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, 
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = 
            "H5C_set_cache_auto_resize_config accepted bad min clean frac 1.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr, 
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 6.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 6.";
        }
    }

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = -0.00001; /* INVALID */

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;
 
        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, 
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = 
            "H5C_set_cache_auto_resize_config accepted bad min clean frac 2.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr, 
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 7.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 7.";
        }
    }


    /* test for invalid max_size and/or min_size rejection */

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size            = H5C__MAX_MAX_CACHE_SIZE + 1;
                                                    /* INVALID */
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, 
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_set_cache_auto_resize_config accepted bad max_size.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr, 
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 8.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 8.";
        }
    }

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size           = 1 * 1024 * 1024;/* INVALID */
        invalid_auto_size_ctl.min_size           = 1 * 1024 * 1024 + 1;/*PAIR */

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, 
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_set_cache_auto_resize_config accepted bad size pair.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr, 
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 9.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 9.";
        }
    }

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size            = H5C__MIN_MAX_CACHE_SIZE - 1;
                                                    /* INVALID */
        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, 
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_set_cache_auto_resize_config accepted bad min_size.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr, 
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 10.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 10.";
        }
    }


    /* test for invalid epoch_length rejection */

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;
 
        invalid_auto_size_ctl.epoch_length       = H5C__MAX_AR_EPOCH_LENGTH + 1;
                                                   /* INVALID */

        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, 
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_set_cache_auto_resize_config accepted bad epoch len 1.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr, 
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 11.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 11.";
        }
    }

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;
 
        invalid_auto_size_ctl.epoch_length       = H5C__MIN_AR_EPOCH_LENGTH - 1;
                                                   /* INVALID */

        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, 
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_set_cache_auto_resize_config accepted bad epoch len 2.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr, 
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 12.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 12.";
        }
    }


    /* test for bad incr_mode rejection */

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;
 
        invalid_auto_size_ctl.epoch_length           = 5000;
                                                   

        invalid_auto_size_ctl.incr_mode              = 
        			(enum H5C_cache_incr_mode) -1; /* INVALID */

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, 
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_set_cache_auto_resize_config accepted bad incr_mode 1.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr, 
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 13.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 13.";
        }
    }

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;
 
        invalid_auto_size_ctl.epoch_length           = 5000;
                                                   

        invalid_auto_size_ctl.incr_mode              = 
        			(enum H5C_cache_incr_mode) 2; /* INVALID */

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, 
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_set_cache_auto_resize_config accepted bad incr_mode 2.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr, 
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 14.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 14.";
        }
    }


    /* check for bad upper and/or lower threshold rejection */

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.7;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 1.01; /* INVALID */

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, 
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = 
             "H5C_set_cache_auto_resize_config accepted bad upper threshold.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr, 
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 15.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 15.";
        }
    }

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.8; /* INVALID */

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.7; /* INVALID */

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, 
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = 
              "H5C_set_cache_auto_resize_config accepted bad threshold pair.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr, 
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 16.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 16.";
        }
    }

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = -0.0001; /* INVALID */

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, 
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = 
             "H5C_set_cache_auto_resize_config accepted bad lower threshold.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr, 
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 17.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 17.";
        }
    }


    /* test for bad increment rejection */

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;
 
        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 0.99999; /* INVALID */

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;


        result = H5C_set_cache_auto_resize_config(cache_ptr, 
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_set_cache_auto_resize_config accepted bad increment.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr, 
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 18.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 18.";
        }
    }


    /* test for bad decr_mode rejection */

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;
 
        invalid_auto_size_ctl.epoch_length           = 5000;
                                                   

        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = 
        			(enum H5C_cache_decr_mode) -1; /* INVALID */

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, 
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_set_cache_auto_resize_config accepted bad decr_mode 1.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr, 
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 19.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 19.";
        }
    }

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;
 
        invalid_auto_size_ctl.epoch_length           = 5000;
                                                   

        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = 
        			(enum H5C_cache_decr_mode) 4; /* INVALID */

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, 
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_set_cache_auto_resize_config accepted bad decr_mode 2.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr, 
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 20.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 20.";
        }
    }


    /* check for bad decrement rejection */

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;
 
        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 1.000001; /* INVALID */

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, 
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_set_cache_auto_resize_config accepted bad decrement 1.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr, 
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 21.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 21.";
        }
    }

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;
 
        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = -0.000001; /* INVALID */

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, 
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_set_cache_auto_resize_config accepted bad decrement 2.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr, 
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 22.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 22.";
        }
    }


    /* check for rejection of bad epochs_before_eviction */

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;
 
        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__age_out;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 0; /* INVALID */

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, 
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config accepted bad epochs_before_eviction 1.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr, 
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 23.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 23.";
        }
    }

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;
 
        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode      = H5C_decr__age_out_with_threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 
                                       H5C__MAX_EPOCH_MARKERS + 1; /* INVALID */

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, 
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config accepted bad epochs_before_eviction 2.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr, 
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 24.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 24.";
        }
    }


    /* Check for bad apply_empty_reserve rejection */

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;
 
        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__age_out;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = -0.0000001; /* INVALID */

        result = H5C_set_cache_auto_resize_config(cache_ptr, 
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config accepted bad empty_reserve 1.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr, 
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 25.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 25.";
        }
    }

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;
 
        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode      = H5C_decr__age_out_with_threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 
                                       H5C__MAX_EPOCH_MARKERS + 1; /* INVALID */

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, 
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config accepted bad empty_reserve 2.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr, 
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 26.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 26.";
        }
    }


    /* finally, before we finish, try feeding 
     * H5C_get_cache_auto_resize_config invalid data.
     */

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(NULL, &test_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_get_cache_auto_resize_config accepted NULL cache_ptr.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config((H5C_t *)&test_auto_size_ctl,
                                                  &test_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_get_cache_auto_resize_config accepted bad cache_ptr.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr, NULL);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_get_cache_auto_resize_config accepted NULL config ptr.\n";
        }
    }

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n", 
                  fcn_name, failure_mssg);

} /* check_auto_cache_resize_input_errs() */


/*-------------------------------------------------------------------------
 * Function:	check_auto_cache_resize_aux_fcns()
 *
 * Purpose:	Verify that the auxilary functions associated with 
 *		the automatic cache resize capability are operating
 *		correctly.  These functions are:
 *
 *			H5C_get_cache_size()
 *			H5C_get_cache_hit_rate()
 *			H5C_reset_cache_hit_rate_stats()
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              11/4/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_auto_cache_resize_aux_fcns(void)
{
    const char * fcn_name = "check_auto_cache_resize_aux_fcns()";
    herr_t result;
    int32_t i;
    H5C_t * cache_ptr = NULL;
    double hit_rate;
    size_t max_size;
    size_t min_clean_size;
    size_t cur_size;
    int32_t cur_num_entries;
    H5C_auto_size_ctl_t auto_size_ctl =
    {
        /* int32_t     version                = */ H5C__CURR_AUTO_SIZE_CTL_VER,
#if 0
        /* H5C_auto_resize_report_fcn rpt_fcn = */ NULL,
#else
        /* H5C_auto_resize_report_fcn rpt_fcn = */ H5C_def_auto_resize_rpt_fcn,
#endif
        /* hbool_t     set_initial_size       = */ TRUE,
        /* size_t      initial_size           = */ (1 * 1024 * 1024),

        /* double      min_clean_fraction     = */ 0.5,

        /* size_t      max_size               = */ (16 * 1024 * 1025),
        /* size_t      min_size               = */ (512 * 1024),

        /* int64_t     epoch_length           = */ 50000,


        /* enum H5C_cache_incr_mode incr_mode = */ H5C_incr__off,

        /* double     lower_hr_threshold      = */ 0.75,

        /* double      increment              = */ 2.0,

        /* hbool_t     apply_max_increment    = */ TRUE,
        /* size_t      max_increment          = */ (4 * 1024 * 1024),


        /* enum H5C_cache_decr_mode decr_mode = */ H5C_decr__off,

        /* double      upper_hr_threshold     = */ 0.995,

        /* double      decrement              = */ 0.9,

        /* hbool_t     apply_max_decrement    = */ TRUE,
        /* size_t      max_decrement          = */ (1 * 1024 * 1024),

        /* int32_t     epochs_before_eviction = */ 3,

        /* hbool_t     apply_empty_reserve    = */ TRUE,
        /* double      empty_reserve          = */ 0.5
    };


    TESTING("automatic cache resize auxilary functions");

    pass = TRUE;

    /* allocate a cache, and then test the various auxilary functions.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));
    }

    if ( pass ) {

        result = H5C_set_cache_auto_resize_config(cache_ptr, 
                                                  &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 1.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (1 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (512 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after initialization.\n";
        }
    }

    /* lets start with the H5C_get_cache_hit_rate(),
     * H5C_reset_cache_hit_rate_stats() pair.
     */

    if ( pass ) {

        if ( ( H5C_get_cache_hit_rate(NULL, &hit_rate) != FAIL ) ||
             ( H5C_get_cache_hit_rate(cache_ptr, NULL) != FAIL ) ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_hit_rate accepts bad params.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_hit_rate(cache_ptr, &hit_rate);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_hit_rate failed.\n";

        } else if ( hit_rate != 0.0 ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_get_cache_hit_rate returned unexpected hit rate 1.\n";
        }
    }

    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, PICO_ENTRY_TYPE, i);

            if ( pass ) {

                unprotect_entry(cache_ptr, PICO_ENTRY_TYPE, i, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }
    }

    if ( pass ) {

        result = H5C_get_cache_hit_rate(cache_ptr, &hit_rate);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_hit_rate failed.\n";

        } else if ( hit_rate != 0.0 ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_get_cache_hit_rate returned unexpected hit rate 2.\n";

        } else if ( ( cache_ptr->cache_accesses != 1000 ) ||
                    ( cache_ptr->cache_hits != 0 ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache hit rate stats.\n";

        } else if ( rpt_fcn_called ) {

            pass = FALSE;
            failure_mssg = "Report function called?.\n";

        }
    }

    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, PICO_ENTRY_TYPE, 0);

            if ( pass ) {

                unprotect_entry(cache_ptr, PICO_ENTRY_TYPE, 0, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }
    }

    if ( pass ) {

        result = H5C_get_cache_hit_rate(cache_ptr, &hit_rate);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_hit_rate failed.\n";

        } else if ( hit_rate != 0.5 ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_get_cache_hit_rate returned unexpected hit rate 3.\n";

        } else if ( ( cache_ptr->cache_accesses != 2000 ) ||
                    ( cache_ptr->cache_hits != 1000 ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache hit rate stats.\n";

        } else if ( rpt_fcn_called ) {

            pass = FALSE;
            failure_mssg = "Report function called?.\n";

        }
    }

    if ( pass ) {

        result = H5C_reset_cache_hit_rate_stats(NULL);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_reset_cache_hit_rate_stats accepted NULL cache_ptr.\n";

        } else if ( ( cache_ptr->cache_accesses != 2000 ) ||
                    ( cache_ptr->cache_hits != 1000 ) ) {

            pass = FALSE;
            failure_mssg = 
              "Failed call to H5C_reset_cache_hit_rate_stats altered stats?\n";
        }
    }

    if ( pass ) {

        result = H5C_reset_cache_hit_rate_stats(cache_ptr);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_reset_cache_hit_rate_stats failed.\n";

        } else if ( ( cache_ptr->cache_accesses != 0 ) ||
                    ( cache_ptr->cache_hits != 0 ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache hit rate stats.\n";

        }
    }

    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, PICO_ENTRY_TYPE, i + 500);

            if ( pass ) {

                unprotect_entry(cache_ptr, PICO_ENTRY_TYPE, i + 500, 
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }
    }


    if ( pass ) {

        result = H5C_get_cache_hit_rate(cache_ptr, &hit_rate);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_hit_rate failed.\n";

        } else if ( hit_rate != 0.5 ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_get_cache_hit_rate returned unexpected hit rate 4.\n";

        } else if ( ( cache_ptr->cache_accesses != 1000 ) ||
                    ( cache_ptr->cache_hits != 500 ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache hit rate stats.\n";

        } else if ( rpt_fcn_called ) {

            pass = FALSE;
            failure_mssg = "Report function called?.\n";

        }
    }

    /*************************************************** 
     * So much for testing H5C_get_cache_hit_rate() and 
     * H5C_reset_cache_hit_rate_stats().  Now on to 
     * H5C_get_cache_size().
     ***************************************************/

    if ( pass ) {

        result = H5C_get_cache_size(NULL, &max_size, &min_clean_size, 
                                    &cur_size, &cur_num_entries);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_size accepted NULL cache_ptr.\n";
        }
    }

    if ( pass ) {

        max_size        = 0;
        min_clean_size  = 0;
        cur_size        = 0;
        cur_num_entries = 0;

        result = H5C_get_cache_size(cache_ptr, &max_size, &min_clean_size, 
                                    &cur_size, &cur_num_entries);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_size failed 1.\n";

        } else if ( max_size != (1 * 1024 * 1024) ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_get_cache_size reports unexpected max_size 1.\n";

        } else if ( min_clean_size != (512 * 1024) ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_get_cache_size reports unexpected min_clean_size 1.\n";

        } else if ( cur_size != (1500 * PICO_ENTRY_SIZE) ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_get_cache_size reports unexpected cur_size 1.\n";

        } else if ( cur_num_entries != 1500 ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_get_cache_size reports unexpected cur_num_entries 1.\n";
        }
    }

    /* read a larger entry so that cur_size and cur_num_entries will be
     * different.
     */
    if ( pass ) {

        protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);
    }

    if ( pass ) {
        unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, NO_CHANGE, 
                        H5C__NO_FLAGS_SET);
    }

    if ( pass ) {

        max_size        = 0;
        min_clean_size  = 0;
        cur_size        = 0;
        cur_num_entries = 0;

        result = H5C_get_cache_size(cache_ptr, &max_size, &min_clean_size, 
                                    &cur_size, &cur_num_entries);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_size failed 2.\n";

        } else if ( max_size != (1 * 1024 * 1024) ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_get_cache_size reports unexpected max_size 2.\n";

        } else if ( min_clean_size != (512 * 1024) ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_get_cache_size reports unexpected min_clean_size 2.\n";

        } else if ( cur_size != 
                   ((1500 * PICO_ENTRY_SIZE) + MONSTER_ENTRY_SIZE) ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_get_cache_size reports unexpected cur_size 2.\n";

        } else if ( cur_num_entries != 1501 ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_get_cache_size reports unexpected cur_num_entries 2.\n";
        }
    }

    if ( pass ) {

        max_size        = 0;
        min_clean_size  = 0;
        cur_size        = 0;
        cur_num_entries = 0;

        result = H5C_get_cache_size(cache_ptr, &max_size, NULL, NULL, NULL);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_size failed 3.\n";

        } else if ( max_size != (1 * 1024 * 1024) ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_get_cache_size reports unexpected max_size 3.\n";

        } else if ( ( min_clean_size != 0 ) ||
                    ( cur_size != 0 ) ||
                    ( cur_num_entries != 0 ) ) {

            pass = FALSE;
            failure_mssg = "Phantom returns from H5C_get_cache_size?\n";

        } 
    }

    if ( pass ) {

        max_size        = 0;
        min_clean_size  = 0;
        cur_size        = 0;
        cur_num_entries = 0;

        result = H5C_get_cache_size(cache_ptr, NULL, &min_clean_size, 
                                    NULL, NULL);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_size failed 4.\n";

        } else if ( min_clean_size != (512 * 1024) ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_get_cache_size reports unexpected min_clean_size 4.\n";

        } else if ( ( max_size != 0 ) ||
                    ( cur_size != 0 ) ||
                    ( cur_num_entries != 0 ) ) {

            pass = FALSE;
            failure_mssg = "Phantom returns from H5C_get_cache_size?\n";

        } 
    }

    if ( pass ) {

        max_size        = 0;
        min_clean_size  = 0;
        cur_size        = 0;
        cur_num_entries = 0;

        result = H5C_get_cache_size(cache_ptr, NULL, NULL, &cur_size, NULL);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_size failed 5.\n";

        } else if ( cur_size != 
                   ((1500 * PICO_ENTRY_SIZE) + MONSTER_ENTRY_SIZE) ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_get_cache_size reports unexpected cur_size 5.\n";

        } else if ( ( max_size != 0 ) ||
                    ( min_clean_size != 0 ) ||
                    ( cur_num_entries != 0 ) ) {

            pass = FALSE;
            failure_mssg = "Phantom returns from H5C_get_cache_size?\n";

        } 
    }

    if ( pass ) {

        max_size        = 0;
        min_clean_size  = 0;
        cur_size        = 0;
        cur_num_entries = 0;

        result = H5C_get_cache_size(cache_ptr, NULL, NULL, NULL, 
                                    &cur_num_entries);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_size failed 6.\n";

        } else if ( cur_num_entries != 1501 ) {

            pass = FALSE;
            failure_mssg = 
                "H5C_get_cache_size reports unexpected cur_num_entries 2.\n";

        } else if ( ( max_size != 0 ) ||
                    ( min_clean_size != 0 ) ||
                    ( cur_size != 0 ) ) {

            pass = FALSE;
            failure_mssg = "Phantom returns from H5C_get_cache_size?\n";

        } 
    }

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n", 
                  fcn_name, failure_mssg);

} /* check_auto_cache_resize_aux_fcns() */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Run tests on the cache code contained in H5C.c
 *
 * Return:	Success:	
 *
 *		Failure:	
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    H5open();

#ifdef NDEBUG
    skip_long_tests = FALSE;
#else /* NDEBUG */
    skip_long_tests = TRUE;
#endif /* NDEBUG */

    smoke_check_1();
    smoke_check_2();
    smoke_check_3();
    smoke_check_4();
    smoke_check_5();
    smoke_check_6();
    smoke_check_7();
    smoke_check_8();

    write_permitted_check();
    check_flush_cache();
    check_flush_protected_err();
    check_destroy_protected_err();
    check_duplicate_insert_err();
    check_rename_err();
    check_double_protect_err();
    check_double_unprotect_err();
    check_auto_cache_resize();
    check_auto_cache_resize_disable();
    check_auto_cache_resize_epoch_markers();
    check_auto_cache_resize_input_errs();
    check_auto_cache_resize_aux_fcns();

    return(0);

} /* main() */
