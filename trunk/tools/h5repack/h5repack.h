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


#ifndef H5REPACK_H__
#define H5REPACK_H__

#include "hdf5.h"
#include "h5trav.h"


#define PFORMAT  "%-7s %-7s %-7s\n" /*chunk info, compression info, name*/
#define PFORMAT1 "%-7s %-7s %-7s"     /*chunk info, compression info, name*/

#define MAX_NC_NAME 256 /* max length of a name */
#define MAX_VAR_DIMS 32 /* max per variable dimensions */

#if 1
#define H5_REPACK_DEBUG
#endif


/*-------------------------------------------------------------------------
 * data structures for command line options
 *-------------------------------------------------------------------------
 */

/* a list of names */
typedef struct {
 char obj[MAX_NC_NAME]; 
} obj_list_t;

/* 
 the type of compression and additional parameter 
 type can be one of the filters
 H5Z_FILTER_NONE    0,  uncompress if compressed
 H5Z_FILTER_DEFLATE	1 , deflation like gzip	   
 H5Z_FILTER_SZIP    4 , szip compression 
*/
typedef struct {
 int type;
 int info;
} comp_info_t;

/* chunk lengths along each dimension and rank */
typedef struct {
 hsize_t chunk_lengths[MAX_VAR_DIMS]; 
 int     rank;
} chunk_info_t;

/* information for one object, contains PATH, CHUNK info and COMP info */
typedef struct {
 char         path[MAX_NC_NAME];            /* name of object */
 comp_info_t  comp;                         /* compression information */
 chunk_info_t chunk;                        /* chunk information */
} pack_info_t;

/* store a table of all objects */
typedef struct {
 int        size;
 int        nelems;
 pack_info_t *objs;
} pack_opttbl_t;


/*-------------------------------------------------------------------------
 * command line options
 *-------------------------------------------------------------------------
 */

/* all the above, ready to go to the hrepack call */
typedef struct {
 pack_opttbl_t   *op_tbl;     /*table with all -c and -t options */
 int             all_chunk;   /*chunk all objects, input of "*" */
 int             all_comp;    /*comp all objects, input of "*" */
 comp_info_t     comp_g;      /*global compress INFO for the ALL case */
 chunk_info_t    chunk_g;     /*global chunk INFO for the ALL case */
 int verbose;                 /*verbose mode */
	int threshold;               /*minimum size to compress, in bytes */
 
} pack_opt_t;



/*-------------------------------------------------------------------------
 * public functions
 *-------------------------------------------------------------------------
 */

#ifdef __cplusplus
extern "C" {
#endif

int h5repack         (const char* infile, const char* outfile, pack_opt_t *options);
int h5repack_addcomp (const char* str, pack_opt_t *options);
int h5repack_addchunk(const char* str, pack_opt_t *options);
int h5repack_init    (pack_opt_t *options, int verbose);
int h5repack_end     (pack_opt_t *options);

#ifdef __cplusplus
}
#endif



/*-------------------------------------------------------------------------
 * private functions
 *-------------------------------------------------------------------------
 */


int check_objects(const char* fname, 
                  pack_opt_t *options);

int copy_file(const char* fnamein, 
              const char* fnameout,
              pack_opt_t *options);

void print_objlist(const char *filename, 
                   int nobjects, 
                   trav_info_t *info );

int do_copy_file(hid_t fidin, 
                 hid_t fidout, 
                 int nobjects, 
                 trav_info_t *info,
                 pack_opt_t *options);

int copy_attr(hid_t loc_in, 
              hid_t loc_out, 
              pack_opt_t *options
              );


void read_info(const char *filename,pack_opt_t *options);



/*-------------------------------------------------------------------------
 * options table
 *-------------------------------------------------------------------------
 */


int          options_table_init( pack_opttbl_t **tbl );
int          options_table_free( pack_opttbl_t *table );
int          options_add_chunk ( obj_list_t *obj_list,
                                 int n_objs,
                                 hsize_t *chunk_lengths,
                                 int chunk_rank,
                                 pack_opttbl_t *table );
int          options_add_comp  ( obj_list_t *obj_list,
                                 int n_objs,
                                 comp_info_t comp,
                                 pack_opttbl_t *table );
pack_info_t* options_get_object( const char *path,
                                 pack_opttbl_t *table);




/*-------------------------------------------------------------------------
 * parse functions
 *-------------------------------------------------------------------------
 */

obj_list_t* parse_comp  (const char *str, 
                         int *n_objs, 
                         comp_info_t *comp);
obj_list_t* parse_chunk (const char *str, 
                         int *n_objs, 
                         hsize_t *chunk_lengths, 
                         int *chunk_rank);
const char* get_scomp   (int code);
int         parse_number(char *str);

/*-------------------------------------------------------------------------
 * tests
 *-------------------------------------------------------------------------
 */


#define FNAME1     "testcopy.h5"
#define FNAME1OUT  "testcopyout.h5"
#define FNAME2     "testfilters.h5"
#define FNAME2OUT  "testfiltersout.h5"



#define FNAME1     "testcopy.h5"
#define FNAME1OUT  "testcopyout.h5"
#define FNAME2     "testfilters.h5"
#define FNAME2OUT  "testfiltersout.h5"


int make_testfiles(void);

int make_all_objects(hid_t fid);

int make_attr(hid_t fid);

int write_dset( hid_t loc_id, 
                int rank, 
                hsize_t *dims, 
                const char *dset_name,
                hid_t type_id, 
                void *buf );

int write_attr(hid_t loc_id, 
               int rank, 
               hsize_t *dims, 
               const char *attr_name,
               hid_t type_id, 
               void *buf);




#endif  /* H5REPACK_H__ */
