/*
 * Copyright (C) 1998-2001 NCSA
 *                         All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Thursday, April 16, 1998
 */
#ifndef _H5Zprivate_H
#define _H5Zprivate_H

#include "H5Zpublic.h"
#include "H5Fprivate.h"

/*
 * The filter table maps filter identification numbers to structs that
 * contain a pointers to the filter function and timing statistics.
 */
typedef struct H5Z_class_t {
    H5Z_filter_t id;		/*filter ID number			*/
    char	*name;		/*comment for debugging			*/
    H5Z_func_t	func;		/*the filter function			*/

#ifdef H5Z_DEBUG
    struct {
	hsize_t	total;		/*total number of bytes processed	*/
	hsize_t	errors;		/*bytes of total attributable to errors	*/
	H5_timer_t timer;	/*execution time including errors	*/
    } stats[2];			/*0=output, 1=input			*/
#endif
} H5Z_class_t;

struct H5O_pline_t; /*forward decl*/

H5_DLL herr_t H5Z_register(H5Z_filter_t id, const char *comment,
			    H5Z_func_t filter);
H5_DLL herr_t H5Z_unregister (H5Z_filter_t id);
H5_DLL herr_t H5Z_append(struct H5O_pline_t *pline, H5Z_filter_t filter,
			  unsigned flags, size_t cd_nelmts,
			  const unsigned int cd_values[]);
H5_DLL herr_t H5Z_pipeline(H5F_t *f, const struct H5O_pline_t *pline,
			    unsigned flags, unsigned *filter_mask/*in,out*/,
			    size_t *nbytes/*in,out*/,
			    size_t *buf_size/*in,out*/, void **buf/*in,out*/);
H5_DLL H5Z_class_t *H5Z_find(H5Z_filter_t id);


/* Filter routines */
size_t H5Z_filter_deflate(unsigned flags, size_t cd_nelmts,
			  const unsigned cd_values[], size_t nbytes,
			  size_t *buf_size, void **buf);

size_t H5Z_filter_shuffle(unsigned flags, size_t cd_nelmts,
			  const unsigned cd_values[], size_t nbytes,
			  size_t *buf_size, void **buf);
#endif
