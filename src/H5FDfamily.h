/*
 * Copyright � 1999 NCSA
 *                  All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Monday, August  4, 1999
 *
 * Purpose:	The public header file for the family driver.
 */
#ifndef H5FDfamily_H
#define H5FDfamily_H

#include <H5Ipublic.h>

#define H5FD_FAMILY	(H5FD_family_init())

hid_t H5FD_family_init(void);
herr_t H5Pset_fapl_family(hid_t fapl_id, hsize_t memb_size,
			  hid_t memb_fapl_id);
herr_t H5Pget_fapl_family(hid_t fapl_id, hsize_t *memb_size/*out*/,
			  hid_t *memb_fapl_id/*out*/);

#endif
