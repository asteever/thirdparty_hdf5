/****************************************************************************
 * NCSA HDF                                                                 *
 * Software Development Group                                               *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/

/* $Id$ */

/*
 * This file contains private information about the H5P module
 */

#ifndef _H5Pprivate_H
#define _H5Pprivate_H
#include <H5Ppublic.h>

/* Private headers needed by this file */
#include <H5private.h>
#include <H5Cprivate.h>		/*for hobjtype_t defn*/

typedef struct {
    uint32 rank;        /* Number of dimensions */
    uint32 dim_flags;   /* Dimension flags */
    uint32 *size;       /* Dimension sizes */
    uint32 *max;        /* Maximum dimension sizes */
    uint32 *perm;       /* Dimension permutations */
} H5P_sdim_t;

#define H5P_RESERVED_ATOMS  1

/* Private functions */
hatom_t H5P_create(hatom_t owner_id, hobjtype_t type, const char *name);
uint32 H5P_get_lrank(H5P_sdim_t *sdim);
hbool_t H5P_is_simple(H5P_sdim_t *sdim);
herr_t H5P_release(hatom_t oid);

#endif
