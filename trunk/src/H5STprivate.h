/****************************************************************************
 * NCSA HDF								    *
 * Software Development Group						    *
 * National Center for Supercomputing Applications			    *
 * University of Illinois at Urbana-Champaign				    *
 * 605 E. Springfield, Champaign IL 61820				    *
 *									    *
 * For conditions of distribution and use, see the accompanying		    *
 * hdf/COPYING file.							    *
 *									    *
 ****************************************************************************/

/*
 * This file contains private information about the H5ST module
 */
#ifndef _H5STprivate_H
#define _H5STprivate_H

#ifdef LATER
#include "H5STpublic.h"
#endif /* LATER */

/* Private headers needed by this file */
#include "H5private.h"

/* Typedefs */

/* Internal nodes for TST */
typedef struct H5ST_node *H5ST_ptr_t;
typedef struct H5ST_node {
    char splitchar;             /* Character represented at node */
    H5ST_ptr_t up;              /* Pointer to the node in the tree above (before) this node */
    H5ST_ptr_t parent;          /* Pointer to the next higher tree node in this tree */
    H5ST_ptr_t lokid;           /* Pointer to the lower node from this one, in this tree */
    H5ST_ptr_t eqkid;           /* Pointer to the parent node in the next tree down (after) this node */
    H5ST_ptr_t hikid;           /* Pointer to the higher node from this one, in this tree */
} H5ST_node_t;

/* Wrapper about TST */
typedef struct {
    H5ST_ptr_t root;            /* Pointer to actual TST */
} H5ST_tree_t;

/* Macro to access "data" pointer in H5ST_node_t's returned from functions */
#define H5ST_NODE_DATA(p)       ((void *)(p->eqkid))

/* Private routines */
H5_DLL H5ST_tree_t *H5ST_create(void);
H5_DLL herr_t H5ST_close(H5ST_tree_t *p);
H5_DLL herr_t H5ST_insert(H5ST_tree_t *root, const char *s, void *obj);
H5_DLL htri_t H5ST_search(H5ST_tree_t *root, const char *s);
H5_DLL H5ST_ptr_t H5ST_find(H5ST_tree_t *root, const char *s);
H5_DLL void *H5ST_locate(H5ST_tree_t *root, const char *s);
H5_DLL H5ST_ptr_t H5ST_findfirst(H5ST_tree_t *p);
H5_DLL H5ST_ptr_t H5ST_findnext(H5ST_ptr_t p);
H5_DLL void *H5ST_remove(H5ST_tree_t *root, const char *s);
H5_DLL herr_t H5ST_delete(H5ST_tree_t *root, H5ST_ptr_t p);
H5_DLL herr_t H5ST_dump(H5ST_ptr_t p);

#endif /* _H5STprivate_H */

