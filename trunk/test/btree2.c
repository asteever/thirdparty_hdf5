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

/* Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Tuesday, February  1, 2005
 */
#include "h5test.h"

/*
 * This file needs to access private datatypes from the H5B2 package.
 * This file also needs to access the v2 B-tree testing code.
 */
#define H5B2_PACKAGE
#define H5B2_TESTING
#include "H5B2pkg.h"

/* Other private headers that this test requires */
#include "H5Iprivate.h"

const char *FILENAME[] = {
    "btree2",
    NULL
};

#define INSERT_SPLIT_ROOT_NREC    80


/*-------------------------------------------------------------------------
 * Function:	iter_cb
 *
 * Purpose:	v2 B-tree iterator callback
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, February 16, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
iter_cb(const void *_record, void *_op_data)
{
    const hsize_t *record = (const hsize_t *)_record;
    hsize_t *idx = (hsize_t *)_op_data;

    if(*record != *idx)
        return(H5B2_ITER_ERROR);

    (*idx)++;
    return(H5B2_ITER_CONT);
}


/*-------------------------------------------------------------------------
 * Function:	test_insert_basic
 *
 * Purpose:	Basic tests for the B-tree v2 code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, February  3, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_basic(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    hsize_t     idx;                    /* Index within B-tree, for iterator */
    haddr_t     bt2_addr;               /* Address of B-tree created */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;
	
    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR;
    }

    /*
     * Test v2 B-tree creation
     */
    TESTING("B-tree creation");
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }
    PASSED();

    /* Attempt to iterate over a B-tree with no records */
    idx = 0;
    if(H5B2_iterate(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, iter_cb, &idx)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }
    /* Make certain that the index hasn't changed */
    if(idx != 0) TEST_ERROR;

    /*
     * Test inserting record into v2 B-tree 
     */
    TESTING("B-tree insert: several records");
    record=42;
    if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting second record into v2 B-tree, before all other records
     */
    record=34;
    if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting third record into v2 B-tree, after all other records
     */
    record=56;
    if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting fourth record into v2 B-tree, in the middle of other records
     */
    record=38;
    if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }
    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_basic() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_split_root
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to split the root node and force the tree to depth 1.
 *              It also continues to add a few more records to each of the
 *              left and right leaf nodes after the split
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, February  3, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_split_root(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    hsize_t     idx;                    /* Index within B-tree, for iterator */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;
	
    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test v2 B-tree creation
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree 
     */
    TESTING("B-tree insert: split root");

    for(u=0; u<INSERT_SPLIT_ROOT_NREC; u++) {
        record=u+2;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }
    record=0;
    if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    }
    record=1;
    if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    }

    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if(H5B2_iterate(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, iter_cb, &idx)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Make certain that the index is correct */
    if(idx != (INSERT_SPLIT_ROOT_NREC+2)) TEST_ERROR;

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_split_root() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_level1_2leaf_redistrib
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to split the root node and force the tree to depth 1.
 *              It continues to add a more records to the each of the
 *              left and right leaf nodes after the split to force a 2 node
 *              redistribution
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, February  8, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_level1_2leaf_redistrib(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;
	
    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Create v2 B-tree 
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree 
     */
    TESTING("B-tree insert: redistribute 2 leaves in level 1 B-tree (l->r)");

    /* Insert enough records to force root to split into 2 leaves */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC; u++) {
        record=u+INSERT_SPLIT_ROOT_NREC/2;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Force redistribution from left node into right node */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC/2; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }
    PASSED();

    /*
     * Create v2 B-tree 
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree 
     */
    TESTING("B-tree insert: redistribute 2 leaves in level 1 B-tree (r->l)");

    /* Insert enough records to force root to split into 2 leaves */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Force redistribution from left node into right node */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC/2; u++) {
        record=u+INSERT_SPLIT_ROOT_NREC;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }
    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_level1_2leaf_redistrib() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_level1_2leaf_split
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to split the root node and force the tree to depth 1.
 *              It continues to add a more records to the each of the
 *              left and right leaf nodes after the split to force a 2 node
 *              split, adding another node to the B-tree
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, February  9, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_level1_2leaf_split(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;
	
    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Create v2 B-tree 
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree 
     */
    TESTING("B-tree insert: split 2 leaves to 3 in level 1 B-tree (l->r)");

    /* Insert enough records to force root to split into 2 leaves */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC; u++) {
        record=u+INSERT_SPLIT_ROOT_NREC;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Force split from left node into right node */
    for(u=0; u<(3*INSERT_SPLIT_ROOT_NREC)/4; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }
    PASSED();

    /*
     * Create v2 B-tree 
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree 
     */
    TESTING("B-tree insert: split 2 leaves to 3 in level 1 B-tree (r->l)");

    /* Insert enough records to force root to split into 2 leaves */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Force redistribution from left node into right node */
    for(u=0; u<(3*INSERT_SPLIT_ROOT_NREC)/4; u++) {
        record=u+INSERT_SPLIT_ROOT_NREC;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }
    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_level1_2leaf_split() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_level1_3leaf_redistrib
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to split the root node and force the tree to depth 1.
 *              It continues to add a more records to the each of the
 *              left and right leaf nodes after the split to force a 2 node
 *              split, adding another node to the B-tree, then continues to
 *              add records until a 3 node redistribution occurs
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, February 10, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_level1_3leaf_redistrib(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    hsize_t     idx;                    /* Index within B-tree, for iterator */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;
	
    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Create v2 B-tree 
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree 
     */
    TESTING("B-tree insert: redistribute 3 leaves in level 1 B-tree");

    /* Insert enough records to force root to split into 2 leaves */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC; u++) {
        record=u+INSERT_SPLIT_ROOT_NREC;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Force split from left node into right node */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if(H5B2_iterate(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, iter_cb, &idx)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Make certain that the index is correct */
    if(idx != (INSERT_SPLIT_ROOT_NREC*2)) TEST_ERROR;

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_level1_3leaf_redistrib() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_level1_3leaf_split
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to split the root node and force the tree to depth 1.
 *              It continues to add a more records to the each of the
 *              left and right leaf nodes after the split to force a 2 node
 *              split, adding another node to the B-tree, then continues to
 *              add records until a 3 node split occurs
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, February 10, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_level1_3leaf_split(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;
	
    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Create v2 B-tree 
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree 
     */
    TESTING("B-tree insert: split 3 leaves to 4 in level 1 B-tree");

    /* Insert enough records to force root to split into 2 leaves */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC; u++) {
        record=u+INSERT_SPLIT_ROOT_NREC*2;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Force split from left node into right node */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*2; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }
    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_level1_3leaf_split() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_make_level2
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to make a level 2 B-tree
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, February 11, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_make_level2(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    hsize_t     idx;                    /* Index within B-tree, for iterator */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;
	
    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Create v2 B-tree 
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree 
     */
    TESTING("B-tree insert: make level 2 B-tree");

    /* Insert enough records to force root to split into 2 leaves */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*5; u++) {
        record=u+2;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }
    for(; u<INSERT_SPLIT_ROOT_NREC*11; u++) {
        record=u+4;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Add some extra records to left-most leaf */
    record=0;
    if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    }
    record=1;
    if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    }

    /* Add some extra records to middle leaf */
    record=(INSERT_SPLIT_ROOT_NREC*5)+2;
    if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    }
    record=(INSERT_SPLIT_ROOT_NREC*5)+3;
    if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    }


    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if(H5B2_iterate(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, iter_cb, &idx)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Make certain that the index is correct */
    if(idx != (INSERT_SPLIT_ROOT_NREC*11)+4) TEST_ERROR;

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_make_level2() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_level2_leaf_redistrib
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to make a level 2 B-tree and then adds enough more
 *              records to force the leaves to redistribute
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, February 17, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_level2_leaf_redistrib(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    hsize_t     idx;                    /* Index within B-tree, for iterator */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;
	
    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Create v2 B-tree 
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree 
     */
    TESTING("B-tree insert: redistrib right-most leaf in level 2 B-tree");

    /* Insert enough records to force root to split into 2 internal nodes */
    /* Also redistributes right leaf */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*4; u++) {
        record=u+(INSERT_SPLIT_ROOT_NREC/2);
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }
    for(; u<INSERT_SPLIT_ROOT_NREC*11; u++) {
        record=u+INSERT_SPLIT_ROOT_NREC;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    PASSED();

    TESTING("B-tree insert: redistrib left-most leaf in level 2 B-tree");

    /* Add more records to left-most leaf, to force a 2 node redistribution on left leaf */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC/2; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    PASSED();

    TESTING("B-tree insert: redistrib middle leaf in level 2 B-tree");

    /* Add more records to middle leaf, to force a 3 node redistribution on middle leaf */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC/2; u++) {
        record=u+(INSERT_SPLIT_ROOT_NREC/2)+(INSERT_SPLIT_ROOT_NREC*4);
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if(H5B2_iterate(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, iter_cb, &idx)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Make certain that the index is correct */
    if(idx != (INSERT_SPLIT_ROOT_NREC*12)) TEST_ERROR;

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_level2_leaf_redistrib() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_level2_leaf_split
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to make a level 2 B-tree and then adds enough more
 *              records to force leaves to split.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, February 17, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_level2_leaf_split(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    hsize_t     idx;                    /* Index within B-tree, for iterator */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;
	
    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Create v2 B-tree 
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree 
     */
    TESTING("B-tree insert: split right-most leaf in level 2 B-tree");

    /* Insert enough records to force root to split into 2 internal nodes */
    /* Also splits right leaf */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*4; u++) {
        record=u+INSERT_SPLIT_ROOT_NREC;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }
    for(; u<INSERT_SPLIT_ROOT_NREC*12; u++) {
        record=u+(INSERT_SPLIT_ROOT_NREC*2);
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    PASSED();

    TESTING("B-tree insert: split left-most leaf in level 2 B-tree");

    /* Add more records to left-most leaf, to force a 2->3 node split on left leaf */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    PASSED();

    TESTING("B-tree insert: split middle leaf in level 2 B-tree");

    /* Add more records to middle leaf, to force a 3->4 node split on middle leaf */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC; u++) {
        record=u+(INSERT_SPLIT_ROOT_NREC*5);
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if(H5B2_iterate(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, iter_cb, &idx)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Make certain that the index is correct */
    if(idx != (INSERT_SPLIT_ROOT_NREC*14)) TEST_ERROR;

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_level2_leaf_split() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_level2_2internal_redistrib
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to make a level 2 B-tree and then adds enough more
 *              records to force the left-most and right-most internal nodes to
 *              redistribute.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, February 18, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_level2_2internal_redistrib(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    hsize_t     idx;                    /* Index within B-tree, for iterator */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;
	
    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Create v2 B-tree 
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree 
     */
    TESTING("B-tree insert: redist. 2 internal (r->l) in level 2 B-tree");

    /* Insert enough records to force root to split into 2 internal nodes */
    /* Also forces right-most internal node to redistribute */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*16; u++) {
        record=u+(INSERT_SPLIT_ROOT_NREC*3);
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    PASSED();

    TESTING("B-tree insert: redist. 2 internal (l->r) in level 2 B-tree");

    /* Force left-most internal node to redistribute */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*3; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if(H5B2_iterate(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, iter_cb, &idx)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Make certain that the index is correct */
    if(idx != (INSERT_SPLIT_ROOT_NREC*19)) TEST_ERROR;

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_level2_2internal_redistrib() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_level2_2internal_split
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to make a level 2 B-tree and then adds enough more
 *              records to force the left-most and right-most internal nodes to
 *              split.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, February 18, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_level2_2internal_split(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    hsize_t     idx;                    /* Index within B-tree, for iterator */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;
	
    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Create v2 B-tree 
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree 
     */
    TESTING("B-tree insert: split 2 internals to 3 in level 2 B-tree (r->l)");

    /* Insert enough records to force root to split into 2 internal nodes */
    /* Also forces right-most internal node to split */
    for(u=0; u<(INSERT_SPLIT_ROOT_NREC*21); u++) {
        record=u+(INSERT_SPLIT_ROOT_NREC*8);
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    PASSED();

    TESTING("B-tree insert: split 2 internals to 3 in level 2 B-tree (l->r)");

    /* Force left-most internal node to split */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*8; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if(H5B2_iterate(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, iter_cb, &idx)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Make certain that the index is correct */
    if(idx != (INSERT_SPLIT_ROOT_NREC*29)) TEST_ERROR;

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_level2_2internal_split() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_level2_3internal_redistrib
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to make a level 2 B-tree and then adds enough more
 *              records to force the left-most and right-most internal nodes to
 *              split and more records to force a 3 node redistribution of the
 *              internal nodes.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Saturday, February 19, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_level2_3internal_redistrib(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    hsize_t     idx;                    /* Index within B-tree, for iterator */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;
	
    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Create v2 B-tree 
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree 
     */
    TESTING("B-tree insert: redistrib 3 internals in level 2 B-tree");

    /* Insert enough records to force root to split into 2 internal nodes */
    /* Also forces right-most internal node to split */
    for(u=0; u<(INSERT_SPLIT_ROOT_NREC*21); u++) {
        record=u+(INSERT_SPLIT_ROOT_NREC*13);
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Force left-most internal node to split */
    /* Force middle node to perform redistribution */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*13; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if(H5B2_iterate(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, iter_cb, &idx)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Make certain that the index is correct */
    if(idx != (INSERT_SPLIT_ROOT_NREC*34)) TEST_ERROR;

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_level2_3internal_redistrib() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_level2_3internal_split
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to make a level 2 B-tree and then adds enough more
 *              records to force the left-most and right-most internal nodes to
 *              split and more records to force a 3->4 node split of the
 *              internal nodes.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Saturday, February 19, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_level2_3internal_split(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    hsize_t     idx;                    /* Index within B-tree, for iterator */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;
	
    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Create v2 B-tree 
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree 
     */
    TESTING("B-tree insert: split 3 internals to 4 in level 2 B-tree");

    /* Insert enough records to force root to split into 2 internal nodes */
    /* Also forces right-most internal node to split */
    for(u=0; u<(INSERT_SPLIT_ROOT_NREC*21); u++) {
        record=u+(INSERT_SPLIT_ROOT_NREC*19)+28;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Force left-most internal node to split */
    /* Force middle node to split */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*19+28; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if(H5B2_iterate(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, iter_cb, &idx)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Make certain that the index is correct */
    if(idx != (INSERT_SPLIT_ROOT_NREC*40)+28) TEST_ERROR;

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_level2_3internal_split() */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test the B-tree v2 code
 *
 * Return:	Success:	
 *
 *		Failure:	
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, February  1, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t	fapl=-1;
    int		nerrors=0;

    /* Reset library */
    h5_reset();
    fapl = h5_fileaccess();

    /* Test basic B-tree insertion */
    nerrors += test_insert_basic(fapl);
    nerrors += test_insert_split_root(fapl);
    nerrors += test_insert_level1_2leaf_redistrib(fapl);
    nerrors += test_insert_level1_2leaf_split(fapl);
    nerrors += test_insert_level1_3leaf_redistrib(fapl);
    nerrors += test_insert_level1_3leaf_split(fapl);
    nerrors += test_insert_make_level2(fapl);
    nerrors += test_insert_level2_leaf_redistrib(fapl);
    nerrors += test_insert_level2_leaf_split(fapl);
    nerrors += test_insert_level2_2internal_redistrib(fapl);
    nerrors += test_insert_level2_2internal_split(fapl);
    nerrors += test_insert_level2_3internal_redistrib(fapl);
    nerrors += test_insert_level2_3internal_split(fapl);

    if (nerrors) goto error;
    puts("All v2 B-tree tests passed.");
#ifndef QAK
    h5_cleanup(FILENAME, fapl);
#else /* QAK */
HDfprintf(stderr,"Uncomment cleanup!\n");
#endif /* QAK */
    return 0;

error:
    puts("*** TESTS FAILED ***");
    H5E_BEGIN_TRY {
	H5Pclose(fapl);
    } H5E_END_TRY;
    return 1;
}

