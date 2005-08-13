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

/*
 * Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Tuesday, May 13, 2003
 *
 * Purpose:	Test dangling IDs
 */
#include "h5test.h"
#include "H5private.h"

const char *FILENAME[] = {
    "dangle",
    NULL
};

#define MAX_DANGLE      1000

#define DSETNAME        "Dataset"
#define GROUPNAME       "Group"
#define TYPENAME        "Type"
#define ATTRNAME        "Attribute"


/*-------------------------------------------------------------------------
 * Function:	test_dangle_dataset
 *
 * Purpose:	Check for dangling dataset IDs causing problems on library
 *              shutdown
 *
 * Return:	Success:	zero
 *		Failure:	non-zero
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, May 13, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_dangle_dataset(H5F_close_degree_t degree)
{
    char	filename[1024];
    hid_t fid;  /* File ID */
    hid_t fapl; /* File access property list */
    hid_t dsid; /* Dataset ID */
    hid_t sid;  /* Dataspace ID */
    unsigned u; /* Local index variable */

    TESTING("    dangling dataset IDs");

    if(H5open()<0)
        TEST_ERROR;

    /* Create file access property list */
    if((fapl=H5Pcreate(H5P_FILE_ACCESS))<0)
        TEST_ERROR;

    /* Set file close degree */
    if(H5Pset_fclose_degree(fapl,degree)<0)
        TEST_ERROR;

    h5_fixname(FILENAME[0], H5P_DEFAULT, filename, sizeof filename);
    if((fid = H5Fcreate (filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
        TEST_ERROR;

    if((sid = H5Screate (H5S_SCALAR))<0)
        TEST_ERROR;

    if((dsid = H5Dcreate (fid, DSETNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT))<0)
        TEST_ERROR;

    if(H5Dclose(dsid)<0)
        TEST_ERROR;

    /* Try creating duplicate dataset */
    H5E_BEGIN_TRY {
        if((dsid = H5Dcreate (fid, DSETNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT))>=0)
            TEST_ERROR;
    } H5E_END_TRY;

    if(H5Sclose(sid)<0)
        TEST_ERROR;

    /* Leave open a _lot_ of objects */
    for(u=0; u<MAX_DANGLE; u++) {
        if((dsid = H5Dopen (fid, DSETNAME))<0)
            TEST_ERROR;
    } /* end for */

    if(degree==H5F_CLOSE_SEMI) {
        H5E_BEGIN_TRY {
            if(H5Fclose(fid)>=0)
                TEST_ERROR;
        } H5E_END_TRY;
    } /* end if */
    else
        if(H5Fclose(fid)<0)
            TEST_ERROR;

    if(H5Pclose(fapl)<0)
        TEST_ERROR;

    if(H5close()<0)
        TEST_ERROR;

    if(h5_get_file_size(filename)==0)
        TEST_ERROR;

    /* Clean up temporary file */
    HDremove(filename);

    PASSED();
    return 0;

error:
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_dangle_group
 *
 * Purpose:	Check for dangling group IDs causing problems on library
 *              shutdown
 *
 * Return:	Success:	zero
 *		Failure:	non-zero
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, May 13, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_dangle_group(H5F_close_degree_t degree)
{
    char	filename[1024];
    hid_t fid;  /* File ID */
    hid_t fapl; /* File access property list */
    hid_t gid;  /* Group ID */
    unsigned u; /* Local index variable */

    TESTING("    dangling group IDs");

    if(H5open()<0)
        TEST_ERROR;

    /* Create file access property list */
    if((fapl=H5Pcreate(H5P_FILE_ACCESS))<0)
        TEST_ERROR;

    /* Set file close degree */
    if(H5Pset_fclose_degree(fapl,degree)<0)
        TEST_ERROR;

    h5_fixname(FILENAME[0], H5P_DEFAULT, filename, sizeof filename);
    if((fid = H5Fcreate (filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
        TEST_ERROR;

    if((gid = H5Gcreate (fid, GROUPNAME, 0))<0)
        TEST_ERROR;

    if(H5Gclose(gid)<0)
        TEST_ERROR;

    /* Try creating duplicate group */
    H5E_BEGIN_TRY {
        if((gid = H5Gcreate (fid, GROUPNAME, 0))>=0)
            TEST_ERROR;
    } H5E_END_TRY;

    /* Leave open a _lot_ of objects */
    for(u=0; u<MAX_DANGLE; u++) {
        if((gid = H5Gopen (fid, GROUPNAME))<0)
            TEST_ERROR;
    } /* end for */

    if((gid = H5Gopen (fid, GROUPNAME))<0)
        TEST_ERROR;

    if(degree==H5F_CLOSE_SEMI) {
        H5E_BEGIN_TRY {
            if(H5Fclose(fid)>=0)
                TEST_ERROR;
        } H5E_END_TRY;
    } /* end if */
    else
        if(H5Fclose(fid)<0)
            TEST_ERROR;

    if(H5Pclose(fapl)<0)
        TEST_ERROR;

    if(H5close()<0)
        TEST_ERROR;

    if(h5_get_file_size(filename)==0)
        TEST_ERROR;

    /* Clean up temporary file */
    HDremove(filename);

    PASSED();
    return 0;

error:
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_dangle_datatype
 *
 * Purpose:	Check for dangling datatype IDs causing problems on library
 *              shutdown
 *
 * Return:	Success:	zero
 *		Failure:	non-zero
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, May 13, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_dangle_datatype(H5F_close_degree_t degree)
{
    char	filename[1024];
    hid_t fid;  /* File ID */
    hid_t fapl; /* File access property list */
    hid_t tid;  /* Datatype ID */
    unsigned u; /* Local index variable */

    TESTING("    dangling named datatype IDs");

    if(H5open()<0)
        TEST_ERROR;

    /* Create file access property list */
    if((fapl=H5Pcreate(H5P_FILE_ACCESS))<0)
        TEST_ERROR;

    /* Set file close degree */
    if(H5Pset_fclose_degree(fapl,degree)<0)
        TEST_ERROR;

    h5_fixname(FILENAME[0], H5P_DEFAULT, filename, sizeof filename);
    if((fid = H5Fcreate (filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
        TEST_ERROR;

    if((tid = H5Tcopy (H5T_NATIVE_INT))<0)
        TEST_ERROR;

    if(H5Tcommit(fid,TYPENAME,tid)<0)
        TEST_ERROR;

    if(H5Tclose(tid)<0)
        TEST_ERROR;

    /* Try creating duplicate named datatype */
    if((tid = H5Tcopy (H5T_NATIVE_INT))<0)
        TEST_ERROR;
    H5E_BEGIN_TRY {
        if(H5Tcommit(fid,TYPENAME,tid)>=0)
            TEST_ERROR;
    } H5E_END_TRY;
    if(H5Tclose(tid)<0)
        TEST_ERROR;

    /* Leave open a _lot_ of objects */
    for(u=0; u<MAX_DANGLE; u++) {
        if((tid = H5Topen (fid, TYPENAME))<0)
            TEST_ERROR;
    } /* end for */

    if(degree==H5F_CLOSE_SEMI) {
        H5E_BEGIN_TRY {
            if(H5Fclose(fid)>=0)
                TEST_ERROR;
        } H5E_END_TRY;
    } /* end if */
    else
        if(H5Fclose(fid)<0)
            TEST_ERROR;

    if(H5Pclose(fapl)<0)
        TEST_ERROR;

    if(H5close()<0)
        TEST_ERROR;

    if(h5_get_file_size(filename)==0)
        TEST_ERROR;

    /* Clean up temporary file */
    HDremove(filename);

    PASSED();
    return 0;

error:
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_dangle_attribute
 *
 * Purpose:	Check for dangling attribute IDs causing problems on library
 *              shutdown
 *
 * Return:	Success:	zero
 *		Failure:	non-zero
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, June 18, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_dangle_attribute(H5F_close_degree_t degree)
{
    char	filename[1024];
    hid_t fid;  /* File ID */
    hid_t fapl; /* File access property list */
    hid_t dsid; /* Dataset ID */
    hid_t sid;  /* Dataspace ID */
    hid_t aid;  /* Attribute ID */
    unsigned u; /* Local index variable */

    TESTING("    dangling attribute IDs");

    if(H5open()<0)
        TEST_ERROR;

    /* Create file access property list */
    if((fapl=H5Pcreate(H5P_FILE_ACCESS))<0)
        TEST_ERROR;

    /* Set file close degree */
    if(H5Pset_fclose_degree(fapl,degree)<0)
        TEST_ERROR;

    h5_fixname(FILENAME[0], H5P_DEFAULT, filename, sizeof filename);
    if((fid = H5Fcreate (filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
        TEST_ERROR;

    if((sid = H5Screate (H5S_SCALAR))<0)
        TEST_ERROR;

    if((dsid = H5Dcreate (fid, DSETNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT))<0)
        TEST_ERROR;

    /* Create an attribute on the dataset */
    if((aid = H5Acreate (dsid, ATTRNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT))<0)
        TEST_ERROR;

    /* Don't worry about writing the attribute - it will have a fill value */

    /* Close the attribute on the dataset */
    if(H5Aclose (aid)<0)
        TEST_ERROR;

    /* Try creating duplicate attribute */
    H5E_BEGIN_TRY {
        if((aid = H5Acreate (dsid, ATTRNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT))>=0)
            TEST_ERROR;
    } H5E_END_TRY;

    if(H5Sclose(sid)<0)
        TEST_ERROR;

    /* Leave open a _lot_ of objects */
    for(u=0; u<MAX_DANGLE; u++) {
        if((aid = H5Aopen_name (dsid, ATTRNAME))<0)
            TEST_ERROR;
    } /* end for */

    if(H5Dclose(dsid)<0)
        TEST_ERROR;

    if(degree==H5F_CLOSE_SEMI) {
        H5E_BEGIN_TRY {
            if(H5Fclose(fid)>=0)
                TEST_ERROR;
        } H5E_END_TRY;
    } /* end if */
    else
        if(H5Fclose(fid)<0)
            TEST_ERROR;

    if(H5Pclose(fapl)<0)
        TEST_ERROR;

    if(H5close()<0)
        TEST_ERROR;

    if(h5_get_file_size(filename)==0)
        TEST_ERROR;

    /* Clean up temporary file */
    HDremove(filename);

    PASSED();
    return 0;

error:
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Executes dangling ID tests
 *
 * Return:	Success:	zero
 *		Failure:	non-zero
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, May 13, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int		nerrors=0;

    /* Run tests w/weak file close */
    puts("Testing dangling objects with weak file close:");
    nerrors += test_dangle_dataset(H5F_CLOSE_WEAK);
    nerrors += test_dangle_group(H5F_CLOSE_WEAK);
    nerrors += test_dangle_datatype(H5F_CLOSE_WEAK);
    nerrors += test_dangle_attribute(H5F_CLOSE_WEAK);

    /* Run tests w/semi file close */
    puts("Testing dangling objects with semi file close:");
    nerrors += test_dangle_dataset(H5F_CLOSE_SEMI);
    nerrors += test_dangle_group(H5F_CLOSE_SEMI);
    nerrors += test_dangle_datatype(H5F_CLOSE_SEMI);
    nerrors += test_dangle_attribute(H5F_CLOSE_SEMI);

    /* Run tests w/strong file close */
    puts("Testing dangling objects with strong file close:");
    nerrors += test_dangle_dataset(H5F_CLOSE_STRONG);
    nerrors += test_dangle_group(H5F_CLOSE_STRONG);
    nerrors += test_dangle_datatype(H5F_CLOSE_STRONG);
    nerrors += test_dangle_attribute(H5F_CLOSE_STRONG);

    /* Check for errors */
    if (nerrors)
        goto error;
    puts("All dangling ID tests passed.");

    return 0;

error:
    puts("***** DANGLING ID TESTS FAILED *****");
    return 1;
}

