/*-------------------------------------------------------------------------
 * Copyright (C) 1997   National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:             tstab.c
 *                      Aug  7 1997
 *                      Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:             
 *
 * Modifications:       
 *
 *-------------------------------------------------------------------------
 */
#include <testhdf5.h>

#include <H5private.h>
#include <H5Iprivate.h>
#include <H5ACprivate.h>
#include <H5Pprivate.h>
#include <H5Fprivate.h>
#include <H5Gprivate.h>
#include <H5Oprivate.h>

#define TEST_FILE_1	"tstab1.h5"
#define TEST_FILE_2 	"tstab2.h5"

/*
 * This file needs to access private datatypes from the H5G package.
 */
#define H5G_PACKAGE
#include <H5Gpkg.h>


/*-------------------------------------------------------------------------
 * Function:	test_1
 *
 * Purpose:	Miscellaneous group tests
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Friday, September 11, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
test_1(void)
{
    hid_t	file;
    hid_t	g1, g2, g3, g4;
    herr_t	status;
    char	comment[64];
    int		cmp;
    
    /* Test current working groups */
    MESSAGE(2,  ("........current working groups\n"));
    file = H5Fcreate(TEST_FILE_1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK_I(file, "H5Fcreate");
    
    g1 = H5Gcreate(file, "test_1a", 0);
    CHECK_I(g1, "H5Gcreate");
    status = H5Gset(file, "test_1a");

    g2 = H5Gcreate(g1, "sub_1", 0);
    CHECK_I(g2, "H5Gcreate");
    g3 = H5Gcreate(file, "sub_2", 0);
    CHECK_I(g3, "H5Gcreate");
    
    H5Gpop(g3);
    g4 = H5Gcreate(file, "test_1b", 0);
    CHECK_I(g4, "H5Gcreate");
    status = H5Gset_comment(g4, ".", "hello world");
    CHECK_I(status, "H5Gset_comment");

    /* Close all groups */
    status = H5Gclose(g1);
    CHECK_I(status, "H5Gclose");
    status = H5Gclose(g2);
    CHECK_I(status, "H5Gclose");
    status = H5Gclose(g3);
    CHECK_I(status, "H5Gclose");
    status = H5Gclose(g4);
    CHECK_I(status, "H5Gclose");
    
    /* Open all groups with absolute names to check for exsistence */
    g1 = H5Gopen(file, "/test_1a");
    CHECK_I(g1, "H5Gopen");
    g2 = H5Gopen(file, "/test_1a/sub_1");
    CHECK_I(g2, "H5Gopen");
    g3 = H5Gopen(file, "/test_1a/sub_2");
    CHECK_I(g3, "H5Gopen");
    g4 = H5Gopen(file, "/test_1b");
    CHECK_I(g4, "H5Gopen");
    status = H5Gget_comment(g4, "././.", sizeof comment, comment);
    CHECK_I(status, "H5Gget_comment");
    cmp = strcmp(comment, "hello world");
    VERIFY(cmp, 0, "strcmp");

    /* Close all groups */
    status = H5Gclose(g1);
    CHECK_I(status, "H5Gclose");
    status = H5Gclose(g2);
    CHECK_I(status, "H5Gclose");
    status = H5Gclose(g3);
    CHECK_I(status, "H5Gclose");
    status = H5Gclose(g4);
    CHECK_I(status, "H5Gclose");

    /* Close file */
    status = H5Fclose(file);
    CHECK_I(status, "H5Fclose");
}


/*-------------------------------------------------------------------------
 * Function:    test_2
 *
 * Purpose:     Creates a really large directory.
 *
 * Return:      void
 *
 * Programmer:  Robb Matzke
 *              robb@maya.nuance.com
 *              Aug 29 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
test_2(void)
{
    hid_t                   fid, create_plist, access_plist, dir;
    H5F_t                  *f;
    int                     i;
    char                    name[256];
    herr_t                  status;
    int                     nsyms = 5000;

    MESSAGE(2, ("........large directories\n"));

    /*
     * Use larger symbol table data structures to be more efficient, use
     * defaults to bang harder on the library for testing.
     */
    create_plist = H5Pcreate(H5P_FILE_CREATE);
    H5Pset_sym_k(create_plist, 16, 16);

    /*
     * File access property list.
     */
    access_plist = H5Pcreate (H5P_FILE_ACCESS);

#if 0
    /* Try temporary core files */
    H5Cset_core (access_plist, 3000000);
#elif 0
    /* Try a default split file but with our own name extensions */
    H5Cset_split (access_plist, ".XX1", H5C_DEFAULT, ".XX2", H5C_DEFAULT);
#elif 0
    {
	/* Try a split file with an in-core meta data part */
	hid_t meta_access = H5Ccreate (H5C_FILE_ACCESS);
	H5Cset_core (meta_access, 1024*1024);
	H5Cset_split (access_plist, NULL, meta_access, NULL, H5C_DEFAULT);
    }
#elif 0
    {
	/* Try a split file with an in-core raw data part */
	hid_t raw_access = H5Ccreate (H5C_FILE_ACCESS);
	H5Cset_core (raw_access, 1024*1024);
	H5Cset_split (access_plist, NULL, H5C_DEFAULT, NULL, raw_access);
    }
#endif

    /* create the file */
    fid = H5Fcreate(TEST_FILE_2, H5F_ACC_TRUNC, create_plist, access_plist);
    CHECK(fid, FAIL, "H5Fcreate");
    f = H5I_object(fid);
    CHECK(f, NULL, "H5I_object");
    f->intent |= H5F_ACC_DEBUG;

    /*
     * Create a directory that has so many entries that the root
     * of the B-tree ends up splitting.
     */
    dir = H5Gcreate(fid, "/big", (size_t)nsyms*16+2);
    CHECK_I(dir, "H5Gcreate");
    status = H5Gclose(dir);
    CHECK_I(status, "H5Gclose");
    status = H5Gset(fid, "/big");
    CHECK_I(status, "H5Gset");

    for (i = 0; i < nsyms; i++) {
        sprintf(name, "%05d%05d", rand() % 100000, i);
        MESSAGE(8, ("%s\n", name));
        dir = H5Gcreate(fid, name, 0);
        CHECK_I(dir, "H5Gcreate");
        status = H5Gclose(dir);
        CHECK_I(status, "H5Gclose");
    }

    /* close the property lists */
    status = H5Pclose(create_plist);
    CHECK_I(status, "H5Pclose");
    status = H5Pclose(access_plist);
    CHECK_I(status, "H5Pclose");

    /* close the file */
    status = H5Fclose(fid);
    CHECK_I(status, "H5Fclose");
}

/*-------------------------------------------------------------------------
 * Function:    test_stab
 *
 * Purpose:     Test symbol tables
 *
 * Return:      void
 *
 * Programmer:  Robb Matzke
 *              matzke@viper.llnl.gov
 *              Aug  7 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
test_stab(void)
{
    test_1();
    test_2();
}


/*-------------------------------------------------------------------------
 * Function:	cleanup_stab
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Albert Cheng
 *              July 2, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_stab(void)
{
    remove(TEST_FILE_1);
    remove(TEST_FILE_2);
}

