/*
 * Copyright � 1998 NCSA
 *                  All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Tuesday, December 22, 1998
 */
#include <h5test.h>
#define CPTR(VAR,CONST)	((VAR)=(CONST),&(VAR))

const char *FILENAME[] = {
    "enum1",
    NULL
};

typedef enum {
    E1_RED,
    E1_GREEN,
    E1_BLUE,
    E1_WHITE,
    E1_BLACK
} c_e1;


/*-------------------------------------------------------------------------
 * Function:	test_named
 *
 * Purpose:	Create an enumeration data type and store it in the file.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Wednesday, December 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_named(hid_t file)
{
    hid_t	type=-1, cwg=-1;
    c_e1	val;
    signed char	val8;
    
    TESTING("named enumeration types");
    if ((cwg=H5Gcreate(file, "test_named", 0))<0) goto error;

    /* A native integer */
    if ((type = H5Tcreate(H5T_ENUM, sizeof(c_e1)))<0) goto error;
    if (H5Tenum_insert(type, "RED",   CPTR(val, E1_RED  ))<0) goto error;
    if (H5Tenum_insert(type, "GREEN", CPTR(val, E1_GREEN))<0) goto error;
    if (H5Tenum_insert(type, "BLUE",  CPTR(val, E1_BLUE ))<0) goto error;
    if (H5Tenum_insert(type, "WHITE", CPTR(val, E1_WHITE))<0) goto error;
    if (H5Tenum_insert(type, "BLACK", CPTR(val, E1_BLACK))<0) goto error;
    if (H5Tcommit(cwg, "e1_a", type)<0) goto error;
    if (H5Tclose(type)<0) goto error;

    /* A smaller type */
    if ((type = H5Tcreate(H5T_ENUM, 1))<0) goto error;
    if (H5Tenum_insert(type, "RED",   CPTR(val8, E1_RED  ))<0) goto error;
    if (H5Tenum_insert(type, "GREEN", CPTR(val8, E1_GREEN))<0) goto error;
    if (H5Tenum_insert(type, "BLUE",  CPTR(val8, E1_BLUE ))<0) goto error;
    if (H5Tenum_insert(type, "WHITE", CPTR(val8, E1_WHITE))<0) goto error;
    if (H5Tenum_insert(type, "BLACK", CPTR(val8, E1_BLACK))<0) goto error;
    if (H5Tcommit(cwg, "e1_b", type)<0) goto error;
    if (H5Tclose(type)<0) goto error;

    /* A non-native type */
    if (H5T_ORDER_BE==H5Tget_order(H5T_NATIVE_INT)) {
	if ((type = H5Tenum_create(H5T_STD_U8LE))<0) goto error;
    } else {
	if ((type = H5Tenum_create(H5T_STD_U8BE))<0) goto error;
    }
    if (H5Tenum_insert(type, "RED",   CPTR(val8, E1_RED  ))<0) goto error;
    if (H5Tenum_insert(type, "GREEN", CPTR(val8, E1_GREEN))<0) goto error;
    if (H5Tenum_insert(type, "BLUE",  CPTR(val8, E1_BLUE ))<0) goto error;
    if (H5Tenum_insert(type, "WHITE", CPTR(val8, E1_WHITE))<0) goto error;
    if (H5Tenum_insert(type, "BLACK", CPTR(val8, E1_BLACK))<0) goto error;
    if (H5Tcommit(cwg, "e1_c", type)<0) goto error;
    if (H5Tclose(type)<0) goto error;
    
    if (H5Gclose(cwg)<0) goto error;
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Tclose(type);
	H5Gclose(cwg);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_noconv
 *
 * Purpose:	Tests creation of datasets when no conversion is present.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Monday, January  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_noconv(hid_t file)
{
    hid_t	cwg=-1, type=-1, space=-1, dset=-1;
    c_e1	val;
    static c_e1	data1[]={E1_RED,   E1_GREEN, E1_BLUE,  E1_GREEN, E1_WHITE,
			 E1_WHITE, E1_BLACK, E1_GREEN, E1_BLUE,  E1_RED,
			 E1_RED,   E1_BLUE,  E1_GREEN, E1_BLACK, E1_WHITE,
			 E1_RED,   E1_WHITE, E1_GREEN, E1_GREEN, E1_BLUE};
    c_e1	data2[NELMTS(data1)];
    hsize_t	i, ds_size[1]={NELMTS(data1)};

    TESTING("no-conversion datasets");
    if ((cwg=H5Gcreate(file, "test_noconv", 0))<0) goto error;

    if ((type = H5Tcreate(H5T_ENUM, sizeof(c_e1)))<0) goto error;
    if (H5Tenum_insert(type, "RED",   CPTR(val, E1_RED  ))<0) goto error;
    if (H5Tenum_insert(type, "GREEN", CPTR(val, E1_GREEN))<0) goto error;
    if (H5Tenum_insert(type, "BLUE",  CPTR(val, E1_BLUE ))<0) goto error;
    if (H5Tenum_insert(type, "WHITE", CPTR(val, E1_WHITE))<0) goto error;
    if (H5Tenum_insert(type, "BLACK", CPTR(val, E1_BLACK))<0) goto error;

    if ((space=H5Screate_simple(1, ds_size, NULL))<0) goto error;
    if ((dset=H5Dcreate(cwg, "color_table", type, space, H5P_DEFAULT))<0)
	goto error;
    if (H5Dwrite(dset, type, space, space, H5P_DEFAULT, data1)<0) goto error;
    if (H5Dread(dset, type, space, space, H5P_DEFAULT, data2)<0) goto error;

    for (i=0; i<ds_size[0]; i++) {
	if (data1[i]!=data2[i]) {
	    FAILED();
	    printf("    data1[%lu]=%d, data2[%lu]=%d (should be same)\n",
		   (unsigned long)i, (int)(data1[i]),
		   (unsigned long)i, (int)(data2[i]));
	    goto error;
	}
    }
    
    if (H5Dclose(dset)<0) goto error;
    if (H5Sclose(space)<0) goto error;
    if (H5Tclose(type)<0) goto error;
    if (H5Gclose(cwg)<0) goto error;
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Dclose(dset);
	H5Sclose(space);
	H5Tclose(type);
	H5Gclose(cwg);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_tr1
 *
 * Purpose:	Writes enumerated data to a dataset which requires
 *		translation. Both memory and file data types use native
 *		integers but the file type has a different mapping between
 *		the integers and symbols.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Monday, January  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_tr1(hid_t file)
{
    hid_t	cwg=-1, m_type=-1, f_type=-1, space=-1, dset=-1;
    hsize_t	i, ds_size[1]={10};
    c_e1	val;
    static c_e1	data1[10]={E1_RED,   E1_GREEN, E1_BLUE,  E1_GREEN, E1_WHITE,
			   E1_WHITE, E1_BLACK, E1_GREEN, E1_BLUE,  E1_RED};
    c_e1	data2[10];

    TESTING("O(1) conversions");
    if ((cwg=H5Gcreate(file, "test_tr1", 0))<0) goto error;

    if ((m_type = H5Tcreate(H5T_ENUM, sizeof(c_e1)))<0) goto error;
    if (H5Tenum_insert(m_type, "RED",   CPTR(val, E1_RED  ))<0) goto error;
    if (H5Tenum_insert(m_type, "GREEN", CPTR(val, E1_GREEN))<0) goto error;
    if (H5Tenum_insert(m_type, "BLUE",  CPTR(val, E1_BLUE ))<0) goto error;
    if (H5Tenum_insert(m_type, "WHITE", CPTR(val, E1_WHITE))<0) goto error;
    if (H5Tenum_insert(m_type, "BLACK", CPTR(val, E1_BLACK))<0) goto error;

    if ((f_type = H5Tcreate(H5T_ENUM, sizeof(c_e1)))<0) goto error;
    if (H5Tenum_insert(f_type, "RED",   CPTR(val, 105))<0) goto error;
    if (H5Tenum_insert(f_type, "GREEN", CPTR(val, 104))<0) goto error;
    if (H5Tenum_insert(f_type, "BLUE",  CPTR(val, 103))<0) goto error;
    if (H5Tenum_insert(f_type, "WHITE", CPTR(val, 102))<0) goto error;
    if (H5Tenum_insert(f_type, "BLACK", CPTR(val, 101))<0) goto error;

    if ((space=H5Screate_simple(1, ds_size, NULL))<0) goto error;
    if ((dset=H5Dcreate(cwg, "color_table", f_type, space, H5P_DEFAULT))<0)
	goto error;
    if (H5Dwrite(dset, m_type, space, space, H5P_DEFAULT, data1)<0) goto error;
    if (H5Dread(dset, m_type, space, space, H5P_DEFAULT, data2)<0) goto error;

    for (i=0; i<ds_size[1]; i++) {
	if (data1[i]!=data2[i]) {
	    FAILED();
	    printf("    data1[%lu]=%d, data2[%lu]=%d (should be same)\n",
		   (unsigned long)i, (int)(data1[i]),
		   (unsigned long)i, (int)(data2[i]));
	    goto error;
	}
    }
    
    if (H5Dclose(dset)<0) goto error;
    if (H5Sclose(space)<0) goto error;
    if (H5Tclose(m_type)<0) goto error;
    if (H5Tclose(f_type)<0) goto error;
    if (H5Gclose(cwg)<0) goto error;
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Dclose(dset);
	H5Sclose(space);
	H5Tclose(m_type);
	H5Tclose(f_type);
	H5Gclose(cwg);
    } H5E_END_TRY;
    return 1;
}
    

/*-------------------------------------------------------------------------
 * Function:	test_tr2
 *
 * Purpose:	Tests conversions that use the O(log N) lookup function.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Tuesday, January  5, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_tr2(hid_t file)
{
    hid_t	cwg=-1, m_type=-1, f_type=-1, space=-1, dset=-1;
    hsize_t	i, ds_size[1]={10};
    c_e1	val1;
    int		val2;
    static c_e1	data1[10]={E1_RED,   E1_GREEN, E1_BLUE,  E1_GREEN, E1_WHITE,
			   E1_WHITE, E1_BLACK, E1_GREEN, E1_BLUE,  E1_RED};
    c_e1	data2[10];

    TESTING("O(log N) converions");
    if ((cwg=H5Gcreate(file, "test_tr2", 0))<0) goto error;

    if ((m_type = H5Tcreate(H5T_ENUM, sizeof(c_e1)))<0) goto error;
    if (H5Tenum_insert(m_type, "RED",   CPTR(val1, E1_RED  ))<0) goto error;
    if (H5Tenum_insert(m_type, "GREEN", CPTR(val1, E1_GREEN))<0) goto error;
    if (H5Tenum_insert(m_type, "BLUE",  CPTR(val1, E1_BLUE ))<0) goto error;
    if (H5Tenum_insert(m_type, "WHITE", CPTR(val1, E1_WHITE))<0) goto error;
    if (H5Tenum_insert(m_type, "BLACK", CPTR(val1, E1_BLACK))<0) goto error;

    if ((f_type = H5Tcreate(H5T_ENUM, sizeof(int)))<0) goto error;
    if (H5Tenum_insert(f_type, "RED",   CPTR(val2, 1050))<0) goto error;
    if (H5Tenum_insert(f_type, "GREEN", CPTR(val2, 1040))<0) goto error;
    if (H5Tenum_insert(f_type, "BLUE",  CPTR(val2, 1030))<0) goto error;
    if (H5Tenum_insert(f_type, "WHITE", CPTR(val2, 1020))<0) goto error;
    if (H5Tenum_insert(f_type, "BLACK", CPTR(val2, 1010))<0) goto error;

    if ((space=H5Screate_simple(1, ds_size, NULL))<0) goto error;
    if ((dset=H5Dcreate(cwg, "color_table", f_type, space, H5P_DEFAULT))<0)
	goto error;
    if (H5Dwrite(dset, m_type, space, space, H5P_DEFAULT, data1)<0) goto error;
    if (H5Dread(dset, m_type, space, space, H5P_DEFAULT, data2)<0) goto error;

    for (i=0; i<ds_size[1]; i++) {
	if (data1[i]!=data2[i]) {
	    FAILED();
	    printf("    data1[%lu]=%d, data2[%lu]=%d (should be same)\n",
		   (unsigned long)i, (int)(data1[i]),
		   (unsigned long)i, (int)(data2[i]));
	    goto error;
	}
    }
    
    if (H5Dclose(dset)<0) goto error;
    if (H5Sclose(space)<0) goto error;
    if (H5Tclose(m_type)<0) goto error;
    if (H5Tclose(f_type)<0) goto error;
    if (H5Gclose(cwg)<0) goto error;
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Dclose(dset);
	H5Sclose(space);
	H5Tclose(m_type);
	H5Tclose(f_type);
	H5Gclose(cwg);
    } H5E_END_TRY;
    return 1;
}
    

/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	
 *
 * Return:	Success:	
 *
 *		Failure:	
 *
 * Programmer:	Robb Matzke
 *              Tuesday, December 22, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t	fapl=-1, file=-1;
    char	name[1024];
    int		nerrors=0;
    
    h5_reset();
    fapl = h5_fileaccess();

    /* Create the file */
    h5_fixname(FILENAME[0], fapl, name, sizeof name);
    if ((file=H5Fcreate(name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) goto error;

    /* Tests */
    nerrors += test_named(file);
    nerrors += test_noconv(file);
    nerrors += test_tr1(file);
    nerrors += test_tr2(file);
    
    if (nerrors) goto error;
    puts("All enum tests passed.");
    h5_cleanup(fapl);
    return 0;

 error:
    puts("*** ENUM TESTS FAILED ***");
    return 1;
}
