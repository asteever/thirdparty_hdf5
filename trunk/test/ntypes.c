/*
 * Copyright (C) 1997 NCSA
 *		      All rights reserved.
 *
 * Programmer:	Raymond Lu
 *              October 14, 2001	
 *
 * Purpose:	Tests the H5Tget_native_type function.
 */

#include "h5test.h"

const char *FILENAME[] = {
    "ntypes",
    NULL
};

#define DSET_ATOMIC_NAME_1	"atomic_type_1"
#define DSET_ATOMIC_NAME_2	"atomic_type_2"
#define DSET_ATOMIC_NAME_3	"atomic_type_3"
#define DSET_ATOMIC_NAME_4	"atomic_type_4"
#define DSET_COMPOUND_NAME      "compound_type"
#define DSET_COMPOUND_NAME_2    "compound_type_2"
#define DSET_ENUM_NAME	        "enum_type"
#define DSET_ARRAY_NAME	        "array_type"
#define DSET_VL_NAME	        "vl_type"
#define DSET_VLSTR_NAME         "vlstr_type"
#define DSET_OPAQUE_NAME        "opaque_type"
#define DSET_BITFIELD_NAME      "bitfield_type"

#define SPACE1_DIM1             4
#define SPACE1_RANK             1


/*-------------------------------------------------------------------------
 * Function:	test_atomic_dtype
 *
 * Purpose:	Test H5Tget_native_type for atomic datatype
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		October 15, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_atomic_dtype(hid_t file)
{
    hid_t		dataset, space;
    hid_t               dtype, native_type;
    int			i, j, n;
    hsize_t		dims[2];
    int	                points[100][200], check[100][200];
    
    TESTING("atomic datatype");

    /* Initialize the dataset */
    for (i = n = 0; i < 100; i++) {
	for (j = 0; j < 200; j++) {
	    points[i][j] = n++;
	}
    }

    /* Create the data space */
    dims[0] = 100;
    dims[1] = 200;
    if ((space = H5Screate_simple(2, dims, NULL))<0) goto error;

    /*------------------- Test data values ------------------------*/
    /* Create the dataset */
    if ((dataset = H5Dcreate(file, DSET_ATOMIC_NAME_1, H5T_STD_I32BE, space,
			     H5P_DEFAULT))<0) goto error;

    /* Write the data to the dataset */
    if (H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, points)<0)
	goto error;

    /* Close dataset */
    if(H5Dclose(dataset)<0) goto error;

    /* Open dataset again to check H5Tget_native_type */
    if((dataset=H5Dopen(file, DSET_ATOMIC_NAME_1))<0) goto error;

    if((dtype=H5Dget_type(dataset))<0) goto error;
    
    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DEFAULT))<0)
        goto error;

    /* Verify the datatype retrieved and converted */
    if(H5Tget_order(native_type) != H5Tget_order(H5T_NATIVE_INT)) 
        goto error;
    if(sizeof(int)!=H5Tget_size(native_type))
        goto error;
    if(H5T_INTEGER!=H5Tget_class(native_type))
        goto error;

    /* Read the dataset back */
    if (H5Dread(dataset, native_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, check)<0)
	goto error;

    /* Check that the values read are the same as the values written */
    for (i = 0; i < 100; i++) {
	for (j = 0; j < 200; j++) {
	    if (points[i][j] != check[i][j]) {
		H5_FAILED();
		printf("    Read different values than written.\n");
		printf("    At index %d,%d\n", i, j);
		goto error;
	    }
	}
    }

    H5Dclose(dataset);
    H5Tclose(dtype);

    /*------------------ Test different data types ----------------*/

    /* Create the dataset of H5T_STD_I64LE */
    if ((dataset = H5Dcreate(file, DSET_ATOMIC_NAME_2, H5T_STD_I64LE, space,
			     H5P_DEFAULT))<0) goto error;

    if((dtype=H5Dget_type(dataset))<0) goto error;
    
    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DEFAULT))<0)
        goto error;
        
    /* Verify the datatype retrieved and converted */
    if(H5Tget_order(native_type) != H5Tget_order(H5T_NATIVE_LLONG)) 
        goto error;
    if(sizeof(long_long)!=H5Tget_size(native_type))
        goto error;
    if(H5T_INTEGER!=H5Tget_class(native_type))
        goto error;
        
    if(H5Dclose(dataset)<0) goto error;
    if(H5Tclose(dtype)<0) goto error;


    /* Create the dataset of H5T_STD_I8LE */
    if ((dataset = H5Dcreate(file, DSET_ATOMIC_NAME_3, H5T_STD_I8LE, space,
			     H5P_DEFAULT))<0) goto error;

    if((dtype=H5Dget_type(dataset))<0) goto error;
    
    if((native_type=H5Tget_native_type(dtype, H5T_DIR_ASCEND))<0)
        goto error;
        
    /* Verify the datatype retrieved and converted */
    if(H5Tget_order(native_type) != H5Tget_order(H5T_NATIVE_CHAR)) 
        goto error;
    if(sizeof(char)!=H5Tget_size(native_type))
        goto error;
    if(H5T_INTEGER!=H5Tget_class(native_type))
        goto error;
        
    if(H5Dclose(dataset)<0) goto error;
    if(H5Tclose(dtype)<0) goto error;
    
    
    /* Create the dataset of H5T_IEEE_F64BE */
    if ((dataset = H5Dcreate(file, DSET_ATOMIC_NAME_4, H5T_IEEE_F32BE, space,
			     H5P_DEFAULT))<0) goto error;

    if((dtype=H5Dget_type(dataset))<0) goto error;
    
    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DESCEND))<0)
        goto error;
        
    /* Verify the datatype retrieved and converted */
    if(H5Tget_order(native_type) != H5Tget_order(H5T_NATIVE_FLOAT)) 
        goto error;
    if(sizeof(float)!=H5Tget_size(native_type))
        goto error;
    if(H5T_FLOAT!=H5Tget_class(native_type))
        goto error;
        
    if(H5Dclose(dataset)<0) goto error;
    if(H5Tclose(dtype)<0) goto error;
       
       
    /* Close dataspace */
    if(H5Sclose(space)<0) goto error;
        
    PASSED();
    return 0;

  error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_compound_dtype_2
 *
 * Purpose:	Test H5Tget_native_type for compound datatype
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		October 15, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_compound_dtype_2(hid_t file)
{
    typedef struct s2 {
        short           c2;
        long            l2;
    } s2;
    typedef struct s1 {
        char            c;
        int             i;
        s2              st;
        unsigned long_long       l;
    } s1;
    hid_t		dataset, space;
    hid_t               dtype, native_type, tid, tid2, tid_m, tid_m2;
    int			i, j, n;
    hsize_t		dims[2];
    s1                 *temp_point, *temp_check;
    s1 	               *points=NULL, *check=NULL;

    TESTING("nested compound datatype");

    /* Allocate space for the points & check arrays */
    if((points=malloc(sizeof(s1)*100*200))==NULL)
        goto error;
    if((check=calloc(sizeof(s1),100*200))==NULL)
        goto error;

    /* Initialize the dataset */
    for (i = n = 0, temp_point=points; i < 100; i++) {
	for (j = 0; j < 200; j++,temp_point++) {
	    points->c = 't';
	    points->i = n++;
	    points->st.c2 = i+j;
	    points->st.l2 = (i*5+j*50)*n;
	    points->l = (i*10+j*100)*n;
	}
    }

    /* Create the data space */
    dims[0] = 100;
    dims[1] = 200;
    if ((space = H5Screate_simple(2, dims, NULL))<0) goto error;

    /* Create compound datatype for disk storage */
    if((tid2=H5Tcreate(H5T_COMPOUND, 6))<0) goto error;
    if((tid=H5Tcreate(H5T_COMPOUND, 19))<0) goto error;

    /* Insert and pack members */
    if(H5Tinsert(tid2, "c2", 0, H5T_STD_I16BE)<0) goto error;
    if(H5Tinsert(tid2, "l2", 2, H5T_STD_I32LE)<0) goto error;

    if(H5Tinsert(tid, "c", 0, H5T_NATIVE_CHAR)<0) goto error;
    if(H5Tinsert(tid, "i", 1, H5T_STD_I32LE)<0) goto error;
    if(H5Tinsert(tid, "st", 5, tid2)<0) goto error;
    if(H5Tinsert(tid, "l", 11, H5T_STD_U64BE)<0) goto error;

    /* Create the dataset */
    if ((dataset = H5Dcreate(file, DSET_COMPOUND_NAME_2, tid, space,
			     H5P_DEFAULT))<0) goto error;

    /* Create compound datatype for memory */
    if((tid_m2=H5Tcreate(H5T_COMPOUND, sizeof(s2)))<0) goto error;
    if((tid_m=H5Tcreate(H5T_COMPOUND, sizeof(s1)))<0) goto error;

    /* Insert members */
    if(H5Tinsert(tid_m2, "c2", HOFFSET(s2, c2), H5T_NATIVE_SHORT)<0) goto error;
    if(H5Tinsert(tid_m2, "l2", HOFFSET(s2, l2), H5T_NATIVE_LONG)<0) goto error;
    if(H5Tinsert(tid_m, "c", HOFFSET(s1, c), H5T_NATIVE_CHAR)<0) goto error;
    if(H5Tinsert(tid_m, "i", HOFFSET(s1, i), H5T_NATIVE_INT)<0) goto error;
    if(H5Tinsert(tid_m, "st", HOFFSET(s1, st), tid_m2)<0) goto error;
    if(H5Tinsert(tid_m, "l", HOFFSET(s1, l), H5T_NATIVE_ULLONG)<0) goto error;

    /* Write the data to the dataset */
    if (H5Dwrite(dataset, tid_m, H5S_ALL, H5S_ALL, H5P_DEFAULT, points)<0)
	goto error;

    /* Close dataset */
    if(H5Dclose(dataset)<0) goto error;

    /* Close datatype */
    if(H5Tclose(tid2)<0) goto error;
    if(H5Tclose(tid)<0) goto error;
    if(H5Tclose(tid_m2)<0) goto error;
        
    /* Close dataspace */
    if(H5Sclose(space)<0) goto error; 

    
    /* Open dataset again to check H5Tget_native_type */
    if((dataset=H5Dopen(file, DSET_COMPOUND_NAME_2))<0) goto error;

    if((dtype=H5Dget_type(dataset))<0) goto error;

    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DEFAULT))<0)
        goto error;
        
    if(sizeof(s1)!=H5Tget_size(native_type))
        goto error;
    if(!H5Tequal(native_type, tid_m)) 
        goto error;
        
    /* Read the dataset back */
    if (H5Dread(dataset, native_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, check)<0)
	goto error;

    /* Check that the values read are the same as the values written */
    for (i = 0, temp_point=points, temp_check=check; i < 100; i++) {
	for (j = 0; j < 200; j++, temp_point++,temp_check++) {
	    if (temp_point->c != temp_check->c ||
	        temp_point->i != temp_check->i ||
	        temp_point->st.c2 != temp_check->st.c2 ||
	        temp_point->st.l2 != temp_check->st.l2 ||
	        temp_point->l != temp_check->l ) {
		H5_FAILED();
		printf("    Read different values than written.\n");
		printf("    At index %d,%d\n", i, j);
		goto error;
	    }
	}
    }

    /* Close HDF5 objects */
    H5Dclose(dataset);
    H5Tclose(dtype);
    H5Tclose(native_type);
    H5Tclose(tid_m);

    /* Free memory for test data */
    free(points);
    free(check);

    PASSED();
    return 0;

error:
    if(points!=NULL)
        free(points);
    if(check!=NULL)
        free(check);
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_compound_dtype
 *
 * Purpose:	Test H5Tget_native_type for compound datatype
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		October 15, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_compound_dtype(hid_t file)
{
    typedef struct {
        char            c;
        unsigned int    i;
        long_long       l;
    } s1;
    hid_t		dataset, space;
    hid_t               dtype, native_type, tid, tid2;
    int			i, j, n;
    hsize_t		dims[2];
    s1	                points[100][200], check[100][200];

    TESTING("compound datatype");

    /* Initialize the dataset */
    for (i = n = 0; i < 100; i++) {
	for (j = 0; j < 200; j++) {
	    (points[i][j]).c = 't';
	    (points[i][j]).i = n++;
	    (points[i][j]).l = (i*10+j*100)*n;
	}
    }

    /* Create the data space */
    dims[0] = 100;
    dims[1] = 200;
    if ((space = H5Screate_simple(2, dims, NULL))<0) goto error;

    /* Create compound datatype for disk storage */
    if((tid=H5Tcreate(H5T_COMPOUND, sizeof(s1)))<0) goto error;

    /* Insert members */
    if(H5Tinsert(tid, "c", 0, H5T_NATIVE_CHAR)<0) goto error;
    if(H5Tinsert(tid, "i", 1, H5T_STD_U32LE)<0) goto error;
    if(H5Tinsert(tid, "l", 5, H5T_STD_I64BE)<0) goto error;
    
    /* Create the dataset */
    if ((dataset = H5Dcreate(file, DSET_COMPOUND_NAME, tid, space,
			     H5P_DEFAULT))<0) goto error;

    /* Create compound datatype for datatype in memory */
    if((tid2=H5Tcreate(H5T_COMPOUND, sizeof(s1)))<0) goto error;
    if(H5Tinsert(tid2, "c", HOFFSET(s1, c), H5T_NATIVE_CHAR)<0) goto error;
    if(H5Tinsert(tid2, "i", HOFFSET(s1, i), H5T_NATIVE_UINT)<0) goto error;
    if(H5Tinsert(tid2, "l", HOFFSET(s1, l), H5T_NATIVE_LLONG)<0) goto error;
    
    /* Write the data to the dataset */
    if (H5Dwrite(dataset, tid2, H5S_ALL, H5S_ALL, H5P_DEFAULT, points)<0)
	goto error;

    /* Close dataset */
    if(H5Dclose(dataset)<0) goto error;

    /* Close datatype */
    if(H5Tclose(tid)<0) goto error;

    /* Close dataspace */
    if(H5Sclose(space)<0) goto error; 

    
    /* Open dataset again to check H5Tget_native_type */
    if((dataset=H5Dopen(file, DSET_COMPOUND_NAME))<0) goto error;

    if((dtype=H5Dget_type(dataset))<0) goto error;

    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DEFAULT))<0)
        goto error;
    
    if(sizeof(s1)!=H5Tget_size(native_type))
        goto error;
    if(!H5Tequal(native_type, tid2)) 
        goto error;
        
    /* Read the dataset back */
    if (H5Dread(dataset, native_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, check)<0)
	goto error;

    /* Check that the values read are the same as the values written */
    for (i = 0; i < 100; i++) {
	for (j = 0; j < 200; j++) {
	    if ((points[i][j]).c != (check[i][j]).c ||
	        (points[i][j]).i != (check[i][j]).i ||
	        (points[i][j]).l != (check[i][j]).l ) {
		H5_FAILED();
		printf("    Read different values than written.\n");
		printf("    At index %d,%d\n", i, j);
		goto error;
	    }
	}
    }

    H5Dclose(dataset);
    H5Tclose(dtype);
    H5Tclose(native_type);
    H5Tclose(tid2);
    PASSED();
    return 0;

  error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_enum_dtype
 *
 * Purpose:	Test H5Tget_native_type for enumerate datatype
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		October 15, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_enum_dtype(hid_t file)
{
    hid_t		dataset, space;
    hid_t               tid, tid_m, dtype, native_type;
    int			i, j, n;
    hsize_t		dims[2];
    short               points[100][200], check[100][200];
    short               colors[8];
    const char         *mname[] = { "RED",
                                     "GREEN",
                                     "BLUE",
                                     "YELLOW",
                                     "PINK",
                                     "PURPLE",
                                     "ORANGE",
                                     "WHITE" };

    TESTING("enum datatype");

    /* Initialize the dataset */
    for (i = 0; i < 100; i++) {
        for (j=0, n=0; j < 200; j++, n++)
	    points[i][j] = (i*10+j*100+n)%8;
    }

    /* Create the data space */
    dims[0] = 100;
    dims[1] = 200;
    if ((space = H5Screate_simple(2, dims, NULL))<0) goto error;

    /* Construct enum type based on native type */   
    if((tid=H5Tenum_create(H5T_STD_I16LE))<0) goto error;

    for (i = 0; i < 8; i++) {
        colors[i] = i;
        if(H5Tenum_insert(tid, mname[i], &(colors[i]))<0) goto error;
    }
     
    /* Create the dataset */
    if ((dataset = H5Dcreate(file, DSET_ENUM_NAME, tid, space,
			     H5P_DEFAULT))<0) goto error;

    /* Construct enum type based on native type in memory */   
    if((tid_m=H5Tenum_create(H5T_NATIVE_SHORT))<0) goto error;

    for (i = 0; i < 8; i++) {
        colors[i] = i;
        if(H5Tenum_insert(tid_m, mname[i], &(colors[i]))<0) goto error;
    }
     
    /* Write the data to the dataset */
    if (H5Dwrite(dataset, tid_m, H5S_ALL, H5S_ALL, H5P_DEFAULT, points)<0)
	goto error;

    /* Close dataset */
    if(H5Dclose(dataset)<0) goto error;

    /* Close datatype */
    if(H5Tclose(tid)<0) goto error;
    
    /* Close dataspace */
    if(H5Sclose(space)<0) goto error; 

    /* Open dataset again to check H5Tget_native_type */
    if((dataset=H5Dopen(file, DSET_ENUM_NAME))<0) goto error;

    if((dtype=H5Dget_type(dataset))<0) goto error;
    
    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DEFAULT))<0)
        goto error;
    
    if(!H5Tequal(native_type, tid_m)) 
        goto error;
        
    /* Read the dataset back */
    if (H5Dread(dataset, native_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, check)<0)
	goto error;

    /* Check that the values read are the same as the values written */
    for (i = 0; i < 100; i++) {
	for (j = 0; j < 200; j++) {
	    if (points[i][j] != check[i][j]) {
		H5_FAILED();
		printf("    Read different values than written.\n");
		printf("    At index %d,%d\n", i, j);
		goto error;
	    }
	}
    }

    H5Dclose(dataset);
    H5Tclose(dtype);
    H5Tclose(native_type);
    H5Tclose(tid_m);
    PASSED();
    return 0;

  error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_array_dtype
 *
 * Purpose:	Test H5Tget_native_type for array datatype
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		October 15, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_array_dtype(hid_t file)
{
    typedef struct {
        char    c;
        int     i;
        long_long l;
    } s1;
    hid_t		dataset, space;
    hid_t               dtype, native_type, tid, tid2, tid3, tid_m;
    int			i, j, k, n;
    hsize_t		space_dims[2], array_dims[1]={5};
    s1                 *temp_point, *temp_check;
    s1 	               *points=NULL, *check=NULL;

    TESTING("array datatype");

    /* Allocate space for the points & check arrays */
    if((points=malloc(sizeof(s1)*100*200*5))==NULL)
        goto error;
    if((check=calloc(sizeof(s1),100*200*5))==NULL)
        goto error;

    /* Initialize the dataset */
    for(i = n = 0, temp_point=points; i < 100; i++)
	for(j = 0; j < 200; j++)
            for(k = 0; k < 5; k++,temp_point++) {
                temp_point->c= 't';
                temp_point->i= n++;
                temp_point->l= (i*10+j*100)*n;
            }

    /* Create the data space */
    space_dims[0] = 100;
    space_dims[1] = 200;
    if ((space = H5Screate_simple(2, space_dims, NULL))<0) goto error;

    /* Create compound datatype for disk storage */
    if((tid2=H5Tcreate(H5T_COMPOUND, 13))<0) goto error;

    /* Insert members */
    if(H5Tinsert(tid2, "c", 0, H5T_NATIVE_CHAR)<0) goto error;
    if(H5Tinsert(tid2, "i", 1, H5T_STD_U32LE)<0) goto error;
    if(H5Tinsert(tid2, "l", 5, H5T_STD_I64BE)<0) goto error;
    
    /* Create array datatype for disk storage */
    if((tid=H5Tarray_create(tid2, 1, array_dims, NULL))<0) goto error;
    
    /* Create the dataset */
    if ((dataset = H5Dcreate(file, DSET_ARRAY_NAME, tid, space,
			     H5P_DEFAULT))<0) goto error;

    /* Create compound datatype for datatype in memory */
    if((tid3=H5Tcreate(H5T_COMPOUND, sizeof(s1)))<0) goto error;
    if(H5Tinsert(tid3, "c", HOFFSET(s1, c), H5T_NATIVE_CHAR)<0) goto error;
    if(H5Tinsert(tid3, "i", HOFFSET(s1, i), H5T_NATIVE_UINT)<0) goto error;
    if(H5Tinsert(tid3, "l", HOFFSET(s1, l), H5T_NATIVE_LLONG)<0) goto error;
    
    /* Create array datatype for memory */
    if((tid_m=H5Tarray_create(tid3, 1, array_dims, NULL))<0) goto error;
    
    /* Write the data to the dataset */
    if (H5Dwrite(dataset, tid_m, H5S_ALL, H5S_ALL, H5P_DEFAULT, points)<0)
	goto error;

    /* Close dataset */
    if(H5Dclose(dataset)<0) goto error;
        
    /* Close datatype */
    if(H5Tclose(tid)<0) goto error;
    if(H5Tclose(tid2)<0) goto error;

    /* Close dataspace */
    if(H5Sclose(space)<0) goto error; 

    /* Open dataset again to check H5Tget_native_type */
    if((dataset=H5Dopen(file, DSET_ARRAY_NAME))<0) goto error;

    if((dtype=H5Dget_type(dataset))<0) goto error;
    
    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DEFAULT))<0)
        goto error;

    if(!H5Tequal(tid_m, native_type)) goto error;

    /* Read the dataset back */
    if (H5Dread(dataset, native_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, check)<0)
	goto error;

    /* Check that the values read are the same as the values written */
    for (i = 0, temp_point=points, temp_check=check; i < 100; i++) {
	for (j = 0; j < 200; j++) {
            for (k = 0; k < 5; k++, temp_point++,temp_check++) {
                if (temp_point->c != temp_check->c ||
	            temp_point->i != temp_check->i ||
	            temp_point->l != temp_check->l ) {
                    H5_FAILED();
                    printf("    Read different values than written.\n");
                    printf("    At index %d,%d\n", i, j);
                    goto error;
                }
	    }
	}
    }

    /* Close HDF5 objects */
    if(H5Dclose(dataset)) goto error;
    if(H5Tclose(native_type)) goto error;
    if(H5Tclose(dtype)) goto error;
    if(H5Tclose(tid_m)<0) goto error;
    if(H5Tclose(tid3)<0) goto error;

    /* Free memory for test data */
    free(points);
    free(check);

    PASSED();
    return 0;

error:
    if(points!=NULL)
        free(points);
    if(check!=NULL)
        free(check);
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_vl_dtype
 *
 * Purpose:	Test H5Tget_native_type for variable length datatype
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		October 15, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
test_vl_dtype(hid_t file)
{
    hvl_t               wdata[SPACE1_DIM1];   /* Information to write */
    hvl_t               rdata[SPACE1_DIM1];   /* Information read in */
    hvl_t               *t1, *t2;             /* Temporary pointer to VL information */
    hsize_t		dims1[] = {SPACE1_DIM1};
    hid_t		dataset, space;
    hid_t               dtype, native_type, tid, tid2, tid_m, tid_m2;
    size_t		i, j, k;

    TESTING("variable length datatype");

    /* Allocate and initialize VL data to write */
    for(i=0; i<SPACE1_DIM1; i++) {
        wdata[i].p=malloc((i+1)*sizeof(hvl_t));
        if(wdata[i].p==NULL) {
            printf("Cannot allocate memory for VL data! i=%u\n",i);
            H5_FAILED();
            goto error;
        } /* end if */
        wdata[i].len=i+1;
        for(t1=wdata[i].p,j=0; j<(i+1); j++, t1++) {
            t1->p=malloc((j+1)*sizeof(unsigned int));
            if(t1->p==NULL) {
                printf("Cannot allocate memory for VL data! i=%u, j=%u\n",i,j);
                H5_FAILED();
                goto error;
            } /* end if */
            t1->len=j+1;
            for(k=0; k<(j+1); k++)
                ((unsigned int *)t1->p)[k]=i*100+j*10+k;
        } /* end for */
    } /* end for */
    
    /* Create dataspace for datasets */
    if((space = H5Screate_simple(SPACE1_RANK, dims1, NULL))<0) goto error;

    /* Create the base VL type */
    if((tid2 = H5Tvlen_create (H5T_STD_U32LE))<0) goto error;

    /* Create a VL datatype for disk storage */
    tid = H5Tvlen_create (tid2);
    
    /* Create a dataset */
    if((dataset=H5Dcreate(file, DSET_VL_NAME, tid, space, H5P_DEFAULT))<0)
        goto error;

    /* Create a base VL datatype for memory */
    if((tid_m2 = H5Tvlen_create (H5T_NATIVE_UINT))<0) goto error;
        
    /* Create a VL datatype for memory */
    if((tid_m = H5Tvlen_create (tid_m2))<0) goto error;
        
    /* Write dataset to disk */
    if(H5Dwrite(dataset,tid_m,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata)<0) goto error;

    /* Close Dataset */
    if(H5Dclose(dataset)<0) goto error;

    /* Close datatype */
    if(H5Tclose(tid2)<0) goto error;
    if(H5Tclose(tid)<0) goto error;

    /* Open a dataset */
    if((dataset=H5Dopen(file, DSET_VL_NAME))<0) goto error;

    /* Get datatype for dataset */
    if((dtype = H5Dget_type(dataset))<0) goto error;
    
    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DEFAULT))<0)
        goto error;
 
    if(!H5Tequal(native_type, tid_m)) 
        goto error;
        
    /* Read dataset from disk */
    if(H5Dread(dataset,native_type,H5S_ALL,H5S_ALL,H5P_DEFAULT,rdata)<0) goto error;
    
    /* Compare data read in */
    for(i=0; i<SPACE1_DIM1; i++) {
        if(wdata[i].len!=rdata[i].len) {
            H5_FAILED();
            printf("VL data length don't match!, wdata[%d].len=%d, rdata[%d].len=%d\n",(int)i,(int)wdata[i].len,(int)i,(int)rdata[i].len);
            goto error;
        } /* end if */
        for(t1=wdata[i].p, t2=rdata[i].p, j=0; j<rdata[i].len; j++, t1++, t2++) {
            if(t1->len!=t2->len) {
                H5_FAILED();
                printf("VL data length don't match!, wdata[%d].len=%d, rdata[%d].len=%d\n",(int)i,(int)wdata[i].len,(int)i,(int)rdata[i].len);
                goto error;
            } /* end if */
            for(k=0; k<t2->len; k++) {
                if( ((unsigned int *)t1->p)[k] != ((unsigned int *)t2->p)[k] ) {
                    H5_FAILED();
                    printf("VL data length don't match!, wdata[%d].len=%d, rdata[%d].len=%d\n",(int)i,(int)wdata[i].len,(int)i,(int)rdata[i].len);
                    goto error;
                }
            } /* end for */
        } /* end for */
    } /* end for */

    /* Reclaim the read VL data */
    if(H5Dvlen_reclaim(native_type,space,H5P_DEFAULT,rdata)<0) goto error;

    /* Reclaim the write VL data */
    if(H5Dvlen_reclaim(native_type,space,H5P_DEFAULT,wdata)<0) goto error;

    /* Close Dataset */
    if(H5Dclose(dataset)<0) goto error;

    /* Close datatype */
    if(H5Tclose(native_type)<0) goto error;
    if(H5Tclose(dtype)<0) goto error;
    if(H5Tclose(tid_m)<0) goto error;
    if(H5Tclose(tid_m2)<0) goto error;
    

    /* Close disk dataspace */
    if(H5Sclose(space)<0) goto error;
    
    PASSED();
    return 0;

 error:
    return -1;
} /* end test_vl_type() */


/*-------------------------------------------------------------------------
 * Function:	test_vlstr_dtype
 *
 * Purpose:	Test H5Tget_native_type for variable length string datatype
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		October 15, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
test_vlstr_dtype(hid_t file)
{
    const char *wdata[SPACE1_DIM1]= {
        "Four score and seven years ago our forefathers brought forth on this continent a new nation,",
        "conceived in liberty and dedicated to the proposition that all men are created equal.",
        "Now we are engaged in a great civil war,",
        "testing whether that nation or any nation so conceived and so dedicated can long endure."
        };   /* Information to write */
    char *rdata[SPACE1_DIM1];   /* Information read in */
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1;       /* Dataspace ID			*/
    hid_t		tid1,dtype,native_type;       /* Datatype ID			*/
    hsize_t		dims1[] = {SPACE1_DIM1};
    unsigned       i;          /* counting variable */

    /* Output message about test being performed */
    TESTING("variable length string datatype");

    /* Create dataspace for datasets */
    if((sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL))<0) goto error;

    /* Create a datatype to refer to */
    if((tid1 = H5Tcopy (H5T_C_S1))<0) goto error;

    if(H5Tset_size (tid1,H5T_VARIABLE)<0) goto error;
    if(H5T_STRING!=H5Tget_class(tid1) || !H5Tis_variable_str(tid1))
        goto error;
    
    /* Create a dataset */
    if((dataset=H5Dcreate(file,DSET_VLSTR_NAME,tid1,sid1,H5P_DEFAULT))<0) goto error;

    /* Write dataset to disk */
    if(H5Dwrite(dataset,tid1,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata)<0) goto error;

    /* Close Dataset */
    if(H5Dclose(dataset)<0) goto error;

    /* Open a dataset */
    if((dataset=H5Dopen(file, DSET_VLSTR_NAME))<0) goto error;

    /* Get datatype for dataset */
    if((dtype = H5Dget_type(dataset))<0) goto error;

    /* Construct native type */
    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DEFAULT))<0)
        goto error;

    /* Check if the data type is equal */
    if(!H5Tequal(native_type, tid1)) 
        goto error;

    /* Read dataset from disk */
    if(H5Dread(dataset,native_type,H5S_ALL,H5S_ALL,H5P_DEFAULT,rdata)<0) goto error;

    /* Compare data read in */
    for(i=0; i<SPACE1_DIM1; i++) {
        if(strlen(wdata[i])!=strlen(rdata[i])) {
            H5_FAILED();
            printf("VL data length don't match!, strlen(wdata[%d])=%d, strlen(rdata[%d])=%d\n",(int)i,(int)strlen(wdata[i]),(int)i,(int)strlen(rdata[i]));
            goto error;
        } /* end if */
        if( strcmp(wdata[i],rdata[i]) != 0 ) {
            H5_FAILED();
            printf("VL data values don't match!, wdata[%d]=%s, rdata[%d]=%s\n",(int)i,wdata[i],(int)i,rdata[i]);
            goto error;
        } /* end if */
    } /* end for */

    /* Close Dataset */
    if(H5Dclose(dataset)<0) goto error;

    /* Close datatype */
    if(H5Tclose(tid1)<0) goto error;
    if(H5Tclose(native_type)<0) goto error;

    /* Close disk dataspace */
    if(H5Sclose(sid1)<0) goto error;
 
    /* Free memory for rdata */
    for(i=0; i<SPACE1_DIM1; i++) {
        HDfree(rdata[i]);
    }
 
    PASSED();                                                 
    return 0;                                                 
                                                                      
error:                                                       
    return -1;         
} /* end test_vlstr_dtype() */


/*-------------------------------------------------------------------------
 * Function:	test_refer_dtype
 *
 * Purpose:	Test H5Tget_native_type for reference datatype
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		October 15, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
test_refer_dtype(hid_t file)
{

    /* Compound datatype */
    typedef struct s1_t {
        unsigned int a;
        unsigned int b;
        float c;
    } s1_t;

    hid_t		dataset;	/* Dataset ID			*/
    hid_t		group;      /* Group ID             */
    hid_t		sid1;       /* Dataspace ID			*/
    hid_t		tid1, dtype, native_type;       /* Datatype ID	*/
    hsize_t		dims1[] = {1};
    hobj_ref_t          *wbuf,      /* buffer to write to disk */
                        *rbuf;       /* buffer read from disk */

    /* Output message about test being performed */
    TESTING("reference datatype");
    
    /* Allocate write & read buffers */
    wbuf=HDmalloc(MAX(sizeof(unsigned),sizeof(hobj_ref_t)));
    rbuf=HDmalloc(MAX(sizeof(unsigned),sizeof(hobj_ref_t)));

    /* Create dataspace for datasets */
    if((sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL))<0) 
        goto error;

    /* Create a group */
    if((group=H5Gcreate(file,"Group1",(size_t)-1))<0)
        goto error;

    /* Create a datatype to refer to */
    if((tid1 = H5Tcreate (H5T_COMPOUND, sizeof(s1_t)))<0)
        goto error;

    /* Insert fields */
    if(H5Tinsert (tid1, "a", HOFFSET(s1_t,a), H5T_NATIVE_INT)<0)
        goto error;

    if(H5Tinsert (tid1, "b", HOFFSET(s1_t,b), H5T_NATIVE_INT)<0)
        goto error;

    if(H5Tinsert (tid1, "c", HOFFSET(s1_t,c), H5T_NATIVE_FLOAT)<0)
        goto error;

    /* Save datatype for later */
    if(H5Tcommit (group, "Datatype1", tid1)<0) 
        goto error;

    /* Close datatype */
    if(H5Tclose(tid1)<0)
        goto error;

    /* Close group */
    if(H5Gclose(group)<0) 
        goto error;

    /* Create a dataset */
    if((dataset=H5Dcreate(file,"Dataset3",H5T_STD_REF_OBJ,sid1,H5P_DEFAULT))<0)
        goto error;

    /* Create reference to named datatype */
    if(H5Rcreate(wbuf,file,"/Group1/Datatype1",H5R_OBJECT,-1)<0) 
        goto error;
#ifdef H5_WANT_H5_V1_4_COMPAT
    if(H5Rget_object_type(dataset,wbuf)!=H5G_TYPE)
        goto error;
#else /* H5_WANT_H5_V1_4_COMPAT */
    if(H5Rget_obj_type(dataset,H5R_OBJECT,wbuf)!=H5G_TYPE)
        goto error;
#endif /* H5_WANT_H5_V1_4_COMPAT */

    /* Write selection to disk */
    if(H5Dwrite(dataset,H5T_STD_REF_OBJ,H5S_ALL,H5S_ALL,H5P_DEFAULT,wbuf)<0)
        goto error;

    /* Close disk dataspace */
    if(H5Sclose(sid1)<0)
        goto error;
    
    /* Close Dataset */
    if(H5Dclose(dataset)<0)
        goto error;


    /* Open the dataset */
    if((dataset=H5Dopen(file,"/Dataset3"))<0)
        goto error;

    /* Get datatype for dataset */
    if((dtype = H5Dget_type(dataset))<0)
        goto error;

    /* Construct native type */
    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DEFAULT))<0)
        goto error;

    /* Check if the data type is equal */
    if(!H5Tequal(native_type, H5T_STD_REF_OBJ))
        goto error;
    
    /* Read selection from disk */
    if(H5Dread(dataset,native_type,H5S_ALL,H5S_ALL,H5P_DEFAULT,rbuf)<0)
        goto error;

    /* Open datatype object */
    if((tid1 = H5Rdereference(dataset,H5R_OBJECT,rbuf))<0)
        goto error;

    /* Verify correct datatype */
    if(H5Tget_class(tid1)!=H5T_COMPOUND)
        goto error;

    if(H5Tget_nmembers(tid1)!=3)
        goto error;

    /* Close datatype */
    if(H5Tclose(tid1)<0)
        goto error;

    /* Close Dataset */
    if(H5Dclose(dataset)<0)
        goto error;

    /* Free memory buffers */
    free(wbuf);
    free(rbuf);

    PASSED();                                                 
    return 0;                                                 
                                                                      
error:                                                       
    return -1;         
}   /* test_refer_dtype() */


/*-------------------------------------------------------------------------
 * Function:	test_opaque_dtype
 *
 * Purpose:	Test H5Tget_native_type for opaque datatype
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		October 15, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_opaque_dtype(hid_t file)
{
    hid_t		type=-1, space=-1, dset=-1;
    hid_t               dataset, dtype, native_type;
    size_t              i;
    unsigned char	wbuf[32], rbuf[32];
    hsize_t		nelmts;
    
    TESTING("opaque datatype");

    /* opaque_1 */
    nelmts = sizeof(wbuf);
    if ((type=H5Tcreate(H5T_OPAQUE, 1))<0 ||
            H5Tset_tag(type, "testing 1-byte opaque type")<0 ||
            (space=H5Screate_simple(1, &nelmts, NULL))<0 ||
            (dset=H5Dcreate(file, DSET_OPAQUE_NAME, type, space, H5P_DEFAULT))<0)
	goto error;

    for (i=0; i<sizeof(wbuf); i++)
        wbuf[i] = (unsigned char)0xff ^ (unsigned char)i;

    if (H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf)<0)
	goto error;
    if (H5Sclose(space)<0) goto error;
    if (H5Dclose(dset)<0) goto error;


    /* Open dataset again to check H5Tget_native_type */
    if((dataset=H5Dopen(file, DSET_OPAQUE_NAME))<0) goto error;

    if((dtype=H5Dget_type(dataset))<0) goto error;
    
    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DEFAULT))<0)
        goto error;

    if(!H5Tequal(native_type, type)) goto error;

    if (H5Dread(dataset, native_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf)<0)
        goto error;
    
    for(i=0; i<sizeof(rbuf); i++) {
	if (rbuf[i] != wbuf[i]) {
	    H5_FAILED();
	    printf("    Read different values than written.\n");
            printf("    At index %d\n", i);
	    goto error;
        }
    }
    
    if (H5Tclose(type)<0) goto error;
    if (H5Tclose(dtype)<0) goto error;
    if (H5Tclose(native_type)<0) goto error;
    if (H5Dclose(dataset)<0) goto error;

    PASSED();                                                 
    return 0;                                                 
                                                                      
error:                                                       
    return -1;         
} /* test_opaque_dtype */


/*-------------------------------------------------------------------------
 * Function:	test_bitfield_dtype
 *
 * Purpose:	Test H5Tget_native_type for bitfield datatype
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		October 15, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_bitfield_dtype(hid_t file)
{
    hid_t		type=-1, space=-1, dset=-1;
    hid_t               dataset, dtype, native_type;
    size_t		i;
    unsigned char	wbuf[32], rbuf[32];
    hsize_t		nelmts;
    
    TESTING("bitfield datatype");

    /* opaque_1 */
    nelmts = sizeof(wbuf);
    if ((type=H5Tcopy(H5T_STD_B8LE))<0 ||
            (space=H5Screate_simple(1, &nelmts, NULL))<0 ||
            (dset=H5Dcreate(file, DSET_BITFIELD_NAME, type, space, H5P_DEFAULT))<0)
	goto error;

    for (i=0; i<sizeof(wbuf); i++)
        wbuf[i] = (unsigned char)0xff ^ (unsigned char)i;

    if (H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf)<0)
	goto error;
    if (H5Sclose(space)<0) goto error;
    if (H5Dclose(dset)<0) goto error;


    /* Open dataset again to check H5Tget_native_type */
    if((dataset=H5Dopen(file, DSET_BITFIELD_NAME))<0) goto error;

    if((dtype=H5Dget_type(dataset))<0) goto error;
    
    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DEFAULT))<0)
        goto error;

    if(!H5Tequal(native_type, type)) goto error;

    if (H5Dread(dataset, native_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf)<0)
        goto error;
    
    for(i=0; i<sizeof(rbuf); i++) {
	if (rbuf[i] != wbuf[i]) {
	    H5_FAILED();
	    printf("    Read different values than written.\n");
            printf("    At index %d\n", i);
	    goto error;
        }
    }
    
    if (H5Tclose(type)<0) goto error;
    if (H5Tclose(dtype)<0) goto error;
    if (H5Tclose(native_type)<0) goto error;
    if (H5Dclose(dataset)<0) goto error;

    PASSED();                                                 
    return 0;                                                 
                                                                      
error:                                                       
    return -1;         
} /* test_opaque_dtype */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test H5Tget_native_type for different datatype
 *
 * Programmer:	Raymond Lu
 *		October 15, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t		file, fapl;
    int			nerrors=0;
    char		filename[1024];

    h5_reset();
    fapl = h5_fileaccess();
    
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
	goto error;

    nerrors += test_atomic_dtype(file)<0 	?1:0;
    nerrors += test_compound_dtype(file)<0 	?1:0;
    nerrors += test_compound_dtype_2(file)<0 	?1:0;
    nerrors += test_enum_dtype(file)<0 	        ?1:0;
    nerrors += test_array_dtype(file)<0 	?1:0;
    nerrors += test_vl_dtype(file)<0 	        ?1:0;
    nerrors += test_vlstr_dtype(file)<0 	?1:0;
    nerrors += test_refer_dtype(file)<0 	?1:0;
    nerrors += test_opaque_dtype(file)<0 	?1:0;
    nerrors += test_bitfield_dtype(file)<0 	?1:0;

    if (H5Fclose(file)<0) goto error;
    if (nerrors) goto error;
    printf("All native datatype tests passed.\n");
    h5_cleanup(FILENAME, fapl);
    return 0;

 error:
    nerrors = MAX(1, nerrors);
    printf("***** %d DATASET TEST%s FAILED! *****\n",
	   nerrors, 1 == nerrors ? "" : "S");
    return 1;
}
