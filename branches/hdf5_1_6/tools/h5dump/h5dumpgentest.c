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
 * Generate the binary hdf5 files for the h5dump tests.
 * Usage: just execute the program without any arguments will
 * generate all the binary hdf5 files in the local directory.
 *
 * If you regenerate the test files (e.g., changing some code,
 * trying it on a new platform, ...), you need to verify the correctness
 * of the expected output and update the corresponding *.ddl files.
 */
#include <limits.h>

#include "hdf5.h"
#include "H5private.h"

#define FILE1 "tgroup.h5"
#define FILE2 "tdset.h5"
#define FILE3 "tattr.h5"
#define FILE4 "tslink.h5"
#define FILE5 "thlink.h5"
#define FILE6 "tcompound.h5"
#define FILE7 "tall.h5"
#define FILE8 "tdset2.h5"
#define FILE9 "tcompound2.h5"
#define FILE10 "tloop.h5"
#define FILE11 "tloop2.h5"
#define FILE12 "tmany.h5"
#define FILE13 "tstr.h5"
#define FILE14 "tstr2.h5"
#define FILE15 "tenum.h5"
#define FILE16 "tobjref.h5"
#define FILE17 "tdatareg.h5"
#define FILE18 "tnestedcomp.h5"
#define FILE19 "topaque.h5"
#define FILE20 "tbitfields.h5"
#define FILE21 "tvldtypes1.h5"
#define FILE22 "tvldtypes2.h5"
#define FILE23 "tvldtypes3.h5"
#define FILE24 "tvldtypes4.h5"
#define FILE25 "tarray1.h5"
#define FILE26 "tarray2.h5"
#define FILE27 "tarray3.h5"
#define FILE28 "tarray4.h5"
#define FILE29 "tarray5.h5"
#define FILE30 "tarray6.h5"
#define FILE31 "tarray7.h5"
#define FILE32 "tempty.h5"
#define FILE33  "tgrp_comments.h5"
#define FILE34  "tsplit_file"
#define FILE35  "tfamily%05d.h5"
#define FILE36  "tmulti"
#define FILE37  "tlarge_objname.h5"
#define FILE38  "tvlstr.h5"
#define FILE39  "tchar.h5"
#define FILE40  "tattr2.h5"
#define FILE41  "tcompound_complex.h5"
#define FILE42  "tnamed_dtype_attr.h5"
#define FILE43  "tvldtypes5.h5"
#define FILE44  "tfilters.h5"
#define FILE46  "tfcontents1.h5"
#define FILE47  "tfcontents2.h5"
#define FILE48  "tfdriver1.h5"
#define FILE49  "tfdriver2.h5"


/*-------------------------------------------------------------------------
 * prototypes
 *-------------------------------------------------------------------------
 */

/* utility functions */
static int 
write_attr(hid_t loc_id, int rank, hsize_t *dims, const char *attr_name,
                hid_t type_id, void *buf);
static int 
write_dset( hid_t loc_id, int rank, hsize_t *dims, const char *dset_name,
                   hid_t type_id, void *buf );


/* a filter operation callback function */
static size_t
myfilter(unsigned int UNUSED flags, size_t UNUSED cd_nelmts, 
      const unsigned int UNUSED *cd_values, size_t nbytes,
      size_t UNUSED *buf_size, void UNUSED **buf);

/* a "set local" callback     */
static herr_t
set_local_myfilter(hid_t dcpl_id, hid_t type_id, hid_t UNUSED space_id);

#define MYFILTER_ID 405

/* This message derives from H5Z */
const H5Z_class_t H5Z_MYFILTER[1] = {{
    MYFILTER_ID,         /* Filter id number  */
    "myfilter",          /* Filter name for debugging */
    NULL,                /* The "can apply" callback     */
    set_local_myfilter,  /* The "set local" callback     */
    myfilter,            /* The actual filter function */
}};



#define LENSTR  50
#define LENSTR2  11

#define SPACE2_RANK 2
#define SPACE2_DIM1 10
#define SPACE2_DIM2 10

#define SPACE1_RANK 1
#define SPACE1_DIM1 4

#define DIM1  20
#define DIM2  10
#define CDIM1 DIM1/2
#define CDIM2 DIM2/2
#define RANK  2

/* Element selection information */
#define POINT1_NPOINTS 10

typedef enum{
     RED,
     GREEN,
     BLUE,
     WHITE,
     BLACK
} enumtype;

/* Compound datatype */
typedef struct s1_t {
    unsigned int a;
    unsigned int b;
    float c;
} s1_t;


/* 1-D array datatype */
#define ARRAY1_RANK 1
#define ARRAY1_DIM1 4

/* 3-D array datatype */
#define ARRAY2_RANK 3
#define ARRAY2_DIM1 3
#define ARRAY2_DIM2 4
#define ARRAY2_DIM3 5

/* 2-D array datatype */
#define ARRAY3_RANK 2
#define ARRAY3_DIM1 6
#define ARRAY3_DIM2 3

/* VL string datatype name */
#define VLSTR_TYPE      "vl_string_type"

/* "File 41" macros */
/* Name of dataset to create in datafile                              */
#define F41_DATASETNAME   "CompoundComplex"
/* Dataset dimensions                                                 */
#define F41_LENGTH         6
#define F41_RANK           1
#define F41_ARRAY_RANK     1
#define F41_ARRAY_RANKd    2
#define F41_DIMb           4 
#define F41_ARRAY_DIMc     6 
#define F41_ARRAY_DIMd1    5 
#define F41_ARRAY_DIMd2    6 
#define F41_ARRAY_DIMf     10 

/* "File 42" macros */
/* Name of dataset to create in datafile                              */
#define F42_DSETNAME "Dataset"
#define F42_TYPENAME "Datatype"
#define F42_ATTRNAME "Attribute"

/* "File 43" macros */
/* Name of dataset to create in datafile                              */
#define F43_DSETNAME "Dataset"

static void gent_group(void)
{
    hid_t fid, group;
  
    fid = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  
    /* / */
    group = H5Gcreate (fid, "/g1", 0);
    H5Gclose(group);
    group = H5Gcreate (fid, "/g2", 0);
    H5Gclose(group);
    group = H5Gcreate (fid, "/g3", 0);
    H5Gclose(group);
  
    /* /g1 */
    group = H5Gcreate (fid, "/g1/g1.1", 0);
    H5Gclose(group);
    group = H5Gcreate (fid, "/g1/g1.2", 0);
    H5Gclose(group);
  
    /* /g2 */
    group = H5Gcreate (fid, "/g2/g2.1", 0);
    H5Gclose(group);
  
    /* /g3 */
    group = H5Gcreate (fid, "/g3/g3.1", 0);
    H5Gclose(group);
    group = H5Gcreate (fid, "/g3/g3.2", 0);
    H5Gclose(group);
    group = H5Gcreate (fid, "/g3/g3.3", 0);
    H5Gclose(group);
    group = H5Gcreate (fid, "/g3/g3.4", 0);
    H5Gclose(group);
  
    /* /g2/g2.1 */
    group = H5Gcreate (fid, "/g2/g2.1/g2.1.1", 0);
    H5Gclose(group);
    group = H5Gcreate (fid, "/g2/g2.1/g2.1.2", 0);
    H5Gclose(group);
    group = H5Gcreate (fid, "/g2/g2.1/g2.1.3", 0);
    H5Gclose(group);
  
    H5Fclose(fid);
}

static void gent_dataset(void)
{
    hid_t fid, dataset, space;
    hsize_t dims[2];
    int dset1[10][20];
    double dset2[30][20];
    int i, j;
  
    fid = H5Fcreate(FILE2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  
    /* dset1 */
    dims[0] = 10; dims[1] = 20;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate(fid, "/dset1", H5T_STD_I32BE, space, H5P_DEFAULT);

    for (i = 0; i < 10; i++)
         for (j = 0; j < 20; j++)
              dset1[i][j] = j+i;

    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);
    H5Sclose(space);
    H5Dclose(dataset);
  
    /* dset2 */
    dims[0] = 30; dims[1] = 20;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate(fid, "/dset2", H5T_IEEE_F64BE, space, H5P_DEFAULT);

    for (i = 0; i < 30; i++)
         for (j = 0; j < 20; j++)
              dset2[i][j] = 0.0001*j+i;

    H5Dwrite(dataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);
  
    H5Sclose(space);
    H5Dclose(dataset);
    H5Fclose(fid);
}

static void gent_dataset2(void)
{
    hid_t fid, dataset, space, create_plist;
    hsize_t dims[2];
    hsize_t maxdims[2];
    int dset1[10][20];
    double dset2[30][10];
    int i, j;
  
    fid = H5Fcreate(FILE8, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    create_plist = H5Pcreate(H5P_DATASET_CREATE);
    dims[0] = 5; dims[1] = 5;
    H5Pset_chunk(create_plist, 2, dims);
  
    /* dset1 */
    dims[0] = 10; dims[1] = 20;
    maxdims[0] = H5S_UNLIMITED; maxdims[1] = 20;
    space = H5Screate_simple(2, dims,  maxdims);
    dataset = H5Dcreate(fid, "/dset1", H5T_STD_I32BE, space, create_plist);

    for (i = 0; i < 10; i++)
         for (j = 0; j < 20; j++)
              dset1[i][j] = j;

    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);
    H5Sclose(space);
    H5Dclose(dataset);
  
    /* dset2 */
    dims[0] = 30; dims[1] = 10;
    maxdims[0] = 30; maxdims[1] = H5S_UNLIMITED;
    space = H5Screate_simple(2, dims, maxdims);
    dataset = H5Dcreate(fid, "/dset2", H5T_IEEE_F64BE, space, create_plist);

    for (i = 0; i < 30; i++)
         for (j = 0; j < 10; j++)
              dset2[i][j] = j;

    H5Dwrite(dataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);

    H5Sclose(space);
    H5Dclose(dataset);
    H5Pclose(create_plist);
    H5Fclose(fid);
}


static void gent_attribute(void)
{
    hid_t fid, root, space, attr, type;
    hsize_t dims[2];
    char buf[60];
    int i, data[10];
    double d[10];
    char string[]= "string attribute";
    int point = 100;
  
    fid = H5Fcreate(FILE3, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    root = H5Gopen (fid, "/");
  
    /* attribute 1 */
    dims[0] = 24;
    space = H5Screate_simple(1, dims, NULL);
    attr = H5Acreate (root, "attr1", H5T_STD_I8BE, space, H5P_DEFAULT);
    sprintf(buf, "attribute of root group");
    H5Awrite(attr, H5T_NATIVE_SCHAR, buf);
    H5Sclose(space);
    H5Aclose(attr);
  
    /* attribute 2 */
    dims[0] = 10;
    space = H5Screate_simple(1, dims, NULL);
    attr = H5Acreate (root, "attr2", H5T_STD_I32BE, space, H5P_DEFAULT);

    for (i = 0; i < 10; i++) data[i] = i+1;

    H5Awrite(attr, H5T_NATIVE_INT, data);
    H5Sclose(space);
    H5Aclose(attr);
  
    /* attribute 3 */
    dims[0] = 10;
    space = H5Screate_simple(1, dims, NULL);
    attr = H5Acreate (root, "attr3", H5T_IEEE_F64BE, space, H5P_DEFAULT);

    for (i = 0; i < 10; i++) d[i] = 0.1 * i;

    H5Awrite(attr, H5T_NATIVE_DOUBLE, d);
    H5Sclose(space);
    H5Aclose(attr);
  
    /* attribute 4 */
    space = H5Screate(H5S_SCALAR);
    attr = H5Acreate (root, "attr4", H5T_STD_I32BE, space, H5P_DEFAULT);
    H5Awrite(attr, H5T_NATIVE_INT, &point);
    H5Sclose(space);
    H5Aclose(attr);
  
    /* attribute 5 */
    space = H5Screate(H5S_SCALAR);
    type = H5Tcopy(H5T_C_S1);
    H5Tset_size(type, 17);
    attr = H5Acreate (root, "attr5", type, space, H5P_DEFAULT);
    H5Awrite(attr, type, string);

    H5Tclose(type);
    H5Sclose(space);
    H5Aclose(attr);
    H5Gclose(root);
    H5Fclose(fid);
}

static void gent_softlink(void)
{
    hid_t fid, root;
  
    fid = H5Fcreate(FILE4, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    root = H5Gopen (fid, "/");
    H5Glink (root, H5G_LINK_SOFT, "somevalue", "slink1");
    H5Glink (root, H5G_LINK_SOFT, "linkvalue", "slink2");

    H5Gclose(root);
    H5Fclose(fid);
}

/*
            /

       /    |   \      the dataset is hardlinked to three names
                       /dset1, /g1/dset2, and /g1/g1.1/dset3
     dset1 g1    g2
                       /g2 and /g1/g1.1 are hardlinked to the same object.
          /  \
       dset2 g1.1
              |
             dset3
*/

static void gent_hardlink(void)
{
    hid_t fid, group, dataset, space;
    hsize_t dim = 5;
    int i, dset[5];
  
    fid = H5Fcreate(FILE5, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  
    space = H5Screate_simple(1, &dim, NULL);
    dataset = H5Dcreate(fid, "/dset1", H5T_STD_I32BE, space, H5P_DEFAULT);

    for (i = 0; i < 5; i++) dset[i] = i;

    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset);
    H5Sclose(space);
    H5Dclose(dataset);
  
    group = H5Gcreate (fid, "/g1", 0);
    H5Glink (group, H5G_LINK_HARD, "/dset1", "dset2");
    H5Gclose(group);
  
    group = H5Gcreate (fid, "/g2", 0);
    H5Glink (group, H5G_LINK_HARD, "/dset1", "dset3");
    H5Gclose(group);
  
    group = H5Gopen(fid, "/g1");
    H5Glink (group, H5G_LINK_HARD, "/g2", "g1.1");
    H5Gclose(group);
    H5Fclose(fid);
}

/*
               /
     /     |       \     \
   dset1  group1  type1 type2
           |
          dset2

*/
static void gent_compound_dt(void) {       /* test compound data type */
    hid_t fid, group, dataset, space, space3, type, type2;
    hid_t array_dt;
    typedef struct {
      int a;
      float b;
      double c;
    } dset1_t;
    dset1_t dset1[5];

    typedef struct {
      int a;
      float b;
    } dset2_t;
    dset2_t dset2[5];

    typedef struct {
      int a[4];
      float b[5][6];
    } dset3_t;
    dset3_t dset3[3][6];

    typedef struct {
      int a;
      float b;
    } dset4_t;
    dset4_t dset4[5];

    typedef struct {
      int a;
      float b;
    } dset5_t;
    dset5_t dset5[5];

    int i, j, k, l, ndims;
    hsize_t dim[2];

    hsize_t sdim = 5;
    hsize_t dset3_dim[2];

  
  for (i = 0; i < (int)sdim; i++) {
       dset1[i].a = i; 
       dset1[i].b = (float)(i*i);
       dset1[i].c = (float)(1./(i+1));
       
       dset2[i].a = i;
       dset2[i].b = (float)(i+ i*0.1);

       dset4[i].a = i;
       dset4[i].b = (float)(i+3);

       dset5[i].a = i;
       dset5[i].b = (float)(i*0.1);
  }


  fid = H5Fcreate(FILE6, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

  space = H5Screate_simple(1, &sdim, NULL);

  type = H5Tcreate (H5T_COMPOUND, sizeof(dset1[0]));
  type2 = H5Tcreate(H5T_COMPOUND, sizeof(dset1[0])); 
  H5Tinsert(type, "a_name", HOFFSET(dset1_t, a), H5T_STD_I32BE);
  H5Tinsert(type, "b_name", HOFFSET(dset1_t, b), H5T_IEEE_F32BE);
  H5Tinsert(type, "c_name", HOFFSET(dset1_t, c), H5T_IEEE_F64BE);
  H5Tinsert(type2, "a_name", HOFFSET(dset1_t, a), H5T_NATIVE_INT);
  H5Tinsert(type2, "b_name", HOFFSET(dset1_t, b), H5T_NATIVE_FLOAT);
  H5Tinsert(type2, "c_name", HOFFSET(dset1_t, c), H5T_NATIVE_DOUBLE);
  dataset = H5Dcreate(fid, "/dset1", type, space, H5P_DEFAULT);
  H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);
  H5Tclose(type2);
  H5Tclose(type);
  H5Dclose(dataset);

  /* shared data type 1 */
  type = H5Tcreate (H5T_COMPOUND, sizeof(dset2_t));
  H5Tinsert(type, "int_name", HOFFSET(dset2_t, a), H5T_STD_I32BE);
  H5Tinsert(type, "float_name", HOFFSET(dset2_t, b), H5T_IEEE_F32BE);
  H5Tcommit(fid, "type1", type);
  type2 = H5Tcreate (H5T_COMPOUND, sizeof(dset2_t));
  H5Tinsert(type2, "int_name", HOFFSET(dset2_t, a), H5T_NATIVE_INT);
  H5Tinsert(type2, "float_name", HOFFSET(dset2_t, b), H5T_NATIVE_FLOAT);
  group = H5Gcreate (fid, "/group1", 0);

  dataset = H5Dcreate(group, "dset2", type, space, H5P_DEFAULT);
  H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);
  H5Tclose(type2);
  H5Tclose(type);
  H5Dclose(dataset);


  /* shared data type 2 */
  type = H5Tcreate (H5T_COMPOUND, sizeof(dset3_t));
  type2 = H5Tcreate (H5T_COMPOUND, sizeof(dset3_t));

  ndims = 1; dim[0] = 4;

  array_dt=H5Tarray_create(H5T_STD_I32BE,ndims,dim,NULL);
  H5Tinsert(type, "int_array", HOFFSET(dset3_t, a), array_dt);
  H5Tclose(array_dt);

  array_dt=H5Tarray_create(H5T_NATIVE_INT,ndims,dim,NULL);
  H5Tinsert(type2, "int_array", HOFFSET(dset3_t, a), array_dt);
  H5Tclose(array_dt);

  ndims = 2; dim[0] = 5; dim[1] = 6;

  array_dt=H5Tarray_create(H5T_IEEE_F32BE,ndims,dim,NULL);
  H5Tinsert(type, "float_array", HOFFSET(dset3_t, b), array_dt);
  H5Tclose(array_dt);

  array_dt=H5Tarray_create(H5T_NATIVE_FLOAT,ndims,dim,NULL);
  H5Tinsert(type2, "float_array", HOFFSET(dset3_t, b), array_dt);
  H5Tclose(array_dt);

  H5Tcommit(fid, "type2", type);


  dset3_dim[0] = 3;  dset3_dim[1] = 6;
  space3 = H5Screate_simple(2, dset3_dim, NULL);
  dataset = H5Dcreate(group, "dset3", type, space3, H5P_DEFAULT);
  for (i = 0; i < (int)dset3_dim[0]; i++) {
       for (j = 0; j < (int)dset3_dim[1]; j++) {
            for (k = 0; k < 4; k++)
                 dset3[i][j].a[k] = k+j+i;
            for (k = 0; k < 5; k++)
                 for (l = 0; l < 6; l++)
                      dset3[i][j].b[k][l] = (float)((k+1)+l+j+i);
       }
  }
  H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset3);
  H5Sclose(space3);
  H5Tclose(type);
  H5Tclose(type2);
  H5Dclose(dataset);

  /* shared data type 3 */
  type = H5Tcreate (H5T_COMPOUND, sizeof(dset4_t));
  type2 = H5Tcreate (H5T_COMPOUND, sizeof(dset4_t));
  H5Tinsert(type, "int", HOFFSET(dset4_t, a), H5T_STD_I32BE);
  H5Tinsert(type, "float", HOFFSET(dset4_t, b), H5T_IEEE_F32BE);
  H5Tcommit(group, "type3", type);
  H5Tinsert(type2, "int", HOFFSET(dset4_t, a), H5T_NATIVE_INT);
  H5Tinsert(type2, "float", HOFFSET(dset4_t, b), H5T_NATIVE_FLOAT);
  dataset = H5Dcreate(group, "dset4", type, space, H5P_DEFAULT);
  H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset4);

  H5Tclose(type);
  H5Tclose(type2);
  H5Dclose(dataset);
  H5Gclose(group);


  /* unamed data type */
  group = H5Gcreate (fid, "/group2", 0);

  type = H5Tcreate (H5T_COMPOUND, sizeof(dset5_t));
  H5Tinsert(type, "int", HOFFSET(dset5_t, a), H5T_STD_I32BE);
  H5Tinsert(type, "float", HOFFSET(dset5_t, b), H5T_IEEE_F32BE);
  H5Tcommit(group, "type4", type);
  type2 = H5Tcreate (H5T_COMPOUND, sizeof(dset5_t));
  H5Tinsert(type2, "int", HOFFSET(dset5_t, a), H5T_NATIVE_INT);
  H5Tinsert(type2, "float", HOFFSET(dset5_t, b), H5T_NATIVE_FLOAT);
  dataset = H5Dcreate(group, "dset5", type, space, H5P_DEFAULT);
  H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset5);

  H5Gunlink(group,"type4");

  H5Tclose(type);
  H5Tclose(type2);
  H5Dclose(dataset);
  H5Sclose(space);
  H5Gclose(group);

  H5Fclose(fid);

}

/*
               /
     /     |       \     \
   dset1  group1  type1 type2
           |
          dset2

*/
static void gent_compound_dt2(void) {       /* test compound data type */
    hid_t fid, group, dataset, space, type, create_plist, type2;
    hid_t array_dt;

    typedef struct {
      int a;
      float b;
      double c;
    } dset1_t;
    dset1_t dset1[10];

    typedef struct {
      int a;
      float b;
    } dset2_t;
    dset2_t dset2[10];

    typedef struct {
      int a[4];
      float b[5][6];
    } dset3_t;

    typedef struct {
      int a;
      float b;
    } dset4_t;
    dset4_t dset4[10];

    typedef struct {
      int a;
      float b;
    } dset5_t;
    dset5_t dset5[10];

    int i, ndims;
    const int perm[2]={0,1};
    hsize_t dim[2];

    hsize_t sdim, maxdim;

  sdim = 10;
  for (i = 0; i < (int)sdim; i++) {
       dset1[i].a = i; 
       dset1[i].b = (float)(i*i);
       dset1[i].c = (float)(1./(i+1));
       
       dset2[i].a = i;
       dset2[i].b = (float)(i+ i*0.1);

       dset4[i].a = i;
       dset4[i].b = (float)(i*1.0);

       dset5[i].a = i;
       dset5[i].b = (float)(i*1.0);
  }

  fid = H5Fcreate(FILE9, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

  create_plist = H5Pcreate(H5P_DATASET_CREATE);

  sdim = 2;
  H5Pset_chunk(create_plist, 1, &sdim);

  sdim = 6;
  maxdim = H5S_UNLIMITED;

  space = H5Screate_simple(1, &sdim, &maxdim);

  type = H5Tcreate (H5T_COMPOUND, sizeof(dset1[0]));
  
  H5Tinsert(type, "a_name", HOFFSET(dset1_t, a), H5T_STD_I32BE);
  H5Tinsert(type, "b_name", HOFFSET(dset1_t, b), H5T_IEEE_F32BE);
  H5Tinsert(type, "c_name", HOFFSET(dset1_t, c), H5T_IEEE_F64BE);

  dataset = H5Dcreate(fid, "/dset1", type, space, create_plist);

  type2 = H5Tcreate (H5T_COMPOUND, sizeof(dset1[0]));
  
  H5Tinsert(type2, "a_name", HOFFSET(dset1_t, a), H5T_NATIVE_INT);
  H5Tinsert(type2, "b_name", HOFFSET(dset1_t, b), H5T_NATIVE_FLOAT);
  H5Tinsert(type2, "c_name", HOFFSET(dset1_t, c), H5T_NATIVE_DOUBLE);

  H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);

  H5Tclose(type);
  H5Tclose(type2);
  H5Sclose(space);
  H5Dclose(dataset);

  sdim = 6;
  maxdim = 10;

  space = H5Screate_simple(1, &sdim, &maxdim);

  /* shared data type 1 */
  type = H5Tcreate (H5T_COMPOUND, sizeof(dset2_t));
  H5Tinsert(type, "int_name", HOFFSET(dset2_t, a), H5T_STD_I32BE);
  H5Tinsert(type, "float_name", HOFFSET(dset2_t, b), H5T_IEEE_F32BE);
  H5Tcommit(fid, "type1", type);

  group = H5Gcreate (fid, "/group1", 0);

  dataset = H5Dcreate(group, "dset2", type, space, create_plist);

  type2 = H5Tcreate (H5T_COMPOUND, sizeof(dset2_t));
  H5Tinsert(type2, "int_name", HOFFSET(dset2_t, a), H5T_NATIVE_INT);
  H5Tinsert(type2, "float_name", HOFFSET(dset2_t, b), H5T_NATIVE_FLOAT); 
  H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);

  H5Tclose(type);
  H5Tclose(type2);
  H5Dclose(dataset);


  /* shared data type 2 */
  type = H5Tcreate (H5T_COMPOUND, sizeof(dset3_t));

  ndims = 1; dim[0] = 4;
  array_dt=H5Tarray_create(H5T_STD_I32BE,ndims,dim,perm);
  H5Tinsert(type, "int_array", HOFFSET(dset3_t, a), array_dt);
  H5Tclose(array_dt);

  ndims = 2; dim[0] = 5; dim[1] = 6;
  array_dt=H5Tarray_create(H5T_IEEE_F32BE,ndims,dim,perm);
  H5Tinsert(type, "float_array", HOFFSET(dset3_t, b), array_dt);
  H5Tclose(array_dt);

  H5Tcommit(fid, "type2", type);
  H5Tclose(type);

  /* shared data type 3 */
  type = H5Tcreate (H5T_COMPOUND, sizeof(dset4_t));
  H5Tinsert(type, "int", HOFFSET(dset4_t, a), H5T_STD_I32BE);
  H5Tinsert(type, "float", HOFFSET(dset4_t, b), H5T_IEEE_F32BE);
  H5Tcommit(group, "type3", type);

  dataset = H5Dcreate(group, "dset4", type, space, create_plist);

  type2 = H5Tcreate (H5T_COMPOUND, sizeof(dset4_t));
  H5Tinsert(type2, "int", HOFFSET(dset4_t, a), H5T_NATIVE_INT);
  H5Tinsert(type2, "float", HOFFSET(dset4_t, b), H5T_NATIVE_FLOAT);
  H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset4);

  H5Tclose(type);
  H5Tclose(type2);
  H5Dclose(dataset);
  H5Gclose(group);


  /* unamed data type */
  group = H5Gcreate (fid, "/group2", 0);

  type = H5Tcreate (H5T_COMPOUND, sizeof(dset5_t));
  H5Tinsert(type, "int", HOFFSET(dset5_t, a), H5T_STD_I32BE);
  H5Tinsert(type, "float", HOFFSET(dset5_t, b), H5T_IEEE_F32BE);
  H5Tcommit(group, "type4", type);
  dataset = H5Dcreate(group, "dset5", type, space, create_plist);
  type2 = H5Tcreate (H5T_COMPOUND, sizeof(dset5_t));
  H5Tinsert(type2, "int", HOFFSET(dset5_t, a), H5T_NATIVE_INT);
  H5Tinsert(type2, "float", HOFFSET(dset5_t, b), H5T_NATIVE_FLOAT);
  H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset5);

  H5Gunlink(group,"type4");

  H5Tclose(type);
  H5Tclose(type2);
  H5Dclose(dataset);
  H5Sclose(space);
  H5Gclose(group);
  H5Pclose(create_plist);

  H5Fclose(fid);

}


/*

/ : g1  g2  attr1  attr2
g1 : g1.1  g1.2
g1.1 : dset1.1.1(attr1, attr2)   dset1.1.2
g1.2 : g1.2.1
g1.2.1 : slink
g2 : dset2.1  dset2.2

*/

static void gent_all(void) {
hid_t fid, group, attr, dataset, space;
hsize_t dims[2];
int data[2][2], dset1[10][10], dset2[20];
char buf[60];
int i, j;
float dset2_1[10], dset2_2[3][5];

  fid = H5Fcreate(FILE7, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

  /* create groups */
  group = H5Gcreate (fid, "/g1", 0);
  H5Gclose(group);

  group = H5Gcreate (fid, "/g2", 0);
  H5Gclose(group);

  group = H5Gcreate (fid, "/g1/g1.1", 0);
  H5Gclose(group);

  group = H5Gcreate (fid, "/g1/g1.2", 0);
  H5Gclose(group);

  group = H5Gcreate (fid, "/g1/g1.2/g1.2.1", 0);
  H5Gclose(group);

  /* root attributes */
  group = H5Gopen (fid, "/");

  dims[0] = 10;
  space = H5Screate_simple(1, dims, NULL);
  attr = H5Acreate (group, "attr1", H5T_STD_I8BE, space, H5P_DEFAULT);
  sprintf(buf, "abcdefghi");
  H5Awrite(attr, H5T_NATIVE_SCHAR, buf);
  H5Sclose(space);
  H5Aclose(attr);

  dims[0] = 2; dims[1] = 2;
  space = H5Screate_simple(2, dims, NULL);
  attr = H5Acreate (group, "attr2", H5T_STD_I32BE, space, H5P_DEFAULT);
  data[0][0] = 0; data[0][1] = 1; data[1][0] = 2; data[1][1] = 3;
  H5Awrite(attr, H5T_NATIVE_INT, data);
  H5Sclose(space);
  H5Aclose(attr);

  H5Gclose(group);

  group = H5Gopen (fid, "/g1/g1.1");

  /* dset1.1.1 */
  dims[0] = 10; dims[1] = 10;
  space = H5Screate_simple(2, dims, NULL);
  dataset = H5Dcreate(group, "dset1.1.1", H5T_STD_I32BE, space, H5P_DEFAULT);
  for (i = 0; i < 10; i++)
       for (j = 0; j < 10; j++)
            dset1[i][j] = j*i;
  H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);
  H5Sclose(space);

  /* attributes of dset1.1.1 */
  dims[0] = 27;
  space = H5Screate_simple(1, dims, NULL);
  attr = H5Acreate (dataset, "attr1", H5T_STD_I8BE, space, H5P_DEFAULT);
  sprintf(buf, "1st attribute of dset1.1.1");
  H5Awrite(attr, H5T_NATIVE_SCHAR, buf);
  H5Sclose(space);
  H5Aclose(attr);

  dims[0] = 27;
  space = H5Screate_simple(1, dims, NULL);
  attr = H5Acreate (dataset, "attr2", H5T_STD_I8BE, space, H5P_DEFAULT);
  sprintf(buf, "2nd attribute of dset1.1.1");
  H5Awrite(attr, H5T_NATIVE_SCHAR, buf);
  H5Sclose(space);
  H5Aclose(attr);

  H5Dclose(dataset);

  /* dset1.1.2 */
  dims[0] = 20;
  space = H5Screate_simple(1, dims, NULL);
  dataset = H5Dcreate(group, "dset1.1.2", H5T_STD_I32BE, space, H5P_DEFAULT);
  for (i = 0; i < 20; i++)
       dset2[i] = i;
  H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);
  H5Sclose(space);
  H5Dclose(dataset);

  H5Gclose(group);

  /* soft link */
  group = H5Gopen (fid, "/g1/g1.2/g1.2.1");
  H5Glink (group, H5G_LINK_SOFT, "somevalue", "slink");
  H5Gclose(group);

  group = H5Gopen (fid, "/g2");

  /* dset2.1 */
  dims[0] = 10;
  space = H5Screate_simple(1, dims, NULL);
  dataset = H5Dcreate(group, "dset2.1", H5T_IEEE_F32BE, space, H5P_DEFAULT);
  for (i = 0; i < 10; i++)
       dset2_1[i] = (float)(i*0.1+1);
  H5Dwrite(dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2_1);
  H5Sclose(space);
  H5Dclose(dataset);
 
  /* dset2.2 */
  dims[0] = 3; dims[1] = 5;
  space = H5Screate_simple(2, dims, NULL);
  dataset = H5Dcreate(group, "dset2.2", H5T_IEEE_F32BE, space, H5P_DEFAULT);
  for (i = 0; i < 3; i++)
       for (j = 0; j < 5; j++)
            dset2_2[i][j] = (float)((i+1)*j*0.1);
  H5Dwrite(dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2_2);
  H5Sclose(space);
  H5Dclose(dataset);

  H5Gclose(group);

  H5Fclose(fid);

}

/*
            o
          /___\
      g1 o/   \o g2
          \___/  

   
o - group objects

*/

static void gent_loop(void) {
hid_t fid, group;

  fid = H5Fcreate(FILE10, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

  group = H5Gcreate (fid, "/g1", 0);
  H5Gclose(group);
  group = H5Gcreate (fid, "/g2", 0);
  H5Gclose(group);

  H5Glink(fid, H5G_LINK_HARD, "/g2", "/g1/g1.1");
  H5Glink(fid, H5G_LINK_HARD, "/g1", "/g2/g2.1");

  H5Fclose(fid);
}

static void gent_loop2(void) {
hid_t fid, group;

  fid = H5Fcreate(FILE11, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

  /* create group object g1 and implcit path from root object */
  group = H5Gcreate (fid, "/g1", 0);
  H5Gclose(group);

  /* create group object g2 and implcit path from root object */
  group = H5Gcreate (fid, "/g2", 0);
  H5Gclose(group);

  /* create path from object at /g1 to object at /g2 and name it g1.1 */
  H5Glink (fid, H5G_LINK_HARD, "/g2", "/g1/g1.1"); 

  /* create path from object at /g2 to object at /g1 and name it g2.1 */
  H5Glink (fid, H5G_LINK_SOFT, "/g1", "/g2/g2.1");

  H5Fclose(fid);

}

/*
                  /
     |       |       |   \    \    \
     g1     g2      g3   g4   g5    g6
    / \      |       |    \     \    \
 g1.1 g1.2 slink2  link3 dset2 slink4 dset3
  |    |    (g1)  (dset2)      (dset3)
 dset1 link1
      (dset1)
*/

static void gent_many(void) {
    hid_t fid, group, attr, dataset, space, space2, type, create_plist, type2;
    hid_t array_dt;
    hsize_t dims[2];
    int data[2][2], dset2[10][10], dset3[10][10];
    double d[10];

    char buf[60];
    int i, j;
    int i0, i1, i2, i3;
    hsize_t sdim, maxdim;

    typedef struct { /* compound type has members with rank > 1 */
      int a[2][2][2][2]; /* arrays are 2x2x2x2    */
      double b[2][2][2][2];
      double c[2][2][2][2];
    } dset1_t;
    dset1_t dset1[6];

    hsize_t dim[4];
    int idx[4] = {0,1,2,3};  /* normal indicies */
    const int perm[4] = {0,1,2,3};  /* the 0'th and the 3'rd indices are permuted */

  fid = H5Fcreate(FILE12, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

  group = H5Gcreate (fid, "/g1", 0);
  H5Gclose(group);

  create_plist = H5Pcreate(H5P_DATASET_CREATE);

  sdim = 2;
  H5Pset_chunk(create_plist, 1, &sdim);

  group = H5Gcreate (fid, "/g1/g1.1", 0);

  type = H5Tcreate (H5T_COMPOUND, sizeof(dset1[0]));

  dim[0] = dim[1] = dim[2] = dim[3] = 2;
  array_dt=H5Tarray_create(H5T_STD_I32BE,4,dim,perm);
  H5Tinsert(type, "a_array", HOFFSET(dset1_t, a), array_dt);
  H5Tclose(array_dt);

  array_dt=H5Tarray_create(H5T_IEEE_F64BE,4,dim,perm);
  H5Tinsert(type, "b_array", HOFFSET(dset1_t, b), array_dt);
  H5Tclose(array_dt);

  array_dt=H5Tarray_create(H5T_IEEE_F64BE,4,dim,perm);
  H5Tinsert(type, "c_array", HOFFSET(dset1_t, c), array_dt);
  H5Tclose(array_dt);

  type2 = H5Tcreate (H5T_COMPOUND, sizeof(dset1[0]));

  array_dt=H5Tarray_create(H5T_NATIVE_INT,4,dim,perm);
  H5Tinsert(type2, "a_array", HOFFSET(dset1_t, a), array_dt);
  H5Tclose(array_dt);

  array_dt=H5Tarray_create(H5T_NATIVE_DOUBLE,4,dim,perm);
  H5Tinsert(type2, "b_array", HOFFSET(dset1_t, b), array_dt);
  H5Tclose(array_dt);

  array_dt=H5Tarray_create(H5T_NATIVE_DOUBLE,4,dim,perm);
  H5Tinsert(type2, "c_array", HOFFSET(dset1_t, c), array_dt);
  H5Tclose(array_dt);


  /* dset1 */
  sdim = 6;
  maxdim = H5S_UNLIMITED;
  space = H5Screate_simple(1, &sdim, &maxdim);
  dataset = H5Dcreate(group, "dset1", type, space, create_plist);

  /* add attributes to dset1 */
  dims[0] = 10;
  space2 = H5Screate_simple(1, dims, NULL);
  attr = H5Acreate (dataset, "attr1", H5T_STD_I8BE, space2, H5P_DEFAULT);
  sprintf(buf, "abcdefghi");
  H5Awrite(attr, H5T_NATIVE_CHAR, buf);
  H5Sclose(space2);
  H5Aclose(attr);

  dims[0] = 2; dims[1] = 2;
  space2 = H5Screate_simple(2, dims, NULL);
  attr = H5Acreate (dataset, "attr2", H5T_STD_I32BE, space2, H5P_DEFAULT);
  data[0][0] = 0; data[0][1] = 1; data[1][0] = 2; data[1][1] = 3;
  H5Awrite(attr, H5T_NATIVE_INT, data);
  H5Sclose(space2);
  H5Aclose(attr);

  dims[0] = 10;
  space2 = H5Screate_simple(1, dims, NULL);
  attr = H5Acreate (dataset, "attr3", H5T_IEEE_F64BE, space2, H5P_DEFAULT);
  for (i = 0; i < 10; i++) d[i] = 0.1 * i;
  H5Awrite(attr, H5T_NATIVE_DOUBLE, d);
  H5Sclose(space2);
  H5Aclose(attr);

  for (j=0; j<(int)sdim; j++) {
 for (i3 = 0; i3 < 2; i3++) {
  idx[perm[3]] = i3;
 for (i2 = 0; i2 < 2; i2++) {
  idx[perm[2]] = i2;
 for (i1 = 0; i1 < 2; i1++) {
  idx[perm[1]] = i1;
 for (i0 = 0; i0 < 2; i0++) {
  idx[perm[0]] = i0;
  
  dset1[j].a[idx[3]][idx[2]][idx[1]][idx[0]] = i0+j;
  dset1[j].b[idx[3]][idx[2]][idx[1]][idx[0]] = (double)(i0+j);
#ifdef WIN32
  dset1[j].c[idx[3]][idx[2]][idx[1]][idx[0]] = (double)(i0+j+(signed __int64)sdim);
#else
  dset1[j].c[idx[3]][idx[2]][idx[1]][idx[0]] = (double)(i0+j+sdim);
#endif
 }
 }
 }
 }
  }

  H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);

  H5Dclose(dataset);
  H5Sclose(space);

  H5Tclose(type);
  H5Tclose(type2);
  H5Gclose(group);

  group = H5Gcreate (fid, "/g1/g1.2", 0);
  H5Glink (group, H5G_LINK_HARD, "/g1/g1.1/dset1", "link1");
  H5Gclose(group);

  group = H5Gcreate (fid, "/g2", 0);
  H5Glink (group, H5G_LINK_SOFT, "/g1", "slink2");
  H5Gclose(group);

  group = H5Gcreate (fid, "/g3", 0);
  H5Gclose(group);

  group = H5Gcreate (fid, "/g4", 0);

  /* dset2 */
  dims[0] = 10; dims[1] = 10;
  space = H5Screate_simple(2, dims, NULL);

  dataset = H5Dcreate(group, "dset2", H5T_STD_I32BE, space, H5P_DEFAULT);
  for (i = 0; i < 10; i++)
       for (j = 0; j < 10; j++)
            dset2[i][j] = j;
  H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);

  H5Dclose(dataset);

  H5Sclose(space);
  H5Gclose(group);

  group = H5Gopen(fid, "/g3");
  H5Glink (group, H5G_LINK_HARD, "/g4/dset2", "link3");
  H5Gclose(group);

  group = H5Gcreate (fid, "/g5", 0);
  H5Gclose(group);

  group = H5Gcreate (fid, "/g6", 0);
  /* dset3 */
  dims[0] = 10; dims[1] = 10;
  space = H5Screate_simple(2, dims, NULL);

  dataset = H5Dcreate(group, "dset3", H5T_STD_I32BE, space, H5P_DEFAULT);
  for (i = 0; i < 10; i++)
       for (j = 0; j < 10; j++)
            dset3[i][j] = i;
  H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset3);

  H5Dclose(dataset);

  H5Sclose(space);
  H5Gclose(group);

  group = H5Gopen(fid, "/g5");
  H5Glink (group, H5G_LINK_SOFT, "/g6/dset3", "slink4");
  H5Gclose(group);
  H5Pclose(create_plist);

  H5Fclose(fid);

}
static hid_t mkstr(int size, H5T_str_t pad) {
hid_t type;

  if ((type=H5Tcopy(H5T_C_S1))<0) return -1;
  if (H5Tset_size(type, (size_t)size)<0) return -1;
  if (H5Tset_strpad(type, pad)<0) return -1;

  return type;
}

static void gent_str(void) {
    hid_t fid, dataset, space, f_type, m_type, str_type, f_type2;
    hid_t array_dt;

    hsize_t dims1[] = { 3, 4};
    char string1[12][3] = {"s1","s2","s3","s4","s5","s6","s7","s8","s9",
                           "s0","s1","s2"};

    hsize_t dims2[]={20};
    char string2[20][10] = {"ab cd ef1", "ab cd ef2", "ab cd ef3", "ab cd ef4",
                        "ab cd ef5", "ab cd ef6", "ab cd ef7", "ab cd ef8", 
                        "ab cd ef9", "ab cd ef0", "ab cd ef1", "ab cd ef2", 
                        "ab cd ef3", "ab cd ef4", "ab cd ef5", "ab cd ef6", 
                        "ab cd ef7", "ab cd ef8", "ab cd ef9", "ab cd ef0"};

    hsize_t dims3[] = { 27};
    char string3[27][6] = {"abcd0", "abcd1", "abcd2", "abcd3", 
                       "abcd4", "abcd5", "abcd6", "abcd7", 
                       "abcd8", "abcd9", "abcd0", "abcd1", 
                       "abcd2", "abcd3", "abcd4", "abcd5", 
                       "abcd6", "abcd7", "abcd8", "abcd9", 
                       "abcd0", "abcd1", "abcd2", "abcd3", 
                       "abcd4", "abcd5", "abcd6"};

    int i, j, k, l;

    hsize_t dims4[] = { 3 };
    char string4[3][21] = { "s1234567890123456789", "s1234567890123456789",
                            "s1234567890123456789"};

    hsize_t dims5[] = { 3, 6};
    typedef struct {
      int a[8][10];
      char s[12][33];
    } compound_t;
    compound_t comp1[3][6];
    hsize_t mdims[2];

  fid = H5Fcreate(FILE13, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

  /* string 1 : nullterm string */
  space = H5Screate_simple(2, dims1, NULL);
  f_type = mkstr(5, H5T_STR_NULLTERM);
  m_type = mkstr(3, H5T_STR_NULLTERM);
  dataset = H5Dcreate(fid, "/string1", f_type, space, H5P_DEFAULT);
  H5Dwrite(dataset, m_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, string1);
  H5Tclose(m_type);
  H5Tclose(f_type);
  H5Sclose(space);
  H5Dclose(dataset);

  /* string 2 : space pad string */
  space = H5Screate_simple(1, dims2, NULL);
  f_type = mkstr(11, H5T_STR_SPACEPAD);
  m_type = mkstr(10, H5T_STR_NULLTERM);
  dataset = H5Dcreate(fid, "/string2", f_type, space, H5P_DEFAULT);
  H5Dwrite(dataset, m_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, string2);
  H5Tclose(m_type);
  H5Tclose(f_type);
  H5Sclose(space);
  H5Dclose(dataset);

  /* string 3 : null pad string */
  space = H5Screate_simple(1, dims3, NULL);
  f_type = mkstr(8, H5T_STR_NULLPAD);
  m_type = mkstr(6, H5T_STR_NULLTERM);
  dataset = H5Dcreate(fid, "/string3", f_type, space, H5P_DEFAULT);
  H5Dwrite(dataset, m_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, string3);
  H5Tclose(m_type);
  H5Tclose(f_type);
  H5Sclose(space);
  H5Dclose(dataset);

  /* string 4 : space pad long string */
  space = H5Screate_simple(1, dims4, NULL);
  f_type = mkstr(168, H5T_STR_SPACEPAD);
  m_type = mkstr(21, H5T_STR_NULLTERM);
  dataset = H5Dcreate(fid, "/string4", f_type, space, H5P_DEFAULT);
  H5Dwrite(dataset, m_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, string4);
  H5Tclose(m_type);
  H5Tclose(f_type);
  H5Sclose(space);
  H5Dclose(dataset);

  /* compound data */
  space = H5Screate_simple(2, dims5, NULL);
  f_type = H5Tcreate (H5T_COMPOUND, sizeof(compound_t));
  f_type2 = H5Tcreate (H5T_COMPOUND, sizeof(compound_t));

  mdims[0] = 8; mdims[1] = 10;

  array_dt=H5Tarray_create(H5T_STD_I32BE,2,mdims,NULL);
  H5Tinsert(f_type, "int_array", HOFFSET(compound_t, a), array_dt);
  H5Tclose(array_dt);

  array_dt=H5Tarray_create(H5T_NATIVE_INT,2,mdims,NULL);
  H5Tinsert(f_type2, "int_array", HOFFSET(compound_t, a), array_dt);
  H5Tclose(array_dt);

  mdims[0] = 3; mdims[1] = 4;

  str_type = mkstr(32, H5T_STR_SPACEPAD);
  array_dt=H5Tarray_create(str_type,2,mdims,NULL);
  H5Tinsert(f_type, "string", HOFFSET(compound_t, s), array_dt);
  H5Tclose(array_dt);
  H5Tclose(str_type);

  str_type = mkstr(33, H5T_STR_NULLTERM);
  array_dt=H5Tarray_create(str_type,2,mdims,NULL);
  H5Tinsert(f_type2, "string", HOFFSET(compound_t, s), array_dt);
  H5Tclose(array_dt);
  H5Tclose(str_type);

  for (i = 0; i < 3; i++)
      for (j = 0; j < 6; j++) {
           for (k = 0 ; k < 8; k++)
                for (l = 0; l < 10; l++)
                   comp1[i][j].a[k][l] = (l+j+k) * (l+j+k);
           for (k = 0 ; k < 12; k++)
               strcpy(comp1[i][j].s[k], "abcdefgh12345678abcdefgh12345678");
      }
           
  dataset = H5Dcreate(fid, "/comp1", f_type, space, H5P_DEFAULT);
  H5Dwrite(dataset, f_type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, comp1);

  H5Tclose(f_type);
  H5Tclose(f_type2);
  H5Sclose(space);
  H5Dclose(dataset);

  H5Fclose(fid);
}

/*
                      /
       /     /     |    \    \     \
     g1     g2    g3    g4    g5    g6
     |       |     |     |     \     \ 
  string1       string3       string5 
         string2       string4       string6
*/

static void gent_str2(void)
{
hid_t fid, group, attr, dataset, space, space2, mem_space, hyper_space;
hid_t fxdlenstr, fxdlenstr2, memtype;
hsize_t dims[1], size[1], stride[1], count[1], block[1];
hssize_t start[1];


int i;
char buf[LENSTR+10];
char buf2[3*LENSTR2];
hsize_t sdim;

  fid = H5Fcreate(FILE14, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

  fxdlenstr = H5Tcopy(H5T_C_S1);
  H5Tset_size(fxdlenstr, LENSTR);
  H5Tset_cset(fxdlenstr, H5T_CSET_ASCII);
  H5Tset_strpad(fxdlenstr, H5T_STR_NULLTERM);

  memtype = H5Tcopy(H5T_C_S1);
  H5Tset_size(memtype, LENSTR);
  H5Tset_cset(memtype, H5T_CSET_ASCII);
  H5Tset_strpad(memtype, H5T_STR_NULLTERM);

  sdim = 10;
  size[0] = sdim;
  space = H5Screate_simple(1, size, NULL);
  size[0] = 1;
  mem_space = H5Screate_simple(1,size,NULL);
  hyper_space = H5Scopy(space);

  /* dset1 */

  group = H5Gcreate (fid, "/g1", 0);
  dataset = H5Dcreate(group, "dset1", fxdlenstr, space, H5P_DEFAULT);

  /* add attributes to dset1 */

  fxdlenstr2 = H5Tcopy(H5T_C_S1);
  H5Tset_size(fxdlenstr2, LENSTR2);
  H5Tset_cset(fxdlenstr2, H5T_CSET_ASCII);
  H5Tset_strpad(fxdlenstr2, H5T_STR_NULLTERM);

  dims[0] = 3;
  space2 = H5Screate_simple(1, dims, NULL);
  attr = H5Acreate (dataset, "attr1", fxdlenstr2, space2, H5P_DEFAULT);
  sprintf(&(buf2[0*LENSTR2]), "0123456789");
  sprintf(&(buf2[1*LENSTR2]), "abcdefghij");
  sprintf(&(buf2[2*LENSTR2]), "ABCDEFGHIJ");
  H5Awrite(attr, fxdlenstr2, buf2);
  H5Sclose(space2);
  H5Tclose(fxdlenstr2);
  H5Aclose(attr);

  stride[0]=1;
  count[0]=1;
  block[0]=1;

  for (i = 0; (hsize_t)i < sdim; i++) {
 start[0] = i;
 sprintf(buf,"This is row %1d of type H5T_STR_NULLTERM of",i);
   H5Tset_size(memtype, HDstrlen(buf)+1);
 H5Sselect_hyperslab(hyper_space, H5S_SELECT_SET, start, stride, count, block);
   H5Dwrite(dataset, memtype, mem_space, hyper_space, H5P_DEFAULT, buf);
  }
  H5Dclose(dataset);
  H5Gclose(group);

  group = H5Gcreate (fid, "/g2", 0);
  dataset = H5Dcreate(group, "dset2", fxdlenstr, space, H5P_DEFAULT);

  for (i = 0; (hsize_t)i < sdim; i++) {
 start[0] = i;
 sprintf(buf,"This is row %1d of type H5T_STR_NULLTERM of string array",i);
   H5Tset_size(memtype, HDstrlen(buf)+1);
 H5Sselect_hyperslab(hyper_space, H5S_SELECT_SET, start, stride, count, block);
   H5Dwrite(dataset, memtype, mem_space, hyper_space, H5P_DEFAULT, buf);
  }
  H5Dclose(dataset);
  H5Gclose(group);


  H5Tclose(fxdlenstr);
  fxdlenstr = H5Tcopy(H5T_C_S1);
  H5Tset_size(fxdlenstr, LENSTR);
  H5Tset_cset(fxdlenstr, H5T_CSET_ASCII);
  H5Tset_strpad(fxdlenstr, H5T_STR_NULLPAD);

  group = H5Gcreate (fid, "/g3", 0);
  dataset = H5Dcreate(group, "dset3", fxdlenstr, space, H5P_DEFAULT);

  for (i = 0;(hsize_t) i < sdim; i++) {
 start[0] = i;
 sprintf(buf,"This is row %1d of type H5T_STR_NULLPAD of",i);
   H5Tset_size(memtype, HDstrlen(buf)+1);
 H5Sselect_hyperslab(hyper_space, H5S_SELECT_SET, start, stride, count, block);
   H5Dwrite(dataset, memtype, mem_space, hyper_space, H5P_DEFAULT, buf);
  }
  H5Dclose(dataset);
  H5Gclose(group);


  group = H5Gcreate (fid, "/g4", 0);
  dataset = H5Dcreate(group, "dset4", fxdlenstr, space, H5P_DEFAULT);

  for (i = 0; (hsize_t)i < sdim; i++) {
 start[0] = i;
 sprintf(buf,"This is row %1d of type H5T_STR_NULLPAD of string array",i);
   H5Tset_size(memtype, HDstrlen(buf)+1);
 H5Sselect_hyperslab(hyper_space, H5S_SELECT_SET, start, stride, count, block);
   H5Dwrite(dataset, memtype, mem_space, hyper_space, H5P_DEFAULT, buf);
  }
  H5Dclose(dataset);
  H5Gclose(group);

  H5Tclose(fxdlenstr);
  fxdlenstr = H5Tcopy(H5T_C_S1);
  H5Tset_size(fxdlenstr, LENSTR);
  H5Tset_cset(fxdlenstr, H5T_CSET_ASCII);
  H5Tset_strpad(fxdlenstr, H5T_STR_SPACEPAD);

  group = H5Gcreate (fid, "/g5", 0);
  dataset = H5Dcreate(group, "dset5", fxdlenstr, space, H5P_DEFAULT);

  for (i = 0; (hsize_t)i < sdim; i++) {
 start[0] = i;
 sprintf(buf,"This is row %1d of type H5T_STR_SPACEPAD of",i);
   H5Tset_size(memtype, HDstrlen(buf)+1);
 H5Sselect_hyperslab(hyper_space, H5S_SELECT_SET, start, stride, count, block);
   H5Dwrite(dataset, memtype, mem_space, hyper_space, H5P_DEFAULT, buf);
  }
  H5Dclose(dataset);
  H5Gclose(group);


  group = H5Gcreate (fid, "/g6", 0);
  dataset = H5Dcreate(group, "dset6", fxdlenstr, space, H5P_DEFAULT);

  for (i = 0; (hsize_t)i < sdim; i++) {
 start[0] = i;
 sprintf(buf,"This is row %1d of type H5T_STR_SPACEPAD of string array",i);
   H5Tset_size(memtype, HDstrlen(buf)+1);
 H5Sselect_hyperslab(hyper_space, H5S_SELECT_SET, start, stride, count, block);
   H5Dwrite(dataset, memtype, mem_space, hyper_space, H5P_DEFAULT, buf);
  }

  H5Dclose(dataset);
  H5Gclose(group);
  H5Tclose(fxdlenstr);
  H5Tclose(memtype);
  H5Sclose(mem_space);
  H5Sclose(hyper_space);
  H5Sclose(space);
  H5Fclose(fid);
}

static void gent_enum(void)
{
    /*some code is taken from enum.c in the test dir */
    hid_t file, type, space, dset;
    int val;
    enumtype data[] = {RED,   GREEN, BLUE,  GREEN, WHITE,
         WHITE, BLACK, GREEN, BLUE,  RED,
         RED,   BLUE,  GREEN, BLACK, WHITE,
         RED,   WHITE, GREEN, GREEN, BLUE};
    hsize_t size[1] = {NELMTS(data)};

    file = H5Fcreate(FILE15,H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Try to test names with special characters */
    type = H5Tcreate(H5T_ENUM, sizeof(enumtype));
    H5Tenum_insert(type, "RED",   (val = 0, &val));
    H5Tenum_insert(type, "GREEN\ngreen", (val = 1, &val));
    H5Tenum_insert(type, "BLUE blue",  (val = 2, &val));
    H5Tenum_insert(type, "WHITE \"white\"", (val = 3, &val));
    H5Tenum_insert(type, "BLACK \'black\'", (val = 4, &val));
    H5Tcommit(file, "enum normal", type);

    space = H5Screate_simple(1,size,NULL);
    dset = H5Dcreate(file,"table",type, space, H5P_DEFAULT);
    H5Dwrite(dset,type,space,space,H5P_DEFAULT,data);

    H5Dclose(dset);
    H5Sclose(space);
    H5Fclose(file);
}

static void gent_objref(void)
{
/*some code is taken from enum.c in the test dir */
    hid_t  fid1;  /* HDF5 File IDs  */
    hid_t  dataset; /* Dataset ID   */

    hid_t  group;      /* Group ID             */
    hid_t  sid1;       /* Dataspace ID   */
    hid_t  tid1;       /* Datatype ID   */
    hsize_t  dims1[] = {SPACE1_DIM1};
    hobj_ref_t      *wbuf,      /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf;       /* temp. buffer read from disk */
    uint32_t   *tu32;      /* Temporary pointer to uint32 data */
    int        i;          /* counting variables */
    const char *write_comment="Foo!"; /* Comments for group */

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(hobj_ref_t)*SPACE1_DIM1);
    rbuf=malloc(sizeof(hobj_ref_t)*SPACE1_DIM1);
    tbuf=malloc(sizeof(hobj_ref_t)*SPACE1_DIM1);

    /* Create file */
    fid1 = H5Fcreate(FILE16, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);

    /* Create a group */
    group=H5Gcreate(fid1,"Group1",(size_t)-1);

    /* Set group's comment */
    H5Gset_comment(group,".",write_comment);

    /* Create a dataset (inside Group1) */
    dataset=H5Dcreate(group,"Dataset1",H5T_STD_U32BE,sid1,H5P_DEFAULT);

    for(tu32=(uint32_t *)((void*)wbuf),i=0; i<SPACE1_DIM1; i++)
        *tu32++=i*3;

    /* Write selection to disk */
    H5Dwrite(dataset,H5T_NATIVE_UINT,H5S_ALL,H5S_ALL,H5P_DEFAULT,wbuf);

    /* Close Dataset */
    H5Dclose(dataset);

    /* Create another dataset (inside Group1) */
    dataset=H5Dcreate(group,"Dataset2",H5T_STD_U8BE,sid1,H5P_DEFAULT);

    /* Close Dataset */
    H5Dclose(dataset);

    /* Create a datatype to refer to */
    tid1 = H5Tcreate (H5T_COMPOUND, sizeof(s1_t));

    /* Insert fields */
    H5Tinsert (tid1, "a", HOFFSET(s1_t,a), H5T_STD_I32BE);

    H5Tinsert (tid1, "b", HOFFSET(s1_t,b), H5T_IEEE_F32BE);

    H5Tinsert (tid1, "c", HOFFSET(s1_t,c), H5T_IEEE_F32BE);

    /* Save datatype for later */
    H5Tcommit (group, "Datatype1", tid1);

    /* Close datatype */
    H5Tclose(tid1);

    /* Close group */
    H5Gclose(group);

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset3",H5T_STD_REF_OBJ,sid1,H5P_DEFAULT);

    /* Create reference to dataset */
    H5Rcreate(&wbuf[0],fid1,"/Group1/Dataset1",H5R_OBJECT,-1);

    /* Create reference to dataset */
    H5Rcreate(&wbuf[1],fid1,"/Group1/Dataset2",H5R_OBJECT,-1);

    /* Create reference to group */
    H5Rcreate(&wbuf[2],fid1,"/Group1",H5R_OBJECT,-1);

    /* Create reference to named datatype */
    H5Rcreate(&wbuf[3],fid1,"/Group1/Datatype1",H5R_OBJECT,-1);

    /* Write selection to disk */
    H5Dwrite(dataset,H5T_STD_REF_OBJ,H5S_ALL,H5S_ALL,H5P_DEFAULT,wbuf);

    /* Close disk dataspace */
    H5Sclose(sid1);
   
    /* Close Dataset */
    H5Dclose(dataset);

    /* Close file */
    H5Fclose(fid1);

    /* Free memory buffers */
    free(wbuf);
    free(rbuf);
    free(tbuf);

}

static void gent_datareg(void)
{
    /*some code is taken from enum.c in the test dir */

    hid_t  fid1;  /* HDF5 File IDs  */
    hid_t  dset1, /* Dataset ID   */
                dset2;      /* Dereferenced dataset ID */
    hid_t  sid1,       /* Dataspace ID #1  */
                sid2;       /* Dataspace ID #2  */
    hsize_t  dims1[] = {SPACE1_DIM1},
             dims2[] = {SPACE2_DIM1, SPACE2_DIM2};
    hssize_t start[SPACE2_RANK];     /* Starting location of hyperslab */
    hsize_t  stride[SPACE2_RANK];    /* Stride of hyperslab */
    hsize_t  count[SPACE2_RANK];     /* Element count of hyperslab */
    hsize_t  block[SPACE2_RANK];     /* Block size of hyperslab */
    hssize_t coord1[POINT1_NPOINTS][SPACE2_RANK]; /* Coordinates for point selection */
    hdset_reg_ref_t      *wbuf,      /* buffer to write to disk */
               *rbuf;       /* buffer read from disk */
    uint8_t    *dwbuf,      /* Buffer for writing numeric data to disk */
               *drbuf;      /* Buffer for reading numeric data from disk */
    uint8_t    *tu8;        /* Temporary pointer to uint8 data */
    int        i;          /* counting variables */

    /* Allocate write & read buffers */
    wbuf=calloc(sizeof(hdset_reg_ref_t), SPACE1_DIM1);
    rbuf=malloc(sizeof(hdset_reg_ref_t)*SPACE1_DIM1);
    dwbuf=malloc(sizeof(uint8_t)*SPACE2_DIM1*SPACE2_DIM2);
    drbuf=calloc(sizeof(uint8_t),SPACE2_DIM1*SPACE2_DIM2);

    /* Create file */
    fid1 = H5Fcreate(FILE17, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid2 = H5Screate_simple(SPACE2_RANK, dims2, NULL);

    /* Create a dataset */
    dset2=H5Dcreate(fid1,"Dataset2",H5T_STD_U8BE,sid2,H5P_DEFAULT);

    for(tu8=dwbuf,i=0; i<SPACE2_DIM1*SPACE2_DIM2; i++)
        *tu8++=i*3;

    /* Write selection to disk */
    H5Dwrite(dset2,H5T_NATIVE_UCHAR,H5S_ALL,H5S_ALL,H5P_DEFAULT,dwbuf);

    /* Close Dataset */
    H5Dclose(dset2);

    /* Create dataspace for the reference dataset */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);

    /* Create a dataset */
    dset1=H5Dcreate(fid1,"Dataset1",H5T_STD_REF_DSETREG,sid1,H5P_DEFAULT);

    /* Create references */

    /* Select 6x6 hyperslab for first reference */
    start[0]=2; start[1]=2;
    stride[0]=1; stride[1]=1;
    count[0]=6; count[1]=6;
    block[0]=1; block[1]=1;
    H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block);

    H5Sget_select_npoints(sid2);

    /* Store first dataset region */
    H5Rcreate(&wbuf[0],fid1,"/Dataset2",H5R_DATASET_REGION,sid2);

    /* Select sequence of ten points for second reference */
    coord1[0][0]=6; coord1[0][1]=9;
    coord1[1][0]=2; coord1[1][1]=2;
    coord1[2][0]=8; coord1[2][1]=4;
    coord1[3][0]=1; coord1[3][1]=6;
    coord1[4][0]=2; coord1[4][1]=8;
    coord1[5][0]=3; coord1[5][1]=2;
    coord1[6][0]=0; coord1[6][1]=4;
    coord1[7][0]=9; coord1[7][1]=0;
    coord1[8][0]=7; coord1[8][1]=1;
    coord1[9][0]=3; coord1[9][1]=3;
    H5Sselect_elements(sid2,H5S_SELECT_SET,POINT1_NPOINTS,(const hssize_t **)coord1);

    H5Sget_select_npoints(sid2);

    /* Store second dataset region */
    H5Rcreate(&wbuf[1],fid1,"/Dataset2",H5R_DATASET_REGION,sid2);

    /* Write selection to disk */
    H5Dwrite(dset1,H5T_STD_REF_DSETREG,H5S_ALL,H5S_ALL,H5P_DEFAULT,wbuf);

    /* Close disk dataspace */
    H5Sclose(sid1);
    
    /* Close Dataset */
    H5Dclose(dset1);

    /* Close uint8 dataset dataspace */
    H5Sclose(sid2);
    
    /* Close file */
    H5Fclose(fid1);

    /* Free memory buffers */
    free(wbuf);
    free(rbuf);
    free(dwbuf);
    free(drbuf);
}

/*taken from Elena's compound test file*/
static void gent_nestcomp(void)
{
   /* Compound memeber of the compound datatype*/
    typedef struct cmp_t {
        char   a;
        float  b[2];
    } cmp_t;

    /* First structure  and dataset*/
    typedef struct s1_t {
 int    a;
 float  b;
 double c; 
        cmp_t  d; 
    } s2_t;
    hid_t      cmp_tid;    /* Handle for the compound datatype */
    hid_t      char_id;    /* Handle for the string datatype */
    hid_t      array_dt;
    hsize_t     array_dims[] = {2};    /* Dataspace dimensions */
    int        ndims = 1;    /* Number of dimensions in the array field */

    s2_t       s1[10];
    hid_t      s2_tid;     /* File datatype identifier */

    int        i;
    hid_t      file, dataset, space; /* Handles */
    herr_t     status;
    hsize_t    dim[] = {10};   /* Dataspace dimensions */

    char datasetname[] = "ArrayOfStructures";


    /*
     * Initialize the data
     */
    for (i = 0; i< 10; i++) {
        s1[i].a = i;
        s1[i].b = (float)(i*i);
        s1[i].c = 1./(i+1);
        s1[i].d.a = 65 + i;
        s1[i].d.b[0] = -100.;
        s1[i].d.b[1] =  100.;
    }

    /*
     * Create the data space.
     */
    space = H5Screate_simple(1, dim, NULL);

    /*
     * Create the file.
     */
    file = H5Fcreate(FILE18, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create the memory data type. 
     */
    /* 
     * Create a datatype for compound field first.
     */
    cmp_tid = H5Tcreate (H5T_COMPOUND, sizeof(cmp_t));

    /* We are using C string of length one to represent "real" character */
    char_id = H5Tcopy(H5T_C_S1);
    H5Tset_strpad(char_id, H5T_STR_NULLTERM);       
    H5Tinsert(cmp_tid, "char_name", HOFFSET(cmp_t, a), char_id);

    array_dt=H5Tarray_create(H5T_NATIVE_FLOAT,ndims,array_dims,NULL);
    H5Tinsert(cmp_tid, "array_name", HOFFSET(cmp_t, b), array_dt);
    H5Tclose(array_dt);

    s2_tid = H5Tcreate (H5T_COMPOUND, sizeof(s2_t));
    H5Tinsert(s2_tid, "a_name", HOFFSET(s2_t, a), H5T_NATIVE_INT);
    H5Tinsert(s2_tid, "c_name", HOFFSET(s2_t, c), H5T_NATIVE_DOUBLE);
    H5Tinsert(s2_tid, "b_name", HOFFSET(s2_t, b), H5T_NATIVE_FLOAT);

    /* Insert compound memeber created above */
    H5Tinsert(s2_tid, "d_name", HOFFSET(s2_t, d), cmp_tid);

    /* 
     * Create the dataset.
     */
    dataset = H5Dcreate(file, datasetname, s2_tid, space, H5P_DEFAULT);

    /*
     * Wtite data to the dataset; 
     */
    status = H5Dwrite(dataset, s2_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s1);
    if (status < 0)
 fprintf(stderr, "gent_nestcomp H5Dwrite failed\n");

    /*
     * Release resources
     */
    H5Tclose(s2_tid);
    H5Tclose(cmp_tid);
    H5Tclose(char_id);
    H5Sclose(space);
    H5Dclose(dataset);
    H5Fclose(file);
}

static void gent_opaque(void)
{
    hid_t file, type, dataset, space;
    char test[100][2];
    int x;
    hsize_t dim = 2;

    for (x = 0; x < 100; x++){
        test[x][0] = x;
        test[x][1] = 99 - x;
    }
 
    /*
     * Create the data space.
     */
    space = H5Screate_simple(1, &dim, NULL);

    /*
     * Create the file.
     */
    file = H5Fcreate(FILE19, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create the memory datatype. 
     */
    type = H5Tcreate (H5T_OPAQUE, sizeof(char)*100*2);
    H5Tset_tag(type, "test opaque type");

    /* 
     * Create the dataset.
     */
    dataset = H5Dcreate(file, "opaque test", type, space, H5P_DEFAULT);

    /*
     * Write data to the dataset; 
     */
    H5Dwrite(dataset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, test);

    H5Tclose(type);
    H5Sclose(space);
    H5Dclose(dataset);
    H5Fclose(file); 
}

static void gent_bitfields(void)
{
    hid_t  file, grp=-1, type=-1, space=-1, dset=-1;
    size_t  i;
    hsize_t  nelmts;
    unsigned char buf[32];

    file = H5Fcreate(FILE20, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
   
    if ((grp=H5Gcreate(file, "typetests", 0))<0) goto error;

    /* bitfield_1 */
    nelmts = sizeof(buf);
    if ((type=H5Tcopy(H5T_STD_B8LE))<0 ||
 (space=H5Screate_simple(1, &nelmts, NULL))<0 ||
 (dset=H5Dcreate(grp, "bitfield_1", type, space, H5P_DEFAULT))<0)
 goto error;

    for (i=0; i<sizeof buf; i++) buf[i] = (unsigned char)0xff ^ (unsigned char)i;
    if (H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf)<0)
 goto error;
    if (H5Sclose(space)<0) goto error;
    if (H5Tclose(type)<0) goto error;
    if (H5Dclose(dset)<0) goto error;

    /* bitfield_2 */
    nelmts = sizeof(buf)/2;
    if ((type=H5Tcopy(H5T_STD_B16LE))<0 ||
 (space=H5Screate_simple(1, &nelmts, NULL))<0 ||
 (dset=H5Dcreate(grp, "bitfield_2", type, space, H5P_DEFAULT))<0)
 goto error;
    for (i=0; i<sizeof buf; i++) buf[i] = (unsigned char)0xff ^ (unsigned char)i;
    if (H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf)<0)
 goto error;
    if (H5Sclose(space)<0) goto error;
    if (H5Tclose(type)<0) goto error;
    if (H5Dclose(dset)<0) goto error;
    if (H5Gclose(grp)<0) goto error;
    H5Fclose(file);

 error:
    H5E_BEGIN_TRY {
 H5Gclose(grp);
 H5Tclose(type);
 H5Sclose(space);
 H5Dclose(dset);
    } H5E_END_TRY;
}

static void gent_vldatatypes(void)
{
    hvl_t adata, wdata[SPACE1_DIM1];
    hid_t file, dset, space, type;
    hsize_t dims[] = { SPACE1_DIM1 };
    int i;
    herr_t ret=0;

    file = H5Fcreate(FILE21, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
   
    /* Allocate and initialize VL dataset to write */
    for(i = 0; i < SPACE1_DIM1; i++) {
 int j;

        wdata[i].p = malloc((i + 1) * sizeof(int));
        wdata[i].len = i + 1;

        for (j = 0; j < i + 1; j++)
            ((int *)wdata[i].p)[j] = i * 10 + j;
    }

    /* write out the integers in little-endian format */
    space = H5Screate_simple(SPACE1_RANK, dims, NULL);
    type = H5Tvlen_create(H5T_NATIVE_INT);
    dset = H5Dcreate(file, "Dataset1.0", type, space, H5P_DEFAULT);
    ret = H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    assert(ret>=0);
    ret = H5Dvlen_reclaim(type, space, H5P_DEFAULT, wdata);
    assert(ret>=0);

    ret = H5Dclose(dset);
    assert(ret>=0);
    ret = H5Tclose(type);
    assert(ret>=0);
    ret = H5Sclose(space);
    assert(ret>=0);

    /* Allocate and initialize VL dataset to write */
    for(i = 0; i < SPACE1_DIM1; i++) {
 int j;

        wdata[i].p = malloc((i + 1) * sizeof(float));
        wdata[i].len = i + 1;

        for (j = 0; j < i + 1; j++)
            ((float *)wdata[i].p)[j] = (float)(i * 10 + ((float)j) / 10.0);
    }

    /* write out the floats in little-endian format */
    space = H5Screate_simple(SPACE1_RANK, dims, NULL);
    type = H5Tvlen_create(H5T_NATIVE_FLOAT);
    dset = H5Dcreate(file, "Dataset2.0", type, space, H5P_DEFAULT);
    ret = H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    assert(ret>=0);
    ret = H5Dvlen_reclaim(type, space, H5P_DEFAULT, wdata);
    assert(ret>=0);

    ret = H5Dclose(dset);
    assert(ret>=0);
    ret = H5Tclose(type);
    assert(ret>=0);
    ret = H5Sclose(space);
    assert(ret>=0);

    /* Allocate and initialize a scalar VL dataset to write */
    adata.p = malloc(37 * sizeof(int));
    adata.len = 37;

    for (i = 0; i < 37; i++)
        ((int *)adata.p)[i] = i * 2;

    /* write out scalar VL dataset in little-endian format */
    space = H5Screate_simple(0, NULL, NULL);
    type = H5Tvlen_create(H5T_NATIVE_INT);
    dset = H5Dcreate(file, "Dataset3.0", type, space, H5P_DEFAULT);
    ret = H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, &adata);
    assert(ret>=0);
    ret = H5Dvlen_reclaim(type, space, H5P_DEFAULT, &adata);
    assert(ret>=0);

    ret = H5Dclose(dset);
    assert(ret>=0);
    ret = H5Tclose(type);
    assert(ret>=0);
    ret = H5Sclose(space);
    assert(ret>=0);
    ret = H5Fclose(file);
    assert(ret>=0);
}

static void gent_vldatatypes2(void)
{
    hvl_t wdata[SPACE1_DIM1];   /* Information to write */
    hvl_t *t1;              /* Temporary pointer to VL information */
    hid_t  fid1;  /* HDF5 File IDs  */
    hid_t  dataset; /* Dataset ID   */
    hid_t  sid1;       /* Dataspace ID   */
    hid_t  tid1, tid2; /* Datatype IDs         */
    hsize_t  dims1[] = {SPACE1_DIM1};
    unsigned       i,j,k;      /* counting variables */
    herr_t  ret;  /* Generic return value  */

    /* Allocate and initialize VL data to write */
    for(i=0; i<SPACE1_DIM1; i++) {
        wdata[i].p=malloc((i+1)*sizeof(hvl_t));
        if(wdata[i].p==NULL) {
            printf("Cannot allocate memory for VL data! i=%u\n",i);
            return;
        } /* end if */
        wdata[i].len=i+1;
        for(t1=wdata[i].p,j=0; j<(i+1); j++, t1++) {
            t1->p=malloc((j+1)*sizeof(unsigned int));
            if(t1->p==NULL) {
                printf("Cannot allocate memory for VL data! i=%u, j=%u\n",i,j);
                return;
            } /* end if */
            t1->len=j+1;
            for(k=0; k<(j+1); k++)
                ((unsigned int *)t1->p)[k]=i*100+j*10+k;
        } /* end for */
    } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILE22, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);

    /* Create a VL datatype to refer to */
    tid1 = H5Tvlen_create (H5T_NATIVE_UINT);

    /* Create the base VL type */
    tid2 = H5Tvlen_create (tid1);

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",tid2,sid1,H5P_DEFAULT);

    /* Write dataset to disk */
    ret=H5Dwrite(dataset,tid2,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Reclaim the write VL data */
    ret=H5Dvlen_reclaim(tid2,sid1,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    assert(ret>=0);
    ret = H5Tclose(tid2);
    assert(ret>=0);
    ret = H5Tclose(tid1);
    assert(ret>=0);
    ret = H5Sclose(sid1);
    assert(ret>=0);
    ret = H5Fclose(fid1);
    assert(ret>=0);

}

static void gent_vldatatypes3(void)
{
    typedef struct {             /* Struct that the VL sequences are composed of */
        int i;
        float f;
        hvl_t v;
    } s1;
    s1 wdata[SPACE1_DIM1];   /* Information to write */
    hid_t  fid1;  /* HDF5 File IDs  */
    hid_t  dataset; /* Dataset ID   */
    hid_t  sid1;       /* Dataspace ID   */
    hid_t  tid1, tid2; /* Datatype IDs         */
    hsize_t  dims1[] = {SPACE1_DIM1};
    unsigned       i,j;        /* counting variables */
    herr_t  ret;  /* Generic return value  */

    /* Allocate and initialize VL data to write */
    for(i=0; i<SPACE1_DIM1; i++) {
        wdata[i].i=i*10;
        wdata[i].f=(float)((i*20)/3.0);
        wdata[i].v.p=malloc((i+1)*sizeof(unsigned int));
        wdata[i].v.len=i+1;
        for(j=0; j<(i+1); j++)
            ((unsigned int *)wdata[i].v.p)[j]=i*10+j;
    } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILE23, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);

    /* Create a VL datatype to refer to */
    tid1 = H5Tvlen_create (H5T_NATIVE_UINT);

    /* Create the base compound type */
    tid2 = H5Tcreate(H5T_COMPOUND, sizeof(s1));

    /* Insert fields */
    ret=H5Tinsert(tid2, "i", HOFFSET(s1, i), H5T_NATIVE_INT);
    assert(ret>=0);
    ret=H5Tinsert(tid2, "f", HOFFSET(s1, f), H5T_NATIVE_FLOAT);
    assert(ret>=0);
    ret=H5Tinsert(tid2, "v", HOFFSET(s1, v), tid1);
    assert(ret>=0);

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",tid2,sid1,H5P_DEFAULT);

    /* Write dataset to disk */
    ret=H5Dwrite(dataset,tid2,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Reclaim the write VL data */
    ret=H5Dvlen_reclaim(tid2,sid1,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    assert(ret>=0);
    ret = H5Tclose(tid2);
    assert(ret>=0);
    ret = H5Tclose(tid1);
    assert(ret>=0);
    ret = H5Sclose(sid1);
    assert(ret>=0);
    ret = H5Fclose(fid1);
    assert(ret>=0);
}

static void gent_vldatatypes4(void)
{
    typedef struct {             /* Struct that the VL sequences are composed of */
        int i;
        float f;
    } s1;
    hvl_t wdata[SPACE1_DIM1];   /* Information to write */
    hid_t  fid1;  /* HDF5 File IDs  */
    hid_t  dataset; /* Dataset ID   */
    hid_t  sid1;       /* Dataspace ID   */
    hid_t  tid1, tid2; /* Datatype IDs         */
    hsize_t  dims1[] = {SPACE1_DIM1};
    unsigned       i,j;        /* counting variables */
    herr_t  ret;  /* Generic return value  */

    /* Allocate and initialize VL data to write */
    for(i=0; i<SPACE1_DIM1; i++) {
        wdata[i].p=malloc((i+1)*sizeof(s1));
        wdata[i].len=i+1;
        for(j=0; j<(i+1); j++) {
            ((s1 *)wdata[i].p)[j].i=i*10+j;
            ((s1 *)wdata[i].p)[j].f=(float)((i*20+j)/3.0);
          } /* end for */
    } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILE24, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);

    /* Create the base compound type */
    tid2 = H5Tcreate(H5T_COMPOUND, sizeof(s1));

    /* Insert fields */
    ret=H5Tinsert(tid2, "i", HOFFSET(s1, i), H5T_NATIVE_INT);
    assert(ret>=0);
    ret=H5Tinsert(tid2, "f", HOFFSET(s1, f), H5T_NATIVE_FLOAT);
    assert(ret>=0);

    /* Create a datatype to refer to */
    tid1 = H5Tvlen_create (tid2);

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",tid1,sid1,H5P_DEFAULT);

    /* Write dataset to disk */
    ret=H5Dwrite(dataset,tid1,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Reclaim the write VL data */
    ret=H5Dvlen_reclaim(tid1,sid1,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    assert(ret>=0);
    ret = H5Tclose(tid1);
    assert(ret>=0);
    ret = H5Tclose(tid2);
    assert(ret>=0);
    ret = H5Sclose(sid1);
    assert(ret>=0);
    ret = H5Fclose(fid1);
    assert(ret>=0);
}

/* Generate a variable-length dataset with NULL values in it */
static void gent_vldatatypes5(void)
{
    hvl_t wdata [SPACE1_DIM1];
    hid_t  fid1;        
    hid_t  dataset;     
    hid_t  sid1;
    hid_t  tid1;
    hsize_t  dims1[] = {SPACE1_DIM1};
    int                 i,j;          /* counting variable */
    herr_t  ret;  /* Generic return value  */

    /* initialize data for dataset */
    for(i=0; i<SPACE1_DIM1; i++) {
        if(i%2) {
            wdata[i].len=0;
            wdata[i].p=NULL;
        } /* end if */
        else {
            wdata[i].len=i+5;
            wdata[i].p=malloc(sizeof(unsigned)*(i+5));
            for(j=0; j<i+5; j++)
                ((unsigned *)wdata[i].p)[j]=j*2;
        } /* end else */
    } /* end for */

    /* Create file */
    fid1 = H5Fcreate (FILE43, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    assert(fid1>0);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple (SPACE1_RANK, dims1, NULL);
    assert(sid1>0);

    /* Create a datatype to refer to */
    tid1 = H5Tvlen_create (H5T_NATIVE_UINT);
    assert(tid1>0);

    /* Create a dataset */
    dataset = H5Dcreate (fid1, F43_DSETNAME, tid1, sid1, H5P_DEFAULT);
    assert(dataset>0);

    ret = H5Dwrite (dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    assert(ret>=0);

    ret = H5Dclose (dataset);
    assert(ret>=0);

    ret = H5Dvlen_reclaim (tid1, sid1, H5P_DEFAULT, wdata);
    assert(ret>=0);

    ret = H5Tclose (tid1);
    assert(ret>=0);

    ret = H5Sclose (sid1);
    assert(ret>=0);

    ret = H5Fclose (fid1);
    assert(ret>=0);
}

static void gent_array1(void)
{
    int wdata[SPACE1_DIM1][ARRAY1_DIM1];   /* Information to write */
    hid_t  fid1;  /* HDF5 File IDs  */
    hid_t  dataset; /* Dataset ID   */
    hid_t  sid1;       /* Dataspace ID   */
    hid_t  tid1;       /* Datatype ID   */
    hsize_t  sdims1[] = {SPACE1_DIM1};
    hsize_t  tdims1[] = {ARRAY1_DIM1};
    int        i,j;        /* counting variables */
    herr_t  ret;  /* Generic return value  */

    /* Allocate and initialize array data to write */
    for(i=0; i<SPACE1_DIM1; i++)
        for(j=0; j<ARRAY1_DIM1; j++)
            wdata[i][j]=i*10+j;

    /* Create file */
    fid1 = H5Fcreate(FILE25, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, sdims1, NULL);

    /* Create a datatype to refer to */
    tid1 = H5Tarray_create (H5T_NATIVE_INT,ARRAY1_RANK,tdims1,NULL);

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",tid1,sid1,H5P_DEFAULT);

    /* Write dataset to disk */
    ret=H5Dwrite(dataset,tid1,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    assert(ret>=0);
    ret = H5Tclose(tid1);
    assert(ret>=0);
    ret = H5Sclose(sid1);
    assert(ret>=0);
    ret = H5Fclose(fid1);
    assert(ret>=0);
}

static void gent_array2(void)
{
    int wdata[SPACE1_DIM1][ARRAY2_DIM1][ARRAY2_DIM2][ARRAY2_DIM3];   /* Information to write */
    hid_t  fid;        /* HDF5 File IDs  */
    hid_t  dataset; /* Dataset ID   */
    hid_t  sid;        /* Dataspace ID   */
    hid_t  tid;        /* Datatype ID   */
    hsize_t  sdims1[] = {SPACE1_DIM1};
    hsize_t  tdims2[] = {ARRAY2_DIM1,ARRAY2_DIM2,ARRAY2_DIM3};
    int        i,j,k,l;    /* counting variables */
    herr_t  ret;  /* Generic return value  */

    /* Allocate and initialize array data to write */
    for(i=0; i<SPACE1_DIM1; i++)
        for(j=0; j<ARRAY2_DIM1; j++)
            for(k=0; k<ARRAY2_DIM2; k++)
                for(l=0; l<ARRAY2_DIM3; l++)
                    wdata[i][j][k][l]=i*1000+j*100+k*10+l;

    /* Create file */
    fid = H5Fcreate(FILE26, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid = H5Screate_simple(SPACE1_RANK, sdims1, NULL);

    /* Create a datatype to refer to */
    tid = H5Tarray_create (H5T_NATIVE_INT,ARRAY2_RANK,tdims2,NULL);

    /* Create a dataset */
    dataset=H5Dcreate(fid,"Dataset1",tid,sid,H5P_DEFAULT);

    /* Write dataset to disk */
    ret=H5Dwrite(dataset,tid,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    assert(ret>=0);
    ret = H5Tclose(tid);
    assert(ret>=0);
    ret = H5Sclose(sid);
    assert(ret>=0);
    ret = H5Fclose(fid);
    assert(ret>=0);
}

static void gent_array3(void)
{
    int wdata[SPACE1_DIM1][ARRAY1_DIM1][ARRAY3_DIM1][ARRAY3_DIM2];   /* Information to write */
    hid_t  fid;        /* HDF5 File IDs  */
    hid_t  dataset; /* Dataset ID   */
    hid_t  sid;        /* Dataspace ID   */
    hid_t  tid1;       /* 1-D array Datatype ID */
    hid_t  tid2;       /* 2-D array Datatype ID */
    hsize_t  sdims1[] = {SPACE1_DIM1};
    hsize_t  tdims1[] = {ARRAY1_DIM1};
    hsize_t  tdims2[] = {ARRAY3_DIM1,ARRAY3_DIM2};
    int        i,j,k,l;    /* counting variables */
    herr_t  ret;  /* Generic return value  */

    /* Allocate and initialize array data to write */
    for(i=0; i<SPACE1_DIM1; i++)
        for(j=0; j<ARRAY1_DIM1; j++)
            for(k=0; k<ARRAY3_DIM1; k++)
                for(l=0; l<ARRAY3_DIM2; l++)
                    wdata[i][j][k][l]=i*1000+j*100+k*10+l;

    /* Create file */
    fid = H5Fcreate(FILE27, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid = H5Screate_simple(SPACE1_RANK, sdims1, NULL);

    /* Create a 2-D datatype to refer to */
    tid2 = H5Tarray_create (H5T_NATIVE_INT,ARRAY3_RANK,tdims2,NULL);

    /* Create a 1-D datatype to refer to */
    tid1 = H5Tarray_create (tid2,ARRAY1_RANK,tdims1,NULL);

    /* Create a dataset */
    dataset=H5Dcreate(fid,"Dataset1",tid1,sid,H5P_DEFAULT);

    /* Write dataset to disk */
    ret=H5Dwrite(dataset,tid1,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    assert(ret>=0);
    ret = H5Tclose(tid1);
    assert(ret>=0);
    ret = H5Tclose(tid2);
    assert(ret>=0);
    ret = H5Sclose(sid);
    assert(ret>=0);
    ret = H5Fclose(fid);
    assert(ret>=0);
}

static void gent_array4(void)
{
    typedef struct {        /* Typedef for compound datatype */
        int i;
        float f;
    } s2_t;
    s2_t wdata[SPACE1_DIM1][ARRAY1_DIM1];   /* Information to write */
    hid_t  fid1;  /* HDF5 File IDs  */
    hid_t  dataset; /* Dataset ID   */
    hid_t  sid1;       /* Dataspace ID   */
    hid_t  tid1;       /* Array Datatype ID   */
    hid_t  tid2;       /* Compound Datatype ID   */
    hsize_t  sdims1[] = {SPACE1_DIM1};
    hsize_t  tdims1[] = {ARRAY1_DIM1};
    int        i,j;        /* counting variables */
    herr_t  ret;  /* Generic return value  */

    /* Initialize array data to write */
    for(i=0; i<SPACE1_DIM1; i++)
        for(j=0; j<ARRAY1_DIM1; j++) {
            wdata[i][j].i=i*10+j;
            wdata[i][j].f=(float)(i*2.5+j);
        } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILE28, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, sdims1, NULL);

    /* Create a compound datatype to refer to */
    tid2 = H5Tcreate(H5T_COMPOUND, sizeof(s2_t));

    /* Insert integer field */
    ret = H5Tinsert (tid2, "i", HOFFSET(s2_t,i), H5T_NATIVE_INT);
    assert(ret>=0);

    /* Insert float field */
    ret = H5Tinsert (tid2, "f", HOFFSET(s2_t,f), H5T_NATIVE_FLOAT);
    assert(ret>=0);

    /* Create an array datatype to refer to */
    tid1 = H5Tarray_create (tid2,ARRAY1_RANK,tdims1,NULL);

    /* Close compound datatype */
    ret=H5Tclose(tid2);
    assert(ret>=0);

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",tid1,sid1,H5P_DEFAULT);

    /* Write dataset to disk */
    ret=H5Dwrite(dataset,tid1,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    assert(ret>=0);
    ret = H5Tclose(tid1);
    assert(ret>=0);
    ret = H5Sclose(sid1);
    assert(ret>=0);
    ret = H5Fclose(fid1);
    assert(ret>=0);
}

static void gent_array5(void)
{
    typedef struct {        /* Typedef for compound datatype */
        int i;
        float f[ARRAY1_DIM1];
    } s2_t;
    s2_t wdata[SPACE1_DIM1][ARRAY1_DIM1];   /* Information to write */
    hid_t  fid1;  /* HDF5 File IDs  */
    hid_t  dataset; /* Dataset ID   */
    hid_t  sid1;       /* Dataspace ID   */
    hid_t  tid1;       /* Array Datatype ID */
    hid_t  tid2;       /* Compound Datatype ID */
    hid_t  tid3;       /* Nested Array Datatype ID */
    hsize_t  sdims1[] = {SPACE1_DIM1};
    hsize_t  tdims1[] = {ARRAY1_DIM1};
    int        i,j,k;      /* counting variables */
    herr_t  ret;  /* Generic return value  */

    /* Initialize array data to write */
    for(i=0; i<SPACE1_DIM1; i++)
        for(j=0; j<ARRAY1_DIM1; j++) {
            wdata[i][j].i=i*10+j;
            for(k=0; k<ARRAY1_DIM1; k++)
                wdata[i][j].f[k]=(float)(i*10+j*2.5+k);
        } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILE29, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, sdims1, NULL);

    /* Create a compound datatype to refer to */
    tid2 = H5Tcreate(H5T_COMPOUND, sizeof(s2_t));

    /* Insert integer field */
    ret = H5Tinsert (tid2, "i", HOFFSET(s2_t,i), H5T_NATIVE_INT);
    assert(ret>=0);

    /* Create an array of floats datatype */
    tid3 = H5Tarray_create (H5T_NATIVE_FLOAT,ARRAY1_RANK,tdims1,NULL);

    /* Insert float array field */
    ret = H5Tinsert (tid2, "f", HOFFSET(s2_t,f), tid3);
    assert(ret>=0);

    /* Close array of floats field datatype */
    ret=H5Tclose(tid3);
    assert(ret>=0);

    /* Create an array datatype to refer to */
    tid1 = H5Tarray_create (tid2,ARRAY1_RANK,tdims1,NULL);

    /* Close compound datatype */
    ret=H5Tclose(tid2);
    assert(ret>=0);

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",tid1,sid1,H5P_DEFAULT);

    /* Write dataset to disk */
    ret=H5Dwrite(dataset,tid1,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    assert(ret>=0);
    ret = H5Tclose(tid1);
    assert(ret>=0);
    ret = H5Sclose(sid1);
    assert(ret>=0);
    ret = H5Fclose(fid1);
    assert(ret>=0);
}

static void gent_array6(void)
{
    hvl_t wdata[SPACE1_DIM1][ARRAY1_DIM1];   /* Information to write */
    hid_t  fid1;  /* HDF5 File IDs  */
    hid_t  dataset; /* Dataset ID   */
    hid_t  sid1;       /* Dataspace ID   */
    hid_t  tid1;       /* Array Datatype ID   */
    hid_t  tid2;       /* VL Datatype ID       */
    hsize_t  sdims1[] = {SPACE1_DIM1};
    hsize_t  tdims1[] = {ARRAY1_DIM1};
    int        i,j,k;      /* counting variables */
    herr_t  ret;  /* Generic return value  */

    /* Initialize array data to write */
    for(i=0; i<SPACE1_DIM1; i++)
        for(j=0; j<ARRAY1_DIM1; j++) {
            wdata[i][j].p=malloc((i+j+1)*sizeof(unsigned int));
            wdata[i][j].len=i+j+1;
            for(k=0; k<(i+j+1); k++)
                ((unsigned int *)wdata[i][j].p)[k]=i*100+j*10+k;
        } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILE30, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, sdims1, NULL);

    /* Create a compound datatype to refer to */
    tid2 = H5Tvlen_create(H5T_NATIVE_UINT);

    /* Create an array datatype to refer to */
    tid1 = H5Tarray_create (tid2,ARRAY1_RANK,tdims1,NULL);

    /* Close VL datatype */
    ret=H5Tclose(tid2);
    assert(ret>=0);

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",tid1,sid1,H5P_DEFAULT);

    /* Write dataset to disk */
    ret=H5Dwrite(dataset,tid1,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Reclaim the write VL data */
    ret=H5Dvlen_reclaim(tid1,sid1,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    assert(ret>=0);
    ret = H5Tclose(tid1);
    assert(ret>=0);
    ret = H5Sclose(sid1);
    assert(ret>=0);
    ret = H5Fclose(fid1);
    assert(ret>=0);
}

static void gent_array7(void)
{
    hvl_t wdata[SPACE1_DIM1][ARRAY1_DIM1];   /* Information to write */
    hid_t  fid1;  /* HDF5 File IDs  */
    hid_t  dataset; /* Dataset ID   */
    hid_t  sid1;       /* Dataspace ID   */
    hid_t  tid1;       /* Array Datatype ID   */
    hid_t  tid2;       /* VL Datatype ID       */
    hid_t  tid3;       /* Nested Array Datatype ID   */
    hsize_t  sdims1[] = {SPACE1_DIM1};
    hsize_t  tdims1[] = {ARRAY1_DIM1};
    int        i,j,k,l;    /* Index variables */
    herr_t  ret;  /* Generic return value  */

    /* Initialize array data to write */
    for(i=0; i<SPACE1_DIM1; i++)
        for(j=0; j<ARRAY1_DIM1; j++) {
            wdata[i][j].p=malloc((i+j+1)*(sizeof(unsigned int)*ARRAY1_DIM1));
            wdata[i][j].len=i+j+1;
            for(k=0; k<(i+j+1); k++)
                for(l=0; l<ARRAY1_DIM1; l++)
                    ((unsigned int *)wdata[i][j].p)[k*ARRAY1_DIM1+l]=i*1000+j*100+k*10+l;
        } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILE31, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, sdims1, NULL);

    /* Create the nested array datatype to refer to */
    tid3 = H5Tarray_create(H5T_NATIVE_UINT,ARRAY1_RANK,tdims1,NULL);

    /* Create a VL datatype of 1-D arrays to refer to */
    tid2 = H5Tvlen_create(tid3);

    /* Close nested array datatype */
    ret=H5Tclose(tid3);
    assert(ret>=0);

    /* Create an array datatype to refer to */
    tid1 = H5Tarray_create (tid2,ARRAY1_RANK,tdims1,NULL);

    /* Close VL datatype */
    ret=H5Tclose(tid2);
    assert(ret>=0);

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",tid1,sid1,H5P_DEFAULT);

    /* Write dataset to disk */
    ret=H5Dwrite(dataset,tid1,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Reclaim the write VL data */
    ret=H5Dvlen_reclaim(tid1,sid1,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    assert(ret>=0);
    ret = H5Tclose(tid1);
    assert(ret>=0);
    ret = H5Sclose(sid1);
    assert(ret>=0);
    ret = H5Fclose(fid1);
    assert(ret>=0);
}

static void gent_empty(void)
{
    typedef struct {
        int a;
        float b;
        char c;
    } empty_struct;
    hid_t file, dset, space, type;
    hsize_t dims[] = { SPACE1_DIM1 };
    herr_t ret=0;

    file = H5Fcreate(FILE32, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    space = H5Screate_simple(SPACE1_RANK, dims, NULL);

    /* write out an empty vlen dataset */
    type = H5Tvlen_create(H5T_NATIVE_INT);
    dset = H5Dcreate(file, "Dataset1.0", type, space, H5P_DEFAULT);
    /* Don't write any data */
    ret = H5Dclose(dset);
    assert(ret>=0);
    ret = H5Tclose(type);
    assert(ret>=0);

    /* write out an empty native integer dataset dataset */
    dset = H5Dcreate(file, "Dataset2.0", H5T_NATIVE_INT, space, H5P_DEFAULT);
    /* Don't write any data */
    ret = H5Dclose(dset);
    assert(ret>=0);

    /* write out an empty native floating-point dataset dataset */
    dset = H5Dcreate(file, "Dataset3.0", H5T_NATIVE_FLOAT, space, H5P_DEFAULT);
    /* Don't write any data */
    ret = H5Dclose(dset);
    assert(ret>=0);

    /* write out an empty array dataset */
    type = H5Tarray_create(H5T_NATIVE_INT,SPACE1_RANK,dims,NULL);
    dset = H5Dcreate(file, "Dataset4.0", type, space, H5P_DEFAULT);
    /* Don't write any data */
    ret = H5Dclose(dset);
    assert(ret>=0);
    ret = H5Tclose(type);
    assert(ret>=0);

    /* write out an empty compound dataset */
    type = H5Tcreate(H5T_COMPOUND,sizeof(empty_struct));
    H5Tinsert(type, "a", HOFFSET(empty_struct, a),H5T_NATIVE_INT);
    H5Tinsert(type, "b", HOFFSET(empty_struct, b),H5T_NATIVE_FLOAT);
    H5Tinsert(type, "c", HOFFSET(empty_struct, c),H5T_NATIVE_CHAR);
    dset = H5Dcreate(file, "Dataset5.0", type, space, H5P_DEFAULT);
    /* Don't write any data */
    ret = H5Dclose(dset);
    assert(ret>=0);
    ret = H5Tclose(type);
    assert(ret>=0);

    ret = H5Sclose(space);
    assert(ret>=0);

    ret = H5Fclose(file);
    assert(ret>=0);
}

static void gent_group_comments(void)
{
    hid_t fid, group;
  
    fid = H5Fcreate(FILE33, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  
    /* / */
    group = H5Gcreate (fid, "/g1", 0);
    H5Gset_comment(group, "/g1", "Comment for group /g1");
    H5Gclose(group);
    group = H5Gcreate (fid, "/g2", 0);
    H5Gset_comment(group, "/g2", "Comment for group /g2");
    H5Gclose(group);
    group = H5Gcreate (fid, "/g3", 0);
    H5Gset_comment(group, "/g3", "Comment for group /g3");
    H5Gclose(group);
  
    /* /g1 */
    group = H5Gcreate (fid, "/g1/g1.1", 0);
    H5Gset_comment(group, "/g1/g1.1", "Comment for group /g1/g1.1");
    H5Gclose(group);
    group = H5Gcreate (fid, "/g1/g1.2", 0);
    H5Gset_comment(group, "/g1/g1.2", "Comment for group /g1/g1.2");
    H5Gclose(group);
  
    /* /g2 */
    group = H5Gcreate (fid, "/g2/g2.1", 0);
    H5Gset_comment(group, "/g2/g2.1", "Comment for group /g2/g2.1");
    H5Gclose(group);
  
    /* /g3 */
    group = H5Gcreate (fid, "/g3/g3.1", 0);
    H5Gset_comment(group, "/g3/g3.1", "Comment for group /g3/g3.1");
    H5Gclose(group);
    group = H5Gcreate (fid, "/g3/g3.2", 0);
    H5Gset_comment(group, "/g3/g3.2", "Comment for group /g3/g3.2");
    H5Gclose(group);
    group = H5Gcreate (fid, "/g3/g3.3", 0);
    H5Gset_comment(group, "/g3/g3.3", "Comment for group /g3/g3.3");
    H5Gclose(group);
    group = H5Gcreate (fid, "/g3/g3.4", 0);
    H5Gset_comment(group, "/g3/g3.4", "Comment for group /g3/g3.4");
    H5Gclose(group);
  
    /* /g2/g2.1 */
    group = H5Gcreate (fid, "/g2/g2.1/g2.1.1", 0);
    H5Gset_comment(group, "/g2/g2.1/g2.1.1", "Comment for group /g2/g2.1/g2.1.1");
    H5Gclose(group);
    group = H5Gcreate (fid, "/g2/g2.1/g2.1.2", 0);
    H5Gset_comment(group, "/g2/g2.1/g2.1.2", "Comment for group /g2/g2.1/g2.1.2");
    H5Gclose(group);
    group = H5Gcreate (fid, "/g2/g2.1/g2.1.3", 0);
    H5Gset_comment(group, "/g2/g2.1/g2.1.3", "Comment for group /g2/g2.1/g2.1.3");
    H5Gclose(group);
  
    H5Fclose(fid);
}

static
void gent_split_file(void)
{
    hid_t fapl, fid, root, attr, space, dataset, atype;
    char meta[] = "this is some metadata on this file";
    hsize_t dims[2];
    int i, j, dset[10][15];

    fapl = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_fapl_split(fapl, "-m.h5", H5P_DEFAULT, "-r.h5", H5P_DEFAULT);
    fid = H5Fcreate(FILE34, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    root = H5Gopen(fid, "/");

    atype = H5Tcopy(H5T_C_S1);
    H5Tset_size(atype, strlen(meta) + 1);
    H5Tset_strpad(atype, H5T_STR_NULLTERM);

    dims[0] = 1;
    space = H5Screate_simple(1, dims, NULL);
    attr = H5Acreate(root, "Metadata", atype, space, H5P_DEFAULT);
    H5Awrite(attr, atype, meta);
    H5Tclose(atype);
    H5Sclose(space);
    H5Aclose(attr);

    /* create dataset */
    dims[0] = 10;
    dims[1] = 15;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate(fid, "/dset1", H5T_STD_I32BE, space, H5P_DEFAULT);

    for (i = 0; i < 10; i++)
         for (j = 0; j < 15; j++)
              dset[i][j] = i + j;

    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset);
    H5Sclose(space);
    H5Dclose(dataset);
    H5Gclose(root);
    H5Fclose(fid);
    H5Pclose(fapl);
}

static
void gent_family(void)
{
    hid_t fapl, fid, space, dataset;
    hsize_t dims[2];
    int i, j, dset[10][15];

#define FAMILY_SIZE     256

    fapl = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_fapl_family(fapl, (hsize_t)FAMILY_SIZE, H5P_DEFAULT);

    fid = H5Fcreate(FILE35, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

    /* create dataset */
    dims[0] = 10;
    dims[1] = 15;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate(fid, "/dset1", H5T_STD_I32BE, space, H5P_DEFAULT);

    for (i = 0; i < 10; i++)
         for (j = 0; j < 15; j++)
              dset[i][j] = i + j;

    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset);
    H5Sclose(space);
    H5Dclose(dataset);
    H5Fclose(fid);
    H5Pclose(fapl);
}

static const char *multi_letters = "msbrglo";

static
void gent_multi(void)
{
    hid_t fapl, fid, space, dataset;
    hsize_t dims[2];
    int i, j, dset[10][15];

    /* Multi-file driver, general case of the split driver */
    H5FD_mem_t mt, memb_map[H5FD_MEM_NTYPES];
    hid_t memb_fapl[H5FD_MEM_NTYPES];
    const char *memb_name[H5FD_MEM_NTYPES];
    char sv[H5FD_MEM_NTYPES][1024];
    haddr_t memb_addr[H5FD_MEM_NTYPES];

    fapl = H5Pcreate(H5P_FILE_ACCESS);

    HDmemset(memb_map, 0, sizeof memb_map);
    HDmemset(memb_fapl, 0, sizeof memb_fapl);
    HDmemset(memb_name, 0, sizeof memb_name);
    HDmemset(memb_addr, 0, sizeof memb_addr);

    assert(HDstrlen(multi_letters) == H5FD_MEM_NTYPES);

    for (mt = H5FD_MEM_DEFAULT; mt < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t,mt)) {
        memb_fapl[mt] = H5P_DEFAULT;
        sprintf(sv[mt], "%%s-%c.h5", multi_letters[mt]);
        memb_name[mt] = sv[mt];
        memb_addr[mt] = MAX(mt - 1,0) * (HADDR_MAX / 10);
    }

    H5Pset_fapl_multi(fapl, memb_map, memb_fapl, memb_name,
                      memb_addr, FALSE);

    fid = H5Fcreate(FILE36, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

    /* create dataset */
    dims[0] = 10;
    dims[1] = 15;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate(fid, "/dset1", H5T_STD_I32BE, space, H5P_DEFAULT);

    for (i = 0; i < 10; i++)
         for (j = 0; j < 15; j++)
              dset[i][j] = i + j;

    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset);
    H5Sclose(space);
    H5Dclose(dataset);
    H5Fclose(fid);
    H5Pclose(fapl);
}

static void gent_large_objname(void)
{
    hid_t fid, group, group2;
    char grp_name[128];
    register int i;
  
    fid = H5Fcreate(FILE37, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    group = H5Gcreate(fid, "this_is_a_large_group_name", 0);

    for (i = 0; i < 50; ++i) {
        sprintf(grp_name, "this_is_a_large_group_name%d", i);
        group2 = H5Gcreate(group, grp_name, 0);
        H5Gclose(group2);
    }

    H5Gclose(group);
    H5Fclose(fid);
}

static void gent_vlstr(void)
{
    const char *wdata[SPACE1_DIM1]= {
        "Four score and seven years ago our forefathers brought forth on this continent a new nation,",
        "conceived in liberty and dedicated to the proposition that all men are created equal.",
        "",
        NULL
        };   /* Information to write */
    const char *string_att= "This is the string for the attribute";
    hid_t  fid1;  /* HDF5 File IDs  */
    hid_t  dataset, root; /* Dataset ID   */
    hid_t  sid1, dataspace;/* Dataspace ID   */
    hid_t  tid1, att;      /* Datatype ID   */
    hsize_t  dims1[] = {SPACE1_DIM1};

    /* Create file */
    fid1 = H5Fcreate(FILE38, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);

    /* Create a VL string datatype to refer to */
    tid1 = H5Tcopy (H5T_C_S1);
    H5Tset_size (tid1,H5T_VARIABLE);

    /* Create a dataset and write VL string to it. */
    dataset=H5Dcreate(fid1,"Dataset1",tid1,sid1,H5P_DEFAULT);
    H5Dwrite(dataset,tid1,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata);
    H5Dclose(dataset);

    /* Create a named VL string type.  Change padding of datatype */
    H5Tset_strpad(tid1, H5T_STR_NULLPAD);
    H5Tcommit(fid1, "vl_string_type", tid1);

    /* Create an group attribute of VL string type */
    root = H5Gopen(fid1, "/");
    dataspace = H5Screate(H5S_SCALAR);

    att = H5Acreate(root, "test_scalar", tid1, dataspace, H5P_DEFAULT);
    H5Awrite(att, tid1, &string_att);

    /* Close */
    H5Tclose(tid1);
    H5Sclose(sid1);
    H5Sclose(dataspace);
    H5Aclose(att);
    H5Gclose(root);
    H5Fclose(fid1);
}

static void gent_char(void)
{
    const char *wdata =
        "Four score and seven years ago our forefathers brought "
        "forth on this continent a new nation, conceived in "
        "liberty and dedicated to the proposition that all "
        "men are created equal. Now we are engaged in a great "
        "civil war, testing whether that nation or any nation "
        "so conceived and so dedicated can long endure.";
    hid_t       fid1;               /* HDF5 File IDs    */
    hid_t       dataset;            /* Dataset ID       */
    hid_t       sid1;               /* Dataspace ID     */
    hsize_t     dims1[1];

    dims1[0] = strlen(wdata);

    /* Create file */
    fid1 = H5Fcreate(FILE39, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    sid1 = H5Screate_simple(1, dims1, NULL);

    /* Create a dataset */
    dataset = H5Dcreate(fid1, "Dataset1", H5T_NATIVE_CHAR, sid1, H5P_DEFAULT);

    /* Write some characters to it. */
    H5Dwrite(dataset, H5T_NATIVE_CHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);

    /* Close */
    H5Dclose(dataset);
    H5Sclose(sid1);
    H5Fclose(fid1);
}



/*-------------------------------------------------------------------------
 * Function: write_attr_in
 *
 * Purpose: write attributes in LOC_ID (dataset, group, named datatype) 
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 28, 2003
 *
 *-------------------------------------------------------------------------
 */

static void write_attr_in(hid_t loc_id, 
                          const char* dset_name, /* for saving reference to dataset*/
                          hid_t file_id)
{
 /* Compound datatype */
 typedef struct s_t 
 {
  char   a;
  double b;
 } s_t;

 typedef enum 
 {
  E_RED,
  E_GREEN
 } e_t;

 hid_t   attr_id;
 hid_t   space_id;  
 hid_t   type_id;  
 herr_t  status;
 int     val, i, j, k, n;
 float   f;

 /* create 1D attributes with dimension [2], 2 elements */
 hsize_t    dims[1]={2};
 char       buf1[2][2]= {"ab","de"};        /* string */
 char       buf2[2]= {1,2};                 /* bitfield, opaque */
 s_t        buf3[2]= {{1,2},{3,4}};         /* compound */
 hobj_ref_t buf4[2];                        /* reference */
 hvl_t      buf5[2];                        /* vlen */
 hsize_t    dimarray[1]={3};                /* array dimension */
 int        buf6[2][3]= {{1,2,3},{4,5,6}};  /* array */
 int        buf7[2]= {1,2};                 /* integer */
 float      buf8[2]= {1,2};                 /* float */

 /* create 2D attributes with dimension [3][2], 6 elements */
 hsize_t    dims2[2]={3,2};
 char       buf12[6][2]= {"ab","cd","ef","gh","ij","kl"};         /* string */
 char       buf22[3][2]= {{1,2},{3,4},{5,6}};                     /* bitfield, opaque */
 s_t        buf32[6]= {{1,2},{3,4},{5,6},{7,8},{9,10},{11,12}};   /* compound */
 hobj_ref_t buf42[3][2];                                          /* reference */
 hvl_t      buf52[3][2];                                          /* vlen */
 int        buf62[6][3]= {{1,2,3},{4,5,6},{7,8,9},{10,11,12},{13,14,15},{16,17,18}};  /* array */
 int        buf72[3][2]= {{1,2},{3,4},{5,6}};                     /* integer */
 float      buf82[3][2]= {{1,2},{3,4},{5,6}};                     /* float */

 /* create 3D attributes with dimension [4][3][2], 24 elements */
 hsize_t    dims3[3]={4,3,2};
 char       buf13[24][2]= {"ab","cd","ef","gh","ij","kl","mn","pq",
 "rs","tu","vw","xz","AB","CD","EF","GH",
 "IJ","KL","MN","PQ","RS","TU","VW","XZ"};  /* string */
 char       buf23[4][3][2];    /* bitfield, opaque */
 s_t        buf33[4][3][2];    /* compound */
 hobj_ref_t buf43[4][3][2];    /* reference */
 hvl_t      buf53[4][3][2];    /* vlen */
 int        buf63[24][3];      /* array */
 int        buf73[4][3][2];    /* integer */
 float      buf83[4][3][2];    /* float */


/*-------------------------------------------------------------------------
 * 1D attributes
 *-------------------------------------------------------------------------
 */
 
/*-------------------------------------------------------------------------
 * H5T_STRING
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcopy(H5T_C_S1);
 status  = H5Tset_size(type_id, 2);
 write_attr(loc_id,1,dims,"string",type_id,buf1);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_BITFIELD
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcopy(H5T_STD_B8LE);
 write_attr(loc_id,1,dims,"bitfield",type_id,buf2);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_OPAQUE
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcreate(H5T_OPAQUE, 1);
 status = H5Tset_tag(type_id, "1-byte opaque type"); /* must set this */
 write_attr(loc_id,1,dims,"opaque",type_id,buf2);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_COMPOUND
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
 H5Tinsert(type_id, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
 H5Tinsert(type_id, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
 write_attr(loc_id,1,dims,"compound",type_id,buf3);
 status = H5Tclose(type_id);
 
/*-------------------------------------------------------------------------
 * H5T_REFERENCE
 *-------------------------------------------------------------------------
 */
 /* Create references to dataset */
 if (dset_name)
 {
  status=H5Rcreate(&buf4[0],file_id,dset_name,H5R_OBJECT,-1);
  status=H5Rcreate(&buf4[1],file_id,dset_name,H5R_OBJECT,-1);
  write_attr(loc_id,1,dims,"reference",H5T_STD_REF_OBJ,buf4);
 }

/*-------------------------------------------------------------------------
 * H5T_ENUM
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcreate(H5T_ENUM, sizeof(e_t));
 H5Tenum_insert(type_id, "RED",   (val = 0, &val));
 H5Tenum_insert(type_id, "GREEN", (val = 1, &val));
 write_attr(loc_id,1,dims,"enum",type_id,0);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_VLEN
 *-------------------------------------------------------------------------
 */
 
 /* Allocate and initialize VL dataset to write */

 buf5[0].len = 1;
 buf5[0].p = malloc( 1 * sizeof(int));
 ((int *)buf5[0].p)[0]=1;
 buf5[1].len = 2;
 buf5[1].p = malloc( 2 * sizeof(int));
 ((int *)buf5[1].p)[0]=2;
 ((int *)buf5[1].p)[1]=3;

 space_id = H5Screate_simple(1,dims,NULL);
 type_id = H5Tvlen_create(H5T_NATIVE_INT);
 attr_id = H5Acreate(loc_id,"vlen",type_id,space_id,H5P_DEFAULT);
 status = H5Awrite(attr_id,type_id,buf5);
 assert(status>=0);
 status = H5Dvlen_reclaim(type_id,space_id,H5P_DEFAULT,buf5);
 assert(status>=0);
 status = H5Aclose(attr_id);
 status = H5Tclose(type_id);
 status = H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * H5T_ARRAY
 *-------------------------------------------------------------------------
 */
 type_id = H5Tarray_create(H5T_NATIVE_INT,1,dimarray,NULL);
 write_attr(loc_id,1,dims,"array",type_id,buf6);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_INTEGER and H5T_FLOAT
 *-------------------------------------------------------------------------
 */
 write_attr(loc_id,1,dims,"integer",H5T_NATIVE_INT,buf7);
 write_attr(loc_id,1,dims,"float",H5T_NATIVE_FLOAT,buf8);


/*-------------------------------------------------------------------------
 * 2D attributes
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * H5T_STRING
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcopy(H5T_C_S1);
 status  = H5Tset_size(type_id, 2);
 write_attr(loc_id,2,dims2,"string2D",type_id,buf12);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_BITFIELD
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcopy(H5T_STD_B8LE);
 write_attr(loc_id,2,dims2,"bitfield2D",type_id,buf22);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_OPAQUE
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcreate(H5T_OPAQUE, 1);
 status = H5Tset_tag(type_id, "1-byte opaque type"); /* must set this */
 write_attr(loc_id,2,dims2,"opaque2D",type_id,buf22);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_COMPOUND
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
 H5Tinsert(type_id, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
 H5Tinsert(type_id, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
 write_attr(loc_id,2,dims2,"compound2D",type_id,buf32);
 status = H5Tclose(type_id);
 
/*-------------------------------------------------------------------------
 * H5T_REFERENCE
 *-------------------------------------------------------------------------
 */
 /* Create references to dataset */
 if (dset_name)
 {
  for (i = 0; i < 3; i++) {
   for (j = 0; j < 2; j++) {
    status=H5Rcreate(&buf42[i][j],file_id,dset_name,H5R_OBJECT,-1);
   }
  }
  write_attr(loc_id,2,dims2,"reference2D",H5T_STD_REF_OBJ,buf42);
 }

/*-------------------------------------------------------------------------
 * H5T_ENUM
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcreate(H5T_ENUM, sizeof(e_t));
 H5Tenum_insert(type_id, "RED",   (val = 0, &val));
 H5Tenum_insert(type_id, "GREEN", (val = 1, &val));
 write_attr(loc_id,2,dims2,"enum2D",type_id,0);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_VLEN
 *-------------------------------------------------------------------------
 */

 /* Allocate and initialize VL dataset to write */
 n=0;
 for (i = 0; i < 3; i++) {
  for (j = 0; j < 2; j++) {
    int l;
    buf52[i][j].p = malloc((i + 1) * sizeof(int));
    buf52[i][j].len = i + 1;
    for (l = 0; l < i + 1; l++)
    ((int *)buf52[i][j].p)[l] = n++;
  }
 }
 
 space_id = H5Screate_simple(2,dims2,NULL);
 type_id = H5Tvlen_create(H5T_NATIVE_INT);
 attr_id = H5Acreate(loc_id,"vlen2D",type_id,space_id,H5P_DEFAULT);
 status = H5Awrite(attr_id,type_id,buf52);
 assert(status>=0);
 status = H5Dvlen_reclaim(type_id,space_id,H5P_DEFAULT,buf52);
 assert(status>=0);
 status = H5Aclose(attr_id);
 status = H5Tclose(type_id);
 status = H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * H5T_ARRAY
 *-------------------------------------------------------------------------
 */
 type_id = H5Tarray_create(H5T_NATIVE_INT,1,dimarray,NULL);
 write_attr(loc_id,2,dims2,"array2D",type_id,buf62);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_INTEGER and H5T_FLOAT
 *-------------------------------------------------------------------------
 */
 write_attr(loc_id,2,dims2,"integer2D",H5T_NATIVE_INT,buf72);
 write_attr(loc_id,2,dims2,"float2D",H5T_NATIVE_FLOAT,buf82);

 
/*-------------------------------------------------------------------------
 * 3D attributes
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * H5T_STRING
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcopy(H5T_C_S1);
 status  = H5Tset_size(type_id, 2);
 write_attr(loc_id,3,dims3,"string3D",type_id,buf13);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_BITFIELD
 *-------------------------------------------------------------------------
 */

 n=1;
 for (i = 0; i < 4; i++) {
  for (j = 0; j < 3; j++) {
   for (k = 0; k < 2; k++) {
    buf23[i][j][k]=n++;
   }
  }
 }
 type_id = H5Tcopy(H5T_STD_B8LE);
 write_attr(loc_id,3,dims3,"bitfield3D",type_id,buf23);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_OPAQUE
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcreate(H5T_OPAQUE, 1);
 status = H5Tset_tag(type_id, "1-byte opaque type"); /* must set this */
 write_attr(loc_id,3,dims3,"opaque3D",type_id,buf23);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_COMPOUND
 *-------------------------------------------------------------------------
 */

 n=1;
 for (i = 0; i < 4; i++) {
  for (j = 0; j < 3; j++) {
   for (k = 0; k < 2; k++) {
    buf33[i][j][k].a=n++;
    buf33[i][j][k].b=n++;
   }
  }
 }
 type_id = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
 H5Tinsert(type_id, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
 H5Tinsert(type_id, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
 write_attr(loc_id,3,dims3,"compound3D",type_id,buf33);
 status = H5Tclose(type_id);
 
/*-------------------------------------------------------------------------
 * H5T_REFERENCE
 *-------------------------------------------------------------------------
 */
 /* Create references to dataset */
 if (dset_name)
 {
  for (i = 0; i < 4; i++) {
   for (j = 0; j < 3; j++) {
    for (k = 0; k < 2; k++)
     status=H5Rcreate(&buf43[i][j][k],file_id,dset_name,H5R_OBJECT,-1);
   }
  }
 write_attr(loc_id,3,dims3,"reference3D",H5T_STD_REF_OBJ,buf43);
 }

/*-------------------------------------------------------------------------
 * H5T_ENUM
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcreate(H5T_ENUM, sizeof(e_t));
 H5Tenum_insert(type_id, "RED",   (val = 0, &val));
 H5Tenum_insert(type_id, "GREEN", (val = 1, &val));
 write_attr(loc_id,3,dims3,"enum3D",type_id,0);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_VLEN
 *-------------------------------------------------------------------------
 */
 
 /* Allocate and initialize VL dataset to write */
 n=0;
 for (i = 0; i < 4; i++) {
  for (j = 0; j < 3; j++) {
   for (k = 0; k < 2; k++) {
    int l;
    buf53[i][j][k].p = malloc((i + 1) * sizeof(int));
    buf53[i][j][k].len = i + 1;
    for (l = 0; l < i + 1; l++)
    ((int *)buf53[i][j][k].p)[l] = n++;
   }
  }
 }
 
 space_id = H5Screate_simple(3,dims3,NULL);
 type_id = H5Tvlen_create(H5T_NATIVE_INT);
 attr_id = H5Acreate(loc_id,"vlen3D",type_id,space_id,H5P_DEFAULT);
 status = H5Awrite(attr_id,type_id,buf53);
 assert(status>=0);
 status = H5Dvlen_reclaim(type_id,space_id,H5P_DEFAULT,buf53);
 assert(status>=0);
 status = H5Aclose(attr_id);
 status = H5Tclose(type_id);
 status = H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * H5T_ARRAY
 *-------------------------------------------------------------------------
 */
 n=1;
 for (i = 0; i < 24; i++) {
  for (j = 0; j < (int)dimarray[0]; j++) {
    buf63[i][j]=n++;
  }
 }

 type_id = H5Tarray_create(H5T_NATIVE_INT,1,dimarray,NULL);
 write_attr(loc_id,3,dims3,"array3D",type_id,buf63);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_INTEGER and H5T_FLOAT
 *-------------------------------------------------------------------------
 */
 n=1; f=1;
 for (i = 0; i < 4; i++) {
  for (j = 0; j < 3; j++) {
   for (k = 0; k < 2; k++) {
    buf73[i][j][k]=n++;
    buf83[i][j][k]=f++;
   }
  }
 }
 write_attr(loc_id,3,dims3,"integer3D",H5T_NATIVE_INT,buf73);
 write_attr(loc_id,3,dims3,"float3D",H5T_NATIVE_FLOAT,buf83);
}



/*-------------------------------------------------------------------------
 * Function: write_dset_in
 *
 * Purpose: write datasets in LOC_ID 
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 28, 2003
 *
 *-------------------------------------------------------------------------
 */

static void write_dset_in(hid_t loc_id, 
                          const char* dset_name, /* for saving reference to dataset*/
                          hid_t file_id)
{
 /* Compound datatype */
 typedef struct s_t 
 {
  char   a;
  double b;
 } s_t;

 typedef enum 
 {
  E_RED,
  E_GREEN
 } e_t;

 hid_t   dset_id;
 hid_t   space_id;  
 hid_t   type_id;  
 hid_t   plist_id;
 herr_t  status;
 int     val, i, j, k, n;
 float   f;
 int     fillvalue=2;

 /* create 1D attributes with dimension [2], 2 elements */
 hsize_t    dims[1]={2};
 char       buf1[2][2]= {"ab","de"};        /* string */
 char       buf2[2]= {1,2};                 /* bitfield, opaque */
 s_t        buf3[2]= {{1,2},{3,4}};         /* compound */
 hobj_ref_t buf4[2];                        /* reference */
 hvl_t      buf5[2];                        /* vlen */
 hsize_t    dimarray[1]={3};                /* array dimension */
 int        buf6[2][3]= {{1,2,3},{4,5,6}};  /* array */
 int        buf7[2]= {1,2};                 /* integer */
 float      buf8[2]= {1,2};                 /* float */

 /* create 2D attributes with dimension [3][2], 6 elements */
 hsize_t    dims2[2]={3,2};
 char       buf12[6][2]= {"ab","cd","ef","gh","ij","kl"};         /* string */
 char       buf22[3][2]= {{1,2},{3,4},{5,6}};                     /* bitfield, opaque */
 s_t        buf32[6]= {{1,2},{3,4},{5,6},{7,8},{9,10},{11,12}};   /* compound */
 hobj_ref_t buf42[3][2];                                          /* reference */
 hvl_t      buf52[3][2];                                          /* vlen */
 int        buf62[6][3]= {{1,2,3},{4,5,6},{7,8,9},{10,11,12},{13,14,15},{16,17,18}};  /* array */
 int        buf72[3][2]= {{1,2},{3,4},{5,6}};                     /* integer */
 float      buf82[3][2]= {{1,2},{3,4},{5,6}};                     /* float */

 /* create 3D attributes with dimension [4][3][2], 24 elements */
 hsize_t    dims3[3]={4,3,2};
 char       buf13[24][2]= {"ab","cd","ef","gh","ij","kl","mn","pq",
 "rs","tu","vw","xz","AB","CD","EF","GH",
 "IJ","KL","MN","PQ","RS","TU","VW","XZ"};  /* string */
 char       buf23[4][3][2];    /* bitfield, opaque */
 s_t        buf33[4][3][2];    /* compound */
 hobj_ref_t buf43[4][3][2];    /* reference */
 hvl_t      buf53[4][3][2];    /* vlen */
 int        buf63[24][3];      /* array */
 int        buf73[4][3][2];    /* integer */
 float      buf83[4][3][2];    /* float */


/*-------------------------------------------------------------------------
 * 1D 
 *-------------------------------------------------------------------------
 */
 
/*-------------------------------------------------------------------------
 * H5T_STRING
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcopy(H5T_C_S1);
 status  = H5Tset_size(type_id, 2);
 write_dset(loc_id,1,dims,"string",type_id,buf1);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_BITFIELD
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcopy(H5T_STD_B8LE);
 write_dset(loc_id,1,dims,"bitfield",type_id,buf2);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_OPAQUE
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcreate(H5T_OPAQUE, 1);
 status = H5Tset_tag(type_id, "1-byte opaque type"); /* must set this */
 write_dset(loc_id,1,dims,"opaque",type_id,buf2);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_COMPOUND
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
 H5Tinsert(type_id, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
 H5Tinsert(type_id, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
 write_dset(loc_id,1,dims,"compound",type_id,buf3);
 status = H5Tclose(type_id);
 
/*-------------------------------------------------------------------------
 * H5T_REFERENCE
 *-------------------------------------------------------------------------
 */
 /* Create references to dataset */
 if (dset_name)
 {
  status=H5Rcreate(&buf4[0],file_id,dset_name,H5R_OBJECT,-1);
  status=H5Rcreate(&buf4[1],file_id,dset_name,H5R_OBJECT,-1);
  write_dset(loc_id,1,dims,"reference",H5T_STD_REF_OBJ,buf4);
 }

/*-------------------------------------------------------------------------
 * H5T_ENUM
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcreate(H5T_ENUM, sizeof(e_t));
 H5Tenum_insert(type_id, "RED",   (val = 0, &val));
 H5Tenum_insert(type_id, "GREEN", (val = 1, &val));
 write_dset(loc_id,1,dims,"enum",type_id,0);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_VLEN
 *-------------------------------------------------------------------------
 */
 
 /* Allocate and initialize VL dataset to write */

 buf5[0].len = 1;
 buf5[0].p = malloc( 1 * sizeof(int));
 ((int *)buf5[0].p)[0]=1;
 buf5[1].len = 2;
 buf5[1].p = malloc( 2 * sizeof(int));
 ((int *)buf5[1].p)[0]=2;
 ((int *)buf5[1].p)[1]=3;

 space_id = H5Screate_simple(1,dims,NULL);
 type_id = H5Tvlen_create(H5T_NATIVE_INT);
 dset_id = H5Dcreate(loc_id,"vlen",type_id,space_id,H5P_DEFAULT);
 status = H5Dwrite(dset_id,type_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf5);
 assert(status>=0);
 status = H5Dvlen_reclaim(type_id,space_id,H5P_DEFAULT,buf5);
 assert(status>=0);
 status = H5Dclose(dset_id);
 status = H5Tclose(type_id);
 status = H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * H5T_ARRAY
 *-------------------------------------------------------------------------
 */
 type_id = H5Tarray_create(H5T_NATIVE_INT,1,dimarray,NULL);
 write_dset(loc_id,1,dims,"array",type_id,buf6);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_INTEGER and H5T_FLOAT
 *-------------------------------------------------------------------------
 */
 write_dset(loc_id,1,dims,"integer",H5T_NATIVE_INT,buf7);
 write_dset(loc_id,1,dims,"float",H5T_NATIVE_FLOAT,buf8);


/*-------------------------------------------------------------------------
 * 2D 
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * H5T_STRING
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcopy(H5T_C_S1);
 status  = H5Tset_size(type_id, 2);
 write_dset(loc_id,2,dims2,"string2D",type_id,buf12);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_BITFIELD
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcopy(H5T_STD_B8LE);
 write_dset(loc_id,2,dims2,"bitfield2D",type_id,buf22);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_OPAQUE
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcreate(H5T_OPAQUE, 1);
 status = H5Tset_tag(type_id, "1-byte opaque type"); /* must set this */
 write_dset(loc_id,2,dims2,"opaque2D",type_id,buf22);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_COMPOUND
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
 H5Tinsert(type_id, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
 H5Tinsert(type_id, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
 write_dset(loc_id,2,dims2,"compound2D",type_id,buf32);
 status = H5Tclose(type_id);
 
/*-------------------------------------------------------------------------
 * H5T_REFERENCE
 *-------------------------------------------------------------------------
 */
 /* Create references to dataset */
 if (dset_name)
 {
  for (i = 0; i < 3; i++) {
   for (j = 0; j < 2; j++) {
    status=H5Rcreate(&buf42[i][j],file_id,dset_name,H5R_OBJECT,-1);
   }
  }
  write_dset(loc_id,2,dims2,"reference2D",H5T_STD_REF_OBJ,buf42);
 }

/*-------------------------------------------------------------------------
 * H5T_ENUM
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcreate(H5T_ENUM, sizeof(e_t));
 H5Tenum_insert(type_id, "RED",   (val = 0, &val));
 H5Tenum_insert(type_id, "GREEN", (val = 1, &val));
 write_dset(loc_id,2,dims2,"enum2D",type_id,0);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_VLEN
 *-------------------------------------------------------------------------
 */

 /* Allocate and initialize VL dataset to write */
 n=0;
 for (i = 0; i < 3; i++) {
  for (j = 0; j < 2; j++) {
    int l;
    buf52[i][j].p = malloc((i + 1) * sizeof(int));
    buf52[i][j].len = i + 1;
    for (l = 0; l < i + 1; l++)
    ((int *)buf52[i][j].p)[l] = n++;
  }
 }
 
 space_id = H5Screate_simple(2,dims2,NULL);
 type_id = H5Tvlen_create(H5T_NATIVE_INT);
 dset_id = H5Dcreate(loc_id,"vlen2D",type_id,space_id,H5P_DEFAULT);
 status = H5Dwrite(dset_id,type_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf52);
 assert(status>=0);
 status = H5Dvlen_reclaim(type_id,space_id,H5P_DEFAULT,buf52);
 assert(status>=0);
 status = H5Dclose(dset_id);
 status = H5Tclose(type_id);
 status = H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * H5T_ARRAY
 *-------------------------------------------------------------------------
 */
 type_id = H5Tarray_create(H5T_NATIVE_INT,1,dimarray,NULL);
 write_dset(loc_id,2,dims2,"array2D",type_id,buf62);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_INTEGER, write a fill value
 *-------------------------------------------------------------------------
 */
 plist_id = H5Pcreate(H5P_DATASET_CREATE);
 status = H5Pset_fill_value(plist_id, H5T_NATIVE_INT, &fillvalue);
 space_id = H5Screate_simple(2,dims2,NULL);
 dset_id = H5Dcreate(loc_id,"integer2D",H5T_NATIVE_INT,space_id,plist_id);
 status = H5Dwrite(dset_id,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf72);
 status = H5Pclose(plist_id);
 status = H5Dclose(dset_id);
 status = H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * H5T_FLOAT
 *-------------------------------------------------------------------------
 */

 write_dset(loc_id,2,dims2,"float2D",H5T_NATIVE_FLOAT,buf82);

 
/*-------------------------------------------------------------------------
 * 3D 
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * H5T_STRING
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcopy(H5T_C_S1);
 status  = H5Tset_size(type_id, 2);
 write_dset(loc_id,3,dims3,"string3D",type_id,buf13);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_BITFIELD
 *-------------------------------------------------------------------------
 */

 n=1;
 for (i = 0; i < 4; i++) {
  for (j = 0; j < 3; j++) {
   for (k = 0; k < 2; k++) {
    buf23[i][j][k]=n++;
   }
  }
 }
 type_id = H5Tcopy(H5T_STD_B8LE);
 write_dset(loc_id,3,dims3,"bitfield3D",type_id,buf23);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_OPAQUE
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcreate(H5T_OPAQUE, 1);
 status = H5Tset_tag(type_id, "1-byte opaque type"); /* must set this */
 write_dset(loc_id,3,dims3,"opaque3D",type_id,buf23);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_COMPOUND
 *-------------------------------------------------------------------------
 */

 n=1;
 for (i = 0; i < 4; i++) {
  for (j = 0; j < 3; j++) {
   for (k = 0; k < 2; k++) {
    buf33[i][j][k].a=n++;
    buf33[i][j][k].b=n++;
   }
  }
 }
 type_id = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
 H5Tinsert(type_id, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
 H5Tinsert(type_id, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
 write_dset(loc_id,3,dims3,"compound3D",type_id,buf33);
 status = H5Tclose(type_id);
 
/*-------------------------------------------------------------------------
 * H5T_REFERENCE
 *-------------------------------------------------------------------------
 */
 /* Create references to dataset */
 if (dset_name)
 {
  for (i = 0; i < 4; i++) {
   for (j = 0; j < 3; j++) {
    for (k = 0; k < 2; k++)
     status=H5Rcreate(&buf43[i][j][k],file_id,dset_name,H5R_OBJECT,-1);
   }
  }
 write_dset(loc_id,3,dims3,"reference3D",H5T_STD_REF_OBJ,buf43);
 }

/*-------------------------------------------------------------------------
 * H5T_ENUM
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcreate(H5T_ENUM, sizeof(e_t));
 H5Tenum_insert(type_id, "RED",   (val = 0, &val));
 H5Tenum_insert(type_id, "GREEN", (val = 1, &val));
 write_dset(loc_id,3,dims3,"enum3D",type_id,0);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_VLEN
 *-------------------------------------------------------------------------
 */
 
 /* Allocate and initialize VL dataset to write */
 n=0;
 for (i = 0; i < 4; i++) {
  for (j = 0; j < 3; j++) {
   for (k = 0; k < 2; k++) {
    int l;
    buf53[i][j][k].p = malloc((i + 1) * sizeof(int));
    buf53[i][j][k].len = i + 1;
    for (l = 0; l < i + 1; l++)
    ((int *)buf53[i][j][k].p)[l] = n++;
   }
  }
 }
 
 space_id = H5Screate_simple(3,dims3,NULL);
 type_id = H5Tvlen_create(H5T_NATIVE_INT);
 dset_id = H5Dcreate(loc_id,"vlen3D",type_id,space_id,H5P_DEFAULT);
 status = H5Dwrite(dset_id,type_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf53);
 assert(status>=0);
 status = H5Dvlen_reclaim(type_id,space_id,H5P_DEFAULT,buf53);
 assert(status>=0);
 status = H5Dclose(dset_id);
 status = H5Tclose(type_id);
 status = H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * H5T_ARRAY
 *-------------------------------------------------------------------------
 */
 n=1;
 for (i = 0; i < 24; i++) {
  for (j = 0; j < (int)dimarray[0]; j++) {
    buf63[i][j]=n++;
  }
 }

 type_id = H5Tarray_create(H5T_NATIVE_INT,1,dimarray,NULL);
 write_dset(loc_id,3,dims3,"array3D",type_id,buf63);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_INTEGER and H5T_FLOAT
 *-------------------------------------------------------------------------
 */
 n=1; f=1;
 for (i = 0; i < 4; i++) {
  for (j = 0; j < 3; j++) {
   for (k = 0; k < 2; k++) {
    buf73[i][j][k]=n++;
    buf83[i][j][k]=f++;
   }
  }
 }
 write_dset(loc_id,3,dims3,"integer3D",H5T_NATIVE_INT,buf73);
 write_dset(loc_id,3,dims3,"float3D",H5T_NATIVE_FLOAT,buf83);
}




/*-------------------------------------------------------------------------
 * Function: gent_attr_all
 *
 * Purpose: generate all datatype attributes  
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 19, 2003
 *
 *-------------------------------------------------------------------------
 */

static void gent_attr_all(void)
{
 hid_t   file_id; 
 hid_t   dset_id;
 hid_t   group_id;
 hid_t   group2_id;
 hid_t   root_id;
 hid_t   space_id;
 hsize_t dims[1]={2};
 herr_t  status;

 /* Create a file and a dataset */
 file_id  = H5Fcreate(FILE40, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT); 

 /* Create a 1D dataset */
 space_id = H5Screate_simple(1,dims,NULL);
 dset_id  = H5Dcreate(file_id,"dset",H5T_NATIVE_INT,space_id,H5P_DEFAULT);
 status   = H5Sclose(space_id);
 assert(status>=0);

 /* Create groups */
 group_id  = H5Gcreate(file_id,"g1",0);
 group2_id = H5Gcreate(file_id,"g2",0);
 root_id   = H5Gopen(file_id, "/");

/*-------------------------------------------------------------------------
 * write a series of attributes on the dataset, group
 *-------------------------------------------------------------------------
 */

 write_attr_in(dset_id,"dset",file_id);
 write_attr_in(group_id,NULL,0);
 write_attr_in(root_id,NULL,0);

/*-------------------------------------------------------------------------
 * write a series of datasets on group 2
 *-------------------------------------------------------------------------
 */

 write_dset_in(group2_id,"/dset",file_id);

 /* Close */
 status = H5Dclose(dset_id);
 assert(status>=0);
 status = H5Gclose(group_id);
 assert(status>=0);
 status = H5Gclose(group2_id);
 assert(status>=0);
 status = H5Gclose(root_id);
 assert(status>=0);

 /* Close file */
 status = H5Fclose(file_id);
 assert(status>=0);
}


/*-------------------------------------------------------------------------
 * Function: write_attr
 *
 * Purpose: utility function to write an attribute
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 19, 2003
 *
 *-------------------------------------------------------------------------
 */

static 
int write_attr(hid_t loc_id, int rank, hsize_t *dims, const char *attr_name,
               hid_t type_id, void *buf)
{
 hid_t   attr_id;
 hid_t   space_id;  
 herr_t  status;

 /* Create a buf space  */
 space_id = H5Screate_simple(rank,dims,NULL);

 /* Create the attribute */
 attr_id = H5Acreate(loc_id,attr_name,type_id,space_id,H5P_DEFAULT);
  
 /* Write the buf */
 if ( buf )
  status = H5Awrite(attr_id,type_id,buf);

 /* Close */
 status = H5Aclose(attr_id);
 status = H5Sclose(space_id);
 return status;
}

/*-------------------------------------------------------------------------
 * Function: write_dset
 *
 * Purpose: utility function to create and write a dataset in LOC_ID
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 27, 2003
 *
 *-------------------------------------------------------------------------
 */

static
int write_dset( hid_t loc_id, int rank, hsize_t *dims, const char *dset_name,
                hid_t type_id, void *buf )
{
 hid_t   dset_id;
 hid_t   space_id;  
 herr_t  status;

 /* Create a buf space  */
 space_id = H5Screate_simple(rank,dims,NULL);

 /* Create a dataset */
 dset_id = H5Dcreate(loc_id,dset_name,type_id,space_id,H5P_DEFAULT);
 assert (dset_id >= 0);
  
 /* Write the buf */
 if ( buf )
 {
  status = H5Dwrite(dset_id,type_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf);
  assert (status >= 0);
 }

 /* Close */
 status = H5Dclose(dset_id);
 assert (status >= 0);

 status = H5Sclose(space_id);
 assert (status >= 0);

 return 1;

}


static void gent_compound_complex(void)
{
    /* Structure and array for compound types                             */
    typedef struct Array1Struct {
        int                a;
 const char         *b[F41_DIMb];
 char               c[F41_ARRAY_DIMc];
 short              d[F41_ARRAY_DIMd1][F41_ARRAY_DIMd2];
 float              e;
 double             f[F41_ARRAY_DIMf];
 char               g;
    } Array1Struct;
    Array1Struct       Array1[F41_LENGTH];

    /* Define the value of the string array                           */
    const char *quote [F41_DIMb] = {
        "A fight is a contract that takes two people to honor.",
        "A combative stance means that you've accepted the contract.",
        "In which case, you deserve what you get.",
        "  --  Professor Cheng Man-ch'ing"
        };

    /* Define the value of the character array                        */
    char chararray [F41_ARRAY_DIMc] = {'H', 'e', 'l', 'l', 'o', '!'};


    hid_t      Array1Structid;            /* File datatype identifier */
    hid_t      array_tid;                 /* Array datatype handle    */
    hid_t      array1_tid;                /* Array datatype handle    */
    hid_t      array2_tid;                /* Array datatype handle    */
    hid_t      array4_tid;                /* Array datatype handle    */
    hid_t      datafile, dataset;         /* Datafile/dataset handles */
    hid_t      dataspace;                 /* Dataspace handle         */
    herr_t     status;                    /* Error checking variable */
    hsize_t    dim[] = {F41_LENGTH};          /* Dataspace dimensions     */
    hsize_t    array_dimb[] = {F41_DIMb};     /* Array dimensions         */
    hsize_t    array_dimd[]={F41_ARRAY_DIMd1,F41_ARRAY_DIMd2}; /* Array dimensions         */
    hsize_t    array_dimf[]={F41_ARRAY_DIMf}; /* Array dimensions         */
    hid_t      str_array_id;                

    int        m, n, o;                   /* Array init loop vars     */

    /* Initialize the data in the arrays/datastructure                */
    for (m = 0; m< F41_LENGTH; m++) {
        Array1[m].a = m;

        for (n = 0; n < F41_DIMb; n++) {
     Array1[m].b[n] = quote[n];
 }

 for (n = 0; n < F41_ARRAY_DIMc; n++) {
            Array1[m].c[n] = chararray[n];
 }

 for (n = 0; n < F41_ARRAY_DIMd1; n++) {
         for (o = 0; o < F41_ARRAY_DIMd2; o++){
             Array1[m].d[n][o] = m + n + o;
            }
        }

        Array1[m].e = (float)( m * .96 );

 for (n = 0; n < F41_ARRAY_DIMf; n++) {
            Array1[m].f[n] = ( m * 1024.9637 );
 }

        Array1[m].g = 'm';
    }

    /* Create the dataspace                                           */
    dataspace = H5Screate_simple(F41_RANK, dim, NULL);
    assert (dataspace >= 0);

    /* Create the file                                                */
    datafile = H5Fcreate(FILE41, H5F_ACC_TRUNC, H5P_DEFAULT,
      H5P_DEFAULT);
    assert (datafile >= 0);

    /* Copy the array data type for the string array                  */
    array_tid = H5Tcopy (H5T_C_S1);
    assert (array_tid >= 0);

    /* Set the string array size to Variable                          */
    status = H5Tset_size (array_tid,H5T_VARIABLE);
    assert (status >= 0);

    /* Create the array data type for the string array                */
    str_array_id = H5Tarray_create(array_tid, F41_ARRAY_RANK,
      array_dimb, NULL);
    assert (str_array_id >= 0);

    /* Copy the array data type for the character array               */
    array1_tid = H5Tcopy (H5T_C_S1);
    assert (array1_tid >= 0);

    /* Set the character array size                                   */
    status = H5Tset_size (array1_tid, F41_ARRAY_DIMc);
    assert (status >= 0);

    /* Create the array data type for the character array             */
    array2_tid = H5Tarray_create(H5T_NATIVE_SHORT, F41_ARRAY_RANKd,
      array_dimd, NULL);
    assert (array2_tid >= 0);

    /* Create the array data type for the character array             */
    array4_tid = H5Tarray_create(H5T_NATIVE_DOUBLE, F41_ARRAY_RANK,
      array_dimf, NULL);
    assert (array4_tid >= 0);

    /* Create the memory data type                                    */
    Array1Structid = H5Tcreate (H5T_COMPOUND, sizeof(Array1Struct));
    assert (Array1Structid >= 0);

    /* Insert the arrays and variables into the structure             */
    status = H5Tinsert(Array1Structid, "a_name",
      HOFFSET(Array1Struct, a), H5T_NATIVE_INT);
    assert (status >= 0);

    status = H5Tinsert(Array1Structid, "b_name",
      HOFFSET(Array1Struct, b), str_array_id);
    assert (status >= 0);

    status = H5Tinsert(Array1Structid, "c_name",
      HOFFSET(Array1Struct, c), array1_tid);
    assert (status >= 0);

    status = H5Tinsert(Array1Structid, "d_name",
      HOFFSET(Array1Struct, d), array2_tid);
    assert (status >= 0);

    status = H5Tinsert(Array1Structid, "e_name",
      HOFFSET(Array1Struct, e), H5T_NATIVE_FLOAT);
    assert (status >= 0);

    status = H5Tinsert(Array1Structid, "f_name",
      HOFFSET(Array1Struct, f), array4_tid);
    assert (status >= 0);

    status = H5Tinsert(Array1Structid, "g_name",
      HOFFSET(Array1Struct, g), H5T_NATIVE_CHAR);
    assert (status >= 0);

    /* Create the dataset                                             */
    dataset = H5Dcreate(datafile, F41_DATASETNAME, Array1Structid,
      dataspace, H5P_DEFAULT);

    /* Write data to the dataset                                      */
    status = H5Dwrite(dataset, Array1Structid, H5S_ALL, H5S_ALL,
      H5P_DEFAULT, Array1);
    assert (status >= 0);

    /* Release resources                                              */
    status = H5Tclose(Array1Structid);
    assert (status >= 0);

    status = H5Tclose(array_tid);
    assert (status >= 0);

    status = H5Tclose(array1_tid);
    assert (status >= 0);

    status = H5Tclose(array2_tid);
    assert (status >= 0);

    status = H5Tclose(array4_tid);
    assert (status >= 0);

    status = H5Tclose(str_array_id);
    assert (status >= 0);

    status = H5Sclose(dataspace);
    assert (status >= 0);

    status = H5Dclose(dataset);
    assert (status >= 0);

    status = H5Fclose(datafile);
    assert (status >= 0);
}


static void gent_named_dtype_attr(void)
{
   hid_t file_id;
   hid_t dset_id;
   hid_t space_id;
   hid_t type_id;
   hid_t attr_id;
   int data=8;
   herr_t ret;

   /* Create a file */
   file_id=H5Fcreate(FILE42, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
   assert(file_id>0);

   /* Create a datatype to commit and use */
   type_id=H5Tcopy(H5T_NATIVE_INT);
   assert(type_id>0);

   /* Commit datatype to file */
   ret=H5Tcommit(file_id,F42_TYPENAME,type_id);
   assert(ret>=0);

   /* Create dataspace for dataset */
   space_id=H5Screate(H5S_SCALAR);
   assert(space_id>0);

   /* Create dataset */
   dset_id=H5Dcreate(file_id,F42_DSETNAME,type_id,space_id,H5P_DEFAULT);
   assert(dset_id>0);

   /* Create attribute on dataset */
   attr_id=H5Acreate(dset_id,F42_ATTRNAME,type_id,space_id,H5P_DEFAULT);
   assert(dset_id>0);

   /* Write data into the attribute */
   ret=H5Awrite(attr_id,H5T_NATIVE_INT,&data);
   assert(ret>=0);

   /* Close attribute */
   ret=H5Aclose(attr_id);
   assert(ret>=0);

   /* Close dataset */
   ret=H5Dclose(dset_id);
   assert(ret>=0);

   /* Close dataspace */
   ret=H5Sclose(space_id);
   assert(ret>=0);

   /* Close datatype */
   ret=H5Tclose(type_id);
   assert(ret>=0);

   /* Close file */
   ret=H5Fclose(file_id);
   assert(ret>=0);
}


/*-------------------------------------------------------------------------
 * Function: make_dset
 *
 * Purpose: utility function to create and write a dataset in LOC_ID
 *
 *-------------------------------------------------------------------------
 */
static 
int make_dset(hid_t loc_id,
              const char *name,
              hid_t sid, 
              hid_t dcpl,
              void *buf)
{
 hid_t   dsid;

 /* create the dataset */
 if((dsid = H5Dcreate (loc_id,name,H5T_NATIVE_INT,sid,dcpl))<0)
  return -1;

 /* write */
 if(H5Dwrite(dsid,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf)<0)
  goto out;

 /* close */
 if(H5Dclose(dsid)<0)
  return -1;

 return 0;
 out:
 H5E_BEGIN_TRY {
  H5Dclose(dsid);
 } H5E_END_TRY;
 return -1;
}



/*-------------------------------------------------------------------------
 * Function: make_external
 *
 * Purpose: make a dataset with external storage
 *
 *-------------------------------------------------------------------------
 */
static void
make_external(hid_t fid)
{
 hid_t   dcpl;         /*dataset creation property list */
 hid_t   sid;          /*dataspace ID */
 hid_t   dsid;         /*dataset ID   */
 hsize_t cur_size[1];  /*data space current size */
 hsize_t max_size[1];  /*data space maximum size */
 hsize_t size;         /*bytes reserved for data in the external file*/
 int     ret;
 
 cur_size[0] = max_size[0] = 100;
 size = (max_size[0]*sizeof(int)/2);

 dcpl=H5Pcreate(H5P_DATASET_CREATE);
 ret=H5Pset_external(dcpl,"ext1.bin",(off_t)0,size);
 assert(ret>=0);

 ret=H5Pset_external(dcpl,"ext2.bin",(off_t)0,size);
 assert(ret>=0);

 sid=H5Screate_simple(1, cur_size, max_size);
 assert(ret>=0);

 dsid=H5Dcreate(fid, "external", H5T_NATIVE_INT, sid, dcpl);
 assert(ret>=0);

 H5Dclose(dsid);
 assert(ret>=0);

 H5Sclose(sid);
 assert(ret>=0);

 H5Pclose(dcpl);
 assert(ret>=0);
}
/*-------------------------------------------------------------------------
 * Function: gent_filters
 *
 * Purpose: make several datasets with filters, external dataset
 *  fill value
 *
 *-------------------------------------------------------------------------
 */
static void gent_filters(void)
{
 hid_t    fid;  /* file id */
 hid_t    dcpl; /* dataset creation property list */
 hid_t    sid;  /* dataspace ID */
 hid_t    sid1; /* dataspace ID */
 hid_t    tid;  /* datatype ID */
 hid_t    did;  /* dataset ID */
 hid_t    gid;  /* group ID */
#if defined (H5_HAVE_FILTER_SZIP)
 unsigned szip_options_mask=H5_SZIP_ALLOW_K13_OPTION_MASK|H5_SZIP_NN_OPTION_MASK;
 unsigned szip_pixels_per_block=4;
#endif
 hsize_t  dims1[RANK]={DIM1,DIM2};
 hsize_t  chunk_dims[RANK]={CDIM1,CDIM2};
 int      buf1[DIM1][DIM2];
 hsize_t  dims2[1]={2};
 hvl_t    buf2[2];
 hsize_t  dims3[1]={1};
 char     buf3[]={"quote \"  backspace\b form feed\f new line\n tab\t new line\n carriage return\r"};
 hsize_t  dims4[1]={6};
 char     buf4[6]={"abcdef"};
 hobj_ref_t buf5[1]; 
 hsize_t  dims5[1]={1};
 int      i, j, n, ret, val;
 int      fillval = -99;

 typedef enum 
 {
  E_RED,
  E_GREEN
 } e_t;

 for (i=n=0; i<DIM1; i++){
  for (j=0; j<DIM2; j++){
   buf1[i][j]=n++;
  }
 }
 
 /* create a file */
 fid  = H5Fcreate(FILE44, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
 assert(fid>=0);
 
 /* create a space */
 sid = H5Screate_simple(SPACE2_RANK, dims1, NULL);

 /* create a dataset creation property list; the same DCPL is used for all dsets */
 dcpl = H5Pcreate(H5P_DATASET_CREATE);

 ret=H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval);
 assert(ret>=0);


/*-------------------------------------------------------------------------
 * create a compact and contiguous storage layout dataset
 * add a comment to the datasets
 *-------------------------------------------------------------------------
 */
 ret=H5Pset_layout(dcpl, H5D_COMPACT);
 assert(ret>=0);

 ret=make_dset(fid,"compact",sid,dcpl,buf1);
 assert(ret>=0);

 ret=H5Gset_comment(fid,"compact", "This is a dataset with compact storage");
 assert(ret>=0);

 ret=H5Pset_layout(dcpl, H5D_CONTIGUOUS);
 assert(ret>=0);

 ret=make_dset(fid,"contiguous",sid,dcpl,buf1);
 assert(ret>=0);

 ret=H5Gset_comment(fid,"contiguous", "This is a dataset with contiguous storage");
 assert(ret>=0);

 ret=H5Pset_layout(dcpl, H5D_CHUNKED);
 assert(ret>=0);

 ret=H5Pset_chunk(dcpl, SPACE2_RANK, chunk_dims);
 assert(ret>=0);

 ret=make_dset(fid,"chunked",sid,dcpl,buf1);
 assert(ret>=0);

 ret=H5Gset_comment(fid,"chunked", "This is a dataset with chunked storage");
 assert(ret>=0);

/*-------------------------------------------------------------------------
 * make several dataset with filters
 *-------------------------------------------------------------------------
 */

 /* set up chunk */
 ret=H5Pset_chunk(dcpl, SPACE2_RANK, chunk_dims);
 assert(ret>=0);

/*-------------------------------------------------------------------------
 * SZIP
 *-------------------------------------------------------------------------
 */
#if defined (H5_HAVE_FILTER_SZIP) && defined (H5_SZIP_CAN_ENCODE)
 /* remove the filters from the dcpl */
 ret=H5Premove_filter(dcpl,H5Z_FILTER_ALL);
 assert(ret>=0);

 /* set szip data */
 ret=H5Pset_szip (dcpl,szip_options_mask,szip_pixels_per_block);
 assert(ret>=0);

 ret=make_dset(fid,"szip",sid,dcpl,buf1);
 assert(ret>=0);
#endif

/*-------------------------------------------------------------------------
 * GZIP
 *-------------------------------------------------------------------------
 */
#if defined (H5_HAVE_FILTER_DEFLATE)
 /* remove the filters from the dcpl */
 ret=H5Premove_filter(dcpl,H5Z_FILTER_ALL);
 assert(ret>=0);

 /* set deflate data */
 ret=H5Pset_deflate(dcpl, 9);
 assert(ret>=0);

 ret=make_dset(fid,"deflate",sid,dcpl,buf1);
 assert(ret>=0);
#endif


/*-------------------------------------------------------------------------
 * shuffle
 *-------------------------------------------------------------------------
 */
#if defined (H5_HAVE_FILTER_SHUFFLE)
 /* remove the filters from the dcpl */
 ret=H5Premove_filter(dcpl,H5Z_FILTER_ALL);
 assert(ret>=0);

 /* set the shuffle filter */
 ret=H5Pset_shuffle(dcpl);
 assert(ret>=0);

 ret=make_dset(fid,"shuffle",sid,dcpl,buf1);
 assert(ret>=0);
#endif


/*-------------------------------------------------------------------------
 * checksum
 *-------------------------------------------------------------------------
 */
#if defined (H5_HAVE_FILTER_FLETCHER32)
 /* remove the filters from the dcpl */
 ret=H5Premove_filter(dcpl,H5Z_FILTER_ALL);
 assert(ret>=0);

 /* set the checksum filter */
 ret=H5Pset_fletcher32(dcpl);
 assert(ret>=0);

 ret=make_dset(fid,"fletcher32",sid,dcpl,buf1);
 assert(ret>=0);
#endif

/*-------------------------------------------------------------------------
 * all filters
 *-------------------------------------------------------------------------
 */
 /* remove the filters from the dcpl */
 ret=H5Premove_filter(dcpl,H5Z_FILTER_ALL);
 assert(ret>=0);

#if defined (H5_HAVE_FILTER_SHUFFLE)
 /* set the shuffle filter */
 ret=H5Pset_shuffle(dcpl);
 assert(ret>=0);
#endif

#if defined (H5_HAVE_FILTER_SZIP) && defined (H5_SZIP_CAN_ENCODE)
 szip_options_mask=H5_SZIP_CHIP_OPTION_MASK | H5_SZIP_EC_OPTION_MASK;
 /* set szip data */
 ret=H5Pset_szip (dcpl,szip_options_mask,szip_pixels_per_block);
 assert(ret>=0);
#endif

#if defined (H5_HAVE_FILTER_DEFLATE)
 /* set deflate data */
 ret=H5Pset_deflate(dcpl, 5);
 assert(ret>=0);
#endif

#if defined (H5_HAVE_FILTER_FLETCHER32)
 /* set the checksum filter */
 ret=H5Pset_fletcher32(dcpl);
 assert(ret>=0);
#endif

 ret=make_dset(fid,"all",sid,dcpl,buf1);
 assert(ret>=0);

 
/*-------------------------------------------------------------------------
 * user defined filter
 *-------------------------------------------------------------------------
 */
 /* remove the filters from the dcpl */
 ret=H5Premove_filter(dcpl,H5Z_FILTER_ALL);
 assert(ret>=0);

#ifdef H5_WANT_H5_V1_4_COMPAT
 ret=H5Zregister (MYFILTER_ID, "myfilter", myfilter);
#else 
 ret=H5Zregister (H5Z_MYFILTER);
#endif 
 assert(ret>=0);

 ret=H5Pset_filter (dcpl, MYFILTER_ID, 0, 0, NULL);
 assert(ret>=0);

 ret=make_dset(fid,"myfilter",sid,dcpl,buf1);
 assert(ret>=0);

 /* remove the filters from the dcpl */
 ret=H5Premove_filter(dcpl,H5Z_FILTER_ALL);
 assert(ret>=0);



/*-------------------------------------------------------------------------
 * make an external dataset
 *-------------------------------------------------------------------------
 */
 make_external(fid);

/*-------------------------------------------------------------------------
 * make datasets with fill value combinations
 * H5D_FILL_TIME_IFSET
 *-------------------------------------------------------------------------
 */
 ret=H5Pset_fill_time(dcpl, H5D_FILL_TIME_IFSET);
 assert(ret>=0);

 ret=H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval);
 assert(ret>=0);

 ret=make_dset(fid,"fill_time_ifset",sid,dcpl,buf1);
 assert(ret>=0);

/*-------------------------------------------------------------------------
 * H5D_FILL_TIME_ALLOC
 *-------------------------------------------------------------------------
 */
 ret=H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC);
 assert(ret>=0);

 ret=H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval);
 assert(ret>=0);
 
 ret=make_dset(fid,"fill_time_alloc",sid,dcpl,buf1);
 assert(ret>=0);

/*-------------------------------------------------------------------------
 * H5D_FILL_TIME_NEVER
 *-------------------------------------------------------------------------
 */
 ret=H5Pset_fill_time(dcpl, H5D_FILL_TIME_NEVER);
 assert(ret>=0);

 ret=H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval);
 assert(ret>=0);

 ret=make_dset(fid,"fill_time_never",sid,dcpl,buf1);
 assert(ret>=0);

/*-------------------------------------------------------------------------
 * H5D_ALLOC_TIME_EARLY 
 *-------------------------------------------------------------------------
 */

 ret=H5Pset_fill_time(dcpl, H5D_FILL_TIME_IFSET);
 assert(ret>=0);

 ret=H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY);
 assert(ret>=0);

 ret=H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval);
 assert(ret>=0);

 ret=make_dset(fid,"alloc_time_early",sid,dcpl,buf1);
 assert(ret>=0);

/*-------------------------------------------------------------------------
 * H5D_ALLOC_TIME_INCR    
 *-------------------------------------------------------------------------
 */
 ret=H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_INCR);
 assert(ret>=0);

 ret=H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval);
 assert(ret>=0);

 ret=make_dset(fid,"alloc_time_incr",sid,dcpl,buf1);
 assert(ret>=0);

/*-------------------------------------------------------------------------
 * H5D_ALLOC_TIME_LATE     
 *-------------------------------------------------------------------------
 */
 ret=H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_LATE);
 assert(ret>=0);

 ret=H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval);
 assert(ret>=0);

 ret=make_dset(fid,"alloc_time_late",sid,dcpl,buf1);
 assert(ret>=0);

/*-------------------------------------------------------------------------
 * commit a H5G_TYPE type with a comment
 *-------------------------------------------------------------------------
 */
 tid=H5Tcopy(H5T_STD_B8LE);
 ret=H5Tcommit(fid, "mytype", tid);
 assert(ret>=0);

 ret=H5Gset_comment(fid,"mytype", "This is a commited datatype");
 assert(ret>=0);

 ret=H5Tclose(tid);
 assert(ret>=0);

/*-------------------------------------------------------------------------
 * enum type with nonprintable characters in the name
 *-------------------------------------------------------------------------
 */
 tid = H5Tcreate(H5T_ENUM, sizeof(e_t));
 H5Tenum_insert(tid, "RED 3 \\n",   (val = 0, &val));
 write_dset(fid,2,dims1,"enum",tid,0);
 ret=H5Tclose(tid);
 assert(ret>=0);

/*-------------------------------------------------------------------------
 * vlen 
 *-------------------------------------------------------------------------
 */

 buf2[0].len = 1;
 buf2[0].p = malloc( 1 * sizeof(int));
 ((int *)buf2[0].p)[0]=1;
 buf2[1].len = 2;
 buf2[1].p = malloc( 2 * sizeof(int));
 ((int *)buf2[1].p)[0]=2;
 ((int *)buf2[1].p)[1]=3;

 sid1=H5Screate_simple(1,dims2,NULL);
 tid=H5Tvlen_create(H5T_NATIVE_INT);
 did=H5Dcreate(fid,"vlen",tid,sid1,H5P_DEFAULT);
 ret=H5Dwrite(did,tid,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf2);
 assert(ret>=0);
 ret=H5Tcommit(fid,"myvlen",tid);
 assert(ret>=0);
 ret=H5Dvlen_reclaim(tid,sid1,H5P_DEFAULT,buf2);
 assert(ret>=0);
 ret=H5Dclose(did);
 assert(ret>=0);
 ret=H5Tclose(tid);
 assert(ret>=0);

/*-------------------------------------------------------------------------
 * bitfield
 *-------------------------------------------------------------------------
 */
 tid = H5Tcopy(H5T_STD_B8LE);
 write_dset(fid,1,dims3,"bitfield",tid,buf3);
 ret=H5Tclose(tid);
 assert(ret>=0);

/*-------------------------------------------------------------------------
 * string with escaped characters
 *-------------------------------------------------------------------------
 */

 tid=H5Tcopy(H5T_C_S1);
 ret=H5Tset_size(tid, sizeof(buf3));
 assert(ret>=0);
 write_dset(fid,1,dims3,"string",tid,buf3);
 assert(ret>=0);
 ret=H5Tclose(tid);
 assert(ret>=0);

/*-------------------------------------------------------------------------
 * char array
 *-------------------------------------------------------------------------
 */
 write_dset(fid,1,dims4,"char",H5T_NATIVE_CHAR,buf4);

/*-------------------------------------------------------------------------
 * links
 *-------------------------------------------------------------------------
 */
 ret=H5Glink (fid, H5G_LINK_SOFT, "all", "slink to all");
 assert(ret>=0);

 ret=H5Glink (fid, H5G_LINK_HARD, "all", "hlink to all");
 assert(ret>=0);

/*-------------------------------------------------------------------------
 * a group 
 *-------------------------------------------------------------------------
 */
 gid  = H5Gcreate(fid,"g1",0);
 write_dset(gid,1,dims4,"mydset",H5T_NATIVE_CHAR,buf4);
 ret  = H5Gclose(gid);
 assert(ret>=0);

/*-------------------------------------------------------------------------
 * reference
 *-------------------------------------------------------------------------
 */
 ret=H5Rcreate(&buf5[0],fid,"g1/mydset",H5R_OBJECT,-1);
 assert(ret>=0);
 write_dset(fid,1,dims5,"reference",H5T_STD_REF_OBJ,buf5);
 
/*-------------------------------------------------------------------------
 * close
 *-------------------------------------------------------------------------
 */
 ret=H5Sclose(sid1);
 assert(ret>=0);

 ret=H5Sclose(sid);
 assert(ret>=0);
 
 ret=H5Pclose(dcpl);
 assert(ret>=0);

 ret=H5Fclose(fid);
 assert(ret>=0);
}

/*-------------------------------------------------------------------------
 * Function: myfilter
 *
 * Purpose: filter operation callback function; the filter does nothing
 *
 *-------------------------------------------------------------------------
 */
static size_t
myfilter(unsigned int UNUSED flags, size_t UNUSED cd_nelmts, 
      const unsigned int UNUSED *cd_values, size_t nbytes,
      size_t UNUSED *buf_size, void UNUSED **buf)
{
 return nbytes;
}


/*-------------------------------------------------------------------------
 * Function: set_local_myfilter
 *
 * Purpose: filter operation "set local" callback
 *
 *-------------------------------------------------------------------------
 */

static herr_t
set_local_myfilter(hid_t dcpl_id, hid_t UNUSED type_id, hid_t UNUSED space_id)
{
 unsigned flags;                /* Filter flags */
 size_t   cd_nelmts=0;          /* Number of filter parameters */
 unsigned cd_values[2]={5,6};   /* Filter parameters */
 
 /* Get the filter's current parameters */
 if(H5Pget_filter_by_id(dcpl_id,MYFILTER_ID,&flags,&cd_nelmts,cd_values,0,NULL)<0)
  return(FAIL);

 cd_nelmts=2; 
 
 /* Modify the filter's parameters for this dataset */
 if(H5Pmodify_filter(dcpl_id,MYFILTER_ID,flags, cd_nelmts,cd_values)<0)
  return(FAIL);
 
 return(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function: gent_fcontents
 *
 * Purpose: generate several files to list its contents
 *
 *-------------------------------------------------------------------------
 */
static void gent_fcontents(void)
{
 hid_t    fid;   /* file id */
 hid_t    gid1;  /* group ID */
 hid_t    gid2;  /* group ID */
 hid_t    gid3;  /* group ID */
 hid_t    tid;   /* datatype ID */
 hsize_t  dims[1]={4};
 int      buf[4]={1,2,3,4};
 int      ret;
 
 /* create a file */
 fid  = H5Fcreate(FILE46, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
 assert(fid>=0);

 write_dset(fid,1,dims,"dset",H5T_NATIVE_INT,buf);

/*-------------------------------------------------------------------------
 * links
 *-------------------------------------------------------------------------
 */

 /* soft link to "dset" */
 ret=H5Glink (fid, H5G_LINK_SOFT, "dset", "slink to dset");
 assert(ret>=0);

 /* hard link to "dset" */
 ret=H5Glink (fid, H5G_LINK_HARD, "dset", "hlink to dset");
 assert(ret>=0);

 /* soft link to itself */
 ret=H5Glink (fid, H5G_LINK_SOFT, "link", "link");
 assert(ret>=0);

/*-------------------------------------------------------------------------
 * groups
 *-------------------------------------------------------------------------
 */
 gid1  = H5Gcreate(fid,"g1",0);
 gid2  = H5Gcreate(gid1,"g2",0);
 gid3  = H5Gcreate(gid2,"g3",0);
 write_dset(gid3,1,dims,"dset",H5T_NATIVE_INT,buf);
 ret  = H5Gclose(gid1);
 assert(ret>=0);
 ret  = H5Gclose(gid2);
 assert(ret>=0);
 ret  = H5Gclose(gid3);
 assert(ret>=0);

/*-------------------------------------------------------------------------
 * datatype 
 *-------------------------------------------------------------------------
 */
 tid=H5Tcopy(H5T_STD_B8LE);
 ret=H5Tcommit(fid, "mytype", tid);
 assert(ret>=0);
 ret=H5Tclose(tid);
 assert(ret>=0);

/*-------------------------------------------------------------------------
 * close
 *-------------------------------------------------------------------------
 */

 ret=H5Fclose(fid);
 assert(ret>=0);

 /* create a file for the bootblock test */
 fid  = H5Fcreate(FILE47, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
 assert(fid>=0);

 ret=H5Fclose(fid);
 assert(ret>=0);
}


/*-------------------------------------------------------------------------
 * Function: gent_fdrivers
 *
 * Purpose: file drivers test
 *
 *-------------------------------------------------------------------------
 */
static void gent_fdrivers(void)
{
 hid_t    fid;   /* file id */
 hid_t    fapl;  /* file access property list */
 int      ret;

 fapl = H5Pcreate(H5P_FILE_ACCESS);
 assert(fapl>=0);

 ret=H5Pset_fapl_stdio(fapl);
 assert(ret>=0);

 /* create a file */
 fid  = H5Fcreate(FILE48, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
 assert(fid>=0);

 ret=H5Fclose(fid);
 assert(ret>=0);


/*-------------------------------------------------------------------------
 * close
 *-------------------------------------------------------------------------
 */
 ret=H5Pclose(fapl);
 assert(ret>=0);


}

/*-------------------------------------------------------------------------
 * Function: main
 *
 *-------------------------------------------------------------------------
 */


int main(void)
{
    gent_group();
    gent_attribute();
    gent_softlink();
    gent_dataset();
    gent_hardlink();
    gent_compound_dt();
    gent_all();
    gent_loop();

    gent_dataset2();
    gent_compound_dt2();
    gent_loop2();
    gent_many();

    gent_str();
    gent_str2();

    gent_enum();

    gent_objref();
    gent_datareg();

    gent_nestcomp();

    gent_opaque();

    gent_bitfields();

    gent_vldatatypes();
    gent_vldatatypes2();
    gent_vldatatypes3();
    gent_vldatatypes4();
    gent_vldatatypes5();

    gent_array1();
    gent_array2();
    gent_array3();
    gent_array4();
    gent_array5();
    gent_array6();
    gent_array7();

    gent_empty();
    gent_group_comments();
    gent_split_file();
    gent_family();
    gent_multi();

    gent_large_objname();
    gent_vlstr();
    gent_char();

    gent_attr_all();

    gent_compound_complex();
    gent_named_dtype_attr();

    gent_filters();
    gent_fcontents();
    gent_fdrivers();

    return 0;
}
