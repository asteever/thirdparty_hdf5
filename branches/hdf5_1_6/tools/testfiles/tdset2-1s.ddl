#############################
Expected output for 'h5dump -d /dset1[;3,2;4,4;1,4] tdset2.h5'
#############################
HDF5 "tdset2.h5" {
DATASET "/dset1" {
   DATATYPE  H5T_STD_I32BE
   DATASPACE  SIMPLE { ( 10, 20 ) / ( H5S_UNLIMITED, 20 ) }
   SUBSET {
      START ( 0, 0 );
      STRIDE ( 3, 2 );
      COUNT ( 4, 4 );
      BLOCK ( 1, 4 );
      DATA {
         0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 2, 3, 4, 5,
         2, 3, 4, 5, 2, 3, 4, 5, 2, 3, 4, 5, 4, 5, 6, 7, 4, 5, 6, 7,
         4, 5, 6, 7, 4, 5, 6, 7, 6, 7, 8, 9, 6, 7, 8, 9, 6, 7, 8, 9,
         6, 7, 8, 9
      }
   }
}
}
