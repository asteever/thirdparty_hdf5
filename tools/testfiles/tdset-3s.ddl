#############################
Expected output for 'h5dump -d /dset1[1,1;;;] tdset.h5'
#############################
HDF5 "tdset.h5" {
DATASET "/dset1" {
   DATATYPE  H5T_STD_I32BE
   DATASPACE  SIMPLE { ( 10, 20 ) / ( 10, 20 ) }
   SUBSET {
      START ( 1, 1 );
      STRIDE ( 1, 1 );
      COUNT ( 9, 19 );
      BLOCK ( 1, 1 );
      DATA {
        (0,0), 2, 3, 4, 5, 6, 7, 8, 9, 10, 3, 4, 5, 6, 7, 8, 9, 10, 11, 4, 5,
        (0,2), 6, 7, 8, 9, 10, 11, 12, 5, 6, 7, 8, 9, 10, 11, 12, 13, 6, 7, 8,
        (0,3), 9, 10, 11, 12, 13, 14, 7, 8, 9, 10, 11, 12, 13, 14, 15, 8, 9, 10,
        (0,3), 11, 12, 13, 14, 15, 16, 9, 10, 11, 12, 13, 14, 15, 16, 17, 10,
        (0,1), 11, 12, 13, 14, 15, 16, 17, 18, 11, 12, 13, 14, 15, 16, 17, 18,
        (0,8), 19, 12, 13, 14, 15, 16, 17, 18, 19, 20, 13, 14, 15, 16, 17, 18,
        (0,6), 19, 20, 21, 14, 15, 16, 17, 18, 19, 20, 21, 22, 15, 16, 17, 18,
        (0,4), 19, 20, 21, 22, 23, 16, 17, 18, 19, 20, 21, 22, 23, 24, 17, 18,
        (0,2), 19, 20, 21, 22, 23, 24, 25, 18, 19, 20, 21, 22, 23, 24, 25, 26,
        (0,0), 19, 20, 21, 22, 23, 24, 25, 26, 27, 20, 21, 22, 23, 24, 25, 26,
        (0,7), 27, 28
      }
   }
}
}
