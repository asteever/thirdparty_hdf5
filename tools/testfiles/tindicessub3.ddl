#############################
Expected output for 'h5dump -d 3d -s 0,1,2 -S 1,3,3 -c 2,2,2 -k 1,2,2 taindices.h5'
#############################
HDF5 "taindices.h5" {
DATASET "3d" {
   DATATYPE  H5T_STD_I32LE
   DATASPACE  SIMPLE { ( 2, 10, 10 ) / ( 2, 10, 10 ) }
   SUBSET {
      START ( 0, 1, 2 );
      STRIDE ( 1, 3, 3 );
      COUNT ( 2, 2, 2 );
      BLOCK ( 1, 2, 2 );
      DATA {
      (0,1,2): 12, 13, 15, 16,
      (0,2,2): 22, 23, 25, 26,
      (0,4,2): 42, 43, 45, 46,
      (0,5,2): 52, 53, 55, 56
      (1,1,2): 112, 113, 115, 116,
      (1,2,2): 122, 123, 125, 126,
      (1,4,2): 142, 143, 145, 146,
      (1,5,2): 152, 153, 155, 156
      }
   }
}
}
