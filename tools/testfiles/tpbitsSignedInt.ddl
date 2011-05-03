#############################
Expected output for 'h5dump -d /DS16BITS -M 0,2,10,6 packedbits.h5'
#############################
HDF5 "packedbits.h5" {
DATASET "/DS16BITS" {
   DATATYPE  H5T_STD_I16LE
   DATASPACE  SIMPLE { ( 8, 16 ) / ( 8, 16 ) }
   PACKED_BITS OFFSET=0 LENGTH=2
   DATA {
   (0,0): 3, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   (1,0): 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   (2,0): 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   (3,0): 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   (4,0): 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   (5,0): 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   (6,0): 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   (7,0): 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
   }
   PACKED_BITS OFFSET=10 LENGTH=6
   DATA {
   (0,0): 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 62, 60, 56, 48, 32,
   (1,0): 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 62, 60, 56, 48, 32, 0,
   (2,0): 63, 63, 63, 63, 63, 63, 63, 63, 63, 62, 60, 56, 48, 32, 0, 0,
   (3,0): 63, 63, 63, 63, 63, 63, 63, 63, 62, 60, 56, 48, 32, 0, 0, 0,
   (4,0): 63, 63, 63, 63, 63, 63, 63, 62, 60, 56, 48, 32, 0, 0, 0, 0,
   (5,0): 63, 63, 63, 63, 63, 63, 62, 60, 56, 48, 32, 0, 0, 0, 0, 0,
   (6,0): 63, 63, 63, 63, 63, 62, 60, 56, 48, 32, 0, 0, 0, 0, 0, 0,
   (7,0): 63, 63, 63, 63, 62, 60, 56, 48, 32, 0, 0, 0, 0, 0, 0, 0
   }
}
}
