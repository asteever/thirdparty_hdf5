#############################
Expected output for 'h5dump -H -p -d fill_never tfilters.h5'
#############################
HDF5 "tfilters.h5" {
DATASET "fill_never" {
   DATATYPE  H5T_STD_I32LE
   DATASPACE  SIMPLE { ( 20, 10 ) / ( 20, 10 ) }
   STORAGE_LAYOUT CHUNKED {
      SIZE 800 ( 10, 5 )
   }
   FILLVALUE {
      FILL_TIME NEVER
      ALLOC_TIME EARLY
      VALUE  -99   
   }
}
}
