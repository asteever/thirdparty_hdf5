#############################
Expected output for 'h5dump -H -p -d chunked tfilters.h5'
#############################
HDF5 "tfilters.h5" {
DATASET "chunked" {
COMMENT "This is a dataset with chunked storage"
   DATATYPE  H5T_STD_I32LE
   DATASPACE  SIMPLE { ( 20, 10 ) / ( 20, 10 ) }
   STORAGE_LAYOUT {
      CHUNKED ( 10, 5 )
      SIZE 800
    }
   FILLVALUE {
      FILL_TIME IFSET
      ALLOC_TIME INCR
      VALUE  -99   
   }
}
}
