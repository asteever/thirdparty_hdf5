#############################
Expected output for 'h5dump -a attr1 attr4 attr5 tattr.h5'
#############################
HDF5 "testfiles\tattr.h5" {
ATTRIBUTE "attr1" {
   DATATYPE { H5T_STD_I8BE }
   DATASPACE { SIMPLE ( 24 ) / ( 24 ) }
   DATA {
      97, 116, 116, 114, 105, 98, 117, 116, 101, 32, 111, 102, 32, 114, 111,
      111, 116, 32, 103, 114, 111, 117, 112, 0
   }
}
ATTRIBUTE "attr4" {
   DATATYPE { H5T_STD_I32BE }
   DATASPACE { SCALAR }
   DATA {
      100
   }
}
ATTRIBUTE "attr5" {
   DATATYPE {
      { STRSIZE 17;
        STRPAD H5T_STR_NULLTERM;
        CSET H5T_CSET_ASCII;
        CTYPE H5T_C_S1;
      }
   }
   DATASPACE { SCALAR }
   DATA {
      "string attribute"
   }
}
}
