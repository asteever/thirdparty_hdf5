#############################
Expected output for 'h5dump tstr.h5'
#############################
HDF5 "tstr.h5" {
GROUP "/" {
   DATASET "comp1" {
      DATATYPE {
         { STRSIZE 32;
           STRPAD H5T_STR_SPACEPAD;
           CSET H5T_CSET_ASCII;
           CTYPE H5T_C_S1;
         } "string"[3][4];
         H5T_STD_I32BE "int_array"[8][10];
      }
      DATASPACE { SIMPLE ( 3, 6 ) / ( 3, 6 ) }
      DATA {
         {
            [ "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678" ],
            [ 0, 1, 4, 9, 16, 25, 36, 49, 64, 81,
              1, 4, 9, 16, 25, 36, 49, 64, 81, 100,
              4, 9, 16, 25, 36, 49, 64, 81, 100, 121,
              9, 16, 25, 36, 49, 64, 81, 100, 121, 144,
              16, 25, 36, 49, 64, 81, 100, 121, 144, 169,
              25, 36, 49, 64, 81, 100, 121, 144, 169, 196,
              36, 49, 64, 81, 100, 121, 144, 169, 196, 225,
              49, 64, 81, 100, 121, 144, 169, 196, 225, 256 ]
         },
         {
            [ "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678" ],
            [ 1, 4, 9, 16, 25, 36, 49, 64, 81, 100,
              4, 9, 16, 25, 36, 49, 64, 81, 100, 121,
              9, 16, 25, 36, 49, 64, 81, 100, 121, 144,
              16, 25, 36, 49, 64, 81, 100, 121, 144, 169,
              25, 36, 49, 64, 81, 100, 121, 144, 169, 196,
              36, 49, 64, 81, 100, 121, 144, 169, 196, 225,
              49, 64, 81, 100, 121, 144, 169, 196, 225, 256,
              64, 81, 100, 121, 144, 169, 196, 225, 256, 289 ]
         },
         {
            [ "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678" ],
            [ 4, 9, 16, 25, 36, 49, 64, 81, 100, 121,
              9, 16, 25, 36, 49, 64, 81, 100, 121, 144,
              16, 25, 36, 49, 64, 81, 100, 121, 144, 169,
              25, 36, 49, 64, 81, 100, 121, 144, 169, 196,
              36, 49, 64, 81, 100, 121, 144, 169, 196, 225,
              49, 64, 81, 100, 121, 144, 169, 196, 225, 256,
              64, 81, 100, 121, 144, 169, 196, 225, 256, 289,
              81, 100, 121, 144, 169, 196, 225, 256, 289, 324 ]
         },
         {
            [ "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678" ],
            [ 9, 16, 25, 36, 49, 64, 81, 100, 121, 144,
              16, 25, 36, 49, 64, 81, 100, 121, 144, 169,
              25, 36, 49, 64, 81, 100, 121, 144, 169, 196,
              36, 49, 64, 81, 100, 121, 144, 169, 196, 225,
              49, 64, 81, 100, 121, 144, 169, 196, 225, 256,
              64, 81, 100, 121, 144, 169, 196, 225, 256, 289,
              81, 100, 121, 144, 169, 196, 225, 256, 289, 324,
              100, 121, 144, 169, 196, 225, 256, 289, 324, 361 ]
         },
         {
            [ "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678" ],
            [ 16, 25, 36, 49, 64, 81, 100, 121, 144, 169,
              25, 36, 49, 64, 81, 100, 121, 144, 169, 196,
              36, 49, 64, 81, 100, 121, 144, 169, 196, 225,
              49, 64, 81, 100, 121, 144, 169, 196, 225, 256,
              64, 81, 100, 121, 144, 169, 196, 225, 256, 289,
              81, 100, 121, 144, 169, 196, 225, 256, 289, 324,
              100, 121, 144, 169, 196, 225, 256, 289, 324, 361,
              121, 144, 169, 196, 225, 256, 289, 324, 361, 400 ]
         },
         {
            [ "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678" ],
            [ 25, 36, 49, 64, 81, 100, 121, 144, 169, 196,
              36, 49, 64, 81, 100, 121, 144, 169, 196, 225,
              49, 64, 81, 100, 121, 144, 169, 196, 225, 256,
              64, 81, 100, 121, 144, 169, 196, 225, 256, 289,
              81, 100, 121, 144, 169, 196, 225, 256, 289, 324,
              100, 121, 144, 169, 196, 225, 256, 289, 324, 361,
              121, 144, 169, 196, 225, 256, 289, 324, 361, 400,
              144, 169, 196, 225, 256, 289, 324, 361, 400, 441 ]
         },
         {
            [ "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678" ],
            [ 0, 1, 4, 9, 16, 25, 36, 49, 64, 81,
              1, 4, 9, 16, 25, 36, 49, 64, 81, 100,
              4, 9, 16, 25, 36, 49, 64, 81, 100, 121,
              9, 16, 25, 36, 49, 64, 81, 100, 121, 144,
              16, 25, 36, 49, 64, 81, 100, 121, 144, 169,
              25, 36, 49, 64, 81, 100, 121, 144, 169, 196,
              36, 49, 64, 81, 100, 121, 144, 169, 196, 225,
              49, 64, 81, 100, 121, 144, 169, 196, 225, 256 ]
         },
         {
            [ "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678" ],
            [ 1, 4, 9, 16, 25, 36, 49, 64, 81, 100,
              4, 9, 16, 25, 36, 49, 64, 81, 100, 121,
              9, 16, 25, 36, 49, 64, 81, 100, 121, 144,
              16, 25, 36, 49, 64, 81, 100, 121, 144, 169,
              25, 36, 49, 64, 81, 100, 121, 144, 169, 196,
              36, 49, 64, 81, 100, 121, 144, 169, 196, 225,
              49, 64, 81, 100, 121, 144, 169, 196, 225, 256,
              64, 81, 100, 121, 144, 169, 196, 225, 256, 289 ]
         },
         {
            [ "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678" ],
            [ 4, 9, 16, 25, 36, 49, 64, 81, 100, 121,
              9, 16, 25, 36, 49, 64, 81, 100, 121, 144,
              16, 25, 36, 49, 64, 81, 100, 121, 144, 169,
              25, 36, 49, 64, 81, 100, 121, 144, 169, 196,
              36, 49, 64, 81, 100, 121, 144, 169, 196, 225,
              49, 64, 81, 100, 121, 144, 169, 196, 225, 256,
              64, 81, 100, 121, 144, 169, 196, 225, 256, 289,
              81, 100, 121, 144, 169, 196, 225, 256, 289, 324 ]
         },
         {
            [ "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678" ],
            [ 9, 16, 25, 36, 49, 64, 81, 100, 121, 144,
              16, 25, 36, 49, 64, 81, 100, 121, 144, 169,
              25, 36, 49, 64, 81, 100, 121, 144, 169, 196,
              36, 49, 64, 81, 100, 121, 144, 169, 196, 225,
              49, 64, 81, 100, 121, 144, 169, 196, 225, 256,
              64, 81, 100, 121, 144, 169, 196, 225, 256, 289,
              81, 100, 121, 144, 169, 196, 225, 256, 289, 324,
              100, 121, 144, 169, 196, 225, 256, 289, 324, 361 ]
         },
         {
            [ "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678" ],
            [ 16, 25, 36, 49, 64, 81, 100, 121, 144, 169,
              25, 36, 49, 64, 81, 100, 121, 144, 169, 196,
              36, 49, 64, 81, 100, 121, 144, 169, 196, 225,
              49, 64, 81, 100, 121, 144, 169, 196, 225, 256,
              64, 81, 100, 121, 144, 169, 196, 225, 256, 289,
              81, 100, 121, 144, 169, 196, 225, 256, 289, 324,
              100, 121, 144, 169, 196, 225, 256, 289, 324, 361,
              121, 144, 169, 196, 225, 256, 289, 324, 361, 400 ]
         },
         {
            [ "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678" ],
            [ 25, 36, 49, 64, 81, 100, 121, 144, 169, 196,
              36, 49, 64, 81, 100, 121, 144, 169, 196, 225,
              49, 64, 81, 100, 121, 144, 169, 196, 225, 256,
              64, 81, 100, 121, 144, 169, 196, 225, 256, 289,
              81, 100, 121, 144, 169, 196, 225, 256, 289, 324,
              100, 121, 144, 169, 196, 225, 256, 289, 324, 361,
              121, 144, 169, 196, 225, 256, 289, 324, 361, 400,
              144, 169, 196, 225, 256, 289, 324, 361, 400, 441 ]
         },
         {
            [ "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678" ],
            [ 0, 1, 4, 9, 16, 25, 36, 49, 64, 81,
              1, 4, 9, 16, 25, 36, 49, 64, 81, 100,
              4, 9, 16, 25, 36, 49, 64, 81, 100, 121,
              9, 16, 25, 36, 49, 64, 81, 100, 121, 144,
              16, 25, 36, 49, 64, 81, 100, 121, 144, 169,
              25, 36, 49, 64, 81, 100, 121, 144, 169, 196,
              36, 49, 64, 81, 100, 121, 144, 169, 196, 225,
              49, 64, 81, 100, 121, 144, 169, 196, 225, 256 ]
         },
         {
            [ "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678" ],
            [ 1, 4, 9, 16, 25, 36, 49, 64, 81, 100,
              4, 9, 16, 25, 36, 49, 64, 81, 100, 121,
              9, 16, 25, 36, 49, 64, 81, 100, 121, 144,
              16, 25, 36, 49, 64, 81, 100, 121, 144, 169,
              25, 36, 49, 64, 81, 100, 121, 144, 169, 196,
              36, 49, 64, 81, 100, 121, 144, 169, 196, 225,
              49, 64, 81, 100, 121, 144, 169, 196, 225, 256,
              64, 81, 100, 121, 144, 169, 196, 225, 256, 289 ]
         },
         {
            [ "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678" ],
            [ 4, 9, 16, 25, 36, 49, 64, 81, 100, 121,
              9, 16, 25, 36, 49, 64, 81, 100, 121, 144,
              16, 25, 36, 49, 64, 81, 100, 121, 144, 169,
              25, 36, 49, 64, 81, 100, 121, 144, 169, 196,
              36, 49, 64, 81, 100, 121, 144, 169, 196, 225,
              49, 64, 81, 100, 121, 144, 169, 196, 225, 256,
              64, 81, 100, 121, 144, 169, 196, 225, 256, 289,
              81, 100, 121, 144, 169, 196, 225, 256, 289, 324 ]
         },
         {
            [ "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678" ],
            [ 9, 16, 25, 36, 49, 64, 81, 100, 121, 144,
              16, 25, 36, 49, 64, 81, 100, 121, 144, 169,
              25, 36, 49, 64, 81, 100, 121, 144, 169, 196,
              36, 49, 64, 81, 100, 121, 144, 169, 196, 225,
              49, 64, 81, 100, 121, 144, 169, 196, 225, 256,
              64, 81, 100, 121, 144, 169, 196, 225, 256, 289,
              81, 100, 121, 144, 169, 196, 225, 256, 289, 324,
              100, 121, 144, 169, 196, 225, 256, 289, 324, 361 ]
         },
         {
            [ "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678" ],
            [ 16, 25, 36, 49, 64, 81, 100, 121, 144, 169,
              25, 36, 49, 64, 81, 100, 121, 144, 169, 196,
              36, 49, 64, 81, 100, 121, 144, 169, 196, 225,
              49, 64, 81, 100, 121, 144, 169, 196, 225, 256,
              64, 81, 100, 121, 144, 169, 196, 225, 256, 289,
              81, 100, 121, 144, 169, 196, 225, 256, 289, 324,
              100, 121, 144, 169, 196, 225, 256, 289, 324, 361,
              121, 144, 169, 196, 225, 256, 289, 324, 361, 400 ]
         },
         {
            [ "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678",
              "abcdefgh12345678abcdefgh12345678", "abcdefgh12345678abcdefgh1" //
              "2345678", "abcdefgh12345678abcdefgh12345678", "abcdefgh123456" //
              "78abcdefgh12345678" ],
            [ 25, 36, 49, 64, 81, 100, 121, 144, 169, 196,
              36, 49, 64, 81, 100, 121, 144, 169, 196, 225,
              49, 64, 81, 100, 121, 144, 169, 196, 225, 256,
              64, 81, 100, 121, 144, 169, 196, 225, 256, 289,
              81, 100, 121, 144, 169, 196, 225, 256, 289, 324,
              100, 121, 144, 169, 196, 225, 256, 289, 324, 361,
              121, 144, 169, 196, 225, 256, 289, 324, 361, 400,
              144, 169, 196, 225, 256, 289, 324, 361, 400, 441 ]
         }
      }
   }
   DATASET "string1" {
      DATATYPE {
         { STRSIZE 5;
           STRPAD H5T_STR_NULLTERM;
           CSET H5T_CSET_ASCII;
           CTYPE H5T_C_S1;
         }
      }
      DATASPACE { SIMPLE ( 3, 4 ) / ( 3, 4 ) }
      DATA {
         "s1", "s2", "s3", "s4",
         "s5", "s6", "s7", "s8",
         "s9", "s0", "s1", "s2"
      }
   }
   DATASET "string2" {
      DATATYPE {
         { STRSIZE 11;
           STRPAD H5T_STR_SPACEPAD;
           CSET H5T_CSET_ASCII;
           CTYPE H5T_C_S1;
         }
      }
      DATASPACE { SIMPLE ( 20 ) / ( 20 ) }
      DATA {
         "ab cd ef1  ", "ab cd ef2  ", "ab cd ef3  ", "ab cd ef4  ", "ab cd " //
         "ef5  ", "ab cd ef6  ", "ab cd ef7  ", "ab cd ef8  ", "ab cd ef9  ", 
         "ab cd ef0  ", "ab cd ef1  ", "ab cd ef2  ", "ab cd ef3  ", "ab cd " //
         "ef4  ", "ab cd ef5  ", "ab cd ef6  ", "ab cd ef7  ", "ab cd ef8  ", 
         "ab cd ef9  ", "ab cd ef0  "
      }
   }
   DATASET "string3" {
      DATATYPE {
         { STRSIZE 8;
           STRPAD H5T_STR_NULLPAD;
           CSET H5T_CSET_ASCII;
           CTYPE H5T_C_S1;
         }
      }
      DATASPACE { SIMPLE ( 27 ) / ( 27 ) }
      DATA {
         "abcd0\000\000\000", "abcd1\000\000\000", "abcd2\000\000\000", "abc" //
         "d3\000\000\000", "abcd4\000\000\000", "abcd5\000\000\000", "abcd6" //
         "\000\000\000", "abcd7\000\000\000", "abcd8\000\000\000", "abcd9" //
         "\000\000\000", "abcd0\000\000\000", "abcd1\000\000\000", "abcd2" //
         "\000\000\000", "abcd3\000\000\000", "abcd4\000\000\000", "abcd5" //
         "\000\000\000", "abcd6\000\000\000", "abcd7\000\000\000", "abcd8" //
         "\000\000\000", "abcd9\000\000\000", "abcd0\000\000\000", "abcd1" //
         "\000\000\000", "abcd2\000\000\000", "abcd3\000\000\000", "abcd4" //
         "\000\000\000", "abcd5\000\000\000", "abcd6\000\000\000"
      }
   }
   DATASET "string4" {
      DATATYPE {
         { STRSIZE 168;
           STRPAD H5T_STR_SPACEPAD;
           CSET H5T_CSET_ASCII;
           CTYPE H5T_C_S1;
         }
      }
      DATASPACE { SIMPLE ( 3 ) / ( 3 ) }
      DATA {
         "s1234567890123456789                                              " //
         "                                                                  " //
         "                                    ", "s1234567890123456789      " //
         "                                                                  " //
         "                                                                  " //
         "          ", "s1234567890123456789                                " //
         "                                                                  " //
         "                                                  "
      }
   }
}
}
