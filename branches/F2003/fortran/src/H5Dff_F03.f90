! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
!   Copyright by The HDF Group.                                               *
!   Copyright by the Board of Trustees of the University of Illinois.         *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the files COPYING and Copyright.html.  COPYING can be found at the root   *
!   of the source code distribution tree; Copyright.html can be found at the  *
!   root level of an installed copy of the electronic HDF5 document set and   *
!   is linked from the top-level documents page.  It can also be found at     *
!   http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
!   access to either file, you may request a copy from help@hdfgroup.org.     *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
!
!
! This file contains Fortran 2003 interfaces for H5D functions.
! 
MODULE H5D_F03
  USE H5GLOBAL

! NOTES: (1) The maximum rank of an array allowed in Fortran is 7, therefore 
!            we only provide an interface for arrays up to and including rank 7.
!       
!        (2) Unfortunately we are using a generic interface and one of the factors
!            used in determining the proper routine to select is that of the array 
!            rank being passed. Therefore, we can not create just one subroutine for
!            each array type (integer, real, etc...) and use a 
!            rank 1 array of assumed size to handle multiple ranks, i.e.
!                          (i.e. integer, dimension(*) :: ... )
!                          (i.e. real   , dimension(*) :: ... ) etc...
!
!        (3) Could not place the USE ISO_C_BINDING here because it may conflict
!            with the USE ISO_C_BINDING included in the user's program. Moved
!            the statement instead to each subroutine.

  INTERFACE h5dwrite_f

     MODULE PROCEDURE h5dwrite_reference_obj
     MODULE PROCEDURE h5dwrite_reference_dsetreg
     MODULE PROCEDURE h5dwrite_integer_scalar
     MODULE PROCEDURE h5dwrite_integer_1 
     MODULE PROCEDURE h5dwrite_integer_2 
     MODULE PROCEDURE h5dwrite_integer_3 
     MODULE PROCEDURE h5dwrite_integer_4 
     MODULE PROCEDURE h5dwrite_integer_5 
     MODULE PROCEDURE h5dwrite_integer_6 
     MODULE PROCEDURE h5dwrite_integer_7 
     MODULE PROCEDURE h5dwrite_char_scalar
     MODULE PROCEDURE h5dwrite_char_1 
     MODULE PROCEDURE h5dwrite_char_2 
     MODULE PROCEDURE h5dwrite_char_3 
     MODULE PROCEDURE h5dwrite_char_4 
     MODULE PROCEDURE h5dwrite_char_5 
     MODULE PROCEDURE h5dwrite_char_6 
     MODULE PROCEDURE h5dwrite_char_7 
     MODULE PROCEDURE h5dwrite_real_scalar
     MODULE PROCEDURE h5dwrite_real_1
     MODULE PROCEDURE h5dwrite_real_2
     MODULE PROCEDURE h5dwrite_real_3
     MODULE PROCEDURE h5dwrite_real_4
     MODULE PROCEDURE h5dwrite_real_5
     MODULE PROCEDURE h5dwrite_real_6
     MODULE PROCEDURE h5dwrite_real_7
     ! Comment if on Crays
     MODULE PROCEDURE h5dwrite_double_scalar
     MODULE PROCEDURE h5dwrite_double_1
     MODULE PROCEDURE h5dwrite_double_2
     MODULE PROCEDURE h5dwrite_double_3
     MODULE PROCEDURE h5dwrite_double_4
     MODULE PROCEDURE h5dwrite_double_5
     MODULE PROCEDURE h5dwrite_double_6
     MODULE PROCEDURE h5dwrite_double_7
     ! End comment if on Crays

     ! This is the preferred way to call h5dwrite 
     ! by passing an address
     MODULE PROCEDURE h5dwrite_ptr

  END INTERFACE

  INTERFACE h5dread_f

     MODULE PROCEDURE h5dread_reference_obj
     MODULE PROCEDURE h5dread_reference_dsetreg
     MODULE PROCEDURE h5dread_integer_scalar
     MODULE PROCEDURE h5dread_integer_1 
     MODULE PROCEDURE h5dread_integer_2 
     MODULE PROCEDURE h5dread_integer_3 
     MODULE PROCEDURE h5dread_integer_4 
     MODULE PROCEDURE h5dread_integer_5 
     MODULE PROCEDURE h5dread_integer_6 
     MODULE PROCEDURE h5dread_integer_7 
     MODULE PROCEDURE h5dread_char_scalar
     MODULE PROCEDURE h5dread_char_1 
     MODULE PROCEDURE h5dread_char_2 
     MODULE PROCEDURE h5dread_char_3 
     MODULE PROCEDURE h5dread_char_4 
     MODULE PROCEDURE h5dread_char_5 
     MODULE PROCEDURE h5dread_char_6 
     MODULE PROCEDURE h5dread_char_7 
     MODULE PROCEDURE h5dread_real_scalar
     MODULE PROCEDURE h5dread_real_1
     MODULE PROCEDURE h5dread_real_2
     MODULE PROCEDURE h5dread_real_3
     MODULE PROCEDURE h5dread_real_4
     MODULE PROCEDURE h5dread_real_5
     MODULE PROCEDURE h5dread_real_6
     MODULE PROCEDURE h5dread_real_7
     ! Comment if on Crays
     MODULE PROCEDURE h5dread_double_scalar
     MODULE PROCEDURE h5dread_double_1
     MODULE PROCEDURE h5dread_double_2
     MODULE PROCEDURE h5dread_double_3
     MODULE PROCEDURE h5dread_double_4
     MODULE PROCEDURE h5dread_double_5
     MODULE PROCEDURE h5dread_double_6
     MODULE PROCEDURE h5dread_double_7
     ! End comment if on Crays
     
     ! This is the preferred way to call h5dread
     ! by passing an address
     MODULE PROCEDURE h5dread_ptr

  END INTERFACE

! Interface for the function used to pass the C pointer of the buffer
! to the C H5Dwrite routine

  INTERFACE
     INTEGER FUNCTION h5dwrite_f_c(dset_id, mem_type_id, &
          mem_space_id_default ,                         & 
          file_space_id_default,                         &
          xfer_prp_default, buf )
       USE H5GLOBAL
       USE, INTRINSIC :: ISO_C_BINDING
       !DEC$ IF DEFINED(HDF5F90_WINDOWS)
       !DEC$ ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_F_C'::h5dwrite_f_c
       !DEC$ ENDIF
       INTEGER(HID_T), INTENT(IN) :: dset_id
       INTEGER(HID_T), INTENT(IN) :: mem_type_id
       INTEGER(HID_T)  :: mem_space_id_default
       INTEGER(HID_T) :: file_space_id_default
       INTEGER(HID_T) :: xfer_prp_default
       TYPE(C_PTR), VALUE :: buf
     END FUNCTION h5dwrite_f_c
  END INTERFACE

! Interface for the function used to pass the C pointer of the buffer
! to the C H5Dread routine

  INTERFACE
     INTEGER FUNCTION h5dread_f_c(dset_id, mem_type_id, &
          mem_space_id_default,                         & 
          file_space_id_default,                        &
          xfer_prp_default, buf)
       USE H5GLOBAL
       USE, INTRINSIC :: ISO_C_BINDING
       !DEC$ IF DEFINED(HDF5F90_WINDOWS)
       !DEC$ ATTRIBUTES C,reference,decorate,alias:'H5DREAD_F_C'::h5dread_f_c
       !DEC$ ENDIF
       INTEGER(HID_T), INTENT(IN) :: dset_id
       INTEGER(HID_T), INTENT(IN) :: mem_type_id
       INTEGER(HID_T) :: mem_space_id_default
       INTEGER(HID_T) :: file_space_id_default
       INTEGER(HID_T) :: xfer_prp_default
       TYPE(C_PTR), VALUE :: buf
     END FUNCTION h5dread_f_c
  END INTERFACE

  INTERFACE h5dfill_f
     MODULE PROCEDURE h5dfill_integer
     MODULE PROCEDURE h5dfill_real
     MODULE PROCEDURE h5dfill_double
     MODULE PROCEDURE h5dfill_char
  END INTERFACE

! Interface for the function used to pass the C pointer of the buffer
! to the C H5Dfill routine

  INTERFACE
     INTEGER FUNCTION h5dfill_c(f_ptr_fill_value, fill_type_id, space_id, & 
          f_ptr_buf, mem_type_id)
       USE H5GLOBAL
       USE, INTRINSIC :: ISO_C_BINDING
       !DEC$ IF DEFINED(HDF5F90_WINDOWS)
       !DEC$ ATTRIBUTES C,reference,decorate,alias:'H5DFILL_C'::h5dfill_c
       !DEC$ ENDIF
       TYPE(C_PTR), VALUE :: f_ptr_fill_value
       INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
       INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
       TYPE(C_PTR), VALUE :: f_ptr_buf
       INTEGER(HID_T) :: mem_type_id
     END FUNCTION h5dfill_c
  END INTERFACE
  
CONTAINS

  SUBROUTINE h5dwrite_reference_obj(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_reference_obj
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims ! size of the bufffer buf
    TYPE(hobj_ref_t_f), DIMENSION(dims(1)), INTENT(IN), TARGET :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
    

  END SUBROUTINE h5dwrite_reference_obj

  SUBROUTINE h5dwrite_reference_dsetreg(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_reference_dsetreg
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims ! size of the bufffer buf  
    TYPE(hdset_reg_ref_t_f), DIMENSION(dims(1)), INTENT(IN), TARGET :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_reference_dsetreg


  SUBROUTINE h5dwrite_integer_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_integer_scalar
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER, INTENT(IN), TARGET :: buf ! Data buffer
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_integer_scalar

  SUBROUTINE h5dwrite_integer_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_integer_1
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T)  :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_integer_1

  SUBROUTINE h5dwrite_integer_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_integer_2
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1),dims(2)),TARGET :: buf   ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 
    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_integer_2

  SUBROUTINE h5dwrite_integer_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_integer_3
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 
    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_integer_3

  SUBROUTINE h5dwrite_integer_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_integer_4
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_integer_4

  SUBROUTINE h5dwrite_integer_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_integer_5
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_integer_5

  SUBROUTINE h5dwrite_integer_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_integer_6
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_integer_6

  SUBROUTINE h5dwrite_integer_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_integer_7
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_integer_7

  SUBROUTINE h5dwrite_char_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_char_scalar
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(*), INTENT(IN), TARGET :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 
    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 

    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_char_scalar

  SUBROUTINE h5dwrite_char_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_char_1
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_char_1

  SUBROUTINE h5dwrite_char_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_char_2
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_char_2

  SUBROUTINE h5dwrite_char_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_char_3
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_char_3

  SUBROUTINE h5dwrite_char_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_char_4
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 
    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_char_4

  SUBROUTINE h5dwrite_char_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_char_5
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_char_5

  SUBROUTINE h5dwrite_char_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_char_6
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_char_6

  SUBROUTINE h5dwrite_char_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_char_7
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_char_7


  SUBROUTINE h5dwrite_real_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_real_scalar
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(IN), TARGET :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 
    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_real_scalar


  SUBROUTINE h5dwrite_real_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_real_1
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(IN), &
         DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 
    INTEGER(HID_T) :: xfer_prp_default  
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_real_1

  SUBROUTINE h5dwrite_real_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_real_2
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_real_2

  SUBROUTINE h5dwrite_real_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_real_3
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 
    INTEGER(HID_T) :: xfer_prp_default  
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_real_3

  SUBROUTINE h5dwrite_real_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_real_4
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 
    INTEGER(HID_T) :: xfer_prp_default  
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_real_4

  SUBROUTINE h5dwrite_real_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_real_5
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 
    INTEGER(HID_T) :: xfer_prp_default  
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_real_5

  SUBROUTINE h5dwrite_real_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_real_6
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 
    INTEGER(HID_T) :: xfer_prp_default  
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_real_6

  SUBROUTINE h5dwrite_real_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_real_7
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 
    INTEGER(HID_T) :: xfer_prp_default  
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_real_7

  SUBROUTINE h5dwrite_double_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_double_scalar
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(IN), TARGET :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default            
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_double_scalar

  SUBROUTINE h5dwrite_double_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_double_1
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(IN), &
         DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_double_1

  SUBROUTINE h5dwrite_double_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_double_2
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(IN), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_double_2

  SUBROUTINE h5dwrite_double_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_double_3
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_double_3

  SUBROUTINE h5dwrite_double_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_double_4
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
 
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_double_4

  SUBROUTINE h5dwrite_double_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_double_5
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_double_5

  SUBROUTINE h5dwrite_double_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_double_6
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_double_6

  SUBROUTINE h5dwrite_double_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dwrite_double_7
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_double_7

  !----------------------------------------------------------------------
  ! Name:		h5dread_f 
  !
  ! Purpose: 	Reads raw data from the specified dataset into buf, 
  !		converting from file datatype and dataspace to memory 
  !		datatype and dataspace.
  !
  ! Inputs:  
  !		dset_id		- dataset identifier
  !		mem_type_id	- memory type identifier
  !		dims		- 1-dim array of size 7; dims(k) has the size 
  !				- of k-th dimension of the buf array
  ! Outputs:  
  !		buf		- buffer to read data in
  !		hdferr:		- error code		
  !				 	Success:  0
  !				 	Failure: -1   
  ! Optional parameters:
  !		mem_space_id	- memory dataspace identifier
  !		file_space_id 	- file dataspace identifier
  !		xfer_prp	- trasfer property list identifier	
  !
  ! Programmer:	Elena Pourmal
  !		August 12, 1999	
  !
  ! Modifications: 	Explicit Fortran interfaces were added for 
  !			called C functions (it is needed for Windows
  !			port).  February 28, 2001 
  !
  !                       dims parameter was added to make code portable;
  !                       n parameter was replaced with dims parameter in
  !			the h5dwrite_reference_obj and h5dwrite_reference_dsetreg
  !			functions.  April 2, 2001
  !
  ! Comment:		This function is overloaded to read INTEGER,
  !			REAL, DOUBLE PRECISION and CHARACTER buffers
  !			up to 7 dimensions, and one dimensional buffers
  !			of the TYPE(hobj_ref_t_f) and TYPE(hdset_reg_ref_t_f)
  !			types.	
  !----------------------------------------------------------------------
  SUBROUTINE h5dread_reference_obj(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_reference_obj
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    TYPE(hobj_ref_t_f), INTENT(INOUT) , &
         DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_reference_obj

  SUBROUTINE h5dread_reference_dsetreg(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_reference_dsetreg
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    TYPE(hdset_reg_ref_t_f), INTENT(INOUT), & 
         DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 
    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_reference_dsetreg


  SUBROUTINE h5dread_integer_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_integer_scalar
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(INOUT) , TARGET :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)


  END SUBROUTINE h5dread_integer_scalar

  SUBROUTINE h5dread_integer_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_integer_1
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(INOUT), &
         DIMENSION(dims(1)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_integer_1

  SUBROUTINE h5dread_integer_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_integer_2
    !DEC$endif
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 
    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_integer_2

  SUBROUTINE h5dread_integer_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_integer_3
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_integer_3

  SUBROUTINE h5dread_integer_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_integer_4
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 
    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_integer_4

  SUBROUTINE h5dread_integer_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_integer_5
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
    
  END SUBROUTINE h5dread_integer_5

  SUBROUTINE h5dread_integer_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_integer_6
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_integer_6

  SUBROUTINE h5dread_integer_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_integer_7
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_integer_7

  SUBROUTINE h5dread_char_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_char_scalar
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT) , TARGET :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_char_scalar

  SUBROUTINE h5dread_char_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_char_1
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_char_1

  SUBROUTINE h5dread_char_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_char_2
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr 

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_char_2

  SUBROUTINE h5dread_char_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_char_3
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_char_3

  SUBROUTINE h5dread_char_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_char_4
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr
    
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_char_4

  SUBROUTINE h5dread_char_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_char_5
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_char_5

  SUBROUTINE h5dread_char_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_char_6
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id  
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_char_6

  SUBROUTINE h5dread_char_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_char_7
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_char_7

  SUBROUTINE h5dread_real_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_real_scalar
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(INOUT) , TARGET :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 
    INTEGER(HID_T) :: xfer_prp_default  
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_real_scalar

  SUBROUTINE h5dread_real_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_real_1
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 
    INTEGER(HID_T) :: xfer_prp_default  
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id  
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_real_1

  SUBROUTINE h5dread_real_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_real_2
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 
    INTEGER(HID_T) :: xfer_prp_default  
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_real_2

  SUBROUTINE h5dread_real_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_real_3
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 
    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id  
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_real_3

  SUBROUTINE h5dread_real_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_real_4
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3), dims(4)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 
    INTEGER(HID_T) :: xfer_prp_default  
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id  
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_real_4

  SUBROUTINE h5dread_real_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_real_5
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 
    INTEGER(HID_T) :: xfer_prp_default  
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id  
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_real_5

  SUBROUTINE h5dread_real_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_real_6
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 
    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_real_6

  SUBROUTINE h5dread_real_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_real_7
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id  
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_real_7

  SUBROUTINE h5dread_double_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_double_scalar
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(INOUT) , TARGET :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_double_scalar

  SUBROUTINE h5dread_double_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_double_1
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(INOUT), &
         DIMENSION(dims(1)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id

    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_double_1

  SUBROUTINE h5dread_double_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_double_2
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_double_2

  SUBROUTINE h5dread_double_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_double_3
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_double_3

  SUBROUTINE h5dread_double_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_double_4
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) , TARGET :: buf
    ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_double_4

  SUBROUTINE h5dread_double_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_double_5
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr


    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)


  END SUBROUTINE h5dread_double_5

  SUBROUTINE h5dread_double_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_double_6
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) , TARGET :: buf
    ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default 
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id 
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_double_6

  SUBROUTINE h5dread_double_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dread_double_7
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) , TARGET :: buf
    ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
    ! Memory dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
    ! File dataspace identfier 
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
    ! Transfer property list identifier 

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default 
    INTEGER(HID_T) :: file_space_id_default 
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp 
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id 
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id  
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_double_7

  SUBROUTINE h5dwrite_ptr(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ptr
!DEC$endif
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    TYPE(C_PTR), INTENT(INOUT) :: buf
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! not used
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
    ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
    ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
    ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf)

  END SUBROUTINE h5dwrite_ptr

  SUBROUTINE h5dread_ptr(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_f
!DEC$endif

    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    TYPE(C_PTR), INTENT(INOUT) :: buf
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! not used
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
    ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
    ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
    ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf)

  END SUBROUTINE h5dread_ptr

  !----------------------------------------------------------------------
  ! Name:		h5dfill_integer
  !
  ! Purpose:      Fills dataspace elements with a fill value in a memory buffer.	
  !               Only INTEGER, CHARACTER, REAL and DOUBLE PRECISION datatypes 
  !               of the fillvalues and buffers are supported. Buffer and fillvalue
  !               are assumed to have the same datatype.
  !               Only one-dimesional buffers are supported.
  !
  ! Inputs:  
  !		fill_value	- fill value
  !		space_id	- memory space selection identifier
  !		buf		- data buffer iin memory ro apply selection to
  !				- of k-th dimension of the buf array
  ! Outputs:  
  !		hdferr:		- error code		
  !				 	Success:  0
  !				 	Failure: -1   
  !
  ! Programmer:	Elena Pourmal
  !		March 12, 2003
  !
  !----------------------------------------------------------------------

  SUBROUTINE h5dfill_integer(fill_value, space_id, buf,  hdferr)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dfill_integer
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER, INTENT(IN), TARGET :: fill_value  ! Fill value
    INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
    INTEGER, INTENT(IN), DIMENSION(*), TARGET :: buf ! Memory buffer to fill in
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 

    INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
    INTEGER(HID_T) :: mem_type_id !  Buffer dadtype identifier

    TYPE(C_PTR) :: f_ptr_fill_value ! C pointer to fill_value
    TYPE(C_PTR) :: f_ptr_buf ! C pointer to buf

    f_ptr_fill_value = C_LOC(fill_value)
    f_ptr_buf = C_LOC(buf)

    fill_type_id = H5T_NATIVE_INTEGER
    mem_type_id  = H5T_NATIVE_INTEGER

    hdferr = h5dfill_c(f_ptr_fill_value, fill_type_id, space_id, & 
         f_ptr_buf, mem_type_id)
    
  END SUBROUTINE h5dfill_integer

  !----------------------------------------------------------------------
  ! Name:		h5dfill_real
  !
  ! Purpose:      Fills dataspace elements with a fill value in a memory buffer.	
  !               Only INTEGER, CHARACTER, REAL and DOUBLE PRECISION datatypes 
  !               of the fillvalues and buffers are supported. Buffer and fillvalue
  !               are assumed to have the same datatype.
  !               Only one-dimesional buffers are supported.
  !
  ! Inputs:  
  !		fill_value	- fill value
  !		space_id	- memory space selection identifier
  !		buf		- data buffer iin memory ro apply selection to
  !				- of k-th dimension of the buf array
  ! Outputs:  
  !		hdferr:		- error code		
  !				 	Success:  0
  !				 	Failure: -1   
  !
  ! Programmer:	Elena Pourmal
  !		March 12, 2003
  !
  !----------------------------------------------------------------------

  SUBROUTINE h5dfill_real(fill_valuer, space_id, buf,  hdferr)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dfill_real
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    REAL, INTENT(IN), TARGET :: fill_valuer  ! Fill value
    INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
    REAL, INTENT(IN), DIMENSION(*), TARGET :: buf ! Memory buffer to fill in
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 

    INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
    INTEGER(HID_T) :: mem_type_id !  Buffer dadtype identifier 

    TYPE(C_PTR) :: f_ptr_fill_valuer ! C pointer to fill_value
    TYPE(C_PTR) :: f_ptr_buf ! C pointer to buf

    f_ptr_fill_valuer = C_LOC(fill_valuer)
    f_ptr_buf = C_LOC(buf)

    fill_type_id = H5T_NATIVE_REAL
    mem_type_id  = H5T_NATIVE_REAL

    hdferr = h5dfill_c(f_ptr_fill_valuer, fill_type_id, space_id, & 
         f_ptr_buf, mem_type_id)

  END SUBROUTINE h5dfill_real

  !----------------------------------------------------------------------
  ! Name:		h5dfill_double
  !
  ! Purpose:      Fills dataspace elements with a fill value in a memory buffer.	
  !               Only INTEGER, CHARACTER, REAL and DOUBLE PRECISION datatypes 
  !               of the fillvalues and buffers are supported. Buffer and fillvalue
  !               are assumed to have the same datatype.
  !               Only one-dimesional buffers are supported.
  !
  ! Inputs:  
  !		fill_value	- fill value
  !		space_id	- memory space selection identifier
  !		buf		- data buffer iin memory ro apply selection to
  !				- of k-th dimension of the buf array
  ! Outputs:  
  !		hdferr:		- error code		
  !				 	Success:  0
  !				 	Failure: -1   
  !
  ! Programmer:	Elena Pourmal
  !		March 12, 2003
  !
  !----------------------------------------------------------------------

  SUBROUTINE h5dfill_double(fill_value, space_id, buf,  hdferr)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dfill_double
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN), TARGET :: fill_value  ! Fill value
    INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
    DOUBLE PRECISION, INTENT(IN), DIMENSION(*), TARGET :: buf ! Memory buffer to fill in
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 

    INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
    INTEGER(HID_T) :: mem_type_id !  Buffer dadtype identifier 

    TYPE(C_PTR) :: f_ptr_fill_value ! C pointer to fill_value
    TYPE(C_PTR) :: f_ptr_buf ! C pointer to buf

    f_ptr_fill_value = C_LOC(fill_value)
    f_ptr_buf = C_LOC(buf)

    fill_type_id = H5T_NATIVE_DOUBLE
    mem_type_id  = H5T_NATIVE_DOUBLE

    hdferr = h5dfill_c(f_ptr_fill_value, fill_type_id, space_id, & 
         f_ptr_buf, mem_type_id)

  END SUBROUTINE h5dfill_double

  !----------------------------------------------------------------------
  ! Name:		h5dfill_char
  !
  ! Purpose:      Fills dataspace elements with a fill value in a memory buffer.	
  !               Only INTEGER, CHARACTER, REAL and DOUBLE PRECISION datatypes 
  !               of the fillvalues and buffers are supported. Buffer and fillvalue
  !               are assumed to have the same datatype.
  !               Only one-dimesional buffers are supported.
  !
  ! Inputs:  
  !		fill_value	- fill value
  !		space_id	- memory space selection identifier
  !		buf		- data buffer iin memory ro apply selection to
  !				- of k-th dimension of the buf array
  ! Outputs:  
  !		hdferr:		- error code		
  !				 	Success:  0
  !				 	Failure: -1   
  !
  ! Programmer:	Elena Pourmal
  !		March 12, 2003
  !
  !----------------------------------------------------------------------

  SUBROUTINE h5dfill_char(fill_value, space_id, buf,  hdferr)
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5dfill_char
    !DEC$endif

    USE ISO_C_BINDING
    IMPLICIT NONE
    CHARACTER, INTENT(IN), TARGET :: fill_value  ! Fill value
    INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
    CHARACTER, INTENT(IN), DIMENSION(*), TARGET :: buf ! Memory buffer to fill in
    INTEGER, INTENT(OUT) :: hdferr      ! Error code 

    INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
    INTEGER(HID_T) :: mem_type_id !  Buffer dadtype identifier 

    TYPE(C_PTR) :: f_ptr_fill_value ! C pointer to fill_value
    TYPE(C_PTR) :: f_ptr_buf ! C pointer to buf

    f_ptr_fill_value = C_LOC(fill_value)
    f_ptr_buf = C_LOC(buf)

    hdferr = h5dfill_c(f_ptr_fill_value, fill_type_id, space_id, & 
         f_ptr_buf, mem_type_id)

  END SUBROUTINE h5dfill_char

END MODULE H5D_F03


