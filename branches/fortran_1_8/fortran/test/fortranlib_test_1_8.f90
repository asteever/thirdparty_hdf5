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
! 
!    Testing Fortran functionality.
!
PROGRAM fortranlibtest

  USE HDF5

  IMPLICIT NONE
  INTEGER :: total_error = 0
  INTEGER :: error 
  INTEGER :: mounting_total_error = 0
  INTEGER :: reopen_total_error = 0
  INTEGER :: fclose_total_error = 0
  INTEGER :: fspace_total_error = 0
  INTEGER :: dataset_total_error = 0
  INTEGER :: extend_dataset_total_error = 0
  INTEGER :: refobj_total_error = 0
  INTEGER :: refreg_total_error = 0
  INTEGER :: dataspace_total_error = 0
  INTEGER :: hyperslab_total_error = 0
  INTEGER :: element_total_error = 0
  INTEGER :: basic_select_total_error = 0
  INTEGER :: total_error_compoundtest = 0
  INTEGER :: basic_datatype_total_error = 0
  INTEGER :: enum_total_error = 0
  INTEGER :: external_total_error = 0
  INTEGER :: multi_file_total_error = 0
  INTEGER :: attribute_total_error = 0
  INTEGER :: group_total_error = 0
  INTEGER :: majnum, minnum, relnum
  CHARACTER(LEN=8) error_string
  CHARACTER(LEN=8) :: success = ' PASSED '
  CHARACTER(LEN=8) :: failure = '*FAILED*'
  CHARACTER(LEN=4) :: e_format ='(8a)'
  LOGICAL :: cleanup = .TRUE.
  !     LOGICAL :: cleanup = .FALSE.

  CALL h5open_f(error) 
  WRITE(*,*) '                       ==========================                            '
  WRITE(*,*) '                              FORTRAN tests '
  WRITE(*,*) '                       ==========================                            '
  CALL h5get_libversion_f(majnum, minnum, relnum, total_error)
  IF(total_error .EQ. 0) THEN

     WRITE(*, '(" FORTRANLIB_TEST is linked with HDF5 Library version ")', advance="NO")
     WRITE(*, '(I1)', advance="NO") majnum
     WRITE(*, '(".")', advance="NO") 
     WRITE(*, '(I1)', advance="NO") minnum
     WRITE(*, '(" release ")', advance="NO")
     WRITE(*, '(I3)') relnum
  ELSE
     total_error = total_error + 1
  ENDIF
  WRITE(*,*)
  !     CALL h5check_version_f(1,4,4,total_error)
  !     write(*,*) '========================================='
  !     write(*,*) 'Testing FILE Interface                   '
  !     write(*,*) '========================================='
  error_string = failure
  CALL mountingtest(cleanup, mounting_total_error)
  IF (mounting_total_error == 0) error_string = success
  WRITE(*, fmt = '(14a)', advance = 'no') ' Mounting test'     
  WRITE(*, fmt = '(56x,a)', advance = 'no') ' ' 

  WRITE(*, fmt = e_format) error_string 
  total_error = total_error + mounting_total_error 
  error_string = failure
  CALL reopentest(cleanup, reopen_total_error)
  IF (reopen_total_error == 0) error_string = success
  WRITE(*, fmt = '(12a)', advance = 'no') ' Reopen test'     
  WRITE(*, fmt = '(58x,a)', advance = 'no') ' ' 
  WRITE(*, fmt = e_format) error_string
  total_error = total_error + reopen_total_error 

  !DEC$ if defined(H5_VMS)
  GOTO 100
  !DEC$ else
  error_string = failure
  CALL file_close(cleanup, fclose_total_error)
  IF (fclose_total_error == 0) error_string = success
  WRITE(*, fmt = '(21a)', advance = 'no') ' File open/close test'     
  WRITE(*, fmt = '(49x,a)', advance = 'no') ' ' 
  WRITE(*, fmt = e_format) error_string
  total_error = total_error + fclose_total_error 
  !DEC$ endif
100 CONTINUE
  error_string = failure
  CALL file_space(cleanup, fspace_total_error)
  IF (fspace_total_error == 0) error_string = success
  WRITE(*, fmt = '(21a)', advance = 'no') ' File free space test'     
  WRITE(*, fmt = '(49x,a)', advance = 'no') ' ' 
  WRITE(*, fmt = e_format) error_string
  total_error = total_error + fspace_total_error 

  !     write(*,*)
  !     write(*,*) '========================================='
  !     write(*,*) 'Testing DATASET Interface                '
  !     write(*,*) '========================================='

  error_string = failure
  CALL datasettest(cleanup, dataset_total_error)
  IF (dataset_total_error == 0) error_string = success
  WRITE(*, fmt = '(13a)', advance = 'no') ' Dataset test'     
  WRITE(*, fmt = '(57x,a)', advance = 'no')  ' '
  WRITE(*, fmt = e_format) error_string
  total_error = total_error + dataset_total_error 
  error_string = failure
  CALL extenddsettest(cleanup, extend_dataset_total_error)
  IF (extend_dataset_total_error == 0)  error_string = success
  WRITE(*, fmt = '(24a)', advance = 'no') ' Extendible dataset test'     
  WRITE(*, fmt = '(46x,a)', advance = 'no') ' '
  WRITE(*, fmt = e_format) error_string
  total_error = total_error + extend_dataset_total_error 
  !     write(*,*)
  !     write(*,*) '========================================='
  !     write(*,*) 'Testing DATASPACE Interface             '
  !     write(*,*) '========================================='

  error_string = failure
  CALL dataspace_basic_test(cleanup, dataspace_total_error)
  IF (dataspace_total_error == 0) error_string = success
  WRITE(*, fmt = '(21a)', advance = 'no') ' Basic dataspace test'     
  WRITE(*, fmt = '(49x,a)', advance = 'no')  ' '
  WRITE(*, fmt = e_format) error_string
  total_error = total_error + dataspace_total_error 


  !     write(*,*)
  !     write(*,*) '========================================='
  !     write(*,*) 'Testing REFERENCE Interface              '
  !     write(*,*) '========================================='

  error_string = failure
  CALL refobjtest(cleanup, refobj_total_error)
  IF (refobj_total_error == 0) error_string = success
  WRITE(*, fmt = '(25a)', advance = 'no') ' Reference to object test'     
  WRITE(*, fmt = '(45x,a)', advance = 'no')  ' '
  WRITE(*, fmt = e_format) error_string
  total_error = total_error + refobj_total_error 

  error_string = failure
  CALL refregtest(cleanup, refreg_total_error)
  IF (refreg_total_error == 0) error_string = success
  WRITE(*, fmt = '(33a)', advance = 'no') ' Reference to dataset region test'     
  WRITE(*, fmt = '(37x,a)', advance = 'no')  ' ' 
  WRITE(*, fmt = e_format) error_string
  total_error = total_error + refreg_total_error 

  !     write(*,*)
  !     write(*,*) '========================================='
  !     write(*,*) 'Testing selection functionalities        '
  !     write(*,*) '========================================='

  error_string = failure
  CALL test_basic_select(cleanup, basic_select_total_error)
  IF (basic_select_total_error == 0) error_string = success
  WRITE(*, fmt = '(21a)', advance = 'no') ' Basic selection test'     
  WRITE(*, fmt = '(49x,a)', advance = 'no')  ' '
  WRITE(*, fmt = e_format) error_string
  total_error = total_error + basic_select_total_error 

  error_string = failure
  CALL  test_select_hyperslab( cleanup, hyperslab_total_error)
  IF ( hyperslab_total_error == 0) error_string = success
  WRITE(*, fmt = '(25a)', advance = 'no') ' Hyperslab selection test'     
  WRITE(*, fmt = '(45x,a)', advance = 'no')  ' ' 
  WRITE(*, fmt = e_format) error_string
  total_error = total_error + hyperslab_total_error 

  error_string = failure
  CALL test_select_element(cleanup, element_total_error)
  IF (element_total_error == 0) error_string = success
  WRITE(*, fmt = '(23a)', advance = 'no') ' Element selection test'     
  WRITE(*, fmt = '(47x,a)', advance = 'no')  ' '
  WRITE(*, fmt = e_format) error_string
  total_error = total_error + element_total_error 


  !     write(*,*)
  !     write(*,*) '========================================='
  !     write(*,*) 'Testing DATATYPE interface               '
  !     write(*,*) '========================================='
  error_string = failure
  CALL basic_data_type_test(cleanup, basic_datatype_total_error)
  IF (basic_datatype_total_error == 0) error_string = success
  WRITE(*, fmt = '(20a)', advance = 'no') ' Basic datatype test'     
  WRITE(*, fmt = '(50x,a)', advance = 'no')  ' '
  WRITE(*, fmt = e_format) error_string
  total_error = total_error + basic_datatype_total_error 

  error_string = failure
  CALL compoundtest(cleanup, total_error_compoundtest)
  IF (total_error_compoundtest == 0) error_string = success
  WRITE(*, fmt = '(23a)', advance = 'no') ' Compound datatype test'     
  WRITE(*, fmt = '(47x,a)', advance = 'no')  ' '
  WRITE(*, fmt = e_format) error_string
  total_error = total_error + total_error_compoundtest
  error_string = failure
  CALL enumtest(cleanup, enum_total_error)
  IF (enum_total_error == 0) error_string = success
  WRITE(*, fmt = '(19a)', advance = 'no') ' Enum datatype test'     
  WRITE(*, fmt = '(51x,a)', advance = 'no')  ' '
  WRITE(*, fmt = e_format) error_string
  total_error = total_error + enum_total_error 
  !     write(*,*)
  !     write(*,*) '========================================='
  !     write(*,*) 'Testing PROPERTY interface               ' 
  !     write(*,*) '========================================='

  error_string = failure
  CALL external_test(cleanup, external_total_error)
  IF (external_total_error == 0) error_string = success
  WRITE(*, fmt = '(22a)', advance = 'no') ' External dataset test'     
  WRITE(*, fmt = '(48x,a)', advance = 'no')  ' '
  WRITE(*, fmt = e_format) error_string
  total_error = total_error + external_total_error 

  error_string = failure
  !     error_string = skip
  cleanup = .FALSE.
  CALL multi_file_test(cleanup, multi_file_total_error)
  IF (multi_file_total_error == 0) error_string = success
  WRITE(*, fmt = '(23a)', advance = 'no') ' Multi file driver test'     
  WRITE(*, fmt = '(47x,a)', advance = 'no')  ' '
  WRITE(*, fmt = e_format) error_string
  total_error = total_error + multi_file_total_error 
  !     write(*,*)
  !     write(*,*) '========================================='
  !     write(*,*) 'Testing ATTRIBUTE interface              ' 
  !     write(*,*) '========================================='

  error_string = failure
  CALL attribute_test_1_8(cleanup, attribute_total_error)
  WRITE(*, fmt = '(15a)', advance = 'no') ' ATTRIBUTE TEST'     
  WRITE(*, fmt = '(55x,a)', advance = 'no')  ' '
  IF (attribute_total_error == 0) error_string = success
  WRITE(*, fmt = e_format) error_string
  total_error = total_error + attribute_total_error

  CALL group_test(cleanup, group_total_error)
  WRITE(*, fmt = '(15a)', advance = 'no') ' GROUP TEST'     
  WRITE(*, fmt = '(55x,a)', advance = 'no')  ' '
  IF (group_total_error == 0) error_string = success
  WRITE(*, fmt = e_format) error_string
  total_error = total_error + group_total_error

  call test_h5o(cleanup, group_total_error )

  CALL dtransform(cleanup, group_total_error)


!!$
!!$  !     write(*,*)
!!$  !     write(*,*) '========================================='
!!$  !     write(*,*) 'Testing IDENTIFIER interface             '
!!$  !     write(*,*) '========================================='
!!$
!!$  error_string = failure
!!$  CALL identifier_test(cleanup, identifier_total_error)
!!$  IF (identifier_total_error == 0) error_string = success
!!$  WRITE(*, fmt = '(16a)', advance = 'no') ' Identifier test'     
!!$  WRITE(*, fmt = '(54x,a)', advance = 'no')  ' '
!!$  WRITE(*, fmt = e_format) error_string
!!$  total_error = total_error + identifier_total_error 
!!$  error_string = failure
!!$  CALL filters_test(cleanup, z_total_error)
!!$  IF (z_total_error == 0) error_string = success
!!$  WRITE(*, fmt = '(13a)', advance = 'no') ' Filters test'     
!!$  WRITE(*, fmt = '(57x,a)', advance = 'no')  ' '
!!$  WRITE(*, fmt = e_format) error_string
!!$  total_error = total_error + z_total_error 
!!$
!!$  CALL szip_test(szip_flag, cleanup, sz_total_error)
!!$  IF (sz_total_error == 0) error_string = success
!!$  ! Reset the flag is compression was not available 
!!$  IF (.NOT. szip_flag) error_string = skip
!!$  IF (sz_total_error .GT. 0) error_string = failure
!!$  WRITE(*, fmt = '(18a)', advance = 'no') ' SZIP filter test'     
!!$  WRITE(*, fmt = '(53x,a)', advance = 'no')  ' '
!!$  WRITE(*, fmt = e_format) error_string
!!$  IF(sz_total_error .GT. 0) total_error = total_error + sz_total_error 
!!$
!!$  !     write(*,*)
!!$  !     write(*,*) '========================================='
!!$  !     write(*,*) 'Testing GROUP interface             '
!!$  !     write(*,*) '========================================='
!!$
!!$  error_string = failure
!!$  CALL group_test(cleanup, group_total_error)
!!$  IF (group_total_error == 0) error_string = success
!!$  WRITE(*, fmt = '(11a)', advance = 'no') ' Group test'     
!!$  WRITE(*, fmt = '(59x,a)', advance = 'no')  ' '
!!$  WRITE(*, fmt = e_format) error_string
!!$  total_error = total_error + group_total_error 
!!$
!!$  error_string = failure
!!$  CALL error_report_test(cleanup, error_total_error)
!!$  IF (error_total_error == 0) error_string = success
!!$  WRITE(*, fmt = '(11a)', advance = 'no') ' Error test'     
!!$  WRITE(*, fmt = '(59x,a)', advance = 'no')  ' '
!!$  WRITE(*, fmt = e_format) error_string
!!$  total_error = total_error + error_total_error 
!!$
!!$  error_string = failure
!!$  CALL vl_test_integer(cleanup, vl_total_error)
!!$  CALL vl_test_real(cleanup, vl_total_error)
!!$  CALL vl_test_string(cleanup, vl_total_error)
!!$  IF (vl_total_error == 0) error_string = success
!!$  WRITE(*, fmt = '(11a)', advance = 'no') ' VL test'     
!!$  WRITE(*, fmt = '(62x,a)', advance = 'no')  ' '
!!$  WRITE(*, fmt = e_format) error_string
!!$  total_error = total_error + vl_total_error 

  WRITE(*,*)

  WRITE(*,*) '                  ============================================  '
  WRITE(*, fmt = '(19x, 27a)', advance='NO') ' FORTRAN tests completed with '
  WRITE(*, fmt = '(i4)', advance='NO') total_error
  WRITE(*, fmt = '(12a)' ) ' error(s) ! '
  WRITE(*,*) '                  ============================================  '

  CALL h5close_f(error)

  ! if errors detected, exit with non-zero code.
  IF (total_error .NE. 0) CALL h5_exit_f (1)

END PROGRAM fortranlibtest

SUBROUTINE dtransform(cleanup, total_error)
  USE HDF5 ! This module contains all necessary modules 
  
  IMPLICIT NONE
  LOGICAL, INTENT(IN)  :: cleanup
  INTEGER, INTENT(INOUT) :: total_error

  INTEGER(HID_T) :: dxpl_id_c_to_f, dxpl_id_c_to_f_copy
  INTEGER(HID_T) :: dxpl_id_simple, dxpl_id_polynomial, dxpl_id_polynomial_copy, dxpl_id_utrans_inv, file_id
  
  CHARACTER(LEN=15), PARAMETER :: c_to_f = "(9/5.0)*x + 123"
  INTEGER :: error
  CHARACTER(LEN=15) :: ptrgetTest
  CHARACTER(LEN=7) :: ptrgetTest_small
  CHARACTER(LEN=30) :: ptrgetTest_big

  INTEGER(SIZE_T) :: size


  CALL H5Fcreate_f("dtransform.h5", H5F_ACC_TRUNC_F, file_id, error)
  CALL check("dtransform.H5Fcreate_f", error, total_error)

  CALL H5Pcreate_f(H5P_DATASET_XFER_F, dxpl_id_c_to_f, error)
  CALL check("dtransform.H5Pcreate_f", error, total_error)
  
  CALL H5Pset_data_transform_f(dxpl_id_c_to_f,c_to_f, error)
  CALL check("dtransform.H5Pset_data_transform_f", error, total_error)

  CALL H5Pget_data_transform_f(dxpl_id_c_to_f, ptrgetTest, error, size=size)
  CALL check("dtransform.H5Pget_data_transform_f",  error, total_error)
  CALL VerifyString("dtransform.H5Pget_data_transform_f", c_to_f, ptrgetTest, total_error)
  CALL VERIFY("dtransform.H5Pget_data_transform_f", size,15, total_error)

! check case when receiving buffer to small

  CALL H5Pget_data_transform_f(dxpl_id_c_to_f, ptrgetTest_small, error, size=size)
  CALL check("dtransform.H5Pget_data_transform_f",  error, total_error)
  CALL VerifyString("dtransform.H5Pget_data_transform_f", c_to_f(1:7), ptrgetTest_small, total_error)
  CALL VERIFY("dtransform.H5Pget_data_transform_f", size,15, total_error)

! check case when receiving buffer to big

  CALL H5Pget_data_transform_f(dxpl_id_c_to_f, ptrgetTest_big, error, size=size)
  CALL check("dtransform.H5Pget_data_transform_f",  error, total_error)
  CALL VerifyString("dtransform.H5Pget_data_transform_f", c_to_f(1:15), ptrgetTest_big(1:15), total_error)
  CALL VERIFY("dtransform.H5Pget_data_transform_f", size, 15, total_error)

END SUBROUTINE dtransform

