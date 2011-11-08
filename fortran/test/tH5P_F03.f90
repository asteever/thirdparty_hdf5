!****h* root/fortran/test/tH5P_F03.f90
!
! NAME
!  tH5P_F03.f90
!
! FUNCTION
!  Test FORTRAN HDF5 H5P APIs which are dependent on FORTRAN 2003
!  features. 
!
! COPYRIGHT
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
! USES
!  test_genprop_cls_cb1_mod
!
! CONTAINS SUBROUTINES
!  test_create, test_genprop_class_callback
!
!*****

! *****************************************
! ***        H 5 P   T E S T S
! *****************************************

MODULE test_genprop_cls_cb1_mod

  ! Callback subroutine for test_genprop_class_callback
  ! and the function H5Pcreate_class_f.

  USE HDF5
  USE ISO_C_BINDING
  IMPLICIT NONE
  
  TYPE, bind(C) :: cop_cb_struct_ ! /* Struct for iterations */
    INTEGER :: count
    INTEGER(HID_T) :: id
  END TYPE cop_cb_struct_

CONTAINS
  
  INTEGER FUNCTION test_genprop_cls_cb1_f(list_id, create_data ) bind(C)
    
    USE HDF5
    USE ISO_C_BINDING
    IMPLICIT NONE

    INTEGER(HID_T), INTENT(IN), VALUE :: list_id
    
    TYPE(cop_cb_struct_) :: create_data
    
    create_data%count = create_data%count + 1
    create_data%id = list_id
    
    test_genprop_cls_cb1_f = 0
    
  END FUNCTION test_genprop_cls_cb1_f

END MODULE test_genprop_cls_cb1_mod

!/*-------------------------------------------------------------------------
! * Function:	test_create
! *
! * Purpose:	Tests H5Pset_fill_value_f and H5Pget_fill_value_f
! *
! * Return:	Success:	0
! *
! *		Failure:	number of errors
! *
! * Programmer:	M. Scot Breitenfeld
! *             June 24, 2008
! *
! * Modifications:
! *
! *-------------------------------------------------------------------------
! */

SUBROUTINE test_create(total_error)

  USE HDF5 
  USE ISO_C_BINDING
  IMPLICIT NONE

  INTEGER, INTENT(INOUT) :: total_error
  INTEGER(HID_T) :: fapl

  INTEGER(hid_t) :: file=-1, space=-1, dcpl=-1, comp_type_id=-1
  INTEGER(hid_t) :: dset1=-1, dset2=-1, dset3=-1, dset4=-1, dset5=-1, &
       dset6=-1, dset7=-1, dset8=-1, dset9=-1
  INTEGER(hsize_t), DIMENSION(1:5), PARAMETER :: cur_size = (/2, 8, 8, 4, 2/)
  INTEGER(hsize_t), DIMENSION(1:5), PARAMETER :: ch_size= (/1, 1, 1, 4, 1/)
  CHARACTER(LEN=14) :: filename ='test_create.h5'

  ! /* compound datatype operations */
  TYPE, BIND(C) :: comp_datatype
    REAL :: a
    INTEGER :: x
    DOUBLE PRECISION :: y
    CHARACTER(LEN=1) :: z
  END TYPE comp_datatype

  TYPE(comp_datatype), TARGET :: rd_c, fill_ctype

  INTEGER(SIZE_T) :: type_sizei  ! Size of the integer datatype 
  INTEGER(SIZE_T) :: type_sizer  ! Size of the real datatype 
  INTEGER(SIZE_T) :: type_sized  ! Size of the double datatype 
  INTEGER(SIZE_T) :: type_sizec  ! Size of the double datatype
  INTEGER(SIZE_T) :: sizeof_compound ! total size of compound
  INTEGER :: error
  INTEGER(SIZE_T) :: h5off
  TYPE(C_PTR) :: f_ptr
  
  !/*
  ! * Create a file.
  ! */
  CALL h5fcreate_f(filename,H5F_ACC_TRUNC_F,file,error)
  CALL check("h5fcreate_f", error, total_error)   

  CALL h5screate_simple_f(5, cur_size, space, error, cur_size)
  CALL check("h5screate_simple_f", error, total_error)

  CALL H5Pcreate_f(H5P_DATASET_CREATE_F, dcpl, error)
  CALL check("H5Pcreate_f", error, total_error)

  CALL h5pset_chunk_f(dcpl, 5, ch_size, error)
  CALL check("h5pset_chunk_f",error, total_error)

  ! /* Create a compound datatype */

  CALL h5tcreate_f(H5T_COMPOUND_F, INT(SIZEOF(fill_ctype),size_t), comp_type_id, error)
  CALL check("h5tcreate_f", error, total_error)
  h5off = H5OFFSETOF(C_LOC(fill_ctype), C_LOC(fill_ctype%a))
  CALL h5tinsert_f(comp_type_id, "a", h5off , H5T_NATIVE_REAL, error)
  CALL check("h5tinsert_f", error, total_error)
  CALL h5tinsert_f(comp_type_id, "x", H5OFFSETOF(C_LOC(fill_ctype), C_LOC(fill_ctype%x)), H5T_NATIVE_INTEGER, error)
  CALL check("h5tinsert_f", error, total_error)
  CALL h5tinsert_f(comp_type_id, "y", H5OFFSETOF(C_LOC(fill_ctype), C_LOC(fill_ctype%y)), H5T_NATIVE_DOUBLE, error)
  CALL check("h5tinsert_f", error, total_error)
  CALL h5tinsert_f(comp_type_id, "z", &
       H5OFFSETOF(C_LOC(fill_ctype), C_LOC(fill_ctype%z)), H5T_NATIVE_CHARACTER, error)
  CALL check("h5tinsert_f", error, total_error)


  CALL H5Pset_alloc_time_f(dcpl, H5D_ALLOC_TIME_LATE_F,error)
  CALL check("H5Pset_alloc_time_f",error, total_error)

  CALL H5Pset_fill_time_f(dcpl, H5D_FILL_TIME_ALLOC_F, error)
  CALL check("H5Pset_fill_time_f",error, total_error)

  ! /* Compound datatype test */

  f_ptr = C_LOC(fill_ctype)

  CALL H5Pget_fill_value_f(dcpl, comp_type_id, f_ptr, error)
  CALL check("H5Pget_fill_value_f",error, total_error)

  fill_ctype%y = 4444.
  fill_ctype%z = 'S'
  fill_ctype%a = 5555.
  fill_ctype%x = 55

  f_ptr = C_LOC(fill_ctype)

  CALL H5Pset_fill_value_f(dcpl, comp_type_id, f_ptr, error)
  CALL check("H5Pget_fill_value_f",error, total_error)

  CALL h5dcreate_f(file,"dset9", comp_type_id, space, dset9, error, dcpl_id=dcpl)
  CALL check("h5dcreate_f", error, total_error)

  CALL h5dclose_f(dset9, error)
  CALL check("h5dclose_f", error, total_error)

  CALL h5fclose_f(file,error)
  CALL check("h5fclose_f", error, total_error)

  ! /* Open the file and get the dataset fill value from each dataset */
  CALL H5Pcreate_f(H5P_FILE_ACCESS_F, fapl, error)
  CALL check("H5Pcreate_f",error, total_error)

  CALL H5Pset_libver_bounds_f(fapl, H5F_LIBVER_LATEST_F, H5F_LIBVER_LATEST_F, error)
  CALL check("H5Pset_libver_bounds_f",error, total_error)

  CALL h5fopen_f (FILENAME, H5F_ACC_RDONLY_F, file, error, fapl)
  CALL check("h5fopen_f", error, total_error)

  !/* Compound datatype test */
  CALL h5dopen_f(file, "dset9", dset9, error)
  CALL check("h5dopen_f", error, total_error)

  CALL H5Dget_create_plist_f(dset9, dcpl, error)
  CALL check("H5Dget_create_plist_f", error, total_error)

  f_ptr = C_LOC(rd_c)

  CALL H5Pget_fill_value_f(dcpl, comp_type_id, f_ptr, error)
  CALL check("H5Pget_fill_value_f", error, total_error)

  IF( rd_c%a .NE. fill_ctype%a .OR. &
       rd_c%y .NE. fill_ctype%y .OR. &
       rd_c%x .NE. fill_ctype%x .OR. &
       rd_c%z .NE. fill_ctype%z )THEN

     PRINT*,"***ERROR: Returned wrong fill value"
     total_error = total_error + 1

  ENDIF

  CALL h5dclose_f(dset9, error)
  CALL check("h5dclose_f", error, total_error)

  CALL H5Pclose_f(dcpl, error)
  CALL check("H5Pclose_f", error, total_error)

  CALL h5fclose_f(file,error)
  CALL check("h5fclose_f", error, total_error)

END SUBROUTINE test_create


SUBROUTINE test_genprop_class_callback(total_error)

  !/****************************************************************
  !**
  !**  test_genprop_class_callback(): Test basic generic property list code.
  !**      Tests callbacks for property lists in a generic class.
  !**
  !**  FORTRAN TESTS:
  !**      Tests function H5Pcreate_class_f with callback.
  !**
  !****************************************************************/

  USE HDF5
  USE ISO_C_BINDING
  USE test_genprop_cls_cb1_mod
  IMPLICIT NONE

  INTEGER, INTENT(INOUT) :: total_error

  INTEGER(hid_t) :: cid1 !/* Generic Property class ID */
  INTEGER(hid_t) :: lid1 !/* Generic Property list ID */
  INTEGER(hid_t) :: lid2 !/* 2nd Generic Property list ID */
  INTEGER(size_t) :: nprops !/* Number of properties in class */

  TYPE cb_struct
     INTEGER :: count
     INTEGER(hid_t) :: id
  END TYPE cb_struct

  TYPE(cb_struct), TARGET :: crt_cb_struct, cls_cb_struct

  CHARACTER(LEN=7) :: CLASS1_NAME = "Class 1"
  TYPE(C_FUNPTR) :: f1, f3, f5
  TYPE(C_PTR) :: f2, f4, f6

  CHARACTER(LEN=10) :: PROP1_NAME = "Property 1"
  INTEGER(SIZE_T) :: PROP1_SIZE = 10
  CHARACTER(LEN=10) :: PROP2_NAME = "Property 2"
  INTEGER(SIZE_T) :: PROP2_SIZE = 10
  CHARACTER(LEN=10) :: PROP3_NAME = "Property 3"
  INTEGER(SIZE_T) :: PROP3_SIZE = 10
  CHARACTER(LEN=10) :: PROP4_NAME = "Property 4"
  INTEGER(SIZE_T) :: PROP4_SIZE = 10
  INTEGER :: PROP1_DEF_VALUE = 10
  INTEGER :: PROP2_DEF_VALUE = 10
  INTEGER :: PROP3_DEF_VALUE = 10
  INTEGER :: PROP4_DEF_VALUE = 10

  INTEGER :: error ! /* Generic RETURN value	*/

  f1 = C_FUNLOC(test_genprop_cls_cb1_f)
  f5 = C_FUNLOC(test_genprop_cls_cb1_f)

  f2 = C_LOC(crt_cb_struct)
  f6 = C_LOC(cls_cb_struct)

  !/* Create a new generic class, derived from the root of the class hierarchy */
  CALL h5pcreate_class_f(h5p_ROOT_F,CLASS1_NAME, cid1, error, f1, f2, c_null_funptr, c_null_ptr, f5, f6)
  CALL check("h5pcreate_class_f", error, total_error)

  !/* Insert first property into class (with no callbacks) */
  CALL h5pregister_f(cid1, PROP1_NAME, PROP1_SIZE, PROP1_DEF_VALUE, error)
  CALL check("h5pregister_f", error, total_error)
  !/* Insert second property into class (with no callbacks) */
  CALL h5pregister_f(cid1, PROP2_NAME, PROP2_SIZE, PROP2_DEF_VALUE, error)
  CALL check("h5pregister_f", error, total_error)
  !/* Insert third property into class (with no callbacks) */
  CALL h5pregister_f(cid1, PROP3_NAME, PROP3_SIZE, PROP3_DEF_VALUE, error)
  CALL check("h5pregister_f", error, total_error)

  !/* Insert fourth property into class (with no callbacks) */
  CALL h5pregister_f(cid1, PROP4_NAME, PROP4_SIZE, PROP4_DEF_VALUE, error)
  CALL check("h5pregister_f", error, total_error)

  ! /* Check the number of properties in class */
  CALL h5pget_nprops_f(cid1, nprops, error)
  CALL check("h5pget_nprops_f", error, total_error)
  CALL VERIFY("h5pget_nprops_f", INT(nprops), 4, total_error)

  ! /* Initialize class callback structs */

  crt_cb_struct%count = 0
  crt_cb_struct%id    = -1
  cls_cb_struct%count = 0
  cls_cb_struct%id    = -1

  !/* Create a property list from the class */
  CALL h5pcreate_f(cid1, lid1, error)
  CALL check("h5pcreate_f", error, total_error)

  !/* Verify that the creation callback occurred */
  CALL VERIFY("h5pcreate_f", crt_cb_struct%count, 1, total_error)
  CALL VERIFY("h5pcreate_f", INT(crt_cb_struct%id), INT(lid1), total_error)

  ! /* Check the number of properties in list */
  CALL h5pget_nprops_f(lid1,nprops, error)
  CALL check("h5pget_nprops_f", error, total_error)
  CALL VERIFY("h5pget_nprops_f", INT(nprops), 4, total_error)

  ! /* Create another property list from the class */
  CALL h5pcreate_f(cid1, lid2, error)
  CALL check("h5pcreate_f", error, total_error)

  ! /* Verify that the creation callback occurred */
  CALL VERIFY("h5pcreate_f", crt_cb_struct%count, 2, total_error)
  CALL VERIFY("h5pcreate_f", INT(crt_cb_struct%id), INT(lid2), total_error)

  ! /* Check the number of properties in list */
  CALL h5pget_nprops_f(lid2,nprops, error)
  CALL check("h5pget_nprops_f", error, total_error)
  CALL VERIFY("h5pget_nprops_f", INT(nprops), 4, total_error)

  ! /* Close first list */
  CALL h5pclose_f(lid1, error);
  CALL check("h5pclose_f", error, total_error)

  !/* Verify that the close callback occurred */
  CALL VERIFY("h5pcreate_f", cls_cb_struct%count, 1, total_error)
  CALL VERIFY("h5pcreate_f", INT(cls_cb_struct%id), INT(lid1), total_error)

  !/* Close second list */
  CALL h5pclose_f(lid2, error);
  CALL check("h5pclose_f", error, total_error)

  !/* Verify that the close callback occurred */
  CALL VERIFY("h5pcreate_f", cls_cb_struct%count, 2, total_error)
  CALL VERIFY("h5pcreate_f", INT(cls_cb_struct%id), INT(lid2), total_error)

  !/* Close class */
  CALL h5pclose_class_f(cid1, error)
  CALL check("h5pclose_class_f", error, total_error)

END SUBROUTINE test_genprop_class_callback
