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

! *****************************************
! ***        H 5 P   T E S T S
! *****************************************

!/*-------------------------------------------------------------------------
! * Function:	test_create
! *
! * Purpose:	Tests H5Pset_fill_value_f and H5Pget_fill_value_f
! *
! * Return:	Success:	0
! *
! *		Failure:	number of errors
! *
! * Programmer:	M.S. Breitenfeld
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
    TYPE comp_datatype
       REAL*4 :: a
       INTEGER :: x
       REAL*8 :: y
       CHARACTER(LEN=1) :: z
    END TYPE comp_datatype

    TYPE(comp_datatype), TARGET :: rd_c, fill_ctype

    INTEGER(SIZE_T) :: type_sizei  ! Size of the integer datatype 
    INTEGER(SIZE_T) :: type_sizer  ! Size of the real datatype 
    INTEGER(SIZE_T) :: type_sized  ! Size of the double datatype 
    INTEGER(SIZE_T) :: type_sizec  ! Size of the double datatype
    INTEGER(SIZE_T) :: sizeof_compound ! total size of compound
    INTEGER :: error

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
    CALL h5tget_size_f(H5T_NATIVE_INTEGER, type_sizei, error)
    CALL check("h5tget_size_f", error, total_error)
    
    CALL h5tget_size_f(H5T_NATIVE_REAL, type_sizer, error)
    CALL check("h5tget_size_f", error, total_error)
    
    CALL h5tget_size_f(H5T_NATIVE_DOUBLE, type_sized, error)
    CALL check("h5tget_size_f", error, total_error)

    CALL h5tget_size_f(H5T_NATIVE_CHARACTER, type_sizec, error)
    CALL check("h5tget_size_f", error, total_error)
    
    sizeof_compound =  INT(type_sizei + type_sizer + type_sized + type_sizec, size_t)

    CALL h5tcreate_f(H5T_COMPOUND_F, sizeof_compound, comp_type_id, error)
    CALL check("h5tcreate_f", error, total_error)

    CALL h5tinsert_f(comp_type_id, "a", 0, H5T_NATIVE_REAL, error)
    CALL check("h5tinsert_f", error, total_error)
    CALL h5tinsert_f(comp_type_id, "x", type_sizer, H5T_NATIVE_INTEGER, error)
    CALL check("h5tinsert_f", error, total_error)
    CALL h5tinsert_f(comp_type_id, "y", type_sizei+type_sizer, H5T_NATIVE_DOUBLE, error)
    CALL check("h5tinsert_f", error, total_error)
    CALL h5tinsert_f(comp_type_id, "z", type_sizei+type_sizer+type_sized, H5T_NATIVE_CHARACTER, error)
    CALL check("h5tinsert_f", error, total_error)

    CALL H5Pset_alloc_time_f(dcpl, H5D_ALLOC_TIME_LATE_F,error)
    CALL check("H5Pset_alloc_time_f",error, total_error)

    CALL H5Pset_fill_time_f(dcpl, H5D_FILL_TIME_ALLOC_F, error)
    CALL check("H5Pset_fill_time_f",error, total_error)
  
  ! /* Compound datatype test */
    f_ptr = C_LOC(fill_ctype)

    CALL H5Pget_fill_value_f(dcpl, comp_type_id, f_ptr, error)
    CALL check("H5Pget_fill_value_f",error, total_error)

    fill_ctype%y = 4444
    fill_ctype%z = '4'
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

    IF( rd_c%a .NE. 0.           .OR. &
        rd_c%y .NE. fill_ctype%y .OR. &
        rd_c%x .NE. 0.           .OR. &
        rd_c%z .NE. fill_ctype%z )THEN

       PRINT*,"***ERROR: Got wrong fill value"
       total_error = total_error + 1

    ENDIF

    CALL h5dclose_f(dset9, error)
    CALL check("h5dclose_f", error, total_error)

    CALL H5Pclose_f(dcpl, error)
    CALL check("H5Pclose_f", error, total_error)

    CALL h5fclose_f(file,error)
    CALL check("h5fclose_f", error, total_error)

  END SUBROUTINE test_create



! *****************************************
! ***        H 5 T   T E S T S
! *****************************************

!/****************************************************************
!**
!**  test_array_compound_atomic(): Test basic array datatype code.
!**  Tests 1-D array of compound datatypes (with no array fields)
!**
!****************************************************************/

  SUBROUTINE test_array_compound_atomic(total_error)
    
    USE HDF5 
    USE ISO_C_BINDING
    IMPLICIT NONE
    
    INTEGER, INTENT(INOUT) :: total_error
    ! /* 1-D dataset WITH fixed dimensions */
    CHARACTER(LEN=6), PARAMETER :: SPACE1_NAME = "Space1"
    INTEGER, PARAMETER :: SPACE1_RANK = 1
    INTEGER, PARAMETER :: SPACE1_DIM1 = 4
    ! /* 1-D array datatype */
    INTEGER, PARAMETER :: ARRAY1_RANK= 1
    INTEGER, PARAMETER :: ARRAY1_DIM1= 4
    CHARACTER(LEN=10), PARAMETER :: FILENAME = "tarray1.h5"

    TYPE s1_t
       INTEGER(c_int) :: i
       REAL(c_float) :: f
    END TYPE s1_t
    TYPE(s1_t), DIMENSION(:,:), ALLOCATABLE, TARGET :: wdata ! /* Information to write */
    TYPE(s1_t), DIMENSION(:,:), ALLOCATABLE, TARGET :: rdata ! /* Information read in */
    INTEGER(hid_t)	:: fid1	!/* HDF5 File IDs		*/
    INTEGER(hid_t) :: dataset	!/* Dataset ID			*/
    INTEGER(hid_t)	:: sid1       !/* Dataspace ID			*/
    INTEGER(hid_t)	:: tid1       !/* Array Datatype ID			*/
    INTEGER(hid_t)	:: tid2       !/* Compound Datatype ID			*/
    
    INTEGER(HSIZE_T), DIMENSION(1) :: sdims1 = (/SPACE1_DIM1/)
    INTEGER(HSIZE_T), DIMENSION(1) :: tdims1=(/ARRAY1_DIM1/)
    INTEGER :: ndims ! /* Array rank for reading */
    INTEGER(HSIZE_T), ALLOCATABLE, DIMENSION(:) :: rdims1 !/* Array dimensions for reading */
    INTEGER(HSIZE_T), ALLOCATABLE, DIMENSION(:) :: rdims !/* Array dimensions for reading */
    INTEGER :: nmemb !/* Number of compound members */
    CHARACTER(LEN=20) :: mname !/* Name of compound field */
    INTEGER(size_t) :: off   !/* Offset of compound field */
    INTEGER(hid_t) :: mtid   !/* Datatype ID for field */
    INTEGER :: i,j      ! /* counting variables */ 
    INTEGER(SIZE_T) :: type_sizei  ! Size of the integer datatype 
    INTEGER(SIZE_T) :: type_sizer  ! Size of the real datatype
    INTEGER(SIZE_T) :: sizeof_compound ! total size of compound
    INTEGER :: error    ! /* Generic RETURN value */
    INTEGER(SIZE_T)     ::   offset     ! Member's offset
    INTEGER :: namelen
    LOGICAL :: flag

    TYPE(C_PTR) :: f_ptr ! Needed to pass the pointer, for g95 compiler to work

    ALLOCATE( wdata(1:SPACE1_DIM1,1:ARRAY1_DIM1) )
    ALLOCATE( rdata(1:SPACE1_DIM1,1:ARRAY1_DIM1) )
      
    !/* Initialize array data to write */
    DO i = 1, SPACE1_DIM1
       DO j = 1, ARRAY1_DIM1
          wdata(i,j)%i = i * 10 + j
          wdata(i,j)%f = i * 2.5 + j
       ENDDO
    ENDDO

    !/* Create file */
    CALL h5fcreate_f(FILENAME,H5F_ACC_TRUNC_F,fid1,error)
    CALL check("h5fcreate_f", error, total_error)    


    !/* Create dataspace for datasets */
    CALL h5screate_simple_f(SPACE1_RANK, sdims1, sid1, error)
    CALL check("h5screate_simple_f", error, total_error)


    !/* Create a compound datatype to refer to */
    CALL h5tget_size_f(H5T_NATIVE_INTEGER, type_sizei, error)
    CALL check("h5tget_size_f", error, total_error)

    CALL h5tget_size_f(H5T_NATIVE_REAL, type_sizer, error)
    CALL check("h5tget_size_f", error, total_error)

    sizeof_compound =  INT(type_sizei + type_sizer, size_t)

    CALL h5tcreate_f(H5T_COMPOUND_F, sizeof_compound, tid2, error)
    CALL check("h5tcreate_f", error, total_error)

    !/* Insert integer field */  
    offset = 0
    CALL h5tinsert_f(tid2, "i", offset, H5T_NATIVE_INTEGER, error)
    CALL check("h5tinsert_f", error, total_error)
     
    !/* Insert float field */

    offset = offset + type_sizei
    CALL h5tinsert_f(tid2, "f", offset, H5T_NATIVE_REAL, error)
    CALL check("h5tinsert_f", error, total_error)


    ! /* Create an array datatype to refer to */
    CALL h5tarray_create_f(tid2, ARRAY1_RANK, tdims1, tid1, error)
    CALL check("h5tarray_create_f", error, total_error)

    !/* Close compound datatype */
    CALL h5tclose_f(tid2,error)
    CALL check("h5tclose_f", error, total_error)


    !/* Create a dataset */
    CALL h5dcreate_f(fid1,"Dataset1",tid1, sid1, dataset,error)
    CALL check("h5dcreate_f", error, total_error)

    !/* Write dataset to disk */

    
    ALLOCATE(rdims(1:2)) ! dummy not needed

    f_ptr = C_LOC(wdata)
    CALL h5dwrite_f(dataset, tid1, f_ptr, rdims, error )
    CALL check("h5dwrite_f", error, total_error)

    !/* Close Dataset */ 
    CALL h5dclose_f(dataset, error)
    CALL check("h5dclose_f", error, total_error)

    !/* Close datatype */
    CALL h5tclose_f(tid1,error)
    CALL check("h5tclose_f", error, total_error)

    !/* Close disk dataspace */
    CALL h5sclose_f(sid1,error)
    CALL check("h5sclose_f", error, total_error)


    !/* Close file */
    CALL h5fclose_f(fid1,error)
    CALL check("h5fclose_f", error, total_error)


    !/* Re-open file */
    CALL h5fopen_f (FILENAME, H5F_ACC_RDONLY_F, fid1, error)
    CALL check("h5fopen_f", error, total_error)

    !/* Open the dataset */ 
    CALL h5dopen_f(fid1, "Dataset1", dataset, error)
    CALL check("h5dopen_f", error, total_error)


    !/* Get the datatype */    
    CALL h5dget_type_f(dataset, tid1, error)
    CALL check("h5dget_type_f", error, total_error)

    !/* Check the array rank */
    CALL h5tget_array_ndims_f(tid1, ndims, error)
    CALL check("h5tget_array_ndims_f", error, total_error)
    CALL VERIFY("h5tget_array_ndims_f",ndims, ARRAY1_RANK, total_error)

    !/* Get the array dimensions */
    ALLOCATE(rdims1(1:ndims))
    CALL h5tget_array_dims_f(tid1, rdims1, error)
    CALL check("h5tget_array_dims_f", error, total_error)


    !/* Check the array dimensions */
    DO i = 1, ndims
       CALL VERIFY("h5tget_array_dims_f", INT(rdims1(i)), INT(tdims1(i)), total_error)
    ENDDO

    !/* Get the compound datatype */
    CALL h5tget_super_f(tid1, tid2, error)
    CALL check("h5tget_super_f", error, total_error)

    !/* Check the number of members */
    CALL h5tget_nmembers_f(tid2, nmemb, error)
    CALL check("h5tget_nmembers_f", error, total_error)
    CALL VERIFY("h5tget_nmembers_f", nmemb, 2, total_error)

    !/* Check the 1st field's name */
    CALL H5Tget_member_name_f(tid2, 0, mname, namelen,error)
    CALL check("H5Tget_member_name_f", error, total_error)
    CALL verifystring("H5Tget_member_name_f",mname(1:namelen),"i", total_error)

    ! /* Check the 1st field's offset */
    CALL H5Tget_member_offset_f(tid2, 0, off, error)
    CALL check("H5Tget_member_offset_f", error, total_error)
    CALL VERIFY("H5Tget_member_offset_f",INT(off),0, total_error)    

    !/* Check the 1st field's datatype */
    CALL H5Tget_member_type_f(tid2, 0, mtid, error)
    CALL check("H5Tget_member_type_f", error, total_error)

    CALL H5Tequal_f(mtid, H5T_NATIVE_INTEGER, flag, error)
    CALL check("H5Tequal_f", error, total_error) 
    CALL VerifyLogical("H5Tequal_f", flag, .TRUE., total_error)

    CALL h5tclose_f(mtid,error)
    CALL check("h5tclose_f", error, total_error)

    !/* Check the 2nd field's name */
    CALL H5Tget_member_name_f(tid2, 1, mname, namelen,error)
    CALL check("H5Tget_member_name_f", error, total_error)
    CALL verifystring("H5Tget_member_name_f",mname(1:namelen),"f", total_error)

    ! /* Check the 2nd field's offset */
    CALL H5Tget_member_offset_f(tid2, 1, off, error)
    CALL check("H5Tget_member_offset_f", error, total_error)
    CALL VERIFY("H5Tget_member_offset_f",INT(off),type_sizei, total_error)    

    !/* Check the 2nd field's datatype */
    CALL H5Tget_member_type_f(tid2, 1, mtid, error)
    CALL check("H5Tget_member_type_f", error, total_error)

    CALL H5Tequal_f(mtid, H5T_NATIVE_REAL, flag, error)
    CALL check("H5Tequal_f", error, total_error) 
    CALL VerifyLogical("H5Tequal_f", flag, .TRUE., total_error)

    CALL h5tclose_f(mtid,error)
    CALL check("h5tclose_f", error, total_error)

    ! /* Close Compound Datatype */
    CALL h5tclose_f(tid2, error)
    CALL check("h5tclose_f", error, total_error)

    !/* Read dataset from disk */

    f_ptr = C_LOC(rdata)
    CALL H5Dread_f(dataset, tid1, f_ptr, rdims1, error)
    CALL check("H5Dread_f", error, total_error)

    !/* Compare data read in */
    DO i = 1, SPACE1_DIM1
       DO j = 1, ARRAY1_DIM1
          IF(wdata(i,j)%i.NE.rdata(i,j)%i)THEN
             PRINT*, 'ERROR: Wrong integer data is read back by H5Dread_f '
             total_error = total_error + 1
          ENDIF
          IF(wdata(i,j)%f.NE.rdata(i,j)%f)THEN
             PRINT*, 'ERROR: Wrong real data is read back by H5Dread_f '
             total_error = total_error + 1
          ENDIF
       ENDDO
    ENDDO


    !/* Close Datatype */
    CALL h5tclose_f(tid1,error)
    CALL check("h5tclose_f", error, total_error)

    !/* Close Dataset */
    CALL h5dclose_f(dataset, error)
    CALL check("h5dclose_f", error, total_error)


    !/* Close file */
    CALL h5fclose_f(fid1,error)
    CALL check("h5fclose_f", error, total_error)

  END SUBROUTINE test_array_compound_atomic

!/****************************************************************
!**
!**  test_array_compound_array(): Test basic array datatype code.
!**      Tests 1-D array of compound datatypes (with array fields)
!**
!****************************************************************/

  SUBROUTINE test_array_compound_array(total_error)
    
    USE HDF5 
    USE ISO_C_BINDING
    IMPLICIT NONE
    
    INTEGER, INTENT(INOUT) :: total_error

    ! /* 1-D array datatype */
    INTEGER, PARAMETER :: ARRAY1_RANK= 1
    INTEGER, PARAMETER :: ARRAY1_DIM1= 4
    INTEGER, PARAMETER :: ARRAY2_DIM1= 5

    INTEGER, PARAMETER :: SPACE1_RANK = 1
    INTEGER, PARAMETER :: SPACE1_DIM1 = 4
    CHARACTER(LEN=10), PARAMETER :: FILENAME = "tarray2.h5"

    TYPE st_t_struct ! /* Typedef for compound datatype */
       INTEGER :: i
       REAL, DIMENSION(1:ARRAY2_DIM1) :: f
       CHARACTER(LEN=2), DIMENSION(1:ARRAY2_DIM1) :: c
    END TYPE st_t_struct
    ! /* Information to write */
    TYPE(st_t_struct), DIMENSION(1:SPACE1_DIM1,1:ARRAY1_DIM1), TARGET :: wdata
    ! /* Information read in */
    TYPE(st_t_struct), DIMENSION(1:SPACE1_DIM1,1:ARRAY1_DIM1), TARGET :: rdata


    INTEGER(hid_t) :: fid1		! /* HDF5 File IDs		*/
    INTEGER(hid_t) :: dataset	! /* Dataset ID			*/
    integer(hid_t) :: sid1      ! /* Dataspace ID			*/
    integer(hid_t) :: tid1      ! /* Array Datatype ID	*/
    integer(hid_t) :: tid2      ! /* Compound Datatype ID	*/
    integer(hid_t) :: tid3      ! /* Nested Array Datatype ID	*/
    integer(hid_t) :: tid4      ! /* Nested Array Datatype ID	*/
    INTEGER(HID_T) :: dt5_id      ! Memory datatype identifier 

    INTEGER(HSIZE_T), DIMENSION(1) :: sdims1 = (/SPACE1_DIM1/)
    INTEGER(HSIZE_T), DIMENSION(1) :: tdims1=(/ARRAY2_DIM1/)

    INTEGER  ndims      !/* Array rank for reading */

    INTEGER(HSIZE_T), ALLOCATABLE, DIMENSION(:) :: rdims1 !/* Array dimensions for reading */
    INTEGER(HSIZE_T), ALLOCATABLE, DIMENSION(:) :: rdims !/* Array dimensions for reading */

    INTEGER :: nmemb !/* Number of compound members */
    CHARACTER(LEN=20) :: mname !/* Name of compound field */
    INTEGER(size_t) :: off   !/* Offset of compound field */
    INTEGER(size_t) :: offset   !/* Offset of compound field */
    INTEGER(hid_t) :: mtid   !/* Datatype ID for field */ 
    INTEGER(hid_t) :: mtid2   !/* Datatype ID for field */ 
    INTEGER(SIZE_T) :: type_sizei  ! Size of the integer datatype 
    INTEGER(SIZE_T) :: type_sizer  ! Size of the real datatype 
    INTEGER(SIZE_T) :: type_sizec  ! Size of the character datatype
    INTEGER(SIZE_T) :: sizeof_compound ! total size of compound

    INTEGER :: mclass     ! /* Datatype class for field */
    INTEGER :: i,j,k      !/* counting variables */

    INTEGER :: error
    CHARACTER(LEN=2) :: ichr2
    INTEGER(SIZE_T) :: sizechar
    INTEGER :: namelen
    LOGICAL :: flag

    TYPE(c_ptr) :: f_ptr

    ! /* Initialize array data to write */
    DO i = 1, SPACE1_DIM1
       DO j = 1, array1_DIM1
          wdata(i,j)%i = i*10+j
          DO k = 1, ARRAY2_DIM1
             wdata(i,j)%f(k) = 10*i+j+.5
             WRITE(ichr2,'(I2.2)') k
             wdata(i,j)%c(k) = ichr2
          ENDDO
       ENDDO
    ENDDO

    ! /* Create file */
    CALL h5fcreate_f(FILENAME,H5F_ACC_TRUNC_F,fid1,error)
    CALL check("h5fcreate_f", error, total_error)   


    ! /* Create dataspace for datasets */
    CALL h5screate_simple_f(SPACE1_RANK, sdims1, sid1, error)
    CALL check("h5screate_simple_f", error, total_error)

    ! /* Create a compound datatype to refer to */
    !
    ! First calculate total size by calculating sizes of each member
    !
    CALL h5tcopy_f(H5T_NATIVE_CHARACTER, dt5_id, error)
    CALL check("h5tcopy_f", error, total_error)
    sizechar = 2
    CALL h5tset_size_f(dt5_id, sizechar, error)
    CALL check("h5tset_size_f", error, total_error)
    CALL h5tget_size_f(dt5_id, type_sizec, error)
    CALL check("h5tget_size_f", error, total_error)

    CALL h5tget_size_f(H5T_NATIVE_INTEGER, type_sizei, error)
    CALL check("h5tget_size_f", error, total_error)

    CALL h5tget_size_f(H5T_NATIVE_REAL, type_sizer, error)
    CALL check("h5tget_size_f", error, total_error)

    sizeof_compound =  INT(type_sizei + type_sizer*ARRAY2_DIM1 + type_sizec*ARRAY2_DIM1, size_t)

    CALL h5tcreate_f(H5T_COMPOUND_F, sizeof_compound, tid2, error)
    CALL check("h5tcreate_f", error, total_error)

    !/* Insert integer field */  
    offset = 0
    CALL h5tinsert_f(tid2, "i", offset, H5T_NATIVE_INTEGER, error)
    CALL check("h5tinsert_f", error, total_error)


    !/* Create an array of floats datatype */
    CALL h5tarray_create_f(H5T_NATIVE_REAL, ARRAY1_RANK, tdims1, tid3, error)
    CALL check("h5tarray_create_f", error, total_error)

    !/* Insert float array field */

    offset = offset + type_sizei
    CALL h5tinsert_f(tid2, "f", offset, tid3, error)
    CALL check("h5tinsert_f", error, total_error)

    !/* Create an array of character datatype */
    CALL h5tarray_create_f(H5T_NATIVE_CHARACTER, ARRAY1_RANK, tdims1, tid4, error)
    CALL check("h5tarray_create_f", error, total_error)

    !/* Insert character array field */

    offset = offset + type_sizer*ARRAY2_DIM1
    CALL h5tinsert_f(tid2, "c", offset, tid4, error)
    CALL check("h5tinsert2_f", error, total_error)

    ! /* Close array of floats field datatype */
    CALL h5tclose_f(tid3,error)
    CALL check("h5tclose_f", error, total_error)

    CALL h5tclose_f(tid4,error)
    CALL check("h5tclose_f", error, total_error)

    !/* Create an array datatype to refer to */
    CALL h5tarray_create_f(tid2, ARRAY1_RANK, tdims1, tid1, error)
    CALL check("h5tarray_create_f", error, total_error)


    !/* Close compound datatype */
    CALL h5tclose_f(tid2,error)
    CALL check("h5tclose_f", error, total_error)

    ! /* Create a dataset */
    CALL h5dcreate_f(fid1,"Dataset1",tid1, sid1, dataset,error)
    CALL check("h5dcreate_f", error, total_error)


    !/* Write dataset to disk */

    ALLOCATE(rdims(1:2)) ! dummy not needed

    f_ptr = C_LOC(wdata)
    CALL h5dwrite_f(dataset, tid1, f_ptr, rdims, error )
    CALL check("h5dwrite_f", error, total_error)

    !/* Close Dataset */
    CALL h5dclose_f(dataset, error)
    CALL check("h5dclose_f", error, total_error)

    ! /* Close datatype */
    CALL h5tclose_f(tid1,error)
    CALL check("h5tclose_f", error, total_error)

    !/* Close disk dataspace */
    CALL h5sclose_f(sid1,error)
    CALL check("h5sclose_f", error, total_error)

    !/* Close file */
    CALL h5fclose_f(fid1,error)
    CALL check("h5fclose_f", error, total_error)

    ! /* Re-open file */
    CALL h5fopen_f (FILENAME, H5F_ACC_RDONLY_F, fid1, error)
    CALL check("h5fopen_f", error, total_error)

    !/* Open the dataset */

    CALL h5dopen_f(fid1, "Dataset1", dataset, error)
    CALL check("h5dopen_f", error, total_error)
    
    ! /* Get the datatype */
    CALL h5dget_type_f(dataset, tid1, error)
    CALL check("h5dget_type_f", error, total_error)

    ! /* Check the array rank */
    CALL h5tget_array_ndims_f(tid1, ndims, error)
    CALL check("h5tget_array_ndims_f", error, total_error)
    CALL VERIFY("h5tget_array_ndims_f",ndims, ARRAY1_RANK, total_error)


    !/* Get the array dimensions */
    ALLOCATE(rdims1(1:ndims))
    CALL h5tget_array_dims_f(tid1, rdims1, error)
    CALL check("h5tget_array_dims_f", error, total_error)

    ! /* Check the array dimensions */
    DO i = 1, ndims
       CALL VERIFY("h5tget_array_dims_f", INT(rdims1(i)), INT(tdims1(i)), total_error)
    ENDDO

    ! /* Get the compound datatype */
    CALL h5tget_super_f(tid1, tid2, error)
    CALL check("h5tget_super_f", error, total_error)

    ! /* Check the number of members */
    CALL h5tget_nmembers_f(tid2, nmemb, error)
    CALL check("h5tget_nmembers_f", error, total_error)
    CALL VERIFY("h5tget_nmembers_f", nmemb, 3, total_error)

    ! /* Check the 1st field's name */
    CALL H5Tget_member_name_f(tid2, 0, mname, namelen,error)
    CALL check("H5Tget_member_name_f", error, total_error)
    CALL verifystring("H5Tget_member_name_f",mname(1:namelen),"i", total_error)

    ! /* Check the 1st field's offset */

    CALL H5Tget_member_offset_f(tid2, 0, off, error)
    CALL check("H5Tget_member_offset_f", error, total_error)
    CALL VERIFY("H5Tget_member_offset_f",INT(off),0, total_error) 

    ! /* Check the 1st field's datatype */
    CALL H5Tget_member_type_f(tid2, 0, mtid, error)
    CALL check("H5Tget_member_type_f", error, total_error)

    CALL H5Tequal_f(mtid, H5T_NATIVE_INTEGER, flag, error)
    CALL check("H5Tequal_f", error, total_error) 
    CALL VerifyLogical("H5Tequal_f", flag, .TRUE., total_error)

    CALL h5tclose_f(mtid,error)
    CALL check("h5tclose_f", error, total_error)

    ! /* Check the 2nd field's name */
    CALL H5Tget_member_name_f(tid2, 1, mname, namelen,error)
    CALL check("H5Tget_member_name_f", error, total_error)
    CALL verifystring("H5Tget_member_name_f",mname(1:namelen),"f", total_error)

    ! /* Check the 2nd field's offset */
    CALL H5Tget_member_offset_f(tid2, 1, off, error)
    CALL check("H5Tget_member_offset_f", error, total_error)
    CALL VERIFY("H5Tget_member_offset_f",INT(off),type_sizei, total_error) 

    ! /* Check the 2nd field's datatype */
    CALL H5Tget_member_type_f(tid2, 1, mtid, error)
    CALL check("H5Tget_member_type_f", error, total_error)

    ! /* Get the 2nd field's class */
    CALL H5Tget_class_f(mtid, mclass, error)
    CALL check("H5Tget_class_f", error, total_error)
    CALL VERIFY("H5Tget_class_f",mclass, H5T_ARRAY_F, total_error)

    ! /* Check the array rank */
    CALL h5tget_array_ndims_f(mtid, ndims, error)
    CALL check("h5tget_array_ndims_f", error, total_error)
    CALL VERIFY("h5tget_array_ndims_f",ndims, ARRAY1_RANK, total_error)

    ! /* Get the array dimensions */
    CALL h5tget_array_dims_f(mtid, rdims1, error)
    CALL check("h5tget_array_dims_f", error, total_error)

    ! /* Check the array dimensions */
    DO i = 1, ndims
       CALL VERIFY("h5tget_array_dims_f", INT(rdims1(i)), INT(tdims1(i)), total_error)
    ENDDO

    ! /* Check the 3rd field's name */
    CALL H5Tget_member_name_f(tid2, 2, mname, namelen,error)
    CALL check("H5Tget_member_name_f", error, total_error)
    CALL verifystring("H5Tget_member_name_f",mname(1:namelen),"c", total_error)

    ! /* Check the 3rd field's offset */
    CALL H5Tget_member_offset_f(tid2, 2, off, error)
    CALL check("H5Tget_member_offset_f", error, total_error)
    CALL VERIFY("H5Tget_member_offset_f",INT(off),type_sizei+type_sizer*ARRAY2_DIM1, total_error) 

    ! /* Check the 3rd field's datatype */
    CALL H5Tget_member_type_f(tid2, 2, mtid2, error)
    CALL check("H5Tget_member_type_f", error, total_error)

    ! /* Get the 3rd field's class */
    CALL H5Tget_class_f(mtid2, mclass, error)
    CALL check("H5Tget_class_f", error, total_error)
    CALL VERIFY("H5Tget_class_f",mclass, H5T_ARRAY_F, total_error)

    ! /* Check the array rank */
    CALL h5tget_array_ndims_f(mtid2, ndims, error)
    CALL check("h5tget_array_ndims_f", error, total_error)
    CALL VERIFY("h5tget_array_ndims_f",ndims, ARRAY1_RANK, total_error)

    ! /* Get the array dimensions */
    CALL h5tget_array_dims_f(mtid2, rdims1, error)
    CALL check("h5tget_array_dims_f", error, total_error)

    ! /* Check the array dimensions */
    DO i = 1, ndims
       CALL VERIFY("h5tget_array_dims_f", INT(rdims1(i)), INT(tdims1(i)), total_error)
    ENDDO

    ! /* Check the nested array's datatype */
    CALL H5Tget_super_f(mtid, tid3, error)
    CALL check("H5Tget_super_f", error, total_error)

    CALL H5Tequal_f(tid3, H5T_NATIVE_REAL, flag, error)
    CALL check("H5Tequal_f", error, total_error) 
    CALL VerifyLogical("H5Tequal_f", flag, .TRUE., total_error)

    ! /* Check the nested array's datatype */
    CALL H5Tget_super_f(mtid2, tid3, error)
    CALL check("H5Tget_super_f", error, total_error)

    CALL H5Tequal_f(tid3, H5T_NATIVE_CHARACTER, flag, error)
    CALL check("H5Tequal_f", error, total_error) 
    CALL VerifyLogical("H5Tequal_f", flag, .TRUE., total_error)

    ! /* Close the array's base type datatype */
    CALL h5tclose_f(tid3, error)
    CALL check("h5tclose_f", error, total_error)

    ! /* Close the member datatype */
    CALL h5tclose_f(mtid,error)
    CALL check("h5tclose_f", error, total_error)

    ! /* Close the member datatype */
    CALL h5tclose_f(mtid2,error)
    CALL check("h5tclose_f", error, total_error)

    ! /* Close Compound Datatype */
    CALL h5tclose_f(tid2,error)
    CALL check("h5tclose_f", error, total_error)

    ! /* READ dataset from disk */
    
    f_ptr = c_null_ptr
    f_ptr = C_LOC(rdata)
    CALL H5Dread_f(dataset, tid1, f_ptr, rdims1, error)
    CALL check("H5Dread_f", error, total_error)

    ! /* Compare data read in */
    DO i = 1, SPACE1_DIM1
       DO j = 1, ARRAY1_DIM1
          PRINT*,wdata(i,j)%i,rdata(i,j)%i
          IF(wdata(i,j)%i.NE.rdata(i,j)%i)THEN
             PRINT*, 'ERROR: Wrong integer data is read back by H5Dread_f '
             total_error = total_error + 1
          ENDIF
          DO k = 1, ARRAY2_DIM1
             PRINT*,wdata(i,j)%f(k), rdata(i,j)%f(k)
             IF(wdata(i,j)%f(k).NE.rdata(i,j)%f(k))THEN
                PRINT*, 'ERROR: Wrong real array data is read back by H5Dread_f '
                total_error = total_error + 1
             ENDIF
             IF(wdata(i,j)%c(k).NE.rdata(i,j)%c(k))THEN
                PRINT*, 'ERROR: Wrong character array data is read back by H5Dread_f '
                total_error = total_error + 1
             ENDIF
          ENDDO
       ENDDO
    ENDDO
    stop

    ! /* Close Datatype */
    CALL h5tclose_f(tid1,error)
    CALL check("h5tclose_f", error, total_error)

    ! /* Close Dataset */
    CALL h5dclose_f(dataset, error)
    CALL check("h5dclose_f", error, total_error)

    ! /* Close file */
    CALL h5fclose_f(fid1,error)
    CALL check("h5fclose_f", error, total_error)

  END SUBROUTINE test_array_compound_array

!/****************************************************************
!**
!**  test_array_bkg(): Test basic array datatype code.
!**      Tests reading compound datatype with array fields and
!**          writing partial fields.
!**
!****************************************************************/

  SUBROUTINE test_array_bkg(total_error)
    
    USE HDF5 
    USE ISO_C_BINDING
    IMPLICIT NONE
    
    INTEGER, INTENT(INOUT) :: total_error

    INTEGER, PARAMETER :: LENGTH = 5
    INTEGER, PARAMETER :: ALEN = 10
    INTEGER, PARAMETER :: RANK = 1
    INTEGER, PARAMETER :: NMAX = 100
    CHARACTER(LEN=17), PARAMETER :: FIELDNAME = "ArrayofStructures"

    INTEGER(hid_t) :: fid, array_dt
    INTEGER(hid_t) :: space
    INTEGER(hid_t) :: type
    INTEGER(hid_t) :: dataset

    INTEGER(hsize_t), DIMENSION(1:1) :: dim =(/LENGTH/)
    INTEGER(hsize_t), DIMENSION(1:1) :: dima =(/ALEN/)

    INTEGER :: i, j
    INTEGER, DIMENSION(1:3) :: ndims = (/1,1,1/)


    TYPE CmpField_struct
       INTEGER, DIMENSION(1:ALEN) :: a
       REAL*4, DIMENSION(1:ALEN) :: b
       REAL*8, DIMENSION(1:ALEN) :: c
    ENDTYPE CmpField_struct

    TYPE(CmpField_struct), DIMENSION(1:LENGTH), TARGET :: cf
    TYPE(CmpField_struct), DIMENSION(1:LENGTH), TARGET :: cfr
    
    TYPE CmpDTSinfo_struct
       INTEGER :: nsubfields
       CHARACTER(LEN=5), DIMENSION(1:nmax) :: name
       INTEGER(size_t), DIMENSION(1:nmax) :: offset
       INTEGER(hid_t), DIMENSION(1:nmax) :: datatype
    END TYPE CmpDTSinfo_struct

    TYPE(CmpDTSinfo_struct) :: dtsinfo

    TYPE fld_t_struct
       REAL*4, DIMENSION(1:ALEN) :: b
    END TYPE fld_t_struct
 
    INTEGER(SIZE_T) :: type_sizei  ! Size of the integer datatype 
    INTEGER(SIZE_T) :: type_sizer  ! Size of the real datatype 
    INTEGER(SIZE_T) :: type_sized  ! Size of the double datatype
    INTEGER(SIZE_T) :: sizeof_compound ! total size of compound

    TYPE(fld_t_struct), DIMENSION(1:LENGTH), TARGET :: fld
    TYPE(fld_t_struct), DIMENSION(1:LENGTH), TARGET :: fldr

    CHARACTER(LEN=10), PARAMETER :: FILENAME = "tarray3.h5"

    INTEGER(HSIZE_T), ALLOCATABLE, DIMENSION(:) :: rdims1 !/* Array dimensions for reading */
    INTEGER(HSIZE_T), ALLOCATABLE, DIMENSION(:) :: rdims !/* Array dimensions for reading */

    INTEGER :: error
    TYPE(c_ptr) :: f_ptr


!    /* Initialize the data */
!    /* ------------------- */

    DO i = 1, LENGTH
       DO j = 1, ALEN
          cf(i)%a(j) = 100*(i+1) + j
          cf(i)%b(j) = (100.*(i+1) + 0.01*j)
          cf(i)%c(j) = 100.*(i+1) + 0.02*j
       ENDDO
    ENDDO

    !/* Set the number of data members */
    !/* ------------------------------ */

    dtsinfo%nsubfields = 3

    !/* Initialize the offsets  */
    !/* ----------------------- */
    CALL h5tget_size_f(H5T_NATIVE_INTEGER, type_sizei, error)
    CALL check("h5tget_size_f", error, total_error)

    CALL h5tget_size_f(H5T_NATIVE_REAL, type_sizer, error)
    CALL check("h5tget_size_f", error, total_error)

    CALL h5tget_size_f(H5T_NATIVE_DOUBLE, type_sized, error)
    CALL check("h5tget_size_f", error, total_error)

    dtsinfo%offset(1)   = 0
    
    dtsinfo%offset(2)   = type_sizei*ALEN

    dtsinfo%offset(3)   = dtsinfo%offset(2) + type_sizer*ALEN

    !/* Initialize the data type IDs */
    !/* ---------------------------- */
    dtsinfo%datatype(1) = H5T_NATIVE_INTEGER;
    dtsinfo%datatype(2) = H5T_NATIVE_REAL;
    dtsinfo%datatype(3) = H5T_NATIVE_DOUBLE;


    !/* Initialize the names of data members */
    !/* ------------------------------------ */
     
    dtsinfo%name(1) = "One  "
    dtsinfo%name(2) = "Two  "
    dtsinfo%name(3) = "Three"
       
    !/* Create file */
    !/* ----------- */
    CALL h5fcreate_f(FILENAME,H5F_ACC_TRUNC_F,fid,error)
    CALL check("h5fcreate_f", error, total_error)   


    !/* Create data space */
    !/* ----------------- */
    CALL h5screate_simple_f(RANK, dim, space, error)
    CALL check("h5screate_simple_f", error, total_error)


    !/* Create the memory data type */
    !/* --------------------------- */

    sizeof_compound =  INT( (type_sizei + type_sizer + type_sized)*ALEN, size_t)

    CALL h5tcreate_f(H5T_COMPOUND_F, sizeof_compound, type, error)
    CALL check("h5tcreate_f", error, total_error)

    !/* Add  members to the compound data type */
    !/* -------------------------------------- */

    DO i = 1, dtsinfo%nsubfields
       CALL h5tarray_create_f(dtsinfo%datatype(i), ndims(i), dima, array_dt, error)
       CALL check("h5tarray_create_f", error, total_error)

       CALL h5tinsert_f(type, dtsinfo%name(i), dtsinfo%offset(i), array_dt, error)
       CALL check("h5tinsert_f", error, total_error)

       CALL h5tclose_f(array_dt,error)
       CALL check("h5tclose_f", error, total_error)
    ENDDO

    !/* Create the dataset */
    !/* ------------------ *//
    CALL h5dcreate_f(fid,FIELDNAME,type, space, dataset,error)
    CALL check("h5dcreate_f", error, total_error)

    !/* Write data to the dataset */
    !/* ------------------------- */

    ALLOCATE(rdims(1:2)) ! dummy not needed

    CALL h5dwrite_f(dataset, type, C_LOC(cf), rdims, error )
    CALL check("h5dwrite_f", error, total_error)


    ALLOCATE(rdims1(1:2)) ! dummy not needed
    f_ptr = C_LOC(cfr)
    CALL H5Dread_f(dataset, type, f_ptr, rdims1, error)
    CALL check("H5Dread_f", error, total_error)

    !/* Verify correct data */
    !/* ------------------- */
    DO i = 1, LENGTH
       DO j = 1, ALEN
           IF( cf(i)%a(j) .NE. cfr(i)%a(j) )THEN
             PRINT*, 'ERROR: Wrong integer data is read back by H5Dread_f '
             total_error = total_error + 1
          ENDIF
          IF( cf(i)%b(j) .NE. cfr(i)%b(j) )THEN
             PRINT*, 'ERROR: Wrong real data is read back by H5Dread_f '
             total_error = total_error + 1
          ENDIF
          IF( cf(i)%c(j) .NE. cfr(i)%c(j) )THEN
             PRINT*, 'ERROR: Wrong double data is read back by H5Dread_f '
             total_error = total_error + 1
          ENDIF
       ENDDO
    ENDDO

    !/* Release IDs */
    !/* ----------- */
    CALL h5tclose_f(type,error)
    CALL check("h5tclose_f", error, total_error)
    CALL h5sclose_f(space,error)
    CALL check("h5sclose_f", error, total_error)
    CALL h5dclose_f(dataset, error)
    CALL check("h5dclose_f", error, total_error)
    CALL h5fclose_f(fid,error)
    CALL check("h5fclose_f", error, total_error)

    !/******************************/
    !/* Reopen the file and update */
    !/******************************/

    CALL h5fopen_f (FILENAME, H5F_ACC_RDWR_F, fid, error)
    CALL check("h5fopen_f", error, total_error)

    CALL h5dopen_f(fid, FIELDNAME, dataset, error)
    CALL check("h5dopen_f", error, total_error)

    sizeof_compound =  INT( type_sizer*ALEN, size_t)

    CALL h5tcreate_f(H5T_COMPOUND_F, sizeof_compound, type, error)
    CALL check("h5tcreate_f", error, total_error)

    CALL h5tarray_create_f(H5T_NATIVE_REAL, 1, dima, array_dt, error)
    CALL check("h5tarray_create_f", error, total_error)

    CALL h5tinsert_f(TYPE, "Two", 0, array_dt, error)
    CALL check("h5tinsert_f", error, total_error)

    !/* Initialize the data to overwrite */
    !/* -------------------------------- */
    DO i = 1, LENGTH
       DO j = 1, ALEN
          fld(i)%b(j) = 1.313
          cf(i)%b(j) = fld(i)%b(j)
       ENDDO
    ENDDO


    CALL h5dwrite_f(dataset, TYPE, C_LOC(fld), rdims, error )
    CALL check("h5dwrite_f", error, total_error)


    ! /* Read just the field changed */
    
    CALL H5Dread_f(dataset, TYPE, C_LOC(fldr), rdims1, error)
    CALL check("H5Dread_f", error, total_error)

    DO i = 1, LENGTH
       DO j = 1, ALEN
          IF( fld(i)%b(j) .NE. fldr(i)%b(j) )THEN
             PRINT*, 'ERROR: Wrong real data is read back by H5Dread_f '
             total_error = total_error + 1
          ENDIF
       ENDDO
    ENDDO
    CALL h5tclose_f(TYPE,error)
    CALL check("h5tclose_f", error, total_error)
    CALL h5tclose_f(array_dt,error)
    CALL check("h5tclose_f", error, total_error)

    CALL h5dget_type_f(dataset, type, error)
    CALL check("h5dget_type_f", error, total_error)


    ! /* Read the entire dataset again */

    CALL H5Dread_f(dataset, TYPE, C_LOC(cfr), rdims1, error)
    CALL check("H5Dread_f", error, total_error)


    !/* Verify correct data */
    !/* ------------------- */

    DO i = 1, LENGTH
       DO j = 1, ALEN
           IF( cf(i)%a(j) .NE. cfr(i)%a(j) )THEN
             PRINT*, 'ERROR: Wrong integer data is read back by H5Dread_f '
             total_error = total_error + 1
          ENDIF
          IF( cf(i)%b(j) .NE. cfr(i)%b(j) )THEN
             PRINT*, 'ERROR: Wrong real data is read back by H5Dread_f '
             total_error = total_error + 1
          ENDIF
          IF( cf(i)%c(j) .NE. cfr(i)%c(j) )THEN
             PRINT*, 'ERROR: Wrong double data is read back by H5Dread_f '
             total_error = total_error + 1
          ENDIF
       ENDDO
    ENDDO

    CALL h5dclose_f(dataset, error)
    CALL check("h5dclose_f", error, total_error)

    CALL h5tclose_f(type,error)
    CALL check("h5tclose_f", error, total_error)

    CALL h5fclose_f(fid,error)
    CALL check("h5fclose_f", error, total_error)

!/****************************************************/
!/* Reopen the file and print out all the data again */
!/****************************************************/

    CALL h5fopen_f (FILENAME, H5F_ACC_RDWR_F, fid, error)
    CALL check("h5fopen_f", error, total_error)


    CALL h5dopen_f(fid, FIELDNAME, dataset, error)
    CALL check("h5dopen_f", error, total_error)


    CALL h5dget_type_f(dataset, type, error)
    CALL check("h5dget_type_f", error, total_error)


    !/* Reset the data to read in */
    !/* ------------------------- */

    DO i = 1, LENGTH
       cfr(i)%a(:) = 0
       cfr(i)%b(:) = 0
       cfr(i)%c(:) = 0
    ENDDO

    CALL H5Dread_f(dataset, TYPE, C_LOC(cfr), rdims1, error)
    CALL check("H5Dread_f", error, total_error)

    !/* Verify correct data */
    !/* ------------------- */

    DO i = 1, LENGTH
       DO j = 1, ALEN
           IF( cf(i)%a(j) .NE. cfr(i)%a(j) )THEN
             PRINT*, 'ERROR: Wrong integer data is read back by H5Dread_f '
             total_error = total_error + 1
          ENDIF
          IF( cf(i)%b(j) .NE. cfr(i)%b(j) )THEN
             PRINT*, 'ERROR: Wrong real data is read back by H5Dread_f '
             total_error = total_error + 1
          ENDIF
          IF( cf(i)%c(j) .NE. cfr(i)%c(j) )THEN
             PRINT*, 'ERROR: Wrong double data is read back by H5Dread_f '
             total_error = total_error + 1
          ENDIF
       ENDDO
    ENDDO

    CALL h5dclose_f(dataset, error)
    CALL check("h5dclose_f", error, total_error)

    CALL h5tclose_f(type,error)
    CALL check("h5tclose_f", error, total_error)

    CALL h5fclose_f(fid,error)
    CALL check("h5fclose_f", error, total_error)

  END SUBROUTINE test_array_bkg

