!
! This example shows how to write and read a hyperslab.  
!

     PROGRAM SELECTEXAMPLE

     USE HDF5 ! This module contains all necessary modules 
        
     IMPLICIT NONE

     CHARACTER(LEN=7), PARAMETER :: filename = "sdsf.h5"  ! File name
     CHARACTER(LEN=8), PARAMETER :: dsetname = "IntArray" ! Dataset name

     INTEGER(HID_T) :: file_id       ! File identifier 
     INTEGER(HID_T) :: dset_id       ! Dataset identifier 
     INTEGER(HID_T) :: dataspace     ! Dataspace identifier 
     INTEGER(HID_T) :: memspace      ! memspace identifier 

     INTEGER(HSIZE_T), DIMENSION(3) :: dimsm = (/7,7,3/) ! Dataset dimensions
                                                         ! in memory
     INTEGER(HSIZE_T), DIMENSION(2) :: dims_out ! Buffer to read in dataset 
                                                ! dimesions
     INTEGER(HSIZE_T), DIMENSION(2) :: dimsf = (/5,6/) ! Dataset dimensions.

     INTEGER(HSIZE_T), DIMENSION(2) :: count = (/3,4/) 
                                            ! Size of the hyperslab in the file
     INTEGER(HSIZE_T), DIMENSION(2) :: offset = (/1,2/)
                                            !hyperslab offset in the file 
     INTEGER(HSIZE_T), DIMENSION(3) :: count_out = (/3,4,1/)
                                            !Size of the hyperslab in memory 
     INTEGER(HSIZE_T), DIMENSION(3) :: offset_out = (/3,0,0/)
                                            !hyperslab offset in memory 
     INTEGER, DIMENSION(5,6) :: data ! Data to write
     INTEGER, DIMENSION(7,7,3) :: data_out ! Output buffer
     INTEGER :: dsetrank = 2 ! Dataset rank ( in file )
     INTEGER :: memrank = 3  ! Dataset rank ( in memory )
     INTEGER :: rank 
     INTEGER :: i, j, k 

     INTEGER :: error, error_n  ! Error flags


   !
   ! Write data to the HDF5 file.  
   !

     !
     ! Data initialization. 
     !
     do i = 1, 5
          do j = 1, 6
               data(i,j) = (i-1) + (j-1);
          end do
     end do
     !
     ! 0,  1,  2,  3,  4,  5
     ! 1,  2,  3,  4,  5,  6
     ! 2,  3,  4,  5,  6,  7
     ! 3,  4,  5,  6,  7,  8
     ! 4,  5,  6,  7,  8,  9
     !
     
     !
     ! Initialize FORTRAN predefined datatypes
     !
     CALL h5init_types_f(error) 

     !
     ! Create a new file using default properties.
     ! 
     CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)

     !
     ! Create the data space for the  dataset. 
     !
     CALL h5screate_simple_f(dsetrank, dimsf, dataspace, error)

     !
     ! Create the dataset with default properties.
     !
     CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_INTEGER, dataspace, &
                      dset_id, error)

     !
     ! Write the dataset.
     !
     CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data, error)

     !
     ! Close the dataspace for the dataset.
     !
     CALL h5sclose_f(dataspace, error)

     !
     ! Close the dataset.
     !
     CALL h5dclose_f(dset_id, error)

     !
     ! Close the file.
     !
     CALL h5fclose_f(file_id, error)

  !
  ! This  part of the code reads the hyperslab from the sds.h5 file just 
  ! created, into a 2-dimensional plane of the 3-dimensional dataset.
  !

     !
     ! Initialize data_out array.
     !
     do i = 1, 7
          do j = 1, 7
              do k = 1,3
                  data_out(i,j,k) = 0;
              end do
          end do
     end do

     !
     ! Open the file.
     !
     CALL h5fopen_f (filename, H5F_ACC_RDONLY_F, file_id, error)
       
     !
     ! Open the  dataset.
     !
     CALL h5dopen_f(file_id, dsetname, dset_id, error)

     !
     ! Get dataset's dataspace identifier.
     !
     CALL h5dget_space_f(dset_id, dataspace, error)

     !
     ! Select hyperslab in the dataset.
     !
     CALL h5sselect_hyperslab_f(dataspace, H5S_SELECT_SET_F, &
                                offset, count, error) 
     !
     ! Create memory dataspace.
     !
     CALL h5screate_simple_f(memrank, dimsm, memspace, error)

     !
     ! Select hyperslab in memory.
     !
     CALL h5sselect_hyperslab_f(memspace, H5S_SELECT_SET_F, &
                                offset_out, count_out, error) 

     !
     ! Read data from hyperslab in the file into the hyperslab in 
     ! memory and display.
     !
     CALL H5Dread_f(dset_id, H5T_NATIVE_INTEGER, data_out, error, &
                    memspace, dataspace)
     
     !
     ! Display data_out array
     !
     do i = 1, 7
         print *, (data_out(i,j,1), j = 1,7)
     end do

     ! 0 0 0 0 0 0 0
     ! 0 0 0 0 0 0 0
     ! 0 0 0 0 0 0 0
     ! 3 4 5 6 0 0 0  
     ! 4 5 6 7 0 0 0
     ! 5 6 7 8 0 0 0
     ! 0 0 0 0 0 0 0
     !

     !
     ! Close the dataspace for the dataset.
     !
     CALL h5sclose_f(dataspace, error)

     !
     ! Close the memoryspace.
     !
     CALL h5sclose_f(memspace, error)

     !
     ! Close the dataset.
     !
     CALL h5dclose_f(dset_id, error)

     !
     ! Close the file.
     !
     CALL h5fclose_f(file_id, error)

     !
     ! Close FORTRAN predefined datatypes.
     !
     CALL h5close_types_f(error)

     END PROGRAM SELECTEXAMPLE 
