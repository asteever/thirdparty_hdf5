!
! The following example code shows how to create groups 
! using absolute and relative names. It creates three groups:
! the first two groups are created using the file identifier and 
! the group absolute names, and the third group is created using 
! a group identifier and the name relative to the specified group.
!


     PROGRAM GRPSEXAMPLE

     USE HDF5 ! This module contains all necessary modules 
        
     IMPLICIT NONE

     CHARACTER(LEN=10), PARAMETER :: filename = "groupsf.h5" ! File name
     CHARACTER(LEN=8),  PARAMETER :: groupname1 = "/MyGroup" ! Group name
     CHARACTER(LEN=16), PARAMETER :: groupname2 = "/MyGroup/Group_A" 
                                                             ! Group name
     CHARACTER(LEN=7),  PARAMETER :: groupname3 = "Group_B"  ! Group name

     INTEGER(HID_T) :: file_id       ! File identifier 
     INTEGER(HID_T) :: group1_id, group2_id, group3_id ! Group identifiers     

     INTEGER     ::   error ! Error flag
     !
     ! Initialize FORTRAN interface.
     !
     CALL h5init_fortran_f(error) 
     
     !
     ! Create a new file using default properties.
     ! 
     CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)

     !
     ! Create group "MyGroup" in the root group using absolute name.
     !
     CALL h5gcreate_f(file_id, groupname1, group1_id, error)

     !
     ! Create group "Group_A" in group "MyGroup" using absolute name.
     !
     CALL h5gcreate_f(file_id, groupname2, group2_id, error)

     !
     ! Create group "Group_B" in group "MyGroup" using relative name.
     !
     CALL h5gcreate_f(group1_id, groupname3, group3_id, error)
     
     !
     ! Close the groups.
     !
     CALL h5gclose_f(group1_id, error)
     CALL h5gclose_f(group2_id, error)
     CALL h5gclose_f(group3_id, error)

     !
     ! Terminate access to the file.
     !
     CALL h5fclose_f(file_id, error)

     !
     ! Close FORTRAN interface.
     !
     CALL h5close_fortran_f(error) 

     END PROGRAM GRPSEXAMPLE 
