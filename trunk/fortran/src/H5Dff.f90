!
! This file contains Fortran90 interfaces for H5D functions.
! 
      MODULE H5D
        USE H5GLOBAL
        USE H5R

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
! Comment if on T3E
            MODULE PROCEDURE h5dwrite_double_scalar
            MODULE PROCEDURE h5dwrite_double_1
            MODULE PROCEDURE h5dwrite_double_2
            MODULE PROCEDURE h5dwrite_double_3
            MODULE PROCEDURE h5dwrite_double_4
            MODULE PROCEDURE h5dwrite_double_5
            MODULE PROCEDURE h5dwrite_double_6
            MODULE PROCEDURE h5dwrite_double_7
! End comment if on T3E
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
! Comment if on T3E
            MODULE PROCEDURE h5dread_double_scalar
            MODULE PROCEDURE h5dread_double_1
            MODULE PROCEDURE h5dread_double_2
            MODULE PROCEDURE h5dread_double_3
            MODULE PROCEDURE h5dread_double_4
            MODULE PROCEDURE h5dread_double_5
            MODULE PROCEDURE h5dread_double_6
            MODULE PROCEDURE h5dread_double_7
! End comment if on T3E

          END INTERFACE 

        CONTAINS
          
!----------------------------------------------------------------------
! Name:		h5dcreate_f 
!
! Purpose: 	Creates a dataset at the specified location 	
!
! Inputs:  
!		loc_id		- file or group identifier
!		name		- dataset name
!		type_id		- dataset datatype identifier
!		space_id	- dataset dataspace identifier
! Outputs:  
!		dset_id		- dataset identifier
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!		createion_prp	- dataset creation property list identifier
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces were added for 
!			called C functions (it is needed for Windows
!			port).  February 28, 2001 
!
! Comment:		
!----------------------------------------------------------------------
  
          SUBROUTINE h5dcreate_f(loc_id, name, type_id, space_id, dset_id, & 
                                 hdferr, creation_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! File or group identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the dataset 
            INTEGER(HID_T), INTENT(IN) :: type_id  ! Datatype identifier 
            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier 
            INTEGER(HID_T), INTENT(OUT) :: dset_id ! Dataset identifier 
            INTEGER, INTENT(OUT) :: hdferr         ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: creation_prp 
                                                   ! Dataset creation propertly
                                                   ! list identifier
            INTEGER :: creation_prp_default
            INTEGER :: namelen                     ! Name length

!            INTEGER, EXTERNAL :: h5dcreate_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dcreate_c(loc_id, name, namelen, type_id, &
                                           space_id, creation_prp_default, dset_id)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DCREATE_C'::h5dcreate_c
              !DEC$ATTRIBUTES reference :: name
              INTEGER(HID_T), INTENT(IN) :: loc_id
              CHARACTER(LEN=*), INTENT(IN) :: name
              INTEGER :: namelen
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER(HID_T), INTENT(IN) :: space_id
              INTEGER :: creation_prp_default
              INTEGER(HID_T), INTENT(OUT) :: dset_id
              END FUNCTION h5dcreate_c
            END INTERFACE

            creation_prp_default = H5P_DEFAULT_F
            if (present(creation_prp)) creation_prp_default = creation_prp 
            namelen = LEN(name)
            hdferr = h5dcreate_c(loc_id, name, namelen, type_id, space_id, & 
                                 creation_prp_default, dset_id) 
          END SUBROUTINE h5dcreate_f
          
!----------------------------------------------------------------------
! Name:		h5dopen_f 
!
! Purpose: 	Opens an existing dataset.  	
!
! Inputs:  
!		loc_id		- file or group identifier
!		name		- dataset name
! Outputs:  
!		dset_id		- dataset identifier
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE			
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces were added for 
!			called C functions (it is needed for Windows
!			port).  February 28, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5dopen_f(loc_id, name, dset_id, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! File or group identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the dataset 
            INTEGER(HID_T), INTENT(OUT) :: dset_id ! Dataset identifier 
            INTEGER, INTENT(OUT) :: hdferr         ! Error code 
            INTEGER :: namelen                     ! Name length

!            INTEGER, EXTERNAL :: h5dopen_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dopen_c(loc_id, name, namelen, dset_id)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DOPEN_C'::h5dopen_c
              !DEC$ATTRIBUTES reference :: name
              INTEGER(HID_T), INTENT(IN) :: loc_id
              CHARACTER(LEN=*), INTENT(IN) :: name
              INTEGER :: namelen
              INTEGER(HID_T), INTENT(OUT) :: dset_id
              END FUNCTION h5dopen_c
            END INTERFACE

            namelen = LEN(name)
            hdferr = h5dopen_c(loc_id, name, namelen, dset_id) 

          END SUBROUTINE h5dopen_f
          
!----------------------------------------------------------------------
! Name:		h5dclose_f 
!
! Purpose: 	Closes a dataset.  	
!
! Inputs:  
!		dset_id		- dataset identifier
! Outputs:  
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE			
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces were added for 
!			called C functions (it is needed for Windows
!			port).  February 28, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5dclose_f(dset_id, hdferr)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id ! Dataset identifier
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5dclose_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dclose_c(dset_id)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DCLOSE_C'::h5dclose_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              END FUNCTION h5dclose_c
            END INTERFACE

            hdferr = h5dclose_c(dset_id)

          END SUBROUTINE h5dclose_f

!----------------------------------------------------------------------
! Name:		h5dwrite_f 
!
! Purpose: 	Reads raw data from the specified dataset into buf, 
!		converting from file datatype and dataspace to memory 
!		datatype and dataspace.
!
! Inputs:  
!		dset_id		- dataset identifier
!		mem_type_id	- memory type identifier
!		buf		- data buffer to write
!		n		- size of the data buffer 
!     				NOTE: This parameter is used only for writing
!				      object and dataset region references.
! Outputs:  
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
! Comment:		This function is overloaded to write INTEGER,
!			REAL, DOUBLE PRECISION and CHARACTER buffers
!			up to 7 dimensions, and one dimensional buffers
!			of the TYPE(hobj_ref_t_f) and TYPE(hdset_reg_ref_t_f)
!			types.	
!----------------------------------------------------------------------

          SUBROUTINE h5dwrite_reference_obj(dset_id, mem_type_id, buf, n, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN) :: n ! size of the bufffer buf
            TYPE(hobj_ref_t_f), DIMENSION(n), INTENT(IN) :: buf ! Data buffer
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
            INTEGER, ALLOCATABLE, DIMENSION(:) :: ref_buf
            INTEGER :: i,j

!            INTEGER, EXTERNAL :: h5dwrite_ref_obj_c
! MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_ref_obj_c(dset_id, mem_type_id,&
                                                  mem_space_id_default, &
                               file_space_id_default, xfer_prp_default, ref_buf, n)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_REF_OBJ_C'::h5dwrite_ref_obj_c  
              INTEGER(HID_T), INTENT(IN) :: dset_id   
              INTEGER(HID_T), INTENT(IN) :: mem_type_id 
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER, ALLOCATABLE, DIMENSION(:) :: ref_buf
              INTEGER :: n 
              END FUNCTION h5dwrite_ref_obj_c
            END INTERFACE 

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 
            
            allocate(ref_buf(REF_OBJ_BUF_LEN*n), stat=hdferr)
            if (hdferr .NE. 0 ) then
                hdferr = -1
                return
            else
                do j = 1, n
                  do i = 1, REF_OBJ_BUF_LEN  
                   ref_buf(REF_OBJ_BUF_LEN*(j-1) + i ) = buf(j)%ref(i)
                 enddo  
                enddo  
            endif
            hdferr = h5dwrite_ref_obj_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, ref_buf, n)
            deallocate(ref_buf)

          END SUBROUTINE h5dwrite_reference_obj

          SUBROUTINE h5dwrite_reference_dsetreg(dset_id, mem_type_id, buf, n, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            TYPE(hdset_reg_ref_t_f), DIMENSION(:), INTENT(IN) :: buf ! Data buffer
            INTEGER, INTENT(IN) :: n ! size of the bufffer buf  
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
            INTEGER, ALLOCATABLE, DIMENSION(:) :: ref_buf
            INTEGER :: i,j

!            INTEGER, EXTERNAL :: h5dwrite_ref_reg_c
! MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_ref_reg_c(dset_id, mem_type_id,&
                                                  mem_space_id_default, &
                               file_space_id_default, xfer_prp_default, ref_buf, n)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_REF_REG_C'::h5dwrite_ref_reg_c  
              INTEGER(HID_T), INTENT(IN) :: dset_id   
              INTEGER(HID_T), INTENT(IN) :: mem_type_id 
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER, ALLOCATABLE, DIMENSION(:) :: ref_buf
              INTEGER :: n 
              END FUNCTION h5dwrite_ref_reg_c
            END INTERFACE 


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            allocate(ref_buf(REF_REG_BUF_LEN*n), stat=hdferr)
            if (hdferr .NE. 0 ) then
                hdferr = -1
                return
            else
                do j = 1, n
                  do i = 1, REF_REG_BUF_LEN  
                   ref_buf(REF_REG_BUF_LEN*(j-1) + i) = buf(j)%ref(i)
                 enddo
                enddo
            endif
            hdferr = h5dwrite_ref_reg_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, ref_buf, n)
            deallocate(ref_buf)

          END SUBROUTINE h5dwrite_reference_dsetreg
           
           
          SUBROUTINE h5dwrite_integer_scalar(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dwrite_integer_scalar

          SUBROUTINE h5dwrite_integer_1(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(:) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dwrite_integer_1

          SUBROUTINE h5dwrite_integer_2(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(:,:) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf)
           
          END SUBROUTINE h5dwrite_integer_2

          SUBROUTINE h5dwrite_integer_3(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(:,:,:) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf)
           
          END SUBROUTINE h5dwrite_integer_3

          SUBROUTINE h5dwrite_integer_4(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(:,:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(:,:,:,:) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F
            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf)
           
          END SUBROUTINE h5dwrite_integer_4

          SUBROUTINE h5dwrite_integer_5(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(:,:,:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(:,:,:,:,:) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F


            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf)
           
          END SUBROUTINE h5dwrite_integer_5

          SUBROUTINE h5dwrite_integer_6(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(:,:,:,:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(:,:,:,:,:,:) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf)
           
          END SUBROUTINE h5dwrite_integer_6

          SUBROUTINE h5dwrite_integer_7(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(:,:,:,:,:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(:,:,:,:,:,:,:) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf)
           
          END SUBROUTINE h5dwrite_integer_7


          SUBROUTINE h5dwrite_char_scalar(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            CHARACTER(LEN=*), INTENT(IN) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwritec_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITEC_C'::h5dwritec_c
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              CHARACTER(LEN=*), INTENT(IN) :: buf
              END FUNCTION h5dwritec_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwritec_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dwrite_char_scalar

          SUBROUTINE h5dwrite_char_1(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            CHARACTER(LEN=*), INTENT(IN), DIMENSION(*) :: buf ! Data buffer
           ! CHARACTER, INTENT(IN), DIMENSION(*) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwritec_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITEC_C'::h5dwritec_c
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              CHARACTER(LEN=*), INTENT(IN), DIMENSION(*) :: buf
              END FUNCTION h5dwritec_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwritec_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dwrite_char_1

          SUBROUTINE h5dwrite_char_2(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            CHARACTER(LEN=*), INTENT(IN), DIMENSION(:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwritec_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITEC_C'::h5dwritec_c
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              CHARACTER(LEN=*), INTENT(IN), DIMENSION(:,:) :: buf
              END FUNCTION h5dwritec_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwritec_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dwrite_char_2

          SUBROUTINE h5dwrite_char_3(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            CHARACTER(LEN=*), INTENT(IN), DIMENSION(:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwritec_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITEC_C'::h5dwritec_c
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              CHARACTER(LEN=*), INTENT(IN), DIMENSION(:,:,:) :: buf
              END FUNCTION h5dwritec_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwritec_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dwrite_char_3

          SUBROUTINE h5dwrite_char_4(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            CHARACTER(LEN=*), INTENT(IN), DIMENSION(:,:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwritec_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITEC_C'::h5dwritec_c
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              CHARACTER(LEN=*), INTENT(IN), DIMENSION(:,:,:,:) :: buf
              END FUNCTION h5dwritec_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwritec_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dwrite_char_4

          SUBROUTINE h5dwrite_char_5(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            CHARACTER(LEN=*), INTENT(IN), DIMENSION(:,:,:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwritec_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITEC_C'::h5dwritec_c
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              CHARACTER(LEN=*), INTENT(IN), DIMENSION(:,:,:,:,:) :: buf
              END FUNCTION h5dwritec_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwritec_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dwrite_char_5

          SUBROUTINE h5dwrite_char_6(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            CHARACTER(LEN=*), INTENT(IN), DIMENSION(:,:,:,:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwritec_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITEC_C'::h5dwritec_c
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              CHARACTER(LEN=*), INTENT(IN), DIMENSION(:,:,:,:,:,:) :: buf
              END FUNCTION h5dwritec_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwritec_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dwrite_char_6

          SUBROUTINE h5dwrite_char_7(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            CHARACTER(LEN=*), INTENT(IN), DIMENSION(:,:,:,:,:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwritec_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITEC_C'::h5dwritec_c
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              CHARACTER(LEN=*), INTENT(IN), DIMENSION(:,:,:,:,:,:,:) :: buf
              END FUNCTION h5dwritec_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwritec_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dwrite_char_7

          SUBROUTINE h5dwrite_real_scalar(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            REAL, INTENT(IN) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              REAL, INTENT(IN) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default  = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F
            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dwrite_real_scalar

          SUBROUTINE h5dwrite_real_1(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            REAL, INTENT(IN), DIMENSION(:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              REAL, INTENT(IN), DIMENSION(:) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dwrite_real_1

          SUBROUTINE h5dwrite_real_2(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            REAL, INTENT(IN), DIMENSION(:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              REAL, INTENT(IN), DIMENSION(:,:) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dwrite_real_2

          SUBROUTINE h5dwrite_real_3(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            REAL, INTENT(IN), DIMENSION(:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              REAL, INTENT(IN), DIMENSION(:,:,:) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dwrite_real_3

          SUBROUTINE h5dwrite_real_4(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            REAL, INTENT(IN), DIMENSION(:,:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              REAL, INTENT(IN), DIMENSION(:,:,:,:) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dwrite_real_4

          SUBROUTINE h5dwrite_real_5(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            REAL, INTENT(IN), DIMENSION(:,:,:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              REAL, INTENT(IN), DIMENSION(:,:,:,:,:) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dwrite_real_5

          SUBROUTINE h5dwrite_real_6(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            REAL, INTENT(IN), DIMENSION(:,:,:,:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              REAL, INTENT(IN), DIMENSION(:,:,:,:,:,:) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dwrite_real_6

          SUBROUTINE h5dwrite_real_7(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            REAL, INTENT(IN), DIMENSION(:,:,:,:,:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              REAL, INTENT(IN), DIMENSION(:,:,:,:,:,:,:) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dwrite_real_7


          SUBROUTINE h5dwrite_double_scalar(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            DOUBLE PRECISION, INTENT(IN) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              DOUBLE PRECISION, INTENT(IN) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dwrite_double_scalar

          SUBROUTINE h5dwrite_double_1(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            DOUBLE PRECISION, INTENT(IN), DIMENSION(:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              DOUBLE PRECISION, INTENT(IN), DIMENSION(:) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dwrite_double_1

          SUBROUTINE h5dwrite_double_2(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            DOUBLE PRECISION, INTENT(IN), DIMENSION(:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              DOUBLE PRECISION, INTENT(IN), DIMENSION(:,:) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dwrite_double_2

          SUBROUTINE h5dwrite_double_3(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            DOUBLE PRECISION, INTENT(IN), DIMENSION(:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              DOUBLE PRECISION, INTENT(IN), DIMENSION(:,:,:) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dwrite_double_3

          SUBROUTINE h5dwrite_double_4(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            DOUBLE PRECISION, INTENT(IN), DIMENSION(:,:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              DOUBLE PRECISION, INTENT(IN), DIMENSION(:,:,:,:) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dwrite_double_4

          SUBROUTINE h5dwrite_double_5(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            DOUBLE PRECISION, INTENT(IN), DIMENSION(:,:,:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              DOUBLE PRECISION, INTENT(IN), DIMENSION(:,:,:,:,:) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dwrite_double_5

          SUBROUTINE h5dwrite_double_6(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            DOUBLE PRECISION, INTENT(IN), DIMENSION(:,:,:,:,:,:) :: buf 
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

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              DOUBLE PRECISION, INTENT(IN), DIMENSION(:,:,:,:,:,:) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dwrite_double_6

          SUBROUTINE h5dwrite_double_7(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            DOUBLE PRECISION, INTENT(IN), DIMENSION(:,:,:,:,:,:,:) :: buf 
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

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              DOUBLE PRECISION, INTENT(IN), DIMENSION(:,:,:,:,:,:,:) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
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
!		n		- size of the buffer to read data in 
!     				NOTE: This parameter is used only for reading
!				      object and dataset region references.
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
! Comment:		This function is overloaded to read INTEGER,
!			REAL, DOUBLE PRECISION and CHARACTER buffers
!			up to 7 dimensions, and one dimensional buffers
!			of the TYPE(hobj_ref_t_f) and TYPE(hdset_reg_ref_t_f)
!			types.	
!----------------------------------------------------------------------
          SUBROUTINE h5dread_reference_obj(dset_id, mem_type_id, buf, n, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN) :: n      ! Size of the budffer buf
            TYPE(hobj_ref_t_f), DIMENSION(N), INTENT(INOUT) :: buf ! Data buffer
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
            INTEGER, ALLOCATABLE, DIMENSION(:) :: ref_buf
            INTEGER :: i,j  

!            INTEGER, EXTERNAL :: h5dread_ref_obj_c
! MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_ref_obj_c(dset_id, mem_type_id,&
                                                  mem_space_id_default, &
                               file_space_id_default, xfer_prp_default, ref_buf, n)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_REF_OBJ_C'::h5dread_ref_obj_c  
              INTEGER(HID_T), INTENT(IN) :: dset_id   
              INTEGER(HID_T), INTENT(IN) :: mem_type_id 
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER, ALLOCATABLE, DIMENSION(:) :: ref_buf
              INTEGER :: n 
              END FUNCTION h5dread_ref_obj_c
            END INTERFACE 

            allocate(ref_buf(REF_OBJ_BUF_LEN*n), stat=hdferr)
            if (hdferr .NE. 0) then
                hdferr = -1
                return
            endif 

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_ref_obj_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, ref_buf, n)
             do j = 1, n
              do i = 1, REF_OBJ_BUF_LEN  
                    buf(j)%ref(i) = ref_buf(REF_OBJ_BUF_LEN*(j-1) + i)
              enddo
             enddo  
             deallocate(ref_buf) 
          END SUBROUTINE h5dread_reference_obj

          SUBROUTINE h5dread_reference_dsetreg(dset_id, mem_type_id, buf, n, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            TYPE(hdset_reg_ref_t_f), DIMENSION(:), INTENT(INOUT) :: buf ! Data buffer
            INTEGER, INTENT(IN) :: n ! Size of the buffer buf 
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
            INTEGER, ALLOCATABLE, DIMENSION(:) :: ref_buf
            INTEGER :: i,j 

!            INTEGER, EXTERNAL :: h5dread_ref_reg_c
! MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_ref_reg_c(dset_id, mem_type_id,&
                                                  mem_space_id_default, &
                               file_space_id_default, xfer_prp_default, ref_buf, n)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_REF_REG_C'::h5dread_ref_reg_c  
              INTEGER(HID_T), INTENT(IN) :: dset_id   
              INTEGER(HID_T), INTENT(IN) :: mem_type_id 
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER, ALLOCATABLE, DIMENSION(:) :: ref_buf
              INTEGER :: n 
              END FUNCTION h5dread_ref_reg_c
            END INTERFACE 

            allocate(ref_buf(REF_REG_BUF_LEN*n), stat=hdferr)
            if (hdferr .NE. 0) then
                hdferr = -1
                return
            endif

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_ref_reg_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, ref_buf, n)
           
            do j = 1, n
             do i = 1, REF_REG_BUF_LEN  
                   buf(j)%ref(i) = ref_buf(REF_REG_BUF_LEN*(j-1) + i)
             enddo
            enddo   
            deallocate(ref_buf)
          END SUBROUTINE h5dread_reference_dsetreg


          SUBROUTINE h5dread_integer_scalar(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(INOUT) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(OUT) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dread_integer_scalar

          SUBROUTINE h5dread_integer_1(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(INOUT), DIMENSION(:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(OUT), DIMENSION(:) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dread_integer_1

          SUBROUTINE h5dread_integer_2(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(INOUT), DIMENSION(:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(OUT), DIMENSION(:,:) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf)
           
          END SUBROUTINE h5dread_integer_2

          SUBROUTINE h5dread_integer_3(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(INOUT), DIMENSION(:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(OUT), DIMENSION(:,:,:) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf)
           
          END SUBROUTINE h5dread_integer_3

          SUBROUTINE h5dread_integer_4(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(INOUT), DIMENSION(:,:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(OUT), DIMENSION(:,:,:,:) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf)
           
          END SUBROUTINE h5dread_integer_4

          SUBROUTINE h5dread_integer_5(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(INOUT), DIMENSION(:,:,:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(OUT), DIMENSION(:,:,:,:,:) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf)
           
          END SUBROUTINE h5dread_integer_5

          SUBROUTINE h5dread_integer_6(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(INOUT), DIMENSION(:,:,:,:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(OUT), DIMENSION(:,:,:,:,:,:) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf)
           
          END SUBROUTINE h5dread_integer_6

          SUBROUTINE h5dread_integer_7(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(INOUT), DIMENSION(:,:,:,:,:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(OUT), DIMENSION(:,:,:,:,:,:,:) :: buf
              END FUNCTION h5dread_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf)
           
          END SUBROUTINE h5dread_integer_7

          SUBROUTINE h5dread_char_scalar(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            CHARACTER(LEN=*), INTENT(INOUT) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dreadc_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREADC_C'::h5dreadc_c
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              CHARACTER(LEN=*), INTENT(OUT) :: buf
              END FUNCTION h5dreadc_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dreadc_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dread_char_scalar

          SUBROUTINE h5dread_char_1(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            CHARACTER(LEN=*), INTENT(INOUT), DIMENSION(*) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dreadc_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREADC_C'::h5dreadc_c
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              CHARACTER(LEN=*), INTENT(OUT), DIMENSION(*) :: buf
              END FUNCTION h5dreadc_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dreadc_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dread_char_1

          SUBROUTINE h5dread_char_2(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            CHARACTER(LEN=*), INTENT(INOUT), DIMENSION(:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dreadc_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREADC_C'::h5dreadc_c
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              CHARACTER(LEN=*), INTENT(OUT), DIMENSION(:,:) :: buf
              END FUNCTION h5dreadc_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dreadc_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dread_char_2

          SUBROUTINE h5dread_char_3(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            CHARACTER(LEN=*), INTENT(INOUT), DIMENSION(:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dreadc_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREADC_C'::h5dreadc_c
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              CHARACTER(LEN=*), INTENT(OUT), DIMENSION(:,:,:) :: buf
              END FUNCTION h5dreadc_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dreadc_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dread_char_3

          SUBROUTINE h5dread_char_4(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            CHARACTER(LEN=*), INTENT(INOUT), DIMENSION(:,:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dreadc_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREADC_C'::h5dreadc_c
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              CHARACTER(LEN=*), INTENT(OUT), DIMENSION(:,:,:,:) :: buf
              END FUNCTION h5dreadc_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dreadc_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dread_char_4

          SUBROUTINE h5dread_char_5(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            CHARACTER(LEN=*), INTENT(INOUT), DIMENSION(:,:,:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dreadc_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREADC_C'::h5dreadc_c
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              CHARACTER(LEN=*), INTENT(OUT), DIMENSION(:,:,:,:,:) :: buf
              END FUNCTION h5dreadc_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dreadc_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dread_char_5

          SUBROUTINE h5dread_char_6(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            CHARACTER(LEN=*), INTENT(INOUT), DIMENSION(:,:,:,:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dreadc_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREADC_C'::h5dreadc_c
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              CHARACTER(LEN=*), INTENT(OUT), DIMENSION(:,:,:,:,:,:) :: buf
              END FUNCTION h5dreadc_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dreadc_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dread_char_6

          SUBROUTINE h5dread_char_7(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            CHARACTER(LEN=*), INTENT(INOUT), DIMENSION(:,:,:,:,:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dreadc_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREADC_C'::h5dreadc_c
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              CHARACTER(LEN=*), INTENT(OUT), DIMENSION(:,:,:,:,:,:,:) :: buf
              END FUNCTION h5dreadc_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dreadc_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dread_char_7

          SUBROUTINE h5dread_real_scalar(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            REAL, INTENT(INOUT) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              REAL, INTENT(OUT) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dread_real_scalar

          SUBROUTINE h5dread_real_1(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            REAL, INTENT(INOUT), DIMENSION(:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              REAL, INTENT(OUT), DIMENSION(:) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dread_real_1

          SUBROUTINE h5dread_real_2(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            REAL, INTENT(INOUT), DIMENSION(:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              REAL, INTENT(OUT), DIMENSION(:,:) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dread_real_2

          SUBROUTINE h5dread_real_3(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            REAL, INTENT(INOUT), DIMENSION(:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              REAL, INTENT(OUT), DIMENSION(:,:,:) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dread_real_3

          SUBROUTINE h5dread_real_4(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            REAL, INTENT(INOUT), DIMENSION(:,:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              REAL, INTENT(OUT), DIMENSION(:,:,:,:) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dread_real_4

          SUBROUTINE h5dread_real_5(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            REAL, INTENT(INOUT), DIMENSION(:,:,:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              REAL, INTENT(OUT), DIMENSION(:,:,:,:,:) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dread_real_5

          SUBROUTINE h5dread_real_6(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            REAL, INTENT(INOUT), DIMENSION(:,:,:,:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              REAL, INTENT(OUT), DIMENSION(:,:,:,:,:,:) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dread_real_6

          SUBROUTINE h5dread_real_7(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            REAL, INTENT(INOUT), DIMENSION(:,:,:,:,:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              REAL, INTENT(OUT), DIMENSION(:,:,:,:,:,:,:) :: buf
              END FUNCTION h5dread_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dread_real_7

          SUBROUTINE h5dread_double_scalar(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            DOUBLE PRECISION, INTENT(INOUT) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              DOUBLE PRECISION, INTENT(OUT) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dread_double_scalar

          SUBROUTINE h5dread_double_1(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            DOUBLE PRECISION, INTENT(INOUT), DIMENSION(:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dread_double_1

          SUBROUTINE h5dread_double_2(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            DOUBLE PRECISION, INTENT(INOUT), DIMENSION(:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              DOUBLE PRECISION, INTENT(OUT), DIMENSION(:,:) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dread_double_2

          SUBROUTINE h5dread_double_3(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            DOUBLE PRECISION, INTENT(INOUT), DIMENSION(:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              DOUBLE PRECISION, INTENT(OUT), DIMENSION(:,:,:) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dread_double_3

          SUBROUTINE h5dread_double_4(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            DOUBLE PRECISION, INTENT(INOUT), DIMENSION(:,:,:,:) :: buf 
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

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              DOUBLE PRECISION, INTENT(OUT), DIMENSION(:,:,:,:) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dread_double_4

          SUBROUTINE h5dread_double_5(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            DOUBLE PRECISION, INTENT(INOUT), DIMENSION(:,:,:,:,:) :: buf ! Data buffer
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

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              DOUBLE PRECISION, INTENT(OUT), DIMENSION(:,:,:,:,:) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dread_double_5

          SUBROUTINE h5dread_double_6(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            DOUBLE PRECISION, INTENT(INOUT), DIMENSION(:,:,:,:,:,:) :: buf 
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

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              DOUBLE PRECISION, INTENT(OUT), DIMENSION(:,:,:,:,:,:) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dread_double_6

          SUBROUTINE h5dread_double_7(dset_id, mem_type_id, buf, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            DOUBLE PRECISION, INTENT(INOUT), DIMENSION(:,:,:,:,:,:,:) :: buf 
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

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              DOUBLE PRECISION, INTENT(OUT), DIMENSION(:,:,:,:,:,:,:) :: buf
              END FUNCTION h5dread_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf)
           
          END SUBROUTINE h5dread_double_7

!----------------------------------------------------------------------
! Name:		h5dget_space_f 
!
! Purpose:	Returns an identifier for a copy of the dataspace for a 
!		dataset.   	
!
! Inputs:  
!		dataset_id	- dataset identifier
! Outputs:  
!		dataspace_id	- dataspace identifier
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE			
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces were added for 
!			called C functions (it is needed for Windows
!			port).  February 28, 2001 
!
! Comment:		
!----------------------------------------------------------------------
          SUBROUTINE h5dget_space_f(dataset_id, dataspace_id, hdferr) 
            IMPLICIT NONE 
            INTEGER(HID_T), INTENT(IN) :: dataset_id      ! Dataset identifier
            INTEGER(HID_T), INTENT(OUT) :: dataspace_id   ! Dataspace identifier
            INTEGER, INTENT(OUT) :: hdferr                ! Error code 

!            INTEGER, EXTERNAL :: h5dget_space_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dget_space_c(dataset_id, dataspace_id)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DGET_SPACE_C'::h5dget_space_c
              INTEGER(HID_T), INTENT(IN) :: dataset_id
              INTEGER(HID_T), INTENT(OUT) :: dataspace_id
              END FUNCTION h5dget_space_c
            END INTERFACE

            hdferr = h5dget_space_c(dataset_id, dataspace_id)
          END SUBROUTINE h5dget_space_f  

!----------------------------------------------------------------------
! Name:		h5dget_type_f 
!
! Purpose:	Returns an identifier for a copy of the datatype for a 
!		dataset.   	
!
! Inputs:  
!		dataset_id	- dataset identifier
! Outputs:  
!		datatype_id	- dataspace identifier
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE			
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces were added for 
!			called C functions (it is needed for Windows
!			port).  February 28, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5dget_type_f(dataset_id, datatype_id, hdferr) 
            IMPLICIT NONE 
            INTEGER(HID_T), INTENT(IN) :: dataset_id      ! Dataset identifier
            INTEGER(HID_T), INTENT(OUT) :: datatype_id    ! Datatype identifier
            INTEGER, INTENT(OUT) :: hdferr                ! Error code 
!            INTEGER, EXTERNAL :: h5dget_type_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dget_type_c (dataset_id, datatype_id)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DGET_TYPE_C'::h5dget_type_c
              INTEGER(HID_T), INTENT(IN) :: dataset_id
              INTEGER(HID_T), INTENT(OUT) :: datatype_id
              END FUNCTION h5dget_type_c
            END INTERFACE

            hdferr = h5dget_type_c (dataset_id, datatype_id)
          END SUBROUTINE h5dget_type_f  

!----------------------------------------------------------------------
! Name:		h5dextend_f 
!
! Purpose:	Extends a dataset with unlimited dimension.	
!
! Inputs:  
!		dataset_id	- dataset identifier
!		size		- array containing the new magnitude of 
!				  each dimension
! Outputs:  
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE			
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces were added for 
!			called C functions (it is needed for Windows
!			port).  February 28, 2001 
!
! Comment:		
!----------------------------------------------------------------------


          SUBROUTINE h5dextend_f(dataset_id, size, hdferr) 
            IMPLICIT NONE 
            INTEGER(HID_T), INTENT(IN) :: dataset_id      ! Dataset identifier
            INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN)  :: size
                                                          ! Array containing 
                                                          ! dimensions' sizes 
            INTEGER, INTENT(OUT) :: hdferr                ! Error code 

!            INTEGER, EXTERNAL ::  h5dextend_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dextend_c(dataset_id, size)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DEXTEND_C'::h5dextend_c
              INTEGER(HID_T), INTENT(IN) :: dataset_id
              INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN)  :: size
              END FUNCTION h5dextend_c
            END INTERFACE

            hdferr = h5dextend_c(dataset_id, size)
          END SUBROUTINE h5dextend_f  


!----------------------------------------------------------------------
! Name:		h5dget_create_plist_f 
!
! Purpose:	Returns an identifier for a copy of the dataset creation 
!		property list for a dataset. 	
!
! Inputs:  
!		dataset_id	- dataset identifier
! Outputs:  
!		plist_id	- creation property list identifier
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE			
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces were added for 
!			called C functions (it is needed for Windows
!			port).  February 28, 2001 
!
! Comment:		
!----------------------------------------------------------------------


          SUBROUTINE h5dget_create_plist_f(dataset_id, plist_id, hdferr) 
            IMPLICIT NONE 
            INTEGER(HID_T), INTENT(IN) :: dataset_id      ! Dataset identifier
            INTEGER(HID_T), INTENT(OUT) :: plist_id    ! Dataset creation
                                                  ! property list identifier
            INTEGER, INTENT(OUT) :: hdferr                ! Error code 

!            INTEGER, EXTERNAL :: h5dget_create_plist_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dget_create_plist_c(dataset_id, plist_id)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5DGET_CREATE_PLIST_C'::h5dget_create_plist_c
              INTEGER(HID_T), INTENT(IN) :: dataset_id
              INTEGER(HID_T), INTENT(OUT) :: plist_id
              END FUNCTION h5dget_create_plist_c
            END INTERFACE

            hdferr = h5dget_create_plist_c(dataset_id, plist_id)
          END SUBROUTINE h5dget_create_plist_f  

      END MODULE H5D
