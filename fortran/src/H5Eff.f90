!****h* ROBODoc/H5E
!
! NAME
!   MODULE H5E
!
! FILE
!   fortran/src/H5Eff.f90
!  
! PURPOSE
!   This Module contains Fortran interfaces for H5E functions.
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
! NOTES
!                          *** IMPORTANT ***
!   If you add a new H5E function to the module you must add the function name 
!   to the Windows dll file 'hdf5_fortrandll.def' in the ROBODoc directory.
!   This is needed for Windows based operating systems.
!
!*****

MODULE H5E
  
  USE H5GLOBAL

  !Turn on automatic printing of errors
  INTEGER, PARAMETER :: PRINTON = 1
  
  !Turn off automatic printing of errors
  INTEGER, PARAMETER :: PRINTOFF = 0
  
CONTAINS

!****s* H5E/h5eclear_f
!
! NAME
!   h5eclear_f
!  
! PURPOSE
!   Clears the error stack for the current thread.
!
! OUTPUTS
!	hdferr: - error code		
!		    Success:  0
!		    Failure: -1
! AUTHOR
!   Elena Pourmal
!   August 12, 1999	
!   
! HISTORY
!   Explicit Fortran interfaces were added for 
!   called C functions (it is needed for Windows
!   port).  April 6, 2001
!
! SOURCE

  SUBROUTINE h5eclear_f(hdferr)
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
!*****

    INTERFACE
       INTEGER FUNCTION h5eclear_c()
         USE H5GLOBAL
         !DEC$ IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ ATTRIBUTES C,reference,decorate,alias:'H5ECLEAR_C'::h5eclear_c
         !DEC$ ENDIF
       END FUNCTION h5eclear_c
    END INTERFACE
    hdferr = h5eclear_c()
  END SUBROUTINE h5eclear_f

!****s* H5E/h5eprint_f
!
! NAME
!   h5eprint_f
!  
! PURPOSE
!   Prints the error stack in a default manner.
!
! OUTPUTS
!	hdferr: - error code		
!		    Success:  0
!		    Failure: -1
! OPTIONAL PARAMETERS
!	name	- name of the file that
!	          contains print output
! AUTHOR
!   Elena Pourmal
!   August 12, 1999	
!   
! HISTORY
!   Explicit Fortran interfaces were added for 
!   called C functions (it is needed for Windows
!   port).  April 6, 2001
!
! SOURCE

  SUBROUTINE h5eprint_f(hdferr, name)
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: name ! File name
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
!*****
    INTEGER :: namelen

    INTERFACE
       INTEGER FUNCTION h5eprint_c1(name, namelen)
         USE H5GLOBAL
         !DEC$ IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ ATTRIBUTES C,reference,decorate,alias:'H5EPRINT_C1'::h5eprint_c1
         !DEC$ ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER :: namelen
         CHARACTER(LEN=*),INTENT(IN) :: name
       END FUNCTION h5eprint_c1
    END INTERFACE

    INTERFACE
       INTEGER FUNCTION h5eprint_c2()
         USE H5GLOBAL
         !DEC$ IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ ATTRIBUTES C,reference,decorate,alias:'H5EPRINT_C2'::h5eprint_c2
         !DEC$ ENDIF
       END FUNCTION h5eprint_c2
    END INTERFACE
    namelen = LEN(NAME)
    IF (PRESENT(name)) THEN
       hdferr = h5eprint_c1(name, namelen) 
    ELSE
       hdferr = h5eprint_c2() 
    ENDIF
  END SUBROUTINE h5eprint_f
!****s* H5E/h5eget_major_f
!
! NAME
!   h5eget_major_f
!  
! PURPOSE
!   Returns a character string describing an error specified 
!   by a major error number.
!
! INPUTS
!   error_no	- major error number 
! 
! OUTPUTS 
!   name	- character string describing the error
!   namelen	- number of characters in the name buffer
!   hdferr:	- error code		
!				 	Success:  0
!				 	Failure: -1
! AUTHOR
!   Elena Pourmal
!   August 12, 1999	
!   
! HISTORY
!   Explicit Fortran interfaces were added for 
!   called C functions (it is needed for Windows
!   port).  April 6, 2001
!
! SOURCE
  SUBROUTINE h5eget_major_f(error_no, name, namelen, hdferr)
    INTEGER, INTENT(IN) :: error_no        ! Major error number
    CHARACTER(LEN=*), INTENT(OUT) :: name  ! Character string describing
                                           ! the error.
    INTEGER(SIZE_T), INTENT(IN) :: namelen ! Anticipated number of characters 
                                           ! in name.
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
!*****
    INTERFACE
       INTEGER FUNCTION h5eget_major_c(error_no, name, namelen)
         USE H5GLOBAL
         !DEC$ IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ ATTRIBUTES C,reference,decorate,alias:'H5EGET_MAJOR_C'::h5eget_major_c
         !DEC$ ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER :: error_no
         CHARACTER(LEN=*) :: name 
         INTEGER(SIZE_T), INTENT(IN) :: namelen
       END FUNCTION h5eget_major_c
    END INTERFACE
    
    hdferr = h5eget_major_c(error_no, name, namelen) 
  END SUBROUTINE h5eget_major_f
!****s* H5E/h5eget_minor_f
!
! NAME
!   h5eget_minor_f
!  
! PURPOSE
!   Returns a character string describing an error specified 
!   by a minor error number.
!
! INPUTS
!   error_no	- minor error number 
! 
! OUTPUTS 
!   name	- character string describing the error
!   hdferr:	- error code		
!			Success:  0
!			Failure: -1
! AUTHOR
!   Elena Pourmal
!   August 12, 1999	
!   
! HISTORY
!   Explicit Fortran interfaces were added for 
!   called C functions (it is needed for Windows
!   port).  April 6, 2001
!
! SOURCE
  SUBROUTINE h5eget_minor_f(error_no, name, hdferr)
    INTEGER, INTENT(IN) :: error_no       ! Major error number
    CHARACTER(LEN=*), INTENT(OUT) :: name ! Character string describing
                                          ! the error
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
!*****
    INTERFACE
       INTEGER FUNCTION h5eget_minor_c(error_no, name)
         USE H5GLOBAL
         !DEC$ IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ ATTRIBUTES C,reference,decorate,alias:'H5EGET_MINOR_C'::h5eget_minor_c
         !DEC$ ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER :: error_no
         CHARACTER(LEN=*) :: name 
       END FUNCTION h5eget_minor_c
    END INTERFACE

    hdferr = h5eget_minor_c(error_no, name) 
  END SUBROUTINE h5eget_minor_f
!****s* H5E/h5eset_auto_f
!
! NAME
!   h5eset_auto_f
!  
! PURPOSE
!   Turns automatic error printing on or off.
!
! INPUTS  
!   printflag	- flag to turn automatic error
!		- Possible values are:
!		- 1 (on), 0 (off)
! OUTPUTS 
!   hdferr:	- error code		
!			Success:  0
!			Failure: -1
! AUTHOR
!   Elena Pourmal
!   August 12, 1999	
!   
! HISTORY
!   Explicit Fortran interfaces were added for 
!   called C functions (it is needed for Windows
!   port).  April 6, 2001
!
! SOURCE
  SUBROUTINE h5eset_auto_f(printflag, hdferr)
    INTEGER, INTENT(IN) :: printflag  ! flag to turn automatic error
                                      ! printing on or off
                                      ! possible values are:
                                      !     printon (1)
                                      !     printoff(0)
    INTEGER, INTENT(OUT) :: hdferr    ! Error code
!*****
    INTERFACE
       INTEGER FUNCTION h5eset_auto_c(printflag)
         USE H5GLOBAL
         !DEC$ IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ ATTRIBUTES C,reference,decorate,alias:'H5ESET_AUTO_C'::h5eset_auto_c
         !DEC$ ENDIF
         INTEGER :: printflag
       END FUNCTION h5eset_auto_c
    END INTERFACE

    hdferr = h5eset_auto_c(printflag) 
  END SUBROUTINE h5eset_auto_f
  
END MODULE H5E
          
