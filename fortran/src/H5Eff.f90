!****h* ROBODoc/H5E
!
! NAME
!  MODULE H5E
!
! FILE
!  fortran/src/H5Eff.f90
!
! PURPOSE
!  This Module contains Fortran interfaces for H5E functions.
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
!                         *** IMPORTANT ***
!  If you add a new H5E function to the module you must add the function name
!  to the Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
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
!  h5eclear_f
!
! PURPOSE
!  Clears the error stack for the current thread.
!
! OUTPUTS
!  hdferr 	 - Returns 0 if successful and -1 if fails
! OPTIONAL PARAMETERS
!  estack_id     - Error Stack id
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  April 6, 2001
!
!  Added optional error stack identifier in order to bring
!  the function in line with the h5eclear2 routine.
!  MSB, July 9, 2009
!
! SOURCE
  SUBROUTINE h5eclear_f(hdferr, estack_id)
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: estack_id
!*****
    INTEGER(HID_T) :: estack_id_default

    INTERFACE
       INTEGER FUNCTION h5eclear_c(estack_id_default)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5ECLEAR_C'::h5eclear_c
         !DEC$ENDIF
         INTEGER(HID_T) :: estack_id_default
       END FUNCTION h5eclear_c
    END INTERFACE

    estack_id_default = H5E_DEFAULT_F
    IF(PRESENT(estack_id)) estack_id_default = estack_id

    hdferr = h5eclear_c(estack_id_default)
  END SUBROUTINE h5eclear_f

!****s* H5E/h5eprint_f
!
! NAME
!  h5eprint_f
!
! PURPOSE
!  Prints the error stack in a default manner.
!
! OUTPUTS
!  hdferr 	 - Returns 0 if successful and -1 if fails
!
! OPTIONAL PARAMETERS
!  name 	 - name of the file that contains print output
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  April 6, 2001
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
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5EPRINT_C1'::h5eprint_c1
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER :: namelen
         CHARACTER(LEN=*),INTENT(IN) :: name
       END FUNCTION h5eprint_c1
    END INTERFACE

    INTERFACE
       INTEGER FUNCTION h5eprint_c2()
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5EPRINT_C2'::h5eprint_c2
         !DEC$ENDIF
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
!  h5eget_major_f
!
! PURPOSE
!  Returns a character string describing an error specified
!  by a major error number.
!
! INPUTS
!  error_no 	 - major error number
!
! OUTPUTS
!  name 	 - character string describing the error
!  namelen 	 - number of characters in the name buffer
!  hdferr 	 - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  April 6, 2001
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
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5EGET_MAJOR_C'::h5eget_major_c
         !DEC$ENDIF
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
!  h5eget_minor_f
!
! PURPOSE
!  Returns a character string describing an error specified
!  by a minor error number.
!
! INPUTS
!  error_no 	 - minor error number
!
! OUTPUTS
!  name 	 - character string describing the error
!  hdferr 	 - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  April 6, 2001
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
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5EGET_MINOR_C'::h5eget_minor_c
         !DEC$ENDIF
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
!  h5eset_auto_f
!
! PURPOSE
!  Returns settings for automatic error stack traversal function and its data.
!
! Inputs:
!  printflag   - Flag to turn automatic error printing on or off;
!                possible values are:
!                  printon (1)
!                  printoff(0)
!  estack_id   - Error stack identifier.
!  func        - Function to be called upon an error condition.
!  client_data - Data passed to the error function
!  
! Outputs:
!  hdferr      - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  M. Scot Breitenfeld
!  July 10, 2009
!
! Fortran2003 Interface:
  SUBROUTINE h5eset_auto_f(printflag, hdferr, estack_id, func, client_data)
    USE, INTRINSIC :: ISO_C_BINDING
    INTEGER       , INTENT(IN)            :: printflag
    INTEGER       , INTENT(OUT)           :: hdferr
    INTEGER(HID_T), INTENT(IN) , OPTIONAL :: estack_id
    TYPE(C_FUNPTR), INTENT(IN) , OPTIONAL :: func
    TYPE(C_PTR)   , INTENT(IN) , OPTIONAL :: client_data
!*****
    INTEGER(HID_T) :: estack_id_default
    TYPE(C_FUNPTR) :: func_default
    TYPE(C_PTR)    :: client_data_default
    INTERFACE
       INTEGER FUNCTION h5eset_auto2_c(printflag, estack_id, func, client_data) &
            BIND(C, NAME='h5eset_auto2_c')
         USE, INTRINSIC :: ISO_C_BINDING, ONLY : c_ptr, c_funptr
         USE H5GLOBAL
         INTEGER :: printflag
         INTEGER(HID_T) :: estack_id
!!$         TYPE(C_FUNPTR) :: func
!!$         TYPE(C_PTR), VALUE :: client_data
         TYPE(C_FUNPTR), VALUE :: func
         TYPE(C_PTR), VALUE :: client_data
       END FUNCTION h5eset_auto2_c
    END INTERFACE

    estack_id_default = -1
    func_default = C_NULL_FUNPTR
    client_data_default = C_NULL_PTR

    IF(PRESENT(estack_id)) estack_id_default = estack_id
    IF(PRESENT(func)) func_default = func
    IF(PRESENT(client_data)) client_data_default = client_data

    hdferr = h5eset_auto2_c(printflag, estack_id_default, func_default, client_data_default)
  END SUBROUTINE h5eset_auto_f

END MODULE H5E

