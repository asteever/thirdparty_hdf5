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
! This file contains Fortran 2003 interfaces for H5P functions.
!
MODULE H5P_F03

  USE H5GLOBAL

  INTERFACE h5pset_fill_value_f
     MODULE PROCEDURE h5pset_fill_value_integer
     MODULE PROCEDURE h5pset_fill_value_real
     ! Comment if on Crays
     MODULE PROCEDURE h5pset_fill_value_double
     ! End comment if on Crays
     MODULE PROCEDURE h5pset_fill_value_char

     ! Recommended procedure:
     MODULE PROCEDURE h5pset_fill_value_ptr

  END INTERFACE

  INTERFACE h5pget_fill_value_f
     MODULE PROCEDURE h5pget_fill_value_integer
     MODULE PROCEDURE h5pget_fill_value_real
     ! Comment if on Crays
     MODULE PROCEDURE h5pget_fill_value_double
     ! End comment if on Crays
     MODULE PROCEDURE h5pget_fill_value_char

     ! Recommended procedure:
     MODULE PROCEDURE h5pget_fill_value_ptr

  END INTERFACE

  INTERFACE h5pset_f
     MODULE PROCEDURE h5pset_integer
     MODULE PROCEDURE h5pset_real
     ! Comment if on Crays
     MODULE PROCEDURE h5pset_double
     ! End comment if on Crays
     MODULE PROCEDURE h5pset_char
     ! Recommended procedure:
     MODULE PROCEDURE h5pset_ptr

  END INTERFACE

  INTERFACE h5pget_f
     MODULE PROCEDURE h5pget_integer
     MODULE PROCEDURE h5pget_real
     ! Comment if on Crays
     MODULE PROCEDURE h5pget_double
     ! End comment if on Crays
     MODULE PROCEDURE h5pget_char
     ! Recommended procedure:
     MODULE PROCEDURE h5pget_ptr
  END INTERFACE

  INTERFACE h5pregister_f
     MODULE PROCEDURE h5pregister_integer
     MODULE PROCEDURE h5pregister_real
     ! Comment if on Crays
     MODULE PROCEDURE h5pregister_double
     ! End comment if on Crays
     MODULE PROCEDURE h5pregister_char
     ! Recommended procedure:
     MODULE PROCEDURE h5pregister_ptr
  END INTERFACE

  INTERFACE h5pinsert_f
     MODULE PROCEDURE h5pinsert_integer
     MODULE PROCEDURE h5pinsert_real
     ! Comment if on Crays
     MODULE PROCEDURE h5pinsert_double
     ! End comment if on Crays
     MODULE PROCEDURE h5pinsert_char
     ! Recommended procedure:
     MODULE PROCEDURE h5pinsert_ptr
  END INTERFACE

  INTERFACE 
     FUNCTION h5pget_fill_value_c(prp_id, type_id, fillvalue)
       USE iso_c_binding
       USE H5GLOBAL
       USE, INTRINSIC :: ISO_C_BINDING
       !DEC$ IF DEFINED(HDF5F90_WINDOWS)
       !DEC$ ATTRIBUTES C,reference,decorate,alias:'H5PGET_FILL_VALUE_C'::h5pget_fill_value_c
       !DEC$ ENDIF
       INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier 
       INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of 
       ! of fillvalue datatype
       ! (in memory) 
       TYPE(C_PTR), VALUE :: fillvalue       ! Fillvalue
     END FUNCTION h5pget_fill_value_c
  END INTERFACE

  INTERFACE 
     FUNCTION h5pset_fill_value_c(prp_id, type_id, fillvalue)
       USE iso_c_binding
       USE H5GLOBAL
       USE, INTRINSIC :: ISO_C_BINDING
       !DEC$ IF DEFINED(HDF5F90_WINDOWS)
       !DEC$ ATTRIBUTES C,reference,decorate,alias:'H5PSET_FILL_VALUE_C'::h5pset_fill_value_c
       !DEC$ ENDIF
       INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier 
       INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of 
       ! of fillvalue datatype
       ! (in memory) 
       TYPE(C_PTR), VALUE :: fillvalue       ! Fillvalue
     END FUNCTION h5pset_fill_value_c
  END INTERFACE

  INTERFACE
     FUNCTION h5pset_c(prp_id, name, name_len, value)
       USE iso_c_binding
       USE H5GLOBAL
       USE, INTRINSIC :: ISO_C_BINDING
       !DEC$ IF DEFINED(HDF5F90_WINDOWS)
       !DEC$ ATTRIBUTES C,reference,decorate,alias:'H5PSET_C'::h5pset_c
       !DEC$ ENDIF
       INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier 
       CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to modify
       INTEGER :: name_len
       TYPE(C_PTR), VALUE :: value ! Property value
     END FUNCTION h5pset_c
  END INTERFACE

  INTERFACE
     FUNCTION h5pget_c(prp_id, name, name_len, value)
       USE iso_c_binding
       USE H5GLOBAL
       USE, INTRINSIC :: ISO_C_BINDING
       !DEC$ IF DEFINED(HDF5F90_WINDOWS)
       !DEC$ ATTRIBUTES C,reference,decorate,alias:'H5PGET_C'::h5pget_c
       !DEC$ ENDIF
       INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier 
       CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to modify
       INTEGER :: name_len
       TYPE(C_PTR), VALUE :: value ! Property value
     END FUNCTION h5pget_c
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5pregister_c(class, name, name_len, size, value)
       USE iso_c_binding
       USE H5GLOBAL
       !DEC$ IF DEFINED(HDF5F90_WINDOWS)
       !DEC$ ATTRIBUTES C,reference,decorate,alias:'H5PREGISTER_C'::h5pregister_c
       !DEC$ ENDIF
       !DEC$ATTRIBUTES reference :: name
       INTEGER(HID_T), INTENT(IN) :: class
       CHARACTER(LEN=*), INTENT(IN) :: name
       INTEGER, INTENT(IN)         :: name_len
       INTEGER(SIZE_T), INTENT(IN) :: size 
       TYPE(C_PTR), INTENT(IN), VALUE :: value
     END FUNCTION h5pregister_c
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5pinsert_c(plist, name, name_len, size, value)
       USE iso_c_binding
       USE H5GLOBAL
       !DEC$ IF DEFINED(HDF5F90_WINDOWS)
       !DEC$ ATTRIBUTES C,reference,decorate,alias:'H5PINSERT_C'::h5pinsert_c
       !DEC$ ENDIF
       !DEC$ATTRIBUTES reference :: name
       INTEGER(HID_T), INTENT(IN) :: plist
       CHARACTER(LEN=*), INTENT(IN) :: name
       INTEGER, INTENT(IN)         :: name_len
       INTEGER(SIZE_T), INTENT(IN) :: size 
       TYPE(c_ptr), INTENT(IN), value :: value
     END FUNCTION h5pinsert_c
  END INTERFACE

CONTAINS

  !----------------------------------------------------------------------
  ! Name:		h5pset(get)fill_value_f 
  !
  ! Purpose: 	Sets(gets) fill value for a dataset creation property list
  !
  ! Inputs:  
  !		prp_id		- dataset creation property list identifier
  !		type_id		- datatype identifier for fill value
  !		fillvalue	- fill value
  ! Outputs:  
  !	(	type_id		- datatype identifier for fill value )
  !	(		fillvalue	- fill value )
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
  !			port).  March 14, 2001
  !
  !                     Added the recommended way of passing fillvalue
  !                     and that is by passing the C address, all other 
  !                     ways are obsolete and should be avoided. June, 2008 MSB
  !
  ! Comment:	h5pset(get)fill_value_f function is overloaded to support
  !		INTEGER, REAL, DOUBLE PRECISION and CHARACTER dtatypes.		
  !----------------------------------------------------------------------


  SUBROUTINE h5pset_fill_value_integer(prp_id, type_id, fillvalue, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pset_fill_value_integer
    !DEC$endif
    !
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier 
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of 
    ! of fillvalue datatype 
    ! (in memory)
    INTEGER, INTENT(IN), TARGET :: fillvalue   ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
    TYPE(C_PTR) :: f_ptr ! C address

    f_ptr = C_LOC(fillvalue)

    hdferr = h5pset_fill_value_c(prp_id, type_id, f_ptr)

  END SUBROUTINE h5pset_fill_value_integer

  SUBROUTINE h5pget_fill_value_integer(prp_id, type_id, fillvalue, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pget_fill_value_integer
    !DEC$endif
    !
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of 
    ! of fillvalue datatype
    ! (in memory) 
    INTEGER, INTENT(OUT), TARGET :: fillvalue   ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
    TYPE(C_PTR) :: f_ptr ! C address

    f_ptr = C_LOC(fillvalue)

    hdferr = h5pget_fill_value_c(prp_id, type_id, f_ptr)

  END SUBROUTINE h5pget_fill_value_integer


  SUBROUTINE h5pset_fill_value_real(prp_id, type_id, fillvalue, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pset_fill_value_real
    !DEC$endif
    !
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of 
    ! of fillvalue datatype 
    ! (in memory)
    REAL, INTENT(IN), TARGET :: fillvalue   ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
    TYPE(C_PTR) :: f_ptr ! C address

    f_ptr = C_LOC(fillvalue)

    hdferr = h5pset_fill_value_c(prp_id, type_id, f_ptr)

  END SUBROUTINE h5pset_fill_value_real


  SUBROUTINE h5pget_fill_value_real(prp_id, type_id, fillvalue, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pget_fill_value_real
    !DEC$endif
    !
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier 
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of 
    ! of fillvalue datatype
    ! (in memory) 
    REAL, INTENT(OUT), TARGET :: fillvalue   ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
    TYPE(C_PTR) :: f_ptr ! C address

    f_ptr = C_LOC(fillvalue)

    hdferr = h5pget_fill_value_c(prp_id, type_id, f_ptr)

  END SUBROUTINE h5pget_fill_value_real


  SUBROUTINE h5pset_fill_value_double(prp_id, type_id, fillvalue, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pset_fill_value_double
    !DEC$endif
    !
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of 
    ! of fillvalue datatype 
    ! (in memory)
    DOUBLE PRECISION, INTENT(IN), TARGET :: fillvalue   ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
    TYPE(C_PTR) :: f_ptr ! C address

    f_ptr = C_LOC(fillvalue)

    hdferr = h5pset_fill_value_c(prp_id, type_id, f_ptr)
  END SUBROUTINE h5pset_fill_value_double


  SUBROUTINE h5pget_fill_value_double(prp_id, type_id, fillvalue, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pget_fill_value_double
    !DEC$endif
    !
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of 
    ! of fillvalue datatype
    ! (in memory) 
    DOUBLE PRECISION, INTENT(OUT), TARGET :: fillvalue   ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
    TYPE(C_PTR) :: f_ptr ! C address

    f_ptr = C_LOC(fillvalue)

    hdferr = h5pget_fill_value_c(prp_id, type_id, f_ptr)
  END SUBROUTINE h5pget_fill_value_double

  SUBROUTINE h5pset_fill_value_char(prp_id, type_id, fillvalue, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pset_fill_value_char
    !DEC$endif
    !
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier 
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of 
    ! of fillvalue datatype 
    ! (in memory)
    CHARACTER, INTENT(IN), TARGET :: fillvalue    ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
    TYPE(C_PTR) :: f_ptr ! C address

    f_ptr = C_LOC(fillvalue)

    hdferr = h5pset_fill_value_c(prp_id, type_id, f_ptr)

  END SUBROUTINE h5pset_fill_value_char

  SUBROUTINE h5pget_fill_value_char(prp_id, type_id, fillvalue, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pget_fill_value_char
    !DEC$endif
    !
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier 
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of 
    ! of fillvalue datatype
    ! (in memory) 
    CHARACTER, INTENT(OUT),TARGET :: fillvalue   ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
    TYPE(C_PTR) :: f_ptr ! C address

    f_ptr = C_LOC(fillvalue)

    hdferr = h5pget_fill_value_c(prp_id, type_id, f_ptr)

  END SUBROUTINE h5pget_fill_value_char

  SUBROUTINE h5pset_fill_value_ptr(prp_id, type_id, fillvalue, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pset_fill_value_ptr
    !DEC$endif
    !
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier 
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of 
    ! of fillvalue datatype 
    ! (in memory)
    TYPE(C_PTR), INTENT(IN), TARGET :: fillvalue  ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr  ! Error code

    hdferr = h5pset_fill_value_c(prp_id, type_id, fillvalue)

  END SUBROUTINE h5pset_fill_value_ptr


  SUBROUTINE h5pget_fill_value_ptr(prp_id, type_id, fillvalue, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pget_fill_value_ptr
    !DEC$endif
    !
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier 
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of 
    ! of fillvalue datatype
    ! (in memory) 
    TYPE(C_PTR), INTENT(OUT) :: fillvalue ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr  ! Error code

    hdferr = h5pget_fill_value_c(prp_id, type_id, fillvalue)

  END SUBROUTINE h5pget_fill_value_ptr

  !----------------------------------------------------------------------
  ! Name:		h5pset_integer 
  !
  ! Purpose: 	Sets a property list value
  !
  ! Inputs:  
  !		prp_id		- iproperty list identifier to modify
  !		name 		- name of property to modify
  !		value		- value to set property to
  ! Outputs:  
  !		hdferr:		- error code		
  !				 	Success:  0
  !				 	Failure: -1   
  ! Optional parameters:
  !				NONE
  !
  ! Programmer:	Elena Pourmal
  !	        October 9, 2002	
  !
  ! Modifications: 	
  !
  ! Comment:		
  !----------------------------------------------------------------------

  SUBROUTINE h5pset_integer(prp_id, name, value, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pset_integer
    !DEC$endif
    !
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier 
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to modify
    INTEGER,   INTENT(IN), TARGET :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
    INTEGER :: name_len   
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)

  END SUBROUTINE h5pset_integer

  !----------------------------------------------------------------------
  ! Name:		h5pset_real
  !
  ! Purpose: 	Sets a property list value
  !
  ! Inputs:  
  !		prp_id		- iproperty list identifier to modify
  !		name 		- name of property to modify
  !		value		- value to set property to
  ! Outputs:  
  !		hdferr:		- error code		
  !				 	Success:  0
  !				 	Failure: -1   
  ! Optional parameters:
  !				NONE
  !
  ! Programmer:	Elena Pourmal
  !	        October 9, 2002	
  !
  ! Modifications: 	
  !
  ! Comment:		
  !----------------------------------------------------------------------

  SUBROUTINE h5pset_real(prp_id, name, value, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pset_real
    !DEC$endif
    !
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier 
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to modify
    REAL,   INTENT(IN), TARGET :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
    INTEGER :: name_len   
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)

  END SUBROUTINE h5pset_real

  !----------------------------------------------------------------------
  ! Name:		h5pset_double
  !
  ! Purpose: 	Sets a property list value
  !
  ! Inputs:  
  !		prp_id		- iproperty list identifier to modify
  !		name 		- name of property to modify
  !		value		- value to set property to
  ! Outputs:  
  !		hdferr:		- error code		
  !				 	Success:  0
  !				 	Failure: -1   
  ! Optional parameters:
  !				NONE
  !
  ! Programmer:	Elena Pourmal
  !	        October 9, 2002	
  !
  ! Modifications: 	
  !
  ! Comment:		
  !----------------------------------------------------------------------

  SUBROUTINE h5pset_double(prp_id, name, value, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pset_double
    !DEC$endif
    !
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier 
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to modify
    DOUBLE PRECISION,   INTENT(IN), TARGET :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
    INTEGER :: name_len   
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)

  END SUBROUTINE h5pset_double

  !----------------------------------------------------------------------
  ! Name:		h5pset_char
  !
  ! Purpose: 	Sets a property list value
  !
  ! Inputs:  
  !		prp_id		- iproperty list identifier to modify
  !		name 		- name of property to modify
  !		value		- value to set property to
  ! Outputs:  
  !		hdferr:		- error code		
  !				 	Success:  0
  !				 	Failure: -1   
  ! Optional parameters:
  !				NONE
  !
  ! Programmer:	Elena Pourmal
  !	        October 9, 2002	
  !
  ! Modifications: 	
  !
  ! Comment:		
  !----------------------------------------------------------------------

  SUBROUTINE h5pset_char(prp_id, name, value, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pset_char
    !DEC$endif
    !
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier 
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to modify
    CHARACTER(LEN=*),   INTENT(IN), TARGET :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
    INTEGER :: name_len   
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)

  END SUBROUTINE h5pset_char

  !----------------------------------------------------------------------
  ! Name:		h5pget_integer 
  !
  ! Purpose: 	Gets a property list value
  !
  ! Inputs:  
  !		prp_id		- iproperty list identifier to modify
  !		name 		- name of property to modify
  ! Outputs:  
  !		value		- value of property
  !		hdferr:		- error code		
  !				 	Success:  0
  !				 	Failure: -1   
  ! Optional parameters:
  !				NONE
  !
  ! Programmer:	Elena Pourmal
  !	        October 9, 2002	
  !
  ! Modifications: 	
  !
  ! Comment:		
  !----------------------------------------------------------------------

  SUBROUTINE h5pget_integer(prp_id, name, value, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pget_integer
    !DEC$endif
    !
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier 
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to modify
    INTEGER,   INTENT(OUT), TARGET :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
    INTEGER :: name_len   
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)

  END SUBROUTINE h5pget_integer

  !----------------------------------------------------------------------
  ! Name:		h5pget_real
  !
  ! Purpose: 	Gets a property list value
  !
  ! Inputs:  
  !		prp_id		- iproperty list identifier to modify
  !		name 		- name of property to modify
  ! Outputs:  
  !		value		- value of property
  !		hdferr:		- error code		
  !				 	Success:  0
  !				 	Failure: -1   
  ! Optional parameters:
  !				NONE
  !
  ! Programmer:	Elena Pourmal
  !	        October 9, 2002	
  !
  ! Modifications: 	
  !
  ! Comment:		
  !----------------------------------------------------------------------

  SUBROUTINE h5pget_real(prp_id, name, value, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pget_real
    !DEC$endif
    !
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier 
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to modify
    REAL,   INTENT(OUT), TARGET :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)
  END SUBROUTINE h5pget_real

  !----------------------------------------------------------------------
  ! Name:		h5pget_double
  !
  ! Purpose: 	Gets a property list value
  !
  ! Inputs:  
  !		prp_id		- iproperty list identifier to modify
  !		name 		- name of property to modify
  ! Outputs:  
  !		value		- value of property
  !		hdferr:		- error code		
  !				 	Success:  0
  !				 	Failure: -1   
  ! Optional parameters:
  !				NONE
  !
  ! Programmer:	Elena Pourmal
  !	        October 9, 2002	
  !
  ! Modifications: 	
  !
  ! Comment:		
  !----------------------------------------------------------------------

  SUBROUTINE h5pget_double(prp_id, name, value, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pget_double
    !DEC$endif
    !
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier 
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to modify
    DOUBLE PRECISION,   INTENT(OUT), TARGET :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)

  END SUBROUTINE h5pget_double

  !----------------------------------------------------------------------
  ! Name:		h5pget_char
  !
  ! Purpose: 	Gets a property list value
  !
  ! Inputs:  
  !		prp_id		- iproperty list identifier to modify
  !		name 		- name of property to modify
  ! Outputs:  
  !		value		- value of property
  !		hdferr:		- error code		
  !				 	Success:  0
  !				 	Failure: -1   
  ! Optional parameters:
  !				NONE
  !
  ! Programmer:	Elena Pourmal
  !	        October 9, 2002	
  !
  ! Modifications: 	
  !
  ! Comment:		
  !----------------------------------------------------------------------

  SUBROUTINE h5pget_char(prp_id, name, value, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pget_char
    !DEC$endif
    !
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier 
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to modify
    CHARACTER(LEN=*), INTENT(OUT), TARGET :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)

  END SUBROUTINE h5pget_char

  SUBROUTINE h5pset_ptr(prp_id, name, value, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pset_ptr
    !DEC$endif
    !
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier 
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to modify
    TYPE(C_PTR), INTENT(OUT) :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
    INTEGER :: name_len

    name_len = LEN(name)
    hdferr = h5pset_c(prp_id, name, name_len, value)

  END SUBROUTINE h5pset_ptr

  SUBROUTINE h5pget_ptr(prp_id, name, value, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pget_ptr
    !DEC$endif
    !
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier 
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to modify
    TYPE(C_PTR), INTENT(OUT) :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
    INTEGER :: name_len

    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, value)

  END SUBROUTINE h5pget_ptr


  !----------------------------------------------------------------------
  ! Name:		h5pregister_integer
  !
  ! Purpose: 	Registers a permanent property with a property list class.
  !
  ! Inputs:  
  !		class		- property list class to register 
  !                                 permanent property within
  !		name 		- name of property to register
  !               size            - size of property in bytes
  !		value		- default value for property in newly 
  !                                 created property lists
  ! Outputs:  
  !		hdferr:		- error code		
  !				 	Success:  0
  !				 	Failure: -1   
  ! Optional parameters:
  !				NONE
  !
  ! Programmer:	Elena Pourmal
  !	        October 10, 2002	
  !
  ! Modifications: 	
  !
  ! Comment:		
  !----------------------------------------------------------------------

  SUBROUTINE h5pregister_integer(class, name, size, value, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pregister_integer
    !DEC$endif
    !
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: class   ! Property list class identifier 
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to register
    INTEGER(SIZE_T), INTENT(IN) :: size   ! Size of the property value	
    INTEGER,   INTENT(IN), TARGET :: value        ! Property value
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pregister_c(class, name, name_len, size, f_ptr)

  END SUBROUTINE h5pregister_integer

  !----------------------------------------------------------------------
  ! Name:		h5pregister_real
  !
  ! Purpose: 	Registers a permanent property with a property list class.
  !
  ! Inputs:  
  !		class		- property list class to register 
  !                                 permanent property within
  !		name 		- name of property to register
  !               size            - size of property in bytes
  !		value		- default value for property in newly 
  !                                 created property lists
  ! Outputs:  
  !		hdferr:		- error code		
  !				 	Success:  0
  !				 	Failure: -1   
  ! Optional parameters:
  !				NONE
  !
  ! Programmer:	Elena Pourmal
  !	        October 10, 2002	
  !
  ! Modifications: 	
  !
  ! Comment:		
  !----------------------------------------------------------------------

  SUBROUTINE h5pregister_real(class, name, size, value, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pregister_real
    !DEC$endif
    !
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: class   ! Property list class identifier 
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to register
    INTEGER(SIZE_T), INTENT(IN) :: size   ! size of the property value	
    REAL,   INTENT(IN), TARGET :: value           ! Property value
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pregister_c(class, name, name_len, size, f_ptr)

  END SUBROUTINE h5pregister_real

  !----------------------------------------------------------------------
  ! Name:		h5pregister_double
  !
  ! Purpose: 	Registers a permanent property with a property list class.
  !
  ! Inputs:  
  !		class		- property list class to register 
  !                                 permanent property within
  !		name 		- name of property to register
  !               size            - size of property in bytes
  !		value		- default value for property in newly 
  !                                 created property lists
  ! Outputs:  
  !		hdferr:		- error code		
  !				 	Success:  0
  !				 	Failure: -1   
  ! Optional parameters:
  !				NONE
  !
  ! Programmer:	Elena Pourmal
  !	        October 10, 2002	
  !
  ! Modifications: 	
  !
  ! Comment:		
  !----------------------------------------------------------------------

  SUBROUTINE h5pregister_double(class, name, size, value, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pregister_double
    !DEC$endif
    !
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: class   ! Property list class identifier 
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to register
    INTEGER(SIZE_T), INTENT(IN) :: size  ! size of the property value	
    DOUBLE PRECISION,   INTENT(IN), TARGET :: value        ! Property value
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pregister_c(class, name, name_len, size, f_ptr)
  END SUBROUTINE h5pregister_double

  !----------------------------------------------------------------------
  ! Name:		h5pregister_char
  !
  ! Purpose: 	Registers a permanent property with a property list class.
  !
  ! Inputs:  
  !		class		- property list class to register 
  !                                 permanent property within
  !		name 		- name of property to register
  !               size            - size of property in bytes
  !		value		- default value for property in newly 
  !                                 created property lists
  ! Outputs:  
  !		hdferr:		- error code		
  !				 	Success:  0
  !				 	Failure: -1   
  ! Optional parameters:
  !				NONE
  !
  ! Programmer:	Elena Pourmal
  !	        October 10, 2002	
  !
  ! Modifications: 	
  !
  ! Comment:		
  !----------------------------------------------------------------------

  SUBROUTINE h5pregister_char(class, name, size, value, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pregister_char
    !DEC$endif
    !
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: class   ! Property list class identifier 
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to register
    INTEGER(SIZE_T), INTENT(IN) :: size  ! size of the property value	
    CHARACTER(LEN=*),   INTENT(IN), TARGET :: value        ! Property value
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pregister_c(class, name, name_len, size, f_ptr)
  END SUBROUTINE h5pregister_char

  !----------------------------------------------------------------------
  ! Name:		h5pregister_ptr
  !
  ! Purpose: 	Registers a permanent property with a property list class.
  !
  ! Inputs:  
  !		class		- property list class to register 
  !                                 permanent property within
  !		name 		- name of property to register
  !             size            - size of property in bytes
  !		value		- default value for property in newly 
  !                                 created property lists
  ! Outputs:  
  !		hdferr:		- error code		
  !				 	Success:  0
  !				 	Failure: -1   
  ! Optional parameters:
  !				NONE
  !
  ! Programmer:	M.S. Breitenfeld
  !	        June 24, 2008	
  !
  ! Modifications: 	
  !
  ! Comment:		
  !----------------------------------------------------------------------

  SUBROUTINE h5pregister_ptr(class, name, size, value, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pregister_ptr
    !DEC$endif
    !
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: class   ! Property list class identifier 
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to register
    INTEGER(SIZE_T), INTENT(IN) :: size   ! Size of the property value	
    TYPE(C_PTR), INTENT(IN) :: value      ! Property value
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
    INTEGER :: name_len

    name_len = LEN(name)
    hdferr = h5pregister_c(class, name, name_len, size, value)
  END SUBROUTINE h5pregister_ptr

  !----------------------------------------------------------------------
  ! Name:		h5pinsert_integer
  !
  ! Purpose: 	Registers a temporary property with a property list class.
  !
  ! Inputs:  
  !		plist		- property list identifier
  !		name 		- name of property to insert
  !               size            - size of property in bytes
  !		value		- initial value for the property 
  ! Outputs:  
  !		hdferr:		- error code		
  !				 	Success:  0
  !				 	Failure: -1   
  ! Optional parameters:
  !				NONE
  !
  ! Programmer:	Elena Pourmal
  !	        October 10, 2002	
  !
  ! Modifications: 	
  !
  ! Comment:		
  !----------------------------------------------------------------------

  SUBROUTINE h5pinsert_integer(plist, name, size, value, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pinsert_integer
    !DEC$endif
    !
    USE iso_c_binding
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist   ! Property list identifier 
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to insert 
    INTEGER(SIZE_T), INTENT(IN) :: size   ! Size of the property value	
    INTEGER,   INTENT(IN), TARGET :: value        ! Property value
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
    INTEGER :: name_len
    TYPE(c_ptr) :: f_ptr

    f_ptr = c_loc(value)

    name_len = LEN(name)
    hdferr = h5pinsert_c(plist, name , name_len, size, f_ptr)
  END SUBROUTINE h5pinsert_integer

  !----------------------------------------------------------------------
  ! Name:		h5pinsert_real
  !
  ! Purpose: 	Registers a temporary property with a property list class.
  !
  ! Inputs:  
  !		plist		- property list identifier
  !                                 permanent property within
  !		name 		- name of property to insert
  !               size            - size of property in bytes
  !		value		- initial value for the property 
  ! Outputs:  
  !		hdferr:		- error code		
  !				 	Success:  0
  !				 	Failure: -1   
  ! Optional parameters:
  !				NONE
  !
  ! Programmer:	Elena Pourmal
  !	        October 10, 2002	
  !
  ! Modifications: 	
  !
  ! Comment:		
  !----------------------------------------------------------------------

  SUBROUTINE h5pinsert_real(plist, name, size, value, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pinsert_real
    !DEC$endif
    !
    USE iso_c_binding
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist   ! Property list identifier 
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to insert 
    INTEGER(SIZE_T), INTENT(IN) :: size   ! Size of the property value	
    REAL,   INTENT(IN) :: value           ! Property value
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
    INTEGER :: name_len
    TYPE(c_ptr) :: f_ptr

    f_ptr = c_loc(value)

    name_len = LEN(name)
    hdferr = h5pinsert_c(plist, name , name_len, size, f_ptr)

  END SUBROUTINE h5pinsert_real

  !----------------------------------------------------------------------
  ! Name:		h5pinsert_double
  !
  ! Purpose: 	Registers a temporary property with a property list class.
  !
  ! Inputs:  
  !		plist		- property list identifier
  !                                 permanent property within
  !		name 		- name of property to insert
  !               size            - size of property in bytes
  !		value		- initial value for the property 
  ! Outputs:  
  !		hdferr:		- error code		
  !				 	Success:  0
  !				 	Failure: -1   
  ! Optional parameters:
  !				NONE
  !
  ! Programmer:	Elena Pourmal
  !	        October 10, 2002	
  !
  ! Modifications: 	
  !
  ! Comment:		
  !----------------------------------------------------------------------

  SUBROUTINE h5pinsert_double(plist, name, size, value, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pinsert_double
    !DEC$endif
    !
    USE iso_c_binding
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist   ! Property list identifier 
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to insert 
    INTEGER(SIZE_T), INTENT(IN) :: size   ! Size of the property value	
    DOUBLE PRECISION, INTENT(IN), TARGET :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
    INTEGER :: name_len
    TYPE(c_ptr) :: f_ptr

    f_ptr = c_loc(value)

    name_len = LEN(name)
    hdferr = h5pinsert_c(plist, name , name_len, size, f_ptr)

  END SUBROUTINE h5pinsert_double

  !----------------------------------------------------------------------
  ! Name:		h5pinsert_char
  !
  ! Purpose: 	Registers a temporary property with a property list class.
  !
  ! Inputs:  
  !		plist		- property list identifier
  !                                 permanent property within
  !		name 		- name of property to insert
  !               size            - size of property in bytes
  !		value		- initial value for the property 
  ! Outputs:  
  !		hdferr:		- error code		
  !				 	Success:  0
  !				 	Failure: -1   
  ! Optional parameters:
  !				NONE
  !
  ! Programmer:	Elena Pourmal
  !	        October 10, 2002	
  !
  ! Modifications: 	
  !
  ! Comment:		
  !----------------------------------------------------------------------

  SUBROUTINE h5pinsert_char(plist, name, size, value, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pinsert_char
    !DEC$endif
    !
    USE iso_c_binding
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist      ! Property list identifier 
    CHARACTER(LEN=*), INTENT(IN) :: name     ! Name of property to insert 
    INTEGER(SIZE_T), INTENT(IN) :: size      ! Size of property value	
    CHARACTER(LEN=*),   INTENT(IN), TARGET :: value  ! Property value
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    INTEGER :: name_len
    TYPE(c_ptr) :: f_ptr

    f_ptr = c_loc(value)

    name_len = LEN(name)
    hdferr = h5pinsert_c(plist, name , name_len, size, f_ptr)

  END SUBROUTINE h5pinsert_char

  !----------------------------------------------------------------------
  ! Name:		h5pinsert_ptr
  !
  ! Purpose: 	Registers a temporary property with a property list class.
  !
  ! Inputs:  
  !		plist		- property list identifier
  !                                 permanent property within
  !		name 		- name of property to insert
  !             size            - size of property in bytes
  !		value		- initial value for the property 
  ! Outputs:  
  !		hdferr:		- error code		
  !				 	Success:  0
  !				 	Failure: -1   
  ! Optional parameters:
  !				NONE
  !
  ! Programmer:	M.S. Breitenfeld
  !	        June 24, 2008
  !
  ! Modifications: 	
  !
  ! Comment:		
  !----------------------------------------------------------------------

  SUBROUTINE h5pinsert_ptr(plist, name, size, value, hdferr) 
    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5pinsert_ptr
    !DEC$endif
    !
    USE iso_c_binding
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist      ! Property list identifier 
    CHARACTER(LEN=*), INTENT(IN) :: name     ! Name of property to insert 
    INTEGER(SIZE_T), INTENT(IN) :: size      ! Size of property value	
    TYPE(c_ptr),   INTENT(IN) :: value  ! Property value
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    INTEGER :: name_len

    name_len = LEN(name)
    hdferr = h5pinsert_c(plist, name , name_len, size, value)
  END SUBROUTINE h5pinsert_ptr

END MODULE H5P_F03

