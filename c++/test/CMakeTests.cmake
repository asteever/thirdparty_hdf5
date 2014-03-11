
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################
# Remove any output file left over from previous test run
add_test (
    NAME cpp_testhdf5-clear-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove 
            tattr_basic.h5
            tattr_compound.h5
            tattr_dtype.h5
            tattr_multi.h5
            tattr_scalar.h5
            tfattrs.h5
)

add_test (NAME cpp_testhdf5 COMMAND $<TARGET_FILE:cpp_testhdf5>)
set_tests_properties (cpp_testhdf5 PROPERTIES DEPENDS cpp_testhdf5-clear-objects)

if (HDF5_TEST_VFD)

  set (VFD_LIST
      sec2
      stdio
      core
      split
      multi
      family
  )
  
  if (DIRECT_VFD)
    set (VFD_LIST ${VFD_LIST} direct)
  endif (DIRECT_VFD)

  MACRO (ADD_VFD_TEST vfdname resultcode)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME VFD-${vfdname}-cpp_testhdf5-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove 
                  tattr_basic.h5
                  tattr_compound.h5
                  tattr_dtype.h5
                  tattr_multi.h5
                  tattr_scalar.h5
                  tfattrs.h5
      )
      add_test (
        NAME VFD-${vfdname}-cpp_testhdf5 
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:cpp_testhdf5>"
            -D "TEST_ARGS:STRING="
            -D "TEST_VFD:STRING=${vfdname}"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_OUTPUT=cpp_testhdf5"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -P "${HDF5_RESOURCES_DIR}/vfdTest.cmake"
      )
      set_tests_properties (VFD-${vfdname}-cpp_testhdf5 PROPERTIES DEPENDS VFD-${vfdname}-cpp_testhdf5-clear-objects)
    endif (NOT HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_VFD_TEST)
  
  # Run test with different Virtual File Driver
  foreach (vfd ${VFD_LIST})
    ADD_VFD_TEST (${vfd} 0)
  endforeach (vfd ${VFD_LIST})

endif (HDF5_TEST_VFD)
