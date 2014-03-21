
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

add_test (NAME testhdf5_fortran COMMAND $<TARGET_FILE:testhdf5_fortran>)
SET_TESTS_PROPERTIES(testhdf5_fortran PROPERTIES PASS_REGULAR_EXPRESSION "[ ]*0 error.s")

#-- Adding test for testhdf5_fortran_1_8
add_test (NAME testhdf5_fortran_1_8 COMMAND $<TARGET_FILE:testhdf5_fortran_1_8>)
SET_TESTS_PROPERTIES(testhdf5_fortran_1_8 PROPERTIES PASS_REGULAR_EXPRESSION "[ ]*0 error.s")

#-- Adding test for fortranlib_test_F03
if (HDF5_ENABLE_F2003)
  add_test (NAME fortranlib_test_F03 COMMAND $<TARGET_FILE:fortranlib_test_F03>)
  SET_TESTS_PROPERTIES(fortranlib_test_F03 PROPERTIES PASS_REGULAR_EXPRESSION "[ ]*0 error.s")
endif (HDF5_ENABLE_F2003)

#-- Adding test for fflush1
add_test (NAME fflush1 COMMAND $<TARGET_FILE:fflush1>)

#-- Adding test for fflush2
add_test (NAME fflush2 COMMAND $<TARGET_FILE:fflush2>)
SET_TESTS_PROPERTIES(fflush2 PROPERTIES DEPENDS fflush1)
