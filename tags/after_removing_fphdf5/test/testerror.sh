#! /bin/sh
#
# Copyright by the Board of Trustees of the University of Illinois.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the files COPYING and Copyright.html.  COPYING can be found at the root
# of the source code distribution tree; Copyright.html can be found at the
# root level of an installed copy of the electronic HDF5 document set and
# is linked from the top-level documents page.  It can also be found at
# http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have
# access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu.
#
# Tests for test_error and err_compat 

CMP='cmp -s'
DIFF='diff -c'

nerrors=0
verbose=yes

# The build (current) directory might be different than the source directory.
if test -z "$srcdir"; then
   srcdir=.
fi

test -d ./testfiles || mkdir ./testfiles

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Testing".
#
TESTING() {
   SPACES="                                                               "
   echo "Testing $* $SPACES" | cut -c1-70 | tr -d '\012'
}

# Run a test and print PASS or *FAIL*.  If a test fails then increment
# the `nerrors' global variable and (if $verbose is set) display the
# difference between the actual output and the expected output. The
# expected output is given as the first argument to this function and
# the actual output file is calculated by replacing the `.ddl' with
# `.out'.  The actual output is not removed if $HDF5_NOCLEANUP has a
# non-zero value.
#
TEST() {
   TEST_ERR=$1                  # The test name
   TEST_ERR_BIN=`pwd`/$TEST_ERR # The path of the test binary

   expect1="$srcdir/testfiles/$1_1"
   expect2="$srcdir/testfiles/$1_2"
   actual="./`basename $1`.out"
   actual_err="./`basename $1`.err"
   actual_ext="./`basename $1`.ext"
   shift

   # Run test.
   TESTING $TEST_ERR
   (
      echo "#############################"
      echo "Expected output for $TEST_ERR" 
      echo "#############################"
      $RUNSERIAL $TEST_ERR_BIN 
   ) >$actual 2>$actual_err
   # Extract file name, line number, version and thread IDs because they may be different
   sed -e 's/thread [0-9]*/thread (IDs)/' -e 's/: .*\.c /: (file name) /' \
	-e 's/line [0-9]*/line (number)/' \
	-e 's/[1-9]*\.[0-9]*\.[0-9]*[^)]*/version (number)/' \
	$actual_err > $actual_ext
   cat $actual_ext >> $actual
    
   if $CMP $expect1 $actual; then
      echo " PASSED"
   elif $CMP $expect2 $actual; then
      echo " PASSED"
   else
      echo "*FAILED*"
      echo "    Expected result differs from actual result"
      nerrors="`expr $nerrors + 1`"
      test yes = "$verbose" && $DIFF $expect1 $actual |sed 's/^/    /'
   fi

   # Clean up output file
   if test -z "$HDF5_NOCLEANUP"; then
      rm -f $actual $actual_err $actual_ext
   fi
}

##############################################################################
##############################################################################
###			  T H E   T E S T S                                ###
##############################################################################
##############################################################################

# test for err_compat
TEST err_compat

# test for error_test
TEST error_test

if test $nerrors -eq 0 ; then
   echo "All Error API tests passed."
fi

exit $nerrors
