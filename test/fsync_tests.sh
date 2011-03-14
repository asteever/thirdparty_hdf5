#! /bin/sh
#
# Copyright by The HDF Group.
# Copyright by the Board of Trustees of the University of Illinois.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the files COPYING and Copyright.html.  COPYING can be found at the root
# of the source code distribution tree; Copyright.html can be found at the
# root level of an installed copy of the electronic HDF5 document set and
# is linked from the top-level documents page.  It can also be found at
# http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have
# access to either file, you may request a copy from help@hdfgroup.org.
#
# Tests for journaling related marking and unmarking of HDF5 files.
#
# These tests used to be in cache_journal.c, but had to be moved 
# out as the tests require simulated crashes, and it is difficult to
# do this in a C program without using fork().

nerrors=0

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
# the `nerrors' global variable.
#
TEST() {
   TEST_ERR=$1                  # The test name
   TEST_ERR_BIN=`pwd`/$TEST_ERR # The path of the test binary
   TEST_DESC=$2
   TEST_SIZE=$3
   TEST_STDERR=fsync_test.stderr

   #Run the test:
   trap "" 6
   $RUNSERIAL $TEST_ERR_BIN $TEST_DESC setup $TEST_SIZE
   trap 6
   TESTING $TEST_DESC $TEST_SIZE bytes
   $RUNSERIAL $TEST_ERR_BIN $TEST_DESC check $TEST_SIZE > $TEST_STDERR
   if [ $? -eq 0 ] 
   then
      
      # it is possible that we are running on a machine that discards
      # our return code -- such as red storm.
      #
      # Thus check the contents of the stderr file before declaring a
      # pass.

      if [ `wc -c < $TEST_STDERR` = '0' ]
      then

         echo " PASSED"
    
      else

         echo "*FAILED*"
         nerrors="`expr $nerrors + 1`"

      fi
   else
      echo "*FAILED*"
      nerrors="`expr $nerrors + 1`"
   fi

   # delete the stderr file before we quit
   rm $TEST_STDERR
}

# Print a "SKIP" message
SKIP() {
    TESTING $@
    echo  " -SKIP-"
}
  
##############################################################################
##############################################################################
###			  T H E   T E S T S                                ###
##############################################################################
##############################################################################

echo "Tests to verify correct operation of the fsync and aio fsync calls"
echo "under various HDF5 file drivers.  Most tests involve abnormal exits."
echo "Thus the \"Aborted\" messages between tests are expected."


# sec2 SIO fsync tests:

TEST fsync_tests "generic_fsync_test sec2" "56"
TEST fsync_tests "generic_fsync_test sec2" "1048576"
TEST fsync_tests "generic_fsync_test sec2" "20000000"


# core SIO fsync tests:

TEST fsync_tests "generic_fsync_test core" "62"
TEST fsync_tests "generic_fsync_test core" "1048576"
TEST fsync_tests "generic_fsync_test core" "3000000"


# stdio SIO fsync tests:

TEST fsync_tests "generic_fsync_test stdio" "88"
TEST fsync_tests "generic_fsync_test stdio" "1048576"
TEST fsync_tests "generic_fsync_test stdio" "4000000"


# family SIO fsync tests:
#
# Note that the family file driver SIO fsync tests don't use file
# sizes below 32 MB.  We do this as the fsync test code sets the 
# family member size to 32 MB, and the family file driver open
# call sets the member size to the size of the first file in 
# the family.
#
# Under normal circumstances, this value is overwritten when the 
# super block is decoded (HDF 1.8 and up), but since these tests
# are working at the file driver level, this adjustment is not 
# made.  Rather than fiddle with the family file driver internal
# data structures, we go with the flow and do our tests file 
# total file sizes greater than 32 MB.
#					JRM -- 1/27/11

TEST fsync_tests "generic_fsync_test family" "33554432"
TEST fsync_tests "generic_fsync_test family" "33554433"
TEST fsync_tests "generic_fsync_test family" "40000000"
TEST fsync_tests "generic_fsync_test family" "67108863"
TEST fsync_tests "generic_fsync_test family" "67108864"
TEST fsync_tests "generic_fsync_test family" "100000000"


# multi SIO fsync tests
#
# The generic SIO fsync test doesn't exercise the fsync multi
# file driver call fully -- but it is a good first smoke check.

TEST fsync_tests "generic_fsync_test multi" "99"
TEST fsync_tests "generic_fsync_test multi" "1048576"
TEST fsync_tests "generic_fsync_test multi" "5000000"

# Now do multi file driver specific fsync tests.  This should
# exercise the multi file driver a bit more fully.

TEST fsync_tests "multi_fd_fsync_test multi" "99"
TEST fsync_tests "multi_fd_fsync_test multi" "1048576"
TEST fsync_tests "multi_fd_fsync_test multi" "5000000"








# sec2 AIO fsync poll tests:

TEST fsync_tests "generic_aio_fsync_poll_test sec2" "56"
TEST fsync_tests "generic_aio_fsync_poll_test sec2" "1048576"
TEST fsync_tests "generic_aio_fsync_poll_test sec2" "20000000"


# core AIO fsync poll tests:

TEST fsync_tests "generic_aio_fsync_poll_test core" "62"
TEST fsync_tests "generic_aio_fsync_poll_test core" "1048576"
TEST fsync_tests "generic_aio_fsync_poll_test core" "3000000"


# stdio AIO fsync poll tests:

TEST fsync_tests "generic_aio_fsync_poll_test stdio" "88"
TEST fsync_tests "generic_aio_fsync_poll_test stdio" "1048576"
TEST fsync_tests "generic_aio_fsync_poll_test stdio" "4000000"


# family AIO fsync poll tests:
#
# Note that the family file driver AIO fsync tests don't use file
# sizes below 32 MB.  We do this as the fsync test code sets the 
# family member size to 32 MB, and the family file driver open
# call sets the member size to the size of the first file in 
# the family.
#
# Under normal circumstances, this value is overwritten when the 
# super block is decoded (HDF 1.8 and up), but since these tests
# are working at the file driver level, this adjustment is not 
# made.  Rather than fiddle with the family file driver internal
# data structures, we go with the flow and do our tests file 
# total file sizes greater than 32 MB.
#					JRM -- 1/27/11

TEST fsync_tests "generic_aio_fsync_poll_test family" "33554432"
TEST fsync_tests "generic_aio_fsync_poll_test family" "33554433"
TEST fsync_tests "generic_aio_fsync_poll_test family" "40000000"
TEST fsync_tests "generic_aio_fsync_poll_test family" "67108863"
TEST fsync_tests "generic_aio_fsync_poll_test family" "67108864"
TEST fsync_tests "generic_aio_fsync_poll_test family" "100000000"


# multi AIO fsync poll tests
#
# The generic AIO fsync test doesn't exercise the fsync multi
# file driver call fully -- but it is a good first smoke check.

TEST fsync_tests "generic_aio_fsync_poll_test multi" "99"
TEST fsync_tests "generic_aio_fsync_poll_test multi" "1048576"
TEST fsync_tests "generic_aio_fsync_poll_test multi" "5000000"


# Now do multi file driver specific fsync tests.  This should
# exercise the multi file driver a bit more fully.

TEST fsync_tests "multi_fd_aio_fsync_poll_test multi" "111"
TEST fsync_tests "multi_fd_aio_fsync_poll_test multi" "1048576"
TEST fsync_tests "multi_fd_aio_fsync_poll_test multi" "5800000"


# sec2 AIO fsync wait tests:

TEST fsync_tests "generic_aio_fsync_wait_test sec2" "56"
TEST fsync_tests "generic_aio_fsync_wait_test sec2" "1048576"
TEST fsync_tests "generic_aio_fsync_wait_test sec2" "20000000"


# core AIO fsync poll tests:

TEST fsync_tests "generic_aio_fsync_wait_test core" "62"
TEST fsync_tests "generic_aio_fsync_wait_test core" "1048576"
TEST fsync_tests "generic_aio_fsync_wait_test core" "3000000"


# stdio AIO fsync poll tests:

TEST fsync_tests "generic_aio_fsync_wait_test stdio" "88"
TEST fsync_tests "generic_aio_fsync_wait_test stdio" "1048576"
TEST fsync_tests "generic_aio_fsync_wait_test stdio" "4000000"


# family AIO fsync poll tests:
#
# Note that the family file driver AIO fsync tests don't use file
# sizes below 32 MB.  We do this as the fsync test code sets the 
# family member size to 32 MB, and the family file driver open
# call sets the member size to the size of the first file in 
# the family.
#
# Under normal circumstances, this value is overwritten when the 
# super block is decoded (HDF 1.8 and up), but since these tests
# are working at the file driver level, this adjustment is not 
# made.  Rather than fiddle with the family file driver internal
# data structures, we go with the flow and do our tests file 
# total file sizes greater than 32 MB.
#					JRM -- 1/27/11

TEST fsync_tests "generic_aio_fsync_wait_test family" "33554432"
TEST fsync_tests "generic_aio_fsync_wait_test family" "33554433"
TEST fsync_tests "generic_aio_fsync_wait_test family" "40000000"
TEST fsync_tests "generic_aio_fsync_wait_test family" "67108863"
TEST fsync_tests "generic_aio_fsync_wait_test family" "67108864"
TEST fsync_tests "generic_aio_fsync_wait_test family" "100000000"


# multi AIO fsync poll tests
#
# The generic AIO fsync test doesn't exercise the fsync multi
# file driver call fully -- but it is a good first smoke check.

TEST fsync_tests "generic_aio_fsync_wait_test multi" "66"
TEST fsync_tests "generic_aio_fsync_wait_test multi" "1048576"
TEST fsync_tests "generic_aio_fsync_wait_test multi" "5200000"


# Now do multi file driver specific fsync tests.  This should
# exercise the multi file driver a bit more fully.

TEST fsync_tests "multi_fd_aio_fsync_wait_test multi" "88"
TEST fsync_tests "multi_fd_aio_fsync_wait_test multi" "1048576"
TEST fsync_tests "multi_fd_aio_fsync_wait_test multi" "5400000"


if test $nerrors -eq 0 ; then
   echo "All fsync tests passed."
fi

exit $nerrors
