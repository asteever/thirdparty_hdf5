#! /bin/bash
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
# Tests for the swmr feature.
#
# Created:
#   Albert Cheng, 2009/07/22


###############################################################################
## test parameters
###############################################################################

Nreaders=5              # number of readers to launch
Nrdrs_spa=3             # number of sparse readers to launch
Nrecords=200000         # number of records to write
Nrecs_rem=40000         # number of times to shrink
Nrecs_spa=20000         # number of records to write in the sparse test
Nsecs_add=5             # number of seconds per read interval
Nsecs_rem=3             # number of seconds per read interval
Nsecs_addrem=8          # number of seconds per read interval
nerrors=0

###############################################################################
## definitions for message file to coordinate test runs
###############################################################################
WRITER_MESSAGE=SWMR_WRITER_MESSAGE      # The message file created by writer that the open is complete
                                        # This should be the same as the define in "./swmr_common.h"
MESSAGE_TIMEOUT=300                     # Message timeout length in secs
                                        # This should be the same as the define in "./h5test.h"

###############################################################################
## short hands and function definitions
###############################################################################
DPRINT=:                # Set to "echo Debug:" for debugging printing,
                        # else ":" for noop.
IFDEBUG=:               # Set to null to turn on debugging, else ":" for noop.

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Testing".
#
TESTING() {
   SPACES="                                                               "
   echo "Testing $* $SPACES" | cut -c1-70 | tr -d '\012'
}

# To wait for the writer message file or till the maximum # of seconds is reached
# $1 is the message file to wait for
# This performs similar function as the routine h5_wait_message() in test/h5test.c
WAIT_MESSAGE() {
    message=$1                                  # Get the name of the message file to wait for
    t0=`date +%s`                               # Get current time in seconds
    difft=0                                     # Initialize the time difference
    mexist=0                                    # Indicate whether the message file is found
    while [ $difft -lt $MESSAGE_TIMEOUT ] ;     # Loop till message times out
    do
        t1=`date +%s`                           # Get current time in seconds
        difft=`expr $t1 - $t0`                  # Calculate the time difference
        if [ -e $message ]; then                # If message file is found:
            mexist=1                            #       indicate the message file is found
            rm $message                         #       remove the message file
            break                               #       get out of the while loop
        fi
    done;
    if test $mexist -eq 0; then
        # Issue warning that the writer message file is not found, continue with launching the reader(s)
        echo warning: $WRITER_MESSAGE is not found after waiting $MESSAGE_TIMEOUT seconds
    else
        echo $WRITER_MESSAGE is found
    fi
}

###############################################################################
## Main
##
## Modifications:
##   Vailin Choi; July 2013
##     Add waiting of message file before launching the reader(s).
##     Due to the implementation of file locking, coordination
##     is needed in file opening for the writer/reader tests
##     to proceed as expected.
##
###############################################################################
# The build (current) directory might be different than the source directory.
if test -z "$srcdir"; then
   srcdir=.
fi

# Parse option
#   -p   run ph5diff tests
#   -h   print help page
while [ $# -gt 0 ]; do
    case "$1" in
    *)  # unknown option
        echo "$0: Unknown option ($1)"
        exit 1
        ;;
    esac
done

# Loop over index types
for index_type in "-i ea" "-i b2" 
do
    # Try with and without compression
    for compress in "" "-c 5"
    do
        echo
        echo "###############################################################################"
        echo "## Generator test"
        echo "###############################################################################"
        # Launch the Generator without SWMR_WRITE
        echo launch the swmr_generator
        ./swmr_generator $compress $index_type
        if test $? -ne 0; then
            echo generator had error
            nerrors=`expr $nerrors + 1`
        fi

        # Launch the Generator with SWMR_WRITE
        echo launch the swmr_generator with SWMR_WRITE
        ./swmr_generator -s $compress $index_type
        if test $? -ne 0; then
            echo generator had error
            nerrors=`expr $nerrors + 1`
        fi

        # Check for error and exit if one occured
        $DPRINT nerrors=$nerrors
        if test $nerrors -ne 0 ; then
            echo "SWMR tests failed with $nerrors errors."
            exit 1
        fi

        echo
        echo "###############################################################################"
        echo "## Use H5Fstart_swmr_write() to enable SWMR writing mode"
        echo "###############################################################################"

	# Remove any possible writer message file before launching writer
        rm -f $WRITER_MESSAGE
        #
        # Launch the Writer
        echo launch the swmr_start_writer
        seed="" # Put -r <random seed> command here
        ./swmr_start_write $compress $index_type $Nrecords $seed &
        pid_writer=$!
        $DPRINT pid_writer=$pid_writer
	#
        # Wait for message from writer process before starting reader(s)
        WAIT_MESSAGE $WRITER_MESSAGE

        #
        # Launch the Readers
        #declare -a seeds=(<seed1> <seed2> <seed3> ... )
        echo launch $Nreaders swmr_readers
        pid_readers=""
        n=0
        while [ $n -lt $Nreaders ]; do
            #seed="-r ${seeds[$n]}"
            seed=""
            ./swmr_reader $Nsecs_add $seed &
            pid_readers="$pid_readers $!"
            n=`expr $n + 1`
        done
        $DPRINT pid_readers=$pid_readers
        $IFDEBUG ps

        # Collect exit code of the readers first because they usually finish
        # before the writer.
        for xpid in $pid_readers; do
            $DPRINT checked reader $xpid
            wait $xpid
            if test $? -ne 0; then
                echo reader had error
                nerrors=`expr $nerrors + 1`
            fi
        done

        # Collect exit code of the writer
        $DPRINT checked writer $pid_writer
        wait $pid_writer
        if test $? -ne 0; then
            echo writer had error
            nerrors=`expr $nerrors + 1`
        fi

        # Check for error and exit if one occured
        $DPRINT nerrors=$nerrors
        if test $nerrors -ne 0 ; then
            echo "SWMR tests failed with $nerrors errors."
            exit 1
        fi

        echo
        echo "###############################################################################"
        echo "## Writer test - test expanding the dataset"
        echo "###############################################################################"

        # Launch the Generator
        echo launch the swmr_generator
        ./swmr_generator $compress $index_type
        if test $? -ne 0; then
            echo generator had error
            nerrors=`expr $nerrors + 1`
        fi
	# Remove any possible writer message file before launching writer
        rm -f $WRITER_MESSAGE
        #
        # Launch the Writer
        echo launch the swmr_writer
        seed="" # Put -r <random seed> command here
        ./swmr_writer $Nrecords $seed &
        pid_writer=$!
        $DPRINT pid_writer=$pid_writer
	#
        # Wait for message from writer process before starting reader(s)
        WAIT_MESSAGE $WRITER_MESSAGE
        #
        # Launch the Readers
        #declare -a seeds=(<seed1> <seed2> <seed3> ... )
        echo launch $Nreaders swmr_readers
        pid_readers=""
        n=0
        while [ $n -lt $Nreaders ]; do
            #seed="-r ${seeds[$n]}"
            seed=""
            ./swmr_reader $Nsecs_add $seed &
            pid_readers="$pid_readers $!"
            n=`expr $n + 1`
        done
        $DPRINT pid_readers=$pid_readers
        $IFDEBUG ps

        # Collect exit code of the readers first because they usually finish
        # before the writer.
        for xpid in $pid_readers; do
            $DPRINT checked reader $xpid
            wait $xpid
            if test $? -ne 0; then
                echo reader had error
                nerrors=`expr $nerrors + 1`
            fi
        done

        # Collect exit code of the writer
        $DPRINT checked writer $pid_writer
        wait $pid_writer
        if test $? -ne 0; then
            echo writer had error
            nerrors=`expr $nerrors + 1`
        fi

        # Check for error and exit if one occured
        $DPRINT nerrors=$nerrors
        if test $nerrors -ne 0 ; then
            echo "SWMR tests failed with $nerrors errors."
            exit 1
        fi

        echo
        echo "###############################################################################"
        echo "## Remove test - test shrinking the dataset"
        echo "###############################################################################"
	#
        # Remove any possible writer message file before launching writer
        rm -f $WRITER_MESSAGE
        # Launch the Remove Writer
        echo launch the swmr_remove_writer
        seed="" # Put -r <random seed> command here
        ./swmr_remove_writer $Nrecs_rem $seed &
        pid_writer=$!
        $DPRINT pid_writer=$pid_writer
	#
        # Wait for message from writer process before starting reader(s)
        WAIT_MESSAGE $WRITER_MESSAGE
        #
        # Launch the Remove Readers
        #declare -a seeds=(<seed1> <seed2> <seed3> ... )
        n=0
        pid_readers=""
        echo launch $Nreaders swmr_remove_readers
        while [ $n -lt $Nreaders ]; do
            #seed="-r ${seeds[$n]}"
            seed=""
            ./swmr_remove_reader $Nsecs_rem $seed &
            pid_readers="$pid_readers $!"
            n=`expr $n + 1`
        done
        $DPRINT pid_readers=$pid_readers
        $IFDEBUG ps

        # Collect exit code of the readers first because they usually finish
        # before the writer.
        for xpid in $pid_readers; do
            $DPRINT checked reader $xpid
            wait $xpid
            if test $? -ne 0; then
                echo reader had error
                nerrors=`expr $nerrors + 1`
            fi
        done

        # Collect exit code of the writer
        $DPRINT checked writer $pid_writer
        wait $pid_writer
        if test $? -ne 0; then
            echo writer had error
            nerrors=`expr $nerrors + 1`
        fi

        # Check for error and exit if one occured
        $DPRINT nerrors=$nerrors
        if test $nerrors -ne 0 ; then
            echo "SWMR tests failed with $nerrors errors."
            exit 1
        fi

        echo
        echo "###############################################################################"
        echo "## Add/remove test - randomly grow or shrink the dataset"
        echo "###############################################################################"

        # Launch the Generator
        echo launch the swmr_generator
        ./swmr_generator $compress $index_type
        if test $? -ne 0; then
            echo generator had error
            nerrors=`expr $nerrors + 1`
        fi

        # Launch the Writer (not in parallel - just to rebuild the datasets)
        echo launch the swmr_writer
        seed="" # Put -r <random seed> command here
        ./swmr_writer $Nrecords $seed
        if test $? -ne 0; then
            echo writer had error
            nerrors=`expr $nerrors + 1`
        fi
	#
        # Remove any possible writer message file before launching writer
        rm -f $WRITER_MESSAGE
        #
        # Launch the Add/Remove Writer
        echo launch the swmr_addrem_writer
        seed="" # Put -r <random seed> command here
        ./swmr_addrem_writer $Nrecords $seed &
        pid_writer=$!
        $DPRINT pid_writer=$pid_writer
	#
        # Wait for message from writer process before starting reader(s)
        WAIT_MESSAGE $WRITER_MESSAGE
        #
        # Launch the Add/Remove Readers
        #declare -a seeds=(<seed1> <seed2> <seed3> ... )
        n=0
        pid_readers=""
        echo launch $Nreaders swmr_remove_readers
        while [ $n -lt $Nreaders ]; do
            #seed="-r ${seeds[$n]}"
            seed=""
            ./swmr_remove_reader $Nsecs_addrem $seed &
            pid_readers="$pid_readers $!"
            n=`expr $n + 1`
        done
        $DPRINT pid_readers=$pid_readers
        $IFDEBUG ps

        # Collect exit code of the readers first because they usually finish
        # before the writer.
        for xpid in $pid_readers; do
            $DPRINT checked reader $xpid
            wait $xpid
            if test $? -ne 0; then
                echo reader had error
                nerrors=`expr $nerrors + 1`
            fi
        done

        # Collect exit code of the writer
        $DPRINT checked writer $pid_writer
        wait $pid_writer
        if test $? -ne 0; then
            echo writer had error
            nerrors=`expr $nerrors + 1`
        fi

        # Check for error and exit if one occured
        $DPRINT nerrors=$nerrors
        if test $nerrors -ne 0 ; then
            echo "SWMR tests failed with $nerrors errors."
            exit 1
        fi

        echo
        echo "###############################################################################"
        echo "## Sparse writer test - test writing to random locations in the dataset"
        echo "###############################################################################"

        # Launch the Generator
        # NOTE: Random seed is shared between readers and writers and is
        #       created by the generator.
        echo launch the swmr_generator
        seed="" # Put -r <random seed> command here
        ./swmr_generator $compress $index_type $seed
        if test $? -ne 0; then
            echo generator had error
            nerrors=`expr $nerrors + 1`
        fi
	#
        # Remove any possible writer message file before launching writer
        rm -f $WRITER_MESSAGE
        # Launch the Sparse writer
        echo launch the swmr_sparse_writer
        nice -n 20 ./swmr_sparse_writer $Nrecs_spa &
        pid_writer=$!
        $DPRINT pid_writer=$pid_writer
	#
        # Wait for message from writer process before starting reader(s)
        WAIT_MESSAGE $WRITER_MESSAGE
        #
        # Launch the Sparse readers
        n=0
        pid_readers=""
        echo launch $Nrdrs_spa swmr_sparse_readers
        while [ $n -lt $Nrdrs_spa ]; do
            # The sparse writer spits out a LOT of data so it's set to 'quiet'
            ./swmr_sparse_reader -q $Nrecs_spa &
            pid_readers="$pid_readers $!"
            n=`expr $n + 1`
        done
        $DPRINT pid_readers=$pid_readers
        $IFDEBUG ps

        # Collect exit code of the writer
        $DPRINT checked writer $pid_writer
        wait $pid_writer
        if test $? -ne 0; then
            echo writer had error
            nerrors=`expr $nerrors + 1`
        fi

        # Collect exit code of the readers
        for xpid in $pid_readers; do
            $DPRINT checked reader $xpid
            wait $xpid
            if test $? -ne 0; then
                echo reader had error
                nerrors=`expr $nerrors + 1`
            fi
        done

        # Check for error and exit if one occured
        $DPRINT nerrors=$nerrors
        if test $nerrors -ne 0 ; then
            echo "SWMR tests failed with $nerrors errors."
            exit 1
        fi
    done
done

###############################################################################
## Report and exit
###############################################################################

$DPRINT nerrors=$nerrors
if test $nerrors -eq 0 ; then
    echo "SWMR tests passed."
    exit 0
else
    echo "SWMR tests failed with $nerrors errors."
    exit 1
fi
