#! /bin/sh -x
##
## Copyright by the Board of Trustees of the University of Illinois.
## All rights reserved.
##
## This file is part of HDF5.  The full HDF5 copyright notice, including
## terms governing use, modification, and redistribution, is contained in
## the files COPYING and Copyright.html.  COPYING can be found at the root
## of the source code distribution tree; Copyright.html can be found at the
## root level of an installed copy of the electronic HDF5 document set and
## is linked from the top-level documents page.  It can also be found at
## http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have
## access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu.
##
# Name: build_h5perf_alone.sh
#
# Puporse: Build the h5perf tool by standalone mode.
#
# Created: Albert Cheng, 2005/09/18
#
# Modification:
#

# Default to use h5pcc to build h5perf unless $H5CC is defined.
# 
h5pcc=${H5CC:-h5pcc}
$h5pcc -DSTANDALONE pio_perf.c pio_engine.c pio_timer.c -o h5perf
