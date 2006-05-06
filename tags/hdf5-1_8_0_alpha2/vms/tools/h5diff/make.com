$!#
$!# Copyright by the Board of Trustees of the University of Illinois.
$!# All rights reserved.
$!#
$!# This file is part of HDF5.  The full HDF5 copyright notice, including
$!# terms governing use, modification, and redistribution, is contained in
$!# the files COPYING and Copyright.html.  COPYING can be found at the root
$!# of the source code distribution tree; Copyright.html can be found at the
$!# root level of an installed copy of the electronic HDF5 document set and
$!# is linked from the top-level documents page.  It can also be found at
$!# http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have
$!# access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu.
$!#
$! Makefile for VAX/VMX systems.
$!
$! Make h5diff tool 
$!
$! ccopt = "/float=ieee_float"
$
$ ccc := cc 'ccopt /include=([-.-.src], [-.lib])
$ type sys$input
       Creating h5diff  
$!
$ cobj= "h5diff_main, h5diff_common, h5diffgentest"

$!                               
$ ccc 'cobj 
$ type sys$input
       Creating h5diffgentest
$ link/exe=h5diffgentest.exe -
           h5diffgentest, -
           [-.lib]libh5tools.olb/lib,[-.-.src]hdf5.olb/lib 
$ type sys$input
       Created  h5difftest
$
$ type sys$input
       Creating h5diff
$ link/exe=h5diff.exe -
           h5diff_main, h5diff_common, -
           [-.lib]libh5tools.olb/lib,[-.-.src]hdf5.olb/lib
!               sys$clib/lib
$ type sys$input
       Created  h5diff
$!
$ exit
