#							-*- shell-script -*-
#
# This file is part of the HDF5 build script.  It is processed shortly
# after configure starts and defines, among other things, flags for
# the various compile modes.
#
# See BlankForm in this directory for detailed information.

# The default compiler is `cc'
if test "X-" =  "X-$CC"; then
    CC=cc
    CC_BASENAME=cc
fi

# Architecture dependent flags
ARCH=${ARCH:='-arch host -tune host'}

# Try compiler flags.
. ./config/gnu-flags
. ./config/dec-flags
