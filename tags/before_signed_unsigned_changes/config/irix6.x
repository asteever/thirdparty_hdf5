#							-*- shell-script -*-
#
# This file is part of the HDF5 build script.  It is processed shortly
# after configure starts and defines, among other things, flags for
# the various compile modes.
#
# See BlankForm in this directory for details.

# Use SGI supplied C compiler by default.  There is no ranlib
if test "X-" = "X-$CC"; then
    CC='cc'
    CC_BASENAME=cc
    # use c99 compiler if available.
    if `c99 -version >/dev/null 2>&1` ; then
        CC='c99'
    fi
fi
RANLIB=:

# Compiler flags
case "X-$CC_BASENAME" in
  X-gcc)
    . $srcdir/config/gnu-flags
    ;;

  *)
    if [ "$CC_BASENAME" = "cc" ] || ($CC -version 2>&1 | grep -s "MIPSpro Compilers") 2>&1 > /dev/null; then
      # use these flags if this is the SGI cc compiler or some compiler
      # command that eventually uses the SGI cc compiler.

      # Check for old versions of the compiler that don't work right.
      case "`$CC -version 2>&1 |head -1`" in
        "Mongoose Compilers: Version 7.00")
          echo "  +---------------------------------------------------+"
          echo "  | You have an old version of cc (Mongoose Compilers |"
          echo "  | version 7.00).  Please upgrade to MIPSpro version |"
          echo "  | 7.2.1.2m (patches are available from the SGI web  |"
          echo "  | site).  The 7.00 version may generate incorrect   |"
          echo "  | code, especially when optimizations are enabled.  |"
          echo "  +---------------------------------------------------+"
          sleep 5
          ;;
      esac

      # Always turn off these compiler warnings for the -64 compiler:
      #    1174:  function declared but not used
      #    1196:  __vfork() (this is an SGI config problem)
      #    1209:  constant expressions
      #    1429:  the `long long' type is not standard
      #    1685:  turn off warnings about turning off invalid warnings
      #    3201:  remark - parameter not referenced
      #CFLAGS="$CFLAGS -woff 1174,1429,1209,1196,1685,3201"
      CFLAGS="$CFLAGS -woff 1209,3201"

      # Always turn off these compiler warnings for the old compiler:
      #    799:   the `long long' type is not standard
      #    803:   turn off warnings about turning off invalid warnings
      #    835:   __vfork() (this is an SGI config problem)
      #CFLAGS="$CFLAGS -woff 799,803,835"

      # Always turn off these loader warnings:
      # (notice the peculiar syntax)
      #      47:  branch instructions that degrade performance on R4000
      #      84:  a library is not used
      #      85:  duplicate definition preemption (from -lnsl)
      #     134:  duplicate weak definition preemption (from -lnsl)
      CFLAGS="$CFLAGS -Wl,-woff,47,-woff,84,-woff,85,-woff,134"
    fi

    # Extra debugging flags
    DEBUG_CFLAGS="-g -fullwarn"
    DEBUG_CPPFLAGS=

    # Extra production flags
    PROD_CFLAGS="-O -OPT:Olimit=0 -s"
    PROD_CPPFLAGS=

    # Extra profiling flags
    PROFILE_CFLAGS=
    PROFILE_CPPFLAGS=
    ;;
esac

# The default Fortran 90 compiler

#
# HDF5 integers
#
# 	R_LARGE is the number of digits for the bigest integer supported.
#	R_INTEGER is the number of digits in INTEGER
#
# (for the IRIX architechture)
#
R_LARGE=18
R_INTEGER=9
HSIZE_T='SELECTED_INT_KIND(R_LARGE)'
HSSIZE_T='SELECTED_INT_KIND(R_LARGE)'
HID_T='SELECTED_INT_KIND(R_INTEGER)'
SIZE_T='SELECTED_INT_KIND(R_LARGE)'
OBJECT_NAMELEN_DEFAULT_F=-1

if test "X-" = "X-$F9X"; then
  F9X="f90"
fi

if test "X-" = "X-$f9x_flags_set"; then
  F9XSUFFIXFLAG=""
  FSEARCH_DIRS=""
  FFLAGS="$FFLAGS -64 -mips4 -O -s"
  DEBUG_FFLAGS="-64 -mips4 -O -s"
  PROD_FFLAGS="-64 -mips4 -O -s"
  PROFILE_FFLAGS="-64 -mips4 -O -s"
  f9x_flags_set=yes
fi

# The default C++ compiler

# The default compiler is `MIPSpro CC'
if test -z "$CXX"; then
  CXX=CC
  CXX_BASENAME=CC
fi

# Try native compiler flags
if test -z "$cxx_flags_set"; then
  # -LANG:std required for std use; -ptused causes templates used to be
  # instantiated
  CPPFLAGS="$CPPFLAGS -LANG:std -ptused"

  # libCio is a default library, since libtool before 1.5 doesn't fully 
  # support C++ yet, default libraries must be explicitly specified.
  # A new macro is used for this temporary and specific task so it 
  # won't polute the existing configuration 
  DEFAULT_LIBS="-lCio"

  DEBUG_CXXFLAGS=-g
  DEBUG_CPPFLAGS=
  PROD_CXXFLAGS="-O -s"
  PROD_CPPFLAGS=
  PROFILE_CXXFLAGS=-xpg
  PROFILE_CPPFLAGS=
  cxx_flags_set=yes
fi

# Hard set flag to indicate that the 'unsigned long long' to floating-point
# value conversion are broken by the compilers (as of 4/27/04 - QAK)
hdf5_cv_sw_ulong_to_fp_bottom_bit_works=${hdf5_cv_sw_ulong_to_fp_bottom_bit_works='no'}

