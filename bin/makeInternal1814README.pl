#!/usr/bin/perl
# makeInternalREADME.pl
use warnings;
use strict;

my $section2="For information on compilers and settings used to build these HDF5 
libraries, please refer to:

    ./lib/libhdf5.settings

The contents of this directory are:

    COPYING     - Copyright notice
    README      - This file
    RELEASE.txt - Detailed information regarding this release
    bin/        - Directory containing HDF5 pre-compiled utilities
    include/    - Directory containing HDF5 include files
    lib/        - Directory containing HDF5 libraries and settings
    share/      - Directory containing HDF5 C, C++ and Fortran example
		  program code and scripts to test compile scripts
                  h5cc, h5c++ and h5fc

These binaries were built with the ZLIB and SZIP (version 2.1, Encoder 
ENABLED) external libraries which are included in the lib directory for 
convenience.

We also provide the ZLIB and SZIP source code on our ftp server at:

   ftp://ftp.hdfgroup.org/lib-external/

The official ZLIB and SZIP pages are at:

   ZLIB: http://www.zlib.net/
   SZIP: http://hdfgroup.org/doc_resource/SZIP/

";

my $section3 = "If using the shared libraries, you must add the HDF5 library path 
to the LD_LIBRARY_PATH variable.
";

my $section4 = "We provide scripts for compiling applications with the HDF5 libraries:

    bin/h5cc  - for C
    bin/h5fc  - for F90 (if Fortran 90 is included with the binaries)
    bin/h5c++ - for C++ (if C++ is included with the binaries)

After you have installed the binaries to their final destination,
you can use these scripts (h5cc, h5fc, h5c++) to compile.  However,
you must first:

  1) Run ./h5redeploy in the bin directory to change site specific paths in the scripts.

  2) Edit each script and update the paths for the external libraries
     in LDFLAGS and CPPFLAGS.

You may also need to change other variables in the scripts, depending
on how things are set up on your system.  Here are some of the variables
to check:

  prefix      - Path to the HDF5 top level installation directory
  CCBASE      - Name of the alternative C compiler
  CLINKERBASE - Name of the alternative linker
  LIBS        - Libraries your application will link with

For further details refer to the INSTALL files in the ./release_docs/
directory of the HDF5 source code:

    ftp://ftp.hdfgroup.org/HDF5/current/src/unpacked/release_docs/

Source code can be found on the THG ftp server in:

    ftp://ftp.hdfgroup.org/HDF5/current/src/

Please send questions, comments, and suggestions to:

    http://hdfgroup.org/about/contact.html 


";

my $indirectory = ".";
$indirectory = shift;
my $linktype = "shared";
if ($indirectory =~ /static/) {
   $linktype = "static";
}

my $version;

my $outfile = "$indirectory/README";
open OUTFILE, ">$outfile" or die "$!Couldn't open $outfile - check permissions for $indirectory\n";
my $hostname;
my $cmd = "grep \"HDF5 Version\" $indirectory/lib/libhdf5.settings";
$_ = `$cmd`;
#print OUTFILE $_, "\n";
s/HDF5 Version://;
s/^\s+//;
chomp;
$version = $_;
#print OUTFILE $_, "\n";

my $versionstring=  "This directory contains the $linktype binary distribution of HDF5-".$version;

$cmd = "grep \"Uname information:\" $indirectory/lib/libhdf5.settings";
$_ = `$cmd`;
s/Uname information://;
s/^\s+//;
#print OUTFILE $_;
chomp;
#s/(^\w+)(\s)(\S+)/$1/;
#s/(^.*)(-)(.*)(200[7-8])(.*)(\s)(\S+)/$1 $5/;
#my $platformstring = "\nthat was compiled on:" . $_ . " ";
my $platformstring = "";
my $hostnamestring = $_;
my @hostnamestring = split / /, $hostnamestring;
#print OUTFILE "Size of hostnamestring is ", scalar @hostnamestring, "\n";
#print OUTFILE $hostnamestring[0] . "\t" . $hostnamestring[2]."\t".$hostnamestring[19]."\n";
$hostname = $hostnamestring[1];
#my $size = scalar @hostnamestring;
if ($hostname =~ /loyalty/) {
   $platformstring = "\nthat was compiled on: " . $hostnamestring[0]." " . $hostnamestring[2]." " . $hostnamestring[-1] . " ";
}
elsif ($hostname =~ /freedom/) {
   $platformstring = "\nthat was compiled on: " . $hostnamestring[0]." " . $hostnamestring[2]." " . $hostnamestring[-1] . " ";
} elsif ($hostname =~ /emu/) {
   $platformstring = "\nthat was compiled on: " . $hostnamestring[0]." " . $hostnamestring[2] . " " . $hostnamestring[-2] . " ";
} elsif ($hostname =~ /fred/) {
   $platformstring = "\nthat was compiled on: " . $hostnamestring[0]." " . $hostnamestring[2] . " " . $hostnamestring[-1] . " ";
} else {
   $_ = $hostnamestring[2];
   my $pos = index $_, '-';
   my $os = substr $_, 0, $pos;
   $platformstring = "\nthat was compiled on: " . $hostnamestring[0] . " " . $os . " " . $hostnamestring[-2] . " ";
}


my $mappingstring = ""; 
#no mappingstring for 1.6.  Put it back for 1.8.
#$cmd = "grep \"Default API Mapping:\" $indirectory/lib/libhdf5.settings";
#$_ = `$cmd`;
#s/Default API Mapping://;
#s/^\s+//;
#chomp;
#if (/v\d+/) {
#   s/v//;
#   s/(\d)(\d)/$1\.$2/g;
#   $mappingstring = "using the default\nAPI mapping for VERSION ".$_.".";
#   
#}
print OUTFILE $versionstring;
print OUTFILE $platformstring."\n\n";
#print OUTFILE $mappingstring;

#if ($hostname eq "loyalty.hdfgroup.uiuc.edu" || $hostname eq "freedom.hdfgroup.uiuc.edu") {
#   print OUTFILE "  It includes the C APIs,\nbuilt using the following ";
#   print OUTFILE "compilers:\n\n";
#}
#else {
print OUTFILE "\n\nIt includes the C, C++, and F90 APIs, built using the following\n";
print OUTFILE "compilers:\n\n";
#}

# Only the gcc compiler version is in libhdf5.settings, so for now I looked 
# up the versions and hardcoded them here.  We will put them in libhdf5.settings
# for the next release.
if ($indirectory =~ /gnu482/) {
   print OUTFILE "\tgcc, g++, and gfortran 4.8.2\n\n";
} elsif ($hostname =~ /jam/ || $hostname =~ /koala/) {
   print OUTFILE "\tgcc, g++, and gfortran 4.1.2\n\n";
} elsif ($hostname =~ /platypus/) {
   print OUTFILE "\tgcc, g++, and gfortran 4.4.7\n\n";
} elsif ($hostname =~ /emu/) {
   print OUTFILE "\tSun C and C++ 5.12, Sun Fortran 95 8.6\n\n";
} elsif ($hostname =~ /loyalty/ || $hostname =~ /freedom/) {
   print OUTFILE "\tgcc, g++, and gfortran 4.6.1\n\n";    
} elsif ($hostname =~ /duck/) {
   print OUTFILE "\tApple clang/clang++ 3.0 from Xcode 4.6.1 and gfortran 4.8.2\n\n";
} elsif ($hostname =~ /kite/) {
   print OUTFILE "\tApple clang/clang++ 5.1 from Xcode 5.0.2 and gfortran 4.8.2\n\n";
} elsif ($hostname =~ /quail/) {
   print OUTFILE "\tgcc, g++ 5.1 from Xcode 5.1 and gfortran 4.8.2\n\n";
}

print OUTFILE $section2;

print OUTFILE $section3;

print OUTFILE $section4;

