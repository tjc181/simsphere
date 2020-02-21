#!/bin/sh

#
# Build the simsphere program and dependencies
#


# These settings should be ok for Ubuntu 18.04.
FC=${FC:-gfortran-7}
DISTROOT=${DISTROOT:-$HOME/simsphere}
BUILDROOT=${BUILDROOT:-$DISTROOT/build}
export FC DISTROOT BUILDROOT


# Download simsphere dependencies
cd $DISTROOT
git submodule update --init --recursive

# Build json-fortran, libcompare, simsphere
scripts/build-json-fortran.sh
cmake -H$DISTROOT -B$BUILDROOT
cmake --build $BUILDROOT

# Test the build
cd $BUILDROOT
bin/config 
ctest
