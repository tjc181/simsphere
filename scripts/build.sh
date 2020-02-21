#!/bin/sh

#
# Build the simsphere program and dependencies
#


# These settings should be ok for Ubuntu 18.04.
FC=${FC:-gfortran-7}
CMAKE=${CMAKE:-cmake}
DISTROOT=${DISTROOT:-$HOME/simsphere}
BUILDROOT=${BUILDROOT:-$DISTROOT/build}
export FC CMAKE DISTROOT BUILDROOT


# Download simsphere dependencies
cd $DISTROOT
git submodule update --init --recursive

# Build json-fortran, libcompare, simsphere
scripts/build-json-fortran.sh
$CMAKE -H$DISTROOT -B$BUILDROOT
$CMAKE --build $BUILDROOT

# Test the build
cd $BUILDROOT
bin/config 
ctest
