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

# Check if we are on Windows
if [ $(uname | grep MINGW) ]
then
    export FC=gfortran
    # Build json-fortran, libcompare, simsphere
    scripts/build-json-fortran.sh
    $CMAKE -H$DISTROOT -B$BUILDROOT -G "MinGW Makefiles"
    $CMAKE --build $BUILDROOT
    # Put the json-fortran shared library where programs can find it
    # We can probably fix the simsphere CMake configuration to not have to do this...
    cp $BUILDROOT/json/libjsonfortran.dll $BUILDROOT/bin
else
    # Otherwise assume we're on a Unix
    # Build json-fortran, libcompare, simsphere
    scripts/build-json-fortran.sh
    $CMAKE -H$DISTROOT -B$BUILDROOT
    $CMAKE --build $BUILDROOT
fi

# Test the build
cd $BUILDROOT
bin/config 
ctest



