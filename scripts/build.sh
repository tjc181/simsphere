#!/bin/sh

#
# Build the simsphere program and dependencies
#


# These settings should be ok for Ubuntu 18.04.
FC=${FC:-gfortran}
CMAKE=${CMAKE:-cmake}
SIMHOME=${SIMHOME:-$(pwd)}
BUILDROOT=${BUILDROOT:-$SIMHOME/build}
CMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE:-"static"}
export FC CMAKE SIMHOME BUILDROOT CMAKE_BUILD_TYPE 


# Download simsphere dependencies
cd $SIMHOME
git submodule update --init --recursive

# Check if we are on Windows
if [ $(uname | grep MINGW) ]
then
    # Build json-fortran, libcompare, simsphere
    scripts/build-json-fortran.sh
    scripts/build-libcompare.sh
    $CMAKE -H$SIMHOME -B$BUILDROOT -G "MinGW Makefiles"
    $CMAKE --build $BUILDROOT
    # Put the json-fortran shared library where programs can find it
    # We can probably fix the simsphere CMake configuration to not have to do this...
    cp $BUILDROOT/json/libjsonfortran.dll $BUILDROOT/bin
else
    # Otherwise assume we're on a Unix
    # Build json-fortran, libcompare, simsphere
    scripts/build-json-fortran.sh
    scripts/build-libcompare.sh
    $CMAKE -H$SIMHOME -B$BUILDROOT
    $CMAKE --build $BUILDROOT
fi

# Test the build
cd $BUILDROOT
bin/config 
ctest



