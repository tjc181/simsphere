#!/bin/sh

# Build libcompare, a Simsphere dependency

CMAKE_FLAGS="-DCMAKE_POLICY_VERSION_MINIMUM=3.5"

LIBCOMPAREBUILD=$BUILDROOT/compare
LIBCOMPAREDIST=$SIMHOME/externals/compare

# Test to see if we are working on Windows
if [ $(uname | grep MINGW) ]
then
    $CMAKE -D SKIP_DOC_GEN:BOOL=TRUE -H$LIBCOMPAREDIST -B$LIBCOMPAREBUILD $CMAKE_FLAGS -G "MinGW Makefiles"
else
    # Otherwise, assume a Unix-like system
    $CMAKE -D SKIP_DOC_GEN:BOOL=TRUE -H$LIBCOMPAREDIST -B$LIBCOMPAREBUILD $CMAKE_FLAGS
fi
$CMAKE --build $LIBCOMPAREBUILD
