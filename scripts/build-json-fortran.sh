#!/bin/sh

# Build json-fortran, a Simsphere dependency

JSONBUILD=$BUILDROOT/json
JSONDIST=$SIMHOME/externals/json-fortran

CMAKE_FLAGS="-DCMAKE_POLICY_VERSION_MINIMUM=3.5"

# Test to see if we are working on Windows
if [ $(uname | grep MINGW) ]
then
    $CMAKE -D SKIP_DOC_GEN:BOOL=TRUE -H$JSONDIST -B$JSONBUILD $CMAKE_FLAGS -G "MinGW Makefiles"
else
    # Otherwise, assume a Unix-like system
    $CMAKE -D SKIP_DOC_GEN:BOOL=TRUE -H$JSONDIST -B$JSONBUILD $CMAKE_FLAGS
fi
$CMAKE --build $JSONBUILD
