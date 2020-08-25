#!/bin/sh

# Build json-fortran, a Simsphere dependency

JSONBUILD=$BUILDROOT/json
JSONDIST=$DISTROOT/externals/json-fortran

# Test to see if we are working on Windows
if [ $(uname | grep MINGW) ]
then
    $CMAKE -D SKIP_DOC_GEN:BOOL=TRUE -H$JSONDIST -B$JSONBUILD -G "MinGW Makefiles"
else
    # Otherwise, assume a Unix-like system
    $CMAKE -D SKIP_DOC_GEN:BOOL=TRUE -H$JSONDIST -B$JSONBUILD 
fi
$CMAKE --build $JSONBUILD
