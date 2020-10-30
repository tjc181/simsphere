#!/bin/sh

# Build libcompare, a Simsphere dependency

LIBCOMPAREBUILD=$BUILDROOT/compare
LIBCOMPAREDIST=$DISTROOT/externals/compare

# Test to see if we are working on Windows
if [ $(uname | grep MINGW) ]
then
    $CMAKE -D SKIP_DOC_GEN:BOOL=TRUE -H$LIBCOMPAREDIST -B$LIBCOMPAREBUILD -G "MinGW Makefiles"
else
    # Otherwise, assume a Unix-like system
    $CMAKE -D SKIP_DOC_GEN:BOOL=TRUE -H$LIBCOMPAREDIST -B$LIBCOMPAREBUILD 
fi
$CMAKE --build $LIBCOMPAREBUILD
