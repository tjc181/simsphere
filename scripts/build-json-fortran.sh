#!/bin/sh

# Build json-fortran, a Simsphere dependency

JSONBUILD=$BUILDROOT/json-build
JSONDIST=$DISTROOT/externals/json-fortran

FC=gfortran-7 cmake -D SKIP_DOC_GEN:BOOL=TRUE -H$JSONDIST -B$JSONBUILD
cmake --build $JSONBUILD
