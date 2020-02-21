#!/bin/sh

# Build json-fortran, a Simsphere dependency

JSONBUILD=$BUILDROOT/json
JSONDIST=$DISTROOT/externals/json-fortran
FC=${FC:-gfortran}

cmake -D SKIP_DOC_GEN:BOOL=TRUE -H$JSONDIST -B$JSONBUILD
cmake --build $JSONBUILD
