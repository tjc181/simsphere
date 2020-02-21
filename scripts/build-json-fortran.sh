#!/bin/sh

# Build json-fortran, a Simsphere dependency

JSONBUILD=$BUILDROOT/json
JSONDIST=$DISTROOT/externals/json-fortran

$CMAKE -D SKIP_DOC_GEN:BOOL=TRUE -H$JSONDIST -B$JSONBUILD
$CMAKE --build $JSONBUILD
