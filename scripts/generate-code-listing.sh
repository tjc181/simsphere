#!/bin/sh

#
# Generate a full code listing in DOCX format.
#
# Tom Canich <tjc181@psu.edu> 2020-12-10
#

VERSION=$(cat .VERSION)
OUT="simsphere-v$VERSION.txt"

echo "Simsphere $VERSION Full Code Listing" > $OUT
echo "------------------------------------" >> $OUT
echo "" >> $OUT
echo "" >> $OUT
cat src/*.f90 >> $OUT
libreoffice --writer --convert-to docx $OUT
rm $OUT

exit 0
