#!/bin/sh

# 
# Package compiled simsphere and required data files.
#
# Usage: package-simsphere.sh distroot buildroot version_tag
#
# Tom Canich <tjc181@psu.edu> 2020-12-10

# Archive structure:
#
# /simsphere-$VERSION
#                  |-/bin
#                  |-/data
#                       |-/default
#                       |-/testing
#
# bin: static binaries
# data: lookup files, default input parameters file, test input/output files
#

DISTROOT=$1
BUILDROOT=$2
VERSION=$3
PROGNAME="simsphere"
DIRS="bin data data/default data/testing"
PRGFILES="config loadjson simsphere" 
TSTFILES="simsphere_test test_air test_albedo test_average test_ball test_below \
  test_bri test_calc test_canres test_capac test_co2flx test_daykm test_fine \
  test_flux test_gtemp test_hot test_intpol test_load_data test_mom test_momday \
  test_netrad test_output test_ozone test_prfile test_pslcal test_psoil test_slope \
  test_snding test_veghot test_vegrad test_vegvel test_water"
DATFILES="lut.dat soils.dat veglut.dat"
DEFFILES="i_model.json  o_model.dat  o_model.json"
TESTING="i_model_test1_f77.dat o_model_test1_f77.dat o_model_test1_f90.json \
  i_model_test1_f90.json o_model_test1_f90.dat README"
DOCFILES="GETTINGSTARTED.md README.md CONTRIBUTING.md CHANGELOG LICENSE"

# Create the directory structure
mkdir $DISTROOT/$PROGNAME-$VERSION
cd $DISTROOT/$PROGNAME-$VERSION
for dir in $DIRS
do
  mkdir $dir
done

# Install the required files
for prog in $PRGFILES
do
  install -m755 -s $BUILDROOT/bin/$prog bin/
done

for testprog in $TSTFILES 
do
  install -m755 -s $BUILDROOT/bin/$testprog bin/
done

for data in $DATFILES
do
  install -m644 $DISTROOT/data/$data data
done

for default in $DEFFILES
do
  install -m644 $DISTROOT/data/default/$default data/default
done

for testing in $TESTING
do
  install -m644 $DISTROOT/data/testing/$testing data/testing
done

for doc in $DOCFILES
do
  install -m644 $DISTROOT/$doc .
done


cd $DISTROOT


exit 0
