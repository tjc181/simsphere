[![Build Status](https://travis-ci.com/tjc181/simsphere.svg?branch=master)](https://travis-ci.com/tjc181/simsphere)

# Simsphere

Welcome to the model Simsphere, a state-of-the art
Soil-Vegetation-Atmosphere Transfer (SVAT) model for use by the general
scientific community. Simsphere is product of 30 years of continuous
experience and utilization by Toby Carlson and his students at Penn
State. Simsphere is a one-dimensional model that allows one to simulate
the transfer of heat and moisture between plants, soil and atmosphere
over a 24 hour day.

Extensive information about the model is available at
https://simsphere.ems.psu.edu .

## Fortran implementation

This is an updated version of the original Fortran implementation.  The goals for this update include:

* Fortran 2003 compliant code with free-form source formatting.
* Full Implementation of unit tests.
* Restructuring of code to eliminate COMMON blocks in favor of modules.
* Reduce (or eliminate!) use of GO TO and DATA statements.
* Aspire to replace many subroutines with pure function calls and to reduce the number of global variables to a minimum.

## Other implementations

A C implementation also exists.  This implementation is not yet published,
but may be in the future.  The implementation dates to the late 1990s.

## Building the code

You will need a Fortran 2003 compiler.  The code is currently being
developed using gfortran 7.3.0.  You will need GNU make.

Run the command _make_ in the top directory.  This will produce the model binary _bin/simsphere_ .

Tests are available by running _make test_ in the top directory.  Tests are automatically executed by this make targe.


