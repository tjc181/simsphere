[![Build Status](https://travis-ci.com/tjc181/simsphere.svg?branch=master)](https://travis-ci.com/tjc181/simsphere) [![codecov](https://codecov.io/gh/tjc181/simsphere/branch/master/graph/badge.svg)](https://codecov.io/gh/tjc181/simsphere)

# Simsphere

Welcome to the model Simsphere, a state-of-the art
Soil-Vegetation-Atmosphere Transfer (SVAT) model for use by the general
scientific community. Simsphere is the product of 30 years of continuous
experience and utilization by Toby Carlson and his students at Penn
State. Simsphere is a one-dimensional model that allows one to simulate
the transfer of heat and moisture between plants, soil and atmosphere
over a 24 hour day.

Extensive information about the model is available at
https://simsphere.ems.psu.edu .

## Fortran implementation

This is an updated version of the original Fortran implementation.  The goals for this update include:

* Fortran 2008 compliant code with free-form source formatting.
* Full Implementation of unit tests.
* Restructuring of code to eliminate COMMON blocks in favor of modules.
* Reduce (or eliminate!) use of GO TO and DATA statements.
* Implement data structures to simplify passing arguments to routines, reduce global variables.
* Aspire to rework subroutines with pure function calls.

## Other implementations

A C implementation also exists.  This is available for download at https://simsphere.ems.psu.edu.  The C implementation dates to the late 1990s.

## Building the code

You will need a Fortran 2008 compiler and CMake.  The code is currently being
developed using gfortran 7.3.0.  Please see the [Getting Started
guide](GETTINGSTARTED.md) for additional instructions.


### Unix:
Clone the repository and execute the include build script:
```
git clone https://github.com/tjc181/simsphere
cd simsphere
scripts/build.sh
```

### Windows:

1. Install msys64 from https://www.msys2.org .  Start the msys64 shell to install the development tools with commands: 
```
pacman --needed -S base-devel
pacman --needed -S mingw-w64-x86_64-gcc
pacman --needed -S mingw-w64-x86_64-toolchain
pacman --needed -S mingw-w64-x86_64-cmake
```
2. Within the msys64 shell, clone the Git repository:
```
git clone https://github.com/tjc181/simsphere
```

3. Within the cloned repository, execute the build script to build the model code and execute the test suite:
```
cd simpshere
scripts/build.sh
```

