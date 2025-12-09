**Repository transfer**: this repository will transfer
to a new simsphere organization account December 11,
2025. The new repository will be available on GitHub at
[simsphere/simsphere](https://github.com/simsphere/simsphere).

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

## Mailing list

Consider subscribing to the mailing list SIMSPHERE-L@lists.psu.edu.  To subscribe, send email to listserv@lists.psu.edu.  The message body must contain the subscribe command and the list name; you may optionally include your name following list name on the same line.

```
SUBSCRIBE SIMSPHERE-L
```
or with optional name
```
SUBSCRIBE SIMSPHERE-L Jane Smith
```

## Fortran implementation

This is an updated version of the original Fortran implementation.  The goals for this update include:

* Fortran 2008 compliant code with free-form source formatting.
* Full Implementation of unit tests.
* Restructuring of code to eliminate COMMON blocks in favor of modules.
* Reduce (or eliminate!) use of GO TO and DATA statements.
* Implement data structures to simplify passing arguments to routines, reduce global variables.
* Aspire to rework subroutines with pure function calls.

## Building the code

You will need a Fortran 2008 compiler and CMake.  The code is currently being
developed using gfortran 7.3.0.  Please see the [Getting Started
guide](GETTINGSTARTED.md) for additional instructions.
