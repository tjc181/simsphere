# Getting Started with Simsphere

## Required software

   * gfortran 7 or higher
   * json-fortran library
   * compare library

## Ubuntu 18.04
While these instructions are tested on Ubuntu 18.04 they should work on
most Linux distributions and Unix-like operating systems.  Installing the
required development tools in step 1 will vary by operating system.
Consult the system documentation for details.

1. Install gfortran 7, git, cmake
```
sudo apt install -y gfortran-7 git cmake
```

2. Clone the git repository and set some variables
```
git clone https://github.com/tjc181/simsphere
export DISTROOT=$HOME/simsphere BUILDROOT=$HOME/simsphere-build
```

3. Clone the libjsonfortran repository and build the library
```
cd $DISTROOT
git submodule update --init --recursive
scripts/build-json-fortran.sh
```
**NOTE:** The _build-json-fortran.sh_ script installs the library in /tmp.  If your system clears this directory on reboot you will need to rebuild the library.  
**NOTE:** You may need to adjust the _FC_ variable in the _build-json-fortran.sh_ script to fit your system.  For example on a CentOS
7 system,
```
FC=gfortran cmake -D SKIP_DOC_GEN:BOOL=TRUE -D ENABLE_TESTS:BOOL=FALSE -H./json-fortran -B./json-build
```
The above will inhibit building of the documentation and prevents running
of the tests (which produced a configuration error for us).

4. Build simsphere
```
cmake -H. -B$BUILDROOT
cmake --build $BUILDROOT
```

5. Test the build
```
cd $BUILDROOT
bin/config 
ctest
```

6. Setup a working directory and adjust input parameters
    1. Create/go to a work space, for example:
```
mkdir $BUILDROOT/work
cd $BUILDROOT/work
```

    2. Create a link to the source root "data" directory:
```
ln -s $DISTROOT/data
```

    3. Create an alias pointing to the executable
```
alias simsphere=$BUILDROOT/bin/simsphere
```

    4. Get a copy of the default json input file:
```
cp data/default/i_model.json .
```
See the [Model Input Parameters](https://simsphere.ems.psu.edu/assets/downloads/Part%20IV;%20model%20input%20parameters.xls) spreadsheet for descriptions of the parameters and acceptable input ranges. 

7. Execute the model
```
simsphere
```

8. Review output
Output will be written to the files *o_model.dat* and *o_model.json*
in the **$BUILDROOT** directory.  *o_model.dat* is plain text while *o_model.json* is JSON suitable for processing with other tools.

## Windows 10
Installation on Windows 10 systems is very similar to Ubuntu, however the development tools are significantly more complicated to install.  See the documentation in [README.md](README.md) for a starting point.
