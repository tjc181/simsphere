# Getting Started with Simsphere

## Required software

   * gfortran 7 or higher
   * json-fortran library
   * compare library
   * Static development library packages

Simsphere is built as a static binary by default.

## Environment Variables
These variables can be set to override default values as described below.
You may wish to change these values if your fortran compiler has a
different name or you to build the software in a different location,
for example.

BUILDROOT = $SIMHOME/build
CMAKE = cmake
CMAKE_BUILD_TYPE = static
SIMHOME = $HOME/simsphere
FC = gfortran

The build script, *scripts/build.sh* will use the value of these
environment variables if they are set.

## Ubuntu 18.04
While these instructions are tested on Ubuntu 18.04 using the bash shell
they should work on most Linux distributions and Unix-like operating
systems.  Installing the required development tools in step 1 will vary
by operating system.  Consult your system documentation for details.
Specific commands, such as *alias* may need to be adjusted or replaced
when using other shells.

1. Install gfortran 7, git, cmake
```
sudo apt install -y gfortran-7 git cmake
```

2. Clone the git repository
```
git clone https://github.com/tjc181/simsphere $HOME/simsphere
```

3. Execute the build script to compile dependencies and the model code
```
cd $HOME/simsphere
./scripts/build.sh
```
**NOTE:** You may need to adjust the *FC* variable in the *build.sh* script to fit your system.

4. Setup a working directory and adjust input parameters
   Create/go to a work space, for example:
```
mkdir work
cd work
```

    Create a link to the source root "data" directory:
```
ln -s $HOME/simsphere/data
```

    Create an alias pointing to the executable
```
alias simsphere=$HOME/simsphere/build/bin/simsphere
```

    Take a copy the default parameter input file
```
cp data/default/i_model.json .
```

See the [Model Input Parameters](https://simsphere.ems.psu.edu/assets/downloads/Part%20IX%20Extended%20list%20of%20input%20variables.docx) document for descriptions of the parameters and acceptable input ranges. 

5. Execute the model
```
simsphere
```

6. Review output
Output will be written to the files *o_model.dat* and *o_model.json*
in the **$BUILDROOT** directory.  *o_model.dat* is plain text while *o_model.json* is JSON suitable for processing with other tools.

## CentOS/RedHat
The procedure for CentOS and RedHat is the same.  However, you will need
to install the package *glibc-static* to build the software.  This can
be done with the command: 
``` 
yum install glibc-static
```

Also, the CentOS gfortran package calls the binary *gfortran*.  You will
need to set the environment variable FC prior to executing the build
script:

```
export FC=gfortran
cd $HOME/simsphere
./scripts/build.sh
```

## Windows 10
Installation on Windows 10 systems is very similar to Ubuntu.  The development tools may be installed using the Msys2 system.  See the documentation in [README.md](README.md) for specific commands.

## Mailing list
Consider subscribing to the mailing list for additional help from other Simsphere users.  See [README.md](README.md) for subscription information.
