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

2. Clone the git repository
```
git clone https://github.com/tjc181/simsphere $HOME/simsphere
```

3. Execute the build script to compile dependencies and the model code
```
cd $HOME/simsphere
./scripts/build.sh
```
**NOTE:** You may need to adjust the _FC_ variable in the _build.sh_ script to fit your system.

4. Setup a working directory and adjust input parameters
    1. Create/go to a work space, for example:
```
mkdir work
cd work
```

    2. Create a link to the source root "data" directory:
```
ln -s ../data
```

    3. Create an alias pointing to the executable
```
alias simsphere=$HOME/simsphere/bin/simsphere
```

    4. Take a copy the default parameter input file
```
cp data/default/i_model.json .
```

See the [Model Input Parameters](https://simsphere.ems.psu.edu/assets/downloads/Part%20IV;%20model%20input%20parameters.xls) spreadsheet for descriptions of the parameters and acceptable input ranges. 

5. Execute the model
```
simsphere
```

6. Review output
Output will be written to the files *o_model.dat* and *o_model.json*
in the **$BUILDROOT** directory.  *o_model.dat* is plain text while *o_model.json* is JSON suitable for processing with other tools.

## Windows 10
Installation on Windows 10 systems is very similar to Ubuntu, however the development tools are significantly more complicated to install.  See the documentation in [README.md](README.md) for a starting point.
