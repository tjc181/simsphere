#Getting Started with Simsphere

##Ubuntu 18.04
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
git clone https://github.com/tjc181/simsphere
```

3. Clone the libjsonfortran repository and build the library
```
git submodule update --init --recursive
scripts/build-json-fortran.sh
```

4. Build simsphere
```
cmake -H. -B../simsphere-build
cmake --build ../simsphere-build
```

5. Test the build
```
cd ../simsphere-build 
bin/config 
ctest
```

6. Adjust input parameters
Edit the file _i_model.json_ in the __simsphere-build__ directory to adjust the input parameter values.  See the [Model Input Parameters](https://simsphere.ems.psu.edu/assets/downloads/Part%20IV;%20model%20input%20parameters.xls) spreadsheet for descriptions of the parameters and acceptable input ranges. 

7. Execute the model
```
bin/simsphere
```

8. Review output
Output will be written to the files _o_model.dat_ and _o_model.json_
in the __simsphere-build__ directory.  _o_model.dat_ is plain text while _o_model.json_ is JSON suitable for processing with other tools.

##Windows 10
Installation on Windows 10 systems is very similar to Ubuntu, however the development tools are significantly more complicated to install.  See the documentation in [README.md](README.md) for a starting point.
