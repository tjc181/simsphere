# Getting Started with Simsphere

Simsphere is distributed as pre-compiled binaries and source code.
Binaries are available for both 64-bit Linux and Windows systems.
This document describes the steps to acquire and begin using Simsphere,
either via the pre-compiled options or source code.

## Acquire Simsphere
### Pre-compiled binaries
Download the latest release from the [Simsphere GitHub
releases](https://github.com/tjc181/simsphere/releases) page.  At the
time of writing, the latest release is version 0.1.6.  This is used
throughout this document in text and hyperlinks.  If a newer release is
available you are encouraged to use that and replace the version numbers
below with the current version.

The software is compiled and tested on 64-bit Windows 10 systems and
64-bit Ubuntu 18.04, 20.04, and CentOS 7 Linux systems.  Other versions
of Windows or Linux may work.  Please let us know if you have success
on others.

* [Windows 10](https://github.com/tjc181/simsphere/releases/download/v0.1.6/simsphere-amd64-windows-v0.1.6.zip)
* [Linux](https://github.com/tjc181/simsphere/releases/download/v0.1.6/simsphere-amd64-linux-v0.1.6.tar.xz)

#### Windows 10
1. Extract the zip archive to a location on your computer.  This document
assumes you will extract to your Documents directory, _C:\Users\your
username\Documents_ .  Windows or third-party security software may
display a security warning when extracting the archive.  Tick the checkbox
"Show extracted files when complete".

2. In the new Windows Explorer window displaying the extracted files,
rename the folder _simsphere-v0.1.6_ to _simsphere_  .

3. Open a _cmd.exe_ or PowerShell terminal and change to the directory where you extracted the archive:
```
cd C:\Users\yourUsername\Documents\simsphere
```

4. Skip ahead to the last section, _First model run_ .

#### Ubuntu or CentOS 7 Linux
1. Extract the compressed tar archive to a location on your computer.  This document assumes you will extract to your home directory, _$HOME_ .

2. Open a terminal.  Rename the extracted directory _simsphere-v0.1.6_ to _simsphere_ .
```
mv simsphere-v0.1.6 simsphere
```

3. Skip ahead to the last section, _First model run_ .

### Compiling Simsphere from source code

You may wish to compile Simsphere binaries from source code.  For example,
to use on a system where pre-compiled binaries are not available or if
you wish to change how the software operates.

#### Required software

   * gfortran 7 or higher
   * [json-fortran library](https://github.com/jacobwilliams/json-fortran)
   * [compare library](https://github.com/tcanich/compare)
   * Static development library packages

Simsphere is built as a static binary by default.

#### Environment Variables
These variables can be set to override default values as described below.
You may wish to change these values if your fortran compiler has a
different name or you to build the software in a different location,
for example.

```
SIMHOME=$HOME/simsphere
BUILDROOT=$SIMHOME/build
CMAKE=cmake
CMAKE_BUILD_TYPE=static
FC=gfortran
```

The build script, _scripts/build.sh_ will use the value of these
environment variables if they are set.

#### Ubuntu 18.04
While these instructions are tested on Ubuntu 18.04 using the bash shell
they should work on most Linux distributions and Unix-like operating
systems.  Installing the required development tools in step 1 will vary
by operating system.  Consult your system documentation for details.
Specific commands, such as _alias_ may need to be adjusted or replaced
when using other shells.  If you compile on another system, please let
us know of any required changes.

**CentOS/RedHat notes**: 
* The procedure for CentOS and RedHat is the same.
However, you will need to install the package _glibc-static_ to build
the software.  This can be done with the command:
``` 
yum install glibc-static
```

* The CentOS gfortran package calls the binary *gfortran*.  You will
need to set the environment variable FC prior to executing the build
script:
```
export FC=gfortran
cd $HOME/simsphere
./scripts/build.sh
```


1. Install gfortran 7, git, cmake
```
sudo apt install -y gfortran-7 git cmake
```

2. Clone the git repository
```
git clone https://github.com/tjc181/simsphere $HOME/simsphere
```

3. Set any desired environment variables.  Execute the build script to
compile dependencies and the model code:
```
cd $HOME/simsphere
./scripts/build.sh
```

**NOTE:** You may need to adjust the _FC_ variable in the _build.sh_ script to fit your system.

4. The compiled software is now available in the _build/bin/_ directory.  Skip ahead to the last section, _First model run_ .

#### Windows 10
We'll use the [msys2](https://www.msys2.org/) platform to install the required development tools.

1. [Follow the msys2 installation instructions](https://www.msys2.org/)
to install the base system.

2. In the same MSYS2 shell, install the cmake and git packages:
```
pacman -S --needed mingw-w64-x86_64-cmake git
```

3. Clone the git repository:
```
git clone https://github.com/tjc181/simsphere $HOME/simsphere
```

4. From the Start menu, open the _MSYS2 MinGW 64-bit_ shell.

5. Set any desired environment variables.  Execute the build script to
compile dependencies and the model code:
```
cd $HOME/simsphere
./scripts/build.sh
```

6. The compiled software is now available in the _build/bin/_ directory.  Skip ahead to the last section, _First model run_ .


### First model run

#### Linux (pre-compiled and compiled from source) and Windows compiled from source
This procedure applies to all Linux systems, pre-compiled or compiled
from source code.  Additionally, if you compiled from source on Windows
this procedure will apply.

1. Setup a working directory and link to the data directory:
```
mkdir work
cd work
ln -s $HOME/simsphere/data
```

2. Create an alias pointing to the executable
```
alias simsphere=$HOME/simsphere/build/bin/simsphere
```

3. Take a copy the default parameter input file.  For the first run,
don't make changes to this file.  On subsequent runs you may adjust the
input parameters as needed.
```
cp data/default/i_model.json .
```

See the [Model Input Parameters](https://simsphere.ems.psu.edu/assets/downloads/Part%20IX%20Extended%20list%20of%20input%20variables.docx) document for descriptions of the parameters and acceptable input ranges. 

4. Execute the model
```
simsphere
```

5. Review output
Output will be written to the files *o_model.dat* and *o_model.json*
in the **$BUILDROOT** directory.  *o_model.dat* is plain text while
*o_model.json* is JSON suitable for processing with other tools.

#### Windows 10
1. Using the cmd or Powershell window already open, take a copy the
default parameter input file.  For the first run, don't make changes
to this file.  On subsequent runs you may adjust the input parameters
as needed.  
``` 
copy data\default\i_model.json .
```

2. Execute the model
```
bin\simsphere
```

3. Review output
Output will be written to the files *o_model.dat* and *o_model.json* in
the current directory.  *o_model.dat* is plain text while *o_model.json*
is JSON suitable for processing with other tools.

## Mailing list
Consider subscribing to the mailing list for additional help from other Simsphere users.  See [README.md](README.md) for subscription information.
