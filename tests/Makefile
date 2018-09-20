FC=gfortran
FILES= ../src/snding_mod.f90 ../src/transm_mod.f90 ../src/vel_mod.f90 ../src/simsphere_mod.f90 tests.f90
#FCFLAGS= -I. -Wall -pedantic -std=f2003 -ggdb -fbacktrace -ffpe-trap=zero,overflow,underflow
FCFLAGS= -I. -Wall -pedantic -std=f2003 -ggdb -fbacktrace

test: 
	$(FC) $(FCFLAGS) -o ../bin/test $(FILES)

all: $(PROG)

clean:
	$(RM) *.o *.mod 
