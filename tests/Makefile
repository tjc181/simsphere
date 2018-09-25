FC=gfortran
FILES= ../src/avr.f90 ../src/vegflx.f90 ../src/psgcal.f90 mod_testing.f90 ../src/snding_mod.f90 ../src/transm_mod.f90 ../src/vel_mod.f90 ../src/stomata_mod.f90 ../src/simsphere_mod.f90 tests.f90
#FCFLAGS= -I. -Wall -pedantic -std=f2003 -ggdb -fbacktrace -ffpe-trap=zero,overflow,underflow
FCFLAGS= -I. -Wall -pedantic -std=f2003 -ggdb -fbacktrace

test: 
	$(FC) $(FCFLAGS) -o ../bin/test $(FILES)

all: $(PROG)

clean:
	$(RM) *.o *.mod 
