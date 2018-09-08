FC=gfortran
FILES= simsphere_mod.f90 advect.f90 air.f90 albedo.f90 avr.f90 ball.f90 below.f90 bri.f90 calc.f90 canres.f90 capac.f90 co2flx.f90 cond.f90 daykm.f90 fine.f90 flux.f90 gettbl.f90 gtemp.f90 hot.f90 input.f90 intpol.f90 levels.f90 main.f90 momday.f90 mom.f90 netrad.f90 output.f90 ozone.f90 prfile.f90 psgcal.f90 pslcal.f90 psoil.f90 slope.f90 snding.f90 spline.f90 splint.f90 start.f90 stomc.f90 stomfs.f90 stomrs.f90 transm.f90 vegflx.f90 veghot.f90 vegrad.f90 vegvel.f90 vel.f90 water.f90
MODSRC= simsphere_mod.f90
MODULE= simsphere_mod
LIBOBJ= simsphere_mod.o
FCFLAGS= -I. -Wall -pedantic -std=f2003 -fPIC
LD= ld
LDARGS= -G
LDFLAGS= -L. 
LIBS= -llibsimsphere_mod
MAJV=0
MINV=1
PROG=simsphere

simsphere: 
	$(FC) $(FCFLAGS) -o $(PROG) src/$(FILES)

all: simsphere

clean:
	$(RM) *.o *.gch *.mod simsphere 
