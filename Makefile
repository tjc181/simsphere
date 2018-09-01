FC=gfortran
FILES= advect.for air.for albedo.for avr.for ball.for below.for block.for bri.for calc.for canres.for capac.for co2flx.for cond.for daykm.for dllstart.for fine.for flux.for gettbl.for gtemp.for hot.for input.for intpol.for levels.for main.for momday.for mom.for netrad.for output.for ozone.for prfile.for psgcal.for pslcal.for psoil.for slope.for snding.for spline.for splint.for start.for stomc.for stomfs.for stomrs.for transm.for vegflx.for veghot.for vegrad.for vegvel.for vel.for water.for
FCFLAGS=-I. -std=legacy -fno-align-commons
PROG=simsphere

simsphere:
	$(FC) -o $(PROG) $(FCFLAGS) $(FILES)

all: simsphere

clean:
	$(RM) *.o *.gch simsphere
