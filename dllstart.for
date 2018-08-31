c Subroutine DLLSTART is essentially the MAIN module from the executable
c version of the model. It has to be a subroutine so it can be exported
c from the DLL, and hence accessable from other programs (i.e. the interface)
      SUBROUTINE PSUBAMS
!MS$ATTRIBUTES C, DLLEXPORT::PSUBAMS

c Declare some variables; this might not work, I'm not sure of the scope and
c visibility rules in Fortran; Even if they do work, they're global which is 
c ridiculous.

      CHARACTER *5	FRACTN

      INTEGER *1 STABCRITERIA, INIT
	REAL *4 OBST_HGT, ZO_PATCH, OLD_AHUM
	INTEGER NO_ROWS
	REAL *4 ZLS(50)

c Include the common blocks and variable definitions via NON-STANDARD extension;
$include:'modvars.h'

c Initialise some variables. CF block.for where a bunch of globals are initialised.
c Completely non-obvious.

      DATA IONCE, MONCE, YCOUNT, TIME / 2*0, 2*0.0 /
      STABCRITERIA=0
	INIT=1

c Open the output file (I have no idea why this is done here).
      OPEN(UNIT=11, FILE='o_model.dat')

c Read the input parameters from i_model.dat in subroutine START
      CALL START(OBST_HGT, DUAL_REGIME, ZO_PATCH)

c Read and interpolate the sounding data.
	CALL SNDING(ZLS, OLD_AHUM)

c Preliminary calculations
	CALL CALC(OLDTMP, NO_ROWS)

c Set the geostrophic wind at the surface.
	CALL PRFILE

c Initialise lookup table for TRANSM
	CALL GETTBL

c Original comments stated that this was the start of the diurnal loop,
c but obviously it isn't; the start is the CONTINUE statement after this,
c which initialises the soils or something like that.
	CALL PSOIL

c *THIS* is where the main loop begins, although you'd never guess.
    5	CONTINUE

c Initially TIME=0, so REALTME=STRTM - presumably STRTM is simply used as an
c offset.

	REALTM=TIME+STRTIM
	PTIME=REALTM/3600.0
	TMOD=AMOD(TIME, OUTTT)

c Plough through all the subroutines.

c Net radiation
	CALL NETRAD (TIME, BareRadioTemp, VegnRadioTemp, BareNetRadn, 
     /	VegnNetRadn, MixedNetRadn, Init)

	CALL VEL (MONCE, IONCE, StabCriteria, YCOUNT, Obst_Hgt, 
	/	dual_regime, zo_patch)

	IF (HEAT.GT.0.00001 .AND. SWAVE .GT. 0 .AND. RNET .GT. 0)
     /	CALL AIR (ZLS, YCOUNT)

	IF (HEAT.GT.0.00001 .AND. SWAVE .GT. 0 .AND. RNET .GT. 0) 
	/	CALL DAYKM

	IF (HEAT.GT.0.00001 .AND. SWAVE .GT. 0 .AND. RNET .GT. 0)
	/	CALL MOMDAY

	CALL FLUX (BareRadioTemp,VegnRadioTemp,BareEvapFlux,
     /           BareHeatFlux)

	IF((HEAT.GE.0 .or. RNET .GT.0).AND.SWAVE .GT.0)
     /   CALL HOT (B,BareNetRadn,BareEvapFlux,BareHeatFlux)

	if (rnet .gt. 0)then
         StabCriteria = 1
      else
         StabCriteria = 2
      endif

c End of the atmospheric cycle.

	CALL BELOW (Time,BareRadioTemp,BareEvapFlux) ! Substrate

*/  Output is written every OUTTT seconds.


       IF (TMOD.EQ.0.) CALL output(No_Rows)

*/ Increment Time.

      TIME = TIME + DELTA


      IF (REALTM .LT. timend) GO TO 5

      ENDFILE (UNIT = 11)  ! Close the output file
      CLOSE (UNIT = 11)


      RETURN
	END