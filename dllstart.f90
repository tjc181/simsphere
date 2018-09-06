! Subroutine DLLSTART is essentially the MAIN module from the executable
! version of the model. It has to be a subroutine so it can be exported
! from the DLL, and hence accessable from other programs (i.e. the interface)
subroutine PSUBAMS
  use simsphere_mod
!MS$ATTRIBUTES C, DLLEXPORT::PSUBAMS

! Declare some variables; this might not work, I'm not sure of the scope and
! visibility rules in Fortran; Even if they do work, they're global which is 
! ridiculous.

! ** Compiler identifies FRACTN as unused
!  character(len=5) :: FRACTN

  integer(kind=1) :: STABCRITERIA, INIT
  real(kind=4) :: OBST_HGT, ZO_PATCH, OLD_AHUM
  integer :: NO_ROWS
  real(kind=4) :: ZLS(50)

! Include the common blocks and variable definitions via NON-STANDARD extension;
!      include 'modvars.h'

! Initialise some variables. CF block.for where a bunch of globals are initialised.
! Completely non-obvious.

  DATA IONCE, MONCE, YCOUNT, TIME / 2*0, 2*0.0 /
  STABCRITERIA=0
  INIT=1

! Open the output file (I have no idea why this is done here).
  OPEN(UNIT=11, FILE='o_model.dat')

! Read the input parameters from i_model.dat in subroutine START
  CALL START(OBST_HGT, DUAL_REGIME, ZO_PATCH)

! Read and interpolate the sounding data.
  CALL SNDING(ZLS, OLD_AHUM)

! Preliminary calculations
  CALL CALC(OLDTMP, NO_ROWS)

! Set the geostrophic wind at the surface.
  CALL PRFILE

! Initialise lookup table for TRANSM
  CALL GETTBL

! Original comments stated that this was the start of the diurnal loop,
! but obviously it isn't; the start is the CONTINUE statement after this,
! which initialises the soils or something like that.
  CALL PSOIL

! *THIS* is where the main loop begins, although you'd never guess.
5        CONTINUE

! Initially TIME=0, so REALTME=STRTM - presumably STRTM is simply used as an
! offset.

  REALTM=TIME+STRTIM
  TIME=REALTM/3600.0
  TMOD=AMOD(TIME, OUTTT)

! Plough through all the subroutines.

! Net radiation
  CALL NETRAD (TIME, BareRadioTemp, VegnRadioTemp, BareNetRadn,         &
       VegnNetRadn, MixedNetRadn, Init)

  CALL VEL (MONCE, IONCE, StabCriteria, YCOUNT, Obst_Hgt,               &
       dual_regime, zo_patch)

  IF (HEAT.GT.0.00001 .AND. SWAVE .GT. 0 .AND. RNET .GT. 0)             &
     CALL AIR (ZLS, YCOUNT)

  IF (HEAT.GT.0.00001 .AND. SWAVE .GT. 0 .AND. RNET .GT. 0)             &
     CALL DAYKM

  IF (HEAT.GT.0.00001 .AND. SWAVE .GT. 0 .AND. RNET .GT. 0)             &
     CALL MOMDAY

  CALL FLUX (BareRadioTemp,VegnRadioTemp,BareEvapFlux,BareHeatFlux)

  IF((HEAT.GE.0 .or. RNET .GT.0).AND.SWAVE .GT.0)                       &
    CALL HOT (B,BareNetRadn,BareEvapFlux,BareHeatFlux)

  if (rnet .gt. 0)then
    StabCriteria = 1
  else
    StabCriteria = 2
  endif

! End of the atmospheric cycle.

  CALL BELOW (Time,BareRadioTemp,BareEvapFlux) ! Substrate

!  Output is written every OUTTT seconds.

  IF (TMOD.EQ.0.) CALL output(No_Rows)

! Increment Time.

  TIME = TIME + DELTA

  IF (REALTM .LT. timend) GO TO 5

  ENDFILE (UNIT = 11)  ! Close the output file
  CLOSE (UNIT = 11)

  return
end
