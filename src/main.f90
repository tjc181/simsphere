program simsphere
  use simsphere_mod
  implicit none

! Main program lists and defines model symbols, coordinates model
! subroutines, and establishes iteration sequence.

! Type declaration of variables.

!** Compiler identifies FRACTN as unused
!**   character(len=5) FRACTN
  integer(kind=1) :: StabCriteria        ! Neutral (0), Unstable (1), Stable (2)
  real(kind=4) :: Obst_Hgt, zo_patch
  real(kind=4) :: Old_Ahum
  integer(kind=1) :: init
  integer :: No_Rows, IONCE, MONCE
!TJC  real(kind=4) :: ZLS(50)
  real :: ZLS(50)

!TJC Guessing at some types...
  real :: B, BareEvapFlux, BareHeatFlux, BareNetRadn, BareRadioTemp
  real :: MixedNetRadn, OLDTMP, TIME, TMOD
  real :: VegnNetRadn, VegnRadioTemp
  integer :: YCOUNT
  logical :: dual_regime

  
!      INCLUDE 'modvars.h'

! Set initial values to variables, see BLOCK.FOR

  DATA IONCE,MONCE,ycount,TIME / 2*0,2*0.0 /
  StabCriteria = 0
  init = 1

! Start reads the values of various input parameters to get the model
! going and Snding reads in the sounding file.


  OPEN ( UNIT=11, FILE = 'o_model.dat' ) ! Open the output file

  CALL START (Obst_Hgt,dual_regime,zo_patch) ! Read and Check data
     
  CALL SNDING (ZLS, Old_Ahum)                ! Read Sounding - Call Spline to Interpolate

  CALL CALC (OLDTMP, No_Rows)        ! Some basic calculations

  CALL PRFILE                       ! Set a Geostrophic wind at the surface

  CALL GETTBL                       ! Is the lookup table for subroutine TRANSM.

! This is the start of the diurnal loop (TIMEND-STRTIM), nominally 22 hrs.

  CALL PSOIL                        ! Set up for the soil

5 CONTINUE                          ! Loop back here every 180 seconds

! Initially TIME = 0, so REALTME = STRTIM but remember
! TIME is incremented each step.

  REALTM = TIME + STRTIM
  PTIME = REALTM / 3600.
  TMOD = AMOD (TIME,OUTTT)
                    
!                CALL SUROUTINES IN THE SOLUTION SEQUENCE.

! Net Radiation

  CALL NETRAD (TIME,BareRadioTemp,VegnRadioTemp,BareNetRadn,VegnNetRadn,MixedNetRadn,Init)

! Resistance values in the Transition and Surface Layers
! Entry to nighttime formulations (BRI & MOM) through this subroutine

  CALL VEL (MONCE,IONCE,StabCriteria,YCOUNT,Obst_Hgt,dual_regime,zo_patch)

! Mixed Layer

  IF (HEAT.GT.0.00001 .AND. SWAVE .GT. 0 .AND. RNET .GT. 0) THEN
    CALL AIR (ZLS, YCOUNT)
  END IF

! Eddy Diffusivities in the Mixed Layer

  IF (HEAT.GT.0.00001 .AND. SWAVE .GT. 0 .AND. RNET .GT. 0) THEN
    CALL DAYKM
  END IF

! Momentum Equations - Mixed Layer

  IF (HEAT.GT.0.00001 .AND. SWAVE .GT. 0 .AND. RNET .GT. 0) THEN
    CALL MOMDAY
  END IF

! Evaporative Flux, Surface Temperature solutions

  CALL FLUX (BareRadioTemp,VegnRadioTemp,BareEvapFlux,BareHeatFlux)

! Heat FLux - Penman Formulation

  IF((HEAT.GE.0 .or. RNET .GT.0).AND.SWAVE .GT.0) THEN
    CALL HOT (B,BareNetRadn,BareEvapFlux,BareHeatFlux)
  END IF
  
!
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


end
