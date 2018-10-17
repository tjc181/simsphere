program simsphere
  use simsphere_mod
  implicit none

! Main program lists and defines model symbols, coordinates model
! subroutines, and establishes iteration sequence.

! Type declaration of variables.

!** Compiler identifies FRACTN as unused
!**   character(len=5) FRACTN
  integer(kind=1) :: StabCriteria=0        ! Neutral (0), Unstable (1), Stable (2)
  real(kind=4) :: Obst_Hgt, zo_patch
  real(kind=4) :: Old_Ahum
  integer(kind=1) :: init=1
  integer :: No_Rows, IONCE=0, MONCE=0
!TJC  real(kind=4) :: ZLS(50)
  real :: ZLS(50)

!TJC Guessing at some types...
  real :: B, BareEvapFlux, BareHeatFlux, BareNetRadn, BareRadioTemp
  real :: MixedNetRadn, OLDTMP, TIME=0.0, TMOD
  real :: VegnNetRadn, VegnRadioTemp, YCOUNT=0.0
  logical :: dual_regime

  type(t_timeloc) :: timeloc
  type(t_temp) :: temp
  type(t_humid) :: humidity
  type(t_wind) :: wind

  
!      INCLUDE 'modvars.h'

! Start reads the values of various input parameters to get the model
! going and Snding reads in the sounding file.


  OPEN ( UNIT=11, FILE = f_output ) ! Open the output file

!  CALL START (Obst_Hgt,dual_regime,zo_patch) ! Read and Check data
  CALL START (Obst_Hgt, temp, humidity, timeloc, wind)   ! Read data
     
  CALL SNDING (ZLS, Old_Ahum, temp, humidity, timeloc, wind)  ! Read Sounding - Call Spline to Interpolate

  CALL CALC (OLDTMP, No_Rows)        ! Some basic calculations

  CALL PRFILE                       ! Set a Geostrophic wind at the surface

  CALL GETTBL                       ! Is the lookup table for subroutine TRANSM.

! This is the start of the diurnal loop (TIMEND-STRTIM), nominally 22 hrs.

  CALL PSOIL                        ! Set up for the soil

5 CONTINUE                          ! Loop back here every 180 seconds

! Initially TIME = 0, so REALTME = STRTIM but remember
! TIME is incremented each step.

  REALTM = TIME + STRTIM
! TJC Removed conversion following rework of dectim()
!  PTIME = REALTM / 3600.
  ptime = realtm
  if ( OUTTT /= 0.0 ) then
    TMOD = MOD (TIME,OUTTT)
  end if
                    
!                CALL SUROUTINES IN THE SOLUTION SEQUENCE.

! Net Radiation

  CALL NETRAD (TIME,BareRadioTemp,VegnRadioTemp,BareNetRadn,VegnNetRadn,MixedNetRadn,Init)

! Resistance values in the Transition and Surface Layers
! Entry to nighttime formulations (BRI & MOM) through this subroutine

  CALL VEL (MONCE,IONCE,StabCriteria,YCOUNT,Obst_Hgt,dual_regime,zo_patch)

! Mixed Layer

!  write(*,*) 'HEAT: ',HEAT,' SWAVE: ',SWAVE,' RNET: ',RNET
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

  TIME = TIME + (DELTA/60)

  IF (REALTM .LT. timend) GO TO 5

  ENDFILE (UNIT = 11)  ! Close the output file
  CLOSE (UNIT = 11)


end
