program simsphere
  use simsphere_mod
  use json_module
  use iso_fortran_env, only: real64
  implicit none

! Main program lists and defines model symbols, coordinates model
! subroutines, and establishes iteration sequence.

! Type declaration of variables.

!  interface
!    subroutine output(j, o)
!      use json_module
!      type(json_core) :: j
!      type(json_value), pointer :: o
!    end subroutine output
!  end interface

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
  logical :: dual_regime = .false.

! Subroutine variables that are preserved between calls
  real :: AIRCHGT=0.0, AIRCDELT, AIRCTHETA=1.0
  real :: WATERWIN=0.0
  real :: BELOWTE(9)=0.0
  integer(kind=1) :: VEGVELinit_vel=1
  integer :: averageinit=1
  integer :: PSLCALINIT=1
  integer :: outputinit=1

  type(t_timeloc) :: timeloc
  type(t_temp) :: temp
  type(t_windsnd) :: windsnd
  type(t_wind) :: wind
!AAP Not used in main:
!  type(t_met) :: met
!  type(t_veg) :: veg
!  type(t_soil) :: soil

  type(json_core) :: json
  type(json_value), pointer :: p, out

  character(len=12), parameter :: out_json = 'o_model.json'
  character(len=11), parameter :: out_file = 'o_model.dat'

  
!      INCLUDE 'modvars.h'

! Start reads the values of various input parameters to get the model
! going and Snding reads in the sounding file.

  open ( unit=11, file = out_file )         ! Open the text output file

!  CALL START (Obst_Hgt,dual_regime,zo_patch) ! Read and Check data
  CALL START (Obst_Hgt, dual_regime, zo_patch, temp, windsnd, timeloc, wind)   ! Read data
     
  CALL SNDING (ZLS, Old_Ahum, temp, windsnd, timeloc, wind)  ! Read Sounding - Call Spline to Interpolate

  CALL CALC (OLDTMP, No_Rows)        ! Some basic calculations

  CALL PRFILE                       ! Set a Geostrophic wind at the surface

  CALL GETTBL                       ! Is the lookup table for subroutine TRANSM.

! This is the start of the diurnal loop (TIMEND-STRTIM), nominally 22 hrs.

  CALL PSOIL                        ! Set up for the soil

! Initialize JSON output
  call json % initialize(compact_reals=.true., real_format='*')
  call json % create_object(p,'')
  
  do while (realtm < timend)
!  5 CONTINUE                          ! Loop back here every 180 seconds
  
  ! Initially TIME = 0, so REALTME = STRTIM but remember
  ! TIME is incremented each step.
  
  ! STRTIM is the local clock time in seconds at which the model starts.
  ! REALTM is the local clock time in seconds of the current model iteration.
  ! TIME is the model integration time in seconds with the first iteration starting at TIME=0
  ! TIMEND is the local clock time in seconds at which the model should stop.
  ! PTIME is the local clock time in decimal hours of the current model iteration (REALTM/3600).
  ! OUTTT is the interval in seconds at which to write output.
    REALTM = TIME + STRTIM
    PTIME = REALTM / 3600.
    if ( .not. eq(OUTTT,0.0) ) then
      TMOD = MOD (TIME,OUTTT)
    end if
                      
  !                CALL SUROUTINES IN THE SOLUTION SEQUENCE.
  
  ! Net Radiation
  
    CALL NETRAD (TIME,BareRadioTemp,VegnRadioTemp,BareNetRadn,VegnNetRadn,MixedNetRadn,Init)
  
  ! Resistance values in the Transition and Surface Layers
  ! Entry to nighttime formulations (BRI & MOM) through this subroutine
  
    CALL VEL (MONCE,IONCE,StabCriteria,YCOUNT,Obst_Hgt,dual_regime, &
    zo_patch,VEGVELinit_vel,PSLCALINIT)
  
  ! Mixed Layer
  
  !  write(*,*) 'HEAT: ',HEAT,' SWAVE: ',SWAVE,' RNET: ',RNET
    IF (HEAT>0.00001 .AND. SWAVE > 0 .AND. RNET > 0) THEN
      CALL AIR (ZLS, YCOUNT, AIRCHGT, AIRCDELT, AIRCTHETA)
    END IF
  
  ! Eddy Diffusivities in the Mixed Layer
  
    IF (HEAT>0.00001 .AND. SWAVE > 0 .AND. RNET > 0) THEN
      CALL DAYKM(windsnd%thick)
    END IF
  
  ! Momentum Equations - Mixed Layer
  
    IF (HEAT>0.00001 .AND. SWAVE > 0 .AND. RNET > 0) THEN
      CALL MOMDAY(windsnd%thick)
    END IF
  
  ! Evaporative Flux, Surface Temperature solutions
  
    CALL FLUX (BareRadioTemp,VegnRadioTemp,BareEvapFlux,BareHeatFlux,averageinit)
  
  ! Heat FLux - Penman Formulation
  
    IF((HEAT>=0 .or. RNET >0).AND.SWAVE >0) THEN
      CALL HOT (B,BareNetRadn,BareEvapFlux,BareHeatFlux)
    END IF
    
  !
    if (rnet > 0)then
      StabCriteria = 1
    else
      StabCriteria = 2
    endif
  
  ! End of the atmospheric cycle.
  
    CALL BELOW (Time,BareRadioTemp,BareEvapFlux,WATERWIN,BELOWTE) ! Substrate

  !  Output is written every OUTTT seconds.
  
    IF (eq(TMOD,0.0)) then
      call json % create_object(out,'output')
      call json % add(p,out)
      CALL output(json, out, outputinit)
    end if
  
  ! Increment Time.
  
    TIME = TIME + DELTA
  
!    IF (REALTM < timend) GO TO 5
  
  end do

  call json % print(p,out_json)
  call json % destroy(p)
  if (json % failed()) stop 1

  ENDFILE (UNIT = 11)  ! Close the text output file
  CLOSE (UNIT = 11)

end
