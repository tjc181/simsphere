subroutine BALL (PES, RHA)
  use simsphere_mod

!        INCLUDE 'modvars.h'

!/ 24th March 1995 -- RST is always unscaled in this subroutine.
     
!       A BRAND NEW ROUTINE TO GET FCO2 USING PHOTO MODEL      
!
!       PAR      >> PAR IN MOL UNITS
!       RRTOT    >> TOTAL RESISTANCE FOR CO2 DIFFUSION IN MOL UNITS
!       RAIR     >> SURFACE LAYER RESISTANCE
!       FCO2     >> CO2 FLUX (EITHER SCALED PER LEAF OR TOTAL)
!       CII      >> INTERNAL LEAF CO2 IN PPM

!       THE FOLLOWING ARE FROM THE TWO FARQUHAR PAPERS AND THE
!       "NOTE ON CARBON FIXATION MODELING" CONTAINS THE CURRENT 
!       PARAMETERIZATIONS FOR CCOMP, KO, KC, WR.  MOST OF THE 
!       PARAMETERS ARE NOW SET ACCORDING TO COLLATZ, BALL, BERRY.
!
!       AKC >> MM CONSTANT FOR CO2
!       AKO >> MM COMNSTANT FOR O2
!       OXY >> OXYGEN CONCENTRATION IN CHLOROPLASTS
!       VCMAX >> MAXIMUM CARBOXYLATION RATE
!       ALPI >> QUANTUM YEILD (PHOTONS PER CO2)
!       CCOMP >> CO2 COMPENSATION POINT
!       FCO2 >> CARBON EXCHANGE RATE
!       RESP >> DARK RESPIRATION 
!       CII >> INTERNAL CO2 CONCENTRATION 
!       WE >> RUBISCO LIMITED PHOTOSYNTHESIS
!       WR >> RUBP REGENERATION LIMITED PHOTOSYNTHESIS
!       VC >> CARBON EXCAHNGE IN ABSENSE OF PHOTORESPIRATION
!       PARQ >> PAR WHEN WR = WE FOR GIVEN CONDITIONS
!       TAU >> CO2/O2 SPECIFICITY RATIO
!
!       CSS >>>    LEAF SFC CO2 CONCENTRATION
!       GS  >>>    STOMATAL CONDUCTANCE

!       
!       CARBON FLUX AND RESISTANCES SHOULD BE PER UNIT LEAF AREA 
!       TO GET PSHYSIOLOGICAL CII
!
  character(len=7) :: LIMIT

  RST = RST * XLAI / PES
  RAF = RAF * XLAI / PES  ! Unscaled
  FCO2 = FCO2 * PES / XLAI / FRVEG  ! Unscaled

!
!       PAR IN MOL UNITS
!

  PAR = ((SOL/2)*4.57)*1E-6      ! IN MOL UNITS

  RAIR = RHA + RZASCR
  RRTOT = 1.32 * RAF + 1.66*RST + RAIR
  RRTOT = RRTOT / 40.             ! IN MOL UNITS  


!	SET SOME PARAMTERS  PhotoSyn Model (NOW READ IN START)

  AKC0 = 3.0E-4
  AKO0 = 0.300
  OXY = 0.209
  VCMAX0 =  2.0E-4 !COLLATZ  !7.5E-5    DATA 1992
  ALPI = 12
  RESP0 = 3.0*1E-6
  TAU0 = 2600
  AJPAR = 380E-6

  AMPAR = 9.0   ! Conductance Model

!
!       CORRECT KINEMATIC PROPERTIES TO TEMPERATURE
!	SEE COLLATZ, BALL, BERRY, FARQUHAR
!
  AKC = AKC0*(2.1**((TF-298)/10))
  TAU = TAU0*(0.57**((TF-298)/10))
  AKO = AKO0*(1.2**((TF-298)/10))
  VCMAX = VCMAX0*(2.4**((TF-298)/10))
  RESP = RESP0*(2.0**((TF-298)/10))
  CCOMP = OXY/(2*TAU)
  AJMAX = AJPAR * (1. + 0.0409*(TF-303.) - 1.54E-3 * ((TF-303.)**2) - 9.42E-5 * ((TF-303.)**3))

!
!	CORRECT RESP INHIBITION AT HIGH TEMPERATURES
!       FROMM COLLATZ, BALL, BERRY
!

  RESP = RESP*1/(1+EXP((1.3*(TF-328))))

!
!       DO INTERATIONS TO GET CII STABLE WHEN CONDITIONS CHANGING
!       RAPIDLY.
!

20      CONTINUE

!
!	GUESS AT CI
!

  CI = CO - FCO2 * RRTOT
  CII = CO - FCO2 * RRTOT  ! Previous Value

  if (CI .LT. 0) CI = 220*1E-6

!
!       CALCULATE NEW RUBISCO AND RUBP LIMITED RATES
!

  WE = VCMAX*CI/(CI + AKC*(1+(OXY/AKO)))
  AJ = AJMAX * PAR / (PAR + 2.1 * AJMAX)
  WR = AJ * CI / (4.5 * CI + 10.5 * CCOMP)
  WS = VCMAX / 2

!
!       DETERMINE THE FACTOR WHICH IS LIMITIING
!

  IF (WE .LE. WR .AND. WE .LE. WS) THEN
    LIMIT = 'RUBISCO'
  ELSE IF (WR .LT. WE .AND. WR .LT. WS) THEN
    LIMIT = 'E TRANS'
  ELSE
    LIMIT = 'SINK'
  END IF

!
!       SET CARBOXYLATION RATE TO THE LIMITING FACTOR
!

  VC = MIN(WE,WR,WS)
  PARQ = WE * (ALPI*(1+(2*CCOMP/CI)))

!
!       CO2 FLUX
!

  FCO2 = VC*(1 - CCOMP/CI) - RESP
  IF (CCOMP .GT. CI) FCO2 = - RESP
       

!
!       BALL BERRY STOMATAL CONDUCTANCE MODEL
!


!         gs = AMPAR * fco2 * (rhl)/css + 0.01  !IN MOL UNITS

!
!       ESITIMATE OF Gs ---- RECHECK THIS: THERE MAY BE A PROBLEM
!       USING 1.32 FOR WATER VAPOR.
!

  AAA = CI
  BBB = CO / (RAF * 1.32 + RAIR) - AMPAR * FCO2
  CCC = -AMPAR * FCO2 * QAF / (QSTF * RAF)

!
!       SOLUTION IF NO RELATIVE HUMIDITY DEPENDANCE
!	Following 2 statements ....

!	 BBB = CO/(RAF * 1.32 + RAIR)
!        CCC = - AMPAR * FCO2
         
  IF (BBB**2 .LT. 4*AAA*CCC) THEN
    RST = RCUT
  ELSE
    GS = (-BBB + SQRT(BBB**2 - 4*AAA*CCC))/(2*AAA)
    GS = GS / 40.0    !MKS UNITS
    IF (GS .NE. 0) RS = 1. / GS  ! Checks
    IF (GS .EQ. 0) RS = 500.0     ! -"-
    RST = RS * RCUT / ( RS + RCUT )
    RRTOT = 1.32 * RAF + 1.66 * RST + RAIR        
    RRTOT = RRTOT / 40.0      ! Mole units

    CI = CO - RRTOT * FCO2

!
!	CHECK IF CI IS CONVERGING
!

    IF (ABS(1 - CI/CII) .GT. 0.05) GOTO 20

  END IF

  IF (FCO2 .LT. 0.0) RST = RCUT

!
!	SCALE THE RESISTANCE AND CO2 FLUX
!	Not Used in any equations -- just for output

  QBND = (QSTF/RST + QAF/RAF) * (1/RAF + 1/RST)**(-1)
! Humidity at leaf surface or leaf boundary layer
  RHL = QBND/QSTF  !  RH at Leaf Surface

  CCAN = (CI/(RST*1.66 + RAF*1.32) + CO/(RAIR))*(1/(RST*1.66 + RAF*1.32) + 1/RAIR)**(-1) ! CO2 in canopy
  CSS = FCO2 * RAF * 1.66 + CCAN    ! CO2 at leaf surface

  RAF = RAF * PES / XLAI   ! Scaling Back
  RST = RST * PES / XLAI
  FCO2 = FCO2 * XLAI / PES * FRVEG

  return
end
