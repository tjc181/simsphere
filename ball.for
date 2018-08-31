	SUBROUTINE BALL (PES, RHA)

$INCLUDE:'modvars.h'

*/ 24th March 1995 -- RST is always unscaled in this subroutine.
     
C       A BRAND NEW ROUTINE TO GET FCO2 USING PHOTO MODEL      
C
C       PAR      >> PAR IN MOL UNITS
C       RRTOT    >> TOTAL RESISTANCE FOR CO2 DIFFUSION IN MOL UNITS
C       RAIR     >> SURFACE LAYER RESISTANCE
C       FCO2     >> CO2 FLUX (EITHER SCALED PER LEAF OR TOTAL)
C       CII      >> INTERNAL LEAF CO2 IN PPM

C       THE FOLLOWING ARE FROM THE TWO FARQUHAR PAPERS AND THE
C       "NOTE ON CARBON FIXATION MODELING" CONTAINS THE CURRENT 
C       PARAMETERIZATIONS FOR CCOMP, KO, KC, WR.  MOST OF THE 
C       PARAMETERS ARE NOW SET ACCORDING TO COLLATZ, BALL, BERRY.
C
C       AKC >> MM CONSTANT FOR CO2
C       AKO >> MM COMNSTANT FOR O2
C       OXY >> OXYGEN CONCENTRATION IN CHLOROPLASTS
C       VCMAX >> MAXIMUM CARBOXYLATION RATE
C       ALPI >> QUANTUM YEILD (PHOTONS PER CO2)
C       CCOMP >> CO2 COMPENSATION POINT
C       FCO2 >> CARBON EXCHANGE RATE
C       RESP >> DARK RESPIRATION 
C       CII >> INTERNAL CO2 CONCENTRATION 
C       WE >> RUBISCO LIMITED PHOTOSYNTHESIS
C       WR >> RUBP REGENERATION LIMITED PHOTOSYNTHESIS
C       VC >> CARBON EXCAHNGE IN ABSENSE OF PHOTORESPIRATION
C       PARQ >> PAR WHEN WR = WE FOR GIVEN CONDITIONS
C       TAU >> CO2/O2 SPECIFICITY RATIO
C
C       CSS >>>    LEAF SFC CO2 CONCENTRATION
C       GS  >>>    STOMATAL CONDUCTANCE

C       
C       CARBON FLUX AND RESISTANCES SHOULD BE PER UNIT LEAF AREA 
C       TO GET PSHYSIOLOGICAL CII
C

        RST = RST * XLAI / PES
	RAF = RAF * XLAI / PES		  ! Unscaled
	FCO2 = FCO2 * PES / XLAI / FRVEG  ! Unscaled

C
C       PAR IN MOL UNITS
C

        PAR = ((SOL/2)*4.57)*1E-6      ! IN MOL UNITS

	RAIR = RHA + RZASCR
        RRTOT = 1.32 * RAF + 1.66*RST + RAIR
        RRTOT = RRTOT / 40.             ! IN MOL UNITS  


C	SET SOME PARAMTERS  PhotoSyn Model (NOW READ IN START)

	 AKC0 = 3.0E-4
	 AKO0 = 0.300
	 OXY = 0.209
	 VCMAX0 =  2.0E-4 !COLLATZ  !7.5E-5    DATA 1992
	 ALPI = 12
	 RESP0 = 3.0*1E-6
	 TAU0 = 2600
	 AJPAR = 380E-6

	 AMPAR = 9.0	   ! Conductance Model

C
C       CORRECT KINEMATIC PROPERTIES TO TEMPERATURE
C	SEE COLLATZ, BALL, BERRY, FARQUHAR
C
        AKC = AKC0*(2.1**((TF-298)/10))
        TAU = TAU0*(0.57**((TF-298)/10))
        AKO = AKO0*(1.2**((TF-298)/10))
        VCMAX = VCMAX0*(2.4**((TF-298)/10))
	RESP = RESP0*(2.0**((TF-298)/10))
        CCOMP = OXY/(2*TAU)
        AJMAX = AJPAR * (1. + 0.0409*(TF-303.) - 1.54E-3 * 
     &		((TF-303.)**2) - 9.42E-5 * ((TF-303.)**3))

C
C	CORRECT RESP INHIBITION AT HIGH TEMPERATURES
C       FROMM COLLATZ, BALL, BERRY
C

       RESP = RESP*1/(1+EXP((1.3*(TF-328))))

C
C       DO INTERATIONS TO GET CII STABLE WHEN CONDITIONS CHANGING
C       RAPIDLY.
C

20      CONTINUE

C
C	GUESS AT CI
C

	 CI = CO - FCO2 * RRTOT
	 CII = CO - FCO2 * RRTOT  ! Previous Value

	IF (CI .LT. 0) CI = 220*1E-6

C
C       CALCULATE NEW RUBISCO AND RUBP LIMITED RATES
C

	WE = VCMAX*CI/(CI + AKC*(1+(OXY/AKO)))
	AJ = AJMAX * PAR / (PAR + 2.1 * AJMAX)
	WR = AJ * CI / (4.5 * CI + 10.5 * CCOMP)
        WS = VCMAX / 2

C
C       DETERMINE THE FACTOR WHICH IS LIMITIING
C

        IF (WE .LE. WR .AND. WE .LE. WS) THEN
           LIMIT = 'RUBISCO'
        ELSE IF (WR .LT. WE .AND. WR .LT. WS) THEN
           LIMIT = 'E TRANS'
        ELSE
           LIMIT = 'SINK'
        END IF

C
C       SET CARBOXYLATION RATE TO THE LIMITING FACTOR
C

	VC = MIN(WE,WR,WS)
	PARQ = WE * (ALPI*(1+(2*CCOMP/CI)))

C
C       CO2 FLUX
C

	FCO2 = VC*(1 - CCOMP/CI) - RESP
	IF (CCOMP .GT. CI) FCO2 = - RESP
       

c
c       BALL BERRY STOMATAL CONDUCTANCE MODEL
c


C         gs = AMPAR * fco2 * (rhl)/css + 0.01  !IN MOL UNITS

C
C       ESITIMATE OF Gs ---- RECHECK THIS: THERE MAY BE A PROBLEM
C       USING 1.32 FOR WATER VAPOR.
C

	AAA = CI
	BBB = CO / (RAF * 1.32 + RAIR) - AMPAR * FCO2
        CCC = -AMPAR * FCO2 * QAF / (QSTF * RAF)

C
C       SOLUTION IF NO RELATIVE HUMIDITY DEPENDANCE
C	Following 2 statements ....

C	 BBB = CO/(RAF * 1.32 + RAIR)
C        CCC = - AMPAR * FCO2
         
	IF (BBB**2 .LT. 4*AAA*CCC) THEN

	  RST = RCUT

	ELSE

	GS = (-BBB + SQRT(BBB**2 - 4*AAA*CCC))/(2*AAA)
	GS = GS / 40.0				    !MKS UNITS

	IF (GS .NE. 0) RS = 1. / GS  ! Checks
	IF (GS .EQ. 0) RS = 500.0     ! -"-
	RST = RS * RCUT / ( RS + RCUT )

        RRTOT = 1.32 * RAF + 1.66 * RST + RAIR        
	RRTOT = RRTOT / 40.0	      ! Mole units

	CI = CO - RRTOT * FCO2

C
C	CHECK IF CI IS CONVERGING
C

	IF (ABS(1 - CI/CII) .GT. 0.05) GOTO 20


        END IF

	IF (FCO2 .LT. 0.0) RST = RCUT

C
C	SCALE THE RESISTANCE AND CO2 FLUX
C	Not Used in any equations -- just for output

	QBND = (QSTF/RST + QAF/RAF) * (1/RAF + 1/RST)**(-1)
	! Humidity at leaf surface or leaf boundary layer
	RHL = QBND/QSTF  !  RH at Leaf Surface

	CCAN = (CI/(RST*1.66 + RAF*1.32) + CO/(RAIR))*
     /	     (1/(RST*1.66 + RAF*1.32) + 1/RAIR)**(-1) ! CO2 in canopy
	CSS = FCO2 * RAF * 1.66 + CCAN		      ! CO2 at leaf surface

	RAF = RAF * PES / XLAI	   ! Scaling Back
        RST = RST * PES / XLAI
	FCO2 = FCO2 * XLAI / PES * FRVEG

        RETURN
        END
