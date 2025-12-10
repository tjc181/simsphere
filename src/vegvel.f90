subroutine  VEGVEL()
  use simsphere_mod
  implicit none

  real :: SGMA
  real :: PES, RHOCP, SIGALF, CDL, PI, SDL, rhg, RMRATIODIF, RHA, QSTA
  real :: XLEFN, TFN

  logical, save :: init_vel = .true.

!      INCLUDE 'modvars.h'

  PES = ( XLAI / 2.0 ) + 1
  RHOCP = CP * PS1 * 100 / ( R * TAF )
  SIGALF = 1 - 0.5 / ( 0.5 + XLAI ) * EXP( -XLAI**2 / 8 )

!     CDL IS THE DRAG COEFFICIENT FOR THE LEAF
!     KEL'S FORMULATION

  CDL = 0.08
  PI = 3.1416

  SDL = XLAI/VEGHEIGHT
  UAF = USTAR * SQRT (PES/(CDL * SDL))

  chf = 0.011 * SQRT(UAF/WIDTH)*(XLAI/PES)

  IF (CHF < 0.001) CHF = 0.001

  RAF = 1 / ( CHF )


!      CHG = ( 1 - SIGALF ) * USTAR**2 / UAF
!        Kell's chg
        chg = Karman**2*uaf/(alog(vegheight*100/0.7))**2 
!       Add surface layer resistance to internal air resistance and recompute conductance
  rhg = 1/chg + rtranw
  chg = 1/rhg

!     FOLLOWING IS KEL'S FORMULATION FROM GOUDRIAAN
!     TO GET CHA

!      ALEN = 2 * ((3 * WIDTH**2) / (4 * PI * XLAI/(VEGHEIGHT/2)))
!     /        ** (0.3333)
!     AKCAN = ALEN * 0.5 * UAF
!      RHA = VEGHEIGHT / (2 * AKCAN)
!      CHA = 1 / RHA
!       Above is outdated formulae from Kell. Use old method

  cha = ustar**2 / (uten - uaf )

  IF (CHA <= 0.001) CHA = 0.001

! Convert Q to ELTL and EA.

  QSTF = 10**( 6.1989 - ( 2353 / TF ) )
  SGMA = RHOCP / 0.666
  RMRATIODIF = QSTF - QAF
  IF (RMRATIODIF <= 0) RMRATIODIF = .0001
  VFL = (RMRATIODIF) * PS1 / 0.622

! Call Deardorff (D) or Carlson/Lynn formulation for RST.
! Note RST is actually RF because it contains a cuticular resistance (RCUT).

  IF (STMTYPE == 'D') THEN  ! Deardorff Formulation
    RS = RMIN * (800.0 / (1.0+SOL) + (1.2 * WILT/                       &
         (0.9*W2G+0.1*WGG))**2) ! Stomatal Resistance 

    RST = RS * RCUT / (RS + RCUT) * PES / XLAI ! Total Leaf/Canopy Resistance

  ELSE IF (STMTYPE == 'L') THEN ! Lynn and Carlson
    CALL PSLCAL (SGMA, PES)

    RL = RS * RCUT / ( RS + RCUT )   ! Leaf Resistance
    RST = RL * PES / XLAI    ! Total Leaf/Canopy Resistance

  ELSE IF (STMTYPE == 'B') THEN ! Ball-Berry
    CALL BALL (PES, RHA)

    RL = RS * RCUT / ( RS + RCUT )   ! Leaf Resistance
    RST = RL * PES / XLAI    ! Total Leaf/Canopy Resistance

  END IF

  QSTA = 10**( 6.1989 - ( 2353 / TA ) )
  XLEFN = DENS * LE * RMRATIODIF / ( RST + RAF )

!    Average TF and XLEF after initial time step.

  IF (init_vel .eqv. .true.) THEN
    TF = TAF + ( RNETF - XLEFN ) / (CHF * DENS * CP)
    XLEF = XLEFN
    init_vel = .false.
  ELSE
    XLEF = ( XLEF + XLEFN ) / 2
    TFN = TAF + ( RNETF - XLEFN ) / (CHF * DENS * CP)
    TF = (TF + TFN) / 2
  END IF

  RETURN
END
