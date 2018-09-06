subroutine  MOM (Pot_S,DT,MONCE)
  use simsphere_mod

! **  Subroutine MOM calculates the momentum and thermodynamic eqs. in the
! **  lowest 500m of the atmosphere and produces profiles of the U & V
! **  components, humidity and temperature at 50m intervals to couple
! **  surface and mixing layers.


  real :: CR(46), OK(46), BX(46), WIND(46), WGEOS(46)
  real :: B(46), DUW(46), KRAD, DVW(46), DTW(46), RI(46)
  real :: DZ, SB
  integer :: IMAX, IMAX1

!      INCLUDE 'modvars.h'

  KRAD = 0.75


  OKMAX = 1
  X1 = 0

  IF ( MONCE .EQ. 0 ) THEN

! **  Perform initialisations.
! **  UIF,UIF2 are correction factors to account for 1st layer not being
! **  centred when the night SFL depth is below 50 m (ZA).
! **  This is not necessary at present as ZA = 50 m.

    DZ = 50.0 
    SB = 784.0
    IMAX = 50

    UIF = ( 2 * DZ / ZA ) - 1
    UIF2 = DZ / ( 2 * DZ - ZA )

! **  Set up constants for the Richardson number calculation.

    RC = ( GRAV * DZ ) / OTEMP
    IMAX1 = IMAX - 1

! **  Set values of Critical Richardson # and turbulent diffusivities to
! **  a nominal value and smooth the temperature profile.

    do I = 1 , IMAX1
      CR(I) = 0.001
      OK(I) = 0.01
      T(I) = ( T(I) + T(I+1) ) / 2
    end do

! **  Make the diffusion coefficient (BX1) for radiation unitless.

    BX1 = KRAD * DT / ( DZ**2 )
    MONCE = 2

  END IF

! **  Calculate bulk parameters .... Windspeed, Geostrophic Windspeed
! **  and Critical Richardson number at all levels.

100 do I = 1 , IMAX
      WIND(I) = SQRT( U(I)**2 + V(I)**2 )
      WGEOS(I) = SQRT( UG(I)**2 + VG(I)**2 )
      CR(I) = ( EXP( -0.2129 * WGEOS(I) ) * 0.5542 ) + 0.2
    end do

! **  Take derivatives of U and V and their absolute value.  Set a
! **  minimum value for the derivatives.  Calculate the local Richardson
! **  number using the expression for Si and our constant (g/  ).
! **  Next, find the values for the eddy exchange coefficients (Kh & Km);
! **  note ... assumed to be the same in the stable nocturnal boundary
! **  layer. (SB = l).  Smooth and set the non-dimensional forms for
! **  momentum (B) and heat (BX).

  do I = 2 , IMAX
    DU = U(I) - U(I-1)
    DV = V(I) - V(I-1)
    ABDU = ABS(DU)
    ABDV = ABS(DV)
    IF ( ABDU .LT. 0.001) DU = 0.001
    IF ( ABDV .LT. 0.001) DV = 0.001
    RI(I) = RC * ( T(I) - T(I-1) ) / ( (DU**2) + (DV**2) )
    OK(I) = SB * ( ( CR(I) - RI(I) ) / CR(I) ) * ( SQRT(DU**2 + DV**2 ) / DZ )
    OK(I) = OK(I) + 0.05 * OK(I-1)

    IF ( RI(I) .GE. CR(I) ) OK(I) = 0
    B(I) = ( OK(I) * DT ) / ( DZ**2 )
    BX(I) = ( (OK(I) + KRAD) * DT ) / ( DZ**2 )

    IF ( B(I) .GT. 0.25 ) B(I) = 0.25
    IF ( BX(I) .GT. 0.25 ) BX(I) = 0.25
    IF ( OK(I) .GT. OKMAX ) OKMAX = OK(I)

  end do

! **  Calculate the vertical profiles of temperature and wind from 50
! **  to 500 metres by integrating the U and V momentum equations and
! **  the thermodynamic equation.

  do I = 2 , IMAX1
    IF ( I .GT. 2) UIF2 = 1
    DUW(I) = B(I+1) * ( U(I+1) - U(I) ) - B(I) * ( U(I) -U(I-1) ) * UIF2
    DVW(I) = B(I+1) * ( V(I+1) - V(I) ) - B(I) * ( V(I) - V(I-1) ) * UIF2
    DTW(I) = BX(I+1) * ( T(I+1) - T(I) ) - BX(I) * ( T(I) - T(I-1) ) * UIF2
    QN(I) = QN(I) + ( B(I+1) * ( QN(I+1) - QN(I) ) - B(I) * ( QN(I) - QN(I-1) ) )
    U(I) = U(I) + CF * DT * ( V(I) - VG(I) ) + DUW(I)
    V(I) = V(I) - CF * DT * ( U(I) - UG(I) ) + DVW(I)
    T(I) = T(I) + DTW(I) - ( RAD - ADVGT ) * DT
  end do

! **  Integrate at top and bottom boundaries where no turbulent
! **  exchange exists and at the surface boundary condition.

  T(1) = T(1) + HEAT * DT / ( CP * DENS * ZA ) + BX(2) *                &
         ( T(2) - T(1) ) - ( RAD - ADVGT ) * DT
  T(1) = T(1) - BX1 * ( T(1) - Pot_S )
  U(1) = U(1) + CF * DT * ( V(1) - VG(1) ) + B(2) * ( U(2) - U(1) )     &
         -DT / DZ * USTAR**2 * U(1) / WIND(1) * UIF
  V(1) = V(1) - CF * DT * ( U(1) - UG(1) ) +B(2) * ( V(2) - V(1) )      &
         -DT / DZ * USTAR**2 * V(1) / WIND(1) * UIF
  QN(1) = QN(1) + ( B(2) * ( QN(2) - QN(1) ) + ( EVAP * DT / ( DENS * LE * DZ ) ) )
  U(IMAX) = U(IMAX) + CF * DT * ( V(IMAX) - VG(IMAX) ) - B (IMAX) *     &
            ( U(IMAX) - U(IMAX1) )
  V(IMAX) = V(IMAX) - CF * DT * ( U(IMAX) - UG(IMAX) ) - B (IMAX) *     &
            ( V(IMAX) - V(IMAX1) )
  T(IMAX) = T(IMAX) - BX(IMAX) * ( T(IMAX) - T(IMAX1) ) - ( RAD - ADVGT ) * DT

! **  Here we set QD = QN to be used in flux.

  QD(1) = QN(1)

! **  Increment the time control and cycle through twice (120s).

  X1 = X1 + 1
  XMOD = AMOD ( X1 , 2.0 )
  IF ( XMOD .NE. 0 ) GO TO 100

  AWIND =  ( SQRT(U(1)**2 + V(1)**2) )

! Fine Mesh 

  do i=1,imax
    u_fine(i) = u(i)
    v_fine(i) = v(i)
    t_fine(i) = t(i)
    q_fine(i) = qn(i)
  end do

  return
end
