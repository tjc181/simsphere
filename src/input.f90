subroutine INPUT
  use simsphere_mod
  implicit none

! ** Modified 8/14/90 >> Modified Hi (SKONST / S).

! **  Subroutine INPUT computes solar radiation as a function of the
! **  day, season, atmospheric attenuation, and albedo.

!  real(kind=8) :: EFFDEC, SOLSIN
  real :: EFFDEC=0.0, SOLSIN=0.0
  real :: SS(12)
  integer :: MD(12)

! Don't need to declare FTABS, FBSCAT, FTSCAT -- subroutine rewritten as function
!  real :: FTABS, FBSCAT, FTSCAT,  RM, S, N
  real :: RM=0.0, S=0.0, N=0.0, RLPATH=0.0, PATH=0.0
  real :: TABS=0.0, BSCAT=0.0, TSCAT=0.0, TABSD=0.0, BSCATD=0.0, TSCATD=0.0
  real :: DF=0.0, DE=0.0, SIG=0.0, EQT=0.0, HRANGL=0.0, SLB=0.0, SHEAT=0.0, HI=0.0, XSER=0.0, SOLEL=0.0
  real :: sinsolelslope=0.0, solelslope=0.0, GMT=0.0
  real, parameter :: skonst = 1.94*4.1868e4/60
  integer :: IMO1=0, JMO=0, I=0, K=0, DAD=0

!      INCLUDE 'modvars.h'

! **  The definition of the variables in the following DATA statements
! **  can be found in the manual.

  DATA MD / 31 , 29 , 31 , 30 , 31 , 30 , 31 , 31 , 30 , 31 , 30, 31/
  DATA SS / 0.967 , 0.971 , 0.982 , 0.999 , 1.016 , 1.029 , 1.034,      &
          1.030 , 1.018 , 1.002 , 0.985 , 0.973 /

! **  Y ...Counter for determining celestial mechanics for long, lat and
! **  time of year.

  IF ( Y /= 1 ) GO TO 19
  Y = Y + 1

  IMO1 = IMO + 1
  IF (IMO1 == 13) IMO1 = 1

! **  Compute the solar distance factor.

  RM = FLOAT( IDAY - 1 ) / FLOAT( MD ( IMO ) )
  S = SS( IMO ) + RM * (SS( IMO1 ) - SS( IMO ) )

! **  Set up the number of the day in the year (DAD).

  K = IDAY
  JMO = IMO - 1
  IF ( JMO .LT. 1 ) GO TO 12
  do I = 1 , JMO
    K = K + MD(I)
  end do
12  N = IYR / 4
  N = IYR - N * 4
  IF (N /= 0 .AND. K .GE. 60) K = K - 1
  DAD = K - 1

! **  Calculate the angular fraction of a year, convert to radians.

  DF = DAD * 360 / 365.242
  DE = real(DF / RADIAN,4)

! **  Correction to declination caused by elliptical orbit.

  SIG = SIGA + DF + 1.914827 * SIN( DE ) - 0.079525 * COS( DE )         &
        & + 0.019938 * SIN( DE * 2 ) - 0.00162 * COS( DE * 2 )
  SIG = real(SIG / RADIAN,4)

! **  Declination of the sun.

  EFFDEC = real(ASIN( SDEC * SIN( SIG ) ),4)

! **  True solar noon.

  EQT = 0.12357 * SIN( DE ) - 0.004289 * COS( DE )                      &
        & + 0.153809 * SIN( DE * 2 ) + 0.060783 * COS( DE * 2 )
  EQT = EQT + 12

19 CONTINUE

! **  Calc time in Greenwich Mean Time.

  GMT = PTIME + TZ

!  Calculate the solar hour angle in radians.

  HRANGL = 15 * ( GMT - EQT ) - XLONG
  HRANGL = real(HRANGL / RADIAN,4)

!  Now we can finally compute the solar elevation angle.

  SLB = real(XLAT / RADIAN,4)
  SOLSIN = SIN(EFFDEC)*SIN(SLB)+COS(EFFDEC)*COS(SLB)*COS(HRANGL)
  SOLEL = real(ASIN( SOLSIN ) * RADIAN,4)

  if (SLOPE .GT. 0) then
    call sslope (sinsolelslope,solelslope,effdec,slb,hrangl)
  else
    call albedo (solsin)
    sinsolelslope = solsin
  end if

!  If the solar altitude is less that or equal to zero it's night so
!  return, otherwise compute absolute optical air mass (PATH).

  IF ( SOLEL <= 0 ) THEN
    SWAVE = 0
    RETURN
  END IF

  RLPATH = ( SOLSIN + 0.15 * ( SOLEL + 3.885 ) ** (-1.253) ) ** (-1)
!      if (rlpath .lt. 1.0) rlpath = 1.0 
  PATH = 0.001 * PS1 * RLPATH
  SOLEL = real(SOLEL / RADIAN,4)

!  write(*,*) 'SOLSIN: ',SOLSIN, ' SOLEL: ',SOLEL,' RLPATH: ',RLPATH,' PATH: ',PATH
!TJC  These lines moved to functions in module: ftabsT, ftscatT, fbscatT
!  CALL TRANSM (RLPATH,FTABS,FTSCAT,FBSCAT)

!  Store values for use.

!  TABS = FTABS
!  TSCAT = FTSCAT
!  BSCAT = FBSCAT

  if ( RLPATH == RLPATH ) then
    TABS = ftabsT(RLPATH)
    TSCAT = ftscatT(RLPATH)
    BSCAT = fbscatT(RLPATH)
  else
    write(*,*) 'RLPATH is NaN, input.f90:128'
    stop
  end if

!TJC End

!  Set the PATH length for diffuse radiation equal to 1.7.

  PATH = 1.7

!TJC  These lines moved to functions in module: ftabsT, ftscatT, fbscatT
!  CALL TRANSM (PATH,FTABS,FTSCAT,FBSCAT)
!
!!  Again store values for use.
!
!  TABSD  = FTABS
!  TSCATD = FTSCAT
!  BSCATD = FBSCAT

   TABSD = ftabsT(PATH)
   TSCATD = ftscatT(PATH)
   BSCATD = fbscatT(PATH)

!TJC End

! **  SHEAT >>> Sunlight amount on horizontal plane outside atmosphere
! **  XSER  >>> Diffuse Shortwave Radiation at the ground.
! **  HI    >>> Direct SW Radiation reaching the ground.
! **  SWAVE >>> Diffuse + Direct = 'Global'

  if (S /= 0) SHEAT = SKONST * sinsolelslope / S
  XSER  = BSCATD * ALBDOE * ( 1 - TSCATD ) * TABSD * SIN( SOLEL )
  if (S /= 0) HI = ( SHEAT * TABS * TSCAT ) + ( SKONST / S* TABS * ( 1 - TSCAT )    &
&       * ( 1 - BSCAT ) ) * SIN( SOLEL )
  if (XSER /= 1.0) then
    SWAVE = ( HI * ( 1 - ALBDOE ) ) / ( 1.0 - XSER )
  else
    write(*,*) 'Attempted divide by zero: input.f90:168'
    stop
  end if
  

  IF(CLOUD_FLAG) SWAVE = SWAVE*(1-(0.7*(0.1*CLD_FRACT)))
       ! impose cloud fraction; reduce swave according to cloud amount
       ! Note: forcing real arithmetic

  return
end


