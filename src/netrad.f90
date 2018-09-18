subroutine  NETRAD (Time,BareRadioTemp,VegnRadioTemp,BareNetRadn,VegnNetRadn,MixedNetRadn,Init)
  use simsphere_mod
  implicit none

!  Subroutine NETRAD computes up and down longwave fluxes and
!  the Net Radiation.

  real :: BareRadioTemp,VegnRadioTemp
  real :: BareNetRadn,VegnNetRadn,MixedNetRadn
  real :: VegnShortWave
  real :: Lwup
  real :: time
  integer(kind=1) :: Init

!      INCLUDE 'modvars.h'

!    Compute the solar radiation.

  if ( time >= 0.0 ) then
    call Input
  else
    write(*,*) 'Error: negative time detected.'
    continue
  end if

!  Calculate the net radiation for the appropriate ground conditions.
!  The decision is made on the fraction of vegetation.

!  Code added 28/11/90 to solve the Partial routine issue
!  Initialise and calculate the effective emissivity of the air and
!  longwave down using a weighted average of surface and air temperature.

  If (Init .eq. 1) Then
    Aepsi = 0.700 + 0.17 * Alog10(Omega)
    if(cloud_flag) aepsi = aepsi + (1 - aepsi)*0.8*(0.1*cld_fract)
    ! Note: forcing real arithmetic

    BareRadioTemp = Tscren - 2
    VegnRadioTemp = Tscren - 2
    Init = 2
  End If

! Vegetation

  If (Frveg .eq. 1) Then

    call Lwdown
    call Vegrad (Time,VegnNetRadn,VegnShortWave,VegnRadioTemp)
    Rnet = VegnNetRadn
    Swave = VegnShortWave

! Bare Soil

  Elseif (Frveg .eq. 0) Then

    call Lwdown
    call Uplong (Lwup,BareRadioTemp)
    BareNetRadn = Lwdn*epsi + Swave - Lwup
    Rnet = BareNetRadn

! Mixed

  Elseif (Frveg .gt. 0 .and. Frveg .lt. 1) Then

    call Lwdown
    call Uplong (Lwup,BareRadioTemp)
    BareNetRadn = Lwdn*epsi + Swave - Lwup
    call Vegrad (Time,VegnNetRadn,VegnShortWave,VegnRadioTemp)

    MixedNetRadn = (VegnNetRadn * Frveg) + (1-Frveg) * BareNetRadn
    Rnet = MixedNetRadn
    Swave = VegnShortWave * Frveg + Swave * (1- Frveg)

! Error

  Else

!  Print*, ' Logic Error has occurred in rountine Netrad '
!  print*, 'frveg = ',frveg, '  frveg-1 = ',(frveg-1)
!   Print*, ' Stop Program '
!   Stop
    continue

  Endif

  Return
End

Subroutine  Lwdown
  use simsphere_mod, only: aepsi, sigma, t_fine, tdif_s
  implicit none

!      INCLUDE 'modvars.h'
  real :: Lwdn

  Lwdn = aepsi*sigma*(T_fine(3) - Tdif_s - 1.5)**4

  Return
End

Subroutine  UpLong (Lwup,SpecTemperature)
  use simsphere_mod, only: epsi, sigma
  implicit none

! epsi and sigma declared in simsphere module
!  real :: SpecTemperature,Lwup,epsi,sigma
  real :: SpecTemperature,Lwup

! Common blocks include via the "use" statement
!  COMMON /KONST/ CP,LE,SIGMA,KARMAN,GRAV,R
!  COMMON /NETCOM/ EPSI,EPSF,ALBF,XLAI,SOL,RNETG,RNETF,AEPSI

  Lwup =  epsi*sigma*SpecTemperature**4

  Return
End
