SUBROUTINE  CALC (OLDTMP, No_Rows)
  use simsphere_mod, only: xlat, xlong, degs_to_radians, rot_rate_earth, &
                           timend, strtim, outtt, atemp, otemp, tscren,  &
                           t, frveg, cf
  implicit none

  integer :: No_Rows
  real :: Lat_in_radians, K

  real :: OLDTMP, DECTIM, out_time_intv

  K = XLAT
  XLAT= (XLAT-K) / 0.6 + K
  K = XLONG
  XLONG = (XLONG-K) / 0.6 + K

! Convert Lat to radians, calc Coriolis force.

  Lat_in_radians = XLAT * Degs_To_Radians
  CF = 2 * rot_rate_earth * SIN(Lat_in_radians)

! Time conversions

  TIMEND = DECTIM(TIMEND) ! Convert to Decimal time
  STRTIM = DECTIM(STRTIM)

!  out_time_intv = outtt / 60
  outtt = dectim(outtt)
  out_time_intv = outtt
! Not needed with dectim(outtt) call -TJC
!  out_time_intv = dectim(out_time_intv)
  No_Rows = INT(((Timend - strtim)) / out_time_intv) + 1 






! Store and initialize temperatures

  OLDTMP = ATEMP
  t(1) = Atemp + 0.5
  OTEMP = TSCREN - 2

  FRVEG  = FRVEG / 100  ! Vegetation % as a fraction

  return
end
 
real pure function dectim(t)
  implicit none
! Converts time formatted as "HHMM" (2-digit hours and 2-digit minutes) to decimal seconds
  real, intent(in) :: t
  real :: hour, minute

  minute = modulo(t,100.0)
  hour = (t - minute)/100.0

  dectim = ( hour + (minute/60.0) ) * 3600
end function dectim
