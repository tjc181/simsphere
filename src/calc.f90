SUBROUTINE  CALC (OLDTMP, No_Rows)
  use simsphere_mod
  implicit none

  integer :: No_Rows
  real :: Lat_in_radians, K

  real :: OLDTMP, DECTIM, out_time_intv
! Apparently only used in this subroutine to generate SATAM, SATPM globals,
! which are unuse in rest of program
! real :: CLKTAM, CLKTPM

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
  out_time_intv = outtt
  out_time_intv = dectim(out_time_intv)
  No_Rows = INT(((Timend - strtim)) / out_time_intv) + 1 

! Apparently unused in entire program...
!  SATAM = DECTIM(CLKTAM)
!  SATPM = DECTIM(CLKTPM)





! Store and initialize temperatures

  OLDTMP = ATEMP
  t(1) = Atemp + 0.5
  OTEMP = TSCREN - 2

  FRVEG  = FRVEG / 100  ! Vegetation % as a fraction

  return
end
 
real function DECTIM_RM(TIMIN)
! Converts time (Hr.Min Format) to decimal
  real :: INTIM, RINTIM, TIMIN
 
  INTIM = TIMIN / 100
  RINTIM = INTIM * 100.
  DECTIM = ((TIMIN - RINTIM) / 60. + INTIM) * 3600.

  return
end

real pure function dectim(t)
! Converts time (HrMinSec Format) to decimal
  real, intent(in) :: t
  real :: hour, minute

  minute = mod(t,100.0)
  second = mod(minute,60.0)
  hour = (t - minute)/100.0

  dectim = hour + (minute/60.0)
end function dectim
