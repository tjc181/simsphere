program test_calc
  use simsphere_mod, only: xlat, xlong, timend, strtim, outtt, atemp, otemp, &
                           tscren, t, frveg, cf, eq
  use mod_testing, only: assert, initialize_tests, report_tests
  use, intrinsic :: ieee_arithmetic
  implicit none

  ! mod_testing setup
  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests

  ! Subroutine arguments (OLDTMP, No_Rows)
  real :: arg1
  integer :: arg2

  ! DECTIM function interface
  interface
    real function dectim (t)
      real, intent(in) :: t
    end function dectim
  end interface

  ! Expected results
  real, parameter :: otemp_exp = 8.0
  real, parameter :: frveg_exp = 4.99999989E-03
  real, parameter :: xlat_exp = 39.25
  real, parameter :: xlong_exp = 53.0
  real, parameter :: cf_exp = 9.19953891E-05
  real, parameter :: timend_exp = 23.5*3600
  real, parameter :: strtim_exp = 5.5*3600
  real, parameter :: arg1_exp = 20.0
  real, parameter :: t_1_exp = 20.5
  real, parameter :: dectim_exp = 22.75*3600 ! 22:45
  integer, parameter :: arg2_exp = 37

  n = 1
  ntests = 11
  call initialize_tests(tests,ntests)

  if (ieee_support_rounding(IEEE_NEAREST)) then
    call ieee_set_rounding_mode(IEEE_NEAREST)
  end if

  arg1 = 200.0
  arg2 = 10

  xlat = 39.25
  xlong = 53.0
  cf = 0.0
  outtt = 30.0
  strtim = 0530
  timend = 2330
  atemp = 20.0
  tscren = 10
  frveg = 0.5
  t(1) = 0.0
  
  call CALC(arg1, arg2)
  tests(n) = assert(eq(arg1,arg1_exp), 'CALC arg1')
  n = n + 1
  tests(n) = assert(arg2 == arg2_exp, 'CALC arg2')
  n = n + 1
  tests(n) = assert(eq(otemp,otemp_exp), 'CALC otemp')
  n = n + 1
  tests(n) = assert(eq(frveg,frveg_exp), 'CALC frveg')
  n = n + 1
  tests(n) = assert(eq(xlat,xlat_exp), 'CALC xlat')
  n = n + 1
  tests(n) = assert(eq(xlong,xlong_exp), 'CALC xlong')
  n = n + 1
  tests(n) = assert(eq(cf,cf_exp), 'CALC cf')
  n = n + 1
  tests(n) = assert(eq(strtim,strtim_exp), 'CALC strtim')
  n = n + 1
  tests(n) = assert(eq(timend,timend_exp), 'CALC timend')
  n = n + 1
  tests(n) = assert(eq(t(1),t_1_exp), 'CALC t(1)')
  n = n + 1
  tests(n) = assert(eq(dectim(2245.0),dectim_exp), 'CALC dectim()')
  n = n + 1

  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1
  
end program test_calc
