program test_slope
  use simsphere_mod
  use mod_testing, only: assert, initialize_tests, report_tests
  use, intrinsic :: ieee_arithmetic
  implicit none

  ! mod_testing setup
  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests

  ! Subroutine arguments (arg1 and arg2 are the return values)
  real :: arg1, arg2, arg3, arg4, arg5

  ! Expected results
  real, parameter :: arg1_exp = 0.563626468
  real, parameter :: arg2_exp = 0.598769486

  n = 1
  ntests = 2
  call initialize_tests(tests,ntests)

  if (ieee_support_rounding(IEEE_NEAREST)) then
    call ieee_set_rounding_mode(IEEE_NEAREST)
  end if

  arg1 = 0.0
  arg2 = 0.0
  arg3 = 10.0
  arg4 = 0.3
  arg5 = 35.0

  call sslope(arg1, arg2, arg3, arg4, arg5)
  tests(n) = assert(eq(arg1,arg1_exp), 'sslope arg1')
  n = n + 1
  tests(n) = assert(eq(arg2,arg2_exp), 'sslope arg2')
  n = n + 1

  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1
  
end program test_slope
