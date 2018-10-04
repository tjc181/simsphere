program test_canres
  use simsphere_mod, only: eq, F, SUMW, XLEF, CHA, OSHUM, QD, OTEMP, DENS, LE, &
                           FRVEG, embar, resist
  use mod_testing, only: assert, initialize_tests, report_tests
  use, intrinsic :: ieee_arithmetic
  implicit none

  ! mod_testing setup
  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests

  ! Expected results
  real, parameter :: resist_exp = -1372130.0
  real, parameter :: embar_exp = 0.499999821

  n = 1
  ntests = 2
  call initialize_tests(tests,ntests)

  if (ieee_support_rounding(IEEE_NEAREST)) then
    call ieee_set_rounding_mode(IEEE_NEAREST)
  end if

  F = 1.0
  SUMW = 1.0
  XLEF = 1.0
  CHA = 1.0
  OSHUM = 2.0
  QD(1) = 1.0
  OTEMP = 265.0
  FRVEG = 0.5

  call CANRES
  tests(n) = assert(eq(RESIST,resist_exp), 'CANRES resist')
  n = n + 1

  call CANRES
  tests(n) = assert(eq(EMBAR,embar_exp), 'CANRES embar')
  n = n + 1

  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1
  
end program test_canres
