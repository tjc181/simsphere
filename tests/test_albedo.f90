program test_albedo
  use simsphere_mod
  use mod_testing, only: assert, initialize_tests, report_tests
  use, intrinsic :: ieee_arithmetic
  implicit none

  ! mod_testing setup
  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests

  ! Expected results
  real, parameter :: caseI_exp = 0.396643847
  real, parameter :: caseII_exp = 0.396643847
  real, parameter :: caseIII_exp = 0.396643847
  real, parameter :: caseIV_exp = 0.396643847

  real :: SOLSIN

  n = 1
  ntests = 4
  call initialize_tests(tests,ntests)

  if (ieee_support_rounding(IEEE_NEAREST)) then
    call ieee_set_rounding_mode(IEEE_NEAREST)
  end if

  SOLSIN = -0.510662317
  XLAI = 1.0
  FRVEG = 1.0
  WGG = 100.0
  WMAX = 1230.0

  ! If ALBFLG = 0 (initialized this way)
    ! ALBG = 0 -> ALGFLG = true
    ! ALBF = 0 ->  ALFFLG = true
    ! ALBFLG = 1

  ! Case I: ALBG == 0 and ALBF == 0

  ALBG = 0
  ALBF = 0
  call ALBEDO(SOLSIN)
  tests(n) = assert(eq(ALBDOE,caseI_exp), 'albedo case I')
  n = n + 1

  ! Case II: ALBG /= 0 and ALBF == 0

  ALBG = 20.0
  ALBF = 0
  call ALBEDO(SOLSIN)
  tests(n) = assert(eq(ALBDOE,caseII_exp), 'albedo case II')
  n = n + 1

  ! Case III: ALBG == 0 and ALBF /= 0

  ALBG = 0
  ALBF = 100.0
  call ALBEDO(SOLSIN)
  tests(n) = assert(eq(ALBDOE,caseIII_exp), 'albedo case III')
  n = n + 1

  ! Case IV: ALBG /= 0 and ALBF /=0

  ALBG = 46
  ALBF = 71
  call ALBEDO(SOLSIN)
  tests(n) = assert(eq(ALBDOE,caseIV_exp), 'albedo case IV')
  n = n + 1

  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1
  
end program test_albedo
