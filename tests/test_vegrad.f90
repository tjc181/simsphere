program test_vegrad
  use simsphere_mod, only: otemp, atemp, taf, tg, rlf, qaf, albdoe, tf, sol, &
                           rsg, sigf, albg, albf, rsf, rlg, epsi, qd, lwdn,  &
                           epsf, rnetg, rnetf, swave, eq
  use mod_testing, only: assert, initialize_tests, report_tests
  implicit none

  ! mod_testing setup
  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests

  ! Four cases: time == 0, time /= 0 (time is arg1), and rnetf <= 0 or rnetf > 0)
  ! When time /= 0 taf, tf, tg, t1, ta, and qaf are not initialized in the
  ! routine (presumably initialized during some previous run
  ! rnetf is calculated in VEGRAD based on values of sol, albf, sigf, albg, 
  ! epsi, lwdn, sigma, tg, epsf.  We can't just set this directly...
  ! Expected results
  real, parameter :: arg2_caseI_exp = -391.401337
  real, parameter :: arg2_caseII_exp = -391.401337
  real, parameter :: arg2_caseIII_exp = -48.4641571 
  real, parameter :: arg2_caseIV_exp = -48.4641571
  real, parameter :: arg3_caseI_exp = 18.1798172
  real, parameter :: arg3_caseII_exp = 18.1798172
  real, parameter :: arg3_caseIII_exp = 18.1798172
  real, parameter :: arg3_caseIV_exp = 18.1798172
  real, parameter :: arg4_caseI_exp = 292.297363
  real, parameter :: arg4_caseII_exp = 292.297363
  real, parameter :: arg4_caseIII_exp = 187.356354
  real, parameter :: arg4_caseIV_exp = 187.356354


  real :: arg1, arg2, arg3, arg4


  n = 1
  ntests = 12
  call initialize_tests(tests,ntests)

  

  ! Case I (time == 0 and rnetf < 0)
  call vegrad_init
  arg1 = 0.0 ! time
  call vegrad(arg1,arg2,arg3,arg4)
  tests(n) = assert(eq(arg2,arg2_caseI_exp), 'vegrad rnetv case I')
  n = n + 1
  tests(n) = assert(eq(arg3,arg3_caseI_exp), 'vegrad swavev case I')
  n = n + 1
  tests(n) = assert(eq(arg4,arg4_caseI_exp), 'vegrad tzero case I')
  n = n + 1

  ! Case II (time /= 0 and rnetf < 0)
  call vegrad_init
  tf = 295.0
  tg = 295.0
  arg1 = 1.0 ! time
  call vegrad(arg1,arg2,arg3,arg4)
  tests(n) = assert(eq(arg2,arg2_caseII_exp), 'vegrad rnetv case II')
  n = n + 1
  tests(n) = assert(eq(arg3,arg3_caseII_exp), 'vegrad swavev case II')
  n = n + 1
  tests(n) = assert(eq(arg4,arg4_caseII_exp), 'vegrad tzero case II')
  n = n + 1

  ! Case III (time == 0 and rnetf > 0
  call vegrad_init
  arg1 = 0.0 ! time
  epsf = 0.1
  epsi = 0.1
  tf = 295.0
  tg = 295.0
  call vegrad(arg1,arg2,arg3,arg4)
  tests(n) = assert(eq(arg2,arg2_caseIII_exp), 'vegrad rnetv case III')
  n = n + 1
  tests(n) = assert(eq(arg3,arg3_caseIII_exp), 'vegrad swavev case III')
  n = n + 1
  tests(n) = assert(eq(arg4,arg4_caseIII_exp), 'vegrad tzero case III')
  n = n + 1
 

  ! Case IV (time /=0 and rnetf > 0)
  call vegrad_init
  arg1 = 1.0 ! time
  epsf = 0.1
  epsi = 0.1
  tf = 295.0
  tg = 295.0
  call vegrad(arg1,arg2,arg3,arg4)
  tests(n) = assert(eq(arg2,arg2_caseIV_exp), 'vegrad rnetv case IV')
  n = n + 1
  tests(n) = assert(eq(arg3,arg3_caseIV_exp), 'vegrad swavev case IV')
  n = n + 1
  tests(n) = assert(eq(arg4,arg4_caseIV_exp), 'vegrad tzero case IV')
  n = n + 1


  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1
  
contains
  subroutine vegrad_init
    arg1 = 0.0
    arg2 = 0.0
    arg3 = 0.0
    arg4 = 0.0

    otemp = 295.0
    atemp = 295.0
    qd(1) = 1.0
    taf = 0.0
    qaf = 0.0
    tf = 1.0
    swave = 10.0
    albdoe = 0.5
    rsg = 1.0
    sigf = 0.9
    albg = 0.1
    albf = 0.1
    rsf = 0.0
    rlf = 0.0
    rlg = 1.0
    rnetg = 1.0
    rnetf = 0.0
  
    epsi = 0.96
    lwdn = 3.0
    epsf = 0.96
    sol = 0.9
    tg = 1.0
    return
  end subroutine vegrad_init

end program test_vegrad
