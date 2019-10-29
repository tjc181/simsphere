program test_vegvel
  use simsphere_mod, only: xlai, ps1, taf, uaf, chf, width, raf, ta, rtranw, &
                           ustar, tf, stmtype, rst, xlef, vegheight, chg, eq
  use mod_testing, only: assert, initialize_tests, report_tests
  implicit none


  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests

  integer :: init_vel, PSLCALINIT

  ! Expected values
  real, parameter :: rst_exp = 0.0
  real, parameter :: tf_d_exp = -17448048.0
  real, parameter :: tf_l_exp = -17448048.0
  real, parameter :: tf_b_exp = -17448048.0
  real, parameter :: xlef_exp = 349979200.0

  n = 1
  ntests = 9
  call initialize_tests(tests,ntests)

  ! stmtype == 'd'
  call vegvel_init
  init_vel = 1
  PSLCALINIT = 1
  stmtype = 'd'
  call vegvel(init_vel,PSLCALINIT)
  tests(n) = assert(eq(rst,rst_exp), 'rst')
  n = n + 1
  tests(n) = assert(eq(tf,tf_d_exp), 'tf')
  n = n + 1
  tests(n) = assert(eq(xlef,xlef_exp), 'xlef')
  n = n + 1

  ! stmtype == 'l'
  call vegvel_init
  init_vel = 1
  PSLCALINIT = 1
  stmtype = 'l'
  call vegvel(init_vel,PSLCALINIT)
  tests(n) = assert(eq(rst,rst_exp), 'rst')
  n = n + 1
  tests(n) = assert(eq(tf,tf_l_exp), 'tf')
  n = n + 1
  tests(n) = assert(eq(xlef,xlef_exp), 'xlef')
  n = n + 1

  ! stmtype == 'b'
  call vegvel_init
  init_vel = 1
  PSLCALINIT = 1
  stmtype = 'b'
  call vegvel(init_vel,PSLCALINIT)
  tests(n) = assert(eq(rst,rst_exp), 'rst')
  n = n + 1
  tests(n) = assert(eq(tf,tf_b_exp), 'tf')
  n = n + 1
  tests(n) = assert(eq(xlef,xlef_exp), 'xlef')
  n = n + 1

  ! chf < .001
  ! cha < .001
  ! rmratiodif < .001

  ! init_vel == 1

  ! init_vel /= 1
  ! This case is never executed unless init_vel is saved between calls

  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1

contains
  subroutine vegvel_init
    xlai = 1.0
    ps1 = 1.0
    taf = 1.0
    uaf = 100.0
    chf = 1.0
    width = 1.0
    raf = 1.0
    vegheight = 2.0
    chg = 1.0
    rtranw = 0.5
    ustar = 1.0
    tf = 1000.0
    ta = 1000.0
    return
  end subroutine vegvel_init

end program test_vegvel
