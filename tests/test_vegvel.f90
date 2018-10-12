program test_vegvel
  use simsphere_mod, only: xlai, ps1, r, taf, uaf, chf, width, raf, ta,    &
                           rtranw, cha, ustar, uten, qstf, tf, vfl, rs,    &
                           rmin, sol, wilt, w2g, wgg, stmtype, rst, rcut,  &
                           xlef, eq
  use mod_testing, only: assert, initialize_tests, report_tests
  implicit none


  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests

  ! Expected values
  real, parameter :: rst_exp = 0.0
  real, parameter :: tf_exp = 0.751202226
  real, parameter :: xlef_exp = 0.275000006

  n = 1
  ntests = 9
  call initialize_tests(tests,ntests)

  ! stmtype == 'd'
  call vegvel_init
  stmtype = 'd'
  call vegvel
  tests(n) = assert(eq(rst,rst_exp), 'rst')
  n = n + 1
  tests(n) = assert(eq(tf,tf_exp), 'tf')
  n = n + 1
  tests(n) = assert(eq(xlef,xlef_exp), 'xlef')
  n = n + 1

  ! stmtype == 'l'
  call vegvel_init
  stmtype = 'l'
  call vegvel
  tests(n) = assert(eq(rst,rst_exp), 'rst')
  n = n + 1
  tests(n) = assert(eq(tf,tf_exp), 'tf')
  n = n + 1
  tests(n) = assert(eq(xlef,xlef_exp), 'xlef')
  n = n + 1

  ! stmtype == 'b'
  call vegvel_init
  stmtype = 'b'
  call vegvel
  tests(n) = assert(eq(rst,rst_exp), 'rst')
  n = n + 1
  tests(n) = assert(eq(tf,tf_exp), 'tf')
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
    uaf = 1.0
    chf = 1.0
    width = 1.0
    raf = 1.0
    return
  end subroutine vegvel_init

end program test_vegvel
