program test_ball
  use simsphere_mod, only: rst, xlai, raf, fco2, frveg, rzascr, tf, ci, co,  &
                           rcut, rs, qstf, qa, ccan, eq
  use mod_testing, only: assert, initialize_tests, report_tests
  implicit none


  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests

  real :: arg1
  integer :: arg2

  ! Expected values
  real, parameter :: fco2_exp = -1.21837866E-06
  real, parameter :: ci_exp = 1.00000024
  real, parameter :: ccan_exp = 1.00000012

  n = 1
  ntests = 3
  call initialize_tests(tests,ntests)

  call ball_init
  call ball(arg1,arg2)
  tests(n) = assert(eq(fco2,fco2_exp), 'fco2')
  n = n + 1
  tests(n) = assert(eq(ci,ci_exp), 'ci')
  n = n + 1
  tests(n) = assert(eq(ccan,ccan_exp), 'ccan')
  n = n + 1

  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1

contains
  subroutine ball_init
    arg1 = 1.0
    arg2 = 10.0
    rst = 1.0
    xlai = 1.0
    raf = 1.0
    fco2 = 0.5
    frveg = 0.5
    rzascr = 1.0
    tf = 295.0
    ci = 1.0
    co = 1.0
    rcut = 1.0
    rs = 1.0
    qstf = 1.0
    qa = 1.0
    ccan = 1.0
    return
  end subroutine ball_init


end program test_ball
