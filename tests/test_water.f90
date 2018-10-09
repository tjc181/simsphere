program test_water
  use simsphere_mod, only: wgg, w2g, rhow, evap, le, frveg, rnet, xleg, xlef, &
                           delta, f, fsub, wmax, eq
  use mod_testing, only: assert, initialize_tests, report_tests
  implicit none

  ! mod_testing setup
  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests

  ! Check values of f, fsub, wgg, w2g
  ! Expected results
  real, parameter :: f_exp = 0.500064552
  real, parameter :: fsub_exp = 0.749999881
  real, parameter :: wgg_exp = 0.170021951
  real, parameter :: w2g_exp = 0.254999965
  real, parameter :: f_time_exp = 0.499739051
  real, parameter :: fsub_time_exp = 0.749999881
  real, parameter :: wgg_time_exp = 0.169911280
  real, parameter :: w2g_time_exp = 0.254999965
  real, parameter :: f_rnet_exp = 0.499739259
  real, parameter :: fsub_rnet_exp = 0.75
  real, parameter :: wgg_rnet_exp = 0.169911355
  real, parameter :: w2g_rnet_exp = 0.254999995


  ! arg1 is time, arg2 is BareEvapFlux
  real :: arg1, arg2

  n = 1
  ntests = 12
  call initialize_tests(tests,ntests)


  ! Case I
  call water_init
  arg1 = 0.0
  arg2 = 0.5
  call water(arg1,arg2)
  tests(n) = assert(eq(f,f_exp), 'water f')
  n = n + 1
  tests(n) = assert(eq(fsub,fsub_exp), 'water fsub')
  n = n + 1
  tests(n) = assert(eq(wgg,wgg_exp), 'water wgg')
  n = n + 1
  tests(n) = assert(eq(w2g,w2g_exp), 'water w2g')
  n = n + 1
  write(*,*) f, fsub, wgg, w2g

  ! Case II (time /= 0 and rnetf < 0)
  call water_init
  arg1 = 1.0 ! time
  arg2 = 0.5
  call water(arg1,arg2)
  tests(n) = assert(eq(f,f_time_exp), 'water time > 0 f')
  n = n + 1
  tests(n) = assert(eq(fsub,fsub_time_exp), 'water time > 0 fsub')
  n = n + 1
  tests(n) = assert(eq(wgg,wgg_time_exp), 'water time > 0 wgg')
  n = n + 1
  tests(n) = assert(eq(w2g,w2g_time_exp), 'water time > 0 w2g')
  n = n + 1
  write(*,*) f, fsub, wgg, w2g

  ! Case III (time /= 0 and rnetf < 0)
  call water_init
  arg1 = 1.0 ! time
  arg2 = 0.5
  rnet = 0.0
  call water(arg1,arg2)
  tests(n) = assert(eq(f,f_rnet_exp), 'water rnet == 0 f')
  n = n + 1
  tests(n) = assert(eq(fsub,fsub_rnet_exp), 'water rnet == 0 fsub')
  n = n + 1
  tests(n) = assert(eq(wgg,wgg_rnet_exp), 'water rnet == 0 wgg')
  n = n + 1
  tests(n) = assert(eq(w2g,w2g_rnet_exp), 'water rnet == 0 w2g')
  n = n + 1
  write(*,*) f, fsub, wgg, w2g

  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1
  
contains
  subroutine water_init
    wgg = 0.17
    w2g = 0.255
    evap = 0.3
    frveg = 0.5
    rnet = 0.5
    xleg = 0.5
    xlef = 0.5
    f = 0.5
    fsub = 0.75
    wmax = 0.34
    return
  end subroutine water_init
end program test_water
