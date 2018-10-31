program test_momday
  use simsphere_mod, only: ud, vd, ugd, vgd, ustar, awind, cf, dqdt2, zi,   &
                           hgt, ntrp, km, qd, evap, td, aptemp, q_fine,     &
                           u_fine, t_fine, v_fine, eq
  use mod_testing, only: assert, initialize_tests, report_tests
  implicit none


  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests

  ! Check value of qd(1), awind, u_fine, v_fine, t_fine, q_fine
  ! Expected values

  real, parameter :: awind_exp = 1.41421354
  real, dimension(51), parameter :: v_fine_exp = (/1.0,-16.7999992,          &
      -34.5999985,-52.4000015,-70.1999969,-88.0,-105.800003,-123.599998,     &
      -141.399994,-159.199997,-177.0,-194.800003,-212.600006,-230.399994,    &
      -248.199997,-266.0,-283.799988,-301.600006,-319.399994,-337.200012,    &
      -355.0,-372.799988,-390.600006,-408.399994,-426.200012,-444.0,         &
      -461.799988,-479.600006,-497.399994,-515.200012,-533.0,-550.799988,    &
      -568.599976,-586.400024,-604.200012,-622.0,-639.799988,-657.599976,    &
      -675.400024,-693.200012,-711.0,-728.799988,-746.599976,-764.400024,    &
      -782.200012,-800.0,0.0,0.0,0.0,0.0,0.0/)
  real, dimension(51), parameter :: t_fine_exp = (/1.0,267.0,267.0,267.0,    &
      267.0,267.0,267.0,267.0,267.0,267.0,267.0,267.0,267.0,267.0,267.0,     &
      267.0,267.0,267.0,267.0,267.0,267.0,5.19999981,5.40000010,5.59999990,  &
      5.80000019,6.0,6.19999981,6.40000010,6.59999990,6.80000019,7.0,        &
      7.19999981,7.40000010,7.59999990,7.80000019,8.0,8.19999981,8.39999962, &
      8.60000038,8.80000019,9.0,9.19999981,9.39999962,9.60000038,9.80000019, &
      10.00,0.0,0.0,0.0,0.0,0.0/)
  real, dimension(51), parameter :: q_fine_exp = (/1.0,1.20000005,1.39999998,&
      1.60000002,1.79999995,2.0,2.20000005,2.40000010,2.59999990,2.79999995, &
      3.0,3.20000005,3.40000010,3.59999990,3.79999995,4.0,4.19999981,        &
      4.40000010,4.59999990,4.80000019,5.0,5.19999981,5.40000010,5.59999990, &
      5.80000019,6.0,6.19999981,6.40000010,6.59999990,6.80000019,7.0,        &
      7.19999981,7.40000010,7.59999990,7.80000019,8.0,8.19999981,8.39999962, &
      8.60000038,8.80000019,9.0,9.19999981,9.39999962,9.60000038,9.80000019, &
      10.0,0.0,0.0,0.0,0.0,0.0/)


  n = 1
  ntests = 5
  call initialize_tests(tests,ntests)

  call momday_init
  call momday
  tests(n) = assert(eq(awind,awind_exp), 'awind')
  n = n + 1
  tests(n) = assert(eq(v_fine,v_fine_exp), 'v_fine')
  n = n + 1
  tests(n) = assert(eq(t_fine,t_fine_exp), 't_fine')
  n = n + 1
  tests(n) = assert(eq(q_fine,q_fine_exp), 'q_fine')
  n = n + 1

  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1

contains
  subroutine momday_init
    integer :: i

    awind = 1.0
    ugd = 1.0
    vgd = 1.0
    ustar = 1.0
    evap = 1.0
    cf = 1.0
    hgt = 1000.0
    aptemp = 267.0
    km = 1.0
    ntrp = 11
    dqdt2 = 1.0
    zi = 1.0
    do i = 1,50
      u_fine = 0.0
      v_fine = 0.0
      t_fine = 0.0
      q_fine = 0.0
      vd(i) = i
      ud(i) = i
      td(i) = i
      qd(i) = i
    end do
    return
  end subroutine momday_init


end program test_momday
