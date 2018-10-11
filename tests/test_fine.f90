program test_fine
  use simsphere_mod, only: u_fine, v_fine, t_fine, q_fine, ud, vd, td, qd,  &
                           hgt, aptemp, eq
  use mod_testing, only: assert, initialize_tests, report_tests
  implicit none


  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests, i

  ! Expected values
  real, parameter, dimension(51) :: uvqt_fine_exp = (/1.0,1.20000005,1.39999998, &
        1.60000002,1.79999995,2.0,2.20000005,2.40000010,2.59999990,2.79999995,&
        3.0,3.20000005,3.40000010,3.59999990,3.79999995,4.0,4.19999981,       &
        4.40000010,4.59999990,4.80000019,5.0,5.19999981,5.40000010,5.59999990,&
        5.80000019,6.0,6.19999981,6.40000010,6.59999990,6.80000019,7.0,       &
        7.19999981,7.40000010,7.59999990,7.80000019,8.0,8.19999981,8.39999962,&
        8.60000038,8.80000019,9.0,9.19999981,9.39999962,9.60000038,9.80000019,&
        10.0,0.0,0.0,0.0,0.0,0.0/)
  real, parameter, dimension(51) :: t_fine_hgt_exp = (/1.0,267.0,267.0,267.0, &
        267.0,267.0,267.0,267.0,267.0,267.0,267.0,267.0,267.0,267.0,267.0,    &
        267.0,267.0,267.0,267.0,267.0,267.0,5.19999981,5.40000010,5.59999990, &
        5.80000019,6.0,6.19999981,6.40000010,6.59999990,6.80000019,7.0,       &
        7.19999981,7.40000010,7.59999990,7.80000019,8.0,8.19999981,8.39999962,&
        8.60000038,8.80000019,9.0,9.19999981,9.39999962,9.60000038,9.80000019,&
        10.0,0.0,0.0,0.0,0.0,0.0/)
  real, parameter :: rlinear_exp = 1.79999995

  interface
    real function rlinear (top,bot,n)
      real :: top, bot
      integer :: n
    end function rlinear
  end interface

  ! Initialize mod_testing
  n = 1
  ntests = 9
  call initialize_tests(tests,ntests)

  ! Case I, hgt < 50
  call fine_init 
  hgt = 0.0
  call fine
  tests(n) = assert(eq(u_fine,uvqt_fine_exp), 'u_fine')
  n = n + 1
  tests(n) = assert(eq(v_fine,uvqt_fine_exp), 'v_fine')
  n = n + 1
  tests(n) = assert(eq(t_fine,uvqt_fine_exp), 't_fine')
  n = n + 1
  tests(n) = assert(eq(q_fine,uvqt_fine_exp), 'q_fine')
  n = n + 1

  ! Case II, hgt > 50
  call fine_init
  hgt = 1000.0
  call fine
  tests(n) = assert(eq(u_fine,uvqt_fine_exp), 'u_fine hgt > 50')
  n = n + 1
  tests(n) = assert(eq(v_fine,uvqt_fine_exp), 'v_fine hgt > 50')
  n = n + 1
  tests(n) = assert(eq(t_fine,t_fine_hgt_exp), 't_fine hgt > 50')
  n = n + 1
  tests(n) = assert(eq(q_fine,uvqt_fine_exp), 'q_fine hgt > 50')
  n = n + 1

  ! rlinear() (top, bot, n) -> bot + (top - bot) * n /5
  tests(n) = assert(eq(rlinear(1.0,2.0,1),rlinear_exp), 'rlinear()')
  n = n + 1

  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1

contains
  subroutine fine_init
    do i = 1,50
      ud(i) = i
      vd(i) = i
      td(i) = i
      qd(i) = i
      aptemp = 267.0
    end do
    return
  end subroutine fine_init 

end program test_fine
