program test_mom
  use simsphere_mod, only: u_fine, v_fine, t_fine, q_fine, qd, qn, cf, ug,  &
                           otemp, t, u, v, heat, ustar, advgt, vg, awind,   &
                           evap, eq
  use mod_testing, only: assert, initialize_tests, report_tests
  implicit none


  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests

  real :: arg1
  integer :: arg2, arg3

  ! Check value of qd(1), awind, u_fine, v_fine, t_fine, q_fine
  ! arg3 is set to 2 when supplied as 0
  ! arg2 is unchanged.
  ! arg1 is unchanged.
  ! Expected values
  integer, parameter :: arg3_exp = 2

  real, parameter :: awind_exp = 9799.88965
  real, dimension(50), parameter :: qd_exp = (/1.00000024,                  &
                                    1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,&
                                    1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,&
                                    1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,&
                                    1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,&
                                    1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0/)
  real, dimension(51), parameter :: u_fine_exp = (/-980.896179,-979.000183, &
     -979.0,-979.0,-979.0,-979.0,-979.0,-979.0,-979.0,-979.0,-979.0,-979.0, &
     -979.0,-979.0,-979.0,-979.0,-979.0,-979.0,-979.0,-979.0,-979.0,-979.0, &
     -979.0,-979.0,-979.0,-979.0,-979.0,-979.0,-979.0,-979.0,-979.0,-979.0, &
     -979.0,-979.0,-979.0,-979.0,-979.0,-979.0,-979.0,-979.0,-979.0,-979.0, &
     -979.0,-979.0,-979.0,-979.0,-979.0,-979.0,-979.0,-979.0,0.0/)
  real, dimension(51), parameter :: v_fine_exp = (/9750.67578,9702.00195,   &
     9702.0,9702.0,9702.0,9702.0,9702.0,9702.0,9702.0,9702.0,9702.0,9702.0, &
     9702.0,9702.0,9702.0,9702.0,9702.0,9702.0,9702.0,9702.0,9702.0,9702.0, &
     9702.0,9702.0,9702.0,9702.0,9702.0,9702.0,9702.0,9702.0,9702.0,9702.0, &
     9702.0,9702.0,9702.0,9702.0,9702.0,9702.0,9702.0,9702.0,9702.0,9702.0, &
     9702.0,9702.0,9702.0,9702.0,9702.0,9702.0,9702.0,9702.0,0.0/)
  real, dimension(51), parameter :: t_fine_exp = (/21.5785923,22.4999332,   &
     23.5595894,24.5598583,25.5598602,26.5598602,27.5598602,28.5598602,     &
     29.5598602,30.5598602,31.5598602,32.5598602,33.5598602,34.5598602,     &
     35.5598602,36.5598602,37.5598602,38.5598602,39.5598602,40.5598602,     &
     41.5598602,42.5598602,43.5598602,44.5598602,45.5598602,46.5598602,     &
     47.5598602,48.5598602,49.5598602,50.5598602,51.5598602,52.5598602,     &
     53.5598602,54.5598602,55.5598602,56.5598602,57.5598602,58.5598602,     &
     59.5598602,60.5598602,61.5598602,62.5598602,63.5598602,64.5598602,     &
     65.5598602,66.5598602,67.5598602,68.5598602,69.5568695,70.0568466,0.0/)
  real, dimension(51), parameter :: q_fine_exp = (/1.00000024,              &
                                    1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,&
                                    1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,&
                                    1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,&
                                    1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,&
                                    1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,0.0/)


  n = 1
  ntests = 7
  call initialize_tests(tests,ntests)

  call mom_init
  arg3 = 0
  call mom(arg1,arg2,arg3)
  tests(n) = assert(arg3 == arg3_exp, 'MONCE 0->2')
  n = n + 1
  tests(n) = assert(eq(qd,qd_exp), 'qd')
  n = n + 1
  tests(n) = assert(eq(awind,awind_exp), 'awind')
  n = n + 1
  tests(n) = assert(eq(u_fine,u_fine_exp), 'u_fine')
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
  subroutine mom_init
    integer :: i

    arg1 = 1.0
    arg2 = 10.0
    arg3 = 2
    USTAR = 1.0
    evap = 1.0
    advgt = 1.0
    cf = 1.0
    awind = 1.0
    heat = 295.0
    vg = 1.0
    ug = 1.0
    u = 1.0
    v = 2.0
    do i = 1,size(t)
      t(i) = real(i)
    end do
    otemp = 295.0
    u_fine = 0.0
    v_fine = 0.0
    t_fine = 0.0
    q_fine = 0.0
    qd = 1.0 
    qn = 1.0 
    return
  end subroutine mom_init


end program test_mom
