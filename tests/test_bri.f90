program test_bri
  use simsphere_mod, only: ifirst, tstar, t, u, v, qn, otemp, atemp, tdif_s, & 
                           ug, vg, tdif_50, zo, awind, ustar, heat, bulk,    &
                           uten, mol, advgt, xmod, evap, qd, u_fine, v_fine, &
                           t_fine, q_fine, qd, qn, cf, aptemp, eq
  use mod_testing, only: assert, initialize_tests, report_tests
  implicit none


  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests

  ! arg1 = T1, arg2 = MONCE, arg3 = PSIHNEW, arg4 = YCOUNT, arg5 = ZTEN
  real :: arg1, arg3, arg4, arg5
  integer :: arg2

  ! Expected values
  real, parameter :: awind_exp = 1.0
  real, parameter :: awind_monce1_exp = 65583752.0
  real, dimension(50), parameter :: qd_exp = 1.0
  real, dimension(51), parameter :: u_fine_monce1_exp = (/-728752.312,-728819.0,&
       -728819.0,-728819.0,-728819.0,-728819.0,-728819.0,-728819.0,-728819.0,&
       -728819.0,-728819.0,-728819.0,-728819.0,-728819.0,-728819.0,-728819.0,&
       -728819.0,-728819.0,-728819.0,-728819.0,-728819.0,-728819.0,-728819.0,&
       -728819.0,-728819.0,-728819.0,-728819.0,-728819.0,-728819.0,-728819.0,&
       -728819.0,-728819.0,-728819.0,-728819.0,-728819.0,-728819.0,-728819.0,&
       -728819.0,-728819.0,-728819.0,-728819.0,-728819.0,-728819.0,-728819.0,&
       -728819.0,-728819.0,0.0,0.0,0.0,0.0,0.0/)
  real, dimension(51), parameter :: v_fine_monce1_exp = (/65579704.0,        &
       65585704.0,65585704.0,65585704.0,65585704.0,65585704.0,65585704.0,    &
       65585704.0,65585704.0,65585704.0,65585704.0,65585704.0,65585704.0,    &
       65585704.0,65585704.0,65585704.0,65585704.0,65585704.0,65585704.0,    &
       65585704.0,65585704.0,65585704.0,65585704.0,65585704.0,65585704.0,    &
       65585704.0,65585704.0,65585704.0,65585704.0,65585704.0,65585704.0,    &
       65585704.0,65585704.0,65585704.0,65585704.0,65585704.0,65585704.0,    &
       65585704.0,65585704.0,65585704.0,65585704.0,65585704.0,65585704.0,    &
       65585704.0,65585704.0,65585704.0,0.0,0.0,0.0,0.0,0.0/)
  real, dimension(51), parameter :: t_fine_monce1_exp = (/179.05423,         &
       182.562531,188.294983,189.484863,190.491669,191.491913,192.491913,    &
       193.491913,194.491913,195.491913,196.491913,197.491913,198.491913,    &
       199.491913,200.491913,201.491913,202.491913,203.491913,204.491913,    &
       205.491913,206.491913,207.491913,208.491913,209.491913,210.491913,    &
       211.491913,212.491913,213.491913,214.491913,215.491913,216.491913,    &
       217.491913,218.491913,219.491913,220.491913,221.491913,222.491913,    &
       223.491913,224.491913,225.491913,226.491913,227.491913,228.491913,    &
       229.491547,230.465256,230.964203,0.0,0.0,0.0,0.0,0.0/)
  real, dimension(51), parameter :: q_fine_monce1_exp = (/1.00000119,       &
                                    1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,&
                                    1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,&
                                    1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,&
                                    1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,&
                                    1.0,1.0,1.0,1.0,1.0,0.0,0.0,0.0,0.0,0.0/)
  real, dimension(51), parameter :: u_fine_exp = 0.0
  real, dimension(51), parameter :: v_fine_exp = 0.0
  real, dimension(51), parameter :: q_fine_exp = 0.0
  real, dimension(51), parameter :: t_fine_exp = 0.0


  n = 1
  ntests = 12
  call initialize_tests(tests,ntests)

  call bri_init
  call mom_init
  call bri(arg1,arg2,arg3,arg4,arg5)
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

  call bri_init
  call mom_init
  arg4 = 1.0
  call bri(arg1,arg2,arg3,arg4,arg5)
  tests(n) = assert(eq(qd,qd_exp,1e-5), 'qd monce == 1.0')
  n = n + 1
  tests(n) = assert(eq(awind,awind_monce1_exp), 'awind monce == 1.0')
  n = n + 1
  tests(n) = assert(eq(u_fine,u_fine_monce1_exp), 'u_fine monce == 1.0')
  n = n + 1
  tests(n) = assert(eq(v_fine,v_fine_monce1_exp), 'v_fine monce == 1.0')
  n = n + 1
  tests(n) = assert(eq(t_fine,t_fine_monce1_exp), 't_fine monce == 1.0')
  n = n + 1
  tests(n) = assert(eq(q_fine,q_fine_monce1_exp), 'q_fine monce == 1.0')
  n = n + 1
  
  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1

contains
  subroutine bri_init
    integer :: i

    arg1 = 1.0
    arg2 = 0
    arg3 = 10.0
    arg4 = 0.0
    arg5 = 1.0
    USTAR = 1.0
    bulk = 1.0
    ifirst = 0
    mol = 0.0
    tstar = 1.0
    uten = 1.0
    xmod = 1.0
    u = 1.0
    v = 2.0
    do i = 1,size(t)
      t(i) = real(i)
    end do
    awind = 1.0
    heat = 295.0
    vg = 1.0
    ug = 1.0
    otemp = 295.0
    atemp = 295.0
    aptemp = 295.0
    advgt = 1.0
    tdif_s = 1.0
    tdif_50 = 1.5
    zo = 1.0

    return
  end subroutine bri_init

  subroutine mom_init
    evap = 1.0
    cf = 1.0
    u_fine = 0.0
    v_fine = 0.0
    t_fine = 0.0
    q_fine = 0.0
    qd = 1.0 
    qn = 1.0 

    return
  end subroutine mom_init

end program test_bri
