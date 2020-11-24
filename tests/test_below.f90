program test_below
  use simsphere_mod, only: nlvls, frveg, otemp, tt, tg, heat, rnet, kappa, &
                           xfun, del, dzeta, ptime, wmax, w2g, wgg, eq
  use mod_testing, only: assert, initialize_tests, report_tests
  implicit none


  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests

  real :: arg1, arg2, arg3 !time, BareRadioTemp, BareEvapFlux
  real :: waterwin
  real :: belowte(9)

  ! Expected values
  real, parameter :: w2g_wmax_exp = 99.99
  real, parameter :: wgg_wmax_exp = 99.99
  real, parameter, dimension(9) :: tt_exp = (/222.837555,1.0,1.0,1.0,1.0,  &
                                              1.0,1.0,1.0,1.0/)
  real, parameter, dimension(9) :: tt_time0_exp = 222.837555
  real, parameter :: tt_frveg0_exp = 265.0
  real, parameter :: tt_frveg_exp = 222.837555
  real, parameter :: tt_frveg1_exp = 1.0
  real, parameter :: tt_low_heat_exp = 265.0

  n = 1
  ntests = 7
  call initialize_tests(tests,ntests)

  ! wmax > 1
  ! 0 < frveg < 1
  call below_init
  arg1 = 1.5
  wmax = 2.0
  call below(arg1,arg2,arg3,waterwin,belowte)
  tests(n) = assert(eq(w2g,w2g_wmax_exp), 'w2g high wmax')
  n = n + 1
  tests(n) = assert(eq(wgg,wgg_wmax_exp), 'wgg high wmax')
  n = n + 1
  tests(n) = assert(eq(tt,tt_exp), 'tt')
  n = n + 1
  tests(n) = assert(eq(tt(1),tt_frveg_exp), 'tt(1) 0 < frveg < 1')
  n = n + 1
  
  ! time == 0.0
  call below_init
  arg1 = 0.0
  call below(arg1,arg2,arg3,waterwin,belowte)
  tests(n) = assert(eq(tt,tt_exp), 'tt time == 0')
  n = n + 1

  ! Frveg  == 0.0
  call below_init
  frveg = 0.0
  call below(arg1,arg2,arg3,waterwin,belowte)
  tests(n) = assert(eq(tt(1),tt_frveg0_exp), 'tt(1) frveg == 0')
  n = n + 1

  ! frveg == 1
  call below_init
  frveg = 1.0
  call below(arg1,arg2,arg3,waterwin,belowte)
  tests(n) = assert(eq(tt(1),tt_frveg1_exp), 'tt(1) frveg == 1')
  n = n + 1
  
  ! frveg > 1
  ! Commented code in subroutine stops execution in this case.  Frveg
  ! may not exceed 1.  This should be checked and enforced when frveg is
  ! read into the program.
  
  ! heat < 0
  call below_init
  heat = -1.0
  call below(arg1,arg2,arg3,waterwin,belowte)
  tests(n) = assert(eq(tt(1),tt_low_heat_exp), 'tt(1) heat < 0')
  n = n + 1


  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1

contains
  subroutine below_init
    nlvls = 1
    frveg = 0.5 
    otemp = 265.0
    tt = 1.0
    tg = 1.0
    heat = 1800.0
    kappa = 1.0
    xfun = 1.0
    del = 10.0
    dzeta = 5.0
    wmax = 1.0
    wgg = 0.75
    w2g = 0.25  
    ptime = 16
    arg2 = 265.0
    arg3 = 1800.0
    waterwin = 0.0
    belowte = 0.0
    return
  end subroutine below_init

end program test_below
