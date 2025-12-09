program test_flux
  use simsphere_mod, only: oshum, qd, sumw, f, rnet, otemp, aptemp, tg, qstf, & 
                           gbl_sum, tdif_s, frveg, ahum, evap, aepsi, qa, rst,&
                           t_fine, swave, heat, z, epsi, taf, chf, chg, qaf,  &
                           hg, xlef, cha, ta, tf, rst, eq
  use mod_testing, only: assert, initialize_tests, report_tests
  implicit none

  ! mod_testing setup
  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests

  ! Expected results
  real, parameter :: arg1_exp = 294.5
  real, parameter :: arg2_exp = 0.0
! real, parameter :: arg3_exp = 0.001
  real, parameter :: arg3_exp = 2.50000012E-04
  real, parameter :: arg4_exp = 0.0
! real, parameter :: arg3_qd_exp = 0.001
  real, parameter :: arg3_qd_exp = 7.36439697E+31
  real, parameter :: ahum_qd_exp = 3.0
  real, parameter :: arg1_night_exp = -8961.73926
  real, parameter :: arg2_night_exp = -8961.73926
! real, parameter :: arg3_night_exp = 0.001
  real, parameter :: arg3_night_exp = 7.36439746E+31
  real, parameter :: arg4_night_exp = 2.49819231
  real, parameter :: ahum_exp = 1.0


  ! arg1 is time, arg2 is BareEvapFlux
  real :: arg1, arg2, arg3, arg4
  integer :: arg5

  arg1 = 267.0
  arg2 = 0.0
  arg3 = 0.0
  arg4 = 0.0




  n = 1
  ntests = 11
  call initialize_tests(tests,ntests)

  ! Case I
  call flux_init
  arg5 = 1
  call flux(arg1,arg2,arg3,arg4,arg5)
  tests(n) = assert(eq(arg1,arg1_exp), 'Flux BareRadioTemp')
  n = n + 1
  tests(n) = assert(eq(arg2,arg2_exp), 'Flux VegnRadioTemp')
  n = n + 1
  tests(n) = assert(eq(arg3,arg3_exp), 'Flux BareEvapFlux')
  n = n + 1
  tests(n) = assert(eq(arg4,arg4_exp), 'Flux BareHeatFlux')
  n = n + 1
  tests(n) = assert(eq(ahum,ahum_exp), 'Flux ahum')
  n = n + 1

  ! Case II, qd(1) >= oshum
  call flux_init
  arg5 = 1
  qd(1) = 3.0
  oshum = 2.0
  call flux(arg1,arg2,arg3,arg4,arg5)
  tests(n) = assert(eq(arg3,arg3_qd_exp), 'Flux BareEvapFlux')
  n = n + 1
  tests(n) = assert(eq(ahum,ahum_qd_exp), 'Flux ahum night')
  n = n + 1

  ! Case III, rnet < 0
  call flux_init
  arg5 = 1
  qd(1) = 1.0
  rnet = -1.0
  call flux(arg1,arg2,arg3,arg4,arg5)
  tests(n) = assert(eq(arg1,arg1_night_exp), 'Flux BareRadioTemp night')
  write(*,*) arg1
  n = n + 1
  tests(n) = assert(eq(arg2,arg2_night_exp), 'Flux VegnRadioTemp night')
  write(*,*) arg2
  n = n + 1
  tests(n) = assert(eq(arg3,arg3_night_exp), 'Flux BareEvapFlux night')
  n = n + 1
  tests(n) = assert(eq(arg4,arg4_night_exp), 'Flux BareHeatFlux night')
  n = n + 1
  tests(n) = assert(eq(ahum,ahum_exp), 'Flux ahum night')
  n = n + 1

  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1

contains
  subroutine flux_init
    oshum = 2.0
    qd(1) = 1.0
    sumw = 1.0
    f = 2.9
    rnet = 0.3
    otemp = 295.0
    aptemp = 295.0
    gbl_sum = 0.0
    tdif_s = 0.5
    frveg = 0.5
    ahum = 0.0
    evap = 0.3
  
    ! gtemp initialization
    AEPSI = 1.17
    t_fine = 1.0
    swave = 1.0
    heat = 2.49819231
    z = 1.0
    epsi = 0.96
  
    ! vegflx initialization
    TAF = 10.0
    CHF = 10.0
    CHG = 0.1
    QAF = 0.5
    HG = 1.0
    CHG = 100.5
    XLEF = 1.2
    CHA = 100.0
    TA = 0.4
    CHF = 10.0
    TF = 1.0
    TG = 0.0
    RST = 1.0
    QA = 100.0
    QSTF = 10.0

    return
  end subroutine flux_init
  
end program test_flux
