program test_netrad
  use simsphere_mod, only: aepsi, omega, cloud_flag, cld_fract, frveg,      &
                           rnet, swave, epsi, t_fine, tdif_s, iday, imo,    &
                           iyr, albdoe, albf, albg, atemp, epsf, otemp, qaf,&
                           rlf, rlg, rnetf, rnetg, rsf, rsg, sigf, sol, taf,&
                           tf, tg, qd, aspect, slope, xlai, wgg, wmax,      &
                           tscren, eq
  use mod_testing, only: assert, initialize_tests, report_tests
  implicit none


  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests

  ! 
  real :: arg1, arg2, arg3, arg4, arg5, arg6
  integer(kind=1) :: arg7

  ! Expected values
!  real, parameter :: lwdn_exp = 0.0
!  real, parameter :: lwup_exp = 0.0
  real, parameter :: arg2_exp = 295.15
  real, parameter :: arg3_exp = 295.15
  real, parameter :: arg4_exp = -411.767578
  real, parameter :: arg5_exp = 0.0
  real, parameter :: arg6_exp = 0.0

  real, parameter :: arg2_no_init_exp = 295.15
  real, parameter :: arg3_no_init_exp = 295.15
  real, parameter :: arg4_no_init_exp = -411.767578
  real, parameter :: arg5_no_init_exp = 0.0
  real, parameter :: arg6_no_init_exp = 0.0

  real, parameter :: arg2_pv_exp = 295.15
  real, parameter :: arg3_pv_exp = 292.673859
  real, parameter :: arg4_pv_exp = -411.767578
  real, parameter :: arg5_pv_exp = -414.710999
  real, parameter :: arg6_pv_exp = -413.239288

  real, parameter :: arg2_fv_exp = 295.15
  real, parameter :: arg3_fv_exp = 292.673859
  real, parameter :: arg4_fv_exp = 0.0
  real, parameter :: arg5_fv_exp = -414.710999
  real, parameter :: arg6_fv_exp = 0.0

  integer, parameter :: arg7_exp = 2


  n = 1
  ntests = 21
  call initialize_tests(tests,ntests)

  ! Time < 0 produces an error message

  ! Test NETRAD init (arg7) == 1, frveg == 0
  call netrad_init
  call vegrad_init
  call sslope_init
  call albedo_init
  arg2 = 0.0
  arg3 = 0.0
  arg7 = 1
  call netrad(arg1,arg2,arg3,arg4,arg5,arg6,arg7)
  tests(n) = assert(eq(arg2,arg2_exp), 'BareRadioTemp')
  n = n + 1
  tests(n) = assert(eq(arg3,arg3_exp), 'VegnRadioTemp')
  n = n + 1
  tests(n) = assert(eq(arg4,arg4_exp), 'BareNetRadn')
  n = n + 1
  tests(n) = assert(eq(arg5,arg5_exp), 'VegnNetRadn')
  n = n + 1
  tests(n) = assert(eq(arg6,arg6_exp), 'MixedNetRadn')
  n = n + 1
  tests(n) = assert(arg7 == arg7_exp, 'Init advanced')
  n = n + 1

  

  ! init (arg6) /= 1, frveg == 0
  call netrad_init
  call vegrad_init
  call sslope_init
  call albedo_init
  arg7 = 2
  call netrad(arg1,arg2,arg3,arg4,arg5,arg6,arg7)
  tests(n) = assert(eq(arg2,arg2_no_init_exp), 'BareRadioTemp no init')
  n = n + 1
  tests(n) = assert(eq(arg3,arg3_no_init_exp), 'VegnRadioTemp no init')
  n = n + 1
  tests(n) = assert(eq(arg4,arg4_no_init_exp), 'BareNetRadn no init')
  n = n + 1
  tests(n) = assert(eq(arg5,arg5_no_init_exp), 'VegnNetRadn no init')
  n = n + 1
  tests(n) = assert(eq(arg6,arg6_no_init_exp), 'MixedNetRadn no init')
  n = n + 1

  ! frveg == 0.5
  call netrad_init
  call vegrad_init
  call sslope_init
  call albedo_init
  arg7 = 2
  frveg = 0.5
  call netrad(arg1,arg2,arg3,arg4,arg5,arg6,arg7)
  tests(n) = assert(eq(arg2,arg2_pv_exp), 'BareRadioTemp partial veg')
  n = n + 1
  tests(n) = assert(eq(arg3,arg3_pv_exp), 'VegnRadioTemp partial veg')
  n = n + 1
  tests(n) = assert(eq(arg4,arg4_pv_exp), 'BareNetRadn partial veg')
  n = n + 1
  tests(n) = assert(eq(arg5,arg5_pv_exp), 'VegnNetRadn partial veg')
  n = n + 1
  tests(n) = assert(eq(arg6,arg6_pv_exp), 'MixedNetRadn partial veg')
  n = n + 1

  ! frveg == 1
  call netrad_init
  call vegrad_init
  call sslope_init
  call albedo_init
  arg7 = 2
  frveg = 1.0
  call netrad(arg1,arg2,arg3,arg4,arg5,arg6,arg7)
  tests(n) = assert(eq(arg2,arg2_fv_exp), 'BareRadioTemp full veg')
  n = n + 1
  tests(n) = assert(eq(arg3,arg3_fv_exp), 'VegnRadioTemp full veg')
  n = n + 1
  tests(n) = assert(eq(arg4,arg4_fv_exp), 'BareNetRadn full veg')
  n = n + 1
  tests(n) = assert(eq(arg5,arg5_fv_exp), 'VegnNetRadn full veg')
  n = n + 1
  tests(n) = assert(eq(arg6,arg6_fv_exp), 'MixedNetRadn full veg')
  n = n + 1

!  ! Test LWDOWN
!  call lwdown
!  tests(n) = assert(eq(lwdn,lwdn_exp), 'lwdown')
!  n = n + 1
!
!  ! Test UPLONG(arg1, arg2)
!  call uplong
!  tests(n) = assert(eq(lwup,lwup_exp), 'uplong')
!  n = n + 1

  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1

contains
  subroutine netrad_init
    arg1 = 0.0
    arg4 = 0.0
    arg5 = 0.0
    arg6 = 0.0
    aepsi = 1.0 
    omega = 3.13
    cloud_flag = .true. 
    cld_fract = 14 
    frveg = 0
    rnet = 0.0 
    swave = 0.0 
    epsi = 0.96
    t_fine(3) = 1.0
    tdif_s = 1.0
    tscren = 297.15
    iyr = 89
    iday = 8
    imo = 12
    return
  end subroutine netrad_init

  subroutine vegrad_init
    otemp = 295.0
    atemp = 295.0
    qd(1) = 1.0
    taf = 0.0
    qaf = 0.0
    tf = 1.0
    swave = 10.0
    albdoe = 0.5
    rsg = 1.0
    sigf = 0.9
    albg = 0.1
    albf = 0.1
    rsf = 0.0
    rlf = 0.0
    rlg = 1.0
    rnetg = 1.0
    rnetf = 0.0
  
    epsi = 0.96
    epsf = 0.96
    sol = 0.9
    tg = 1.0
    return
  end subroutine vegrad_init

  subroutine sslope_init
    aspect = 0
    slope = 0
    return
  end subroutine sslope_init

  subroutine albedo_init
    XLAI = 1.0
    WGG = 100.0
    WMAX = 1230.0
    return
  end subroutine albedo_init


end program test_netrad
