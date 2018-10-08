program test_psoil
  use simsphere_mod, only: dzeta, dual_ti, lambda, kappa, cg, tp, tt, del,  &
                           delta, btemp, otemp, nlvls, tt, z, xfun, w2g,    &
                           wgg, wmax, fsub, f, ti_a, ti_b, eq
  use mod_testing, only: assert, initialize_tests, report_tests
  implicit none

  ! mod_testing setup
  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests

  ! kappa, lambda, cg, tp, w2g, wgg, tt, xfun, z
  ! Expected results
  real, parameter :: lambda_exp = 8.37905556E-02
  real, parameter :: kappa_exp = 1.14126131E-07
  real, parameter :: cg_exp = 694511.812
  real, parameter :: tp_exp = 267.0
  real, parameter :: w2g_exp = 1.0
  real, parameter :: wgg_exp = 1.0
  real, parameter, dimension(9) :: tt_exp = (/265.0,265.200012,265.400024,    &
                                              265.600037,265.800049,         &
                                              266.000061,266.200073,         &
                                              266.400085,266.600098/)
  real, parameter, dimension(9) :: xfun_exp = (/1.0,2.71828175,7.38905621,   &
                                                 20.0855370,54.5981483,      &
                                                 148.413162,403.428802,      &
                                                 1096.63318,2980.95801/)
  real, parameter, dimension(9) :: z_exp = (/0.0,6.4097899e-03,2.38334071e-02,&
                                             7.11957067e-02,0.199939772,      &
                                             0.549902499,1.50119972,          &
                                             4.08709383,11.1162825/)

  real, parameter :: lambda_N_exp = 8.92166607E-03
  real, parameter :: kappa_N_exp = 5.17543608E-09
  real, parameter :: cg_N_exp = 1630679.62
  real, parameter :: tp_N_exp = 267.0
  real, parameter :: w2g_N_exp = 1.0
  real, parameter :: wgg_N_exp = 1.0
  real, parameter, dimension(9) :: tt_N_exp = (/265.0,265.200012,265.400024, &
                                              265.600037,265.800049,         &
                                              266.000061,266.200073,         &
                                              266.400085,266.600098/)
  real, parameter, dimension(9) :: xfun_N_exp = (/1.0,2.71828175,7.38905621, &
                                                 20.0855370,54.5981483,      &
                                                 148.413162,403.428802,      &
                                                 1096.63318,2980.95801/)
  real, parameter, dimension(9) :: z_N_exp = (/0.0,1.36497512e-03,            &
                                               5.07536251e-03,1.51612405e-02, &
                                               4.25774977e-02,0.117102623,    &
                                               0.319682896,0.870353222,       &
                                               2.36723018/)

  ! Check value with dual_ti set to each of 'Y' and 'N'

  n = 1
  ntests = 18
  call initialize_tests(tests,ntests)

  ! Case I: dual_ti = 'Y'
  call psoil_init
  dual_ti = 'Y'
  call psoil
  tests(n) = assert(eq(lambda,lambda_exp), 'psoil lambda')
  n = n + 1
  tests(n) = assert(eq(kappa,kappa_exp), 'psoil kappa')
  n = n + 1
  tests(n) = assert(eq(cg,cg_exp), 'psoil cg')
  n = n + 1
  tests(n) = assert(eq(tp,tp_exp), 'psoil tp')
  n = n + 1
  tests(n) = assert(eq(w2g,w2g_exp), 'psoil w2g')
  n = n + 1
  tests(n) = assert(eq(wgg,wgg_exp), 'psoil wgg')
  n = n + 1
  tests(n) = assert(eq(tt,tt_exp), 'psoil tt')
  n = n + 1
  tests(n) = assert(eq(xfun,xfun_exp), 'psoil xfun')
  n = n + 1
  tests(n) = assert(eq(z,z_exp), 'psoil z')
  n = n + 1
  write(*,*) tp

  ! Case II: dual_ti = 'N'
  call psoil_init
  dual_ti = 'N'
  call psoil
  tests(n) = assert(eq(lambda,lambda_N_exp), 'psoil not dual lambda')
  n = n + 1
  tests(n) = assert(eq(kappa,kappa_N_exp), 'psoil not dual kappa')
  n = n + 1
  tests(n) = assert(eq(cg,cg_N_exp), 'psoil not dual cg')
  n = n + 1
  tests(n) = assert(eq(tp,tp_N_exp), 'psoil not dual tp')
  n = n + 1
  tests(n) = assert(eq(w2g,w2g_N_exp), 'psoil not dual w2g')
  n = n + 1
  tests(n) = assert(eq(wgg,wgg_N_exp), 'psoil not dual wgg')
  n = n + 1
  tests(n) = assert(eq(tt,tt_N_exp), 'psoil not dual tt')
  n = n + 1
  tests(n) = assert(eq(xfun,xfun_N_exp), 'psoil not dual xfun')
  n = n + 1
  tests(n) = assert(eq(z,z_N_exp), 'psoil not dual z')
  n = n + 1
  write(*,*) tp

  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1

contains
  subroutine psoil_init
    btemp = 267.0
    cg = 0.0
    del = 0.0
    f = 1.0
    fsub = 1.0
    kappa = 0.0
    lambda = 0.0
    nlvls = 10
    otemp = 265.0
    ti_a = 1.0
    ti_b = 1.0
    tp = 1.0
    tt = 0.0
    w2g = 1.0
    wgg = 1.0
    wmax = 1.0
    xfun = 0.0
    z = 0.0
    return
  end subroutine psoil_init
  
end program test_psoil
