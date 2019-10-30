program test_pslcal
  use simsphere_mod, only: thv, w2g, fs, zg, zp, b1, b2, rmin, vegheight, sc,&
                           xlai, raf, rmin, psice, vfl, qstf, rst, ps1, sol, &
                           steady, rcut, rscrit, beta, psisup, psig, psiwc,  &
                           psim, psie, mintemp, maxtemp, wpsi, rlpsi, thmax, &
                           rks, cosbyb, psis, rccap, zst, psist, zstini,     &
                           volrel, volini, rzcap, psix, frhgt, rlelf, frzp,  &
                           rkocap, eq
  use mod_testing, only: assert, initialize_tests, report_tests
  implicit none


  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests

  real, parameter :: arg1 = 1.0
  real, parameter :: arg2 = 5.0
  integer :: arg3

  ! Expected values
  real, parameter :: wpsi_unsteady_exp = 23.7398777
  real, parameter :: wpsi_steady_exp = 29.0206356
  real, parameter :: rlpsi_unsteady_exp = 24.9543571
  real, parameter :: rlpsi_steady_exp = 28.9508419

  n = 1
  ntests = 4
  call initialize_tests(tests,ntests)

  call pslcal_init
  arg3 = 1
  steady = 'N'
  call pslcal(arg1,arg2,arg3)
  tests(n) = assert(eq(wpsi,wpsi_unsteady_exp), 'wpsi unsteady')
  n = n + 1
  tests(n) = assert(eq(rlpsi,rlpsi_unsteady_exp), 'rlpsi unsteady')
  n = n + 1

  call pslcal_init
  arg3 = 1
  steady = 'Y'
  call pslcal(arg1,arg2,arg3)
  !write(6,*) 'pslcal steady: wpsi,rlpsi: ',wpsi,rlpsi
  tests(n) = assert(eq(wpsi,wpsi_steady_exp), 'wpsi steady')
  n = n + 1
  tests(n) = assert(eq(rlpsi,rlpsi_steady_exp), 'rlpsi steady')
  n = n + 1

  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1

contains
  subroutine pslcal_init
    thv = 3.0
    thmax = 5.0
    w2g = 0.25
    fs = 1.0
    zg = 1.0
    zp = 1.7
    frzp = 1.0
    b1 = 5.0
    b2 = 2.0
    cosbyb = 3.0
    rmin = 1.0
    vegheight = 2.0
    xlai = 1.0
    raf = 3.0
    rmin = 0.1
    psice = 20.0 
    vfl = 1.0 
    qstf = 2.5 
    rst = 1.3
    rks = 3.0
    ps1 = 2.7 
    rcut = 1.0 
    rscrit = 2.5 
    beta = 0.3 
    psisup = 0.5 
    psig = 1.3
    psiwc = 2.7
    psist = 1.5
    psim = 1.3 
    psie = 1.5
    psis = 1.0
    mintemp = 0.0 
    maxtemp = 100.0
    wpsi = 20.0
    rlpsi = 14.0
    rccap = 1.0
    sc = 1.0
    sol = 1.0
    volrel = 1.0
    volini = 1.0
    psix = 1.0
    frhgt = 1.0
    rlelf = 1.0
    rzcap = 1.0
    rkocap = 1.0
    zstini = 0.001
    zst = 0.001
    return
  end subroutine pslcal_init

end program test_pslcal
