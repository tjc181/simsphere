program test_capac
  use simsphere_mod, only: frzp, zp, frhgt, rccap, jcap, psix, psig, volrel, &
                           volini, rkocap, volist, volrmv, volist, rscrit,   &
                           psist, deltvst, zst, zstini, rzcap, rcut, psie,   &
                           fs, eq
  use mod_testing, only: assert, initialize_tests, report_tests
  implicit none


  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests

  real :: arg1, arg2, arg3, arg4, arg5, arg6

  ! Expected values
  real, parameter :: psist_exp = -46.0517044
  real, parameter :: psix_exp = 15.9380102
  real, parameter :: volrel_exp = 9.99999978E-03
  real, parameter :: deltvst_exp = 2.23162977E-09
  real, parameter :: zst_exp = 1000.0

  n = 1
  ntests = 5
  call initialize_tests(tests,ntests)

  call capac_init
  call capac(arg1,arg2,arg3,arg4,arg5,arg6)
  tests(n) = assert(eq(psist,psist_exp), 'psist')
  n = n + 1
  tests(n) = assert(eq(psix,psix_exp), 'psix')
  n = n + 1
  tests(n) = assert(eq(volrel,volrel_exp), 'volrel')
  n = n + 1
  tests(n) = assert(eq(deltvst,deltvst_exp), 'deltvst')
  n = n + 1
  tests(n) = assert(eq(zst,zst_exp), 'zst')
  n = n + 1

  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1

contains
  subroutine capac_init
    arg1 = 1.0
    arg2 = 10.0
    arg3 = 2.0
    arg4 = 1.0
    arg5 = 1.0
    arg6 = 2.0

    fs = 1.0
    frzp = 1.0
    zp = 1.0
    frhgt = 0.5
    rccap = 1.0
    jcap = 1.0 
    psie = 20.0
    psix = 20.0
    psig = 21.0
    volrel = 1.0
    volini = 1.0
    rkocap = 10.0
    volist = 1.0
    volrmv = 1.0
    volist = 10.0
    psist = 1.0
    deltvst = 1.0
    zst = 10.0
    zstini = 10.0
    rzcap = 1.0
    rcut = 1.0
    rscrit = 1.0
    return
  end subroutine capac_init


end program test_capac
