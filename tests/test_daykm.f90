program test_daykm
  use simsphere_mod, only: hgt, ustar, mol, km, ntrp, deltaz, zi, zk, za, eq, &
    Vert_Spacing
  use mod_testing, only: assert, initialize_tests, report_tests
  use, intrinsic :: ieee_arithmetic
  implicit none

  ! mod_testing setup
  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: i, n, ntests

  real :: arg1

  ! Three cases:
  !
  !    1) HGT=ZA, MOL=0.0, USTAR=3.0 (USTAR and MOL don't matter in this case)
  !    2) HGT=1200, MOL=-400.0, USTAR=6.0
  !    3) HGT=2100, MOL=-800.0, USTAR=9.0
  !
  ! Expected results:
  !
  real, parameter, dimension(50) :: km_I_exp = 0.0

  real, parameter, dimension(50) :: km_II_exp = (/156.257446,670.694214,  &
                                                  921.362793,647.406067,   &
                                                  234.105774,31.7086887,   &
                                                  0.0,0.0,0.0,0.0,         &
                                                  0.0,0.0,0.0,0.0,0.0,     &
                                                  0.0,0.0,0.0,0.0,0.0,     &
                                                  0.0,0.0,0.0,0.0,0.0,     &
                                                  0.0,0.0,0.0,0.0,0.0,     &
                                                  0.0,0.0,0.0,0.0,0.0,     &
                                                  0.0,0.0,0.0,0.0,0.0,     &
                                                  0.0,0.0,0.0,0.0,0.0,     &
                                                  0.0,0.0,0.0,0.0,0.0/)

  real, parameter, dimension(50) :: km_III_exp = (/212.364990,1016.39429,  &
                                                  1794.22278,2026.54041,   &
                                                  1858.56458,1435.51331,   &
                                                  902.604553,405.056061,   &
                                                  88.0854797,2.62376094,   &
                                                  0.0,0.0,0.0,0.0,0.0,     &
                                                  0.0,0.0,0.0,0.0,0.0,     &
                                                  0.0,0.0,0.0,0.0,0.0,     &
                                                  0.0,0.0,0.0,0.0,0.0,     &
                                                  0.0,0.0,0.0,0.0,0.0,     &
                                                  0.0,0.0,0.0,0.0,0.0,     &
                                                  0.0,0.0,0.0,0.0,0.0,     &
                                                  0.0,0.0,0.0,0.0,0.0/)

  real, parameter :: thick_I_exp = 0.0
  real, parameter :: thick_II_exp = 1150.0
  real, parameter :: thick_III_exp = 2050.0

  n = 1
  ntests = 6
  call initialize_tests(tests,ntests)

  if (ieee_support_rounding(IEEE_NEAREST)) then
    call ieee_set_rounding_mode(IEEE_NEAREST)
  end if

  ! Setup
  ntrp = 10
  do i=0,ntrp
  zi(i+1) = 50 + i*Vert_Spacing
  end do

  ! Case I
  hgt = za
  mol = 0.0
  ustar = 3.0
  call daykm(arg1)
  !write(6,*) 'Case I: thick= ',arg1
  !write(6,*) 'Case I: km: ',km
  !write(6,*) 'Case I: km_I_exp: ',km_I_exp
  tests(n) = assert(eq(km,km_I_exp), 'daykm km case I')
  n = n + 1
  tests(n) = assert(eq(arg1,thick_I_exp), 'daykm thick case I')
  n = n + 1

  ! Case II
  hgt = 1200
  mol = -400.0
  ustar = 6.0
  call daykm(arg1)
  !write(6,*) 'Case II: thick= ',arg1
  !write(6,*) 'Case II: km: ',km
  !write(6,*) 'Case II: km_II_exp: ',km_II_exp
  tests(n) = assert(eq(km,km_II_exp), 'daykm km case II')
  n = n + 1
  tests(n) = assert(eq(arg1,thick_II_exp), 'daykm thick case II')
  n = n + 1

  ! Case III
  hgt = 2100
  mol = -800.0
  ustar = 9.0
  call daykm(arg1)
  !write(6,*) 'Case III: thick= ',arg1
  !write(6,*) 'Case III: km: ',km
  !write(6,*) 'Case III: km_III_exp: ',km_III_exp
  tests(n) = assert(eq(km,km_III_exp), 'daykm km case III')
  n = n + 1
  tests(n) = assert(eq(arg1,thick_III_exp), 'daykm thick case III')
  n = n + 1

  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1
  
end program test_daykm
