program test_daykm
  use simsphere_mod, only: hgt, za, karman, ustar, mol, km, ntrp, &
                           deltaz, zi, zk, eq
  use mod_testing, only: assert, initialize_tests, report_tests
  use, intrinsic :: ieee_arithmetic
  implicit none

  ! mod_testing setup
  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests

  real :: arg1

  ! Two cases: HGT - ZA == 0 and HGT - ZA /= 0
  ! Expected results
  real, parameter, dimension(50) :: km_I_exp = 0.0
  real, parameter, dimension(50) :: km_II_exp = (/-253.990555,50889.9414,  &
                                                  76076.1875,76076.1875,   &
                                                  76076.1875,76076.1875,   &
                                                  76076.1875,76076.1875,   &
                                                  76076.1875,76076.1875,   &
                                                  0.0,0.0,0.0,0.0,0.0,     &
                                                  0.0,0.0,0.0,0.0,0.0,     &
                                                  0.0,0.0,0.0,0.0,0.0,     &
                                                  0.0,0.0,0.0,0.0,0.0,     &
                                                  0.0,0.0,0.0,0.0,0.0,     &
                                                  0.0,0.0,0.0,0.0,0.0,     &
                                                  0.0,0.0,0.0,0.0,0.0,     &
                                                  0.0,0.0,0.0,0.0,0.0/)
  real, parameter, dimension(50) :: zk_I_exp = 0.0
  real, parameter, dimension(50) :: zk_II_exp = (/5.0,4.5,4.5,4.5,4.5,     &
                                                  4.5,4.5,4.5,4.5,4.5,     &
                                                  4.5,0.0,0.0,0.0,0.0,     &
                                                  0.0,0.0,0.0,0.0,0.0,     & 
                                                  0.0,0.0,0.0,0.0,0.0,     & 
                                                  0.0,0.0,0.0,0.0,0.0,     & 
                                                  0.0,0.0,0.0,0.0,0.0,     & 
                                                  0.0,0.0,0.0,0.0,0.0,     & 
                                                  0.0,0.0,0.0,0.0,0.0,     & 
                                                  0.0,0.0,0.0,0.0,0.0/)


  n = 1
  ntests = 6
  call initialize_tests(tests,ntests)

  if (ieee_support_rounding(IEEE_NEAREST)) then
    call ieee_set_rounding_mode(IEEE_NEAREST)
  end if

  hgt = 50.0
  ustar = 3.0
  km = 0.0
  ntrp = 10
  deltaz = 1.0
  zi = 5.0
  zk = 0.0
  mol = 1.0

  ! Case I (thick == 1, hgt == za)
  arg1 = 1.0
  call daykm(arg1)
  tests(n) = assert(eq(km,km_I_exp), 'daykm km case I')
  n = n + 1
  tests(n) = assert(eq(zk,zk_I_exp), 'daykm zk case I')
  n = n + 1

  ! Case II (thick == 1, hgt /= za)
  arg1 = 1.0
  km = 0.0
  zk = 0.0
  hgt = 60.0
  call daykm(arg1)
  tests(n) = assert(eq(km,km_II_exp), 'daykm km case II')
  n = n + 1
  tests(n) = assert(eq(zk,zk_II_exp), 'daykm zk case II')
  n = n + 1

  ! Case III (thick = 0)
  arg1 = 0.0
  call daykm(arg1)
  tests(n) = assert(eq(km,km_II_exp), 'daykm km case III')
  n = n + 1
  tests(n) = assert(eq(zk,zk_II_exp), 'daykm zk case III')
  n = n + 1

  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1
  
end program test_daykm
