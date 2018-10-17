program test_gtemp
  use simsphere_mod, only: eq, aepsi, t_fine, tdif_s, swave, heat, otemp,   &
                           evap, z, epsi
  use mod_testing, only: assert, initialize_tests, report_tests
  use, intrinsic :: ieee_arithmetic
  implicit none

  ! mod_testing setup
  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests

  ! Expected results
  real, parameter :: otemp_exp = 219.793854

  n = 1
  ntests = 1
  call initialize_tests(tests,ntests)

  if (ieee_support_rounding(IEEE_NEAREST)) then
    call ieee_set_rounding_mode(IEEE_NEAREST)
  end if

  AEPSI = 1.17
  t_fine = 1.0
  tdif_s = 0.5
  swave = 1.0
  heat = 2.49819231
  otemp = 265.0
  evap = 0.3
  z = 1.0
  epsi = 0.96
  
  call GTEMP
  tests(n) = assert(eq(otemp,otemp_exp), 'GTEMP otemp')
  n = n + 1

  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1
  
end program test_gtemp
