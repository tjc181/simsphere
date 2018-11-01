program test_ozone
  use simsphere_mod, only: ustar, uten, uaf, xlai, raf, rcut, rst, chg, eq,  &
                           rzascr, coz_air, frveg, sumo3, fglobal,           &
                           flux_plant, ozone
  use mod_testing, only: assert, initialize_tests, report_tests
  implicit none


  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests

  ! Expected values
  real, parameter :: ozone_flux_plant_expected = 581.332764
  real, parameter :: ozone_fglobal_expected = 10028.6299

  n = 1
  ntests = 2
  call initialize_tests(tests,ntests)

  call ozone_init
  call ozone
  tests(n) = assert(eq(flux_plant,ozone_flux_plant_expected), 'ozone flux_plant')
  n = n + 1
  tests(n) = assert(eq(fglobal,ozone_fglobal_expected), 'ozone fglobal')
  n = n + 1

  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1

contains
  subroutine ozone_init
    USTAR = 1.0
    UTEN = 1.0
    UAF = 0.5
    XLAI = 2.0
    RAF = 1.0
    RCUT = 1.0
    RST = 1.0
    CHG = 2.0
    RZASCR = 1.0
    COZ_AIR = 1.0
    frveg = 0.5
    sumo3 = 0.1
    return
  end subroutine ozone_init


end program test_ozone
