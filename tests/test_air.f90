program test_air
  use simsphere_mod, only: HGT, GAM, GM, HET, HEAT, OTEMP, ADVGT, DENS, CP,  &
                           APTEMP, ATEMP, TDIF_50, DELT, DHET, TD, NTRP, RAD,&
                           GRAV, DELTA, ZA, eq
  use, intrinsic :: ieee_arithmetic
  use mod_testing, only: assert, initialize_tests, report_tests
  implicit none


  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests


  real, dimension(50) :: ZLS
  real :: YCOUNT = 1.0
  integer :: i
  ! Returned values
  real :: tdel = 0.0, ttop = 0.0

  ! Expected values
  real, parameter :: tdel_exp = 6.27781712E-42
  real, parameter :: ttop_exp = 6.67093044E-37
  real, parameter :: ycount_exp = 2.0

  if (ieee_support_rounding(IEEE_NEAREST)) then
    call ieee_set_rounding_mode(IEEE_NEAREST)
  end if

  ! Initialize mod_testing
  n = 1
  ntests = 3
  call initialize_tests(tests,ntests)

  ! Initialize globals
  HGT = 3.0
  GAM = 1.0
  do i = 1,50
    ZLS(i) = real(i)
    GM(i) = real(i)*0.5
  end do
  HET = 1.0
  HEAT = 2.49819231
  OTEMP = 295.149994
  ADVGT = 9.41291146E-05
  APTEMP = 2.0
  ATEMP = 2.0
  TDIF_50 = 1.0
  DELT = 1.0
  DHET = 1.0
  TD(1) = 1.0
  NTRP=10

  
  call air(ZLS,YCOUNT)

  write(*,*) tdel, tdel_exp
  tests(n) = assert(tdel == tdel_exp, 'tdel')
  n = n + 1
  tests(n) = assert(eq(ttop,ttop_exp), 'ttop')
  n = n + 1
  tests(n) = assert(eq(ycount,ycount_exp), 'ycount')
  n = n + 1

  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1


end program test_air
