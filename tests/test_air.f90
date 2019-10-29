program test_air
  use simsphere_mod, only: HGT, GAM, GM, HET, HEAT, OTEMP, ADVGT, APTEMP,   &
                           ATEMP, TDIF_50, DELT, DHET, TD, NTRP, deltaz, eq 
  use mod_testing, only: assert, initialize_tests, report_tests
  implicit none


  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests


  real, dimension(50) :: ZLS
  real :: YCOUNT = 1.0
  real :: CHGT, CDELT, CTHETA
  integer :: i

  ! Expected values
  real, parameter, dimension(50) :: td_exp = (/2.0,0.0,0.0,0.0,0.0,     &
                                               0.0,0.0,0.0,0.0,0.0,     &
                                               0.0,0.0,0.0,0.0,0.0,     &
                                               0.0,0.0,0.0,0.0,0.0,     &
                                               0.0,0.0,0.0,0.0,0.0,     &
                                               0.0,0.0,0.0,0.0,0.0,     &
                                               0.0,0.0,0.0,0.0,0.0,     &
                                               0.0,0.0,0.0,0.0,0.0,     &
                                               0.0,0.0,0.0,0.0,0.0,     &
                                               0.0,0.0,0.0,0.0,0.0/)
  real, parameter :: aptemp_exp = 2.0
  real, parameter :: ycount_exp = 2.0
!  real, parameter, dimension(50) :: td_hgt_exp = (/2.0,0.0,0.0,0.0,0.0,     &
!                                               0.0,0.0,0.0,0.0,0.0,     &
!                                               0.0,0.0,0.0,0.0,0.0,     &
!                                               0.0,0.0,0.0,0.0,0.0,     &
!                                               0.0,0.0,0.0,0.0,0.0,     &
!                                               0.0,0.0,0.0,0.0,0.0,     &
!                                               0.0,0.0,0.0,0.0,0.0,     &
!                                               0.0,0.0,0.0,0.0,0.0,     &
!                                               0.0,0.0,0.0,0.0,0.0,     &
!  real, parameter, dimension(50) :: td_hgt_exp = 2.0
!  real, parameter :: aptemp_hgt_exp = 2.01912546
!  real, parameter :: ycount_hgt_exp = 3.0


  ! Initialize mod_testing
  n = 1
  ntests = 3
  call initialize_tests(tests,ntests)

  ! Case I, hgt < zmix
  call air_init 
  hgt = 3.0
  call air(ZLS,YCOUNT,CHGT,CDELT,CTHETA)
  tests(n) = assert(eq(td,td_exp), 'td')
  n = n + 1
  tests(n) = assert(eq(ycount,ycount_exp), 'ycount')
  n = n + 1
  tests(n) = assert(eq(aptemp,aptemp_exp), 'aptemp')
  n = n + 1

!  ! Case II, hgt > zmix
!  call air_init
!  hgt = 51.0
!  call air(ZLS,YCOUNT,CHGT,CDELT,CTHETA)
!  tests(n) = assert(eq(td,td_hgt_exp), 'td')
!  n = n + 1
!  tests(n) = assert(eq(ycount,ycount_exp), 'ycount')
!  n = n + 1
!  tests(n) = assert(eq(aptemp,aptemp_exp), 'aptemp')
!  n = n + 1
!  write(*,*) td, ycount, aptemp

  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1

contains
  subroutine air_init
    ! Initialize globals
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
    TD = 0.0
    NTRP = 5
    deltaz = 5.0
    return
  end subroutine air_init 

end program test_air
