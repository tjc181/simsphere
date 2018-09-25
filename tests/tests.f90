program test_simsphere
  use simsphere_mod
  use mod_testing, only: assert,initialize_tests,report_tests
  implicit none

  ! mod_testing test setup

  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests
  integer, parameter :: stdout = 6

  ! end mod_testing test setup

  integer :: i
  
  logical :: splint_test, spline_test, start_test 
  logical :: transm_ftabs_test, transm_ftabs_test2
  logical :: transm_ftscatT_test, transm_ftscatT_test2
  logical :: transm_fbscatT_test, transm_fbscatT_test2
  logical :: advect_func_test
!  logical :: air_test
  logical :: cond_test
  logical :: stomfs_test
  logical :: stomrs_test_hi_temp, stomrs_test_lo_temp, stomrs_test_hi_psi, stomrs_test_lo_psi
  logical :: stomc_test
  logical :: you_star_test, r_ohms_test, windf_test, stab_test, stabh_test, fstabh_test, fstabm_test, restrn_test
  logical :: psgcal_test
  logical :: vegflx_test
  logical :: avr_test_init1, avr_test_init_not1

! splint_test variables
  integer, parameter :: splint_max_array = 50
  real, parameter :: splint_expected = 1.5
  real :: splint_arg1(splint_max_array), splint_arg2(splint_max_array)  
  real(kind=4), allocatable :: splint_arg3(:)
  integer :: splint_arg4, splint_arg5
  real :: splint_output

! spline_test variables
  integer, parameter :: spline_max_array = 50
  integer :: spline_arg3
  real, parameter :: spline_expected = 2.59807634
  real :: spline_arg1(spline_max_array), spline_arg2(spline_max_array)
  real :: spline_arg4, spline_arg5
  real, allocatable :: spline_output(:)

! transm_ftabsT variables
  real :: transm_ftabs_arg1
  real, parameter :: transm_ftabs_expected = 0.722923875
  real, parameter :: transm_ftabs_expected2 = 0.437107921

! transm_ftscatT variables
  real :: transm_ftscatT_arg1
  real, parameter :: transm_ftscatT_expected = 0.553545594
  real, parameter :: transm_ftscatT_expected2 = 0.173223004 

! transm_fbscatT variables
  real :: transm_fbscatT_arg1
  real, parameter :: transm_fbscatT_expected = 0.307550341
  real, parameter :: transm_fbscatT_expected2 = 0.304252803

! netrad_init_test variables
  real, parameter :: aepsi_expected = 0.804384410
  real, parameter :: aepsi_cloud_flag_expected = 1.02347386

! advect_test variables
  real, parameter :: advect_test_expected = 9.41291146E-05

! air_test variables
!  real, parameter :: air_test_expected = 0.0

! cond_test variables
  real, parameter :: cond_test_expected = 1.46884334

! stomfs_test variables
  real, parameter :: stomfs_test_expected = 1.58197677

! stomrs_test_* variables
  real, parameter :: stomrs_test_hi_temp_expected = 5000.0
  real, parameter :: stomrs_test_lo_temp_expected = 5000.0
  real, parameter :: stomrs_test_hi_psi_expected = 3.16395354
  real, parameter :: stomrs_test_lo_psi_expected = 7.90988398

! stomc_test variables
  real, parameter :: stomc_test_expected = 4.74593019

! you_star_test variables
  real, parameter :: you_star_test_expected = 1.73396431E-02
  real :: you_star_arg1, you_star_arg2, you_star_arg3, you_star_arg4, you_star_arg5

! r_ohms_test variables
  real, parameter :: r_ohms_test_expected = 42.6767731
  real :: r_ohms_arg1, r_ohms_arg2, r_ohms_arg3, r_ohms_arg4, r_ohms_arg5

! windf_test variables
  real, parameter :: windf_test_expected = 8.98633766
  real :: windf_arg1, windf_arg2, windf_arg3, windf_arg4, windf_arg5

! stab_test variables
  real, parameter :: stab_test_expected = 0.707106769
  real :: stab_arg1, stab_arg2

! stabh_test variables
  real, parameter :: stabh_test_expected = 0.741619825

! fstabh_test variables
  real, parameter :: fstabh_test_expected = -0.810930133
  real :: fstabh_arg1, fstabh_arg2

! fstabm_test variables
  real, parameter :: fstabm_test_expected = -0.572894096

! restrn_test variables
  real, parameter :: restrn_test_expected = 0.590972006
  real :: restrn_arg1, restrn_arg2, restrn_arg3, restrn_arg4

! psgcal_test variables
  real, parameter :: psgcal_test_expected = -2.14821539E-05

! vegflx_test variables
  real, parameter :: vegflx_test_expected = 0.986029029
  real :: vegflx_arg1

! avr_test_* variables
  real, parameter :: avr_test_init1_expected = 20.0
  real, parameter :: avr_test_init_not1_expected = 20.0
  real :: avr_arg1, avr_arg2

! mod_testing variable setup
  n = 1
  ntests = 26
  call initialize_tests(tests,ntests)
! end  mod_testing variable setup


! Set logical to control test execution
  start_test = .false.
  splint_test = .true.
  spline_test = .true.
  transm_ftabs_test = .true.
  transm_ftabs_test2 = .true.
  transm_ftscatT_test = .true.
  transm_ftscatT_test2 = .true.
  transm_fbscatT_test = .true.
  transm_fbscatT_test2 = .true.
  advect_func_test = .true.
  cond_test = .true.
  stomfs_test = .true.
  stomrs_test_hi_temp = .true.
  stomrs_test_lo_temp = .true.
  stomrs_test_hi_psi = .true.
  stomrs_test_lo_psi = .true.
  stomc_test = .true.
  you_star_test = .true.
  r_ohms_test = .true.
  windf_test = .true.
  stab_test = .true.
  stabh_test = .true.
  fstabh_test = .true.
  fstabm_test = .true.
  restrn_test = .true.
  psgcal_test = .true.
  vegflx_test = .true.
  avr_test_init1 = .true.
  avr_test_init_not1 = .true.


!
! splint_test
!
  if (splint_test) then
    do i = 1,splint_max_array
      splint_arg1(i) = 2*i
      splint_arg2(i) = 2*i
    end do
    splint_arg4 = 12
    if (.not. allocated(splint_arg3)) then
      allocate(splint_arg3(splint_arg4))
      do i = 1,splint_arg4
        splint_arg3(i) = 2*i
      end do
    end if
    splint_arg5 = 3

    splint_output=splint(splint_arg1, splint_arg2, splint_arg3, splint_arg4, splint_arg5)
    if (splint_output .ne. splint_expected) then
      write(*,*) 'splint_test: actual /= expected: ', splint_output, splint_expected
    else
      write(*,*) 'splint_test: OK'
    end if
    if (allocated(splint_arg3)) then
      deallocate(splint_arg3)
    end if
  end if

! 
! spline_test
!
  if (spline_test) then
    do i = 1,spline_max_array
      spline_arg1(i) = 2*i
      spline_arg2(i) = 2*i
    end do
    spline_arg3 = spline_max_array
    spline_arg4 = 1.0
    spline_arg5 = 2.5
    if (.not. allocated(spline_output)) then
      allocate(spline_output(spline_arg3))
    end if
    spline_output=spline(spline_arg1, spline_arg2, spline_arg3, spline_arg4, spline_arg5)
    if (spline_output(size(spline_output)) .ne. spline_expected) then
      write(*,*) 'spline_test: actual /= expected: ', spline_output, spline_expected
    else
      write(*,*) 'spline_test: OK'
    end if
    if (allocated(spline_output)) then
      deallocate(spline_output)
    end if
  end if

!
! transm_ftabs_test
!
! Test the "else" branch in ftabsT()

  if (transm_ftabs_test) then
    call ftabsT_init
    transm_ftabs_arg1 = 2.75401473
    tests(n) = assert(ftabst(transm_ftabs_arg1) == transm_ftabs_expected, 'ftabs() else')
    n = n + 1
  end if

!
! transm_ftabs_test2
!
! Test the "if" branch in ftabsT()

  if (transm_ftabs_test2) then
    call ftabsT_init
    transm_ftabs_arg1 = 11.0
    tests(n) = assert(ftabst(transm_ftabs_arg1) == transm_ftabs_expected2, 'ftabs() if')
    n = n + 1 
  end if

!
! transm_ftscatT_test
!
! Test the "else" branch in ftscatT()

  if (transm_ftscatT_test) then
    call ftscatT_init
    transm_ftscatT_arg1 = 2.75401473
    tests(n) = assert(ftscatT(transm_ftscatT_arg1) == transm_ftscatT_expected, 'ftscatT() else')
    n = n + 1
  end if

!
! transm_ftscatT_test2
!
! Test the "if" branch in ftscatT()

  if (transm_ftscatT_test2) then
    call ftscatT_init
    transm_ftscatT_arg1 = 11.0
    tests(n) = assert(ftscatT(transm_ftscatT_arg1) == transm_ftscatT_expected2, 'ftscatT() if')
    n = n + 1
  end if

!
! transm_fbscatT_test
!
! Test the "else" branch in fbscatT()

  if (transm_fbscatT_test) then
    call fbscatT_init
    transm_fbscatT_arg1 = 2.75401473
    tests(n) = assert(fbscatT(transm_fbscatT_arg1) == transm_fbscatT_expected, 'fbscatT() else')
    n = n + 1
  end if

!
! transm_fbscatT_test2
!
! Test the "if" branch in fbscatT()

  if (transm_fbscatT_test2) then
    call fbscatT_init
    transm_fbscatT_arg1 = 11.0
    tests(n) = assert(fbscatT(transm_fbscatT_arg1) == transm_fbscatT_expected2, 'fbscatT() if')
    n = n + 1
  end if

!
! advect_func_test
!
  if (advect_func_test) then
    call advect_init
    tests(n) = assert(advect() == advect_test_expected, 'advect()')
    n = n + 1
  end if   

!
! cond_test
!
  if (cond_test) then
    call cond_init
    tests(n) = assert(cond() == cond_test_expected,'cond()')
    n = n + 1
  end if

!
! stomfs_test
!
  if (stomfs_test) then
    call stomfs_init
    tests(n) = assert(stomfs(sc, sol) == stomfs_test_expected, 'stomfs()')
    n = n + 1
  end if

!
! stomrs_test_hi_temp
!
  if (stomrs_test_hi_temp) then
    call stomrs_init
    TF = MAXTEMP + 1.0
    tests(n) = assert( &
               stomrs(ft,tf,rmin,mintemp,maxtemp,psisup,psiwc,b1,b2,psie,psice,fs) &
               == stomrs_test_hi_temp_expected, 'stomrs_test_hi_temp_expected')
    n = n + 1
  end if

!
! stomrs_test_lo_temp
!
  if (stomrs_test_lo_temp) then
    call stomrs_init
    TF = MINTEMP - 1.0
    tests(n) = assert( &
               stomrs(ft,tf,rmin,mintemp,maxtemp,psisup,psiwc,b1,b2,psie,psice,fs) &
               == stomrs_test_lo_temp_expected, 'stomrs_test_lo_temp_expected')
    n = n + 1
  end if

!
! stomrs_test_hi_psi
!
  if (stomrs_test_hi_psi) then
    call stomrs_init
    TF = (MINTEMP + MAXTEMP)/2
    PSISUP = PSIWC + 1.0
    tests(n) = assert( &
               stomrs(ft,tf,rmin,mintemp,maxtemp,psisup,psiwc,b1,b2,psie,psice,fs) &
               == stomrs_test_hi_psi_expected, 'stomrs_test_hi_psi')
    n = n + 1
  end if

!
! stomrs_test_lo_psi
!
  if (stomrs_test_lo_psi) then
    call stomrs_init
    TF = (MINTEMP + MAXTEMP)/2
    PSISUP = PSIWC - 1.0
    tests(n) = assert( &
               stomrs(ft,tf,rmin,mintemp,maxtemp,psisup,psiwc,b1,b2,psie,psice,fs) &
               == stomrs_test_lo_psi_expected, 'stomrs_test_lo_psi')
    n = n + 1
  end if

!
! stomc_test
!

  if (stomc_test) then
    call stomc_init
    tests(n) = assert(stomc(ft,rmin,b1,psice,fs) == stomc_test_expected,'stomc_test')
    n = n + 1
  end if

!
! you_start_test
!

  if (you_star_test) then
    call you_star_init
    tests(n) = assert( &
               you_star(you_star_arg1,you_star_arg2,you_star_arg3,you_star_arg4,you_star_arg5) &
               == you_star_test_expected,'you_star_test')
    n = n + 1
  end if

!
! r_ohms_test
!

  if (r_ohms_test) then
    call r_ohms_init
    tests(n) = assert( &
               r_ohms(r_ohms_arg1,r_ohms_arg2,r_ohms_arg3,r_ohms_arg4,r_ohms_arg5) &
               == r_ohms_test_expected,'r_ohms_test')
    n = n + 1
  end if

!
! windf_test
!

  if (windf_test) then
    call windf_init
    tests(n) = assert(windf(windf_arg1,windf_arg2,windf_arg3,windf_arg4,windf_arg5) == windf_test_expected,'windf_test')
    n = n + 1
  end if

!
! stab_test
!

  if (stab_test) then
    call stab_init
    tests(n) = assert(stab(stab_arg1,stab_arg2) == stab_test_expected,'stab_test')
    n = n + 1
  end if

!
! stabh_test
!

  if (stabh_test) then
    ! stabh takes same argument as stab so we'll recycle initialization
    call stab_init
    tests(n) = assert(stabh(stab_arg1,stab_arg2) == stabh_test_expected,'stabh_test')
    n = n + 1
  end if

!
! fstabh_test
!

  if (fstabh_test) then
    call fstabh_init
    tests(n) = assert(fstabh(fstabh_arg1,fstabh_arg2) == fstabh_test_expected,'fstabh_test')
    n = n + 1
  end if

!
! fstabm_test
!

  if (fstabm_test) then
    ! fstabm takes the same arguments as fstabh so we'll recycle the initialization
    call fstabh_init
    tests(n) = assert(fstabm(fstabh_arg1,fstabh_arg2) == fstabm_test_expected,'fstabm_test')
    n = n + 1
  end if

!
! restrn_test
!

  if (restrn_test) then
    call restrn_init
    tests(n) = assert(restrn(restrn_arg1,restrn_arg2,restrn_arg3,restrn_arg4) == restrn_test_expected,'restrn()')
    n = n + 1
  end if

!
! psgcal_test
!

  if (psgcal_test) then
    call psgcal_init
    call psgcal
    tests(n) = assert(PSIG == psgcal_test_expected, 'psgcal_test')
    n = n + 1
  end if

! 
! vegflx_test
!

  if (vegflx_test) then
    call vegflx_init
    call vegflx(vegflx_arg1)
    tests(n) = assert(QAF == vegflx_test_expected, 'vegflx_test')
    n = n + 1
  end if

!
! avr_test_init1
!

  if (avr_test_init1) then
    call avr_init
    call average(avr_arg1, avr_arg2)
    tests(n) = assert(avr_arg2 == avr_test_init1_expected, 'avr_test_init1')
    n = n + 1
  end if

!
! avr_test_init_not1
!

  if (avr_test_init_not1) then
    call avr_init
    call average(avr_arg1, avr_arg2)
    tests(n) = assert(avr_arg2 == avr_test_init_not1_expected, 'avr_test_init_not1')
    n = n + 1
  end if

! Report test summary
  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1

contains 
  subroutine ftabsT_init
    OMEGA = 3.13
    ABSTBL(9) = 0.718309104
    ABSTBL(10) = 0.707092881
    ABSTBL(46) = 0.437107921
    PS1 = 967.0
    return
  end subroutine ftabsT_init
  
  subroutine ftscatT_init
    OMEGA = 3.13
    SCATBL(9) = 0.548576415
    SCATBL(10) = 0.527300596
    SCATBL(46) = 0.173223004
    PS1 = 967.0
    return
  end subroutine ftscatT_init

  subroutine fbscatT_init
    OMEGA = 3.13
    BSCTBL(9) = 0.307897508
    BSCTBL(10) = 0.307446688
    BSCTBL(46) = 0.304252803
    PS1 = 967.0
    return
  end subroutine fbscatT_init

  subroutine advect_init
    CF = 9.19953891E-05
    OTEMP = 295.149994
    VGD(5) = 8.41951942
    VGD(3) = 8.43165302
    VGD(1) = 8.44378662
    UGD(5) = 13.0658665
    UGD(3) = 9.05783463
    UGD(1) = 5.04980326
    return
  end subroutine advect_init

  subroutine cond_init
    RKS = 6.60699987
    THV = 1.00000000
    THMAX = 0.338999987
    COSBYB = 2.78999996
    return
  end subroutine cond_init

  subroutine stomfs_init
    sc = 1.0
    sol = 1.0
    return
  end subroutine stomfs_init

  subroutine stomrs_init
    RMIN = 1.0
    MINTEMP = 1.0
    MAXTEMP = 100.0
    PSIWC = 1.0
    PSIE = 1.0
    PSICE = 2.0
    B1 = 1.0
    B2 = 2.0
    call stomfs_init
    FS = stomfs(sc,sol)
    return
  end subroutine stomrs_init

  subroutine stomc_init
    B1 = 1.0
    PSICE = 2.0
    RMIN = 1.0
    call stomfs_init
    FS = stomfs(sc,sol)
    return
  end subroutine stomc_init

  subroutine you_star_init
    you_star_arg1 = 0.1
    you_star_arg2 = 1.0
    you_star_arg3 = 2.0
    you_star_arg4 = 3.0
    you_star_arg5 = karman
    return
  end subroutine you_star_init

  subroutine r_ohms_init
    r_ohms_arg1 = 0.1
    r_ohms_arg2 = 1.0
    r_ohms_arg3 = 2.0
    r_ohms_arg4 = 3.0
    r_ohms_arg5 = karman
    return
  end subroutine r_ohms_init

  subroutine windf_init
    windf_arg1 = 1.0
    windf_arg2 = 2.0
    windf_arg3 = 3.0
    windf_arg4 = 4.0
    windf_arg5 = karman
    return
  end subroutine windf_init

  subroutine stab_init
    ! also used to test stabh
    stab_arg1 = 0.1
    stab_arg2 = 2.0
    return
  end subroutine stab_init

  subroutine fstabh_init
    ! also used to test fstabm
    fstabh_arg1 = 1.0
    fstabh_arg2 = 2.0
    return
  end subroutine fstabh_init

  subroutine restrn_init
    restrn_arg1 = 1.0
    restrn_arg2 = 2.0
    restrn_arg3 = 3.0
    restrn_arg4 = karman
    return
  end subroutine

  subroutine psgcal_init
    THMAX = 0.338999987
    THV = 1.0
    COSBYB = 2.78999996
    PSIS = 1.0
    return
  end subroutine psgcal_init

  subroutine vegflx_init
    vegflx_arg1 = 0.3
    TAF = 10.0
    CHF = 10.0
    F = 100.0
    CHG = 0.1
    QAF = 0.5
    HG = 1.0
    CHG = 100.5
    XLEF = 1.2
    CHA = 100.0
    TA = 0.4
    CHF = 10.0
    TF = 1.0
    RST = 1.0
    QA = 100.0
    QSTF = 10.0
    return
  end subroutine vegflx_init

  subroutine avr_init
    avr_arg1 = 20.0
    avr_arg2 = 0.0
  end subroutine avr_init

end program
