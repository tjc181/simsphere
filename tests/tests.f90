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
  
  logical :: splint_test, spline_test
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
  logical :: f_control_test

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
  real, parameter :: spline_expected(62) =                            &
    (/-8.84081032E-07,1.76816206E-06,-6.18856711E-06,2.29861053E-05,  &  
      -8.57558480E-05,3.20037303E-04,-1.19439338E-03,4.45753615E-03,  &
      -1.66357514E-02,6.20854683E-02,-6.20854646E-02,1.66357514E-02,  &
      -4.45753615E-03,1.19439315E-03,-3.20036663E-04,8.57535633E-05,  &
      -2.29775978E-05,6.15682848E-06,-1.64971721E-06,4.42040374E-07,  &
      -1.18444362E-07,3.17370699E-08,-8.50392201E-09,2.27861907E-09,  &
      -6.10554107E-10,1.63597469E-10,-4.38358100E-11,1.17457693E-11,  &
      -3.14726977E-12,8.43309526E-13,-2.25968590E-13,6.05648480E-14,  &
      -1.62907932E-14,4.59832376E-15,-2.10250179E-15,3.81168384E-15,  &
      -1.31442336E-14,4.87652521E-14,-1.81916778E-13,6.78901867E-13,  &
      -2.53369049E-12,9.45585971E-12,-3.52897503E-11,1.31703135E-10,  &
      -4.91522767E-10,1.83438786E-09,-6.84602863E-09,2.55497259E-08,  &
      -9.53528740E-08,3.55861772E-07,-1.32809419E-06,4.95651511E-06,  &
      -1.84979654E-05,6.90353481E-05,-2.57643434E-04,9.61538346E-04,  &
      -3.58850998E-03,1.33925015E-02,-4.99814972E-02,0.186533481,     &
      -0.696152449,2.59807634/)
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

! f_control variables
  integer, parameter :: iyr_expected = 89
  integer, parameter :: imo_expected = 8
  integer, parameter :: iday_expected = 4
  real, parameter :: tz_expected = 6
  real, parameter :: xlat_expected = 39.25
  real, parameter :: xlong_expected = 96.34
  real, parameter :: strtim_expected = 0530
  real, parameter :: timend_expected = 2330
  real, parameter :: outtt_expected = 30
  real, parameter :: slope_expected = 0
  real, parameter :: aspect_expected = 0
  real, parameter :: f_expected = 0.5
  real, parameter :: fsub_expected = 0.75
  real, parameter :: wmax_expected = 0.34
  real, parameter :: btemp_expected = 24.63
  real, parameter :: tp_expected = 13
  character, parameter :: dual_ti_expected = 'Y'
  real, parameter :: ti_a_expected = 12.0
  real, parameter :: ti_b_expected = 12.0
  character, parameter :: albedo_gflag_expected = 'F'
  real, parameter :: albg_expected = 0.1
  real, parameter :: epsi_expected = 0.96
  integer, parameter :: index_soils_expected = 5
  real, parameter :: omega_expected = 3.13
  real, parameter :: zo_expected = 0.05
  real, parameter :: obst_hgt_expected = 1
  logical, parameter :: cloud_flag_expected = .true.
  real, parameter :: cld_fract_expected = 14
  real, parameter :: frveg_expected = 1
  real, parameter :: xlai_expected = 0
  real, parameter :: epsf_expected = 0.96
  character, parameter :: albedo_fflag_expected = 'N'
  real, parameter :: albf_expected = 0.1
  character, parameter :: stmtype_expected =  'L'
  integer, parameter :: index_veggies_expected = 4
  real, parameter :: volrel_expected = 10
  real, parameter :: rmin_expected = 150
  real, parameter :: rcut_expected = 1000
  real, parameter :: wilt_expected = 0.08
  real, parameter :: vegheight_expected = 0.5
  real, parameter :: width_expected = 0.12
  character, parameter :: steady_expected = 'Y'
  real, parameter :: ci_expected = 300
  real, parameter :: co_expected = 330
  real, parameter :: coz_sfc_expected = 0
  real, parameter :: coz_air_expected = 0.08
  real, parameter :: NOBS_pTq_expected = 12
  real, parameter :: NOBS_wind_expected = 11
  real, parameter :: station_height_expected = 0.886
  real, parameter :: ugs_expected = 4.649
  real, parameter :: vgs_expected = 8.445
  real, parameter :: ps_expected(12) = (/967,24,5,949,24,5,900,23,3,850,24,8 /)
  real, parameter :: ts_expected(12) = (/700,11,7,677,9,11,628,4,6,612,2,7/)
  real, parameter :: dep_expected(12) = (/606,2,15,530,-7,17,460,-10,30,250,-40,30/)
  real, parameter :: dd0_expected(11) = (/180,7,0,185,10,1,225,35,3,240,25/)
  real, parameter :: ff0_expected(11) = (/5,225,15,7,215,15,9,230,30,14,240/)
  real, parameter :: zh_expected(11) = (/25,20,245,44,30,255,43,41,195,14,54/)


! mod_testing variable setup
  n = 1
  ntests = 28
  call initialize_tests(tests,ntests)
! end  mod_testing variable setup


! Set logical to control test execution
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
  f_control_test = .false.


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
    tests(n) = assert(splint_output == splint_expected, 'SPLINT')
    n = n + 1
    if (allocated(splint_arg3)) then
      deallocate(splint_arg3)
    end if
  end if

! 
! spline_test
!
  if (spline_test) then
    do i = 1,62
      spline_arg1(i) = 2*i
      spline_arg2(i) = 2*i
    end do
    spline_arg3 = 62
    spline_arg4 = 1.0
    spline_arg5 = 2.5
    if (.not. allocated(spline_output)) then
      allocate(spline_output(spline_arg3))
    end if
    spline_output=spline(spline_arg1, spline_arg2, spline_arg3, spline_arg4, spline_arg5)
    tests(n) = assert(all(spline_output == spline_expected), 'SPLINE')
    n = n + 1
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
! you_star_test
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

!
! f_control_test
!

  if (f_control_test) then
!    call start
    tests(n) = assert(iyr == iyr_expected, 'iyr')
    n = n + 1
    tests(n) = assert(imo == imo_expected, 'imo')
    n = n + 1
    tests(n) = assert(iday == iday_expected, 'iday')
    n = n + 1
    tests(n) = assert(tz == tz_expected, 'tz')
    n = n + 1
    tests(n) = assert(xlat == xlat_expected, 'xlat')
    n = n + 1
    tests(n) = assert(xlong == xlong_expected, 'xlong')
    n = n + 1
    tests(n) = assert(strtim == strtim_expected, 'strtim')
    n = n + 1
    tests(n) = assert(timend == timend_expected, 'timend')
    n = n + 1
    tests(n) = assert(outtt == outtt_expected, 'outtt')
    n = n + 1
    tests(n) = assert(slope == slope_expected, 'slope')
    n = n + 1
    tests(n) = assert(aspect == aspect_expected, 'aspect')
    n = n + 1
    tests(n) = assert(f == f_expected, 'f')
    n = n + 1
    tests(n) = assert(fsub == fsub_expected, 'f_sub')
    n = n + 1
    tests(n) = assert(wmax == wmax_expected, 'wmax')
    n = n + 1
    tests(n) = assert(btemp == btemp_expected, 'btemp')
    n = n + 1
    tests(n) = assert(tp == tp_expected, 'tp')
    n = n + 1
    tests(n) = assert(dual_ti == dual_ti_expected, 'dual_ti')
    n = n + 1
    tests(n) = assert(ti_a == ti_a_expected, 'ti_a')
    n = n + 1
    tests(n) = assert(ti_b == ti_b_expected, 'ti_b')
    n = n + 1
!    tests(n) = assert(albedo_gflag == albedo_gflag_expected, 'albedo_gflag')
!    n = n + 1
    tests(n) = assert(albg == albg_expected, 'albg')
    n = n + 1
    tests(n) = assert(epsi == epsi_expected, 'epsi')
    n = n + 1
!    tests(n) = assert(index_soils == index_soils_expected, 'index_soils')
!    n = n + 1
    tests(n) = assert(omega == omega_expected, 'omega')
    n = n + 1
    tests(n) = assert(zo == zo_expected, 'zo')
    n = n + 1
!    tests(n) = assert(obst_hgt == obst_hgt_expected, 'obst_hgt')
!    n = n + 1
!    tests(n) = assert(cloud_flag == cloud_flag_expected, 'cloud_flag')
!    n = n + 1
    tests(n) = assert(cld_fract == cld_fract_expected, 'cld_fract')
    n = n + 1
    tests(n) = assert(frveg == frveg_expected, 'frveg')
    n = n + 1
    tests(n) = assert(xlai == xlai_expected, 'xlai')
    n = n + 1
    tests(n) = assert(epsf == epsf_expected, 'epsf')
    n = n + 1
!    tests(n) = assert(albedo_fflag == albedo_fflag_expected, 'albedo_flag')
!    n = n + 1
    tests(n) = assert(albf == albf_expected, 'albf')
    n = n + 1
    tests(n) = assert(stmtype == stmtype_expected, 'stmtype')
    n = n + 1
!    tests(n) = assert(index_veggies == index_veggies_expected, 'index_veggies')
!    n = n + 1
    tests(n) = assert(volrel == volrel_expected, 'volrel')
    n = n + 1
    tests(n) = assert(rmin == rmin_expected, 'rmin')
    n = n + 1
    tests(n) = assert(rcut == rcut_expected, 'rcut')
    n = n + 1
    tests(n) = assert(wilt == wilt_expected, 'wilt')
    n = n + 1
    tests(n) = assert(vegheight == vegheight_expected, 'vegheight')
    n = n + 1
    tests(n) = assert(width == width_expected, 'width')
    n = n + 1
    tests(n) = assert(steady == steady_expected, 'steady')
    n = n + 1
    tests(n) = assert(ci == ci_expected, 'ci')
    n = n + 1
    tests(n) = assert(co == co_expected, 'co')
    n = n + 1
    tests(n) = assert(coz_sfc == coz_sfc_expected, 'coz_sfc')
    n = n + 1
    tests(n) = assert(coz_air == coz_air_expected, 'coz_air')
    n = n + 1
!    call snding
!    tests(n) = assert(NOBS_pTq == NOBS_pTq_expected, 'NOBS_pTq')
!    n = n + 1
!    tests(n) = assert(NOBS_wind == NOBS_wind_expected, 'NOBS_wind')
!    n = n + 1
!    tests(n) = assert(station_height == station_height_expected, 'station_height')
!    n = n + 1
!    tests(n) = assert(ugs == ugs_expected, 'ugs')
!    n = n + 1
!    tests(n) = assert(vgs == vgs_expected, 'vgs')
!    n = n + 1
!    tests(n) = assert(ps == ps_expected, 'ps')
!    n = n + 1
!    tests(n) = assert(ts == ts_expected, 'ts')
!    n = n + 1
!    tests(n) = assert(dep == dep_expected, 'dep')
!    n = n + 1
!    tests(n) = assert(dd0 == dd0_expected, 'dd0')
!    n = n + 1
!    tests(n) = assert(ff0 == ff0_expected, 'ff0')
!    n = n + 1
!    tests(n) = assert(zh == zh_expected, 'zh')
!    n = n + 1
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
