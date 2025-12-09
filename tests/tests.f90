program test_simsphere
  use, intrinsic :: ieee_arithmetic
  use simsphere_mod
  use mod_testing, only: assert,initialize_tests,report_tests
  implicit none

  ! mod_testing test setup

  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests

  ! end mod_testing test setup

  integer :: i
  
  logical, parameter :: splint_test = .true.
  logical, parameter :: spline_test = .true.
  logical, parameter :: transm_ftabs_test = .true.
  logical, parameter :: transm_ftabs_test2 = .true.
  logical, parameter :: transm_ftscatT_test = .true.
  logical, parameter :: transm_ftscatT_test2 = .true.
  logical, parameter :: transm_fbscatT_test = .true.
  logical, parameter :: transm_fbscatT_test2 = .true.
  logical, parameter :: advect_func_test = .true.
!  logical :: air_test
  logical, parameter :: cond_test = .true.
  logical, parameter :: stomfs_test = .true.
  logical, parameter :: stomrs_test_hi_temp = .true.
  logical, parameter :: stomrs_test_lo_temp = .true.
  logical, parameter :: stomrs_test_hi_psi = .true.
  logical, parameter :: stomrs_test_lo_psi = .true.
  logical, parameter :: stomc_test = .true.
  logical, parameter :: you_star_test = .true.
  logical, parameter :: r_ohms_test = .true.
  logical, parameter :: windf_test = .true.
  logical, parameter :: stab_test = .true.
  logical, parameter :: stabh_test = .true.
  logical, parameter :: fstabh_test = .true.
  logical, parameter :: fstabm_test = .true.
  logical, parameter :: restrn_test = .true.
  logical, parameter :: psgcal_test = .true.
  logical, parameter :: vegflx_test = .true.
  logical, parameter :: avr_test_init1 = .true.
!  logical, parameter :: avr_test_init_not1 = .true.
  logical, parameter :: ozone_test = .true.
  logical, parameter :: co2flx_test = .true.
  logical, parameter :: veghot_test = .true.
  logical, parameter :: hot_test = .true.
  logical, parameter :: albedo_test = .true.
  logical, parameter :: intpol_test = .true.
  logical, parameter :: prfile_test = .true.
  logical, parameter :: f_control_test = .false.

! Tolerance for comparisons
  real, parameter :: eq_tol = 1e-6

! splint_test variables
  integer, parameter :: splint_max_array = 50
  real, parameter :: splint_expected = 1.5
  real :: splint_arg1(splint_max_array), splint_arg2(splint_max_array)  
  real(kind=4), allocatable :: splint_arg3(:)
  integer :: splint_arg4, splint_arg5
  real :: splint_output

! spline_test variables
  integer, parameter :: spline_max_array = 62
  integer :: spline_arg3
  real, parameter :: spline_expected(spline_max_array) =              &
    (/-6.71245960E-35,1.34249192E-34,-4.69872138E-34,1.74523920E-33,  &
      -6.51108452E-33,2.42991007E-32,-9.06853161E-32,3.38442174E-31,  &
      -1.26308335E-30,4.71389109E-30,-1.75924808E-29,6.56560291E-29,  &
      -2.45031637E-28,9.14470538E-28,-3.41285032E-27,1.27369312E-26,  &
      -4.75348754E-26,1.77402566E-25,-6.62075378E-25,2.47089892E-24,  &
      -9.22152050E-24,3.44151835E-23,-1.28439215E-22,4.79341673E-22,  &
      -1.78892752E-21,6.67636859E-21,-2.49165461E-20,9.29898156E-20,  &
      -3.47042721E-19,1.29518110E-18,-4.83368161E-18,1.80395452E-17,  &
      -6.73245014E-17,2.51258454E-16,-9.37709307E-16,3.49957867E-15,  &
      -1.30606051E-14,4.87428430E-14,-1.81910761E-13,6.78900187E-13,  &
      -2.53369005E-12,9.45585971E-12,-3.52897503E-11,1.31703135E-10,  &
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
!  real, parameter :: aepsi_expected = 0.804384410
!  real, parameter :: aepsi_cloud_flag_expected = 1.02347386

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
  real, parameter :: avr_test_init1_expected = 300.5
!  real, parameter :: avr_test_init_not1_expected = 20.0
  real :: avr_arg1, avr_arg2
  real :: avr_arg3(4)

! co2flx_test variables
  real, parameter :: co2flx_ccan_expected = 1.77409637
  real, parameter :: co2flx_fco2_expected = 3.13184571

! veghot_test variables
  real, parameter :: veghot_heatv_expected = 1.5
  real :: veghot_arg1, veghot_arg2

! hot_test variables
  real, parameter :: hot_caseI_expected = 2.49819231
  real, parameter :: hot_caseII_expected = 1.62364423
  real, parameter :: hot_caseIII_expected = 0.999096155
  real :: hot_arg1, hot_arg2, hot_arg3, hot_arg4

! albedo_test variables
  real, parameter :: albedo_caseI_expected = 0.396643847
  real, parameter :: albedo_caseII_expected = 0.396643847
  real, parameter :: albedo_caseIII_expected = 0.396643847
  real, parameter :: albedo_caseIV_expected = 0.396643847
  real :: albedo_test_arg1

! intpol_test variables
  real, dimension(50), parameter :: intpol_ug_exp =  &
           (/9.18181801,8.36363602,7.54545498,6.72727299,5.90909100, &
             5.09090948,4.27272749,3.45454597,2.63636398,1.81818199, &
             1.00000000,0.181818962,-0.636363029,-1.45454502,-2.27272701, &
             -3.09090805,-3.90909004,-4.72727203,-5.54545403,-6.36363602, &
             0.00000000,0.00000000,0.00000000,0.00000000,0.00000000, &
             0.00000000,0.00000000,0.00000000,0.00000000,0.00000000, &
             0.00000000,0.00000000,0.00000000,0.00000000,0.00000000, &
             0.00000000,0.00000000,0.00000000,0.00000000,0.00000000, &
             0.00000000,0.00000000,0.00000000,0.00000000,0.00000000, &
             0.00000000,0.00000000,0.00000000,0.00000000,0.00000000/)
  real, dimension(50), parameter :: intpol_vg_exp =  &
           (/9.18181801,8.36363602,7.54545498,6.72727299,5.90909100, &
             5.09090948,4.27272749,3.45454597,2.63636398,1.81818199, &
             1.00000000,0.181818962,-0.636363029,-1.45454502,-2.27272701, &
             -3.09090805,-3.90909004,-4.72727203,-5.54545403,-6.36363602, &
             0.00000000,0.00000000,0.00000000,0.00000000,0.00000000, &
             0.00000000,0.00000000,0.00000000,0.00000000,0.00000000, &
             0.00000000,0.00000000,0.00000000,0.00000000,0.00000000, &
             0.00000000,0.00000000,0.00000000,0.00000000,0.00000000, &
             0.00000000,0.00000000,0.00000000,0.00000000,0.00000000, &
             0.00000000,0.00000000,0.00000000,0.00000000,0.00000000/)
  real, dimension(50), parameter :: intpol_ugd_exp =  &
            (/9.57142830,7.42857170,5.28571463,3.14285755,1.00000000, &
              1.00000000,1.00000000,1.00000000,1.00000000,1.00000000, &
              1.00000000,1.00000000,1.00000000,1.00000000,1.00000000, &
              1.00000000,1.00000000,1.00000000,1.00000000,1.00000000, &
              0.00000000,0.00000000,0.00000000,0.00000000,0.00000000, &
              0.00000000,0.00000000,0.00000000,0.00000000,0.00000000, &
              0.00000000,0.00000000,0.00000000,0.00000000,0.00000000, &
              0.00000000,0.00000000,0.00000000,0.00000000,0.00000000, &
              0.00000000,0.00000000,0.00000000,0.00000000,0.00000000, &
              0.00000000,0.00000000,0.00000000,0.00000000,0.00000000/)
  real, dimension(50), parameter :: intpol_vgd_exp =  &
            (/9.57142830,7.42857170,5.28571463,3.14285755,1.00000000, &
              1.00000000,1.00000000,1.00000000,1.00000000,1.00000000, &
              1.00000000,1.00000000,1.00000000,1.00000000,1.00000000, &
              1.00000000,1.00000000,1.00000000,1.00000000,1.00000000, &
              0.00000000,0.00000000,0.00000000,0.00000000,0.00000000, &
              0.00000000,0.00000000,0.00000000,0.00000000,0.00000000, &
              0.00000000,0.00000000,0.00000000,0.00000000,0.00000000, &
              0.00000000,0.00000000,0.00000000,0.00000000,0.00000000, &
              0.00000000,0.00000000,0.00000000,0.00000000,0.00000000, &
              0.00000000,0.00000000,0.00000000,0.00000000,0.00000000/)

! prfile_test variables
  real, dimension(51), parameter :: prfile_uf_exp = (/1.00000000,         &
              0.800000012,0.600000024,0.399999976,0.199999988,0.00000000, &
              0.200000003,0.400000006,0.600000024,0.800000012,1.00000000, &
              0.800000012,0.600000024,0.399999976,0.199999988,0.00000000, &
              0.200000003,0.400000006,0.600000024,0.800000012,1.00000000, &
              1.00000000,1.00000000,1.00000000,1.00000000,1.00000000,     &
              1.00000000,1.00000000,1.00000000,1.00000000,1.00000000,     &
              1.00000000,1.00000000,1.00000000,1.00000000,1.00000000,     &
              1.00000000,1.00000000,1.00000000,1.00000000,1.00000000,     &
              1.00000000,1.00000000,1.00000000,1.00000000,1.00000000,     &
              0.800000012,0.600000024,0.399999976,0.199999988,0.00000000/)
  real, dimension(51), parameter :: prfile_vf_exp = (/1.00000000,         &
              0.800000012,0.600000024,0.399999976,0.199999988,0.00000000, &
              0.200000003,0.400000006,0.600000024,0.800000012,1.00000000, &
              0.800000012,0.600000024,0.399999976,0.199999988,0.00000000, &
              0.200000003,0.400000006,0.600000024,0.800000012,1.00000000, &
              1.00000000,1.00000000,1.00000000,1.00000000,1.00000000,     &
              1.00000000,1.00000000,1.00000000,1.00000000,1.00000000,     &
              1.00000000,1.00000000,1.00000000,1.00000000,1.00000000,     &
              1.00000000,1.00000000,1.00000000,1.00000000,1.00000000,     &
              1.00000000,1.00000000,1.00000000,1.00000000,1.00000000,     &
              0.800000012,0.600000024,0.399999976,0.199999988,0.00000000/)
  real, dimension(51), parameter :: prfile_tf_exp = (/1.00000000,0.800000012, &
        0.600000024,0.399999976,0.199999988,0.00000000,0.00000000,0.00000000, &
        0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,    &
        0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,    &
        0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,    &
        0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,    &
        0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,    &
        0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,    &
        0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,    &
        0.00000000/)
  real, dimension(51), parameter :: prfile_qf_exp = (/1.00000000,0.800000012, &
        0.600000024,0.399999976,0.199999988,0.00000000,0.00000000,0.00000000, &
        0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,    &
        0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,    &
        0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,    &
        0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,    &
        0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,    &
        0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,    &
        0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,    &
        0.00000000/)
  real, parameter :: prfile_awind_exp = 1.41421354


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
!  character, parameter :: albedo_gflag_expected = 'F'
  real, parameter :: albg_expected = 0.1
  real, parameter :: epsi_expected = 0.96
!  integer, parameter :: index_soils_expected = 5
  real, parameter :: omega_expected = 3.13
  real, parameter :: zo_expected = 0.05
!  real, parameter :: obst_hgt_expected = 1
!  logical, parameter :: cloud_flag_expected = .true.
  real, parameter :: cld_fract_expected = 1.4
  real, parameter :: frveg_expected = 1
  real, parameter :: xlai_expected = 0
  real, parameter :: epsf_expected = 0.96
!  character, parameter :: albedo_fflag_expected = 'N'
  real, parameter :: albf_expected = 0.1
  character, parameter :: stmtype_expected =  'L'
!  integer, parameter :: index_veggies_expected = 4
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
!  real, parameter :: NOBS_pTq_expected = 12
!  real, parameter :: NOBS_wind_expected = 11
!  real, parameter :: station_height_expected = 0.886
!  real, parameter :: ugs_expected = 4.649
!  real, parameter :: vgs_expected = 8.445
!  real, parameter :: ps_expected(12) = (/967,24,5,949,24,5,900,23,3,850,24,8 /)
!  real, parameter :: ts_expected(12) = (/700,11,7,677,9,11,628,4,6,612,2,7/)
!  real, parameter :: dep_expected(12) = (/606,2,15,530,-7,17,460,-10,30,250,-40,30/)
!  real, parameter :: dd0_expected(11) = (/180,7,0,185,10,1,225,35,3,240,25/)
!  real, parameter :: ff0_expected(11) = (/5,225,15,7,215,15,9,230,30,14,240/)
!  real, parameter :: zh_expected(11) = (/25,20,245,44,30,255,43,41,195,14,54/)
  real, parameter :: ozone_flux_plant_expected = 581.332764
  real, parameter :: ozone_fglobal_expected = 10028.6299

! ieee_arithmetic setup -- choices: up, down, nearest, to zero
  if (ieee_support_rounding(IEEE_NEAREST)) then
    call ieee_set_rounding_mode(IEEE_NEAREST)
  end if


! mod_testing variable setup
  n = 1
  ntests = 48
  call initialize_tests(tests,ntests)
! end  mod_testing variable setup


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
    tests(n) = assert(eq(splint_output,splint_expected), 'SPLINT')
    n = n + 1
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
    tests(n) = assert(eq(                                                   &
       spline(spline_arg1,spline_arg2,spline_arg3,spline_arg4,spline_arg5), &
       spline_expected) .eqv. .true., 'SPLINE')
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
    tests(n) = assert(eq(ftabst(transm_ftabs_arg1),transm_ftabs_expected), 'ftabs() else')
    n = n + 1
  end if

!
! transm_ftabs_test2
!
! Test the "if" branch in ftabsT()

  if (transm_ftabs_test2) then
    call ftabsT_init
    transm_ftabs_arg1 = 11.0
    tests(n) = assert(eq(ftabst(transm_ftabs_arg1),transm_ftabs_expected2), 'ftabs() if')
    n = n + 1 
  end if

!
! transm_ftscatT_test
!
! Test the "else" branch in ftscatT()

  if (transm_ftscatT_test) then
    call ftscatT_init
    transm_ftscatT_arg1 = 2.75401473
    tests(n) = assert(eq(ftscatT(transm_ftscatT_arg1),transm_ftscatT_expected), 'ftscatT() else')
    n = n + 1
  end if

!
! transm_ftscatT_test2
!
! Test the "if" branch in ftscatT()

  if (transm_ftscatT_test2) then
    call ftscatT_init
    transm_ftscatT_arg1 = 11.0
    tests(n) = assert(eq(ftscatT(transm_ftscatT_arg1),transm_ftscatT_expected2), 'ftscatT() if')
    n = n + 1
  end if

!
! transm_fbscatT_test
!
! Test the "else" branch in fbscatT()

  if (transm_fbscatT_test) then
    call fbscatT_init
    transm_fbscatT_arg1 = 2.75401473
    tests(n) = assert(eq(fbscatT(transm_fbscatT_arg1),transm_fbscatT_expected), 'fbscatT() else')
    n = n + 1
  end if

!
! transm_fbscatT_test2
!
! Test the "if" branch in fbscatT()

  if (transm_fbscatT_test2) then
    call fbscatT_init
    transm_fbscatT_arg1 = 11.0
    tests(n) = assert(eq(fbscatT(transm_fbscatT_arg1),transm_fbscatT_expected2), 'fbscatT() if')
    n = n + 1
  end if

!
! advect_func_test
!
  if (advect_func_test) then
    call advect_init
    tests(n) = assert(eq(advect(),advect_test_expected), 'advect()')
    n = n + 1
  end if   

!
! cond_test
!
  if (cond_test) then
    call cond_init
    tests(n) = assert(eq(cond(),cond_test_expected),'cond()')
    n = n + 1
  end if

!
! stomfs_test
!
  if (stomfs_test) then
    call stomfs_init
    tests(n) = assert(eq(stomfs(sc, sol),stomfs_test_expected), 'stomfs()')
    n = n + 1
  end if

!
! stomrs_test_hi_temp
!
  if (stomrs_test_hi_temp) then
    call stomrs_init
    TF = MAXTEMP + 1.0
    tests(n) = assert(eq( &
               stomrs(ft,tf,rmin,mintemp,maxtemp,psisup,psiwc,b1,b2,psie,psice,fs), &
               stomrs_test_hi_temp_expected), 'stomrs_test_hi_temp_expected')
    n = n + 1
  end if

!
! stomrs_test_lo_temp
!
  if (stomrs_test_lo_temp) then
    call stomrs_init
    TF = MINTEMP - 1.0
    tests(n) = assert(eq( &
               stomrs(ft,tf,rmin,mintemp,maxtemp,psisup,psiwc,b1,b2,psie,psice,fs), &
               stomrs_test_lo_temp_expected), 'stomrs_test_lo_temp_expected')
    n = n + 1
  end if

!
! stomrs_test_hi_psi
!
  if (stomrs_test_hi_psi) then
    call stomrs_init
    TF = (MINTEMP + MAXTEMP)/2
    PSISUP = PSIWC + 1.0
    tests(n) = assert(eq( &
               stomrs(ft,tf,rmin,mintemp,maxtemp,psisup,psiwc,b1,b2,psie,psice,fs), &
               stomrs_test_hi_psi_expected), 'stomrs_test_hi_psi')
    n = n + 1
  end if

!
! stomrs_test_lo_psi
!
  if (stomrs_test_lo_psi) then
    call stomrs_init
    TF = (MINTEMP + MAXTEMP)/2
    PSISUP = PSIWC - 1.0
    tests(n) = assert(eq( &
               stomrs(ft,tf,rmin,mintemp,maxtemp,psisup,psiwc,b1,b2,psie,psice,fs), &
               stomrs_test_lo_psi_expected), 'stomrs_test_lo_psi')
    n = n + 1
  end if

!
! stomc_test
!

  if (stomc_test) then
    call stomc_init
    tests(n) = assert(eq(stomc(ft,rmin,b1,psice,fs),stomc_test_expected),'stomc_test')
    n = n + 1
  end if

!
! you_star_test
!

  if (you_star_test) then
    call you_star_init
    tests(n) = assert(eq( &
               you_star(you_star_arg1,you_star_arg2,you_star_arg3,you_star_arg4,you_star_arg5), &
               you_star_test_expected),'you_star_test')
    n = n + 1
  end if

!
! r_ohms_test
!

  if (r_ohms_test) then
    call r_ohms_init
    tests(n) = assert(eq( &
               r_ohms(r_ohms_arg1,r_ohms_arg2,r_ohms_arg3,r_ohms_arg4,r_ohms_arg5), &
               r_ohms_test_expected),'r_ohms_test')
    n = n + 1
  end if

!
! windf_test
!

  if (windf_test) then
    call windf_init
    tests(n) = assert(eq(windf(windf_arg1,windf_arg2,windf_arg3,windf_arg4,windf_arg5),windf_test_expected),'windf_test')
    n = n + 1
  end if

!
! stab_test
!

  if (stab_test) then
    call stab_init
    tests(n) = assert(eq(stab(stab_arg1,stab_arg2),stab_test_expected),'stab_test')
    n = n + 1
  end if

!
! stabh_test
!

  if (stabh_test) then
    ! stabh takes same argument as stab so we'll recycle initialization
    call stab_init
    tests(n) = assert(eq(stabh(stab_arg1,stab_arg2),stabh_test_expected),'stabh_test')
    n = n + 1
  end if

!
! fstabh_test
!

  if (fstabh_test) then
    call fstabh_init
    tests(n) = assert(eq(fstabh(fstabh_arg1,fstabh_arg2),fstabh_test_expected),'fstabh_test')
    n = n + 1
  end if

!
! fstabm_test
!

  if (fstabm_test) then
    ! fstabm takes the same arguments as fstabh so we'll recycle the initialization
    call fstabh_init
    tests(n) = assert(eq(fstabm(fstabh_arg1,fstabh_arg2),fstabm_test_expected,eq_tol),'fstabm_test')
    n = n + 1
  end if

!
! restrn_test
!

  if (restrn_test) then
    call restrn_init
    tests(n) = assert(eq(restrn(restrn_arg1,restrn_arg2,restrn_arg3,restrn_arg4),restrn_test_expected),'restrn()')
    n = n + 1
  end if

!
! psgcal_test
!

  if (psgcal_test) then
    call psgcal_init
    psig = psgcal(thmax,thv,cosbyb,psis)
    tests(n) = assert(eq(PSIG,psgcal_test_expected), 'psgcal_test')
    n = n + 1
  end if

! 
! vegflx_test
!

  if (vegflx_test) then
    call vegflx_init
    call vegflx(vegflx_arg1)
    tests(n) = assert(eq(QAF,vegflx_test_expected), 'vegflx_test')
    n = n + 1
  end if

!
! avr_test_init1
!

  if (avr_test_init1) then
    call avr_init
    do i=1,10
    call avr(avr_arg1,avr_arg2,avr_arg3)
    avr_arg1 = avr_arg1+1
    end do
!    write(6,*) 'avr after 10 passes: avr_arg1, avr_arg2, avr_arg3: ',avr_arg1, &
!      avr_arg2, avr_arg3
    tests(n) = assert(eq(avr_arg2,avr_test_init1_expected), 'avr_test_init1')
    n = n + 1
  end if

!
! avr_test_init_not1
!

!  if (avr_test_init_not1) then
!    call avr_init
!    call avr(avr_arg1,avr_arg2,avr_arg3)
!    call avr(avr_arg1,avr_arg2,avr_arg3)
!    call avr(avr_arg1,avr_arg2,avr_arg3)
!    call avr(avr_arg1,avr_arg2,avr_arg3)
!    tests(n) = assert(eq(avr_arg2,avr_test_init_not1_expected), 'avr_test_init_not1')
!    n = n + 1
!  end if

!
! ozone_test
!

  if (ozone_test) then
    call ozone_init
    call ozone
    tests(n) = assert(eq(flux_plant,ozone_flux_plant_expected), 'ozone flux_plant')
    n = n + 1
    tests(n) = assert(eq(fglobal,ozone_fglobal_expected), 'ozone fglobal')
    n = n + 1
  end if

!
! co2flx_test
!

  if (co2flx_test) then
    call co2flx_init
    call co2flx
    tests(n) = assert(eq(ccan,co2flx_ccan_expected), 'co2flx ccan')
    n = n + 1
    tests(n) = assert(eq(fco2,co2flx_fco2_expected), 'co2flx fco2')
    n = n + 1
  end if

!
! veghot_test
!

  if (veghot_test) then
    call veghot_init
    call veghot(veghot_arg1, veghot_arg2)
    tests(n) = assert(eq(veghot_arg2,veghot_heatv_expected), 'veghot heatv')
    n = n + 1
  end if

!
! hot_test
!

  if (hot_test) then
    call hot_init
    ! Case I: FRVEG == 1 AND RNET > 0
    FRVEG = 1.0
    RNET = 1.0
    call hot(hot_arg1,hot_arg2,hot_arg3,hot_arg4)
    tests(n) = assert(eq(heat, hot_caseI_expected), 'hot case I')
    n = n + 1
    ! Case II: 1 > FRVEG > 0 AND RNET > 0
    FRVEG = 0.5
    RNET = 1.0
    call hot(hot_arg1,hot_arg2,hot_arg3,hot_arg4)
    tests(n) = assert(eq(heat, hot_caseII_expected), 'hot case II')
    n = n + 1
    ! Case III: All others
    FRVEG = 1.5
    RNET = 1.0
    call hot(hot_arg1,hot_arg2,hot_arg3,hot_arg4)
    tests(n) = assert(eq(heat, hot_caseIII_expected), 'hot case III')
    n = n + 1
  end if

!
! albedo_test (4 cases)
!

  if (albedo_test) then
    call albedo_init

!  Case I
    ALBG = 0.0
    ALBF = 0.0
    call albedo(albedo_test_arg1)
    tests(n) = assert(eq(ALBDOE,albedo_caseI_expected), 'albedo case I')
    n = n + 1

!  Case II
    ALBG = 20.0
    ALBF = 0.0
    call albedo(albedo_test_arg1)
    tests(n) = assert(eq(ALBDOE,albedo_caseII_expected), 'albedo case II')
    n = n + 1
    
!  Case III
    ALBG = 0.0
    ALBF = 20.0
    call albedo(albedo_test_arg1)
    tests(n) = assert(eq(ALBDOE,albedo_caseIII_expected), 'albedo case III')
    n = n + 1

!  Case IV
    ALBG = 46.0
    ALBF = 71.0
    call albedo(albedo_test_arg1)
    tests(n) = assert(eq(ALBDOE,albedo_caseIV_expected), 'albedo case IV')
    n = n + 1
  end if

!
! intpol_test (4 array variables)
!
  if (intpol_test) then
    call intpol_init
    call intpol
    tests(n) = assert(eq(ug,intpol_ug_exp) .eqv. .true., 'Intpol UG(50)')
    n = n + 1
    tests(n) = assert(eq(vg,intpol_vg_exp) .eqv. .true., 'Intpol VG(50)')
    n = n + 1
    tests(n) = assert(eq(ugd,intpol_ugd_exp) .eqv. .true., 'Intpol UGD(50)')
    n = n + 1
    tests(n) = assert(eq(vgd,intpol_vgd_exp) .eqv. .true., 'Intpol VGD(50)')
    n = n + 1
  end if

!
! prfile_test (4 array variables and scalar variable)
!

  if (prfile_test) then
    call prfile_init
    call prfile
    tests(n) = assert(eq(u_fine,prfile_uf_exp) .eqv. .true., 'PRFILE u_fine')
    n = n + 1
    tests(n) = assert(eq(v_fine,prfile_vf_exp) .eqv. .true., 'PRFILE v_fine')
    n = n + 1
    tests(n) = assert(eq(t_fine,prfile_tf_exp) .eqv. .true., 'PRFILE t_fine')
    n = n + 1
    tests(n) = assert(eq(q_fine,prfile_qf_exp) .eqv. .true., 'PRFILE q_fine')
    n = n + 1
    tests(n) = assert(eq(awind,prfile_awind_exp), 'PRFILE awind')
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
    tests(n) = assert(eq(tz,tz_expected), 'tz')
    n = n + 1
    tests(n) = assert(eq(xlat,xlat_expected), 'xlat')
    n = n + 1
    tests(n) = assert(eq(xlong,xlong_expected), 'xlong')
    n = n + 1
    tests(n) = assert(eq(strtim,strtim_expected), 'strtim')
    n = n + 1
    tests(n) = assert(eq(timend,timend_expected), 'timend')
    n = n + 1
    tests(n) = assert(eq(outtt,outtt_expected), 'outtt')
    n = n + 1
    tests(n) = assert(eq(slope,slope_expected), 'slope')
    n = n + 1
    tests(n) = assert(eq(aspect,aspect_expected), 'aspect')
    n = n + 1
    tests(n) = assert(eq(f,f_expected), 'f')
    n = n + 1
    tests(n) = assert(eq(fsub,fsub_expected), 'f_sub')
    n = n + 1
    tests(n) = assert(eq(wmax,wmax_expected), 'wmax')
    n = n + 1
    tests(n) = assert(eq(btemp,btemp_expected), 'btemp')
    n = n + 1
    tests(n) = assert(eq(tp,tp_expected), 'tp')
    n = n + 1
    tests(n) = assert(dual_ti == dual_ti_expected, 'dual_ti')
    n = n + 1
    tests(n) = assert(eq(ti_a,ti_a_expected), 'ti_a')
    n = n + 1
    tests(n) = assert(eq(ti_b,ti_b_expected), 'ti_b')
    n = n + 1
!    tests(n) = assert(albedo_gflag == albedo_gflag_expected, 'albedo_gflag')
!    n = n + 1
    tests(n) = assert(eq(albg,albg_expected), 'albg')
    n = n + 1
    tests(n) = assert(eq(epsi,epsi_expected), 'epsi')
    n = n + 1
!    tests(n) = assert(index_soils == index_soils_expected, 'index_soils')
!    n = n + 1
    tests(n) = assert(eq(omega,omega_expected), 'omega')
    n = n + 1
    tests(n) = assert(eq(zo,zo_expected), 'zo')
    n = n + 1
!    tests(n) = assert(obst_hgt == obst_hgt_expected, 'obst_hgt')
!    n = n + 1
!    tests(n) = assert(cloud_flag == cloud_flag_expected, 'cloud_flag')
!    n = n + 1
    tests(n) = assert(cld_fract == cld_fract_expected, 'cld_fract')
    n = n + 1
    tests(n) = assert(eq(frveg,frveg_expected), 'frveg')
    n = n + 1
    tests(n) = assert(eq(xlai,xlai_expected), 'xlai')
    n = n + 1
    tests(n) = assert(eq(epsf,epsf_expected), 'epsf')
    n = n + 1
!    tests(n) = assert(albedo_fflag == albedo_fflag_expected, 'albedo_flag')
!    n = n + 1
    tests(n) = assert(eq(albf,albf_expected), 'albf')
    n = n + 1
    tests(n) = assert(stmtype == stmtype_expected, 'stmtype')
    n = n + 1
!    tests(n) = assert(index_veggies == index_veggies_expected, 'index_veggies')
!    n = n + 1
    tests(n) = assert(eq(volrel,volrel_expected), 'volrel')
    n = n + 1
    tests(n) = assert(eq(rmin,rmin_expected), 'rmin')
    n = n + 1
    tests(n) = assert(eq(rcut,rcut_expected), 'rcut')
    n = n + 1
    tests(n) = assert(eq(wilt,wilt_expected), 'wilt')
    n = n + 1
    tests(n) = assert(eq(vegheight,vegheight_expected), 'vegheight')
    n = n + 1
    tests(n) = assert(eq(width,width_expected), 'width')
    n = n + 1
    tests(n) = assert(steady == steady_expected, 'steady')
    n = n + 1
    tests(n) = assert(eq(ci,ci_expected), 'ci')
    n = n + 1
    tests(n) = assert(eq(co,co_expected), 'co')
    n = n + 1
    tests(n) = assert(eq(coz_sfc,coz_sfc_expected), 'coz_sfc')
    n = n + 1
    tests(n) = assert(eq(coz_air,coz_air_expected), 'coz_air')
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
    avr_arg1 = 293.0
    do i = 1,4
      avr_arg3(i) = 1.0
    enddo
    return
  end subroutine avr_init

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

  subroutine co2flx_init
    USTAR = 1.0
    UTEN = 2.0
    UAF = 1.0
    XLAI = 1.0
    RZASCR = 2.0
    RAF = 1.0
    RST = 1.0
    CO = 2.0
    CI = 1.0
    FRVEG = 0.5
    return
  end subroutine co2flx_init

  subroutine veghot_init
    TF = 1.0
    TAF = 1.0
    CHF = 1.0
    CHG = 1.0
    HF = 1.0
    TT(2) = 1.0
    Z(2) = 1.0
    LAMBDA = 1.0
    RNETG = 3.0
    XLEG = 1.0
    veghot_arg1 = 1.0
    veghot_arg2 = 1.0
  end subroutine veghot_init

  subroutine hot_init
    hot_arg2 = 4.0
    hot_arg3 = 2.0
    hot_arg4 = 1.0
    LAMBDA = 1.0
    ATEMP = 2.0
    TT(2) = 1.0
    Z(2) = 1.0
    TF = 1.0
    TAF = 1.0
    CHF = 1.0
    CHG = 1.0
    HF = 1.0
    RNETG = 3.0
    XLEG = 1.0
    GBL_sum = 1.0
    return
  end subroutine hot_init

  subroutine albedo_init
    albedo_test_arg1 = -0.510662317
    XLAI = 1.0
    FRVEG = 1.0
    WGG = 100.0
    WMAX = 1230.0
    return
  end subroutine albedo_init

  subroutine intpol_init
    NTRP = 10
    VGS = 10.0
    UGS = 10.0
    VD = 1.0
    UD = 1.0
    VGD(NTRP) = 1.0
    UGD(NTRP) = 1.0
    return
  end subroutine intpol_init

  subroutine prfile_init
    integer :: i

    call advect_init
    call intpol_init

    ud = 0.0
    vd = 0.0
    ud(1) = 1.0
    vd(1) = 1.0
    ud(3) = 1.0
    vd(3) = 1.0
    do i = 5, NTRP
      ud(i) = 1.0
      vd(i) = 1.0
    end do
    td(1) = 1.0
    qd(1) = 1.0
    return
  end subroutine prfile_init

end program test_simsphere
