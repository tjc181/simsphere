program test_load_data
  use simsphere_mod, only: t_met, t_wind, t_soil, t_veg, t_temp, t_windsnd,   &
                           t_timeloc, load_config, eq
  use mod_testing, only: assert, initialize_tests, report_tests
  use iso_fortran_env, only: real32, real64
  implicit none

  type(t_met) :: met
  type(t_timeloc) :: timeloc
  type(t_wind) :: wind
  type(t_soil) :: soil
  type(t_veg) :: veg
  type(t_temp) :: temp
  type(t_windsnd) :: windsnd
  character(len=:), allocatable :: cfg_file

  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests

  ! Expected values

  ! Initialize mod_testing
  n = 1
  ntests = 16
  call initialize_tests(tests,ntests)

  ! Initialize JSON, data structures
  cfg_file = 'i_model.json'

  ! Initialize data structures
  call load_config(cfg_file, met, timeloc, veg, wind, soil, temp, windsnd)

  tests(n) = assert(wind%num_obs == 11, 'nobs_wind == 11')
  n = n + 1
  tests(n) = assert(windsnd%num_obs == 12, 'nobs_ptq == 12')
  n = n + 1
  tests(n) = assert(eq(met%omega,3.13_real64), 'omega == 3.13')
  n = n + 1
  tests(n) = assert(eq(met%zo,0.05_real64), 'zo == 0.05')
  n = n + 1
  tests(n) = assert(eq(met%obst_hgt,1.0_real64), 'obst_height == 1.0')
  n = n + 1
  tests(n) = assert(met%cloud_flag .eqv. .false., 'cloud_flag eqv false')
  n = n + 1
  tests(n) = assert(eq(met%cld_fract,0.14_real64), 'cld_fract == 0.14')
  n = n + 1
  tests(n) = assert(eq(soil%f,0.5_real64), 'f == 0.5')
  n = n + 1
  tests(n) = assert(eq(soil%fsub,0.75_real64), 'fsub == 0.75')
  n = n + 1
  tests(n) = assert(eq(soil%wmax,0.34_real64), 'wmax == 0.34')
  n = n + 1
  tests(n) = assert(eq(soil%btemp,24.63_real64), 'btemp == 24.63')
  n = n + 1

  tests(n) = assert(timeloc%year == 89, 'year == 89')
  n = n + 1
  tests(n) = assert(eq(timeloc%strtim,530.0_real64), 'strtim == 530')
  n = n + 1

  tests(n) = assert(eq(veg%frveg,0.0_real64), 'frveg == 0')
  n = n + 1
  tests(n) = assert(veg%steady == 'Y', 'steady == Y')
  n = n + 1
  tests(n) = assert(eq(wind%ugs,4.649_real64), 'ugs = 4.649')
  n = n + 1


  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1

end program test_load_data
