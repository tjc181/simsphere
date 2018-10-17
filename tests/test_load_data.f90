program test_load_data
  use simsphere_mod, only: t_met, t_wind, t_soil, t_veg, t_temp, t_humid,   &
                           t_timeloc, init_json, load_config, destroy_json, eq
  use mod_testing, only: assert, initialize_tests, report_tests
  use json_module
  implicit none

  type(t_met) :: met
  type(t_timeloc) :: timeloc
  type(t_wind) :: wind
  type(t_soil) :: soil
  type(t_veg) :: veg
  type(t_temp) :: temp
  type(t_humid) :: humidity
  type(json_file) :: cfg_json
  character(len=:), allocatable :: cfg_file

  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests, i

  ! eq() doesn't support derived types, we'll use 'junk' as a temporary
  ! for comparisons
  real :: junk

  ! Expected values

  ! Initialize mod_testing
  n = 1
  ntests = 7
  call initialize_tests(tests,ntests)

  ! Initialize JSON, data structures
  cfg_file = 'i_model.json'

  ! Initialize data structures
  call init_json(cfg_file, cfg_json)
  call load_config(cfg_json, met, timeloc, veg, wind, soil, temp, humidity)
  call destroy_json(cfg_json)

  tests(n) = assert(wind%num_obs == 11, 'nobs_wind == 11')
  n = n + 1
  tests(n) = assert(humidity%num_obs == 12, 'nobs_ptq == 12')
  n = n + 1
  junk = met%omega
  tests(n) = assert(eq(junk,3.13), 'omega == 3.13')
  n = n + 1
  junk = met%zo
  tests(n) = assert(eq(junk,0.05), 'zo == 0.05')
  n = n + 1
  junk = met%obst_hgt
  tests(n) = assert(eq(junk,1.0), 'obst_height == 1.0')
  n = n + 1
  tests(n) = assert(met%cloud_flag .eqv. .true., 'cloud_flag eqv true')
  n = n + 1
  junk = met%cld_fract
  tests(n) = assert(eq(junk,14.0), 'cld_fract == 14.0')
  n = n + 1


  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1

end program test_load_data
