program test_snding
  use simsphere_mod, only: deltaz, ts, gm, ntrp, atemp, tdif_s, aptemp, zi,   &
                           tscren, oshum, ahum, ps1, o_pot_tmp, tdif_50, ugs, &
                           vgs, f_control, t_met, t_wind, t_soil, t_veg,      &
                           t_temp, t_humid, t_timeloc, init_json, load_config,&
                           destroy_json, eq
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

  real, dimension(50) :: arg1
  real :: arg2

  ! Expected values
  real, parameter :: atemp_exp = 297.149994
  real, parameter :: aptemp_exp = 300.508942
  real, parameter :: o_pot_tmp_exp = 300.508942
  real, parameter :: tscren_exp = 297.149994
  real, parameter :: oshum_exp = 1.93754267E-02
  real, parameter :: ahum_exp = 1.42002506E-02
  real, parameter :: arg2_exp = 1.42002506E-02
  real, parameter :: ps1_exp = 967.0
  real, parameter :: tdif_50_exp = 3.35894775
  real, parameter :: tdif_s_exp = 2.85894775
  real, parameter :: deltaz_exp = 250.0
  real, parameter,dimension(50) :: gm_exp = (/                              &
           9.88652930E-03,7.76793063E-03,1.22398427E-02,                    &
           2.04411335E-03,2.83346325E-03,1.85281364E-03,2.10259561E-04,     &
           1.12541160E-02,1.51908118E-03,8.60635564E-03,4.06347681E-03,0.0, &
           0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0, &
           0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0, &
           0.0,0.0,0.0,0.0,0.0,0.0/)

  ! Initialize mod_testing
  n = 1
  ntests = 12
  call initialize_tests(tests,ntests)

  ! Initialize JSON, data structures
  cfg_file = 'i_model.json'

  call init_json(cfg_file, cfg_json)
  call load_config(cfg_json, met, timeloc, veg, wind, soil, temp, humidity)
  call destroy_json(cfg_json)

  ! Initialize
!  call start

  call snding_init
  call snding(arg1,arg2, temp, humidity, timeloc, wind)

  tests(n) = assert(eq(atemp,atemp_exp), 'atemp')
  n = n + 1
  tests(n) = assert(eq(tscren,tscren_exp), 'tscren')
  n = n + 1
  tests(n) = assert(eq(oshum,oshum_exp), 'oshum')
  n = n + 1
  tests(n) = assert(eq(ahum,ahum_exp), 'ahum')
  n = n + 1
  tests(n) = assert(eq(arg2,arg2_exp), 'old_ahum')
  n = n + 1
  tests(n) = assert(eq(ps1,ps1_exp), 'ps1')
  n = n + 1
  tests(n) = assert(eq(aptemp,aptemp_exp), 'aptemp')
  n = n + 1
  tests(n) = assert(eq(o_pot_tmp,o_pot_tmp_exp), 'o_pot_tmp')
  n = n + 1
  tests(n) = assert(eq(tdif_50,tdif_50_exp), 'tdif_50')
  n = n + 1
  tests(n) = assert(eq(tdif_s,tdif_s_exp), 'tdif_s')
  n = n + 1
  tests(n) = assert(eq(deltaz,deltaz_exp), 'deltaz')
  n = n + 1
  tests(n) = assert(eq(gm,gm_exp), 'gm')
  n = n + 1


  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1

contains
  subroutine snding_init
    arg1 = 1.0
    arg2 = 0.0
    ntrp = 0
    deltaz = 0.0
    aptemp = 0.0
    atemp = 0.0
    zi = 0.0
    gm = 0.0
    ugs = 0.0
    vgs = 0.0
    return
  end subroutine snding_init 

end program test_snding
