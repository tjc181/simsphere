program loadjson
  use config, only: t_met, t_timeloc, t_veg, t_wind, t_soil, t_temp, t_humid
  use json_module
  use, intrinsic :: iso_fortran_env, only: error_unit

  type(json_file) :: json
  type(json_value), pointer :: p
  type(t_met) :: met
  type(t_timeloc) :: timeloc
  type(t_veg) :: veg
  type(t_wind) :: wind
  type(t_soil) :: soil
  type(t_temp) :: temp
  type(t_humid) :: humidity
  logical :: found, namelist_style
  integer :: error_cnt

  real :: buf

  found = .false.
  namelist_style = .true.

  error_cnt = 0
  call json%initialize(compact_reals=.true.)
  if (json%failed()) then
    call json%print_error_message(error_unit)
    error_cnt = error_cnt + 1
  end if

  call json%load_file(filename = './i_model.json')
  if (json%failed()) then
    call json%print_error_message(error_unit)
    error_cnt = error_cnt + 1
  end if

  ! Load data structures: met, veg, wind, soil, temp_snd, humid_snd
  call json%get('inputs.meteorological.omega',met%omega,found)
  if (.not. found) stop 1
  call json%get('inputs.meteorological.zo',met%zo,found)
  if (.not. found) stop 1
  call json%get('inputs.meteorological.obst_hgt',met%obst_hgt,found)
  if (.not. found) stop 1
  call json%get('inputs.meteorological.cld_fract',met%cld_fract,found)
  if (.not. found) stop 1
  call json%get('inputs.meteorological.cloud_flag',met%cloud_flag,found)
  if (.not. found) stop 1

  call json%get('inputs.time_location.iyr',timeloc%iyr, found)
  if (.not. found) stop 1
  call json%get('inputs.time_location.imo',timeloc%imo, found)
  if (.not. found) stop 1
  call json%get('inputs.time_location.iday',timeloc%iday, found)
  if (.not. found) stop 1
  call json%get('inputs.time_location.tz',timeloc%tz, found)
  if (.not. found) stop 1
  call json%get('inputs.time_location.xlat',timeloc%xlat, found)
  if (.not. found) stop 1
  call json%get('inputs.time_location.xlong',timeloc%xlong, found)
  if (.not. found) stop 1
  call json%get('inputs.time_location.strtim',timeloc%strtim, found)
  if (.not. found) stop 1
  call json%get('inputs.time_location.timend',timeloc%timend, found)
  if (.not. found) stop 1
  call json%get('inputs.time_location.outtt',timeloc%outtt, found)
  if (.not. found) stop 1
  call json%get('inputs.time_location.slope',timeloc%slope, found)
  if (.not. found) stop 1
  call json%get('inputs.time_location.aspect',timeloc%aspect, found)
  if (.not. found) stop 1
  call json%get('inputs.time_location.station_height',timeloc%station_height, found)
  if (.not. found) stop 1

  call json%get('inputs.vegetation.frveg',veg%frveg, found)
  if (.not. found) stop 1
  call json%get('inputs.vegetation.xlai',veg%xlai, found)
  if (.not. found) stop 1
  call json%get('inputs.vegetation.epsf',veg%epsf, found)
  if (.not. found) stop 1
  call json%get('inputs.vegetation.albf',veg%albf, found)
  if (.not. found) stop 1
  call json%get('inputs.vegetation.volrel',veg%volrel, found)
  if (.not. found) stop 1
  call json%get('inputs.vegetation.rmin',veg%rmin, found)
  if (.not. found) stop 1
  call json%get('inputs.vegetation.rcut',veg%rcut, found)
  if (.not. found) stop 1
  call json%get('inputs.vegetation.wilt',veg%wilt, found)
  if (.not. found) stop 1
  call json%get('inputs.vegetation.vegheight',veg%vegheight, found)
  if (.not. found) stop 1
  call json%get('inputs.vegetation.width',veg%width, found)
  if (.not. found) stop 1
  call json%get('inputs.vegetation.ci',veg%ci, found)
  if (.not. found) stop 1
  call json%get('inputs.vegetation.co',veg%co, found)
  if (.not. found) stop 1
  call json%get('inputs.vegetation.coz_sfc',veg%coz_sfc, found)
  if (.not. found) stop 1
  call json%get('inputs.vegetation.coz_air',veg%coz_air, found)
  if (.not. found) stop 1
  call json%get('inputs.vegetation.albedo_fflag',veg%albedo_fflag, found)
  if (.not. found) stop 1
  call json%get('inputs.vegetation.stmtype',veg%stmtype, found)
  if (.not. found) stop 1
  call json%get('inputs.vegetation.steady',veg%steady, found)
  if (.not. found) stop 1
  call json%get('inputs.vegetation.index_veggies',veg%index_veggies, found)
  if (.not. found) stop 1

  call json%get('inputs.wind.ugs',wind%ugs, found)
  if (.not. found) stop 1
  call json%get('inputs.wind.vgs',wind%vgs, found)
  if (.not. found) stop 1

  call json%get('inputs.soil.f',soil%f, found)
  if (.not. found) stop 1
  call json%get('inputs.soil.fsub',soil%fsub, found)
  if (.not. found) stop 1
  call json%get('inputs.soil.wmax',soil%wmax, found)
  if (.not. found) stop 1
  call json%get('inputs.soil.btemp',soil%btemp, found)
  if (.not. found) stop 1
  call json%get('inputs.soil.tp',soil%tp, found)
  if (.not. found) stop 1
  call json%get('inputs.soil.albg',soil%albg, found)
  if (.not. found) stop 1
  call json%get('inputs.soil.epsi',soil%epsi, found)
  if (.not. found) stop 1
  call json%get('inputs.soil.ti_a',soil%ti_a, found)
  if (.not. found) stop 1
  call json%get('inputs.soil.ti_b',soil%ti_b, found)
  if (.not. found) stop 1
  call json%get('inputs.soil.dual_ti',soil%dual_ti, found)
  if (.not. found) stop 1
  call json%get('inputs.soil.albedo_gflag',soil%albedo_gflag, found)
  if (.not. found) stop 1
  call json%get('inputs.soil.index_soils',soil%index_soils, found)
  if (.not. found) stop 1

  call json%get('inputs.temp_sounding.ps',temp%ps, found)
  if (.not. found) stop 1
  call json%get('inputs.temp_sounding.ts',temp%ts, found)
  if (.not. found) stop 1
  call json%get('inputs.temp_sounding.dep',temp%dep, found)
  if (.not. found) stop 1

  call json%get('inputs.humidity_sounding.dd0', humidity%dd0, found)
  if (.not. found) stop 1
  call json%get('inputs.humidity_sounding.ff0', humidity%ff0, found)
  if (.not. found) stop 1
  call json%get('inputs.humidity_sounding.zh', humidity%zh, found)
  if (.not. found) stop 1

  
  call json%destroy()
  if (json%failed()) stop 1

end program loadjson
