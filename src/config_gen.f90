program sim_input_gen
  use,intrinsic :: iso_fortran_env, only: wp => real64
  use json_module
  implicit none

  type(json_core) :: json
  type(json_value), pointer :: p, inp, met, soil, veg, wind, time
  type(json_value), pointer :: temp_snd, wind_snd

  ! initialize the class
  call json%initialize(compact_reals=.true., real_format='*')

  ! initialize the structure:
  call json%create_object(p,'')

  ! add an "inputs" object to the structure:
  call json%create_object(inp,'inputs')
  call json%add(p, inp) !add it to the root

  ! Uncategorized inputs
  call json%add(inp, 'nobs_ptq', 12)
  call json%add(inp, 'nobs_wind', 11)

  ! Meteorological inputs
  call json%create_object(met, 'meteorological')
  call json%add(inp, met)
  call json%add(met, 'omega', 3.13_wp)
  call json%add(met, 'zo', 0.05_wp)
  call json%add(met, 'obst_hgt', 1)
  call json%add(met, 'cloud_flag', .false.)
  call json%add(met, 'cld_fract', 0.14)

  ! Soil inputs
  call json%create_object(soil,'soil')
  call json%add(inp, soil)
  call json%add(soil, 'f', 0.5_wp)
  call json%add(soil, 'fsub', 0.75_wp)
  call json%add(soil, 'wmax', 0.34_wp)
  call json%add(soil, 'btemp', 24.63_wp)
  call json%add(soil, 'tp', 13)
  call json%add(soil, 'dual_ti', 'Y')
  call json%add(soil, 'ti_a', 12.0_wp)
  call json%add(soil, 'ti_b', 12.0_wp)
  call json%add(soil, 'albg', 0.1_wp)
  call json%add(soil, 'epsi', 0.96_wp)
  call json%add(soil, 'index_soils', 1)
  call json%add(soil, 'albedo_gflag', 'F')

  ! Time and location inputs
  call json%create_object(time, 'time_location')
  call json%add(inp, time)
  call json%add(time, 'year', 89)
  call json%add(time, 'month', 8)
  call json%add(time, 'day', 4)
  call json%add(time, 'tz', 6)
  call json%add(time, 'xlat', 39.25_wp)
  call json%add(time, 'xlong', 96.34_wp)
  call json%add(time, 'strtim', 0530)
  call json%add(time, 'timend', 2330)
  call json%add(time, 'outtt', 30)
  call json%add(time, 'slope', 0)
  call json%add(time, 'aspect', 0)
  call json%add(time, 'station_height', 0.886_wp)

  ! Vegetation inputs
  call json%create_object(veg, 'vegetation')
  call json%add(inp, veg)
  call json%add(veg, 'frveg', 0)
  call json%add(veg, 'xlai', 0)
  call json%add(veg, 'epsf', 0.96_wp)
  call json%add(veg, 'albedo_fflag', 'N')
  call json%add(veg, 'albf', 0.1_wp)
  call json%add(veg, 'stmtype', 'L')
  call json%add(veg, 'index_veggies', 4)
  call json%add(veg, 'volrel', 10)
  call json%add(veg, 'rmin', 150)
  call json%add(veg, 'rcut', 1000)
  call json%add(veg, 'wilt', 0.08_wp)
  call json%add(veg, 'vegheight', 0.5_wp)
  call json%add(veg, 'width', 0.12_wp)
  call json%add(veg, 'steady', 'Y')
  call json%add(veg, 'ci', 300)
  call json%add(veg, 'co', 330)
  call json%add(veg, 'coz_sfc', 0)
  call json%add(veg, 'coz_air', 0.08_wp)

  ! Wind inputs
  call json%create_object(wind, 'wind')
  call json%add(inp,wind)
  call json%add(wind, 'ugs', 4.649_wp)
  call json%add(wind, 'vgs', 8.445_wp)

  ! Temperature sounding
  call json%create_object(temp_snd, 'temp_sounding')
  call json%add(inp,temp_snd)
  call json%add(temp_snd, 'ps', [967,949,900,850,700,677,628,612,606,530,460,250])
  call json%add(temp_snd, 'ts', [24,24,23,24,11,9,4,2,2,-7,-10,-40])
  call json%add(temp_snd, 'dep', [5,5,3,8,7,11,6,7,15,17,30,30])

  ! Wind sounding
  call json%create_object(wind_snd, 'wind_sounding')
  call json%add(inp, wind_snd)
  call json%add(wind_snd, 'dd0', [180,185,225,240,225,215,230,240,245,255,195])
  call json%add(wind_snd, 'ff0', [7,10,35,25,15,15,30,25,44,43,14])
  call json%add(wind_snd, 'zh', [0,1,3,5,7,9,14,20,30,41,54])
  nullify(inp)  !don't need this anymore

  ! write the file:
  call json%print(p,'i_model.json')

  !cleanup:
  call json%destroy(p)
  if (json%failed()) stop 1

end program sim_input_gen
