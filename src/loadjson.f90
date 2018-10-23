program loadjson
  use simsphere_mod, only: t_met, t_timeloc, t_veg, t_wind, t_soil, t_temp,  &
                           t_humid, load_config
  use, intrinsic :: iso_fortran_env, only: error_unit

  type(t_met) :: met
  type(t_timeloc) :: timeloc
  type(t_veg) :: veg
  type(t_wind) :: wind
  type(t_soil) :: soil
  type(t_temp) :: temp
  type(t_humid) :: humidity

  character(len=:), allocatable :: cfg_file

  if (.not. allocated(cfg_file)) then
    allocate(character(len=12) :: cfg_file)
    cfg_file = 'i_model.json'
  end if

  call load_config(cfg_file, met, timeloc, veg, wind, soil, temp, humidity)
  
  deallocate(cfg_file)

end program loadjson
