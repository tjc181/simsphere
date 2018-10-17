!subroutine  START (Obst_Hgt,dual_regime,zo_patch)
! dual_regime and zo_patch are not currently used.  Comment at end of this
! file suggests planned data read checks, not currently implemented.
subroutine  START (Obst_Hgt, temp, humidity, timeloc, wind)
  use simsphere_mod
  use json_module
  implicit none

!  real(kind=4) :: Obst_Hgt, zo_patch
  real(kind=4) :: Obst_Hgt
! logical :: zo_flag, ob_flag, dual_regime
  logical :: zo_flag, ob_flag

  character :: Class
  character(len=1) :: ALBEDO_GFLAG, ALBEDO_FFLAG ! For Interface
  character(len=15) :: soiltype
  character(len=30) :: planttype
  integer(kind=1) :: num_soils, num_of_veggies, index_soils, index_veggies
  integer :: i

  type(json_file) :: cfg_json
  type(t_met) :: met
  type(t_timeloc) :: timeloc
  type(t_veg) :: veg
  type(t_wind) :: wind
  type(t_soil) :: soil
  type(t_temp) :: temp
  type(t_humid) :: humidity

  character(len=:), allocatable :: cfg_file

  if (.not. allocated(cfg_file)) then
    allocate(character(len=12) :: cfg_file )
    cfg_file = 'i_model.json'
  end if

  call init_json(cfg_file, cfg_json)

  call load_config(cfg_json, met, timeloc, veg, wind, soil, temp, humidity)

  deallocate(cfg_file)

  call destroy_json(cfg_json)

!        INCLUDE 'modvars.h'

  zo_flag = .false.
  ob_flag = .false.
  Class = 'U'

! ** Take input from JSON file (above), assign existing variables values from 
! data structures

! **  This subroutine reads in the control variables from the input
! **  file.

! **  Read stuff for main program.

  iyr = timeloc%year
  imo = timeloc%month
  iday = timeloc%day
  tz = timeloc%tz
  xlat = timeloc%xlat
  xlong = timeloc%xlong
  strtim = timeloc%strtim
  timend = timeloc%timend
  outtt = timeloc%outtt
  slope = timeloc%slope
  aspect = timeloc%aspect

  f = soil%f
  fsub = soil%fsub
  wmax = soil%wmax
  btemp = soil%btemp
  tp = soil%tp
  dual_ti = soil%dual_ti
  ti_a = soil%ti_a
  ti_b = soil%ti_b
  albedo_gflag = soil%albedo_gflag
  albg = soil%albg
  epsi = soil%epsi
  index_soils = soil%index_soils

  omega = met%omega
  zo = met%zo
  obst_hgt = met%obst_hgt
  cloud_flag = met%cloud_flag
  cld_fract = met%cld_fract

  frveg = veg%frveg
  xlai = veg%xlai
  epsf = veg%epsf
  albedo_fflag = veg%albedo_fflag
  albf = veg%albf
  stmtype = veg%stmtype
  index_veggies = veg%index_veggies
  volrel = veg%volrel
  rmin = veg%rmin
  rcut = veg%rcut
  wilt = veg%wilt
  vegheight = veg%vegheight
  width = veg%width
  steady = veg%steady
  ci = veg%ci
  co = veg%co
  coz_sfc = veg%coz_sfc
  coz_air = veg%coz_air

! Include the vegetation and soils databases in the calculations.
!

  if(eq(xlai,0.0)) xlai = 1.0
  xlef = 1

  If (gt(FRVEG,0.0) .and. stmtype == 'L') then
    open (1, file = f_soil_lut) ! Begin with opening the file

    Read (1, *) num_soils             ! Required for Interface

    do i = 1, index_soils
      Read (1,*) soiltype, rks, cosbyb, thmax, psis ! Read
    end do

    close (unit = 1)

    open (1, file = f_veg_lut) ! Open Veg File

    Read (1, *) num_of_veggies    ! Required for Interface

    do i = 1, index_veggies
      Read (1,*) planttype, rmin, mintemp, maxtemp, beta, b1, b2,       &
                 psice, sc, rcut, zp, frhgt, frzp,                      &
                 rkocap, rccap, rzcap, volini, zstini   ! Read
    end do

    close (unit = 1)

    mintemp = mintemp + 273
    maxtemp = maxtemp + 273

  end if


! Make conversions

  Btemp = btemp + Celsius_to_Kelvin
  CO = CO*1E-6
  CI = CI*1E-6


! The relevant data for rough.for  08/04/92

  If (eq(frveg,0.0)) Class = 'B'
  If (eq(frveg,100.0)) Class = 'V'
  If (lt(frveg,100.0) .or. gt(frveg,0.0)) Class = 'P'
     
  If (.not. eq(ZO,0.0)) zo_flag = .true.
  If (.not. eq(Obst_Hgt,0.0)) ob_flag = .true.

! End data

! Call the rountine Rough

!        Call RoughCk (class, zo_flag, ob_flag, dual_regime,
!     /                      zo_patch, Obst_Hgt)

  return
end
