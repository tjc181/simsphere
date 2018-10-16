module config_mod
  use, intrinsic :: iso_fortran_env, only: wp => real64, error_unit
  use json_module
  implicit none

  private
  public :: init_json, destroy_json, load_config
  public :: t_met, t_veg, t_wind, t_soil, t_temp, t_humid, t_timeloc

! config reads data from the model configuration file and initializes variables
! to default values.  Data are stored in a JSON file.  The file name is stored
! in variable config_json.

! Provides public function: get_var which wraps calls to json%get.  get_var is
! a generic interface to a group of functions handling different data types:
! real, integer, character, logical

! Data structures to hold variables in various categories: meteorological (met),
! vegetation (veg), wind, soil, temperature and humidity soundings (temp_snd, 
! humid_snd), time and location (timeloc).

  type t_met
    real(kind=wp) :: omega, zo, obst_hgt
    integer :: cld_fract
    logical :: cloud_flag
  end type t_met

  type t_timeloc
    integer :: iyr, imo, iday, tz
    real(kind=wp) :: xlat, xlong
    real(kind=wp) :: strtim, timend, outtt
    real(kind=wp) :: slope, aspect, station_height
  end type t_timeloc

  type t_veg
    real(kind=wp) :: frveg, xlai, epsf, albf, volrel
    real(kind=wp) :: rmin, rcut, wilt, vegheight, width
    real(kind=wp) :: ci, co, coz_sfc, coz_air
    character(len=:), allocatable :: albedo_fflag, stmtype, steady
    integer :: index_veggies
  end type t_veg

  type t_wind
    real(kind=wp) :: ugs, vgs
    integer :: num_obs
  end type t_wind

  type t_soil
    real(kind=wp) :: f, fsub, wmax, btemp, tp, albg, epsi
    real(kind=wp) :: ti_a, ti_b
    character(len=:), allocatable :: dual_ti
    logical :: albedo_gflag
    integer :: index_soils
  end type t_soil

  type t_temp
    real(kind=wp), dimension(:), allocatable :: ps, ts, dep
  end type t_temp

  type t_humid
    real(kind=wp), dimension(:), allocatable :: dd0, ff0, zh
    integer :: num_obs
  end type t_humid
  
!  interface get_var
!    module procedure get_var_character
!    module procedure get_var_integer
!    module procedure get_var_real
!    module procedure get_var_logical
!  end interface get_var

contains

  subroutine init_json(file, json)
    character(len=:),allocatable :: file
    integer :: error_cnt
    type(json_file) :: json

    error_cnt = 0
    call json%initialize(compact_reals=.true.)
    if (json%failed()) then
      call json%print_error_message(error_unit)
      error_cnt = error_cnt + 1
    end if
  
    call json%load_file(filename = file)
    if (json%failed()) then
      call json%print_error_message(error_unit)
      error_cnt = error_cnt + 1
    end if
    return
  end subroutine init_json

  subroutine destroy_json(json)
    type(json_file) :: json

    call json%destroy()
    if (json%failed()) stop 1
    return
  end subroutine destroy_json 

  subroutine load_config(json, met, timeloc, veg, wind, soil, temp, humidity)
    type(json_file) :: json
    type(t_met) :: met
    type(t_timeloc) :: timeloc
    type(t_veg) :: veg
    type(t_wind) :: wind
    type(t_soil) :: soil
    type(t_temp) :: temp
    type(t_humid) :: humidity
    logical :: found, namelist_style

    found = .false.
    namelist_style = .true.

    ! Load data structures: met, veg, wind, soil, temp_snd, humid_snd

    ! Meteorological
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
  
    ! Time and Location
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
  
    ! Vegetation
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
  
    ! Wind
    call json%get('inputs.wind.ugs',wind%ugs, found)
    if (.not. found) stop 1
    call json%get('inputs.wind.vgs',wind%vgs, found)
    if (.not. found) stop 1
    call json%get('inputs.nobs_wind',wind%num_obs, found)
    if (.not. found) stop 1
  
    ! Soil
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
  
    ! Temperature
    call json%get('inputs.temp_sounding.ps',temp%ps, found)
    if (.not. found) stop 1
    call json%get('inputs.temp_sounding.ts',temp%ts, found)
    if (.not. found) stop 1
    call json%get('inputs.temp_sounding.dep',temp%dep, found)
    if (.not. found) stop 1
  
    ! Humidity
    call json%get('inputs.humidity_sounding.dd0', humidity%dd0, found)
    if (.not. found) stop 1
    call json%get('inputs.humidity_sounding.ff0', humidity%ff0, found)
    if (.not. found) stop 1
    call json%get('inputs.humidity_sounding.zh', humidity%zh, found)
    if (.not. found) stop 1
    call json%get('inputs.nobs_ptq', humidity%num_obs, found)
    if (.not. found) stop 1

    return
  end subroutine load_config


!  character pure function get_var_character (var)
!    character(len=:), intent(in) :: var
!
!  end function get_var_character
!
!  integer pure function get_var_integer (var)
!    character(len=:), intent(in) :: var
!
!  end function get_var_integer
!
!  real pure function get_var_real (var)
!    character(len=:), intent(in) :: var
!
!  end function get_var_real
!
!  logical pure function get_var_logical (var)
!    character(len=:), intent(in) :: var
!
!  end function get_var_logical
!
!  subroutine init_json (fh)
!    ! config file name
!    character(len=:) :: fh
!    type(json_file) :: json
!    type(json_core) :: core
!    integer :: error_cnt
!    logical :: namelist_style
!    
!    error_cnt = 0
!    call json%initialize()
!    if (json%failed()) then
!      call json%print_error_message(error_unit)
!      error_cnt = error_cnt + 1
!    end if
!
!    call json%load_file(filename = fh)
!    if (json%falide()) then
!      call json%print_error_message(error_unit)
!      error_cnt = error_cnt + 1
!    end if
!
!    call core%intialize()
!
!  end subroutine init_json
    



end module config_mod
