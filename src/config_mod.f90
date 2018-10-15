module config
  use, intrinsic :: iso_fortran_env, only: wp => real64, error_unit
  use json_module
  implicit none

  private
!  public :: init_json, json, core
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
  end type t_humid
  
!  interface get_var
!    module procedure get_var_character
!    module procedure get_var_integer
!    module procedure get_var_real
!    module procedure get_var_logical
!  end interface get_var

contains

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
    



end module config
