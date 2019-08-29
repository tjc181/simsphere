module config_mod
  use, intrinsic :: iso_fortran_env, only: real64, error_unit
  use json_module
  implicit none

  private
  public :: load_config
  public :: t_met, t_veg, t_wind, t_soil, t_temp, t_windsnd, t_timeloc

! config_mod provides data structures and subroutines to read data from the 
! model configuration file and initialize variables to default values.  Data 
! are stored in a JSON file.

! Data structures are per categories in the sample input document, 
! https://simsphere.ems.psu.edu/assets/downloads/Part%20IV;%20model%20input%20parameters.xls
!  t_met: Meteorological 
!  t_timeloc: Time and Location
!  t_veg: Vegetation
!  t_soil: Soil
!  t_wind: Geostrophic wind components
!  t_temp: Temperature sounding
!  t_windsnd: Wind sounding

  type t_met
    real(kind=real64) :: omega, zo, obst_hgt
    real(kind=real64) :: cld_fract
    logical :: cloud_flag
  end type t_met

  type t_timeloc
    integer :: year, month, day, tz
    real(kind=real64) :: xlat, xlong
    real(kind=real64) :: strtim, timend, outtt
    real(kind=real64) :: slope, aspect, station_height
  end type t_timeloc

  type t_veg
    real(kind=real64) :: frveg, xlai, epsf, albf, volrel
    real(kind=real64) :: rmin, rcut, wilt, vegheight, width
    real(kind=real64) :: ci, co, coz_sfc, coz_air
    character(len=:), allocatable :: albedo_fflag, stmtype, steady
    integer :: index_veggies
  end type t_veg

  type t_wind
    real(kind=real64) :: ugs, vgs
    integer :: num_obs
  end type t_wind

  type t_soil
    real(kind=real64) :: f, fsub, wmax, btemp, tp, albg, epsi
    real(kind=real64) :: ti_a, ti_b
    character(len=:), allocatable :: dual_ti, albedo_gflag
    integer :: index_soils
  end type t_soil

  type t_temp
    real(kind=real64), dimension(:), allocatable :: ps, ts, dep
  end type t_temp

  type t_windsnd
    real(kind=real64), dimension(:), allocatable :: dd0, ff0, zh
    real(kind=real64) :: thick
    integer :: num_obs
  end type t_windsnd

contains

  subroutine init_json(json)
    integer :: error_cnt
    type(json_file) :: json

    error_cnt = 0
    call json%initialize(compact_reals=.true.)
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

  subroutine load_config(file, met, timeloc, veg, wind, soil, temp, windsnd)
    type(json_file) :: json
    type(t_met) :: met
    type(t_timeloc) :: timeloc
    type(t_veg) :: veg
    type(t_wind) :: wind
    type(t_soil) :: soil
    type(t_temp) :: temp
    type(t_windsnd) :: windsnd
    logical :: found, namelist_style
    integer :: error_cnt

    character(len=:),allocatable :: file
    character(len=6), parameter :: root = 'inputs'
    character(len=20) :: path

    call init_json(json)

    error_cnt = 0
    call json%load_file(filename = file)
    if (json%failed()) then
      call json%print_error_message(error_unit)
      error_cnt = error_cnt + 1
    end if

    found = .false.
    namelist_style = .true.

    ! Load data structures: met, veg, wind, soil, temp_snd, wind_snd

    ! Meteorological
    path = '.meteorological'
    call json%get(root//path//'.omega',met%omega,found)
    if (.not. found) stop 1
    call json%get(root//path//'.zo',met%zo,found)
    if (.not. found) stop 1
    call json%get(root//path//'.obst_hgt',met%obst_hgt,found)
    if (.not. found) stop 1
    call json%get(root//path//'.cld_fract',met%cld_fract,found)
    if (.not. found) stop 1
    call json%get(root//path//'.cloud_flag',met%cloud_flag,found)
    if (.not. found) stop 1
  
    ! Time and Location
    path = '.time_location'
    call json%get(root//path//'.year',timeloc%year, found)
    if (.not. found) stop 1
    call json%get(root//path//'.month',timeloc%month, found)
    if (.not. found) stop 1
    call json%get(root//path//'.day',timeloc%day, found)
    if (.not. found) stop 1
    call json%get(root//path//'.tz',timeloc%tz, found)
    if (.not. found) stop 1
    call json%get(root//path//'.xlat',timeloc%xlat, found)
    if (.not. found) stop 1
    call json%get(root//path//'.xlong',timeloc%xlong, found)
    if (.not. found) stop 1
    call json%get(root//path//'.strtim',timeloc%strtim, found)
    if (.not. found) stop 1
    call json%get(root//path//'.timend',timeloc%timend, found)
    if (.not. found) stop 1
    call json%get(root//path//'.outtt',timeloc%outtt, found)
    if (.not. found) stop 1
    call json%get(root//path//'.slope',timeloc%slope, found)
    if (.not. found) stop 1
    call json%get(root//path//'.aspect',timeloc%aspect, found)
    if (.not. found) stop 1
    call json%get(root//path//'.station_height',timeloc%station_height, found)
    if (.not. found) stop 1
  
    ! Vegetation
    path = '.vegetation'
    call json%get(root//path//'.frveg',veg%frveg, found)
    if (.not. found) stop 1
    call json%get(root//path//'.xlai',veg%xlai, found)
    if (.not. found) stop 1
    call json%get(root//path//'.epsf',veg%epsf, found)
    if (.not. found) stop 1
    call json%get(root//path//'.albf',veg%albf, found)
    if (.not. found) stop 1
    call json%get(root//path//'.volrel',veg%volrel, found)
    if (.not. found) stop 1
    call json%get(root//path//'.rmin',veg%rmin, found)
    if (.not. found) stop 1
    call json%get(root//path//'.rcut',veg%rcut, found)
    if (.not. found) stop 1
    call json%get(root//path//'.wilt',veg%wilt, found)
    if (.not. found) stop 1
    call json%get(root//path//'.vegheight',veg%vegheight, found)
    if (.not. found) stop 1
    call json%get(root//path//'.width',veg%width, found)
    if (.not. found) stop 1
    call json%get(root//path//'.ci',veg%ci, found)
    if (.not. found) stop 1
    call json%get(root//path//'.co',veg%co, found)
    if (.not. found) stop 1
    call json%get(root//path//'.coz_sfc',veg%coz_sfc, found)
    if (.not. found) stop 1
    call json%get(root//path//'.coz_air',veg%coz_air, found)
    if (.not. found) stop 1
    call json%get(root//path//'.albedo_fflag',veg%albedo_fflag, found)
    if (.not. found) stop 1
    call json%get(root//path//'.stmtype',veg%stmtype, found)
    if (.not. found) stop 1
    call json%get(root//path//'.steady',veg%steady, found)
    if (.not. found) stop 1
    call json%get(root//path//'.index_veggies',veg%index_veggies, found)
    if (.not. found) stop 1
  
    ! Wind
    path = '.wind'
    call json%get(root//path//'.ugs',wind%ugs, found)
    if (.not. found) stop 1
    call json%get(root//path//'.vgs',wind%vgs, found)
    if (.not. found) stop 1
    call json%get(root//'.nobs_wind',wind%num_obs, found)
    if (.not. found) stop 1
  
    ! Soil
    path = '.soil'
    call json%get(root//path//'.f',soil%f, found)
    if (.not. found) stop 1
    call json%get(root//path//'.fsub',soil%fsub, found)
    if (.not. found) stop 1
    call json%get(root//path//'.wmax',soil%wmax, found)
    if (.not. found) stop 1
    call json%get(root//path//'.btemp',soil%btemp, found)
    if (.not. found) stop 1
    call json%get(root//path//'.tp',soil%tp, found)
    if (.not. found) stop 1
    call json%get(root//path//'.albg',soil%albg, found)
    if (.not. found) stop 1
    call json%get(root//path//'.epsi',soil%epsi, found)
    if (.not. found) stop 1
    call json%get(root//path//'.ti_a',soil%ti_a, found)
    if (.not. found) stop 1
    call json%get(root//path//'.ti_b',soil%ti_b, found)
    if (.not. found) stop 1
    call json%get(root//path//'.dual_ti',soil%dual_ti, found)
    if (.not. found) stop 1
    call json%get(root//path//'.albedo_gflag',soil%albedo_gflag, found)
    if (.not. found) stop 1
    call json%get(root//path//'.index_soils',soil%index_soils, found)
    if (.not. found) stop 1
  
    ! Temperature
    path = '.temp_sounding'
    call json%get(root//path//'.ps',temp%ps, found)
    if (.not. found) stop 1
    call json%get(root//path//'.ts',temp%ts, found)
    if (.not. found) stop 1
    call json%get(root//path//'.dep',temp%dep, found)
    if (.not. found) stop 1
  
    ! Wind
    path = '.wind_sounding'
    call json%get(root//path//'.dd0', windsnd%dd0, found)
    if (.not. found) stop 1
    call json%get(root//path//'.ff0', windsnd%ff0, found)
    if (.not. found) stop 1
    call json%get(root//path//'.zh', windsnd%zh, found)
    if (.not. found) stop 1
    call json%get(root//'.nobs_ptq', windsnd%num_obs, found)
    if (.not. found) stop 1

    ! Clean up
    call destroy_json(json)
    return
  end subroutine load_config

end module config_mod
