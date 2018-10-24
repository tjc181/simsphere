module constants
  implicit none
  private
  public :: vert_spacing, rhow, DELTA, rot_rate_earth, siga, dens, ft, ZA, SIGMA
  public :: LE, KARMAN, GRAV, R, RAD, CP, radian, sdec, Kts_To_Metres
  public :: Ft_To_Metres, Degs_To_Radians, Celsius_To_Kelvin
  public :: f_soil_lut, f_veg_lut, f_precip_lut

  integer, parameter :: vert_spacing = 250
  integer, parameter :: rhow=1000                ! Density of Water
  integer, parameter ::  DELTA = 90

  real, parameter :: rot_rate_earth = 7.27e-5
  real, parameter :: siga = 279.9348
!  real, parameter :: radian = 57.29578
  real, parameter :: dens = 1.1                  ! Density of Air
  real, parameter :: ft = 1.0
  real, parameter :: ZA = 50.0
  real, parameter :: SIGMA = 5.6521E-8
  real, parameter :: LE = 2.5E6
  real, parameter :: KARMAN = 0.4
  real, parameter :: GRAV = 9.78
  real, parameter :: R = 287.5
  real, parameter :: RAD = 1.6E-5
  real, parameter :: CP = 1004.832
  double precision, parameter :: radian = 572957.75913D-4
  double precision, parameter :: sdec = 39784.988432D-5

!    conversion factors (formerly factors.h)
  real, parameter :: Kts_To_Metres = 1.944       ! Knots to ms-1
  real, parameter :: Ft_To_Metres = 3.281        ! Feet to meters
  real, parameter :: Degs_To_Radians = 0.0174533 ! Degrees to Radians
  real, parameter :: Celsius_To_Kelvin = 273.15  ! Celsius to Kelvin

! Data file names
  character(len=1024), parameter :: f_soil_lut = 'data/soils.dat'
  character(len=1024), parameter :: f_veg_lut = 'data/veglut.dat'
  character(len=1024), parameter :: f_precip_lut = 'data/lut.dat'

end module constants
