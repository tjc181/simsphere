module constants
!    list of parameter constants.

  real, parameter :: radian = 57.29578
  real, parameter :: rot_rate_earth = 7.27e-5

!    conversion factors

  real, parameter :: Kts_To_Metres = 1.944       ! Knots to ms-1
  real, parameter :: Ft_To_Metres = 3.281        ! Feet to meters
  real, parameter :: Degs_To_Radians = 0.0174533 ! Degrees to Radians
  real, parameter :: Celsius_To_Kelvin = 273.15  ! Celsius to Kelvin

end module constants
