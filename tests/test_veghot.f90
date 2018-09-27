program test_veghot
  use simsphere_mod
  implicit none

! Call subroutine VEGHOT to determine variables changed, develop tests

  real :: B = 1.0, Heatv = 1.0


  call VEGHOT(B, Heatv)

  write(*,*) HeatV


end program test_veghot
