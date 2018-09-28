program test_veghot
  use simsphere_mod
  implicit none

! Call subroutine VEGHOT to determine variables changed, develop tests

  real :: B = 1.0, arg2 = 1.0

  TF = 1.0
  TAF = 1.0
  CHF = 1.0
  CHG = 1.0
  HF = 1.0
  TT(2) = 1.0
  Z(2) = 1.0
  LAMBDA = 1.0
  RNETG = 3.0
  XLEG = 1.0
  
  
  write(*,*) arg2

  call VEGHOT(B, arg2)

  write(*,*) arg2

  B = LAMBDA / ( Z(2) * Dens * Cp )

  call VEGHOT(B,arg2)

  write(*,*) arg2


end program test_veghot
