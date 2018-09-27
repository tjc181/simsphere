program test_psgcal
  use simsphere_mod
  implicit none

  THMAX = 0.338999987
  THV = 1.0
  COSBYB = 2.78999996
  PSIS = 1.0

  write(*,*) PSIG
  call PSGCAL
  write(*,*) PSIG

end program test_psgcal
