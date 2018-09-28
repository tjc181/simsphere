program test_co2flx
  use simsphere_mod
  implicit none

  USTAR = 1.0
  UTEN = 2.0
  UAF = 1.0
  XLAI = 1.0
  RZASCR = 2.0
  RST = 1.0
  CO = 2.0
  CI = 1.0
  RAF = 1.0
  FRVEG = 0.5

  write(*,*) CCAN, FCO2

  call co2flx

  write(*,*) CCAN, FCO2
  

end program test_co2flx
