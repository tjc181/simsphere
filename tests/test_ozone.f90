program test_ozone
  use simsphere_mod
  implicit none

  USTAR = 1.0
  UTEN = 1.0
  UAF = 0.5
  XLAI = 2.0
  RAF = 1.0
  RCUT = 1.0
  RST = 1.0
  CHG = 2.0
  RZASCR = 1.0
  COZ_AIR = 1.0
  frveg = 0.5
  sumo3 = 0.1

  write(*,*) flux_plant, fglobal

  call ozone

  write(*,*) flux_plant, fglobal

end program test_ozone
