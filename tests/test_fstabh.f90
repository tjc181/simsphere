program test_fstabh
  use simsphere_mod
  implicit none

  real :: arg1, arg2

  arg1 = 1.0
  arg2 = 2.0

  write(*,*) fstabh(arg1,arg2)

  write(*,*) fstabm(arg1,arg2)

end program
