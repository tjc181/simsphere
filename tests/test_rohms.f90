program test_rohms
  use simsphere_mod
  implicit none

  real :: arg1,arg2,arg3,arg4

  arg1 = 0.1
  arg2 = 1.0
  arg3 = 2.0
  arg4 = 3.0

  write(*,*) r_ohms(arg1,arg2,arg3,arg4)

end program
