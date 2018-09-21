program test_windf
  use simsphere_mod
  implicit none

  real :: arg1,arg2,arg3,arg4

  arg1 = 1.0
  arg2 = 2.0
  arg3 = 3.0
  arg4 = 4.0

  write(*,*) windf(arg1,arg2,arg3,arg4)

end program
