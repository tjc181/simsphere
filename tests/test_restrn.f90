program test_restrn
  use simsphere_mod
  implicit none

  real :: arg1,arg2,arg3

  arg1 = 1.0
  arg2 = 2.0
  arg3 = 3.0

  write(*,*) restrn(arg1,arg2,arg3)

end program
