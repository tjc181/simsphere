program test_average
  implicit none

  real :: arg1, arg2

  arg1 = 16.0
  arg2 = 0.0

  call average(arg1,arg2)

  write(*,*) arg2

end program test_average
