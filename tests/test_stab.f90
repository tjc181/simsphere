program test_stab
  use simsphere_mod
  implicit none

  real :: arg1 = 0.1 

  MOL = 2.0

  write(*,*) stab(arg1)

  write(*,*) stabh(arg1)

end program

