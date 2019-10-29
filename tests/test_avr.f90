program test_avr
  use simsphere_mod
  implicit none

  real :: avr1, avr2
  integer :: avr3 = 1

  avr1 = 100.0
  avr2 = 0.0

  call avr(avr1, avr2, avr3)

  write(*,*) avr2

end program
