program test_avr
  use simsphere_mod
  implicit none

  real :: avr1, avr2

  avr1 = 100.0
  avr2 = 0.0

  call average(avr1, avr2)

  write(*,*) avr2

end program
