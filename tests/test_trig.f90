program test_trig
  implicit none

  real :: SOLSIN, EFFDEC, SLB, HRANGL

  EFFDEC = 4.635570647466656e-310
  SLB = 0.685041785
  HRANGL = 1.28342795
  SOLSIN = 0.0

  SOLSIN = SIN(EFFDEC)*SIN(SLB)+COS(EFFDEC)*COS(SLB)*COS(HRANGL)

  write(*,*) SOLSIN

end program test_trig
