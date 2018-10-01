program test_albedo
  use simsphere_mod
  implicit none

  real :: SOLSIN

  SOLSIN = -0.510662317
  XLAI = 1.0
  FRVEG = 1.0
  WGG = 100.0
  WMAX = 1230.0

  ! If ALBFLG = 0 (initialized this way)
    ! ALBG = 0 -> ALGFLG = true
    ! ALBF = 0 ->  ALFFLG = true
    ! ALBFLG = 1

  ! Case I: ALBG == 0 and ALBF == 0

  ALBG = 0
  ALBF = 0
  call ALBEDO(SOLSIN)
  write(*,*) 'case I: ',ALBDOE

  ! Case II: ALBG /= 0 and ALBF == 0

  ALBG = 20.0
  ALBF = 0
  call ALBEDO(SOLSIN)
  write(*,*) 'case II: ',ALBDOE

  ! Case III: ALBG == 0 and ALBF /= 0

  ALBG = 0
  ALBF = 100.0
  call ALBEDO(SOLSIN)
  write(*,*) 'case III: ',ALBDOE

  ! Case IV: ALBG /= 0 and ALBF /=0

  ALBG = 46
  ALBF = 71
  call ALBEDO(SOLSIN)
  write(*,*) 'case IV: ',ALBDOE

  
end program test_albedo
