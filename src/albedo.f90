subroutine  ALBEDO (SOLSIN)
  use simsphere_mod, only: albg, albf, wgg, wmax, frveg, xlai, albdoe, sigf, eq
  implicit none

  real :: SOLSIN
  logical, save :: ALGFLG = .false. , ALFFLG = .false.
  integer, save :: ALBFLG = 0

!      INCLUDE 'modvars.h'

! **  Modified 1/8/90 to allow user to specify Albedoes.

! **  If the albedoes are ommitted in START then they are calculated,
! **  otherwise the albedo of the ground and the foliage input are used
! **  to calculate the weighted albedo (ALBDOE).
! **  Note: ALBF depends on solar angle (SOLSIN); ALBG on WGG.

  IF (ALBFLG == 0) THEN
    IF (eq(ALBG,0.0)) ALGFLG = .TRUE.
    IF (eq(ALBF,0.0)) ALFFLG = .TRUE.
    ALBFLG = 1
  ENDIF

  IF ( ALGFLG ) THEN
    ALBG = 0.25 - 0.20 * WGG / WMAX     !  Toby's Value
!	ALBG = 0.20 - 0.15 * WGG / WMAX     !  Fudged Dim
!	ALBG = 0.30 - 0.20 * WGG / WMAX     !  Fudged Bright
  ENDIF

  IF ( ALFFLG ) THEN
!	ALBF = 0.025 /( 0.1 + 0.1 * SOLSIN ) + ( 1 - SOLSIN**2 ) * 0.1	! T
    ALBF = 0.032 /( 0.1 + 0.1 * SOLSIN ) + ( 1 - SOLSIN**2 ) * 0.1 ! A
  ENDIF

  SIGF = 1 - EXP(-0.4 * XLAI )
  ALBDOE = (SIGF*ALBF + (1 - SIGF)*ALBG)*FRVEG + (1-FRVEG) * ALBG

  return
end
