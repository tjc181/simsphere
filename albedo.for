      SUBROUTINE  ALBEDO (SOLSIN)

      LOGICAL ALGFLG,ALFFLG
      INTEGER ALBFLG

      INCLUDE 'modvars.h'

C **  Modified 1/8/90 to allow user to specify Albedoes.

C **  If the albedoes are ommitted in START then they are calculated,
C **  otherwise the albedo of the ground and the foliage input are used
C **  to calculate the weighted albedo (ALBDOE).
C **  Note: ALBF depends on solar angle (SOLSIN); ALBG on WGG.

      DATA ALBFLG,ALGFLG,ALFFLG / 0, .FALSE., .FALSE./

      IF (ALBFLG .EQ. 0) THEN
        IF (ALBG .EQ. 0.0) ALGFLG = .TRUE.
        IF (ALBF .EQ. 0.0) ALFFLG = .TRUE.
        ALBFLG = 1
      ENDIF

      IF ( ALGFLG ) THEN
	ALBG = 0.25 - 0.20 * WGG / WMAX     !  Toby's Value
c	ALBG = 0.20 - 0.15 * WGG / WMAX     !  Fudged Dim
c	ALBG = 0.30 - 0.20 * WGG / WMAX     !  Fudged Bright
      ENDIF

      IF ( ALFFLG ) THEN
c	ALBF = 0.025 /( 0.1 + 0.1 * SOLSIN ) + ( 1 - SOLSIN**2 ) * 0.1	! T
	ALBF = 0.032 /( 0.1 + 0.1 * SOLSIN ) + ( 1 - SOLSIN**2 ) * 0.1	! A
      ENDIF

      SIGF = 1 - EXP(-0.4 * XLAI )
      ALBDOE = (SIGF*ALBF + (1 - SIGF)*ALBG)*FRVEG + (1-FRVEG) * ALBG

      RETURN
      END
