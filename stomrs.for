       SUBROUTINE  STOMRS

$INCLUDE:'modvars.h'

*/  This program calculates stomatal resistance coefficients are
*/  initialized in stmcof.for

       IF ( TF .GT. MINTEMP .AND. TF .LT. MAXTEMP ) THEN

        IF ( PSISUP.GT. PSIWC ) THEN
           FPSIE = 1 + B1 * PSIE 
        ELSE
           FPSIE = 1 + B1 * PSICE + B2 * ( PSICE - PSIE ) 
        END IF

       ELSE

       RS = 5000
       PRINT*, 'TEMPERATURE IS LESS THAN OR GREATER THAN THE'
       PRINT*, 'CRITICAL TEMPERATURE -- STOMATAL RESISTANCE '
       PRINT*, 'SET EQUAL TO 5000 S M-1'
       RETURN

       END IF

       RS = RMIN * FS * FPSIE * FT
       
       RETURN
      END
