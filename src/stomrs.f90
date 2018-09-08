subroutine  STOMRS
  use simsphere_mod

!       INCLUDE 'modvars.h'

!  This program calculates stomatal resistance coefficients are
!  initialized in stmcof.for

  if ( TF .GT. MINTEMP .AND. TF .LT. MAXTEMP ) then
    if ( PSISUP.GT. PSIWC ) then
      FPSIE = 1 + B1 * PSIE 
    else
      FPSIE = 1 + B1 * PSICE + B2 * ( PSICE - PSIE ) 
    end if
  else
    RS = 5000
    PRINT*, 'TEMPERATURE IS LESS THAN OR GREATER THAN THE'
    PRINT*, 'CRITICAL TEMPERATURE -- STOMATAL RESISTANCE '
    PRINT*, 'SET EQUAL TO 5000 S M-1'
    return

  end if

  RS = RMIN * FS * FPSIE * FT
       
  return
end
