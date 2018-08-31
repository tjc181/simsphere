      SUBROUTINE  STOMC

      INCLUDE:'modvars.h'

*/  Calculates the critical stomatal resistance for the
*/  critical ground water potential

      FPSICE = 1 + B1 * PSICE

      RSCRIT = RMIN * FS * FPSICE * FT

      RETURN
      END
