      SUBROUTINE  STOMFS
  use simsphere_mod

!      INCLUDE 'modvars.h'

*/ The exponential function for solar radiation -- Albert Olioso

      fs = 1 / (1 - exp(-1/sc*sol))

      RETURN
      END
