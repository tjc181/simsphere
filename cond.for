      SUBROUTINE  COND

$INCLUDE:'modvars.h'


c * * program units are in m/s

*/  Use Field Capacity water content 75% that of THMAX.

      RKW = ( 6.9E-6 ) * RKS * (THV / (THMAX*0.75)) ** (2 * COSBYB + 2)

      RETURN
      END
