      SUBROUTINE  TRANSM (PATH,FTABS,FTSCAT,FBSCAT)

C     Subroutine TRANSM calculates solar transmission by using the
C     three-way lookup table produced in GETTBL.

$INCLUDE:'modvars.h'

C **  If the path length is very large (sun almost on the horizon) use
C **  longest path length possible, ie last number in the table. Otherwise
C **  calc trans coeff's for entries bracketing the supplied path length.
C **  FRACTP - Scaling fact for depth of atmos. FRACT & FRACT2 weighting
C **  factors for interpol'n between 2 successive path lenghts in table.

      IF ( PATH .GE. 10 )THEN
       FTABS = ABSTBL (46)
       FTSCAT = SCATBL (46)
       FBSCAT = BSCTBL(46)
      ELSE
       FRACP = PS1 / 1013.25
       FRACT= 5 * ( PATH - 1 ) + 1
       IPATH = IFIX( FRACT )
       JPATH = IPATH + 1
       FRACT = ( FRACT - IPATH )
       FRACT2 = 1 - FRACT
       FTABS = FRACT2 * ABSTBL( IPATH ) + FRACT * ABSTBL( JPATH )
       FTSCAT = FRACT2 * SCATBL( IPATH ) + FRACT * SCATBL( JPATH )
       FBSCAT = FRACT2 * BSCTBL( IPATH ) + FRACT * BSCTBL( JPATH )
       FTABS = FRACP * ( FTABS - 1 ) + 1
       FTSCAT = FRACP * ( FTSCAT - 1 ) + 1
      ENDIF

      RETURN
      END
