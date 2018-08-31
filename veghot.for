      SUBROUTINE  VEGHOT (B,Heatv)

$INCLUDE:'modvars.h'

      HFN = DENS * CP * CHF * (TF - TAF)
      HF  = (HF + HFN) / 2
      AVEG = ( LAMBDA * ( TAF - TT(2) ) ) / Z(2)

c      VGDENS = PS1 * 100 / (R * TAF )

      HG = ( RNETG - XLEG - AVEG ) / ( 1 + B / CHG )

      HEATV = HG + HF

      CALL CO2FLX

	call ozone

      RETURN
      END
