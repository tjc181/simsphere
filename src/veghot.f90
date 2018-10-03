subroutine  VEGHOT (B,Heatv)
  use simsphere_mod, only: dens, cp, chf, tf, taf, lambda, tt, z, hg, rnetg, &
                           xleg, chg, hf
  implicit none

  real :: B, Heatv, HFN, AVEG

!      INCLUDE 'modvars.h'

  HFN = DENS * CP * CHF * (TF - TAF)
  HF  = (HF + HFN) / 2
  AVEG = ( LAMBDA * ( TAF - TT(2) ) ) / Z(2)

!      VGDENS = PS1 * 100 / (R * TAF )

  HG = ( RNETG - XLEG - AVEG ) / ( 1 + B / CHG )

  HEATV = HG + HF

  call CO2FLX

  call ozone

  return
end
