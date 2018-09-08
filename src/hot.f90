subroutine  HOT (B,BareNetRadn,BareEvapFlux,BareHeatFlux)
  use simsphere_mod


!  HOT is called during the day to compute the sensible heat flux HEAT
!  from net all wave radiation and the evaporation.

  real :: BareNetRadn,BareEvapFlux
  real :: BareHeatFlux,VegnHeatFlux,MixedHeatFlux

!      INCLUDE 'modvars.h'

  A = ( Lambda * ( Atemp - TT(2) ) ) / Z(2)
  B = Lambda / ( Z(2) * Dens * Cp )

  if (Frveg .eq. 1 .and. Rnet .gt. 0) then

! Vegetation

    call Veghot(B,VegnHeatFlux)
    Heat = VegnHeatFlux

  else if (Frveg .gt. 0 .and. Frveg .lt. 1 .and. Rnet .gt. 0) then

! Mixed
    BareHeatFlux = (BareNetRadn - BareEvapFlux - A) / (1+B*SUM)
    call Veghot (B,VegnHeatFlux)
    MixedHeatFlux = (1-Frveg) * BareHeatFlux + Frveg * VegnHeatFlux
    Heat = MixedHeatFlux
  else

! Bare Soil

    BareHeatFlux = (BareNetRadn - BareEvapFlux - A) / (1 + B * SUM)
    Heat = BareHeatFlux

  end if
  return
end
