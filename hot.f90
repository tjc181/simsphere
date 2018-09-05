      SUBROUTINE  HOT (B,BareNetRadn,BareEvapFlux,BareHeatFlux)
  use simsphere_mod


*/  HOT is called during the day to compute the sensible heat flux HEAT
*/  from net all wave radiation and the evaporation.

      Real BareNetRadn,BareEvapFlux
      Real BareHeatFlux,VegnHeatFlux,MixedHeatFlux

!      INCLUDE 'modvars.h'

      A = ( Lambda * ( Atemp - TT(2) ) ) / Z(2)
      B = Lambda / ( Z(2) * Dens * Cp )

      If (Frveg .eq. 1 .and. Rnet .gt. 0) Then

*/ Vegetation

            Call Veghot(B,VegnHeatFlux)
            Heat = VegnHeatFlux

      Else If (Frveg .gt. 0 .and. Frveg .lt. 1 .and. Rnet .gt. 0) Then

*/ Mixed

	    BareHeatFlux = (BareNetRadn - BareEvapFlux - A) / (1+B*SUM)

	    Call Veghot (B,VegnHeatFlux)

	    MixedHeatFlux = (1-Frveg) * BareHeatFlux + Frveg *
     /                       VegnHeatFlux

            Heat = MixedHeatFlux

        Else

*/ Bare Soil

		BareHeatFlux = (BareNetRadn - BareEvapFlux - A) / (1 + B * SUM)
		Heat = BareHeatFlux

	End If

      Return
      End
