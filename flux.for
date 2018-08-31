      SUBROUTINE FLUX (BareRadioTemp,VegnRadioTemp,BareEvapFlux,
     /                 BareHeatFlux)

*/  During the day FLUX calc's surface temp and surface specific humidity
*/  from temp and humidity at ZA and the sensible & latent heat fluxes.
*/  It also computes the updated value of the evaporative flux.
*/  During the night it calls GTEMP to calc the temp in the absence of
*/  turbulence.

      Real BareRadioTemp,VegnRadioTemp,MixedRadioTemp
      Real BareEvapFlux,VegnEvapFlux,MixedEvapFlux
      Real BareHeatFlux
      Real Evap_Smooth

      INCLUDE 'modvars.h'

*/  We calculate OTEMP from GTEMP routine once we have reached
*/  radiative balance at night.

*/  Put in a time restraint so you do not call GTEMP in the am.


      Oshum = 10**( 6.1989 - (2353 / BareRadioTemp) )
      BareEvapFlux = Le * Dens * ( Oshum - Qd(1) ) / Sumw * F
      If ( qd(1) .ge. oshum ) BareEvapFlux = 0.001

      call average (BareEvapFlux, Evap_Smooth)
      BareEvapFlux = Evap_Smooth
      Evap = BareEvapFlux

*/ Nighttime

      If ( rnet .lt. 0 ) Then

       Call Gtemp

	 BareHeatFlux = Heat
         VegnRadioTemp = Otemp
         BareRadioTemp = Otemp
         MixedRadioTemp = Otemp
	 Evap = BareEvapFlux

*/ Daytime
*/
*/ Bare soil

      Else

       BareRadioTemp = Aptemp + ( BareHeatFlux * Sum / ( Dens * Cp ) )
     /		       - Tdif_s

	Otemp=BareRadioTemp

*/ Daytime Restraint

        If (Rnet .gt. 0) Then

*/ Vegetation

          If (Frveg .eq. 1 ) Then

             Call Vegflx (VegnEvapFlux)

             Evap = VegnEvapFlux
             Otemp = VegnRadioTemp

*/ Mixed

          Else if (Frveg.gt. 0 .and. Frveg .lt. 1) Then

             Call Vegflx (VegnEvapFlux)
             MixedEvapFlux = (1-Frveg) * BareEvapFlux + Frveg *
     /			     VegnEvapFlux
             MixedRadioTemp = (BareRadioTemp**4 * (1-Frveg) + Frveg *
     /			      VegnRadioTemp**4)**0.25

             Evap = MixedEvapFLux
             Otemp = MixedRadioTemp

          End If

        End If

      End If

      Ahum = Qd(1)

      RETURN
      END
