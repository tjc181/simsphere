subroutine FLUX (BareRadioTemp,VegnRadioTemp,BareEvapFlux,BareHeatFlux)
  use simsphere_mod
  implicit none

!  During the day FLUX calc's surface temp and surface specific humidity
!  from temp and humidity at ZA and the sensible & latent heat fluxes.
!  It also computes the updated value of the evaporative flux.
!  During the night it calls GTEMP to calc the temp in the absence of
!  turbulence.

  real :: BareRadioTemp,VegnRadioTemp,MixedRadioTemp
  real :: BareEvapFlux,VegnEvapFlux,MixedEvapFlux
  real :: BareHeatFlux
  real :: Evap_Smooth

!      INCLUDE 'modvars.h'

!  We calculate OTEMP from GTEMP routine once we have reached
!  radiative balance at night.

!  Put in a time restraint so you do not call GTEMP in the am.


  Oshum = 10**( 6.1989 - (2353 / BareRadioTemp) )
  BareEvapFlux = Le * Dens * ( Oshum - Qd(1) ) / Sumw * F
  if ( qd(1) .ge. oshum ) BareEvapFlux = 0.001

  call average (BareEvapFlux, Evap_Smooth)
  BareEvapFlux = Evap_Smooth
  Evap = BareEvapFlux

! Nighttime

  if ( rnet .lt. 0 ) then
    Call Gtemp
    BareHeatFlux = Heat
    VegnRadioTemp = Otemp
    BareRadioTemp = Otemp
    MixedRadioTemp = Otemp
    Evap = BareEvapFlux

! Daytime
!
! Bare soil

  else
    BareRadioTemp = Aptemp + ( BareHeatFlux * GBL_sum / ( Dens * Cp ) )     &
                    - Tdif_s

    Otemp=BareRadioTemp

! Daytime Restraint

    if (Rnet .gt. 0) then

! Vegetation

      if (eq(frveg,1.0) ) then
        call Vegflx (VegnEvapFlux)
        Evap = VegnEvapFlux
        Otemp = VegnRadioTemp

! Mixed
      else if (Frveg.gt. 0 .and. Frveg .lt. 1) then
        call Vegflx (VegnEvapFlux)
        MixedEvapFlux = (1-Frveg) * BareEvapFlux + Frveg * VegnEvapFlux
        MixedRadioTemp = (BareRadioTemp**4 * (1-Frveg) + Frveg *        &
                         VegnRadioTemp**4)**0.25

        Evap = MixedEvapFLux
        Otemp = MixedRadioTemp

      end if

    end if

  end if

  Ahum = Qd(1)

  return
end
