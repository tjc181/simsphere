module out_mod
  implicit none

  private
  public :: output

contains 

  subroutine  OUTPUT(json, out)
    use globals
    use constants
    use compare, only: eq
    use json_module
    use iso_fortran_env, only: real64
    implicit none
  
  ! Here we finally get around to printing out the variables.
  
    real, parameter :: Undefined = 0.0
    real :: G_Flux=0.0, Bowen=0.0, air_leaf_T=0.0
    real :: PES=0.0, Stom_R=0.0, co2_flux=0.0, ccan_concn=0.0, Water_Use_Eff=0.0
  
    type(json_core) :: json
    type(json_value), pointer :: out
  
  
  !      INCLUDE 'modvars.h'
  
  
  ! Write the Header Information
  
    G_Flux = Rnet - Heat - Evap
    Bowen = Heat/Evap
    If (Bowen < 0.0) Bowen = undefined
  
    If (.not. eq(FRVEG,0.0)) then
      air_leaf_T = TAF - Celsius_to_Kelvin
  !ground_T = TG - Celsius_to_Kelvin
    else
      air_leaf_T = undefined
  !ground_T = undefined
      vfl = undefined
    end if
  
  
    PES = ( XLAI / 2.0 ) + 1
    Stom_R = RS * PES / XLAI
    co2_flux = fco2*1e6
    ccan_concn = ccan*1e6
    Water_Use_Eff = (co2_flux*4.4e-8)/(xlef/le)
  
  
  !TJC  There doesn't seem to be any difference between these three output
  !TJC  cases.  They're intended to serve different purposes, but the same
  !TJC  calculations are done to the same variables during the write().
  
    if (rnet <= 0 .OR. swave <= 0) then
    ! Night
    ! No Vegetation Response
  
    ! Convert outputs to real64 to be compatible with JSON library.  Min/Max
    ! values are 2**-53..2**53 in JSON so the library only supports kind=real64
      call json%add(out,'Time',real(ptime,real64))
      call json%add(out,'Shortwave Flux/Wm-2',real(swave,real64))
      call json%add(out,'Net Radiation/Wm-2',real(rnet,real64))
      call json%add(out,'Sensible Heat Flux/Wm-2',real(heat,real64))
      call json%add(out,'Latent Heat Flux/Wm-2',real(evap,real64))
      call json%add(out,'Ground Flux/Wm-2',real(g_flux,real64))
      call json%add(out,'Air Temperature 50m/C',real(atemp-Celsius_to_Kelvin,real64))
      call json%add(out,'Air Temperature 10m/C',real(ta-Celsius_to_Kelvin,real64))
      call json%add(out,'Air Temperature Foliage/C',real(air_leaf_t,real64))
      call json%add(out,'Radiometric Temperature/C',real(otemp-Celsius_to_Kelvin,real64))
      call json%add(out,'Wind 50m/kts',real(awind*1.98,real64))
      call json%add(out,'Wind 10m/kts',real(uten*1.98,real64))
      call json%add(out,'Wind in foliage/kts',real(uaf*1.98,real64))
      call json%add(out,'Specific_Humidity_50m/gKg-1',real(q_fine(1)*1000,real64))
      call json%add(out,'Specific_Humidity_10m/gKg-1',real(qa*1000,real64))
      call json%add(out,'Specific_Humidity_In_Foliage/gKg-1',real(qaf*1000,real64))
      call json%add(out,'Bowen_Ratio',real(bowen,real64))
      call json%add(out,'Surface_Moisture_Availability',real(f,real64))
      call json%add(out,'Root_Zone_Moisture_Availability',real(fsub,real64))
      call json%add(out,'Stomatal_Resistance/sm-1',real(stom_r,real64))
      call json%add(out,'Vapour_Pressure_Deficit/mbar',real(vfl,real64))
      call json%add(out,'Leaf_Water_Potential/bars',real(psim,real64))
      call json%add(out,'Epidermal_Water_Potential/bars',real(psie,real64))
      call json%add(out,'Ground_Water_Potential/bars',real(psig,real64))
      call json%add(out,'CO2_Flux/micromolesm-2s-1',real(co2_flux,real64))
      call json%add(out,'CO2_Concentration_Canopy/ppmv',real(ccan_concn,real64))
      call json%add(out,'Water_Use_Efficiency',real(water_use_eff,real64))
      call json%add(out,'O3_conc_canopy/ppmv',real(caf,real64))
      call json%add(out,'Global_O3_flux/ugm-2s-1',real(fglobal,real64))
      call json%add(out,'O3_flux_plant/ugm-2s-1',real(flux_plant,real64))
  
    else
    ! Day
      if (heat > 0) then
      ! Convert outputs to real64 to be compatible with JSON library.  Min/Max
      ! values are 2**-53..2**53 in JSON so the library only supports kind=real64
        call json%add(out,'Time',real(ptime,real64))
        call json%add(out,'Shortwave Flux/Wm-2',real(swave,real64))
        call json%add(out,'Net Radiation/Wm-2',real(rnet,real64))
        call json%add(out,'Sensible Heat Flux/Wm-2',real(heat,real64))
        call json%add(out,'Latent Heat Flux/Wm-2',real(evap,real64))
        call json%add(out,'Ground Flux/Wm-2',real(g_flux,real64))
        call json%add(out,'Air Temperature 50m/C',real(atemp-Celsius_to_Kelvin,real64))
        call json%add(out,'Air Temperature 10m/C',real(ta-Celsius_to_Kelvin,real64))
        call json%add(out,'Air Temperature Foliage/C',real(air_leaf_t,real64))
        call json%add(out,'Radiometric Temperature/C',real(otemp-Celsius_to_Kelvin,real64))
        call json%add(out,'Wind 50m/kts',real(awind*1.98,real64))
        call json%add(out,'Wind 10m/kts',real(uten*1.98,real64))
        call json%add(out,'Wind in foliage/kts',real(uaf*1.98,real64))
        call json%add(out,'Specific_Humidity_50m/gKg-1',real(q_fine(1)*1000,real64))
        call json%add(out,'Specific_Humidity_10m/gKg-1',real(qa*1000,real64))
        call json%add(out,'Specific_Humidity_In_Foliage/gKg-1',real(qaf*1000,real64))
        call json%add(out,'Bowen_Ratio',real(bowen,real64))
        call json%add(out,'Surface_Moisture_Availability',real(f,real64))
        call json%add(out,'Root_Zone_Moisture_Availability',real(fsub,real64))
        call json%add(out,'Stomatal_Resistance/sm-1',real(stom_r,real64))
        call json%add(out,'Vapour_Pressure_Deficit/mbar',real(vfl,real64))
        call json%add(out,'Leaf_Water_Potential/bars',real(psim,real64))
        call json%add(out,'Epidermal_Water_Potential/bars',real(psie,real64))
        call json%add(out,'Ground_Water_Potential/bars',real(psig,real64))
        call json%add(out,'CO2_Flux/micromolesm-2s-1',real(co2_flux,real64))
        call json%add(out,'CO2_Concentration_Canopy/ppmv',real(ccan_concn,real64))
        call json%add(out,'Water_Use_Efficiency',real(water_use_eff,real64))
        call json%add(out,'O3_conc_canopy/ppmv',real(caf,real64))
        call json%add(out,'Global_O3_flux/ugm-2s-1',real(fglobal,real64))
        call json%add(out,'O3_flux_plant/ugm-2s-1',real(flux_plant,real64))
      else
      ! Convert outputs to real64 to be compatible with JSON library.  Min/Max
      ! values are 2**-53..2**53 in JSON so the library only supports kind=real64
        call json%add(out,'Time',real(ptime,real64))
        call json%add(out,'Shortwave Flux/Wm-2',real(swave,real64))
        call json%add(out,'Net Radiation/Wm-2',real(rnet,real64))
        call json%add(out,'Sensible Heat Flux/Wm-2',real(heat,real64))
        call json%add(out,'Latent Heat Flux/Wm-2',real(evap,real64))
        call json%add(out,'Ground Flux/Wm-2',real(g_flux,real64))
        call json%add(out,'Air Temperature 50m/C',real(atemp-Celsius_to_Kelvin,real64))
        call json%add(out,'Air Temperature 10m/C',real(ta-Celsius_to_Kelvin,real64))
        call json%add(out,'Air Temperature Foliage/C',real(air_leaf_t,real64))
        call json%add(out,'Radiometric Temperature/C',real(otemp-Celsius_to_Kelvin,real64))
        call json%add(out,'Wind 50m/kts',real(awind*1.98,real64))
        call json%add(out,'Wind 10m/kts',real(uten*1.98,real64))
        call json%add(out,'Wind in foliage/kts',real(uaf*1.98,real64))
        call json%add(out,'Specific_Humidity_50m/gKg-1',real(q_fine(1)*1000,real64))
        call json%add(out,'Specific_Humidity_10m/gKg-1',real(qa*1000,real64))
        call json%add(out,'Specific_Humidity_In_Foliage/gKg-1',real(qaf*1000,real64))
        call json%add(out,'Bowen_Ratio',real(bowen,real64))
        call json%add(out,'Surface_Moisture_Availability',real(f,real64))
        call json%add(out,'Root_Zone_Moisture_Availability',real(fsub,real64))
        call json%add(out,'Stomatal_Resistance/sm-1',real(stom_r,real64))
        call json%add(out,'Vapour_Pressure_Deficit/mbar',real(vfl,real64))
        call json%add(out,'Leaf_Water_Potential/bars',real(psim,real64))
        call json%add(out,'Epidermal_Water_Potential/bars',real(psie,real64))
        call json%add(out,'Ground_Water_Potential/bars',real(psig,real64))
        call json%add(out,'CO2_Flux/micromolesm-2s-1',real(co2_flux,real64))
        call json%add(out,'CO2_Concentration_Canopy/ppmv',real(ccan_concn,real64))
        call json%add(out,'Water_Use_Efficiency',real(water_use_eff,real64))
        call json%add(out,'O3_conc_canopy/ppmv',real(caf,real64))
        call json%add(out,'Global_O3_flux/ugm-2s-1',real(fglobal,real64))
        call json%add(out,'O3_flux_plant/ugm-2s-1',real(flux_plant,real64))
      endif
  
    end if
  
  !10  FORMAT (F5.2,1x,5(F7.2,1X),1x,10(F5.2,1x)                           &
  !            ,1x,F7.3,1x,2(F5.3),1X,F6.1,1x,F5.2,1x,2(F6.2,1x),          &
  !            F6.3,1x,2(F6.2,1x),4(f5.3,1x))
  
  
    return
  end subroutine output

end module out_mod
