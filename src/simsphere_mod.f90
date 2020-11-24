module simsphere_mod
  use config_mod, only: t_met, t_timeloc, t_veg, t_wind, t_soil, t_temp,    &
                        t_windsnd, load_config
  use constants
  use globals
  use snding_mod, only: splint, spline
  use transm_mod, only: ftabsT, ftscatT, fbscatT, ABSTBL, SCATBL, BSCTBL, PS1
  use vel_mod, only: You_star, R_ohms, WindF, Stab, StabH, FStabH, FStabM, ResTrn, MOL
  use stomata_mod, only: stomc, stomfs, stomrs
  use compare, only: eq, gt, lt
  use json_module
  use iso_fortran_env, only: real64, error_unit
  implicit none
!  private
  public :: advect 
  public :: co2flx
  public :: cond
  public :: output 
  public :: ozone
  public :: psgcal
!  public :: smooth
  public :: veghot 

!
! Simsphere module provides subroutines and functions implementing the model
! components.  It also provides constants, data structures, and global variables.
!
! This module was originally three "header" files: constants.h, factors.h, and 
! modvars.h.  These files were used via an INCLUDE (or, originally, $INCLUDE for a
! suspected DEC compiler).  The contents have been collected into this module in
! an initial effort to modernize the code.
!



  contains
    
!
! advect function replaces ADVECT subroutine
!

    real pure function advect ()
      implicit none

      real, parameter :: dz = 1000
      real :: dtdx, dtdy

      dtdx = cf * otemp / (grav * dz) * (vgd(5) - vgd(1))
      dtdy = -cf * otemp / (grav * dz) * (ugd(5) - ugd(1))
      advect = (-(ugd(3) * dtdx + vgd(3) * dtdy))/2

    end function advect

!
! cond function replaces COND subroutine
!

    real pure function cond ()
      implicit none

      !  program units are in m/s
      !  Use Field Capacity water content 75% that of THMAX.

      cond = (6.9E-6) * RKS * (THV / (THMAX*0.75)) ** (2*COSBYB +2)

    end function cond

!
! OUTPUT is called on every iteration of main loop.  It performs final
! calculations and conversions before adding output data to JSON object.
!

    subroutine  output(json, out, outputinit)
      implicit none
    
    ! Here we finally get around to printing out the variables.
    
      real, parameter :: Undefined = 0.0
      real :: G_Flux=0.0, Bowen=0.0, air_leaf_T=0.0
      real :: PES=0.0, Stom_R=0.0, co2_flux=0.0, ccan_concn=0.0, Water_Use_Eff=0.0

      integer :: outputinit
    
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

      if (outputinit .eq. 1) then

        if (writeTXT) then
        WRITE (11,*) 'TIME: Local time (hours)'
        WRITE (11,*) 'SWF: Shortwave_Flux (W m-2)'
        WRITE (11,*) 'NETRAD: Net_Radiation (W m-2)'
        WRITE (11,*) 'SENS: Sensible_Heat_Flux (W m-2)'
        WRITE (11,*) 'LE: Latent_Heat_Flux (W m-2)'
        WRITE (11,*) 'GRF: Ground_Flux (W m-2)'
        WRITE (11,*) 'T50: Air_Temperature_50m (C)'
        WRITE (11,*) 'T10: Air_Temperature_10m (C)'
        WRITE (11,*) 'TFOL: Air_Temperature_Foliage (C)'
        WRITE (11,*) 'TRAD: Radiometric_Temperature (C)'
        WRITE (11,*) 'W50: Wind_50_Meters (Kts)'
        WRITE (11,*) 'W10: Wind_10_Meters (Kts)'
        WRITE (11,*) 'WFOL: Wind_In_Foliage (Kts)'
        WRITE (11,*) 'SH50: Specific_Humidity_50m (g Kg-1)'
        WRITE (11,*) 'SH10: Specific_Humidity_10m (g Kg-1)'
        WRITE (11,*) 'SHFOL: Specific_Humidity_In_Foliage (g Kg-1)'
        WRITE (11,*) 'BOW: Bowen_Ratio'
        WRITE (11,*) 'SMA: Surface_Moisture_Availability'
        WRITE (11,*) 'RZMA: Root_Zone_Moisture_Availability'
        WRITE (11,*) 'STMR: Stomatal_Resistance (s m-1)'
        WRITE (11,*) 'VPDEF: Vapour_Pressure_Deficit (mbar)'
        WRITE (11,*) 'LWPOT: Leaf_Water_Potential (bars)'
        WRITE (11,*) 'EWPOT: Epidermal_Water_Potential (bars)'
        WRITE (11,*) 'GWPOT: Ground_Water_Potential (bars)'
        WRITE (11,*) 'CO2F: CO2_Flux (micromoles m-2 s-1)'
        WRITE (11,*) 'CO2CAN: CO2_Concentration_Canopy (ppmv)'
        WRITE (11,*) 'WATEFF: Water_Use_Efficiency'
        WRITE (11,*) 'O3CAN: O3_conc_canopy (ppmv)'
        WRITE (11,*) 'GBLO3: Global_O3_flux (ug m-2 s-1)'
        WRITE (11,*) 'O3F: O3_flux_plant (ug m-2 s-1)'

        WRITE (11,9)
9       FORMAT(/' TIME    SWF   NETRAD    SENS    LE      GRF', &
          '    T50   T10', &
          '   TFOL TRAD', &
          '   W50   W10   WFOL  SH50  SH10 SHFOL ', &
          '    BOW   SMA  RZMA', &
          '    STMR  VPDEF  LWPOT EWPOT   GWPOT     CO2F ', &
          ' CO2CAN', &
          '    WATEFF   O3CAN  GBLO3   O3F'/)
        endif

        outputinit = 2

      endif

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

        if (writeTXT) then
         WRITE (11,10) PTIME,SWAVE,RNET,HEAT,EVAP,G_Flux, &
              atemp-273.15,ta-273.15,air_leaf_T,OTEMP-273.15, &
              awind*1.98, uten*1.98, uaf*1.98, &
              Q_Fine(1)*1000, QA*1000, QAF*1000, &
              Bowen, F, FSUB, Stom_R, vfl, psim, psie, psig, &
              co2_flux, ccan_concn, Water_Use_Eff,caf,fglobal, &
              flux_plant
        endif
    
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

         if (writeTXT) then
         WRITE (11,10) PTIME,SWAVE,RNET,HEAT,EVAP,G_Flux, &
              atemp-273.15,ta-273.15,air_leaf_T,OTEMP-273.15, &
              awind*1.98, uten*1.98, uaf*1.98, &
              Q_Fine(1)*1000, QA*1000, QAF*1000, &
              Bowen, F, FSUB, Stom_R, vfl, psim, psie, psig, &
              co2_flux, ccan_concn, Water_Use_Eff,caf,fglobal, &
              flux_plant
         end if

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

         if (writeTXT) then
         WRITE (11,10) PTIME,SWAVE,RNET,HEAT,EVAP,G_Flux, &
              atemp-273.15,ta-273.15,air_leaf_T,OTEMP-273.15, &
              awind*1.98, uten*1.98, uaf*1.98, &
              Q_Fine(1)*1000, QA*1000, QAF*1000, &
              Bowen, F, FSUB, Stom_R, vfl, psim, psie, psig, &
              co2_flux, ccan_concn, Water_Use_Eff,caf,fglobal, &
              flux_plant
         end if

        endif
    
      end if
    
  10  FORMAT (F5.2,1X,5(F7.2,1X),1X,10(F5.2,1X) &
              ,1X,F7.3,1X,F5.3,1X,F5.3,1X,F6.1,1X,F5.2,1X,2(F6.2,1X), &
              G9.3E1,1X,F7.3,1X,F6.2,2X,G9.3E1,1X,3(F6.3,1X))

      return
    end subroutine output

! VEGHOT

    subroutine  VEGHOT (B,Heatv)
!      use simsphere_mod, only: dens, cp, chf, tf, taf, lambda, tt, z, hg, rnetg, &
!                               xleg, chg, hf
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
    end subroutine VEGHOT

!    subroutine average(T_Unsmoothed, T_smoothed)
!      implicit none
!    
!      real :: av_array(4)=0.0, sum_array=0.0, T_Unsmoothed, T_smoothed
!      integer :: init=1, i=0, j=0, k=0
!    
!    !  data init /1/
!    
!      if (init == 1) then ! fill all 4 elements with initial value of otemp
!    
!        do i = 1,4
!          av_array(i) = T_Unsmoothed
!        end do
!      
!        init = 2
!    
!      else
!    
!        do j = 2,4
!          av_array(j-1) = av_array(j)
!        end do
!    
!        av_array(4) = T_Unsmoothed
!    
!      endif
!    
!      sum_array = 0
!      do k = 1,4
!        sum_array = av_array(k) + sum_array
!      end do
!    
!      T_smoothed = sum_array / 4
!    
!      return
!    end subroutine average

! smooth() is a replacement function for AVERAGE.

!    real pure function smooth(unsmooth)
!      real, intent(in) :: unsmooth
!      real :: av_array(4), sum_array
!      integer :: init, i, j, k
!
!      init = 1
!      av_array(4) = 0.0
!      sum_array = 0.0
!
!      !TJC When will init not be 1?  Never...
!      if (init == 1) then ! fill all 4 elements with initial value of otemp
!        do i = 1,4
!          av_array(i) = unsmooth
!        end do
!        init = 2
!
!      else
!        do j = 2,4
!          av_array(j-1) = av_array(j)
!        end do
!        av_array(4) = unsmooth
!      end if
!
!      sum_array = 0
!      do k = 1,4
!        sum_array = av_array(k) + sum_array
!      end do
!      smooth = sum_array/4
!
!    end function smooth

! psgcal() is a function to replace subroutine PSGCAL

    real pure function psgcal(thmax,thv,cosbyb,psis)
      real, intent(in) :: thmax, thv, cosbyb, psis
      real :: perwmax, perw2g, rlogpsig, rnpsig

    ! **  Calculates the conductivity of the soil
    ! **  Cosby curves and coefficients (1984)
    
    !  Field Capacity(75% of THMAX) used instead of THMAX.  Lower value
    !  felt to fit local measurements better than the fit with tabulated
    !  values.
    
    ! **  convert ground water contents to percents

      perwmax = thmax * 100 * 0.75
      perw2g = thv * 100
      rlogpsig = alog10(psis) + cosbyb * alog10(perwmax) - cosbyb * alog10(perw2g)
    ! * * psig is positive in this program
      rnpsig = 10**(rlogpsig)

    ! * * convert cm to bars
      psgcal = -rnpsig / 1020

    end function psgcal

! PSGCAL

!    subroutine  psgcal
!      implicit none
!    
!      real :: perwmax, perw2g, rlogpsig, rnpsig
!    
!    !	REAL RLOGPSIL
!    
!    !      INCLUDE 'modvars.h'
!    
!    ! **  Calculates the conductivity of the soil
!    ! **  Cosby curves and coefficients (1984)
!    
!    !  Field Capacity(75% of THMAX) used instead of THMAX.  Lower value
!    !  felt to fit local measurements better than the fit with tabulated
!    !  values.
!    
!    ! **  convert ground water contents to percents
!    
!      PERWMAX = THMAX * 100 * 0.75
!      PERW2G = THV * 100
!      RLOGPSIG = ALOG10 ( PSIS ) + COSBYB * ALOG10 ( PERWMAX )              &
!                 - COSBYB * ALOG10 ( PERW2G )
!    
!    ! * * psig is positive in this program
!    
!      RNPSIG = 10 ** ( RLOGPSIG )
!    
!    
!    ! * * convert cm to bars
!    
!      PSIG = -RNPSIG / 1020
!    
!      return
!    end subroutine psgcal
    
! OZONE

    subroutine  OZONE
      implicit none
    
      real :: CHAX, PES, RAIR, rag, RROz, the_time, rtot
      real :: fleaf, fmeso, fg, fbare
     
    !       INCLUDE 'modvars.h'
    
    !      created 4/5/96; MODIFIED 
    !      coz_sfc=  0. suggested value for c4 plants
    !      coz_air  50 m above the canopy
    
      CHAX = USTAR**2 / ( UTEN - UAF )
    
      PES  = ( XLAI / 2.0 ) + 1.
      RAIR =  1.0 / CHAX + RZASCR
     
      rag = 1 / chg
      RROz = 1.9
           
    
    !     
    !     RROz IS DENSITY OF OZONE (KG M-3) AT 20 C
    ! 
    !          *****    ONE MOLE OF OZONE IS 48 GRAMS    *****

    !       We don;t apply 1.32 correction for fbare or rair_bare
    
    !        SUBROUTINE ozone COMPUTES OZONE FLUXES, ozone concentration in canopy
    !         fluxes: from air to leaf Fleaf
    !                 from air to stomatal cavities Fm
    !                 from air to ground Fg
    !  Total ozone flux from air is sum of above three
    !   Total ozone flux from air to leaf is sum of first two
    !      Flux from air to leaf surface scaled by lai only - 2 sides of leaf
    !      Flux from air to interior of leaf scaled by lai /pes one sided stomates
    !     Concentration of ozone in canopy CO3af depends on all three fluxes
    
    !     WE FOLLOW CLOSELY THE SUBROUTINE CO2FLX IN PSUBAMS.
    !     Ozone diffusivity approximately the same as that of CO2
    !           Use same resistances as those for CO2 flux
    ! 
    
    !       surface OZONE CONCENTRATION ON LEAF; assume = 0
    !      - COz_air IS AMBIENT OZONE CONCENTRATION ABOVE CANOPY
    !     This is typically 0.04 to 0.12 ppmv 
    !    MULTIPLY CONCENTRATIONS BY 10-6 TO GET proper units kg / m2 /s
    !    Fluxes typically order of 0.2 micrograms per meter square per second
    
    !   RESISTANCES FOR STOMATAL RESISTANCE (RST)
    !                   LEAF BOUNDARY LAYER (RAF)
    !                   TOP OF CANOPY (1/CHAX) TO 2 METERS
    !                   SCREEN LEVEL (2 M) TO 50 METERS (RZASCR)
    !   
    !       LEAF BOUNDARY LAYER RESISTANCE RAF CORRECTED FOR OZONE MOLECULAR
    !                   DIFFUSIVITY  (S M-1): FACTOR OF 1.32
    !   stomatal resistance scaled by 1.66 as in co2 flux
    !   
      the_time = ptime
    
            
    
    !    rair governs flux from above canopy to interior outside leaf boundary     
    
      rtot = 1 / ( 1 / ((raf + rcut) * 1.32 /(2 * xlai))  + 1 /(raf         &
             * 1.32 * pes /xlai + rst  * 1.66 * pes / xlai ) + 1 / ( rag * 1.32 ))
              
      caf = coz_air / ( 1 + rair / rtot)
     
      fleaf = rroz * caf / (( raf + rcut) * 1.32 / (2 * xlai )) *1e3
    !       Fleaf is flux of ozone to leaf, two sides
    
      fmeso = rroz*caf/((1.32*raf*pes/xlai)+(1.66 * rst * pes / xlai))
      fmeso=fmeso * 1e3   
    !     Fmeso is flux of ozone into stomates
         
      fg = rroz* caf / (rag * 1.32) *1e3
    !        Fg is flux of ozone into ground
    
      flux_plant = fleaf + fmeso
    !        Flux_ plant is total flux into plant: leaf and stomate
    
    !        caf is concentration of ozone in canopy, ppmv
               
    !         No adjustment made for fractional vegetation cover
     
      fbare = rroz* coz_air / sumo3 *1e3
    !       Fbare is the flux of ozone over bare soil fraction
    
      fglobal = (flux_plant+ fg) * frveg + fbare* (1 - frveg)
    !       Fglobal is total flux weighted for vegetation and bare soil fractions
    
    !       UNITS OF FLUX  (FOZONE) ARE KG (OZONE) PER METER SQUARED PER SECOND
    !         IN CONCENTRATION (Coz_air)  PARTS VOLUME PER MILLION (VPM)
    !         Multiply by 10**9 to get in micrograms per meter per second
    !         
    
          
    !     
      return
    end subroutine ozone

! CO2FLX

    subroutine  CO2FLX
      implicit none
    
      real :: CHAX, PEX, RAIR, RROE, RAFCANOPY, RRTOT
    
    !       INCLUDE 'modvars.h'
    
    !      created 11/2/90; MODIFIED 06/17/91
    
    !      CI =  120. suggested value for c4 plants
    !      CI =  210  suggested value for c3 plants
    !      CO =  330  50 m above the canopy
    
      CHAX = USTAR**2 / ( UTEN - UAF )
    
      PEX  = ( XLAI / 2.0 ) + 1.
      RAIR =  1.0 / CHAX + RZASCR
      RROE =  1.83
      RAFCANOPY = RAF * PEX / XLAI
    
    !    RAFCANOPY IS RAF SCALED FOR XLAI TO A CANOPY
    !    RROE IS DENSITY OF PURE CARBON DIOXIDE (KG M-3) AT 20 C
    !    SUBROUTINE COMPUTES CARBON DIOXIDE FLUXES
    !    WE FOLLOW CLOSELY GOUDRIAAN'S BOOK, CHAPTER 3
    !      (CROP MICROMETEOROLOGY:  A SIMULATION STUDY; 1977)
    
    !    CI IS INTERNAL CO2 CONCENTRATION IN LEAF
    !      (120 VPM FOR C4 PLANT (CORN); 220 FOR C3 PLANT (BEANS))
    !      - CO IS AMBIENT CO2 CONCENTRATION ABOVE CANOPY - - 330 VPM
    !    MULTIPLY CONCENTRATIONS BY 10-6 TO GET FRACTIONAL AMOUNT
    
    !   RESISTANCES FOR STOMATAL RESISTANCE (RST)
    !                   LEAF BOUNDARY LAYER (RAF)
    !                   TOP OF CANOPY (1/CHAX) TO 2 METERS
    !                   SCREEN LEVEL (2 M) TO 50 METERS (RZASCR)
    !   LEAF BOUNDARY LAYER RESISTANCE RAF CORRECTED FOR CO2 MOLECULAR
    !                   DIFFUSIVITY  (S M-1): FACTOR OF 1.32
    !   STOMATAL RESISTANCE CORRECTED FOR CO2 DIFFUSIVITY (FACTOR OF 1.66)
    !          CALCULATE FLUX OF CO2 TO ENVIRONMENT
    !          FACTORS 1.32 AND 1.66 ADJUST VALUES FROM H20 TO CO2.
    
      RRTOT = 1.32 * RAFCANOPY + 1.66*RST + RAIR
    
      FCO2 = RROE * ( CO - CI )  / RRTOT
    
      FCO2 = (FCO2 * FRVEG) / 0.044 ! In moles/m2/s
    !
    !      CALCULATE CO2 FLUX IN CANOPY FCO2; ADJUST FOR
    !         PARTIAL VEGETATION FRACTION FRVEG
    !      UNITS OF FLUX  (FCO2) ARE KG (CO2) PER METER SQUARED PER SECOND
    !         IN CONCENTRATION (CCAM)  PARTS VOLUME PER MILLION (VPM)
    
    !         **** NOTE SOME PAPERS LIST CO2 FLUX IN MOLES PER M2 PER S ****
    !
    !         *****    ONE MOLE OF CO2 IS 46 GRAMS    *****
    
      CCAN  =  CO - ( CO - CI ) * RAIR * FRVEG / RRTOT
    
    !      CALCULATE CO2 CONCENTRATION JUST INSIDE CANOPY - - CCAN
    
      return
    end subroutine co2flx


end module simsphere_mod
