      SUBROUTINE  OUTPUT(No_Rows)

*/ Here we finally get around to printing out the variables.

      integer*1 I_Header, I_Columns
      integer No_Rows


$INCLUDE:'modvars.h'


*/ Write the Header Information

      Data I_Header, Undefined / 1, 0.0 /

      G_Flux = Rnet - Heat - Evap
      Bowen = Heat/Evap
      If (Bowen .lt. 0.0) Bowen = undefined

      If (FRVEG .ne. 0.0) then
	air_leaf_T = TAF - 273.23
*/ground_T = TG - 273.23
      else
	air_leaf_T = undefined
*/ground_T = undefined
	vfl = undefined
      endif


      PES = ( XLAI / 2.0 ) + 1
      Stom_R = RS * PES / XLAI
      co2_flux = fco2*1e6
      ccan_concn = ccan*1e6
      Water_Use_Eff = (co2_flux*4.4e-8)/(xlef/le)


      if (I_Header .eq. 1) then

	I_Columns = 30
	write (11, 4) I_Columns, No_Rows
 4	format(4(I4,1X))

	WRITE (11,*) 'Time ','Shortwave_Flux/Wm-2 ','Net_Radiation/Wm-2 '
	WRITE (11,*) 'Sensible_Heat_Flux/Wm-2 ','Latent_Heat_Flux/Wm-2 '
	WRITE (11,*) 'Ground_Flux/Wm-2 ','Air_Temperature_50m/C '
	WRITE (11,*) 'Air_Temperature_10m/C ','Air_Temperature_Foliage/C '
	WRITE (11,*) 'Radiometric_Temperature/C ','Wind_50_Meters/Kts '
	WRITE (11,*) 'Wind_10_Meters/Kts ', 'Wind_In_Foliage/Kts '
	WRITE (11,*) 'Specific_Humidity_50m/gKg-1 '
	WRITE (11,*) 'Specific_Humidity_10m/gKg-1 '
	WRITE (11,*) 'Specific_Humidity_In_Foliage/gKg-1 ','Bowen_Ratio '
	WRITE (11,*) 'Surface_Moisture_Availability '
	WRITE (11,*) 'Root_Zone_Moisture_Availability '
	WRITE (11,*) 'Stomatal_Resistance/sm-1 '
	WRITE (11,*) 'Vapour_Pressure_Deficit/mbar '
	WRITE (11,*) 'Leaf_Water_Potential/bars '
	WRITE (11,*) 'Epidermal_Water_Potential/bars '
	WRITE (11,*) 'Ground_Water_Potential/bars '
	WRITE (11,*) 'CO2_Flux/micromolesm-2s-1 '
	WRITE(11,*)'CO2_Concentration_Canopy/ppmv ','Water_Use_Efficiency'
	write(11,*)'O3_conc_canopy/ppmv ','Global_O3_flux/ugm-2s-1'
	write (11,*) 'O3_flux_plant/ugm-2s-1 '

	I_Header = 2

      endif

*/ Default data file (Primy.dat)

       IF (RNET .LE. 0 .OR. SWAVE .LE. 0) THEN

*/ Night
*/ No Vegetation Response


	 WRITE (11,10) PTIME,SWAVE,RNET,HEAT,EVAP,G_Flux,
     /	      atemp-273.23,ta-273.23,air_leaf_T,OTEMP-273.23,
     /	      awind*1.98, uten*1.98, uaf*1.98,
     /	      Q_Fine(1)*1000, QA*1000, QAF*1000,
     /	      Bowen, F, FSUB, Stom_R, vfl, psim, psie, psig,
     /	      co2_flux, ccan_concn, Water_Use_Eff,caf,fglobal,
	/		  flux_plant		          
       ELSE

*/ Day

       If (heat .gt. 0) then

	 WRITE (11,10) PTIME,SWAVE,RNET,HEAT,EVAP,G_Flux,
     /	      atemp-273.23,ta-273.23,air_leaf_T,OTEMP-273.23,
     /	      awind*1.98, uten*1.98, uaf*1.98,
     /	      Q_Fine(1)*1000, QA*1000, QAF*1000,
     /	      Bowen, F, FSUB, Stom_R, vfl, psim, psie, psig,
     /	      co2_flux, ccan_concn, Water_Use_Eff,caf,fglobal,
	/		  flux_plant
       else

	 WRITE (11,10) PTIME,SWAVE,RNET,HEAT,EVAP,G_Flux,
     /	      atemp-273.23,ta-273.23,air_leaf_T,OTEMP-273.23,
     /	      awind*1.98, uten*1.98, uaf*1.98,
     /	      Q_Fine(1)*1000, QA*1000, QAF*1000,
     /	      Bowen, F, FSUB, Stom_R, vfl, psim, psie, psig,
     /	      co2_flux, ccan_concn, Water_Use_Eff,caf,fglobal,
	/		  flux_plant
       endif

      END IF

  10  FORMAT (F5.2,1x,5(F7.2,1X),1x,10(F5.2,1x)
     /	      ,1x,F7.3,1x,2(F5.3),1X,F6.1,1x,F5.2,1x,2(F6.2,1x),
     /	      F6.3,1x,2(F6.2,1x),4(f5.3,1x))


      RETURN
      END
