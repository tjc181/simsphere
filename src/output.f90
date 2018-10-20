subroutine  OUTPUT(No_Rows)
  use simsphere_mod
  use json_module
  use iso_fortran_env, only: real64
  implicit none

! Here we finally get around to printing out the variables.

  integer(kind=1) :: I_Header = 1, I_Columns = 0
  integer :: No_Rows

  real, parameter :: Undefined = 0.0
  real :: G_Flux=0.0, Bowen=0.0, air_leaf_T=0.0
  real :: PES=0.0, Stom_R=0.0, co2_flux=0.0, ccan_concn=0.0, Water_Use_Eff=0.0

  type(json_core) :: json
  type(json_value), pointer :: p, out

  character(len=:), allocatable :: out_file


  call json%initialize(compact_reals=.true., real_format='*', allow_duplicate_keys=.true.)
  call json%create_object(p,'')
  call json%create_object(out,'output')
  call json%add(p,out)



!      INCLUDE 'modvars.h'


! Write the Header Information

  G_Flux = Rnet - Heat - Evap
  Bowen = Heat/Evap
  If (Bowen .lt. 0.0) Bowen = undefined

  If (FRVEG .ne. 0.0) then
    air_leaf_T = TAF - 273.23
!ground_T = TG - 273.23
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


  if (I_Header .eq. 1) then

    I_Columns = 30
    write (11, 4) I_Columns, No_Rows
4   format(4(I4,1X))

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
    WRITE(11,*) 'CO2_Concentration_Canopy/ppmv ','Water_Use_Efficiency'
    write(11,*)'O3_conc_canopy/ppmv ','Global_O3_flux/ugm-2s-1'
    write (11,*) 'O3_flux_plant/ugm-2s-1 '

    I_Header = 2

  end if

! Default data file (Primy.dat)

  IF (RNET .LE. 0 .OR. SWAVE .LE. 0) THEN

! Night
! No Vegetation Response

    WRITE (11,10) PTIME,SWAVE,RNET,HEAT,EVAP,G_Flux,                    &
                  atemp-273.23,ta-273.23,air_leaf_T,OTEMP-273.23,       &
                  awind*1.98, uten*1.98, uaf*1.98,                      &
                  Q_Fine(1)*1000, QA*1000, QAF*1000,                    &
                  Bowen, F, FSUB, Stom_R, vfl, psim, psie, psig,        &
                  co2_flux, ccan_concn, Water_Use_Eff,caf,fglobal,      &
                  flux_plant

! Convert outputs to real64 to be compatible with JSON library.  Min/Max
! values are 2**-53..2**53 in JSON so the library only supports kind=real64
    call json%add(out,'Time',real(ptime,real64))
    call json%add(out,'Shortwave Flux/Wm-2',real(swave,real64))
    call json%add(out,'Net Radiation/Wm-2',real(rnet,real64))
    call json%add(out,'Sensible Heat Flux/Wm-2',real(heat,real64))
    call json%add(out,'Latent Heat Flux/Wm-2',real(evap,real64))
    call json%add(out,'Ground Flux/Wm-2',real(g_flux,real64))
    call json%add(out,'Air Temperature 50m/C',real(atemp-273.23,real64))
    call json%add(out,'Air Temperature 10m/C',real(ta-273.23,real64))
    call json%add(out,'Air Temperature Foliage/C',real(air_leaf_t,real64))
    call json%add(out,'Radiometric Temperature/C',real(otemp-273.23,real64))
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
  ELSE

! Day

    If (heat .gt. 0) then
      WRITE (11,10) PTIME,SWAVE,RNET,HEAT,EVAP,G_Flux,                  &
                    atemp-273.23,ta-273.23,air_leaf_T,OTEMP-273.23,     &
                    awind*1.98, uten*1.98, uaf*1.98,                    &
                    Q_Fine(1)*1000, QA*1000, QAF*1000,                  &
                    Bowen, F, FSUB, Stom_R, vfl, psim, psie, psig,      &
                    co2_flux, ccan_concn, Water_Use_Eff,caf,fglobal,    &
                    flux_plant
    else

      WRITE (11,10) PTIME,SWAVE,RNET,HEAT,EVAP,G_Flux,                  &
                    atemp-273.23,ta-273.23,air_leaf_T,OTEMP-273.23,     &
                    awind*1.98, uten*1.98, uaf*1.98,                    &
                    Q_Fine(1)*1000, QA*1000, QAF*1000,                  &
                    Bowen, F, FSUB, Stom_R, vfl, psim, psie, psig,      &
                    co2_flux, ccan_concn, Water_Use_Eff,caf,fglobal,    &
                    flux_plant
    endif

  END IF

10  FORMAT (F5.2,1x,5(F7.2,1X),1x,10(F5.2,1x)                           &
            ,1x,F7.3,1x,2(F5.3),1X,F6.1,1x,F5.2,1x,2(F6.2,1x),          &
            F6.3,1x,2(F6.2,1x),4(f5.3,1x))

  if (.not. allocated(out_file)) then
    allocate(character(len=12) :: out_file)
    out_file = 'o_model.json'
  end if

! write the file
  call json%print(p,'o_model.json')

! cleanup
  deallocate(out_file)
  nullify(out)
  call json%destroy(p)
  if (json%failed()) stop 1

  return
end
