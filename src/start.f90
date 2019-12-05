subroutine  START (Obst_Hgt, dual_regime, zo_patch, temp, windsnd, timeloc, wind)
  use simsphere_mod
  implicit none

  real(kind=4) :: Obst_Hgt, zo_patch
  logical :: zo_flag, ob_flag, dual_regime
!  logical :: zo_flag, ob_flag

  character :: Class
  character(len=1) :: ALBEDO_GFLAG, ALBEDO_FFLAG ! For Interface
  character(len=15) :: soiltype
  character(len=30) :: planttype
  integer(kind=1) :: num_soils, num_of_veggies, index_soils, index_veggies
  integer :: i

  type(t_met) :: met
  type(t_timeloc) :: timeloc
  type(t_veg) :: veg
  type(t_wind) :: wind
  type(t_soil) :: soil
  type(t_temp) :: temp
  type(t_windsnd) :: windsnd

  character(len=:), allocatable :: cfg_file

  if (.not. allocated(cfg_file)) then
    allocate(character(len=12) :: cfg_file)
    cfg_file = 'i_model.json'
  end if

  call load_config(cfg_file, met, timeloc, veg, wind, soil, temp, windsnd)

  deallocate(cfg_file)

!        INCLUDE 'modvars.h'

  zo_flag = .false.
  ob_flag = .false.
  Class = 'U'

! ** Take input from JSON file (above), assign existing variables values from 
! data structures

! **  This subroutine reads in the control variables from the input
! **  file.

! **  Read stuff for main program.

  iyr = timeloc%year
  imo = timeloc%month
  iday = timeloc%day
  tz = timeloc%tz
  xlat = timeloc%xlat
  xlong = timeloc%xlong
  strtim = timeloc%strtim
  timend = timeloc%timend
  outtt = timeloc%outtt
  slope = timeloc%slope
  aspect = timeloc%aspect

  f = soil%f
  fsub = soil%fsub
  wmax = soil%wmax
  btemp = soil%btemp
  tp = soil%tp
  dual_ti = soil%dual_ti
  ti_a = soil%ti_a
  ti_b = soil%ti_b
  albedo_gflag = soil%albedo_gflag
  albg = soil%albg
  epsi = soil%epsi
  index_soils = soil%index_soils

  omega = met%omega
  zo = met%zo
  obst_hgt = met%obst_hgt
  cloud_flag = met%cloud_flag
  cld_fract = met%cld_fract

  frveg = veg%frveg
  xlai = veg%xlai
  epsf = veg%epsf
  albedo_fflag = veg%albedo_fflag
  albf = veg%albf
  stmtype = veg%stmtype
  index_veggies = veg%index_veggies
  volrel = veg%volrel
  rmin = veg%rmin
  rcut = veg%rcut
  wilt = veg%wilt
  vegheight = veg%vegheight
  width = veg%width
  steady = veg%steady
  ci = veg%ci
  co = veg%co
  coz_sfc = veg%coz_sfc
  coz_air = veg%coz_air

  if (writeTXT) then
!     Echo the input controls in the output
      WRITE (11,'(/A/)') '         ***  INPUT PARAMETERS  ***'
      WRITE (11,151) 'Year, Month, Day (IYR, IMO, IDAY): ', &
        IYR, IMO, IDAY, &
        'Time Zone (TZ): ',TZ, &
        'Lat/Long (XLAT,XLONG): ',XLAT, XLONG, &
        'Start/End Time (STRTIM, TIMEND): ',STRTIM, TIMEND, &
        'Output Time Interval (OUTTT): ',OUTTT, &
        'Slope and Aspect: ', SLOPE, ASPECT
151   FORMAT (A,I5,I3,I3,/, &
        A,F6.2,/, &
        A,F8.3,',',F8.3,/, &
        A,F6.2,F8.2,/, &
        A,F6.0,/, &
        A,2F6.1)

!     Echo the input controls in the output
      WRITE (11,152) 'Moisture Availability (F): ', F, &
        'Root Zone Moisture Availability (FSUB): ', FSUB
152   FORMAT(A,F5.2,/,A,F5.2)
      WRITE (11,153) 'Substrate Maximum Water (WMAX): ', WMAX, &
        'Lowest Soil Level Temperature (BTEMP): ', BTEMP
153   FORMAT(A,F5.2,/,A,F7.2)
      WRITE (11,154) 'Thermal Inertia (TP): ', TP, &
        'Dual Thermal Inertia Flag and Values (DUAL_TI, TI_A, TI_B): ', &
          DUAL_TI, TI_A, TI_B, &
        'Ground Albedo Flag (ALBEDO_GFLAG): ', ALBEDO_GFLAG, &
        'Ground Albedo (ALBG): ', ALBG, &
        'Emissivity (EPSI): ', EPSI, &
        'Soil Type Index Number (index_soils): ', index_soils
154   FORMAT(A,F5.2,/,A,1X,A,1X,F7.2,F7.2, &
        /,A,1X,A,/,A,F7.2, &
        /,A,F7.2,/,A,I3)

      WRITE (11,155) OMEGA, ZO, OBST_HGT, cloud_flag, cld_fract
155   FORMAT('Precipitable Water Content (OMEGA): ',F5.2, &
        /,'Roughness Height (ZO): ',F7.2, &
        /,'Obstructions Height (OBST_HGT): ',F7.2,/, &
        'Clouds Flag (CLOUD_FLAG): ',L2, &
        /,'Cloud Fraction (CLOUD_FRACT): ',F5.2)

      WRITE (11,156) FRVEG, XLAI, ALBEDO_FFLAG, EPSF, ALBF, STMTYPE, &
        index_veggies, VOLREL, rmin, rcut, WILT, VEGHEIGHT, &
        WIDTH, STEADY, CI, CO , coz_sfc, coz_air
156   FORMAT('Vegetation Percent (FRVEG): ',F5.2,/, &
        'Leaf Area Index (XLAI): ',F7.2,/, &
        'Foliage Emissivity Flag (ALBEDO_FFLAG): ',A,/, &
        'Foliage Emissivity (EPSF): ',F7.2,/, &
        'Foliage Albedo (ALBF): ',F7.2,/, &
        'Stomatal Resistance Scheme (STMTYPE): ',A,/, &
        'Plant Type (index_veggies): ',I3,/, &
        'Relative Water Volume (VOLREL): ',F7.2,/, &
        'Bulk Stomatal Resistance (RMIN): ',F7.2,/, &
        'Cuticular Resistance (RCUT): ',F7.2,/, &
        'Wilting Point (WILT): ',F7.2,/, &
        'Vegetation Height (VEGHEIGHT, STMTYPE=L): ',F7.2,/, &
        'Leaf Width (WIDTH, STMTYPE=L): ',F7.2,/, &
        'Capacitance Flag (STEADY, STMTYPE=L): ',A,/, &
        'Internal CO2 Concentration (CI): ',F7.2,/, &
        'External CO2 Concentration (CO): ',F7.2,/, &
        'Surface Ozone Concentration (COZ_SFC): ',F7.2,/, &
        'Ambient Ozone Concentration (COZ_AIR): ',F7.2)
  end if


! Include the vegetation and soils databases in the calculations.
!

  if(eq(xlai,0.0)) xlai = 1.0
  xlef = 1

  If (gt(FRVEG,0.0) .and. stmtype == 'L') then
    open (1, file = f_soil_lut) ! Begin with opening the file

    Read (1, *) num_soils             ! Required for Interface

    do i = 1, index_soils
      Read (1,*) soiltype, rks, cosbyb, thmax, psis ! Read
    end do

    close (unit = 1)

  if (writeTXT) then
      WRITE(11,166) soiltype, rks, cosbyb, thmax, psis
166   FORMAT(//'   ***  Soil LUT Data Used for this run  ***'// &
        'Soiltype: ',A/ &
        'RKS: ',F6.1/ &
        'CosbyB: ',F6.1/ &
        'THMAX: ',F6.1/ &
        'PSIS: ',F6.1)
  end if


    open (1, file = f_veg_lut) ! Open Veg File

    Read (1, *) num_of_veggies    ! Required for Interface

    do i = 1, index_veggies
      Read (1,*) planttype, rmin, mintemp, maxtemp, beta, b1, b2,       &
                 psice, sc, rcut, zp, frhgt, frzp,                      &
                 rkocap, rccap, rzcap, volini, zstini   ! Read
    end do

    close (unit = 1)

    if (writeTXT) then
      WRITE(11,177) planttype, rmin, mintemp, maxtemp, beta, b1, b2, &
        psice, sc, rcut, zp, frhgt, frzp, rkocap, rccap, rzcap, volini, &
        zstini
177   FORMAT(//'   ***  Plant LUT data used for this run  ***'// &
        'Planttype: ',A/ &
        'RMIN: ',F6.1/ &
        'MINTEMP: ',F6.2/ &
        'MAXTEMP: ',F6.2/ &
        'BETA: ',F6.1/ &
        'B1: ',F6.1/ &
        'B2: ',F6.1/ &
        'PSICE: ',F6.1/ &
        'SC: ',F6.1/ &
        'RCUT: ',F6.1/ &
        'ZP: ',F6.1/ &
        'FRHGT: ',F6.1/ &
        'FRZP: ',F6.1/ &
        'RKOCAP: ',F6.1/ &
        'RCCAP: ',F6.1/ &
        'RZCAP: ',F6.1/ &
        'VOLINI: ',F6.1/ &
        'ZSTINI: ',F6.1)
    end if

    mintemp = mintemp + Celsius_to_Kelvin
    maxtemp = maxtemp + Celsius_to_Kelvin

  end if


! Make conversions

  Btemp = btemp + Celsius_to_Kelvin
  CO = CO*1E-6
  CI = CI*1E-6


! The relevant data for rough.for  08/04/92

  If (eq(frveg,0.0)) Class = 'B'
  If (eq(frveg,100.0)) Class = 'V'
  If (lt(frveg,100.0) .or. gt(frveg,0.0)) Class = 'P'
     
  If (.not. eq(ZO,0.0)) zo_flag = .true.
  If (.not. eq(Obst_Hgt,0.0)) ob_flag = .true.

! End data

! Call the rountine Rough

!        Call RoughCk (class, zo_flag, ob_flag, dual_regime,
!     /                      zo_patch, Obst_Hgt)

! Test a problem with zo_patch being passed to you_start() unitialized
  zo_patch = zo

  return
end
