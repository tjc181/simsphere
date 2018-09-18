!subroutine  START (Obst_Hgt,dual_regime,zo_patch)
! dual_regime and zo_patch are not currently used.  Comment at end of this
! file suggests planned data read checks, not currently implemented.
subroutine  START (Obst_Hgt)
  use simsphere_mod

!  real(kind=4) :: Obst_Hgt, zo_patch
  real(kind=4) :: Obst_Hgt
! logical :: zo_flag, ob_flag, dual_regime
  logical :: zo_flag, ob_flag

  character :: Class
  character(len=1) :: ALBEDO_GFLAG, ALBEDO_FFLAG ! For Interface
  character(len=15) :: soiltype
  character(len=30) :: planttype
  integer(kind=1) :: num_soils, num_of_veggies, index_soils, index_veggies

!        INCLUDE 'modvars.h'

  zo_flag = .false.
  ob_flag = .false.
  Class = 'U'

! **  This subroutine reads in the control variables from the input
! **  file.

  OPEN ( UNIT=9, FILE = f_control ) ! Open file for input
                                              ! Closed in Snding.for
!     ^^^^------------------- it should have been, but it wasn't.

! **  Read stuff for main program.


  READ (9,*) IYR, IMO, IDAY, TZ, XLAT, XLONG, STRTIM, TIMEND,           &
             OUTTT, SLOPE, ASPECT

  READ (9,*) F, FSUB, WMAX, BTEMP, TP, DUAL_TI, TI_A, TI_B,             &
             ALBEDO_GFLAG, ALBG, EPSI, index_soils
  cloud_flag=(1.eq.1)

  READ (9,*) OMEGA, ZO, OBST_HGT, cloud_flag, cld_fract

  READ (9,*) FRVEG, XLAI, EPSF, ALBEDO_FFLAG, ALBF, STMTYPE,            &
             index_veggies, VOLREL, rmin, rcut, WILT, VEGHEIGHT,        &
             WIDTH, STEADY, CI, CO , coz_sfc, coz_air

! Include the vegetation and soils databases in the calculations.
!

  if(xlai .eq. 0.0) xlai = 1.0
  xlef = 1

  If (FRVEG .gt. 0.0 .and. stmtype .eq. 'L') then
    open (1, file = f_soil_lut) ! Begin with opening the file

    Read (1, *) num_soils             ! Required for Interface

    do i = 1, index_soils
      Read (1,*) soiltype, rks, cosbyb, thmax, psis ! Read
    end do

    close (unit = 1)

    open (1, file = f_veg_lut) ! Open Veg File

    Read (1, *) num_of_veggies    ! Required for Interface

    do i = 1, index_veggies
      Read (1,*) planttype, rmin, mintemp, maxtemp, beta, b1, b2,       &
                 psice, sc, rcut, zp, frhgt, frzp,                      &
                 rkocap, rccap, rzcap, volini, zstini   ! Read
    end do

    close (unit = 1)

    mintemp = mintemp + 273
    maxtemp = maxtemp + 273

  end if


! Make conversions

  Btemp = btemp + Celsius_to_Kelvin
  Outtt = outtt * 60
  CO = CO*1E-6
  CI = CI*1E-6


! The relevant data for rough.for  08/04/92

  If (frveg .eq. 0.0) Class = 'B'
  If (frveg .eq. 100) Class = 'V'
  If (frveg .lt. 100 .or. frveg .gt. 0.0) Class = 'P'
     
  If (ZO .ne. 0) zo_flag = .true.
  If (Obst_Hgt .ne. 0) ob_flag = .true.

! End data

! Call the rountine Rough

!        Call RoughCk (class, zo_flag, ob_flag, dual_regime,
!     /                      zo_patch, Obst_Hgt)

  return
end
