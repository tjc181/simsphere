	SUBROUTINE  START (Obst_Hgt,dual_regime,zo_patch)

	Real*4 Obst_Hgt, zo_patch
	logical  zo_flag, ob_flag, dual_regime

	Character*1  ALBEDO_GFLAG, ALBEDO_FFLAG ! For Interface
	Character*15  soiltype
	Character*30  planttype
	Integer*1 num_soils, num_of_veggies, index_soils, index_veggies

        INCLUDE 'modvars.h'

	zo_flag = .false.
	ob_flag = .false.
	Class = 'U'

C **  This subroutine reads in the control variables from the input
C **  file.

	OPEN ( UNIT=9, FILE = 'i_model.dat' ) ! Open file for input
					      ! Closed in Snding.for
c     ^^^^------------------- it should have been, but it wasn't.

C **  Read stuff for main program.


      READ (9,*) IYR, IMO, IDAY, TZ, XLAT, XLONG, STRTIM, TIMEND,
     /		 OUTTT, SLOPE, ASPECT

	READ (9,*) F, FSUB, WMAX, BTEMP, TP, DUAL_TI, TI_A, TI_B,
	/         ALBEDO_GFLAG, ALBG, EPSI, index_soils
	cloud_flag=(1.eq.1)

	READ (9,*) OMEGA, ZO, OBST_HGT, cloud_flag, cld_fract

      READ (9,*) FRVEG, XLAI, EPSF, ALBEDO_FFLAG, ALBF, STMTYPE,
     /		 index_veggies, VOLREL, rmin, rcut, WILT, VEGHEIGHT,
     /		 WIDTH, STEADY, CI, CO , coz_sfc, coz_air

*/ Include the vegetation and soils databases in the calculations.
*/

      if(xlai .eq. 0.0) xlai = 1.0
      xlef = 1

      If (FRVEG .gt. 0.0 .and. stmtype .eq. 'L') then

	open (1, file = 'soils.dat') ! Begin with opening the file

	Read (1, *) num_soils	     ! Required for Interface

	Do 10 i = 1, index_soils
	 Read (1,*) soiltype, rks, cosbyb, thmax, psis ! Read
  10	continue

	close (unit = 1)

	open (1, file = 'veglut.dat') ! Open Veg File

	Read (1, *) num_of_veggies    ! Required for Interface

	Do 20 i = 1, index_veggies
	 Read (1,*) planttype, rmin, mintemp, maxtemp, beta, b1, b2,
     /		    psice, sc, rcut, zp, frhgt, frzp,
     /		    rkocap, rccap, rzcap, volini, zstini   ! Read
  20	continue

	close (unit = 1)

	mintemp = mintemp + 273
	maxtemp = maxtemp + 273

      Endif


 ! Make conversions

      Btemp = btemp + 273.15
      Outtt = outtt * 60
      CO = CO*1E-6
      CI = CI*1E-6


*/ The relevant data for rough.for  08/04/92

	If (frveg .eq. 0.0) Class = 'B'
	If (frveg .eq. 100) Class = 'V'
	If (frveg .lt. 100 .or. frveg .gt. 0.0) Class = 'P'
     
	If (ZO .ne. 0) zo_flag = .true.
	If (Obst_Hgt .ne. 0) ob_flag = .true.

*/ End data

*/ Call the rountine Rough

c	Call RoughCk (class, zo_flag, ob_flag, dual_regime,
c     /		      zo_patch, Obst_Hgt)

	RETURN
	END
