      SUBROUTINE  NETRAD (Time,BareRadioTemp,VegnRadioTemp,
     /	BareNetRadn,VegnNetRadn,MixedNetRadn,Init)

*/  Subroutine NETRAD computes up and down longwave fluxes and
*/  the Net Radiation.

      Real BareRadioTemp,VegnRadioTemp
      Real BareNetRadn,VegnNetRadn,MixedNetRadn
      Real VegnShortWave
      Real Lwup
      Integer *1 Init

$INCLUDE:'modvars.h'

*/    Compute the solar radiation.

      Call Input

*/  Calculate the net radiation for the appropriate ground conditions.
*/  The decision is made on the fraction of vegetation.

*/  Code added 28/11/90 to solve the Partial routine issue
*/  Initialise and calculate the effective emissivity of the air and
*/  longwave down using a weighted average of surface and air temperature.

      If (Init .eq. 1) Then
        Aepsi = 0.700 + 0.17 * Alog10(Omega)
        if(cloud_flag) aepsi = aepsi + (1 - aepsi)*0.8*(0.1*cld_fract)
        ! Note: forcing real arithmetic

        BareRadioTemp = Tscren - 2
        VegnRadioTemp = Tscren - 2
        Init = 2
      End If

*/ Vegetation

      If (Frveg .eq. 1) Then

	 Call Lwdown
	 Call Vegrad (Time,VegnNetRadn,VegnShortWave,VegnRadioTemp)
	 Rnet = VegnNetRadn
	 Swave = VegnShortWave

*/ Bare Soil

      Elseif (Frveg .eq. 0) Then

		Call Lwdown
		Call Uplong (Lwup,BareRadioTemp)
		BareNetRadn = Lwdn*epsi + Swave - Lwup
		Rnet = BareNetRadn

*/ Mixed

      Elseif (Frveg .gt. 0 .and. Frveg .lt. 1) Then

		Call Lwdown
		Call Uplong (Lwup,BareRadioTemp)
		BareNetRadn = Lwdn*epsi + Swave - Lwup
		Call Vegrad (Time,VegnNetRadn,VegnShortWave,VegnRadioTemp)

		MixedNetRadn = (VegnNetRadn * Frveg) + (1-Frveg) * BareNetRadn
		Rnet = MixedNetRadn
		Swave = VegnShortWave * Frveg + Swave * (1- Frveg)

*/ Error

      Else

c	 Print*, ' Logic Error has occurred in rountine Netrad '
c	 print*, 'frveg = ',frveg, '  frveg-1 = ',(frveg-1)
c	  Print*, ' Stop Program '
c	  Stop
	 continue

      Endif

      Return
      End

      Subroutine  Lwdown

$INCLUDE:'modvars.h'

      Lwdn = aepsi*sigma*(T_fine(3) - Tdif_s - 1.5)**4

      Return
      End

      Subroutine  UpLong (Lwup,SpecTemperature)

      Real SpecTemperature,Lwup,epsi,sigma

      COMMON /KONST/ CP,LE,SIGMA,KARMAN,GRAV,R
      COMMON /NETCOM/ EPSI,EPSF,ALBF,XLAI,SOL,RNETG,RNETF,AEPSI

      Lwup =  epsi*sigma*SpecTemperature**4

      Return
      End
