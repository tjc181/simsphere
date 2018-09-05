      subroutine  sslope(sinsolelslope,solelslope,effdec,slb,hrangl)

      real sinsolelslope,solelslope,dipan,dipaz

      INCLUDE 'modvars.h'

*/ subroutine calculates solar elevation angle and azimuth 
*/ for sloping terrain when slope is non-zero.
*/ elevation for northwest corner of square box is ZNW, etc.
*/ grid size (called xmeshl) is in same units as ZNW

*/ calculate dip angle and azimuth angle of sloping surface plane

c      dipaz = 0.0
c      dipan = 0.0

c      edip = (ZNW + ZSW - ZNE - ZSE)/2.0
c      sdip = (ZNW - ZSW + ZNE - ZSE)/2.0

c      if ( edip. ne. 0 ) then

c	dipan = abs (atan (edip / xmeshl / (cos(atan(sdip/edip)))))
c	dipaz = 3. * 3.14159/ 2. + atan( sdip / edip)

c	if ( edip. lt. 0. ) then
c	  dipaz = dipaz - 3.14159
c	end if

c      else

c	dipan = abs (atan (sdip / xmeshl ))
c	dipaz = 3.14159

c	  if ( sdip. gt. 0. ) then
c	    dipaz = 0.0
c	  end if

c	end if

       dipaz = ASPECT / 57.2958

c	if (dipaz. gt. 6.2835) then
c	  azimuthangle = azimuthangle  - 360.
c	endif

       dipan = SLOPE / 57.2958

*/  compute solar elevation angle function for sloping terrain

      sinsolelslope = cos(dipan) * ( sin(slb) * sin(effdec) + cos(slb) *
     / cos(effdec) * cos(hrangl)) + sin(dipan) * ( cos(dipaz) *
     / ( tan(slb) * ( sin(slb) * sin(effdec) + cos(slb) * cos(effdec)
     / * cos(hrangl)) - sin(effdec) / cos(slb)) + sin(dipaz) *
     / cos(effdec) * sin(hrangl))

       solelslope = asin(sinsolelslope)

       if ( solelslope .LE. 0.01) then
        solelslope = 0.01
       end if

       call albedo(sinsolelslope)

*/   note that solar elevation can be less than zero for non-zero slope
*/   although greater than zero for flat slope.
*/   we use solar elevation for slope in albedo but not for path
*/   which depends on solar angle with respect to flat plane

       return
       end
