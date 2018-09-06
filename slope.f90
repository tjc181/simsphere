subroutine  sslope(sinsolelslope,solelslope,effdec,slb,hrangl)
  use simsphere_mod

  real :: sinsolelslope,solelslope,dipan,dipaz

!      INCLUDE 'modvars.h'

! subroutine calculates solar elevation angle and azimuth 
! for sloping terrain when slope is non-zero.
! elevation for northwest corner of square box is ZNW, etc.
! grid size (called xmeshl) is in same units as ZNW

! calculate dip angle and azimuth angle of sloping surface plane

!      dipaz = 0.0
!      dipan = 0.0

!      edip = (ZNW + ZSW - ZNE - ZSE)/2.0
!      sdip = (ZNW - ZSW + ZNE - ZSE)/2.0

!      if ( edip. ne. 0 ) then

! dipan = abs (atan (edip / xmeshl / (cos(atan(sdip/edip)))))
! dipaz = 3. * 3.14159/ 2. + atan( sdip / edip)

! if ( edip. lt. 0. ) then
!   dipaz = dipaz - 3.14159
! end if

!      else

! dipan = abs (atan (sdip / xmeshl ))
! dipaz = 3.14159

!   if ( sdip. gt. 0. ) then
!     dipaz = 0.0
!   end if

! end if

  dipaz = ASPECT / 57.2958

! if (dipaz. gt. 6.2835) then
!   azimuthangle = azimuthangle  - 360.
! endif

  dipan = SLOPE / 57.2958

!  compute solar elevation angle function for sloping terrain

  sinsolelslope = cos(dipan) * ( sin(slb) * sin(effdec) + cos(slb) *    &
                  cos(effdec) * cos(hrangl)) + sin(dipan) *             &
                  ( cos(dipaz) * ( tan(slb) * ( sin(slb) * sin(effdec) +&
                  cos(slb) * cos(effdec) * cos(hrangl)) - sin(effdec) / &
                  cos(slb)) + sin(dipaz) * cos(effdec) * sin(hrangl))

  solelslope = asin(sinsolelslope)

  if ( solelslope .LE. 0.01) then
    solelslope = 0.01
  end if

  call albedo(sinsolelslope)

!   note that solar elevation can be less than zero for non-zero slope
!   although greater than zero for flat slope.
!   we use solar elevation for slope in albedo but not for path
!   which depends on solar angle with respect to flat plane

  return
end
