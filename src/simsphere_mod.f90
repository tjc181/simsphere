module simsphere_mod
  use config_mod, only: t_met, t_timeloc, t_veg, t_wind, t_soil, t_temp,    &
                        t_humid, load_config
  use constants
  use globals
  use snding_mod, only: splint, spline
  use transm_mod, only: ftabsT, ftscatT, fbscatT, ABSTBL, SCATBL, BSCTBL, PS1
  use vel_mod, only: You_star, R_ohms, WindF, Stab, StabH, FStabH, FStabM, ResTrn, MOL
  use stomata_mod, only: stomc, stomfs, stomrs
  use compare, only: eq, gt, lt
  implicit none
  public

!
! Simsphere module provides parameters and functions used by various other parts of the program.
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

end module simsphere_mod
