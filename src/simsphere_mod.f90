module simsphere_mod
  use snding_mod, only: splint, spline
  use transm_mod, only: ftabsT, ftscatT, fbscatT, ABSTBL, SCATBL, BSCTBL, PS1
  use vel_mod, only: You_star, R_ohms, WindF, Stab, StabH, FStabH, FStabM, ResTrn, MOL
  implicit none
  public

!
! Simsphere module provides parameters and functions used by various other parts of the program.
! This module was originally three "header" files: constants.h, factors.h, and 
! modvars.h.  These files were used via an INCLUDE (or, originally, $INCLUDE for a
! suspected DEC compiler).  The contents have been collected into this module in
! an initial effort to modernize the code.
!

  integer, parameter :: vert_spacing = 250
  integer, parameter :: rhow=1000                ! Density of Water
  integer, parameter ::  DELTA = 90

  real, parameter :: rot_rate_earth = 7.27e-5
  real, parameter :: siga = 279.9348
!  real, parameter :: radian = 57.29578
  real, parameter :: dens = 1.1                  ! Density of Air
  real, parameter :: ft = 1.0
  real, parameter :: ZA = 50.0
  real, parameter :: SIGMA = 5.6521E-8
  real, parameter :: LE = 2.5E6
  real, parameter :: KARMAN = 0.4
  real, parameter :: GRAV = 9.78
  real, parameter :: R = 287.5
  real, parameter :: RAD = 1.6E-5
  real, parameter :: CP = 1004.832
  double precision, parameter :: radian = 572957.75913D-4
  double precision, parameter :: sdec = 39784.988432D-5

!    conversion factors (formerly factors.h)
  real, parameter :: Kts_To_Metres = 1.944       ! Knots to ms-1
  real, parameter :: Ft_To_Metres = 3.281        ! Feet to meters
  real, parameter :: Degs_To_Radians = 0.0174533 ! Degrees to Radians
  real, parameter :: Celsius_To_Kelvin = 273.15  ! Celsius to Kelvin

! Data file names
  character(len=12), parameter :: f_control = 'i_model.dat'
  character(len=12), parameter :: f_soil_lut = 'soils.dat'
  character(len=12), parameter :: f_veg_lut = 'veglut.dat'
  character(len=12), parameter :: f_precip_lut = 'LUT.DAT'
  character(len=12), parameter :: f_output = 'o_model.dat'

!    COMMON blocks to initialize various things (formerly modvars.h)
! **  This file contains the declaration of the common blocks for the
! **  model.

  real :: KM(50), LAMBDA, KAPPA, LWDN
  real :: u_fine(51),v_fine(51),t_fine(51),q_fine(51)
  real :: UGS,VGS,ANGL,DTX,DTY,CF
  real :: DELTAZ
  real :: ZI(50),ZK(50)
  real :: UD(50),VD(50),UGD(50),VGD(50)
  real :: XTDIF,YTDIF,XPDIF,YPDIF
  real :: GM(50),TS(50),TD(50),Tdif_50,Tdif_s,O_Pot_Tmp
  real :: TA,TAF,TG,QAF,QSTF,QSTG
  real :: HF,XLEF,XLEG,TF,QA,WIDTH
  real :: RST,UAF,RSF,RSG,RLF,RLG,UTEN
  real :: CHA, CHG, CHF, RTRANW
  real :: KQFLAG,QD(50)=0.0,DQDT2(50),QQ(50),QN(51)
  real :: ATEMP,AWIND,OMEGA,OTEMP,BTEMP,APTEMP
  real :: GBL_sum,OSHUM,XMAX,DQDT,SUMW
  real :: FSUB,F,ZO,TP,TI_A,TI_B,DELZ(8),CG
  real :: GAM,HET
  real :: U(50),V(50),UG(50),VG(50),T(50),ZCOUNT
  real :: OUTTT,SATAM,SATPM,STRTIM, TIMEND, REALTM, PTIME
  real :: DEL,DZETA,Z(9),TT(9),XFUN(9)
  real :: XLAT,XLONG,TZ,IYR,ALBDOE
  real :: WMAX,W2G,WGG,WILT
  real :: EPSI,EPSF,XLAI,SOL,RNETG,RNETF,AEPSI
  real :: SWAVE
  real :: ADVGT
  real :: FRVEG
  real :: TSCREN,PTM100
  real :: RESIST,EMBAR,RZASCR
  real :: THV,THMAX,PSIG,RKW,VFL,BETA,B1,B2,PSICM,PSICE,SC,ZP,MINTEMP,MAXTEMP,RCUT,RAF,RMIN,VEGHEIGHT
  real :: FS,RSCRIT,PSIWC,PSIM,PSIE,RS,WPSI,RLPSI,FC,FPSIE,RL,ZG,RLELF
  real :: VOLINI,RKOCAP,ZSTINI,FRHGT,FRZP,RZCAP,VOLREL
  real :: PSIST,PSIX,FST,DELTVST,VOLRMV,ZST,CAPACI,FLUXGD,VOLIST,PSISUP
  real :: rks, cosbyb, psis
  real :: FCO2,CCAN,CI,CO,FRCO2
  real :: coz_sfc, coz_air, caf, fglobal, flux_plant, sumo3
  real :: SLOPE, ASPECT
  real :: Y = 1.0
  real :: ALBG = 0.0
  real :: ALBF = 0.0
  real :: XMOD = 0.0
  real :: SIGF = 0.0
  real :: HG = 0.0
  real :: AHUM = 0.0
  real :: RNET = 0.0
  real :: USTAR = 0.0
  real :: TSTAR = 0.0
  real :: HEAT = 0.0
  real :: HGT = 50.0
  real :: DELT = 1.0
  real :: DHET = 0.0
  real :: EVAP = 0.0
  real :: BULK = 0.0


  integer(kind=1) :: cld_fract
  integer :: RCCAP
  integer :: NTRP
  integer :: IMO
  integer :: IDAY
  integer :: IFIRST = 0
  integer :: NLVLS = 5
  integer :: JCAP = 1

  character(len=1) :: STMTYPE, STEADY, DUAL_TI

  logical :: cloud_flag


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

end module simsphere_mod
