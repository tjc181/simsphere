module simsphere_mod
!
! Simsphere module provides parameters used by various other parts of the program.
! This module was originally three "header" files: constants.h, factors.h, and 
! modvars.h.  These files were used via an INCLUDE (or, originally, $INCLUDE for a
! suspected DEC compiler).  The contents have been collected into this module in
! an initial effort to modernize the code.
!

!    list of parameter constants (formerly constants.h).

  real, parameter :: radian = 57.29578
  real, parameter :: rot_rate_earth = 7.27e-5

!    conversion factors (formerly factors.h)

  real, parameter :: Kts_To_Metres = 1.944       ! Knots to ms-1
  real, parameter :: Ft_To_Metres = 3.281        ! Feet to meters
  real, parameter :: Degs_To_Radians = 0.0174533 ! Degrees to Radians
  real, parameter :: Celsius_To_Kelvin = 273.15  ! Celsius to Kelvin

!    COMMON blocks to initialize various things (formerly modvars.h)
! **  This file contains the declaration of the common blocks for the
! **  model.

  character(len=1) :: STMTYPE, STEADY, DUAL_TI
! ** Removed LE, KARMAN, MOL from following declaration 
! ** (declared below in previous DATA blocks)
  real :: KM(50), LAMBDA, KAPPA, LWDN
  real :: u_fine(51),v_fine(51),t_fine(51),q_fine(51)
  integer(kind=1) :: cld_fract
  logical :: cloud_flag

  real, parameter :: dens = 1.1 ! Density of Air
  real, parameter :: ft = 1.0
  integer, parameter :: rhow=1000 ! Density of Water

  real :: ABSTBL(50),BSCTBL(50),SCATBL(50)
  real :: UGS,VGS,ANGL,DTX,DTY,CF
! ** NTRP is used as a step in a do loop and array index, must be an integer
  integer :: NTRP
  real :: DELTAZ
  real :: ZI(50),ZK(50)
  real :: UD(50),VD(50),UGD(50),VGD(50)
  real :: CLKTAM,CLKTPM,XTDIF,YTDIF,XPDIF,YPDIF
  real :: GM(50),TS(50),TD(50),Tdif_50,Tdif_s,O_Pot_Tmp

  real :: TA,TAF,TG,QAF,QSTF,QSTG
! ** Removed HG, parameter declared elsewhere
  real :: HF,XLEF,XLEG,TF,QA,WIDTH
  real :: RST,UAF,RSF,RSG,RLF,RLG,UTEN
  real :: CHA, CHG, CHF, RTRANW
  real :: KQFLAG,QD(50)=0.0,DQDT2(50),QQ(50),QN(51)
! ** Removed AHUM, parameter declared elsewhere
  real :: ATEMP,AWIND,OMEGA,OTEMP,BTEMP,APTEMP
! ** Removed SIGMA, LE, KARMAN, GRAV, R, CP parameter declared elsewhere
! **   real :: 
! ** Removed RNET, HEAT, EVAP parameter declared elsewhere
  real :: SUM,OSHUM,XMAX,DQDT,SUMW
! ** Removed KAPPA, LAMBDA, ZA declared previously
  real :: FSUB,F,ZO,TP,TI_A,TI_B,DELZ(8),CG
! ** Removed CHGT, HGT, DELT, CTHETA, DHET, RAD, DELTA parameter declared elsewhere
  real :: GAM,HET
  real :: U(50),V(50),UG(50),VG(50),T(50),ZCOUNT
  real :: OUTTT,SATAM,SATPM,TIMEND,STRTIM,REALTM,PTIME
! ** Remove NLVLS declared elsewhere
  real :: DEL,DZETA,Z(9),TT(9),XFUN(9)
! ** IMO is used as an array index, must be integer
  integer :: IMO
! ** IDAY is argument to float() which requires integer
  integer :: IDAY
! ** Removed ALBG parameter declared elsewhere
  real :: XLAT,XLONG,TZ,IYR,ALBDOE
  real :: WMAX,W2G,WGG,WILT
! ** Removed ALBF parameter declared elsewhere
  real :: EPSI,EPSF,XLAI,SOL,RNETG,RNETF,AEPSI
! ** Removed LWDN declared previously
! ** Removed Y, XMOD, SIGF, USTAR, TSTAR parameter declared elsewhere
  real :: SWAVE

! ** Declared and initialized elsewhere
! **   integer :: IFIRST

  real :: ADVGT
  real :: FRVEG
  real :: TSCREN,PS1,PTM100

  real :: RESIST,EMBAR,RZASCR
! ** Removed MOL, BULK parameter declared elsewhere
! **   real :: 

! ** Declared previously as character
! **   real :: STMTYPE,STEADY,DUAL_TI

  real :: THV,THMAX,PSIG,RKW,VFL,BETA,B1,B2,PSICM,PSICE,SC,ZP,MINTEMP,MAXTEMP,RCUT,RAF,RMIN,VEGHEIGHT

  real :: FS,RSCRIT,PSIWC,PSIM,PSIE,RS,WPSI,RLPSI,FC,FPSIE,RL,ZG,RLELF
! ** Removed JCAP declared elsewhere
  real :: VOLINI,RKOCAP,ZSTINI,FRHGT,FRZP,RCCAP,RZCAP,VOLREL
  real :: PSIST,PSIX,FST,DELTVST,VOLRMV,ZST,CAPACI,FLUXGD,VOLIST,PSISUP
  real :: rks, cosbyb, psis
! ** Declared previously
! **   real :: u_fine(51),v_fine(51),t_fine(51),q_fine(51) 

  real :: FCO2,CCAN,CI,CO,FRCO2
  real :: coz_sfc, coz_air, caf, fglobal, flux_plant, sumo3
! ** cld_fract declared previously as integer(kind=1)
! ** cloud_flag declared previously as logical
! ** real :: cld_fract, cloud_flag

  real :: SLOPE, ASPECT

! Included from block.f90
! Reworked to not use DATA statements
! Constants

! These should all have the parameter attribute added after the COMMON statements 
! are removed
  real :: Y = 1.0
  real :: ALBG = 0.0
  real :: ALBF = 0.0
  real :: XMOD = 0.0
  real :: SIGF = 0.0
  real :: HG = 0.0
  real :: AHUM = 0.0
  real :: RNET = 0.0
!  DATA Y,ALBG,ALBF,XMOD,SIGF /1.0,4*0.0/,                               & 
!       HG,AHUM,RNET/3*0.0/,                                             &
!       (QD(I),I=1,21)/21*0.0/
! QD declared previously...moved initialization to declaration

  real :: CHGT = 0.0
  real :: USTAR = 0.0
  real :: TSTAR = 0.0
  real :: HEAT = 0.0
  real :: HGT = 50.0
  real, parameter :: ZA = 50.0
  real :: DELT = 1.0
  real :: CTHETA = 1.0
  real :: DHET = 0.0
  real :: EVAP = 0.0
!  DATA CHGT,USTAR,TSTAR,HEAT /4*0.0/,                                   &
!       HGT,ZA,DELT,CTHETA,DHET,EVAP/2*50.,2*1,2*0/

  real, parameter :: SIGMA = 5.6521E-8
  real, parameter :: LE = 2.5E6
  real, parameter :: KARMAN = 0.4
  real, parameter :: GRAV = 9.78
  real, parameter :: R = 287.5
  real, parameter :: RAD = 1.6E-5
  real, parameter :: CP = 1004.832
  integer, parameter ::  DELTA = 90
!  DATA SIGMA,LE,KARMAN,GRAV,R,RAD,CP                                    &
!       /5.6521E-8,2.5E6,0.4,9.78,287.5,1.6E-5,1004.832/,                &
!       DELTA/90/

  real :: MOL = 0.0
  integer :: BULK = 0
  integer :: IFIRST = 0
  integer :: NLVLS = 5
!  DATA MOL,BULK,IFIRST,NLVLS /3*0,5/


! Initialization of variables

  integer :: JCAP = 1
!  DATA JCAP/1/


!  SUBROUTINE  BLOCK ()

!  END
end module simsphere_mod
