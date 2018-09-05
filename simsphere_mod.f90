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
  real :: KM(50), LAMBDA, KAPPA, LE, KARMAN, LWDN, MOL
  real :: u_fine,v_fine,t_fine,q_fine
  integer*1 :: cld_fract
  logical :: cloud_flag

  real, parameter :: dens = 1.1 ! Density of Air
  real, parameter :: ft = 1.0
  integer, parameter :: rhow=1000 ! Density of Water

      COMMON /TRANS/ ABSTBL(50),BSCTBL(50),SCATBL(50)
      COMMON /PRFCOM/ UGS,VGS,ANGL,DTX,DTY,CF
      COMMON /INITZ/ NTRP,DELTAZ
      COMMON /OBRIAN/ ZI(50),ZK(50),KM
      COMMON /WINDY/ UD(50),VD(50),UGD(50),VGD(50)
      COMMON /STRCOM/ CLKTAM,CLKTPM,XTDIF,YTDIF,XPDIF,YPDIF
      COMMON /SND/ GM(50),TS(50),TD(50),Tdif_50,Tdif_s,O_Pot_Tmp

      COMMON /VEG1/ TA,TAF,TG,QAF,QSTF,QSTG
      COMMON /VEG2/ HF,HG,XLEF,XLEG,TF,QA,WIDTH
      COMMON /VEG3/ RST,UAF,RSF,RSG,RLF,RLG,UTEN
      COMMON /VEG_COND/ CHA, CHG, CHF, RTRANW
      COMMON /QDAT/ KQFLAG,QD(50),DQDT2(50),QQ(50),QN(51)
      COMMON /METEO/ ATEMP,AHUM,AWIND,OMEGA,OTEMP,BTEMP,APTEMP
      COMMON /KONST/ CP,LE,SIGMA,KARMAN,GRAV,R
      COMMON /EXTRA/ HEAT,RNET,SUM,OSHUM,EVAP,XMAX,DQDT,SUMW
      COMMON /SHGT/ FSUB,F,KAPPA,LAMBDA,ZA,ZO,TP,TI_A,TI_B,DELZ(8),CG
      COMMON /BOUND/ GAM,DELTA,RAD,DHET,HET,DELT,HGT,CHGT,CTHETA
      COMMON /PARM/ U(50),V(50),UG(50),VG(50),T(50),ZCOUNT
      COMMON /PRNTIM/ OUTTT,SATAM,SATPM,TIMEND,STRTIM,REALTM,PTIME
      COMMON /BELCOM/ DEL,DZETA,NLVLS,Z(9),TT(9),XFUN(9)
      COMMON /INPCOM/ XLAT,XLONG,IMO,IDAY,TZ,ALBG,IYR,ALBDOE
      COMMON /WAT/ WMAX,W2G,WGG,WILT
      COMMON /NETCOM/ EPSI,EPSF,ALBF,XLAI,SOL,RNETG,RNETF,AEPSI
      COMMON /REQD/ Y,USTAR,SWAVE,SIGF,LWDN,TSTAR,XMOD

      COMMON /AYRE/ IFIRST

      COMMON /TADV/ ADVGT
      COMMON /PART/ FRVEG
      COMMON /TZB/ TSCREN,PS1,PTM100

      COMMON /CANRC/ RESIST,EMBAR,RZASCR
      COMMON /MOLDAY/ MOL,BULK

      COMMON/SWITCH/STMTYPE,STEADY,DUAL_TI

      COMMON/STOMINIT/THV,THMAX,PSIG,RKW,VFL,BETA,B1,B2,PSICM,PSICE,SC,ZP,MINTEMP,MAXTEMP,RCUT,RAF,RMIN,VEGHEIGHT

      COMMON/STOMCALC/FS,RSCRIT,PSIWC,PSIM,PSIE,RS,WPSI,RLPSI,FC,FPSIE,RL,ZG,RLELF
      COMMON/CAPACINIT/VOLINI,JCAP,RKOCAP,ZSTINI,FRHGT,FRZP,RCCAP,RZCAP,VOLREL
      COMMON/CAPACCALC/PSIST,PSIX,FST,DELTVST,VOLRMV,ZST,CAPACI,FLUXGD,VOLIST,PSISUP
      Common / soils / rks, cosbyb, psis
      Common /FineMesh/ u_fine(51),v_fine(51),t_fine(51),q_fine(51) 

      COMMON/CO2/FCO2,CCAN,CI,CO,FRCO2
    common/oz/ coz_sfc, coz_air, caf, fglobal, flux_plant, sumo3
    common/cloud/ cld_fract, cloud_flag

      COMMON/SLOPE/ SLOPE, ASPECT

end module simsphere_mod
