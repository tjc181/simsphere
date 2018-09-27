module simsphere_mod
  use constants
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


!    COMMON blocks to initialize various things (formerly modvars.h)
! **  This file contains the declaration of the common blocks for the
! **  model.

  real :: KM(50)=0.0, LAMBDA=0.0, KAPPA=0.0, LWDN=0.0
  real :: u_fine(51)=0.0,v_fine(51)=0.0,t_fine(51)=0.0,q_fine(51)=0.0
  real :: UGS=0.0,VGS=0.0,ANGL=0.0,DTX=0.0,DTY=0.0,CF=0.0
  real :: DELTAZ=0.0
  real :: ZI(50)=0.0,ZK(50)=0.0
  real :: UD(50)=0.0,VD(50)=0.0,UGD(50)=0.0,VGD(50)=0.0
  real :: XTDIF=0.0,YTDIF=0.0,XPDIF=0.0,YPDIF=0.0
  real :: GM(50)=0.0,TS(50)=0.0,TD(50)=0.0,Tdif_50=0.0,Tdif_s=0.0,O_Pot_Tmp=0.0
  real :: TA=0.0,TAF=0.0,TG=0.0,QAF=0.0,QSTF=0.0,QSTG=0.0
  real :: HF=0.0,XLEF=0.0,XLEG=0.0,TF=0.0,QA=0.0,WIDTH=0.0
  real :: RST=0.0,UAF=0.0,RSF=0.0,RSG=0.0,RLF=0.0,RLG=0.0,UTEN=0.0
  real :: CHA=0.0, CHG=0.0, CHF=0.0, RTRANW=0.0
  real :: KQFLAG=0.0,QD(50)=0.0,DQDT2(50)=0.0,QQ(50)=0.0,QN(51)=0.0
  real :: ATEMP=0.0,AWIND=0.0,OMEGA=0.0,OTEMP=0.0,BTEMP=0.0,APTEMP=0.0
  real :: GBL_sum=0.0,OSHUM=0.0,XMAX=0.0,DQDT=0.0,SUMW=0.0
  real :: FSUB=0.0,F=0.0,ZO=0.0,TP=0.0,TI_A=0.0,TI_B=0.0,DELZ(8)=0.0,CG=0.0
  real :: GAM=0.0,HET=0.0
  real :: U(50)=0.0,V(50)=0.0,UG(50)=0.0,VG(50)=0.0,T(50)=0.0,ZCOUNT=0.0
  real :: OUTTT=0.0,SATAM=0.0,SATPM=0.0,STRTIM=0.0, TIMEND=0.0, REALTM=0.0,PTIME=0.0
  real :: DEL=0.0,DZETA=0.0,Z(9)=0.0,TT(9)=0.0,XFUN(9)=0.0
  real :: XLAT=0.0,XLONG=0.0,TZ=0.0,ALBDOE=0.0
  real :: WMAX=0.0,W2G=0.0,WGG=0.0,WILT=0.0
  real :: EPSI=0.0,EPSF=0.0,XLAI=0.0,SOL=0.0,RNETG=0.0,RNETF=0.0,AEPSI=0.0
  real :: SWAVE=0.0
  real :: ADVGT=0.0
  real :: FRVEG=0.0
  real :: TSCREN=0.0,PTM100=0.0
  real :: RESIST=0.0,EMBAR=0.0,RZASCR=0.0
  real :: THV=0.0,THMAX=0.0,PSIG=0.0,VFL=0.0,BETA=0.0,B1=0.0,B2=0.0,PSICM=0.0
  real :: PSICE=0.0,SC=0.0,ZP=0.0,MINTEMP=0.0,MAXTEMP=0.0
  real :: RCUT=0.0,RAF=0.0,RMIN=0.0,VEGHEIGHT=0.0
  real :: FS=0.0,RSCRIT=0.0,PSIWC=0.0,PSIM=0.0,PSIE=0.0,RS=0.0,WPSI=0.0
  real :: RLPSI=0.0,FC=0.0,FPSIE=0.0,RL=0.0,ZG=0.0,RLELF=0.0
  real :: VOLINI=0.0,RKOCAP=0.0,ZSTINI=0.0,FRHGT=0.0,FRZP=0.0,RZCAP=0.0,VOLREL=0.0
  real :: PSIST=0.0,PSIX=0.0,FST=0.0,DELTVST=0.0,VOLRMV=0.0,ZST=0.0,CAPACI=0.0
  real :: FLUXGD=0.0,VOLIST=0.0,PSISUP=0.0
  real :: rks=0.0, cosbyb=0.0, psis=0.0
  real :: FCO2=0.0,CCAN=0.0,CI=0.0,CO=0.0,FRCO2=0.0
  real :: coz_sfc=0.0, coz_air=0.0, caf=0.0, fglobal=0.0, flux_plant=0.0, sumo3=0.0
  real :: SLOPE=0.0, ASPECT=0.0
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


  integer(kind=1) :: cld_fract = 0
  integer :: RCCAP = 0
  integer :: NTRP = 0
  integer :: IMO = 0
  integer :: IDAY = 0
  integer :: IFIRST = 0
  integer :: NLVLS = 5
  integer :: JCAP = 1
  integer :: IYR = 0

  character(len=1) :: STMTYPE, STEADY, DUAL_TI

  logical :: cloud_flag = .false.


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
