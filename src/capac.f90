subroutine Capac (Unscaled_RAF,H,B1_P,B2_P,vfl2, sgma)
  use simsphere_mod
  implicit none

!       INCLUDE 'modvars.h'

  real :: IXCAP
  integer :: NCAP, IDEL, JDEL, CAPRAT, ITRAP=0
  real :: capini, ZADD, FPM, ZUH, Unscaled_RAF, B1_P, B2_P, vfl2 , H
  real :: VOLISO, RSTDIV, ADDIT, AROOT, BROOT, CROOT, ACTSQRT
  real :: PES, VOLRMO, SGMA

  ZADD = ZG + ZP * FRZP
  FPM = 1 - FRHGT
  ZUH = ZP * (1-FRZP)

! **  below there are two solutions for psist. The first is the
! **  log, while the second is a regular integral based on volume
! **  to a power.  There are also two similar solutions for zst
! **  and capacitance.
! **  Note also that psist, psist and psix are a time step behind because
! **  psist is not allowed to exceed psix of the previous time
! **  step (psix is a function of psis).
! **  There are also to ways to set the volume of water in
! **  storage. If xcap is set equal to zero, then volist storage is a
! **  function of psig. If not, it is a function of xcap or
! **  relative water content

  IF ( JCAP .EQ. 1 ) THEN

    NCAP = RCCAP
    IDEL = 0
    JDEL = 0
    PSIX = PSIG - FRHGT * H
    IXCAP = VOLREL
    VOLREL = .01 * VOLREL
    CAPINI = VOLINI / RKOCAP
    JCAP = 2

    IF ( IXCAP .EQ. 0 ) THEN

       IF ( NCAP .EQ. 1 ) THEN
         VOLIST = VOLINI * EXP ( CAPINI * (PSIG - FRHGT * H)            &
                  / VOLINI )

         CAPACI = CAPINI * ( VOLIST ) / VOLINI

       ELSE IF ( NCAP .NE. 1 ) THEN

         CAPRAT =  (RCCAP - 1)/ RCCAP
         VOLIST = VOLINI * ( 1 + CAPINI * (PSIG - FRHGT * H)            &
                  * CAPRAT / VOLINI ) ** ( 1 / CAPRAT )

         CAPACI = CAPINI * ( ( VOLIST ) / VOLINI ) **                   &
                  ( 1 / RCCAP )

       END IF

! ** It is possible that the volist will be calculated as being
! ** less than zero.  This is impossible.  Therefore, we set:

     IF ( VOLIST .LT. 0 ) THEN

       VOLIST = .00001*VOLINI
       VOLRMV = .99999*VOLINI

     END IF

   ELSE

     VOLIST = VOLREL * VOLINI
     VOLISO = VOLIST

     IF ( NCAP .EQ. 1 ) THEN

       CAPACI = CAPINI * VOLREL

     ELSE IF ( NCAP .NE. 1 ) THEN

       CAPRAT =  (RCCAP - 1)/ RCCAP
       CAPACI = CAPINI * ( VOLREL ) ** ( 1 / RCCAP )

     END IF

   END IF

 END IF

 VOLREL = VOLIST / VOLINI

 IF ( NCAP .EQ. 1 ) THEN

    PSIST = ( VOLINI / CAPINI ) * LOG ( VOLREL )
    CAPACI = CAPINI * ( VOLIST ) / VOLINI

 ELSE

    PSIST = (VOLINI/CAPINI) * ( VOLREL ** CAPRAT - 1 ) * (RCCAP /( RCCAP - 1 ) )

! **  THIS IF STATEMENT IS IN PLACE OF VOLUME CONSTRAINTS

    IF ( PSIST .GE. 0 ) THEN
      PSIST = 0
    END IF

! **  The following if statement causes psist to fall as volume
! **  approaches zero (the above equation reaches a limit).
! **  (ITRAP is defined below to prevent the loop from being
! **   exercised on the first time step.)
! **   The next statement prevents psist from becoming more
! positive then psig

    IF ( ITRAP .EQ. 1 .AND. DELTVST .EQ. 0 ) THEN
      PSIST = PSIX
    END IF

    CAPACI = CAPINI * ( VOLREL ) ** ( 1 / RCCAP)

  END IF

  IF ( NCAP .EQ. 1 ) THEN
    ZST = ZSTINI / VOLREL
  ELSE IF ( NCAP .NE. 1 ) THEN
    ZST = ZSTINI * ( 1./VOLREL ) ** ( RZCAP )
  END IF

! **  The following if statement prevents zst from becoming very
!**   large.

  IF ( ZST .GT. 1E3 ) THEN
    ZST = 1E3
  END IF

! **  The following if statements prevent psist from exceeding
! **  psix during a time interval

  IF ( DELTVST .LT. 0 .AND. PSIST .LT. PSIX ) THEN
    PSIST = PSIX
  ELSE IF ( DELTVST .GT. 0 .AND. PSIST .GT. PSIX ) THEN
    PSIST = PSIX
  END IF


  RSTDIV = Unscaled_RAF + ( RCUT * RSCRIT ) / ( RCUT + RSCRIT )

! **  we need to compare psig to psigc before proceeding
! **  we can determine this by solving for psig given various
! **  parameters.

! **   ADDIT  is an additional term that incorporates storage flux.

  ADDIT = 1 + ZUH / ZADD + ZUH / ZST

!**    define a critical water potential. As the flux
! **   from storage approaches zero, it approaches that of
! **   the soil.

  PSIWC =  SGMA * VFL * ZUH / RSTDIV + PSICE + beta * vfl2 + FPM * H
  PSISUP = PSIX

  IF ( PSISUP .GT. PSIWC ) THEN
    AROOT = FS * FT * b1_p * (RCUT + Unscaled_RAF) * ( ZST * (-1) - ZADD )

    BROOT = FS * FT * (RCUT + Unscaled_RAF) * ( RMIN *                  &
            ( - ZST - ZADD ) + b1_p * ( ZST * ( PSIG - BETA *           &
            vfl2 - H ) + ZADD * (PSIST - beta * vfl2 - FPM * H )        &
            - ZADD * ZST * SGMA * VFL * ADDIT /                         &
            ( RCUT + Unscaled_RAF) ) ) + RCUT * Unscaled_RAF * ( - ZST - ZADD )


    CROOT = FS * FT * (RCUT + Unscaled_RAF) * ( RMIN * ( ZST            &
            * (PSIG - beta * vfl2 - H ) + ZADD * ( PSIST                &
            - beta * vfl2 - FPM * H )                                   &
            - ZADD * ZST * SGMA * VFL * ADDIT / (RCUT + Unscaled_RAF)   &
            ) )                                                         &
            + RCUT * Unscaled_RAF * ( ZST * (PSIG - beta * vfl2 - H )   &
            + ZADD * ( PSIST - beta * vfl2 - FPM * H ) )                &
            - RCUT * ZADD * ZST * SGMA * VFL * ADDIT

  ELSE

    AROOT = FS * FT * b2_p * ( RCUT + Unscaled_RAF ) * ( ZST + ZADD )

    BROOT = FS * FT *  ( RCUT + Unscaled_RAF ) * ( ( RMIN               &
            + b1_p * PSICE + b2_p * PSICE) * ( -ZST - ZADD )            &
            - b2_p * ( ZST * (PSIG - beta * vfl2 - H )                  &
            + ZADD * ( PSIST - beta * vfl2 - FPM * H )                  &
            - ZADD * ZST * SGMA * VFL * ADDIT /                         &
            (RCUT + Unscaled_RAF) ) )                                   &
            + RCUT * Unscaled_RAF * ( - ZST - ZADD )                    

    CROOT = FS * FT* (RCUT + Unscaled_RAF) * ( RMIN + b1_p * PSICE      &
            + b2_p * PSICE ) * ( ZST * (PSIG - beta * vfl2 - H )        &
            + ZADD * (PSIST - beta * vfl2 - FPM * H )                   &
            - ZADD * ZST * SGMA * VFL * ADDIT /(RCUT + Unscaled_RAF))   &
            + RCUT * Unscaled_RAF * ( ZST * ( PSIG - beta * vfl2 - H)   &
            + ZADD * (PSIST - beta * vfl2 - FPM * H ) )                 &
            - RCUT * ZADD * ZST * SGMA * VFL * ADDIT

  END IF

  ACTSQRT  = BROOT ** 2 - 4 * AROOT * CROOT
  PSIE = ( -BROOT - SQRT ( ACTSQRT ) ) / (2 * AROOT )

  RS = stomrs(ft,tf,rmin,mintemp,maxtemp,psisup,psiwc,b1,b2,psie,psice,fs)


! * * STRESS INDICES

  PSIM = PSIE + beta * vfl2
  PES = XLAI/2 + 1
  RLELF = SGMA * VFL / (RS*RCUT/(RS + RCUT) + Unscaled_RAF)
  PSIX = RLELF  * ZUH + PSIM + FPM * H
  FST = -( PSIST - PSIX ) / ZST
  DELTVST = -DELTA * ( PSIST - PSIX ) / (ZST * LE * RHOW )
  FLUXGD = ( PSIG - PSIX - FRHGT * H ) / ZADD
  VOLRMO = VOLRMV
  VOLIST = VOLIST + DELTVST
  VOLRMV = VOLINI - VOLIST

! **   the following if statements prevent the volume in
! **   storage from becoming negative.

  IF ( VOLRMV .GT. VOLINI .AND. DELTVST .LE. 0 ) THEN
    IF ( IDEL .EQ. 0 ) THEN
      DELTVST = (VOLINI - VOLRMO)
      IDEL = 1
    ELSE
      DELTVST = 0.0
      ITRAP = 1
    END IF
    VOLRMV = .99999*VOLINI
    VOLIST = .00001*VOLINI
  END IF

  return
end
