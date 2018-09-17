subroutine  GETTBL
  use simsphere_mod
  implicit none

! This subroutine takes the precipitable water content (OMEGA) and
! calc's the transmission coefficient for absorbtion using a lookup
! table.  Interpolate linearly on OMEGA.  The lookup table contains
! 46-entry tables for OMEGA from 0 to 5 in increments of 0.5.

! Note that the maximum value allowed for OMEGA is 5.0.

! The subroutine also copies the scattering (SCATBL) and the
! backscattering (BSCTBL) tables from file into the common block.

  real :: A(46) , B(46) , C(46)
  real :: FRACT, FRACT2
  integer :: I,J,K,L

!      INCLUDE 'modvars.h'

  OPEN (UNIT = 14, FILE = 'LUT.DAT')

  I = 1 + OMEGA * 2
  J = I - 1
  IF ( I .GT. 11 ) I = 11

! For no precipitable water use 1st 46 values, otherwise skip J blocks
! of 46 table entries and read appropriate values.

  IF ( J .EQ. 0 ) THEN
    do L = 1 , 46
      READ ( 14 , 1000 ) ABSTBL(L) , SCATBL(L) , BSCTBL(L)
    end do
  ELSE
    do K = 1 , J
      do L = 1 , 46
        READ ( 14 , 1000 ) A(L) , B(L) , C(L)
      end do
    end do
    do L = 1 , 46
      READ ( 14 , 1000 ) ABSTBL(L) , SCATBL(L) , BSCTBL(L)
    end do
  ENDIF

! For a maximum OMEGA no interpolation is performed but for all other
! values of OMEGA interpol'n is done using weighting factors FRACT2 &
! FRACT.  Values are written to channel 69.

  IF ( I .EQ. 11 ) THEN
!  WRITE ( 19 , 6901 ) OMEGA
!  DO 25 L = 1 , 46
!   25   WRITE ( 19 , 1000 ) ABSTBL(L) , SCATBL(L) , BSCTBL(L)
    continue
  ELSE
    do L = 1 , 46
      READ ( 14 , 1000 ) A(L) , B(L) , C(L)
    end do
    FRACT = 2 * OMEGA - J
    FRACT2 = 1 - FRACT
    do L = 1 , 46
      ABSTBL(L) = ABSTBL(L) * FRACT2 + A(L) * FRACT
    end do
!  WRITE (19,6947) FRACT , FRACT2
!  WRITE (19,6901) OMEGA
!  DO 40 L = 1 , 46
!    WRITE ( 19 , 1000 ) ABSTBL(L) , SCATBL(L) , BSCTBL(L)
!   40  CONTINUE
  ENDIF

  REWIND 14
  CLOSE (UNIT = 14)


! Format statements

1000 FORMAT (3F12.7)
! 6947 FORMAT (//,' GETTBL.  INTERPOLATION FACTORS:',2(2X,F10.7))
! 6901 FORMAT (//,' LOOKUP TABLE FOR OMEGA:  ',F10.7,/)

  return
end
