module transm_mod
  implicit none
  private
  public :: ftabsT, ftscatT, fbscatT, ABSTBL, BSCTBL, SCATBL, PS1

  integer, parameter :: TRANSM_MAX_PATH = 10
  real :: ABSTBL(46),BSCTBL(46),SCATBL(46), PS1

contains
! Functions from former subroutine transm

    pure function ftabsT(path)
!      use simsphere_mod, only: abstbl, ps1, TRANSM_MAX_PATH
      real :: ftabst, fracp, fract, fract2
      real, intent(in) :: path
      integer :: ipath, jpath

!     Subroutine TRANSM calculates solar transmission by using the
!     three-way lookup table produced in GETTBL.

! **  If the path length is very large (sun almost on the horizon) use
! **  longest path length possible, ie last number in the table. Otherwise
! **  calc trans coeff's for entries bracketing the supplied path length.
! **  FRACTP - Scaling fact for depth of atmos. FRACT & FRACT2 weighting
! **  factors for interpol'n between 2 successive path lenghts in table.

      if ( path >= TRANSM_MAX_PATH ) then
        ftabst = abstbl(size(abstbl))
      else
        fracp = ps1 / 1013.25
        fract= 5 * ( path - 1 ) + 1
        ipath = INT( fract )
        jpath = ipath + 1
        fract = ( fract - ipath )
        fract2 = 1 - fract
        ftabst = fract2 * abstbl( ipath ) + fract * abstbl( jpath )
        ftabst = fracp * ( ftabst - 1 ) + 1
      end if
     end function ftabsT

    pure function ftscatT(path)
!      use simsphere_mod, only: scatbl, ps1, TRANSM_MAX_PATH
      real :: ftscatT, fracp, fract, fract2
      real, intent(in) :: path
      integer :: ipath, jpath

!     Reference comments in ftabsT()

      if ( path >= TRANSM_MAX_PATH ) then
        ftscatT = scatbl(size(scatbl))
      else
        fracp = ps1 / 1013.25
        fract= 5 * ( path - 1 ) + 1
        ipath = INT( fract )
        jpath = ipath + 1
        fract = ( fract - ipath )
        fract2 = 1 - fract
        ftscatT = fract2 * scatbl( ipath ) + fract * scatbl( jpath )
        ftscatT = fracp * ( ftscatT - 1 ) + 1
      end if
    end function ftscatT

    pure function fbscatT(path)
!      use simsphere_mod, only: bsctbl, ps1, TRANSM_MAX_PATH
      real :: fbscatT, fracp, fract, fract2
      real, intent(in) :: path
      integer :: ipath, jpath

!     Reference comments in ftabsT()

      if ( path >= TRANSM_MAX_PATH ) then
        fbscatT = bsctbl(size(bsctbl))
      else
        fracp = ps1 / 1013.25
        fract= 5 * ( path - 1 ) + 1
        ipath = INT( fract )
        jpath = ipath + 1
        fract = ( fract - ipath )
        fract2 = 1 - fract
        fbscatT = fract2 * bsctbl( ipath ) + fract * bsctbl( jpath )
      end if
    end function fbscatT

end module transm_mod
