module vel_mod
  implicit none
  private
  public :: You_star, R_ohms, WindF, Stab, StabH, FStabH, FStabM, ResTrn, MOL

  real :: MOL = 0.0

  contains
!  Function Definitions for Vel
    
    real function You_star (Wind,Height,Roughness,Stability)
      real(kind=4), parameter :: R_Karman = 0.4
      real :: Wind, Height, Roughness, Stability
    
      You_star = R_Karman * Wind / ((ALOG(Height / Roughness) + Stability))  
    
    end function You_star
    

    real function R_ohms (Friction,Height,Roughness,Stability)
      real(kind=4), parameter :: Karman = 0.4
      real(kind=4), parameter :: Konst = 0.74
      real :: Friction, Height, Roughness, Stability
             
      R_ohms = Konst * (ALOG(Height/Roughness) + Stability)/(Karman * Friction)
       
    end function R_ohms
    

    real function WindF (Star,Height,Roughness,Stability)
      real(kind=4), parameter :: R_Karman = 0.4
      real :: Star, Height, Roughness, Stability
    
      WindF = (Star / R_Karman)*(ALOG(Height/Roughness) + Stability)
    
    end function WindF
    

    real function Stab (Height)
!      use simsphere_mod, only: MOL
      real :: Height
    
      Stab = (1 - 15 * Height / MOL)**0.25
    
    end function Stab
    
    
    real function StabH (Height)
!      use simsphere_mod, only: MOL
      real :: Height
    
      StabH = (1 - 9 * Height / MOL)**0.5
           
    end function StabH
    

    real function FstabH (Par1,Par2)
      real :: Par1, Par2
    
      FstabH = 2 * ALOG(( Par1 + 1) / (Par2 + 1))
           
    end function FstabH
    

    real function FstabM (Par1,Par2)
      real :: Par1, Par2
    
      FStabM = ALOG (((Par1**2 + 1 ) * (Par1 + 1 )**2 ) /                   &
                    ((Par2 + 1 ) * (Par2 + 1 )**2 )) + 2 *                  &
                    ( ATAN(Par2) - ATAN(Par1))
           
    end function FstabM
    
    
    real function ResTrn (Star,Roughness,Par3)
      real :: Star, Roughness, Par3
    
      real(kind=4), parameter :: R_KARMAN = 0.4
    
      RESTRN = (ALOG(R_KARMAN* Star*Roughness+Par3) - ALOG(Par3)) / (R_KARMAN  * Star)
    
    end function ResTrn

end module vel_mod
