module vel_mod
  implicit none
  private
  public :: You_star, R_ohms, WindF, Stab, StabH, FStabH, FStabM, ResTrn, MOL

  real :: MOL = 0.0

  contains
!  Function Definitions for Vel
    
    real pure function You_star (w,Height,Roughness,Stability,R_Karman)
      real, intent(in) :: w, Height, Roughness, Stability, R_Karman

      You_star = R_Karman * w / ((ALOG(Height / Roughness) + Stability))  
    
    end function You_star
    

    real pure function R_ohms (Friction,Height,Roughness,Stability,Karman)
      real, intent(in) :: Friction, Height, Roughness, Stability, Karman
      real, parameter :: Konst = 0.74
             
      R_ohms = Konst * (ALOG(Height/Roughness) + Stability)/(Karman * Friction)
       
    end function R_ohms
    

    real pure function WindF (Star,Height,Roughness,Stability,R_Karman)
      real, intent(in) :: Star, Height, Roughness, Stability, R_Karman
    
      WindF = (Star / R_Karman)*(ALOG(Height/Roughness) + Stability)
    
    end function WindF
    

    real pure function Stab (Height,MOL)
      real, intent(in) :: Height, MOL
    
      Stab = (1 - 15 * Height / MOL)**0.25
    
    end function Stab
    
    
    real pure function StabH (Height, MOL)
      real, intent(in) :: Height, MOL
    
      StabH = (1 - 9 * Height / MOL)**0.5
           
    end function StabH
    

    real pure function FstabH (Par1,Par2)
      real, intent(in) :: Par1, Par2
    
      FstabH = 2 * ALOG(( Par1 + 1) / (Par2 + 1))
           
    end function FstabH
    

    real pure function FstabM (Par1,Par2)
      real, intent(in) :: Par1, Par2
    
      FStabM = ALOG (((Par1**2 + 1 ) * (Par1 + 1 )**2 ) /                   &
                    ((Par2 + 1 ) * (Par2 + 1 )**2 )) + 2 *                  &
                    ( ATAN(Par2) - ATAN(Par1))
           
    end function FstabM
    
    
    real pure function ResTrn (Star,Roughness,Par3,R_KARMAN)
      real, intent(in) :: Star, Roughness, Par3, R_KARMAN
    
      RESTRN = (ALOG(R_KARMAN* Star*Roughness+Par3) - ALOG(Par3)) / (R_KARMAN  * Star)
    
    end function ResTrn

end module vel_mod
