module stomata_mod
  implicit none
  private
  public :: stomfs, stomrs, stomc


contains

  !
  ! stomfs function replaces STOMFS subroutine
  !
  
      real pure function stomfs (sc, sol)
        implicit none
        real, intent(in) :: sc, sol
       
        ! The exponential function for solar radiation -- Albert Olioso
  
        stomfs = 1 / (1 - exp(-1 / sc * sol))
      end function stomfs
  
  !
  ! stomrs function replaces the STOMRS subroutine
  !
  
     real pure function stomrs (ft,tf,rmin,mintemp,maxtemp,psisup,psiwc,b1,b2,psie,psice,fs)
       implicit none
  
       real, intent(in) :: ft, tf, rmin, mintemp, maxtemp, b1, b2
       real, intent(in) :: psisup, psiwc, psie, psice, fs
       real :: fpsice
  
       ! This program calculates stomatal resistance coefficients are initialized in
       ! stmcof.for (referenced file does not exist -TJC)
  
       if ( tf .GT. mintemp .AND. tf .LT. maxtemp ) then
         if ( psisup.GT. psiwc ) then
           fpsice = 1 + b1 * psie 
         else
           fpsice = 1 + b1 * psice + b2 * ( psice - psie ) 
         end if
       else
         stomrs = 5000
  !       PRINT*, 'TEMPERATURE IS LESS THAN OR GREATER THAN THE'
  !       PRINT*, 'CRITICAL TEMPERATURE -- STOMATAL RESISTANCE '
  !       PRINT*, 'SET EQUAL TO 5000 S M-1'
         return
       end if
     
       stomrs = rmin * fs * fpsice * ft
  
     end function stomrs
  
  !
  ! stomc function replaces the STOMC subroutine
  !
  
     real pure function stomc (ft,tf,rmin,b1,psice,fs)
       implicit none
       
       real, intent(in) :: ft, tf, rmin, b1, psice, fs
       real :: fpsice
  
       ! Calclates the critical stomatal resistance  for the ground water potential
  
       fpsice = 1 + b1 * psice
       stomc = rmin * fs * fpsice * ft
  
     end function stomc

end module stomata_mod
