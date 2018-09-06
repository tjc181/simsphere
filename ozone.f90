subroutine  OZONE
  use simsphere_mod

!       INCLUDE 'modvars.h'

!      created 4/5/96; MODIFIED 

!      coz_sfc=  0. suggested value for c4 plants

!      coz_air  50 m above the canopy

  CHAX = USTAR**2 / ( UTEN - UAF )

  PES  = ( XLAI / 2.0 ) + 1.
  RAIR =  1.0 / CHAX + RZASCR
 
  rag = 1 / chg
  RROz = 1.9
       

!     
!     RROz IS DENSITY OF OZONE (KG M-3) AT 20 C
! 
!          *****    ONE MOLE OF OZONE IS 48 GRAMS    *****

!       We don;t apply 1.32 correction for fbare or rair_bare

!        SUBROUTINE ozone COMPUTES OZONE FLUXES, ozone concentration in canopy
!         fluxes: from air to leaf Fleaf
!                 from air to stomatal cavities Fm
!                 from air to ground Fg
!  Total ozone flux from air is sum of above three
!   Total ozone flux from air to leaf is sum of first two
!      Flux from air to leaf surface scaled by lai only - 2 sides of leaf
!      Flux from air to interior of leaf scaled by lai /pes one sided stomates
!     Concentration of ozone in canopy CO3af depends on all three fluxes

!     WE FOLLOW CLOSELY THE SUBROUTINE CO2FLX IN PSUBAMS.
!     Ozone diffusivity approximately the same as that of CO2
!           Use same resistances as those for CO2 flux
! 

!       surface OZONE CONCENTRATION ON LEAF; assume = 0
!      - COz_air IS AMBIENT OZONE CONCENTRATION ABOVE CANOPY
!     This is typically 0.04 to 0.12 ppmv 
!    MULTIPLY CONCENTRATIONS BY 10-6 TO GET proper units kg / m2 /s
!    Fluxes typically order of 0.2 micrograms per meter square per second

!   RESISTANCES FOR STOMATAL RESISTANCE (RST)
!                   LEAF BOUNDARY LAYER (RAF)
!                   TOP OF CANOPY (1/CHAX) TO 2 METERS
!                   SCREEN LEVEL (2 M) TO 50 METERS (RZASCR)
!   
!       LEAF BOUNDARY LAYER RESISTANCE RAF CORRECTED FOR OZONE MOLECULAR
!                   DIFFUSIVITY  (S M-1): FACTOR OF 1.32
!   stomatal resistance scaled by 1.66 as in co2 flux
!   
  the_time = ptime

        

!    rair governs flux from above canopy to interior outside leaf boundary     

  rtot = 1 / ( 1 / ((raf + rcut) * 1.32 /(2 * xlai))  + 1 /(raf         &
         * 1.32 * pes /xlai + rst  * 1.66 * pes / xlai ) + 1 / ( rag * 1.32 ))
          
  caf = coz_air / ( 1 + rair / rtot)
 
  fleaf = rroz * caf / (( raf + rcut) * 1.32 / (2 * xlai )) *1e3
!       Fleaf is flux of ozone to leaf, two sides

  fmeso = rroz*caf/((1.32*raf*pes/xlai)+(1.66 * rst * pes / xlai))
  fmeso=fmeso * 1e3   
!     Fmeso is flux of ozone into stomates
     
  fg = rroz* caf / (rag * 1.32) *1e3
!        Fg is flux of ozone into ground

  flux_plant = fleaf + fmeso
!        Flux_ plant is total flux into plant: leaf and stomate

!        caf is concentration of ozone in canopy, ppmv
           
!         No adjustment made for fractional vegetation cover
 
  fbare = rroz* coz_air / sumo3 *1e3
!       Fbare is the flux of ozone over bare soil fraction

  fglobal = (flux_plant+ fg) * frveg + fbare* (1 - frveg)
!       Fglobal is total flux weighted for vegetation and bare soil fractions

!       UNITS OF FLUX  (FOZONE) ARE KG (OZONE) PER METER SQUARED PER SECOND
!         IN CONCENTRATION (Coz_air)  PARTS VOLUME PER MILLION (VPM)
!         Multiply by 10**9 to get in micrograms per meter per second
!         

      
!     
  return
end

