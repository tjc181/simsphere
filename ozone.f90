       SUBROUTINE  OZONE
  use simsphere_mod

!       INCLUDE 'modvars.h'

cc      created 4/5/96; MODIFIED 

CCCCC      coz_sfc=  0. suggested value for c4 plants

CCCCC      coz_air  50 m above the canopy

      CHAX = USTAR**2 / ( UTEN - UAF )

      PES  = ( XLAI / 2.0 ) + 1.
      RAIR =  1.0 / CHAX + RZASCR
	
	rag = 1 / chg
      RROz = 1.9
       

CC    
CC    RROz IS DENSITY OF OZONE (KG M-3) AT 20 C
CC
CC         *****    ONE MOLE OF OZONE IS 48 GRAMS    *****

cc      We don;t apply 1.32 correction for fbare or rair_bare

cc       SUBROUTINE ozone COMPUTES OZONE FLUXES, ozone concentration in canopy
cc        fluxes: from air to leaf Fleaf
cc                from air to stomatal cavities Fm
cc                from air to ground Fg
cc	Total ozone flux from air is sum of above three
cc	 Total ozone flux from air to leaf is sum of first two
cc     Flux from air to leaf surface scaled by lai only - 2 sides of leaf
cc     Flux from air to interior of leaf scaled by lai /pes one sided stomates
cc    Concentration of ozone in canopy CO3af depends on all three fluxes

CC    WE FOLLOW CLOSELY THE SUBROUTINE CO2FLX IN PSUBAMS.
cc    Ozone diffusivity approximately the same as that of CO2
cc          Use same resistances as those for CO2 flux
CC

CC      surface OZONE CONCENTRATION ON LEAF; assume	= 0
CC      - COz_air IS AMBIENT OZONE CONCENTRATION ABOVE CANOPY
cc     This is typically 0.04 to 0.12 ppmv 
CC    MULTIPLY CONCENTRATIONS BY 10-6 TO GET proper units kg / m2 /s
cc    Fluxes typically order of 0.2 micrograms per meter square per second

CC   RESISTANCES FOR STOMATAL RESISTANCE (RST)
CC                   LEAF BOUNDARY LAYER (RAF)
CC                   TOP OF CANOPY (1/CHAX) TO 2 METERS
CC                   SCREEN LEVEL (2 M) TO 50 METERS (RZASCR)
CC   
cc       LEAF BOUNDARY LAYER RESISTANCE RAF CORRECTED FOR OZONE MOLECULAR
CC                   DIFFUSIVITY  (S M-1): FACTOR OF 1.32
cc   stomatal resistance scaled by 1.66 as in co2 flux
cC   
	the_time = ptime

        

cc    rair governs flux from above canopy to interior outside leaf boundary     

       rtot = 1 / ( 1 / ((raf + rcut) * 1.32 /(2 * xlai))  + 1 /(raf
     &   * 1.32 * pes /xlai 
     &    + rst  * 1.66 * pes / xlai ) + 1 / ( rag * 1.32 ))
         	
	 caf = coz_air / ( 1 + rair / rtot)
 
    	 fleaf = rroz * caf / (( raf + rcut) * 1.32 / (2 * xlai ))	*1e3
     & 	  
cc       Fleaf is flux of ozone to leaf, two sides

      fmeso = rroz*caf/((1.32*raf*pes/xlai)+(1.66 * rst * pes / xlai))
	 fmeso=fmeso * 1e3   
cc     Fmeso is flux of ozone into stomates
     
       fg = rroz* caf / (rag * 1.32) *1e3
cc        Fg is flux of ozone into ground

       flux_plant = fleaf + fmeso
cc        Flux_ plant is total flux into plant: leaf and stomate

cc        caf is concentration of ozone in canopy, ppmv
 	  	   	  
cc         No adjustment made for fractional vegetation cover
	
	  fbare = rroz* coz_air / sumo3 *1e3
cc       Fbare is the flux of ozone over bare soil fraction

        fglobal = (flux_plant+ fg) * frveg + fbare* (1 - frveg)
cc       Fglobal is total flux weighted for vegetation and bare soil fractions

CC       UNITS OF FLUX  (FOZONE) ARE KG (OZONE) PER METER SQUARED PER SECOND
CC         IN CONCENTRATION (Coz_air)  PARTS VOLUME PER MILLION (VPM)
cc         Multiply by 10**9 to get in micrograms per meter per second
CC         

      
CC     

        RETURN
        END
