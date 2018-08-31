      BLOCK DATA

      INCLUDE 'modvars.h'

*/ Constants

       DATA Y,ALBG,ALBF,XMOD,SIGF /1.0,4*0.0 /
     *	    HG,AHUM,RNET/3*0.0/,
     *	    (QD(I),I=1,21)/21*0.0/

      DATA CHGT,USTAR,TSTAR,HEAT /4*0.0/,
     *	   HGT,ZA,DELT,CTHETA,DHET,EVAP/2*50.,2*1,2*0/

      DATA SIGMA,LE,KARMAN,GRAV,R,RAD,CP
     *	/5.6521E-8,2.5E6,0.4,9.78,287.5,1.6E-5,1004.832/
     *	 DELTA/90/

      DATA MOL,BULK,IFIRST,NLVLS /3*0,5/


*/ Initialization of variables

      DATA JCAP/1/


      END

      SUBROUTINE  BLOCK ()
      END
