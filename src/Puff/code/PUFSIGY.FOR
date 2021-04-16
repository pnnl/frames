
C---------------------------------------------------------------------------
c
c     PUFSIGY
c     
c     Date:           August 17, 1995
c
c     Description:  Computes horizontal diffusion coefficients for the 
c                   Gaussian puff model
c
c     Required modules:  None
c
c---------------------------------------------------------------------------

      REAL FUNCTION PUFSIGY ( age, sy_cnst, sigv, delt, sigy_in )

      IMPLICIT        NONE

      REAL            age, delt, sigv, sigy_in, symax, sy_cnst

      DATA            symax / 100000. /

      IF ( sigy_in .GE. symax ) THEN
         PUFSIGY = sigy_in
         RETURN
      ENDIF

C  SIGMA Y CALCULATION

      IF ( age .LE. 3600 ) THEN

         PUFSIGY = sigy_in + 0.5 * sigv * delt

      ELSE      
  
         PUFSIGY = sigy_in + sy_cnst * delt

      ENDIF
        
      RETURN    
      
      END
