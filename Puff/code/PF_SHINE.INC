C----------------------------------------------------------------------
C
C     Shine_Pf.INC
C
C     Date:          3/18/1998
C
C     Description:   Shine variables for Puff Model Include file
C
C----------------------------------------------------------------------      
      
      REAL        pdx(20), pdy(20) 
      REAL        drate0
      
      INTEGER     mdist                                                
      
      COMMON /Shine_pf/
     &            pdx, pdy,
     &            drate0,
     &            mdist