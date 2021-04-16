C--------------------------------------------------------------------------
C
C     DIFTER.INC                                       Component of: TADMOD
C
C     Date: 10/4/95
C
C     Description:  Diffusion terms INCLUDE file
C
C---------------------------------------------------------------------------
     
      REAL     chimdt, dt, gcpchi, hsgsq, pufchi, rpew, rpge,
     &         sigv, sigw, sigysq, sigzlim, sigzsq, vexp
      
      INTEGER  sigparm
      
      COMMON /DIFTER/
     &      chimdt, dt, gcpchi, hsgsq, pufchi, rpew, rpge,
     &      sigparm, sigv, sigw, sigysq, sigzlim, sigzsq, vexp
