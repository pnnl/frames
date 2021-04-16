C---------------------------------------------------------------------------
c
c     PUFFSIGZ
c     
c     Date:              August 17, 1995
c
c     Description:  Computes vertical diffusion coefficients for the Gaussian
c                   puff model using the procedures recommended by the
c                   participants in the atmospheric modeling working
c                   meeting in March 1991 (Ramsdell 1992)
c
c     Required modules:  None
c
C---------------------------------------------------------------------------

      REAL FUNCTION PUFFSIGZ ( sigw, ist, time, delt, erht, depth, 
     &                         sigz_in, sigzlim )

      IMPLICIT            NONE

      REAL                delt, depth, erht, midtime, fz, sigw, sigz,
     &                    sigz_in, sigzlim, time 

      INTEGER             ist      
     
C **  SIGMA Z CALCULATION

      sigz = sigz_in

C **  test to see if sigz has reached a maximum value, if it has 
C **  return
    
      PUFFSIGZ = sigz

      IF ( sigz .GE. sigzlim ) RETURN
      
C  Determine if stable or above mixing layer 
C  to select Sigma Z algorithms

      IF ( (erht .GT. depth) .OR. (ist .GT. 4) ) THEN

C  Assume slow vertical diffusion 

         midtime = time - delt / 2.0
         fz = 1 + 0.127279 * SQRT ( midtime )
         sigz = sigz 
     &        + delt * sigw * (1. + 0.0636396 * SQRT ( midtime) )
     &                               /  fz**2 

      ELSE

C  Neutral or Unstable

         sigz = sigz + sigw * delt

      ENDIF

C **  Check Sigma Z for limitation by top of mixing layer
      
      IF ( sigz .GT. sigzlim ) sigz = sigzlim

      PUFFSIGZ = sigz
       
      RETURN 
      
      END
