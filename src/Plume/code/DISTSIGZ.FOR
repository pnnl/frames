C***********************************************************************
C
C     DISTSIGZ
C     
C     C.J. Fosmire 
C     Pacific Northwest Laboratory
C     P.O. Box 999
C     Richland, Washington  99352
C
C     Created: 6/12/95
C
C     Description:  Computes distance at which the sigz calculated using
c                   the vertical diffusion coefficients for the Gaussian
C                   plume model using the procedures recommended by the
C                   participants in the atmospheric modeling working
C                   meeting in March 1991 (Ramsdell 1992) is equal to 
c                   the a given sigz
C
C     Input:    vertical turbulance component (m/s)    ==> SIGW
C               stability class                        ==> IST
c               wind speed (m/s)                       ==> WNDSPD   
C               effective release height (m)           ==> ERHT
C               mixing layer thickness  (m)            ==> DEPTH
C               previous diffusion coefficient (m)     ==> SIGZ_IN
C
C     Required Modules:  NONE
C
C***********************************************************************
      SUBROUTINE DISTSIGZ( sigv, sigw, ist, wndspd, erht, depth, sigz, 
     &                     sigy, disty, distz )

      IMPLICIT    NONE

      REAL        a, b, c, depth, disty, distz, erht, sigv, sigw, 
     &            sigy, sigz, soln, tiy, tiz, wndspd
      
      INTEGER     ist      

c     Time Constants

      tiz = 50.
      tiy = 1000.                         
      
C **  Determine if stable or above mixing layer 
C **     to select Sigma Z algorithms

      IF( (erht .GT. depth) .OR. (ist .GT. 4) ) THEN

c **    Have to solve a quadric equation 
        
         b = -0.9 * sigz
         a = tiz * sigw
         c = -sigz
        
         soln = (-b + SQRT(b*b - 4 * a * c)) / (2 * a)
        
         distz = wndspd * tiz * soln * soln

      ELSE

C **    Neutral or Unstable

         distz = sigz * wndspd / sigw

      ENDIF

c **  Solve for distance for sigy
      
      b = -0.9 * sigy
      a = tiy * sigv
      c = -sigy
      
      soln = (-b + SQRT(b*b - 4 * a * c)) / (2 * a)
      
      disty = wndspd * Tiy * soln * soln     

      RETURN
      
      END