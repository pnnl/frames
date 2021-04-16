      SUBROUTINE BOCCSIG( dist, stabin, sigy, sigz )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     BOCCSIG.FOR
c     Christian J Fosmire
c     Pacific Northwest Lab
c     P.O. Box 999
C     Richland, WA 99352
c
c     Created: 3/30/95
c
c     Description:  This subroutine calculates the dispersion 
c        coefficients using the Brigg's Open-Country conditions 
c        parameterization
c   
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      IMPLICIT    NONE
      
      REAL        dist, sigy, sigz
      INTEGER     stab, stabin
      
      stab = stabin

c     Have only six stability class in Brigg's Open Country
      
      IF( stab.gt.6 ) stab = 6

      SELECT CASE(stab)
         CASE(1)
            sigy = 0.22*dist/SQRT(1.0+0.0001*dist)
            sigz = 0.20*dist
         CASE(2)
            sigy = 0.16*dist/SQRT(1.0+0.0001*dist)
            sigz = 0.12*dist
         CASE(3)
            sigy = 0.11*dist/SQRT(1.0+0.0001*dist)
            sigz = 0.08*dist/SQRT(1.0+0.0002*dist)
         CASE(4)
            sigy = 0.08*dist/SQRT(1.0+0.0001*dist)
            sigz = 0.06*dist/SQRT(1.0+0.0015*dist)
         CASE(5)
            sigy = 0.06*dist/SQRT(1.0+0.0001*dist)
            sigz = 0.03*dist/(1.0+0.0003*dist)
         CASE(6)
            sigy = 0.04*dist/SQRT(1.0+0.0001*dist)
            sigz = 0.016*dist/(1.0+0.0003*dist)
      END SELECT
 
      RETURN
      END                    