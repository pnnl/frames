      SUBROUTINE BUCSIG(dist,stabin,sigy,sigz)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     BUCSIG.FOR
c     Christian J Fosmire
c     Pacific Northwest Lab
c     P.O. Box 999
C     Richland, WA 99352
c
c     Created: 4/20/95
c
c     Description:  This subroutine calculates the dispersion coefficients
c      for the Brigg's Urban conditions or the distance at
c      which sigz equal to a given sigz (dodist = true).
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      IMPLICIT none
      
      REAL sigy,sigz,dist
      INTEGER stab,stabin
      
      stab = stabin
      IF( stab.gt.5 ) stab = 5
      
      SELECT CASE(stab)
         CASE(1)
            sigy = 0.32*dist/SQRT(1.0+0.0004*dist)
            sigz = 0.24*dist*SQRT(1.0+0.0010*dist)
        
         CASE(2)
            sigy = 0.32*dist/SQRT(1.0+0.0004*dist)
            sigz = 0.24*dist*SQRT(1.0+0.0010*dist)
         
         CASE(3)
            sigy = 0.22*dist/SQRT(1.0+0.0004*dist)
            sigz = 0.20*dist
        
         CASE(4)
            sigy = 0.16*dist/SQRT(1.0+0.0004*dist)
            sigz = 0.14*dist/SQRT(1.0+0.0003*dist)
        
         CASE(5)
            sigy = 0.11*dist/SQRT(1.0+0.0004*dist)
            sigz = 0.08*dist/SQRT(1.0+0.0015*dist)
      
      END SELECT
                         
      RETURN
      END                    