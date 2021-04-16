      SUBROUTINE BOCCDIST( stabin, sigy, sigz, disty, distz )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     BOCCDist.FOR
c     Christian J Fosmire
c     Pacific Northwest Lab
c     P.O. Box 999
c     Richland, WA 99352
c
c     Created: 6/28/95
c
c     Description:  Calculates the distances (disty and distz)  
c      associated with the given sigy and sigz using the Brigg's
c      Open Country Condition's sigma parameterizatons.
c
c     Modules Required:  None
c      
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT    NONE
      
      REAL        a(6)
      REAL        disty, distz, sigy, sigz
      
      INTEGER     stab,stabin
      
      DATA   a    /0.22, 0.16, 0.11, 0.08, 0.06, 0.04/
      
      stab = stabin

c     Only have six stability classes for Brigg's Open Country

      IF( stab.gt.6 ) stab = 6

c     Calculate distance from sigz
      
      SELECT CASE(stab)
         CASE(1)

            distz = sigz / 0.20

         CASE(2)

            distz = sigz / 0.12

         CASE(3)

            distz = (0.0002 * sigz**2 + SQRT((0.0002 * sigz**2)**2 + 
     &               4 * sigz**2 * 0.08**2)) / (2.0 * 0.08**2)

         CASE(4)

            distz = (0.0015 * sigz**2 + SQRT((0.0015 * sigz**2)**2 + 
     &               4 * sigz**2 * 0.06**2)) / (2.0 * 0.06**2)

         CASE(5)                  
         
            IF( sigz.lt.100 ) THEN
               distz = sigz / (0.03 - 0.0003 * sigz)
            ELSE

c     Assume the distance is infinite

               distz = 1E11

            ENDIF

         CASE(6)

            IF( sigz * 0.0003 .LT. 0.016 ) THEN
               distz = sigz / (0.016 - 0.0003 * sigz)
            ELSE

c      Assume the distance is infinite

               distz = 1E11

            ENDIF

      END SELECT

c     Calculate distance from sigy
      
      disty = (0.0001 * sigy**2 + SQRT((0.0001 * sigy**2)**2 +
     &         4.0 * sigy**2 * a(stab)**2)) / (2.0 * a(stab)**2)  
            
      RETURN                                           
      
      END