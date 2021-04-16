      SUBROUTINE BUCDIST( stabin, sigy, sigz, disty, distz )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     BUCDist.FOR
c     Christian J Fosmire
c     Pacific Northwest Laboratories
c     P.O. Box 999
c     Richland, WA 99352
c
c     created: 6/28/95
c
c     description:  Calculates the distances (disty distz) for which
c       sigy and sigz equal the given sigy and sigz for the Brigg's
c       Urban Conditions.
c
c     Calls: CubicSlv
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT    NONE
      
      REAL        ay(5), soln(3)
      REAL        a1, a2, a3, a4, disty, distz, sigy, sigz
      
      INTEGER     i, numsoln, stab, stabin
      
      DATA ay /0.32, 0.32, 0.22, 0.16, 0.11/
      
      stab = stabin

c     Have only five stability classes in Brigg's Open Country

      IF( stab.gt.5 ) stab = 5

c     Calculate distance from sigz
      
      SELECT CASE(stab)
         
         CASE(1)

c     Have to solve a cubic equation..this is the real root
            
            a1 = 1
            a2 = 1/0.001
            a3 = 0
            a4 = -sigz*sigz
            CALL CUBICSLV(a1,a2,a3,a4,soln,numsoln)

c     if just one soln...then it's the first one, otherwise look for
c      the largest one

            IF( numsoln.eq.1 ) THEN
               distz = soln(1)
            ELSE
               distz = soln(1)
               DO i = 1,2
                  IF( distz.lt.soln(i+1) ) distz = soln(i+1)
               ENDDO
            ENDIF
              
         CASE(2)

c     Have to solve a cubic equation..this is the real root
            
            a1 = 1
            a2 = 1/0.001
            a3 = 0
            a4 = -sigz*sigz
            CALL CUBICSLV(a1,a2,a3,a4,soln,numsoln)

c     if just one soln...then it's the first one, otherwise look for
c      the largest one

            IF( numsoln.eq.1 ) THEN
               distz = soln(1)
            ELSE
               distz = soln(1)
               DO i = 1,2
                  IF( distz.lt.soln(i+1) ) distz = soln(i+1)
               ENDDO
            ENDIF  
      
         CASE(3)

            distz = sigz/0.2
        
         CASE(4)

            distz = (0.0003*sigz**2 + SQRT((0.0003*sigz**2)**2 + 
     &               4*0.14**2*sigz**2))/(2.0*0.14**2)
        
         CASE(5)
            distz = (0.0015*sigz**2 + SQRT((0.0015*sigz**2)**2 + 
     &               4*0.08**2*sigz**2))/(2.0*0.08**2)
      
      END SELECT

c     Calculate the distance from sigy
      
      disty = (0.0004*sigy**2 + SQRT((0.0004*sigy**2)**2 + 
     &         4*sigy**2*ay(stab)**2))/(2.0*ay(stab)**2)
     
      RETURN
      END      