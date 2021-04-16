      SUBROUTINE DEPPLUM( plmtyp, dv, wcoef, concrec, relspd,
     &                    dist, y, sigy, a, drydep, wetdep )
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     DEPPLUM.FOR
c     Chrisitian J Fosmire
c     Pacific Northwest Lab
c     P.O. Box 999
C     Richland, WA 99352
c
c     Created;6/2/95
c
c     Description:  This function computes the deposition rate which can
c          be multiplied by the time to get the total surface deposition
c
c     Subroutines:   NONE
c     FUNCTIONS:     EFR1
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT    NONE
      
      REAL        ERF1
      REAL        a, concrec, dist, drydep, dv, exparg, horzdisp, 
     &            relspd, secwdth, sigy, wcoef, wetdep, y
      
      INTEGER     plmtyp
      
c     If plmtyp = 0 then normal sector averaged point source
c     if plmtyp = 1 then non-sector averaged point source
c     if plmtyp = 2 then non-sector averaged area source
      
      IF( plmtyp .EQ. 1 ) THEN
 
         exparg = y*y / (2 * sigy*sigy)
 
         IF( exparg .LT. 1E-3 ) THEN
            horzdisp = 1
         ELSE IF( exparg .LT. 15 ) THEN
            horzdisp = EXP(-exparg)
         ELSE 
            horzdisp = 0
         ENDIF
         drydep = dv * concrec
         wetdep = wcoef * horzdisp / (SQRT(2 * 3.14159) *
     &            relspd * sigy)
     
      ELSE IF( plmtyp .EQ. 2 ) THEN

         IF( y .NE. 0 ) THEN

            horzdisp = 0.5 * (ERF1((a + y) / (SQRT(2.) * sigy)) +
     &                        ERF1((a - y) / SQRT(2.) * sigy))

            IF (horzdisp .LT. 1E-7) THEN
               horzdisp = 0
            ENDIF  
            
         ELSE
            
            horzdisp = 1
         
         ENDIF
         
         drydep = dv * concrec 
         wetdep = wcoef * horzdisp / ( a * relspd )
      
      ELSE 
      
         secwdth = AMAX1(4.0*sigy,2*3.14159*dist/16)
         drydep  = dv * concrec 
         wetdep  = wcoef / (relspd * secwdth)
      
      ENDIF
           
      RETURN                         
      
      END      