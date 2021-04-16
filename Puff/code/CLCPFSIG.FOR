      SUBROUTINE CLCPFSIG( dist, stab, sigzlim, sigz, sigy, sigparm)
C-----------------------------------------------------------------------
C     ClcPFSig.FOR
C
C     Christian J Fosmire
C
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Date:    7/22/96
C
C     Description:   This program computes sigy and sigz at the given
C                    distance (dist) with an initial sigz and sigy.
C                    The type of sigma parameterization depends upon
C                    the values of sigparm
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      REAL     dist, disty, distz, dum, nwdist, sigy, sigz, sigzlim
      REAL     sigylim
      
      INTEGER  sigparm, stab
      
      DATA  sigylim / 100000./
      
      SELECT CASE( sigparm )
      
         CASE(1)  ! Brigg's Open Country
            
            IF( (sigy .GT. 0) .OR. (sigz .GT. 0) ) THEN
            
               CALL BOCCDIST( stab, sigy, sigz, disty, distz )
            
            ELSE
               
               disty = 0
               distz = 0
               
            ENDIF
            
            nwdist = dist + disty
            CALL BOCCSIG( nwdist, stab, sigy, dum )
            
            IF( sigz .GT. sigzlim ) THEN
               sigz = sigzlim
            ELSE
               nwdist = dist + distz
               CALL BOCCSIG( nwdist, stab, dum, sigz )
               IF( sigz .GT. sigzlim ) sigz = sigzlim
            ENDIF 
                     
         CASE(2)  ! Brigg's Urban Condition

            IF( (sigy .GT. 0) .OR. (sigz .GT. 0) ) THEN
            
               CALL BUCDIST( stab, sigy, sigz, disty, distz )
            
            ELSE
               
               disty = 0
               distz = 0
               
            ENDIF
            
            nwdist = dist + disty
            CALL BUCSIG( nwdist, stab, sigy, dum )
            
            IF( sigz .GT. sigzlim ) THEN
               sigz = sigzlim
            ELSE
               nwdist = dist + distz
               CALL BUCSIG( nwdist, stab, dum, sigz )
               IF( sigz .GT. sigzlim ) sigz = sigzlim
            ENDIF 
            
         
         CASE(3)  ! NRC
         
            CALL NSIG( dist, stab, sigzlim, sigz, sigy )
         
         CASE(4)  ! ISC
            
            IF( (sigy .GT. 0) .OR. (sigz .GT. 0) ) THEN
            
               CALL ISC2DIST( stab, sigy, sigz, disty, distz )
            
            ELSE
               
               disty = 0
               distz = 0
               
            ENDIF
            
            nwdist = dist + disty
            CALL ISC2SIG( nwdist, stab, sigy, dum )
            
            IF( sigz .GT. sigzlim ) THEN
               sigz = sigzlim
            ELSE
               nwdist = dist + distz
               CALL ISC2SIG( nwdist, stab, dum, sigz )
               IF( sigz .GT. sigzlim ) sigz = sigzlim
            ENDIF 
                                                            
                                                            
      END SELECT
   
      IF ( sigy .GT. sigylim ) sigy = sigylim
      
      RETURN
      
      END      