      SUBROUTINE CALCSIG( sigin )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     CalcSig.For
c     Christian J Fosmire
c     Pacific Northwest Lab
c     P O Box 999
C     Richland, WA 99352
c
c     Created: 4/20/95
c
c     Description:  This subroutine determines which routine to use to
c       calculate the dispersion coefficients or the distance from 
c       the dispersion coeff.  Sigopt controls the routine used, while
c       dodist controls whether calculate distances or coefficents  
c
c     SUBROUTINES: BOCCDIST, BOCCSIG, BUCDIST, BUCSIG, NRCDIST, NSIG1,
c                  ISC2DIST, ISC2SIG, DISTSIGZ, PLUMESIG
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT    NONE

      INCLUDE     'sigin.inc'
            
      REAL        disty, distz, mixhgt, relhgt, sigv, sigw,  
     &            sigzlim, time, wndspd
      
      INTEGER     stab
      

      sigzlim = 1.2 * MAX(sigin.mixhgt,sigin.relhgt)
      stab = sigin.stab
      
c     If release above the mixing layer then assume very stable

      SELECT CASE( sigin.sigopt )
      
         CASE( 1 )

c     Brigg's Open Country Conditions
     
            IF( sigin.dodist ) THEN
               CALL BOCCDIST( stab, sigin.sigy, sigin.sigz, disty, 
     &                        distz )
               sigin.disty = disty
               sigin.dist = distz
            ELSE 
               CALL BOCCSIG( sigin.dist, stab, sigin.sigy, sigin.sigz )
               IF( sigin.sigz .GT. sigzlim ) sigin.sigz = sigzlim
            ENDIF
           
         CASE( 2 )
          
c     Brigg's Urban Conditions
           
            IF( sigin.dodist ) THEN
               CALL BUCDIST( stab, sigin.sigy, sigin.sigz, disty, 
     &                       distz )
               sigin.disty = disty
               sigin.dist = distz
            ELSE
               CALL BUCSIG( sigin.dist, stab, sigin.sigy, sigin.sigz )
               IF( sigin.sigz .GT. sigzlim ) sigin.sigz = sigzlim
            ENDIF
            
         CASE( 3 )
          
c     NRC's Pasquill-Gifford Approx.

c            WRITE (25,'(a,L5)' ) ' Calcsig ', sigin.dodist
c            WRITE (25,'(2x,i2,2x,4f9.2)') stab, sigin.sigy, sigin.sigz,
c     &                 disty, distz 
            IF( sigin.dodist ) THEN
               CALL NRCDIST( stab, sigin.sigy, sigin.sigz, disty, 
     &                       distz )
               sigin.disty = disty
               sigin.dist = distz
            ELSE    
               
               sigin.sigy = 0
               sigin.sigz = 0
               CALL NSIG1( sigin.dist, stab, sigzlim, sigin.sigz, 
     &                     sigin.sigy )
            ENDIF
             
         CASE( 4 )
          
c     ISCST2 Pasquill-Gifford Approx.

            IF( sigin.dodist ) THEN
               CALL ISC2DIST( stab, sigin.sigy, sigin.sigz, disty, 
     &                        distz )
               sigin.disty = disty
               sigin.dist = distz
            ELSE    
               CALL ISC2SIG( sigin.dist, stab, sigin.sigy, sigin.sigz )
               IF(sigin.sigz .GT. sigzlim ) sigin.sigz = sigzlim
            ENDIF
            
         CASE( 5 )
c          
c     Ramsdell 1991 sigmas
c     requires more input
c           
            sigv = sigin.sigv
            sigw = sigin.sigw   
            relhgt = sigin.relhgt
            mixhgt = sigin.mixhgt
            wndspd = sigin.wndspd
c          
            IF( .NOT.sigin.dodist ) THEN
               time = sigin.dist / wndspd
               CALL PLUMESIG ( time, relhgt, stab, mixhgt, sigv, sigw, 
     &                         0.0, 0.0, sigzlim, sigin.sigy, 
     &                         sigin.sigz )
            ELSE
               CALL DISTSIGZ( sigv, sigw, stab, wndspd, relhgt, mixhgt,
     &                        sigin.sigz, sigin.sigy, disty, distz )
               sigin.disty = disty
               sigin.dist = distz   
            ENDIF                   
            
      END SELECT
      
      RETURN
      
      END      