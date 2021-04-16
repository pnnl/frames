      SUBROUTINE MODSIGS( sigin, iscwkin, dist, isrc, wndsec, 
     &                    sigbouy, sigy, sigz )
C----------------------------------------------------------------------
C     MODSIGS
C
C     Christian J Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Date:    7/25/96
C
C     Description:   This subroutine modifies sigy and sigz for 
C                    building wake, low wnd speed, or bouyancy
C                    induced dispersion                           
C
C     Subroutines:   ISCWKSIG, ENCHSIG
C
C----------------------------------------------------------------------
      IMPLICIT NONE
      
      INCLUDE  'parm.inc'
      INCLUDE  'srcrec.inc'
      INCLUDE  'sigin.inc'
      INCLUDE  'iscwkin.inc'
      
      REAL     dist, ensigy2, ensigz2, sigbouy, sigy, sigz, wksigy,
     &         wksigz
      
      INTEGER  isrc, wndsec


c     Modify for wake (ISC)

      IF( iscwkin.doiscwk ) THEN

         CALL ISCWKSIG( sigin, iscwkin, dist, wksigy, wksigz )
         sigy = AMAX1(sigy,wksigy)
         sigz = AMAX1(sigz,wksigz)

c     Modify for Bouyancy Induced

         IF( (BIDFLG(isrc)) .AND. (.NOT.iscwkin.sswake) ) THEN
            sigz = SQRT(sigz*sigz + sigbouy*sigbouy)
            sigy = SQRT(sigy*sigy + sigbouy*sigbouy)
         ENDIF

c     Modify using PNL correction for high and low wnd speeds
                
      ELSE IF( (Hgwndflg(isrc)) .OR. (Lwwndflg(isrc)) ) THEN

         CALL ENHCSIG( Hgwndflg(isrc), Lwwndflg(isrc), sigin.sigopt, 
     &                 sigin.wndspd, area(wndsec,isrc), ensigy2, 
     &                 ensigz2, dist )
     
         sigy = SQRT(sigy*sigy + ensigy2)
         sigz = SQRT(sigz*sigz + ensigz2)
            
      ENDIF 
      
      RETURN
      
      END