      SUBROUTINE  CALCRISE( isrc, dist, fnrise, iscwkin, hgt, sigbouy )
C-----------------------------------------------------------------------
C     CALCRISE
C
C     Christian J Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Date:    7/25/96
C
C     Description:   Computes for a given distance the final height
C                    and the bouyancy associated with it                
C
C     Subroutines:   NONE
C     Functions:     SSPLMRSE, TNPLMRSE
C
C-----------------------------------------------------------------------
      IMPLICIT NONE 
      
      INCLUDE  'parm.inc'
      INCLUDE  'metdata.inc'
      INCLUDE  'srcrec.inc'
      INCLUDE  'iscwkin.inc'
      
      REAL     SSPLMRSE
      REAL     TNPLMRSE
      REAL     dist, fnrise, hgt, plmrise, relhgt, sigbouy, tnrise
      
      INTEGER  isrc

c     Do either Scire and Schubert or normal transitional rise
               
      IF( (iscwkin.doiscwk) .AND. (iscwkin.sswake) ) THEN
         tnrise = SSPLMRSE( iscwkin.st_rad, iscwkin.st_flw, 
     &                      iscwkin.etemp, iscwkin.atemp, 
     &                      iscwkin.wndspd, iscwkin.wndmin, dist, 
     &                      iscwkin.r0, iscwkin.Ly, stab )
               
      ELSE
         tnrise =  TNPLMRSE( relhgt, st_rad(isrc), St_flow(isrc), 
     &                       Etemp(isrc), atemp, relspd, stab, mixhgt, 
     &                       dist, wndmin )
      ENDIF

c     The Plume rise is the smaller of the transitional or final rise     

      plmrise = MIN(fnrise,tnrise)

c     The effective height is the sum of the release height and plume
c     rise.  It cannot go above the mixed layer unless the 
c     release height is above the mixed layer and it cannot go below 
c     the ground.  
      
      relhgt = Src_Var1(isrc)
      
      hgt = relhgt + plmrise
               
      IF( relhgt .LE. mixhgt ) THEN
         
         IF( hgt .GT. mixhgt ) hgt = mixhgt
      
      ENDIF         
      
      IF( hgt .LT. 0. ) hgt = 0.0

      IF( .NOT.iscwkin.sswake ) THEN
         sigbouy = (hgt - relhgt) / 3.5
      ENDIF
               
      RETURN
      
      END