      SUBROUTINE  PRE_PLUM( isrc, fnrise, crtdist, udepth, 
     &                      iscwkin, sigin )
C----------------------------------------------------------------------
C     PRE_PLUM
C
C     Christian J Fosmire
C     Pacific Northwest National Lab
C     P.O Box 999
C     Richland, WA 99352
C
C     Date:    7/26/96
C
C     Description:      Set up the various variables for the guassian
C                       plume assuming a point source                  
C
C     Subroutines:      ISCWKFLG, CALCSIG
C     Functions:        FNPLMRS, PROFILE, TURBSIGV, TURBSIGW
C
C----------------------------------------------------------------------
      IMPLICIT NONE
      
      INCLUDE  'parm.inc'
      INCLUDE  'metdata.inc'
      INCLUDE  'srcrec.inc' 
      INCLUDE  'sigin.inc'
      INCLUDE  'iscwkin.inc'
      
      REAL     FNPLMRS
      REAL     PROFILE
      REAL     TURBSIGV
      REAL     TURBSIGW
      
      REAL     crtdist, fnrise, heff, relhgt, sigv, sigw, sigz,
     &         udepth
      
      INTEGER  isrc      
            
c     Compute winds at release height  Note: If release height is 
c      below 12 meters then calculate the wind at 
c      10 m else use the release height.

      relhgt = Src_Var1(isrc)

c      WRITE ( 25,'(a,3f9.4,i3,f9.2)')  'Subroutine Pre_Plume ', z0, 
c     &                                  ustr, molinv, stab, relhgt
      
      IF( relhgt .GT. 12.0 ) THEN
         relspd = PROFILE( z0, ustr, molinv, stab, relhgt )
      ELSE          
         relspd = PROFILE( z0, ustr, molinv, stab, 10.0 )
      ENDIF

c     If plume rise...calculate the final plume rise

      IF( plmflg(isrc) ) THEN
         fnrise = FNPLMRS( stab, relhgt, St_rad(isrc),
     &                     St_flow(isrc), relspd, Etemp(isrc), 
     &                     Atemp, mixhgt, wndmin )
     
      ENDIF

c  If calculating turbulence sigmas...calculate sigv and sigw
c  Note:  use the final height of the plume
          
      heff = relhgt + fnrise
      
      IF( heff .LT. 0 ) heff = 0
      IF( (heff .GT. mixhgt) .AND. (relhgt.LT.mixhgt) ) heff = mixhgt
          
      sigv = TURBSIGV( stab, ustr, heff, mixhgt, molinv )
      sigw = TURBSIGW( stab, ustr, heff, mixhgt, molinv, sigv )
      sigin.sigv = sigv
      sigin.sigw = sigw
      sigin.wndspd = relspd


c     Set the flags for doing iscwake
            
      iscwkin.doiscwk = iscwakflg(isrc)
      iscwkin.sslowbnd = sslowbnd(isrc)
      
c     If doing ISC wake, then check whether wake applies, whether doing
c     Schulman Scire or just Hubert Synder, etc.

      IF( iscwkin.doIscWk ) THEN

         sigin.sigopt = sigparm
         sigin.stab = stab
         sigin.relhgt = relhgt
         sigin.mixhgt = mixhgt
               
         iscwkin.st_hgt = relhgt
         iscwkin.doplume = plmflg(isrc)
         IF ( iscwkin.doplume ) THEN
            iscwkin.st_rad = st_rad(isrc)
            iscwkin.st_flw = st_flow(isrc)
            iscwkin.wndmin = wndmin
         ENDIF
         iscwkin.wndspd = relspd
         iscwkin.bldhgt = bldhgt(wndsec,isrc)
         iscwkin.bldwth = bldwth(wndsec,isrc)
               
         CALL ISCWKFLG( iscwkin, sigin )
               
      ENDIF     

c     Calculate at what distance, crtdist, sigz = 1.2*udepth
c     udepth is either the mixing height or effective release height
c     if above the mixing layer 

      udepth = MAX(fnrise,mixhgt)
               
c     Need to account for the bouyancy induced plume rise on sigz               

      IF( (BIDFlg(isrc)) .AND. (.NOT.iscwkin.sswake) ) THEN
         sigz = SQRT((1.2 * udepth)**2 - (fnrise / 3.5)**2)
      ELSE
         sigz = 1.2 * udepth
      ENDIF

      sigin.sigopt = sigparm
      sigin.stab = stab
      sigin.relhgt = relhgt
      sigin.mixhgt = mixhgt

c  If release height is above mixing layer then never have a crtdist
               
      IF( relhgt .GT. mixhgt ) THEN
      
         crtdist = 9999999.0
         
      ELSE
         
         sigin.sigz = sigz
         sigin.dodist = .TRUE.
               
         CALL CALCSIG( sigin )
               
         crtdist = sigin.dist
      
      ENDIF
      
d     WRITE(25,*)'for sigz = ',sigz,' crtdist = ',crtdist

      RETURN
      
      END