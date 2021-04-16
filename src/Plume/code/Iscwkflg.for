      SUBROUTINE ISCWKFLG( iscwkin, sigin )
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     ISCWkFLG.For
c     Christian J Fosmire
c     Pacific Northwest Laboratories
c     P.O. Box 999
c     Richland, WA 99352
c
c     Created: 6/28/95
c
c     Description:  Set up the various flags for computing the ISC wakes
c                    and compute various parameters to be used in the
c                    wake calculations
c
c     Subroutines:      CalcSig   
c     Functions:        None
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE

      INCLUDE  'sigin.inc'
      INCLUDE  'iscwkin.inc'
      
      REAL     atempk, bldhgt, bldwth, Bm, dist, etempk, Fm, heff, 
     &         hgtchk, plmrstn, wspd, w0

c  initally, set the various flags to false
      
      iscwkin.tallbld = .FALSE.
      iscwkin.squatbld = .FALSE.
      iscwkin.ssquatbld = .FALSE.
      iscwkin.hswake = .FALSE.
      iscwkin.sswake = .FALSE.
      iscwkin.sigywk = .FALSE.
      iscwkin.vtzdist = 0
      iscwkin.vtydist = 0
      iscwkin.ssdecay = 1.0
      
      bldhgt = iscwkin.bldhgt
      bldwth = iscwkin.bldwth
      
c  First check if have a building height and a building width
c  If not...then turn don't do ISC wake
            
      IF( (bldhgt .LE. 0) .OR. (bldwth .LE. 0) ) THEN
      
         iscwkin.doiscwk = .FALSE.

d        WRITE(25,*) 'No building width or height, so no wake calc.'

         RETURN
      ENDIF

c  Set the release height

      heff = iscwkin.st_hgt
      
c  If plume rise, compute momentum rise at 2*building height
c  do not include downwash
      
      IF ( iscwkin.doplume ) THEN

         etempk = iscwkin.etemp + 273.16
         atempk = iscwkin.atemp + 273.16
         wspd = iscwkin.wndspd

         IF( wspd .LT. iscwkin.wndmin ) wspd = iscwkin.wndmin
                                          
         w0 = iscwkin.st_flw / (3.14159 * iscwkin.st_rad*iscwkin.st_rad)
         Fm = atempk * w0*w0 * iscwkin.st_rad*iscwkin.st_rad / etempk
         Bm = 0.4 + 1.2 * wspd / w0
      
         dist = 2 * bldhgt
      
         plmrstn = (3.0 * Fm * dist/(Bm*Bm * wspd*wspd))**(1./3.) 
      
         heff = iscwkin.st_hgt + plmrstn

c  if plume released below the mixing layer...then plume can go only
c  as height as the mixing layer
      
         IF( iscwkin.st_hgt .LE. sigin.mixhgt ) THEN
            IF( heff .GT. sigin.mixhgt ) heff = sigin.mixhgt
         ENDIF
         
      ENDIF   
         
c   Check if stack height is less than hb + 0.5MIN(hb,hw)
c   If it is, then doing Schulman and Scire 
      
      hgtchk = bldhgt + 0.5 * AMIN1(bldhgt,bldwth)
            
      IF( iscwkin.st_hgt .LE. hgtchk ) THEN
         
         iscwkin.sswake = .TRUE.
         
         hgtchk = bldhgt + 2.0 * AMIN1(bldhgt,bldwth)
         
         IF( heff .GT. hgtchk) THEN
            iscwkin.doiscwk = .FALSE.
            iscwkin.sswake = .FALSE.
            RETURN
         ENDIF
      
      ELSE

c  Doing Hubert and Synder wake..so check if plume is effected by
c  the building (plume height > hb + 1.5*AMIN(hb,hw))

         hgtchk = bldhgt + 1.5 * AMIN1(bldhgt,bldwth)

c  If plume height is greater than hb + 1.5*AMIN(hb,hw), then
c  plume is not effected by the building

         IF( heff .GT. hgtchk ) THEN
            iscwkin.doiscwk = .FALSE.
            RETURN
         ENDIF

      ENDIF
      
c  find out if the building is tall (hb>hw), supersquat (hw > 5*hb),
c  or just squat (5*hb >= hw > hb)
         
      IF( bldhgt .GT. bldwth ) THEN
         iscwkin.tallbld = .TRUE.
      ELSE
         IF( bldwth .GT. 5 * bldhgt ) THEN
            iscwkin.ssquatbld = .TRUE.
         ELSE
            iscwkin.squatbld = .TRUE.
         ENDIF
      ENDIF

      IF( iscwkin.sswake ) THEN

c  For Schulman and Scire, calculate the decay

         IF( heff .LT. bldhgt ) THEN

            iscwkin.ssdecay = 1

         ELSE IF( heff .LT. bldhgt+2.0*AMIN1(bldhgt,bldwth) ) THEN

            iscwkin.ssdecay = ((bldhgt - heff)/
     &                        (2 * AMIN1(bldhgt,bldwth))) + 1
         ELSE

            iscwkin.ssdecay = 0.0

         ENDIF

c  Compute the parameters R0 and Ly for Schulman and Scire Building Downwash

         sigin.dist = 3.*AMIN1(bldhgt,bldwth)
         sigin.dodist = .FALSE.
         
         CALL CALCSIG( sigin )
         
         iscwkin.r0 = SQRT(2.0) * iscwkin.ssdecay * sigin.sigz

         IF( sigin.sigy .GT. sigin.sigz ) THEN
            iscwkin.Ly = SQRT(2. * 3.14159) * (sigin.sigy - sigin.sigz)
         ELSE
            iscwkin.Ly = 0.0
         ENDIF
      ENDIF
      
c  Calculate virtual distance   

      IF( iscwkin.tallbld ) THEN
         dist = 10 * bldwth
         sigin.sigz = 1.169 * bldwth
      ELSE
         dist = 10 * bldhgt
         sigin.sigz = 1.169 * bldhgt
      ENDIF
      
      sigin.sigz = iscwkin.ssdecay * sigin.sigz
      sigin.dodist = .TRUE.
      
      CALL CALCSIG( sigin )
      
      iscwkin.vtzdist = sigin.dist - dist   

c     Don't allow the virtual source distance to be less than zero
      
      IF ( iscwkin.vtzdist .LT. 0 ) iscwkin.vtzdist = 0
      
c     For Hubert and Synder,  also have horizontial dispersion 
c     if plume rise is less than 1.2*building height
      
      IF( heff .LT. 1.2 * bldhgt ) THEN
         iscwkin.sigywk = .TRUE.
         dist = 10 * bldhgt

         IF( iscwkin.squatbld ) THEN
            sigin.sigy = 0.35 * bldwth + 0.469 * bldhgt
         ELSE IF( iscwkin.sslowbnd ) THEN
            sigin.sigy = 2.219 * bldhgt
         ELSE
            sigin.sigy = 0.819 * bldhgt
         ENDIF
         
         sigin.dodist = .TRUE.

         CALL CALCSIG( sigin )

         iscwkin.vtydist = sigin.dist - dist 

c     Don't allow the virtual source distance be less than zero
         
         IF( iscwkin.vtydist .LT. 0 ) iscwkin.vtydist = 0

      ELSE

         iscwkin.vtydist = 0.0

      ENDIF
      
      RETURN                     
      
      END        