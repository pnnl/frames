      SUBROUTINE ISCWKSIG( sigin, iscwkin, dist, sigy, sigz )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     ISCWKSIG.FOR
c     Christian J Fosmire
c     Pacific Northwest Laboratories
c     P.O. Box 999
c     Richland, WA 99352
c
c     Created: 7/5/95
c
c     Description:  This routine calculates the wake influenced sigy
c      and sigz using either the Hubert and Synder or Schuman and Scire
c      parameterization as described in the ISC2 model.
c
c     Subroutines:      CALCSIG
c     Functions:        NONE
c     
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT    NONE      
      
      REAL        bldhgt, bldwth, chkdim, dist, sigy, sigz

      STRUCTURE /sigma_input/
         INTEGER sigopt  ! Flag to denote which sigma parameterization
         INTEGER stab    ! Atmospheric Stability
         REAL    dist    ! The downwind distance (m)
         REAL    disty   ! The distance where sigy = given sigy
         REAL    sigy    ! The horizontial dispersion coefficient (m)
         REAL    sigz    ! The vertical dispersion coefficient (m)
         REAL    sigv    ! The horizontial turbulence stats (m/s)
         REAL    sigw    ! The vertical turbulence stats (m/s)
         REAL    wndspd  ! The wind speed (m/s)
         REAL    relhgt  ! The release height (m)
         REAL    mixhgt  ! The mixing height (m)
         LOGICAL dodist  ! Flag to denote whether calculate sigs or dist
      END STRUCTURE
      
      RECORD /sigma_input/ sigin

      STRUCTURE /iscwk_input/
         LOGICAL tallbld   ! Flag whether building is tall
         LOGICAL squatbld  ! Flag whether building is squat
         LOGICAL ssquatbld ! Flag whether building super squat
         LOGICAL hswake    ! Flag whether using Hubert-Synder
         LOGICAL sswake    ! Flag whether using Schulman-Scire
         LOGICAL sigywk    ! Flag whether building effects sigy
         LOGICAL doiscwk   ! Flag whether doing ISC wake calc
         LOGICAL sslowbnd  ! Flag whether using low bnd eqn for s.squat
         LOGICAL doplume   ! Flag whether to do plume rise
         REAL etemp        ! Effulevent Temperature (deg C)
         REAL atemp        ! Atmospheric Temperature (deg C)
         REAL st_hgt       ! Stack Height (m)
         REAL st_rad       ! Stack Radius (m)
         REAL st_flw       ! Stack Flow (m3/s)
         REAL wndspd       ! Wind Speed (m/s)
         REAL wndmin       ! Minimum wind speed allow in plm rise (m/s)
         REAL bldhgt       ! The height of the building (m)
         REAL bldwth       ! The width of the building (m)
         REAL ssdecay      ! The decay factor on sigz for Sh-Sc wake
         REAL vtzdist      ! The virtual x distance for sigz using wake
         REAL vtydist      ! The virtual x distance for sigy using wake
         REAL r0           ! Depth of Line in SS plume rise
         REAL Ly           ! Length of Line in SS plume rise
      END STRUCTURE
      
      RECORD /iscwk_input/iscwkin

c     Initialize variables
      
      sigy = 0.0
      sigz = 0.0
      bldhgt = iscwkin.bldhgt
      bldwth = iscwkin.bldwth
      
      IF( iscwkin.tallbld ) THEN 
         chkdim = bldwth
      ELSE
         chkdim = bldhgt
      ENDIF
          
      IF( dist .GT. 10.*chkdim ) THEN
         sigin.dist = dist + iscwkin.vtzdist

         CALL CALCSIG( sigin )

         sigz = sigin.sigz
         IF( iscwkin.sigywk ) THEN
            sigin.dist = dist + iscwkin.vtydist

            CALL CALCSIG( sigin )

            sigy = sigin.sigy
         ENDIF
      ELSE
         sigz = 0.7 * chkdim + 0.067 * (dist - 3. * chkdim)
         IF( iscwkin.sigywk ) THEN
         
            IF( iscwkin.tallbld ) THEN
            
               sigy = 0.35 * bldwth + 0.067 * (dist - 3. * bldwth)
            
            ELSE IF( iscwkin.squatbld ) THEN
            
               sigy = 0.35 * bldwth + 0.067 * (dist - 3. * bldhgt)
            
            ELSE IF( (iscwkin.ssquatbld) .AND. (iscwkin.sslowbnd) ) THEN
            
               sigy = 1.75 * bldhgt + 0.067 * (dist - 3. * bldhgt)
            
            ELSE IF( iscwkin.ssquatbld ) THEN
            
               sigy = 0.35 * bldhgt + 0.067 * (dist - 3. * bldhgt)
            
            ENDIF
         ENDIF
      ENDIF
      
      IF( iscwkin.sswake ) THEN
         sigz = iscwkin.ssdecay * sigz
      ENDIF
      
      RETURN                      
      
      END          