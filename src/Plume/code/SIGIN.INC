C-----------------------------------------------------------------------
C
C     SIGIN.INC
C
C     Date:    8/28/96
C
C     Description:   Contains the sigma calculation structure, sigin
C
C-----------------------------------------------------------------------

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
