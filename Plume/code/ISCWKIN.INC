C-----------------------------------------------------------------------
C
C     ISCWKIN.INC
C
C     Date:    8/28/96
C
C     Description:   Contains the structure for ISC wakes, iscwkin
C
C-----------------------------------------------------------------------

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
