C-----------------------------------------------------------------------
C
C     PTSRCIN.INC
C
C     Date:    8/28/96
C
C     Description:   Contains the structure for the Gaussian Pt Src
C
C-----------------------------------------------------------------------

      STRUCTURE /point_input/
         REAL q_src  ! The release rate of the point source (Ci/s)
         REAL wndspd ! The wind speed at the stack height (m/s)
         REAL relhgt ! The effective release height (m)
         REAL rechgt ! The receptor height (m)
         REAL mixhgt ! The mixing layer height (m)
         REAL dwdist ! The downwind distance (m)
         REAL cwdist ! The cross-wind direction (m)
         REAL udepth ! The depth of the uniform layer (m)
         REAL sigy   ! The horizontial dipserion coefficient (m)
         REAL sigz   ! The vertical dispersion coefficient (m) 
         INTEGER numrec ! The number of receptors (m)
      END STRUCTURE
      
      RECORD /point_input/ ptsrcin
 
