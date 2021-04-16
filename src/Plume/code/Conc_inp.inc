C-----------------------------------------------------------------------
C     CONC_INP.INC
C
C     Date:    8/29/96
C
C     Description:   Contains the structure for calculating the
C                    area guasssian plume equation
C
C-----------------------------------------------------------------------

      STRUCTURE /conc_input/
         REAL wndspd   ! Wind Speed in m/s
         REAL q_area   ! Area Emission Rate in Ci(g)/(s m2)
         REAL rel_hgt  ! Effective Release Height in m
         REAL rec_hgt  ! Effective Receptor Height in m
         REAL cwdist   ! Crosswind distance in m
         REAL dwdist   ! Downwind Distance in m
         REAL zmin     ! Height used is rel_ and rec_hgt are zero (m)
         REAL lenside  ! Radius of the area (a) in m
         REAL mix_hgt  ! Height of the mixing layer in m
         REAL minsigz  ! Minimum Sigmaz allowed in m
         REAL udepth   ! The depth at which vertical mixing uniform
         REAL sigv     ! Sigma v
         REAL sigw     ! Sigma w
         INTEGER*2 stab   ! Atmospheric Stability
         INTEGER*2 sigopt ! Flag for which sigma parameterization  
         INTEGER   numrec ! Number of receptors
      END STRUCTURE

      RECORD /conc_input/conc_inp
