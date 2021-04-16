C***********************************************************************
C
C     INVMOL
C
C     J.V. Ramsdell, C.A. Simonen
C     Pacific Northwest Laboratory
C     P.O. Box 999
C     Richland, Washington  99352
C   
C     Created:    5/7/91
C     Revised:    4/20/92
C
C     Description:
C
C        INVMOL computes an estimate of the inverse of the Monin-Obukhov 
C        length (1/L) based the relationship between Turner stability  
C        class, z0, and 1/L shown in Figure 5 in Golder's paper in 
C        Boundary Layer Meteorology 3:47-58.  If lflg = .FALSE., the    
C        function returns a mid-range estimate of 1/L given the 
C        stability class and z0.  If lflg = .TRUE., the function returns
C        a random value within the range assuming a uniform distribution 
C        within the range.  
C
C     Input:   Turner stability                 ==> STAB
C              Surface Roughness Length         ==> Z0  (in meters)
C      c        random flag                      ==> LFLG
C      c        random number                    ==> rv
C
C    Relationship to other Program Units:
C 
C       Makes Calls To:  NONE
C 
C
C***********************************************************************

      REAL FUNCTION INVMOL( Stab, z0 )

      IMPLICIT   NONE 

      REAL       Bnd1L(7), Bnd1U(7),                         !  z0 .LE. 0.01
     &           Bnd2LA(7), Bnd2LB(7), Bnd2UA(7), Bnd2UB(7), ! 0.01< z0 < 0.1
     &           Bnd3LA(7), Bnd3LB(7), Bnd3UA(7), Bnd3UB(7), ! 0.1< z0 < 1.0
     &           Bnd4L(7), Bnd4U(7)                          !  z0 .GE. 1.0
      REAL       Fract, LoBnd, UpBnd, z0
      
      INTEGER    Stab

      DATA Bnd1L / -0.160, -0.106, -0.066, -0.024, 0.005, 0.010, 0.056 /
      DATA Bnd1U / -0.106, -0.066, -0.024,  0.005, 0.010, 0.056, 0.120 /
      DATA Bnd2LA / -0.160, -0.058, -0.014, 0.0, 0.002, 0.008, 0.008 /
      DATA Bnd2LB / 0.0, 0.024, 0.026, 0.012, -0.0015, -0.001, -0.024 / 
      DATA Bnd2UA / -0.058, -0.014, 0.000, 0.002, 0.008, 0.008, 0.12 /
      DATA Bnd2UB / 0.024, 0.026, 0.012, -0.0015, -0.001, -0.024, 0.0 / 
      DATA Bnd3LA / -0.16, -0.046, -0.008, -0.003, 0.002, 0.004, 0.014 /
      DATA Bnd3LB / 0.0, 0.036, 0.032, 0.009, -0.0015, -0.005, -0.018 /
      DATA Bnd3UA / -0.046, -0.008, -0.003, 0.002, 0.004, 0.014, 0.12 /
      DATA Bnd3UB / 0.036, 0.032, 0.009, -0.0015, -0.005, -0.018, 0.0 /
      DATA Bnd4L / -0.16, -0.046, -0.008, -0.003, 0.002, 0.004, 0.014 /
      DATA Bnd4U / -0.046, -0.008, -0.003, 0.002, 0.004, 0.014, 0.12 /

c      IF( lflg ) THEN
c        Fract = rv
c      ELSE
        Fract = 0.5
c      ENDIF

      IF( (z0 .GT. 0.01) .AND. (z0 .LE. 0.1) ) THEN 
         LoBnd = Bnd2LA( Stab ) + Bnd2LB( Stab ) * ALOG10( z0 )
         UpBnd = Bnd2UA( Stab ) + Bnd2UB( Stab ) * ALOG10( z0 )
      ELSE IF( (z0 .GT. 0.1) .AND. (z0 .LT. 1.0) ) THEN  
         LoBnd = Bnd3LA( Stab ) + Bnd3LB( Stab ) * ALOG10( z0 )
         UpBnd = Bnd3UA( Stab ) + Bnd3UB( Stab ) * ALOG10( z0 )
      ELSE IF( z0 .LE. 0.01 ) THEN  
         LoBnd = Bnd1L( Stab ) 
         UpBnd = Bnd1U( Stab ) 
      ELSE IF( z0 .GE. 1.0 ) THEN  
         LoBnd = Bnd4L( Stab ) 
         UpBnd = Bnd4U( Stab ) 
      ELSE
         STOP 'ERROR IN INVMOL'
      ENDIF

      INVMOL = LoBnd + (UpBnd - LoBnd) * Fract

      RETURN
      END