
C***********************************************************************
C
C     USTAR
C     
C     J.V. RAMSDELL
C     Pacific Northwest Laboratory
C     P.O. Box 999
C     Richland, Washington 99352
C
C     Created:  10/30/91
C     Revised:  4/21/92
C
C     Description:
C         
C        USTAR computes the friction velocity in the atmospheric
C        boundary layer from a measured wind using a diabatic wind
C        profile assumption.  (See Panofsky and Dutton, ATMOSPHERIC
C        TURBULENCE, 1984, Section 6.5 for details.)   
C
C     Input:    Height of measured wind  (m)            ==>    HT 
C               Measured wind speed  (m/s)              ==>    SPD
C               Surface roughness length (m)            ==>    Z0
C               Inverse Monin-Obukhov length (1/m)      ==>    MOLINV
C               Stability class                         ==>    IST
C
C     Relationship to other program units
C
C        Makes Calls to:  NONE
C
C
C***********************************************************************

      FUNCTION USTAR( ht, spd, z0, molinv, ist )

      IMPLICIT   NONE

      INTEGER    ist
       
      REAL       ht, spd, z0, USTAR, molinv, pi, x, psi

      pi = 3.14159
      x = 0.0

C **  ESTIMATE  USTAR BASED ON STABILITY

      IF( ist .LE. 3 ) THEN                        !  Unstable
         x = (1.0 - 16.0 * ht * molinv )**0.25
         psi = ALOG( (1+x**2)/2. * ((1.+x)/2)**2 ) - 2. * ATAN(x) + pi/2
      ELSE IF( ist .GE. 5 ) THEN                   !  Stable
         psi = -5.0 * ht * molinv
      ELSE                                       !  Neutral
         psi = 0.0
      ENDIF
      
      IF( ht .EQ. 0.0 ) ht = 1.0    !  TEMP CHANGES VALUES TO NOT ZERO
      
      USTAR = 0.4 * spd / ( ALOG( ht/z0 ) - psi ) 

      IF( USTAR .LT. 0.01 ) USTAR = 0.01
     
      RETURN
      END