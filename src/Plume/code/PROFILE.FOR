C***********************************************************************
C
C     PROFILE
C     
C     J.V. Ramsdell
C     Pacific Northwest Laboratory
C     P.O. Box 999
C     Richland, Washington 99352
C
C     Created:  12/12/86
C     Revised:  3/23/93
C
C     Description:
C        PROFILE computes the wind at any height in the atmospheric
C        boundary layer using a diabatic wind profile.  (See Panofsky 
C        and Dutton, ATMOSPHERIC TURBULENCE, 1984, Section 6.5 for 
C        details.)   
C
C     Input:    Surface roughness length (m)            ==>    Z0
C               Friction velocity  (m/s)                ==>    USTR
C               Inverse Monin-Obukhov length (1/m)      ==>    MOLINV
C               Stability class                         ==>    IST
C               Height for output wind speed (m)        ==>    HGHT
C
C     Required Modules: None
C
C***********************************************************************

      REAL FUNCTION PROFILE( z0, ustr, molinv, ist, hght )

      IMPLICIT   NONE

      REAL       hght, ht1, mol, molinv, pi, psi, ustr, x, z0 

      INTEGER    ist
       
      pi = 3.14159
      x = 0.0


C **  LIMIT PROFILE TO LOWEST 100M ... OR 3 TIMES M-O LENGTH IN STABLE
C     CONDITIONS

      ht1 = hght
      IF( ist .GT. 4 ) THEN
         mol = 1.0 / molinv             
         ht1 = AMIN1( 3*mol, ht1)
      ENDIF

      IF( hght .GT. 100. ) ht1 = 100.

C **  ESTIMATE WIND SPEED AT HEIGHT

      IF( ist .LE. 3) THEN
         x = (1.0 - 16.0 * ht1 * molinv )**0.25
         psi = ALOG( (1+x**2)/2. * ((1.+x)/2)**2) - 2. * ATAN(x) + pi/2
      ELSE IF( IST .GE. 5 ) THEN 
         psi = -5.0 * ht1 * molinv
      ELSE        
         psi = 0.0
      ENDIF
      
      PROFILE = ustr / 0.4 * ( ALOG( ht1/z0 ) - psi )

      RETURN

      END