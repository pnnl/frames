      REAL FUNCTION SSPLMRSE( stk_rad, stk_flow, etemp, atemp, wndspd, 
     &                        wndmin, dist, r0, Ly, stab )
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     SSPLMRSE.FOR
c     Christian J Fosmire
C     Pacific Northwest Laboratories
c     P.O. Box 999
c     Richland, WA 99352
c
c     Created: 7/5/95
c
c     Description:  This code calculates the Scire and Schulman (1980) 
c                   transition rise found in the ISC code.
c
c     Subroutines:      CUBICSLV
c     Functions:        None
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT    NONE
      
      REAL        fnsoln(2), s0(3), soln(3)
      REAL        atemp, atempk, a1, a2, a3, a4, Bb, Bm, delt, dist, 
     &            etemp, etempk, Fb, Fm, ly, pi, risebouy, risemom,    
     &            r0, stk_flow, stk_rad, wndmin, wndspd, risema, riseba, 
     &            wp, wspd, s   
 
      INTEGER     i, j, numsoln, stab
      
      DATA        s0 / 0.02, 0.035, 0.05 /
      
C **  Prepare to compute momentum and buoyancy fluxes
      
      pi = 3.14159      
      wp =  stk_flow / ( pi * stk_rad**2 ) 
      etempk = etemp + 273.16
      atempk = atemp + 273.16   
      delt  = etempk - atempk
      wspd = wndspd         
      
      IF( wspd.lt.wndmin ) wspd = wndmin
      
c**   Compute Buoyancy Flux

      fb = 9.8 * delt * wp * stk_rad*stk_rad / etempk      

      IF ( fb .LT. 1E-10) fb = 1E-10

c     Calculate Transitional Plume Rise

      fm = (atempk / etempk) * wp*wp * stk_rad*stk_rad
      bm = 0.3333 + wspd / wp
      bb = 0.6
      
c     Determine a4

      risemom = (3.0 * fm * dist / (bm*bm * wspd*wspd))
      risebouy = (3. * fb * dist*dist / (2.*bb*bb*wspd*wspd*wspd))


c     IF Stable compare the stable vs the neutral soln and take smaller

      IF( stab .GT. 4 ) THEN
         s = 9.8 * s0(stab-4) / atempk
         risema = (3 * fm * sin(dist * SQRT(s)) / wspd)/
     &             (bm*bm * wspd * SQRT(s))
         risemom = MIN( risemom, risema)
         
         riseba = (6 * fb)/(bb*bb * wspd * s)
         
         risebouy = MIN(risebouy, riseba)
      ENDIF
            
         
c     Solve the cubic equation  a1*Z^3 + a2*Z^2 + a3*Z + a4 = 0
      
      a1 = 1
      a2 = 3 * ly / (bb * pi) + 3 * r0 / bb
      a3 = 6 * r0 * ly / (pi * bb*bb) + 3. * r0*r0 / (bb*bb)

c     Take the larger of the bouyancy or momentum rise 
 
      DO j = 1, 2
         IF( j .EQ. 1 ) THEN
            a4 = -risemom
         ELSE
            a4 = -risebouy
         ENDIF

         CALL CUBICSLV( a1, a2, a3, a4, soln, numsoln )

         IF( numsoln .NE. 1 ) THEN
            fnsoln(j) = soln(1)
            DO i = 1, 2
               IF( fnsoln(j) .LT. soln(i+1) ) fnsoln(j) = soln(i+1)
            ENDDO
         ELSE
            fnsoln(j) = soln(1)
         ENDIF
      ENDDO
      
      ssplmrse = AMAX1(fnsoln(1),fnsoln(2))
      
      RETURN                     
      
      END      