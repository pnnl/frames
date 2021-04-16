C*********************************************************************** 
C 
C     TNPLMRSE
C
C     C. J. Fosmire                              
C     Pacific Northwest Laboratory
C     P.O. Box 999
C     Richland, Washington  99352                                               
C        
C     Created:   5/23/95 
C                                                            
C     Description:  
C   
C        TNPLMRSE calculates the transitional plume rise given 
c        atmospheric and effluent release conditions.  
c                    
C     Input:
c        Stack Height (m)                       ==> STK_HT
C        Stack Radius (m)                       ==> STK_RAD 
C        Stack Flow (m^3/s)                     ==> STK_FLOW
C        Effluent Temp (C)                      ==> ETEMP
C        Air Temp (C)                           ==> ATEMP
C        Release Ht. Wind Speed (m/s)           ==> WNDSPD
C        Stability Class                        ==> STAB
C        Mixing Layer Thickness (m)             ==> LDEPTH
c        Distance to Receptor (m)               ==> DIST
c        Minimum Wind Speed for Downwash (m/s)  ==> WNDMIN 
C        
C
C     Required Modules:    NONE
C
C*********************************************************************** 
      REAL FUNCTION TNPLMRSE( stk_ht, stk_rad, stk_flow, etemp, atemp, 
     &                        wndspd, stab, ldepth, dist, wndmin )

      IMPLICIT   NONE

      REAL       s0(3)
      REAL       atemp, atempk, bb, bm, delt, dist, downwash, dtc,     
     &           etemp, etempk, fb, fm, ldepth, pi, plmrstn,
     &           s, stk_flow, stk_ht, stk_rad, wndmin, wndspd, 
     &           wp, wspd   

      INTEGER    ist, stab   

      LOGICAL    momentum
      
      DATA s0 / 0.02, 0.035, 0.05 /

      pi = 3.14159
      momentum = .FALSE.
      wspd = wndspd
      
      IF( stk_ht .GT. Ldepth ) THEN
         ist = 7
      ELSE 
         ist = stab
      ENDIF
      
C **  Prepare to compute momentum and buoyancy fluxes
 
      wp =  stk_flow / ( pi * stk_rad**2 ) 
      etempk = etemp + 273.16
      atempk = atemp + 273.16   
      delt  = etempk - atempk
      
c**   Compute Buoyancy Flux

      fb = 9.8 * delt * wp * stk_rad*stk_rad / etempk      
      
C **  Compute the downwash correction

      IF( wspd.LT.wndmin ) wspd = wndmin
      downwash = 0.0
      IF( wp .LT. (1.5*wspd)) downwash = 4 * stk_rad * (wp / wspd - 1.5)

C **  Determine whether a bouyant or momentum plume  

      IF( ist .LE. 4 ) THEN         !  Neutral or Unstable

C **  Determine if buoyancy is dominant

         IF( delt .LE. 0.0 ) THEN
            momentum = .TRUE.          
         ELSE
            IF( fb .LT. 55 ) THEN
               dtc = 0.0187 * wp**(1./3.) * etempk / (stk_rad)**(2./3.)
               IF( delt .LE. dtc ) momentum = .TRUE.
            ELSE
               dtc = 0.00456 * (wp*wp / stk_rad)**(1./3.) * etempk
               IF( delt .LE. dtc) momentum = .TRUE.
            ENDIF
         ENDIF
      
      ELSE ! Stable
      
         s = 9.8 * s0(ist-4) / atempk 
  
C **  Determine if buoyancy is dominant

         IF( delt .LE. 0.0 ) THEN
            momentum = .TRUE.          
         ELSE
            dtc = 0.019582 * wp * atempk * SQRT( s )
            IF( delt .LE. dtc ) momentum = .TRUE.
         ENDIF
      
      ENDIF
      
c     Calculate Transitional Plume Rise

      fm = (atempk / etempk) * wp*wp * stk_rad*stk_rad
      bm = 0.4 + 1.2 * wspd / wp
      bb = 0.6
      IF( momentum ) THEN
         plmrstn = (3.0 * fm * dist / (bm*bm * wspd*wspd))**(1./3.)
      ELSE
         plmrstn = (3. * fm * dist / (bm*bm * wspd*wspd) +
     +           3. * fb * dist*dist / (2. * bb*bb * wspd**3.))**(1./3.)
      
      ENDIF

      TNPLMRSE = plmrstn + downwash      
              
      RETURN                                                                  

      END