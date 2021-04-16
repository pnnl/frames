c--------------------------------------------------------------------------
c     FNPLMRSE
c
c     Date:              October 9, 1995
c
c     Description:       Computes the final plume rise.
c
c     Required Modules:  None
c
c
c--------------------------------------------------------------------------
      REAL FUNCTION FNPLMRS( Stab, stk_ht, stk_rad, stk_flow, wndspd,
     &etemp, atemp, ldepth, wndmin)

      IMPLICIT    NONE
      
      REAL        s0(3)
      REAL        atemp, atempk, delt, downwash, dtc, etemp, etempk,
     &            fb, ldepth, pi, plmrsfn, p1, p2, s, stk_flow, stk_ht,
     &            stk_rad, uc, wndmin, wndspd, wp, wspd
      
      INTEGER     ist, stab
      
      LOGICAL     momentum
      
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

      fb = 9.8*delt*wp*stk_rad*stk_rad/etempk      
      
C **  Compute the downwash correction

      IF( wspd.LT.wndmin ) wspd = wndmin
      
      downwash = 0.0
      IF( wp .LT. (1.5*wspd) ) downwash =4*stk_rad*(wp/wspd - 1.5)

C **  Start final plume rise calculations  

      IF( ist .LE. 4 ) THEN         !  Neutral or Unstable

C **  Determine if buoyancy is dominant

         IF( delt .LE. 0.0 ) THEN
            momentum = .TRUE.          
         ELSE
            IF( fb.lt.55 ) THEN
               dtc = 0.0187 * wp**(1./3.) * etempk / (stk_rad)**(2./3.)
               IF( delt .LE. dtc ) momentum = .TRUE.
            ELSE
               dtc = 0.00456*(wp*wp/stk_rad)**(1./3.)*etempk
               IF( delt.LE.dtc ) momentum = .TRUE.
            ENDIF
         ENDIF

         IF( momentum ) THEN
            plmrsfn = 6 * stk_rad * ( wp / wspd )
         ELSE                  ! Buoyancy Rise
            IF( fb.lt.55 ) THEN
               plmrsfn = 1.6 * 49 ** (2./3.) * fb ** 0.75 / wspd
            ELSE
               plmrsfn = 1.6 * 119 ** (2./3.) * fb ** 0.60 / wspd
            ENDIF  
         ENDIF

      ELSE                             !  Stable

         s = 9.8 * s0(ist-4) / ATEMPK 
  
C **  Determine if buoyancy is dominant

         IF( delt .LE. 0.0 ) THEN
            momentum = .TRUE.          
         ELSE
            dtc = 0.019582 * wp * atempk * SQRT( s )
            IF( delt .LE. dtc ) momentum = .TRUE.
         ENDIF

         IF( MOMENTUM ) THEN
            p1 = 6 * stk_rad * ( wp / wspd )
            p2 = 1.5 / s**(1./6.)
     +         * ( (stk_flow/pi)*(wp/wspd)*(atempk/etempk) )**(1./3.)  
            plmrsfn = AMIN1( p1,p2 )
         ELSE                  ! Buoyancy Rise
          
            uc = 0.2746 * fb**0.25 * s**0.125
            IF( wspd .LT. uc ) THEN
               plmrsfn = 4.0 * fb**0.25 / s**0.375
            ELSE
               plmrsfn = 2.6 * ( fb / (wspd * s))**(1./3.)
            ENDIF
         ENDIF       ! MOMENTUM
     
      ENDIF         ! IST
      
      FNPLMRS = plmrsfn + downwash
      
      RETURN
      END