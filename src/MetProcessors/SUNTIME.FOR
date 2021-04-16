      SUBROUTINE SUNTIME( julDay, latitude, longitude, tRise, tSet )
C--------------------------------------------------------------------------
C     SUNTIME
C
C     Date:    10/19/95
C
C     Description:  Computes sun rise and sun set times.
C        Following the discussion in Stull, 1988
C        An Introduction to Boundary Layer Meteorology
C        Page 257f.
C        Time are only accurate to about +/-16 minutes.
C        TRise or TSet = -999 is returned if the sun does not 
C        set or rise on that day.
C
C     Required Modules:  None   
C     
C--------------------------------------------------------------------------
      IMPLICIT    NONE
      
      REAL*4      cos_T, latitude, longitude, rpd, rr, solardecl, 
     &            solrang, ss, tilt, tRise, tSet, twopi, t0
      
      INTEGER*4   jdSolstice, julDay
      
C  Set constants
      
      twopi = 8.0 * ATAN(1.0)
      rpd = twopi / 360.0     !radians per degree
      tilt = rpd * 23.45      !tilt of earth's axis
      jdSolstice = 173        !Julian Day of the summer solstice
      
c  Find solar declination angle (radians)
      
      solardecl = tilt * 
     &   COS( twopi / 365.25 * ( FLOAT(julDay) - FLOAT(jdSolstice) ) )

c  rr is sin(solar angle at sun rise and set).
C  Due to refraction, the sun appears to rise and set when 
C  it is 0.833 degrees below the horizon.
         
      rr = -0.0104528
      
      cos_T = ( SIN(rpd*latitude) * SIN( solardecl ) - rr ) 
     &   / COS(rpd*latitude) / COS(solardecl)
     
c  ss is used to get the correct quadrant for acos
      
      ss = SIGN(1.0,cos_T)
      
      cos_T = ABS( cos_T)
      
      IF( cos_T .gt. 1.0 ) THEN
      
c  The sun does not rise or set
c  Check if always night or always day
         
         solrang = ASIN ( SIN(rpd*latitude) * SIN(solardecl) + 
     &COS(rpd*latitude) * COS ( solardecl) )/rpd
         IF ( solrang.lt.1.0 ) THEN
            tSet = -1
            tRise = 24
         ELSE
            tRise = -1
            tSet = 24
         ENDIF
c         tRise = -999.0
c         tSet  = -999.0
         
      ELSE
      
c  T0 is the sunrise time on the prime meridian
c  at this latitude.
         
         IF( ss .lt. 0.0 ) THEN
         
            t0 = 12.0 - 24.0 / twopi * ACOS(cos_T)
            
         ELSE
         
            t0 = 24.0 / twopi * ACOS(cos_T)
            
         ENDIF
         
c  Find times at specified Longitude
         
         tRise =        t0 + longitude / 15.0
         tSet  = 24.0 - t0 + longitude / 15.0
         solardecl = solardecl/RPD
      ENDIF
      
      RETURN      
      END