      SUBROUTINE UNITS( ihr )
C***********************************************************************
C     UNITS.FOR
C     
C     Christian Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Created:    1/16/96
C
C     Description:  This Routine Converts All Met Variables to 
C        Appropriate Units; it also converts any missing values to a
C        nonmissing value for selected variables.  Adapted from the
C        UNITS subroutine in PCRAMMET
C            
C***********************************************************************
      IMPLICIT    NONE

      INCLUDE     'hrly.inc'
            
      REAL        tfahr
      
      INTEGER     ihr
      
      IF( sfctyp.EQ.'CD144' .OR.  sfctyp.EQ.'SCRAM' ) THEN

C*       CONVERT TEMP FROM FAHRENHEIT TO KELVIN
         IF( temp(ihr) .LT. 9000.0 ) THEN
            temp(ihr) = 0.5556 * (temp(ihr) - 32.0) + 273.15
         ENDIF
                 
C*       CONVERT WIND SPEED FROM KNOTS TO METERS/SEC,
C*       but save the wind speed in knots (integer) for use in
C*       computing stability category
         
         kspeed(ihr) = wspeed(ihr) + 0.01
         IF( wspeed(ihr) .LT. 99. ) THEN
            wspeed(ihr) = wspeed(ihr)*0.51444

C*          IF Wind Speed < 1.0 m/s, Wind Speed Is Set To 1 m/s

            IF( wspeed(ihr) .LT. 1.0 ) wspeed(ihr)=1.0
         ENDIF

C*       Convert cloud cover from tenths to fraction
C*       Note that a missing cloud cover is assumed to be 10/10
C        as performed in SUBR.CCCODE
c         
c         fcover(ihr) = 0.1 * FLOAT(icover(ihr))
c
C*       Convert surface pressure from inches hg to mb
C*       (33.864 mb = 1 inch hg)
c         IF( PRESS(IHR) .LT. 9000.0 )THEN
c            PRESS(IHR) = PRESS(IHR) * 33.864
c         ELSE
c            PRESS(IHR) = 1000.0
c         ENDIF

      ELSEIF( sfctyp.EQ.'SAMSON' ) THEN

C*       Ceiling height must be converted back to hundreds of feet
C*       77777 = unlimited   999999 = missing

         IF( iceil(ihr).EQ.77777 .OR. iceil(ihr).GT.999990 ) THEN

C           Unlimited ceiling

            iceil(ihr) = 998
         ELSE
            iceil(ihr) = NINT( ( iceil(ihr) / 0.3048 ) / 100.0 )
         ENDIF

C*       Convert temp from celsius to Kelvin
C*       For the SAMSON results to be consistent with the CD144 results,
C*       temperature is converted from celsius to fahrenheit then back
C*       to celsius

         IF( temp(ihr).LT.9000.0 ) THEN
            tfahr = NINT(temp(ihr)/0.5556 + 32.0)
            temp(ihr) = 0.5556 * (tfahr - 32.0) + 273.15
         ENDIF
                 
C*       Wind speed is already in meters/sec, but we need knots for
C*       stability class;
C*       Convert the KSPEED back to m/s so the SAMSON results are
C*       consistent with the CD-144 results

         IF( wspeed(ihr).LT.99.0 ) THEN
            kspeed(ihr) = NINT(wspeed(ihr)/0.51444)
            wspeed(ihr) = kspeed(ihr) * 0.51444

C*          IF Wind Speed < 1.0 m/s, Wind Speed Is Set To 1 m/s

            IF( wspeed(ihr).LT.1.0 ) wspeed(ihr)=1.0
         ENDIF

C*       Convert cloud cover from tenths to fraction

c         IF( icover(ihr) .LT. 90 ) THEN
c            fcover(ihr) = 0.1 * FLOAT(icover(ihr))
c         ELSE
c            fcover(ihr) = 1.0
c         ENDIF

C*       Surface pressure is already in millibars, but check for missing
c         IF( PRESS(IHR) .GT. 9000.0 )THEN
c            PRESS(IHR) = 1000.0
c         ENDIF

C*       Precipitation - already converted to millimeters (in S.FILSAM),
C*       but check for missing
         IF( precip(ihr) .GT. 9999.0 ) THEN
            precip(ihr) = 0.0
         ENDIF

      ENDIF

C*    Check the surface pressure - if less than 500 mb or greater than
C*    9000.0, then the pressure could be missing, so set it to 1000 mb
C     (This check and substitution may already have been performed in
C      SUBR.UNITS)
c      IF( PRESS(IHR) .LT. 500.0  .OR.  PRESS(IHR) .GT. 9000.0 )THEN
c          PRESS(IHR) = 1000.0
c      ENDIF

C*    Compute air density in kg/m**3 -- rho = p/(r * t) --
C*    r=287 m**2/(deg k * sec**2)
C*    0.3484321=(100. kg/(m*sec**2) per mb)/(287 m**2/(deg k * sec**2))
c      IF(TEMP(IHR) .LT. 900.0 )THEN
c         RHO(IHR) = 0.3484321 * PRESS(IHR) / TEMP(IHR)
c      ELSE
c         RHO(IHR) = -1.0
c      ENDIF
      
      RETURN
      END

