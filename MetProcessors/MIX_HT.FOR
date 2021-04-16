C***********************************************************************
C
C     MIX_HT
C     
C     J.V. Ramsdell
C     Pacific Northwest Laboratory
C     P.O. Box 999
C     Richland, Washington 99352
C
C     Created:  10/30/91
C     Revised:  5/23/94
C
C     Description:
C         
C        MIX_HT estimates the mixing layer depth at meteorological 
C        stations.  The depth is estimated using expressions given 
C        by Zilitinkevich (Boundary Layer Meteorology, 3:141-145). 
C        These estimates are compared with climatological data from
C        the Hanford Met. Station.  The final estimate, is the computed
C        climatological value depending on stability, time of day, and 
C        and season.  The friction velocity and 1/L are assumed to be in 
C        SI units.  
C
C        5/23/94 Revision:  Changed to find the mixing layer depth using
C                           data provided by the NWS file.  Time of day
C                           dependance has been removed. --LK Berg
C
C
C     Input:    Friction velocity                       ==>    USTR 
C               Inverse Monin-Obukhov length            ==>    MOLINV   
C               Stability class                         ==>    STABLTY
C
C     Relationship to other program units
C
C        Makes Calls to:  NONE
C
C
C***********************************************************************

      FUNCTION MIX_HT( ustr, molinv, stablty, lat)

      IMPLICIT   NONE

      INTEGER    stablty, month, hour, ist, ih
       
      REAL       mix_ht, ustr, molinv, coriolis, k1, k2
      REAL       mix_f, default, lat

c      LOGICAL daytime, summer, hflg

C **  Approximate sunrise (PST) at Hanford by month
c        write(76,*) ustr, molinv, stablty, lat 

***      DATA  SR /  8,  7,  6,  5,  5,  4,  5,  5,  6,  6,  7,  8 /  
      
C **  Approximate sunset (PST) at Hanford by month

***      DATA  SS / 16, 17, 18, 19, 19, 20, 20, 19, 19, 17, 16, 16 / 

C **  Coriolis parameter = 2 * 7.29e-5 * SIN( Latitude = 46.6667 Degrees )

      coriolis = 2 * 7.29e-5 * sin (lat * .01745329)

C **  Combine the extremely stable and extremely unstable classed
C **  and reduce the number of stability classes to 5

      IF( (stablty .GT. 1) .AND. (stablty .LT. 7) ) THEN
         ist = stablty - 1
      ELSE IF( stablty .EQ. 7 ) THEN
         ist = 5
      ELSE IF( stablty .EQ. 1 ) THEN
         ist = 1
      ENDIF

C **  Set flag for summer

***      SUMMER = .TRUE.
***      IF( MONTH.LE.3.OR.MONTH.GE.10) THEN
***          SUMMER = .FALSE.
***      ENDIF
***
***      IF( HOUR .EQ. 0 ) THEN
***        IH = 6
***      ELSE
***        IH = ((HOUR-1)/3)+1
***      ENDIF
***
***      DAYTIME = .TRUE.
***      IF((HOUR .LE. SR(MONTH)) .OR. (HOUR .GE. SS(MONTH))) THEN
***        DAYTIME = .FALSE.
***      ENDIF

C **  Check stability conditions for time of day ... no unstable at 
C **  night and no stable during the day during summer.

***      IF( (IST .LE. 2) .AND. (.NOT. DAYTIME)) THEN
***        IST = 3
***      ELSE IF( (IST .GE. 4) .AND. DAYTIME .AND. SUMMER ) THEN
***        IST = 3
***      ENDIF

***      IF( hflg ) THEN
***        K1 = 0.2 + rv * 0.5   
***        K2 = 0.15 + rv * 0.15
***      ELSE
      k1 = 0.4
      k2 = 0.2
***      ENDIF

C **  Compute the mixing height from surface layer conditions

      IF( ist .LE. 3 ) THEN                      ! Unstable or Neutral
         mix_f = k2 * ustr / coriolis
      ELSE IF( ist .LE. 5 ) THEN                 ! Stable 
         mix_f = k1 * SQRT( ustr  / ( molinv * coriolis ))
      ENDIF    

C **  Get the default value for comparison with the computed values

***      DEFAULT = DFMD(IH,IST,MONTH)
      default = 1.0

C **  Choose between computed and default values

c        write(76,*) mix_f

      IF( default .LT. 10. ) THEN 
         mix_ht = mix_f       
      ELSE
         mix_ht = AMAX1( mix_f, default )
      ENDIF

C **  Make sure mixed layer height does not exceed 2000 m or fall under
C **  10 meters
c          write(76,*) mix_ht
      IF( MIX_HT.GT.2000.) THEN
         MIX_HT = 2000.
      ELSE IF(MIX_HT.LT.10.) THEN
         MIX_HT = 10.
      ENDIF

      RETURN
      END
