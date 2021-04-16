      SUBROUTINE FIL144( ihr )
C***********************************************************************
C     FIL144.FOR
C
C     Christian J Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Created:    1/16/96
C
C     Description: Translates the Character Weather Variables to
C       Numeric for CD-144 or SCRAM.  Adapted from FIL144 subroutine
c       in PCRAMMET
c
C***********************************************************************
      IMPLICIT    NONE

      INCLUDE     'hrly.inc'
      
      INTEGER     id, idsfc, ih, ihr, im, iy, j, jd     
      
      CHARACTER*5 ccc, ddd

c      INTEGER     PCOUNT
      
c      DATA PCOUNT /0/

C     1 = Station ID
C     2 = Year
C     3 = Month
C     4 = Day
C     5 = Hour
C     6 = Ceiling ht
C     7 = Present weather (missing in SCRAM)
C     8 = Wind direction
C     9 = Wind speed
C     10 = Station pressure (missing in SCRAM)
C     11 = Dry bulb temperature
C     12 = Opaque sky cover

C*    Obtain Station ID, Date and Time Info           

      READ( acd144(1,ihr),'(I5)' ) idsfc
      READ( acd144(2,ihr),'(I2)' ) iy
      READ( acd144(3,ihr),'(I2)' ) im
      READ( acd144(4,ihr),'(I2)' ) id
      READ( acd144(5,ihr),'(I2)' ) ih

C*    Change Hour Zero to Hour 24 of Previous Day
      
      IF( ih.EQ.0 ) THEN
         CALL JULIAN( iy, im, id, jd, 0 )
         CALL HR0024( iy, jd, ih ) 
         CALL JULIAN( iy, im, id, jd, 1 )
      ENDIF        
      
C*    Save Date/Time Info In Appropriate Arrays

      isyr(ihr) = iy
      ismo(ihr) = im
      isdy(ihr) = id
      ishr(ihr) = ih

C*--- Decode Ceiling Height - Standard RAMMET Procedures

      ccc = acd144(6,ihr)
      ddd = acd144(12,ihr)
      
      CALL CCCODE( idiag, ccc, ddd, icover(ihr), iceil(ihr) )

C*    Save Other Variables in Appropriate Arrays      

C*--- Present weather: only the 5 fields, in cols. 25-29 for CD-144,
C                      for liquid and frozen precipitation;
C                      for SCRAM data, these fields are blanks

      DO j = 1, 5
         READ( acd144(7,ihr)(j:j), '(I1)' ) iprec(j,ihr)
      ENDDO

C*    Missing value codes correspond to the codes used in SAMSON data

C*--- Wind direction

      IF( acd144(8,ihr).NE.'  ' ) THEN
         READ( acd144(8,ihr),'(I2)' ) idir(ihr)
      ELSE
         WRITE( idiag, 500 ) iy, im, id, ih
         idir(ihr) = 999
         recok(ihr) = .FALSE.
c         NOWARN = .FALSE.
      ENDIF

C*--- Wind speed
      IF( acd144(9,ihr).NE.'  ' ) THEN
         READ( acd144(9,ihr),'(f2.0)') wspeed(ihr)
      ELSE
         WRITE( idiag, 510 ) iy, im, id, ih
         wspeed(ihr) = 99.0
         recok(ihr) = .FALSE.
c         NOWARN = .FALSE.
      ENDIF

C*--- Station pressure: needed only for dry or wet deposition
c      IF( DRYFLG  .OR.  WETFLG )THEN
c         IF( ACD144(10,IHR) .NE. '    ' )THEN
c            READ (ACD144(10,IHR),'(f4.2)')PRESS(IHR)
c         ELSE
C*          In SUBR.UNITS, the station pressure will be set to 1000mb
C           when the pressure is missing; the message is written only
C           for the first 500 occurrences of missing pressure
c            PCOUNT = PCOUNT + 1
c            IF( PCOUNT .LE. 500 )THEN
c               WRITE( IDIAG, 520 ) IY, IM, ID, IH
c            ENDIF
c            PRESS(IHR) = 9999.0
c            NOWARN = .FALSE.
c         ENDIF
c      ENDIF

C*--- Temperature
      IF( acd144(11,ihr) .NE. '   ' ) THEN
         READ( acd144(11,ihr),'(F3.0)' )  temp(ihr)
      ELSE
         WRITE( idiag, 530 ) iy, im, id, ih
         temp(ihr) = 9999.
         recok(ihr) = .FALSE.
c         NOWARN = .FALSE.
      ENDIF

      RETURN

  500 FORMAT( ' FIL144: Wind direction missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )
  510 FORMAT( ' FIL144: Wind speed missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )
c  520 FORMAT( ' FIL144: Station pressure missing for (yy/mm/dd/hh) ',
c     &         3(i2.2:,'/'),i2.2, ', using 1000 mb')
  530 FORMAT( ' FIL144: Temperature missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )

      END
