      SUBROUTINE CEAM ( NVARS, IDVAR, JVALUE, ITZONE,
     &                    XLAT, XLON, IUNIT, EOFSFC, NREAD )
C***********************************************************************
C     CEAM Module of GENII Meteorological Pre-processor 
C*
C*    PURPOSE:    Reads met data from a CEAM met file
C*                in 25 hour blocks.  Returns a character array of
C*                variables for 24 hours
C*
C*    PROGRAMMER: B.A. Napier
C*
C*    DATE:       October 5, 2004
C*     
C***********************************************************************
C*    Variable Declarations
      IMPLICIT NONE

      SAVE          ILINE
      
      INTEGER        MAXVAR

      PARAMETER     (MAXVAR = 26)

      CHARACTER     CITY*22, STATE*2, NS*1, EW*1
      INTEGER       LATDEG,LATMIN,IWBAN,ITZONE
      INTEGER       LONDEG,LONMIN,ISELEV, IFC, ILINE, NVARS, IUNIT
      INTEGER       IPOS, IVAR, IMIT, IHR, NREAD, IV, II, IALL, ier
      INTEGER       IDVAR(MAXVAR-5)
      CHARACTER*9   JVALUE(MAXVAR,24)
      CHARACTER*9   JTEMP(MAXVAR)
	CHARACTER*1   DUM

      REAL          XLAT,XLON, FNUM

      LOGICAL       EOFSFC

      DATA ILINE /0/
     

C*    First time reading the file, read the file headers
      IF (ILINE .EQ. 0) THEN
C*       READ STATION HEADER RECORD
         READ (IUNIT,6000,IOSTAT = ier) 
     &              IWBAN, CITY, STATE, ITZONE, NS, LATDEG,
     &              LATMIN, EW, LONDEG, LONMIN, ISELEV
      
6000  FORMAT (T2,I5,T8,A22,T39,A2,T42,I3,T47,A1,T50,I2,T53,I2,
     &        T57,A1,T59,I3,T63,I2,T67,I4)
           
         IF( ier .LT. 0 ) THEN
            WRITE(*,*) 
     &'Reached end of file before station header record in CEAM File'
            STOP 1
         ELSEIF ( ier .GT. 0 ) THEN
            WRITE(*,*) 
     &            'Error reading station header record in CEAM File'
            WRITE(*,*) 'Error Number - ', ier
            STOP 1
         ENDIF
         
C*       CONVERT DEG/MIN TO DECIMAL DEGREES
C*       North and West = positive, South and East = negative
         XLAT = LATDEG + LATMIN/60.0
         IF(NS.EQ. 'S') XLAT=XLAT*(-1.0)
         XLON=LONDEG+LONMIN/60.0
         IF(EW.EQ. 'E') XLON=XLON*(-1.0)

C*       LEAVE the time zone so POSITIVE represents west longitudes
C         ITZONE = -ITZONE

C*       SET FLAG FOR FIRST-TIME READ
         ILINE = ILINE + 1


C     Endif first time reading the file
      ENDIF

C      IDVAR(1)=0	!YEAR
C      IDVAR(2)=0	!MONTH
C      IDVAR(3)=0	!DAY
C      IDVAR(4)=0  !HOUR
C	 IDVAR(5)=0  !an unused "observation indicator"
C      IDVAR(6)=1  !OPAQUE SKY COVER
C      IDVAR(7)=2	!TEMPERATURE
C      IDVAR(8)=3 !WIND DIRECTION
C      IDVAR(9)=4 !WIND SPEED
C      IDVAR(10)=5 !CEILING HEIGHT    
C      IDVAR(11)=6 !PRESENT WEATHER CODE
C      IDVAR(12)=7 !HOURLY PRECIPITATION	
      IDVAR(1)=7	
      IDVAR(2)=8
      IDVAR(3)=12
      IDVAR(4)=13
	IDVAR(5)=15
      IDVAR(6)=16
      IDVAR(7)=21     
           
C*    For all calls to this routine, read data
C*    NREAD keeps track of the number of data records read

      NREAD = 0
	NVARS = 7

C*    LOOP HOURS
      DO 700 IHR = 1,25
	
	IF(IHR .LE. 24) THEN

C*       READ DATA
         READ (IUNIT,6001,IOSTAT = ier) (JTEMP(IV),IV=1,11)
 6001    FORMAT(T4,A2,T7,A2,T10,A2,T13,A2,T61,A2,T65,A5,T91,A3,
     &          T96,A5,T111,A6,T122,A9,T157,A6)   
         IF( ier .LT. 0 ) THEN
            EOFSFC = .TRUE.
            RETURN
         ELSEIF( ier .GT. 0 ) THEN
            WRITE(*,*) 'Error Reading Values from CEAM file'
            WRITE(*,*) 'Error Number - ', ier
            STOP 1
         ENDIF
         NREAD = NREAD + 1
     
C*       ASSIGN THE DATA TO THE APPROPRIATE VARIABLE INDICES
         DO 300 II = 1,4
            JVALUE(II,IHR) = JTEMP(II)
300      CONTINUE
            JVALUE(5,IHR) = '0'

         DO 600 IALL = 6,MAXVAR
            DO 500 IV = 1,NVARS
               IF (IALL-5 .EQ. IDVAR(IV)) THEN
                  JVALUE(IALL,IHR) = JTEMP(IV+4)
                  GO TO 600
               ELSE
                  IF (IV .EQ. NVARS) THEN
                     JVALUE(IALL,IHR) = '   '
                  ENDIF
               ENDIF
500         CONTINUE
600      CONTINUE

      ELSE
		READ(IUNIT,'(A1)') DUM
	ENDIF

700   CONTINUE

C*    Error Handling

1000  RETURN
      END
