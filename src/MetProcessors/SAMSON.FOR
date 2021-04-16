      SUBROUTINE SAMSON ( NVARS, IDVAR, JVALUE, ITZONE,
     &                    XLAT, XLON, IUNIT, EOFSFC, NREAD, tilde )
C***********************************************************************
C*    SAMSON Module of PCRAMMET Meteorological Pre-processor 
C*
C*    PURPOSE:    Reads met data from a SAMSON met file
C*                in 24 hour blocks.  Returns a character array of up
C*                to 21 variables for 24 hours
C*
C*    PROGRAMMER: Jayant Hardikar, Jim Paumier
C*                PES Inc., Modifed by Christian Fosmire PNNL
C*                for use in HRLYPROC
C*                BA Napier; added call to CEAM
C*
C*    DATE:       July 10, 1998
C*                OCTOBER 5, 2004
C*     
C***********************************************************************
C*    Variable Declarations
      IMPLICIT NONE

      SAVE          ILINE, PFMT
      
      INTEGER        MAXVAR

      PARAMETER     (MAXVAR = 26)

      CHARACTER*40  FIELD(MAXVAR)
      CHARACTER*256 ALINE
      CHARACTER     CITY*22, STATE*2, NS*1, EW*1
      INTEGER       LATDEG,LATMIN,IWBAN,ITZONE
      INTEGER       LONDEG,LONMIN,ISELEV, IFC, ILINE, NVARS, IUNIT
      INTEGER       IPOS, IVAR, IMIT, IHR, NREAD, IV, II, IALL, ier
      INTEGER       IDVAR(MAXVAR-5)
      CHARACTER*9   JVALUE(MAXVAR,24)
      CHARACTER*9   JTEMP(MAXVAR)
      CHARACTER*2   VFMT(MAXVAR-5)
      CHARACTER*180 PFMT

	character*1 tilde

      REAL          XLAT,XLON, FNUM
      LOGICAL       GOTOPQ, GOTDBT, GOTWD, GOTWS, GOTCHT, GOTPWX, GOTPPT
      LOGICAL       EOFSFC

      DATA ILINE /0/
      DATA VFMT /'A4','A4','A7','A7','A7','A2','A2','A5','A5','A3',
     &           'A4','A3','A5','A6','A6','A9','A4','A6','A4','A3',
     &           'A6'/
     

C
C     CHECK IF SAMSON OR CEAM FORMAT (SAMSON HAS LEADING ~)
C       - SAMSON OUTPUT IS VARIABLE AND MUST BE ACCOMODATED
C       - CEAM OUTPUT IS FIXED FORMAT
C
      IF(ILINE .EQ. 0) THEN
      READ (IUNIT, '(A1)') TILDE
	REWIND (IUNIT)
	ENDIF
	IF (TILDE .EQ. '~') THEN ! USE ORIGINAL STUFF
	  GO TO 100
	ELSE
	  CALL CEAM(NVARS, IDVAR, JVALUE, ITZONE,
     &                    XLAT, XLON, IUNIT, EOFSFC, NREAD )
	ILINE=ILINE+1
	  GO TO 800
	ENDIF
  100 CONTINUE

C*    First time reading the file, read the file headers
      IF (ILINE .EQ. 0) THEN
C*       READ STATION HEADER RECORD
         READ (IUNIT,6000,IOSTAT = ier) 
     &              IWBAN, CITY, STATE, ITZONE, NS, LATDEG,
     &              LATMIN, EW, LONDEG, LONMIN, ISELEV
      
         IF( ier .LT. 0 ) THEN
            WRITE(*,*) 
     &'Reached end of file before station header record in Samson File'
            STOP 1
         ELSEIF ( ier .GT. 0 ) THEN
            WRITE(*,*) 
     &            'Error reading station header record in Samson File'
            WRITE(*,*) 'Error Number - ', ier
            STOP 1
         ENDIF
         
C*       CONVERT DEG/MIN TO DECIMAL DEGREES
C*       North and West = positive, South and East = negative
         XLAT = LATDEG + LATMIN/60.0
         IF(NS.EQ. 'S') XLAT=XLAT*(-1.0)
         XLON=LONDEG+LONMIN/60.0
         IF(EW.EQ. 'E') XLON=XLON*(-1.0)

C*       Convert the time zone so POSITIVE represents west longitudes
         ITZONE = -ITZONE

C*       READ EXTRACTED-VARIABLES HEADER RECORD
         READ (IUNIT,'(A)') ALINE

C*       PARSE THE LINE TO DETERMINE THE INDIVIDUAL VARIABLES
         CALL PARSER (ALINE,FIELD,IFC)

C*       DECREMENT THE COUNTER FOR NUMBER OF VARIABLES TO EXCLUDE
C*       YR,MO,DY,HR,I FIELDS
         NVARS = IFC-5

C*       DETERMINE THE FORMAT AND THE VARIABLES TO READ
         WRITE (PFMT(1:19),'(A)') "(4(1X,A2),1X,A1,1X,"
         IPOS = 19

         DO 200 IVAR = 1,NVARS

C*          GET THE VARIABLE ID
            CALL STONUM(FIELD(IVAR+5),40,FNUM,IMIT)
            IF (IMIT .NE. 1) THEN
               WRITE(*,*) 'Error Reading Variable ID in Samson File'
               WRITE(*,*) 'Unabled to Process Data'
               STOP 1
            ELSE
               IDVAR(IVAR) = INT(FNUM)
            END IF

            IPOS = IPOS+1
            WRITE (PFMT(IPOS:IPOS+1),'(A2)') VFMT(IDVAR(IVAR))
            IPOS = IPOS +2
            IF (IVAR .NE. NVARS) THEN
               WRITE (PFMT(IPOS:IPOS+3),'(A4)') ",1X,"
               IPOS = IPOS +3
            ELSE
              WRITE (PFMT(IPOS:IPOS),'(A1)') ")"
            ENDIF

200      CONTINUE

C*       SET FLAG FOR FIRST-TIME READ
         ILINE = ILINE + 1

C*       Check the ID numbers of the variables to be sure there
C*       is sufficient data to perform the calculations
         DO 250 IV = 1,NVARS
            IF( IDVAR(IV) .EQ. 7 )THEN
               GOTOPQ = .TRUE.
            ELSEIF( IDVAR(IV) .EQ. 8 )THEN
               GOTDBT = .TRUE.
            ELSEIF( IDVAR(IV) .EQ. 12 )THEN
               GOTWD = .TRUE.
            ELSEIF( IDVAR(IV) .EQ. 13 )THEN
               GOTWS = .TRUE.
            ELSEIF( IDVAR(IV) .EQ. 15 )THEN
               GOTCHT = .TRUE.
            ELSEIF( IDVAR(IV) .EQ. 16 )THEN
               GOTPWX = .TRUE.
            ELSEIF( IDVAR(IV) .EQ. 21 )THEN
               GOTPPT = .TRUE.
            ENDIF
            
  250    CONTINUE

         IF( .NOT. GOTOPQ  .OR.  .NOT. GOTDBT  .OR.  .NOT. GOTCHT
     &       .OR.  .NOT. GOTWD   .OR.  .NOT. GOTWS 
     &                           .OR. .NOT. GOTPWX ) THEN
            WRITE(*,*) 'Not Enough Variables in Samson File'
            IF( .NOT. GOTOPQ ) THEN
               WRITE(*,*) 'Missing Opaque Sky Cover'
            ENDIF
            IF( .NOT. GOTDBT ) THEN
               WRITE(*,*) 'Missing Dry Bulb Temperature'
            ENDIF
            IF( .NOT. GOTCHT ) THEN
               WRITE(*,*) 'Missing Ceiling Height'
            ENDIF
            IF( .NOT. GOTWD ) THEN
               WRITE(*,*) 'Missing Wind Direction'
            ENDIF
            IF( .NOT. GOTWS ) THEN
               WRITE(*,*) 'Missing Wind Speed'
            ENDIF
            IF( .NOT. GOTPWX ) THEN
               WRITE(*,*) 'Missing Weather'
            ENDIF
            STOP 1
         ENDIF

C     Endif first time reading the file
      ENDIF
      
6000  FORMAT (T2,I5,T8,A22,T31,A2,T34,I3,T39,A1,T40,I2,T43,I2,
     &        T47,A1,T48,I3,T52,I2,T56,I4)
 
C*    For all calls to this routine, read data
C*    NREAD keeps track of the number of data records read

      NREAD = 0

C*    LOOP HOURS
      DO 700 IHR = 1,24

C*       READ DATA
         READ (IUNIT,PFMT,IOSTAT = ier) (JTEMP(IV),IV=1,NVARS+5)
         
         IF( ier .LT. 0 ) THEN
            EOFSFC = .TRUE.
            RETURN
         ELSEIF( ier .GT. 0 ) THEN
            WRITE(*,*) 'Error Reading Values from SAMSON file'
            WRITE(*,*) 'Error Number - ', ier
            STOP 1
         ENDIF
         NREAD = NREAD + 1
     
C*       ASSIGN THE DATA TO THE APPROPRIATE VARIABLE INDICES
         DO 300 II = 1,5
            JVALUE(II,IHR) = JTEMP(II)
300      CONTINUE

         DO 600 IALL = 6,MAXVAR
            DO 500 IV = 1,NVARS
               IF (IALL-5 .EQ. IDVAR(IV)) THEN
                  JVALUE(IALL,IHR) = JTEMP(IV+5)
                  GO TO 600
               ELSE
                  IF (IV .EQ. NVARS) THEN
                     JVALUE(IALL,IHR) = '   '
                  ENDIF
               ENDIF
500         CONTINUE
600      CONTINUE

700   CONTINUE
800   CONTINUE

C*    Error Handling

1000  RETURN
      END
