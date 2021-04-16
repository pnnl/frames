      SUBROUTINE FILSAM(IHR)
C***********************************************************************
C*    FILSAM      Module of PCRAMMET Meteorological Pre-processor 
C*
C*    PURPOSE:    Translates the Weather Variables from Character to
C*                Numeric for SAMSON data
C*
C*    PROGRAMMER: Jayant Hardikar
C*                PES Inc.  Modified by Christian Fosmire, PNNL
C*                for use in HRLYPROC
C*
C*    DATE:       July 10, 1998
C*     
C***********************************************************************
C  Revision History
C
C      6 Oct 2004   BAN  Split precipitation read for SAMSON/CEAM
C                   SAMSON is in 1/100 inch, CEAM is in cm 
C
C-----------------------------------------------------------------------
C*
C*    Variable Declarations
      IMPLICIT NONE
      
      INCLUDE 'HRLY.INC'
      
      INTEGER  MAXVAR
      PARAMETER ( MAXVAR = 26 )
      
      INTEGER  IALL, IV, JJ, IPCP, IHR
	real pcp2
      
      CHARACTER*6 WFMT(MAXVAR)
      CHARACTER*9 ATEMP

      DATA WFMT /'(I2)','(I2)','(I2)','(I2)','(I1)',
     &           '(I4)','(I4)','(A7)','(A7)','(A7)','(I2)','(I2)',
     &           '(F5.0)','(F5.0)','(I3)','(I4)','(I3)','(F5.0)',
     &           '(F6.0)',
     &           '(I6)','(A9)','(I4)','(F6.0)','(I4)','(I3)','(I6)'/

      
C*    Obtain Date and Time Info           
      READ (JSAMSN(1,IHR),WFMT(1)) ISYR(IHR)
      READ (JSAMSN(2,IHR),WFMT(2)) ISMO(IHR)
      READ (JSAMSN(3,IHR),WFMT(3)) ISDY(IHR)
      READ (JSAMSN(4,IHR),WFMT(4)) ISHR(IHR)


C*    Loop Over All the SAMSON Variables
      DO 600 IALL = 6,MAXVAR
         DO 300 IV = 1,NVARS
         
C*          If the Current Variable is Present, Then Process It
C*            As with CD-144 data, if the last hour of the last day
C*            of the year is missing, then substitute with data from
C*            hour 23

            IF (IALL-5 .NE. IDVAR(IV)) THEN
               IF (IV .EQ. NVARS) GO TO 600
               
            ELSE
C*------------ Opaque Sky Cover               
               IF (IDVAR(IV) .EQ. 7) THEN
                  READ (JSAMSN(IALL,IHR),WFMT(7+5)) ICOVER(IHR)
                  IF( ICOVER(IHR) .EQ. 99 ) THEN
                     IF( IHR .EQ. 24  .AND. ISMO(IHR).EQ.12  .AND.
     &                   ISDY(IHR).EQ.31 )THEN
                         ICOVER(IHR)  =  ICOVER(IHR-1)
                     ELSE
                         WRITE( IDIAG,540 ) ISYR(IHR), ISMO(IHR),
     &                                      ISDY(IHR), ISHR(IHR)
c                         NOWARN = .FALSE.
                     ENDIF
                  ENDIF
               ENDIF

C*------------ Ceiling Height               
               IF (IDVAR(IV) .EQ. 15) THEN
                  READ (JSAMSN(IALL,IHR),WFMT(15+5)) ICEIL(IHR)
                  IF( ICEIL(IHR) .GT. 999990 )THEN
                     IF( IHR .EQ. 24  .AND. ISMO(IHR).EQ.12  .AND.
     &                   ISDY(IHR).EQ.31 )THEN
                         ICEIL(IHR)  =  ICEIL(IHR-1)
                     ELSE
                        ICEIL(IHR) = 77777
                        WRITE( IDIAG,550 ) ISYR(IHR), ISMO(IHR),
     &                                     ISDY(IHR), ISHR(IHR)
c                        NOWARN = .FALSE.
                     ENDIF
                  ENDIF
               ENDIF

C*------------ Pressure
c               IF (IDVAR(IV) .EQ. 11) THEN
c                  READ (JSAMSN(IALL,IHR),WFMT(11+5)) IPRESS
c                  PRESS(IHR) = IPRESS
c                  IF( PRESS(IHR) .GT. 9990.0 )THEN
c                     IF( IHR .EQ. 24  .AND. ISMO(IHR).EQ.12  .AND.
c     &                   ISDY(IHR).EQ.31 )THEN
c                         PRESS(IHR) =  PRESS(IHR-1)
c                     ELSE
c                        WRITE( IDIAG,520 ) ISYR(IHR), ISMO(IHR),
c     &                                     ISDY(IHR), ISHR(IHR)
c                        NOWARN = .FALSE.
c                     ENDIF
c                  ENDIF
c               ENDIF

C*------------ Wind Direction               
               IF (IDVAR(IV) .EQ. 12) THEN
                  READ (JSAMSN(IALL,IHR),WFMT(12+5)) IDIR(IHR)
                  IF( IDIR(IHR) .EQ. 999 ) THEN
                     IF( IHR .EQ. 24  .AND. ISMO(IHR).EQ.12  .AND.
     &                   ISDY(IHR).EQ.31 )THEN
                         IDIR(IHR) =  IDIR(IHR-1)
                     ELSE
                        WRITE( IDIAG,500 ) ISYR(IHR), ISMO(IHR),
     &                                     ISDY(IHR), ISHR(IHR)
c                        NOWARN = .FALSE.
                     ENDIF
                  ENDIF
               ENDIF
                       
C*------------ Wind Speed (wind speed has two possible missing codes:
C*                         99 and 9999 - use 99 as the missing code)
               IF (IDVAR(IV) .EQ. 13) THEN
                  READ (JSAMSN(IALL,IHR),WFMT(13+5)) WSPEED(IHR)
                  IF( WSPEED(IHR) .GE. 99.0 ) THEN
                     IF( IHR .EQ. 24  .AND. ISMO(IHR).EQ.12  .AND.
     &                   ISDY(IHR).EQ.31 )THEN
                         WSPEED(IHR) =  WSPEED(IHR-1)
                     ELSE
                        WSPEED(IHR) = 99.0
                        WRITE( IDIAG,510 ) ISYR(IHR), ISMO(IHR),
     &                                     ISDY(IHR), ISHR(IHR)
c                        NOWARN = .FALSE.
                     ENDIF
                  ENDIF
               ENDIF

C*------------ Temperature
               IF (IDVAR(IV) .EQ. 8) THEN
                  READ (JSAMSN(IALL,IHR),WFMT(8+5)) TEMP(IHR)
                  IF( TEMP(IHR) .GT. 9990. )THEN
                     IF( IHR .EQ. 24  .AND. ISMO(IHR).EQ.12  .AND.
     &                   ISDY(IHR).EQ.31 )THEN
                         TEMP(IHR) =  TEMP(IHR-1)
                     ELSE
                        WRITE( IDIAG,530 ) ISYR(IHR), ISMO(IHR),
     &                                     ISDY(IHR), ISHR(IHR)
c                        NOWARN = .FALSE.
                     ENDIF
                  ENDIF
               ENDIF

C*------------ Present Weather Code
               IF (IDVAR(IV) .EQ. 16) THEN
                  READ (JSAMSN(IALL,IHR),WFMT(16+5)) ATEMP
                  DO 200 JJ = 1, 9
                     READ (ATEMP(JJ:JJ),'(I1)') IWXCOD(JJ,IHR)
200               CONTINUE                  
               ENDIF

C*             Hourly Precip
C*             Convert precip to millimeters here rather than in S.UNITS
               IF (IDVAR(IV) .EQ. 21) THEN
C
                 if(tilde .eq. '~') then
C      SAMSON format, in 1/100 inch
                  READ (JSAMSN(IALL,IHR),WFMT(21+5)) IPCP
                  PRECIP(IHR) = IPCP
                  IF( PRECIP(IHR) .GT. 99990.0 ) THEN
                     IF( IHR .EQ. 24  .AND. ISMO(IHR).EQ.12  .AND.
     &                   ISDY(IHR).EQ.31 )THEN
                         PRECIP(IHR) =  PRECIP(IHR-1)
                     ELSE
                        WRITE( IDIAG,560 ) ISYR(IHR), ISMO(IHR),
     &                                     ISDY(IHR), ISHR(IHR)
c                        NOWARN = .FALSE.
                     ENDIF
                  ENDIF
                  IF( PRECIP(IHR) .LT. 99990.0 ) THEN
                     PRECIP(IHR) = ( PRECIP(IHR) /100.0 ) * 25.4
                  ENDIF
	           else
C      CEAM format, in cm
                  READ (JSAMSN(IALL,IHR),'(f6.2)') PCP2
                  PRECIP(IHR) = PCP2
                  IF( PRECIP(IHR) .GT. 99990.0 ) THEN
                     IF( IHR .EQ. 24  .AND. ISMO(IHR).EQ.12  .AND.
     &                   ISDY(IHR).EQ.31 )THEN
                         PRECIP(IHR) =  PRECIP(IHR-1)
                     ELSE
                        WRITE( IDIAG,560 ) ISYR(IHR), ISMO(IHR),
     &                                     ISDY(IHR), ISHR(IHR)
c                        NOWARN = .FALSE.
                     ENDIF
                  ENDIF
                  IF( PRECIP(IHR) .LT. 99990.0 ) THEN
                     PRECIP(IHR) = PRECIP(IHR) * 10.
                  ENDIF
	           endif
               ENDIF
               
            ENDIF

C*       End Loops Over Variables               
300      CONTINUE
600   CONTINUE               
      
      RETURN

  500 FORMAT( ' FILSAM: Wind direction missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )
  510 FORMAT( ' FILSAM: Wind speed missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )
  520 FORMAT( ' FILSAM: Station pressure missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )
  530 FORMAT( ' FILSAM: Temperature missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )
  540 FORMAT( ' FILSAM: Cloud cover missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )
  550 FORMAT( ' FILSAM: Ceiling height missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )
  560 FORMAT( ' FILSAM: Precipitation missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )

      END
