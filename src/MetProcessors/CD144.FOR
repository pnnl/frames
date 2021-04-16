      SUBROUTINE CD144( iunit, adata, eofsfc, lwd, nread )
C***********************************************************************
C     CD144.FOR 
C
C     Christian J Fosmire
c     Pacific Northwest National Lab
c     P.O. Box 999
c     Richland, WA 99352
c
c     Created:  1/15/96
c
c     Description:    Reads met data from a CD144 met file
C       in 24 hour blocks.  Returns a character array of
C       12 variables for 24 hours.  Adapted from CD144 subroutine in 
c       PCRAMMET.
C
C***********************************************************************
      IMPLICIT    NONE

C*    (ISF is Used to Check If This Routine is Being Called 
C*    for the Very First Time or Not)

      SAVE        isf

      INTEGER     i, ier, ihr, isf, iunit, ivbl, lwd, nread

      CHARACTER*5 adata(12,24)

      LOGICAL     eofsfc

      DATA isf    /0/

C*    Skip the Very First Hour-00 (Belongs to Previous Day)
C*    However, keep the station ID and wind direction for calms processing

      isf = isf + 1

      IF( isf.EQ.1 ) THEN
         READ( iunit, 410, IOSTAT = ier ) adata(1,1), lwd
         IF ( ier.NE.0 ) THEN
            WRITE(*,*) 'Error reading first line of surface file'
            WRITE(*,*) 'Error # - ', ier
            STOP 1
         ENDIF
      ENDIF

410   FORMAT( A5,T39,I2 )

C     1 = Station ID
C     2 = Year
C     3 = Month
C     4 = Day
C     5 = Hour
C     6 = Ceiling ht
C     7 = Present weather
C     8 = Wind direction
C     9 = Wind speed
C     10 = Station pressure
C     11 = Dry bulb temperature
C     12 = Opaque sky cover

      nread = 0

C*    Begin Processing With Hour 01                                      
      
      DO ihr = 1,24
         READ( iunit, 420, END=999 ) (adata(i,ihr), i=1,12)
         nread = nread + 1
      ENDDO

420   FORMAT (A5,4A2,A3,8X,A5,9X,2A2,A4,A3,T79,A1)

      GO TO 1000

c1038  CALL ERRHDL('E','1038',1)

999   eofsfc = .TRUE.
      
      IF( ihr.EQ.24 ) THEN

C     *****Do we need this ?????******
C*    Duplicate last hour's data for hr=24

         DO ivbl = 1,12
            adata(ivbl,24) = adata(ivbl,23)
         ENDDO
         
         adata(5,24) = '24'
         
         nread = nread + 1
      
      ENDIF

1000  RETURN
      
      END