      SUBROUTINE SCRAM (IUNIT,ADATA,EOFSFC,LWD,NREAD)
C***********************************************************************
C*    SCRAM Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE:    Reads met data from a SCRAM met file
C*                in 24 hour blocks.  Returns a character array of
C*                12 variables for 24 hours (with station pressure
C*                and present weather as missing)
C*
C*    PROGRAMMER: Jayant Hardikar, Jim Paumier
C*                PES Inc.
C*
C*    DATE:       June 30, 1995
C*     
C*    INPUTS:     SCRAM met data file
C*
C*    OUTPUTS:    Arrays of Weather Varaibles for 24 hours
C*        
C*    CALLED FROM: READSF
C***********************************************************************
C*
C*    Variable Declarations
C*    (ISF is Used to Check If This Routine is Being Called 
C*    for the Very First Time or Not)
      SAVE ISF
      INTEGER IUNIT
      CHARACTER*5 ADATA(12,24)
      CHARACTER  SPEED*3, TOTAL*2, OPAQ*2
      LOGICAL EOFSFC

      DATA ISF /0/

C*    Skip the Very First Hour-00 (Belongs to Previous Day)
C*    However, keep the station ID and wind direction for calms processing
      ISF = ISF + 1
      IF (ISF .EQ. 1) THEN
         READ (IUNIT,410,ERR=1038) ADATA(1,1),LWD
      ENDIF
  410 FORMAT( A5,T17,I2 )

C     1 = Station ID
C     2 = Year
C     3 = Month
C     4 = Day
C     5 = Hour
C     6 = Ceiling ht
C     7 = Present weather (not available)
C     8 = Wind direction
C     9 = Wind speed
C     10 = Station pressure (not available)
C     11 = Dry bulb temperature
C     12 = Opaque sky cover

      NREAD = 0

C*    Begin Processing With Hour 01                                      
      DO 10 IHR = 1,24
         READ (IUNIT,420,END=999,ERR=1038) ADATA(1,IHR),ADATA(2,IHR),
     &         ADATA(3,IHR),ADATA(4,IHR),ADATA(5,IHR),ADATA(6,IHR),
     &         ADATA(8,IHR),SPEED,ADATA(11,IHR),TOTAL,OPAQ
         NREAD = NREAD + 1
C------- Set present weather (vbl 7) and stn pressure (vbl 10) to missing
         ADATA(7,IHR)  = '     '
         ADATA(10,IHR) = '    '

C------- The SCRAM data are processed through the CD-144 routines;
C        therefore, the data first must be formatted after CD-144.
C        The program MET144 was used as a guide to developing the
C        conversion.

C------- Ceiling height, temperature and wind direction do not need
C        converting

C------- Wind Speed
         IF( SPEED .EQ. '   ' )THEN
            ADATA(9,IHR) = '     '

         ELSE
            READ( SPEED,'(I3)') IWS
            IF( IWS .LE. 99 )THEN
               WRITE( ADATA(9,IHR),'(A2)' ) SPEED(2:3)

            ELSE
               ADATA(9,IHR)(2:2) = SPEED(3:3)
               IF( IWS .LT. 200 )THEN
                  ADATA(9,IHR)(1:1) = 'R'

               ELSEIF( IWS .LT. 190 )THEN
                  ADATA(9,IHR)(1:1) = 'Q'

               ELSEIF( IWS .LT. 180 )THEN
                  ADATA(9,IHR)(1:1) = 'P'

               ELSEIF( IWS .LT. 170 )THEN
                  ADATA(9,IHR)(1:1) = 'O'

               ELSEIF( IWS .LT. 160 )THEN
                  ADATA(9,IHR)(1:1) = 'N'

               ELSEIF( IWS .LT. 150 )THEN
                  ADATA(9,IHR)(1:1) = 'M'

               ELSEIF( IWS .LT. 140 )THEN
                  ADATA(9,IHR)(1:1) = 'L'

               ELSEIF( IWS .LT. 130 )THEN
                  ADATA(9,IHR)(1:1) = 'K'

               ELSEIF( IWS .LT. 120 )THEN
                  ADATA(9,IHR)(1:1) = 'J'

               ELSEIF( IWS .LT. 110 )THEN
                  ADATA(9,IHR)(1:1) = '}'

               ENDIF
            ENDIF
         ENDIF

C----    Opaque cloud cover
         IF( OPAQ .NE. '  ') THEN
            IF( OPAQ .EQ. '10' )THEN
               ADATA(12,IHR) = '-'
            ELSE
               WRITE( ADATA(12,IHR),'(A1)' ) OPAQ(2:2)
            ENDIF

         ELSE
            IF( TOTAL .NE. '  ' )THEN
               IF( TOTAL .EQ. '10' )THEN
                  ADATA(12,IHR) = '-'
               ELSE
                  WRITE( ADATA(12,IHR),'(A1)' ) TOTAL(2:2)
               ENDIF

            ELSE
               IF( ADATA(6,IHR) .EQ. '   ' )THEN
                  ADATA(12,IHR) = ' '

               ELSEIF( ADATA(6,IHR) .EQ. '---' )THEN
                  ADATA(12,IHR) = '0'

               ELSEIF( ADATA(6,IHR) .LT. '070' )THEN
                  ADATA(12,IHR) = '7'

               ELSE
                  ADATA(12,IHR) = '0'

               ENDIF
            ENDIF
         ENDIF

   10 CONTINUE

  420 FORMAT (A5,4A2,A3,A2,A3,A3,a2,A2)

      GO TO 1000

1038  CALL ERRHDL('E','1038',1)

999   EOFSFC = .TRUE.
      IF( IHR .EQ. 24 )THEN
C*       Duplicate last hour's data for hr=24
         DO 20 IVBL = 1,12
            ADATA(IVBL,24) = ADATA(IVBL,23)
   20    CONTINUE
         ADATA(5,24) = '24'
         NREAD = NREAD + 1
      ENDIF

1000  RETURN
      END      
