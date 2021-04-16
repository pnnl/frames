      SUBROUTINE FILES
C***********************************************************************
c     FILES.FOR
c
c     Christian J Fosmire
c     Pacific Northwest National Lab
c     P.O. Box 999
c     Richland, WA 99352
c
c     Created:  1/15/96
c
c     Description: Checks to See If All the Required Files Are Present 
c        and Opens Them; adapted from the PCRAMMET FILES subroutine
C
C------------  Revision history-----------------------------------------
C
C      5 Oct 2004  BAN  revised OPEN statement for output
c
C***********************************************************************
      IMPLICIT    NONE

      INCLUDE     'hrly.inc'

      INTEGER     ier, nch
      
      LOGICAL     exists
      
      CHARACTER   atest*1, btest*3, sctest*80

C*    First the Surface File

      exists = .FALSE.
      INQUIRE( FILE=sfcfile, EXIST=exists ) 

      IF( exists ) THEN
         OPEN( UNIT = iosfc, FILE=sfcfile, IOSTAT=ier )
         IF ( ier.NE.0 ) THEN
            WRITE(*,*) 'Error opening hourly sfc file'
            WRITE(*,*) 'Error # - ', ier
            WRITE(*,*) 'File is ', sfcfile
            STOP 1
         ENDIF      
      ELSE
         WRITE( *,* ) 'Hourly surface file does not exist '
         WRITE( *,* ) 'File is ', sfcfile
         STOP 1
      ENDIF
     

C     The Precip File - Open Only if Wet Deposition is Desired
C     And Either the Surface Data is Not from Samson or When
C     a TD3240 File Supplements the SAMSON Precip.

      IF( td32flg ) THEN
         exists = .FALSE.
         INQUIRE( FILE= td32file, EXIST= exists )
         
         IF( exists ) THEN
            OPEN( iotd, FILE=td32file, IOSTAT=ier )
            
            IF( ier.ne.0 ) THEN
               WRITE(*,*) 'Error opening TD3240 file - ',td32file
               WRITE(*,*) 'Error # - ',ier
               STOP 1
            ENDIF
               
C     IF THE TD3240 File Is A Variable Format File, 
c     Then First Rewrite All Precip Data To a Scratch Formatted 
C     File (VAR2FIX logic)

            IF( td32form.EQ. 'V' ) THEN
              
C*    Check To See If The File Is REALLY A Variable FMT
      
               READ( iotd, '(T28,A3)', IOSTAT = ier ) BTEST
               
               IF( ier.ne.0 ) THEN
                  WRITE(*,*) 'Error reading first line of TD3240 file'
                  WRITE(*,*) 'Error # - ', ier
                  WRITE(*,*) 'File is ', td32file
                  STOP 1
               ENDIF   
                  
               IF (BTEST .EQ. '   ') THEN
                  WRITE(*,*) 'Error TD3240 file is not variable format'
                  WRITE(*,*) 'Error # - ', ier
                  WRITE(*,*) 'File is ', td32file
                  STOP 1
               ENDIF
               
               REWIND( iotd )

               OPEN( iotd+50, STATUS='SCRATCH', IOSTAT = ier )
               
               IF ( ier.ne.0 ) THEN
                  WRITE(*,*) 'Error in opening scratch file'
                  WRITE(*,*) 'Error # - ',ier
                  STOP 1
               ENDIF
               
               CALL VAR2FIX( iotd )
               
               iotd = iotd + 50
               REWIND( iotd )

            ELSE

C*    Check To See If The File Is REALLY A Fixed FMT

               READ ( iotd, '(T28,A3)', IOSTAT = ier ) btest
               
               IF ( ier.NE.0 ) THEN
                  WRITE(*,*) 'Error Reading First Line of TD3240 file'
                  WRITE(*,*) 'Error # - ', ier
                  WRITE(*,*) 'File is ', td32file
                  STOP 1
               ENDIF
               
               IF( btest.NE. '   ') THEN
                  WRITE(*,*) 'TD3240 file is not fixed format'
                  WRITE(*,*) 'File is ', td32file
                  STOP 1
               ENDIF
               
               REWIND( iotd )
                  
            ENDIF
               
         ELSE
            
            WRITE(*,*) 'TD3240 file does not exist'
            WRITE(*,*) 'File is ', td32file
            STOP 1
            
         ENDIF
      ENDIF
      
      
C*    The Output File
      
      OPEN( iout, FILE=outfile, IOSTAT=ier, FORM = 'FORMATTED')
c     &      ACCESS = 'DIRECT', RECL = 66 )
c     &      ACCESS = 'DIRECT', RECL = 58 )
      IF ( ier.NE.0 ) THEN
         WRITE(*,*) 'Error opening output file - ', outfile
         WRITE(*,*) 'Error # - ', ier
         STOP 1
      ENDIF

C*    Check to See If The File Are Of The Type That The User Says
C*    They Are; a difficulty may arise if the first record in the
C     CD-144 file has 28 characters or less - the program will
C     determine the file is not of type CD144 when, in fact, it is.

      IF( sfctyp.EQ.'CD144' ) THEN

         READ( iosfc, '(A80)', IOSTAT=ier ) sctest
         
         IF( ier.ne.0 ) THEN
            WRITE(*,*) 'Error reading first line of surface file'
            WRITE(*,*) 'Error # - ', ier
            WRITE(*,*) 'File is ', sctest
            STOP 1
         ENDIF
         
         nch = 80
         DO WHILE( nch.GT. 0  .AND.  sctest(nch:nch) .EQ. ' ')
            nch = nch - 1
         ENDDO

         IF( nch.LE.28 ) THEN

C---------- Likely not a CD144 file; records should be > 28 characters

            
            WRITE(*,*)'Surface File is not a CD144 file'
            WRITE(*,*)'File is ', sfcfile
            STOP 1
            
         ENDIF


      ELSEIF( sfctyp.EQ.'SCRAM' ) THEN
         
         READ( iosfc, '(A80)', IOSTAT = ier ) sctest
         
         IF ( ier.NE.0 ) THEN
            WRITE(*,*) 'Error reading first line of surface file'
            WRITE(*,*) 'Error # - ', ier
            WRITE(*,*) 'File is ', sfcfile
            STOP 1
         ENDIF   
         
         nch = 80
         DO WHILE( nch.GT.0  .AND.  sctest(nch:nch).EQ.' ' )
            nch = nch - 1
         ENDDO

         IF( nch.GT.28 ) THEN

C---------- Not a SCRAM file, which should have <= 28-character records

            
            WRITE(*,*) 'Surface file is not SCRAM'
            WRITE(*,*) 'File is ', sfcfile
            STOP 1
            
         ENDIF

      ELSEIF( sfctyp.EQ.'SAMSON' ) THEN
         
         READ( iosfc, '(A1)', IOSTAT = ier ) atest
         
         IF ( ier.ne.0 ) THEN
            WRITE(*,*) 'Error reading first line of surface file'
            WRITE(*,*) 'Error # - ', ier
            WRITE(*,*) 'File is ', sfcfile
            STOP 1
         ENDIF
c  commented out BAN 5 Oct 2004         
c         IF( atest.NE.'~' ) THEN

C---------- Not a SAMSON file

            
c            WRITE(*,*) 'Surface file is not a SAMSON file'
c            WRITE(*,*) 'File is ', sfcfile
c            STOP 1
            
c         ENDIF

      ENDIF

      REWIND( iosfc )

c      GO TO 999

c1023  CALL ERRHDL('E','1023',0)
c1024  CALL ERRHDL('E','1024',0)
c1025  CALL ERRHDL('E','1025',0)
c1026  CALL ERRHDL('E','1026',0)
c1027  CALL ERRHDL('E','1027',0)
c1028  CALL ERRHDL('E','1028',0)
c1029  CALL ERRHDL('E','1029',0)
c1030  CALL ERRHDL('E','1030',0)
c1031  CALL ERRHDL('E','1031',0)
c1032  CALL ERRHDL('E','1032',0)
c1033  CALL ERRHDL('E','1033',0)
c1034  CALL ERRHDL('E','1034',0)
c999  continue
      
      RETURN
      
      END