      SUBROUTINE READREAL( inpstring, start_pos, realval, end_pos )
C-----------------------------------------------------------------------
C     READREAL
C
C     Christian Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Date:    4/10/97
C
C     Description:   Reads a real number (realval) from a string 
C                    (inpstring) starting at start_pos.  Number is 
C                    assumed to be followed by a comma.  End_pos is
C                    the position after the comma.
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      CHARACTER*(*)  inpstring
      INTEGER*4      commpos, start_pos, end_pos, ier
      
      REAL*4         realval
      
C     Find the comma in inpstring
      
      IF( start_pos .GT. LEN(inpstring) ) THEN
         WRITE(*,*) 'Start position ', start_pos, 'past the length of '
         WRITE(*,*) 'input string ', inpstring
         STOP 1
      ENDIF
            
      commpos = SCAN(inpstring(start_pos:),',')
      IF( commpos .EQ. 0 ) THEN
         READ(inpstring(start_pos:),'(f30.0)', IOSTAT = ier ) realval
         IF( ier .NE. 0 ) THEN
            WRITE(*,*) 
     &            'Error reading real value from ',inpstring(start_pos:)
            WRITE(*,*)
     &            'Error Number - ', ier                     
            STOP 1
         ENDIF
         end_pos = LEN(inpstring)
      ELSE
         READ(inpstring(start_pos:start_pos+commpos),'(f30.0)', 
     &                  IOSTAT = ier) realval
         IF( ier .NE. 0 ) THEN
            WRITE(*,*) 'Error reading real value from ',
     &                  inpstring(start_pos:start_pos + commpos)
            WRITE(*,*)
     &            'Error Number - ', ier
            STOP 1
         ENDIF
                                                             
         end_pos = start_pos + commpos
      ENDIF
      
      RETURN
      
      END         