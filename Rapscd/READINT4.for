      SUBROUTINE READINT4( inpstring, start_pos, int4val, end_pos)
C-----------------------------------------------------------------------
C
C     READINT4
C
C     Christian J Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Date:    4/10/97
C
C     Description:   Gets an integer(*4) value (int4val) from a
C                    character string(inpstring) starting at start_pos.
C                    Integer is assumed to be delimited by a comma.
C                    End_pos is the position of the comma.
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      CHARACTER*(*) inpstring
      INTEGER*4     int4val, start_pos, end_pos, ier, commpos
      
      IF( start_pos .GT. LEN(inpstring) ) THEN
         WRITE(*,*) 'Start position ', start_pos, 'past the length of '
         WRITE(*,*) 'input string ', inpstring
         STOP 1
      ENDIF
      
C     Find the position of the comma

      commpos = SCAN(inpstring(start_pos:),',')
      
      IF( commpos .EQ. 0 ) THEN
         READ( inpstring(start_pos:),'(i35)',IOSTAT = ier ) int4val
         IF( ier .NE. 0 ) THEN
            WRITE(*,*) 'Error reading integer from ', 
     &                  inpstring(start_pos:)
            WRITE(*,*) 'Error number = ', ier
            STOP 1
         ENDIF          
         end_pos = len(inpstring)
      ELSE
         READ( inpstring(start_pos:start_pos + commpos),'(i35)',
     &         IOSTAT = ier ) int4val
         IF( ier .NE. 0 ) THEN
            WRITE(*,*) 'Error reading integer from ', 
     &                  inpstring(start_pos:start_pos + commpos)
            WRITE(*,*) 'Error number = ', ier
            STOP 1
         ENDIF
         end_pos = start_pos + commpos
      ENDIF
      
      RETURN
      
      END  