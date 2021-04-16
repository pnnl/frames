      SUBROUTINE READSTR( inpstring, start_pos, outstring, end_pos )
C-----------------------------------------------------------------------
C     READSTR
C
C     Christian J Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Date:    4/10/97
C
C     Description:   Grabs a character string (outstring) from a larger 
C                    string (inpstring) starting at start_pos.  
C                    The string is assumed to be in double quotes with
C                    a comma following. End_pos is the 
C                    postion after the last character in the string 
C                    (the double quote or the comma)
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      CHARACTER*(*) inpstring, outstring
      
      INTEGER*4      firstpos, firstpos1, lastpos, commpos, start_pos,
     &               end_pos

      IF( start_pos .GT. LEN(inpstring) ) THEN
         WRITE(*,*) 'Start position ', start_pos, 'past the length of '
         WRITE(*,*) 'input string ', inpstring
         STOP 1
      ENDIF
      
C     Find the position of the first quote or comma
      firstpos = SCAN(inpstring(start_pos:),'"')
      firstpos1 = SCAN(inpstring(start_pos:),',')         

      IF( firstpos1 .EQ. 0 .AND. firstpos .EQ. 0 ) THEN
         outstring = ' ' 
         end_pos = 0
         RETURN
      ENDIF
      
      IF( firstpos1 .GT. 0 .AND. firstpos .EQ. 0 ) THEN
         IF( firstpos1 .GT. 1 ) THEN
            outstring = inpstring(start_pos:start_pos + firstpos1-2)
         ELSE
            outstring = ' ' 
         ENDIF
         end_pos = start_pos + firstpos1 
         RETURN
      ELSEIF( firstpos1 .GT. 0 .AND. firstpos1 .LT. firstpos ) THEN
         IF( firstpos1 .GT. 1 ) THEN
            outstring = inpstring(start_pos:start_pos + firstpos1-2)
         ELSE
            outstring = ' ' 
         ENDIF
         end_pos = start_pos + firstpos1 
         RETURN
      ENDIF
          
      IF( (firstpos .EQ. 0) .OR. 
     &    ((firstpos + start_pos+1) .GT. LEN(inpstring)) ) THEN
         outstring = ' '
         end_pos = 0
         RETURN
      ENDIF
            
C     Find the next quote
      lastpos = SCAN(inpstring(start_pos + firstpos + 1:),'"')
      
      IF( lastpos .EQ. 0 ) THEN                    
         outstring = ' '
         end_pos = 0
         RETURN
      ENDIF         
      
      IF (start_pos+firstpos .LE. start_pos+firstpos+lastpos-1 ) THEN
         outstring = inpstring(start_pos+firstpos:
     &                      start_pos + firstpos + lastpos-1)
      ELSE
         outstring = ' ' 
      ENDIF
      
C     Find the comma if any
      commpos = SCAN(inpstring(start_pos+firstpos+lastpos:),',')
      end_pos = start_pos+firstpos + lastpos
      IF( commpos .GT. 0 ) THEN
         end_pos = end_pos + commpos 
      ENDIF
      
      RETURN
      
      END      