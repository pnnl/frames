      LOGICAL FUNCTION COMPSTR ( oldstr, newstr)
C-----------------------------------------------------------------------
C     COMPSTR
C
C     Christian Fosmire
C     Pacific Northwest National Lab
C
C     Date: 9/10/97
C
C     Description:   Determines if two strings are equal without
C                    regard to case (upper or lower)
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      CHARACTER*(*) oldstr, newstr
      INTEGER*4   lenstr, lenstr1, charindex, i, charindex1
      CHARACTER*1 chkchar, chkchar1
      
      
      lenstr = LEN_TRIM(oldstr)
      lenstr1 = LEN_TRIM(newstr)
      
      IF (lenstr .NE. lenstr1) THEN
         COMPSTR = .FALSE.
         RETURN
      ENDIF
      
      COMPSTR = .TRUE.
      
c     Check each character in string                                
      DO i = 1, MIN0(lenstr,lenstr1)
                                           
         READ(oldstr(i:i),'(a1)') chkchar
         READ(newstr(i:i),'(a1)') chkchar1
         charindex = ICHAR(chkchar)
         charindex1 = ICHAR(chkchar1)                      
         
         IF ( charindex .EQ. charindex1 ) THEN
            CYCLE
         ELSE
c     Determine if lower case letter
         
            IF( charindex .GE. 97 .AND. charindex .LE. 122 ) THEN

c     Convert to upper case letter

               chkchar = CHAR(charindex - 32 )
            ENDIF
            
            IF( charindex1 .GE. 97 .AND. charindex .LE. 122 ) THEN
               chkchar1 = CHAR(charindex1 - 32 )
            ENDIF
            
            IF( chkchar .NE. chkchar1 ) THEN
               COMPSTR = .FALSE.
               EXIT
            ENDIF
         ENDIF
      ENDDO                       


      RETURN
      
      END
