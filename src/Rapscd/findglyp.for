      SUBROUTINE FINDGLYP(io, glypnm, ier)
C-----------------------------------------------------------------------
C     FINDGLYP
C
C     Christian Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Date:    6/10/97
C
C     Description:   Finds the correct module in the AFF file. 
C                    Glypnm is the name of the module to find.
C                    IO is the unit number of the AFF File and 
C                    ier is the error number (0 if no error)
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      CHARACTER*100  teststr
      CHARACTER*33   glypnm, testglyp   
      CHARACTER*1    dum
      INTEGER        io, ier, i, nextpos, glyplines
      
      LOGICAL        RightGlyp, COMPSTR

C     Initialize Loop Logical Variable
      
      RightGlyp = .FALSE.
                                      
      DO WHILE( .NOT. RightGlyp )

C     Read in data string
         READ( io,'(a)', IOSTAT = ier )teststr
         
         IF ( ier .NE. 0 ) THEN
            RETURN
         ENDIF       

C        Get glyph name and lines from data string

         CALL READSTR(teststr,1,testglyp,nextpos)
         CALL READINT4(teststr,nextpos,glyplines,nextpos)   
cx         write(*,*) 'str=',teststr
cx         write(*,*) ' glypnm  =', glypnm
cx         write(*,*) ' testglyp=', testglyp
C        Determine if glyph name is correct one (skip ahead if not)
         
         IF ( COMPSTR(glypnm,testglyp) ) THEN
            RightGlyp = .TRUE.
cx            write(*,*) 'true'
            CYCLE 
         ELSE
            DO i = 1, glyplines
              READ( io, '(a1)') dum
            ENDDO
cx            write(*,*) 'false'


         ENDIF
      ENDDO
                  
      RETURN
      
      END            
