      SUBROUTINE HR0024(IYR,JULDAY,IHR)
C***********************************************************************
C*    HR0024 Utility Module
C*
C*    PURPOSE:    TO CONVERT A DATE/TIME GROUP FROM HOUR=0 TO HOUR=24 
C*                OF THE PREVIOUS DAY
C*
C*    INPUTS:     Julian day and hour
C*
C*    OUTPUTS:    Julian day and hour
C*        
C*    CALLED FROM: Various subprograms
C***********************************************************************

C*    Variable DeclarationsC
      INTEGER IYR, JULDAY, IHR

C*    IYR    = YEAR
C*    JULDAY = JULIAN DAY
C*    IHR    = IHR

      IHR = 24
      IF(JULDAY .EQ. 1) THEN
         IYR = IYR - 1
         IF( (MOD(IYR,4).EQ.0) .OR. (MOD(IYR,400).EQ.0) ) THEN
            JULDAY = 366
         ELSE
            JULDAY = 365
         ENDIF
      ELSE
         JULDAY = JULDAY - 1
      ENDIF

      RETURN
      END
