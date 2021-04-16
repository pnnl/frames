      SUBROUTINE CCCODE(IDIAG,CCC,DDD,ISKY,IROOF)
C***********************************************************************
C*    CCCODE      Module of PCRAMMET Meteorological Pre-processor 
C*
C*    PURPOSE:    Decodes the Ceiling Height and Cloud Cover from the 
C*                CD144 or SCRAM Data
C*
C*    PROGRAMMER: Jayant Hardikar
C*                PES Inc.
C*
C*    DATE:       February 23, 1995
C*     
C*    INPUTS:     Surface Data from CD144 or SCRAM
C*
C*    OUTPUTS:    1 Hour Numeric Value for Ceiling and Cloud Cover
C*        
C*    CALLED FROM: FIL144
C***********************************************************************
C*
C*    Variable Declarations
      CHARACTER*1 DIG(11), CTMP(3), CC
      CHARACTER*5 CCC,DDD
      INTEGER IDG(3)
      LOGICAL NOWARN
      DATA DIG /'0','1','2','3','4','5','6','7','8','9','-'/    

C*    Decode Cloud Cover - if it is missing, set to 10/10
      READ (DDD(1:1),'(A1)') CC

80    DO 90 JK=1,11                                                 
         IF (CC .EQ. DIG(JK)) GO TO 100
90    CONTINUE                                                      
      JK=11                                                         
      WRITE (IDIAG,530) CC
530   FORMAT (' CCCODE: The character ',A1,' is not allowable.',
     &        ' Cloud cover defaults to 10.')
      NOWARN = .FALSE.
100   ISKY=JK-1

C*    Decode Ceiling Height
      IF( CCC(1:1) .EQ. '-'  .OR.  CCC(1:3) .EQ. '   ' )THEN
         IDG(1)=9
         IDG(2)=9
         IDG(3)=8

      ELSE
         DO 20 JJ= 1,3
            READ (CCC(JJ:JJ),'(A1)') CTMP(JJ)
20       CONTINUE

110      DO 140 JI=1,3
            DO 120 JK=1,10
               IF (CTMP(JI).EQ.DIG(JK)) GO TO 130
120         CONTINUE
130         IDG(JI)=JK-1
140      CONTINUE
      ENDIF

C*    Compute ceiling height in hundreds of feet.
150   IROOF = IDG(1)*100 + IDG(2)*10 + IDG(3)

      RETURN
      END
