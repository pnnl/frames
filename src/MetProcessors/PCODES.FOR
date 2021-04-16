      SUBROUTINE PCODES( iprec, prpcode, precip )
C***********************************************************************
C     PCODES.FOR 
C
C     Christian Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Created:    1/16/96
C
C     Description: Convert CD144 precipitation type data to two-digit
C       precipitation codes; SCRAM has blanks (zeroes) for these fields
C       Adapted from the PCODES subroutine in PCRAMMET 
C             
C***********************************************************************
      IMPLICIT    NONE
      
      INTEGER     PRCPCODE
      INTEGER     iprec(5)
      INTEGER     fzcod, kt, lqcod, prpcode

      REAL        prrate(6)
      REAL        precip
      
      DATA        prrate / 1, 5, 10, 0.25, 1.25, 2.5 /

c      INTEGER     idate, idiag            
c      CHARACTER*6 sftype

c      LOGICAL      NOWARN

      kt = iprec(1) + iprec(2) + iprec(3) + iprec(4) + iprec(5)
      
      IF( kt.EQ.0 ) THEN
         precip = 0
         prpcode = 0
         RETURN
      ELSE
         
c        Get liquid and frozen precipitation codes
         
         lqcod = iprec(1)*10 + iprec(2)
         fzcod = iprec(3)*100 + iprec(4)*10 + iprec(5)

      ENDIF
      
      prpcode = PRCPCODE( lqcod, fzcod )

      IF ( prpcode.NE.0 .AND. (precip.EQ.0.OR.precip.GT.9990) ) THEN
         precip = prrate(prpcode)
      ENDIF
      
C*--- CHECK FOR MORE THAN ONE TYPE OF PRECIPITATION REPORTED FOR THE SAME HOUR
      
c      IF( iliq.EQ.1 .AND. ifroz.EQ.1)THEN
c         WRITE(IDIAG,1000) IDATE,SFTYPE,IPREC,IPCODE
c1000     FORMAT(' Warning -- more than one type of precipitation ',
c     1          'reported on (YYDDDHH):(',I7,')',/,15X,
c     2          A6,' TYPE = ',5I1,3X,'precip. code used = ',I2)
c         NOWARN = .FALSE.
c      ENDIF

      RETURN
      END
