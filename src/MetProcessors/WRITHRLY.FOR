      SUBROUTINE WRITHRLY( irec )
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     WRITHRLY
C
C     Christian Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Created:    1/17/96
C
C     Description:   Writes out the hourly weather obs in a format
C        to be used by CHRONSRC or ACUTESRC
C
C------------------------------------Modification history---------------
C
C       5 OCtober 2004   BAN   remove "REC" from write statement
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE

      INCLUDE  'hrly.inc'
      
      INTEGER  ihr, irec
      
      DO ihr = 1, 24
      
c         IF( recok(ihr) ) THEN
            
            irec = irec + 1
            
c            WRITE( iout, 101, REC = irec  ) 
c     &                       isyr(ihr), ismo(ihr), isdy(ihr), ishr(ihr), 
c     &                       kst(ihr), afv(ihr), ustr(ihr), temp(ihr),
c     &                       mixhgt(ihr), prpcode(ihr), precip(ihr), 
c     &                       1.0
            WRITE( iout, 101)
     &                       isyr(ihr), ismo(ihr), isdy(ihr), ishr(ihr), 
     &                       kst(ihr), afv(ihr), wndspd(ihr), temp(ihr),
     &                       mixhgt(ihr), prpcode(ihr), precip(ihr), 
     &                       1.0


d          WRITE( 70, 102 ) isyr(ihr), ismo(ihr), isdy(ihr), ishr(ihr),
cd    &                       kst(ihr), afv(ihr), ustr(ihr), temp(ihr),
cd    &                       mixhgt(ihr), prpcode(ihr), precip(ihr), 
cd    &                       1 
d    &                       kst(ihr), afv(ihr), wndspd(ihr), temp(ihr),
d    &                       mixhgt(ihr), prpcode(ihr), precip(ihr), 
d    &                       1 
     
c         ENDIF

      ENDDO

c101   FORMAT(1x,i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,f6.0,1x,f6.3,1x,f6.2,1x,
c     &       f6.1,1x,i6,1x,f6.2,1x,f6.0)
101   FORMAT(1x,i4,1x,i2,1x,i2,1x,i2,1x,i1,1x,f5.0,1x,f5.1,1x,f6.2,1x,
     &       f6.0,1x,i1,1x,f6.2,1x,f6.0)

d102   FORMAT( 4(1x,i2),1x,i2,1x,f6.0,1x,f6.3,1x,f6.2,1x,f6.1,1x,i6,1x,
d    &        f6.2,1x,i1)         
      
      RETURN
      END       