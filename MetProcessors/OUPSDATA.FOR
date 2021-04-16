      SUBROUTINE OUPSDATA( io, irec, stab, wdir, wndspd, atemp, mixhgt,
     &                     ptyp, prate, wght )
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     
C     OUPSDATA
C
C     Christian J Fosmire
C     Pacific Northwest National Laboratory
C     P.O. Box 999
C     Richland, WA 99352
C
C     Created:    1/19/96
C
C     Description:   This program writes out the psuedo hrly data for
C       use in CHRONSRC and ACUTESRC
C
C     Revision history
C       24 Dec 2002  BAN  Changed from direct to sequential output file
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE

      REAL     atemp, mixhgt, prate, wndspd, wdir, wght
      INTEGER  io, irec, ptyp, stab
      
c     Check if wght is greater than 0.01

      IF( wght .ge. 0.01 ) THEN
         irec = irec + 1
C         WRITE( io, 101, REC = irec ) 
         WRITE( io, 101) 
     &            -1, -1, -1, -1, stab, wdir, wndspd, 
     &            atemp, mixhgt, ptyp, prate, wght
     
      ENDIF

101   FORMAT( 1x, i4, 3(1x, i2), 1x, i1, 1x, f5.0, 1x, f5.1, 1x, f6.2, 
     &        1x, f6.0, 1x, i1, 1x, f6.2, 1x, f8.4)
           
      RETURN
      
      END            