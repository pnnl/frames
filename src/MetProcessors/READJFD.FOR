      SUBROUTINE READJFD
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     READJFD
C
C     Christian J Fosmire
C     Pacific Northwest National Laboratory
C     P.O. Box 999
C     Richland, WA 99352
C
C     Created:    1/19/96
C
C     Description:  This program reads in the joint frequency 
C        distribution (JFD).  Assumes the JFD has columns of wind speed,
C        and row of wind direction by stability
C
C     Revision History
C        24 Dec 2002   BAN  Convert read to Barry Parks' STAR format for EPA
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT  NONE

      INCLUDE  'jfdproc.inc'
      
      INTEGER  idir, ier, ist, iwsp
C Addition for STAR
      Character*4 A1
	Character*3 A2
      
c     Read in JFD
      
      
      DO ist = 1, nstab
      
         DO idir = 1, ndir
         
C            READ( jdio, *, IOSTAT = ier ) 
C     &         (freq(ist,iwsp,idir), iwsp = 1, nwspd)
C
C      Change by BAN
C
            READ( jdio, 1, IOSTAT = ier ) 
     &         A1, A2, (freq(ist,iwsp,idir), iwsp = 1, nwspd)
    1       format(a4,a3,8f7.5)
C
     
            IF( ier.NE.0 ) THEN
               WRITE(*,*) 'Error reading JFD file'
               WRITE(*,*) 'Error Number - ', ier
               WRITE(*,*) 'jdio = ', jdio, oio
	         WRITE(*,*) 'Number of stability classes input=',nstab,
     .                     ' Number checked =',ist
	         WRITE(*,*) 'Number of directions input=',ndir,
     .                     ' Number checked =',idir
	         WRITE(*,*) 'Number of windspeeds input=',nwspd,
     .                     ' Number checked =',iwsp-1
               STOP 1
            ENDIF
            
         ENDDO
      ENDDO
      
      CLOSE( jdio )
      
      RETURN
      END         