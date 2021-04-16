      SUBROUTINE READGENI
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     READGENI
C
C     Christian J Fosmire
C     Pacific Northwest National Laboratory
C     P.O. Box 999
C     Richland, WA 99352
C
C     Created:    1/19/96
C
C     Description:  This program reads in the joint frequency 
C        distribution (JFD).  Assumes the JFD is GENII format
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT  NONE

      INCLUDE  'jfdproc.inc'
      
      REAL     prec(16)
      INTEGER  idir, ier, ist, iwsp, i
      CHARACTER*1 dum

C     skip first 4 lines - header stuff
      
      DO i = 1, 4
         READ( jdio, '(a)', IOSTAT = ier ) dum
         IF ( ier .NE. 0 ) THEN
            WRITE(*,*) 'Error trying to read header',i,' of GENII JFD'
            WRITE(*,*) 'Error Number - ',ier
            Stop 1
         ENDIF
      ENDDO
      
c     Read in JFD
      
      
      DO iwsp = 1, nwspd
            
         DO ist = 1, nstab
         
            READ( jdio, *, IOSTAT = ier ) 
     &         (prec(idir), idir = 1, ndir)
            
            IF( ier.NE.0 ) THEN
               WRITE(*,*) 'Error reading JFD file'
               WRITE(*,*) 'Error Number - ', ier
               WRITE(*,*) 'jdio = ', jdio, oio
               STOP 1
            ENDIF
            
            DO idir = 1, ndir
               freq(ist,iwsp,idir) = prec(idir)/100.0
            ENDDO
            
         ENDDO
      ENDDO
      
      CLOSE( jdio )
      
      RETURN
      END      