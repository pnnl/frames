      SUBROUTINE RDPRMJFD( rio )
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     RDPRMJFD
C
C     Christian J Fosmire
C     Pacific Northwest National Laboratory
C     P.O. Box 999
C     Richland, WA 99352
C
C     Created:    1/19/96
C
C     Description:   The program reads in the parmeter from the run
C        specification file to run the JFDPROC
C
C     Revision History
C       24 Dec 2002  BAN  Changed output from direct to sequential
C       16 July 2003 BAN  Corrected for precip probability being percent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      
      INCLUDE  'jfdproc.inc'
      
      INTEGER  i, ier, j, rio
      
      LOGICAL  exists
      
      CHARACTER*80   jfdfile, outfile

c     Set io values for output and jfd

      oio = 20
      jdio = 30

C     Read in output file

      READ( rio, '(a80)', IOSTAT = ier ) outfile
         
      IF( ier.NE.0 ) THEN
         WRITE(*,*) 'Error reading run specification file - output file'
         WRITE(*,*) 'Error Number - ',ier
         STOP 1
      ENDIF
      
      OPEN( oio, FILE = outfile, STATUS = 'UNKNOWN', FORM = 'FORMATTED',
c     &      ACCESS = 'DIRECT', RECL = 66, IOSTAT = ier )
     &      IOSTAT = ier )
      
      IF( ier.NE.0 ) THEN
         WRITE(*,*) 'Error open output file'
         WRITE(*,*) 'Error Number - ', ier
         WRITE(*,*) 'File name - ', outfile
         STOP 1
      ENDIF

C     Read in file name of JFD

      READ( rio, '(a80)', IOSTAT = ier )jfdfile
      
      IF( ier.ne.0 ) THEN
         WRITE(*,*) 'Error reading run specification file - jfdfile'
         WRITE(*,*) 'Error Number - ', ier
         STOP 1
      ENDIF

c     Check if JFD file exists

      exists = .FALSE.
      INQUIRE( FILE = jfdfile, EXIST = exists )
      
      IF ( .NOT.exists ) THEN
         WRITE(*,*) 'JFD file does not exist'
         WRITE(*,*) 'JFD file is ', jfdfile
         STOP 1
      ENDIF
      
      OPEN ( jdio, FILE = jfdfile, STATUS = 'OLD', IOSTAT = ier )
      
      IF( ier.NE.0 ) THEN
         WRITE(*,*) 'Error opening JFD file'
         WRITE(*,*) 'Error Number - ', ier
         STOP 1
      ENDIF

c     Read in Flag on whether EPA's STAR or GENII JFD

      READ( rio, *, IOSTAT = ier ) jfdtype
      
      IF( ier .NE. 0 ) THEN
         WRITE(*,*) 'Error Reading Type of JFD File'
         WRITE(*,*) 'Error Number - ', ier
         Stop 1
      ENDIF      
                
c     Read Latitude of the Station

      READ( rio, *, IOSTAT = ier ) lat
      
      IF( ier.ne.0 ) THEN
         WRITE(*,*) 'Error reading run specification file - latitude'
         WRITE(*,*) 'Error Number - ', ier
         STOP 1
      ENDIF

c     Read height of wind measurement

      READ( rio, *, IOSTAT = ier ) amhgt
      
      IF( ier.NE.0 ) THEN
         WRITE(*,*) 'Error reading run specification file - amem. hgt'
         WRITE(*,*) 'Error Number - ', ier
         STOP 1
      ENDIF

c     Get the surface roughness at the station

      READ( rio, *, IOSTAT = ier ) z0meas
      
      IF( ier.NE.0 ) THEN
         WRITE(*,*) 'Error reading run specification file - z0 meas'
         WRITE(*,*) 'Error Number - ', ier
         STOP 1
      ENDIF
      
c     Get the surface roughness at application site

      READ( rio, *, IOSTAT = ier ) z0appl
      
      IF( ier.NE.0 ) THEN
         WRITE(*,*) 'Error reading run specification file - z0 appl'
         WRITE(*,*) 'Error Number - ', ier
         STOP 1
      ENDIF
      
c     Read number of Stability Classes

      READ( rio, *, IOSTAT = ier ) nstab
      
      IF( ier.ne.0 ) THEN
         WRITE(*,*) 'Error reading run specification file - num stab'
         WRITE(*,*) 'Error Number - ', ier
         STOP 1
      ENDIF
      
      IF( nstab.gt.7 ) THEN
         WRITE(*,*) 'Error number of stability classes too large'
         WRITE(*,*) 'num stab = ', nstab
         STOP 1
      ENDIF
      
c     Read in Number of Wind directions

      READ( rio, *, IOSTAT = ier ) ndir
      
      IF( ier.ne.0 ) THEN
         WRITE(*,*) 'Error reading run specification file - num dir'
         WRITE(*,*) 'Error Number - ',ier
         STOP 1
      ENDIF
      
      IF( ndir.NE.16 .AND. ndir.NE.36 ) THEN
         WRITE(*,*) 'Number of wind direction must be 16 or 36'
         WRITE(*,*) 'Num dir = ', ndir
         STOP 1
      ENDIF
      
      IF (ndir.EQ.16) THEN
         DO i = 1, ndir
            wdir(i) = (i-1)*360./ndir
         ENDDO
      ELSE
         DO i = 1, ndir
            wdir(i) = (i-1)*360./ndir + 5.
         ENDDO
      ENDIF

c     Read in the Number of Wind Speed Classes

      READ( rio, *, IOSTAT = ier )nwspd
      
      IF( ier.NE.0 ) THEN
         WRITE(*,*) 'Error reading run specification file - num wndspd'
         WRITE(*,*) 'Error Number - ', ier
         STOP 1
      ENDIF
      
      IF( nwspd.gt.8 ) THEN
         WRITE(*,*) 'Error - number of wind speed class <= 8 '
         WRITE(*,*) 'Num wind speed Class = ', nwspd
         STOP 1
      ENDIF
      
c     Read in the Wind Speed Midpoints

      READ( rio, *, IOSTAT = ier )(wspd(i), i = 1, nwspd)
      
      IF( ier.ne.0 ) THEN
         WRITE(*,*) 'Error reading run specification file - wnd spds'
         WRITE(*,*) 'Error Number - ',ier
         STOP 1
      ENDIF
      
c     Read in Monthly Average Max. Temps

      READ( rio, *, IOSTAT = ier )(maxtmp(i), i = 1, 12)
      
      IF( ier.NE.0 ) THEN
         WRITE(*,*) 'Error reading run specification file - max temps'
         WRITE(*,*) 'Error Number - ', ier
         STOP 1
      ENDIF
      
c     Read in Monthly average Min Temps

      READ( rio, *, IOSTAT = ier )(mintmp(i), i = 1, 12)
      
      IF( ier.NE.0 ) THEN
         WRITE(*,*) 'Error reading run specification file - min temps'
         WRITE(*,*) 'Error Number - ', ier
         STOP 1
      ENDIF

c     Read in Flag for including Precipiation

      Read( rio, *, IOSTAT = ier)doprcp
      
      IF (ier.ne.0 ) THEN
         WRITE(*,*) 'Error reading run specification file - flag prcp'
         WRITE(*,*) 'Error Number - ', ier
         STOP 1
      ENDIF

      
c     Read in array of prob. of rain or snow as a function of stability
      
      IF (doprcp) THEN
      
         DO i = 1, 4
         
            READ( rio, *, IOSTAT = ier )(prcpprob(i, j), j = 1, 5)
      
            IF( ier.NE.0 ) THEN
               WRITE(*,*) 'Error reading run specification file'
               WRITE(*,*) 'Error reading precip. probability'
               WRITE(*,*) 'Error Number - ', ier
               STOP 1
            ENDIF
         
         ENDDO
         
c     Determine the prob. on no rain for given stability class

         DO j = 1, 5                      
            nprcp_prob(j) = 0
            DO i = 1, 4
CCC               nprcp_prob(j) = nprcp_prob(j) + prcpprob(i,j)
               nprcp_prob(j) = nprcp_prob(j) + prcpprob(i,j)/100.
            ENDDO
            nprcp_prob(j) = 1 - nprcp_prob(j)
         ENDDO          
         
      ENDIF
      
c     Read in number of hours in JFD

      READ( rio, *, IOSTAT = ier ) numhr
      
      IF( ier.NE.0 ) THEN
         WRITE(*,*) 'Error reading run specification file - num hours'
         WRITE(*,*) 'Error Number - ',ier
         STOP 1
      ENDIF
      
      RETURN
      END                                   