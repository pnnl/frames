      SUBROUTINE  OPMETFIL( metfile, io, z0, amhgt )
C-----------------------------------------------------------------------
C
C     OPMETFIL
C
C     Date:    3/16/1998
C
C     Description:   Opens the Meteorological Data file and Reads the
C                    Surface Roughness
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      INTEGER        io, ier
      
      REAL           z0, amhgt
      
      CHARACTER*50   metfile
      
      io = 41
      
      OPEN( io, FILE = metfile, STATUS = 'OLD', IOSTAT = ier )
      
      IF( ier .NE. 0 ) THEN
        WRITE(25,*) 'Error Reading Meteorological Data File - ', metfile
         WRITE(25,*) 'Error Number - ', ier
         STOP 1
      ENDIF
      
      READ( io, *, IOSTAT = ier )z0, amhgt
      
      IF( ier .NE. 0 ) THEN
         WRITE(25,*) 'Error Reading First Line of Met Data File - ', 
     &               metfile
         WRITE(25,*) 'Error Number - ', ier
         STOP 1
      ENDIF
      
      RETURN
      
      END
      