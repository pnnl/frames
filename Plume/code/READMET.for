      SUBROUTINE READMET( io, newmet)
C-----------------------------------------------------------------------
C
C     READMET.FOR
C
C     Date:    2/23/98
C
C     Description:   Reads the Meteorological Data from Meteorological
C                    File
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      INCLUDE  'metinput.inc'
      RECORD /metinput/ newmet
      
      INTEGER  io, ier
      
      newmet.end = .FALSE.
      
c      READ( io, 101, IOSTAT = ier ) newmet.year, newmet.month, 
      READ( io, *, IOSTAT = ier ) newmet.year, newmet.month, 
     &newmet.day, newmet.hour, newmet.stab, newmet.wnddir, 
     &newmet.wndspd, newmet.tempk, newmet.mixhgt, newmet.pcode,
     &newmet.prate, newmet.wght

c101   FORMAT(1x,i4,3(1x,i2),1x,i1,1x,f5.0,1x,f5.1,1x,f6.2,1x,f6.0,1x,
c     &       i1,1x,f6.2,1x,f6.3)      
            
      IF( ier .LT. 0 ) THEN
         newmet.end = .TRUE.
         RETURN
      ELSEIF( ier .GT. 0 ) THEN
         WRITE( 25, * ) 'Error Reading Meteorological Data'
         WRITE( 25, * ) 'Error Number - ', ier
         STOP 1
      ENDIF
      
c      WRITE (25,'(a,2i3,i5,i3)') ' Met date ',
c     &     newmet.month, newmet.day,newmet.year, newmet.hour
c      WRITE (25,'(a,f5.0,f5.1,2i2,f7.4,f7.2,f7.0,f7.3)') 
c     &    ' Met Data ',newmet.wnddir, newmet.wndspd, newmet.stab,
c     &      newmet.pcode,newmet.prate, newmet.tempk, newmet.mixhgt,
c     &      newmet.wght         

      CALL CHECKMET( newmet )
      
      IF (newmet.missmet) RETURN
      
      newmet.tempc = newmet.tempk - 273.15
      
      RETURN
      
      END                    
      