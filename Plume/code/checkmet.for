      SUBROUTINE CHECKMET( newmet )
C-----------------------------------------------------------------------
C
C     CHECKMET
C
C     Date: 2/23/98
C
C     Description:   Check the meteorological data to see if missing
C                    data and if came from joint frequency distribution
C                    (JFD) file.
c
c
c ----------------------------------------------------------------------
c  REVISION HISTORY
c 
C       1 JUNE 2010   BAN   ADDED TRAP FOR ZERO PCODE, NON-ZERO PRATE
C       9 March 2012  BAN   Some new checks on wind speed and mixing depth
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      INCLUDE  'metinput.inc'
      RECORD   /metinput/ newmet
      
C     JFD files have year set to -1
      IF( newmet.year .LT. 0 ) THEN
         newmet.jfdmet = .TRUE.
      ELSE
         newmet.jfdmet = .FALSE.
      ENDIF

c     Check to see if data is missing
      
      newmet.missmet = .FALSE.
      
      IF( newmet.stab .LT. 1 .OR. newmet.stab .GT. 7 ) THEN
         newmet.missmet = .TRUE.
      ELSEIF( newmet.wnddir .LT. 0 .OR. newmet.wnddir .GT. 360.0 ) THEN
         newmet.missmet = .TRUE.
c      ELSEIF( newmet.ustar .LT. 0 ) THEN
c      this seems to have been an error with ustar.
c      BAN 3/9/2012
      ELSEIF( newmet.wndspd .LT. 0 ) THEN
         newmet.missmet = .TRUE.
      ELSEIF( newmet.tempk .LT. 0 .OR. newmet.tempk .GT. 385.0 ) THEN
         newmet.missmet = .TRUE.
c      ELSEIF( newmet.mixhgt .LT. 0 .OR. newmet.mixhgt .GT. 8000.0 ) THEN
c      trap added BAN 3/9/2012 for too-low mixing depths
      ELSEIF( newmet.mixhgt .LT. 5 .OR. newmet.mixhgt .GT. 8000.0 ) THEN
         newmet.missmet = .TRUE.
      ELSEIF( newmet.pcode .LT. 0 .OR. newmet.pcode .GT. 6 ) THEN
         newmet.missmet = .TRUE.
      ELSEIF( newmet.prate .LT. 0 ) THEN
         newmet.missmet = .TRUE.
      ENDIF
C  add a trap for pcode=0, prate>0  BAN 1 June 2010
      IF( NEWMET.PCODE .EQ. 0 .AND. NEWMET.PRATE .NE. 0) THEN
	   NEWMET.PRATE = 0.0
	END IF
                              
      RETURN
      
      END                        
      