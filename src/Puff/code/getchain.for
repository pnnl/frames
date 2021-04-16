      SUBROUTINE GETCHAIN( nucfile, nio, numchain )
C-----------------------------------------------------------------------
C     GETCHAIN
C
C     Christian Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C                                   
C     Date: 9/26/97
C
C     Description:   Opens nuclide file and gets the number of chains
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      INTEGER  nio, ier, numchain
      CHARACTER*(*) nucfile
      
      nio = 37
      
      OPEN(unit = nio, File = nucfile, STATUS = 'OLD', IOSTAT = ier )
      
      IF (ier .NE. 0 ) THEN
         WRITE(*,*) 'Error reading file - ', nucfile
         WRITE(*,*) 'Error Number - ', ier
         STOP 1
      ENDIF
      
      READ( nio, *) numchain
      
      RETURN
      
      END