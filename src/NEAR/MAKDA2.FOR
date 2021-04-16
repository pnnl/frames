C--------------------------------------------------------------------------
C
      SUBROUTINE MAKDA2 
C
C     This subroutine gets system date and time and stores them as
C     character strings.  
C   
C     General Module of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System
C 
C     Last Modification: 24-Mar-89  RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C
C--------------------------------------------------------------------------
C
      INCLUDE 'DAYPC.CMN'
C
      CALL DATE (TODAY)
      CALL TIME (CLOCK)
C     
      RETURN
C--------------------------------------------------------------------------
      END
