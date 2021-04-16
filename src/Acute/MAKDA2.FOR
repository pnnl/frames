C---- ACUTE: MAKDA2.FOR ------------- Version: 29-Jul-97 ---------------------
C
C      SUBROUTINE MAKDA2 
C
C     This subroutine gets system date and time and stores them as
C     character strings.  
C   
C     General Module of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System
C 
C     Inidial GENII version Modification: 24-Mar-89  RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C     Last Modification: 29-Jul-97 DLS
C
C--- Modification History ----------------------------------------------------
C    Date     Who    Description of Modification
C  ---------  ---  -----------------------------------------------------------
C  29-Jul-97  DLS  Updated heading information and comments
C
C-----------------------------------------------------------------------------
C
      SUBROUTINE MAKDA2 
C
C----- Include Statements ----------------------------------------------------
C
      INCLUDE 'DAYPC.CMN'
C
C---- Get date ---------------------------------------------------------------
C
      CALL DATE (TODAY)
C
C---- Get time of day --------------------------------------------------------
C
      CALL TIME (CLOCK)
C     
      RETURN
      END
C
C---- End of Module MAKDA2 ---------------------------------------------------
C
