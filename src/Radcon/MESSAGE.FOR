C     MEPAS RADCON: MESSAGE.FOR          Version Date: 11-05-1996
C     Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                            SUBROUTINE MESSAGE                              *
C                                                                            *
C This subroutine writes a message to the *.ERR file when an error has       *
C occurred and the run is aborted.  The message notifies the user that an    *
C error has occurred and to consult the *.WLS or *.WRN file for details.     *
C                                                                            *
C                                                                            *
C Written By:           John McDonald                                        *
C                       Battelle Pacific Northwest Laboratories              *
C                       P.O. Box 999                                         *
C                       Richland, WA 99352                                   *
C                                                                            *
C Creation Date:        07/28/95 JPM                                         *
C Last Modified:        11/05/96 JPM                                         *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of:  RADCON
C     Called by:  
C     Calls to:   none
C     Common blocks referenced: none
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter  Set/          Location
C     Name       Used  Type    Export/Import  Parameter Description
C     ---------- ----- ------- -------------- --------------------------------
C
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     08/25/95     JPM  Carriage return adjustment in FORMAT statement.
C                       Message was wrapping around screen if code aborts
C                       within the NTIMES loop.
C     11/05/96     JPM  Writing the message to the *.ERR file instead of the
C                       screen (PH1-E004).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE MESSAGE ()
C
C==== COMMON Block Definitions ===============================================
C
C     none
C
      WRITE (13,99)
   99 FORMAT (/
     +' ERROR --> Run aborted!!! - see the *.WLS or *.WRN file')
      RETURN
      END
