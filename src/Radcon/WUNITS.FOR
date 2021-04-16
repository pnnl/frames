C     MEPAS RADCON: WUNITS.FOR            Version Date: 06-09-1998
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                            SUBROUTINE WUNITS                               *
C                                                                            *
C  Subroutine WUNITS reads the number of waste sites per facility            *
C  (NUMWU).  It is called from the main program.                             *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Northwest Laboratories                         *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    06/09/98 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: RADCONPC
C     Calls to: None
C     Common blocks referenced: INDEX5
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter  Set/           Location
C     Name       Used   Type    Export/Import  Parameter Description
C     --------- ------ -------  -------------- -----------------------
C     NUMWU      SET   INTEGER  COMMON - EXP   # OF SITES PER FACILITY
C pdm NUMITR     SET   INTEGER  COMMON - EXP   # OF INFILTRATION RATES FOR REMEDIATION PURPOSES   
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/26/91     JWB  Added required comments and header for MEPAS QA plan
C     05/15/98     PDM  The definition of NUMITR was added.  Include
C                       statements for MAXITR and ERRORSIG were added.  A
C                       check to make sure NUMITR-1 doesn't exceed MAXITR,
C                       the dimension of TREMED was added. ERRORSIG is set
C                       and messages are written to the *.WLS and *.WRN files
C                       if NUMITR is too large. A check was added to make
C                       sure NUMITR is no smaller than 1.  Statement number
C                       9999 was added to the RETURN statement (RAD-E054).
C     06/09/98     JPM  Reading NUMFLX from the WIN file (PH1-E017).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE WUNITS
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'INDEX5.WTN'
      INCLUDE 'ERROR.WTN'
      INCLUDE 'CAP.WTN'
C
C==== DIMENSION Statements ===================================================
C
C     None
C
C==== Variable Declarations ==================================================
C
C     None
C
C==== DATA Statements ========================================================
C
C     None
C
c
c modified read, to obtain remediation data  JLS
c
      read(5,'(3I5)') NUMWU, numitr, NUMFLX
c      READ(5,1) NUMWU
    1 FORMAT (I5)
      IF (NUMWU .LE. 0) NUMWU=1
c pdm
      if (numitr .le. 0) numitr = 1
      if (numitr .gt. MAXITR+1) then
           write (6,20) numitr,MAXITR+1,MAXITR+1
           write (11,20) numitr,MAXITR+1,MAXITR+1
           CALL MESSAGE ()
           ERRORSIG=.TRUE.
           GOTO 9999
      endif
20    FORMAT (
     C /2x,'ERROR --> NUMITR = ',I5,
     C /2x,'Cannot run with more than ',I2,' separate infiltration ',
     C 'rates',/2x,'(NUMITR must be <= ',I2,')')
9999  RETURN
c pdm
      END

