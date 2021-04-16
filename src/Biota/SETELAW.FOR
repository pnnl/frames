C  SETELAW.FOR EXPOS                Version Date: 30-Dec-96               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE SETELAW                               *
C                                                                            *
C  Subroutine SETELAW sets element symbol into ELTT and atomic weight into   *
C             AWT for use in testing environmental parameters read from      *
C             transfer factor and bioaccumulation data files                 *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    30-Dec-96                                               *
C  Last Modified:    30-Dec-96      DLS                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EDUP/EXPOS
C     Called by: EXPOS
C     Calls: NONE  
C     Common blocks referenced: RADIN
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     ELTT       S     CHAR*2  NUCNAM    Element symbol for each chain member
C     AWT        S     CHAR*2  NUCNAM    Atomic weight for each chain member
C     RNAMES     U     CHAR*12 Arg       Radionuclide names for each member
C     NPRG       U     INT     Arg       Number of progeny
C==== Modification History ===================================================
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C   30-Dec-96      DLS  Initial programming started
C   25 May 2001    BAN  Adjustement to the looping over chain length (1 less)
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE SETELAW(RNAMES,NPRG)
C      
C---- Variable Type Declarations ---------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'NUCNAM.CMN'
C     LOGICAL
      INTEGER NPRG
      CHARACTER*12 RNAMES(LENCHAIN)
C
C---- Data Statements --------------------------------------------------------
C
C
C---- Start of Analysis
C  
      NRADS = NPRG
C NOTE: In this instance, NPRG is the chain length, not the # of progeny
      DO IN = 1,NRADS
        IF(RNAMES(IN)(2:2).LE.'9') THEN    ! One-character element name
          ELT(IN)(1:1) = RNAMES(IN)(1:1)   ! Set element name
          ELT(IN)(2:2) = ' '
          AW(IN)(1:6) = RNAMES(IN)(2:7)    ! Set atomic weight
        ELSE
          ELT(IN) = RNAMES(IN)(1:2)        ! Set element name
          AW(IN)(1:6) = RNAMES(IN)(3:8)    ! Set atomic weight
        ENDIF
      END DO
      RETURN
C----- END OF MODULE SETELAW ------------------------------------------------
      END

