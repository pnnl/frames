C  SETELAW.FOR ACUTE                Version Date: 29-Jul-97               
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
C  Last Modified:    29-Jul-97      DLS                                      *
C
C==== Modular Organization ===================================================
C
C     Module of: EDUP/ACUTE
C     Called by: ACUTE
C     Calls: NONE  
C     Common blocks referenced: RADIN
C
C==== Significant Parameter Designation and Description ======================
C     Name      Use    Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     ELTT       S     CHAR*2  RADIN     Element symbol for each chain member
C     AWT        S     CHAR*2  RADIN     Atomic weight for each chain member
C     RNAMES     U     CHAR*12 Arg       Radionuclide names for each member
C     NPRG       U     INT     Arg       Number of progeny
C==== Modification History ===================================================
C    Date     Who  Modification Description
C  ---------  ---  -----------------------------------------------------------
C  30-Dec-96  DLS  Initial programming started
C  29-Jul-97  DLS  Heading and comments revised for use with ACUTE
C  25 May 2001 BAN NRADS set to NPRG, rather than NPRG+1; chain length
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE SETELAW(RNAMES,NPRG)
C      
C---- Include Statements -----------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'NUCNAM.CMN'
C      
C---- Variable Type Declarations ---------------------------------------------
C
      INTEGER NPRG
      CHARACTER*12 RNAMES(9)
C
C---- Start of Analysis ------------------------------------------------------
C  
      NRADS = NPRG
C Note: In this instance, NPRG is the chain length, not the # of progeny
      DO IN = 1,NRADS
        IF(RNAMES(IN)(2:2).LE.'9') THEN     ! One-character element name
          ELT(IN)(1:1) = RNAMES(IN)(1:1)    ! Set element name
          ELT(IN)(2:2) = ' '
          AW(IN)(1:6) = RNAMES(IN)(2:7)     ! Set atomic weight
        ELSE
          ELT(IN) = RNAMES(IN)(1:2)         ! Set element name
          AW(IN)(1:6) = RNAMES(IN)(3:8)     ! Set atomic weight
        ENDIF
      END DO
      RETURN
      END
C
C----- END OF MODULE SETELAW ------------------------------------------------
C
