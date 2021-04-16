C     MEPAS RADCON: COMPAR.FOR            Version Date: 04-25-1997
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                             SUBROUTINE COMPAR                              *
C                                                                            *
C  Subroutine COMPAR compares the source-term concentrations with simulated  *
C  concentrations to ensure the simulated concentrations are less than the   *
C  source-term concentrations.                                               *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    01/19/91 (Converted to PC)                              *
C  Last Modified:    04/25/97 - JPM                                          *
C                                                                            *
C*****************************************************************************
C 
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: RADCONPC
C     Calls to: None
C     Common blocks referenced: TIME, INDEX1, INDEX3, INDEX4, INDEX8
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/           Location
C     Name      Used  Type     Export/Import  Parameter Description
C     --------- ----- -------- -------------  --------------------------------
C     C         SET   REAL     ARG - EXPORT   CONCENTRATION OF CONTAMINANT IF 
C                                             LAST MEDIA, ELSE FLUX OF CON-   
C                                             TAMINANT                        
C     CO        USED  REAL     ARG - IMP      INITIAL CONTAMINANT CONC.       
C     CON       SET   REAL     LOCAL          CONCENTRATION OF CONTAMINANT IN 
C                                             LAST MEDIA DERIVED FROM INITIAL 
C                                             CONCENTRATION AND TAKING INTO   
C                                             ACCOUNT CONTAMINANT DECAY       
C     ISTYPE    USED  INTEGER  COMMON - IMP   INDEX ON SOURCE TERM TYPE       
C                                               = 1 OR 2; GROUNDWATER         
C                                               > 2; OVERLAND RUNOFF INVOLVED 
C     K         USED  INTEGER  COMMON - IMP   INDEX FOR NUMBER OF MEDIA       
C     M1        USED  INTEGER  ARG - IMP      INDEX ON THE CONTAMINANT-PARENT 
C     NFLAG     SET   INT      ARG - EXP      TURNED ON WHEN COMPUTED
C                                             CONCENTRATIONS EXCEED THE DECAYED
C                                             CONCENTRATION AT THE SOURCE.
C     NMED      USED  INTEGER  COMMON - IMP   TOTAL NUMBER OF MEDIA           
C     TTIM      SET   REAL     LOCAL          TIME COUNTER, FUNCTION OF IDECAY
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/24/91     JWB  Added required comments and header for MEPAS QA plan  
C     01/25/93     JWB  Modification made to reflect CAP cahnges from JLS
C     06/02/94     JPM  Added a check on ICONC to make sure that we truncate
C                       to a dissolved concentration instead of a total
C                       concentration.
C     07/12/94     JPM  Added NFLAG - turned on when concentrations exceed
C                       the decayed initial concentration at the source.
C                       Indicates in ORIGIN if the warning message should be
C                       written to the *.WLS file.
C     02/23/96     JPM  Removed the FLUXMX check. (RAD-P060)
C     04/25/97     JPM  Greatly simplified this subroutine.  This routine is
C                       only called if concentrations were computed, so only
C                       comparing the receptor concentration to the source
C                       concentration.  No longer decaying the source
C                       concentration, because this was causing problems
C                       for short-lived constituents under IDECAY = 2
C                       (see PH1-E001 for details) (PH1-E013).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE COMPAR (M1,C,CO,ICONC,NFLAG,MFLG)
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'MAXIMUM.WTN'
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION CO(MAXCON),ICONC(MAXCON),NFLAG(MAXCON)
C
C==== Variable Declarations ==================================================
C
      INTEGER*4 MFLG,ICONC,M1,NFLAG
      REAL C,CO
C
C==== DATA Statements ========================================================
C
C     None
C
      IF (C.GT.CO(M1).AND.ICONC(M1).EQ.3) THEN
        C = CO(M1)
        NFLAG(M1) = 1
        MFLG = MFLG + 1
      ENDIF
      RETURN
      END
