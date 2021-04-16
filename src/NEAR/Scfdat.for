C  SCFDAT.FOR   NEAR                 Version Date: 20-Feb-98               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE SCFDAT                                *
C                                                                            *
C  Subroutine SCFDAT reads data for a radionuclide chain from the SCF file   *
C             Soil concentrations are read for each soil compartment needed  *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    31-Aug-97                                               *
C  Last Modified:    20-Feb-98      BAN                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EDUP/NEAR 
C     Called by: NEAR 
C     Calls: NONE  
C     Common blocks referenced:      
C
C==== Significant Parameter Designation and Description ======================
C
C  Parameter Set/
C  Name      Used  Type    Location  Parameter Description
C  --------- ----- ------  --------- -------------------------------------
C  SSOIL(20)  S    REAL    Argument  Surface soil concentration values at the 
C                                    start of the analysis, for each chain
C                                    member
C  DSOIL(20)  S    REAL    Argument  Deep soil concentration values at the 
C                                    start of the analysis, for each chain
C                                    member
C  WPCON(20)  S    REAL    Argument  Waste package concentration values at the 
C                                    start of the analysis, for each chain
C                                    member
C  RNAMES(20) S    CHAR    Argument  Names of radionuclide chain members
C                                    read from the WCF file
C
C==== Modification History ===================================================
C   Date      Who  Modification Description
C  ---------  ---  ------------------------------------------------------
C  31-Aug-97  DLS  Initial programming started
C  19-Jan-98  DLS  Added units conversion from Bq to pCi on input soil conc.
C  20-Feb-98  BAN  Adjusted units on WPCON to Bq/kg
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE SCFDAT(CONID,NUMCON,NUMNUC,STYPE)
C
C---- Include statements -----------------------------------------------------
C
      INCLUDE 'CONIN.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'SOLPAR.CMN'
      INCLUDE 'TIMES.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      CHARACTER*8 TU,CONU
      CHARACTER*12 RNAMES(9)
      CHARACTER*12 CONID(100)
      CHARACTER*20 CNAMIN
      REAL REL(100),TIME(100)
      INTEGER STYPE
      LOGICAL SKIP
C
C---- Data Statements --------------------------------------------------------
C
C
C---- Start of Analysis
C
C----- Loop on number of parent pollutants in SCF data set -------------------
C
      DO IPAR = 1,NUMNUC
C
C---- Read name of radionuclide/number of progeny/ number of time points -----
C
      SKIP = .FALSE.
      READ(NSCF,*) CNAMIN,RNAMES(1),TU,CONU,NTIN,NPROG
      TU = TU
      CONU = CONU
      CNAMIN = CNAMIN
C
C----- Identify input parent name against master list to know index for ------
C      saving information for this data set.
C
      IMST = 0
      DO IN = 1,NUMCON
         IF(CONID(IN).EQ.RNAMES(1)) THEN
            IMST = IN
            GO TO 10
         ENDIF
      END DO
        WRITE(NELS,1010) RNAMES(1)
 1010   FORMAT(' Error in parent name, not in GID master list. ',
     .         ' Name is ',A12)
C     NERROR = NERROR + 1
C     RETURN
      SKIP = .TRUE.
 10   CONTINUE
c
c---- Test value for number of time periods/write error message --------------
c
      IF(NTIN.LT.1.OR.NTIN.GT.100) THEN
        WRITE(NERR,100) RNAMES(1),NTIN
 100    FORMAT(' Error in SCF data input: incorrect number of time '
     .    'data points for ',A12/'    Value should be between 1 and ',
     .    '100.  Value found: ',I4)
        GO TO 999
      ENDIF
C
C----- Read time/soil concentration data pairs ------------------------------
c
      DO IT = 1,NTIN
        READ(NSCF,*) TIME(IT),REL(IT)
        REL(IT) = REL(IT) * BQPCI
      END DO
      TIME(1) = TIME(1)
c
c---- Evaluate compartment concentrations for first time point ---------------
C
        IF(.NOT.SKIP) THEN
          IF(STYPE.EQ.1) THEN
             SSOIL(1,IMST) = REL(1)
          ELSE IF(STYPE.EQ.2) THEN
             DSOIL(1,IMST) = REL(1)
          ELSE IF(STYPE.EQ.3) THEN
             WPCON(1,IMST) = REL(1) 
          ELSE
            WRITE(NERR,5000) STYPE
 5000       FORMAT(' Error reading SCF data: bad soil type')
            NERROR = NERROR + 1
            RETURN
          ENDIF
        ENDIF
C
C----- If progeny, then read data for progeny
C----- Read name of progeny/ number of time points ---------------------------
C
      DO IP = 1,NPROG
        IN = IP + 1
        READ(NSCF,*) CNAMIN,RNAMES(IP+1),TU,CONU,NTIN
C
c---- Test value for number of time periods/write error message --------------
c
        IF(NTIN.LT.1.OR.NTIN.GT.100) THEN
          WRITE(NERR,100) RNAMES(ip+1),NTIN
          GO TO 999
        ENDIF
C
C----- Read time/soil concentration data pairs -------------------------------
c
        DO IT = 1,NTIN
          READ(NSCF,*) TIME(IT),REL(IT)
          REL(IT) = REL(IT) * BQPCI
        END DO
c
c---- Evaluate  concentration for each type necessary -----------
C
        IF(.NOT.SKIP) THEN
          IF(STYPE.EQ.1) THEN
             SSOIL(IN,IMST) = REL(1)
          ELSE IF(STYPE.EQ.2) THEN
             DSOIL(IN,IMST) = REL(1)
          ELSE IF(STYPE.EQ.3) THEN
             WPCON(IN,IMST) = REL(1)
          ELSE
            WRITE(NERR,5000) STYPE
            NERROR = NERROR + 1
            RETURN
          ENDIF
        ENDIF
C
C----   End loop on number of progeny for current parent, if any -------------
C
      END DO
C
C-----  End loop on parent pollutants in SCF data set ------------------------
C
      END DO
C
 999  RETURN
C----- END OF MODULE sCFDAT -------------------------------------------------
      END

