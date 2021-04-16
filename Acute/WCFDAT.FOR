C  WCFDAT.FOR   ACUTE                Version Date: 29-Jul-97               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE WCFDAT                                *
C                                                                            *
C  Subroutine WCFDAT reads data for a radionuclide chain from the WCF file   *
C             Water concentrations are read and interpolated for each year   *
C             of interest for the analysis                                   *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    26-Dec-96                                               *
C  Last Modified:    29-Jul-97      DLS                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EDUP/EXPOS
C     Called by: EXPOS
C     Calls: AVGCON
C     Common blocks referenced: TIMES
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     WATC(100,9) S    REAL    Argument  Water concentration values for each
C                                        year for the analysis by chain member
C     RNAMES(9)   S    CHAR    Argument  Names of radionuclide chain members
C                                        read from the WCF file
C
C==== Modification History ===================================================
C    Date     Who  Modification Description
C  ---------  ---  ------------------------------------------------------
C  26-Dec-96  DLS  Initial programming started
C  29-Jul-97  DLS  Headings and comments modified for use with ACUTE
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE WCFDAT(RNAMES,NPROG)
C
C---- Include statements -----------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'CONIN.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'TIMES.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      CHARACTER*8 TU,CONU
      CHARACTER*12 RNAMES(CHAINLEN)
      CHARACTER*20 CNAMIN
      REAL REL(100),TIME(100)
      REAL MLL,BQPI,UNT
C
C---- Data Statements --------------------------------------------------------
C
      DATA MLL/1000./, BQPI/0.037/
C
C---- Start of Analysis
C
      UNT = MLL * BQPI
C
C---- Determine number of time periods for analysis --------------------------
C
      NT = INT(NTKEND)
      NTIMES = NT + 1   ! These are time periods in addition to acute period
      IF(NTIMES.GT.100) THEN
        WRITE(NERR,1000) NTIMES
 1000   FORMAT(' Error in definition of number of time period for ',
     .         'waterborne conenctration input.'/
     .         '   Maximum value is 100, but input requires:',i5)
        NERROR = NERROR + 1
        RETURN
      ENDIF
C
C---- Read name of radionuclide/number of progeny/ number of time points -----
C
      READ(NWCF,*) CNAMIN,RNAMES(1),TU,CONU,NTIN,NPROG
      TU = TU
      CONU = CONU
      CNAMIN = CNAMIN
C
C---- Test value for number of time periods/write error message --------------
C
      IF(NTIN.LT.1.OR.NTIN.GT.100) THEN
        WRITE(NERR,100) RNAMES(1),NTIN
 100    FORMAT(' Error in WCF data input: incorrect number of time '
     .    'data points for ',A12/'    Value should be between 1 and ',
     .    '100.  Value found: ',I4)
        NERROR = NERROR + 1
        GO TO 999
      ENDIF
C
C----- Read time/water concentration data pairs ------------------------------
c
      DO IT = 1,NTIN
        READ(NWCF,*) TIME(IT),REL(IT)
      END DO
C
C---- Evaluate average water concentration for each year necessary -----------
C
      TSTRT1 = TIME(1)
      TEND1 = TIME(NTIN)
C      TREL = 1.
      INUC = 1
      T1 = BEFORE
      START1 = T1
C
C----- Evaluate concentration for acute release period -----------------------
C
         TA = ACUTIM
         WATC(1,INUC) = UNT*TA*AVGCON(START1,TA,TSTRT1,TEND1,REL,TIME)
         IF(NERROR.GT.0) GO TO 999
C
      IF(NTIMES.GE.2) THEN
       START1 = START1 + ACUTIM
       DO IT = 2,NTIMES
c        START = START1 + FLOAT(IT-1)
        WATC(IT,INUC)= 0.0
C        WATC(IT,INUC)=UNT*TREL*AVGCON(START,TREL,TSTRT1,TEND1,REL,TIME)
       END DO
      ENDIF
C
C----- If progeny, then read data for progeny
C----- Read name of progeny/ number of time points ---------------------------
C
      DO IP = 1,NPROG
        READ(NWCF,*) CNAMIN,RNAMES(IP+1),TU,CONU,NTIN
C
c---- Test value for number of time periods/write error message --------------
c
        IF(NTIN.LT.1.OR.NTIN.GT.100) THEN
          WRITE(NERR,100) RNAMES(ip+1),NTIN
          NERROR = NERROR + 1
          GO TO 999
        ENDIF
C
C----- Read time/water concentration data pairs ------------------------------
c
        DO IT = 1,NTIN
          READ(NWCF,*) TIME(IT),REL(IT)
        END DO
C
c---- Evaluate average water concentration for each year necessary -----------
C
C        TREL = 1.
        TSTRT1 = TIME(1)
        TEND1 =  TIME(NTIN)
        T1 = BEFORE
        TA = ACUTIM
        START1 = T1
        WATC(1,IP+1) = UNT*TA*AVGCON(START1,TA,TSTRT1,TEND1,REL,TIME)
        IF(NERROR.GT.0) GO TO 999
        IF(NTIMES.GE.2) THEN
        START1 = START1 + ACUTIM
         DO IT = 2,NTIMES
c          START = START1 + FLOAT(IT-1)
          WATC(IT,IP+1)=0.0
C          WATC(IT,IP+1)=UNT*TREL*AVGCON(START,TREL,TSTRT1,TEND1,REL,TIME)
         END DO
        ENDIF
      END DO
 999  RETURN
      END
C
C----- END OF MODULE WCFDAT -------------------------------------------------
C

