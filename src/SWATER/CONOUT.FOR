C   CONOUT.FOR                        Version Date: 01-May-97                *
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.     *
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE CONOUT                                *
C                                                                            *
C  Subroutine CONOUT writes a set of concentration time data to the WCF file *
C             decay chain for release to a river                             *
C
C  Written by:       Dennis Strenge                                          *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    31-Oct-1996                                             *
C  Last Modified:    01-May-1997  DLS                                        *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: GENII/SWATER
C     Called by: SWATER
C     Calls: NONE
C     Common blocks referenced: DEVICE, SWPAR, SWINFO, SWCON
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     
C==== Modification History ===================================================
C   Date     Who  Modification Description
C ---------  ---  ------------------------------------------------------
C 01-May-97  DLS  Switch order of output of time periods and number of progeny
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE CONOUT(IL,NCON,SWOUT)
C
C==== COMMON Block Definitions ===============================================
C
        include 'DEVICE.CMN'
        INCLUDE 'OPT.CMN'
        include 'SWPAR.CMN'
        INCLUDE 'SWINFO.CMN'
        INCLUDE 'SWCON.CMN'
        INCLUDE 'DECAY.CMN'
C
C==== DIMENSION Statements ===================================================
C
      REAL SC(3),T(3)
      REAL SWOUT(10,20,9)
      CHARACTER*1 Q
C 
C==== Variable Declarations ==================================================
C
C
C==== DATA Statements ========================================================
C
      DATA Q/'"'/
C
        IF(DEBUG) WRITE(NSLS,100) IL
 100    FORMAT(' CONOUT called for IL =',I2)
C
C----  Write a heading line for the current parent constituent and location --
C
       WRITE(NWCF,1000) Q,EXLOCNAM(IL),NCON
 1000  FORMAT(A,A32,'","Surface water",',I3)
C
C----- Write water concentration as a function of time for the release -------
C      Three time points, 0.,TREL, and TREL+delta, where delta is a small
C      increament.  This provides a point estimate of a square wave, stopping
C      after TREL.
C
      DELTA = TREL*0.001
      T(1) = 0.
      T(2) = TREL
      T(3) = TREL+DELTA
      SC(3) = 0.0
      DO IC = 1,NCON
C
C----- Write line for parent radionuclide -------------------------------
C
        WRITE(NWCF,2002) Q,CNAM(1,IC),CID(1,IC),NPROG(IC)
 2002   FORMAT(A,A20,'","',A12,'","yr","pCi/ml",3,',I2)
        SC(1) = SWOUT(IL,IC,1) * PCIBQ * L3ML
        SC(2) = SC(1)
        DO IT = 1,3
          WRITE(NWCF,1001) T(IT),SC(IT)
 1001     FORMAT(1PE10.3,',',1PE10.3,',')
        END DO
C
C----- Write lines for each progeny -------------------------------------
C
        NPR = NPROG(IC)
        IF(NPR.GT.0) THEN
          DO IN = 1,NPR
            SC(1) = SWOUT(IL,IC,IN+1) * PCIBQ * L3ML
            SC(2) = SC(1)
            WRITE(NWCF,2003) Q,CNAM(IN+1,IC),CID(IN+1,IC)
 2003       FORMAT(A,A20,'","',A12,'","yr","pCi/ml",','3')
            DO IT = 1,3
              WRITE(NWCF,1001) T(IT),SC(IT)
            END DO
          END DO
        ENDIF
      END DO
C
 99   RETURN
      END
C====================== END OF MODULE CONOUT =============================
