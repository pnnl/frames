C   EPFDAT.FOR EXPOS                Version Date: 22-May-97               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE EPFDAT                                *
C                                                                            *
C  Subroutine EPFDAT  writes data for a parent(&progeny) for each time period*
C       and each exposure pathway                                            *
C                                                                            *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    07-Jan-97                                               *
C  Last Modified:    22-May-97      DLS                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EDUP/EXPOS
C     Called by: EXPOS
C     Calls: NONE  
C     Common blocks referenced: NONE
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     LNAMES(9)   U   CHAR*12    ARG     Name of each chain member
C     NUMNUC      U    INT       ARG     Number of chain members in current chain
C     TPATH       U    INT       ARG     Transport path index (1-GW, 2-SW, 3-air
C                                        4-Soil
C     NPOINTS     U    INT       ARG     Number of location points for data output
c                                        For GW, SW, and Soil = 1.  For air = 1
C                                        for MI, = grid points for population.
C==== Modification History ===================================================
C     Date     Who  Modification Description
C     -------- ---  ------------------------------------------------------
C   07-Jan-97  DLS  Initial programming started
C   15-May-97  DLS  Added TPATH and NPOINTS to argument list, revised output
C                   to be consistent with current EPF format.
C   22-May-97  DLS  Moved exposure pathway names/units to common EXPNAM
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE EPFDAT(NIC,LNAMES,NUMNUC)
C
C---- Include statements -----------------------------------------------------
c
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'EXPALL.CMN'
      INCLUDE 'EXINFO.CMN'
      INCLUDE 'EXPNAM.CMN'
      INCLUDE 'AIRINFO.CMN'
      INCLUDE 'FLUX.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'TIMES.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      REAL EXPVAL(20)
      INTEGER NUMNUC
      CHARACTER*12 LNAMES(9),EXPROUT(20)
      CHARACTER*20 EXPLABL(20)
      CHARACTER*6 EXPUNIT(20)
      INTEGER NLAB(20),NROUT(20),NPUN(20)
C
C---- Data Statements --------------------------------------------------------
C
      DATA EMIN/1.E-25/
C
C---- Write coordinate data -------------------------------------------------
C
        IF(NIC.EQ.1) THEN
          WRITE(NEPF,400) EXPX,EXPY    ! for all except air.  One point
 400      FORMAT(F9.1,',"km",',F9.1,',"km",')
        ENDIF
C
C---- Write line of: parent name, progeny, time periods -------
C
      NL = LENWORD(LNAMES(1),12)
      WRITE(NEPF,100) LNAMES(1)(1:NL), LNAMES(1)(1:NL), NUMNUC-1, NTIME
 100  FORMAT('"',A,'","',A,'",',I2,',',I3,',')
c
c----- For each time period, write parent media concentrations, the progeny --
c      media concentrations for all exposure pathways with positive conc.
C
      DO IT = 1,NTIME
        START = BEFORE + FLOAT(IT-1)
        TREL = 1.
        DO IC = 1,NUMNUC
          NC = LENWORD(LNAMES(IC),12)
          IOUT = 0
          DO IP = 1,28
C
C----- Set values for output for selected pathways and non-zero concentrations-
C
            IF(PATH(IP).AND.EXPOST(IT,IC,IP).GT.EMIN) THEN
              IOUT = IOUT + 1
              EXPLABL(IOUT) = EXPLAB(IP)
              NLAB(IOUT)    = NEXPLAB(IP)
              EXPROUT(IOUT) = EXPRUT(IP)
              NROUT(IOUT)   = NEXPRUT(IP)
              EXPUNIT(IOUT) = EXPUN(IP)
              NPUN(IOUT)    = NEXPUN(IP)
              EXPVAL(IOUT) = EXPOST(IT,IC,IP)
            ENDIF
          END DO
c
C----- Write concentrations for parent at each time --------------------------
c
            IF(IC.EQ.1) THEN
              WRITE(NEPF,300) START,TREL,IOUT
 300   FORMAT(F5.1,',"yr",',F5.1,',"yr",',I3,',')
            ELSE
              WRITE(NEPF,100) LNAMES(IC)(1:NC),LNAMES(IC)(1:NC),IOUT
            ENDIF
            IF(IOUT.GT.0) THEN  ! write results
              DO IO = 1,IOUT
                WRITE(NEPF,200) EXPLABL(IO)(1:NLAB(IO)),
     .                          EXPROUT(IO)(1:NROUT(IO)),
     .                          EXPUNIT(IO)(1:NPUN(IO))
                WRITE(NEPF,201) EXPVAL(IO)
 200   FORMAT('"',A,'","',A,'","',A,'"')
 201   FORMAT(1PE10.3,',')
              END DO
            END IF
        END DO  ! End of loop on chain members
      END DO    ! End of loop on time periods
      RETURN
C
C----- END OF MODULE EPFDAT -------------------------------------------------
C
      END

