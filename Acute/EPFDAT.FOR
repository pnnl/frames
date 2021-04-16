C   EPFDAT.FOR EXPOS                Version Date: 28-Jan-98               
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
C  Last Modified:    28-Jan-98      DLS                                      *
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
C==== Modification History ===================================================
C     Date     Who  Modification Description
C     -------- ---  ------------------------------------------------------
C   07-Jan-97  DLS  Initial programming started
C   15-May-97  DLS  Added TPATH and NPOINTS to argument list, revised output
C                   to be consistent with current EPF format.
C   22-May-97  DLS  Moved exposure pathway names/units to common EXPNAM
C   28-Jan-98  DLS  Removed TPATH and NPOINTS from argument list.  Divided output
C                   for first period into to sets: acute, 1st year chronic.
C   5 Feb 2008 BAN  Changed start time for 2nd acute period to ACUTIM
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE EPFDAT(LNAMES,NUMNUC)
C
C---- Include statements -----------------------------------------------------
c
      INCLUDE 'PARMTR.PAR'
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
      REAL EXPVAL(28)
      INTEGER NUMNUC, NP1LST(10),NP2LST(18)
      CHARACTER*12 LNAMES(9),EXPROUT(28)
      CHARACTER*20 EXPLABL(28)
      CHARACTER*7 EXPUNIT(28)
      INTEGER NLAB(28),NROUT(28),NPUN(28)
C
C---- Data Statements --------------------------------------------------------
C
      DATA EMIN/1.E-30/
      DATA NP1LST/1,2,13,14,15,17,22,23,24,26/
      DATA NP2LST/3,4,5,6,7,8,9,10,11,12,16,18,19,20,21,25,27,28/
      DATA NP1/10/,NP2/18/
C
C---- Write line of: parent name, progeny, time periods -------
C
      NPOUT = NTIME + 1
      NL = LENWORD(LNAMES(1),12)
      WRITE(NEPF,100) LNAMES(1)(1:NL), LNAMES(1)(1:NL), NUMNUC-1, NPOUT
 100  FORMAT('"',A,'","',A,'",',I2,',',I3,',')
c
c----- For each time period, write parent media concentrations, the progeny --
c      media concentrations for all exposure pathways with positive conc.
C
      TREL = ACUTIM
      START = 0.
      NPX = NP1
      DO IT = 1,NPOUT
        IF(IT.GT.1) THEN
          START = BEFORE + FLOAT(IT-2)
	    if(IT .EQ. 2)START = ACUTIM
          TREL = 1.
          NPX = NP2
        END IF
        DO IC = 1,NUMNUC
          NC = LENWORD(LNAMES(IC),12)
          IOUT = 0
          DO IPX = 1,NPX
C
C----- Set values for output for selected pathways and non-zero concentrations-
C

            IP = NP2LST(IPX)
            ITX = IT - 1
            IF(IT.EQ.1) THEN
              ITX = 1
              IP = NP1LST(IPX)
            ENDIF
            IF(PATH(IP).AND.EXPOST(ITX,IC,IP).GT.EMIN) THEN
              IOUT = IOUT + 1
              EXPLABL(IOUT) = EXPLAB(IP)
              NLAB(IOUT)    = LENWORD(EXPLAB(IP),20)
              EXPROUT(IOUT) = EXPRUT(IP)
              NROUT(IOUT)   = LENWORD(EXPRUT(IP),12)
              EXPUNIT(IOUT) = EXPUN(IP)
              NPUN(IOUT)    = LENWORD(EXPUN(IP),7)
              EXPVAL(IOUT) = EXPOST(ITX,IC,IP)
            ENDIF
          END DO
c
C----- Write concentrations for parent at each time --------------------------
c
            IF(IC.EQ.1) THEN
              WRITE(NEPF,300) START,TREL,IOUT
 300   FORMAT(1PE10.2,',"yr",',1PE10.2,',"yr",',I3,',')
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

