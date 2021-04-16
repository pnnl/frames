C   EPFRIF.FOR INTAKE               Version Date: 6-May-98               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE EPFRIF                                *
C                                                                            *
C  Subroutine EPFRIF reads radionuclide concentrations, calculates intakes,  *
C                    and writes results to RIF file                          *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    10-Jan-97                                               *
C  Last Modified:    10/11/01  BAN                                           *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EDUP/INTAKE
C     Called by: INTAKE
C     Calls: INTAKER
C     Common blocks referenced: NONE
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     RNAMES     S    Char*    Radionuclide chain member names
C     NPRG       S      Int    Number of progeny for current parent radionuclide
C     NPOINTS    S      Int    Number of x,y locations for receptor
C==== Modification History ===================================================
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C   10-Jun-97      DLS  Initial programming from CHRONIC EPFDSET
C   6-May-98       BAN  Updated to 41x41
C   10-Oct-01      BAN  Eliminated variable population; set to 1.0
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE EPFRIF(SETDATA,NLINES,NPRG,NPOINTS,RNAME,IAGE,ACUTE)
C
C---- Include statements -----------------------------------------------------
c
	include 'parmtr.par'
	INCLUDE 'PATHFAC.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'AGES.CMN'
      INCLUDE 'OPT.CMN'
	INCLUDE 'RADON.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
C     LOGICAL
      REAL EXPCON(1681), EXPINT(1681,5)
      CHARACTER*80 SETDATA(linemax)
      INTEGER NPRG,NPOINTS,NPTHX
      CHARACTER*12 PNAME, RNAME
      CHARACTER*20  EPATH
      CHARACTER*12 EROUTE
      CHARACTER*6 C,C1,EXPUN
      CHARACTER*7 EXPUNIT
      CHARACTER*14 ETYPOUT
C
C---- Data Statements --------------------------------------------------------
C     NONE
C
C---- Read first line of time data set: -------------------------------------
C
        READ(NEPF,*) TSTART,C,TREL,C1,NEXPTH
C
C---- Call PATHSET to set path factors for exposure pathways and time TREL ---
C
C
        TRELD = TREL*DAYYR
        CALL PATHSET(SETDATA,NLINES,ACUTE,TRELD)
        NC = LENWORD(C,6)
        NC1 = LENWORD(C1,6)
        WRITE(NRIF,300) TSTART,C(1:NC),TREL,C1(1:NC1),NEXPTH
 300    FORMAT(1PE9.2,',"',A,'",',E9.2,',"',A,'",',I3,',')
C
C---- Identify parent radionuclide in master list from GID
C
        IPAR = 0
        CALL RADTEST(RNAME,PNAME,IPAR,NRAD,NPROG)
        IF(NRAD.LE.0) THEN
          WRITE(NERR,500) RNAME
 500      FORMAT(' Error in parent name in EPF: not in GID: ',A)
          NERROR = NERROR + 1
          GO TO 999
        END IF
C
C------ Loop on number of exposure pathways ---------------------------------
C
      DO IE = 1,NEXPTH
C
C-------- Read exposure medium concentrations for all points ----------------
C
        READ(NEPF,*) EPATH,EROUTE,EXPUN
        READ(NEPF,*) (EXPCON(I),I=1,NPOINTS)
        CALL EXPTEST(EPATH,EROUTE,EXPUN,EXPUNIT,NPTHX,ETYPOUT)
        IF(NPTHX.GT.0) THEN
C
C-------- Call INTAKER to calculate intake for each point and age group
C
         CALL INTAKER(NPTHX,NRAD,NPROG,EXPCON,EXPINT,NPOINTS,IAGE,RNAME)
C
C-------- Write results to RIF file for this exposure pathway ----------------
C
          NP = LENWORD(EPATH,20)
          NR = LENWORD(EROUTE,12)
          NX = LENWORD(EXPUNIT,7)
          NT = LENWORD(ETYPOUT,14)
C-------- Populations should not go here, so just set place holder------------
		POPAGE = 1.0
C          DO IA = 1,NAGES
            WRITE(NRIF,100) POPAGE,EPATH(1:NP),EROUTE(1:NR),
     .                      EXPUNIT(1:NX),ETYPOUT(1:NT)
 100        FORMAT(F10.0,',"',A,'","',A,'","',A,'","',A,'"')
            WRITE(NRIF,200) (EXPINT(I,IAGE),I=1,NPOINTS)
 200        FORMAT(1PE9.2,',',1680(E9.2,','))
C          END DO
C
        END IF  ! If on NPTHX found
C
      END DO  !  End of loop on exposure pathways, IE
C
C---- If there are progeny, read and process data ----------------------------
C
      IF(NPRG.GT.0) THEN
C
C------ Loop on number of progeny for current parent -------------------------
C
        IPAR = NPRG
        DO IP = 1,NPRG
C
C------- Read name of progeny and number of exposure pathways ----------------
C
          READ(NEPF,*) PNAME,PNAME,NEXPTH
          NPG = LENWORD(PNAME,12)
          WRITE(NRIF,400) PNAME(1:NPG),PNAME(1:NPG),NEXPTH
 400      FORMAT('"',A,'","',A,'",',I3,',')
C
C---- Identify progeny radionuclide in master list from GID
C

        CALL RADTEST(RNAME,PNAME,IPAR,NRAD,NPROG)
        IF(NRAD.LE.0) THEN
          WRITE(NERR,501) PNAME
 501      FORMAT(' Error in progeny name in EPF: not in GID: ',A)
          NERROR = NERROR + 1
          GO TO 999
        END IF
C
C------ Loop on number of exposure pathways ---------------------------------
C
          DO IE = 1,NEXPTH
C
C-------- Read exposure medium concentrations for all points ----------------
C
            READ(NEPF,*) EPATH,EROUTE,EXPUN
            READ(NEPF,*) (EXPCON(I),I=1,NPOINTS)
            CALL EXPTEST(EPATH,EROUTE,EXPUN,EXPUNIT,NPTHX,ETYPOUT)
            IF(NPTHX.GT.0) THEN
C
C-------- Call INTAKER to calculate intake for each point and age group
C
         CALL INTAKER(NPTHX,NRAD,NPROG,EXPCON,EXPINT,NPOINTS,IAGE,PNAME)
C
C-------- Write results to RIF file for this exposure pathway ----------------
C
              NP = LENWORD(EPATH,20)
              NR = LENWORD(EROUTE,12)
              NX = LENWORD(EXPUNIT,7)
              NT = LENWORD(ETYPOUT,14)
C
                WRITE(NRIF,100) POPAGE,EPATH(1:NP),EROUTE(1:NR),
     .                        EXPUNIT(1:NX),ETYPOUT(1:NT)
                WRITE(NRIF,200) (EXPINT(I,IAGE),I=1,NPOINTS)
C
C
            END IF  ! If on NPTHX found
C
          END DO  !  End of loop on exposure pathways, IE

        END DO  !  End loop on progeny, IP
C
      END IF  ! If on NPRG
      EXPUN = EXPUN
C
 999  RETURN
C
C----- END OF MODULE EPFRIF -------------------------------------------------
C
      END

