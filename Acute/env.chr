C  ENV.FOR      EXPOS                Version Date: 29-May-97               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE ENV                                   *
C
C    Problem description:  To perform environmental transport and 
C                          exposure calculations for individuals
C                          or population groups, acute or chronic,
C                          as required for near-field and 
C                          far-field short-term scenarios.
C                                                                            *
C    Code developers:  B. A. Napier, R. A. Peloquin,
C                      D. L. Strenge, and J. V. Ramsdell
C                      Pacific Northwest Laboratory
C                      Richland WA  99352
C
C     Previously main PROGRAM ENV of GENII program
C     Converted to subroutine for EDUP program (Dec 1996)
C     This module controls reading of the input files, initializing
C     parameters, and indexing of the radionuclide chain and time step 
C     loops.
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     GENII Master Version: 19-Aug-88  RAP                                   *
C     Creation Date:    20-Dec-96                                            *
C     Last Modified:    29-May-97      DLS                                   *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EDUP/EXPOS
C     Called by: EXPOS
C     Calls: 
C     Common blocks referenced: 
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C
C     IC        - Index of the current radionuclide chain
C     IP        - Index of the current exposure pathway
C
C==== Modification History ===================================================
C     Date     Who  Modification Description
C   ---------  ---  ------------------------------------------------------
C   20-Dec-96  DLS  Initial programming started
C   27-May-97  DLS  Eliminated unnecessary programming for deep soil
C   29-May-97  DLS  Redirected screen output to NELS file
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE ENV(TPATH)
C
C----- Include Statements ----------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'CONC.CMN'
      INCLUDE 'DECAY.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'NUCNAM.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'SOLPAR.CMN'
      INCLUDE 'TIMES.CMN'
      INCLUDE 'AFLAGS.CMN'
c
      INTEGER  IBEFOR,  TPATH
      REAL TYR
      LOGICAL SAVBUG
C
C---- Open output file -----
c
c      LUN = 6
c      CALL OPNFIL (2,0,LUN)
C      CALL RITQA
c      CLOSE(6)
C
C----- Open outputs file and write heading information -----------------------
C
C----- Initialize chain parameters -------------------------------------------
c
        NONUC = NUC
        CALL INITNV
        SAVBUG = DEBUG
c
C-------- Chronic exposure ---------------------------------------------
C         Do transport pre-calculations--
c
        NBEFORE = 0
        ISET = 1
        IF(TPATH.EQ.3) CALL SETCON(ISET,TPATH)
        IF(DOPRIOR) THEN
          ITIMT = 1
c
C----- Determine number of years of prior decay/building--
c
          IBEFOR = INT (BEFORE)
          IF(TPATH.EQ.3.AND.AIR) CALL AIRCAL
c
C----- Non-agricultural conditions considered only for biotic transport--
c
          NBEFORE = 0
          IF (TPATH.EQ.3.AND.BEFAIR.GT. 0) THEN
            NBEFORE = BEFAIR
            DOAIR = .TRUE.
          ENDIF
          IF (TPATH.LE.2.AND.BEFIRR .GT. 0) THEN
            NBEFORE = BEFIRR
            DOIRR = .TRUE.
          ENDIF
          IF (TPATH.EQ.4) THEN
            NBEFORE = IBEFOR
          ENDIF
C
C----- Set concentrations period prior to intake for deposition/decay --------
c
        ISET = 1
        CALL SETCON(ISET,TPATH)
c
C----- For each year of the decay/buildup period--
c
          DO 102 ITIME = 1, NBEFORE 
c
C----- Increment over-all time index, years-to-go--
c
            ITIMT = ITIMT + 1
            IF (ITIME .GT. 3) DEBUG = .FALSE. 
            IF (DEBUG) WRITE (NELS,2) ITIME, (ELT(IN),AW(IN),IN=1,NONUC)
C
C----- Set concentrations period prior to intake for deposition/decay --------
c
             IF(TPATH.NE.3) THEN
               ISET = ITIMT 
               CALL SETCON(ISET,TPATH)
             ENDIF
c
            CALL TRNSPT (.FALSE.,TPATH)
c
  102     CONTINUE 
C
        END IF
c
C----- Clear non-agricultural and skip flags ---------------------------------
c
          DOAIR = .TRUE.
          DOIRR = .TRUE.
          DEBUG = SAVBUG
          NTIME = INT (NTKEND)
          ITIMT = 0
          IF(NBEFORE.EQ.0) THEN
            ITIMT = 0
          ELSE
            ITIMT = NBEFORE
          ENDIF
          DO 200 ITIME = 1, NTIME
c
C----- Increment over-all time index--
c
            ITIMT = ITIMT + 1
            TYR = FLOAT (ITIME) + BEFORE - 1.
            ISET = ITIMT 
C
C----- Write heading line for this time period to temporary file -------------
C      (atmospheric pathway only)
C
            IF(TPATH.EQ.3)WRITE(NATP,1000)NTIME,ITIME,TYR,APTYPE,CMEM
 1000       FORMAT(2I3,F8.1,2I3)
C
C---- If start of current period is at or after the end of release then ------
C     concentrations are zero.  Otherwise call SETCON to get concentrations.
C
            IF(TPATH.NE.3) THEN
              IF(TYR.LT.RELEND) THEN
                CALL SETCON(ISET,TPATH)
              ENDIF
            ENDIF
            IF (DEBUG) WRITE (NELS,1) ITIME, ITIMT,
     .                       (ELT(IN),AW(IN), IN=1,NONUC)
            IF (ITIME .GT. 2) DEBUG = .FALSE.
C          
            IF (TYR .GE. RELEND) THEN
C
              IF (DOGWX) THEN
C
C----- Ground water release has ended--
C
                DO IN = 1, NONUC          
                  GWCON(IN) = 0.0
                END DO  
              ENDIF
              IF (DOSWX) THEN
C
C----- Surface water release has ended--
C
                DO IN = 1, NONUC          
                  SWCON(IN) = 0.0
                END DO
              ENDIF
C
              IF (DOAIRX) THEN
C
C----- Air release has ended--
C
                DO IN = 1, NONUC
                  ARPCON(IN) = 0.0
                  WETDEP(IN) = 0.0
                  DRYDEP(IN) = 0.0
                  EXTD(IN)   = 0.0
                  DO ITF = 1, NTF
                    ARFCON(IN,ITF) = 0.0
                  END DO  
                  DO IAN = 1, NAN+2
                    ARF2CN(IN,IAN) = 0.0
                  END DO  
                END DO  
              ENDIF
            ENDIF
c
C----- Initialize exposures--
c
            DO IN = 1, NONUC          
              DO IP = 1, NPATH
                EXPOS(IN,IP) = 0.0
              END DO                
            END DO  
C
C----- Calculate transport--
C
            CALL TRNSPT (.TRUE.,TPATH)
C
C----- Calculate exposure--
C
            CALL EXPOSR (TYR,tpath,.TRUE.)
C
  200     CONTINUE
C
      DEBUG = SAVBUG
      RETURN
c
C---- Format Statements -----------------------------------------------
c
    1 FORMAT (' Release Year:',I4, ' Over-all year:',I4, 4X,
     .        9(A2,A6,1X)) 
    2 FORMAT (' Yr: ',I3, 2X, 9(A2,A6, 1X))
C
C---- END OF MODULE ENV ------------------------------------------------
C
      END

