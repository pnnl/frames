C-------------------------------------- Version: 26-Feb-98 ------------------
C
C    Subroutine ENV: A module of the GENII environmental dosimetry software
C                    package.  
C
C    Prepared for:   U. S. Department of Energy under
C                    Contract DE-AC06-76RLO 1830
C
C    Contact:          Bruce Napier
C                      Pacific Northwest National Laboratory
C                      Richland WA 99352
C                      (509) 375-3916
C
C    Code developers:  B. A. Napier, R. A. Peloquin,
C                      D. L. Strenge, and J. V. Ramsdell
C                      Pacific Northwest Laboratory
C                      Richland WA  99352
C                                  
C    This code was prepared for an agency of the United States
C    Government.  Neither the United States Government or any agency
C    thereof, of any of their employees, make any warranty, expressed
C    or implied, or assumes any legal liability or responsibility for 
C    any third party's use, or the results of such use, of any portion 
C    of this program or represents that its use by such third party
C    would not infringe privately onwed rights.
C
C--- Program Information -----------------------------------------------
C
C    This subroutine controls calculation of exposure medium concentrations
C    for near-field scenarios.
C
C==== Modification History ===================================================
C   Date         Who  Modification Description
C  --------     ---  ------------------------------------------------------
C  12-Nov-97  DLS  Revised call to BIOCAL to allow control of pre-intake
C                  biotic transport
C  26-Feb-98  BAN  If BURWAS, set DEEP = .TRUE.
C----------------------------------------------------------------------------
C
      SUBROUTINE ENV(NMST)
C
C----------------------------------------------------------------------------
C
C
C     Initial GENII Version: 19-Aug-88  RAP
C     Initial Reviewed and Approved: 12-Sept-88  BA Napier
C     Last modification:  13-Nov-97
C-----------------------------------------------------------------------
C
C   NONUC   - number of chain members in current chain
C
C---- Include Statements -----------------------------------------------
C
      INCLUDE 'CONC.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'DECAY.CMN'
      INCLUDE 'SOLPAR.CMN'
      INCLUDE 'TIMES.CMN'
C
C---- Type statements -----------------------------------------------
C
      INTEGER  YRTOGO
C      REAL TYR
      LOGICAL SAVBUG
C
C----- Initialize chain parameters and read a radionuclide chain data set-----
C
      NONUC = NUC
      CALL INITNV
      SAVBUG = DEBUG
C
C----- Call SETCON to set concentration in each soil model compartment -------
C
      CALL SETCON(NMST)
C
C-------- Chronic exposure ---------------------------------------------------
C         Do transport pre-calculations 
C
          IF (BURWAS) DEEP = .TRUE.
          IPRE = 0
          IF (BIOT .OR. DEEP .OR. BURWAS) CALL BIOCAL(IPRE)
C
C-----    Determine number of years of prior decay/building ------------------
C
          NBEFORE = INT (BEFORE)
          IF(NBEFORE.GT.0) THEN
C
C-----    For each year of the decay/buildup period --------------------------
C
          DO 102 ITIME = 1, NBEFORE
C
C-----      Increment over-all time index, years-to-go -----------------------
C
            ITIMT = ITIMT + 1
            YRTOGO = NBEFORE - ITIME + 1
            IF (ITIME .GT. 3) DEBUG = .FALSE. 
            IF (DEBUG) WRITE (*,2) ITIME, (ELT(IN),AW(IN),IN=1,NONUC)
C
C-----      Clear skip flags if calculations are to be performed--
C
            DOBIOT = .FALSE.
            DOWAST = .FALSE.
            IF (BIOT .AND. BTPRE .AND. LOIC .GE. YRTOGO) DOBIOT=.TRUE.
C  Added by BAN 
            IF (BIOT .AND. (BTDSET .LT. 3)) NONAG = .TRUE.
            IF (BEFORE .GE. YRTOGO) DOWAST = .TRUE.
C
            CALL TRNSPT (.FALSE.)
C
  102     CONTINUE
          ENDIF
C
C-----    Land use change from nonag to ag -----------------------------------
C
          CALL PRIOR
C
C-----    Clear non-agricultual and skip flags -------------------------------
C
          DOBIOT = .TRUE.
          DOWAST = .TRUE.
          NONAG = .FALSE.
          DEBUG = SAVBUG
          NTIME = INT (NTKEND)
          IPRE = 1
          EXCAMT = 0.0
          IF(BIOT.AND.(DEEP.OR.BURWAS)) CALL BIOCAL(IPRE)
          DO 200 ITIME = 1, NTIME
C
C-----      Increment over-all time index ------------------------------------
C
            ITIMT = ITIMT + 1
C            TYR = FLOAT (ITIME)
            IF (DEBUG) WRITE (*,1) ITIME, ITIMT,
     .                       (ELT(IN),AW(IN), IN=1,NONUC)
            IF (ITIME .GT. 2) DEBUG = .FALSE.
C
C-----      Initialize exposure ----------------------------------------------
C
            DO 316 IN = 1, NONUC          
              DO 318 IP = 1, NPATH
                EXPOS(IN,IP) = 0.0
  318         CONTINUE              
  316       CONTINUE
C
C-----      Calculate transport ----------------------------------------------
C
            CALL TRNSPT (.TRUE.)
C
C-----      Calculate exposure -----------------------------------------------
C
            CALL EXPOSR ()
C
  200     CONTINUE
C

  100 CONTINUE
      RETURN
C
C---- Format Statements -----------------------------------------------

    1 FORMAT (' Release Year:',I4, ' Over-all year:',I4, 4X,
     .        9(A2,A6,1X)) 
    2 FORMAT (' Yr: ',I3, 2X, 9(A2,A6, 1X))

      END
C
C----- End of Module ENV -----------------------------------------------------
C
