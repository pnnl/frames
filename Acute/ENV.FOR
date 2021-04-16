C---------------------------------------Version: 19-Mar-98 -------------------
C
CProgram title:                    ENV - A module of the GENII
C                                      environmental dosimetry software
C                                      package.  
C
C----- Converted to operate as a subroutine for FRAMES application -----------
C
C
C    Prepared for:                     U. S. Department of Energy under
C                                      Contract DE-AC06-76RLO 1830
C
C
C    Contact:                          Bruce Napier
C                                      Pacific Northwest Laboratory
C                                      Richland WA 99352
C                                      (509) 375-3916
C
C    Code developers:                  B. A. Napier, R. A. Peloquin,
C                                      D. L. Strenge, and J. V. Ramsdell
C                                      Pacific Northwest Laboratory
C                                      Richland WA  99352
C                                  
C
C    This code was prepared for an agency of the United States
C    Government.  Neither the United States Government or any agency
C    thereof, of any of their employees, make any warranty, expressed
C    or implied, or assumes any legal liability or responsibility for 
C    any third party's use, or the results of such use, of any portion 
C    of this program or represents that its use by such third party
C    would not infringe privately onwed rights.
C
C
C--- Program Information -----------------------------------------------
C
C    Problem description:       To perform environmental transport and 
C                               exposure calculations for individuals
C                               or population groups, acute,
C                               as required for 
C				far-field short-term scenarios.
C
C    Results format:            The output of ENV is data transferred
C                               called ENV.OUT used as input to the following
C                               programs of the package.  A separate 
C                               report is prepared describing the media
C                               concentrations calculated.
C
C    Computer:                  This version is for a PC.
C
C    Programming language:      FORTRAN compiled using Lahey(tm)
C                               FORTRAN Compiler.
C
C    Machine requirements:      IBM PC/AT with a minimum of 640 Kbytes
C                               random access memory and 1 20-Mbyte fixed
C                               disk drive.
C                              
C    Reference:                 B. A. Napier, R. A. Peloquin, D. L. Strenge,
C                               and J. V. Ramsdell. 1988. "GENII - The Hanford
C                               Environmental Radiation Dosimetry Software
C                               System."  PNL-6584, Volumes 1, 2, and 3.
C                               Pacific Northwest Laboratory, Richland, WA
C                               99352
C
C
C----------------------------------------------------------------------------
C
C     This module controls calculation of exposure media concentrations.
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Initial GENII version: 19-Aug-88  RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C     Modified for EDUP/GENII ACUTE component last: 23-Aug-97
C-----------------------------------------------------------------------
C
C     IC        - Index of the current radionuclide chain
C     IP        - Index of the current exposure pathway
C     INCHAN()  - Number of radionuclides in each chain
C     QAIRAV()  - Air term saved for other seasons of acute release
C     QSWSV()   - Surface water release term saved for other seasons
C                 of acute release
C
C----  Modification histor ---------------------------------------------------
C   Date      Who  Description of modifications
C  ---------  ---  ------------------------------------------------------------
C  23-Aug-97  DLS  Added TPATH to call list.  Conversion from CHRONIC version.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE ENV(TPATH)
C
C----- Include Statements ----------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'AFLAGS.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'NUCNAM.CMN'
      INCLUDE 'SOLPAR.CMN'
      INCLUDE 'TIMES.CMN'
C
C----- Type/dimension statements ---------------------------------------------
C
      INTEGER TPATH
C      REAL QAIRSV(9), QSWSV(9)
C      REAL TYR, QAIRSV(9), QSWSV(9)
      LOGICAL SAVBUG
C
C----- Get system date and time, read file names -----------------------------
C
C     Open temporary files for acute case--
c
C      CALL OPNFIL (8,0,7)
C      CALL OPNFIL (8,0,49)
C      CALL OPNFIL (8,0,50)
C
C----- Initialize arrays for currnet chain -----------------------------------
C
        NERROR = NERROR
        CALL INITNV
        SAVBUG = DEBUG
c
C------ Acute calculation ----------------------------------------------------
C         Clear flags-- 
C
          DOAIR = .TRUE.
          DOIRR = .TRUE.
          DEBUG = SAVBUG
          NTIME = INT (NTKEND)
            ITIME = 1
            ITIMT = 1
            TYR = 0.0
C
C----- Write heading line for this time period to temporary file -------------
C      (atmospheric pathway only)
C
            IF(TPATH.EQ.3)WRITE(NATP,1000)NTIME,ITIME,TYR,APTYPE,CMEM
 1000       FORMAT(2I3,F8.1,2I3)
C
C----- Set media concentration for acute release period ----------------------
C
      ISET = 1
      CALL SETCON(ISET,TPATH)
C
C----- Call ACUTE1 to calculate .....for acute exposure period
C
            CALL ACUTE1 ()
C
C----- Call EXPOSR to calculate ....for acute exposure period
C
            CALL EXPOSR (TPATH)
C
C---- Loop over chronic intake times (each year) following acute exposure ----
C
            DO 214 ITIME = 2, NTIME
C
C-----------  Increment over-all time index ----------------------------------
C
              ITIMT = ITIMT + 1
              TYR = FLOAT (ITIME-1)
C
C----- Write heading line for this time period to temporary file -------------
C      (atmospheric pathway only)
C
            IF(TPATH.EQ.3)WRITE(NATP,1000)NTIME,ITIME,TYR,APTYPE,CMEM
              IF (DEBUG) WRITE (*,1) ITIME, ITIMT,
     .                         (ELT(IN),AW(IN), IN=1,NONUC)
              IF (ITIME .GT. 2) DEBUG = .FALSE.
C
C----- Set media concentration values for current time period ----------------
C
              CALL SETCON(ITIME,TPATH)
C         
C-----        Initialize exposures -------------------------------------------
C
              DO 312 IN = 1, NONUC          
                DO 314 IP = 1, NPATH
                  EXPOS(IN,IP) = 0.0
  314           CONTINUE              
  312         CONTINUE
C
C-------      Calculate transport --------------------------------------------
C
              CALL TRNSPT () 
C
C-----        Calculate exposure ---------------------------------------------
C
              CALL EXPOSR (TPATH)
  214       CONTINUE
C
  100 CONTINUE
C
      RETURN
C
C---- Format Statements ------------------------------------------------------
C
    1 FORMAT (' Release Year:',I4, ' Over-all year:',I4, 4X,
     .        9(A2,A6,1X)) 
    2 FORMAT (' Yr: ',I3, 2X, 9(A2,A6, 1X))
      END
C
C---- End of Module ENV ------------------------------------------------------
C
