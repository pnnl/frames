C RISKCALC.FOR AHAZH                Version Date: 19-Jun-97               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE RISKCALC                              *
C                                                                            *
C  Subroutine RISKCALC tests exposure pathway name and route, and if found,  *
C       calculates risks from the intake amounts                             *
C                                                                            *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    19-Jun-97  DL Strenge                                   *
C  Last Modified:    19-Jun-97  DLS                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: MEPAS/AHAZH
C     Called by: ANAZH
C     Calls: NONE  
C     Common blocks referenced: NONE
C
C==== Significant Parameter Designation and Description ======================
C
C  Parameter  Set/
C   Name      Used  Type  Location  Parameter Description
C  --------- ----- ------ --------- -------------------------------------
C  EXNAME      U    CHAR  Argument  Exposure pathway medium name
C  EXROUTE     U    CHAR  Argument  Exposure pathway route name
C  EU          U    CHAR  Argument  Exposure pathway units
C  TPATH       U    INT   Argument  Transport pathway index  (1-GW, 2-SW,
C                                   3-Air, 5-meas. soil, 6-meas. food
C  NPOINTS     U    INT   Argument  Number of location (points) to evaluate
C  OTYPE       U    INT   Argument  Output type to be evaluated
C  VALIN       U    REAL  Argument  Input "intakes" for each point
C  VALOUT      S    REAL  Argument  Calculated output results for each point
C  FOUNDE      S    LOG   Argument  Flag to indicate exposure pathway was found
C==== Modification History ===================================================
C    Date     Who  Modification Description
C  ---------  ---  ------------------------------------------------------
C  07-Jan-97  DLS  Initial programming started
C  07-Aug-97  DLS  Revised to calculate for specific endpoints, OTYPE  
C  5-JUN-01   BAN  Trap soil .NE. ground external (MEPAS variant)
C  26 july 2002 BAN Replaced .eq. with SEQI in 3 places
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE RISKCALC(EXNAME,EXROUTE,EU,TPATH,NPOINTS,
     .                    VALIN,VALOUT,FOUNDE)
C
C---- Include statements -----------------------------------------------------
c
      INCLUDE 'DEVICE.CMN'
	INCLUDE 'EXPNAM.CMN'
	INCLUDE 'DERMFAC.CMN'
	INCLUDE 'SETF.CMN'
C      INCLUDE 'PSET1.FTN'
C      INCLUDE 'PFLAGS.FTN'
C      INCLUDE 'DECAY.FTN'
C      INCLUDE 'GWPATH.FTN'
C      INCLUDE 'SWPATH.FTN'
C      INCLUDE 'SLPATH.FTN'
C      INCLUDE 'ATPATH.FTN'
C      INCLUDE 'FDPATH.FTN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      REAL VALIN(1681), VALOUT(1681)
      LOGICAL FOUNDE, SEQI
      INTEGER NPOINTS,TPATH
      CHARACTER*7 EU
      CHARACTER*12  EXROUTE, ROUTYP(4)
c      CHARACTER*20 EXNAME
      CHARACTER*22  EXNAME
C
C---- Data Statements --------------------------------------------------------
C
      DATA ROUTYP/'ingestion   ',
     .            'inhalation  ',
     .            'dermal      ',
     .            'external    '/
C
C---- Find exposure name in list of known pathways ---------------------------
C
      ICHK = 0
      FOUNDE = .FALSE.
      DO IP = 1,29
C         IF(EXNAME.EQ.EXPLAB(IP)) THEN
          IF(EXNAME .EQ. EXPLAB(IP)) ICHK = 1 
         IF(SEQI(EXNAME,EXPLAB(IP),3)) THEN
C            IF(EXROUTE.EQ.EXPRUT(IP)) THEN
            IF(SEQI(EXROUTE,EXPRUT(IP),3)) THEN
                IPATH = IP
            ENDIF
         ENDIF
      END DO
C
      IF(IPATH .EQ. 29)IPATH = 3
C
C----- Find exposure route in list of known routes ---------------------------
C
      DO IP = 1,4
C        IF(EXROUTE.EQ.ROUTYP(IP)) THEN
        IF(SEQI(EXROUTE,ROUTYP(IP),3)) THEN
           FOUNDE = .TRUE.
           INTAK = IP
        ENDIF
      END DO
      KTYPE = 1
C
C---- If pathway found, set pathway cross index for SIF factors --------------
C
      IF(FOUNDE) THEN
         FAC = 1.0
         IF(TPATH.EQ.1) THEN   !  Set groundwater index
C               FAC = SETFAC(KTYPE,INTAKE,IPATH,OTYPE,RTYPE)
              FAC = SETFAC()
         ELSE IF(TPATH.EQ.2) THEN   ! Set surface water index
C              FAC = SETFAC(KTYPE,INTAKE,IPATH,OTYPE,RTYPE)
              FAC = SETFAC()
         ELSE IF(TPATH.EQ.3) THEN   ! Set atmospheric index
              FAC = SETFAC()
C              FAC = SETFAC(KTYPE,INTAKE,IPATH,OTYPE,RTYPE)
         ELSE IF(TPATH.EQ.5) THEN   ! Set measured soil index
              FAC = SETFAC()
C              FAC = SETFAC(KTYPE,INTAKE,IPATH,OTYPE,RTYPE)
         ENDIF
      ELSE
        GO TO 999
      ENDIF
C
C---- Calculate health inpacts for current constituent type and pathway -----
C
      IF(.NOT.FOUNDE) THEN
        DO IPN = 1,NPOINTS
           VALOUT(IPN) = 0.0
        END DO
      ELSE
        DO IPN = 1,NPOINTS
           VALOUT(IPN) = VALIN(IPN) * FAC
        END DO
      ENDIF
C
 999  RETURN
C
C----- END OF MODULE RISKCALC -----------------------------------------------
C
      END

