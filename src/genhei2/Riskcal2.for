C RISKCAL2.FOR                      Version Date: 28-OCT-98               
C   Copyright 1998 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE RISKCAL2                              *
C                                                                            *
C  Subroutine RISKCAL2 tests exposure pathway name and route, and if found,  *
C       calculates risks from the intake amounts                             *
C                                                                            *
C                                                                            *
C  Written by:       BA Napier                                               *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    28-OCT-98  BA NAPIER                                    *
C  Last Modified:    28-OCT-98  BAN                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: GENII      
C     Called by: NEW  
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
C  TPATH       U    INT   Argument  Transport pathway index  (1-GW, 2-SW,
C                                   3-Air, 5-meas. soil, 6-meas. food
C  NPOINTS     U    INT   Argument  Number of location (points) to evaluate
C  OTYPE       U    INT   Argument  Output type to be evaluated
C  VALIN       U    REAL  Argument  Input "intakes" for each point
C  VALOUT2     S    REAL  Argument  Calculated output results for each point
C  FOUNDE      S    LOG   Argument  Flag to indicate exposure pathway was found
C==== Modification History ===================================================
C    Date     Who  Modification Description
C  ---------  ---  ------------------------------------------------------
C  28-OCT-98  BAN  Initial programming started
C  5-JUN-01   BAN  Trap soil .NE. ground external (MEPAS variant)
C  26 jULY 2002 BAN  replaced .eq. with SEQI in 3 places
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE RISKCAL2(EXNAME,EXROUTE,TPATH,NPOINTS,
     .                    VALIN,VALOUT2,FOUNDE,indxage,INDX2)
C
C---- Include statements -----------------------------------------------------
c
      INCLUDE 'DEVICE.CMN'
	INCLUDE 'EXPNAM.CMN'
	INCLUDE 'SETF.CMN'
	INCLUDE 'ALLPAR.CMN'
	INCLUDE 'PAKPARM.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      REAL VALIN(1681), VALOUT2(25,1681), FAC2(24)
      LOGICAL FOUNDE, SEQI
      INTEGER NPOINTS,TPATH
      CHARACTER*12  EXROUTE, ROUTYP(4)
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
      FOUNDE = .FALSE.
	ICHK = 0
      DO IP = 1,29
C         IF(EXNAME.EQ.EXPLAB(IP)) THEN
         IF(EXNAME .EQ. EXPLAB(IP)) ICHK=1
         IF(SEQI(EXNAME,EXPLAB(IP),3)) THEN
C            IF(EXROUTE.EQ.EXPRUT(IP)) THEN
            IF(SEQI(EXROUTE,EXPRUT(IP),3)) THEN
                IPATH = IP
            ENDIF
         ENDIF
      END DO
C
      IF(IPATH .EQ. 29) IPATH = 3
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
C
C---- ESTABLISH ORGAN/CANCER SITE DIMENSIONS
C
	IF(OTYPE .EQ. 1 .OR. OTYPE .EQ. 2) THEN
	  ICOUNT = MCAN
	ELSE
	  ICOUNT = MORG
	ENDIF
C
C---- If pathway found, set pathway cross index for SIF factors --------------
C
      IF(FOUNDE) THEN
       DO J=1,ICOUNT
         FAC2(J) = 1.0
         IF(TPATH.EQ.1) THEN   !  Set groundwater index
              CALL SETFAC2(INDXAGE, INDX2,J,FAC)
	        FAC2(J) = FAC
         ELSE IF(TPATH.EQ.2) THEN   ! Set surface water index
              CALL SETFAC2(INDXAGE, INDX2,J,FAC)
	        FAC2(J) = FAC
         ELSE IF(TPATH.EQ.3) THEN   ! Set atmospheric index
              CALL SETFAC2(INDXAGE, INDX2,J,FAC)
	        FAC2(J) = FAC
         ELSE IF(TPATH.EQ.5) THEN   ! Set measured soil index
              CALL SETFAC2(INDXAGE, INDX2,J,FAC)
	        FAC2(J) = FAC
         ENDIF
        END DO
      ELSE
        GO TO 999
      ENDIF
C
C---- Calculate health inpacts for current constituent type and pathway -----
C
      IF(.NOT.FOUNDE) THEN
        DO IPN = 1,NPOINTS
          DO J=1,ICOUNT
           VALOUT2(J,IPN) = 0.0
          END DO
        END DO
      ELSE
        DO IPN = 1,NPOINTS
          DO J=1,ICOUNT
           VALOUT2(J,IPN) = VALIN(IPN) * FAC2(J)
          END DO
        END DO
      ENDIF
C
 999  RETURN
C
C----- END OF MODULE RISKCAL2 -----------------------------------------------
C
      END

