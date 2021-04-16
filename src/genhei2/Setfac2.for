C   HEALTH:SETFAC2.FOR                      Version Date: 16-July-2003
C   Copyright 1998 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                 SUBROUTINE SETFAC2                                         *
C                                                                            *
C  Subroutine SETFAC2 determines the risk conversion factor for a path a     *
C           pollutant type                                                   *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    09-OCT-98      BA Napier                                *
C  Last Modified:    16-Jul-03      BAN                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: HEALTH 
C     Called by: RISKCAL2
C     Calls: NONE         
C     Common blocks referenced: SETF, OPT, FACTORS
C
C==== Significant Parameter Designation and Description ======================
C
C Parameter Set/
C   Name    Used Type Location  Parameter Description
C  ------  ----- ---- --------  -------------------------------------
C  J             INT  ARGUEMENT ORGAN/CANCER SITE INDEX
C  INDXAGE       INT  ARGUEMENT AGE GROUP INDEX (1-6)
C  INDX2         INT  ARGUEMENT RISK AGE GROU INDEX (1-5)
C  INTAK     U   Int  COMMON    route index (1,2,4)        
C  OTYPE     U   INT  COMMON    index of endpoint type to calculate (1,2,4)
C  RTYPE     U   INT  COMMON    radionuclide input type (1 - Bq, 2 - Sv)
C==== Modification History ===================================================
C     Date    Who  Modification Description
C  ---------  ---  ------------------------------------------------------
C  28-Jun-98  BAN  Initial conversion for GENII/HEALTH module
C   3-N0v-98  BAN  Addition of risk factors
C   5-NOV-98  BAN  ADDITION OF SURFACE WATER IMMERSION
C   5-JUN-01  BAN  Added SOILT to external dose factors
C   8-May-03  BAN  Added SOILT to external risk factors, too!
C   16-July-2003 BAN  Made risk factor selection for swimming water ingestion parallel
C==== SUBROUTINE CALL ========================================================
C
        SUBROUTINE SETFAC2(INDXAGE,INDX2,J,FAC)
C
C---- Include Statements for Parameter and Common Declarations ---------------
C      
	INCLUDE 'SETF.CMN'
	INCLUDE 'OPT.CMN'
      INCLUDE 'FACTORS.CMN'
	include 'allpar.cmn'
C
C---- Data Statements --------------------------------------------------------
C
      DATA C8766/3.15E7/   !   Conversion factor, SECONDS per year
C
C----- Initialize factor to zero ---------------------------------------------
C
      FAC = 0.0
	HEFAC=1.0
	SOILD = SLDN
	IF (SOILD .LE. 0.0) SOILD=1500.
C
	  IF (OTYPE .EQ. 4) THEN
C
C----- For input as intake (Bq), use dose conversion factors to get dose in Sv
C
        IF(RTYPE.EQ.1) THEN
C
C----- Set dose conversion factor --------------------------------------------
C
            IF(INTAK.EQ.1) THEN
               FAC = DFGSUM(INDXAGE,J)
            ELSE IF(INTAK.EQ.2) THEN
               FAC = DFHSUM(INDXAGE,J)
            ELSE                      ! Set external dose factors
C----NOTE: HERE AND BELOW, FACTOR OF 1000 IS UNIT CONVERSION, L/M3---
               IF(IPATH.EQ.14) THEN
                  FAC = DFWSUM(J) * C8766 * 1000.0        ! Swimming external
               ELSEIF(IPATH.EQ.15) THEN
                  FAC = DFWSUM(J) * C8766 * 1000.0 /2.0   ! Boating external
               ELSEIF(IPATH.EQ.16) THEN
                  FAC = DFXSUM(2,J)  * C8766 * SOILD * soilt  ! Shoreline external
               ELSEIF(IPATH.EQ.3) THEN
                  FAC = DFXSUM(2,J)  * C8766 * SOILD * soilt ! Soil external
               ELSEIF(IPATH.EQ.1) THEN
                  FAC = DFXSUM(1,J)  * C8766          ! Air external, semi-infinite plume
               ENDIF
               FAC = FAC * TEXP
            ENDIF
        ELSE
C           FAC = 100.0  *** CHANGED BAN 4/14/2002 Sv not Rem! For submersion dose only
            FAC = 1.0
        ENDIF

	 END IF

C
	 IF (OTYPE .EQ.2) THEN
CC
C----- For input as intake (Bq), use FATALITY FACTORS
C
        IF(RTYPE.EQ.1) THEN
C
C----- Set INTERNAL RISK factor --------------------------------------------
C
            IF(INTAK.EQ.1) THEN
	        IF ((IPATH .EQ. 17) .OR. (IPATH .EQ. 13)) THEN  !DRINKING WATER
               FAC = RFGSUM(INDX2,J,1,1)
	        ELSE 
	         FAC = RFGSUM(INDX2,J,1,2)     ! FOODS
	        ENDIF
            ELSE IF(INTAK.EQ.2) THEN
               FAC = RFHSUM(INDX2,J,1)
            ELSE                      ! Set external dose factors
	         CALL XREF(J,KK)
               IF(IPATH.EQ.14) THEN
                  FAC = DFWSUM(KK) * HECONFAT * C8766 *1000.0 ! Swimming external
               ELSEIF(IPATH.EQ.15) THEN
                  FAC = DFWSUM(KK) * HECONFAT*C8766*1000.0 /2.0 ! Boating external
               ELSEIF(IPATH.EQ.16) THEN
                  FAC = RFXSUM(indx2,J,2,1)  * C8766 * SOILD * soilt ! Shoreline external
               ELSEIF(IPATH.EQ.3) THEN
                  FAC = RFXSUM(indx2,J,2,1)  * C8766 * SOILD * soilt ! Soil external
               ELSEIF(IPATH.EQ.1) THEN
                  FAC = RFXSUM(indx2,J,1,1)  * C8766          ! Air external, semi-infinite plume
               ENDIF
               FAC = FAC * TEXP
            ENDIF
        ELSE
            FAC = HECONFAT
        ENDIF
	 END IF
C
	 IF (OTYPE .EQ.1) THEN
CC
C----- For input as intake (Bq), use INCIDENCE FACTORS
C
        IF(RTYPE.EQ.1) THEN
C
C----- Set INTERNAL RISK factor --------------------------------------------
C
            IF(INTAK.EQ.1) THEN
	        IF ((IPATH .EQ. 17) .OR. (IPATH .EQ. 13)) THEN  !drinking and swimming ingestion
C			IF (IPATH .EQ. 17) THEN                         ! BAN 16 July 2003
               FAC = RFGSUM(INDX2,J,2,1)     !DRINKING WATER
	        ELSE
	         FAC = RFGSUM(INDX2,J,2,2)      ! FOODS
	        ENDIF
            ELSE IF(INTAK.EQ.2) THEN
               FAC = RFHSUM(INDX2,J,2)
            ELSE                      ! Set external dose factors
	         CALL XREF(J,KK)
               IF(IPATH.EQ.14) THEN
                  FAC = DFWSUM(KK) * HECONINC * C8766 * 1000.0 ! Swimming external
               ELSEIF(IPATH.EQ.15) THEN
                  FAC = DFWSUM(KK) * HECONINC * C8766*1000.0 /2.0   ! Boating external
               ELSEIF(IPATH.EQ.16) THEN
                  FAC = RFXSUM(indx2,J,2,2)  * C8766 * SOILD * soilt ! Shoreline external
               ELSEIF(IPATH.EQ.3) THEN
                  FAC = RFXSUM(indx2,J,2,2)  * C8766 * SOILD * soilt ! Soil external
               ELSEIF(IPATH.EQ.1) THEN
                  FAC = RFXSUM(indx2,J,1,2)  * C8766          ! Air external, semi-infinite plume
               ENDIF
               FAC = FAC * TEXP
            ENDIF
        ELSE
            FAC = HECONINC
        ENDIF

	END IF

C
C---- End of analysis for radionuclides
C
C
      RETURN
C
C
C----- END OF MODULE SETFAC2 -------------------------------------------------
C
      END


