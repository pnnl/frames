C   HEALTH:SETFAC.FOR                      Version Date: 23-Sep-09             
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                     FUNCTION   SETFAC                                      *
C                                                                            *
C  Function SETFAC determines the risk conversion factor for a path a        *
C           pollutant type                                                   *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    16-Jun-98      BA Napier                                *
C  Last Modified:    23 sep 09      BAN                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: HEALTH 
C     Called by: RISKCALC
C     Calls: NONE         
C     Common blocks referenced: DEVICE, OPT, DFACTR
C
C==== Significant Parameter Designation and Description ======================
C
C Parameter Set/
C   Name    Used Type Location  Parameter Description
C  ------  ----- ---- --------  -------------------------------------
C  KTYPE     U   Int  Argument  index of pollutant type (1-5)
C  INTAK     U   Int  Argument  route index (1-4)           
C  OTYPE     U   INT  Argument  index of endpoint type to calculate
C  RTYPE     U   INT  Argument  radionuclide input type (1 - Bq, 2 - Sv)
C==== Modification History ===================================================
C     Date    Who  Modification Description
C  ---------  ---  ------------------------------------------------------
C  16-Jun-98  BAN  Initial conversion for GENII/HEALTH module
C  5-JUN-01   BAN  Added SOILT to external dose factors
C  23 Sept 09 BAN  Added the divide-by-two to the DIMR boating terms
C==== SUBROUTINE CALL ========================================================
C
	REAL FUNCTION SETFAC()
C
C---- Include Statements for Parameter and Common Declarations ---------------
C      
	INCLUDE 'SETF.CMN'
	INCLUDE 'DFACTR.CMN'
	INCLUDE 'OPT.CMN'
	INCLUDE 'ALLPAR.CMN'
C
C---- Data Statements --------------------------------------------------------
C
      DATA C8766/8766./   !   Conversion factor, hours per year
      DATA C1EM3/1.E-3/   !   Conversion factor, kg/g
C
C----- Initialize factor to zero ---------------------------------------------
C
      FAC = 0.0
	SOILD = SLDN
	IF (SOILD .LE. 0.0) SOILD=1500.
C
C----- Evaluate for radionuclides, KTYPE = 1 ---------------------------------
C
      IF(KTYPE.LE.1) THEN           ! Radionuclides(ONLY in GENII)
C
C----- Set health effects conversion factor for requested output, OTYPE ------
C
       HEFAC = 0.01             ! Sv/rem
       IF(IHEAST.NE.1.OR.INTAK.EQ.3) THEN  ! use ICRP radiation dose factors
C                                           ! and health effects converion 
C                                           ! factors if requested and for Dermal
C                                           ! exposure pathways
        IF(OTYPE.EQ.1) THEN     
          HEFAC = HECONINC/100
        ELSEIF(OTYPE.EQ.2) THEN
          HEFAC = HECONFAT/100
        ELSEIF(OTYPE.EQ.3) THEN
          HEFAC = HECONFSH/100
        ELSE
          HEFAC = 0.01            ! Sv/rem
        ENDIF
C
C----- Set radionuclide input type conversion factor, pCi/Bq for intake ------
C
        PCIFAC = 1.0
        IF(RTYPE.EQ.1) PCIFAC = 27.027
C
C----- For input as intake (Bq), use dose conversion factors to get dose in rem 
C
        IF(RTYPE.EQ.1) THEN
C
C----- Set dose conversion factor --------------------------------------------
C
            IF(INTAK.EQ.1) THEN
               FAC = DFG 
            ELSE IF(INTAK.EQ.2) THEN
               FAC = DFA 
            ELSE IF (INTAK.EQ.3) THEN
               FAC = DFS
            ELSE                      ! Set external dose factors
               IF(IPATH.EQ.14) THEN
                  FAC = DIMR * C8766          ! Swimming external
               ELSEIF(IPATH.EQ.15) THEN
                  FAC = DIMR * C8766 / 2.     ! Boating external
               ELSEIF(IPATH.EQ.16) THEN
                  FAC = DSH  * C8766 * SOILD * soilt  ! Shoreline external
               ELSEIF(IPATH.EQ.3) THEN
                  FAC = DSH  * C8766 * SOILD * soilt ! Soil external
               ELSEIF(IPATH.EQ.1) THEN
                  FAC = DEX  * C8766          ! Air external, semi-infinite plume
               ENDIF
               FAC = FAC * TEXP
            ENDIF
        ELSE
            FAC = 100.0
        ENDIF
C
C----- End of seting factors for ICRP methods, now do for HEAST slope factors
C
       ELSE
         IF(RTYPE.EQ.1) THEN  ! Only can do slope factor method for Bq intake
            HEFAC = 1.0
            PCIFAC = 27.027
            IF(INTAK.EQ.1) THEN
               FAC = CPFG
            ELSE IF(INTAK.EQ.2) THEN
               FAC = CPFH
            ELSE IF (INTAK.EQ.3) THEN
               FAC = CPFG / FONE
            ELSE                      ! Set external dose factors
               IF(IPATH.EQ.16) THEN
                  FAC = CPFS * C1EM3   ! Shorline external
               ELSEIF(IPATH.EQ.3) THEN
                  FAC = CPFS * C1EM3   ! Soil external
               ENDIF
               FAC = FAC * TEXP
            ENDIF
         ELSE
            FAC = 0.0
         ENDIF
         IF(OTYPE.EQ.4) THEN
            HEFAC = 0.01            ! Sv/rem
            PCIFAC = 1.0
            IF(RTYPE.EQ.1) PCIFAC = 27.027
C
C----- Set dose conversion factor --------------------------------------------
C
               IF(INTAK.EQ.1) THEN
                  FAC = DFG 
               ELSE IF(INTAK.EQ.2) THEN
                  FAC = DFA 
               ELSE IF (INTAK.EQ.3) THEN
                  FAC = DFS
               ELSE                      ! Set external dose factors
                  IF(IPATH.EQ.14) THEN
                     FAC = DIMR * C8766          ! Swimming external
                  ELSEIF(IPATH.EQ.15) THEN
                     FAC = DIMR * C8766 / 2.0    ! Boating external
                  ELSEIF(IPATH.EQ.16) THEN
                     FAC = DSH  * C8766 * SOILD * soilt ! Shoreline external
                  ELSEIF(IPATH.EQ.3) THEN
                     FAC = DSH  * C8766 * SOILD * soilt ! Soil external
                  ELSEIF(IPATH.EQ.1) THEN
                     FAC = DEX  * C8766          ! Air external, semi-infinite plume
                  ENDIF
                  FAC = FAC * TEXP
               ENDIF
         ENDIF
C
       ENDIF
        FAC = FAC * HEFAC * PCIFAC
C
C---- End of analysis for radionuclides
C
      ENDIF
	
C
      SETFAC = FAC
      RETURN
C
C
C----- END OF MODULE SETFAC -------------------------------------------------
C
      END

