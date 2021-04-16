C   PATHSET.FOR INTAKE              Version Date: 03-Mar-98               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE PATHSET                               *
C                                                                            *
C  Subroutine PATHSET calculates intake factors by pathway and age group     *
C                                                                            *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    10-Jan-97    DL Strenge                                 *
C  Last Modified:    03-Mar-98    BAN                                        *                                                                                       *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EDUP/INTAKE
C     Called by: INTAKE
C     Calls: NONE  
C     Common blocks referenced: AGES, PATHFAC, OPT
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C  Name      Used   Type    Location  Parameter Description
C  --------- -----  ------  --------- -------------------------------------
C  
C==== Modification History ===================================================
C    Date     Who  Modification Description
C  --------   ---  ------------------------------------------------------
C  10-Jun-97  DLS  Initial programming
C  05-Feb-98  DLS  Modified selected pathways for acute exposure period
C  03-Mar-98  BAN  Correction of indices for ABSFAC and PERMK
C  16-Sept-2002 BAN Added FRINH, for radon
C  2 May 2007 BAN  Expansion of acute submersion to allow > 24 hours
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE PATHSET(SETDATA,NLINES,ACUTE,TRELD)
C
C---- Include statements -----------------------------------------------------
c
      INCLUDE 'AGES.CMN'
      INCLUDE 'ALLPAR.CMN'
      INCLUDE 'DERMFAC.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'PATHFAC.CMN'
      INCLUDE 'RADATA.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      CHARACTER*80 SETDATA(1000)
      INTEGER IZ, NLINES
      LOGICAL ACUTE
C
C---- Data Statements --------------------------------------------------------
C
      DATA IZ/0/
C
C---- Get radionuclide specific parameter values -----------------------------
C
      DO IP = 1,NUMCON
        PK   =  GETREAL(SETDATA,NLINES,'PERMK         ',
     .                 IP,IZ,IZ,IZ,IZ,IZ)
        ABSF = GETREAL(SETDATA,NLINES,'ABSFAC        ',
     .                 IP,IZ,IZ,IZ,IZ,IZ)
        IF(PK.LE.0.) PK = PKDFLT
        IF(ABSF.LE.0.) ABSF = ABSDFLT
        PERMK(IP,1)  = PK
        ABSFAC(IP,1) = ABSF
C
C------- Extract dermal factors for progeny radionuclides, if any ------------
C
        IF(NDS(IP).GT.0) THEN
C
           DO IPR = 1,NDS(IP)
              PK   = GETREAL(SETDATA,NLINES,'PERMK         ',
     .                     IP,IPR,IZ,IZ,IZ,IZ)
              ABSF = GETREAL(SETDATA,NLINES,'ABSFAC        ',
     .                     IP,IPR,IZ,IZ,IZ,IZ)
              IF(PK.LE.0.) PK = PKDFLT
              IF(ABSF.LE.0.) ABSF = ABSDFLT
              PERMK(IP,IPR+1)  = PK
              ABSFAC(IP,IPR+1) = ABSF
C
           END DO   ! Loop on progeny
C
        ENDIF   ! If progeny
C
      END DO   !  Loop on parent radionuclides
C
C---- Set exposure pathway factors, independent of radionuclide --------------
C    Initialize array to zero
C
      DO IA = 1,5
        DO IX = 1,28 
          PFACTOR(IP,IA) = 0.0
        END DO
      END DO
C
C----- Evaluate each exposure pathway (1 to 28) as requested for -------------
C      this run.
C
C----- Outdoor Air/Inhalation ------------------------------------------------
C
        DO IA = 1,NAGES
          IF(ACUTE) THEN
            TXHR = TRELD                          ! Factor is volume inhalated
c            TXHR = TRELD * DAYYR                  ! Factor is volume inhalated
            PFACTOR(2,IA) = TXHR * UINH(IA) * FRINH(IA)    ! during acute period
          ELSE
            PFACTOR(2,IA) = UINH(IA) * TINH(IA) * FRINH(IA)
          ENDIF
        END DO
C           
C----- Air/External ----------------------------------------------------------
C
        DO IA = 1,NAGES
          IF(ACUTE) THEN
            TXHR = TRELD /DAYHR
c this way is only good if the release is < 24 hours
c            TX = 1.0
c            IF(TXHR.GT.UEXAIR(IA)) TX = UEXAIR(IA)/TXHR
c            PFACTOR(1,IA) =  TX
c  BAN 2 May 2007: Revise as follows:
		  IF (TXHR .LE. UEXAIR(IA)) TX = 1.0
	      IF (TXHR .GT. UEXAIR(IA)) THEN
	        IF(TXHR .LE. 24.0) THEN
	          TX = UEXAIR(IA)/TXHR
	        ELSE
	          TX = UEXAIR(IA)/24.0
	        ENDIF
	      ENDIF
	      PFACTOR(1,IA) = TX
ccccc
          ELSE
            PFACTOR(1,IA) = UEXAIR(IA) * TEXAIR(IA) /HRYR
          ENDIF
        END DO
C           
C----- Ground/external -------------------------------------------------------
C
        DO IA = 1,NAGES
          SFIO = SHIN * FTIN(IA) + SHOUT * FTOUT(IA)
          PFACTOR(3,IA) = SFIO * UEXGRD(IA) * TEXGRD(IA) /HRYR
        END DO
C
C----- Food crops/ingestion --------------------------------------------------
C
      DO ICRP = 4,7
          DO IA = 1,NAGES
            PFACTOR(ICRP,IA) = UCRP(ICRP-3,IA) * TCRP(ICRP-3,IA)
          END DO
      END DO
C
C----- Animal product/ingestion ----------------------------------------------
C
      DO IANM = 8,11
            DO IA = 1,NAGES
               PFACTOR(IANM,IA) = Uanm(IANM-7,IA) * Tanm(IANM-7,IA)
            END DO
      END DO
C
C----- Soil/ingestion --------------------------------------------------------
C
          DO IA = 1,NAGES
             PFACTOR(12,IA) = USOIL(IA) * TSOIL(IA) * KGMG
          END DO
C
C----- Swimming/ingestion ----------------------------------------------------
C
          DO IA = 1,NAGES
            IF(ACUTE) THEN
              HRSWIM = EVSWIM(IA) * TESWIM(IA)
              TXHR = TRELD /DAYHR
C             TXHR = TRELD * HRYR
              IF(TXHR.GT.HRSWIM) TXHR = HRSWIM 
              PFACTOR(13,IA) = USWIM(IA) * TXHR
            ELSE
              PFACTOR(13,IA) = USWIM(IA) * TESWIM(IA) * TSWIM(IA)
     .                        * EVSWIM(IA) 
            ENDIF
          END DO
C
C----- Swimming/external -----------------------------------------------------
C
          DO IA = 1,NAGES
            IF(ACUTE) THEN
              HRSWIM = EVSWIM(IA) * TESWIM(IA)
              TXHR = TRELD /DAYHR
C             TXHR = TRELD * HRYR
              TX = 1.0
              IF(TXHR.GT.HRSWIM) TX = HRSWIM / TXHR
              PFACTOR(14,IA) = TX
            ELSE
              PFACTOR(14,IA) = TESWIM(IA)*TSWIM(IA)*EVSWIM(IA)/HRYR
            ENDIF
          END DO
C
C----- Boating/external ------------------------------------------------------
C
          DO IA = 1,NAGES
            IF(ACUTE) THEN
              HRBOAT = EVBOAT(IA) * TEBOAT(IA)
              TXHR = TRELD /DAYHR
C             TXHR = TRELD * HRYR
              TX = 1.0
              IF(TXHR.GT.HRBOAT) TX = HRBOAT  / TXHR
              PFACTOR(15,IA) = SFBOAT * TX
            ELSE
              PFACTOR(15,IA) = SFBOAT * TEBOAT(IA) * TBOAT(IA)
     .                       * EVBOAT(IA) / HRYR
            ENDIF
          END DO
C
C----- Shoreline/external ----------------------------------------------------
C
          DO IA = 1,NAGES
              PFACTOR(16,IA) = SWFAC * TESHOR(IA) * TSHOR(IA)
     .                        * EVSHOR(IA) / HRYR
          END DO
C
C----- Water/ingestion -------------------------------------------------------
C
          DO IA = 1,NAGES
            IF(ACUTE) THEN
              TXDAY = TRELD                      ! days in release period
C             TXDAY = TRELD * DAYYR              ! days in release period
              PFACTOR(17,IA) =  UDW(IA) * TXDAY
            ELSE
              PFACTOR(17,IA) = UDW(IA) * TDW(IA)
            ENDIF
          END DO
C
C----- Fish/ingestion --------------------------------------------------------
C
      DO IAQU = 18,21
            DO IA = 1,NAGES
               PFACTOR(IAQU,IA) = UAQU(IAQU-17,IA) * TAQU(IAQU-17,IA)
            END DO
      END DO
C
C----- Indoor air/inhalation -------------------------------------------------
C
        DO IA = 1,NAGES
          IF(ACUTE) THEN
            TXDAY = TRELD             
C           TXDAY = TRELD * DAYYR
            PFACTOR(22,IA) = UINDRH(IA) * FRINDR(IA) * TXDAY
          ELSE
            PFACTOR(22,IA) = UINDRH(IA) * TINDRH(IA) *FRINDR(IA)
          ENDIF
        END DO
C
C----- Showering/dermal ------------------------------------------------------
C
        DO IA = 1,NAGES
          IF(ACUTE) THEN
            HRSHWR =TESHWR(IA) * EVSHWR(IA)
            TXHR = TRELD /DAYHR
C           TXHR = TRELD * HRYR
            IF(TXHR.GT.HRSHWR) TXHR = HRSHWR
            PFACTOR(23,IA) = TXHR * ASKINSH(IA) * LML
          ELSE
            PFACTOR(23,IA) = TESHWR(IA) * ASKINSH(IA) * EVSHWR(IA)
     .                     * TSHWR(IA) * LML
          ENDIF
        END DO
C
C----- Showering/ingestion ---------------------------------------------------
C
        DO IA = 1,NAGES
          IF(ACUTE) THEN
            HRSHWR =TESHWR(IA) * EVSHWR(IA)
            TXHR = TRELD  /DAYHR
C           TXHR = TRELD * HRYR
            IF(TXHR.GT.HRSHWR) TXHR = HRSHWR
            PFACTOR(24,IA) = TXHR * USHIN(IA)
          ELSE
            PFACTOR(24,IA) = TESHWR(IA) * USHIN(IA) * EVSHWR(IA)
     .                     * TSHWR(IA)
          ENDIF
        END DO
C
C----- Soil/dermal -----------------------------------------------------------
C
        DO IA = 1,NAGES
          PFACTOR(25,IA) = ADHSOL(IA) * ASKINSL(IA) * EVSOIL(IA)
     .                     * TSOILD(IA) * KGMG
        END DO
C
C----- Swimming/dermal -------------------------------------------------------
C
        DO IA = 1,NAGES
          IF(ACUTE) THEN
            HRSWIM =TESWIM(IA) * EVSWIM(IA)
            TXHR = TRELD  /DAYHR
C           TXHR = TRELD * HRYR
            IF(TXHR.GT.HRSWIM) TXHR = HRSWIM
            PFACTOR(26,IA) = TXHR * ASKINSW(IA) * LML
          ELSE
            PFACTOR(26,IA) = TESWIM(IA) * ASKINSW(IA) * EVSWIM(IA)
     .                     * TSWIM(IA) * LML
          ENDIF
        END DO
C
C----- Shoreline/dermal ------------------------------------------------------
C
        DO IA = 1,NAGES
          PFACTOR(27,IA) = ADHSED(IA) * ASKINSD(IA) * EVSHOR(IA)
     .                     * TSHOR(IA) * KGMG
        END DO
C
C----- Soil/inhalation -------------------------------------------------------
C
        DO IA = 1,NAGES
          PFACTOR(28,IA) = UINHR(IA) * FRINHR(IA) * TINHR(IA)
        END DO
C
 999  RETURN
      END
C
C----- END OF MODULE PATHSET ------------------------------------------------
C

