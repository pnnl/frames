C------------------------------------ Version: 20-Feb-98 ---------------------
C
C     SUBROUTINE ANMCAL
C
C     This subroutine calculates uptake rate from comsumption of 
C     animal products. 
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Initial GENII Version: 17-Aug-88  RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C     Last Modification:     20-Feb-98  ban         
C-----------------------------------------------------------------------
C
C     Internal parameters:
C
C     ANFLAG    - option flag for each of the feed and fodder types
C     DW        - animal drinking water contribution, UoA/L
C     EDBL()    - Concentration in animals from the six feed categories
C     IBV       - index of BVI value to use, either (1) fresh, (4) stored;
C                 soil-to-plant transfer factor for feed and forage
C     LEAF()    - Total leaf concentration, each radionuclide, UoA/kg
C     LEAF1a    - Leaf concentration from air, UoA/kg 
C     LEAF1b    - Leaf concentration from resuspension, UoA/kg
C     LEAF2     - Leaf concentration from irrigation, UoA/kg
C     PLT()     - Plant concentration for each feed and fodder type, UoA/kg  
C     ROOT1     - Root uptake contribution from surface soil, UoA/kg
C     ROOT2     - Root uptake contribution from deep soil, UoA/kg
C     T         - decay/integral time, sec
C     TENV      - Weathering time constant (-) for each radionuclide
C     WATCON()  - water concentration used for each radionuclide in the,
C                 current chain, either ground water or surface water
C
C     Assumptions:
C
C     1) Animals drink irrigation water if irrigation is considered,
C        otherwise animals assumed to drink surface water
C
C---- Modification History ---------------------------------------------------
C    Date     Who  Modification Summary
C  ---------  ---  -----------------------------------------------------------
C  09-Sep-97  DLS  Initial modification of old GENII
C  05-Feb-98  BAN  Correction of soil units
C  12-Feb-98  BAN  Conversion to use of annual average soil concs.
C  20-Feb-98  BAN  Allow use of DEPFR and fix units on HARVT
C  14-Sep-99  BAN  Update assignment of Bv for animal feeds, SOIL CONSUMPTION
C-----------------------------------------------------------------------------
C
      SUBROUTINE ANMCAL
C
C---- Include Statments ------------------------------------------------------
C
      INCLUDE 'OPT.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'FODPAR.CMN' 
      INCLUDE 'ANMPAR.CMN'
      INCLUDE 'AFPPAR.CMN'
      INCLUDE 'SOLPAR.CMN'
C
C---- Type/dimension Statments -----------------------------------------------
C
      REAL TENV(9), LEAF(9), PLT(9,6), EDBL(9,6),             LEAF1B, 
     .            ROOT1, ROOT2
C     REAL TENV(9), LEAF(9), PLT(9,6), EDBL(9,6), DW, LEAF1A, LEAF1B, 
C     .     LEAF2, ROOT1, ROOT2, WATCON(9)
      INTEGER IBV
      LOGICAL ANFLG
C
C---- Start of Analysis ------------------------------------------------------
C
C-----  Calculate weathering constant ----------------------------------------
C
        TENV2 = ALOG2 / WTIM / YRDA
C
C-----  Move fresh forage concentration into PLT -----------------------------
C
        IF (ANMFED) THEN
          DO 112 IN = 1, NONUC
            PLT(IN,5) = ANMCON(IN,1)
            PLT(IN,6) = ANMCON(IN,3)
            ANMCON(IN,1) = 0.0
            ANMCON(IN,3) = 0.0
  112     CONTINUE
        ENDIF
C
C-----  For each animal feed and fodder pathway ------------------------------
C
        DO 100 IAN = 1, NAN+2
C
C-----    Set option flag/concentration ratio index for feed or forage -------
C
C          IF (IAN .LT. 5) THEN
C            ANFLG = ANF(IAN)
C            IBV = 4
C          ELSEIF (IAN .EQ. 5) THEN
C            ANFLG = ANF(1)
C            IBV = 1
C          ELSE
C            ANFLG = ANF(3)
C            IBV = 1
C          ENDIF
	    IF ( IAN .EQ. 1 .OR. IAN .EQ. 2 .OR. IAN .EQ. 4 ) THEN
	      ANFLG = ANF(IAN)
	      IBV = 4
	    ELSEIF ( IAN .EQ. 3 ) THEN
	      ANFLG = ANF(IAN)
	      IBV = 1
	    ELSEIF ( IAN .EQ. 5 ) THEN
		  ANFLG = ANF(1)
		  IBV = 1 
	    ELSE 
		  ANFLG = ANF(3)
	      IBV = 1
	    ENDIF
C
          IF (ANFLG) THEN
c            IF (.NOT. ANMFED) THEN
C
C----- Calculate deposition interception fraction onto leaves from air -------
C
             IF ( .NOT. DRYSET ) THEN
              DEPFR1 = 1.0 - EXP (-2.9 * BIOMA2(IAN) * DRYFA2(IAN))
             ENDIF
C             
C-----        Calculate contribution from deposition onto leaves -------------
C
              DO 110 IN = 1, NONUC 
                PLT(IN,IAN) = 0.0
                EDBL(IN,IAN) = 0.0
C
C-----          Calculate leaf deposition from resuspension ------------------
C
                LEAF1B = SL2AVG(IN,IAN) * LEAFRS * DPVRES * SECYR
     .                   * DEPFR1 * SLDN
C     
C-----          Calculate total leaf deposition rate -------------------------
C
                LEAF(IN) = 0.0
                IF (BIOMA2(IAN) .GT. 0.0) 
     .            LEAF(IN) = (         LEAF1B        ) * TRANSA(IAN) 
     .                       / BIOMA2(IAN) 
C    .            LEAF(IN) = (LEAF1A + LEAF1B + LEAF2) * TRANSA(IAN) 
C    .                       / BIOMA2(IAN) 
C
                IF (DEBUG .AND. IN .EQ. 1) 
     .            WRITE (*,1) IAN,         LEAF1B,        LEAF(1)
C    .            WRITE (*,1) IAN, LEAF1A, LEAF1B, LEAF2, LEAF(1)

C
C-----          Set weathering constant for each radionuclide ----------------
C
                TENV(IN) = TENV2
  110         CONTINUE
C
C-----        Apply weathering correction (accumulation & decay) to leaves ---
C
              T = GRWPA(IAN) * YRDA
              IF (T .GT. 0.0) CALL CHAIN (T, TENV, LEAF, LEAF, 1)
C
C-----        Calculate root contribution and fodder concentration -----------
C
              DO 120 IN = 1, NONUC
C
C-----          Calculate root uptake contribution (surface and deep soil) ---
C
C                ROOT1 = SL2CON(IN,IAN) / SLDN
                ROOT1 = SL2AVG(IN,IAN) 
     .                  * BVI(IBV,IN) * DRYFA2(IAN) * RF1  
                ROOT2 = 0.0
                IF (DEEP) THEN
C     .            ROOT2 = DS2CON(IN,IAN) / SSLDN 
                  ROOT2 = DS2AVG(IN,IAN)
     .                    * BVI(IBV,IN) * DRYFA2(IAN) * RF2
                ENDIF
C
C-----          Save uptake from soil for harvest removal --------
C
                IF (HARVST) THEN
                  HARVA(IN,IAN,1) = (LEAF(IN) + ROOT1) *
     .                                YELDA(IAN) / SLDN
                  IF (DEEP .AND. RF2 .GT. 0.0) THEN 
                    HARVA(IN,IAN,2) = ROOT2 * YELDA(IAN) / WASDEP
     .                                 / SSLDN
                  ELSE
                    HARVA(IN,IAN,2) = 0.0
                  ENDIF
                ENDIF
C
C-----          Total plant concentration for each food and fodder -----------
C
                PLT(IN,IAN) = LEAF(IN) + ROOT1 + ROOT2
                IF (DEBUG .AND. IN .EQ. 1) WRITE (*,2) 
     .            IAN,LEAF(1),ROOT1,ROOT2,PLT(1,IAN)           
  120         CONTINUE
            ENDIF
C
C---------- Decay for holdup between feed harvest and animal consumption -----
C
            T = STORTM(IAN) * YRDA
            IF (T .GT. 0.0) 
     .        CALL CHAIN (T, DUMMY, PLT(1,IAN), PLT(1,IAN), 0)
C
C---------- Calculate animal product concentration ---------------------------
C
            DO 130 IN = 1, NONUC
C
C-----        Animal product concentration for each type of fodder -----------
C
              EDBL(IN,IAN) = PLT(IN,IAN) * FMI(IAN,IN) * CONSUM(IAN) *
     .                       DIETFR(IAN) 
C
C------  SOIL CONSUMPTION
C
	        SL = 0.0
	        IF(IAN .LT.5) SL=SL2AVG(IN,IAN)*SLCONA(IAN) * FMI(IAN,IN)
C
C-----        Add stored feed, fresh forage, AND SOIL------------------------
C
              IF (IAN .LT. 5) THEN 
                ANMCON(IN,IAN) = EDBL(IN,IAN) +SL
              ELSEIF (IAN .EQ. 5) THEN
                ANMCON(IN,1) = ANMCON(IN,1) + EDBL(IN,IAN)
              ELSEIF (IAN .EQ. 6) THEN
                ANMCON(IN,3) = ANMCON(IN,3) + EDBL(IN,IAN)
              ENDIF  
  130       CONTINUE
C          ENDIF
  100   CONTINUE
C      ENDIF
C
C---- For each animal product ------------------------------------------------
C
      DO 200 IAN = 1, NAN
        IF (ANF(IAN)) THEN
C
          IF (DEBUG) WRITE (*,*) 'ANMCON:', ANMCON(1,IAN)
C
C-----    Holdup between slaughter/harvest and human consumption -------------
C
            T = HLDUPA(IAN) * YRDA
            CALL CHAIN (T, DUMMY, ANMCON(1,IAN), ANMCON(1,IAN), 0)
C
C-----    Human uptake from contaminated animal products ---------------------
C
          DO 210 IN = 1, NONUC
            EXPOS(IN,7+IAN) = ANMCON(IN,IAN) 
  210     CONTINUE
        ENDIF
  200 CONTINUE
      RETURN
C
C---- Format Statements ------------------------------------------------------
C
    1 FORMAT (' ANM: (   1B/  LEAF):',I2,1P,4E10.2)
    2 FORMAT (' ANM: (LEAF/ROOT1/ROOT2/PLT):',I2,1P,4E10.2)
      END
C
C---- End of Module ANMCAL ---------------------------------------------------
C
