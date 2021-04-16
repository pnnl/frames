C------------------------------------ Version: 23-Mar-98 ---------------------
C
C    SUBROUTINE ANMCAL
C
C     This subroutine calculates uptake rate from comsumption of
C     animal products. 
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Original coding: 17-Aug-88  RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C     Last modification:  14-Sep-99  BAN
C
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
C---- MODIFICATION HISTORY ---------------------------------------------------
C   Date      Who  Modification Summary
C  ---------  ---  ------------------------------------------------
C  30-Aug-97  DLS  Added wet deposition by rain and estimation of wet
C                  deposition interception fraction DEPFR2
C  23-Mar-98  BAN  Adjust units, etc.
C  14-SEP-99  BAN  Update selection of Bv for animal feeds, added soil consumption
C
C-- SUBROUTINE CALL ----------------------------------------------------------
C 
      SUBROUTINE ANMCAL
C
C--- INCLUDE STATEMENTS ------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'OPT.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'FODPAR.CMN'
      INCLUDE 'ANMPAR.CMN'
      INCLUDE 'SOLPAR.CMN'
C
C----- Type/dimension Statements ---------------------------------------------
C
      REAL TENV(CHAINLEN), LEAF(CHAINLEN), PLT(CHAINLEN,6),
     .     EDBL(CHAINLEN,6), DW,  LEAF1B,
     .      ROOT1
      INTEGER IBV
      LOGICAL ANFLG
C
C-----  Calculate weathering constant--
C
        TENV2 = ALOG2 / WTIM / YRDA
C
C-----  For each animal feed and fodder pathway--
C
        DO 100 IAN = 1, NAN+2
C
C-----    Set option flag/concentration ratio index for feed or forage--
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
C
C-----    Water source for irrigation and drinking water
C         surface water only in acute cases
C
          IF (ANFLG) THEN
C
              IF(.NOT.DRYSET) THEN
                DEPFR1 = 1.0 - EXP (-2.9 * BIOMA2(IAN) * DRYFA2(IAN))
              ENDIF
C             
C-----        Calculate contribution from deposition onto leaves--
C
              DO 110 IN = 1, NONUC
 
                PLT(IN,IAN) = 0.0
                EDBL(IN,IAN) = 0.0
C
C-----          Calculate leaf deposition from resuspension--
C
                LEAF1B = SL2AVG(IN,IAN) * LEAFRS * DPVRES * SECYR
     .                   * DEPFR1 * SLDN
c         
C               Calculate total leaf deposition rate--
                LEAF(IN) = 0.0
                IF (BIOMA2(IAN) .GT. 0.0) 
     .            LEAF(IN) = LEAF1B * TRANSA(IAN) / BIOMA2(IAN) 
C
                IF (DEBUG .AND. IN .EQ. 1) 
     .            WRITE (*,1) IAN,  LEAF1B,  LEAF(1)
C
C-------------  Set weathering constant for each radionuclide--
C
                TENV(IN) = TENV2
  110         CONTINUE
C
C             Apply weathering correction (accumulation & decay) to leaves--
C
              T = GRWPA(IAN) * YRDA
              IF (T .GT. 0.0) CALL CHAIN (T, TENV, LEAF, LEAF, 1)
C
C-----------  Calculate root contribution and fodder concentration--
C
              DO 120 IN = 1, NONUC
C
C-------------- Calculate root uptake contribution (surface and deep soil)--
C
                ROOT1 = SL2AVG(IN,IAN)
     .                  * BVI(IBV,IN) * DRYFA2(IAN) * RF1  
C
C-------------  Save uptake from soil for next year's harvest removal--
C
                IF (HARVST) THEN
                  HARVA(IN,IAN) = (LEAF(IN) + ROOT1) * YELDA(IAN)
     .                             / SLDN
                ENDIF
C
C-------------- Total plant concentration for each food and fodder--
C
                PLT(IN,IAN) = LEAF(IN) + ROOT1
                IF (DEBUG .AND. IN .EQ. 1) WRITE (*,2) 
     .            IAN,LEAF(1),ROOT1,PLT(1,IAN)           
  120         CONTINUE
C
C---------- Decay for holdup between feed harvest and animal consumption--  
C
            T = STORTM(IAN) * YRDA
            IF (T .GT. 0.0) 
     .        CALL CHAIN (T, DUMMY, PLT(1,IAN), PLT(1,IAN), 0)
C
C---------- Calculate animal product concentration -----------------------
C
            DO 130 IN = 1, NONUC
C
C----------- Animal product concentration for each type of fodder--
C
              EDBL(IN,IAN) = PLT(IN,IAN) * FMI(IAN,IN) * CONSUM(IAN) * 
     .                       DIETFR(IAN) 
C
C------------ Calculate animal drinking water contribution--
C  (Always zero in years after 1st year)
              DW = 0.0
	        SL = 0.0
			IF (IAN .LT. 5) SL=SL2AVG(IN, IAN)*SLCONA(IAN)*FMI(IAN,IN)
C
C--------- Add stored feed, fresh forage,soil,and drink water contributions
C
              IF (IAN .LT. 5) THEN 
                ANMCON(IN,IAN) = EDBL(IN,IAN) + DW + SL
              ELSEIF (IAN .EQ. 5) THEN
                ANMCON(IN,1) = ANMCON(IN,1) + EDBL(IN,IAN)
              ELSEIF (IAN .EQ. 6) THEN
                ANMCON(IN,3) = ANMCON(IN,3) + EDBL(IN,IAN)
              ENDIF  
  130       CONTINUE
          ENDIF
  100   CONTINUE
C
C---- For each animal product ------------------------------------------
C
      DO 200 IAN = 1, NAN
        IF (ANF(IAN)) THEN
          IF (DEBUG) WRITE (*,*) 'ANMCON:', ANMCON(1,IAN)
C
C-------- Holdup between slaughter/harvest and human consumption--
C
          IF (.NOT. DERANM) THEN
            T = HLDUPA(IAN) * YRDA
            CALL CHAIN (T, DUMMY, ANMCON(1,IAN), ANMCON(1,IAN), 0)
          ENDIF
C
C-------- Concentration in contaminated animal products--
C
          DO 210 IN = 1, NONUC
            EXPOS(IN,7+IAN) = ANMCON(IN,IAN) 
  210     CONTINUE
        ENDIF
  200 CONTINUE
C
      RETURN
C
C---- Format Statements -----------------------------------------------
C
    1 FORMAT (' ANM: (1B/LEAF):',I2,1P,4E10.2)
    2 FORMAT (' ANM: (LEAF/ROOT1/PLT):',I2,1P,4E10.2)
      END   
C
C---- END OF MODULE ANMCAL --------------------------------------------
C
