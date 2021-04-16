C--------------------------------------- VERSION: 14-Sep-99 ----------
C
C      SUBROUTINE ANMCAL(RELFLG)
C
C     This subroutine calculates uptake rate from comsumption of 
C     animal products. 
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Original Programing: 17-Aug-88  RAP
C     Last Modification:   24-Feb-1998 BAN
C     Reviewed and Approved: 
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
C     T         - decay/integral time, sec
C     TENV      - Weathering time constant (-) for each radionuclide
C     WATCON()  - water concentration used for each radionuclide in the,
C                 current chain, either ground water or surface water
C
C     Assumptions:
C
C     1) Animals assumed to drink surface water
C
C-----------------------------------------------------------------------
C   Modification History
C   Date      Who  Description of Modifications
C  ---------  ---  ------------------------------------------------------------
C  03-Jun-97  DLS  Modified logic for analysis
C  30-Aug-97  DLS  Revised equation for wet depostion to use particle eq.
C  9-Feb-1998 BAN  Adjusted plant uptakes to use annual average, not year end.
C  28-Feb-98  BAN  Units adjustment
C  14-SEP-99  BAN  Update selection of Bv for animal feeds, added soil consumption
c  17-Sep-04  BAN  Added zeroing of WATCON 
c  2010-2012  BAN  Adapted for biota module
C-----------------------------------------------------------------------------
C
      SUBROUTINE ANMCAL(RELFLG,TPATH)
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'OPT.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'FODPAR.CMN' 
      INCLUDE 'ANMPAR.CMN'
      INCLUDE 'SOLPAR.CMN'
C 
      REAL TENV(LENCHAIN), LEAF(LENCHAIN), PLT(LENCHAIN,5),
     .     EDBL(LENCHAIN,5), DW, LEAF1A, LEAF1B,
     .     LEAF2, ROOT1, WATCON(LENCHAIN)
      INTEGER IBV, TPATH
      LOGICAL ANFLG,RELFLG
C
C----- Calculate weathering constant -----------------------------------------
C
        TENV2 = ALOG2 / WTIM / YRDA
C
      DO IN=1,NONUC
	  WATCON(IN)=0.0
	END DO
C
C----- For each animal feed and fodder pathway -------------------------------
C
        DO 100 IAN = 1, NAN
C

	      ANFLG = ANF(IAN)
	      IBV = 1
C
c
C-------- Set water source for irrigation and drinking water, drinking
C         water defaults to surface water if no irrigation             -------
c
          IF(TPATH.LE.2) THEN
            DO IN = 1, NONUC
                WATCON(IN) = SWCON(IN)
            END DO
          ENDIF
C
          IF (ANFLG) THEN
C
C----------- Calculate dry deposition retention fraction to leaves -----------
C
              IF(.NOT.DRYSET) THEN
                DEPFR1 = 1.0 - EXP (-2.9 * BIOMA2(IAN) * DRYFA2(IAN))
              ENDIF
C             
C------------ Calculate contribution from deposition onto leaves -------------
c
              DO 110 IN = 1, NONUC
c
                leaf1 = 0.
	          leaf1a = 0.
	          leaf2 = 0.
c
                PLT(IN,IAN) = 0.0
                EDBL(IN,IAN) = 0.0
                IF(TPATH.EQ.3) THEN
C
C-------------- Calculate leaf dry deposition rate from air ------------------
C 
                  LEAF1A = DRYDEP(IN) * DEPFR1 
C
C-------------- Calculated wet deposition retention fraction (rain & irrigation)
C
                  IF(.NOT.WETSET) THEN
                  IF(.NOT.ANION(IN)) THEN
                    DEPFR2 = 2.95*BIOMA2(IAN)*DRYFA2(IAN)*
     .                       RAIN**(-.191)
                  ELSE
                    DEPFR2 = 2.30*BIOMA2(IAN)*DRYFA2(IAN)*
     .                       RAIN**(-.92)
                  ENDIF
                  IF(DEPFR2.GT.1.) DEPFR2 = 1.
                  ENDIF ! End if on WETSET
C
C-------------- Calculate wet deposition from air ----------------------------
C
                  LEAF1A = LEAF1A + WETDEP(IN) * DEPFR2 
                 ENDIF
C
C-------------- Calculate leaf deposition from resuspension ------------------
C
                LEAF1B = SL2AVG(IN,IAN) * LEAFRS * DPVRES * SECYR 
     .                   * DEPFR1 * SLDN
c     
C-------------- Calculate leaf deposition rate from irrigation ---------------
c
                LEAF2 = 0.0
                IF (TPATH.LE.2.AND.IRRSr .GT. 0) THEN
                  IF (IRTIMr .GT. 0.0) THEN
                    RATEIRR = RIRRR * MMIN / (IRTIMR * DAYMO)
                    IF(.NOT.WETSET) THEN
                    IF(.NOT.ANION(IN)) THEN
	                IF(RATEIRR .eq. 0) then
	                  DEPFR2 = 0.0
	                ELSE
                        DEPFR2 = 2.95*BIOMA2(IAN)*DRYFA2(IAN)*
     .                         RATEIRR**(-.191)
	                END IF
                    ELSE
	                IF(RATEIRR .eq. 0) then
	                  DEPFR2 = 0.0
	                ELSE
                        DEPFR2 = 2.30*BIOMA2(IAN)*DRYFA2(IAN)*
     .                         RATEIRR**(-.92)
	                END IF
                    ENDIF
                    IF(DEPFR2.GT.1.) DEPFR2 = 1.
                    ENDIF
                    LEAF2 = WATCON(IN) * RIRRR * LM2IN /
     .                      IRTIMr * MOYR * DEPFR2
                  ENDIF
                ENDIF
C
C-------------- Calculate total leaf deposition rate -------------------------
C
                LEAF(IN) = 0.0
                IF (BIOMA2(IAN) .GT. 0.0) 
     .            LEAF(IN) = (LEAF1A + LEAF1B + LEAF2) * TRANSA(IAN) 
     .                       / BIOMA2(IAN) 
C
                IF (DEBUG .AND. IN .EQ. 1) 
     .            WRITE (NELS,1) IAN, LEAF1A, LEAF1B, LEAF2, LEAF(1)
C
C-------------- Set weathering constant for each radionuclide ----------------
C
                TENV(IN) = TENV2
C
  110         CONTINUE
C
C------------ Apply weathering, accumulation, & decay to leaves ---
C
              T = GRWPA(IAN) * YRDA
              IF (T .GT. 0.0) CALL CHAIN (T, TENV, LEAF, LEAF, 1)
C
C------------ Calculate root contribution and fodder concentration -----------
C
              DO 120 IN = 1, NONUC
C
C-------------- Calculate root uptake contribution surface soil) -------------
C
                ROOT1 = SL2Avg(IN,IAN)  
     .                  * BVI(IBV,IN) * DRYFA2(IAN) * RF1  
C
C-------------- Total plant concentration for each food and fodder -----------
C
               IF(RELFLG) THEN
                PLT(IN,IAN) = LEAF(IN) + ROOT1
                IF (DEBUG .AND. IN .EQ. 1) WRITE (NELS,2) 
     .            IAN,LEAF(1),ROOT1,PLT(1,IAN)
               ENDIF
  120         CONTINUE
            ENDIF
C
C---------- Calculate animal product concentration ---------------------------
C
            DO 130 IN = 1, NONUC
C
C------------ Animal product concentration for each type of INTAKE -----------
C  SET IT UP HERE SO THAT RIPARIAN CRITTERS (2=MAMMAL, 4=BIRD) EAT FISH, BQ/KG
               EATS = PLT(IN,IAN)
	IF((IAN .EQ. 2) .OR. (IAN .EQ. 4)) EATS = EXPOS(IN,18)
C
              EDBL(IN,IAN) = EATS * FMI(IAN,IN) * CONSUM(IAN) 
C
C------------ Calculate animal drinking water & SOIL contribution -------------
C
              DW = 0.0
	        SL = 0.0
 			  DW = WATCON(IN) * FMI(IAN,IN) * DWATER(IAN) 
	          SL = SL2AVG(IN,IAN) * SLCONA(IAN) * FMI(IAN,IN)
C
C------------ Add stored feed, fresh forage, SOIL,and drink water contributions
C
                ANMCON(IN,IAN) = EDBL(IN,IAN) + DW + SL

  130       CONTINUE

  100   CONTINUE
C      ENDIF
C
C---- For each animal --------------------------------------------------------
C
      IF(RELFLG) THEN
      DO 200 IAN = 1, NAN
        IF (ANF(IAN)) THEN
C
C-------- Concentration in animal---------------------------------------------
C
          DO 210 IN = 1, NONUC
            EXPOS(IN,7+IAN) = ANMCON(IN,IAN)
  210     CONTINUE
C
        ENDIF
  200 CONTINUE
      ENDIF
C
      RETURN
C
C---- Format Statements ------------------------------------------------------
C
    1 FORMAT (' ANM: (1A/1B/2/LEAF):',I2,1P,4E10.2)
    2 FORMAT (' ANM: (LEAF/ROOT1/PLT):',I2,1P,4E10.2)
c
C----- END OF MODULE ANMCAL --------------------------------------------------
c
      END

