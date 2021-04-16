C--------------------------------------- Version: 25-Nov-2001--------------
C
C     SUBROUTINE CANDH 
C
C     This module calculates environmental concentrations of C-14 and
C     H-3 using simplified specific-activity models for acute 
C     exposures. 
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System
C
C     Initial GENII Version:  17-Apr-90 RAP
C     Initial Reviewed and Approved: 3-Apr-90  BA Napier
C     Last Modification:  25-Nov-01   BAN
C-----------------------------------------------------------------------
C
C     AIRBOX()  - air concentration for each radionuclide in the 
C                 current chain, set based on acute exposure 
C     ANFLAG    - option flag for each of the feed and fodder types
C     ANMFR     - fraction of animal diet for the current animal feed 
C                 type, set based on acute exposure 
C     DW1()     - Intake via drinking water, used for tritium
C     DW2()     - Concentration in drinking water, used for tritium 
C     EDBL()    - Concentration in animals from the six feed categories,
C                 used for carbon
C     EDBL1()   - Intake via food, used for tritium
C     EDBL2()   - Concentration in food, used for tritium
C     LEAF()    - Total leaf concentration, each radionuclide, UoA/kg
C     LEAF1a    - Leaf concentration from air, UoA/kg 
C     LEAF2     - Leaf concentration from irrigation, UoA/kg
C     WATCON()  - Water concentration used for each radionuclide in the
C                 current chain, either ground water or surface water
C
C     Assumptions:
C
C     1) Animals drink irrigation water if irrigation is considered,
C        otherwise animals assumed to drink surface water
C
C-----------------------------------------------------------------------
C  Modification History
C    Data     Who  Description
C  ---------  ---  -----------------------------------------------------------
C  22-Jul-97  DLS  Initial GENII subroutine revised to eliminate all chonic
C                  exposure considerations.
C  23-Aug-97  DLS  Minor changes to eliminate DERCRP and DERANM logic
C  23-Mar-98  BAN  Correction/addition of ACUTIM, set to zero other times
C  25-Nov-2001 BAN  Incorporate Ring Peterson's HT approximation 
C------------------------------------------------------------------------------
C
      SUBROUTINE CANDH 
C
C---- Include statements -----------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'FODPAR.CMN' 
      INCLUDE 'ANMPAR.CMN'
C      INCLUDE 'AFPPAR.CMN'
      INCLUDE 'SOLPAR.CMN'
	INCLUDE 'TIMES.CMN'
C
C---- Type/dimension statments -----------------------------------------------
C
      REAL LEAF(9), EDBL(9,6), LEAF1A, LEAF2, WATCON(9), AIRBOX(9),
     .     EDBL1(6), EDBL2(6), DW1(4), DW2(4), ANMFR,
     .     C(6), C1, C2, C3,
     .     H(6), H1, H33, 
     .     W(6), W1, W2, W3,
     .     EQUILH, EQUILC
c     .     CIH, CIC, EQUILH, EQUILC
      LOGICAL ANFLG
C
C---- Data statments ---------------------------------------------------------
C
      DATA C /0.24, 0.20, 0.07, 0.15, 0.24, 0.07/
      DATA C1 /1.6E-4/
      DATA C2 /0.09/
      DATA C3 /0.4/
      DATA H / 0.094, 0.087, 0.083, 0.092, 0.094, 0.083/
      DATA H1 /9.0/
      DATA H33 /0.0625/
      DATA W / 0.60, 0.70, 0.88, 0.75, 0.60, 0.88/
      DATA W1 /0.80/
      DATA W2 /0.12/
      DATA W3 /0.80/
      DATA SKG /0.01/
      DATA PKG /0.1/
C   Updated acute model with consumption integral based on decorporation
      DATA CIH /1.32E-3/, CIC /1.0/
      DATA EQUILH /1.0/, EQUILC /4.17E-2/
C
C---- Start of Analysis -------------------------------------------------------
C----If after acute release, all values go to zero for chronic period---------
      IF(ITIMT .GT. 1) THEN
        DO IN = 1, NONUC
          DO ITF = 1, NTF
            EXPOS(IN,ITF+3) = 0.0
          ENDDO
          DO IAN = 1, NAN
            EXPOS(IN,IAN+3) = 0.0
          ENDDO
        ENDDO
	go to 333
      ENDIF
C
C
      IF (H3) THEN
        W1M = W1 / H1 + (1.0 - W1) * H33
        W2M = W2 / H1 + (1.0 - W2) * H33
        W3M = W3 / H1 + (1.0 - W3) * H33
        HTring = 1.0
        if (H3EL) HTring = 8.0 * ABSHUM
      ENDIF
C
C---- Process each selected terrestrial food type ----------------------------
C
      IF (TFOOD) THEN
        DO 101 ITF = 1, NTF
          IF (TFD(ITF)) THEN
C
C-----        Set media conpartments--
C
              DO 122 IN = 1, NONUC
                  AIRBOX(IN) = PWTAIR(IN) / 3.156E7 / ACUTIM * HTring
C                   AIRBOX(IN) = PWTAIR(IN)
                IF (IRRST(ITF) .EQ. 2) THEN
                  WATCON(IN) = SWACUT(IN) / 3.156E7 / ACUTIM
C                  WATCON(IN) = SWACUT(IN)
                ELSE 
                  WATCON(IN) = 0.0
                ENDIF
  122         CONTINUE
C
C----- 
C
              DO 111 IN = 1, NONUC
                EQUIL = 1.0
                IF (C14) EQUIL = EQUILC / GRWP(ITF)
                IF (H3) EQUIL = EQUILH
C
C-------        Calculate leaf concentration ---------------------------------
C
                IF (C14) THEN
                  LEAF1A = AIRBOX(IN) / C1 * EQUIL
                ELSEIF (H3) THEN
                  LEAF1A = AIRBOX(IN) * H1 / ABSHUM * EQUIL
                ENDIF
C
C-----          Calculate leaf concentration from irrigation -----------------
C
                LEAF2 = 0.0
                IF (IRTIMT(ITF) .GT. 0.0) THEN
                  IF (C14) THEN
                    LEAF2 = WATCON(IN) * RIRR(ITF) * LM2IN / 
     .                      IRTIMT(ITF) * MOYR * PKG  
     .                      / SLDN / SKG
                  ELSEIF (H3) THEN
                    LEAF2 = WATCON(IN) * H1
                  ENDIF
                ENDIF
C         
C-----          Calculate total leaf and plant concentrations ----------------
C
                LEAF(IN) = LEAF1A + LEAF2
                IF (ITF .EQ. 4) THEN
                  IF (C14) THEN
                    PLTCON(IN,ITF) = LEAF(IN) * C2
                  ELSEIF (H3) THEN
                    PLTCON(IN,ITF) = LEAF(IN) * W1M
                  ENDIF
                ELSE
                  IF (C14) THEN
                    PLTCON(IN,ITF) = LEAF(IN) * C3
                  ELSEIF (H3) THEN
                    PLTCON(IN,ITF) = LEAF(IN) * W2M
                  ENDIF
                ENDIF
  111         CONTINUE
C
C-----        Decay for holdup time between harvest and consumption ----------
C
              T = HLDUP(ITF) * YRDA
              CALL CHAIN (T, DUMMY, PLTCON(1,ITF), PLTCON(1,ITF), 0)
C
C-----      Calculate human uptake from crops --------------------------------
C
            DO 131 IN = 1, NONUC
              CI = 1.0
              IF (C14) CI = CIC
              IF (H3) CI = CIH
              EXPOS(IN,3+ITF) = PLTCON(IN,ITF) * CI
  131       CONTINUE
          ENDIF
  101   CONTINUE
      ENDIF
C
C---- Process each selected animal pathway -----------------------------
C
      IF (ANFOOD) THEN
        IF (ANMFED) THEN
          DO 112 IN = 1, NONUC
            ANMCON(IN,1) = 0.0
            ANMCON(IN,3) = 0.0
  112     CONTINUE
C
C-----    For each animal feed and fodder pathway ----------------------------
C
          DO 100 IAN = 1, NAN+2
C
C-----      Set option flag/concentration ratio index for feed or forage -----
C
            IF (IAN .LT. 5) THEN
              ANFLG = ANF(IAN)
            ELSEIF (IAN .EQ. 5) THEN
              ANFLG = ANF(1)
            ELSE
              ANFLG = ANF(3)
            ENDIF
            IF (ANFLG) THEN
C
C-----        Set media compartments -----------------------------------------
C 
              DO 132 IN = 1, NONUC
                  AIRBOX(IN) = PWTAIR(IN) * HTring / 3.156E7 / ACUTIM
                IF (IRRSA(IAN) .EQ. 2) THEN
                  WATCON(IN) = SWACUT(IN) / 3.156E7 / ACUTIM
                ELSE 
                  WATCON(IN) = 0.0
                ENDIF
  132           CONTINUE
C
C-----          Set acute diet fraction for autumn ---------------------------
C
                IF (IAN .EQ. 1) THEN
                  ANMFR = 1.0 - FRACUT(1)
                ELSEIF (IAN .EQ. 2) THEN
                  ANMFR = 1.0
                ELSEIF (IAN .EQ. 3) THEN
                  ANMFR = 1.0 - FRACUT(3)
                ELSEIF (IAN .EQ. 4) THEN
                  ANMFR = 1.0
                ELSE IF(IAN.EQ.5) THEN
                  ANMFR = FRACUT(1)
                ELSE
                  ANMFR = FRACUT(3)
                ENDIF
                IF (ANMFED) THEN
C
C-----          Calculate contribution from deposition onto leaves -----------
C
                DO 110 IN = 1, NONUC
                  EQUIL = 1.0
                  IF (C14) EQUIL = EQUILC / GRWPA(IAN)
                  IF (H3) EQUIL = EQUILH
C
C-----            Calculate leaf deposition rate from air --------------------
C
                  IF (C14) THEN
                    LEAF1A = AIRBOX(IN) / C1 * EQUIL
                  ELSEIF (H3) THEN	
                    LEAF1A = AIRBOX(IN) * H1 / ABSHUM * EQUIL
                  ENDIF
C
C-----            Calculate leaf deposition rate from irrigation -------------
C
                  LEAF2 = 0.0
                  IF (IRRSA(IAN) .GT. 0.0 .AND. IRTIMA(IAN) .GT. 0.0) 
     .            THEN
                    IF (C14) THEN
                      LEAF2 = WATCON(IN) * RIRRA(IAN) * LM2IN / 
     .                        IRTIMA(IAN) * MOYR * PKG 
     .                        / SLDN / SKG
                    ELSEIF (H3) THEN
                      LEAF2 = WATCON(IN) * H1
                    ENDIF
                  ENDIF
C
C-----            Calculate total plant concentration ------------------------
C
                  LEAF(IN) = LEAF1A + LEAF2
                  IF (C14) THEN
                    EDBL(IN,IAN) = LEAF(IN) * C(IAN) * ANMFR
                  ELSEIF (H3) THEN
                    WXM = W3M
                    IF (IAN .LT. 5) WXM = W2M
                    EDBL1(IAN) = LEAF(IN) * CONSUM(IAN) * WXM * ANMFR
                    EDBL2(IAN) = CONSUM(IAN) * WXM * ANMFR
C                 
C-----              Calculate animal drinking water contribution -------------
C
                    IF (IAN .LT. 5) THEN
                      DW1(IAN) = WATCON(IN) * DWATER(IAN) * DWFACA(IAN) 
                      DW2(IAN) = DWATER(IAN) * DWFACA(IAN) / H1
                    ENDIF
                  ENDIF
  110           CONTINUE
              ENDIF
            ENDIF
  100     CONTINUE
        ENDIF
C
C-----  Add stored feed, fresh forage, and drinking water contributions ------
C
        IF (C14) THEN
          ANMCON(1,1) = EDBL(1,1) + EDBL(1,5)  
          ANMCON(1,2) = EDBL(1,2) 
          ANMCON(1,3) = EDBL(1,3) + EDBL(1,6) 
          ANMCON(1,4) = EDBL(1,4) 
        ELSEIF (H3) THEN
C
C-----    Meat Concentration -------------------------------------------------
C
          RTEMP = (EDBL2(1) + EDBL2(5) + DW2(1))
          IF (RTEMP .GT. 0.0) THEN
            ANMCON(1,1) = (EDBL1(1) + EDBL1(5) + DW1(1)) / RTEMP *
     .                    (W(1) / H1  +  (1.0 - W(1)) * H(1))
          ELSE
            ANMCON(1,1) = 0.0
          ENDIF
C
C-----    Poultry Concentration ----------------------------------------------
C
          RTEMP = (EDBL2(2) + DW2(2)) 
          IF (RTEMP .GT. 0.0) THEN
            ANMCON(1,2) = (EDBL1(2) + DW1(2)) / RTEMP *
     .                    (W(2) / H1  +  (1.0 - W(2)) * H(2))
          ELSE
            ANMCON(1,2) = 0.0
          ENDIF
C
C-----    Milk Concentration -------------------------------------------------
C
          RTEMP = (EDBL2(3) + EDBL2(6) + DW2(3)) 
          IF (RTEMP .GT. 0.0) THEN          
            ANMCON(1,3) = (EDBL1(3) + EDBL1(6) + DW1(3)) / RTEMP *
     .                    (W(3) / H1  +  (1.0 - W(3)) * H(3))
          ELSE
            ANMCON(1,3) = 0.0
          ENDIF
C
C-----    Eggs Concentration -------------------------------------------------
C
          RTEMP = (EDBL2(4) + DW2(4))
          IF (RTEMP .GT. 0.0) THEN
            ANMCON(1,4) = (EDBL1(4) + DW1(4)) / RTEMP *
     .                    (W(4) / H1  +  (1.0 - W(4)) * H(4))
          ELSE
            ANMCON(1,4) = 0.0
          ENDIF
        ENDIF
C
C---- Calculate final concentration in animal product ------------------------
        DO 200 IAN = 1, NAN
          IF (ANF(IAN)) THEN
C
C-----      Holdup between slaughter/harvest and human consumption -----------
C
            IF (.NOT. DERANM) THEN
              T = HLDUPA(IAN) * YRDA
              CALL CHAIN (T, DUMMY, ANMCON(1,IAN), ANMCON(1,IAN), 0)
            ENDIF
C
C-----      Human uptake from contaminated animal products -------------------
C
            DO 210 IN = 1, NONUC
              CI = 1.0
              IF (C14) CI = CIC
              IF (H3) CI = CIH
              EXPOS(IN,7+IAN) = ANMCON(IN,IAN) * CI
  210       CONTINUE
          ENDIF
  200   CONTINUE
      ENDIF
C
  333 continue
      RETURN
C-----------------------------------------------------------------------
      END
