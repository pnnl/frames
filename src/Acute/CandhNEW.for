C--------------------------------------- Version: 18 Nov 2002--------------
C
C     SUBROUTINE CANDH 
C
C     This module calculates environmental concentrations of C-14 and
C     H-3 using simplified specific-activity models for acute 
C     exposures. Elemental tritium and environmental OBT are included using
C     the NEWTRIT formulation of S.R. Peterson
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System
C
C     Initial GENII Version:  17-Apr-90 RAP
C     Last Modification:    18 Nov 2002 BAN  Complete revision to incorporate
C                           the NEWTRIT models
C
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
C  25-Nov-2001 BAN  Incorporate Ring Peterson's NEWTRIT; complete rewrite
C  26 June 2006  BAN Revise FWAP from GENII 1.485 to Newtrit
c  26 jUNE 2006  BAN Change from FRACUT to DIETFR for animal diets
c  26 June 2006  BAN Correct zeroing of chronic animal products
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
     .     H(4), FWAP(4), FWPP(5), WEQP(5), WEQA(4), RFPP(5), RF1R(5),
     .     EQUILH, EQUILC
      LOGICAL ANFLG
C
C---- Data statments ---------------------------------------------------------
C
      DATA C /0.24, 0.20, 0.07, 0.15, 0.24, 0.07/
      DATA C1 /1.6E-4/
      DATA C2 /0.09/
      DATA C3 /0.4/

C Fraction H in animal products - meat, poultry, milk, eggs (kg H/kg)
      DATA H / 0.104, 0.104, 0.107, 0.106/
C Inverse of fraction H in H2O
      DATA H1 /9.0/
C Fraction of water in animal products - meat, poultry, milk
      DATA FWAP / 0.668, 0.67, 0.897, 0.74/
C Fraction water in plants - LV, OV, Fruit,Cereal/grain, animal forage
      DATA FWPP / 0.906, 0.824, 0.853, 0.117, 0.8/
C Fraction dry matter = (1 - wet fraction)
C
C Water equivalent fraction of dry matter
	DATA WEQP / 0.6, 0.58, 0.59, 0.577, 0.616/
	DATA WEQA / 0.795, 0.796, 0.669, 0.835/
C NEWTRIT reduction factors
	DATA RFPP / 0.9, 0.8, 0.8, 0.8, 0.9/
	DATA RF1R / 0.9, 0.9, 0.9, 0.9, 0.9/
C
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
            EXPOS(IN,IAN+7) = 0.0
          ENDDO
        ENDDO
	go to 333
      ENDIF
C
C
C---- Process each selected terrestrial food type ----------------------------
C
      IF (TFOOD) THEN
        DO 101 ITF = 1, NTF
          IF (TFD(ITF)) THEN
C
           IF (H3) THEN
            HTring = 1.0
            if (H3EL) HTring = 8.0 * ABSHUM *1.5/RFPP(ITF)
           ENDIF

C
C-----        Set media conpartments--
C
                  AIRBOX(1) = PWTAIR(1) / 3.156E7 / ACUTIM 
C                   AIRBOX(1) = PWTAIR(1)
                IF (IRRST(ITF) .EQ. 2) THEN
                  WATCON(1) = SWACUT(1) / 3.156E7 / ACUTIM
C                  WATCON(1) = SWACUT(1)
                ELSE 
                  WATCON(1) = 0.0
                ENDIF
C
C        Factor of 0.9 is isotopic discrimination factor ID_PP
                FIXOBT = 0.9 * RF1R(ITF)/RFPP(ITF)*(1. - 
     .                       FWPP(ITF)) / FWPP(ITF)*WEQP(ITF)
	          IF (H3EL) FIXOBT = FIXOBT*RFPP(ITF)/RF1R(ITF)
C
C----- 
C
                EQUIL = 1.0
                IF (C14) EQUIL = EQUILC / GRWP(ITF)
                IF (H3) EQUIL = EQUILH
C
C-------        Calculate leaf concentration ---------------------------------
C
                IF (C14) THEN
                  LEAF1A = AIRBOX(1) / C1 * EQUIL
                ELSEIF (H3) THEN
                  LEAF1A = AIRBOX(1) / ABSHUM * EQUIL * HTring
                ENDIF
C
C-----          Calculate leaf concentration from irrigation -----------------
C
                LEAF2 = 0.0
                IF (IRTIMT(ITF) .GT. 0.0) THEN
                  IF (C14) THEN
                    LEAF2 = WATCON(1) * RIRR(ITF) * LM2IN / 
     .                      IRTIMT(ITF) * MOYR * PKG  
     .                      / SLDN / SKG
                  ELSEIF (H3) THEN
                    LEAF2 = WATCON(1)
                  ENDIF
                ENDIF
C         
C-----          Calculate total leaf and plant concentrations ----------------
C
                LEAF(1) = LEAF1A + LEAF2
                IF (ITF .LE. 3) THEN  !for C14, LV, OV, Fruit
                  IF (C14) THEN
                    PLTCON(1,ITF) = LEAF(1) * C2
                  ELSEIF (H3) THEN
                    PLTCON(1,ITF) = LEAF(1)*RFPP(ITF)*FWPP(ITF)
	              PLTCON(2,ITF) = PLTCON(1,ITF)*FIXOBT
                  ENDIF
                ELSE   ! grain
                  IF (C14) THEN
                    PLTCON(1,ITF) = LEAF(1) * C3
                  ELSEIF (H3) THEN
                    PLTCON(1,ITF) = LEAF(1)*RFPP(ITF)*FWPP(ITF)
	              PLTCON(2,ITF) = PLTCON(1,ITF)*FIXOBT
                  ENDIF
                ENDIF
  
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
            IF (H3) THEN
C           This is where we differentiate H3 from H3EL input.  1.5 is Supplemental Factor
	        HTring = 1.0
              value = RFPP(5)
	        if(IAN .LT. 5)value = RFPP(4)
	        if (H3EL) HTring = 8 * ABSHUM * 1.5/value
            ENDIF
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
                  AIRBOX(1) = PWTAIR(1) / 3.156E7 / ACUTIM
                IF (IRRSA(IAN) .EQ. 2) THEN
                  WATCON(1) = SWACUT(1) / 3.156E7 / ACUTIM
                ELSE 
                  WATCON(1) = 0.0
                ENDIF
C
C-----          Set acute diet fraction ---------------------------
C
                IF (IAN .EQ. 1) THEN
                  ANMFR = dietfr(1)
                ELSEIF (IAN .EQ. 2) THEN
                  ANMFR = dietfr(2)
                ELSEIF (IAN .EQ. 3) THEN
                  ANMFR = DIETFR(3)
                ELSEIF (IAN .EQ. 4) THEN
                  ANMFR = DIETFR(4)
                ELSE IF(IAN.EQ.5) THEN
                  ANMFR = DIETFR(5)
                ELSE
                  ANMFR = DIETFR(6)
                ENDIF
                IF (ANMFED) THEN
C
C-----          Calculate contribution from deposition onto leaves -----------
C
                  EQUIL = 1.0
                  IF (C14) EQUIL = EQUILC / GRWPA(IAN)
                  IF (H3) EQUIL = EQUILH
C
C-----            Calculate leaf deposition rate from air --------------------
C
                  IF (C14) THEN
                    LEAF1A = AIRBOX(1) / C1 * EQUIL
                  ELSEIF (H3) THEN	
                    LEAF1A = AIRBOX(1) * HTring / ABSHUM * EQUIL
                  ENDIF
C
C-----            Calculate leaf deposition rate from irrigation -------------
C
                  LEAF2 = 0.0
                  IF (IRRSA(IAN) .GT. 0.0 .AND. IRTIMA(IAN) .GT. 0.0) 
     .            THEN
                    IF (C14) THEN
                      LEAF2 = WATCON(1) * RIRRA(IAN) * LM2IN / 
     .                        IRTIMA(IAN) * MOYR * PKG 
     .                        / SLDN / SKG
                    ELSEIF (H3) THEN
                      LEAF2 = WATCON(1)
                    ENDIF
                  ENDIF
C
C-----            Calculate total plant concentration ------------------------
C
                  LEAF(1) = LEAF1A + LEAF2
                  IF (C14) THEN
                    EDBL(1,IAN) = LEAF(1) * C(IAN) * ANMFR
                  ELSEIF (H3) THEN
C ---Grain
                     RFA = RFPP(4)
	               FWA = FWPP(4)
	               WEQ = WEQP(4)
C                   NEW STUFF ABN 29 AUG 2012	               
	               IF (IAN .GE. 5) THEN
	                  RF1Rc = RF1R(5)
	               ELSE
	                  RF1Rc = RF1R(IAN)
	               END IF
C --- Grass and hay
                     IF (IAN .GE. 5) THEN
	                 RFA = RFPP(5)
	                 FWA = FWPP(5)
	                 WEQ = WEQP(5)
	               END IF
C --- INCLUDES OBT FRACTION
	               EDBL1(IAN) = CONSUM(IAN) * ANMFR * (LEAF(1) * RFA *
     .                 FWA * (1.+ 0.9*RF1Rc/RFA*(1. - FWA)/FWA*WEQ))
C     .                 FWA * (1.+ 0.9*RF1R(IAN)/RFA*(1. - FWA)/FWA*WEQ))
c	               EDBL2(IAN) = CONSUM(IAN) *RFA * FWA * ANMFR
                     EDBL2(IAN) = CONSUM(IAN)*ANMFR*(FWA +(1-FWA)*WEQ)
                  
C                   Calculate animal drinking water contribution--
                    IF (IAN .LT. 5) THEN
                      DW1(IAN) = WATCON(1) * DWATER(IAN) * DWFACA(IAN) 
                      DW2(IAN) = DWATER(IAN) * DWFACA(IAN) 
                    ENDIF
                  ENDIF

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

C         Meat--
          RTEMP = (EDBL2(1) + EDBL2(5) + DW2(1)) 
          IF (RTEMP .GT. 0.0) THEN
            ANMCON(1,1) = (EDBL1(1) + EDBL1(5) + DW1(1)) / RTEMP *
     .                     FWAP(1)
c     .                    (FWAP(1) / H1  +  (1.0 - FWAP(1)) * H(1))
c        (2,1) is always the OBT contribution
	      ANMCON(2,1) = ANMCON(1,1)*(1. - FWAP(1))/FWAP(1)*WEQA(1)

          ELSE
            ANMCON(1,1) = 0.0
	      ANMCON(2,1) = 0.0
          ENDIF

C         Poultry--
          RTEMP = (EDBL2(2) + DW2(2)) 
          IF (RTEMP .GT. 0.0) THEN
            ANMCON(1,2) = (EDBL1(2) + DW1(2)) / RTEMP *
     .                    FWAP(2)
c     .                    (FWAP(2) / H1  +  (1.0 - FWAP(2)) * H(2))
	      ANMCON(2,2) = ANMCON(1,2)*(1. - FWAP(2))/FWAP(2)*WEQA(2)

          ELSE
            ANMCON(1,2) = 0.0
	      ANMCON(2,2) = 0.0
          ENDIF

C         Milk--
          RTEMP = (EDBL2(3) + EDBL2(6) + DW2(3)) 
          IF (RTEMP .GT. 0.0) THEN          
            ANMCON(1,3) = (EDBL1(3) + EDBL1(6) + DW1(3)) / RTEMP *
     .                     FWAP(3)
c     .                    (FWAP(3) / H1  +  (1.0 - FWAP(3)) * H(3))
	      ANMCON(2,3) = ANMCON(1,3)*(1. - FWAP(3))/FWAP(3)*WEQA(3)

          ELSE
            ANMCON(1,3) = 0.0
	      ANMCON(2,3) = 0.0
          ENDIF

C         Eggs--
          RTEMP = (EDBL2(4) + DW2(4))
          IF (RTEMP .GT. 0.0) THEN
            ANMCON(1,4) = (EDBL1(4) + DW1(4)) / RTEMP *
     .                     FWAP(4)
c     .                    (FWAP(4) / H1  +  (1.0 - FWAP(4)) * H(4))
	      ANMCON(2,4) = ANMCON(1,4)*(1. - FWAP(4))/FWAP(4)*WEQA(4)

          ELSE
            ANMCON(1,4) = 0.0
	      ANMCON(2,4) = 0.0
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
