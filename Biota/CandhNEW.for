C----------------------------------------8 Nov 2001-----------------------
C
      SUBROUTINE CANDH 
C
C     This module calculates environmental concentrations of C-14 and
C     H-3 using simplified specific-activity models for chronic
C     exposures. Elemental tritium and environmental OBT are included using
C     the NEWTRIT formulation of S.R. Peterson
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System
C
C     Original coding:  17-Apr-90 RAP
C     Last Modification:    16 Nov 2002 BAN  Complete revision to incorporate
C                           the NEWTRIT models
C
C                           16 Feb 2003 BAN  Adjust animal THO eqn as per Ring P.
C                           26 June 2006 BAN Update FWAP from 1.485 to Newtrit
C
C-----------------------------------------------------------------------
C
C     AIRBOX()  - air concentration for each radionuclide in the 
C                 current chain, set based on whether acute or chronic
C                 exposure 
C     ANFLAG    - option flag for each of the feed and fodder types
C     ANMFR     - fraction of animal diet for the current animal feed 
C                 type, set based on whether acute or chronic exposure 
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
C     2) Either HT or HTO are input, HTO and OBT are output
C
C-----------------------------------------------------------------------
C
c THIS SECTION HAS NOT BEEN TESTED - THE APPROXIMATIONS MAY BE INCORRECT FOR BIOTA
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'FODPAR.CMN' 
      INCLUDE 'ANMPAR.CMN'
      INCLUDE 'SOLPAR.CMN'
 
      REAL LEAF(LENCHAIN), EDBL(LENCHAIN,5), LEAF1A, LEAF2,
     .     WATCON(LENCHAIN), AIRBOX(LENCHAIN),
     .     EDBL1(5), EDBL2(5), DW1(5), DW2(5), ANMFR,
     .     C(6), C1, C2, C3,
     .     H(5), FWAP(5), FWPP(5), WEQP(5), WEQA(5), RFPP(5), RF1R(5)
      LOGICAL ANFLG

      DATA C /0.24, 0.20, 0.07, 0.15, 0.24, 0.07/
      DATA C1 /1.6E-4/
      DATA C2 /0.09/
      DATA C3 /0.4/

C Fraction H in animals - TMAM, RMAM, BIRD, BIRD, eggs (kg H/kg)
      DATA H / 0.104, 0.104, 0.104, 0.104, 0.106/
C Inverse of fraction H in H2O
      DATA H1 /9.0/
C Fraction of water in animal products - TMAM, RMAM, BIRD, BIRD, eggs 
      DATA FWAP / 0.668, 0.67, 0.67, 0.67, 0.74/
C Fraction water in plants - animal forage
      DATA FWPP / 0.906, 0.906, 0.906, 0.906, 0.906/
C Fraction dry matter = (1 - wet fraction)
C
C Water equivalent fraction of dry matter
	DATA WEQP / 0.6, 0.6, 0.6, 0.6, 0.6/
	DATA WEQA / 0.795, 0.796, 0.796, 0.796, 0.835/
C NEWTRIT reduction factors
	DATA RFPP / 0.9, 0.9, 0.9, 0.9, 0.9/
	DATA RF1R / 0.9, 0.9, 0.9, 0.9, 0.9/
C
      DATA SKG /0.01/
      DATA PKG /0.1/
      DATA RMV /1.25/
C-----------------------------------------------------------------------

C     Process each selected terrestrial food type--                     

      IF (TFOOD) THEN
        DO 101 ITF = 1, NTF   ! LV ONLY FOR BIOTA
          IF (TFD(ITF)) THEN
           IF (H3) THEN
C           This is where we differentiate H3 from H3EL input.  1.5 is Supplemental Factor
	       HTring = 1.0
	       if (H3EL) HTring = 8 * ABSHUM * 1.5/RFPP(ITF)
           ENDIF
 

            IF (.NOT. DERCRP) THEN

C             Set media conpartments--

                  AIRBOX(1) = ARFCON(1,ITF)   ! Bq/m3

                  IF (IRRSr .EQ. 2) THEN
                    WATCON(1) = swCON(1)
                  ELSE 
                   WATCON(1) = 0.0
                  ENDIF
C
C        Factor of 0.9 is isotopic discrimination factor ID_PP
                FIXOBT = 0.9 * RF1R(ITF)/RFPP(ITF)*(1. - 
     .                       FWPP(ITF)) / FWPP(ITF)*WEQP(ITF)
	          IF (H3EL) FIXOBT = FIXOBT*RFPP(ITF)/RF1R(ITF)

C               Calculate leaf concentration--
                IF (C14) THEN
                  LEAF1A = AIRBOX(1) / C1 
                ELSEIF (H3) THEN
                  LEAF1A = HTring * AIRBOX(1) / ABSHUM   ! Bq/L
                ENDIF

C               Calculate leaf concentration from irrigation--
                LEAF2 = 0.0
                IF (IRTIMr .GT. 0.0) THEN
 
                  IF (C14) THEN
                    LEAF2 = WATCON(1) * RIRRr * LM2IN / 
     .                      IRTIMr * MOYR * PKG * RMV 
     .                      / SLDN / SKG

                  ELSEIF (H3) THEN
                    LEAF2 = WATCON(1)

                  ENDIF
                ENDIF
          
C               Calculate total leaf and plant concentrations --

                LEAF(1) = LEAF1A + LEAF2
 
                IF (ITF .LE. 3) THEN  ! for C14: LV, OV, Fruit
                  IF (C14) THEN
                    PLTCON(1,ITF) = LEAF(1) * C2
                  ELSEIF (H3) THEN
                    PLTCON(1,ITF) = LEAF(1)*RFPP(ITF)*FWPP(ITF)
	              PLTCON(2,ITF) = PLTCON(1,ITF)*FIXOBT
                  ENDIF

                ELSE                  ! Grain
                  IF (C14) THEN
                    PLTCON(1,ITF) = LEAF(1) * C3
                  ELSEIF (H3) THEN
                    PLTCON(1,ITF) = LEAF(1)*RFPP(ITF)*FWPP(ITF)
	              PLTCON(2,ITF) = PLTCON(1,ITF)*FIXOBT
                  ENDIF
                ENDIF

C  111         CONTINUE

            ENDIF  !  DRCRP

C           Calculate human uptake from crops ---

              EXPOS(1,3+ITF) = PLTCON(1,ITF) 
	        EXPOS(2,3+ITF) = PLTCON(2,ITF)

          ENDIF   !  TFD(ITF)
  101   CONTINUE  !  NTF loop
      ENDIF
 
C---- Process each selected animal pathway -----------------------------
C
      IF (ANFOOD .or. tfood) THEN

            DO 112 IN = 1, NONUC
              ANMCON(IN,1) = 0.0
              ANMCON(IN,3) = 0.0
  112       CONTINUE

C         For each animal feed and fodder pathway--
          DO 100 IAN = 1, NAN
            IF (H3) THEN
C           This is where we differentiate H3 from H3EL input.  1.5 is Supplemental Factor
	        HTring = 1.0
              value = RFPP(5)
Cbbbbbbb	        if(IAN .LT. 5)value = RFPP(4)
	        if (H3EL) HTring = 8 * ABSHUM * 1.5/value
            ENDIF

C           Set option flag/concentration ratio index for feed or forage--
              ANFLG = ANF(IAN)

            IF (ANFLG) THEN

C             Set media compartments--
  
C               Chronic--

                  AIRBOX(1) = ARF2CN(1,IAN)
                  IF (IRRSr .EQ. 2) THEN
                    WATCON(1) = SWCON(1)
                  ELSE 
                    WATCON(1) = 0.0
                  ENDIF

C               Set chronic exposure animal diet fraction--    
                ANMFR = 1.

C               Calculate contribution from deposition onto leaves--

C                 Calculate leaf deposition rate from air--
                  IF (C14) THEN
                    LEAF1A = AIRBOX(1) / C1 
                  ELSEIF (H3) THEN	
                    LEAF1A = HTring * AIRBOX(1) / ABSHUM
                  ENDIF

C                 Calculate leaf deposition rate from irrigation--
                  LEAF2 = 0.0
                  IF(IRRSr .GT. 0.0 )THEN

                    IF (C14) THEN
                      LEAF2 = WATCON(1) * RIRRr * LM2IN / 
     .                        IRTIMr * MOYR * PKG * RMV 
     .                        / SLDN / SKG

                    ELSEIF (H3) THEN
                      LEAF2 = WATCON(1)

                    ENDIF
                  ENDIF

C                 Calculate total plant concentration--
                  LEAF(1) = LEAF1A + LEAF2

                  IF (C14) THEN
                    EDBL(1,IAN) = LEAF(1) * C(IAN) * ANMFR

                  ELSEIF (H3) THEN
C ---Grain
Cbbbbb                     RFA = RFPP(4)
Cbbbbb	               FWA = FWPP(4)
Cbbbbb	               WEQ = WEQP(4)
C --- Grass and hay
Cbbbbb                     IF (IAN .GE. 5) THEN
	                 RFA = RFPP(5)
	                 FWA = FWPP(5)
	                 WEQ = WEQP(5)
Cbbbbb	               END IF
C --- INCLUDES OBT FRACTION
	               EDBL1(IAN) = CONSUM(IAN) * ANMFR * (LEAF(1) * RFA *
     .                 FWA * (1.+ 0.9*RF1R(IAN)/RFA*(1. - FWA)/FWA*WEQ))
C
C ban - BIOTA: 2, 4 ARE RIPARIAN, EAT FISH, AND ARE ABOUT AT EQUILIBRIUM
	IF(IAN .EQ. 2 .OR. IAN .EQ. 4) EDBL1(IAN) = WATCON(1)*CONSUM(IAN)
C	              
                     EDBL2(IAN) = CONSUM(IAN)*ANMFR*(FWA+(1-FWA)*WEQ)
                  
C                   Calculate animal drinking water contribution--
                    
                      DW1(IAN) = WATCON(1) * DWATER(IAN)
                      DW2(IAN) = DWATER(IAN)
 
                  ENDIF

            ENDIF
  100     CONTINUE
C        ENDIF

C       Add stored feed, fresh forage, and drinking water contributions--

        IF (C14) THEN 
	    IF (CONSUM(1) .GT. 0.0) ANMCON(1,1) = EDBL(1,1)  
          IF (CONSUM(2) .GT. 0.0) ANMCON(1,2) = EDBL(1,2) 
          IF (CONSUM(3) .GT. 0.0) ANMCON(1,3) = EDBL(1,3) 
          IF (CONSUM(4) .GT. 0.0) ANMCON(1,4) = EDBL(1,4) 
          IF (CONSUM(5) .GT. 0.0) ANMCON(1,5) = EDBL(1,5)

        ELSEIF (H3) THEN

C         T.MAMMAL--
          RTEMP = (EDBL2(1)  + DW2(1)) 
          IF (RTEMP .GT. 0.0) THEN
            ANMCON(1,1) = (EDBL1(1)  + DW1(1)) / RTEMP *
     .                     FWAP(1)
c
c        (2,1) is always the OBT contribution
	      ANMCON(2,1) = ANMCON(1,1)*(1. - FWAP(1))/FWAP(1)*WEQA(1)

          ELSE
            ANMCON(1,1) = 0.0
	      ANMCON(2,1) = 0.0
          ENDIF

C         R.MAMMAL--
          RTEMP = (EDBL2(2) + DW2(2)) 
          IF (RTEMP .GT. 0.0) THEN
            ANMCON(1,2) = (EDBL1(2) + DW1(2)) / RTEMP *
     .                    FWAP(2)
c  
	      ANMCON(2,2) = ANMCON(1,2)*(1. - FWAP(2))/FWAP(2)*WEQA(2)

          ELSE
            ANMCON(1,2) = 0.0
	      ANMCON(2,2) = 0.0
          ENDIF

C         T.BIRD--
          RTEMP = (EDBL2(2) + DW2(2)) 
          IF (RTEMP .GT. 0.0) THEN
            ANMCON(1,3) = (EDBL1(3) + DW1(3)) / RTEMP *
     .                    FWAP(3)
c  
	      ANMCON(2,3) = ANMCON(1,3)*(1. - FWAP(3))/FWAP(3)*WEQA(3)

          ELSE
            ANMCON(1,2) = 0.0
	      ANMCON(2,2) = 0.0
          ENDIF

C         R.BIRD--
          RTEMP = (EDBL2(4) + DW2(4))
          IF (RTEMP .GT. 0.0) THEN
            ANMCON(1,4) = (EDBL1(4) + DW1(4)) / RTEMP *
     .                     FWAP(4)
c 
	      ANMCON(2,4) = ANMCON(1,4)*(1. - FWAP(4))/FWAP(4)*WEQA(4)

          ELSE
            ANMCON(1,4) = 0.0
	      ANMCON(2,4) = 0.0
          ENDIF
C         Eggs--
          RTEMP = (EDBL2(5) + DW2(5))
          IF (RTEMP .GT. 0.0) THEN
            ANMCON(1,5) = (EDBL1(5) + DW1(5)) / RTEMP *
     .                     FWAP(5)
c 
	      ANMCON(2,5) = ANMCON(1,5)*(1. - FWAP(5))/FWAP(5)*WEQA(5)

          ELSE
            ANMCON(1,4) = 0.0
	      ANMCON(2,4) = 0.0
          ENDIF
        ENDIF   ! (H3)

        DO 200 IAN = 1, NAN
          IF (ANF(IAN)) THEN

C           CRITTER CONCENTRATIONS --
            DO 210 IN = 1, NONUC

              EXPOS(IN,7+IAN) = ANMCON(IN,IAN) 

  210       CONTINUE

          ENDIF
  200   CONTINUE
      ENDIF   !  ANFOOD

      RETURN
C-----------------------------------------------------------------------
      END
