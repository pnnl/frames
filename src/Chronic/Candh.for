C----------------------------------------8 Nov 2001-----------------------
C
      SUBROUTINE CANDH 
C
C     This module calculates environmental concentrations of C-14 and
C     H-3 using simplified specific-activity models for chronic
C     exposures. 
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System
C
C     Original coding:  17-Apr-90 RAP
C     Reviewed and Approved: 3-Apr-90  BA Napier
C     Last Modification:    23-Mar-98  BAN - to fix grain-fruit bug
C                           8 Nov 2001 BAN - year/second unit correction
C                           24 Nov 2001 BAN - added S.-R. Peterson's HT model
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
C
C-----------------------------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'FODPAR.CMN' 
      INCLUDE 'ANMPAR.CMN'
      INCLUDE 'SOLPAR.CMN'
 
      REAL LEAF(LENCHAIN), EDBL(LENCHAIN,6), LEAF1A, LEAF2,
     .     WATCON(LENCHAIN), AIRBOX(LENCHAIN),
     .     EDBL1(6), EDBL2(6), DW1(4), DW2(4), ANMFR,
     .     C(6), C1, C2, C3,
     .     H(6), H1, H33, 
     .     W(6), W1, W2, W3
c     .     CIH, CIC
      LOGICAL ANFLG

      DATA C /0.24, 0.20, 0.07, 0.15, 0.24, 0.07/
c      DATA C0 /2.0E-5/
      DATA C1 /1.6E-4/
      DATA C2 /0.09/
      DATA C3 /0.4/

C Fraction T in organic animal residuals - meat, poultry, milk, eggs, unused, unused
      DATA H / 0.094, 0.087, 0.083, 0.092, 0.094, 0.083/
C Inverse of fraction H in H2O
      DATA H1 /9.0/
C Fraction T in HTO
      DATA H33 /0.0625/

C Fraction of water in animal products - meat, poultry, milk, eggs, unused, unused
      DATA W / 0.60, 0.70, 0.88, 0.75, 0.60, 0.88/
C Fraction water in plants - LV+OV+Fruit, Cereal/grain, animal forage
      DATA W1 /0.80/
      DATA W2 /0.12/
      DATA W3 /0.80/

c      DATA Z1 /0.1/, Z2 /0.03/

      DATA SKG /0.01/
      DATA PKG /0.1/
      DATA RMV /1.25/

c      DATA CIH /1.32E-3/, CIC /1.0/
c      DATA EQUILC /4.17E-2/

      IF (H3) THEN
        W1M = W1 / H1 + (1.0 - W1) * H33
        W2M = W2 / H1 + (1.0 - W2) * H33
        W3M = W3 / H1 + (1.0 - W3) * H33
	HTring = 1.0
	if (H3EL) HTring = 8 * ABSHUM
      ENDIF

C-----------------------------------------------------------------------

C     Process each selected terrestrial food type--                     

      IF (TFOOD) THEN
        DO 101 ITF = 1, NTF
          IF (TFD(ITF)) THEN
            IF (.NOT. DERCRP) THEN

C             Set media conpartments--

c             IF (ACUTE) THEN
c                DO 122 IN = 1, NONUC
c
c                  IF (C14) THEN
c                    AIRBOX(IN) = PWTAIR(IN) / 3600.0 
c     .                           * 2.0 / (GRWP(ITF) * 24.0)
c                  ELSEIF (H3) THEN
c                    AIRBOX(IN) = PWTAIR(IN) / 3600.0 
c                  ENDIF
c
c                  IF (IRRST(ITF) .EQ. 2) THEN
c                    WATCON(IN) = SWACUT(IN) / 3600.0
c                 ELSE 
c                    WATCON(IN) = 0.0
c                  ENDIF
c
c  122           CONTINUE

c             ELSE

                DO 123 IN = 1, NONUC

C                  AIRBOX(IN) = ARFCON(IN,ITF) * YRSEC
                  AIRBOX(IN) = ARFCON(IN,ITF)

                  IF (IRRST(ITF) .EQ. 2) THEN
                    WATCON(IN) = SWCON(IN)
                  ELSEIF (IRRST(ITF) .EQ. 1) THEN
                    WATCON(IN) = GWCON(IN)
                  ELSE 
                    WATCON(IN) = 0.0
                  ENDIF
  123           CONTINUE

c              ENDIF

              DO 111 IN = 1, NONUC
                
                EQUIL = 1.0
c                IF (ACUTE) THEN
c                  IF (C14) EQUIL = EQUILC / GRWP(ITF)
c                  IF (H3) EQUIL = EQUILH
c                ENDIF

C               Calculate leaf concentration--
                IF (C14) THEN
                  LEAF1A = AIRBOX(IN) / C1 * EQUIL
                ELSEIF (H3) THEN
                  LEAF1A = HTring * AIRBOX(IN) * H1 / ABSHUM * EQUIL
                ENDIF

C               Calculate leaf concentration from irrigation--
                LEAF2 = 0.0
                IF (IRTIMT(ITF) .GT. 0.0) THEN
 
                  IF (C14) THEN
                    LEAF2 = WATCON(IN) * RIRR(ITF) * LM2IN / 
     .                      IRTIMT(ITF) * MOYR * PKG * RMV 
     .                      / SLDN / SKG

                  ELSEIF (H3) THEN
                    LEAF2 = WATCON(IN) * H1

                  ENDIF
                ENDIF
          
C               Calculate total leaf and plant concentrations --

                LEAF(IN) = LEAF1A + LEAF2
 
CCC Changed .EQ. to .LE. here BAN 8 Nov 2001, interchanged C2 and C3
                IF (ITF .LE. 3) THEN
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

C             Decay for holdup time between harvest and consumption--
              T = HLDUP(ITF) * YRDA
              CALL CHAIN (T, DUMMY, PLTCON(1,ITF), PLTCON(1,ITF), 0)

            ENDIF

C           Calculate human uptake from crops ---
            DO 131 IN = 1, NONUC 

              CI = 1.0
c              IF (ACUTE) THEN
c                IF (C14) CI = CIC
c                IF (H3) CI = CIH
c              ENDIF
   
              EXPOS(IN,3+ITF) = PLTCON(IN,ITF) 
C              EXPOS(IN,3+ITF) = PLTCON(IN,ITF) * CONS(ITF) 
C     .                           * ADJEAT(ITF) * CI
  131       CONTINUE

          ENDIF
  101   CONTINUE
      ENDIF
 
C---- Process each selected animal pathway -----------------------------
C
      IF (ANFOOD) THEN
C        IF (.NOT. DERANM .OR. ANMFED) THEN

C          IF (ANMFED) THEN
            DO 112 IN = 1, NONUC
              ANMCON(IN,1) = 0.0
              ANMCON(IN,3) = 0.0
  112       CONTINUE
C          ENDIF

C         For each animal feed and fodder pathway--
          DO 100 IAN = 1, NAN+2

C           Set option flag/concentration ratio index for feed or forage--
            IF (IAN .LT. 5) THEN
              ANFLG = ANF(IAN)
            ELSEIF (IAN .EQ. 5) THEN
              ANFLG = ANF(1)
            ELSE
              ANFLG = ANF(3)
            ENDIF
            IF (ANFLG) THEN

C             Set media compartments--
  
c              IF (ACUTE) THEN
c
c                DO 132 IN = 1, NONUC
c    
c                  IF (C14) THEN
c                    AIRBOX(IN) = PWTAIR(IN) / 3600.0 
c    .                           * 2.0 / (GRWPA(IAN) * 24.0)
c                 ELSEIF (H3) THEN
c                   AIRBOX(IN) = PWTAIR(IN) / 3600.0 
c                 END IF
c               
c                 IF (IRRSA(IAN) .EQ. 2) THEN
c                   WATCON(IN) = SWACUT(IN) / 3600.0
c                 ELSE 
c                   WATCON(IN) = 0.0
c                 ENDIF
c
c 132           CONTINUE
c
C               Set acute diet fraction for autumn--
c               IF (IAN .EQ. 1) THEN
c                 ANMFR = 1.0 - FRACUT(4)
c               ELSEIF (IAN .EQ. 2) THEN
c                 ANMFR = 1.0
c               ELSEIF (IAN .EQ. 3) THEN
c                 ANMFR = 1.0 - FRACUT(4)
c               ELSEIF (IAN .EQ. 4) THEN
c                 ANMFR = 1.0
c               ELSE
c                 ANMFR = FRACUT(4)
c               ENDIF
c      
c             ELSE

C               Chronic--

                DO 133 IN = 1, NONUC

C                  AIRBOX(IN) = ARF2CN(IN,IAN) * YRSEC
                  AIRBOX(IN) = ARF2CN(IN,IAN)
                  IF (IRRSA(IAN) .EQ. 2) THEN
                    WATCON(IN) = SWCON(IN)
                  ELSEIF (IRRSA(IAN) .EQ. 1) THEN
                    WATCON(IN) = GWCON(IN)
                  ELSE 
                    WATCON(IN) = 0.0
                  ENDIF

  133           CONTINUE

C               Set chronic exposure animal diet fraction--    
                ANMFR = DIETFR(IAN)

c             ENDIF
C              IF (ANMFED) THEN

C               Calculate contribution from deposition onto leaves--
                DO 110 IN = 1, NONUC

                  EQUIL = 1.0
c                 IF (ACUTE) THEN
c                   IF (C14) EQUIL = EQUILC / GRWPA(IAN)
c                   IF (H3) EQUIL = EQUILH
c                 ENDIF

C                 Calculate leaf deposition rate from air--
                  IF (C14) THEN
                    LEAF1A = AIRBOX(IN) / C1 * EQUIL
                  ELSEIF (H3) THEN	
                    LEAF1A = HTring * AIRBOX(IN) * H1 / ABSHUM * EQUIL
                  ENDIF

C                 Calculate leaf deposition rate from irrigation--
                  LEAF2 = 0.0
                  IF (IRRSA(IAN) .GT. 0.0 .AND. IRTIMA(IAN) .GT. 0.0) 
     .            THEN

                    IF (C14) THEN
                      LEAF2 = WATCON(IN) * RIRRA(IAN) * LM2IN / 
     .                        IRTIMA(IAN) * MOYR * PKG * RMV 
     .                        / SLDN / SKG

                    ELSEIF (H3) THEN
                      LEAF2 = WATCON(IN) * H1

                    ENDIF
                  ENDIF

C                 Calculate total plant concentration--
                  LEAF(IN) = LEAF1A + LEAF2

                  IF (C14) THEN
                    EDBL(IN,IAN) = LEAF(IN) * C(IAN) * ANMFR

                  ELSEIF (H3) THEN

                    WXM = W3M
                    IF (IAN .LT. 5) WXM = W2M

                    EDBL1(IAN) = LEAF(IN) * CONSUM(IAN) * WXM * ANMFR
                    EDBL2(IAN) = CONSUM(IAN) * WXM * ANMFR
                  
C                   Calculate animal drinking water contribution--
                    IF (IAN .LT. 5) THEN
                      DW1(IAN) = WATCON(IN) * DWATER(IAN) * DWFACA(IAN) 
                      DW2(IAN) = DWATER(IAN) * DWFACA(IAN) / H1
                    ENDIF
 
                  ENDIF
  110           CONTINUE
C              ENDIF

            ENDIF
  100     CONTINUE
C        ENDIF

C       Add stored feed, fresh forage, and drinking water contributions--

        IF (C14) THEN
          ANMCON(1,1) = EDBL(1,1) + EDBL(1,5)  
          ANMCON(1,2) = EDBL(1,2) 
          ANMCON(1,3) = EDBL(1,3) + EDBL(1,6) 
          ANMCON(1,4) = EDBL(1,4) 

        ELSEIF (H3) THEN

C         Meat--
          RTEMP = (EDBL2(1) + EDBL2(5) + DW2(1)) 
          IF (RTEMP .GT. 0.0) THEN
            ANMCON(1,1) = (EDBL1(1) + EDBL1(5) + DW1(1)) / RTEMP *
     .                    (W(1) / H1  +  (1.0 - W(1)) * H(1))

          ELSE
            ANMCON(1,1) = 0.0
          ENDIF

C         Poultry--
          RTEMP = (EDBL2(2) + DW2(2)) 
          IF (RTEMP .GT. 0.0) THEN
            ANMCON(1,2) = (EDBL1(2) + DW1(2)) / RTEMP *
     .                    (W(2) / H1  +  (1.0 - W(2)) * H(2))
          ELSE
            ANMCON(1,2) = 0.0
          ENDIF

C         Milk--
          RTEMP = (EDBL2(3) + EDBL2(6) + DW2(3)) 
          IF (RTEMP .GT. 0.0) THEN          
            ANMCON(1,3) = (EDBL1(3) + EDBL1(6) + DW1(3)) / RTEMP *
     .                    (W(3) / H1  +  (1.0 - W(3)) * H(3))
          ELSE
            ANMCON(1,3) = 0.0
          ENDIF

C         Eggs--
          RTEMP = (EDBL2(4) + DW2(4))
          IF (RTEMP .GT. 0.0) THEN
            ANMCON(1,4) = (EDBL1(4) + DW1(4)) / RTEMP *
     .                    (W(4) / H1  +  (1.0 - W(4)) * H(4))
          ELSE
            ANMCON(1,4) = 0.0
          ENDIF

        ENDIF

        DO 200 IAN = 1, NAN
          IF (ANF(IAN)) THEN

C           Holdup between slaughter/harvest and human consumption--
            IF (.NOT. DERANM) THEN
              T = HLDUPA(IAN) * YRDA
              CALL CHAIN (T, DUMMY, ANMCON(1,IAN), ANMCON(1,IAN), 0)
            ENDIF

C           Human uptake from contaminated animal products--
            DO 210 IN = 1, NONUC

              CI = 1.0
c             IF (ACUTE) THEN
c               IF (C14) CI = CIC
c               IF (H3) CI = CIH
c             ENDIF

              EXPOS(IN,7+IAN) = ANMCON(IN,IAN) 
C              EXPOS(IN,7+IAN) = ANMCON(IN,IAN) * CONS2(IAN) * 
C     .                          ADJET2(IAN) * CI
  210       CONTINUE

          ENDIF
  200   CONTINUE
      ENDIF

      RETURN
C-----------------------------------------------------------------------
      END


