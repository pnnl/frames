C-----------------------------------------------------------------------
C
      SUBROUTINE CRPCAL
C
C     This subroutine calculates human uptake rate from consumption of 
C     terrestrial foods after 1st year (chronic conditions) 
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Initial coding:  24-Apr-90 RAP 
C     Reviewed and Approved: 12-Sept-88  BA Napier
C     Last modification:  23-Mar-98  BAN
C
C-----------------------------------------------------------------------
C
C     LEAF1a    - Intermediate leaf deposition rate from air  (UoA/m2 yr)
C     LEAF1b    - Intermediate leaf depostion rate from resuspension,
C                 (UoA/m2 yr)
C     LEAF2     - Intermediate leaf deposition rate from irrigation,
C                 (UoA/M2 yr)  
C     TENV      - Weathering constant, yr-1
C     WATCON    - water concentration used for the current radionuclide,
C                 either ground water or surface water (UoA/L)
C
C-----------------------------------------------------------------------
C      23-Mar-98  BAN  Adjust units
C-----------------------------------------------------------------------
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'CONC.CMN'
      INCLUDE 'FODPAR.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'SOLPAR.CMN'
      REAL TENV(9), LEAF(9),         LEAF1B
C
C---- Calculate weathering constant ------------------------------------
C      
      TENV2 =  ALOG2 / WTIM / YRDA
      IF (DEBUG) WRITE (*,*) 'Weathering (TENV2): ', TENV2
C
C---- Process each selected terrestrial food type--                     
C
      DO 100 ITF = 1, NTF
        IF (TFD(ITF)) THEN
C
C---------- Calculate contribution from leaves -------------------------
C
C           IF(.NOT.DRYSET) THEN
C             IF (ITF .EQ. 2 .OR. ITF .EQ. 3) THEN
C               DEPFR1 = 1.0 - EXP (-3.6 * BIOMAS(ITF) * DRYFAC(ITF))
C             ELSE
C               DEPFR1 = 1.0 - EXP (-2.9 * BIOMAS(ITF) * DRYFAC(ITF))
C             ENDIF
C           ENDIF
            DO 110 IN = 1, NONUC
C
C-----        Calculate leaf depositon rate from resuspension--
C
              LEAF1B = SOLAVG(IN,ITF) * LEAFRS * DPVRES * SECYR *
     .                  DEPFR1 * SLDN
C
C------       Calculate total leaf deposition rate --
C
              LEAF(IN) = 0.0
              IF (BIOMAS(ITF) .GT. 0.0) 
     .          LEAF(IN) = LEAF1b * TRANS(ITF) / BIOMAS(ITF) 
              IF (DEBUG .AND. IN .EQ. 1) WRITE (*,*) 
     .          'LEAF1b/LEAF:',LEAF1B,LEAF(1)
C
C--------------- Set weathering constant for each radionuclide ---------
C
              TENV(IN) = TENV2
  110       CONTINUE
C
C-----      Apply weathering correction (accumulation & decay) to leaves--
C
            T = GRWP(ITF) * YRDA
            CALL CHAIN (T, TENV, LEAF, LEAF, 1)
C
C-------    LEAF() now in concentration units
C
C---------- Calculate root contribution and plant concentration --------
C
            DO 120 IN = 1, NONUC
C
C------------ Calculate root uptake contribution (surface & deep soil)--
C
              ROOT1 = SOLAVG(IN,ITF) 
     .                * BVI(ITF,IN) * DRYFAC(ITF) * RF1  
C
C---------    Save uptake from soil for next year's harvest removal--
C
              IF (HARVST) THEN
                HARVT(IN,ITF) = (LEAF(IN) + ROOT1) * YELD(ITF)/SLDN
              ENDIF
C
C---------    Calculate total plant concentration--
C
              PLTCON(IN,ITF) = LEAF(IN) + ROOT1
              IF (DEBUG .AND. IN .EQ. 1) WRITE (*,*) 
     .          'LEAF/ROOT1/PLTCON:',
     .          ITF, LEAF(1),ROOT1, PLTCON(1,ITF)
  120       CONTINUE
C
C---------- Decay for holdup time between harvest and consumption ------
C
             T = HLDUP(ITF) * YRDA
             CALL CHAIN (T, DUMMY, PLTCON(1,ITF), PLTCON(1,ITF), 0)
C
C-------- Save concentration in each crop type --------------------
C
          DO 130 IN = 1, NONUC 
            EXPOS(IN,3+ITF) = PLTCON(IN,ITF) 
  130     CONTINUE
C
         ENDIF
  100 CONTINUE
C
      RETURN
      END   
C
C-----------------------------------------------------------------------
C
