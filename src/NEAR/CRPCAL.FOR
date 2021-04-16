C------------------------------------ Version: 20-Feb-98 ---------------------
C
C     SUBROUTINE CRPCAL
C
C     This subroutine calculates human uptake rate from consumption of 
C     terrestrial foods. 
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Initial GENII Version:  24-Apr-90  RAP 
C     Reviewed and Approved: 12-Sept-88  BA Napier
C     Last Modification:     20-Feb-98  BAN     
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
C---- Modification History ---------------------------------------------------
C    Date     Who  Modification Summary
C  ---------  ---  -----------------------------------------------------------
C  09-Sep-97  DLS  Initial modification of old GENII
C  19-Jan-98  DLS  Removed soil density SSLD from the root equation
C  12-Feb-98  BAN  Conversion to use of annual average soil concs.
C  20-Feb-98  BAN  Adjust for DEPFR input and HARVT units
C-----------------------------------------------------------------------------
C
      SUBROUTINE CRPCAL
C
C---- Include Statements -----------------------------------------------------
C
      INCLUDE 'AFPPAR.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'FODPAR.CMN' 
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'SOLPAR.CMN'
C
C---- Type/Dimension Statements ----------------------------------------------
C
      REAL TENV(9), LEAF(9),          LEAF1B
C      REAL TENV(9), LEAF(9), LEAF1A, LEAF1B, LEAF2, WATCON
C
C---- Calculate weathering constant ------------------------------------------
C     
      TENV2 =  ALOG2 / WTIM / YRDA
      IF (DEBUG) WRITE (*,*) 'Weathering (TENV2): ', TENV2
C
C---- Process each selected terrestrial food type ----------------------------
C
      DO 100 ITF = 1, NTF
        IF (TFD(ITF)) THEN
C
C---------- Calculate contribution from leaves -------------------------------
C
           IF ( .NOT. DRYSET ) THEN
            IF (ITF .EQ. 2 .OR. ITF .EQ. 3) THEN
              DEPFR1 = 1.0 - EXP (-3.6 * BIOMAS(ITF) * DRYFAC(ITF))
            ELSE
              DEPFR1 = 1.0 - EXP (-2.9 * BIOMAS(ITF) * DRYFAC(ITF))
            ENDIF
           ENDIF
            DO 110 IN = 1, NONUC
C
C-----        Calculate leaf depositon rate from resuspension ----------------
C
             LEAF1B = SOLAVG(IN,ITF) * LEAFRS * DPVRES * SECYR
     .               * DEPFR1 * SLDN
C         
C-----        Calculate total leaf deposition rate ---------------------------
C
              LEAF(IN) = 0.0
              IF (BIOMAS(ITF) .GT. 0.0) 
     .          LEAF(IN) = (         LEAF1B        ) 
     .                     * TRANS(ITF) / BIOMAS(ITF) 
C    .          LEAF(IN) = (LEAF1a + LEAF1b + LEAF2) 
C    .                     * TRANS(ITF) / BIOMAS(ITF) 
C
              IF (DEBUG .AND. IN .EQ. 1) WRITE (*,*) 
     .          'LEAF1B/LEAF:',LEAF1B,LEAF(1)
C
C--------        Set weathering constant for each radionuclide ---------------
C
              TENV(IN) = TENV2
  110       CONTINUE
C
C------     Apply weathering correction (accumulation & decay) to leaves -----
C
            T = GRWP(ITF) * YRDA
            CALL CHAIN (T, TENV, LEAF, LEAF, 1)
C           LEAF() now in concentration units
C
C---------- Calculate root contribution and plant concentration --------------
C
            DO 120 IN = 1, NONUC
C
C-----        Calculate root uptake contribution (surface & deep soil) -------
C
              ROOT1 = SOLAVG(IN,ITF) 
     .                * BVI(ITF,IN) * DRYFAC(ITF) * RF1  
              ROOT2 = 0.0
              IF (DEEP) 
     .          ROOT2 = DSCAVG(IN,ITF)         
     .                  * BVI(ITF,IN) * DRYFAC(ITF)* RF2
C
C-----        Save uptake from soil for harvest removal ----------
C
              IF (HARVST) THEN
                HARVT(IN,ITF,1) = (LEAF(IN) + ROOT1) * YELD(ITF)
     .                             / SLDN
                IF (DEEP .AND. RF2 .GT. 0.0) THEN
                  HARVT(IN,ITF,2) = ROOT2 * YELD(ITF) / WASDEP
     .                              / SSLDN
                ELSE
                  HARVT(IN,ITF,2) = 0.0
                ENDIF
              ENDIF
C
C-----        Calculate total plant concentration ----------------------------
C
              PLTCON(IN,ITF) = LEAF(IN) + ROOT1 + ROOT2
C
              IF (DEBUG .AND. IN .EQ. 1) WRITE (*,*) 
     .          'LEAF/ROOT1/ROOT2/PLTCON:',
     .          ITF, LEAF(1),ROOT1,ROOT2, PLTCON(1,ITF)
  120       CONTINUE
C
C---------- Decay for holdup time between harvest and consumption ------------
C
            T = HLDUP(ITF) * YRDA
            CALL CHAIN (T, DUMMY, PLTCON(1,ITF), PLTCON(1,ITF), 0)
C
C-------- Calculate human uptake from crops ----------------------------------
C
          DO IN = 1, NONUC 
            EXPOS(IN,3+ITF) = PLTCON(IN,ITF)
          END DO 
C
        ENDIF
  100 CONTINUE
      RETURN
      END
C
C---- End of Module CRPCAL ---------------------------------------------------
C
