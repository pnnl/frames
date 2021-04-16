C-------------------------------------- Version: 24-Feb-1998 ------------
C
C      SUBROUTINE CRPCAL
C
C     This subroutine calculates plant concentrations from air, soil, and
C     irrigation pathways to terrestrial foods.
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Initial GENII Version:  24-Apr-90 RAP 
C     Initial Reviewed and Approved: 12-Sept-88  BA Napier
C     Last Modification:  24-Feb-98 BAN 
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
C     RELFLG    - Flag set false if during prior period, true if during intake
C-----------------------------------------------------------------------------
C    Modification History
C  Date      Who   Modification summary
C  ---------  ---  ------
C  30-Aug-97  DLS  Revised equation for particalute deposition (ANION = .false.)
C  24-Feb-98  BAN  Units correction
C-----------------------------------------------------------------------------
C
      SUBROUTINE CRPCAL(RELFLG,TPATH)
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'CONC.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'FODPAR.CMN' 
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'SOLPAR.CMN'
C
C----- Type Statments --------------------------------------------------------
C
      REAL TENV(LENCHAIN), LEAF(LENCHAIN), LEAF1A, LEAF1B, LEAF2,
     .     WATCON
      LOGICAL RELFLG
      INTEGER TPATH
C
C---- Calculate weathering constant ------------------------------------------
C      
      TENV2 =  ALOG2 / WTIM / YRDA
      IF (DEBUG) WRITE (NELS,*) 'Weathering (TENV2): ', TENV2
C
C----- Process each selected terrestrial food type ---------------------------
C
      DO 100 ITF = 1, NTF
        IF (TFD(ITF)) THEN
C
C---------- Calculate contribution from leaves -------------------------------
C
            IF(.NOT.DRYSET) THEN
              IF (ITF .EQ. 2 .OR. ITF .EQ. 3) THEN
                DEPFR1 = 1.0 - EXP (-3.6 * BIOMAS(ITF) * DRYFAC(ITF))
              ELSE
                DEPFR1 = 1.0 - EXP (-2.9 * BIOMAS(ITF) * DRYFAC(ITF))
              ENDIF
            ENDIF
            DO 110 IN = 1, NONUC
c
	        Leaf1 = 0.
	        Leaf1a = 0.
	        Leaf2 = 0.
c
             IF(TPATH.EQ.3) THEN
c
C----- Calculate leaf deposition rate from air -------------------------------
c
              LEAF1A = DRYDEP(IN) * DEPFR1
c         
C----- Calculate leaf deposition rate from rain ------------------------------
c
              LEAF2 = 0.0
              IF(.NOT.WETSET) THEN
                IF(.NOT.ANION(IN)) THEN           ! Cations/particles
                  DEPFR2 =2.95*BIOMAS(ITF)*DRYFAC(ITF)*RAIN**(-0.191)
                ELSE                           ! Anions
                  DEPFR2 = 2.30*BIOMAS(ITF)*DRYFAC(ITF)*RAIN**(-0.92)
                ENDIF
                IF(DEPFR2.GT.1.) DEPFR2 = 1.0
              ENDIF
              LEAF1A = LEAF1A + WETDEP(IN) * DEPFR2
             END IF
C
C----- Calculate leaf depositon rate from resuspension -----------------------
c
              LEAF1B = SOLAVG(IN,ITF) * LEAFRS * DPVRES * SECYR
     .                  * DEPFR1 * SLDN
C
C----- Calculate leaf deposition rate from irrigation ------------------------
C
              IF (TPATH.LE.2.AND.IRRSr .GT. 0) THEN
                IF(.NOT.WETSET) THEN
                RATEIRR = RIRRr * MMIN / (IRTIMr * DAYMO)
                 IF(.NOT.ANION(IN)) THEN           ! Cations/particles
	            IF(RATEIRR .eq. 0.0) THEN
	              DEPFR2 = 0.0
	            ELSE
                  DEPFR2 =2.95*BIOMAS(ITF)*DRYFAC(ITF)*RATEIRR**(-0.191)
	            END IF
                 ELSE  
	            IF(RATEIRR .eq. 0.0) THEN
	              DEPFR2 = 0.0
	            ELSE			                            ! Anions
                  DEPFR2 = 2.30*BIOMAS(ITF)*DRYFAC(ITF)*RATEIRR**(-0.92)
	            END IF
                 ENDIF
                IF(DEPFR2.GT.1.) DEPFR2 = 1.0
                ENDIF ! End if on WETSET
                  WATCON = SWCON(IN)             ! Surface water source
                  LEAF2 = WATCON * RIRRR * LM2IN / IRTIMR 
     .                    * MOYR * DEPFR2
              ENDIF
c          
C----- Calculate total leaf deposition rate ----------------------------------
c
              LEAF(IN) = 0.0
              IF (BIOMAS(ITF) .GT. 0.0) 
     .          LEAF(IN) = (LEAF1a + LEAF1b + LEAF2) 
     .                     * TRANS(ITF) / BIOMAS(ITF) 
c
              IF (DEBUG .AND. IN .EQ. 1) WRITE (NELS,*) 
     .          'LEAF1a/LEAF1b/LEAF2/LEAF:',LEAF1A,LEAF1B,LEAF2,LEAF(1)
c
C----- Set weathering constant for each radionuclide -------------------------
c
              TENV(IN) = TENV2
c
  110       CONTINUE
c
C----- Apply weathering correction, accumulation, & decay to leaves ----------
c
            T = GRWP(ITF) * YRDA
            CALL CHAIN (T, TENV, LEAF, LEAF, 1)
C---  LEAF() now in concentration units
c
C----- Calculate root contribution and plant concentration -------------------
c
            DO 120 IN = 1, NONUC
c
C----- Calculate root uptake contribution ------------------------------------
c
              ROOT1 = SOLavg(IN,ITF) 
     .                * BVI(ITF,IN) * DRYFAC(ITF) * RF1  
c
C----- Calculate total plant concentration -----------------------------------
c
             IF(RELFLG) THEN
              PLTCON(IN,ITF) = LEAF(IN) + ROOT1 
              IF (DEBUG .AND. IN .EQ. 1) WRITE (NELS,*) 
     .          'LEAF/ROOT1/PLTCON:',
     .          ITF, LEAF(1),ROOT1, PLTCON(1,ITF)
             ENDIF
  120       CONTINUE
          IF(RELFLG) THEN
C
C---------- Decay for holdup time between harvest and consumption ------
C
C
C-------- Save concentration in each crop at consumption ---------------------
c
          DO 130 IN = 1, NONUC 
            EXPOS(IN,3+ITF) = PLTCON(IN,ITF)
  130     CONTINUE

         ENDIF
c
        ENDIF
  100 CONTINUE
c
      RETURN
c
C----- END OF MODULE CRPCAL --------------------------------------------------
c
      END

