C----------------------------------------VERSION: 15-Apr-06 ------------------
C
      SUBROUTINE ACUTEC (TPATH)

      USE DFLIB

C
C     This subroutine calculates exposure through ingestion of
C     terrestrial foods from an acute release for a given accident start time.
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Initial GENII Version: 2-Dec-88  RAP
C     Initial Reviewed and Approved: 12-Sept-88  BA Napier
C     Last Modification:  15-Apr-06  MAS
C     Last Review:  BAN 8 May 2006
C-----------------------------------------------------------------------
C 
C     CBARA()  - Time integrated plant concentration from air for each
C                radionuclide in the current chain, UoA-yr/kg
C     CBARI()  - Time-integrated plant concentration from irrigation
C                for each radionuclide in the current chain, UoA-yr/kg
C     CT0IRR() - Initial concentration in the surface soil from 
C                irrigation for each radionuclide in the current
C                chain, UoA/m2
C     FOODCN() - Concentration in food, UoA yr/kg 
C     ISEAS    - Index of the current season, where 1=winter, 2=spring,
C                3=summer, 4=autumn
C     LEAFCN() - Concentration in plant from leaf uptake for grains,
C                fruits, and root vegetables for each radionuclide in
C                the current chain, UoA/kg
C     LEAFDK   - Time to decay leaves of grains, fruits, and root 
C                vegetables, yr 
C     ROOTCN() - Concentration in plant from root uptake for each radio-
C                nuclide in the current chain, UoA/kg
C     SOLHAR() - Surface soil concentration at time of harvest, UoA/m2
C     T        - Time in adjusted units for CHAIN calls 
C     TENV2    - Weathering constant, yr-1
C     TIMHAR   - Time to harvest, yr
C     WATCON() - Time-integrated surface water concentration decayed for
C                transit to irrigation-withdrawal location, UoA yr/L
C	JHOUR    - Time of accident start from beginning of calendar year, hr
C
C-----------------------------------------------------------------------------
C  Modification History
C     Date    Who   Description of modifications
C  ---------  ---  -----------------------------------------------------------
C  22-Jul-97  DLS  Modified heading information
C  23-Aug-97  DLS  Modified to do autumn only and stop at concentration
C  30-Aug-97  DLS  Added rain deposition and calculation of wet deposition
C                  retention factor for irrigation (DEPFR2) and rain (DEPFRR)
C  27-Mar-98  BAN  additional decay step for Year 2+ to get to avg.
C  30-Oct-01  BAN  Put in a trap for zero RATEIRR or RAIN
C  15-Apr-06  MAS  Added time to harvest based on accident start time; return to
c                  GENII 1.485 seasonal structure.
C-----------------------------------------------------------------------------
C
C----- INCLUDE STATEMENTS ----------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
C      INCLUDE 'AFPPAR.CMN'
      INCLUDE 'AIRPAR.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'DECAY.CMN'
      INCLUDE 'FODPAR.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'SOLPAR.CMN' 
      INCLUDE 'SWPAR.CMN'
      INCLUDE 'TIMES.CMN'
C
C----- TYPE STATEMENTS -------------------------------------------------------
C
      REAL TIMHAR, LEAFDK, TENV2, TENV(9), SOLHAR(9),
     .     CT0IRR(9), CBARA(9), CBARI(9), WATCON(9),
     .     ROOTCN(9), LEAFCN(9), FOODCN(9)
      INTEGER TPATH, ISEAS, ISEASHR(8)
C      LOGICAL SWAT
C
C
C---- Determine season based on time of accident start (JHOUR) ---------------
C   Read from external file Season.Dat to get actual dates
C
      OPEN (UNIT=92,FILE='\FRAMES\SEASON.DAT',STATUS='OLD')
	READ (92,*) (ISEASHR(I), I=1,8)
	CLOSE (92)
C
C
      IF (JHOUR .gt. 0) THEN
C WINTER
	 IF (ISEASHR(1) .GT. ISEASHR(2)) THEN 
	  IF ((JHOUR.GE.1.AND.JHOUR .LE. ISEASHR(2)) .OR.
     .    (JHOUR .GE. ISEASHR(1) .AND. JHOUR .LE. 8760)) ISEAS = 1
	 ELSE
		IF (JHOUR .GE. ISEASHR(1) .AND. JHOUR .LE. ISEASHR(2)) ISEAS=1
	 END IF
C SPRING	
	 IF (ISEASHR(3) .GT. ISEASHR(4)) THEN 
	  IF ((JHOUR .GE. 1 .AND .JHOUR .LE. ISEASHR(4)) .OR.
     .    (JHOUR .GE. ISEASHR(3) .AND. JHOUR .LE. 8760)) ISEAS = 2
	 ELSE
		IF (JHOUR .GE. ISEASHR(3) .AND. JHOUR .LE. ISEASHR(4)) ISEAS=2
	 END IF
C SUMMER
	 IF (ISEASHR(5) .GT. ISEASHR(6)) THEN 
	  IF ((JHOUR .GE. 1 .AND. JHOUR .LE. ISEASHR(6)) .OR.
     .    (JHOUR .GE. ISEASHR(5).AND. JHOUR .LE. 8760)) ISEAS = 3
	 ELSE
		IF (JHOUR .GE. ISEASHR(5) .AND. JHOUR .LE. ISEASHR(6)) ISEAS=3
	 END IF
C AUTUMN
	 IF (ISEASHR(7) .GT. ISEASHR(8)) THEN 
	  IF ((JHOUR .GE. 1 .AND. JHOUR .LE. ISEASHR(8)) .OR.
     .    (JHOUR .GE. ISEASHR(7).AND. JHOUR .LE. 8760)) ISEAS = 4
	 ELSE
		IF (JHOUR .GE. ISEASHR(7) .AND. JHOUR .LE. ISEASHR(8)) ISEAS=4
	 END IF
C DEFAULT
	ELSE
	  ISEAS = 4
	ENDIF
C
C---- Set parameters based on season -----------------------------------------
C     Winter (ISEAS=1), Spring (2), Summer (3), Autumn (4)
C
      IF (ISEAS.EQ.1) THEN
	  DOIRR = .FALSE.
	  TENV2 = ALOG2 / WTIM / YRDA
	  LEAFDK = 0.0
C
	  IF (JHOUR.GE. ISEASHR(1)) THEN
	    TIMHAR = ((8760. - JHOUR) + ISEASHR(7)) / 8760.
	  ELSE
	    TIMHAR = (ISEASHR(7) - JHOUR) / 8760.
	  ENDIF
C
	ELSEIF (ISEAS.EQ.2) THEN
	  DOIRR = .TRUE.
	  TENV2 = ALOG2 / WTIM / YRDA
	  LEAFDK = 0.0
	  IF (JHOUR.GE. ISEASHR(1)) THEN
	    TIMHAR = ((8760. - JHOUR) + ISEASHR(7)) / 8760.
	  ELSE
	    TIMHAR = (ISEASHR(7) - JHOUR) / 8760.
	  ENDIF
C
	ELSEIF (ISEAS.EQ.3) THEN
	  DOIRR = .TRUE.
	  TENV2 = ALOG2 / WTIM / YRDA
	  LEAFDK = (ISEASHR(7) - JHOUR) / 8760.
	  IF (JHOUR.GE. ISEASHR(1)) THEN
	    LEAFDK = ((8760. - JHOUR) + ISEASHR(7)) / 8760.
	  ELSE
	    LEAFDK = (ISEASHR(7) - JHOUR) / 8760.
	  ENDIF
	  IF (JHOUR.GE. ISEASHR(1)) THEN
	    TIMHAR = ((8760. - JHOUR) + ISEASHR(7)) / 8760.
	  ELSE
	    TIMHAR = (ISEASHR(7) - JHOUR) / 8760.
	  ENDIF
C
	ELSEIF (ISEAS.EQ.4) THEN
	  DOIRR = .TRUE.
	  TENV2 = 0.0
	  LEAFDK = 0.0
	  TIMHAR = 0.0
C
	ENDIF
C
C----- Weathering constant changes by season ---------------------------------
C
      DO 100 IN = 1, NONUC
        TENV(IN) = TENV2
  100 CONTINUE
C
C----- For each terrestrial food pathway -------------------------------------
C
      DO 310 ITF = 1, NTF
        IF (TFD(ITF)) THEN
C
C-----    Irrigation deposition ----------------------------------------------
C         surface water only (TPATH = 2)
C
          IF (TPATH.EQ.2) THEN
            DO IN = 1,nonuc
               watcon(in) = swacut(in)
            END DO
            DO 320 IN = 1, NONUC
              CT0IRR(IN) = WATCON(IN) * RIRR(ITF)*LM2IN*IRTIMT(ITF)*MOYR
  320       CONTINUE
          ELSE
            DO 328 IN = 1, NONUC
              CT0IRR(IN) = 0.0
  328       CONTINUE
          ENDIF
C
C-----    Set surface soil concentration including air deposition ------------
C
          DO 330 IN = 1, NONUC
            SOLCON(IN,ITF) = (CT0IRR(IN) + DRYDEP(IN) + WETDEP(IN))
     .              / SLDN
  330     CONTINUE
C
C-----	Plant concentration from root uptake, except autumn ----------------
C

C		Autumn, no root uptake
		IF (ISEAS.EQ.4) THEN
		  DO 351 IN = 1, NONUC
			ROOTCN(IN) = 0.0
  351		  CONTINUE

C		Leafy vegetables, spring and summer
		ELSEIF (ITF.EQ.1.AND.(ISEAS.EQ.2.OR.ISEAS.EQ.3)) THEN
		  DO 353 IN = 1, NONUC
c			ROOTCN(IN) = SOLCON(IN,ITF) / SLDN
			ROOTCN(IN) = SOLCON(IN,ITF) 
     .					 * BVI(ITF,IN) * DRYFAC(ITF) * RF1
  353		  CONTINUE
		  CALL CHAIN ((1.0), DUMMY, ROOTCN, ROOTCN, 1)

C		Decay soil to harvest time
		ELSE
		  CALL CHAIN (TIMHAR, DUMMY, SOLCON(1,ITF), SOLHAR, 0)
		  DO 350 IN = 1,NONUC
c			ROOTCN(IN) = SOLHAR(IN) / SLDN
			ROOTCN(IN) = SOLHAR(IN)
     .					 * BVI(ITF,IN) * DRYFAC(ITF) * RF1
  350		  CONTINUE
		ENDIF
C
C-----    Decay crop soil to start of second year (rest handled in DKHARV) ---
C
          CALL CHAIN(1.0, LEACHR, SOLCON(1,ITF), SOLCON(1,ITF), 0)
C
C-----    Calculate irrigation interception fraction to all plants -----------
C             (DEPFR2)
C
          IF(.NOT.WETSET) THEN
            RATEIRR = RIRR(ITF) * MMIN / (IRTIMT(ITF) * DAYMO)
		  IF (RATEIRR .LE. 0.0) THEN
	        DEPFR2 = 0.0
	      ELSE
		    IF(.NOT.ANION(IN)) THEN           ! Cations/particles
                DEPFR2 = 2.95*BIOMAS(ITF)*DRYFAC(ITF)*RATEIRR**(-0.191)
              ELSE                           ! Anions
                DEPFR2 = 2.30*BIOMAS(ITF)*DRYFAC(ITF)*RATEIRR**(-0.92)
              ENDIF
	      ENDIF
            IF(DEPFR2.GT.1.) THEN
		    DEPFR2 = 1.0
		  ENDIF
          ENDIF ! End if on WETSET
C
C-----    Calculate rain interception fraction to all vegetable plants -------
C         (Calculate DEPFRR or use input wet deposition factor DEPFR2)
C
          DEPFRR = DEPFR2
          IF(.NOT.WETSET) THEN
            IF (RAIN .LE. 0.0) THEN
	        DEPFRR = 0.0
	      ELSE
		    IF(.NOT.ANION(IN)) THEN           ! Cations/particles
                DEPFRR = 2.95*BIOMAS(ITF)*DRYFAC(ITF)*RAIN**(-0.191)
              ELSE                              ! Anions
                DEPFRR = 2.30*BIOMAS(ITF)*DRYFAC(ITF)*RAIN**(-0.92)
              ENDIF
	      END IF 
            IF(DEPFRR.GT.1.) THEN
		    DEPFRR = 1.0
	      ENDIF
          ENDIF
C
          IF (ITF .EQ. 1) THEN       ! Leafy vegetables
C
C-----    Leaf uptake, leafy vegetables --------------------------------------
C         Calculate once for all plant types
C
C-----    Calculate air interception fraction to leafy vegetable plants
C
            IF(.NOT.DRYSET) THEN
              DEPFR1 = 1.0 - EXP (-2.9 * BIOMAS(ITF) * DRYFAC(ITF))
            ENDIF
C
C-----    Sum contributions to leafy vegetables from air, rain, and irrigation 
C
            DO 340 IN = 1, NONUC
		    IF (ISEAS.EQ.1) THEN
			  LEAFCN(IN) = 0.0
		    ELSE
                CBARA(IN) = DRYDEP(IN) * DEPFR1 + WETDEP(IN) * DEPFRR
                CBARI(IN) = CT0IRR(IN) * DEPFR2
                LEAFCN(IN) = (CBARA(IN) + CBARI(IN))
     .                       * TRANS(ITF) / BIOMAS(ITF)
			ENDIF
  340       CONTINUE
C
C-----    Autumn, Spring, or Summer integral over one year -------------------
C
	      IF (ISEAS.GE.2) THEN
	        CALL CHAIN ((1.0),TENV, LEAFCN, LEAFCN, 1)
	      ENDIF
          ELSE
C
C-----    Leaves of grain, fruits, root vegetables ---------------------------
C         Calculate DEPFR1 interception fraction from air and DEPFR2,
C         interception fraction from irrigation
C
	      IF (ISEAS.GT.2) THEN
              IF(.NOT.DRYSET) THEN
                IF (ITF .EQ. 4) THEN
                  DEPFR1 = 1.0 - EXP (-2.9 * BIOMAS(ITF) * DRYFAC(ITF))
                ELSE
                  DEPFR1 = 1.0 - EXP (-3.6 * BIOMAS(ITF) * DRYFAC(ITF))
                ENDIF
	        ENDIF
C         depfr2 calculation to plant type (ITF = 2,3, or 4.  No leafy V. here).
              DO 352 IN = 1, NONUC
                LEAFCN(IN) = DRYDEP(IN) * DEPFR1 + WETDEP(IN) * DEPFRR
                LEAFCN(IN) = (LEAFCN(IN) +  CT0IRR(IN) * DEPFR2) 
     .                       / BIOMAS(ITF) * TRANS(ITF)
  352         CONTINUE

              IF (ISEAS.EQ.3) THEN
	          CALL CHAIN (LEAFDK, TENV, LEAFCN, LEAFCN, 0)
	        ENDIF
	      ELSE
	        DO 354 IN = 1, NONUC
	          LEAFCN(IN) = 0.0
  354	        CONTINUE
            ENDIF
          ENDIF
C
C-----    Total food concentration -------------------------------------------
C
          DO 360 IN = 1, NONUC
            FOODCN(IN) = ROOTCN(IN) + LEAFCN(IN)
  360     CONTINUE
C
C-----    Holdup between harvest and consumption -----------------------------
C
          T = HLDUP(ITF) * YRDA
          CALL CHAIN (T, DUMMY, FOODCN, FOODCN, 0)
C
C-----    Human Consumption Integral -----------------------------------------
C
          IF (ITF.GT.1.OR.ISEAS.EQ.1) THEN   ! all non-leafy vegetables and Winter leafy
            T = 1.0
            CALL CHAIN (T, DUMMY, FOODCN, FOODCN, 1)
          ENDIF
          DO 362 IN = 1, NONUC
C
C-----    Human uptake from foods -------------------------------------------
C
            EXPOS(IN,ITF+3) = FOODCN(IN)
  362     CONTINUE
        ENDIF   !  End of IF on inclusion of plant type
  310 CONTINUE  !  End of loop on plant type
      RETURN
C
C----- END OF MODULE ACUTEC -------------------------------------------------
C
      END   
