C------------------------------------- Version: 8-Mar-07 --------------
C
      SUBROUTINE ACUTEA ()
C
C     This subroutine calculates exposure through ingestion of
C     animal products from an acute release for a given accident start time.
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Initial GENII Version:  2-Dec-88  RAP
C     Initial Reviewed and Approved:  12-Sept-88  BA Napier
C     Last Modification:  8-Mar-07  BAN
C-----------------------------------------------------------------------
C 
C     ANIMCN   - Animal product concentration from forage, feed, and
C                drinking water, UoA-yr/kg and UoA/kg
C     CBARA()  - Time integrated forage concentration from air for each
C                radionuclide in the current chain, UoA-yr/kg
C     CBARI()  - Time-integrated forage concentration from irrigation
C                for each radionuclide in the current chain, UoA-yr/kg
C     CT0IRR() - Initial concentration in the surface soil from 
C                irrigation for each radionuclide in the current
C                chain, UoA/m2
C     DW       - Time-integrated animal product concentration resulting 
C                from ingestion of drinking water, UoA-yr/kg     
C     FORAGE() - Time-integrated animal product concentration from
C                consumption of fresh forage of meat,(5), and milk
C                cows,(6), UoA-yr/kg, for each radionuclide in the 
C                current chain 
C     GRAIN()  - Animal product concentration from ingestion of grain,
C                UoA/kg
C     ISEAS    - Index of the current season, where 1=winter, 2=spring
C                3=summer, 4=autumn
C     LEAFCN() - Concentration in plant from leaf uptake for grains,
C                for each radionuclide in the current chain, UoA/kg
C     LEAFDK   - Time to decay leaves of grains, yr 
C     PWTAIR() - Average population-weighted air concnetration for each
C                radionuclide in the current chain, UoA-sec/m3 
C     ROOTCN() - Concentration in grains from root uptake for each radio-
C                nuclide in the current chain, UoA/kg
C     SOLHAR() - Surface soil concentration at time of harvest, UoA/m2
C     TENV2    - Weathering constant, yr-1
C     TIMHAR   - Time to harvest, yr
C     WATCON() - Time-integrated surface water concentration decayed for
C                transit time to irrigation-withdrawal location, UoA yr/L    
C     JHOUR    - Time of accident start from beginning of calendar year, hr
C
C-----------------------------------------------------------------------------
C   Modification History
C     Date     Who  Modification description
C  ---------  ---  -----------------------------------------------------------
C  22-Jul-97  DLS  Updated heading and comment information
C  23-Aug-97  DLS  Revised to do analysis only for autumn, and stop at
C                  concentration in animal products
C  23-Mar-98  BAN  Removed include of ENVPAR cmn
c  26-Mar-98  BAN  Added 1 year of decay for SL2CON, parallel to ACUTE1
C  27-Mar-98  BAN  Took it out, then put it back again - Year 2+ average
C  14-Sep-99  BAN  Added soil consumption by animals
C  01-Apr-00  BAN  Corrected last holdup decay step
C  30-Oct-01  BAN  Put in a trap for zero RATEIRR or RAIN
C  27-Apr-06  MAS  Added time to harvest based on accident start time; return to 
C                  GENII 1.485 seasonal structure
C  8-Mar-07    BAN  Add Dale Dexheimer's suggestion for Autumn Continuous Forage
c                  option; also remove FRACUT which was included in error
C-----------------------------------------------------------------------------
C
C---- INCLUDE STATEMENTS -----------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
C      INCLUDE 'AFPPAR.CMN'
      INCLUDE 'AIRPAR.CMN'
      INCLUDE 'ANMPAR.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'DECAY.CMN'
C      INCLUDE 'ENVPAR.CMN'
      INCLUDE 'FODPAR.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'SOLPAR.CMN' 
      INCLUDE 'SWPAR.CMN'
      INCLUDE 'TIMES.CMN'
C
C---- TYPE and Dimension STATEMENTS -----------------------------------------
C
      REAL TIMHAR, LEAFDK, TENV2, TENV(9), SOLHAR(9), ROOTCN(9),
     .	 CT0IRR(9), CBARA(9), CBARI(9), WATCON(9), SLA(4),
     .     ANIMCN(9), LEAFCN(9), FORAGE(9,6), DW, GRAIN(9), FR,
c some new parameters for autumn fresh forage
     .     leafcna(9), rootcna(9), tenva(9)
	INTEGER ISEAS, ISEASHR(8)
c new for fresh autumn forage
           facfor = 0.
	     tenv2a = 0.
	     do jkl = 1,9
	     leafcna(jkl) = 0.
	     rootcna(jkl) = 0.
	     tenva(jkl) = 0.
	     end do
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
        tenva(in)= tenv2a
  100 CONTINUE
C
C---- Irrigation deposition from surface water only
C
      DO IN = 1,NONUC
        WATCON(IN) = SWACUT(IN)
      END DO
C
C---- First do fresh forage --------------------------------------------
C
      DO 400 IAN = 5, 6
        IF ((IAN.EQ.5 .AND. ANF(1)) .OR. (IAN.EQ.6 .AND. ANF(3))) THEN
          DO 320 IN = 1, NONUC
C
C--------  Irrigation deposition--
C
            IF (IRRSA(IAN).EQ.2) THEN
C              CT0IRR(IN) = WATCON(IN)*RIRRA(IAN)*LM2IN*IRTIMA(ian)*MOYR
              CT0IRR(IN) = WATCON(IN)*RIRRA(IAN)*LM2IN
            ELSE
              CT0IRR(IN) = 0.0
            ENDIF
C
C---------- Set surface soil concentration -----------------------------------
C
            SL2CON(IN,IAN) = (DRYDEP(IN) + WETDEP(IN) + CT0IRR(IN))
     .            / SLDN
  320     CONTINUE
C
C-----	Plant concentration from root uptake, except autumn ----------------
C

C		Autumn, no root uptake
		IF (ISEAS.EQ.4) THEN
		  DO 322 IN = 1, NONUC
			ROOTCN(IN) = 0.0
c new for autumn fresh forage
			rootcna(in) = SL2CON(IN,IAN)
     .					 * BVI(3,IN) * DRYFA2(IAN) * RF1
  322		  CONTINUE
              call chain((1.0),Dummy,rootcna,rootcna,1)

C		Spring or summer
		ELSEIF (ISEAS.EQ.2.OR.ISEAS.EQ.3) THEN
		  DO 324 IN = 1, NONUC
c			ROOTCN(IN) = SL2CON(IN,IAN) / SLDN
			ROOTCN(IN) = SL2CON(IN,IAN)
     .					 * BVI(3,IN) * DRYFA2(IAN) * RF1
  324		  CONTINUE
		  CALL CHAIN ((1.0), DUMMY, ROOTCN, ROOTCN, 1)

C		Winter, decay soil to harvest time
		ELSE
		  CALL CHAIN (TIMHAR, DUMMY, SL2CON(1,IAN), SOLHAR, 0)
		  DO 326 IN = 1,NONUC
c			ROOTCN(IN) = SOLHAR(IN) / SLDN
			ROOTCN(IN) = SOLHAR(IN)
     .					 * BVI(3,IN) * DRYFA2(IAN) * RF1
  326		  CONTINUE
C**
             CALL CHAIN ((1.0),DUMMY,ROOTCN,ROOTCN,1)
c**
		ENDIF
C
C-----    Decay SL2CON soils to start of 2nd year (rest handled in DKHARV)
C
          CALL CHAIN(1.0, LEACHR, SL2CON(1,IAN), SL2CON(1,IAN),0)
C
C-----    Fresh forage leaves, except for winter -----------------------------
C
          IF (ISEAS.GE.2) THEN
C
C          Set interception fraction to leaves from air deposition
C
            IF(.NOT.DRYSET) THEN
              DEPFR1 = 1.0 - EXP (-2.9 * BIOMA2(IAN) * DRYFA2(IAN))
            ENDIF
C
C          Set interception fraction to leaves from irrigation deposition
C
            IF(.NOT.WETSET) THEN
              RATEIRR = RIRRA(IAN) * MMIN / (IRTIMA(IAN) * DAYMO)
               IF (RATEIRR .LE. 0.0) THEN
	         DEPFR2 = 0.0
	         ELSE
			IF(.NOT.ANION(IN)) THEN           ! Cations/particles
                DEPFR2 =2.95*BIOMA2(IAN)*DRYFA2(IAN)*RATEIRR**(-0.191)
              ELSE                           ! Anions
                DEPFR2 = 2.30*BIOMA2(IAN)*DRYFA2(IAN)*RATEIRR**(-0.92)
              ENDIF
	         END IF
              IF(DEPFR2.GT.1.) DEPFR2 = 1.0
            ENDIF ! End if on WETSET
C
C----- Set interception fraction for rain ------------------------------------
C
            DEPFRR = DEPFR2
            IF(.NOT.WETSET) THEN
               IF (RAIN .LE. 0.0) THEN
	         DEPFRR = 0.0
	         ELSE		
			IF(.NOT.ANION(IN)) THEN           ! Cations/particles
                 DEPFRR =2.95*BIOMA2(IAN)*DRYFA2(IAN)*RAIN**(-0.191)
              ELSE                           ! Anions
                 DEPFRR = 2.30*BIOMA2(IAN)*DRYFA2(IAN)*RAIN**(-0.92)
              ENDIF
	         END IF
              IF(DEPFRR.GT.1.) DEPFRR = 1.0
            ENDIF
C
C----- Sum contribution to leaf concentration from air, irrigation, and rain
C
            DO 340 IN = 1, NONUC
              CBARA(IN) = DRYDEP(IN) * DEPFR1 + WETDEP(IN) * DEPFRR 
              CBARI(IN) = CT0IRR(IN) * DEPFR2 
              IF (BIOMA2(IAN) .GT. 0.0) THEN
                LEAFCN(IN) = (CBARA(IN) + CBARI(IN)) * TRANSA(IAN) 
     .                       / BIOMA2(IAN) 
              ELSE
                LEAFCN(IN) = 0.0
              ENDIF
  340       CONTINUE
C
C----- Spring, or Summer integral over one year with weathering----------------
C----- Autumn integral without weathering
C
           IF(ISEAS .EQ. 4) THEN
c  new for autumn fresh forage
              do jkl = 1,9 
              leafcna(jkl)=leafcn(jkl)
	        end do
		  call chain ((1.0), tenva,leafcna, leafcna, 1)
            CALL CHAIN ((1.0), DUMMY, LEAFCN, LEAFCN, 1)
           ELSE
		  CALL CHAIN ((1.0), TENV, LEAFCN, LEAFCN, 1)
           ENDIF
          ELSE
		  DO 342 IN = 1, NONUC
			LEAFCN(IN) = 0.0
  342		  CONTINUE
		ENDIF
 
          IF(IAN.EQ.5) THEN
c		  FACUTE = FRACUT(1)
c new for autumn fresh forage
            facfor = franfr(1)	    
		ELSEIF (IAN.EQ.6) THEN
c		  FACUTE = FRACUT(3)
	      facfor = franfr(3)
          ELSE
c		  FACUTE = 0.
	      facfor = 0.
		ENDIF

		DO 344 IN = 1, NONUC
c  adjusted for autumn fresh forage
            FORAGE(IN,IAN)=((1.-facfor)* LEAFCN(IN)+facfor*leafcna(in)+
     .       (1.-facfor) * ROOTCN(IN)+
     .       facfor * rootcna(in)) 
c     .       * FACUTE :This was an unneeded addition/reversion
  344     CONTINUE  
        ENDIF
  400 CONTINUE
C
C-------- Next do stored feed and grain --------------------------------
C
      DO 310 IAN = 1, NAN
        IF (ANF(IAN)) THEN
          DO 350 IN = 1, NONUC
            IF (IRRSA(IAN).EQ.2 .AND. DOIRR) THEN
              CT0IRR(IN) = WATCON(IN) * RIRRA(IAN) * LM2IN
            ELSE
              CT0IRR(IN) = 0.0
            ENDIF
  350     CONTINUE
C
C-----    Set surface soil concentration -------------------------------------
C
          DO IN = 1, NONUC
            SL2CON(IN,IAN) = (WETDEP(IN) + DRYDEP(IN) + CT0IRR(IN))
     .           / SLDN 
C---    Retain briefly for animal soil consumption----------------------------
	      SLA(IN) = SL2CON(IN,IAN)   
          END DO  
C
C-----	Plant concentration from root uptake, except autumn ----------------
C
C		Autumn, no root uptake
		IF (ISEAS.EQ.4) THEN
		  DO 352 IN = 1, NONUC
			ROOTCN(IN) = 0.0
  352		  CONTINUE

C		Winter, spring, or autumn decay soil to harvest time
		ELSE
		  CALL CHAIN (TIMHAR, DUMMY, SL2CON(1,IAN), SOLHAR, 0)
		  DO 354 IN = 1,NONUC
c			ROOTCN(IN) = SOLHAR(IN) / SLDN
			ROOTCN(IN) = SOLHAR(IN)
     .					 * BVI(3,IN) * DRYFA2(IAN) * RF1
  354		  CONTINUE
		ENDIF
C
C-----    Decay SL2CON soils to start of 2nd year (rest handled in DKHARV)
C-----    Integrate animal soil concentration for 1 year for soil consumption
C
          CALL CHAIN(1.0, LEACHR, SL2CON(1,IAN), SL2CON(1,IAN),0)
	    CALL CHAIN(1.0, LEACHR, SLA(1), SLA(1), 1)
C
C-----    Set interception fraction to leaves from irigation deposition
C
          IF(.NOT.WETSET) THEN
            RATEIRR = RIRRA(IAN) * MMIN / (IRTIMA(IAN) * DAYMO)
            IF (RATEIRR .LE. 0.0) THEN
	        DEPFR2 = 0.0
	      ELSE
		    IF(.NOT.ANION(IN)) THEN           ! Cations/particles
                DEPFR2 = 2.95*BIOMA2(IAN)*DRYFA2(IAN)*RATEIRR**(-0.191)
              ELSE                           ! Anions
                DEPFR2 = 2.30*BIOMA2(IAN)*DRYFA2(IAN)*RATEIRR**(-0.92)
              ENDIF
	      END IF
            IF(DEPFR2.GT.1.) THEN
		    DEPFR2 = 1.0
		  ENDIF
          ENDIF ! End if on WETSET
C
C----- Set interception fraction for rain ------------------------------------
C
          DEPFRR = DEPFR2
          IF(.NOT.WETSET) THEN
	      IF(RAIN .LE. 0.0) THEN
	        DEPFRR = 0.0
	      ELSE
              IF(.NOT.ANION(IN)) THEN           ! Cations/particles
                DEPFRR = 2.95*BIOMA2(IAN)*DRYFA2(IAN)*RAIN**(-0.191)
              ELSE                           ! Anions
                DEPFRR = 2.30*BIOMA2(IAN)*DRYFA2(IAN)*RAIN**(-0.92)
              ENDIF
	      END IF
            IF(DEPFRR.GT.1.) THEN
		    DEPFRR = 1.0
		  ENDIF
          ENDIF
C
C-----    Leaf uptake --------------------------------------------------------
C
          DO IN = 1, NONUC  
            LEAFCN(IN) = 0.0
          END DO  
C
C-----    Interception fraction for air and irrigation deposition ------------
C		Summer or autumn
C
		IF (ISEAS.GT.2) THEN
            IF(.NOT.DRYSET) THEN
              DEPFR1 = 1.0 - EXP (-2.9 * BIOMA2(IAN) * DRYFA2(IAN))
            ENDIF
C
C-----    Calculate leaf concentration ---------------------------------------
C
            DO IN = 1, NONUC
              IF (BIOMA2(IAN) .GT. 0.0) THEN
                LEAFCN(IN) = (DRYDEP(IN) * DEPFR1 + WETDEP(IN) * DEPFRR
     .                       +  CT0IRR(IN) * DEPFR2) 
     .                       * TRANSA(IAN) / BIOMA2(IAN)
              ENDIF
            END DO  
		  IF (ISEAS.EQ.3) THEN
			CALL CHAIN (LEAFDK, TENV, LEAFCN, LEAFCN, 0)
		  ENDIF
		ENDIF

C
C----     Set grain concentration --------------------------------------------
C
          DO 356 IN = 1, NONUC
            GRAIN(IN) = (ROOTCN(IN) + LEAFCN(IN))
  356     CONTINUE
C
C-----    Time integral of grain concentration over one year -----------------
C
          CALL CHAIN ((1.0), DUMMY, GRAIN, GRAIN, 1)
C
C-------- Total --------------------------------------------------------------
C
          DO 358 IN = 1, NONUC
C
C-----      Animal consumption of drinking water -----------------------------
C
            DW = WATCON(IN) * DWATER(IAN) * DWFACA(IAN)
	      SL = SLA(IN) * SLCONA(IAN)
C
C---------- Total animal product concentration--
C
            ANIMCN(in) = GRAIN(IN) * CONSUM(IAN) + DW + SL
            IF (IAN .EQ. 1) ANIMCN(in) = ANIMCN(in) + 
     .          FORAGE(IN,5) * CONSUM(5)
            IF (IAN .EQ. 3) ANIMCN(in) = ANIMCN(in)    
     .          + FORAGE(IN,6) * CONSUM(6)
            ANIMCN(in) = ANIMCN(in) * FMI(IAN,IN) 
  358     Continue
C
C---------- Holdup between harvest and consumption--
C
            T = HLDUPA(IAN) * YRDA
            CALL CHAIN (T, DUMMY, ANIMCN, ANIMCN, 0)
C
C---------- Human consumption of animal products--
C
          DO 359 in=1,nonuc
            EXPOS(IN,7+IAN) = ANIMCN(in)          
  359     CONTINUE
        ENDIF
  310 CONTINUE
C
      RETURN 
C
C---- END OF MODULE ACUTEA --------------------------------------------------
C
      END
