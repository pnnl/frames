C------------------------------------ Version: 11 Nov 2001 -------------------
C
C     SUBROUTINE EDRANM
C
C     This subroutine calculates radionuclide concentrations in the 
C     surface soil (top 15 cm) and deep soil (available for uptake) 
C     for the animal foods pathways for a single radionuclide decay 
C     chain. Processes considered: leaching, and biotic transport.  
C
C     NEAR FIELD MODULE
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Initial Version: 14-Jan-88  RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C     Last Modification: 11 Nov 2001  BAN
C
C-----------------------------------------------------------------------
C 
C     ANFLG    - Option flag for each of the animal fodder types
C     IBV      - Index of BVI value to use, either 1-fresh, 4-stored;
C                soil-to-plant transfer factor for fodder, corresponds
C                to leafy vegetables and grains of terrestrial plants  
C     RTATM    - Atmospheric removal rate, UoA/m2-yr
C     RTBIO()  - Biotic transport removal rate, UoA/m2-yr
C     RTIRR    - Irrigation removal rate, UoA/m2-yr
C     WATCON   - Irrigation water concentration of current radionuclide
C                chain member, UoA/l
C
C---- Modification History ---------------------------------------------------
C   Date      Who  Modification Summary
C  ---------  ---  -----------------------------------------------------------
C  08-Sep-97  DLS  Initial modification of old GENII
C  12-Feb-98  BAN  Conversion to use of annual average soil concs.
C  20-Feb-98  BAN  Add dry/wet to YELDA
c  11 NOV 01  BAN  Trap CHAIN double integral failure at small lambdas
C-----------------------------------------------------------------------------
C   Subroutine Call Statement
C
      SUBROUTINE EDRANM
C
C---- Include Statements -----------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'ANMPAR.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'FODPAR.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'SOLPAR.CMN'
	INCLUDE 'DECAY.CMN' 
C
C---- Type/dimension Statements ----------------------------------------------
C
      REAL RTBIO(LENCHAIN), SL2OUT(LENCHAIN), DS2OUT(LENCHAIN)
C     REAL RTIRR, RTATM, RTBIO(LENCHAIN), WATCON
      LOGICAL ANFLG
      INTEGER IBV
C
C---- Start of Analysis ------------------------------------------------------
C
      DO 410 IAN = 1, NAN+2
C
C-----  Set option ratio, concentration ratio index for grass or grain -------
C
        IF (IAN .LT. 5) THEN
          ANFLG = ANF(IAN)
          IBV = 4
        ELSEIF (IAN .EQ. 5) THEN
          ANFLG = ANF(1)
          IBV = 1
        ELSE
          ANFLG = ANF(3)
          IBV = 1
        ENDIF

        IF (ANFLG) THEN
          DO 400 IN = 1, NONUC
C
C-----      Biotic transport -------------------------------------------------
C
        RTAVAL(IN) = 0.0
	  RTBIO(IN) = 0.0
        IF (BIOT .AND. DOBIOT) THEN
C ---RTEMP units are kg/(m2*yr) per kg/m3
          RTEMP = BVI(IBV,IN) * RF2 * YELDA(IAN)*DRYFA2(IAN) / SSLDN
c ------ Revision for units BAN 10 Nov 2001 (Soils are all now in Bq/kg)
C ---Amount taken per kg below grade, Bq/kg yr
c
         RTAVAL(IN) = DS2CON(IN,IAN) * (EXCAMT + RTEMP)/WASDEP
C ---Amount deposited on surface, Bq/kg yr 
	   RTBIO(in)=DS2CON(IN,IAN)*(EXCAMT*SSLDN + BVI(IBV,IN)*RF2
     .             *YELDA(IAN)*DRYFA2(IAN))/SLDN
C
        ENDIF
C
C
C-----      Sum environmental rates ------------------------------------------
C
            RTENV(IN) = RTBIO(IN)
C           RTENV(IN) = RTIRR + RTATM + RTBIO(IN)
C            RTAVAL(IN) = RTBIO(IN)/WASDEP
  400     CONTINUE
C
C---------  FIRST CALCULATE ANNUAL AVERAGE SOIL CONCENTRATIONS-----------
c           CALL CHAIN (ONEYR, LEACHR, RTENV, SL2OUT, 2)
C           CALL CHAIN (ONEYR, DUMMY, RTAVAL, DS2OUT, 2)
C---------  NEED TO TRAP CHAIN DOUBLE-INTEGRAL FAILURE AT SMALL LAMBDAS--
c
		 IF (AL(1) .LT. 0.00001) THEN
	       DO JK = 1, NONUC
             SL2OUT(JK) = RTENV(JK)/2.
	       END DO
	     ELSE
		   CALL CHAIN (ONEYR, LEACHR, RTENV, SL2OUT, 2)
	     END IF
c          
           IF (DEEP) THEN
		   IF (AL(1) .LT. 0.00001) THEN
	         DO JK = 1, NONUC
               DS2OUT(JK) = RTAVAL(JK)/2.
	         END DO
	       ELSE
		     CALL CHAIN (ONEYR, DUMMY, RTAVAL, DS2OUT, 2)
	       END IF
	     END IF
C
           DO IN = 1, NONUC
            SL2AVG(IN, IAN) = SL2AVG(IN, IAN) + SL2OUT(IN)/ONEYR
            IF (DEEP) DS2AVG(IN, IAN) = DS2AVG(IN,IAN)-DS2OUT(IN)/ONEYR
           ENDDO
C
          IF (DEBUG) WRITE (*,*)
     .      'Anm soil:(RTBIO):', 
     .      RTBIO(1)
C    .      'Anm soil:(RTIRR/RTATM/RTBIO):', 
C    .      RTIRR, RTATM, RTBIO(1)
C
C-------- SECOND CALCULATE YEAR-END CONCENTRATIONS FOR NEXT PERIOD----
C
C-----    Total deposition/removal contributions to surface soil -------------
C
          CALL CHAIN (ONEYR, LEACHR, RTENV, RTENV, 1)
C
C-----    Calculate total change in available waste --------------------------
C
          IF (DEEP) CALL CHAIN (ONEYR, DUMMY, RTAVAL, RTAVAL, 1)
C
C-----         Add total integrations to soil and waste concentrations -------
C
          DO 420 IN = 1, NONUC
            SL2CON(IN,IAN) = SL2CON(IN,IAN) + RTENV(IN)
            IF (DEEP) DS2CON(IN,IAN) = DS2CON(IN,IAN) - RTAVAL(IN)
  420     CONTINUE
C
          IF (DEBUG) THEN
            WRITE (*,*) 'Anm soil:',IAN,SL2CON(1,IAN)
            IF (DEEP) WRITE (*,*) ' and deep:',DS2CON(1,IAN)
          ENDIF
C
        ENDIF
  410 CONTINUE
      RETURN
      END
C
C---- End of ModuLE EDRANM ---------------------------------------------------
C
