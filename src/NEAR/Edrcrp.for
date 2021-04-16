C------------------------------------ Version:  11 Nov 2001 ------------------
C
C     SUBROUTINE EDRCRP
C
C     This subroutine calculates radionuclide concentrations in the 
C     surface soil (top 15 cm) and deep soil (available for uptake) 
C     compartments for the terrestrial food pathways for a single 
C     radionuclide decay chain.  This subroutine is used for both 
C     near-field and far-field scenarios. Processes considered: 
C     leaching, and biotic transport  
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Initial GENII Version: 14-Jan-88  RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C     Last Modification:  11 Nov 2001   BAN      
C
C-----------------------------------------------------------------------
C 
C     RTATM    - Atmospheric removal rate, UoA/m2-yr
C     RTBIO()  - Biotic transport removal rate, UoA/m2-yr
C     RTIRR    - Irrigation removal rate, UoA/m2-yr
C     WATCON   - Irrigation water concentration of current radionuclide
C                  chain member, UoA/l
C
C---- Modification History ---------------------------------------------------
C    Date     Who  Modification Summary
C  ---------  ---  -----------------------------------------------------------
C  08-Sep-97  DLS  Initial modification from old GENII
C  12-FEB-98  BAN  Conversion to use of annual average soil conc.
C  20-Feb-98  BAN  add dry/wet to YELD
C  11 NOV 2001 BAN  Trap CHAIN double integral failure at small lambdas
C---- Subroutine Call Statement ----------------------------------------------
C
      SUBROUTINE EDRCRP
C
C---- Include Statements -----------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'AFPPAR.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'FODPAR.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'SOLPAR.CMN'
	INCLUDE 'DECAY.CMN' 
C
C---- Type/dimension Statements ----------------------------------------------
C
      REAL RTBIO(LENCHAIN), SOLOUT(LENCHAIN), DSCOUT(LENCHAIN)
C      REAL RTIRR, RTATM, RTBIO(LENCHAIN), WATCON
C
C---- Start of Analysis ------------------------------------------------------
C
      DO 310 ITF = 1, NTF
        IF (TFD(ITF)) THEN
          DO 300 IN = 1, NONUC
C
C-----      Biotic transport -------------------------------------------------
C
        RTAVAL(IN) = 0.0
	  RTBIO(IN) = 0.0
        IF (BIOT .AND. DOBIOT) THEN
C ---RTEMP units are kg/(m2*yr) per kg/m3
          RTEMP = BVI(ITF,IN) * RF2 * YELD(ITF)*DRYFAC(ITF) / SSLDN
c ------ Revision for units BAN 10 Nov 2001 (Soils are all now in Bq/kg)
C ---Amount taken per kg below grade, Bq/kg yr
c
         RTAVAL(IN) = DSCON(IN,ITF) * (EXCAMT + RTEMP)/WASDEP
C ---Amount deposited on surface, Bq/kg yr 
	   RTBIO(in)=DSCON(IN,ITF)*(EXCAMT*SSLDN + BVI(ITF,IN)*RF2
     .             *YELD(ITF)*DRYFAC(ITF))/SLDN
C
        ENDIF
C
C
C-----      Sum environmental deposition/contribution rates ------------------
C
            RTENV(IN) = RTBIO(IN)
C           RTENV(IN) = RTIRR + RTATM + RTBIO(IN)
C            RTAVAL(IN) = RTBIO(IN)/WASDEP
C
            IF (DEBUG .AND. IN .EQ. 1) WRITE (*,*) 
     .        'Crop soil:(RTBIO):', 
     .        RTBIO(1)
C    .        'Crop soil:(RTIRR/RTATM/RTBIO):', 
C    .        RTIRR, RTATM, RTBIO(1)
C
  300     CONTINUE
C
C --------FIRST CALCULATE ANNUAL AVERAGE CONCENTRATIONS---------------
C
c          CALL CHAIN (ONEYR, LEACHR, RTENV, SOLOUT, 2)
c          IF (DEEP) CALL CHAIN(ONEYR, DUMMY, RTAVAL, DSCOUT, 2)
C---------  NEED TO TRAP CHAIN DOUBLE-INTEGRAL FAILURE AT SMALL LAMBDAS--
c
		 IF (AL(1) .LT. 0.00001) THEN
	       DO JK = 1, NONUC
             SOLOUT(JK) = RTENV(JK)/2.
	       END DO
	     ELSE
		   CALL CHAIN (ONEYR, LEACHR, RTENV, SOLOUT, 2)
	     END IF
C
           IF (DEEP) THEN
		   IF (AL(1) .LT. 0.00001) THEN
	         DO JK = 1, NONUC
               DSCOUT(JK) = RTAVAL(JK)/2.
	         END DO
	       ELSE
		     CALL CHAIN (ONEYR, DUMMY, RTAVAL, DSCOUT, 2)
	       END IF
	     END IF
C
C
          DO IN = 1, NONUC
            SOLAVG(IN, ITF) = SOLAVG(IN, ITF) + SOLOUT(IN)/ONEYR
            IF (DEEP) DSCAVG(IN, ITF)=DSCAVG(IN, ITF)-DSCOUT(IN)/ONEYR
          ENDDO
C
C---- THEN GET SOIL CONCENTRATION AT YEAR END FOR NEXT PERIOD----------
C
          RTEMP = RTENV(1)
C
C-----    Environmental deposition/removal contributions to surface soil -----
C
          CALL CHAIN (ONEYR, LEACHR, RTENV, RTENV, 1)
          IF (DEBUG) WRITE (*,*) 'RTENV before/after integration:',
     .      RTEMP, RTENV(1)
C
C-----    Calculate net rate of change in deep soil --------------------------
C
          IF (DEEP) CALL CHAIN (ONEYR, DUMMY, RTAVAL, RTAVAL, 1)
C
C-----         Add total integrations to soil and waste concentrations -------
C
          DO 320 IN = 1, NONUC
            SOLCON(IN,ITF) = SOLCON(IN,ITF) + RTENV(IN)
            IF (DEEP) DSCON(IN,ITF) = DSCON(IN,ITF) - RTAVAL(IN)
  320     CONTINUE
C
          IF (DEBUG) THEN
            WRITE (*,*) 'Crop soil:',ITF, SOLCON(1,ITF)
            IF (DEEP) WRITE (*,*) ' and deep:', DSCON(1,ITF)
          ENDIF
C
        ENDIF
  310 CONTINUE
C
      RETURN
      END
C
C---- End of Module EDRCRP ---------------------------------------------------
C
