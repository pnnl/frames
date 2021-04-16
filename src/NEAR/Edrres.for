C------------------------------------ Version: 09-Sep-97 ---------------------
C
C    SUBROUTINE EDRRES
C
C     This subroutine calculates radionuclide depositon/removal to
C     residential/resuspension soil for a single radionuclide decay chain.  
C
C     NEAR FIELD MODULE
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Initial GENII Version: 14-Jan-88  RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C     Last Modification:     12-Feb-98  BAN        
C-----------------------------------------------------------------------
C 
C     RTBIO()  - Biotic transport removal rate, UoA/m2-yr
C     RTENV()  - Total environmental removal rate, UoA/m2-yr, for each
C                member of the current radionuclide chain
C
C-----------------------------------------------------------------------------
C    Date     Who  Modification summary
C  ---------  ---  -----------------------------------------------------------
C  09-Sep-97  DLS  Initial modification of old GENII
C  12-Feb-98  BAN  Conversion to use of annual average soil concs.
C-----------------------------------------------------------------------------
C
      SUBROUTINE EDRRES
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
      REAL RESOUT(LENCHAIN), RTBIO(LENCHAIN), DSROUT(LENCHAIN)
C     REAL RTIRR, RTATM, RTBIO(LENCHAIN), WATCON
C
C---- Start of Analysis ------------------------------------------------------
C
      DO 500 IN = 1, NONUC
C
C-----  Biotic transport -----------------------------------------------------
C
        RTAVAL(IN) = 0.0
	  RTBIO(IN) = 0.0
        IF (BIOT .AND. DOBIOT) THEN
C ---RTEMP units are kg/(m2*yr) per kg/m3
          RTEMP = BVI(1,IN) * RF2 * YELDBT(3) / SSLDN
c ------ Revision for units BAN 10 Nov 2001 (Soils are all now in Bq/kg)
C ---Amount taken per kg below grade, Bq/kg yr
c
         RTAVAL(IN) = DSRES(IN) * (EXCAMT + RTEMP)/WASDEP
C ---Amount deposited on surface, Bq/kg yr 
	   RTBIO(in)=DSRES(IN)*(EXCAMT*SSLDN
     .              + BVI(1,IN)*RF2*YELDBT(3))/SLDN
C
        ENDIF
CC        ENDIF
C
C-----  Sum environmental deposition/contribution rates ----------------------
C
        RTENV(IN) =                 RTBIO(IN)
C       RTENV(IN) = RTIRR + RTATM + RTBIO(IN)
C        RTAVAL(IN) = RTBIO(IN)/WASDEP
C
        IF (DEBUG .AND. IN .EQ. 1) WRITE (*,*) 
     .    'Res soil:(RTBIO):', 
     .    RTBIO(1)
C
  500 CONTINUE
C--FIRST CALCULATE ANNUAL AVERAGE CONCENTRATIONS DURING YEAR-----------
C
C        CALL CHAIN (ONEYR, LEACHR, RTENV, RESOUT, 2)
C        IF (DEEP) CALL CHAIN (ONEYR, DUMMY, RTAVAL, DSROUT, 2)
C---------  NEED TO TRAP CHAIN DOUBLE-INTEGRAL FAILURE AT SMALL LAMBDAS--
c
		 IF (AL(1) .LT. 0.00001) THEN
	       DO JK = 1, NONUC
             RESOUT(JK) = RTENV(JK)/2.
	       END DO
	     ELSE
		   CALL CHAIN (ONEYR, LEACHR, RTENV, RESOUT, 2)
	     END IF
c 
           IF (DEEP) THEN
		   IF (AL(1) .LT. 0.00001) THEN
	         DO JK = 1, NONUC
               DSROUT(JK) = RTAVAL(JK)/2.
	         END DO
	       ELSE
		     CALL CHAIN (ONEYR, DUMMY, RTAVAL, DSROUT, 2)
	       END IF
	     END IF
C
C
        DO IN = 1, NONUC
          RESAVG(IN) = RESAVG(IN) + RESOUT(IN)/ONEYR
          IF (DEEP) DSRAVG(IN) = DSRAVG(IN) - DSROUT(IN)/ONEYR
        ENDDO
C
C---- SECOND, CALCULATE CONCENTRATION AT YEAR END     ------------------
C     Environmental deposition/removal contributions to soils
C
      CALL CHAIN (ONEYR, LEACHR, RTENV, RTENV, 1)
      IF (DEEP) CALL CHAIN (ONEYR, DUMMY, RTAVAL, RTAVAL, 1)
C
C---- Add total integrations to residential soils ----------------------------
C
      DO 520 IN = 1, NONUC
        RESSOL(IN) = RESSOL(IN) + RTENV(IN)
        IF (DEEP) DSRES(IN) = DSRES(IN) - RTAVAL(IN)
  520 CONTINUE
      RETURN
      END   
C
C----- End of Module EDRRES --------------------------------------------------
C
