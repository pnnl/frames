C-------------------------------------- Version:  20-Feb-98 ------------------
C
C     SUBROUTINE EDRNON
C
C     This subroutine calculates radionuclide concentrations in the 
C     surface soil (top 15 cm), deep soil (available for uptake) 
C     and the waste form compartments for a single radionuclide decay 
C     chain for non agricultual scenarios during the option inventory
C     decay/buildup period.  This subroutine is used for near-field 
C     scenarios, Processes considered: leaching and biotic transport.
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Initial GENII Version: 6-Sep-87  RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C     Last Modification:  20-Feb-98  BAN       
C
C---- Significant Parameters -------------------------------------------------
C 
C     RTBIO()  - Biotic transport removal rate, UoA/m2-yr
C
C---- Modification History ---------------------------------------------------
C    Date     Who  Modification Summary
C  ---------  ---  -----------------------------------------------------------
C  08-Sep-97  DLS  Initial modification from old GENII
C  12-Feb-98  ban  Conversion to use annual average soil concs.
C  20-Feb-98  BAN  no change - just note that YELDBT is DRY WEIGHT
C
C---- Subroutine Call Statement-----------------------------------------------
C
      SUBROUTINE EDRNON
C
C---- Include Statements -----------------------------------------------------
C
      INCLUDE 'SOLPAR.CMN' 
      INCLUDE 'OPT.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'TIMES.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'FODPAR.CMN'
      INCLUDE 'ANMPAR.CMN'
C
C---- Type/dimension Statements ----------------------------------------------
C
      REAL RTBIO(9)
C
C---- Start of Analysis ------------------------------------------------------
C
      DO 320 IN = 1, NONUC
C
C-----  Non-agricultural biotic transport scenario ---------------------------
C
        RTAVAL(IN) = 0.0
	  RTBIO(IN) = 0.0
        IF (BIOT .AND. DOBIOT) THEN
C ---RTEMP units are kg/(m2*yr) per kg/m3
          RTEMP = BVI(1,IN) * RF2 * YELDBT(BTDSET) / SSLDN
c
c ------ Revision for units BAN 10 Nov 2001 (Soils are all now in Bq/kg)
C ---Amount taken per kg below grade, Bq/kg yr
c
         RTAVAL(IN) = NONDAV(IN) * (EXCAMT + RTEMP)/WASDEP
C ---Amount deposited on surface, Bq/kg yr 
	   RTBIO(in)=NONDAV(IN)*(EXCAMT*SSLDN 
     .               +BVI(1,IN)*RF2*YELDBT(BTDSET))/SLDN
C
        ENDIF
C
  320 CONTINUE
      IF (DEBUG .AND. DEEP) 
     .  WRITE (*,*) 'Non-ag biotic transport rate:',RTAVAL(1)
C
C----      Integrate over one year -------------------------------------------
C
C---- Environmental deposition/removal contributions to surface soil ---------
C
      CALL CHAIN (ONEYR, LEACHR, RTBIO, RTBIO, 1)
C
C---- Calculate removal from deep soil due to biotic transport ---------------
C
      IF (DEEP) CALL CHAIN (ONEYR, DUMMY, RTAVAL, RTAVAL, 1)
      IF (DEBUG) THEN
        WRITE (*,*) 'Add to non-agr surface soil:',RTBIO(1),
     .    '  Subtract from deep:',RTAVAL(1)
      ENDIF
C
C-----     Add total integrations to soil and waste concentrations -----------
C
      DO 520 IN = 1, NONUC
        NONAGS(IN) = NONAGS(IN) + RTBIO(IN)
        IF (DEEP) NONAGD(IN) = NONAGD(IN) - RTAVAL(IN)
  520 CONTINUE
C
      IF (DEBUG) WRITE (*,*) 
     .  'Non-agr soil:',NONAGS(1),' and deep:',NONAGD(1)
      RETURN
      END   
C
C---- End of Module EDRNON ---------------------------------------------------
C      
