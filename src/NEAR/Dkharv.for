C------------------------------------- Version: 27-Dec-98--------------------
C
C     SUBROUTINE DKHARV
C
C     This subroutine calculates radionuclide decay in the surface 
C     soil (top 15 cm), deep soil (available for uptake), and the waste 
C     form compartments for a single radionuclide decay chain.  In 
C     addition,  this subroutine calculates harvest removal of radio-
C     nuclides in the surface and deep soil compartments for both 
C     terrestrial foods and animal product pathways.  This subroutine 
C     is used for both near-field and far-field scenarios.
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Initial GENII Version: 12-Aug-88  RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C     Last Modfication:  27-Dec-98  ban
C
C---- Modification History ---------------------------------------------------
C    Date     Who  Modification Summary
C  ---------  ---  -----------------------------------------------------------
C  08-Sep-97  DLS  Initial Modification of old GENII version
C  12-Feb-98  BAN  Conversion to use of annual average soil concs.
C  20-Feb-98  BAN  Relocation of PACKAG call
C  27-Dec-98  BAN  Revised harvest removal to use annual avg.
C
C---- Subroutine Call Statement ----------------------------------------------
C
      SUBROUTINE DKHARV
C
C---- Include Statements -----------------------------------------------------
C
      INCLUDE 'SOLPAR.CMN' 
      INCLUDE 'OPT.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'FODPAR.CMN'
      INCLUDE 'ANMPAR.CMN'
C
C---- Type/dimension statements ----------------------------------------------
C
      LOGICAL ANFLG
	DIMENSION RatioS(9,25), RatioD(9,25)
C
C---- Start of Analysis ------------------------------------------------------
C
      IF (NONAG) THEN
C
C----   Decay non-agricultural soil compartments -----------------------------
C
        CALL CHAIN (ONEYR, LEACHR, NONAGS, NONAGS, 0)
        IF (DEEP) THEN
          CALL CHAIN (ONEYR, DUMMY, NONAGD, NONDAV, 1)
          CALL CHAIN (ONEYR, DUMMY, NONAGD, NONAGD, 0)
        ENDIF
      ELSE     
C
C-----  Residential/resuspension soil compartments ---------------------------
C
        CALL CHAIN (ONEYR, LEACHR, RESSOL, RESAVG, 1)
        CALL CHAIN (ONEYR, LEACHR, RESSOL, RESSOL, 0)
        IF (DEEP) THEN
          CALL CHAIN (ONEYR, DUMMY, DSRES, DSRAVG, 1)
          CALL CHAIN (ONEYR, DUMMY, DSRES, DSRES, 0)
        ENDIF
C
C-----  For terrestrial foods soil compartment -------------------------------
C
        IF (TFOOD) THEN
          DO 130 ITF = 1, NTF
            IF (TFD(ITF)) THEN
C
C-----   Set up for Harvest Removal ------------------------------------------
C
              IF (HARVST) THEN
                DO IN = 1, NONUC
	            if (solavg(in,itf) .gt. 0.0) then
                  RatioS(IN,ITF) = (solavg(in,itf) - 
     .             HARVT(IN,ITF,1)) / solavg(in,itf)
	            else 
	            RatioS(IN,ITF) = 1.0
	            end if
                  IF (DEEP)THEN
                    if (dscavg(in,itf) .gt. 0.0) then
				  RatioD(IN,ITF) = (dscavg(in,itf) -
     .               HARVT(IN,ITF,2)) / dscavg(in,itf)
	              else
	              RatioD(in,itf) = 1.0
	              end if
                  ENDIF
                END DO
              ENDIF
C
C-----        Harvest removal ------------------------------------------------
C
              IF (HARVST) THEN
                DO 120 IN = 1, NONUC
                  SOLCON(IN,ITF) = SOLCON(IN,ITF) * RatioS(IN,ITF)
                  IF (DEEP)THEN
                    DSCON(IN,ITF) = DSCON(IN,ITF) * RatioD(IN,ITF)
                  ENDIF
  120           CONTINUE
              ENDIF
              IF (DEBUG) WRITE (*,*) 
     .          'Crop soil before/after decay & harvest, HARVT:',
     .           ITF, TEMP, SOLCON(1,ITF),HARVT(1,ITF,1)
            ENDIF
C
C-----        Decay soils for THIS year ----------------------------------
C
              TEMP = SOLCON(1,ITF)
              CALL CHAIN (ONEYR, LEACHR, SOLCON(1,ITF), SOLAVG(1,ITF),1)
              CALL CHAIN (ONEYR, LEACHR, SOLCON(1,ITF), SOLCON(1,ITF),0)
              IF (DEEP) THEN
               CALL CHAIN (ONEYR, DUMMY, DSCON(1,ITF), DSCAVG(1,ITF), 1)
               CALL CHAIN (ONEYR, DUMMY, DSCON(1,ITF), DSCON(1,ITF), 0)
              ENDIF
  130     CONTINUE
        ENDIF
C 
C-----  For animal product soil compartment ----------------------------------
C
        IF (ANFOOD) THEN
          DO 132 IAN = 1, NAN+2
            IF (IAN .LT. 5) THEN 
              ANFLG = ANF(IAN)
            ELSEIF (IAN .EQ. 5) THEN
              ANFLG = ANF(1)
            ELSE
              ANFLG = ANF(3)
            ENDIF
            IF (ANFLG) THEN
C
C-----  Set up for Harvest Removal -------------------------------------------
C
              IF (HARVST) THEN               
                DO IN = 1, NONUC
                  if (sl2avg(in,ian) .gt. 0.0) then
				RatioS(IN,IAN) = (sl2avg(in,ian) - 
     .             HARVA(IN,IAN,1)) / sl2avg(in,ian)
	            else
	            RatioS(in,ian) = 1.0
	            end if
                  IF (DEEP) THEN
                    if (ds2avg(in,ian) .gt. 0.0) then
				  RatioD(IN,IAN) = (ds2avg(in,ian) - 
     .               HARVA(IN,IAN,2)) / ds2avg(in,ian)
	              else
	              RatioD(in,ian) = 1.0
	              end if
                  ENDIF
                END DO
              ENDIF
C
C-----        Harvest removal ------------------------------------------------
C
              IF (HARVST) THEN               
                DO 122 IN = 1, NONUC
                  SL2CON(IN,IAN) = SL2CON(IN,IAN) * RatioS(IN,IAN)
                  IF (DEEP) THEN
                    DS2CON(IN,IAN) = DS2CON(IN,IAN) * RatioD(IN,IAN)
                  ENDIF
  122           CONTINUE
              ENDIF
C
              IF (DEBUG) WRITE (*,*) 
     .          'Anm soil before/after decay & harvest, HARVA:',
     .           IAN, TEMP, SL2CON(1,IAN), HARVA(1,IAN,1)
C
            ENDIF
C
C-----        Decay soils for THIS year ----------------------------------
C
              TEMP = SL2CON(1,IAN)
              CALL CHAIN (ONEYR, LEACHR, SL2CON(1,IAN), SL2AVG(1,IAN),1)
              CALL CHAIN (ONEYR, LEACHR, SL2CON(1,IAN), SL2CON(1,IAN),0)
              IF (DEEP) THEN
               CALL CHAIN(ONEYR, DUMMY, DS2CON(1,IAN), DS2AVG(1,IAN), 1)
               CALL CHAIN(ONEYR, DUMMY, DS2CON(1,IAN), DS2CON(1,IAN), 0)
              ENDIF
  132     CONTINUE
        ENDIF
      ENDIF 
C----DEAL WITH BURIED WASTE PACKAGES AND DISTRIBUTION INTO DEEP SOIL
C
       IF (DOWAST .AND. BURWAS) CALL PACKAG
C
      RETURN
      END
C
C----------------------------------------------------------------------
C
