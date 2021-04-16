C-------------------------------------- VERSION: 27-Dec-98  BAN -----------------
C
C     SUBROUTINE DKHARV
C
C     This subroutine calculates radionuclide decay in the surface 
C     soil (top 15 cm) for a single radionuclide decay chain.  In 
C     addition,  this subroutine calculates harvest removal of radio-
C     nuclides in the surface for both terrestrial foods and animal
C     product pathways.  This subroutine in far-field scenarios.
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Last Modification: 27-Dec-98  BAN
C     Reviewed and Approved:             BA Napier
C
C-----------------------------------------------------------------------
C  27-Dec-98  BAN  Change Harvest removal to use annual averages.
C-----------------------------------------------------------------------
C
      SUBROUTINE DKHARV
C
C---- Include Statements -----------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'ANMPAR.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'FODPAR.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'SOLPAR.CMN' 
C
      LOGICAL ANFLG
	Dimension RatioS(9,25)
C
C----- Calculate average concentration during current year from amount at ----
C      end of previous year
C
        CALL CHAIN (ONEYR, LEACHR, RESSOL, RESAVG, 1)
C
C----- Residential/resuspension soil compartments ----------------------------
C      Decay concentration to end of current year
C
        CALL CHAIN (ONEYR, LEACHR, RESSOL, RESSOL, 0)
C
C----- For terrestrial foods soil compartment --------------------------------
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
     .             HARVT(IN,ITF)) / solavg(in,itf)
	            else 
	            RatioS(IN,ITF) = 1.0
	            end if
                END DO
              ENDIF
C
C-----        Harvest removal ------------------------------------------------
C
              IF (HARVST) THEN
                DO 120 IN = 1, NONUC
                  SOLCON(IN,ITF) = SOLCON(IN,ITF) * RatioS(IN,ITF)
  120           CONTINUE
              ENDIF
              IF (DEBUG) WRITE (*,*) 
     .          'Crop soil before/after decay & harvest, HARVT:',
     .           ITF, TEMP, SOLCON(1,ITF),HARVT(1,ITF)
CC
C----- Decay soils for one year -----------------------------------------
C
              CALL CHAIN (ONEYR, LEACHR, SOLCON(1,ITF), SOLAVG(1,ITF),1)
              TEMP = SOLCON(1,ITF)
              CALL CHAIN (ONEYR, LEACHR, SOLCON(1,ITF), SOLCON(1,ITF),0)
C
            ENDIF
  130     CONTINUE
        ENDIF
C 
C----- For animal product soil compartment -----------------------------------
C
        IF (ANFOOD) THEN
          DO 132 IAN = 1, NAN+2
C
            IF (IAN .LT. 5) THEN 
              ANFLG = ANF(IAN)
            ELSEIF (IAN .EQ. 5) THEN
              ANFLG = ANF(1)
            ELSE
              ANFLG = ANF(3)
            ENDIF
C
            IF (ANFLG) THEN
C
C-----   Set up for Harvest Removal ------------------------------------------
C
              IF (HARVST) THEN
                DO IN = 1, NONUC
	            if (sl2avg(in,ian) .gt. 0.0) then
                  RatioS(IN,Ian) = (sl2avg(in,ian) - 
     .             HARVa(IN,Ian)) / sl2avg(in,ian)
	            else 
	            RatioS(IN,Ian) = 1.0
	            end if
                END DO
              ENDIF
C
C-----        Harvest removal ------------------------------------------------
C
              IF (HARVST) THEN
                DO 129 IN = 1, NONUC
                  Sl2CON(IN,Ian) = Sl2CON(IN,Ian) * RatioS(IN,Ian)
  129           CONTINUE
              ENDIF
C
C----- Decay soils for previous year -----------------------------------------
C
              CALL CHAIN (ONEYR, LEACHR, SL2CON(1,IAN), SL2AVG(1,IAN),1)
              TEMP = SL2CON(1,IAN)
              CALL CHAIN (ONEYR, LEACHR, SL2CON(1,IAN), SL2CON(1,IAN),0)
C
            ENDIF
  132     CONTINUE
        ENDIF
C
      RETURN
C
C----- END OF SUBROUTINE DKHARV ----------------------------------------------
C
      END   


