C------------------------------------- Version: 11 Nov 2001 ------------------
C
C    SUBROUTINE PACKAG
C
C     This subroutine calculates radionuclide release for packaging
C     for a single radionuclide decay chain.  This subroutine is used 
C     for near-field.
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Initial GENII Version: 23-Mar-88  RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C     Last Modification:  11 Nov 2001  BAN
C
C-----------------------------------------------------------------------
C 
C     RTWAST() - Waste removal rate, UoA/m3 yr 
C
C--- Modification History ----------------------------------------------------
C   Date      Who  Modification summary
C  ---------  ---  -----------------------------------------------------------
C  08-Sep-97  DLS  Initial modification from old GENII
C  09-Sep-97  DLS  Correct use of waste package half life in release calc.
C  20-Feb-98  BAN  Conversion to use of annual average soil concentrations
C  11 NOV 01  BAN  Trap CHAIN double integral failure at small lambdas
C-----------------------------------------------------------------------------
C
      SUBROUTINE PACKAG
C
C---- Include Statements -----------------------------------------------------
C
      INCLUDE 'CONC.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'SOLPAR.CMN' 
      INCLUDE 'TIMES.CMN'
      INCLUDE 'DECAY.CMN'
C
C---- Type/dimension Statements ----------------------------------------------
C
      REAL RTWAST(9), PAL(9)
C
C---- Start of Analysis ------------------------------------------------------
C
      DO I = 1, NONUC
        PAL(I) = LOG(2.0)/PACKHL
      ENDDO
      DO IJ = 1, NONUC
        RTWAST(IJ) = QWAS(IJ) * (AL(IJ) + PAL(IJ))
      ENDDO
C
C---- Remove waste degradation contribution from waste, and add --------------
C     to deep soil
C
      CALL CHAIN (ONEYR, PAL, QWAS, QWAS, 0)
      CALL CHAIN (ONEYR, PAL, RTWAST, PAKADD, 1)
C      CALL CHAIN (ONEYR, PAL, RTWAST, RTWAST, 2)
	IF (AL(1) .LT. 0.00001) THEN
	  DO JK = 1, NONUC
	  RTWAST(JK) = RTWAST(JK) / 2.0
	  END DO
	ELSE
        CALL CHAIN (ONEYR, PAL, RTWAST, RTWAST, 2)
	END IF
C
C
      DO 360 IN = 1, NONUC
       IF (NONAG) THEN
C
C-----    Non-agricultural scenario ------------------------------------------
C
          NONAGD(IN) = NONAGD(IN) + PAKADD(IN)
          NONDAV(IN) = NONDAV(IN) + RTWAST(IN)
        ELSE
C
C-----    Agricutural/residential scenario -----------------------------------
C
          DSRES(IN) = DSRES(IN) + PAKADD(IN)
          DSRAVG(IN) = DSRAVG(IN) + RTWAST(IN)
C
C----     Deep soil for terrestrial crops ------------------------------------
C
          DO 361 ITF = 1, NTF
            DSCON(IN,ITF) = DSCON(IN,ITF) + PAKADD(IN)
            DSCAVG(IN,ITF) = DSCAVG(IN,ITF) + RTWAST(IN)
  361     CONTINUE
C
C----     Deep soil for animal feed crops ------------------------------------
C
          DO 362 IAN = 1, NAN+2
            DS2CON(IN,IAN) = DS2CON(IN,IAN) + PAKADD(IN)
            DS2AVG(IN,IAN) = DS2AVG(IN,IAN) + RTWAST(IN)
  362     CONTINUE
        ENDIF
  360 CONTINUE   ! End of chain member loop
C
      IF (DEBUG) THEN
        WRITE (*,*) 'Waste after degradation:',QWAS(1)
        WRITE (*,*) 'Available waste:',PAKADD(1)
      ENDIF
      RETURN
      END
C
C---- End of Module PACKAG ---------------------------------------------------
C
