C-------------------------------------VERSION: 25-Feb-1998--------------
C
      SUBROUTINE EDRCRP
C
C     This subroutine calculates radionuclide concentrations in the 
C     surface soil (top 15 cm) compartment for the terrestrial food
C     pathways for a single radionuclide decay chain.  This subroutine
C     is used for near-field scenarios. Processes considered: 
C     irrigation deposition, atmospheric deposition, and leaching.
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Last Modification: 25-Feb-1998  BAN
C     Reviewed and Approved: 
C
C-----------------------------------------------------------------------------
C 
C     RTATM    - Atmospheric removal rate, UoA/m2-yr
C     RTIRR    - Irrigation removal rate, UoA/m2-yr
C     WATCON   - Irrigation water concentration of current radionuclide
C                  chain member, UoA/l
C
C-----------------------------------------------------------------------------
C   DATE     WHO  Description of change
C ---------  ---  ----------------------------------------------------------
C 27-May-97  DLS  Changed air deposition to use deposition rates
C 25-Feb-98  BAN  Convert to Bq/kg from Bq/m2
C-----------------------------------------------------------------------------
C
C--- INCLUDE STATEMENTS ------------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'CONC.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'FODPAR.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'SOLPAR.CMN' 
C
C---- Type statements ---------------------------------------------------------
C
      REAL RTIRR, RTATM, WATCON
      REAL RESOUT(LENCHAIN)
C
C----- Loop on food crop types -----------------------------------------------
C
      DO 310 ITF = 1, NTF
        IF (TFD(ITF)) THEN
          DO 300 IN = 1, NONUC      ! Loop on chain members
C
C----- Irrigation deposition -------------------------------------------------
C
            RTIRR = 0.0
            IF (IRRST(ITF) .GT. 0 .AND. DOIRR) THEN 
              IF (IRRST(ITF) .EQ. 1) THEN
                WATCON = GWCON(IN) 
              ELSE 
                WATCON = SWCON(IN)
              ENDIF
              RTIRR = WATCON * RIRR(ITF) * LM2IN 
            ENDIF
C
C----- Atmospheric deposition ------------------------------------------------
C
            RTATM = 0.0
            IF (DOAIR) RTATM = WETDEP(IN) + DRYDEP(IN)
C
C----- Sum environmental deposition/contribution rates -----------------------
C
            RTENV(IN) = ( RTIRR + RTATM ) / SLDN
            IF (DEBUG .AND. IN .EQ. 1) WRITE (NELS,*) 
     .        'Crop soil:(RTIRR/RTATM:', 
     .        RTIRR, RTATM
C
  300     CONTINUE
C
C----- Integrate over one year -----------------------------------------------
C
          RTEMP = RTENV(1)
C
C----- First calculate average soil concentration from deposition over year --
C
          CALL CHAIN (ONEYR, LEACHR, RTENV, RESOUT, 2)
          DO IN = 1,NONUC
            SOLAVG(IN,ITF) = SOLAVG(IN,ITF) + RESOUT(IN) / ONEYR
          END DO
C
C----- Environmental deposition/removal contributions to surface soil --------
C
          CALL CHAIN (ONEYR, LEACHR, RTENV, RTENV, 1)
          IF (DEBUG) WRITE (NELS,*) 'RTENV before/after integration:',
     .      RTEMP, RTENV(1)
C
C----- Add total integrations to soil concentrations -------------------------
C
          DO IN = 1, NONUC
            SOLCON(IN,ITF) = SOLCON(IN,ITF) + RTENV(IN)
          END DO  
C
          IF (DEBUG) THEN
            WRITE (NELS,*) 'Crop soil:',ITF, SOLCON(1,ITF)
          ENDIF
C
        ENDIF
  310 CONTINUE
C
      RETURN
C
C---- END OF MODULE EDRCRP ---------------------------------------------------
C
      END

