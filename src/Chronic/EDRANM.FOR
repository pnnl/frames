C--------------------------------------VERSION: 25-Feb-1998 ------------------
C
      SUBROUTINE EDRANM
C
C     This subroutine calculates radionuclide concentrations in the 
C     surface soil (top 15 cm) for the animal foods pathways for a single
C     radionuclide decay chain.  This subroutine is used for far-field
C     scenarios.  Processes considered: irrigation deposition, atmospheric 
C     deposition, and leaching.
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Last Modification: 25-Feb-98  BAN
C     Reviewed and Approved: 
C
C-----------------------------------------------------------------------
C 
C     ANFLG    - Option flag for each of the animal fodder types
C     IBV      - Index of BVI value to use, either 1-fresh, 4-stored;
C                soil-to-plant transfer factor for fodder, corresponds
C                to leafy vegetables and grains of terrestrial plants  
C     RTATM    - Atmospheric removal rate, UoA/m2-yr
C     RTIRR    - Irrigation removal rate, UoA/m2-yr
C     WATCON   - Irrigation water concentration of current radionuclide
C                chain member, UoA/l
C
C-----------------------------------------------------------------------
C   MODIFICATION HISTORY
C  25-Feb-98  BAN  Convert to Bq/kg from Bq/m2
C----------------------------------------------------------------------
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
C----- Type Statements -------------------------------------------------------
C
      REAL RTIRR, RTATM, WATCON
      REAL SL2OUT(LENCHAIN)
      LOGICAL ANFLG
C
      DO 410 IAN = 1, NAN+2
C
C----- Set option ratio, concentration ratio index for grass or grain --------
C
        IF (IAN .LT. 5) THEN
          ANFLG = ANF(IAN)
C          IBV = 4
        ELSEIF (IAN .EQ. 5) THEN
          ANFLG = ANF(1)
C          IBV = 1
        ELSE
          ANFLG = ANF(3)
C          IBV = 1
        ENDIF
C
        IF (ANFLG) THEN
          DO 400 IN = 1, NONUC
C
C----- Irrigation deposition -------------------------------------------------
C
            RTIRR = 0.0
            IF (IRRSA(IAN) .GT. 0) THEN 
              IF (IRRSA(IAN) .EQ. 1) THEN
                WATCON = GWCON(IN) 
              ELSE 
                WATCON = SWCON(IN)
              ENDIF
              RTIRR = WATCON * RIRRA(IAN) * LM2IN 
            ENDIF
C
C----- Atmospheric deposition ------------------------------------------------
C
            RTATM = 0.0
            IF (DOAIR) RTATM = WETDEP(IN) + DRYDEP(IN)
C
C----- Sum environmental rates -----------------------------------------------
C
            RTENV(IN) =  ( RTIRR + RTATM ) / SLDN
  400     CONTINUE
C
          IF (DEBUG) WRITE (NELS,*) 
     .      'Anm soil:(RTIRR/RTATM):', 
     .      RTIRR, RTATM
C
C         ---- Integrate over one year -------------------------------
C
C----- First calculate average soil concentration over current year from -----
C      depositon
C
          CALL CHAIN (ONEYR, LEACHR, RTENV, SL2OUT, 2)
          DO IN = 1,NONUC
            SL2AVG(IN,IAN) = SL2AVG(IN,IAN) + SL2OUT(IN)/ONEYR
          END DO
C
C----- Total deposition/removal contributions to surface soil ----------------
C
          CALL CHAIN (ONEYR, LEACHR, RTENV, RTENV, 1)
C
C----- Add total integrations to soil and waste concentrations  --------------
C
          DO 420 IN = 1, NONUC
            SL2CON(IN,IAN) = SL2CON(IN,IAN) + RTENV(IN)
  420     CONTINUE
C
          IF (DEBUG) THEN
            WRITE (NELS,*) 'Anm soil:',IAN,SL2CON(1,IAN)
          ENDIF
C
        ENDIF
  410 CONTINUE
C      
      RETURN
C
C----- END OF SUBROUTINE EDRANM ----------------------------------------------
C
      END

