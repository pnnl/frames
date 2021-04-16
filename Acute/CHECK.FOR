C-----------------------------------------------------------------------
C
      SUBROUTINE CHECK (ISTOP)
C
C     This module checks input parameters for validity.  Relationships
C     between dependent parameters are also checked.  Flags are also
C     set for improved program control.
C
C     Module of Program ENVIN of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System
C
C     Last Modification: 18-Jun-90  RAP
C     Reviewed and Approved: 12-Sept-88 BA Napier
C
C-----------------------------------------------------------------------
C
C     CHK       - Logical set or cleared to check parameter interactions 
C     IPRIOR    - Logical set if any prior years are considered in this
C                 scenario
C     ISTOP     - Logical set true if any error conditions are
C                   discovered
C     ISTOP2    - Temporary logical variable 
C     STORE     - Logical flag set if acute and storage times specified
C
C-----------------------------------------------------------------------

      INCLUDE 'AFPPAR.CMN' 
      INCLUDE 'ANMPAR.CMN'
      INCLUDE 'AQUPAR.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'EXTPAR.CMN'
      INCLUDE 'FODPAR.CMN'
      INCLUDE 'LABELS.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'RADIN.CMN'
      INCLUDE 'SOLPAR.CMN'
      INCLUDE 'SWPAR.CMN'
      INCLUDE 'AIRPAR.CMN'
      INCLUDE 'TIMES.CMN'

      LOGICAL ISTOP, ISTOP2, ISTOP3, ISTOP4, CHK, STORE, IPRIOR
      DATA ISTOP2 /.FALSE./

C---- Clear screen and print heading ----------------------------------

      CALL HEADNG (0)

C---- Clear option flags not in use as precaution, alert user if no 
C     selection has been made within a group --------------------------

      ISTOP2 = .FALSE.
      ISTOP3 = .FALSE.
      ISTOP4 = .FALSE.
      IMPORT = .FALSE.

      IF (TFOOD) THEN
        CHK = .TRUE.
        DO 151 ITF = 1, NTF
          IF (TFD(ITF)) THEN
            CHK = .FALSE.
            IF (ISALT .AND. IRRST(ITF).EQ.2) ISTOP2 = .TRUE.
            IF (FOQOPT .EQ. 2) THEN
              IF (TPRODT(ITF) .EQ. 0.0) ISTOP3 = .TRUE.
              IF (CONS(ITF) .EQ. 0.0) ISTOP4 = .TRUE.
            ELSEIF (FOQOPT .EQ. 3) THEN
              TPRODT(ITF) = 0.0
            ENDIF
            IF (TPRODT(ITF) .GT. 0.0) IMPORT = .TRUE.
          ENDIF
  151   CONTINUE
        IF (CHK) THEN
          WRITE (*,159) 'terrestrial food'
          ISTOP = .TRUE.
        ENDIF
      ELSE
        DO 141 ITF = 1, NTF
          IF (TFD(ITF)) THEN
            WRITE (*,149) 'Terrestrial food', EXPLAB(ITF+3)
            TFD(ITF) = .FALSE.
          ENDIF
  141   CONTINUE
      ENDIF


      STORE = .FALSE.
      IF (ANFOOD) THEN
        CHK = .TRUE.
        DO 152 IAN = 1, NAN
          IF (ANF(IAN)) THEN
            CHK = .FALSE.

            IF (ISALT .AND. IRRSA(IAN) .EQ. 2) ISTOP2 = .TRUE.

            IF (FOQOPT .EQ. 2) THEN
              IF (TPRODA(IAN) .EQ. 0.0) ISTOP3 = .TRUE.
              IF (CONS2(IAN) .EQ. 0.0) ISTOP4 = .TRUE.
            ELSEIF (FOQOPT .EQ. 3) THEN
              TPRODA(IAN) = 0.0
            ENDIF
            IF (TPRODA(IAN) .GT. 0.0) IMPORT = .TRUE.

            IF (ACUTE) THEN 
              IF (STORTM(IAN) .GT. 0.0) THEN
                STORTM(IAN) = 0.0
                STORE = .TRUE.
              ENDIF
              IF (IAN .EQ. 1 .AND. STORTM(5) .GT. 0.0) THEN
                STORTM(5) = 0.0
                STORE = .TRUE.
              ELSEIF (IAN .EQ. 3 .AND. STORTM(6) .GT. 0.0) THEN
                STORTM(6) = 0.0
                STORE = .TRUE.
              ENDIF
            ENDIF
  
          ENDIF
  152   CONTINUE
        IF (CHK) THEN
          WRITE (*,159) 'animal food products'
          ISTOP = .TRUE.
        ENDIF
      ELSE
        DO 142 IAN = 1, NAN
          IF (ANF(IAN)) THEN
            WRITE (*,149) 'Animal food product', EXPLAB(IAN+7)
            ANF(IAN) = .FALSE.
          ENDIF
  142   CONTINUE
      ENDIF

      IF (STORE .AND. ACUTE) WRITE (*,*) 
     .  'CHECK:  Animal food storage times not input to',
     .  ' acute scenario, defaults used.'
 
      IF (RECRE) THEN
        DO 153 IRC = 1, NRE
          REC(IRC) = .TRUE.
  153   CONTINUE
        SWING = .TRUE.
      ELSE
        DO 143 IRC = 1, NRE
          REC(IRC) = .FALSE.
  143   CONTINUE
        SWING = .FALSE.
      ENDIF

      IF (AQFOOD) THEN
        CHK = .TRUE.
        DO 154 IAQ = 1, NAQ
          IF (AQF(IAQ)) THEN
            CHK = .FALSE.
            IF (FOQOPT .EQ. 3) TPRODQ(IAQ) = 0.0
          ENDIF
          IF (TPRODQ(IAQ) .GT. 0.0) IMPORT = .TRUE.
  154   CONTINUE
        IF (CHK) THEN
          WRITE (*,159) 'aquatic foods'
          ISTOP = .TRUE.
        ENDIF
      ELSE
        DO 144 IAQ = 1, NAQ
          IF (AQF(IAQ)) THEN
            WRITE (*,149) 'Aquatic foods', EXPLAB(IAQ+17)
            AQF(IAQ) = .FALSE.
          ENDIF
  144   CONTINUE
      ENDIF

      IF (ISALT .AND. DRINK .AND. DWSRC .EQ. 1) ISTOP2 = .TRUE.
      IF (ISALT .AND. RESIRR .AND. IRRSR .EQ. 2) ISTOP2 = .TRUE.

      IF (ISTOP2) THEN
        ISTOP = .TRUE.
        WRITE (*,*) 'CHECK:  Cannot drink or irrigate with salt water'
      ENDIF
      
      IF (ISTOP3) THEN
        ISTOP = .TRUE.
        WRITE (*,'(2A)') 'CHECK:  When uniform production is assumed, ',
     .                   'total production must be input.'
      ENDIF

      IF (ISTOP4) THEN
        ISTOP = .TRUE.
        WRITE (*,'(2A)') 'CHECK:  When uniform production is assumed, ',
     .    'consumption must be input.'
      ENDIF

      IF (EXPORT) THEN
        IF (IMPORT) THEN
          IMPORT = .FALSE.
        ENDIF
      ENDIF

  159 FORMAT (' CHECK:  No ',A,' exposure paths have been selected:'/
     .        '         Please check input file.' )

  149 FORMAT (' CHECK:  ',A,' exposure not selected:'/
     .        '         ',A,' will not be considered.')

C---- Set dose type and check interactions ----------------------------
   
      ONEDOS = .TRUE.
      CUMDOS = .TRUE.
      MAXDOS = .TRUE.
      
      IF (NEAR) THEN
        IF (POPDOS) THEN 
          Write (*,'(2A)') 'CHECK:  Cannot calculate population dose ',
     .                     'for near field scenario'
          ISTOP = .TRUE.
        ENDIF
      ELSE
        IF (POPDOS) MAXDOS = .FALSE.
      ENDIF

C---- Check for exposure incompatibilities ----------------------------

      IF (FINITE .AND. AIREXT) THEN
        ISTOP = .TRUE.
        WRITE (*,*) 'CHECK: Finite and infinite cloud exposures ',
     .              '       cannot be  considered in one scenario.'
      ENDIF

C---- Check if any prior times considered -----------------------------

      IPRIOR = .FALSE.
      IF (BEFAIR .GT. 0.0 .OR. BEFIRR .GT. 0.0) THEN
        IPRIOR = .TRUE.
      ELSEIF (NEAR) THEN
        IF (BEFORE .GT. 0.0 .OR. LOIC .GT. 0.0) IPRIOR = .TRUE.
      ENDIF
         
C---- Check and total population --------------------------------------

 
      IF (.NOT. POPDOS) THEN
        TPOPS = 1
        POPT = 1
        DWPOP = 1
        AQUPOP = 1
      ENDIF

      IF (.NOT. NEAR .AND. POPDOS) THEN

        IF (POPOPT .EQ. 1) THEN
C         Open population file, read title and total--
          LUN  = 27
          CALL XQIN (2)
          CLOSE (LUN)

        ELSEIF (POPOPT .EQ. 2) THEN
          POPT = POPIN

        ELSE
          WRITE (*,*) 'CHECK:  Invalid population definition option: ',
     .                POPOPT
          ISTOP = .TRUE.
        ENDIF

        IF (DWPOP .EQ. 0) DWPOP = POPT
        IF (AQUPOP .EQ. 0) AQUPOP = POPT

      ENDIF

C---- Check air transport parameter interactions -----------------------

      IF (AIR) THEN

        IF (POPDOS) THEN
C         Population-- 

          IF (XOQOPT .EQ. 1) THEN
C           Value should be input--

            IF (XOQI .EQ. 0.0) THEN
              WRITE (*,*) 
     .        ' CHECK:  Population-weighted chi/Q value must be ',
     .        '         entered when air transport option = 1.'
              ISTOP = .TRUE.
            ENDIF

          ELSEIF (ACUTE) THEN
C           Select direction only valid option--
            IF (XOQOPT .NE. 3) THEN
              WRITE (*,*) 'CHECK:  Invalid E/Q input option for acute',
     .                    '        release.'
              ISTOP = .TRUE.
            ENDIF

          ELSEIF (XOQOPT .EQ. 0) THEN 
C           Chi/Q will be calculated--
            IF (POPOPT .EQ. 2) THEN
              WRITE (*,'(2A)') 
     .        ' CHECK:  Population grid must be used with ',
     .        '         air transport option = 0.'
              ISTOP = .TRUE.
            ENDIF

          ELSE 
            WRITE (*,*) 
     .      'CHECK:  Air transport option cannot be 2 or 3 ',
     .      '        for population.'
            ISTOP = .TRUE.
          ENDIF

        ELSE 
C         Individual--

          IF (XOQOPT .EQ. 1) THEN
C           Value will be input--
            IF (XOQI .EQ. 0.0) THEN
              WRITE (*,*) 'CHECK:  Chi/Q value must be entered when ',
     .                    '        air transport option = 1.'
              ISTOP = .TRUE.
            ENDIF

          ELSEIF (ACUTE) THEN
C           Select direction only valid option--
            IF (XOQOPT .NE. 3) THEN
              WRITE (*,*) 
     .        'CHECK:  Invalid E/Q input option for acute release'
              ISTOP = .TRUE.
            ENDIF

          ELSEIF (XOQOPT .EQ. 2) THEN
            IF (POPOPT .EQ. 2) THEN
              WRITE (*,*) 'CHECK:  Population grid must be used with ',
     .                    '        air transport option = 2.'
              ISTOP = .TRUE.
            ENDIF

          ELSEIF (XOQOPT .EQ. 3) THEN
            IF (MIDIST .EQ. 0.0 .OR. MIDIR .EQ. 0) THEN
              WRITE (*,37) MIDIST, MIDIR
              ISTOP = .TRUE.
            ENDIF

          ELSE
            WRITE (*,*) 
     .      'CHECK:  Invalid air transport option for individual.'
            ISTOP = .TRUE.
          ENDIF
        ENDIF

        IF (TFOOD .OR. ANFOOD) THEN
          IF (FOQOPT .GT. 3) THEN
            WRITE (*,*) 'CHECK:  Invalid atmospheric production ',
     .                  '        definition for ingestion population'
            ISTOP = .TRUE.
          ENDIF
        ENDIF
      ENDIF

      IF (XOQOPT .EQ. 1 .AND. FINITE) THEN
        WRITE (*,*) 'CHECK:  Finite plume cannot be considered ',
     .              '        when chi/Q value entered.'
        ISTOP = .TRUE.
      ENDIF

      IF (.NOT. STACK) THEN
        IF (SHITE .NE. 0.0) THEN
          SHITE = 0.0
          IF (AIR) WRITE (*,*) 
     .      'CHECK:  Not stack release, stack height set to 0.0'
        ENDIF
        IF (SFLOW .NE. 0.0) THEN
          SFLOW = 0.0
          IF (AIR) WRITE (*,*) 
     .      'CHECK:  Not stack release, stack flow rate set to 0.0'
        ENDIF
        IF (SRAD .NE. 0.0) THEN
          SRAD = 0.0
          IF (AIR) WRITE (*,*) 
     .      'CHECK:  Not stack release, stack radius set to 0.0'
        ENDIF
        IF (ETEMP .NE. 0.0) THEN
          ETEMP = 0.0
          IF (AIR) WRITE (*,*) 
     .      'CHECK:  Not stack release, effluent temp. set to 0.0'
        ENDIF
        EFFSTK = .FALSE.
      ELSE
        IF (SFLOW.EQ.0.0 .AND. SRAD.EQ.0.0 .AND. ETEMP.EQ.0.0) THEN
          EFFSTK = .TRUE.
        ELSE
          EFFSTK = .FALSE.
          IF (SRAD .EQ. 0.0) THEN
            WRITE (*,*) 'CHECK:  Stack radius cannot be zero.'
            ISTOP = .TRUE.
          ENDIF
        ENDIF
      ENDIF

   37 FORMAT (/' CHECK: Maximum individual distance and direction must',
     .         '        be entered when air transport option = 3.'//
     .         '        MI distance: ',1PE8.2,0P,/
     .         '        MI direction index (where 1 is S->N):',I8) 
       

C---- Set inventory flags ----------------------------------------------
 
      RELTRM = .FALSE.
      BASIC = .FALSE.
      DERIVE = .FALSE.
      AIRCN = .FALSE.

C     Set release term flag, check inventory against transport selection--
      DO 12 J = 1, 3

        CHK = .FALSE.
        DO 10 IN = 1, NUCTOT
          IF (CASQ(J,IN) .GT. 0.0) THEN
            RELTRM = .TRUE.
            CHK = .TRUE.
          ENDIF
   10   CONTINUE

        IF (J .EQ. 1) THEN
          IF (CHK .AND. .NOT. AIR) THEN
            WRITE (*,19) 'Air'
            ISTOP = .TRUE.
          ELSEIF (.NOT. CHK .AND. AIR) THEN
            WRITE (*,119) 'Air'
            ISTOP = .TRUE.
          ENDIF

        ELSEIF (J .EQ. 2) THEN
          IF (CHK .AND. .NOT. SWAT) THEN
            WRITE (*,19) 'Surface water'
            ISTOP = .TRUE.
          ELSEIF (.NOT. CHK .AND. SWAT) THEN
            WRITE (*,119) 'Surface water'
            ISTOP = .TRUE.
          ENDIF

        ELSEIF (J .EQ. 3) THEN
          IF (CHK .AND. .NOT. BURWAS) THEN
            WRITE (*,19) 'Buried waste'
            ISTOP = .TRUE.
          ELSEIF (.NOT. CHK .AND. BURWAS) THEN
            WRITE (*,119) 'Buried waste'
            ISTOP = .TRUE.
          ENDIF
        ENDIF

   12 CONTINUE

C     Set basic concentration flag, check inventory against transport
C     selection--

      DO 14 J = 1, 5
        BAS(J) = .FALSE.
        DO 15 IN = 1, NUCTOT 
          IF (CASCON(J,IN) .GT. 0.0) THEN
            BASIC = .TRUE.
            BAS(J) = .TRUE.
          ENDIF
   15   CONTINUE

        IF (J .EQ. 1) THEN
C         Set air concentration flag--
          IF (BAS(J)) AIRCN = .TRUE.
          IF (BAS(J) .AND. AIR) THEN
            WRITE (*,20) J, 'Air'
            ISTOP = .TRUE.
          ENDIF
        ELSEIF (J .EQ. 5) THEN
          IF (BAS(J) .AND. SWAT) THEN
            WRITE (*,20) J, 'Surface water'
            ISTOP = .TRUE.
          ENDIF
        ENDIF

        IF (BAS(J) .AND. AIR) THEN
          WRITE (*,*) 'CHECK:  Air transport cannot be considered with '
     .               ,'        basic concentrations.'
          ISTOP = .TRUE.
        ENDIF

   14 CONTINUE

      IF (AIRCN .AND. FINITE) THEN
        WRITE (*,*) 'CHECK: Cannot consider finite plume with input ',
     .              '       air concentration.'
        ISTOP = .TRUE.
      ENDIF

C     Set derived concentration flag, check against basic concentration
C     input and transport flags--

      DO 16 J = 6, 9

        CHK = .FALSE.
        DO 17 IN = 1, NUCTOT
          IF (CASCON(J,IN) .GT. 0.0) THEN
            DERIVE = .TRUE.
            CHK = .TRUE.
          ENDIF
   17   CONTINUE

        IF (J .LT. 8) THEN
          IF (CHK .AND. (BASIC .OR. RELTRM)) THEN
            WRITE (*,21) J
            ISTOP = .TRUE.
          ENDIF

          IF (CHK .AND. J .EQ. 6) THEN
C           Terrestrial foods, set flag temporarily--
            DERCRP = .TRUE.
            DO 301 ITF = 1, NTF
              GRWP(ITF) = 0.0
              IRRST(ITF) = 0.0
              RIRR(ITF) = 0.0
              IRTIMT(ITF) = 0.0
              YELD(ITF) = 0.0
  301       CONTINUE

          ELSEIF (CHK .AND. J .EQ. 7) THEN
C           Animal products, set flag temporarily--
            DERANM = .TRUE.
            DO 302 IAN = 1, NAN+2
              GRWPA(IAN) = 0.0
              IRRSA(IAN) = 0.0
              RIRRA(IAN) = 0.0
              IRTIMA(IAN) = 0.0
              YELDA(IAN) = 0.0
  302       CONTINUE

          ENDIF

        ELSEIF (J. EQ. 8) THEN
          IF (CHK .AND. .NOT. DRINK) THEN
            WRITE (*,*) 'CHECK:  Drinking water exposure flag should ',
     .                  '       be set.'
            ISTOP =. TRUE.
          ENDIF
          IF (CHK .AND. (DWSRC .NE. 3)) THEN
            WRITE (*,*) 'CHECK:  Drinking water source flag incorrect.'
            ISTOP = .TRUE.
          ENDIF 

          IF (CHK) HOLDDW = 0.0
             
        ELSEIF (J .EQ. 9) THEN
          IF (CHK .AND. (SWAT .OR. BAS(4))) THEN
            WRITE (*,21) J
            ISTOP = .TRUE. 
          ENDIF

          IF (CHK) THEN
            DO 304 IAQ = 1, NAQ
              AQUTT(IAQ) = 0.0
              HLDUP2(IAQ) = 0.0
  304       CONTINUE
          ENDIF

        ENDIF

   16 CONTINUE

      IF (DRINK .AND. DWSRC .EQ. 3 .AND. .NOT. DERIVE) THEN
        ISTOP = .TRUE.
        WRITE (*,*) 'CHECK:  Drinking water is from water system;'
        WRITE (*,*) '        however, no derived conc. has been entered'
      ENDIF


   19 FORMAT (/' CHECK: ',A,' tranport has not been selected:'/
     .        '  Inventory has been entered, please check input file.')

  119 FORMAT (/' CHECK:  ',A,' tranport has been selected:'/
     .        '  No inventory entered, please check input file.')

   20 FORMAT (/' CHECK: Basic concentration may not be input for path ',
     .        I1,/
     .        '         when ',A,' transport selected.')

   21 FORMAT (/' CHECK:  Derived concentration may not be input for ',
     .         'path ', I1,
     .        /'         when transport or basic concentrations are ',
     .         'input.')

C---- Check for near/far field scenario incompatiblitlities -----------

      NONAG = .FALSE.
      IF (NEAR .AND. BIOT .AND. (BTDSET .LT. 3)) NONAG = .TRUE.  
      IF (NONAG .AND. (BEFAIR .GT. 0 .OR. BEFIRR .GT. 0)) THEN
        WRITE (*,*) 'CHECK:  Cannot consider irrigation or air ',
     .              '        deposition during a non-agricultural',
     .              '        scenario.'
        ISTOP = .TRUE.
      ENDIF
     
      DEEP = .FALSE.
      IF (NEAR) THEN

        IF (BURWAS) DEEP = .TRUE.
        IF (BIOT) DEEP = .TRUE.
        IF (MANULR .GT. 0.0) DEEP = .TRUE. 
        IF (RF2 .GT. 0.0) DEEP = .TRUE.
        IF (BAS(3)) DEEP = .TRUE.

        IF (DEEP) THEN

          IF (BURWAS .AND. WASDEP .EQ. 0.0) THEN
            ISTOP = .TRUE.
            WRITE (*,*) 'CHECK:  Waste depth cannot be zero.'
          ENDIF

C         Set exposure flags for external dose--
          IF (GROUND) PATH(22) = .TRUE.
          IF (GROUND .AND. BURWAS) PATH(23) = .TRUE.

        ENDIF

      ELSE
C       Far-field scenario--

        IF (BURWAS) THEN
          ISTOP = .TRUE.
          WRITE (*,*) 
     .    'CHECK:  Waste degradation incompatible with far-field',
     .    '        scenario.'
        ENDIF

      ENDIF

C---- Check time imcompatibilities -------------------------------------
 
      IF (NEAR .AND. (LOIC .GT. BEFORE)) THEN
        WRITE (*,*) 'CHECK:  Loss of institutional control occurred',
     .              '        before waste was stored. Invalid.'
        ISTOP = .TRUE.
      ENDIF

C---- Distances check --------------------------------------------------

      IF (NDIST .GT. 10) THEN
        WRITE (*,*) 'CHECK:  Maximum number of distances that can be ',
     .              '        considered is 10,',
     .              '        Input value for NDIST is ',NDIST
        ISTOP = .TRUE.
      ENDIF

      IF (X(1) .LT. MINDIS) THEN
        WRITE (*,*) 'CHECK:  Minimum distance cannot be less than ',
     .              MINDIS
        ISTOP = .TRUE.
      ENDIF

C---- Divide-by-zero checks --------------------------------------------
 
      IF (BURWAS .AND. WASDEP .EQ. 0.0) THEN
        WRITE (*,*) 'CHECK:  Waste thickness cannot be set to 0.0'
        ISTOP = .TRUE.
      ENDIF
 
      IF (SWAT) THEN
        IF (MIXFLG .EQ. 0) THEN

          ISTOP2 = .FALSE.
          DO 28 IN = 1, NONUC
            IF (SWCON(IN) .EQ. 0.0) ISTOP2 = .TRUE.
   28     CONTINUE
          IF (ISTOP2 .AND. SWFLOW .EQ. 0.0) THEN
            WRITE (*,*) 'CHECK:  Surface water flow rate cannot = 0.0'
            ISTOP = .TRUE.
          ENDIF 

        ELSEIF (MIXFLG .EQ. 2) THEN
          IF (SWDPTH .EQ. 0 .OR. SWFLOW .EQ. 0) THEN
            WRITE (*,*) 
     .      'CHECK:  Surface water depth and flow rate cannot = 0.0'
            ISTOP = .TRUE.
          ENDIF

        ELSE
          IF (SWDPTH .EQ. 0 .OR. SWIDTH .EQ. 0 .OR. SWFLOW .EQ. 0) THEN
            WRITE (*,*) 'CHECK:  Surface water width, depth, or flow ',
     .                  '        rate cannot = 0.0'
            ISTOP = .TRUE.
          ENDIF
        ENDIF

      ENDIF


C---- Inhalation -------------------------------------------------------
 
      IF (INHAL) THEN
        IF (HRINH .LE. 0.0) WRITE (*,*)
     .  'CHECK:  Zero hours of inhalation exposure will equal zero dose'
        IF (RINH .LE. 0.0) WRITE (*,*)
     .  'CHECK:  Zero inhalation rate will equal zero dose'
      ENDIF
 
 
C---- Check parameter limits, divide by zero -------------------------- 
 
      IF (GROUND) THEN
        IF (IRRSR .LT. 0 .OR. IRRSR .GT. 2) THEN
          WRITE (*,*) 
     .    'CHECK: Invalid residential irrigation source index'
          ISTOP = .TRUE.
        ENDIF
      ENDIF

      IF (TFOOD .AND. .NOT. DERCRP) THEN
        DO 101 ITF = 1, NTF
          IF (TFD(ITF)) THEN

            IF (IRRST(ITF).LT.0 .OR. IRRST(ITF).GT.2) THEN
              WRITE (*,*) 'CHECK:  Invalid terrestrial food no: ',ITF,
     .                    '        irrigation source index'
              ISTOP = .TRUE.
            ENDIF

            IF (YELD(ITF) .EQ. 0.0) THEN
              WRITE (*,*) 
     .        'CHECK:  Yield cannot be 0.0 for food pathway',ITF
              ISTOP = .TRUE.
            ENDIF

          ENDIF
  101   CONTINUE
      ENDIF

      IF (ANFOOD .AND. .NOT. DERANM) THEN
        DO 102 IAN = 1, NAN
          IF (ANF(IAN)) THEN

            IF (IRRSA(IAN).LT.0 .OR. IRRSA(IAN).GT.2) THEN
              WRITE (*,*) 'CHECK:  Invalid animal product no: ',IAN,
     .                    '        irrigation source index'
              ISTOP = .TRUE.
            ENDIF

            IF (DIETFR(IAN) .GT. 0.0 .AND. YELDA(IAN) .EQ. 0.0) THEN
              WRITE (*,213) IAN
              ISTOP = .TRUE.
            ENDIF
            
            IF (IAN .EQ. 1) THEN
              IF (DIETFR(5) .GT. 0.0 .AND. YELDA(5) .EQ. 0.0) THEN
                WRITE (*,213) IAN
                ISTOP = .TRUE.
              ENDIF
            ENDIF
            
            IF (IAN .EQ. 3) THEN
              IF (DIETFR(6) .GT. 0.0 .AND. YELDA(6) .EQ. 0.0) THEN
                WRITE (*,213) IAN
                ISTOP = .TRUE.
              ENDIF
            ENDIF
            
          ENDIF
  102   CONTINUE
      ENDIF

  213 FORMAT ('CHECK:  Yield should not be 0.0 for animal product no. ',
     .         I2)

C---- Read library titles for modules processed after RITQA call -------

C     Open default parameter file to store title--
      CALL OPNFIL (4,0,22) 
      CLOSE (22)

C     Dose factor titles for QA--
      CALL OPNFIL (4, 0, 37)
      CLOSE (37)
      CALL OPNFIL (4, 0, 10)
      CLOSE (10)

      IF ((POPDOS .AND. POPOPT .EQ. 1) .OR.
     .   (.NOT. POPDOS .AND. AIR .AND. XOQOPT .EQ. 2)) THEN
        CALL OPNFIL (4,0,27)
        CLOSE (27)
      ENDIF
 

      IF (AIR) THEN
        IF (JFIN .AND. XOQOPT .NE. 1) THEN
C         Open joint frequency data file and read title--
          CALL OPNFIL (4,0,23)
          CLOSE (23)
        ELSE
          IF (POPDOS  .AND. .NOT. ACUTE .AND. XOQOPT .EQ. 2) THEN
C           Open Chi/Q input files and read title--
            CALL OPNFIL (4,0,24)
            CLOSE (24)
          ENDIF
        ENDIF

        IF ((TFOOD .OR. ANFOOD) .AND. FOQOPT .EQ. 3) THEN

          IF (ACUTE) THEN
            IF (POPDOS) THEN
C             Open population file and read title--
              CALL OPNFIL (4,0,27)
              CLOSE (27)
            ENDIF
          ENDIF

C         Open food production grid input file and read title--
          CALL OPNFIL (4,0,42)
          CLOSE (42)

C         Calculate total production and effective population for report--
          CALL XQIN (3)

        ENDIF

      ENDIF
 

C---- Stop if necessary ------------------------------------------------
 
      IF (ISTOP) THEN

        WRITE (*,114) 
  114   FORMAT (//' Module CHECK detected probable errors.'/
     .            ' Correct mistakes and rerun ENVIN '//)

      ENDIF

      RETURN
 
C-----------------------------------------------------------------------
      END
