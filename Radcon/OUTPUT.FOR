C     MEPAS RADCON: OUTPUT.FOR             Version Date: 08-24-2006
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                         Subroutine OUTPUT                                  *
C                                                                            *
C  Subroutine OUTPUT performs the necessary intermediate computations prior  *
C  to writing out the *.WFF and *.WCF files.  It then calls WRITE_HEADER or  *
C  WRITE_DATA to create the output file.                                     *
C                                                                            *
C  Written by:       John P. McDonald                                        *
C                    Battelle Northwest Laboratories                         *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    11/08/96 - JPM                                          *
C  Last Modified:    08/24/06 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: SUBROUTINE RADCOND
C     Calls to: WRITE_HEADER,WRITE_DATA
C     Common blocks referenced: MAXIMUM,ERROR
C
C==== Significant Parameter Designation and Descriptions =====================
C
C     Parameter Set/               Location
C     Name      Used       Type    Exp/Imp    Parameter Description
C     --------- --------- ------  ----------- ----------------------------
C     CONCHEDR  SET       LOGICAL IMPORT      TRUE IF THE HEADER FOR THE *.WCF
C                                             FILE HAS NOT YET BEEN WRITTEN
C     CS        USED      REAL    IMPORT      ARRAY HOLDING THE COMPUTATION
C                                             RESULTS (FLUX IN G/YR OR pCI/YR,
C                                             OR CONC IN G/ML OR pCI/ML)
C     DEPTH     SET       REAL    LOCAL       DISTANCE FROM THE TOP OF THE
C                                             AQUIFER OR RIVER TO THE TOP OF
C                                             THE RECTANGULAR AREA (M)
C     EFFPOR    USED      REAL    IMPORT      EFFECTIVE POROSITY FOR SZ OR
C                                             MOISTURE CONTENT FOR VZ
C     FLUX      SET       LOGICAL LOCAL       TRUE IF FLUX COMPUTATIONS WERE
C                                             PERFORMED, FALSE IF CONCENTRATION
C                                             COMPUTATIONS WERE PERFORMED
C     FLUXHEDR  SET       LOGICAL IMPORT      TRUE IF THE HEADER FOR THE *.WFF
C                                             FILE HAS NOT YET BEEN WRITTEN
C     HEIGHT    SET       REAL    LOCAL       HEIGHT OF THE RECTANGULAR AREA
C                                             BETWEEN THE CURRENT MEDIUM AND
C                                             THE NEXT (M)
C     JSCE      USED      INTEGER IMPORT      INDICATES THE CURRENT SCENARIO
C                                             (CURRENT PARENT, REALLY)
C     LENGTH    SET       REAL    LOCAL       LENGTH OF THE RECTANGULAR AREA
C                                             BETWEEN THE CURRENT VADOSE ZONE
C                                             MEDIUM AND THE NEXT MEDIUM (M)
C     MEDTYPE   SET       CHARACT EXPORT      MEDIUM TYPE
C     NUMFLX    USED      INTEGER IMPORT      NUMBER OF "RECEPTORS" FOR WHICH
C                                             MASS FLUX COMPUTATIONS WILL BE
C                                             PERFORMED
C     NUMPAR    SET       INTEGER LOCAL       TOTAL NUMBER OF PARENTS
C     NUMSCE    USED      INTEGER IMPORT      TOTAL NUMBER OF SCENARIOS
C                                             (PARENTS, REALLY)
C     ROWS      USED      INTEGER IMPORT      INDICATES THE NUMBER OF RECORDS
C                                             IN THE X- AND Y-ARRAYS.
C     RVDEPTH   USED      REAL    IMPORT      DEPTH OF FLOW IN THE RIVER,
C                                             EQUIVALENT TO H1 (CM)
C     RVWIDTH   USED      REAL    IMPORT      LATERAL MIXING LENGTH IN THE
C                                             RIVER (CM), EQUIVALENT TO XMIX
C     SZWIDTH   USED      REAL    IMPORT      SATURATED ZONE LATERAL MIXING
C                                             LENGTH (CM), EQUIVALENT TO ALEN
C     TIM       USED      REAL    IMPORT      ARRAY HOLDING THE TOTAL TIMES
C                                             ASSOCIATED WITH THE CS ARRAY (YR)
C     VP        USED      REAL    IMPORT      FLOW VELOCITY IN THE RIVER, PORE
C                                             WATER VELOCITY IN THE VZ OR SZ
C                                             (CM/YR)
C     VZHEIGHT  USED      REAL    IMPORT      HEIGHT OF THE SOURCE (CM),
C                                             EQUIVALENT TO AL1
C     VZWIDTH   USED      REAL    IMPORT      WIDTH OF THE SOURCE (CM),
C                                             EQUIVALENT TO B1
C     WIDTH     SET       REAL    LOCAL       WIDTH OF THE RECTANGULAR AREA
C                                             BETWEEN THE CURRENT AND NEXT
C                                             MEDIA (M)
C     KTYPE     USED      INTEGER IMPORT      MEPAS CONTAMINANT TYPE - 1
C                                             INDICATES A RADIONUCLIDE, ANY
C                                             OTHER VALUE INDICATES A NON-
C                                             RADIONUCLIDE.
C     CFUNITS   SET/USED  CHAR    LOCAL       CONCENTRATION OR FLUX UNITS
C                                             FOR THE PARENT CONSTITUENTS
C     RIVX      USED      CHAR    IMPORT      RIVER RECEPTOR LOCATION
C                                             X-COORDINATE (METERS)
C     RIVY      USED      CHAR    IMPORT      RIVER RECEPTOR LOCATION
C                                             Y-COORDINATE (METERS)
C     RIVZ      USED      CHAR    IMPORT      RIVER RECEPTOR LOCATION
C                                             DEPTH BELOW WATER (METERS)
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/11/96     JPM  Changed units from Ci to pCi (PH1-E006).
C     04/16/97     JPM  Allowing for medium type 7 (wetlands).  Describing a
C                       wetland as "surface water" in the *.WCF and *.WFF
C                       files (PH1-E007).
C     04/18/97     JPM  Replaced the IV array with the IVS array for the
C                       constituent names.  The IV array is over-written with
C                       groups 3 through 6 for multiple river receptors
C                       (PH1-P007).
C     05/01/97     JPM  Because decay products are now being run as parents,
C                       we can't use NUMCON to indicate the number of parents
C                       in the *.WCF and *.WFF files.  Using NUMPAR instead
C                       (number of original parents) (PH1-P012).
C     06/09/98     JPM  Modified output to the *.WCF and *.WFF files to meet
C                       the latest framework specifications - trimming blanks
C                       from the end of strings, outputting a "natural"
C                       recharge rate, outputting the water flux as a time
C                       series, and changed the definitions of length and
C                       height for the vadose zone (PH1-E016).
C     06/09/98     JPM  Made changes to be compatible with the new WIN file
C                       configuration of having NUMWU be the number of
C                       scenarios (receptors), and NUMSCE be the number of
C                       parents.  This allows for an unlimited number of
C                       scenarios and parent constituents (PH1-E017).
C     02/27/01     JPM  Using KTYPE to set the flux/concentration units
C                       correctly (PH1-P021).
C     11/12/02     JPM  Changed MEDNAME to LOCNAM in the WFF file.  Now
C                       indicating the medium receiving the fluxes instead
C                       of the medium producing the fluxes.  PH1-E018.
C     08/24/06     JPM  Writing the river receptor location coordinates to
C                       the WCF file (PH1-E021).
C
C==== FUNCTION CALL ==========================================================
C
      SUBROUTINE OUTPUT(MEDNAME,MED,NUMCON,IVS,CASN1,ROWS,NDS,TIM,CS,
     +                  VZHEIGHT,VZWIDTH,ZTOP,ZBOT,SZWIDTH,RVWIDTH,
     +                  RVDEPTH,EFFPOR,VP,UNITS,NUMSCE,FLUXHEDR,
     +                  CONCHEDR,LOCNAM,JSCE,NUMWU,NUMFLX,KTYPE,RIVX,
     +                  RIVY,RIVZ)
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'ERROR.WTN'
C
C==== DIMENSION Statements ===================================================
C
C     None
C
C==== Variable Declarations ==================================================
C
      CHARACTER MEDNAME*8,MEDTYPE*13,LOCNAM*21
      INTEGER*4 ROWS,NDS(MAXCON),JSCE,KTYPE(MAXCON)
      INTEGER*4 NUMCON,MED,NUMSCE,NUMPAR,NDSTOTAL,NUMWU,NUMFLX
      REAL*4 TIM(MAXNTI,MAXCON),CS(MAXNTI,MAXCON)
      REAL*4 VZHEIGHT,VZWIDTH,ZTOP,ZBOT,SZWIDTH,RVWIDTH,RVDEPTH
      REAL*4 EFFPOR,VP,WATFLUX,LENGTH,WIDTH,DEPTH,HEIGHT,UNITS
      CHARACTER*18 IVS(MAXCON),CASN1(MAXCON,12)
      LOGICAL FLUX,FLUXHEDR,CONCHEDR
      CHARACTER*6 CFUNITS(MAXCON)
      CHARACTER*15 RIVX,RIVY,RIVZ
C
C==== DATA Statements ========================================================
C
C     None
C
C*****************************************************************************
C     Set the value of FLUX.  Fluxes are computed for MED = 1,4,6 and
C     concentrations for MED = 2,3,5.  Write an error message and abort if
C     MED equals something else.
C*****************************************************************************
C
      IF (MED.EQ.1.OR.MED.EQ.4.OR.MED.EQ.6) THEN
        FLUX = .TRUE.
      ELSE IF (MED.EQ.2.OR.MED.EQ.3.OR.MED.EQ.5.OR.MED.EQ.7) THEN
        FLUX = .FALSE.
      ELSE
        WRITE (6,100) MED
        WRITE (11,100) MED
        CALL MESSAGE()
100     FORMAT (/,
     +  /2x,'ERROR -->  MED = ',I2,/
     +  /2x,'           Medium type (MED) unknown - only MED = 1 thru',
     +  /2x,'           7 are currently defined.',/)
        ERRORSIG = .TRUE.
        GOTO 9999
      ENDIF
C
C*****************************************************************************
C     Set the value of MEDTYPE.  MED = 1,2 for vadose zones; 3,4 for aquifers;
C     5,6 for rivers; and 7 for a wetland.
C*****************************************************************************
C
      IF (MED.EQ.1.OR.MED.EQ.2) THEN
        MEDTYPE = 'Vadose'
      ELSE IF (MED.EQ.3.OR.MED.EQ.4) THEN
        MEDTYPE = 'Aquifer'
      ELSE
        MEDTYPE = 'Surface water'
      ENDIF
C
C*****************************************************************************
C     Set NUMPAR equal to the total number of parents to be written to the
C     *.WCF and *.WFF files.  NUMCON is the total number of parents in the
C     RADCON run, but it includes decay products run as parents.  Thus,
C     NUMPAR is NUMCON less the number of progeny run as parents.
C*****************************************************************************
C
C      M1 = 1
C      NDSTOTAL = 0
C      DO WHILE (M1.LE.NUMCON)
C        NDSTOTAL = NDSTOTAL + NDS(M1) - 1
C        M1 = M1 + NDS(M1)
C      ENDDO
C      NUMPAR = NUMCON - NDSTOTAL
       NUMPAR = NUMSCE
C
C*****************************************************************************
C     Loads the CFUNITS array with the correct units for each parent.
C*****************************************************************************
C
      DO M1 = 1,NUMCON
        IF (FLUX) THEN
          IF (KTYPE(M1).EQ.1) THEN
            CFUNITS(M1) = 'pCi/yr'
          ELSE
            CFUNITS(M1) = 'g/yr'
          ENDIF
        ELSE
          IF (KTYPE(M1).EQ.1) THEN
            CFUNITS(M1) = 'pCi/ml'
          ELSE
            CFUNITS(M1) = 'g/ml'
          ENDIF
        ENDIF
      ENDDO
C
C*****************************************************************************
C     If fluxes were computed, then check to see if a header has already been
C     written to the *.WFF file.  If FLUXHEDR is true, then a header has not
C     been written and the file has not yet been opened during this run.  In
C     this case, open the file as 'sequential' (which deletes the current
C     contents of the file) and write out the header.  If FLUXHEDR is false,
C     then the file has been opened before during this run (i.e., for a
C     previous scenario), so open the file as 'append' and do not write the
C     header.
C*****************************************************************************
C
      IF (FLUX) THEN
        IF (FLUXHEDR) THEN
          OPEN(UNIT=14,FILE=MEDNAME//'.WFF',STATUS='UNKNOWN',IOSTAT=IOS,
     +    ACCESS='SEQUENTIAL')
          CALL WRITE_HEADER(MEDTYPE,MEDNAME,FLUXHEDR,NUMFLX)
        ELSE
          OPEN(UNIT=14,FILE=MEDNAME//'.WFF',STATUS='UNKNOWN',IOSTAT=IOS,
     +    ACCESS='APPEND')
        ENDIF
C
C*****************************************************************************
C     Determine the dimensions of the rectangular area and the water flux if
C     flux computations were performed.
C*****************************************************************************
C
        IF (MEDTYPE.EQ.'Vadose') THEN
          WIDTH = VZWIDTH/100.
          LENGTH = -99.
          HEIGHT = VZHEIGHT/100.
          DEPTH = 0.0
          WATFLUX = (VP/100.)*EFFPOR*WIDTH*HEIGHT
        ELSE IF (MEDTYPE.EQ.'Aquifer') THEN
          WIDTH = SZWIDTH/100.
          LENGTH = 0.0
          HEIGHT = (ZBOT-ZTOP)/100.
          DEPTH = ZTOP/100.
          WATFLUX = (VP/100.)*EFFPOR*WIDTH*HEIGHT
        ELSE
          WIDTH = RVWIDTH/100.
          LENGTH = 0.0
          HEIGHT = RVDEPTH/100.
          DEPTH = 0.0
          WATFLUX = (VP/100.)*WIDTH*HEIGHT
        ENDIF
C
C*****************************************************************************
C     Write out the medium name, medium type, dimensions of the rectangular
C     area, and the water flux.
C*****************************************************************************
C
        IF (JSCE.EQ.1) THEN
          WRITE (14,105) LOCNAM(1:LEN_TRIM(LOCNAM)),
     +    MEDTYPE(1:LEN_TRIM(MEDTYPE)),WIDTH,HEIGHT,DEPTH,
     +    WATFLUX/(WIDTH*HEIGHT),NUMPAR
          WRITE(14,110) 2
          WRITE(14,111) 0.0,WATFLUX
          WRITE(14,111) 10000.0,WATFLUX
105       FORMAT('"',A,'","',A,'",',1P,3(E10.3,',"m",'),E10.3,
     +    ',"m/yr",',I2)
110       FORMAT('"yr","m^3/yr",',I2)
111       FORMAT(1P,2(E10.3,','))
        ENDIF
C
C*****************************************************************************
C     If fluxes were not computed, then check to see if a header has already
C     been written to the *.WCF file.  If CONCHEDR is true, then a header has
C     not been written and the file has not yet been opened during this run.
C     In this case, open the file as 'sequential' (which deletes the current
C     contents of the file) and write out the header.  If CONCHEDR is false,
C     then the file has been opened before during this run (i.e., for a
C     previous scenario), so open the file as 'append' and do not write the
C     header.
C*****************************************************************************
C
      ELSE
        IF (CONCHEDR) THEN
          OPEN(UNIT=14,FILE=MEDNAME//'.WCF',STATUS='UNKNOWN',IOSTAT=IOS,
     +    ACCESS='SEQUENTIAL')
          CALL WRITE_HEADER(MEDTYPE,MEDNAME,CONCHEDR,NUMWU-NUMFLX)
        ELSE
          OPEN(UNIT=14,FILE=MEDNAME//'.WCF',STATUS='UNKNOWN',IOSTAT=IOS,
     +    ACCESS='APPEND')
        ENDIF
C
C*****************************************************************************
C     Write out the usage location name, medium type, and number of
C     constituents.
C*****************************************************************************
C
        IF (JSCE.EQ.1.AND.MEDTYPE.EQ.'Surface water') THEN
          WRITE (14,135) LOCNAM(1:LEN_TRIM(LOCNAM)),
     +    MEDTYPE(1:LEN_TRIM(MEDTYPE)),NUMPAR,RIVX(1:LEN_TRIM(RIVX)),
     +    RIVY(1:LEN_TRIM(RIVY)),RIVZ(1:LEN_TRIM(RIVZ))
135       FORMAT('"',A,'","',A,'",',I5,',',A,',"m",',A,
     +           ',"m",',A,',"m"')
        ELSE IF (JSCE.EQ.1) THEN
          WRITE (14,136) LOCNAM(1:LEN_TRIM(LOCNAM)),
     +    MEDTYPE(1:LEN_TRIM(MEDTYPE)),NUMPAR
136       FORMAT('"',A,'","',A,'",',I5,',')
        ENDIF
      ENDIF
C
C*****************************************************************************
C     Write out fluxes or concentrations for each parent and decay product.
C*****************************************************************************
C
      M3 = 1
      M1 = 1
      DO WHILE (M1.LE.NUMCON)
        IF (FLUX) THEN
          WRITE (14,115) IVS(M3)(1:LEN_TRIM(IVS(M3))),
     +    CASN1(M1,1)(1:LEN_TRIM(CASN1(M1,1))),
     +    CFUNITS(M1)(1:LEN_TRIM(CFUNITS(M1))),ROWS,1,NDS(M1)-1
        ELSE
          WRITE (14,120) IVS(M3)(1:LEN_TRIM(IVS(M3))),
     +    CASN1(M1,1)(1:LEN_TRIM(CASN1(M1,1))),
     +    CFUNITS(M1)(1:LEN_TRIM(CFUNITS(M1))),ROWS,NDS(M1)-1
        ENDIF
        CALL WRITE_DATA(ROWS,TIM,CS,M3,UNITS)
        DO I = 1,NDS(M1)-1
          M3 = M3 + NDS(M1+I-1)
          IF (FLUX) THEN
            WRITE (14,125) IVS(M3)(1:LEN_TRIM(IVS(M3))),
     +      CASN1(M1+I,1)(1:LEN_TRIM(CASN1(M1+I,1))),
     +      CFUNITS(M1)(1:LEN_TRIM(CFUNITS(M1))),
     +      ROWS,1,IVS(M1)(1:LEN_TRIM(IVS(M1))),
     +      CASN1(M1,1)(1:LEN_TRIM(CASN1(M1,1)))
          ELSE
            WRITE (14,130) IVS(M3)(1:LEN_TRIM(IVS(M3))),
     +      CASN1(M1+I,1)(1:LEN_TRIM(CASN1(M1+I,1))),
     +      CFUNITS(M1)(1:LEN_TRIM(CFUNITS(M1))),ROWS,
     +      IVS(M1)(1:LEN_TRIM(IVS(M1))),
     +      CASN1(M1,1)(1:LEN_TRIM(CASN1(M1,1)))
          ENDIF
          CALL WRITE_DATA(ROWS,TIM,CS,M3,UNITS)
        ENDDO
        M1 = M1 + NDS(M1)
        M3 = M3 + 1
      ENDDO
115   FORMAT('"',2(A,'","'),'yr","',A,'",',2(I3,','),I3)
120   FORMAT('"',2(A,'","'),'yr","',A,'",',I3,',',I3)
125   FORMAT('"',2(A,'","'),'yr","',A,'",',2(I3,','),
     +'"',A,'","',A,'"')
130   FORMAT('"',2(A,'","'),'yr","',A,'",',I3,',',
     +'"',A,'","',A,'"')   
C
C*****************************************************************************
C     Close the output file and return.
C*****************************************************************************
C
      CLOSE(UNIT=14)
9999  RETURN
      END
