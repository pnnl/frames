C     MEPAS RADCON: LEACH1.FOR            Version Date: 05-15-1998
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                    SUBROUTINE LEACH1                                       *
C                                                                            *
C  Subroutine LEACH1 is the main subroutine for calculating leachate and     *
C  overland runoff volume on a monthly time scale.  It addresses snowmelt    *
C  and overland volumes as well as an estimate  of potential evapotranspir-  *
C  ation.  It is called from subroutine LEACHV and in turn calls subroutine  *
C  AWPL, FOAPET, LEACHG, PRECIP, RUNVOL, SEDOVR, STORAG, TABLE1, and WLIMIT. *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    05/15/98 - PDM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: SUBROUTINE LEACHV
C     Calls to: AWPL, FOAPET, LEACHG, PRECIP, RUNVOL, SEDOVR, STORAG, 
C               TABLE1, and WLIMIT.
C     Common blocks referenced: INDEX1, INDEX3
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- -----  ------  -------------- --------------------------------
C     AVAILW    SET    REAL    LOCAL          AVAILABLE WATER OF TOP SOIL     
C     CN1       SET    REAL    LOCAL          SCS CURVE NUMBER FOR SITE       
C     CN2       SET    REAL    LOCAL          SCS CURVE NUMBER FOR OOF-SITE   
C     FPSUM     SET    REAL    ARG - EXPORT   SUM OF POTENTIAL INFILTRATION   
C     IFLAG     SET    INTEGER LOCAL          INDEX FOR LAST POSITIVE FP MONTH
C                                             FOR WET SEASON                  
C     IPOND     USED   INTEGER COMMON - IMP   INDEX DEFINING PONDING CONDITION
C                                               = 1; PONDED,                  
C                                               = 2; NONPONDED                
C     ISCONF    USED   INTEGER COMMON - IMP   INDEX FOR DEFINING SOURCE TERM  
C                                             CONFIGURATION:                  
C                                               = 1; POINT SOURCE             
C                                               = 2; X-DIRECTION LINE SOURCE  
C                                               = 3; Y-DIRECTION LINE SOURCE  
C                                               = 4; AREA SOURCE              
C     ISOURC    USED   INTEGER COMMON - IMP   INDEX DEFINING SOURCE TERM      
C                                             BOUNDARY CONDITIONS             
C     ISTYPE    USED   INTEGER COMMON - IMP   INDEX FOR SOURCE TERM TYPE      
C     JFLAG     SET    INTEGER LOCAL          INDEX FOR LAST NEGATIVE FP MONTH
C                                             FOR WET SEASON                  
C     SEDFLX    USED   REAL    ARG - IMPORT   SEDIMENT FLUX                   
C     VLEACH    SET    REAL    ARG - EXPORT   INFILTRATION RATE - DEPENDENT ON
C                                             SPECIFIC CONDITIONS             
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/25/91     JWB  Added required comments and header for MEPAS QA plan  
C     09/22/94     JPM  Writing a water balance report to a *.WBR file on a
C                       monthly basis when NPRINT = -9.  Declared AET to be
C                       an array and passing to LEACHG.  This allows the
C                       actual ET to be saved for printing to the report.
C     09/22/94     JPM  Removed the units from the column heading when
C                       temperatures are written to the *.WLS file.  The units
C                       were listed as DEF F even when the data is actually
C                       celsius.
C     10/10/94     JPM  Changed the name of variable STOR1 to SMRUNOF
C     05/15/98     PDM  Commented out several statements that were used in
C                       the prior implementation of multiple infiltration
C                       rates (RAD-E054).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE LEACH1 (FPSUM,BLEN,BWIDTH,VLEACH,SEDFLX)
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'INDEX1.WTN'
      INCLUDE 'INDEX3.WTN'
      INCLUDE 'INDEX4.WTN'
      INCLUDE 'EXTFLUX.WTN'
c pdm      INCLUDE 'CAP.WTN'
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION STA(8),PET(12,3),DAYS(12),TEMP(12),RAIN(12),WINDV(12),
     1          CLOUD(12),FMAX(12),VOL1(12),FP(12),WL(12),ST(12),
     2          SLOP(11),YINTER(11),ALG(12),PRENUM(12),PREC(12),AET(12)
C
C==== EQUIVALENCE Statements =================================================
C
      EQUIVALENCE (PREC(12),ALG(12)), (PET(12,1),FMAX(12)), (PET(12,2),
     1            VOL1(12)), (PET(12,3),FP(12)), (RAIN(12),WL(12)),
     2            (WINDV(12),ST(12))
C
C==== Variable Declarations ==================================================
C
c pdm REAL LAT,cap_aw
      REAL LAT
      INTEGER MONTH
C
C==== DATA Statements ========================================================
C
      DATA DAYS/31.,28.25,31.,30.,31.,30.,31.,31.,30.,31.,30.,31./
      DATA SLOP /-4.9889E-01,-2.3511E-01,-1.4580E-01,-1.0855E-01,
     1           -8.5939E-02,-7.1000E-02,-5.2015E-02,-4.2005E-02,
     2           -3.4411E-02,-2.9372E-02,-2.5798E-02/
      DATA YINTER /0.959473,1.668503,2.043748,2.339144,2.556872,
     1             2.739307,3.012135,3.245520,3.414250,3.568117,
     2             3.706006 /
C
C*****************************************************************************
C     Open a *.WBR file for the Water Balance Report on a monthly basis
C     when NPRINT = -9.
C*****************************************************************************
C
      IF (NPRINT.EQ.-9) THEN
        IF (JSCE.EQ.1) THEN
          OPEN (UNIT=19, FILE=FILENM//'.WBR', STATUS='UNKNOWN',
     +    IOSTAT=IOS)
          WRITE (19,103)
        ENDIF
        WRITE (19,104) JSCE
103     FORMAT(/,'Water Balance Report')
104     FORMAT(//,'Scenario number',I5,//,T30,'JAN',T40,'FEB',T50,'MAR',
     +  T60,'APR',T70,'MAY',T80,'JUN',T90,'JUL',T100,'AUG',T110,'SEP',
     +  T120,'OCT',T130,'NOV',T140,'DEC',/)
      ENDIF
C
C*****************************************************************************
C     THE CONSTANTS AND CONVERSION FACTORS OF THE GIVEN STATION ARE READ AND   
C     PRINTED.
C     SOURCE TERM CONFIGURATION (ISTYPE) VARIES AS FOLLOWS:
C       ISTYPE = 1; LEACHING TO THE PARTIALLY SATURATED ZONE 
C       ISTYPE = 2; LEACHING DIRECTLY TO THE SATURATED ZONE
C       ISTYPE = 3; OVERLAND RUN TO DISCHARGE TO STREAM -- CONTAMINATED
C                   SEDIMENT LEVELS NOT PROVIDED BY ATMOSPHERIC MODELING
C       ISTYPE = 4; HOLDING POND CALCULATIONS -- CONTAMINATED SEDIMENT LEVELS
C                   NOT PROVIDED BY ATMOSPHERIC MODELING
C       ISTYPE = 5; OVERLAND RUN TO DISCHARGE TO STREAM -- CONTAMINATED
C                   SEDIMENT LEVELS PROVIDED BY ATMOSPHERIC MODELING
C       ISTYPE = 6; HOLDING POND CALCULATIONS -- CONTAMINATED SEDIMENT LEVELS
C                   PROVIDED BY ATMOSPHERIC MODELING
C*****************************************************************************
C
      BLEN=0.
      BWIDTH=0.
      READ(5,500) (STA(I), I=1,7),UNITT,ELLCD,LAT,UHT,ALT,AVAILW,CN1
c pdm READ(5,500) (STA(I), I=1,7),UNITT,ELLCD,LAT,UHT,ALT,
c pdm1 AVAILW,CN1,cap_aw,clay_ks
c pdm*
c pdm* JLS if we are in the 2nd iteration (cap on) then use the available
c pdm*   water of the cap (natural layer of soil above the clay cap).
c pdm*
c pdm if ( incr .EQ. 2 ) AVAILW = cap_aw
      CN2=CN1
      IF ((ISTYPE .EQ. 4) .OR. (ISTYPE .EQ. 6)) READ(5,510) CN2,BLEN,
     1 BWIDTH
      NDATA=12
      ALT=ALT/3.2808
      UHT=UHT/3.2808
      ELLCD=ELLCD/3.2808
      WRITE(6,620)
 620  FORMAT (/1X,78('*')/)
      WRITE(6,600) (STA(II),II=1,7),ALT,LAT,UHT,UNITT
 600  FORMAT ('   Station                              = ',7A4,/,
     1        '   Altitude in Meters                   =',F9.1,/,
     2        '   Latitude in Degrees                  =',F9.1,/,
     3        '   Height of Wind Measurement in Meters =',F9.2,/,
     4        '   Temperature Data is Given in Degrees : ',A1)
      WRITE(6,620)
C*****************************************************************************
C     THIS LOOP READS IN EACH MONTHLY DATA CARD
C*****************************************************************************
      WRITE(6,200)
 200  FORMAT (' MONTH  TEMPERATURE    PRECIPITATION     WIND',
     1'        CLOUDINESS    NUMBER OF',/,
     2                 26x,'VOLUME         (MPH)',
     3'        (TENTHS)   PRECIPITATION',/,27x,'(IN)',37x,'EVENTS'/)
 201  FORMAT (I5,3x,1P,E10.2,6x,E10.2,4x,E10.2,4x,E10.2,3x,E10.2)
      DO 269 I=1,NDATA
      READ(5,530) MONTH,TEMP(I),RAIN(I),WINDV(I),CLOUD(I),
     1 PRENUM(I)
      WRITE(6,201) MONTH,TEMP(I),RAIN(I),WINDV(I),CLOUD(I),
     1 PRENUM(I)
      IF (UNITT .NE. 'C') TEMP(I)=(TEMP(I)-32.)*5./9.
      RAIN(I)=RAIN(I)*2.54
      WINDV(I)=WINDV(I)*0.447
      PET(I,1)=0.
      PET(I,2)=0.
      PET(I,3)=0.
  269 TEMP(I)=TEMP(I)-0.007*(ALT-ELLCD)
      IF (NPRINT.EQ.-9) THEN
        WRITE(19,665) 'Adjusted Temperature (C) ',(TEMP(I), I=1,12)
        WRITE(19,665) 'Cloudiness (tenths)      ',(CLOUD(I), I=1,12)
        WRITE(19,665) 'Wind Velocity (m/sec)    ',(WINDV(I), I=1,12)
      ENDIF
C*****************************************************************************
C     COMPUTE ADJUSTED PRECIPITATION (PREC)
C*****************************************************************************
      CALL PRECIP(UHT,ALT,TEMP,RAIN,WINDV,CLOUD,DAYS,PREC,SMRUNOF,
     +            NPRINT)
C*****************************************************************************
C    COMPUTE AVERAGE-ANNUAL OVERLAND SEDIMENT FLUX USING THE USLE (G/CM**2/YR)
C*****************************************************************************
      IF (ISTYPE .GT. 2) CALL SEDOVR (SEDFLX,SMRUNOF)
C*****************************************************************************
C     COMPUTE POTENTIAL EVAPOTRANSPIRATION (PET TO CLOUD)
C*****************************************************************************
      IF (ISTYPE .LE. 2) CALL FAOPET (MONTH,NDATA,LAT,ALT,
     C UHT,ELLCD,PET,DAYS,TEMP,WINDV,CLOUD)
C*****************************************************************************
C     INITIALIZRE PERTINENT VARIABLES AND ARRAYS
C*****************************************************************************
  380 IFLAG=0
      VOLSUM=0.
      FPSUM=0.
      FPNEG=0.
      FPPOS=0.
      DO 369 I=1,NDATA
        WL(I)=0.
        ST(I)=0.
        FP(I)=0.
        FMAX(I)=0.
        VOL1(I)=0.
        IF (TEMP(I) .LE. 0.) GO TO 370
C*****************************************************************************
C     COMPUTE THE MONTHLY RUNOFF VOLUME FROM THE SITE (VOL1)
C*****************************************************************************
        CALL RUNVOL (CN1,PRENUM(I),VOL1(I),PREC(I))
        VOLSUM=VOLSUM+VOL1(I)
        IF (ISTYPE .GT. 2) GO TO 369
C*****************************************************************************
C     COMPUTE MAXIMUM INFILTRATION (FMAX)
C*****************************************************************************
        IF (PREC(I) .GT. VOL1(I)) FMAX(I)=PREC(I)-VOL1(I)
C*****************************************************************************
C     COMPUTE POTENTIAL INFILTRATION (FP)
C*****************************************************************************
        FP(I)=FMAX(I)-CLOUD(I)
370     IF (ISTYPE .GT. 2) GO TO 369
C*****************************************************************************
C     COMPUTE POSITIVE (FPPOS), NEGATIVE (FPNEG), AND SUMMED (FPSUM) AVAILABLE 
C        POTENTIAL WATER LOSS (APWL)
C*****************************************************************************
        IF (FP(I) .LT. 0.) FPNEG=FPNEG+FP(I)
        IF (FP(I) .GE. 0.) FPPOS=FPPOS+FP(I)
        FPSUM=FPSUM+FP(I)
C*****************************************************************************
C     IFLAG DESIGNATES THE LAST POSITIVE FP MONTH FOR THE WET SEASON
C     JFLAG DESIGNATES THE LAST NEGATIVE FP MONTH FOR THE DRY SEASON
C*****************************************************************************
        IF ((IFLAG .EQ. 0) .AND. (FP(I) .LT. 0.)) IFLAG=I-1
  369 CONTINUE
      IF (NPRINT.EQ.-9) THEN
        WRITE(19,105) 'Adjusted                 '
        WRITE(19,665) 'Precipitation (cm)       ',(PREC(I), I=1,12)
        WRITE(19,665) 'Runoff Volume (cm)       ',(VOL1(I), I=1,12)
        IF (ISTYPE.LE.2) THEN
          WRITE(19,665) 'Maximum Infiltration (cm)',(FMAX(I), I=1,12)
          WRITE(19,105) 'Potential                '
          WRITE(19,665) 'Evapotranspiration (cm)  ',(CLOUD(I), I=1,12)
          WRITE(19,105) 'Potential                '
          WRITE(19,665) 'Infiltration (cm)        ',(FP(I), I=1,12)
        ENDIF
      ENDIF
C       WRITE(6,530) IFLAG
105   FORMAT(A25)
665   FORMAT (A25,T27,1P,E9.2,T37,E9.2,T47,E9.2,T57,E9.2,T67,E9.2,
     +T77,E9.2,T87,E9.2,T97,E9.2,T107,E9.2,T117,E9.2,T127,E9.2,
     +T137,E9.2,/)
  963 IF (ISTYPE .GT. 2) GO TO 390
      JFLAG=0
      DO 372 I=IFLAG,NDATA-1
      IF ((JFLAG .EQ. 0) .AND. (FP(I+1) .GE. 0.)) JFLAG=I
  372 CONTINUE
      IF ((JFLAG .EQ. 0) .AND. (FP(1) .GE. 0.)) JFLAG=12
      IF (JFLAG .NE. 0) GO TO 385
      IF (IFLAG .NE. 1) GO TO 366
      FPSUM=0.
      VLEACH=0.
      GO TO 385
  366 CONTINUE
      DO 375 I=1,IFLAG-1
      IF ((JFLAG .EQ. 0) .AND. (FP(I+1) .GE. 0.)) JFLAG=I
  375 CONTINUE
  385 CONTINUE
      IF (NPRINT .LE. 1) GO TO 962
      WRITE(6,530) JFLAG
  962 CONTINUE
      DO 396 I=1,NDATA
  396 ALG(I)=0.
      IF (JFLAG .EQ. 0) GO TO 300
C*****************************************************************************
C     DEVELOP SOIL MOISTURE RETENTION TABLES (ST VS WL)
C*****************************************************************************
      CALL TABLE1 (KFLAG,FRACT,SLOP,YINTER,AVAILW)
      IF (NPRINT .LE. 1) GO TO 960
      WRITE(6,530) KFLAG,FRACT,AVAILW,FPSUM,FPPOS,FPNEG
  960 CONTINUE
      WL(IFLAG)=0.
      IF (FPSUM .GE. 0.) GO TO 376
      WL(IFLAG)=FPNEG
C*****************************************************************************
C     COMPUTE THE INITIAL SOIL MOISTURE DEFICIT FOR AN ARID REGION KFLAG
C        IDENTIFIES THE APPLICABLE RETENTION TABLE
C*****************************************************************************
      CALL WLINIT (WL(IFLAG),FPPOS,FPNEG,KFLAG,FRACT,SLOP,YINTER,
     1             AVAILW)
  376 CONTINUE
C*****************************************************************************
C     CALCULATE THE ACCUMULATED WATER POTENTIAL LOSS (WL)
C*****************************************************************************
      CALL AWPL (IFLAG,JFLAG,WL,FP,NDATA)
      IF (NPRINT.EQ.-9) THEN
        WRITE(19,105) 'Accumulated Water        '
        WRITE(19,665) 'Potential Loss (cm)      ',(WL(I), I=1,12)
      ENDIF
  959 CONTINUE
C*****************************************************************************
C     CALCULATE THE SOIL MOISTURE STORAGE (ST)
C*****************************************************************************
      CALL STORAG (IFLAG,JFLAG,KFLAG,NDATA,WL,ST,FP,SLOP,YINTER,
     1             FRACT,AVAILW)
      IF (NPRINT.EQ.-9) THEN
        WRITE(19,105) 'Soil Moisture            '
        WRITE(19,665) 'Storage (cm)             ',(ST(I), I=1,12)
      ENDIF
  958 CONTINUE
C*****************************************************************************
C     COMPUTE THE CHANGE IN SOIL MOISTURE STORAGE
C     COMPUTE THE ACTUAL EVAPOTRANSPIRATION
C     COMPUTE THE LEACHATE GENERATION (ALG)
C*****************************************************************************
      CALL LEACHG (ALG,ST,FMAX,CLOUD,TEMP,NDATA,AET)
      IF (NPRINT.EQ.-9) THEN
        WRITE(19,105) 'Actual                   '
        WRITE(19,665) 'Evapotranspiration (cm)  ',(AET(I), I=1,12)
        WRITE(19,665) 'Leachate Generation (cm) ',(ALG(I), I=1,12)
      ENDIF
  957 CONTINUE
C*****************************************************************************
C     ADD LEACHATE GENERATION TO NET POTENTIAL INFILTRATION
C*****************************************************************************
      FPSUM=0.
      DO 481 I=1,NDATA
  481 FPSUM=FPSUM+ALG(I)
  956 CONTINUE
      VLEACH=FPSUM/365.25
      GO TO 300
  390 CONTINUE
      FPSUM=VOLSUM
      VLEACH=FPSUM/365.25
      IF ((ISTYPE .LT. 4) .OR. (ISTYPE .EQ. 5)) GO TO 300
      FPPOS=0.
      DO 410 I=1,NDATA
      CALL RUNVOL (CN2,PRENUM(I),VOL1(I),PREC(I))
  410 FPPOS=FPPOS+VOL1(I)
      FPSUM=VOLSUM/FPPOS
C*****************************************************************************
C     READ FORMATS
C*****************************************************************************
 500  FORMAT (7A4,A1,4E10.3,/,4E10.3)
 510  FORMAT (8E10.3)
 530  FORMAT (I10,7E10.3)
C***********************************************************************
C     PROGRAM END
C***********************************************************************
  300 IF (NPRINT.EQ.-9) THEN
        IF (ISOURC.EQ.1.AND.ISTYPE.LT.3) THEN
          WRITE (19,606) FPSUM
        ENDIF
        IF (ISOURC.EQ.1.AND.ISTYPE.EQ.3) THEN
          WRITE (19,607) FPSUM
        ENDIF
        IF (JSCE.EQ.NUMSCE) CLOSE (UNIT=19)
      ENDIF
606   FORMAT (/,'Deep-Drainage Percolation (cm/yr): ',1PE9.2,///)
607   FORMAT (/,'Total Overland Runoff (cm/yr): ',1PE9.2,///)
      RETURN
      END
