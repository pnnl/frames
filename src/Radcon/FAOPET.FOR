C     MEPAS RADCON: FAOPET.FOR            Version Date: 01-25-1996
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                    SUBROUTINE FAOPET                                       *
C                                                                            *
C  Subroutine FAOPET is the main subroutine for computing potential          *
C  evapotranspiration.  FAOPET compares monthly values of evapotranspiration *
C  (matrix PET dimensions 12 x 3) calculated by three different methods      *
C  (from subroutines BLANEY, CORPEN, and PENMAN)                             *
C     1) BLANEY-CRIDDLE (FAO)                                                *
C     2) MODIFIED PENMAN (FAO) WITH C = 1.0                                  *
C     3) MODIFIED PENMAN (FAO) WITH CORRECTION FACTOR                        *
C  and chooses the most conservative value.  Each conservative value/month   *
C  is then stored in the array CLOUD(I).  FAOPET is called from the          *
C  subroutines AWPL, BLANEY, CORPEN, PENMAN, and FUNCTION.                   *
C                                                                            *
C  This is a computer program based on 'Crop Water Requirements' by          *
C  J. Doorenbos and W. O. Pruitt, Irrigation and Drainage Paper 24           *
C  (Second Edition), Water Resources Development and Management Service,     *
C  Land and Water Development Division, F.A.O. of United Nations, Rome.      *
C                                                                            *
C  Most portions of the FAO potential evapotranspiration computer program    *
C  were developed in 1977 by:                                                *
C                                                                            *
C  Written by:     S.K. Gupta, W.O. Pruitt, J. Lonczak, and K.K. Tanji       *
C                  Department of Land, Air, and Water Resources              *
C                  Water Science and Engineering Section                     *
C                  University of California                                  *
C                  Davis, California USA                                     *
C                                                                            *
C  Modified by:    Gene Whelan                                               *
C                  Battelle Pacific Northwest Laboratories                   *
C                  P.O. Box 999                                              *
C                  Richland, WA  99352                                       *
C                                                                            *
C  Creation Date:  01/19/89 (Converted to PC)                                *
C  Last Modified:  01/25/96 - JPM                                            *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: SUBROUTINE AWPL
C     Calls to:  BLANEY, CORPEN, PENMAN, and FUNCTION
C     Common blocks referenced: INDEX3
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- ----- -------- -------------- --------------------------------
C
C     CLOUD     USED   REAL    ARG - IMPORT   STORAGE ARRAY FOR THE MOST CON- 
C                                             SERVATIVE VALUES OF EVAPOTRANS- 
C                                             PIRATION PER MONTH              
C     NRATIO    SET    REAL    LOCAL          RATIO OF ACTUAL TO MAXIMUM      
C                                             POSSIBLE SUNSHINE HOURS         
C     PET       USED   REAL    ARG - IMPORT   POTENTIAL EVAPOTRANSPIRATION    
C     RA        SET    REAL    LOCAL          EXTRATERRESTRIAL RADIATION      
C     RN        SET    REAL    LOCAL          RS CORRECTED FOR REFLECTION OF  
C                                             LAND SURFACE                    
C     RS        SET    REAL    LOCAL          RA CORRECTED FOR SOLAR RADIATION
C     UDAY      SET    REAL    LOCAL          AVERAGE MONTH DAYTIME WIND SPEED
C     UU        SET    REAL    LOCAL          AVERAGE WIND SPEED; = WINDV     
C     WINDV     USED   REAL    ARG - EXPORT   AVERAGE WIND SPEED              
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C
C     11/25/91     JWB  Added required comments and header for MEPAS QA plan  
C     07/28/95     JPM  Warning message is now written to the *.WRN file
C                       instead of the *.WLS file.
C     01/25/96     JPM  Corrected an error - U24 was not ever being set equal
C                       to the wind speed (RAD-P059)
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE FAOPET (MONTH,NDATA,LAT,ALT,UHT,ELLCD,PET,DAYS,TEMP,
     1                   WINDV,CLOUD)
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'INDEX3.WTN'
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION PET(12,3),DAYS(12),TEMP(12),WINDV(12),CLOUD(12)
C
C==== Variable Declarations ==================================================
C
      REAL LAT,NACT,NTBL,NTBL1,NTBL2,NRATIO
      REAL NNTBL(11,12),RRAN(11,12),RRAS(11,12)
C
C==== DATA Statements ========================================================
C
C*****************************************************************************
C     THE VALUE OF SIGMA IS EQUIVALENT TO USING L=586 CAL/GM WATER BECAUSE    
C     SIGMA = 11.71*10E-8 CAL /CM.CM
C*****************************************************************************
      DATA SIGMA / 2.0E-9 /
      DATA RGAS / 4620. /
C
      DATA NNTBL
     1 / 12.1,11.8,11.6,11.3,11.0,10.7,10.4,10.1, 9.6, 9.2, 8.5,
     2   12.1,11.9,11.8,11.6,11.5,11.3,11.1,11.0,10.7,10.4,10.1,
     3   12.1,12.0,12.0,12.0,12.0,12.0,12.0,11.9,11.9,11.9,11.8,
     4   12.1,12.2,12.3,12.5,12.6,12.7,12.9,13.1,13.3,13.5,13.8,
     5   12.1,12.3,12.6,12.8,13.1,13.3,13.6,14.0,14.4,14.8,15.4,
     6   12.1,12.4,12.7,13.0,13.3,13.7,14.0,14.5,15.0,15.6,16.3,
     7   12.1,12.3,12.6,12.9,13.2,13.5,13.9,14.3,14.7,15.3,15.9,
     8   12.1,12.3,12.4,12.6,12.8,13.0,13.2,13.5,13.7,14.1,14.5,
     9   12.1,12.1,12.1,12.2,12.3,12.3,12.4,12.4,12.5,12.6,12.7,
     A   12.1,12.0,11.8,11.8,11.7,11.6,11.5,11.3,11.2,11.0,10.8,
     B   12.1,11.9,11.6,11.4,11.2,10.9,10.6,10.3,10.0, 9.6, 9.1,
     C   12.1,11.8,11.5,11.2,10.9,10.6,10.2, 9.8, 9.3, 8.8, 8.1 /
C
      DATA RRAN
     1 / 15.0,14.1,13.2,12.2,11.2,10.1, 8.9, 7.6, 6.4, 5.1, 3.8,
     2   15.5,14.9,14.3,13.5,12.7,11.7,10.7, 9.6, 8.5, 7.3, 6.1,
     3   15.7,15.6,15.3,14.9,14.4,13.7,13.0,12.2,11.3,10.3, 9.3,
     4   15.3,15.5,15.6,15.7,15.6,15.5,15.2,14.7,14.2,13.5,12.7,
     5   14.4,15.0,15.5,16.0,16.3,16.4,16.5,16.4,16.3,16.1,15.7,
     6   13.9,14.6,15.2,15.8,16.3,16.7,17.0,17.2,17.3,17.3,17.2,
     7   14.1,14.7,15.3,15.8,16.3,16.6,16.7,16.8,16.7,16.6,16.4,
     8   14.8,15.2,15.5,15.8,15.9,15.8,15.7,15.5,15.1,14.6,14.0,
     9   15.3,15.3,15.3,15.1,14.8,14.5,13.9,13.2,12.5,11.7,10.9,
     A   15.4,15.1,14.6,14.1,13.4,12.6,11.7,10.7, 9.6, 8.5, 7.2,
     B   15.1,14.4,13.6,12.7,11.7,10.6, 9.5, 8.2, 7.0, 5.6, 4.3,
     C   14.8,13.9,13.0,11.9,10.8, 9.5, 8.3, 7.0, 5.7, 4.3, 3.9 /
C
      DATA RRAS
     1 / 15.0,15.7,16.4,16.9,17.3,17.6,17.8,17.9,17.9,17.7,17.4,
     2   15.5,15.9,16.2,16.4,16.5,16.5,16.3,16.0,15.7,15.2,14.6,
     3   15.7,15.7,15.6,15.3,15.0,14.6,14.0,13.4,12.6,11.7,10.7,
     4   15.3,14.8,14.3,13.6,12.9,12.1,11.2,10.2, 9.2, 8.1, 7.0,
     5   14.4,13.7,12.9,12.0,11.0,10.0, 8.9, 7.8, 6.6, 5.4, 4.2,
     6   13.9,13.0,12.1,11.1,10.0, 8.9, 7.8, 6.6, 5.4, 4.2, 3.0,
     7   14.1,13.3,12.4,11.4,10.4, 9.3, 8.2, 7.1, 5.9, 4.7, 3.5,
     8   14.8,14.2,13.5,12.8,11.9,11.0,10.0, 9.0, 7.9, 6.7, 5.6,
     9   15.3,15.1,14.8,14.4,14.0,13.4,12.7,11.9,11.0,10.0, 8.8,
     A   15.4,15.7,15.9,15.9,15.8,15.6,15.2,14.8,14.2,13.6,12.8,
     B   15.1,15.7,16.2,16.7,17.0,17.2,17.2,17.1,17.0,16.7,16.4,
     C   14.8,15.6,16.3,16.9,17.4,17.7,18.0,18.2,18.3,18.3,18.1 /
C
C*****************************************************************************
C     PMB IS DERIVED FROM LONG TIME MEAN PMB FOR NUMBER OF STATION
C     IN AFRICA (CLIMATE OF AFRICA,VOL 10,WORLD SURVEY OF CLIMATOLOGY)
C*****************************************************************************
C
      PMB=1013.-0.1152*ALT+5.44*10.E-6*ALT*ALT
C
      DO 200 I=1,NDATA
    6 READ(5,530) MONTH,RHMAX,RHMIN
      TMEAN=TEMP(I)
      IF (TMEAN .LE. 0.) GO TO 200
      NACT=CLOUD(I)
      NRATIO=1.-0.016*NACT-0.0084*NACT**2.
      URATIO=2.
      UDAY=WINDV(I)*1.33
   10 IDAYYY=IDAYYY+1
      EA=0.0
      IF (NPRINT .LT. 3) GO TO 25
   20 IF (NPRINT .LE. 1) GO TO 25
      WRITE(6,634) MONTH,TMEAN,RHMAX,RHMIN,UDAY,U24,NACT,NRATIO,URATIO
C*****************************************************************************
C     CALCULATING RHMEAN AND CONVERTING TEMPERATURE DATA TO KELVIN
C*****************************************************************************
   25 IF ((RHMAX .GT. 100.) .OR. (RHMIN .GT. 100.))
     C WRITE(11,624) RHMAX,RHMIN
      TK=TMEAN+273.16
C*****************************************************************************
C     ADJUSTING FOR MEASUREMENT HEIGHT OF WIND SPEED IF OTHER THAN 2 M
C*****************************************************************************
      IF (UHT .LE. 2.) THEN
        A=0.22
      ELSE
        A=0.17
      ENDIF
      UHTCF=(2./UHT)**A
      UDAY=UDAY*UHTCF
      U24=WINDV(I)*UHTCF
C*****************************************************************************
C     CORRECTION FACTORS MULTIPLIED: CONVERTS U24 M/S TO KM/DAY
C*****************************************************************************
      U24=U24*86.4
C*****************************************************************************
C     CALCULATION OF EA AND ED
C*****************************************************************************
      EA=VAPORP(TMEAN)
      RHMEAN=(RHMAX+RHMIN)/2.
      ED=RHMEAN*EA/100.
C*****************************************************************************
C     THIS SECTION CONVERTS CLOUDINESS DATA (NACT) TO THE RATIO OF ACTUAL
C     TO POSSIBLE SUNSHINE (NRATIO), IF NRATIO IS ORIGINALLY ASSIGNED ZERO
C*****************************************************************************
   55 IF (NRATIO .EQ. 0.) NRATIO=0.95-0.066*NACT-0.0023*NACT*NACT
C*****************************************************************************
C    THIS SECTION CALCULATES A VALUE FOR EXTRATERRESTRIAL RADIATION (RA) USING
C    VARIABLES RRAN & RRAS (DATA STATEMENTS)
C*****************************************************************************
C
   65 LL=INT(LAT/5)*5
      IF (LAT .GT. 50.) LL=50
      L1=LL/5+1
      L2=L1+1
      IF (L2 .GT. 11) L2=11
      FAC1=(LAT-LL)/5.0
      MONTH1=MONTH
      MONTH2=MONTH
      RA1=RRAN(L1,MONTH1)+FAC1*(RRAN(L2,MONTH1)-RRAN(L1,MONTH1))
      RA2=RRAN(L1,MONTH2)+FAC1*(RRAN(L2,MONTH2)-RRAN(L1,MONTH2))
      FAC2=0.5
      RA=RA1+FAC2*(RA2-RA1)
C*****************************************************************************
C     NRATIO AND RS ARE CALCULATED IF NOT GIVEN
C*****************************************************************************
      IF (NRATIO .GT. 1.) NRATIO=0.999
      IF (NRATIO .LT. 0.) NRATIO = 0.0
      RS=(0.25+0.5*NRATIO)*RA
C*****************************************************************************
C     CALCULATION OF 'W' USING PMB, EA, AND T.
C*****************************************************************************
      GG=0.0006595*PMB
      D=(EA/TK)*(6790.4985/TK-5.02808)
      W=D/(D+GG)
      TK=TMEAN+273.16
      FT=SIGMA*(TK**4)
      FED=0.34-0.044*SQRT(ED)
      FNN=0.1+0.9*NRATIO
      RN=0.75*RS-FT*FED*FNN
      IF (NPRINT .LE. 1) GO TO 135
      WRITE(6,631)
      WRITE(6,635) MONTH,TMEAN,RHMAX,RHMIN,RHMEAN,EA,ED,UDAY,U24,
     C NACT,NRATIO,RS,RN,URATIO,W
  135 CALL BLANEY (LAT,MONTH,TMEAN,RHMIN,NRATIO,UDAY,PET(I,1))
      IF (PET(I,1) .LE. 0.) PET(I,1)=0.
      CALL PENMAN (W,TMEAN,EA,ED,U24,RS,NRATIO,PET(I,2),RN,ETFAC)
      IF (PET(I,2) .LE. 0.) PET(I,2)=0.
      CALL CORPEN (UDAY,U24,URATIO,RHMAX,RS,PET(I,2),PET(I,3))
      IF (PET(I,3) .LE. 0.) PET(I,3)=0.
  200 CONTINUE
      IF (NPRINT .LE. 1) GO TO 136
      WRITE(6,655)
      WRITE(6,660) (I, PET(I,1),PET(I,2),PET(I,3), I=1,NDATA)
  136 CONTINUE
      DO 228 I=1,NDATA
      PET(I,1)=PET(I,1)*DAYS(I)/10.
      PET(I,2)=PET(I,2)*DAYS(I)/10.
  228 PET(I,3)=PET(I,3)*DAYS(I)/10.
      IF (NPRINT .LE. 0) GO TO 441
      WRITE(6,655)
      WRITE(6,660) (I, PET(I,1),PET(I,2),PET(I,3), I=1,NDATA)
C*****************************************************************************
C     CHOOSE MOST CONSERVATIVE PET (MM) FOR EACH MONTH
C     COMPUTE MONTHLY PET
C     STORE MONTHLY PET IN 'CLOUD'
C*****************************************************************************
  441 continue
      DO 230 I=1,NDATA
      IF (PET(I,1) .LT. PET(I,2)) GO TO 261
      IF (PET(I,2) .LT. PET(I,3)) GO TO 262
  263 CLOUD(I)=PET(I,3)
      GO TO 229
  261 IF (PET(I,3) .LT. PET(I,1)) GO TO 263
      CLOUD(I)=PET(I,1)
      GO TO 229
  262 CLOUD(I)=PET(I,2)
  229 IF (NPRINT .GT. 0) WRITE(6,660) I,CLOUD(I)
  230 CONTINUE
C
C*****************************************************************************
C     READ FORMATS
C*****************************************************************************
 500  FORMAT (7A4,A1,I2,5E10.3)
 530  FORMAT (I10,7E10.3)
C*****************************************************************************
C     WRITE FORMATS
C*****************************************************************************
 624  FORMAT (
     1 /2x,'WARNING --> RHMAX = ',1PE10.3,' RHMIN = ',1PE10.3,
     2 /2x,'A relative humidity value exceeds 100%'/)
 631  FORMAT (1x,'MONTH   TMEAN   RHMAX   RHMIN   RHMEAN   EA   ED
     C UDAY   U24   SUNHRS   NRATIO   SOLRAD   RN   URATIO   W '/)
 634  FORMAT (1x,I4,1P8E10.3)
 635  FORMAT (1x,I3,1P12E10.3/4X,1P3E10.3/)
 655  FORMAT (2X,' MONTH',5X,'BLANEY',2X,'PENMAN',3X,
     C 'CORR PEN'/)
 660  FORMAT (I10,1P3E10.3)
C*****************************************************************************
C     PROGRAM END
C*****************************************************************************
  300 CONTINUE
      RETURN
      END
