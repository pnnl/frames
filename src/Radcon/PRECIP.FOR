C     MEPAS RADCON: PRECIP.FOR            Version Date: 10-10-1994
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                       SUBROUTINE PRECIP                                    *
C                                                                            *
C  Subroutine PRECIP computes the adjusted monthly precipitation (PREC(I)).  *
C  It is called from subroutine LEACH1.                                      *
C                                                                            *
C  Written by:       Gene Whelan and John McDonald                           *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    10/10/94 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: SUBROUTINE LEACH1
C     Calls to: None
C     Common blocks referenced: None
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- ----- ------   ------------- ---------------------------------
C     SM        SET    REAL    LOCAL          TOTAL SNOWMELT                  
C     SMC       SET    REAL    LOCAL          SNOWMELT DUE TO CONVECTION      
C     SMP       SET    REAL    LOCAL          SNOWMELT FROM PRECIPITATION     
C     SMR       SET    REAL    LOCAL          SNOWMELT FROM RADIATION         
C     SMRUNOF   SET    REAL    LOCAL - ARG    TOTAL WATER AVAILABLE FOR
C                                             SNOWMELT SEDIMENT FLUX.  EQUALS
C                                             TOTAL INVENTORY OF PRECIP THAT
C                                             FELL AS SNOW PLUS THE PRECIP
C                                             DURING THAW MONTHS
C     SMVC      SET    REAL    LOCAL          SNOWMELT FROM VAPOR CONDENSATION
C     SNOVEL    SET    REAL    LOCAL          CONSTANT USED TO ADJUST FOR WIND
C                                             HEIGHT OF WIND MEASUREMENTS     
C     SNOWINV   SET    REAL    LOCAL          RUNNING INVENTORY OF SNOW ON
C                                             THE GROUND
C     VP        SET    REAL    LOCAL          VAPOR PRESSURE OF AIR           
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/26/91     JWB  Added required comments and header for MEPAS QA plan  
C     10/10/94     JPM  Restructured module using structured programming
C                       techniques.  Fixed over-counting of water available for
C                       snowmelt sediment flux (SMRUNOF).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE PRECIP (UHT,ALT,TEMP,RAIN,WINDV,CLOUD,DAYS,PREC,
     1                   SMRUNOF,NPRINT)
C
C==== COMMON Block Definitions ===============================================
C
C     None
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION TEMP(12),RAIN(12),WINDV(12),CLOUD(12),PREC(12),DAYS(12)
C
C==== Variable Declarations ==================================================
C
C     None
C
C==== DATA Statements ========================================================
C
C     None
C
C*****************************************************************************
C     INITIALIZATION OF VARIABLES
C*****************************************************************************
      SNOWINV=0.
      SMRUNOF=0.
C*****************************************************************************
C     COMPUTE THE ADJUSTED PRECIPITATION FOR THE MONTHS OF AUGUST THRU
C     DECEMBER, THEN JANUARY THRU JULY
C*****************************************************************************
      DO J=1,2
        IF (J.EQ.1) THEN
          ISTART=8
          IEND=12
        ELSE
          ISTART=1
          IEND=7
        ENDIF
        DO I=ISTART,IEND
          SMVC=0.
          SMC=0.
          SMR=0.
          SMP=0.
          SM=0.
C*****************************************************************************
C     DETERMINE CONSTANT TO ADJUST FOR HEIGHT OF WIND MEASUREMENTS USING
C     EQUATION 3.9 FROM WHELAN ET AL. 1987
C*****************************************************************************
          SNOVEL=1.59*WINDV(I)*UHT**(-0.17)
C*****************************************************************************
C     ADD THE RAINFALL TO THE INVENTORY OF SNOW ON THE GROUND (SNOWINV) AND
C     TO THE TOTAL WATER AVAILABLE FOR SNOWMELT SEDIMENT FLUX (SMRUNOF) DURING
C     MONTHS WHERE THE TEMPERATURE IS BELOW OR EQUAL TO FREEZING
C*****************************************************************************
          IF (TEMP(I).LE.0.0) THEN
            SNOWINV=SNOWINV+RAIN(I)
            SMRUNOF=SMRUNOF+RAIN(I)
            RAIN(I)=0.0
C*****************************************************************************
C     IF THE TEMPERATURE DURING THE CURRENT MONTH IS ABOVE FREEZING, CHECK
C     TO SEE IF THERE IS SNOW ON THE GROUND (SNOWINV > 0).  IF THERE IS,
C     THEN ADD THE PRECIP FOR THE CURRENT MONTH TO THE TOTAL WATER AVAILABLE
C     FOR SNOWMELT SEDIMENT FLUX (SMRUNOF), AND COMPUTE THE AMOUNT OF SNOWMELT
C*****************************************************************************
          ELSE
            IF (SNOWINV.GT.0.0) THEN
              SMRUNOF=SMRUNOF+RAIN(I)
              VP=VAPORP(TEMP(I))
C*****************************************************************************
C     MONTHLY ESTIMATE OF SNOWMELT FROM VAPOR CONDENSATION USING
C     EQUATION 3.20 FROM WHELAN ET AL. 1987
C*****************************************************************************
              SMVC=0.102*DAYS(I)*SNOVEL*(VP-6.11)
C*****************************************************************************
C     6 HR ESTIMATE OF SNOWMELT BY CONVECTION USING EQ 3.24 
C     FROM WHELAN ET AL. 1987
C*****************************************************************************
              SMC=4.*DAYS(I)*SNOVEL*TEMP(I)*10.**(-1.725-5.12E-05*ALT)
              II=I
              IF ((I .GE. 7) .AND. (I .LE. 9)) II=6
              IF ((I .EQ. 10) .OR. (I .EQ. 11)) II=4
              IF (I .EQ. 12) II=2
C*****************************************************************************
C     SNOWMELT FROM RADIATION USING EQ 3.27 FROM WHELAN ET AL. 1987
C*****************************************************************************
              SMR=DAYS(I)*(1.-0.75*CLOUD(I)/10.)*(0.15*II+0.43)
C*****************************************************************************
C     SNOWMELT FROM PRECIPITATION USING EQ 3.31 FROM WHELAN ET AL. 1987
C*****************************************************************************
              SMP=0.032*RAIN(I)*TEMP(I)
C*****************************************************************************
C     TOTAL SNOWMELT USING EQUATION 3.35 FROM WHELAN ET AL. 1987
C*****************************************************************************
              SM=SMVC+SMC+SMR+SMP
C*****************************************************************************
C     IF THE POTENTIAL SNOWMELT (SM) IS LARGER THAN THE AMOUNT OF SNOW ON THE
C     GROUND (THE SNOW INVENTORY, SNOWINV), THEN SET THE SNOWMELT EQUAL TO THE
C     SNOW INVENTORY, AND SET THE SNOW INVENTORY TO ZERO.  OTHERWISE, REDUCE
C     THE SNOW INVENTORY BY THE SNOWMELT DEPTH
C*****************************************************************************
              IF (SM.GT.SNOWINV) THEN
                SM=SNOWINV
                SNOWINV=0.0
              ELSE
                SNOWINV=SNOWINV-SM
              ENDIF
            ENDIF
          ENDIF
C*****************************************************************************
C     COMPUTE THE ADJUSTED PRECIPITATION
C*****************************************************************************
          PREC(I)=RAIN(I)+SM
        ENDDO
      ENDDO
      IF (NPRINT.GT.0) THEN
        WRITE(6,2)
2       FORMAT (/'Precipitation in cm by Month that is ',
     +          'Available for Leaching'/)
        WRITE(6,1) (I,PREC(I), I=1,12)
1       FORMAT (2X,I10,1PE10.3)
      ENDIF
      WRITE(6,3) SMRUNOF
3     FORMAT (/,'Total Water Available for Snowmelt Sediment Flux',
     +        ' is = ',1P,E10.2,' cm')
      RETURN
      END
