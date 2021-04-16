C     MEPAS RADCON: SEDOVR.FOR            Version Date: 10-11-1994
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE SEDOVR                                *
C                                                                            *
C  Subroutine SEDOVR calculates the overland sediment transport flux         *
C  (SEDFLX) based on equation 4.14 from Whelan et al. 1987.  The flux        *
C  is calculated using the Universal Soil Loss Equation (USLE).              *
C  Subroutine SEDOVR is called from subroutine LEACH1.                       *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    10/11/94 - JPM                                          *
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
C==== Significant Parameter Designation and Descriptions =====================
C
C     Parameter Set/           Location 
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- ----- -------- -------------- --------------------------------
C     ISTORM    SET   INTEGER  LOCAL          INDEX INDICATING WHAT PART OF   
C                                             THE COUNTRY THE SITE IS LOCATED 
C     OSLOPE    SET   REAL     LOCAL          OVERLAND SLOPE OF SITE IN (%)   
C     RFACTR    SET   REAL     LOCAL          RAINFALL EROSIVITY FACTOR       
C     SEDFLX    SET   REAL     ARG - EXPORT   OVERLAND SEDIMENT TRANSPORT FLUX
C     SLFACT    SET   REAL     LOCAL          SLOPE LENGTH FACTOR             
C     SMRUNOF   USED  REAL     ARG - IMPORT   PRECIPITATION AVAILABLE FOR
C                                             SNOWMELT SEDIMENT FLUX - SUM
C                                             OF PRECIP STORED AS SNOW AS WELL
C                                             AS RAINFALL DURING SNOWMELT
C                                             MONTHS ("SNOWMELT RUNOFF")
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/26/91     JWB  Added required comments and header for MEPAS QA plan
C     10/10/94     JPM  Changed the name of variable STOR1 to SMRUNOF.  Fixed
C                       a mistake in computing the R-Factor - to adjust for
C                       the snowmelt sediment flux, we were dividing by a
C                       factor of 1.5 when we should multiply by 1.5.
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE SEDOVR (SEDFLX,SMRUNOF)
C
C==== COMMON Block Definitions ===============================================
C
C     None
C
C==== DIMENSION Statements ===================================================
C
C     None
C
C==== Variable Declarations ==================================================
C
      REAL KFACTR
C
C==== DATA Statements ========================================================
C
C     None
C
      READ(5,1) ISTORM,P26,OSLOPE,KFACTR,CFACTR,PFACTR
1     FORMAT (I5,7E10.3)
C*****************************************************************************
C     R-FACTOR CALCULATIONS USES SMRUNOF IN CALCULATING SNOWMELT EFFECTS
C     ENGLISH UNITS
C*****************************************************************************
      GO TO (10,20,30), ISTORM
C*****************************************************************************
C     USING EQUATION 4.15 FROM WHELAN ET AL. 1987
C*****************************************************************************
   10 RFACTR=16.55*P26**2.20
      GO TO 40
C*****************************************************************************
C     USING EQUATION 4.16 FROM WHELAN ET AL. 1987
C*****************************************************************************
   20 RFACTR=27.00*P26**2.17
      GO TO 40
C*****************************************************************************
C     USING EQUATION 4.17 FROM WHELAN ET AL. 1987
C*****************************************************************************
   30 RFACTR=10.20*P26**2.20
   40 CONTINUE
      RFACTR=RFACTR+SMRUNOF/2.54*1.5
C*****************************************************************************
C     SLOPE-LENGTH FACTOR CALCULATIONS ASSUMES A LENGTH OF 72.5 FT 
C     (I.E., UNIT LENGTH) UNITLESS  EQUATION 4.22 (WHELAN ET AL. 1987)
C*****************************************************************************
      A=SQRT(OSLOPE**2.+1.0E+04)
      SLFACT=65.41*(OSLOPE/A)**2.+4.56*OSLOPE/A+0.065
C*****************************************************************************
C     TOTAL AVERAGE-ANNUAL SEDIMENT FLUX; SEDFLX=R K SL C P
C     OVERLAND SEDIMENT FLUX CONVERTED FROM TONS/ACRE/YR TO G/CM**2/YR
C     WITH THE MULTIPLICATION FACTOR 0.002246; EQ. 4.14 (WHELAN ET AL. 1987)
C*****************************************************************************
      SEDFLX=RFACTR*KFACTR*SLFACT*CFACTR*PFACTR*0.002246
      WRITE(6,2)
2     FORMAT (//,'OVERLAND Sediment Flux Calculations')
      WRITE(6,3) ISTORM,P26,OSLOPE,RFACTR,KFACTR,SLFACT,CFACTR,
     1           PFACTR,SEDFLX
3     FORMAT(/,
     1       'Storm Type Index (ISTORM)       = ',I10/
     2       '2-yr, 6-hr Rainfall Event (P26) = ',1p,E10.2,' IN'/
     3       'Overland Slope (OSLOPE)         = ',E10.2,' %'/
     4       'R-FACTOR (RFACTR)               = ',E10.2/
     5       'K-FACTOR (KFACTR)               = ',E10.2/
     6       'SLOPE-LENGTH FACTOR (SLFACT)    = ',E10.2/
     7       'C-FACTOR (CFACTR)               = ',E10.2/
     8       'P-FACTOR (PFACTR)               = ',E10.2/
     9       'Sediment Flux (SEDFLX)          = ',E10.2,' g/cm**2/yr'/)
C
      RETURN
      END
