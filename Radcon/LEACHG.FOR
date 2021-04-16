C     MEPAS RADCON: LEACHG.FOR            Version Date: 11-25-1991            
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                      SUBROUTINE LEACHG                                     *
C                                                                            *
C  Subroutine LEACHG calculates leachate generation per month based on the   *
C  difference between maximum infiltration and evapotranspiration.  The      *
C  leachate volume generated (ALG(L)) is the difference between the maximum  *
c  infiltration (FMAX(L)) calculated in subroutine LEACH1, adjusted for      *
c  changes in storage, and the actual evapotranspiration (CLOUD(I)) chosen   *
C  from the subroutine FAOPET.  Subroutine LEACHG is called from subroutine  *
c  LEACH1.                                                                   *
C                                                                            *
C  Written by:      Gene Whelan                                              *
C                   Battelle Pacific Northwest Laboratories                  *
C                   P.O. Box 999                                             *
C                   Richland, WA 99352                                       *
C                                                                            *
C  Creation Date    01/19/89 (Converted to PC)                               *
C  Last Modified:   11/25/91 - JWB                                           *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C    Module of:  RADCON
C    Called by:  SUBROUTINE LEACH1
C    Calls to:
C    Common blocks referenced: INDEX3
C
C==== Significant Parameter Designation and Description ======================
C
C    Parameter Set/           Location
C    Name      Used  Type     Export/Import  Parameter Description
C    --------- ----- -------  -------------  ---------------------------------
C     AET      SET   REAL     LOCAL          ACTUAL EVAPOTRANSPIRATION        
C     ALG      SET   REAL     ARG - EXPORT   LEACHATE GENERATION              
C     CLOUD    USED  REAL     ARG - IMPORT   ARRAY OF CONSERVATIVE PET VALUES 
C     DELTST   SET   REAL     LOCAL          CHANGE IN SOIL MOISTURE          
C     FMAX     USED  REAL     ARG - IMPORT   MAXIMUM MOISTURE AVAILABLE FOR   
C                                            PERCOLATION                      
C     FMAXST   SET   REAL     LOCAL          ACTUAL EVAPOTRANSPIRATION WHEN   
C                                            PET > DELST                      
C     NDATA    USED  INTEGER  ARG - IMPORT   SET EQUAL TO # OF MONTHS; = 12   
C     ST       USED  REAL     ARG - IMPORT   SOIL MOISTURE CONTENT            
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/25/91     JWB  Added required comments and header for MEPAS QA plan  
C     09/22/94     JPM  Made AET an array to store the actual ET values for
C                       printing to the Water Balance Report (*.WBR) when
C                       NPRINT = -9.  Passed between this module and LEACH1.
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE LEACHG (ALG,ST,FMAX,CLOUD,TEMP,NDATA,AET)
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'INDEX3.WTN'
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION ALG(12),ST(12),FMAX(12),CLOUD(12),TEMP(12),AET(12)
C
C==== Variable Declarations ==================================================
C
C     None
C
C==== DATA Statements ========================================================
C
C     None
C
      WRITE(6,2)
      DO 10 I=1,NDATA-1
      L=I+1
C*****************************************************************************
C     CHANGE IN SOIL MOISTURE USING EQ. 3.50 FROM WHELAN ET AL. 1987
C*****************************************************************************
      DELTST=ST(L)-ST(I)
C*****************************************************************************
C     ACTUAL EVAPOTRANSPIRATION USING EQ. 3.52 FROM WHELAN ET AL. 1987
C*****************************************************************************
      FMAXST=FMAX(L)-DELTST
      AET(L)=CLOUD(L)
      IF (CLOUD(L) .GT. FMAXST) AET(L)=FMAXST
      ALG(L)=0.
C*****************************************************************************
C     LEACHATE GENERATION USING EQUATION 3.54 FROM WHELAN ET AL. 1987 
C*****************************************************************************
      IF (TEMP(L) .GT. 0.) ALG(L)=FMAXST-AET(L)
      IF (NPRINT .LE. 1) GO TO 10
      WRITE(6,1) L,DELTST,FMAXST,AET(L),ALG(L)
   10 CONTINUE
      I=1
      DELTST=ST(I)-ST(12)
      FMAXST=FMAX(I)-DELTST
      AET(I)=CLOUD(I)
      IF (CLOUD(I) .GT. FMAXST) AET(I)=FMAXST
      IF (TEMP(I) .GT. 0.) ALG(I)=FMAXST-AET(I)
      IF (NPRINT .LE. 1) GO TO 950
      WRITE(6,1) I,DELTST,FMAXST,AET(I),ALG(I)
      WRITE(6,2)
    1 FORMAT (I10,1P12E10.3)
    2 FORMAT (//)
  950 CONTINUE
      RETURN
      END
