C     MEPAS RADCON: WRITDATA.FOR             Version Date: 03-23-1999
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                      Subroutine WRITE_DATA                                 *
C                                                                            *
C  Subroutine WRITe DATA writes the flux or concentration results to the     *
C  *.WFF or *.WCF file.                                                      *
C                                                                            *
C  Written by:       John P. McDonald                                        *
C                    Battelle Northwest Laboratories                         *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    11/07/96 - JPM                                          *
C  Last Modified:    03/23/99 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: SUBROUTINE OUTPUT
C     Calls to: NONE
C     Common blocks referenced: MAXIMUM
C
C==== Significant Parameter Designation and Descriptions =====================
C
C     Parameter Set/               Location
C     Name      Used       Type    Exp/Imp    Parameter Description
C     --------- --------- ------  ----------- ----------------------------
C     CS        USED      REAL    IMPORT      ARRAY HOLDING THE COMPUTATION
C                                             RESULTS (FLUX OR CONC)
C     M3        USED      INTEGER IMPORT      THE COLUMN OF THE TIM- AND CS-
C                                             ARRAYS TO WRITE OUT
C     ROWS      USED      INTEGER IMPORT      INDICATES THE NUMBER OF RECORDS
C                                             IN THE TIM- AND CS-ARRAYS
C     TIM       USED      REAL    IMPORT      ARRAY HOLDING THE TOTAL TIME
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     03/23/99     JPM  Writing out more decimal places - code was rounding
C                       the times such that multiple concentrations were
C                       occurring for the same time (PH1-P018).
C
C==== FUNCTION CALL ==========================================================
C
      SUBROUTINE WRITE_DATA(ROWS,TIM,CS,M3,UNITS)
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'MAXIMUM.WTN'
C
C==== DIMENSION Statements ===================================================
C
C     None
C
C==== Variable Declarations ==================================================
C
      INTEGER ROWS,M3,I
      REAL TIM(MAXNTI,MAXCON),CS(MAXNTI,MAXCON),UNITS
C
C==== DATA Statements ========================================================
C
C     None
C
      DO I = 1,ROWS
        WRITE (14,100) TIM(I,M3),CS(I,M3)*UNITS
      ENDDO
100   FORMAT (1P,2(E14.7,','))
      RETURN
      END
