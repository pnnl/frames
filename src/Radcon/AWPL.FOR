C     MEPAS RADCON: AWPL.FOR              Version Date: 11-22-1991            
C     Copyright 1989 by Battelle Memorial Institute.  All rights reserved.    
C*****************************************************************************
C                                                                            *
C                      SUBROUTINE AWPL                                       *
C                                                                            *
C  Subroutine AWPL computes the accumulated water potential loss for humid   *
C  and arid regions using equation 3.45 from Whelan et al. 1987.  The water  *
C  loss for the current month (WL(I+1)) is calculated by summing the water   *
C  loss from the previous month (WL(I)) with the currents months potential   *
C  percolation (fp(I+1)).  Subroutine AWPL is called from subroutine LEACH1. *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    11/22/91 - JWB                                          *
C                                                                            *
C*****************************************************************************
C 
C=== Modular Organization ====================================================
C
C     Module of: RADCONPC
C     Called by: Subroutine LEACH1
C     Calls to: None
C     Common blocks referenced: None
C
C=== Significant Parameter Designation and Description =======================
C
C     Parameter   Set/             Location
C     Name        Used   Type    Export/Import  Parameter Description
C     ---------  ------  ------  -------------  ------------------------------
C     FP          USED   REAL    ARG-IMPORT     POTENTIAL PERCOLATION
C     IFLAG       USED   INTEGER ARG-IMPORT     INDEX FOR FIRST MONTH WITH 
C                                               NEGATIVE FP
C     JFLAG       USED   INTEGER ARG-IMPORT     INDEX FOR LAST MONTH WITH
C                                               NEGATIVE FP
C     NDATA       USED   INTEGER ARG-IMPORT     CONSTANT = NUMBER OF MONTHS-12
C     WL          SET    REAL    ARG-EXPORT     ACCUMULATED POTENTIAL H2O LOSS
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/22/91     JWB  Added required comments and header for MEPAS QA plan
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE AWPL (IFLAG,JFLAG,WL,FP,NDATA) 
C
C==== COMMON Block Definitions ===============================================
C
C     None
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION WL(12),FP(12)
C
C==== Variable Declarations ==================================================
C
C     None
C
C==== DATA Statements ========================================================
C
C     None
C
      IF (IFLAG .GT. JFLAG) GO TO 10
      DO 20 I=IFLAG,JFLAG-1
C*************************************************************************
C     WATER LOSS USING EQUATION 3.45 FROM WHELAN ET AL. 1987 
C*************************************************************************
   20 WL(I+1)=WL(I)+FP(I+1)
      GO TO 31
   10 DO 40 I=IFLAG,NDATA-1
C**************************************************************************
C     WATER LOSS USING EQUATION 3.45 FROM WHELAN ET AL. 1987 
C**************************************************************************
   40 WL(I+1)=WL(I)+FP(I+1)
      WL(1)=WL(12)+FP(1)
      IF (JFLAG .EQ. 1) GO TO 31
C
      DO 30 I=1,JFLAG-1
      WL(I+1)=WL(I)+FP(I+1)
   30 CONTINUE
   31 CONTINUE
      RETURN
      END
