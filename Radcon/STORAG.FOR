C     MEPAS RADCON: STORAG.FOR            Version Date: 11-26-1991            
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                       SUBROUTINE STORAG                                    *
C                                                                            *
C  Subroutine STORAG computes the cumulative soil moisture storage.  The     *
C  calculations are based on equation 3.49 from Whelan et al. 1987 which     *
C  sums the soil moisture from previous with the percolation of the current  *
C  month.  Subroutine STORAG is called from subroutine LEACH1.               *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Northwest Laboratories                         *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Modified by:      Robert Brockhaus - PNL                                  *
C                    John Buck - PNL                                         *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: SUBROUTINE LEACH1
C     Calls to: SUBROUTINE STWL
C     Common blocks referenced: None
C
C==== Significant Parameter Designation and Descriptions =====================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- ----- ------- --------------- --------------------------------
C     AVAILW    USED   REAL    ARG - IMPORT   AVAILABLE WATER CAPACITY        
C     IFLAG     USED   INTEGER ARG - IMPORT   INDEX FOR LAST MONTH WITH       
C                                             POSITIVE PERCOLATION            
C     JFLAG     USED   INTEGER ARG - IMPORT   INDEX FOR LAST MONTH WITH       
C                                             NEGATIVE PERCOLATION            
C     ST        SET    REAL    ARG - EXPORT   SOIL MOISTURE STORAGE           
C     WL        USED   REAL    ARG - IMPORT   ACCUMULATED POTENTIAL WATER LOSS
C     WLO       SET    REAL    LOCAL          ACCUMULATED WATER LOSS DURING   
C                                             THE WET SEASON                  
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/26/91     JWB  Added required comments and header for MEPAS QA plan  
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE STORAG (IFLAG,JFLAG,KFLAG,NDATA,WL,ST,FP,SLOP,YINTER,
     1                   FRACT,AVAILW)
C
C==== COMMON Block Definitions ===============================================
C
C     None
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION WL(12),ST(12),FP(12),SLOP(11),YINTER(11)
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
C     NOTE UNITS IN CM
C*****************************************************************************
      JJJ=1
      AWATER=AVAILW
      IF (IFLAG .GT. JFLAG) GO TO 10
      DO 20 I=IFLAG,JFLAG
      WLO=ABS(WL(I))
      CALL STWL (JJJ,KFLAG,ST(I),WLO,FRACT,SLOP,YINTER,AVAILW)
      IF (ST(I) .GT. AWATER) ST(I)=AWATER
   20 CONTINUE
      DO 30 I=JFLAG+1,NDATA
      ST(I)=ST(I-1)+FP(I)
      IF (ST(I) .GT. AWATER) ST(I)=AWATER
   30 CONTINUE
      IF (IFLAG .EQ. 1) GO TO 42
      ST(1)=ST(12)+FP(1)
      IF (ST(1) .GT. AWATER) ST(1)=AWATER
      IF (IFLAG .EQ. 2) GO TO 42
      DO 40 I=2,IFLAG-1
      ST(I)=ST(I-1)+FP(I)
      IF (ST(I) .GT. AWATER) ST(I)=AWATER
   40 CONTINUE
   42 CONTINUE
      GO TO 52
   10 DO 60 I=IFLAG,NDATA
      WLO=ABS(WL(I))
      CALL STWL (JJJ,KFLAG,ST(I),WLO,FRACT,SLOP,YINTER,AVAILW)
      IF (ST(I) .GT. AWATER) ST(I)=AWATER
   60 CONTINUE
      DO 70 I=1,JFLAG
      WLO=ABS(WL(I))
      CALL STWL (JJJ,KFLAG,ST(I),WLO,FRACT,SLOP,YINTER,AVAILW)
      IF (ST(I) .GT. AWATER) ST(I)=AWATER
   70 CONTINUE
      DO 50 I=JFLAG+1,IFLAG-1
      ST(I)=ST(I-1)+FP(I)
      IF (ST(I) .GT. AWATER) ST(I)=AWATER
   50 CONTINUE
   52 CONTINUE
      RETURN
      END
