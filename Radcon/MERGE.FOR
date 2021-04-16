C     MEPAS RADCON: MERGE.FOR             Version Date: 05-15-1998
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE MERGE                                 *
C                                                                            *
C  Subroutine MERGE takes two different sets of x,y points (i.e., two        *
C  different functions) and sums them into a single set (or function).       *
C  Specifically, it merges two different sets of flux and/or concentration   *
C  time pairs into one set.                                                  *
C                                                                            *
C  Written by:       John P. McDonald                                        *
C                    Battelle Northwest Laboratories                         *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    10/30/96 - JPM                                          *
C  Last Modified:    05/15/98 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: SUBROUTINE RADCOND
C     Calls to: FUNCTION L_INTERP
C     Common blocks referenced: MAXIMUM
C
C==== Significant Parameter Designation and Descriptions =====================
C
C     Parameter Set/               Location
C     Name      Used       Type    Exp/Imp    Parameter Description
C     --------- --------- ------  ----------- ----------------------------
C     COL1      USED      INTEGER IMPORT      INDICATES THE COLUMN OF THE
C                                             X- AND Y-ARRAYS HOLDING THE
C                                             FIRST DATA SET
C     COL2      USED      INTEGER IMPORT      INDICATES THE COLUMN OF THE
C                                             X- AND Y-ARRAYS HOLDING THE
C                                             SECOND DATA SET
C     DELTA     SET       REAL    LOCAL       THE TIMESTEP FOR THE FINAL
C                                             MERGED ARRAY
C     ROWS      USED      INTEGER IMPORT      INDICATES THE NUMBER OF RECORDS
C                                             IN THE X- AND Y-ARRAYS.
C     TEMP      SET       REAL    LOCAL       TEMPORARY HOLDING ARRAY. THREE 
C                                             COLUMNS AND UP TO MAXNTI ROWS.
C                                             COLUMN 1 HOLDS THE TIMES FOR THE
C                                             MERGED DATA SET, COLUMN 2 HOLDS
C                                             INTERPOLATED Y-DATA FOR THE
C                                             FIRST DATA SET, AND COLUMN 3
C                                             HOLDS INTERPOLATED Y-DATA FOR THE
C                                             SECOND DATA SET.
C     X-ARRAY   SET/USED  REAL    IMPORT      ARRAY HOLDING THE X-DATA
C     Y-ARRAY   SET/USED  REAL    IMPORT      ARRAY HOLDING THE Y-DATA
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     04/29/97     JPM  When the x-series data for both datasets are
C                       identical, just add the y-data together (PH1-P009).
C     05/15/98     JPM  If the y-data in one column is all zero and the
C                       other is not, just use the other column as the sum.
C                       This prevents the times associated with the zero
C                       column from being used to set the times for the sum
C                       and results in better resolution (RAD-E054).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE MERGE(X_ARRAY,Y_ARRAY,COL1,COL2,ROWS)
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'MAXIMUM.WTN'
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION Y_ARRAY(MAXNTI,MAXCON),X_ARRAY(MAXNTI,MAXCON)
      DIMENSION TEMP(MAXNTI,3)
C
C==== Variable Declarations ==================================================
C
      INTEGER COL1,COL2,ROWS,I
      REAL DELTA,L_INTERP
      LOGICAL EQUAL_X,COL1_ZERO,COL2_ZERO
C
C==== DATA Statements ========================================================
C
C     None
C
C*****************************************************************************
C     Check to see if the x-data for both datasets are identical.
C*****************************************************************************
      EQUAL_X = .TRUE.
      DO I = 1,ROWS
        IF (X_ARRAY(I,COL1).NE.X_ARRAY(I,COL2)) EQUAL_X = .FALSE.
      ENDDO
C*****************************************************************************
C     Check to see if the y-data in COL1 are all zero.
C*****************************************************************************
      COL1_ZERO = .TRUE.
      DO I = 1,ROWS
        IF (Y_ARRAY(I,COL1).NE.0.0) COL1_ZERO = .FALSE.
      ENDDO
C*****************************************************************************
C     Check to see if the y-data in COL2 are all zero.
C*****************************************************************************
      COL2_ZERO = .TRUE.
      DO I = 1,ROWS
        IF (Y_ARRAY(I,COL2).NE.0.0) COL2_ZERO = .FALSE.
      ENDDO
C*****************************************************************************
C     If the x-data in both datasets are equal, then just sum the y-data
C*****************************************************************************
      IF (EQUAL_X) THEN
        DO I = 1,ROWS
          Y_ARRAY(I,COL2) = Y_ARRAY(I,COL1) + Y_ARRAY(I,COL2)
        ENDDO
C*****************************************************************************
C     If the y-data in COL2 are zero and they are nonzero in COL1, then the
C     sum is just the COL1 data.  So, move the COL1 data into COL2 for both
C     datasets.
C*****************************************************************************
      ELSE IF (COL2_ZERO.AND..NOT.COL1_ZERO) THEN
        DO I = 1,ROWS
          Y_ARRAY(I,COL2) = Y_ARRAY(I,COL1)
          X_ARRAY(I,COL2) = X_ARRAY(I,COL1)
        ENDDO
C*****************************************************************************
C     If the y-data in COL1 are zero and they are nonzero in COL2, then the
C     sum is just the COL2 data.  So, do nothing.
C*****************************************************************************
      ELSE IF (COL1_ZERO.AND..NOT.COL2_ZERO) THEN
        CONTINUE
      ELSE
C*****************************************************************************
C     Find the smallest and largest values in the X_ARRAY and store them in
C     the first and last slots of the first column of the TEMP array.
C*****************************************************************************
        TEMP(1,1) = MIN(X_ARRAY(1,COL1),X_ARRAY(1,COL2))
        TEMP(ROWS,1) = MAX(X_ARRAY(ROWS,COL1),X_ARRAY(ROWS,COL2))
C*****************************************************************************
C     Compute the step size for the x-data and populate the remaining slots
C     of column 1 of the TEMP array.
C*****************************************************************************
        DELTA = (TEMP(ROWS,1)-TEMP(1,1))/(ROWS-1)
        DO I = 2,ROWS-1
          TEMP(I,1) = TEMP(I-1,1)+DELTA
        ENDDO
C*****************************************************************************
C     Populate columns 2 and 3 of the TEMP array with interpolated y-values
C     from the first and second data sets, respectively.
C*****************************************************************************
        DO I = 1,ROWS
          TEMP(I,2) = L_INTERP(TEMP(I,1),X_ARRAY,Y_ARRAY,COL1,COL1,
     +                         ROWS,MAXNTI,MAXCON)
          TEMP(I,3) = L_INTERP(TEMP(I,1),X_ARRAY,Y_ARRAY,COL2,COL2,
     +                         ROWS,MAXNTI,MAXCON)
        ENDDO
C*****************************************************************************
C     Compute the sum of columns 2 and 3 of the TEMP array and store the new
C     x,y data in the second data set.
C*****************************************************************************
        DO I = 1,ROWS
          Y_ARRAY(I,COL2) = TEMP(I,2)+TEMP(I,3)
          X_ARRAY(I,COL2) = TEMP(I,1)
        ENDDO
      ENDIF
      RETURN
      END
