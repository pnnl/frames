C     MEPAS RADCON: L_INTERP.FOR             Version Date: 11-04-1996
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                      FUNCTION Linear_INTERP                                *
C                                                                            *
C  Function L_INTERP interpolates the y value associated with a given        *
C  x value from a set of x,y data.
C                                                                            *
C  Written by:       John P. McDonald                                        *
C                    Battelle Northwest Laboratories                         *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    10/30/96 - JPM                                          *
C  Last Modified:    11/04/96 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: SUBROUTINE MERGE
C     Calls to: NONE
C     Common blocks referenced: NONE
C
C==== Significant Parameter Designation and Descriptions =====================
C
C     Parameter Set/               Location
C     Name      Used       Type    Exp/Imp    Parameter Description
C     --------- --------- ------  ----------- ----------------------------
C     NOT_FOUND SET       LOGICAL LOCAL       TRUE IF THE INTERVAL OF THE
C                                             X-ARRAY CONTAINING THE TARGET
C                                             X VALUE HAS NOT BEEN FOUND.
C                                             FALSE IF THE INTERVAL HAS BEEN
C                                             FOUND
C     ROWS      USED      INTEGER IMPORT      INDICATES THE NUMBER OF RECORDS
C                                             IN THE X- AND Y-ARRAYS.
C     X         USED      REAL    IMPORT      THE X VALUE AT WHICH TO
C                                             INTERPOLATE A Y VALUE
C     X-ARRAY   SET/USED  REAL    IMPORT      ARRAY HOLDING THE X-DATA
C     XCOL      USED      INTEGER IMPORT      INDICATES THE COLUMN OF THE 
C                                             X-ARRAY HOLDING THE X VALUES
C     Y-ARRAY   SET/USED  REAL    IMPORT      ARRAY HOLDING THE Y-DATA
C     YCOL      USED      INTEGER IMPORT      INDICATES THE COLUMN OF THE 
C                                             Y-ARRAY HOLDING THE Y VALUES
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/04/96     JPM  Function would occasionally return a negative value
C                       when the value at which to interpolate (X) is equal
C                       (or very nearly equal) to a value in the X_ARRAY, and
C                       the corresponding y-value is zero.  Problem is due to
C                       rounding the slope computation.  Thus, provisions were
C                       added such that when X equals a value in the X_ARRAY,
C                       then L_INTERP is set equal to the corresponding value
C                       in the Y_ARRAY.  In addition, a statement was added to
C                       set L_INTERP = 0.0 if L_INTERP < 0.0 (PH1-P001).
C
C==== FUNCTION CALL ==========================================================
C
      FUNCTION L_INTERP(X,X_ARRAY,Y_ARRAY,XCOL,YCOL,ROWS,MAXNTI,
     +                    MAXCON)
C
C==== COMMON Block Definitions ===============================================
C
C     None
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION Y_ARRAY(MAXNTI,MAXCON),X_ARRAY(MAXNTI,MAXCON)
C
C==== Variable Declarations ==================================================
C
      INTEGER ROWS,YCOL,XCOL,I,MAXNTI,MAXCON
      LOGICAL NOT_FOUND
      REAL L_INTERP,SLOPE,X
C
C==== DATA Statements ========================================================
C
C     None
C
C*****************************************************************************
C     Find the interval of the X_ARRAY containing the target X value
C*****************************************************************************
      I = 1
      NOT_FOUND = .TRUE.
      DO WHILE (I.LE.ROWS-1.AND.NOT_FOUND)
        IF (X.GE.X_ARRAY(I,XCOL).AND.X.LE.X_ARRAY(I+1,XCOL)) THEN
          NOT_FOUND = .FALSE.
        ELSE
          I = I + 1
        ENDIF
      ENDDO
C*****************************************************************************
C     If I is less than ROWS, then the interval containing the target X value
C     was found.  If the x values bounding the interval are equal, then we
C     assume the y values are also equal (if they weren't, then we don't have
C     a function) and set L_INTERP equal to the first y value.  This will also
C     prevent a divede by zero error in the SLOPE computation.  If the
C     bounding x values are not equal, but the target X value is equal to one
C     of the endpoints of the interval, then set L_INTERP equal to the
C     associated value in the Y_ARRAY.  Otherwise, use the point slope form of
C     the equation for a line to interplate the y value.  If I is not less than
C     ROWS, then the interval containing the target x value was not found and
C     the target value is outside the bounds of the array.  In this case,
C     return a zero.
C*****************************************************************************
      IF (I.LT.ROWS) THEN
        IF (X_ARRAY(I+1,XCOL).EQ.X_ARRAY(I,XCOL)) THEN
          L_INTERP = Y_ARRAY(I,YCOL)
        ELSE IF (X.EQ.X_ARRAY(I,XCOL)) THEN
          L_INTERP = Y_ARRAY(I,YCOL)
        ELSE IF (X.EQ.X_ARRAY(I+1,XCOL)) THEN
          L_INTERP = Y_ARRAY(I+1,YCOL)
        ELSE
          SLOPE = (Y_ARRAY(I+1,YCOL)-Y_ARRAY(I,YCOL))/
     +            (X_ARRAY(I+1,XCOL)-X_ARRAY(I,XCOL))
          L_INTERP = SLOPE*(X-X_ARRAY(I,XCOL))+Y_ARRAY(I,YCOL)
          IF (L_INTERP.LT.0.0) L_INTERP = 0.0
        ENDIF
      ELSE
        L_INTERP = 0.0
      ENDIF
      RETURN
      END
