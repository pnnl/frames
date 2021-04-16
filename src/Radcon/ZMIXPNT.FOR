C     MEPAS RADCON: ZMIXPNT.FOR             Version Date: 05-06-1996
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                            SUBROUTINE ZMIXPNT                              *
C                                                                            *
C  Subroutine ZMIXPNT calculates the vertical mixing length from a point     *
C  source at the water table using Equation 5.54 in Whelan et al (1987),     *
C  with the exception that phi is an empirically determined function of      *
C  longitudinal travel distance and aquifer thickness (RAD-E046).            *
C                                                                            *
C                                                                            *
C  Written by:       John P. McDonald                                        *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    05/06/96 - JPM                                          *
C  Last Modified:                                                            *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: PLUME
C     Calls to: NONE
C     Common blocks referenced: NONE
C
C==== Significant Parameter Designation and Descriptions =====================
C
C     Parameter  Set/           Location
C     Name       Used   Type    Export/Import  Parameter Description
C     --------- ------ -------  -------------- -------------------------------
C     ALPHA     SET    REAL     LOCAL          COEFFICIENT IN THE CORRECTION
C                                              FACTOR (DIMENSIONLESS)
C     ARG       SET    REAL     LOCAL          ARGUMENT TO THE COSH FUNCTION
C     DZ        USED   REAL     IMPORT         Z-DIRECTION DISPERSION NOT
C                                              DIVIDED BY RETARDATION FACTOR
C                                              (I.E., CONTAMINANT INDEPENDENT)
C                                              (CM^2/YR)
C     H         USED   REAL     IMPORT         AQUIFER THICKNESS (CM)
C     PHI       SET    REAL     LOCAL          THE CONSTANT OF PROPORTIONALITY
C                                              IN THE MIXING LENGTH EQUATIONS
C                                              (DIMENSIONLESS)
C     VP        USED   REAL     IMPORT         PORE-WATER VELOCITY (CM/YR)
C     X         USED   REAL     IMPORT         TRAVEL DISTANCE THROUGH THE
C                                              CURRENT MEDIUM (CM)
C     XHM       SET    REAL     LOCAL          LONGITUDINAL TRAVEL DISTANCE
C                                              WHERE THE PLUME WILL BE HALF
C                                              MIXED VERTICALLY (I.E.,
C                                              DISTANCE WHERE ZMIX = H1/2)
C                                              (CM)
C     ZMIX      SET    REAL     LOCAL          VERTICAL MIXING LENGTH (CM)
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE ZMIXPNT(H,VP,DZ,X,ZMIX)
C
C==== Common Block Definitions ===============================================
C
C     None
C
C==== DIMENSION Statements ===================================================
C
C     None
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
C     If the distance at which the plume is half-mixed vertically (XHM) equals
C     zero, then the aquifer han no thickness, so ZMIX = 0.0.
C*****************************************************************************
      XHM=((H**2)*VP+H*(((H*VP)**2)+203153.7*DZ*VP)**0.5)/39.056/DZ
      IF (XHM.EQ.0.0) THEN
        ZMIX = 0.0
      ELSE
        PHI = 19.528*X/(2600.8+X)
        IF (X.GT.XHM) THEN
          ALPHA = 0.5132*(XHM**(-1.0221))
C*****************************************************************************
C     The COSH function overflows when the argument is greater than 89.41.
C     Thus, compute the argument, and if it is larger than 89.41, set it equal
C     to 89.41.
C*****************************************************************************
          ARG = (X-XHM)*ALPHA
          IF (ARG.GT.89.41) ARG = 89.41
          PHI = PHI*(0.56/COSH(ARG)+0.44)
        ENDIF
        ZMIX = (0.25*PHI*DZ*X/VP)**0.5
      ENDIF
      IF (ZMIX.GT.H) ZMIX = H
      RETURN
      END
