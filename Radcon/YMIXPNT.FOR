C     MEPAS RADCON: YMIXPNT.FOR             Version Date: 05-06-1996
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                            SUBROUTINE YMIXPNT                              *
C                                                                            *
C  Subroutine YMIXPNT calculates the lateral mixing length from a point      *
C  source using Equation 5.61 in Whelan et al (1987), with the exception     *
C  that phi is an empirically determined function of longitudinal travel     *
C  distance (RAD-E046).                                                      *
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
C     DY        USED   REAL     IMPORT         Y-DIRECTION DISPERSION NOT
C                                              DIVIDED BY RETARDATION FACTOR
C                                              (I.E., CONTAMINANT INDEPENDENT)
C                                              (CM^2/YR)
C     PHI       SET    REAL     LOCAL          THE CONSTANT OF PROPORTIONALITY
C                                              IN THE MIXING LENGTH EQUATIONS
C                                              (DIMENSIONLESS)
C     VP        USED   REAL     IMPORT         PORE-WATER VELOCITY (CM/YR)
C     X         USED   REAL     IMPORT         TRAVEL DISTANCE THROUGH THE
C                                              CURRENT MEDIUM (CM)
C     YMIX      SET    REAL     EXPORT         LATERAL MIXING LENGTH
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE YMIXPNT(VP,DY,X,YMIX)
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
      PHI = (19.543*X)/(1457.3+X)
      YMIX = SQRT(PHI*DY*X/VP)
      RETURN
      END
