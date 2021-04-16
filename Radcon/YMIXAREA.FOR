C     MEPAS RADCON: YMIXAREA.FOR             Version Date: 05-06-1996
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE YMIXAREA                              *
C                                                                            *
C  Subroutine YMIXAREA calculates the lateral mixing length from an area     *
C  source using an empirically determined technique based on the lateral     *
C  mixing length from a point source (RAD-E046).                             *
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
C     A,B       SET    REAL     LOCAL          COEFFICIENTS USED TO COMPUTE
C                                              FRAC
C     DY        USED   REAL     IMPORT         Y-DIRECTION DISPERSION NOT
C                                              DIVIDED BY RETARDATION FACTOR
C                                              (I.E., CONTAMINANT INDEPENDENT)
C                                              (CM^2/YR)
C     FRAC      SET    REAL     LOCAL          FRACTION OF THE POINT SOURCE
C                                              MIXING LENGTH TO ADD TO THE
C                                              WIDTH OF THE SOURCE
C                                              (DIMENSIONLESS)
C     GAMMA     SET    REAL     LOCAL          SPATIAL INDEX (APPROACHES 0 IN
C                                              THE SPATIAL NEAR FIELD AND 1 IN
C                                              THE SPATIAL FAR FIELD
C                                              (DIMENSIONLESS)
C     WIDTH     USED   REAL     IMPORT         WIDTH OF THE SOURCE (CM)
C     X         USED   REAL     IMPORT         TRAVEL DISTANCE THROUGH THE
C                                              CURRENT MEDIUM (CM)
C     YMIX      SET    REAL     EXPORT         LATERAL MIXING LENGTH (PASSED
C                                              IN AS THE MIXING LENGTH FROM A
C                                              POINT SOURCE, AND RETURNED AS
C                                              THE MIXING LENGTH FROM AN AREA
C                                              SOURCE) (CM)
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE YMIXAREA(WIDTH,DY,X,YMIX)
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
      GAMMA = X/(WIDTH+X)
      A = (110.4474*WIDTH+32457.1)*(DY**(-0.9))
      B = 2.0507*(A**3.5895e-02)
      FRAC = 1-((1-(GAMMA**A))**(1/B))
      YMIX = FRAC*YMIX+WIDTH
      RETURN
      END
