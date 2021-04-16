C     MEPAS RADCON: PLUME.FOR             Version Date: 11-07-1996
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                            SUBROUTINE PLUME                                *
C                                                                            *
C  Subroutine PLUME calculates the source dimensions for the next medium     *
C  to be modeled, using the mixing length equations.                         *
C                                                                            *
C                                                                            *
C  Written by:       John P. McDonald                                        *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    05/07/96 - JPM                                          *
C  Last Modified:    11/07/96 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: MAIN PROGRAM
C     Calls to: YMIXPNT,YMIXAREA,ZMIXPNT
C     Common blocks referenced: NONE
C
C==== Significant Parameter Designation and Descriptions =====================
C
C     Parameter  Set/           Location
C     Name       Used   Type    Export/Import  Parameter Description
C     --------- ------ -------  -------------- -------------------------------
C     ALEN      SET    REAL     EXPORT         GW PLUME WIDTH AT INTERFACE
C                                              BETWEEN MEDIA (CM)
C     DY        USED   REAL     IMPORT         Y-DIRECTION DISPERSION
C                                              (CONTAMINANT INDEPENDENT, I.E.,
C                                              NOT DIVIDED BY RETARDATION
C                                              FACTOR) (CM^2/YR)
C     DZ        USED   REAL     IMPORT         Z-DIRECTION DISPERSION (ALSO
C                                              CONTAMINANT INDEPENDENT)
C                                              (CM^2/YR)
C     H         USED   REAL     EXPORT         EQUIVALENT AQUIFER THICKNESS (CM)
C     H1        USED   REAL     IMPORT         ACTUAL AQUIFER THICKNESS (CM)
C     ISCONF    USED   INTEGER  IMPORT         SOURCE CONFIGURATION INDICATOR
C                                              1=POINT SOURCE AT WATER TABLE
C                                              2=XLINE SOURCE AT WATER TABLE
C                                              3=YLINE SOURCE AT WATER TABLE
C                                              4=X-Y AREA SOURCE AT WATER TABLE
C                                              5=Y-Z AREA SOURCE AT ANY DEPTH
C     IZBOT     SET    INTEGER  LOCAL          Z-COORDINATE OF THE LOWERMOST
C                                              POINT SOURCE OF A DISCRETIZED
C                                              Y-Z AREA SOURCE (CM)
C     IZTOP     SET    INTEGER  LOCAL          Z-COORDINATE OF THE I-TH POINT
C                                              SOURCE OF A DISCRETIZED Y-Z
C                                              AREA SOURCE (CM)
C     KMED      USED   INTEGER  IMPORT         MEDIUM NUMBER
C     MED       USED   INTEGER  IMPORT         MEDIUM TYPE:
C                                              1=PSZ
C                                              3=SZ AT A WELL
C                                              4=SZ AT A RIVER OR ANOTHER SZ
C                                              5=RIVER
C     VP        USED   REAL     IMPORT         PORE WATER VELOCITY (CM/YR)
C     WIDTH     USED   REAL     IMPORT         SOURCE WIDTH FOR THE CURRENT
C                                              MEDIUM (CM)
C     X         USED   REAL     IMPORT         TRAVEL DISTANCE THROUGH THE
C                                              CURRENT MEDIUM (CM)
C     ZBOT      SET    REAL     EXPORT         Z-COORDINATE OF PLUME'S LOWER
C                                              SURFACE (CM)
C     ZBOTISUM  SET    REAL     LOCAL          SUM OF THE POINT SOURCE MIXING
C                                              LENGTHS (CM)
C     ZBOTNEW   SET    REAL     LOCAL          TEMPORARY HOLDING VARIABLE FOR
C                                              THE LOWER Z-COORDINATE OF THE
C                                              SOURCE FOR THE NEXT MEDIUM (CM)
C     ZMIX      SET    REAL     LOCAL          VERTICAL MIXING LENGTH, USED
C                                              TO SET ZTOP AND ZBOT (CM)
C     ZTOP      SET    REAL     EXPORT         Z-COORDINATE OF PLUME'S UPPER
C                                              SURFACE (CM)
C     ZTOPNEW   SET    REAL     LOCAL          TEMPORARY HOLDING VARIABLE FOR
C                                              THE UPPER Z-COORDINATE OF THE
C                                              SOURCE FOR THE NEXT MEDIUM (CM)
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/07/96     JPM  Computing new vertical dimensions/coordinates whenever
C                       this subroutine is called - needed for Framework
C                       output file specifications (PH1-E005).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE PLUME (ALEN,ZTOP,ZBOT,DY,DZ,X,H1,VP,WIDTH,ISCONF)
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
C     Compute the source width for the next medium to be modeled (ALEN).
C     First, compute the lateral mixing length from a point source.
C*****************************************************************************
      CALL YMIXPNT(VP,DY,X,ALEN)
C*****************************************************************************
C     For y-line, x-y area, and y-z area sources, use the mixing length for
C     a point source to compute the mixing length for an area source.
C*****************************************************************************
      IF (ISCONF.GT.2) THEN
        CALL YMIXAREA(WIDTH,DY,X,ALEN)
      ENDIF
C*****************************************************************************
C     Compute the coordinates of the upper and lower surfaces of the plume
C     (ZTOP and ZBOT).  For point, line and x-y area sources (i.e., ISCONF <=
C     4), the z-dimension of the source is zero, so the vertical mixing length
C     equations for a point source are used.  In this case, ZTOP = 0.0.
C*****************************************************************************
      IF (ISCONF.LE.4) THEN
        CALL ZMIXPNT(H1,VP,DZ,X,ZBOT)
        ZTOP = 0.0
      ELSE
C*****************************************************************************
C     For y-z area sources (i.e., ISCONF = 5) or volume sources (i.e., ISCONF
C     = 6), discretize the z-dimension of the source into a series of point
C     sources at 1 cm intervals.  Calculate the vertical mixing length for
C     each point, then use the average as an estimate of the vertical mixing
C     length from a y-z area source.  First, calculate a new ZBOT.
C*****************************************************************************
        IZTOP = INT(ZTOP)
        IZBOT = INT(ZBOT)
        ZBOTISUM = 0.0
        DO I = IZTOP,IZBOT
C*****************************************************************************
C     Calculate the vertical mixing length assuming that the i-th point is at
C     the water table.  Thus, define a dummy aquifer thickness as the actual
C     aquifer thickness minus the z-coordinate of the i-th point.
C*****************************************************************************
          H = H1 - REAL(I)
          CALL ZMIXPNT(H,VP,DZ,X,ZMIX)
C*****************************************************************************
C     Add the mixing length for the i-th point to the z-coordinate of the i-th
C     point, to give a new zbot for the i-th point (ZBOTI).  If this new
C     coordinate is less than ZBOT, set it equal to ZBOT.
C*****************************************************************************
          ZBOTI = ZMIX + REAL(I)
          IF (ZBOTI.LT.ZBOT) ZBOTI = ZBOT
          ZBOTISUM = ZBOTISUM + ZBOTI
        ENDDO
C*****************************************************************************
C     Divide the sum of the new z-coordinates from all the i-th points by the
C     number of points to estimate an new ZBOT for the area source.
C*****************************************************************************
        ZBOTNEW = ZBOTISUM/REAL(IZBOT-IZTOP+1)
        IF (ZBOTNEW.GT.H1) ZBOTNEW = H1
        IF (ZBOTNEW.LT.ZBOT) ZBOTNEW = ZBOT
C*****************************************************************************
C     If the upper coordinate of the source is not at the water table, then
C     a new upper coordinate must be determined.  The method is the same as
C     was used to compute the new ZBOT, except that the coordinate system is
C     reversed (e.g., the redefined aquifer thickness becomes equal to the
C     z-coordinate of the i-th point).
C*****************************************************************************
        IF (IZTOP.NE.0) THEN
          ZTOPISUM = 0.0
          DO I = IZTOP,IZBOT
            H = REAL(I)
            CALL ZMIXPNT(H,VP,DZ,X,ZMIX)
            ZTOPI = REAL(I) - ZMIX
            IF (ZTOPI.GT.ZTOP) ZTOPI = ZTOP
            ZTOPISUM = ZTOPISUM + ZTOPI
          ENDDO
          ZTOPNEW = ZTOPISUM/REAL(IZBOT-IZTOP+1)
          IF (ZTOPNEW.LT.0.0) ZTOPNEW = 0.0
          IF (ZTOPNEW.GT.ZTOP) ZTOPNEW = ZTOP
        ELSE
          ZTOPNEW = 0.0
        ENDIF
        ZTOP = ZTOPNEW
        ZBOT = ZBOTNEW
      ENDIF
      WRITE (6,915) ALEN,ZTOP,ZBOT
915   FORMAT(/,' SOURCE DIMENSIONS FOR THE NEXT MEDIUM:',//,
     1' Width of Plume (ALEN)                      = ',1P,E10.2,' cm',/,
     2' Depth to top of Plume (ZTOP)               = ',E10.2,' cm',/,
     3' Depth to bottom of Plume (ZBOT)            = ',E10.2,' cm',/)
      RETURN
      END
