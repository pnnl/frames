C     MEPAS RADCON: WRITDATA.FOR             Version Date: 08-24-2006
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                     Subroutine WRITE_HEADER                                *
C                                                                            *
C  Subroutine WRITE_HEADER writes the header to the *.WFF or *.WCF file.     *
C                                                                            *
C  Written by:       John P. McDonald                                        *
C                    Battelle Northwest Laboratories                         *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    11/08/96 - JPM                                          *
C  Last Modified:    08/24/06 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: SUBROUTINE OUTPUT
C     Calls to: NONE
C     Common blocks referenced: NONE
C
C==== Significant Parameter Designation and Descriptions =====================
C
C     Parameter Set/               Location
C     Name      Used       Type    Exp/Imp    Parameter Description
C     --------- --------- ------  ----------- ----------------------------
C
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     04/29/97     JPM  Added an appropriate header for MEPAS 3.2, supressing
C                       output of the FRAMES header (PH1-E015).
C     06/09/98     JPM  Changed variable NUMSCE to NUMWU and restored the
C                       FRAMES header (PH1-E017).
C     08/24/98     JPM  Changed version date for a bug fix in FLXTABLE.
C                       Note - failed to change the version date for the
C                       07/23/1998 release (PH1-P017).
C     06/18/99     JPM  Changed version date (PH1-P019).
C     08/08/00     JPM  Changed version date (PH1-P020).
C     02/28/01     JPM  Changed version date (PH1-P021).
C     05/22/01     JPM  Changed version date (PH1-P022).
C     08/13/01     JPM  Changed for format description for NUMWU from I2
C                       to I10, to accommodate a large number for NUMWU.
C                       Also changed version date (PH1-P023).
C     08/23/01     JPM  Changed version date for a bug fix in subroutine
C                       DECAY (PH1-P024).
C     11/12/02     JPM  Changed version date for an enhancement in subroutine
C                       OUTPUT (PH1-E018).
C     09/29/04     JPM  Changed version date for changed to subroutine LIMITS
C                       (PH1-P025) and changes to properly handle an
C                       infiltration rate of zero in the vadose zone
C                       (PH1-P026).
C     08/24/06     JPM  Changed version date for an enhancement to read in
C                       river receptor coordinates and write them to the WCF
C                       file (PH1-E021).
C
C==== FUNCTION CALL ==========================================================
C
      SUBROUTINE WRITE_HEADER(MEDTYPE,MEDNAME,INDICATOR,NUMWU)
C
C==== COMMON Block Definitions ===============================================
C
C     None
C
C==== DIMENSION Statements ===================================================
C
C     None
C
C==== Variable Declarations ==================================================
C
      CHARACTER*8 MEDNAME
      CHARACTER*13 MEDTYPE
      INTEGER IYR,IMON,IDAY,IHR,IMIN,ISEC,I100TH,NUMWU
      LOGICAL INDICATOR
C
C==== DATA Statements ========================================================
C
C     None
C
      CALL GETDAT(IYR,IMON,IDAY)
      CALL GETTIM(IHR,IMIN,ISEC,I100TH)
      WRITE (14,100) 17
      WRITE (14,105)
      IF (MEDTYPE.EQ.'Vadose') THEN
        WRITE (14,110) 
      ELSE IF (MEDTYPE.EQ.'Aquifer') THEN
        WRITE (14,115) 
      ELSE
        WRITE (14,120)
      ENDIF
C      WRITE (14,135)
      WRITE (14,125)
      WRITE (14,105)
      WRITE (14,130) MEDNAME,IMON,IDAY,IYR,IHR,IMIN,ISEC
      WRITE (14,105)
      WRITE (14,100) NUMWU
      INDICATOR = .FALSE.
100   FORMAT (I10,',')
105   FORMAT (
     +'"===========================================================",')
110   FORMAT (
     +'"  MEPAS_VZ (Version 08/24/2006)                            ",'/,
     +'"                                                           ",'/,
     +'"  VADOSE ZONE COMPONENT of the Multimedia Environmental    ",'/,
     +'"  Pollutant Assessment System (MEPAS):  Models the         ",'/,
     +'"  movement of chemicals and radionuclides through vadose   ",'/,
     +'"  zone media.                                              ",')
115   FORMAT (
     +'"  MEPAS_AQ (Version 08/24/2006)                            ",'/,
     +'"                                                           ",'/,
     +'"  AQUIFER COMPONENT of the Multimedia Environmental        ",'/,
     +'"  Pollutant Assessment System (MEPAS):  Models the         ",'/,
     +'"  movement of chemicals and radionuclides through          ",'/,
     +'"  saturated zone media.                                    ",')
120   FORMAT (
     +'"  MEPAS_RV (Version 08/24/2006)                            ",'/,
     +'"                                                           ",'/,
     +'"  RIVER COMPONENT of the Multimedia Environmental          ",'/,
     +'"  Pollutant Assessment System (MEPAS):  Models the         ",'/,
     +'"  movement of chemicals and radionuclides through          ",'/,
     +'"  surface-water media.                                     ",')
125   FORMAT (
     +'"                                                           ",'/,
     +'"  Pacific Northwest National Laboratory                    ",'/,
     +'"  Operated for the U.S Department of Energy                ",'/,
     +'"  by Battelle Memorial Institute                           ",'/,
     +'"  P.O. Box 999                                             ",'/,
     +'"  Richland, Washington  99352                              ",')
130   FORMAT (
     +'"  Run Name: ","',A,'",',/
     +'"  Run Performed: ","',2(I2,'/'),I4,'","',2(I2,':'),I2,'",')
135   FORMAT (
     +'"  RADCON VERSION 08/24/2006                                ",'/,
     +'"                                                           ",'/,
     +'"  WATERBORNE COMPONENT of the Multimedia Environmental     ",'/,
     +'"  Pollutant Assessment System (MEPAS):  Models the         ",'/,
     +'"  movement of chemicals and radionuclides through vadose   ",'/,
     +'"  zone, saturated zone, surface water, and wetlands media. ",')
      RETURN
      END
