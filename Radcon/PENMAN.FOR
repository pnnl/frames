C     MEPAS RADCON: PENMAN.FOR            Version Date: 11-26-1991            
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                       SUBROUTINE PENMAN                                    *
C                                                                            *
C  Subroutine PENMAN computes the evapotranspiration (ET3) using the         *
C  Penman technique based on equation 3.7 from Whelan et al. 1987.           *
C  The Penman method is one of three methods used to compute                 *
C  evapotranspiration.  The most conservative value is chosen for            *
C  further computations.  The subroutine is called from subroutine           *
C  FAOPET.                                                                   *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    11/26/91 - JWB                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of:  RADCON
C     Called by:  SUBROUTINE FAOPET
C     Calls to: None
C     Common blocks referenced: None
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- -----  -----  --------------- --------------------------------
C     EA        SET    REAL    ARG - IMPORT   SATURATION VAPOR PRESSURE       
C     ED        SET    REAL    ARG - IMPORT   ACTUAL MEAN VAPOR PRESSURE      
C     ET3       SET    REAL    ARG - EXPORT   EVAPOTRANSPIRATION              
C     U2        SET    REAL    LOCAL          WIND SPEED                      
C     W         USED   REAL    ARG - IMPORT   TEMPERATURE WEIGHTING FACTOR    
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/26/91     JWB  Added required comments and header for MEPAS QA plan  
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE PENMAN (W,TMEAN,EA,ED,U24,RS,NRATIO,ET3,RN,ETFAC)
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
      REAL NRATIO
C
C==== DATA Statements ========================================================
C
C     None
C
      U2=U24
      FU=0.27*(1.0+U2/100.0)
      ETVAP=(1.0-W)*FU*(EA-ED)
      ET3=W*RN+ETVAP
      ETFAC=ETVAP/ET3
      RETURN
      END
