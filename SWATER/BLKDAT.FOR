C   SWATER:     BLKDAT.FOR             Version Date: 30-Oct-1996             
C   Copyright 1989, 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           BLOCK DATA BLKDAT                                *
C                                                                            *
C  BLOCK DATA BLKDAT This data block contains default parameter              *
C                    values for SWATER.                                         *
C                                                                            *
C  Written by:       Dennis Strenge                                          *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    02/08/86 (Converted to PC)                              *
C  Last Modified:    30-Oct-1996  DLS                                        *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: GENII/SWATER
C     Called by: NONE
C     Calls: NONE
C     Common blocks referenced: DEVICE
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter      Set/
C     Name           Used  Type  Location  Parameter Description
C     -------------- ----  ----  --------  ------------------------------
C     NERR            S    INT   DEVICE    Error file I/O unit
C     NGID            S    INT   DEVICE    GID file Input unit
C     NIDF            S    INT   DEVICE    FACIL.ID file input unit
C     NWFF            S    INT   DEVICE    Water Flux File input unit
C     NWCF            S    INT   DEVICE    Wacter Concentration File output unit
C     NRMD            S    INT   DEVICE    Radionuclide decay data input unit
C==== Modification History ===================================================
C
C     Date          Who  Modification Description
C     --------      ---  ------------------------------------------------------
C     30-Oct-96     DLS  Rewritten for GENII/SWATER 
C==== SUBROUTINE CALL ========================================================
C
      BLOCK DATA BLKDAT
C
C==== COMMON Block Definitions ===============================================
C
	include 'DEVICE.CMN'
	INCLUDE 'OPT.CMN'
C
C==== DIMENSION Statements ===================================================
C
C     NONE
C
C==== Variable Declarations ==================================================
C
C     NONE
C
C==== DATA Statements ========================================================
C
C----- Logical I/O unit definitions for COMMON Block DEVICE --------------
C
      DATA NERR,NGID,NWFF,NWCF,NSLS,NIDF,NRMD/6,7,8,9,10,11,2/
C
C----- Constants for COMMON Block OPT ------------------------------------
C
      DATA P/3.14159265/, YRSEC/3.1688E-8/, M3L/0.001/, YRHR/1.1408E-4/
      DATA BQPCI/0.037/, PCIBQ/27.027/, M3ML/1.E-6/, L3ML/1.E-3/
      DATA DAYHR/0.0416667/
      DATA DAYYR/365.25/
C
C============ END OF MODULE BLKDAT =========================================
      END
