C   NESHAPS:     BLKDAT.FOR             Version Date: 26-DEC-2001            
C   Copyright 1989, 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           BLOCK DATA BLKDAT                                *
C                                                                            *
C  BLOCK DATA BLKDAT This data block contains default parameter              *
C                    values for doserisk                                     *
C                                                                            *
C  Written by:       Dennis Strenge/BA Napier                                *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Last Modified:    26-DEC-2001  BAN                                        *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: GENII/DOSERISK
C     Called by: NONE
C     Calls: NONE
C     Common blocks referenced: 
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter      Set/
C     Name           Used  Type  Location  Parameter Description
C     -------------- ----  ----  --------  ------------------------------
C     NERR            S    INT   DEVICE    Error file I/O unit
C     NGID            S    INT   DEVICE    GID file Input unit
C     NATO            S    INT   DEVICE    EXISTING AIR TRANSPORT FILE
C     NHIF            S    INT   DEVICE    HEALTH EFFECTS output file unit
C     NEPA            S    INT   DEVICE    NESHAPS output file 
C     NPOP            S    INT   DEVICE    INPUT POPULATION FILE
C==== Modification History ===================================================
C
C     Date          Who  Modification Description
C     --------      ---  ------------------------------------------------------
C     26-DEC-2001   BAN  UPDATED FOR NESHAPS USE
c
C==== SUBROUTINE CALL ========================================================
C
      BLOCK DATA BLKDAT
C
C==== COMMON Block Definitions ===============================================
C
      include 'NESHAPS.CMN'
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
      DATA NERR,NGID,NBTF,NEPA,NATO,NSHP,nref/52,53,58,59,60,61,62/

C
C============ END OF MODULE BLKDAT =========================================
C
      END
