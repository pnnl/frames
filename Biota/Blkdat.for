C   EXPOS:      BLKDAT.FOR             Version Date: 6-May-1998             
C   Copyright 1989, 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           BLOCK DATA BLKDAT                                *
C                                                                            *
C  BLOCK DATA BLKDAT This data block contains default parameter              *
C                    values for EXPOS.                                          *
C                                                                            *
C  Written by:       Dennis Strenge                                          *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    19-Dec-1996 (Converted for EXPOS)                       *
C  Last Modified:    6-Feb-1998  BAN                                        *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: GENII/EXPOS
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
C   Date      Who  Modification Description
C  ---------  ---  ------------------------------------------------------
C  19-Dec-96  DLS  Rewritten for GENII/EXPOS
C  22-May-97  DLS  Added common block EXPNAM
C  03-Feb-98  DLS  Changed normalization units for external ground and sediment
C                  from m2 and kg.
C  6-May-98   BAN  Pieces moved to BLOCKD.FOR
C==== SUBROUTINE CALL ========================================================
C
      BLOCK DATA BLKDAT
C
C==== COMMON Block Definitions ===============================================
C
      include 'DEVICE.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'EXPNAM.CMN'
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
      DATA NERR,NGID,NWFF,NWCF,NELS,NIDF,NRMD/46,47,48,49,50,51,2/
      DATA NBTF,NATO,NSCF,NATP/53,54,55,56/
      DATA NERROR/0/
C
C
C----- Character parameters for COMMON block EXPNAM ----------------------
C
      DATA EXPLAB/'Air                 ',
     .            'Air                 ',
     .            'Ground              ',
     .            'Terrestrial plant   ',
     .            'abcd                ',
     .            'efgh                ',
     .            'ijkl                ',
     .            'Terrestrial mammal  ',
     .            'Riparian mammal     ',
     .            'Terrestrial bird    ', 
     .            'Riparian Bird       ',
     .            'Eggs                ',
     .            'Swimming            ',
     .            'Swimming            ',
     .            'Boating             ',
     .            'Shoreline           ',
     .            'Water               ',
     .            'Fish                ',
     .            'Mollusks            ',
     .            'Crustacea           ',
     .            'Aquatic plants      ',
     .            'Indoor air          ',
     .            'Showering           ',
     .            'Showering           ',
     .            'Soil                ',
     .            'Swimming            ',
     .            'Shoreline           ',
     .            'Soil                '/
      DATA EXPRUT/'external    ',
     .            'inhalation  ',
     .            'external    ',
     .            'dose rate   ',
     .            'ingestion   ',
     .            'ingestion   ',
     .            'ingestion   ',
     .            'dose rate   ',
     .            'dose rate   ',
     .            'dose rate   ',
     .            'dose rate   ',
     .            'dose rate   ',
     .            'ingestion   ',
     .            'external    ',
     .            'external    ',
     .            'external    ',
     .            'ingestion   ',
     .            'dose rate   ',
     .            'dose rate   ',
     .            'dose rate   ',
     .            'dose rate   ',
     .            'dose rate   ',
     .            'dermal      ',
     .            'ingestion   ',
     .            'dermal      ',
     .            'dermal      ',
     .            'dermal      ',
     .            'inhalation  '/
      DATA EXPUN /'Bq/m^3',        ! air external, semi-infinite
     .            'Bq/m^3',        ! air inhalation
     .            'Bq/kg ',        ! ground external
     .            'rad/day',        ! food crops
     .            'Bq/kg ',
     .            'Bq/kg ',
     .            'Bq/kg ',
     .            'rad/day',        ! animal products
     .            'rad/day',        
     .            'rad/day',
     .            'rad/day',
     .            'rad/day',        ! 
     .            'Bq/L  ',        ! swimming
     .            'Bq/L  ',        ! swimming
     .            'Bq/L  ',        ! boating
     .            'Bq/kg ',        ! shoreline
     .            'Bq/L  ',        ! water ingestion
     .            'rad/day',        ! aquatic foods
     .            'rad/day',        ! aquatic foods
     .            'rad/day',        ! aquatic foods
     .            'rad/day',        ! aquatic foods
     .            'Bq/m^3',        ! indoor air inhalation
     .            'Bq/L  ',        ! shower dermal
     .            'Bq/L  ',        ! shower ingestion
     .            'Bq/kg ',        ! soil dermal
     .            'Bq/L  ',        ! swimming dermal
     .            'Bq/kg ',        ! shoreline dermal
     .            'Bq/m^3'/        ! soil inhalation
      DATA NEXPLAB/3,3,6,17,4,4,4,18,15,16,13,4,8,8,7,9,5,4,8,9,14,10,
     .             9,9,4,8,9,4/
      DATA NEXPRUT/8,10,8,10*9,3*8,5*9,9,6,9,3*6,10/
      DATA NEXPUN/3*6,7,3*5,5*7,3*4,5,4,4*7,6,4,4,5,4,5,6/
C
C============ END OF MODULE BLKDAT =========================================
C
      END
