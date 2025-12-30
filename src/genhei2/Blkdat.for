C   doserisk:     BLKDAT.FOR             Version Date: 5-Jun-1998             
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
C  Creation Date:    10-Jun-1997 (Converted for DOSERISK)                    *
C  Last Modified:    5-Jun-1998  BAN                                         *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: GENII/DOSERISK
C     Called by: NONE
C     Calls: NONE
C     Common blocks referenced: DEVICE, EXPNAM
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter      Set/
C     Name           Used  Type  Location  Parameter Description
C     -------------- ----  ----  --------  ------------------------------
C     NERR            S    INT   DEVICE    Error file I/O unit
C     NGID            S    INT   DEVICE    GID file Input unit
C     NIDF            S    INT   DEVICE    FACIL.ID file input unit
C     NRIF            S    INT   DEVICE     concentration input file unit
C     NHIF            S    INT   DEVICE    HEALTH EFFECTS output file unit
C     NHLS            S    INT   DEVICE    DOSERISK run information output file 
C==== Modification History ===================================================
C
C     Date          Who  Modification Description
C     --------      ---  ------------------------------------------------------
C     10-Jun-97     DLS  Rewritten for GENII/INTAKE
C     5-JUN-01      BAN  Added soil external .NE. ground external (MEPAS variant)
C==== SUBROUTINE CALL ========================================================
C
      BLOCK DATA BLKDAT
C
C==== COMMON Block Definitions ===============================================
C
      include 'DEVICE.CMN'
      INCLUDE 'EXPNAM.CMN'
      INCLUDE 'DERMFAC.CMN'
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
      DATA NERR,NGID,NRIF,NHIF,NHLS,NIDF/52,53,54,55,56,57/
      DATA NERROR/0/
C

C----- Character parameters for COMMON block DERMFAC ---------------------
C
C      DATA PKDFLT/0.001/,ABSDFLT/0.001/ 
C
C----- Character parameters for COMMON block EXPNAM ----------------------
C
      DATA EXPLAB/'Air                 ',
     .            'Air                 ',
     .            'Ground              ',
     .            'Leafy vegetables    ',
     .            'Root vegetables     ',
     .            'Fruit               ',
     .            'Grain               ',
     .            'Meat                ',
     .            'Poultry             ',
     .            'Milk                ',
     .            'Eggs                ',
     .            'Soil                ',
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
     .            'Soil                ',
     .            'Soil                '/
      DATA EXPRUT/'external    ',
     .            'inhalation  ',
     .            'external    ',
     .            'ingestion   ',
     .            'ingestion   ',
     .            'ingestion   ',
     .            'ingestion   ',
     .            'ingestion   ',
     .            'ingestion   ',
     .            'ingestion   ',
     .            'ingestion   ',
     .            'ingestion   ',
     .            'ingestion   ',
     .            'external    ',
     .            'external    ',
     .            'external    ',
     .            'ingestion   ',
     .            'ingestion   ',
     .            'ingestion   ',
     .            'ingestion   ',
     .            'ingestion   ',
     .            'inhalation  ',
     .            'dermal      ',
     .            'ingestion   ',
     .            'dermal      ',
     .            'dermal      ',
     .            'dermal      ',
     .            'inhalation  ',
     .            'external    '/
      DATA EXPUN /'Bq/m3  ',        ! air external, semi-infinite
     .            'Bq     ',        ! air inhalation
     .            'Bq/kg  ',        ! ground external
     .            'Bq     ',        ! food crops
     .            'Bq     ',
     .            'Bq     ',
     .            'Bq     ',
     .            'Bq     ',        ! animal products
     .            'Bq     ',        
     .            'Bq     ',
     .            'Bq     ',
     .            'Bq     ',        ! soil ingestion
     .            'Bq     ',        ! swimming
     .            'Bq/L   ',        ! swimming
     .            'Bq/L   ',        ! boating
     .            'Bq/kg  ',        ! shoreline
     .            'Bq     ',        ! water ingestion
     .            'Bq     ',        ! aquatic foods
     .            'Bq     ',        ! aquatic foods
     .            'Bq     ',        ! aquatic foods
     .            'Bq     ',        ! aquatic foods
     .            'Bq     ',        ! indoor air inhalation
     .            'Bq     ',        ! shower dermal
     .            'Bq     ',        ! shower ingestion
     .            'Bq     ',        ! soil dermal
     .            'Bq     ',        ! swimming dermal
     .            'Bq     ',        ! shoreline dermal
     .            'Bq     ',        ! soil inhalation
     .            'Bq/kg  '/        ! soil external - extra trap
C
C============ END OF MODULE BLKDAT =========================================
C
      END
