C     MEPAS RADCON: EXPOSR.FOR            Version Date: 04-18-1997
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE EXPOSR                                *
C                                                                            *
C  This subroutine reads the facility name, waste unit identification,       *
C  receptor location number index, and receptor location exposure medium.    *
c  It is called by the main program (RADCONPC).                              *
C                                                                            *
C  Written by:      Gene Whelan                                              *
C                   Battelle Pacific Northwest Laboratories                  *
C                   P.O. Box 999                                             *
C                   Richland, WA 99352                                       *
C                                                                            *
C  Creation Date:   01/19/89 (Converted to PC)                               *
C  Last Modified:   04/18/97 -  JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C 
C     Module of: RADCON
C     Called by: RADCONPC
C     Calls to: None
C     Common blocks referenced: INIT, INDEX5
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- -----  -----   -------------  --------------------------------
C     LOCMED    SET    INTEGER  EXPORT         INDEX FOR USAGE LOCATION MEDIA 
C     LOCNUM    SET    INTEGER  EXPORT         # OF USAGE LOCATIONS PER MEDIA 
C     NSC       USED   INTEGER  ARG - IMPORT   # OF SCENARIOS, SAME AS J      
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     07/01/91     JGD  Read statement for LOCNAM modified                    
C     11/25/91     JWB  Added required comments and header for MEPAS QA Plan  
C     11/08/96     JPM  Added LOCNAM to the argument list and suppressing
C                       output to the *.WAT file (PH1-E005).
C     04/18/97     JPM  Format statement for group 7 modified.  Was cutting
C                       off the first character of LOCNAM (PH1-P007).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE EXPOSR (NSC,LOCNAM)
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'INDEX5.WTN'
      INCLUDE 'INIT.WTN'
C
C==== DIMENSION Statements ===================================================
C
C     None
C
C==== Variable Declarations ==================================================
C
      CHARACTER*21 LOCNAM
C
C==== DATA Statments =========================================================
C
C     None
C
C*****************************************************************************
C     READ INPUT INFORMATION (CONSTITUENT NAMES, LOCATION # AND MEDIUM)       
C*****************************************************************************
      READ(5,2) (IV(I,1), I=1,4)
      READ(5,2) (IV(I,2), I=1,4)
      READ(5,2) (IV(I,3), I=1,4)
      READ(5,2) (IV(I,4), I=1,4)
    2 FORMAT (8A18)
      READ(5,1) LOCNUM,LOCMED,locnam
    1 FORMAT (2I5,5x,a21)
C*****************************************************************************
C   1 FORMAT (2I5,7x,a20) REPLACED  06/2791 - JGD
C*****************************************************************************
C     WRITE FACILITY AND WASTE SITE IDENTIFICATIONS TO APPROPRIATE FILES      
C     LOGICAL UNIT 7 TRANSFERS INFORMATION TO EXPOSURE ASSESSMENT COMPONENT   
C*****************************************************************************
      WRITE(6,3) (IV(I,1), I=1,4)
      WRITE(6,4) (IV(I,2), I=1,4)
      WRITE(6,5) (IV(I,3), I=1,4)
      WRITE(6,6) (IV(I,4), I=1,4)
    3 FORMAT (' Name of Facility      = ',4A8)
    4 FORMAT (' Name of Waste Unit    = ',4A8)
    5 FORMAT (' Transport Scenario    = ',4A8)
    6 FORMAT (' Exposure Scenario     = ',4A8)
      WRITE(6,7) LOCNUM,LOCMED,locnam
    7 FORMAT (' Usage Location Number = ',I2/
     .        ' Usage Location Medium = ',I2/,
     .        ' Usage Location Name   = ',a21/)
    8 FORMAT (2X,8A18)
      RETURN
      END
