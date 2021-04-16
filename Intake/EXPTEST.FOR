C   EXPTEST.FOR INTAKE              Version Date: 10-Jun-97               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE EXPTEST                               *
C                                                                            *
C  Subroutine EXPTEST test the exposure pathway and route to determine which *
C                     pathway is being considered                            *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    10-Jan-97    DL Strenge                                 *
C  Last Modified:    10-Jun-97    DLS                                        *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EDUP/INTAKE
C     Called by: EPFRIF
C     Calls: NONE  
C     Common blocks referenced: EXPNAM
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     EPATH      U    Char*20  Name of exposure pathway                 
C     EROUTE     U    Char*12  Name of intake route             
C     NPATH      S      Int    Pathway index                  
C==== Modification History ===================================================
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C   09-Jun-97      DLS  Initial programming from CHRONIC EPFDSET
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE EXPTEST(EPATH,EROUTE,EUNIT,EXPUNIT,NPATH,ETYPOUT)
C
C---- Include statements -----------------------------------------------------
c
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'EXPNAM.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      INTEGER NPATH 
      CHARACTER*12 EROUTE
      CHARACTER*20 EPATH
      CHARACTER*7 EXPUNIT
      CHARACTER*6 EUNIT
      CHARACTER*1 S
      CHARACTER*14 ETYPE(4),ETYPOUT
      LOGICAL SEQI
C
C---- Data Statements --------------------------------------------------------
C
      DATA ETYPE/'intake        ',
     .           'concentration ',
     .           'radiation dose',
     .           'no calculation'/
      DATA S/'S'/
      NPATH = 0
C
C----- Loop on exposure pathways --------------------------------------------
C
      EXPUNIT = ' '
      DO IP = 1,28
        IF(SEQI(EROUTE,EXPRUT(IP),12)) THEN
          IF(SEQI(EPATH,EXPLAB(IP),20)) THEN
            NPATH = IP
            EXPUNIT = EXPUN(IP)
            GO TO 100
          END IF
        END IF
      END DO
C
C----- Pathway not recognized, write error message --------------------------
C
      WRITE(NRLS,200) EPATH,EROUTE
 200  FORMAT(' Exposure pathway not recognized in EPF file'/
     .       '   Input pathway names are ',A,A)
c     ETYPOUT = ETYPE(4)
      EXPUNIT = 'none'
      RETURN
 100  ETYPOUT = ETYPE(1)
      IF(NPATH.EQ.1.OR.NPATH.EQ.3.OR.NPATH.EQ.14.OR.NPATH.EQ.15.
     .   OR.NPATH.EQ.16) ETYPOUT = ETYPE(2)
      IF(SEQI(EUNIT,S,1)) THEN
        ETYPOUT = ETYPE(3)
        EXPUNIT = 'Sv     '
      ENDIF
 999  RETURN
C
C----- END OF MODULE EPFNUM -------------------------------------------------
C
      END

