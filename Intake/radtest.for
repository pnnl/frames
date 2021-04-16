C   RADTEST.FOR INTAKE              Version Date: 10-Jun-97               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE RADTEST                               *
C                                                                            *
C  Subroutine RADTEST test a radionuclide name against the master list from  *
C                     the GID file.                                          *
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
C     Common blocks referenced: RADATA
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C  Name      Used   Type    Location  Parameter Description
C  --------- -----  ------  --------- -------------------------------------
C  RNAME      U    Char*12  Name of paraent radionuclide             
C  PNAME      U    Char*12  Name of progeny radionuclide, if any
C  IPAR       U      Int    Seach type flag: 0 for parent, >0 for progeny
C  NRAD      S/U     Int    Position of parent in GID master list
C  NPROG     S/U     Int    Position of progeny in GID master list
C==== Modification History ===================================================
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C   10-Jun-97      DLS  Initial programming 
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE RADTEST(RNAME,PNAME,IPAR,NRAD,NPROG)
C
C---- Include statements -----------------------------------------------------
c
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'RADATA.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      INTEGER NRAD, IPAR, NPROG
      CHARACTER*12 RNAME,PNAME
C
C---- Data Statements --------------------------------------------------------
C
C     NONE
C-----------------------------------------------------------------------------
C
      NRAD = 0
      NPROG = 0
C
C----- Loop on parents in GID ------------------------------------------------
C
        DO IP = 1,NUMCON
C
C---- First find parent in GID list (RNAME) ----------------------------------
C
          IF(CONID(IP).EQ.RNAME) THEN   ! Parent found
            NRAD = IP
            IF(IPAR.GT.0) THEN  ! Need index for progeny
              DO IPR = 1,NDS(IP)
                IF(CONPID(IP,IPR).EQ.PNAME) THEN  ! Progeny found
                  NPROG = IPR
                ENDIF
              END DO   ! Loop on progeny IPR for current parent
              IF(NPROG.LE.0) THEN
                WRITE(NERR,100) PNAME
 100            FORMAT(' Error in EPF progeny name: not in GID. ',A)
                NERROR = NERROR + 1
                GO TO 999
              ENDIF
            ENDIF  ! If on parent/progeny search, IPAR > 0
          ENDIF    ! If on parent name found
        END DO     ! Loop on parent radionuclides in master list
C
C----- Check if parent was found in master list. -----------------------------
C
              IF(NRAD.LE.0) THEN
                WRITE(NERR,101) RNAME
 101            FORMAT(' Error in EPF parent name: not in GID. ',A)
                NERROR = NERROR + 1
                GO TO 999
              ENDIF
C
 999  RETURN
C
C----- END OF MODULE RADTEST ------------------------------------------------
C
      END

