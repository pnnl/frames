C   RIFRAD.FOR INTAKE               Version Date: 10-Jun-97               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE RIFRAD                                *
C                                                                            *
C  Subroutine RIFRAD writes radionuclide name record for current constituent *
C                    to RIF file                                             *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    10-Jan-97                                               *
C  Last Modified:    10-Jun-97      DLS                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EDUP/INTAKE
C     Called by: INTAKE
C     Calls: NONE  
C     Common blocks referenced: NONE
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     RNAMES     S    Char*    Radionuclide chain member names
C     NPRG       S      Int    Number of progeny for current parent radionuclide
C     NTIMES     S      Int    Number of integrating times for receptor
C==== Modification History ===================================================
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C   10-Jun-97      DLS  Initial programming from CHRONIC EPFDSET
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE RIFRAD(RNAME,NPRG,NTIMES)
C
C---- Include statements -----------------------------------------------------
c
      INCLUDE 'DEVICE.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
C     LOGICAL 
      INTEGER NPRG,NTIMES
      CHARACTER*12 RNAME    
C
C---- Data Statements --------------------------------------------------------
C
C     NONE
C
C---- Read first line of data set: number of locations and chronic ----------
C
      NR = LENWORD(RNAME,12)
      WRITE(NRIF,100) RNAME(1:NR),RNAME(1:NR),NPRG,NTIMES
 100  FORMAT('"',A,'","',A,'",',I2,',',I4,',')
C
      RETURN
C
C----- END OF MODULE RIFRAD -------------------------------------------------
C
      END

