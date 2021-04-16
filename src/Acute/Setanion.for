C   SETANION.FOR EXPOS              Version Date: 27-Aug-97               
C   Copyright 1997 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE SETANION                              *
C                                                                            *
C  Subroutine SETANION sets anion flags by element for each chain member     *
C       The flags are used to evaluate wet deposition to plant leaves        *
C                                                                            *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    27-Aug-97                                               *
C  Last Modified:    27-Aug-97      DLS                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EDUP/EXPOS
C     Called by: EXPOS
C     Calls: NONE  
C     Common blocks referenced: NONE
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     aniMES(9)   U   CHAR*12    ARG     Name of each chain member
C     NUMNUC      U    INT       ARG     Number of chain members in current chain
C     ANION(9)    S    LOG       RAD     Flag to indicte if the radionuclide 
C                                        is to be considered an anion for wet
C                                        deposition fraction calculation.
C==== Modification History ===================================================
C     Date     Who  Modification Description
C     -------- ---  ------------------------------------------------------
C   27-Aug-97  DLS  Initial programming started
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE SETANION()
C
C---- Include statements -----------------------------------------------------
c
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'NUCNAM.CMN'
      INCLUDE 'RAD.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      CHARACTER*2 ANELTS(15)
      INTEGER NANIONS
      LOGICAL SEQI
C
C---- Data Statements --------------------------------------------------------
C
      DATA NANIONS/15/
      DATA ANELTS/'F ','Cl','Br','I ','At','O ','S ',
     .            'Se','Te','N ','P ','As','Sb','C ',
     .            'B '/
C
C----- Start of analysis -----------------------------------------------------
C
C     Loop on chain members
C
      nnn = nonuc
      if(nonuc .gt. 15)nnn = 15
      DO IN = 1,nnn
         ANION(IN) = .FALSE.
         DO IA = 1,NANIONS
            IF(SEQI(ANELTS(IA),ELT(IN),2)) THEN
               ANION(IN) = .TRUE.
               GO TO 100
            ENDIF
         END DO
 100     CONTINUE
      END DO
      RETURN
      END

