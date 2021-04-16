C   SWACUTE.FOR                        Version Date: 27-Feb-97               *
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.     *
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE SWACUTE                               *
C                                                                            *
C  Subroutine SWACUTE evaluates water concentration at a location for one    *
C             decay chain for release to a river                             *
C
C  Written by:       Dennis Strenge                                          *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    31-Oct-1996                                             *
C  Last Modified:    27-Feb-1997  DLS                                        *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: GENII/SWATER
C     Called by: SWATER
C     Calls: NONE
C     Common blocks referenced: DEVICE, SWPAR, SWINFO, SWCON
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     
C==== Modification History ===================================================
C
C    Date     Who  Modification Description
C  ---------  ---  -----------------------------------------------------------
C  27-Feb-97  DLS  Added common block DEVICE   
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE SWACUTE(IL,IX)
C
C==== COMMON Block Definitions ===============================================
C
        include 'DEVICE.CMN'
C       INCLUDE 'FNAMES.CMN'
        INCLUDE 'OPT.CMN'
        include 'SWPAR.CMN'
        INCLUDE 'SWINFO.CMN'
        INCLUDE 'SWCON.CMN'
        INCLUDE 'DECAY.CMN'
C
C==== DIMENSION Statements ===================================================
C
C     NONE
C 
C==== Variable Declarations ==================================================
C
C     CHARACTER*7 ACUTE,CHRONIC
C     CHARACTER*15  RIVER,NSLAKE
C
C==== DATA Statements ========================================================
C
C---- Calculate water dispersion factor --------------------------------------
C
      DISP = 1.0 / (TREL * SWFLOW(IL))        ! 1/m3 for all constituents
C
C---- Loop on radionuclides in decay chain -----------------------------------
C
      NUC = NDS(IX)+1
      DO IN = 1,NUC
        SWCON(IN) = DISP * QSW(IN) * YRSEC * M3L
      END DO
      IF(SWCON(1).GT.0.) THEN
        INTGRL = 0
        T = SWTT(IL) * DAYHR
        CALL CHAIN (T,DUMMY,SWCON,SWCON,INTGRL)
      ENDIF
      IF(DEBUG) write(NSLS,101) nuc,(swcon(i),i=1,nuc)
 101  format(' Acute water conc. nuc =',I3,' Water Conc:'/
     .       1p9e9.1)
 99   RETURN
      END
C====================== END OF MODULE SWACUTE ============================
C
