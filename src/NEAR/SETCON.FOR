C  SETCON.FOR   NEAR                 Version Date: 29-Sep-97              
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE SETCON                                *
C                                                                            *
C  Subroutine SETCON sets concentration input values into arrays for the     *
C         current time period analysis.
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    29-Sep-97                                               *
C  Last Modified:    29-Sep-97      DLS                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EDUP/NEAR 
C     Called by: ENV
C     Calls: NONE  
C     Common blocks referenced: CONIN
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     SETDATA    S      CHR    GID file  Storage array for GID data set 
C
C==== Modification History ===================================================
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C   29-Sep-97      DLS  Initial programming started
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE SETCON(NMST)
C
      INCLUDE 'CONIN.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
C      INTEGER ITM, TPATH
C
C---- Data Statements --------------------------------------------------------
C
C---- Start of Analysis
C
C----- Loop on radionuclides in current decay chain --------------------------
C
        DO IN = 1,NONUC
          INX = ICROS(IN)    ! cross index to chain position in master list
C
C----- Set surface soil concentrations at start of analysis ------------------
C
          IF (SURFS) THEN
            IF(INX.GT.0) THEN
               NONAGS(IN) = SSOIL(INX,NMST)
               DO ITF = 1,NTF
                  IF (TFD(ITF)) SOLCON(IN,ITF) = SSOIL(INX,NMST)
               END DO
               DO IAN = 1,NAN
                  IF (ANF(IAN)) THEN
                    SL2CON(IN,IAN) = SSOIL(INX,NMST)
                    IF(IAN.EQ.1) SL2CON(IN,5) = SSOIL(INX,NMST)
                    IF(IAN.EQ.3) SL2CON(IN,6) = SSOIL(INX,NMST)
                  ENDIF
               END DO
               RESSOL(IN) = SSOIL(INX,NMST)
             END IF
          ENDIF
C
C----- Set deep soil concentrations at start of analysis ---------------------
C
          IF(DEEPS) THEN
            IF(INX.GT.0) THEN
               NONAGD(IN) = DSOIL(INX,NMST)
               DO ITF = 1,NTF
                  IF (TFD(ITF)) DSCON(IN,ITF) = DSOIL(INX,NMST)
               END DO
               DO IAN = 1,NAN
                  IF (ANF(IAN)) THEN
                     DS2CON(IN,IAN) = DSOIL(INX,NMST)
                     IF(IAN.EQ.1) DS2CON(IN,5) = DSOIL(INX,NMST)
                     IF(IAN.EQ.3) DS2CON(IN,6) = DSOIL(INX,NMST)
                  ENDIF
               END DO
               DSRES(IN) = DSOIL(INX,NMST)
             END IF
          ENDIF
C
C----- Set waste package concentrations at start of analysis -----------------
C
          IF(WASTEP) THEN
            IF(INX.GT.0) THEN
               QWAS(IN) = WPCON(INX,NMST)
            ENDIF
          ENDIF
        END DO
C
      RETURN
      END
C
C----- END OF MODULE SETCON -------------------------------------------------
C

