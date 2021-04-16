C  SETCON.FOR   EXPOS                Version Date: 01-Jan-97              
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
C  Creation Date:    01-Jan-97                                               *
C  Last Modified:    01-Jan-97      DLS                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EDUP/EXPOS
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
C   20-Dec-96      DLS  Initial programming started
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE SETCON(ITM,TPATH)
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'CONIN.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      INTEGER ITM, TPATH
C
C---- Data Statements --------------------------------------------------------
C
C---- Start of Analysis
C
      IF(ITM.LE.0.OR.ITM.GT.100) WRITE(NERR,1001) ITM
 1001   FORMAT(' Error, invalid value for time selection in SETCON'/
     .         '    Value found was:',I3,'. Must be between 1 and 100.')
c
      IF(TPATH.EQ.1) THEN              ! Groundwater pathways
        DO IN = 1,NONUC
          INX = ICROS(IN)
          IF(INX.GT.0) GWCON(IN) = WATC(ITM,INX)
        END DO
c
      ELSE IF(TPATH.EQ.2) THEN          ! Surface water pathways
        DO IN = 1,NONUC
          INX = ICROS(IN)
          IF(INX.GT.0) SWCON(IN) = WATC(ITM,INX)
        END DO
C
      ELSE IF(TPATH.EQ.3) THEN         ! Atmospheric pathways
        DO IN = 1,NONUC
C         INX = ICROS(IN)
C         IF(INX.GT.0) THEN
            ARPCON(IN) = AIRCON(IN)
            DO ITF = 1,NTF
              IF(TFD(ITF)) THEN
                ARFCON(IN,ITF) = AIRCON(IN)
              END IF
            END DO
C            DO IAN = 1,NAN+2
            DO IAN = 1,NAN
              IF (ANF(IAN)) THEN
                ARF2CN(IN,IAN) = AIRCON(IN)
                IF(IAN.EQ.1) ARF2CN(IN,5) = AIRCON(IN)
                IF(IAN.EQ.3) ARF2CN(IN,6) = AIRCON(IN)
              END IF
            END DO
C          ENDIF
        END DO
C
C      ELSE IF(TPATH.EQ.4) THEN         ! Soil at the source pathways
C        DO IN = 1,NONUC
C          INX = ICROS(IN)
C          IF(INX.GT.0) THEN
C            NONAGS(IN) = SOILCIN(ITM,INX)
C            DO ITF = 1,NTF
C              IF (TFD(ITF)) SOLCON(IN,ITF) = SOILCIN(ITM,INX)
C            END DO
C           DO IAN = 1,NAN+2
C            DO IAN = 1,NAN
C              IF (ANF(IAN)) SL2CON(IN,IAN) = SOILCIN(ITM,INX)
C              IF(IAN.EQ.1) SL2CON(IN,5) = SOILCIN(ITM,INX)
C              IF(IAN.EQ.3) SL2CON(IN,6) = SOILCIN(ITM,INX)
C            END DO
C            RESSOL(IN) = SOILCIN(ITM,INX)
C          END IF
C        END DO
C
      ELSE
        WRITE(NERR,1000) TPATH
 1000   FORMAT(' Error, invalid value for pathway selection in SETCON'/
     .         '    Value found was:',I3,'. Must be between 1 and 3.')
      ENDIF
      RETURN
C----- END OF MODULE SETCON -------------------------------------------------
      END

