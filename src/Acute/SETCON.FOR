C  SETCON.FOR   EXPOS                Version Date: 23-Mar-98              
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
C  Last Modified:    23-Mar-98   BAN                                     *
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
C   23-Mar-98      BAN  Standardized nomenclature for integrated soil values
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
      IF(TPATH.EQ.2) THEN          ! Surface water pathways
        DO IN = 1,NONUC
          INX = ICROS(IN)
          IF(INX.GT.0) SWACUT(IN) = WATC(ITM,INX)
        END DO
C
      ELSE IF(TPATH.EQ.3) THEN         ! Atmospheric pathways
        IF(ITM.EQ.1) THEN
          DO IN = 1,NONUC
            PWTAIR(IN) = AIRCON(IN)
            PWTXAR(IN) = EXTD(IN)
            DO ITF = 1,NTF
              IF(TFD(ITF)) THEN
                CT0AIR(IN,ITF) = AIRCON(IN)
              END IF
            END DO
            DO IAN = 1,NAN+2
              IF(IAN.LE.4) THEN
                IF (ANF(IAN)) THEN
                  CT0AI2(IN,IAN) = AIRCON(IN)
                END IF
              ELSE IF(IAN.EQ.5.AND.ANF(1)) THEN
                CT0AI2(IN,5) = AIRCON(IN)
              ELSE IF(IAN.EQ.6.AND.ANF(3)) THEN
                CT0AI2(IN,6) = AIRCON(IN)
              ENDIF
            END DO
          END DO
C
C----- Set initial depostion to soil from air --------------------------------
C
          DO IN = 1,NONUC
            INX = ICROS(IN)
            IF(INX.GT.0) THEN
                RESSOL(IN) = (DRYDEP(IN) + WETDEP(IN)) / SLDN
                DO ITF = 1,NTF
                  IF (TFD(ITF)) SOLCON(IN,ITF) = RESSOL(IN)
                END DO
                DO IAN = 1,NAN+2
                  IF(IAN.LE.4) THEN
                    IF (ANF(IAN)) THEN
                      SL2CON(IN,IAN) = RESSOL(IN)
                    END IF
                  ELSE IF(IAN.EQ.5.AND.ANF(1)) THEN
                    SL2CON(IN,IAN)  = RESSOL(IN)
                  ELSE IF(IAN.EQ.6.AND.ANF(3)) THEN
                    SL2CON(IN,IAN)  = RESSOL(IN)
                  ENDIF
                END DO
            END IF
          ENDDO
        ELSE
          PWTAIR(IN) = 0.0
          PWTXAR(IN) = 0.0
          DO ITF = 1,NTF
            CT0AIR(IN,ITF) = 0.0
          END DO
          DO IAN = 1,NAN+2
            CT0AI2(IN,IAN) = 0.0
          END DO
        ENDIF
C
C----- When TPATH is not 2, or 3, there is an error condition ---------------
C      Note: groundwater is not allowed for acute releases in GENII
C
      ELSE
        WRITE(NERR,1000) TPATH
 1000   FORMAT(' Error, invalid value for pathway selection in SETCON'/
     .         '    Value found was:',I3,'. Must be between 2 or 3.')
      ENDIF
      RETURN
C
C----- END OF MODULE SETCON -------------------------------------------------
C
      END

