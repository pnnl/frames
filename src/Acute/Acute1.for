C-------------------------------------- VERSION 23-Mar-98 ---------------------------------
C
C      SUBROUTINE ACUTE1 ()
C
C     This subroutine calculates initial soil conc. from air and surface
C     water from an acute release
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Initial Version:       27-Jun-90  RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C     Last modification:     23-Mar-98  BAN
C-----------------------------------------------------------------------
C 
C     CT0IRR() - Initial concentration in surface soil from irrigation 
C                for each radionuclide in the current chain, UoA/m2
C     DAL      - Double precision temporary variable
C     DKAIRA() - Air concentration decayed for travel time to each
C                distance in the specified sector for population dose, 
C                UoA
C     DNTGRL   - Double precision temporary varaible
C     SWACUT() - Time-integrated surface water concentration decayed
C                for travel time to  irrigation-withdrawal location,
C                UoA yr/L 
C-----------------------------------------------------------------------------
C   Modification History
C   Date     Who   Modification Summary
C  ---------  ---  -----------------------------------------------------------
C  20-Nov-97  DLS  Revised comment lines for EDUP version
C  21-Nov-97  DLS  Added leaching to chain decay calculations
C  23-Mar-98  BAN  Standardize nomenclature and use of integrated soils
C-----------------------------------------------------------------------------
C
      SUBROUTINE ACUTE1 ()
C
C----- Include statements ----------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'AIRPAR.CMN'
      INCLUDE 'ANMPAR.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'DECAY.CMN'
      INCLUDE 'FODPAR.CMN'
      INCLUDE 'LABELS.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'SOLPAR.CMN'
      INCLUDE 'SWPAR.CMN'
      INCLUDE 'TIMES.CMN'
C
C---- Type statements --------------------------------------------------------
C
      REAL T, CT0IRR(CHAINLEN)
C
      IF(IRRSR.EQ.2) THEN
        DO IN  = 1, NONUC
C
C----- Irrigation contribution -----------------------------------------------
C
          CT0IRR(IN) = SWACUT(IN) * RIRRR * LM2IN /SLDN*IRTIMR*MOYR 

        END DO
      ENDIF
C
C---- Get time-integral of soil concentration Bq y/m2 ------------------------
C
      T = 1.0                  ! 1 year integration time
      DO 308 IN = 1, NONUC
        RESSOL(IN) = RESSOL(IN) + CT0IRR(IN)
  308 CONTINUE
      CALL CHAIN (T,LEACHR, RESSOL, RESAVG, 1) 
      CALL CHAIN (T,LEACHR, RESSOL, RESSOL, 0) 
C
      RETURN
      END
C
C---- END OF MODULE ACUTE1 ---------------------------------------------------
C

