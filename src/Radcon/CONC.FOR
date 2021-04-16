C     MEPAS RADCON: CONC.FOR              Version Date: 07-28-1995
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.
C*****************************************************************************
C                                                                            *
C                       SUBROUTINE CONC                                      *
C                                                                            *
C  Subroutine CONC calculates the dissolved and particulate contaminant      *
C  concentrations.  It first checks to see how the units of total            *
C  concentration are expressed.  If the total concentration is expressed     *
C  as weight of dry soil, the dissolved liquid phase (CO(I)) is calculated   *
C  using equation 4.6 from Whelan et al. 1987.  If the total contaminant     *
C  concentration is expressed as total volume, then equation 4.72 is used.   *
C  Finally, subroutine CONC calculates the particulate concentration (CP).   *
C  Subroutine CONC is called from subroutine PNTTIM and INPUTM.              *
C                                                                            *
C  Written by:            Gene Whelan                                        *
C                         Battelle Pacific Northwest Laboratory              *
C                         P.O. Box 999                                       *
C                         Richland, WA  99352                                *
C                                                                            *
C  Creation Date:         01/19/91 (Converted to PC)                         *
C  Last Modified:         07/28/95 - JPM                                     *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCONPC
C     Called by: SUBROUTINE PNTTIM and INPUTM
C     Calls to: None
C     Common Blocks referenced: TIME, INDEX2, CONC1, INDEX9, ISV
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter  Set/          Location
C     Name       Used   Type   Export/Import  Parameter Description
C     ---------  -----  -----  -------------- --------------------------------
C     AN5        USED   REAL   ARG - IMPORT   MOISTURE CONTENT
C     CO         S/U    REAL   ARG - EXPORT   BROUGHT IN AS TOTAL CONTAMINANT
C                                             CONCENTRATION, REDEFINED TO BE
C                                             DISSOLVE CONCENTRATION
C     CP         SET    REAL   ARG - EXPORT   PARTICULATE CONCENTRATION
C     EQCOEF     USED   REAL   COMMON - IMP   EQUILIBRIUM COEFFICIENT
C     II         S/U    INT    LOCAL          SAME AS ICONC
C     ICONC      USED   INT    COMMON - IMP   FLAG FOR UNITS ON SOIL SAMPLE;
C                                               = 1; CONCENTRATION PER UNIT
C                                                    VOLUME BASIS
C                                               = 2; CONCENTRATION PER MASS
C                                                    BASIS
C     R2         USED   REAL   ARG - IMPORT   BULK DENSITY OF SOIL
C     SOLLIM     USED   REAL   COMMON         SOLUBILITY LIMIT OF CONSTITUENT
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/24/91     JWB  Added required comments and header for MPEAS QA plan
C     02/12/92     JWB  Converted all COMMON blocks to INCLUDE statements
C     11/14/92     JWB  Added INDEX1 INCLUDE statement and modified the way
C                       CO and CP are computed
C     02/28/93     KJC  Added isv for RAAS
C     01/27/94     KJC  Added lines so no stop is used ERRORSIG is set instead
C     04/13/94     JPM  Added a check on the EQCOEF.  If it is less than zero,
C                       an error message is written to *.WLS and program
C                       execution is aborted.  This helps prevent the user
C                       from entering a -99.9 in the *.WIN file when the
C                       surface Kd is required.
C     06/07/94     JPM  Made changes so that when the initial concentration
C                       is <= zero and is thus set equal to the solubility
C                       limit, then this subroutine returns instead of
C                       continuing on to compute the particulate concentration
C                       when one is not needed.
C     07/28/95     JPM  Writing the error message to the *.WRN file and the
C                       screen, in addition to the *.WLS file.
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE CONC (CO,AN5,R2,I)
C
C==== COMMON Block Definitions ===============================================
C
CKJC  Added for dll compilation
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'ERROR.WTN'
      INCLUDE 'INDEX1.WTN'
      INCLUDE 'INDEX2.WTN'
      INCLUDE 'INDEX9.WTN'
      INCLUDE 'CONC1.WTN'
      INCLUDE 'CTIME.WTN'
      include 'isv.wtn'
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION CO(MAXCON)
C
C==== Variable Declarations ==================================================
C
C     None
C
C==== DATA Statements ========================================================
C
C     None
C
C*****************************************************************************
C CHECK THAT INITIAL CONCENTRATION OR SOLUBILITY LIMIT is NONZERO
C*****************************************************************************
CJPM      IF( CO(I).LE.0.0 .AND. SOLLIM(I).LE.0.0 ) THEN
CJPM        WRITE(6,1)
CJPM1       FORMAT (/2X,'WARNING ---> The concentration (CO) and the',
CJPM     1        ' solubility limit (SOLLIM) are both equal to',
CJPM     2        ' zero.  One of these must be nonzero.'/)
CKJC  Added for dll compilation
CJPM        ERRORSIG=.TRUE.
CJPM        goto 9999
CKJC        STOP ' CO and SOLLIM can not both equal zero (0.0) - CONC'
C*****************************************************************************
C WHEN INITIAL CONCENTRATIONS ARE NOT PROVIDED, CALCULATED THEM FOR USER
C*****************************************************************************
CJPM      else IF( CO(I).GT.1.0E+29 .and. sollim(i) .gt. 0.0) THEN
CJPM        CO(I) = SOLLIM(I)
CJPM        ICONC(I) = 3
CJPM      ENDIF
CJPM      IF( CO(I).GT.0.0 ) then
      if( ISTYPE.LE.2 .AND. ICONC(I).EQ.3 ) THEN
        RETURN
      ELSE
        A = AN5+R2*EQCOEF(I)
      ENDIF
CJPM      endif
      IF (EQCOEF(I).LT.0) THEN
        WRITE (6,78) I,EQCOEF(I)
        WRITE (11,78) I,EQCOEF(I)
        CALL MESSAGE ()
78      FORMAT (
     1  /2x,'ERROR ---> Contaminant number ',I2,' has a negative',
     2  /2x,'surface Kd (EQCOEF = ',1PE10.3,').  This scenario',
     3  /2x,'scenario requires that non-negative surface equilibrium',
     4  /2x,'coefficients be supplied for each constituent')
        ERRORSIG=.TRUE.
        GOTO 9999
      ENDIF
      if (raas_cs(i) .gt. 0.0 .and. iconc(i) .eq. 3) iconc(i)=2
      ii=iconc(i)
      go to (20,10,30) II

C*****************************************************************************
C CALCULATED DISSOLVED CONCENTRATION BASED ON EQ. 4.69 (WHELAN ET AL. 1987)
C*****************************************************************************
CKJC        IF( ICONC(I).EQ.1 ) CO(I) = CO(I)*R2/A
 10   if (raas_cs(i) .gt. 0.0) co(i) = raas_cs(i) * 0.000413
      co(i)=co(i)/a
      go to 30
C*****************************************************************************
C CALCULATED DISSOLVED CONCENTRATION BASED ON EQ. 4.72 (WHELAN ET AL. 1987)
C*****************************************************************************
CKJC        IF( ICONC(I).EQ.2 ) CO(I) = CO(I)/A
 20   if (raas_cs(i) .gt. 0.0 ) co(i) = raas_cs(i) * 0.001
      co(i)=co(i)*r2/a
C*****************************************************************************
C CALCULATED PARTICULATE CONCENTRATION EQ. 4.71 (WHELAN ET AL.1987)
C*****************************************************************************
 30   CP = CO(I)*EQCOEF(I)
        ICONC(I) = 3
 9999 RETURN
      END
