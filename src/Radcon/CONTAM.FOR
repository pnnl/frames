C     MEPAS RADCON: CONTAM.FOR            Version Date: 02-27-2001
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.
C*****************************************************************************
C                                                                            *
C                         SUBROUTINE CONTAM                                  *
C                                                                            *
C  Subroutine CONTAM reads the contaminant chemistry input.  Such data       *
C  includes case number, half-life, equilibrium coefficient and others.      *
C  Subroutine CONTAM is called by the main programs and in turn calls        *
C  subroutine CHECKS.                                                        *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    01/19/91 (Converted to PC)                              *
C  Last Modified:    02/27/01 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: RADCONPC
C     Calls to: SUBROUTINE CHECKS
C     Common blocks referenced: DUMMY, INIT, INTER, TIME, INDEX1, INDEX2,
C                               INDEX3, INDEX4, INDEX7, INDEX9
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter  Set/               Location
C     Name       Used     Type      Export/Import  Parameter Description
C     ---------  -----  ----------- -------------  ---------------------------
C     CO         SET      REAL      COMMON - EXP   SOURCE TERM CONCENTRATION
C     TFINAL     SET                COMMON - EXP   SIMULATION TIME PERIOD
C     IDECAY     SET      INTEGER   COMMON - EXP   INDEX FOR TYPE OF DECAY
C     IDDSW      SET      INTEGER   COMMON - EXP   INDEX FOR DIRECT DISCHARGE
C                                                  TO SURFACE WATER:
C                                                    = 0; NO DIRECT DISCHARGE
C                                                    = 1; DIRECT DISCHARGE
C     IPOND      SET      INTEGER   COMMON - EXP   INDEX FOR POND AS SOURCE:
C                                                    = 1; DESIGNATED POND
C                                                    = 2; NOT PONDED
C     ISOURC     SET      INTEGER   COMMON - EXP   INDEX FOR SOURCE TERM:
C                                                    = 1; UNKNOWN SOURCE TERM
C                                                    = 2; KNOWN SOURCE TERM
C                                                    = 3; KNOWN SOURCE TERM &
C                                                         PRECIPITATION
C                                                         GENERATED LEACHATE
C                                                    = 4; KNOWN CONCENTRATIONS
C                                                         AT RECEPTOR
C     ISCONF    SET    INTEGER COMMON - EXP   INDEX FOR SOURCE CONFIGURATION
C                                               = 1; POINT SOURCE
C                                               = 2; X LINE SOURCE
C                                               = 3; Y LINE SOURCE
C                                               = 4; AREA SOURCE
C     ISTYPE     SET      INTEGER   COMMON - EXP   INDEX FOR SOURCE TERM TYPE:
C                                                    = 1 OR 2; GROUNDWATER
C                                                    > 2; OVERLAND RUNOFF
C     ND         S/U      INTEGER   COMMON - EXP   TOTAL # OF CONTAMINANTS IN
C                                                  CHAIN (PARENT & DAUGHTERS)
C     NDS        S/U      INTEGER   COMMON - EXP   ARRAY CONTAINING ND VALUES
C     NPRINT     SET      INTEGER   COMMON - EXP   INDEX FOR DETAIL OF OUTPUT
C     NTIMSPEC   SET      INTEGER   COMMON - EXP   Time Specific Modification -
C                                                  this variable indicates the
C                                                  number of specific times at
C                                                  which concentrations will
C                                                  be computed (see INDEX1.WTN).
C                                                  Usually implemented for
C                                                  finding spatially varying
C                                                  concentrations.
C     NUMCON     SET      INTEGER   COMMON - EXP   # OF PARENT CONTAMINANTS
C     RRATIO     SET      REAL      COMMON - EXP   The threshold ratio for
C                                                  implementing the
C                                                  instantaneous solution.  If
C                                                  the time to peak (TPEAK1(M1))
C                                                  divided by the duration of
C                                                  of time over which a
C                                                  contaminant enters the
C                                                  medium is greater than or
C                                                  equal to RRATIO, then the
C                                                  instantaneous solution is
C                                                  implemented.
C     TIMSPEC    SET      REAL      COMMON - EXP   Time Specific Modification -
C                                                  this is an array holding the
C                                                  specific times at which
C                                                  concentrations will be
C                                                  computed (see CTIME.WTN).
C                                                  Usually used for finding
C                                                  spatially varying
C                                                  concentrations.
C     KTYPE      SET      INTEGER   ARG - EXP      MEPAS contaminant type from
C                                                  the chemical database. 1
C                                                  indicates a radionuclide,
C                                                  all other values indicate
C                                                  a non-radionuclide.
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     06/27/91     JGD  Ignore decay-products for ISOURC = 4
C     11/24/91     JWB  Added required comments and header for MEPAS QA plan
C     08/10/92     JWB  TFINAL REPLACES GAMMA AS A VARIABLE (SIMULATION TIME)
C     11/14/92     JWB  ISCONF variable added to read statement and removed
C                       INITIN
C     01/25/93     JWB  Modifications made to include CAP changes from JLS
C     03/02/93     JWB  Read statement modified to read in PNTFRAC (multiple
C                       aquifer modification)
C     01/27/94     KJC  Added lines so no stop is used ERRORSIG is set instead
C     04/15/94     JPM  Time Specific Modification.  NTIMSPEC is read in along
C                       with the other waterborne control parameters, and the
C                       TIMSPEC array is filled if necessary.  An NTIMSPEC
C                       <= zero, bypasses the time specific modification.  An
C                       NTIMSPEC > MAX_ST causes an error, and the run will
C                       abort.  JPM/GW
C     04/17/94     JPM  Addition of Instantaneous Solution - Read in RRATIO
C                       as part of group 9.
C     05/02/94     JPM  Changed the default value of TFINAL to 10001 (for
C                       HRA-EIS).  Added a check on the specific times - the
C                       run aborts if a specific time exceeding TFINAL is
C                       entered.
C     05/31/94     JPM  Replaced references to 20 as the maximum number of
C                       allowed specific times to MAX_ST, which is defined
C                       in MAXIMUM.FOR.
C     06/01/94     JPM  Removed the statement that set initial concentrations
C                       to 1E+30 if they are read in <= zero.  Initial
C                       concentrations are now set to 1E+30 in INPUTM if
C                       necessary.
C     06/07/94     JPM  When TLIFE is zero, set it equal to TFINAL.
C     07/28/94     JPM  Redefined TDIFF as the difference between the start
C                       date for the release of a contaminant and the start
C                       date for the risk calculations (WS-TRISK - WS-CDATE).
C                       This makes TDIFF in RADCON consistent with the shell's
C                       definition of TDIFF.
C     09/16/94     JPM  Using the solubility limit for CO when ISOURC=1 and
C                       the user did not enter CO.  If the solubility limit
C                       is unreasonable, the run is aborted.
C     07/28/95     JPM  Writing warning messages to the *.WRN file only.
C                       Writing error messages to the screen and the *.WRN
C                       file in addition to the *.WLS file.
C     11/06/96     JPM  Setting TLIFE equal to TFINAL under all conditions.
C                       TLIFE is set equal to the last flux time minus the
C                       first flux time in subroutine SOURCE.  TLIFE was
C                       causing problems because its value in the *.WIN file
C                       is not always equal to the last flux time minus the
C                       first flux time because of rounding (PH1-P003).
C     02/27/01     JPM  Reading in KTYPE from the WIN file (PH1-P021).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE CONTAM (TLIFE,CO,GAMMA,KTYPE)
C
C==== COMMON Block Definitions ===============================================
C
CKJC  Added for dll compilation
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'ERROR.WTN'
      INCLUDE 'INDEX1.WTN'
      INCLUDE 'INDEX2.WTN'
      INCLUDE 'INDEX3.WTN'
      INCLUDE 'INDEX4.WTN'
      INCLUDE 'INDEX7.WTN'
      INCLUDE 'INDEX9.WTN'
      INCLUDE 'CTIME.WTN'
      INCLUDE 'DUMMY.WTN'
      INCLUDE 'INIT.WTN'
      INCLUDE 'INTER.WTN'
      INCLUDE 'COM21.WTN'
      INCLUDE 'cap.WTN'
C
C==== DIMENSION Statements ===================================================
C
      INTEGER*4 KTYPE
      DIMENSION CO(MAXCON),TLIFE(MAXCON),KTYPE(MAXCON)
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
C     READS INPUT CONTROL PARAMETERS
C     ISCONF ADDED AT THE END OF READ STATEMENT TO REDUCE IMPACT ON OLD FILE
C                            JWB - 11/14/92
C     Read statement modified to added PNTFRAC (multiple aquifer)
C                            JWB - 01/31/93
C*****************************************************************************
CJPM      READ(5,1) ISTYPE,ISOURC,IPOND,IDECAY,IDDSW,NPRINT,NUMCON,
CJPM     1          tfinal,ISCONF,PNTFRAC
CJPM1     FORMAT (7I5,1E10.3,I5,e10.3)
CKJC      READ(5,1) ISTYPE,ISOURC,IPOND,IDECAY,IDDSW,NPRINT,NUMCON,
CKJC     1          tfinal,ISCONF
CKJC1     FORMAT (7I5,1E10.3,I5)
      READ(5,1) ISTYPE,ISOURC,IPOND,IDECAY,IDDSW,NPRINT,NUMCON,
     1          ISCONF,NTIMSPEC
1     FORMAT (16I5)
      IF (NTIMSPEC.GT.MAX_ST) THEN
           WRITE (6,894) NTIMSPEC,MAX_ST,MAX_ST
           WRITE (11,894) NTIMSPEC,MAX_ST,MAX_ST
           CALL MESSAGE ()
           ERRORSIG=.TRUE.
           GOTO 9999
      ENDIF
894   FORMAT (
     C /2x,'ERROR --> NTIMSPEC = ',I5,
     C /2x,'Cannot run on more than ',I2,' specific times',
     C /2x,'(NTIMSPEC must be <= ',I2,')')
      READ(5,263) TFINAL,PNTFRAC,RRATIO,(TIMSPEC(I),I=1,NTIMSPEC)
263   FORMAT (8E10.3)
      IF (RRATIO.LE.0) THEN RRATIO = 1.0E+20
      if( isconf.eq.0 ) isconf=4
      IF( TFINAL .LE. 0.0 ) TFINAL = 10001.0
C
C*****************************************************************************
C     If any specific times are greater than TFINAL, then abort the run.
C*****************************************************************************
C
      IF (NTIMSPEC.GT.0) THEN
          DO 739 I = 1,NTIMSPEC
               IF (TIMSPEC(I).GT.TFINAL) THEN
                    WRITE (6,481) TFINAL,TIMSPEC(I)
                    WRITE (11,481) TFINAL,TIMSPEC(I)
                    CALL MESSAGE ()
                    ERRORSIG=.TRUE.
                    GOTO 9999
               ENDIF
739       CONTINUE
      ENDIF
481   FORMAT (
     1 /2x,'ERROR ---> TFINAL =     ',1PE10.3,
     2 /2x,'A specific time of ',1PE10.3,' was entered.  Specific',
     3 /2x,'times must not exceed TFINAL')
      GAMMA = 0.0
C*****************************************************************************
C     CHECKS TO ENSURE THAT THE SCENARIO OUTLINED CAN BE HANDLED
C*****************************************************************************
      CALL CHECKS
CKJC  Added for dll compilation
      if (ERRORSIG) goto 9999
C*****************************************************************************
C     READS IN CONTAMINANT INFORMATION FOR EACH CONTAMINANT BUT FIRST
C     INITIALIZES THE ARRAYS
C*****************************************************************************
      NUMCO=NUMCON
      DO 10 I=1,NUMCON
      CO(I)=0.
      ICONC(I)=0
      EQCOEF(I)=0.
      IF ((ISTYPE .NE. 4) .AND. (ISTYPE .NE. 6)) GO TO 624
      WRITE(6,625)
  625 FORMAT ('-----> See JIM DROPPO for ATMOSPHERIC input <------')
CKJC  Added for dll compilation
      ERRORSIG=.TRUE.
CKJC      STOP
      goto 9999
  624 if( incr.eq.1) then
        READ(5,102) IV(I,1),CASN1(I,1),AL(I,1),NDS(I),CO(I),ICONC(I),
     1              EQCOEF(I),SOLLIM(I),CINVEN(I),TLIFE(I),TDIFF(I),
     1              TDELTA(I),KTYPE(I)
      else
        READ(5,102) IV(I,1),CASN1(I,1),AL(I,1),NDS(I),CO(I),ICONC(I),
     1              EQCOEF(I),SOLLIM(I),xjunk,TLIFE(I),TDIFF(I),
     1              TDELTA(I),KTYPE(I)
      endif
  102 FORMAT (A18,2X,A18,2X,E10.3,I5,E10.3,I5,3E10.3/10X,3E10.3,I5)
C*****************************************************************************
C     When ISOURC = 1, the user must supply an initial concentration at the
C     source or there must be a solubility limit for this constituent, so
C     fluxes can be calculated.  If CO(I) is <= zero and ISOURC = 1 then use
C     the solubility limit for CO.  If the solubility limit is > 10 g/mL, then
C     abort the run.
C*****************************************************************************
      IF ((ISOURC.EQ.1).AND.(CO(I).LE.0.0)) THEN
         IF (SOLLIM(I).LE.10.0) THEN
           CO(I) = SOLLIM(I)
           ICONC(I) = 3
           WRITE (11,175) I
         ELSE
           WRITE (6,174) CO(I),I
           WRITE (11,174) CO(I),I
           CALL MESSAGE ()
           ERRORSIG=.TRUE.
           GO TO 9999
         ENDIF
175      FORMAT (/
     1   2X,'WARNING -->  Contaminant number ',0PI2,/
     2   2X,'The initial concentration is less than or equal to zero,',/
     3   2X,'so the solubility limit is being used for the initial',/
     4   2X,'concentration.',/)
174      FORMAT (/
     1   2X,'ERROR -->  ISOURC = 1, CO(I) = ',1PE10.3,/
     2   2X,'Contaminant number ',0PI2,' has an initial concentration',/
     3   2X,'(CO(I)) less than or equal to zero, and the solubility',/
     4   2X,'limit for this constituent is unreasonable (>10 g/mL).'/
     5   2X,'To run this case, a non-negative and non-zero initial',/
     6   2X,'concentration is required in order to compute source',/
     7   2X,'term fluxes when ISOURC = 1')
      ENDIF
      TLIFE(I) = TFINAL
      IF (NDS(I) .LE. 1) NDS(I)=1
      IF (NDS(I) .LE. 1) GO TO 10
C*****************************************************************************
C     READ DECAY-PRODUCT INFORMATION
C*****************************************************************************
      ND=NDS(I)
      DO 502 N=2,ND
      READ(5,102) IV(I,N),CASN1(I,N),AL(I,N)
  502 CONTINUE
c     ignore decay products for isourc=4 /June 27, 1991/jgd
      if (isourc.eq.4) then
        nds(i)=1
        write(11,12) IV(I,1)
   12   FORMAT(
     C  /2x,'WARNING --> For a known concentration, decay products',
     C  /2x,'for ',A18,' are not considered.')
      endif
   10 CONTINUE
      IF (ISTYPE .GT. 2) TDIFF(I)=0.
c      WRITE(6,11)
c   11 FORMAT (2X,'THE TIME DIFFERENCE (TDIFF) IN YEARS BETWEEN TODAY
c     C (E.G., 1990) AND WHEN THE CONTAMINANTS WERE FIRST'/2X,'RELEASED
c     C INTO THE ENVIRONMENT (E.G., 1970), AND THE DURATION OF TIME'/2X,
c     C '(I.E., THE LIFE OF THE WASTE SITE FOR EACH CONTAMINANT; TLIFE)
C     C CONTAMINANTS ARE RELEASED FROM THE WASTE SITE:'//)
      WRITE(6,11)
   11 FORMAT (
     +' TLIFE is the duration of release for each contaminant expressed 
     +in years. '/
     +' TDIFF is the time difference in years between the start date for
     + risk'/' calculations and the date when a contaminant was first re
     +leased into'/' the environment (WS-TRISK minus WS-CDATE).'//)
      WRITE(6,2)
    2 FORMAT (T15,'Constituent',T35,'TLIFE',T45,'TDIFF',t60,'TFINAL'/
     C t19,'Name',T34,'(years)',T44,'(years)',T60,'(years)'/)
      DO 5 I=1,NUMCON
      WRITE(6,3) IV(I,1),TLIFE(I),TDIFF(I),tfinal
    3 FORMAT (T16,A18,T32,1PE10.3,T42,1PE10.3,T57,1PE10.3)
    5 CONTINUE
      RETURN
 9999 END
