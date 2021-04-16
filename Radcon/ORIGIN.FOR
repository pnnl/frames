C     MEPAS RADCON: ORIGIN.FOR            Version Date: 06-16-1998
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                          SUBROUTINE ORIGIN                                 *
C                                                                            *
C  Subroutine ORIGIN shifts the modeling time origin when crossing from      *
C  one medium to the next.  It is called by subroutine MEASUR and the        *
C  the main program.                                                         *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    06/16/98 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of:  RADCON
C     Called by:  RADCONPC and SUBROUTINE MEASUR
C     Calls to: None
C     Common blocks referenced:  INTER, TIME. INDEX3, INDEX4, INDEX8,
C                                INDEX9, COM21
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- ------ ------  ------------- ---------------------------------
C     CS        SET    REAL    ARG - EXPORT   CONCENTRATION OF CONTAMINANT    
C     CONCMAX   SET    REAL    ARG - EXPORT   MAX CONCENTRATION; ALSO CMAX    
C     CONCTIM   SET    REAL    ARG - EXPORT   TIME OF MAX CONCENTRATION, ALSO 
C                                             KNOWN AS CTMAX                  
C     KKFLAG    SET    INT     LOCAL          WHEN EQUAL TO 1, INDICATES THAT
C                                             THE FOOTNOTE TO THE MASS TABLE
C                                             SHOULD BE WRITTEN.  NOT WRITTEN
C                                             WHEN KKFLAG = 0.
C     NFLAG     USED   INT     ARG - IMPORT   INDICATES IF THE WARNING MESSAGE
C                                             ABOUT EXCEEDING THE INITIAL
C                                             CONCENTRATION AT THE SOURCE
C                                             SHOULD BE WRITTEN
C     TOTTIM    SET    REAL    COMMON-EXP     ACTUAL TIME AT THE RECEPTOR,    
C                                             MEDIUM TIME PLU TRAVEL TIME     
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     06/27/91     JGD  Output text revised
C     11/26/91     JWB  Added required comments and header for MEPAS QA plan  
C     03/11/92     JWB  COMMON blocks converted to INCLUDE statements
C     03/12/92     JWB  INDEX1.WTN added for ISOURC value to compute DMAX for
C                       ISOURC=4
C     02/28/93     KJC  Commented line to divide cs by unit
C     04/15/94     JPM  Time Specific Modification - write out dummy
C                       CS and TIM array to *.POL file.  Skip output
C                       of the present and maximum concentrations when
C                       concentrations are calculated only at specific
C                       times.  JPM/GW
C     04/20/94  GW/AdH  Saving the concentrations that are written to the
C                       *.POL file in the CSSW array for multiple river
C                       receptor concentration calculations.
C     05/27/94     JPM  Made changes to compute the total undecayed mass for
C                       each parent passing thru each medium in which fluxes
C                       are computed.
C     05/31/94     JPM  Using MAX_ST in dimension statement for CS_SPEC.
C                       MAX_ST is set in MAXIMUM.FOR and is the maximum
C                       allowed number of specific times.
C     05/31/94     AdH  Removed CSSW array - no longer needed since COUPLE
C                       has been modified and no longer overwrites the CS
C                       array.
C     06/01/94     JPM  Removing DMAX.  Setting CONCMAX equal to CO directly
C                       when ISOURC = 4.  For warning message, if the maximum
C                       concentrations exceeds CO, then the message is
C                       written.
C     06/03/94     JPM  Writing a warning message to the *.WLS file if the
C                       maximum concentration exceeds the solubility limit.
C     06/08/94     JPM  Changed the conditional that determines if the warning
C                       message when the maximum concentration exceeds the
C                       initial concentration is written out.  If CONCMAX is
C                       > OR EQUAL TO the initial concentration, the warning
C                       message is written.  CONCMAX is the maximum of the
C                       already truncated concentrations.
C     07/14/94     JPM  Added NFLAG - is turned on by COMPAR if the computed
C                       concentration exceeds the decayed initial concentration
C                       at the source.  Revised the text of the warning message
C                       that is output when the maximum concentration exceeds
C                       the initial source concentration.
C     07/21/94     JPM  Changed the setting of the loop index when
C                       concentrations at specific times are written out.
C                       No longer storing the time = zero record in the
C                       CS_SPEC and CS_TIME arrays (see note in WLIST.FOR).
C     07/25/94     JPM  Using the array of decay constants (AL) to determine
C                       which contaminants decay.  No longer writing the
C                       percent of total mass released from source in the
C                       mass table for decaying substances.
C     07/29/94     JPM  Made changes in the output to the *.WLS file to
C                       reflect the revised definition of TDIFF (TDIFF = 
C                       WS-TRISK minus WS-CDATE).  No longer using IYR.
C     08/04/94     JPM  When ISOURC=4, need to divide CO by UNITS when setting
C                       CONCMAX. (RAD-P037)
C     08/23/94     JPM  Removed the code that writes to the maximum
C                       constituent table.  Replaced with a call to MAXTABLE.
C     08/26/94     JPM  Changed the mass table that is written to the *.WLS
C                       file.  It now includes instantaneous solution
C                       information as well.
C     09/06/94     JPM  Modified column headings for mass table.
C     09/09/94     JPM  Saving the mass under the flux curve as TOTMAS for
C                       use by the instantaneous solution
C     12/22/94     JPM  Code was writing the message about zero concentrations
C                       due to the contaminant being past the receptor by the
C                       end of TDIFF in cases where the source term flux is
C                       zero.  Now writing a different message if the source
C                       term flux for the parent (SMASS array) is <= zero. 
C     02/07/95     JPM  The "Mass Passed this Medium" was not being computed
C                       correctly when decay products are involved.  Changed
C                       the subscript on the CS array from M1 to M3.  Using M1
C                       assumes that the next slot in the CS array contains
C                       data for the next parent, when it may contain decay
C                       product fluxes for the previous parent which are zero.
C     02/10/95     JPM  Discontinued the output of decay product fluxes to
C                       the *.POL file.  They are always zero.  Also,
C                       reworded the explanation written to the *.WLS file
C                       about the numerical/instantaneous solution.
C     08/01/95     JPM  Writing out a mass balance table for the wetland
C                       environment when the wetland is not the last medium.
C     08/03/95     JPM  Changing "Travel Time" in the *.POL file to "Total
C                       Time" which will hopefully result in less confusion
C                       on the part of the MEPAS users.
C     10/18/96     JPM  Allowing for MED=2 to indicate a concentration
C                       computation in the vadose zone, and allowed fluxes to
C                       be computed in the last medium.  Changes being made to
C                       prepare the Phase 1 MEPAS waterborne executable for
C                       the framework (PH1-E001).
C     10/30/96     JPM  Allowing MED = 6 to indicate a flux computation in
C                       the river (PH1-E003).
C     11/11/96     JPM  Changed units from Ci to pCi (PH1-E006).
C     04/15/97     JPM  As part of PH1-E001 (above), it was assumed that the
C                       current medium is always the last medium, because in
C                       the framework, each medium would be a separate RADCON
C                       run.  Thus, the conditional NMED=KMED was hard coded.
C                       Because the framework version has now been designated
C                       for use in MEPAS 3.2, NMED=KMED can no longer be
C                       assumed.  Thus, the original conditionals are being
C                       restored (PH1-E008).
C     04/16/97     JPM  Revised how the mass table is written to the *.WLS
C                       file.  Relying on medium type to determine the type
C                       of table to output, instead of distinguishing between
C                       table types based on whether the current medium is
C                       the last medium or not (PH1-E007).
C     04/28/97     JPM  Removed the writing of the concentration at the start
C                       of risk computations and writing of the maximum
C                       concentrations to the *.WLS file.  These are now
C                       written after decay product summation (PH1-E014).
C     04/29/97     JPM  Dimensioning CS_SPEC to MAXNTI instead of MAX_ST
C                       (PH1-P009).
C     05/15/98     JPM  Made changes to prevent a divide by zero error when
C                       SMASS is zero (writing a -99.9 to the WLS file
C                       instead of computing the percentage mass
C                       conservation) (RAD-E054).
C     06/09/98     JPM  Making IVS and CASN2 character variable types
C                       (PH1-E016).
C     06/16/98     JPM  Changed the criterion for deciding if a constituent
C                       decays from a decay constant LT 1.0E-20 to
C                       LT 1.0E-17 (PH1-P013).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE ORIGIN (CS,IVS,CASN2,ICNT,M1,UNITS,CONCMAX,
     1                   CONCTIM,NDS,CO,CS_SPEC,NFLAG,AL,RATIO,
     2                   GW_MASS,VOL_MAS,SW_MASS,TIMAX)
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'INDEX1.WTN'
      INCLUDE 'INDEX3.WTN'
      INCLUDE 'INDEX4.WTN'
      INCLUDE 'INDEX8.WTN'
      INCLUDE 'INDEX9.WTN'
      INCLUDE 'COM21.WTN'
      INCLUDE 'CTIME.WTN'
      INCLUDE 'INIT.WTN'
      INCLUDE 'INTER.WTN'
      INCLUDE 'MASSS.WTN'
      INCLUDE 'MEDIA.WTN'
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION CS(MAXNTI,MAXCON),CO(MAXCON),NFLAG(MAXCON)
      DIMENSION CONCMAX(MAXCON),TIMAX(MAXCON),AL(MAXCON,12)
      DIMENSION NDS(MAXCON),CONCTIM(MAXCON),CS_SPEC(MAXNTI,MAXCON)
      DIMENSION RATIO(MAXCON),GW_MASS(MAXCON),VOL_MAS(MAXCON)
      DIMENSION SW_MASS(MAXCON)
C
C==== Variable Declarations ==================================================
C
      CHARACTER*18 IVS(MAXCON),CASN2(MAXCON)
      CHARACTER*9 SOLMET(MAXCON)
      REAL*4 TIMAX
C
C==== DATA Statement =========================================================
C
C     None
C
C*****************************************************************************
C     Set the solution method array (SOLMET)
C*****************************************************************************
C
      DO M1 = 1,NUMCON
        IF (RATIO(M1).GE.RRATIO.AND.MED(KMED).LT.5) THEN
          SOLMET(M1) = 'INSTANT  '
        ELSE IF (MED(KMED).LT.5) THEN
          SOLMET(M1) = 'NUMERICAL'
        ELSE
          SOLMET(M1) = 'N/A      '
        ENDIF
      ENDDO
C*****************************************************************************
C     NEED TO REINITIALIZE CONCMAX AND CONCTIM FOR EACH RECEPTOR
C*****************************************************************************
      KKFLAG = 0
      DO 777 NN = 1,ICNT
        CONCMAX(NN) = 0.0
        CONCTIM(NN) = 0.0
        TIMAX(NN) = 0.0
777   CONTINUE     
      IF (NPRINT .GT. 1) GO TO 10
      IF (MED(KMED).EQ.2.OR.MED(KMED).EQ.3.OR.MED(KMED).EQ.5) THEN
        WRITE(9,2)
      ELSE
        WRITE(9,1)
      ENDIF
1     FORMAT (T6,'#',T9,'TIME',T15,'NAME',T25,'MEDIUM',T35,
     1        'TOTAL',T43,'FLUX/UNITS',T56,'UNITS'/T9,'INC.',T26,
     2        'TIME',T36,'TIME',T44,'(G/YR) OR'/T26,'(YR)',T36,
     3        '(YR)',T45,'(pCI/YR)'/)
2     FORMAT (T6,'#',T9,'TIME',T15,'NAME',T25,'MEDIUM',T35,
     1        'TOTAL',T43,'CONC/UNITS',T56,'UNITS'/T9,'INC.',T26,
     2        'TIME',T36,'TIME',T44,'(G/ML) OR'/T26,'(YR)',T36,
     3        '(YR)',T45,'(pCI/ML)'/)
C
   10 M3=0
C*****************************************************************************
C     NOTE THAT M1 IS THE PARENT COUNTER AND M3 IS THE DAUGHTER COUNTER.
C                           JWB 3-26-89
C*****************************************************************************
      IF( ISOURC .EQ. 4 ) THEN
        DO M1=1,NUMCON
          TRAVEL(M1)=TRAVEL(M1)+STRT(M1+1)
          DO II=1,NTIMES
            IF(MM.EQ.1)TIME(II,M1)=TIM(II,M1)-STRT(M1)
C*****************************************************************************
C     TOTTIM IS THE ACTUAL TIME AT THE RECEPTOR;(MEDIUM TIME + TRAVEL TIME)
C*****************************************************************************
            TOTTIM(II,M1)=TIME(II,M1)+TRAVEL(M1)
C*****************************************************************************
C     CHECKING FOR MAXIMUM CONCENTRATION AND THE ASSOCIATED TIME FOR EACH 
C     CONSTITUENT AT THE RECEPTOR   JWB 1-26-1989
C*****************************************************************************
            concmax(m1) = co(m1)/UNITS
            conctim(m1) = 0.0
CKJC            CS(II,M1) = CS(II,M1) / UNITS
            WRITE(9,996) M1,II,IVS(M1),TIM(II,M1),TOTTIM(II,M1),
     1                   CS(II,M1),UNITS
          END DO
        END DO
C
C*****************************************************************************
C     Time Specific Modification - write out the dummy CS and TIM
C     arrays if this modification is invoked, else write out the regular
C     arrays.  JPM /GW  04/05/94
C*****************************************************************************
C 
      ELSE IF ((NTIMSPEC.GT.0).AND.(MED(KMED).EQ.3)) THEN
        DO M1=1,NUMCON
          TRAVEL(M1)=TRAVEL(M1)+STRT(M3+1)
          DO MM=1,NDS(M1)
            M3=M3+1
            DO II=1,IKOUNT
              IF(MM.EQ.1)TIME(II,M1)=CS_TIME(II,M3)-STRT(M3)
C*****************************************************************************
C     TOTTIM IS THE ACTUAL TIME AT THE RECEPTOR;(MEDIUM TIME + TRAVEL TIME)
C*****************************************************************************
              TOTTIM(II,M1)=TIME(II,M1)+TRAVEL(M1)
C*****************************************************************************
C     CHECKING FOR MAXIMUM CONCENTRATION AND THE ASSOCIATED TIME FOR EACH 
C     CONSTITUENT AT THE RECEPTOR   JWB 1-26-1989
C*****************************************************************************
              IF (KMED .EQ. NMED ) THEN
                IF( TOTTIM(II,M1) .LE. TFINAL ) THEN
                  if( concmax(m3) .lt. CS_SPEC(ii,m3) ) then
                    concmax(m3) = CS_SPEC(ii,m3)
                    conctim(m3) = tottim(ii,m1)
                  endif
                ENDIF
              ENDIF
              WRITE(9,896) M3,II,IVS(M3),CS_TIME(II,M3),TOTTIM(II,M1),
     1                     CS_SPEC(II,M3),UNITS
896           FORMAT (2X,2I5,2X,A18,1P9E10.3)
            END DO
          END DO
        END DO
      ELSE
C*****************************************************************************
C     WRITE HEADER FOR MASS TABLE
C*****************************************************************************
        IF (MED(KMED).EQ.7) THEN
          WRITE (6,509)
        ELSE IF (MED(KMED).EQ.1.OR.MED(KMED).EQ.4.OR.MED(KMED).EQ.6)
     +  THEN
          WRITE (6,489)
        ELSE IF (MED(KMED).EQ.2.OR.MED(KMED).EQ.3) THEN
          WRITE (6,505)
        ENDIF
489     FORMAT (//,T43,'Mass Released',T63,'Mass Passed',/,T33,
     +  'Solution',T44,'From Source',T63,'This Medium',/,T4,'Name',T13,
     +  'CASID',T22,'RATIO(a)',T34,'Method',T45,'(g or pCi)',T64,
     +  '(g or pCi)',/,T2,8('-'),T12,8('-'),T22,9('-'),T33,8('-'),
     +  T43,13('-'),T58,20('-'),/)
505     FORMAT (//,T43,'Mass Released',T58,/,T33,
     +  'Solution',T44,'From Source',T58,/,T4,'Name',T13,
     +  'CASID',T22,'RATIO(a)',T34,'Method',T45,'(g or pCi)',/,
     +  T2,8('-'),T12,8('-'),T22,9('-'),T33,8('-'),T43,13('-'),/)
509     FORMAT (//T22,'Mass Released',T38,'Mass Through Wetland Path',
     +  'ways (g or pCi):',/,T23,'From Source',T38,'Atmo-',T46,'Ground',
     +  T55,'Surface',/,T4,'Name',T14,'CASID',T24,'(g or pCi)',T38,
     +  'sphere',T47,'Water',T56,'Water',T70,'Total',/,T2,8('-'),
     +  T12,8('-'),T22,13('-'),T37,7('-'),T46,7('-'),T55,7('-'),
     +  T64,16('-'))
        DO M1=1,NUMCON
          TOTMAS(M1) = 0.0
          TRAVEL(M1)=TRAVEL(M1)+STRT(M3+1)
          DO MM=1,NDS(M1)
            M3=M3+1
            DO II=1,NTIMES+1
              IF(MM.EQ.1)TIME(II,M1)=TIM(II,M3)-STRT(M3)
C*****************************************************************************
C     TOTTIM IS THE ACTUAL TIME AT THE RECEPTOR;(MEDIUM TIME + TRAVEL TIME)
C*****************************************************************************
              TOTTIM(II,M1)=TIME(II,M1)+TRAVEL(M1)
C*****************************************************************************
C     CHECKING FOR MAXIMUM CONCENTRATION AND THE ASSOCIATED TIME FOR EACH 
C     CONSTITUENT AT THE RECEPTOR   JWB 1-26-1989
C*****************************************************************************
              IF (KMED .EQ. NMED ) THEN
                IF( TOTTIM(II,M1) .LE. TFINAL ) THEN
                  if( concmax(m3) .lt. cs(ii,m3) ) then
                    concmax(m3) = cs(ii,m3)
                    conctim(m3) = tottim(ii,m1)
                  endif
                ENDIF
              ENDIF
              IF (MED(KMED).EQ.2.OR.MED(KMED).EQ.3.OR.MED(KMED).EQ.5)
     +        THEN
                IF (MM.EQ.1.AND.II.EQ.NTIMES+1.AND.MED(NMED).NE.5) THEN
                  WRITE (6,285) IV(M1,MM),CASN1(M1,MM),RATIO(M1),
     +            SOLMET(M1),SMASS(M1)
                ENDIF
285   FORMAT (T2,A18,T12,A18,T21,1P,1E10.3,T33,A9,T45,1E10.3)
C*****************************************************************************
C     SUM THE TOTAL MASS PASSED EACH MEDIUM FOR EACH PARENT CONSITIUENT
C     IF THE CURRENT MEDIUM IS NOT THE LAST MEDIUM
C*****************************************************************************
              ELSE IF ((II.NE.1).AND.(MM.EQ.1)) THEN
                TOTMAS(M1) = TOTMAS(M1) + 0.5 * (TOTTIM(II,M1) - 
     +          TOTTIM(II-1,M1)) * (CS(II,M3) + CS(II-1,M3)) * UNITS
                IF (II.EQ.NTIMES+1) THEN
                   IF (MED(KMED).EQ.7.AND.AL(M1,1).LT.1.0E-17.AND.
     +               SMASS(M1).GT.0.0) THEN
                     WRITE (6,514) IV(M1,MM),CASN1(M1,MM),SMASS(M1),
     +               VOL_MAS(M1),GW_MASS(M1),SW_MASS(M1),
     +               VOL_MAS(M1)+GW_MASS(M1)+SW_MASS(M1),
     +               (VOL_MAS(M1)+GW_MASS(M1)+SW_MASS(M1))/SMASS(M1)*100
                   ELSE IF (MED(KMED).EQ.7.AND.AL(M1,1).LT.1.0E-17) THEN
                     WRITE (6,514) IV(M1,MM),CASN1(M1,MM),SMASS(M1),
     +               VOL_MAS(M1),GW_MASS(M1),SW_MASS(M1),
     +               VOL_MAS(M1)+GW_MASS(M1)+SW_MASS(M1),-99.9
                   ELSE IF (MED(KMED).EQ.7.AND.AL(M1,1).GE.1.0E-17) THEN
                     WRITE (6,519) IV(M1,MM),CASN1(M1,MM),SMASS(M1),
     +               VOL_MAS(M1),GW_MASS(M1),SW_MASS(M1),
     +               VOL_MAS(M1)+GW_MASS(M1)+SW_MASS(M1)
                     KKFLAG = 1
                   ELSE IF (MED(KMED).EQ.6.AND.AL(M1,1).LT.1.0E-17.AND.
     +             SMASS(M1).GT.0.0) THEN
                     WRITE (6,829) IV(M1,MM),CASN1(M1,MM),RATIO(M1),
     +               SOLMET(M1),SMASS(M1),TOTMAS(M1),
     +               TOTMAS(M1)/SMASS(M1)*100
                   ELSE IF (MED(KMED).EQ.6.AND.AL(M1,1).LT.1.0E-17) THEN
                     WRITE (6,829) IV(M1,MM),CASN1(M1,MM),RATIO(M1),
     +               SOLMET(M1),SMASS(M1),TOTMAS(M1),-99.9
                   ELSE IF (MED(KMED).EQ.6.AND.AL(M1,1).GE.1.0E-17) THEN
                     WRITE (6,830) IV(M1,MM),CASN1(M1,MM),RATIO(M1),
     +               SOLMET(M1),SMASS(M1),TOTMAS(M1)
                     KKFLAG = 1
                   ELSE IF (AL(M1,1).LT.1.0E-17.AND.SMASS(M1).GT.0.0)
     +             THEN
                     WRITE (6,829) IV(M1,MM),CASN1(M1,MM),RATIO(M1),
     +               SOLMET(M1),SMASS(M1),TOTMAS(M1),
     +               TOTMAS(M1)/SMASS(M1)*100
                   ELSE IF (AL(M1,1).LT.1.0E-17)
     +             THEN
                     WRITE (6,829) IV(M1,MM),CASN1(M1,MM),RATIO(M1),
     +               SOLMET(M1),SMASS(M1),TOTMAS(M1),-99.9
                   ELSE
                     WRITE (6,830) IV(M1,MM),CASN1(M1,MM),RATIO(M1),
     +               SOLMET(M1),SMASS(M1),TOTMAS(M1)
                     KKFLAG = 1
                   ENDIF
                   IF (M1.EQ.NUMCON.AND.KKFLAG.EQ.1) WRITE (6,831)
                ENDIF
514             FORMAT(T2,A18,T12,A18,1P,T23,E10.3,T37,E7.1,3E9.1,
     +          ' (',0PF5.1,'%)')
519             FORMAT(T2,A18,T12,A18,1P,T23,E10.3,T37,E7.1,3E9.1,
     +          T72,'(  **  )')
829             FORMAT(T2,A18,T12,A18,T21,1PE10.3,T33,A9,T45,1E10.3,
     +          T59,1E10.3,T70,'(',0PF5.1,'%)')
830             FORMAT(T2,A18,T12,A18,T21,1PE10.3,T33,A9,T45,1E10.3,
     +          T59,1E10.3,T70,'(  **  )')
831             FORMAT(/,' **  Decaying constituent')
              ENDIF
              IF (M1.EQ.NUMCON.AND.MM.EQ.1.AND.II.EQ.NTIMES+1) THEN
                IF (MED(KMED).LT.5) THEN
                  WRITE (6,854) RRATIO
                ENDIF
              ENDIF
C
C*****************************************************************************
C     When KMED does not equal NMED, only contaminant fluxes were
C     computed and only for parent constituents.  Thus, write out fluxes
C     to the *.POL file only for the parent constituents when KMED <> NMED.
C     A parent constituent is indicated when MM = 1.  In the last medium,
C     i.e., KMED = NMED, then concentrations are computed for all
C     constituents (parents and decay products).  Thus, write records to the
C     *.POL file for all constituents.  JPM  02/10/95.
C*****************************************************************************
C
              IF (NMED.EQ.KMED.OR.MM.EQ.1) THEN
                 WRITE(9,996) M3,II,IVS(M3),TIM(II,M3),TOTTIM(II,M1),
     1                     CS(II,M3),UNITS
              ENDIF
854             FORMAT(/,T2,'(a) RRATIO=',1PE10.3,'.  RATIO is the ',
     +          'estimated time to peak divided by the',/,T6,
     +          'duration of time over which the contaminant enters ',
     +          'this medium.  When',/,T6,'RATIO >= RRATIO, the ',
     +          'equation for an instantaneous release is used,',
     +          /,T6,'otherwise the numerical solution for a time ',
     +          'varying release is used.'//)
996           FORMAT (2X,2I5,2X,A18,1P9E10.3)
            END DO
          END DO
        END DO
      END IF
      IF( KMED .EQ. NMED ) THEN
        WRITE(9,'(/,'' END OF RECEPTOR '',I2,'' RUN'',//)')JSCE
      ELSE
        WRITE(9,'(//)')
      ENDIF
C
        M3=0
      DO 998 M1=1,NUMCON
        NUM(M1)=NTIMES
C
C*****************************************************************************
C     Reset TLIF for each contaminant.  TLIF is the duration of time in
C     which the contaminant enters the next medium.  It is used in
C     determining whether the instantaneous solution is utilized. JPM 4/18/94
C*****************************************************************************
C
        TLIF(M1) = TEND2(M1) - TSTRT2(M1)
C
C*****************************************************************************
C     CSMAX IS THE MAXIMUM CONCENTRATIONS FOR THE GIVE CONSTITUENT.
C     TIMAX IS THE MAXIMUM TIME FOR GIVEN CONSTITUENT.  JWB 1-26-1989
C
C                      CHECK FOR MAXIMUM TIME
C     IF TIMAX IS <OR= TO 0.0, A WARNING MESSAGE IS WRITTEN OUT
C*****************************************************************************
C
        DO 998 MM=1,NDS(M1)
        M3=M3+1
        DO 997 II=1,NTIMES+1
          IF( KMED .EQ. NMED ) THEN
            TIMER = TOTTIM(II,M1) - TDIFF(M1)
            IF( TIMAX(M1) .LT. TIMER ) TIMAX(M1) = TIMER          
          ENDIF
          IF(MM.EQ.1)THEN
            PNT(II,M1)=CS(II,M3)
            IF (KMED .NE. NMED) PNT(II,M1)=PNT(II,M1)*UNITS
          ENDIF
997     CONTINUE
998   CONTINUE
      RETURN
      END
