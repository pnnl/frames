C     MEPAS RADCON: MAXTABLE.FOR            Version Date: 06-09-1998
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                          SUBROUTINE MAXTABLE                               *
C                                                                            *
C  Subroutine MAXTABLE writes out the maximum constituent concentration      *
C  table to the bottom of the *.WLS file.                                    *
C                                                                            *
C                                                                            *
C  Written by:       John P. McDonald                                        *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    08/25/94 - JPM                                          *
C  Last Modified:    06/09/98 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of:  RADCON
C     Called by:  ORIGIN and MULREC
C     Calls to: None
C     Common blocks referenced:  MAXIMUM.FOR
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- ------ ------  ------------- ---------------------------------
C     CASN1     USED   CHAR    ARG - IMPORT   CASID'S
C     CMAX      SET    REAL    LOCAL          A TEMPORARY HOLDING VARIABLE
C                                             FOR CONCMAX*UNITS
C     CO        USED   REAL    ARG - IMPORT   INITIAL DISSOLVED CONCENTRATION
C                                             AT THE SOURCE FOR THE CURRENT
C                                             PARENT
C     CONCMAX   USED   REAL    ARG - IMPORT   MAX CONCENTRATION; ALSO CMAX    
C     CONCMIN   SET    REAL    LOCAL          CONCENTRATION THAT IS A SMALL
C                                             FRACTION LESS THAN THE MAXIMUM
C                                             CONCENTRATION - LOWER LIMIT FOR
C                                             THE STEADY STATE CONDITION
C     CONCTIM   SET    REAL    ARG - EXPORT   TIME OF MAX CONCENTRATION, ALSO 
C                                             KNOWN AS CTMAX
C     CS        USED   REAL    ARG - IMPORT   CONCENTRATION OF CONTAMINANT    
C     CTYPE     SET    CHAR    LOCAL          HOLDS THE CURVE TYPE INDICATOR
C     I         SET    INT     LOCAL          INDEX OF THE TIME ARRAY FOR THE
C                                             MAXIMUM CONCENTRATION
C     IBEGIN    SET    INT     LOCAL          THE INDEX OF THE TIME ARRAY IN
C                                             WHICH A STEADY STATE CONDITION
C                                             IS THOUGHT TO START
C     IEND      SET    INT     LOCAL          THE INDEX OF THE TIME ARRAY IN
C                                             WHICH A STEADY STATE CONDITION
C                                             IS THOUGHT TO END
C     IPEAK     SET    INT     COMMON         FLAG TO INDICATE IF THE FOOTNOTE
C                                             FOR PEAK CONCENTRATIONS SHOULD
C                                             BE WRITTEN (INDEX8.WTN)
C     ISOURC    USED   INT     ARG - IMPORT   INDEX FOR THE BOUNDARY CONDITION
C                                             OF THE RELEASE
C     ISS       SET    INT     COMMON         FLAG TO INDICATE IF THE FOOTNOTE
C                                             FOR STEADY STATE CONCENTRATIONS
C                                             SHOULD BE WRITTEN (INDEX8.WTN)
C     ITRUNC    SET    INT     COMMON         FLAG TO INDICATE IF THE FOOTNOTE
C                                             FOR TRUNCATED CONCENTRATIONS
C                                             SHOULD BE WRITTEN (INDEX8.WTN)
C     IVS       USED   CHAR    ARG - IMPORT   CONSTITUENT NAMES
C     IZERO     SET    INT     COMMON         FLAG TO INDICATE IF THE FOOTNOTE
C                                             FOR ZERO CONCENTRATIONS SHOULD
C                                             BE WRITTEN (INDEX8.WTN)
C     M1        USED   INT     ARG - IMPORT   INDEX ON THE PARENT CONSTITUENTS
C     M3        USED   INT     ARG - IMPORT   INDEX ON ALL CONSTITUENTS,
C                                             INCLUDING DAUGHTERS
C     MED       USED   INT     ARG - IMPORT   RECEPTOR MEDIUM TYPE
C     MM        USED   INT     ARG - IMPORT   INDEX ON DECAY CHAIN SPECIES
C                                             FOR A GIVEN PARENT
C     NDS       USED   INT     ARG - IMPORT   TOTAL NUMBER OF SPECIES IN THE
C                                             DECAY CHAIN FOR THIS PARENT
C     NFLAG     USED   INT     ARG - IMPORT   INDICATES IF THE CURVE HAS BEEN
C                                             TRUNCATED DUE TO EXCEEDING THE
C                                             INITIAL CONCENTRATION AT THE
C                                             SOURCE
C     NTIMES    USED   INT     ARG - IMPORT   NUMBER OF TIME STEPS
C     NUMCON    USED   INT     ARG - IMPORT   TOTAL NUMBER OF PARENTS IN THIS
C                                             SIMULATION
C     SOLLIM    USED   REAL    ARG - IMPORT   SOLUBILITY LIMIT FOR THE CURRENT
C                                             PARENT
C     TDIFF     USED   REAL    ARG - IMPORT   THE START DATE FOR THE RISK
C                                             CALCULATIONS MINUS THE START
C                                             DATE FOR THE RELEASE OF A
C                                             PARTICULAR CONTAMINANT
C     TIME      USED   REAL    ARG - IMPORT   TIME IN THE CURRENT MEDIUM
C     TRAVEL    USED   REAL    ARG - IMPORT   THE AMOUNT OF OFFSET BETWEEN THE
C                                             MEDIUM TIME AND THE TOTAL TIME
C                                             FOR THE CURRENT MEDIUM
C     UNITS     USED   REAL    ARG - IMPORT   CONVERSION FACTOR FOR
C                                             CONCENTRATIONS
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     08/31/94     JPM  Accounting for a new curve type - zero concentrations
C     09/15/94     JPM  Removed references to "daughters" in the output, and
C                       replaced with "decay products".
C     07/28/95     JPM  Writing the warning messages to the *.WRN file in
C                       addition to the *.WLS file.
C     10/18/96     JPM  Made changes allowing for fluxes to be computed in the
C                       last medium.  Also allowing MED=2 to be vadose zone
C                       concentrations (PH1-E001).
C     11/11/96     JPM  Changed units from Ci to pCi (PH1-E006).
C     04/16/97     JPM  Made provisions for concentration computations in a
C                       wetland (PH1-P005).
C     04/28/97     JPM  Made changes so this routine will work for permuted
C                       decay products (PH1-E014).
C     06/09/98     JPM  Making IVS and CASN1 character variable types
C                       (PH1-E016).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE MAXTABLE (CONCTIM,CONCMAX,CS,TIM,TRAVEL,M1,M3,NFLAG,
     +                     NTIMES,IVS,TDIFF,NUMCON,CASN1,SOLLIM,CO,
     +                     UNITS,MED,ISOURC)
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'INDEX8.WTN'
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION CS(MAXNTI,MAXCON),TIM(MAXNTI,MAXCON)
C
C==== Variable Declarations ==================================================
C
      CHARACTER*18 IVS,CASN1
      CHARACTER*5 CTYPE
C
C*****************************************************************************
C     Write column headings for the maximum constituent concentration table
C     if this is the first constituent
C*****************************************************************************
C
      IF (M1.EQ.1) THEN
        WRITE (6,899)
        IF (MED.EQ.2.OR.MED.EQ.3.OR.MED.EQ.5.OR.MED.EQ.7) THEN
          WRITE (6,900)
          WRITE (6,901)
        ELSE
          WRITE (6,912)
          WRITE (6,913)
        ENDIF
      ENDIF
899   FORMAT(/1X,78('*'))
900   FORMAT(' Times are given as years since the start of risk ',
     +'calculations (WS-TRISK)'//,5X,'Maximum Constituent ',
     +'Concentrations:'//)
901   FORMAT (T42,'Maximum',/,T39,'Concentration',/,T8,'Name',T19,
     +'CASID',T28,'Type(a)',T38,'(g/mL or pCi/mL)',T60,'Time (Years)',/,
     +T6,8('-'),T18,8('-'),T28,7('-'),T38,15('-'),T56,20('-'),/)
912   FORMAT(' Times are given as years since the start of risk ',
     +'calculations (WS-TRISK)'//,5X,'Maximum Constituent ',
     +'Fluxes:'//)
913   FORMAT (T42,'Maximum',/,T44,'Flux',/,T8,'Name',T19,
     +'CASID',T28,'Type(a)',T38,'(g/yr or pCi/yr)',T60,'Time (Years)',/,
     +T6,8('-'),T18,8('-'),T28,7('-'),T38,15('-'),T56,20('-'),/)
C
C*****************************************************************************
C     If concentrations at the receptor exceed concentrations at the source,
C     then write a message to the table.
C*****************************************************************************
C
      IF(NFLAG.GT.0) THEN
        IF (MED.EQ.2.OR.MED.EQ.3.OR.MED.EQ.5.OR.MED.EQ.7) THEN
          WRITE(6,902) IVS,CO*UNITS
          WRITE(11,902) IVS,CO*UNITS
        ELSE
          WRITE(6,914) IVS
          WRITE(11,914) IVS
        ENDIF
      ENDIF
902   FORMAT(/5x,A18,'      ----- IMPORTANT NOTE FOR THIS RUN -------'
     +/10x,'The computed maximum concentration is greater than the'
     +/10x,'designated initial concentration at the source of'
     +/10x,1P,E10.3,'(g/ml or pCi/ml), after accounting for decay.'
     +/10x,'The current receptor is located too close to the source,'
     +/10x,'creating a near-field condition which cannot be properly'
     +/10x,'assessed by a flux boundary condition model.')
914   FORMAT(/5x,A18,'      ----- IMPORTANT NOTE FOR THIS RUN -------'
     +/10x,'The computed maximum flux is greater than the designated'
     +/10x,'maximum flux from the source, after accounting for decay.')
C
C*****************************************************************************
C     Find the time of the maximum concentration in the TIM array
C*****************************************************************************
C
      I = 1
      DO WHILE (I.LT.NTIMES+1.AND.CONCTIM.GT.TIM(I,M3))
        I = I + 1
      END DO
C
C*****************************************************************************
C     Compute the concentration that is one ten-thousandth less than the
C     maximum concentration
C*****************************************************************************
C
      CONCMIN = CONCMAX * .9999
C
C*****************************************************************************
C     Find the lower limit in the CS array where concentrations lie in the
C     interval from CONCMIN to CONCMAX (that is, find the starting point of
C     the interval in which concentrations deviate by less than or equal to
C     one ten-thousandth of the maximum concentration)
C*****************************************************************************
C
      IBEGIN = I
      DO WHILE (IBEGIN.GT.1.AND.CS(IBEGIN,M3).GE.CONCMIN)
        IBEGIN = IBEGIN - 1
      END DO
C
C*****************************************************************************
C     If the above loop ended because a concentration less than CONCMIN was
C     found, then add one to IBEGIN so IBEGIN refers to the first
C     concentration greater than or equal to CONCMIN.  If the loop ended
C     only because IBEGIN refers to the first concentration in the CS array
C     (IBEGIN = 1), then leave IBEGIN unaltered.
C*****************************************************************************
C
      IF (CS(IBEGIN,M3).LT.CONCMIN) IBEGIN = IBEGIN + 1
C
C*****************************************************************************
C     Find the upper limit in the CS array where concentrations lie in the
C     interval from CONCMIN to CONCMAX (that is, find the ending point of
C     the interval in which concentrations deviate by less than or equal to
C     one ten-thousandth of the maximum concentration)
C*****************************************************************************
C
      IEND = I
      DO WHILE (IEND.LT.NTIMES+1.AND.CS(IEND,M3).GE.CONCMIN)
        IEND = IEND + 1
      END DO
C
C*****************************************************************************
C     If the above loop ended because a concentration less than CONCMIN was
C     found, then subtract one from IEND so IEND refers to the last
C     concentration greater than or equal to CONCMIN.  If the loop ended
C     only because IEND refers to the last concentration in the CS array
C     (IEND = NTIMES+1), then leave IEND unaltered.
C*****************************************************************************
C
      IF (CS(IEND,M3).LT.CONCMIN) IEND = IEND - 1
C
C*****************************************************************************
C     If ISOURC = 4, then this is a known concentration case, and the curve
C     type should be MEAS.  If MED = 3 and NFLAG > 0, then we have a receptor
C     at a well with a truncated curve, and the curve type is TRUNC.
C     If IEND - IBEGIN >= 2, then there are at least three points within
C     one ten-thousandth of the maximum concentration, including the peak
C     itself.  When this is the case, write the steady state time interval
C     to the table.  Else, the curve must have a peak concentration or a small
C     enough steady state interval to consider it a peak concentration.
C     But, if the maximum concentration is zero, the curve type is zero.
C     Thus, we cannot have a steady state condition if there is no
C     contaminant at the well.
C*****************************************************************************
C
      CMAX = CONCMAX*UNITS
      IF (ISOURC.EQ.4) THEN
        CTYPE = 'MEAS '
        IMEAS = 1
        WRITE (6,903) IVS,CASN1,CTYPE,CMAX,CONCTIM-TDIFF
      ELSE IF (NFLAG.GT.0) THEN
        CTYPE = 'TRUNC'
        ITRUNC = 1
        WRITE (6,903) IVS,CASN1,CTYPE,CMAX,CONCTIM-TDIFF
      ELSE IF (IEND-IBEGIN.LE.1.AND.CMAX.GT.0.0) THEN
        CTYPE = 'PEAK '
        IPEAK = 1
        WRITE (6,903) IVS,CASN1,CTYPE,CMAX,CONCTIM-TDIFF
      ELSE IF (IEND-IBEGIN.GE.2.AND.CMAX.GT.0.0) THEN
        CTYPE = 'SS   '
        ISS = 1
        WRITE (6,904) IVS,CASN1,CTYPE,CMAX,
     +        TIM(IBEGIN,M3)-TDIFF,TIM(IEND,M3)-TDIFF
      ELSE
        CTYPE = 'ZERO '
        IZERO = 1
        WRITE (6,904) IVS,CASN1,CTYPE,0.0,TIM(1,M3)-TDIFF,
     +  TIM(NTIMES+1,M3)-TDIFF
      ENDIF
903   FORMAT(T6,A18,T18,A18,T29,A5,T40,1P,1E10.3,T56,0P,1F8.1)
904   FORMAT(T6,A18,T18,A18,T29,A5,T40,1P,1E10.3,T56,0P,1F8.1,' to ',
     +1F8.1)
C
C******************************************************************************
C     Write a warning message if the maximum concentration exceeds the
C     solubility limit
C******************************************************************************
C
      IF ((MED.EQ.2.OR.MED.EQ.3.OR.MED.EQ.5.OR.MED.EQ.7)
     +.AND.CMAX.GT.SOLLIM) THEN
        WRITE (6,905) IVS,SOLLIM
        WRITE (11,905) IVS,SOLLIM
      ENDIF
905   FORMAT (/2x,'WARNING! --> ',A18,': The computed maximum ',
     1'concentration EXCEEDS',
     2 /15x,'the solubility limit of ',1PE10.3,' (g/mL).'/)
C
C*****************************************************************************
C     Write a footnote to the maximum constituent concentration table after
C     the last constituent
C*****************************************************************************
C
      IF(M1.EQ.NUMCON.AND.(MED.EQ.2.OR.MED.EQ.3.OR.MED.EQ.5.OR.
     +MED.EQ.7)) THEN
        WRITE (6,906)
        IF (IMEAS.EQ.1) WRITE (6,907)
        IF (IPEAK.EQ.1) WRITE (6,908)
        IF (ISS.EQ.1) WRITE (6,909)
        IF (ITRUNC.EQ.1) WRITE (6,910)
        IF (IZERO.EQ.1) WRITE (6,911)
        WRITE (6,899)
      ELSE IF (M1.EQ.NUMCON) THEN
        WRITE (6,906)
        IF (IMEAS.EQ.1) WRITE (6,915)
        IF (IPEAK.EQ.1) WRITE (6,916)
        IF (ISS.EQ.1) WRITE (6,917)
        IF (ITRUNC.EQ.1) WRITE (6,918)
        IF (IZERO.EQ.1) WRITE (6,919)
        WRITE (6,899)
      ENDIF
906   FORMAT(/,T6,'(a)')
907   FORMAT(T10,'MEAS: The maximum concentration is based on a ',
     +'measured concentration.'/)
908   FORMAT(T10,'PEAK: The maximum concentration occurs as a ',
     +'peak concentration',/,T16,'at the given time.',/)
909   FORMAT(T12,'SS: Concentrations vary by less than 1/100th of one ',
     +'percent',/,T16,'of the maximum concentration over the given ',
     +'time range',/,T16,'(steady state).'/)
910   FORMAT(T9,'TRUNC: For parent constituents, concentrations at a ',
     +'well have been',/,T16,'truncated to the initial dissolved ',
     +'concentration (accounting',/,T16,'for decay if necessary) ',
     +'due to near-field conditions.  For',/,T16,'decay ',
     +'products, concentrations are based on a truncated parent.',/)
911   FORMAT(T10,'ZERO: Concentrations are zero over the given time',
     +' range.',/)
915   FORMAT(T10,'MEAS: The maximum flux is based on a measured flux.'/)
916   FORMAT(T10,'PEAK: The maximum flux occurs as a peak flux',
     +' at the given time.',/)
917   FORMAT(T12,'SS: Fluxes vary by less than 1/100th of one percent',
     +' of the',/,T16,'maximum flux over the given time range (steady',
     +' state).'/)
918   FORMAT(T9,'TRUNC: For parent constituents, fluxes have been ',
     +'truncated to',/,T16,'the designated maximum flux from the ',
     +'source, accounting',/,T16,'for decay if necessary.  For decay ',
     +'products, fluxes are',/,T16,'based on a truncated parent.',/)
919   FORMAT(T10,'ZERO: Receptor fluxes are zero over the given time',
     +' range.',/)
      RETURN
      END
