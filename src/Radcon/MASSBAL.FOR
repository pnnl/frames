C     MEPAS RADCON: MASSBAL.FOR             Version Date: 11-11-1996
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                          SUBROUTINE MASSBAL                                *
C                                                                            *
C  This routine computes the mass remaining at the source and the product of *
C  particulate concentration and total volume at the source for each         *
C  consitiuent at specific times listed in the *.WIN file.  The latter will  *
C  yield the particulate concentration at the source when divided by the     *
C  total volume at the source.  Output from this routine is written to the   *
C  *.MAS file.  This routine is called from the main program.                *
C                                                                            *
C                                                                            *
C  Written by:       John P. McDonald                                        *
C                    Battelle Pacific Northwest Laboratories                 *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    05/13/94                                                *
C  Last Modified:    11/11/96 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: RADCOND
C     Calls to: INTERP
C     Common blocks referenced: CTIME, DUMMY, INDEX1, INDEX2, INDEX3, INDEX9,
C                               INIT, INTER, MASSS, MAXIMUM
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter   Set/           Location
C     Name        Used   Type    Export/Import  Parameter Description
C     ---------  ------  ------  -------------  ------------------------------
C     AL          USED   REAL    COMMON         HALF-LIFE ARRAY
C     ALAMBDA     SET    REAL    LOCAL          DECAY CONSTANT ARRAY
C     ALAM_TIM    S/U    REAL    LOCAL          DEGRADATION/DECAY COEFFICIENT 
C                                               FOR EXPONENT IN DECAY EQUATION
C                                               (DECAY CONSTANT*TIME)
C     AN5         USED   REAL    ARG - IMPORT   MOISTURE CONTENT AT THE SOURCE
C     CDA         SET    REAL    LOCAL          MASS OF DAUGHTER
C     CINVEN      USED   REAL    COMMON         TOTAL BEGINNING MASS OF EACH
C                                               PARENT AT THE SOURCE
C     DECMAS      SET    REAL    LOCAL          DECAYED MASS REMAINING AT THE
C                                               SOURCE
C     EQCOEF      USED   REAL    COMMON         SURFACE EQUILIBRIUM COEFFICIENT
C     FLUX1       SET    REAL    LOCAL          FLUX FROM THE SOURCE AT THE
C                                               START OF THE CURRENT INTERVAL
C                                               FOR WHICH MASS IS BEING COMPUTED
C     FLUX2       SET    REAL    LOCAL          FLUX FROM THE SOURCE AT THE END
C                                               OF THE CURRENT INTERVAL FOR
C                                               WHICH MASS IS BEING COMPUTED
C     I           SET    INTEGER LOCAL          COUNTER ON THE NUMBER OF SOURCE
C                                               FLUX POINTS AND A LOOP INDEX
C                                               FOR THE DAUGHTER SPECIES IN
C                                               THE BATEMAN EQUATION
C     IDECAY      USED   INTEGER COMMON         INDEX FOR DETERMINING TYPE OF 
C                                               CONTAMINANT DECAY             
C                                                 = 1; NOT REPLENISHED, DECAY 
C                                                      AT SOURCE AND ENVIRON. 
C                                                 = 2; REPLENISHED AT SOURCE, 
C                                                      DECAY ONLY IN ENVIRON. 
C                                                 = 3; NOT REPLENISHED, DECAY 
C                                                      ONLY AT SOURCE         
C     NDS        USED   INTEGER COMMON         # OF DECAY-PRODUCTS FOR EACH   
C                                              CONTAMINANT INCLUDING PARENT   
C     NT         SET    INTEGER LOCAL          'NUMBER OF TIMES' - LOOP INDEX
C                                              ON THE SPECIFIC TIMES LOOP
C     NTIMSPEC   USED   INTEGER COMMON         THE NUMBER OF SPECIFIC TIMES
C     NUMCON     USED   INTEGER COMMON         NUMBER OF PARENT CONSITIUENTS
C     P1 & P2    USED   REAL    LOCAL          HOLDS INTERMEDIATE COMPUTATION
C                                              RESULTS FROM THE BATEMAN EQ.
C     PC_COEF    SET    REAL    LOCAL          PARTICULATE CONCENTRATION
C                                              COEFFICIENT - SEE COMMENT BELOW
C     PNT        USED   REAL    COMMON         SOURCE TERM FLUX ARRAY
C     R2         USED   REAL    ARG - IMPORT   BULK DENSITY AT THE SOURCE
C     RELMAS     SET    REAL    LOCAL          MASS RELEASED FROM THE SOURCE
C                                              FOR A PARTICULAR CONSTITUENT
C     REMMAS     SET    REAL    LOCAL          MASS REMAINING AT THE SOURCE
C                                              FOR A PARTICULAR CONSTITUENT -
C                                              UNDECAYED
C     S1         SET    REAL    LOCAL          HOLDS INTERMEDIATE COMPUTATION
C                                              RESULTS FROM THE BATEMAN EQ.
C     TIME       USED   REAL    COMMON         TIMES OF THE FLUXES IN THE PNT
C                                              ARRAY
C     TIME1      SET    REAL    LOCAL          TIME ASSOCIATED WITH FLUX1
C     TIME2      SET    REAL    LOCAL          TIME ASSOCIATED WITH FLUX2
C     TIMSPEC    USED   REAL    COMMON         SPECIFIC-TIMES ARRAY
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     05/23/94     JPM  Using another variable for the decayed mass remaining
C                       at the source.  Previously, REMMAS was decayed,
C                       written to the output file, and then undecayed for use
C                       in the Bateman equation.  However, the undecay caused
C                       overflow errors.
C     06/03/94     JPM  Opening and closing the *.MBS file from within this
C                       subroutine.  This makes sure the file is created only
C                       if records will be written to it.
C     11/11/96     JPM  Changed units from Ci to pCi (PH1-E006).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE MASSBAL (AN5,R2,FILENM)
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'CTIME.WTN'
      INCLUDE 'DUMMY.WTN'
      INCLUDE 'INDEX1.WTN'
      INCLUDE 'INDEX2.WTN'
      INCLUDE 'INDEX3.WTN'
      INCLUDE 'INDEX9.WTN'
      INCLUDE 'INIT.WTN'
      INCLUDE 'INTER.WTN'
      INCLUDE 'MASSS.WTN'
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION ALAMBDA(MAXCON,12)
C
C==== Variable Declaration ===================================================
C
      DOUBLE PRECISION S1,ALAM_TIM,P2
      CHARACTER*8 FILENM
C
C==== DATA Statements ========================================================
C
C     None
C
      OPEN(UNIT=10,FILE=FILENM//'.MBS',STATUS='UNKNOWN',IOSTAT=IOS)
C
C*****************************************************************************
C     Change the half lives to decay constants
C*****************************************************************************
C
      DO M1 = 1,NUMCON
         DO K = 1,NDS(M1)
            ALAMBDA(M1,K) = ALOG(2.)/AL(M1,K)
         END DO
      END DO
C
C*****************************************************************************
C     Write header to the *.MAS file and loop on specific times
C*****************************************************************************
C
      WRITE (10,900)
      DO 100, NT = 1,NTIMSPEC
C
C*****************************************************************************
C     Loop on the parent constituents
C*****************************************************************************
C
         DO 200, M1 = 1,NUMCON
C
C*****************************************************************************
C     Compute a particulate concentration coefficient at the source if the
C     necessary input information is provided.  If the necessary information
C     is not available, set the coefficient equal to -9.999E-09.  From Whelan
C     et al., 1987, equation 4.73:
C
C     P = Ct * Kd / (theta + beta * Kd)
C
C     Since Ct (total concentration) is the total mass divided by the total
C     volume:
C
C     P = (Mt / Vt) * Kd / (theta + beta * Kd)
C
C     P * Vt = Mt * Kd / (theta + beta * Kd)
C
C     The units of P * Vt are (g/g)*cm^3 or (pCi/g)*cm^3.
C
C     Since Mt is computed below, only the Kd / (theta + beta * Kd) term is
C     computed here.
C*****************************************************************************
C
            IF ((AN5.GE.0.).AND.(R2.GT.0.).AND.(EQCOEF(M1).GE.0.)) THEN
               PC_COEF = EQCOEF(M1) / (AN5 + R2 * EQCOEF(M1))
            ELSE
               PC_COEF = -9.999E-09
            ENDIF
C
C*****************************************************************************
C     Initialize variables
C*****************************************************************************
C
            RELMAS = 0.
            I = 1
C
C*****************************************************************************
C     Determine the mass remaining at the source for this parent.  Sum the
C     mass for each source term flux interval until the interval containing
C     the specific time is computed.  Then just sum the mass up to the
C     specific time.
C*****************************************************************************
C
            DO WHILE ((TIMSPEC(NT).GT.TIME(I,M1)).AND.(I.LT.NUM(M1)))
               FLUX1 = PNT(I,M1)
               TIME1 = TIME(I,M1)
               I = I + 1
               IF (TIMSPEC(NT).GT.TIME(I,M1)) THEN
                  FLUX2 = PNT(I,M1)
                  TIME2 = TIME(I,M1)
               ELSE
                  CALL INTERP (TIMSPEC(NT),FLUX2,M1)
                  TIME2 = TIMSPEC(NT)
               ENDIF
               RELMAS = RELMAS + (FLUX1 + FLUX2) / 2 * (TIME2 - TIME1)
            END DO
            REMMAS = CINVEN(M1) - RELMAS
            IF (REMMAS.LT.0.0) REMMAS = 0.0
C
C*****************************************************************************
C      If decay is occurring at the source, deplete the mass remaining for
C      the parent and write to the output file.  Then use the Bateman eq.
C      (Whelan et al., 1987, eq. 5.69) to compute the mass of the daughters.
C      If the specific time is zero, then skip the Bateman eq. and set the
C      mass of the daughters equal to zero.
C*****************************************************************************
C
            IF ((IDECAY.EQ.1).OR.(IDECAY.EQ.3)) THEN
               ALAM_TIM = ALAMBDA(M1,1) * TIMSPEC(NT)
               DECMAS = REMMAS * EXP(-ALAM_TIM)
               IF (PC_COEF.GE.0) THEN
                  WRITE (10,901) CASN1(M1,1),CASN1(M1,1),IV(M1,1),
     +                           TIMSPEC(NT),DECMAS,DECMAS*PC_COEF
               ELSE
                  WRITE (10,901) CASN1(M1,1),CASN1(M1,1),IV(M1,1),
     +                           TIMSPEC(NT),DECMAS,PC_COEF
               ENDIF
C              REMMAS = REMMAS * EXP(ALAM_TIM)
               DO 1 I=2,NDS(M1)
                  IF (TIMSPEC(NT) .GT. 0.) THEN
                       P1 = 1 
                       S1 = 0
                       I1 = I - 1
                       DO 2 J=1,I1
                          P1 = P1 * ALAMBDA(M1,J)
2                      CONTINUE
                       DO 3 J=1,I
                          P2 = 1
                          DO 4 K=1,I
                             IF( K .EQ. J) GO TO 4
                             P2 = P2 * (ALAMBDA(M1,K) - ALAMBDA(M1,J))
4                         CONTINUE
                          ALAM_TIM = ALAMBDA(M1,J) * TIMSPEC(NT)
                          S1 = S1 + DEXP(-ALAM_TIM) / P2
3                      CONTINUE
                       CDA = P1 * S1 * REMMAS * ALAMBDA(M1,I)
     +                       / ALAMBDA(M1,1)
                  ELSE
                       CDA = 0.0
                  ENDIF
                  IF (PC_COEF.GE.0) THEN
                     WRITE (10,901) CASN1(M1,1),CASN1(M1,I),IV(M1,I),
     +                              TIMSPEC(NT),CDA,CDA*PC_COEF
                  ELSE
                     WRITE (10,901) CASN1(M1,1),CASN1(M1,I),IV(M1,I),
     +                              TIMSPEC(NT),CDA,PC_COEF
                  ENDIF
1              CONTINUE
            ELSE
               IF (PC_COEF.GE.0) THEN
                  WRITE (10,901) CASN1(M1,1),CASN1(M1,1),IV(M1,1),
     +                           TIMSPEC(NT),REMMAS,REMMAS*PC_COEF
               ELSE
                  WRITE (10,901) CASN1(M1,1),CASN1(M1,1),IV(M1,1),
     +                           TIMSPEC(NT),REMMAS,PC_COEF
               ENDIF
            ENDIF
200      CONTINUE
100   CONTINUE
      CLOSE (UNIT=10)
900   FORMAT (T4,'PARENT',T16,'CASID',T27,'NAME',T39,'TIME',
     +        T50,'TOTAL MASS',T62,'PARTICULATE CONC',/,T4,'CASID',
     +        T38,'(YEARS)',T50,'AT SOURCE',T62,'* TOTAL VOLUME',/,
     +        T50,'(g or pCi)',T65,'AT SOURCE',/,
     +        T61,'(g/g or pCi/g)*cm^3',/)
901   FORMAT (T3,A18,T14,A18,T25,A18,T36,1PE10.3,T49,1PE10.3,
     +        T62,1PE10.3)
      RETURN
      END
