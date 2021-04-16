C     MEPAS RADCON: DECAY.FOR             Version Date: 08-23-2001
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                          SUBROUTINE DECAY                                  *
C                                                                            *
C  Subroutine DECAY calculates the degradation/decay product concentrations  *
C  from a known parent concentration based on half-life information.  The    *
C  calculation IS based on eq. 5.69 (Whelan et al. 1987).  The concentrations*
C  of the decay product are stored in DCA(I) until they are reassigned.      *
C  Decay-product concentrations are only calculated for the last media.  The *
C  subroutine is called from the main program and subroutine MEASUR and calls*
C  subroutine FUNCTION.                                                      *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    08/23/01 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCONPC
C     Called by: RADCONPC, MEASUR
C     Calls to: None
C     Common blocks referenced: INDEX3, INDEX4, CAP, MAXIMUM, ERROR
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter   Set/           Location
C     Name        Used   Type    Export/Import  Parameter Description
C     ---------  ------  ------  -------------  ------------------------------
C     AL          USED   REAL    ARG - IMPORT   CONTAMINANT HALF LIFE         
C     ARG         S/U    REAL    LOCAL          DEGRADATION/DECAY COEFFICIENT 
C                                               FOR EXPONENT IN DECAY EQUATION
C                                               HALF-LIFE*TIME                
C     C           S/U    REAL    ARG - IMPORT   ORIGINAL UNDECAY CONCENTRATION
C                                               OF PARENT (BACK CALCULATED)   
C     CO          USED   REAL    ARG - IMPORT   INITIAL PARENT CONCENTRATION
C     CDA         SET    REAL    ARG - EXPORT   CONCENTRATION OF DECAY-PRODUCT
C     DLAM        SET    DOUBLE  LOCAL          DOUBLE PRECISION DECAY
C                                               COEFFICIENTS
C     IDECAY      USED   INTEGER ARG - IMPORT   INDEX FOR DETERMINING TYPE OF 
C                                               CONTAMINANT DECAY             
C                                                 = 1; NOT REPLENISHED, DECAY 
C                                                      AT SOURCE AND ENVIRON. 
C                                                 = 2; REPLENISHED AT SOURCE, 
C                                                      DECAY ONLY IN ENVIRON. 
C                                                 = 3; NOT REPLENISHED, DECAY 
C                                                      ONLY AT SOURCE         
C     KK         USED   INTEGER COMMON - IMP   SAME AS INDEX K; COUNTER FOR   
C                                              MEDIA, KK = 1, NMED            
C     ND         USED   INTEGER ARG -IMPORT    # OF DECAY-PRODUCTS FOR EACH   
C                                              CONTAMINANT EXCLUDING PARENT   
C     NMED       USED   INTEGER COMMON - IMP   TOTAL # OF MEDIA IN SCENARIO   
C     T          USED   REAL    ARG - IMP      SAME AS T2; CURRENT MEDIA TIME 
C     TPK        USED   REAL    ARG - IMP      SAME AS TPKSUM; TIME TO PEAK   
C                                              CONCENTRATION                  
C     TTIM       SET    REAL    LOCAL          TOTAL TIME SINCE START; DEPENDS
C                                              ON VARIABLE IDECAY             
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     03/24/89     JWB  Modified decay calculations for better book-keeping
C     11/25/91     JWB  Added required comments and header for MEPAS QA plan
C     04/19/94     JPM  Made the local variables S1, ARG, and P2 double
C                       precision.  Changed EXPF calls to EXP and DEXP.
C     06/02/94     JPM  When river concentrations are truncated to an initial
C                       concentration, make sure the initial concentration is
C                       a dissolved concentration.
C     07/12/94     JPM  Removed the statement that truncates the computed
C                       concentration for the river environment (MED=5).
C                       if it exceeds the initial source concentraion.
C                       Truncating river concentrations affects all subsequent
C                       river receptors.
C     05/15/98     PDM  The time shift was modified to use TREMED instead of
C                       TCAP and TFAIL (RAD-E054).
C     08/08/00     JPM  Added a check to see if P2 equals zero in order to
C                       avoid divide by zero errors.  Using double precsion
C                       decay coefficients (PH1-P020).
C     08/23/01     JPM  Ensuring the undecayed parent concentration/flux does
C                       not exceed the initial concentration or maximum input
C                       flux (PH1-P024).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE DECAY (T,AL,CDA,ND,C,M1,TRAVEL,MED,CO,TPK,IDECAY,ICONC)
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'INDEX3.WTN'
      INCLUDE 'INDEX4.WTN'
      INCLUDE 'CAP.WTN'
      INCLUDE 'ERROR.WTN'
      INCLUDE 'INTER.WTN'
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION AL(MAXCON,12),TRAVEL(MAXCON),CDA(12)
C
C==== Variable Declaration ===================================================
C
      DOUBLE PRECISION S1,ARG,P2,P1,DLAM(MAXCON,12)
C
C==== DATA Statements ========================================================
C
C     None
C
C**********************************************************************
C     Back calculate the half-lives read from the input file (as close
C     as possible), and calculate double precision decay coefficients
C     (PH1-P020).
C**********************************************************************
      DO I = 1,ND
        DLAM(M1,I) = DLOG(DBLE(2.0))/(0.6931472/AL(M1,I))
      END DO
C
      ARG=0.
      IF (KMED .EQ. NMED) ARG=DLAM(M1,1)*(T+TRAVEL(M1))
C
      CDA(1)=0.
      IF (C .GT. 0.) CDA(1)=C
CJPM      IF(MED.EQ.5.AND.C.GT.CO.AND.ICONC.EQ.3) CDA(1) = CO
      IF (NPRINT .LE. 1) GO TO 100
      WRITE(6,10) M1,T,C,ARG
   10 FORMAT (I10,1P3E10.3)
  100 CONTINUE
      IF ((ND .EQ. 1) .OR. (KMED .NE. NMED)) RETURN
C******************************************************************************
C     CDA(1)=C*EXPF(-ARG) IS REPLACED BY WORK BELOW  JWB 3-24-1989
C******************************************************************************
      IF( IDECAY .EQ. 1 ) THEN
        TTIM = T  
      ELSE IF( IDECAY .EQ. 2 ) THEN
        TTIM = TPK
        IF( TPK .GT. T ) TTIM = T
      ELSE IF( IDECAY .EQ. 3 ) THEN
        TTIM = T - TPK
        IF( TPK .GT. T ) TTIM = 0.0
      END IF

c pdm shift time values for correct decay time JLS
      if(incr .eq. 1) then
        tshift = 0.0
      else 
        tshift = tremed(incr-1)
      end if
c
c      if(incr .eq. 1) then
c        tshift = 0.0
c      else if (incr .eq. 2) then
c        tshift = tcap
c      else
c        tshift = tfail
c      end if
c pdm

      TTIM = TTIM + TRAVEL(M1) + tshift
      ARG = DLAM(M1,1) * TTIM
      CDA(1) = C
C******************************************************************************
C  CALCULATE DECAY TIME AND SOURCE FOR PARENT [C=CDA*EXP(+AL*T)]  JWB 3-24-1989
C******************************************************************************
      CLOG = 0.0
      IF( C .GT. 1.) CLOG = ALOG(C)
      ATSUM = ARG + CLOG
      IF( ATSUM .GT. 70.0 ) THEN
        ARG = 70.0 - CLOG
        TTIM = ARG / DLAM(M1,1)
      END IF
C
C*****************************************************************************
C  THE CONCENTRATION OF THE DECAY-PRODUCT IN TERMS OF THE PARENT CONCENTRATION
C  IS CALCULATED USING EQUATION 5.69 FROM WHELAN ET AL. 1987.  BEFORE THE     
C  EQUATION CAN BE USED HOWEVER, SOME INTERMEDIATE CALCULATIONS MUST BE DONE. 
C
C  THE EQUATION IS BASED ON ORIGINAL PARENT CONCENTRATION, SO FIRST UNDECAY   
C  THE PARENT CONCENTRATION TO ITS ORIGINAL VALUE.                            
C*****************************************************************************
C
      C = C * DEXP(ARG)
      IF (MED.EQ.1.OR.MED.EQ.4.OR.MED.EQ.6) THEN
        FLUXMAX = 0.0
        DO II = 1,NUM(M1)
          IF (FLUXMAX.LT.PNT(II,M1)) FLUXMAX = PNT(II,M1)
        ENDDO
        IF (C.GT.FLUXMAX) C = FLUXMAX
      ELSE
        IF (C.GT.CO) C = CO
      ENDIF
C*****************************************************************************
C     CALCULATE THE PRODUCT AND SUMMATION SERIES BASED ON DECAY COEFFICIENTS  
C*****************************************************************************
      DO 1 I=2,ND
        P1 = 1.
        S1 = 0.
        I1 = I - 1
        DO 2 J=1,I1
          P1 = P1 * DLAM(M1,J)
2       CONTINUE
        DO 3 J=1,I
          P2 = 1.
          DO 4 K=1,I
            IF( K .EQ. J) GO TO 4
            P2 = P2 * (DLAM(M1,K) - DLAM(M1,J))
4         CONTINUE
          ARG = DLAM(M1,J) * TTIM
          IF (P2.NE.0.0) THEN
            S1 = S1 + DEXP(-ARG) / P2
          ELSE
            write(6,663)
            write(11,663)
            CALL MESSAGE ()
663         format(
     C      /2x,'ERROR --> P2 = 0.0',
     C      /2x,'Can occur when two half-lives in the same',
     C      /2x,'decay chain are equal')
            ERRORSIG=.TRUE.
            GOTO 9999
          ENDIF
3       CONTINUE
C*****************************************************************************
C     CALCULATE THE CONCENTRATION USING EQUATION 5.69 WHELAN ET AL. 1987      
C*****************************************************************************
        CDA(I) = P1 * S1 * C * DLAM(M1,I) / DLAM(M1,1)
1     CONTINUE
9999  RETURN
      END
