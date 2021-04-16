C     Last change:  DLS  13 Nov 2001    6:48 am
C-----------------------------------------------------------------------
C
      SUBROUTINE CHAIN (T, AB, AM, AO, INTGRL)              
C
C     This subroutine calculates radioactive decay and buildup for a
C     chain of radionuclides for a given time period, allowing for 
C     nonradiological removal to a sink, the time integral of the
C     activity, or the double integral of deposition rate
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Envoronmental Dosimetry System
C
C     GENII Master Version:  2-Dec-88  RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C     Last Modification:  9-Feb-98 BAN
C
C-----------------------------------------------------------------------
C
C     T        	- Time over which decay is to be considered.  Units of
C                 T must be compatible with the units of the decay con-
C                 stants, AL
C     AB()      - Sum of the non radiological removal constants in units
C                 compatible with T
C     AM()      - Initial quantity of each radionuclide (Ci)
C     AO()      - Final activity (or time integral) of each radionuclide
C     INTRGL    - Control integer set to 0 to calculate chain decay,
C                 to 1 to calculate time integral of activity,
C                 or to 2 to calculate double integral
C
C-----------------------------------------------------------------------
C
C---- Include Statements -----------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'DECAY.CMN'
      INCLUDE 'DEVICE.CMN'
C
C---- Type and Dimension statements ------------------------------------------
C
      REAL*8 A(LENCHAIN *(LENCHAIN-1)/2+LENCHAIN),
     .       SUMPR, ASUM, AMD(LENCHAIN), EXPO(LENCHAIN),
     .       ABD(LENCHAIN), EX2(LENCHAIN)
      REAL*8 ARG, TERM
      DIMENSION AM(LENCHAIN), AO(LENCHAIN), AB(LENCHAIN)                                                           
c      
C-----  Divide by lambda to convert to atom equivalent units -----------------
c
      DO 113 IJK = 1, LENCHAIN
        EXPO(IJK) = 0.0D0
        EX2(IJK) = 0.0D0
        ABD(IJK) = DBLE (AB(IJK) + AL(IJK))
        IF (AL(IJK) .NE. 0.0) AMD(IJK) = DBLE (AM(IJK) / AL(IJK))
  113 CONTINUE
C                            
C---- Initialize coefficient array to zero -----------------------------------
c     First calculate number of possitions to zero
c
      N2N = NUC * (NUC-1) / 2 + NUC
c
      DO 100 IJK = 1, N2N
        A(IJK) = 0.0D0
  100 CONTINUE
C                                                                               
C---- DO LOOP ON CHAIN MEMBERS,  MAX = NUC -----------------------------------
c
      DO 5 J = 1, NUC                                                           
C                                                                               
C----- CALCULATE EXPONENTIAL FOR CURRENT NUCLIDE -----------------------------
c
        ARG=-ABD(J) * T
C                                                                               
        IF (INTGRL .EQ. 1)    THEN
          IF (ARG .GT. 0.0) THEN 
            WRITE (NERR,111) ARG
  111       FORMAT (' CHAIN:  Positive argument ',1PE10.3)
          ELSE
c
C----- FORM IS:  (1 - DEXP (ARG) ) / AB   FOR INTGRL = 1  ---------------------
c
            IF (-ARG .GT. 50.0) THEN
              EXPO(J) = 1.0D0 / ABD(J)
            ELSEIF (-ARG .GT. 0.001) THEN
              EXPO(J) = (1.0D0 - DEXP(ARG)) / ABD(J)
            ELSE
              IF(-ARG.LE.0.) THEN
                WRITE(NERR,112)
 112            FORMAT(' Error in chain decay subroutine: argument ',
     .          'equal zero.'/'   Check input for bad time values')
                NERROR = NERROR + 1
              ENDIF
              FX = -(DLOG10(-ARG))
              I = 10 - IFIX(FX)
              IF (I .LT. 2)  I=2
              TERM = - ARG
              EXPO(J) = -ARG / ABD(J)
              DO 13 IT = 2,I
                TERM = (TERM*ARG) / DBLE(IT)
                EXPO(J) = EXPO(J) + TERM / ABD(J)
  13          CONTINUE
            ENDIF
          ENDIF
        ENDIF                                                                    
c
c----- FORM IS EXP(ARG) FOR INTGRL = 0 ---------------------------------------
c
         IF (INTGRL .EQ. 0) THEN
          IF (-ARG .GT. 50.0) THEN
            EXPO(J) = 0.0D0
          ELSE
            EXPO(J) = DEXP (ARG)                                                   
          ENDIF
        ENDIF
C
C-----FORM IS [T - (1 - EXP(ARG))/ABD]/ABD FOR INTGRL = 2---------------------
C
        IF (INTGRL .EQ. 2) THEN
C
         IF (-ARG .GT. 50) THEN
           EXPO(J) = (T - (1.0D0/ABD(J)))/ABD(J)
C
         ELSEIF (-ARG .GT. 0.001) THEN
           EXPO(J) = (T - (1.0D0 - DEXP(ARG))/ABD(J))/ABD(J)
C
C------SERIES EXPANSION OF EXPONENTIAL TERM ----
         ELSE
           FX = -(DLOG10(-ARG))
              I = 11 - IFIX(FX)                  ! Revised Nov 2001 DLS
              IF (I .LT. 3)  I=3                 ! Revised Nov 2001 DLS
              TERM = T*T/2.0D0                   ! Revised Nov 2001 DLS
              EXPO(J) = TERM                     ! Revised Nov 2001 DLS
              DO 14 IT = 3,I                     ! Revised Nov 2001 DLS
                TERM = (TERM*ARG) / DBLE(IT)     ! Revised Nov 2001 DLS
                EXPO(J) = EXPO(J) + TERM         ! Revised Nov 2001 DLS
  14          CONTINUE                           ! Revised Nov 2001 DLS
c
c           I = 10 - IFIX(FX)
c           IF (I .LT. 2) I = 2
c           TERM = -ARG
c           DO 14  IT = 2, I
c             TERM = TERM * ARG / ABD(J)
c             EX2(J) = EX2(J) + TERM / ABD(J)
c   14      CONTINUE
c           EXPO(J) = (T - EX2(J))/ABD(J)
         ENDIF
        ENDIF
C
C-----  SET STARTING INDEX FOR TERM ARRAY A ----------------------------------
C
        JJ = J * (J-1) / 2                                                      
C                                                                               
C-----  SET CHAIN POSITION MINUS ONE -----------------------------------------
c
        J1 = J - 1                                                              
C                                                                               
        IF(J1 .GT. 0)   THEN                                                    
C                                                                               
          IMAX = MIN0 (J1, 2)                                                   
          DO 3 M = 1, J1                                                        
            DO 2 L = M, J1                                                      
              DO 1 I = 1, IMAX                                                  
C                                                                               
                IF (IFRM(I,J) .EQ. L)    THEN                                   
                  A(M+JJ) = A(M+JJ) + DK(I,J) * AL(L) * A(M+L * (L-1)/2)        
                ENDIF                                                           
C                                                                               
    1         CONTINUE                                                          
    2       CONTINUE                                                            
C                                                                               
            A(M+JJ) = A(M+JJ) / (ABD(J) - ABD(M))                                 
C                                                                               
    3     CONTINUE                                                              
C                                                                               
        ENDIF  
C
        ASUM = 0.0D0
        IF (J1 .EQ. 0) GO TO 11
          DO 12 IRAP = 1, J1
            JK = JJ + IRAP
            ASUM = ASUM + A(JK)
   12     CONTINUE
   11   CONTINUE
c
        A(J+JJ) = AMD(J) - ASUM
c
        SUMPR = 0.0D0
        J2 = J
        DO 8884 IN = 1, J2
          JK = JJ + IN
          SUMPR = SUMPR + EXPO(IN) * A(JK)
 8884   CONTINUE
c
C----- Multiply by lambda, return to single precision activity units ---------
c
        AO(J) = SNGL (SUMPR * AL(J))
C                                                                               
    5 CONTINUE                                                                  
      RETURN                                                                    
c
c----- End of Subroutine CHAIN -----------------------------------------------
c
      END                                                                       
