C-----------------------------------------------------------------------
C
      SUBROUTINE CHAIN (T, AB, AM, AO, INTGRL)              
C
C     This subroutine calculates radioactive decay and buildup for a
C     chain of radionuclides for a given time period, allowing for 
C     nonradiological removal to a sink, or the time integral of the
C     activity
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Envoronmental Dosimetry System
C
C     Last Modification:  2-Dec-88  RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
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
C     INTRGL    - Control integer set > 0 to calculate time integral
C                 of activity, otherwise calculate decay 
C
C-----------------------------------------------------------------------

      INCLUDE 'DECAY.CMN'

      REAL*8 A(45), SUMPR, ASUM, AMD(9), EXPO(9), ABD(9) 
      REAL*8 ARG, TERM
      DIMENSION AM(9), AO(9), AB(9)
C
C---- Divide by lambda to convert to mass units
C
      DO 113 IJK = 1, 9
        EXPO(IJK) = 0.0D0
        ABD(IJK) = DBLE (AB(IJK) + AL(IJK))
        IF (AL(IJK) .NE. 0.0) AMD(IJK) = DBLE (AM(IJK) / AL(IJK))
  113 CONTINUE
C                            
C---  INITIALIZE COEFFICIENT ARRAY TO ZERO -------------------------------------
C
      N2N = NUC * (NUC-1) / 2 + NUC                    
      DO 100 IJK = 1, N2N
        A(IJK) = 0.0D0
  100 CONTINUE
C                                                                               
C---- DO LOOP ON CHAIN MEMBERS,  MAX = NUC -------------------------------------
C
      DO 5 J = 1, NUC                                                           
C                                                                               
C------ CALCULATE EXPONENTIAL FOR CURRENT NUCLIDE ------------------------------
C
        ARG=-ABD(J) * T                                                                                                                                         
        IF (INTGRL .GT. 0)    THEN
          IF (ARG .GT. 0.0) THEN 
            WRITE (*,111) ARG
  111       FORMAT (' CHAIN:  Positive argument ',1PE10.3)
          ELSE
C           FORM IS:  (1 - DEXP (ARG) ) / AB   FOR INTGRL > 0                      
            IF (-ARG .GT. 50.0) THEN
              EXPO(J) = 1.0D0 / ABD(J)
            ELSEIF (-ARG .GT. 0.001) THEN
              EXPO(J) = (1.0D0 - DEXP(ARG)) / ABD(J)
            ELSE
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
        ELSE                                                                    
C         FORM IS EXP(ARG) FOR INTGRL = 0                                       
          IF (-ARG .GT. 50.0) THEN
            EXPO(J) = 0.0D0
          ELSE
            EXPO(J) = DEXP (ARG)                                                   
          ENDIF
        ENDIF                                                                   
C                            
C       SET STARTING INDEX FOR TERM ARRAY A--                                   
        JJ = J * (J-1) / 2                                                      
C                                                                               
C       SET CHAIN POSITION MINUS ONE--                                          
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
C
        ASUM = 0.0D0
        IF (J1 .EQ. 0) GO TO 11
          DO 12 IRAP = 1, J1
            JK = JJ + IRAP
            ASUM = ASUM + A(JK)
   12     CONTINUE
   11   CONTINUE

        A(J+JJ) = AMD(J) - ASUM

        SUMPR = 0.0D0
        J2 = J
        DO 8884 IN = 1, J2
          JK = JJ + IN
          SUMPR = SUMPR + EXPO(IN) * A(JK)
 8884   CONTINUE

C       Multiply by lambda, return to single precision--
        AO(J) = SNGL (SUMPR * AL(J)) 

C                                                                               
    5 CONTINUE                                                                  
      RETURN                                                                    
C----------------------------------------------------------------------
      END                                                                       


