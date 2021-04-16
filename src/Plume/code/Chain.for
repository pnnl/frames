C   MEPAS HAZ2: CHAIN.FOR             Version Date: 20-Apr-94               
C   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE CHAIN                                 *
C                                                                            *
C  Subroutine CHAIN  evaluate decay chain activities, time-ingetrals of      *
C                    activity, or activity after constant deposition for a   *
C                    chain of radioactive pollutants.
C                                                                            *
C  Written by:       Dennis Strenge                                          *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    20-Apr-1994                                             *
C  Last Modified:    20-Apr-94      DLS                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: MEPAS/HAZ2
C     Called by: SUBROUTINE (several)
C     Calls: SUBROUTINE EXFCT
C     Common blocks referenced: NONE
C
C==== Significant Parameter Designation and Description ======================
C
c  Name        Type             Purpose
c  ---------   ------    ---------------------------------------------------
c   T           Real     Time over which decay calculation is to be performed,
c                        in units constent with AB and radiological decay.
c   AB(9)       Real     Non-radiological loss-rate constant for each member,
c                        inverse time units.
c   AM(9)       Real     Initial activity of each chain member, activity
c                        units.
c   AO(9)       Real     Final activity (or time integral) of each nuclide in
c                        the decay chain, activity units ( or activity-days)
c   INTGRL      Integer  Control flag set 0 to calculate decay for time T,
c                        set to 1 to calculate time integral over T,
c                        set to 2 to calculate double time integral over T.
c
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE CHAIN (T, AB, AM, AO, INTGRL)
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'DECAY.INC'
C
C==== DIMENSION Statements ===================================================
C
      REAL*8 A(45)                     
      REAL*8 SUMPR, ASUM, AMD(9), EXPO(9), ABD(9)  
      REAL*8 ARG, TERM 
      REAL*8 AM(9), AO(9), AB(9)                                           
C
C==== Variable Declarations ============================================
C
      INTEGER  IJK, N2N, M, J, JJ, J1,J2, JK, INTGRL, I, IT, IMAX, L, 
     &         IRAP, IN

      REAL     FX, T
C
C==== DATA Statements ========================================================
C
C    None
C   Initialize chain parameters
C
      DO 113 IJK = 1, NUC
C   Initialize exponential term
        EXPO(IJK) = 0.0D0 
C   Calculate effective removal constant, sum of radiological and 
C     non-radiological constants
        ABD(IJK) = DBLE (AB(IJK) + AL(IJK)) 
C   Divide by lambda to convert activity to atom units
        IF (AL(IJK) .NE. 0.0) AMD(IJK) = DBLE (AM(IJK) / AL(IJK)) 
  113 CONTINUE 
C                             
C   Initialize coefficient array to zero--                                    
      N2N = NUC * (NUC-1) / 2 + NUC                     
C
      DO 100 IJK = 1, N2N 
        A(IJK) = 0.0D0 
  100 CONTINUE 
C                                                                             
C     Do loop on chain members,  Maximum = NUC
      DO 5 J = 1, NUC                                                       
C                                                                           
C       Calculate exponential for current nuclide                           
        ARG=-ABD(J) * T                                                     
C                                                                           
        IF (INTGRL .GT. 0)    THEN 
C    Test for error in argument value, must be negative or zero              
          IF (ARG .GT. 0.0) THEN  
c            WRITE(*,111) ARG 
c  111       FORMAT (' CHAIN:  Positive argument ',1PE10.3) 
          ELSE 
C    For integral form: (1 - DEXP (ARG) ) / AB,  For INTGRL > 0             
            IF (-ARG .GT. 50.0) THEN 
              EXPO(J) = 1.0D0 / ABD(J) 
            ELSEIF (-ARG .GT. 0.001) THEN 
              EXPO(J) = (1.0D0 - DEXP(ARG)) / ABD(J) 
            ELSE
              FX = -(DLOG10(-ARG)) 
              I = 10 - IFIX(FX) 
              IF (I .LT. 2)  I=2 
              TERM = T 
              EXPO(J) = -ARG / ABD(J) 
              DO 13 IT = 2,I 
                TERM = (TERM*ARG) / DBLE(IT) 
                EXPO(J) = EXPO(J) + TERM 
  13          CONTINUE 
            ENDIF 
          ENDIF 
        ELSE                                                             
C    For standard form: EXP(ARG), for INTGRL = 0          
          IF (-ARG .GT. 50.0) THEN 
            EXPO(J) = 0.0D0 
          ELSE 
            EXPO(J) = DEXP (ARG)                                         
          ENDIF 
        ENDIF                                                            
        IF(INTGRL.GT.1) THEN
C    Double integral form: (T-(1-DEXP(ARG))/AB)AB,  For INTGRL > 1 
C    (Above EXPO(J) values may be used in this evaluation) 
            IF (-ARG .GT. 50.0) THEN 
              EXPO(J) = (T- 1.D0 / ABD(J) ) / ABD(J)
            ELSEIF (-ARG .GT. 0.001) THEN 
              EXPO(J) = (T - EXPO(J)) / ABD(J) 
            ELSE 
              FX = -(DLOG10(-ARG)) 
              I = 11 - IFIX(FX) 
              IF (I .LT. 3)  I=3 
              TERM = T*T/2.0D0
              EXPO(J) = TERM
              DO 14 IT = 3,I 
                TERM = (TERM*ARG) / DBLE(IT) 
                EXPO(J) = EXPO(J) + TERM 
  14          CONTINUE 
            ENDIF 
        ENDIF 
C             
C   Set starting index for term array A                              
        JJ = J * (J-1) / 2                                               
C                                                                        
C   Set number of daughters following current radionuclide   
C     (chain position minus one)
        J1 = J - 1                                                        
C   Calculate coefficient contributions for daughters (if any)            
        IF(J1 .GT. 0)   THEN                                              
C   Set maximum number of parents to consider for current radionuclide (<=2)
          IMAX = MIN0 (J1, 2)                                             
          DO 3 M = 1, J1                                                  
            DO 2 L = M, J1                                                
              DO 1 I = 1, IMAX                                            
C   If parent radionuclide L decays to daughter J, include term in 
C     coefficient calculation
                IF (IFRM(I,J) .EQ. L)    THEN                             
                  A(M+JJ) = A(M+JJ) + DK(I,J) * AL(L) * A(M+L * (L-1)/2)  
                ENDIF                                                     
C                                                                         
    1         CONTINUE                                                    
    2       CONTINUE                                                      
C   Calculate final contribution to coefficient for current chain member
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

C   Calculate last coefficient for current chain member

        A(J+JJ) = AMD(J) - ASUM  
        SUMPR = 0.0D0 
        J2 = J 
        DO 8884 IN = 1, J2 
          JK = JJ + IN 
          SUMPR = SUMPR + EXPO(IN) * A(JK) 
 8884   CONTINUE 

C   Calculate activity of current chain member 
C       Multiply by lambda, and return to single precision

        AO(J) = SUMPR * AL(J)  

C                                                                         
    5 CONTINUE                                                            
      RETURN                                                              
C----------------  CHAIN.FOR  -----------------------------------------c
      END                                                                  