      SUBROUTINE C_DKFACT ( Time )
C----------------------------------------------------------------------
C     C_DKFACT
C
C     Christian J Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Created:    9/9/96
C
C     Description:   Computes the factor for decay and ingrowth for
C                    the given chain
C
C----------------------------------------------------------------------
      IMPLICIT NONE
      
      INCLUDE  'parm.inc'
      INCLUDE  'nuc_data.inc'
      INCLUDE  'decay.inc'
      
      REAL*8   q0(MaxNucs), q1(MaxNucs), qb(MaxNucs)
      REAL     time
      
      INTEGER  i, j

      WRITE (25,*)  'In C_dkfact, time =  '

c     Set Up Arrays for Subroutine Chain

      DO i = 1, nnucs
         AL(i) = Dk_Const(i)
         DK(1,i)= Br_Fract(1,i)
         DK(2,i) = Br_Fract(2,i)
         IFRM(1,i) = Par_Indx(1,i)
         IFRM(2,i) = Par_Indx(2,i)
      ENDDO
      
      DO i = 1,nnucs
         qb(i) = 0.0
         q0(i) = 0.0
      ENDDO 
     
      nuc = nnucs
      
c     Calculate the fractions for each nuclide

      DO i = 1, nnucs
         IF ( i .GT. 1 ) q0(i-1) = 0.0
         q0(i) = 1.0
         
         CALL CHAIN( time, qb, q0, q1, 0 )
          
         DO j = 1, nnucs
            dkfract(i,j) = q1(j)/q0(i)
            IF( dkfract(i,j) .LT. 1E-8 ) dkfract(i,j) = 0.0 
         ENDDO
      ENDDO
      
      WRITE(25,*) 'Array of Decay and Ingrowth Factors'
      WRITE(25,'(9x,9a9)')(NucName(i), i = 1, nnucs)
      DO i = 1, nnucs
         WRITE(25,'(a9,1p,9e9.3)')NucName(i),(dkfract(i,j), j= 1, nnucs)      
      ENDDO
      
      RETURN
      
      END