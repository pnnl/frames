      SUBROUTINE OUTP_INT
C----------------------------------------------------------------------
C     OUTP_INT
C
C     Christian J Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Created: 8/28/96
C
C     Description:   This routine initializes all the output arrays
C
C----------------------------------------------------------------------
      IMPLICIT NONE
      
      INCLUDE  'parm.inc'
      INCLUDE  'const.inc'
      INCLUDE  'matrix.inc'
      INCLUDE  'nuc_data.inc'
      
      INTEGER  i, j, k
      
      DO j = 1, numy
         DO i = 1, numx
            DO k = 1, nnucs
                  
               cg_eoq(k,i,j) = 0.0
               cg_wet(k,i,j) = 0.0
               cg_dry(k,i,j) = 0.0
            ENDDO
        
            DO k = 1, 2
               cg_xoq(k,i,j) = 0.0
            ENDDO
            
            cg_drq(i,j) = 0.0
            cg_weq(i,j) = 0.0
            cg_shine(i,j) = 0.0
            
            DO k = 1, maxpbins
               cg_xoq_p(k,i,j) = 0.0
               cg_drq_p(k,i,j) = 0.0
               cg_weq_p(k,i,j) = 0.0
            ENDDO
         ENDDO
      ENDDO
      
      RETURN
      
      END