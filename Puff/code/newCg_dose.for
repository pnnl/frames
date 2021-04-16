      SUBROUTINE CG_DOSE (m)  
C----------------------------------------------------------------------
c
c     CG_DOSE
c
c     Date:              February 15, 2000
c
c     Description:       Cartesian exposures, and dry and wet deposition 
c                        for the advection period
c
c     Required Modules:  None
c
C-----------------------------------------------------------------------

      IMPLICIT       NONE

      INCLUDE        'parm.inc'     
      INCLUDE        'depos.inc'
      INCLUDE        'matrix.inc'
      INCLUDE        'nuc_data.inc'
      INCLUDE        'puffs.inc'
      
      INTEGER         i, idis, j, k, m, n

      DO j = farsouth, farnorth
         DO i = farwest, fareast
            IF ( (cg_xoq(1,i,j) .GT. 0.0) .OR. 
     &           (cg_drq(i,j) .GT. 0.0) .OR. 
     &           (cg_weq(i,j) .GT. 0.0)  ) THEN

               DO n = 1,nnucs
                  cg_eoq(n,i,j) = cg_eoq(n,i,j) +
     &                cg_xoq(k+1,i,j) * (qp(n,m,1) + qp(n,m,2))
                  cg_dry(n,i,j) = cg_dry(n,i,j) +
     &                cg_drq(i,j) * qp(n,m,2)
                  cg_wet(n,i,j) = cg_wet(n,i,j) +
     &               cg_weq(i,j) * qp(n,m,2)

                  DO idis = 1, numpardis(sp(m))
                     cg_eoq(n,i,j) = cg_eoq(n,i,j) + 
     &                  cg_xoq_p(idis,i,j) * qp(n,m,3) * qpf(idis,m) 
                     cg_dry(n,i,j) = cg_dry(n,i,j) +
     &                  cg_drq_p(idis,i,j) * qp(n,m,3) * qpf(idis,m)
                     cg_wet(n,i,j) = cg_wet(n,i,j) +
     &                  cg_weq_p(idis,i,j) * qp(n,m,3) * qpf(idis,m)
                  ENDDO
               ENDDO
               
               cg_xoq(1,i,j) = 0
               cg_xoq(2,i,j) = 0
               cg_drq(i,j) = 0
               cg_weq(i,j) = 0
               DO idis = 1, numpardis(sp(m))
                  cg_xoq_p(idis,i,j) = 0
                  cg_drq_p(idis,i,j) = 0
                  cg_weq_p(idis,i,j) = 0
               ENDDO               
            
            ENDIF 
         ENDDO
      ENDDO

      RETURN

      END