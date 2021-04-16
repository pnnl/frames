
      SUBROUTINE DK_PUFFS ( m, rq_i2, rqp )

C---------------------------------------------------------------------------
c
c     DK_PUFFS
C
c     Date:              April 3, 1996
c     Updated:           September 14, 1999
c                        February 15, 2000
C                        14 December 2001 BAN  to partition progeny
c
c     Description:       This subroutine decays and depletes the radionuclide 
c                        activity of each puff after the puff is processed for 
c                        the sampling interval
c
c     Required modules:  None
c
C---------------------------------------------------------------------------

      IMPLICIT      NONE
        
      INCLUDE       'parm.inc'
      INCLUDE       'depos.inc' 
      INCLUDE       'nuc_data.inc'
      INCLUDE       'puffs.inc' 
	INCLUDE       'rel.inc'  
        
      REAL*8        rq_i2, rqp(MaxPBins)
      
      REAL          qp0, qp1
      
      INTEGER       i, idis, m, n, k
      

c     Calculate the total fraction of mass for all particles bins
c     Before and After taking into account depletion 
      
      qp0 = 0.0
      qp1 = 0.0
      
      DO idis = 1, numpardis(sp(m))
         qp0 = qp0 + qpf(idis,m)   
         qpf(idis,m) = qpf(idis,m) * rqp(idis)
         qp1 = qp1 + qpf(idis,m)
      ENDDO
      
      DO n = nnucs, 1, -1 

c  decay the daughter and deplete in reverse order, then the parent 
         
         qp(n,m,1) = qp(n,m,1) * dkfract(n,n)
         qp(n,m,2) = qp(n,m,2) * rq_i2 * dkfract(n,n)                         
         qp(n,m,3) =  qp(n,m,3) * qp1 * dkfract(n,n)
               
c  add in the contribution from the other nuclides (if any)
         
         DO i = n-1,1,-1
         
            IF( dkfract(i,n) .EQ. 0 ) CYCLE
         
C            qp(n,m,1) = qp(n,m,1) + qp(i,m,1) * dkfract(i,n)
C            qp(n,m,2) = qp(n,m,2) + qp(i,m,2) * dkfract(i,n)
C            qp(n,m,3) = qp(n,m,3) + qp(i,m,3) * qp0 * dkfract(i,n)
C
C   Repartition progeny into particles, gases
C   BAN  14 Dec 2001
C      (The term qp0 accounts for some preferential depostion if it isn't always 1.0)
C
            qp(n,m,1) = qp(n,m,1) + dkfract(i,n)*ng_fract(n)*
     &	              (qp(i,m,1) + qp(i,m,2) + qp(i,m,3))          
            qp(n,m,2) = qp(n,m,2) + dkfract(i,n)*rg_fract(n)*
     &	              (qp(i,m,1) + qp(i,m,2) + qp(i,m,3)) 
            qp(n,m,3) = qp(n,m,3) + dkfract(i,n)*part_fract(n)*qp0*
     &	              (qp(i,m,1) + qp(i,m,2) + qp(i,m,3)) 

         ENDDO
      
      ENDDO
      
      RETURN

      END
