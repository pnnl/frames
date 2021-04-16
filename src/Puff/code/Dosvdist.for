
      SUBROUTINE DOSVDIST ( m, symin, szmin, sy, rshine, rq_i2, rqp, 
     &                      acute, PrgStat )
      
C-----------------------------------------------------------------------
c
c     DOSVSDIST
c
c     Date:              November 16, 1998
c     Updated:           February 15, 2000
c                        March 20, 2000  
c
c     Description:       This subroutine controls calculation of the shine 
c                        dose vs distance for the Cartesian grid. 
c
c     Required modules:
c
c          Subroutines:  PUFSHINE, SHINEPRP
c
c            Functions:  None
c
C-----------------------------------------------------------------------

      IMPLICIT     NONE
      
      INCLUDE      'parm.inc'  
      INCLUDE      'const.inc'
      INCLUDE      'depos.inc'
      INCLUDE      'difter.inc'
      INCLUDE      'met_data.inc'
      INCLUDE      'nuc_data.inc'
      INCLUDE      'puffs.inc'
      INCLUDE      'shine.inc'
      INCLUDE      'pf_shine.inc'

      REAL          rdyt, rshine, sy, sz, symin, szmin, qpart

      REAL*8        rqp(MaxPBins)
      REAL*8        rq_i2

      INTEGER       i, idis, m, n
      
      CHARACTER*50  PrgStat

      LOGICAL       acute

c  Select the larger of the diffusion coefficients for cloud shine        
c  calculations

      sy = AMAX1 ( symin, sigmay(m) )
      sz = AMAX1 ( szmin, sigmaz(m) )
          
C  Combine the relative doses for all radionuclides, accounting for 
c  depletion of the depositing radionuclides

      DO i = 1,20
         rdyt = 0.0
         DO n = 1,nnucs
            IF ( i .GT. ndx(n,1) ) CYCLE 
            qpart = 0.0
            DO idis = 1,numpardis(sp(m))
               qpart = qpart + qp(n,m,3) * rqp(idis) * qpf(idis,m)
            ENDDO

            rdyt = rdyt + rdy(i,n,1) * 
     &                   (qp(n,m,1) + qp(n,m,2) * rq_i2 + qpart)
         ENDDO  
         
         IF ( rdyt .GT. 0.0 ) THEN
            rdyc(i) = ALOG ( rdyt )
            maxn = i
         ELSE 
            rdyc(i) = -100.
         ENDIF
      ENDDO   
       
      CALL SHINEPRP ( zp(m), sy, sy**2, sz, sz**2, sigzlim, ldepth )

c  open temporary file for debugging cloud shine logic

cs     OPEN ( 91, file='shine.log',status='unknown', access='append')

      CALL PUFSHINE ( acute, PrgStat )
ccc   return added by BAN 30 May 2008 at JVR's suggestion
      IF (mdist .eq.0) RETURN
      IF ( PrgStat.NE.' ') RETURN
C
C  Return if rdyc(1) = 0
C  Added by BAN at JVR's suggestion 22 Oct 2001
      IF (rdyc(1) .eq. -100.0) RETURN


C  Determine maximum radius affected by shine

      IF ( sy .LT. chgmod ) THEN
            
c  use puff calculations
            
         rshine = pdx( mdist ) / delxy
      
      ELSE
            
c  use semi-infinite cloud approx.
            
         rshine = AMIN1 ( (5.3*sy),10000. ) / delxy
      
      ENDIF

      RETURN
      
      END
