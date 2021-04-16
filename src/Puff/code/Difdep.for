C---------------------------------------------------------------------------
c
c     DIFDEP
c
c     Date:              November 16, 1998
c     Revised:           March 18, 2000
c
c     Description:       Diffusion, deposition and decay computations and
c                        time integration during the advection step.
c
c     Required modules:
c
c          Subroutines:  CG_DOSE, DK_PUFFS, DOSVDIST, PRE_PUFF, PUFF_EFF,   
c                        PUFFCS, TIMESTEP, VERDIST
c
c            Functions:  OFF_GRID
c
C---------------------------------------------------------------------------
 
      SUBROUTINE DIFDEP ( m, acute, PrgStat )

      IMPLICIT      NONE

      INCLUDE       'parm.inc'
      INCLUDE       'const.inc'
      INCLUDE       'depos.inc'
      INCLUDE       'difter.inc'
      INCLUDE       'met_data.inc'
      INCLUDE       'nuc_data.inc'
      INCLUDE       'puffs.inc'    
      INCLUDE       'pf_shine.inc'
      INCLUDE       'shine.inc'

      REAL          dxmi, dymi, pufcx, pufcy, rshine, sy,        
     &              symin, szmin, xgmn, xgmx, ygmn, ygmx

      REAL*8        dry_fract_p(MaxPBins), rqp(MaxPBins)
      REAL*8        dry_fract_i2, rq_i2
      
      INTEGER       i, jn, m, n, nsi
      
      CHARACTER*50  PrgStat
      
      LOGICAL       OFF_GRID

      LOGICAL       acute, scalc        
      
      rq_i2 = 1.0
      rqp   = 1.0
      
      fareast  = -99
      farnorth = -99
      farsouth = 99
      farwest  = 99

      sigzlim = 1.05 * AMAX1 ( zp(m),ldepth )
                                              
c  determine time step for integration of air conc. and sfc contam.                                             
                                              
      CALL TIMESTEP ( m, nsi, symin, szmin, acute, PrgStat )
      IF ( PrgStat.NE.' ' ) RETURN

      chimdt = chimin *  dt 
      dxmi = dxs(M) / nsi 
      dymi = dys(M) / nsi 

      DO jn = 1, nsi

c  determine puff characteristics
       
         CALL PRE_PUFF ( m, nsi, acute, PrgStat )
         IF ( PrgStat.NE.' ' ) RETURN
               
         CALL VERDIST ( m ) 

         IF ( mf(m) .EQ. 0 ) RETURN

         IF ( pufchi .LT. chimdt) THEN
            mf(m) = 0
            RETURN
         ENDIF

c      IF ( m .EQ. 1 ) WRITE ( 25,'(a, 4f10.2,1pe10.2)' ) 
c     & 'Subroutine DifDep ', age(m), dt, sigmay(m), sigmaz(m), pufchi
         
c  move puff one step                                           
                                       
         xp(m) = xp(m) + dxmi
         yp(m) = yp(m) + dymi
         pufcx = xp(m)
         pufcy = yp(m)   

         IF ( OFF_GRID ( xp(m), yp(m), xsmin, xsmax, ysmin, 
     &                   ysmax ) )  THEN 
     
c  out of tracking region and zero activity
      
            mf(m) = 0 
            RETURN

         ENDIF

         dry_fract_i2 = 1.0
         dry_fract_p = 1.0

         gcpchi = pufchi * vexp

c  deplete puff to account for deposition
               
         IF ( (zp(m) .LE. 10.0) .OR. (gcpchi .GT. chimdt) ) THEN
            dry_fract_i2 = 1.0 - 2.0 * dv_I2 * vexp * dt 
     &                        / (SQRT( twopi ) * sigmaz(m))
           DO i = 1, numpardis(sp(m))
                dry_fract_p(i) = 1.0 - 2.0 * dv_p(i) * vexp * dt 
     &                             / (SQRT( twopi ) * sigmaz(m))
            ENDDO
         ENDIF

         rq_i2 = rq_i2 * dry_fract_I2 * EXP( -wcdt_I2 )
         DO i = 1, numpardis(sp(m))
            rqp(i) = rqp(i) * dry_fract_p(i) * EXP( -wcdt_p )
         ENDDO    

         xgmn = 1.0 - rpew
         xgmx = FLOAT( numx ) + rpew
         ygmn = 1.0 - rpew
         ygmx = FLOAT( numy ) + rpew
             
         IF ( .NOT.OFF_GRID ( xp(m),yp(m),xgmn,xgmx,ygmn,ygmx ) )  THEN 

c  finite cloud shine dose calculations 

            IF ( ndxt(1) .GT. 0 ) THEN

c               IF ( MOD (jn,4) .EQ. 1 ) THEN 
                  CALL DOSVDIST ( m, symin, szmin, sy, rshine, rq_i2, 
     &                            rqp, acute, PrgStat)
                  IF ( PrgStat.NE. ' ' ) RETURN
c               ENDIF    

c              Calculate Cloud Shine if doses are significant               
               IF ( rdyc(1) .GE. -20.0 ) THEN
                  CALL PUFFCS( m, pufcx, pufcy, sy, rshine )
               ENDIF
               
            ENDIF                                            

c  exposures on Cartesian grid for remaining dose calculations

            CALL PUFF_EFF( m, pufcx, pufcy, rq_i2, rqp )
                                
         ENDIF       ! IF Not OFF_GRID
         
      ENDDO          ! do jn = 1, nsi   

c  dose calculations other than finite cloud

      CALL CG_DOSE( m )

      CALL DK_PUFFS( m, rq_i2, rqp )
 
      RETURN
      
      END
 
 