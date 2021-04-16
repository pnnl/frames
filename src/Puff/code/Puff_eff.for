
      SUBROUTINE PUFF_EFF ( m, pufcx, pufcy, rq_i2, rqp )                               
      
C---------------------------------------------------------------------------
c
c     PUFF_EFF
c
c     Date:              March 11, 1996
c     Updated:           September 14, 1999
c                        March 20, 2000
c
c     Description:       This subroutine accumulates the air exposures
c                        and ground contamination on the Cartesian grid.
c
c     Required modules:
c
c          Subroutines:  
c
c            Functions:  None
c
c---------------------------------------------------------------------------
      
      IMPLICIT      NONE
      
      INCLUDE       'parm.inc'
      INCLUDE       'const.inc'
      INCLUDE       'depos.inc'
      INCLUDE       'difter.inc'
      INCLUDE       'matrix.inc'
      INCLUDE       'met_data.inc'
      INCLUDE       'puffs.inc'
       
      REAL          hexp, pchi, pufcx, pufcy, radius, rpee, rpgw, rsq
      
      REAL*8        dry_fract_p(MaxPBins), rqp(MaxPBins)
      REAL*8        dry_fract_i2, rq_i2, wshoi_i2, wshoi_p
                              
      INTEGER       east, i, idis, j, k, m, north, south, west
           
      LOGICAL       calc_flg, dryflg   
      
      calc_flg = .FALSE.              
      dryflg = .FALSE.   
           
      IF ( zp(m) .LE. 10.0 ) THEN
         rpgw = rpew
         dryflg = .TRUE.
         calc_flg = .TRUE.
      ELSE IF ( gcpchi .GT. chimdt )  THEN
         rpgw = sigmay(m) * radcnst / delxy
         dryflg = .TRUE.
         calc_flg = .TRUE.
      ENDIF             

      radius = rpgw
      
      wshoi_I2 = 0.0
      wshoi_p = 0.0
      IF ( pcode .GT. 0 ) THEN 
         calc_flg = .TRUE.
         rpee = rpew                  
         radius = AMAX1 ( rpee, radius )
         wshoi_I2 = (1.0 - EXP( -wcdt_I2 )) / ( twopi * sigysq )
         wshoi_p = (1.0 - EXP( -wcdt_p )) / ( twopi * sigysq )
      ENDIF
      
      IF ( calc_flg )  THEN 

         IF ( dryflg ) THEN
            dry_fract_I2 = 1.0 - 2.0 * dv_I2 * vexp 
     &                     / (SQRT( twopi ) * sigmaz(m) )
            DO idis = 1, numpardis(sp(m))
               dry_fract_p(idis) = 1.0 - 2.0 * dv_p(idis) * vexp
     &                           / (SQRT( twopi ) * sigmaz(m) )
            ENDDO
         ELSE
            dry_fract_I2 = 0.0
            dry_fract_p = 0.0
         ENDIF 

c     determine the area of influence of the puff
      
         west = pufcx - radius
         east = pufcx + radius
         south = pufcy - radius
         north = pufcy + radius
         IF ( west .LT. 1 )  west = 1
         IF ( east .GT. numx ) east = numx
         IF ( south .LT. 1 )  south = 1
         IF ( north .GT. numy ) north = numy 
         
         fareast = MAX ( fareast, east )
         farwest = MIN ( farwest, west ) 
         farnorth = MAX ( farnorth, north )
         farsouth = MIN ( farsouth, south )
         
c        accumulate air concentration and surface deposition 
 
         DO j = south, north
            DO i = west, east
               rsq = ( xp(m) - i )**2 +
     &               ( yp(m) - j )**2
               IF ( rsq .LE. radius**2 )  THEN
                  hexp = EXP ( hsgsq * rsq )
                  pchi = pufchi * hexp * vexp

                  cg_xoq(1,i,j) = cg_xoq(1,i,j) + pchi 

                  cg_xoq(2,i,j) = cg_xoq(2,i,j) + pchi * rq_i2
                  cg_drq(i,j) = cg_drq(i,j) + (dv_I2 * pchi) * rq_i2 
                  cg_weq(i,j) = cg_weq(i,j) + (wshoi_I2 * hexp) * rq_i2

                  DO idis = 1, numpardis(sp(m))
                     cg_xoq_p(idis,i,j) = cg_xoq_p(idis,i,j) + 
     &                                    pchi * rqp(idis)

                     cg_drq_p(idis,i,j) = cg_drq_p(idis,i,j) + 
     &                                 (dv_p(idis) * pchi ) * rqp(idis)
                     cg_weq_p(idis,i,j) = cg_weq_p(idis,i,j) +
     &                                 (wshoi_p * hexp ) * rqp(idis)
                  ENDDO
                  
               ENDIF    
            ENDDO
         ENDDO

      ENDIF      ! deposition

c     deplete puff
                       
      rq_i2 = rq_i2 * dry_fract_I2 * EXP( -wcdt_I2 )
      DO idis = 1, numpardis(sp(m))
         rqp(idis) = rqp(idis) * dry_fract_p(idis) * EXP( -wcdt_p )
      ENDDO
 
c      IF ( m .EQ. 1 ) WRITE ( 25,'(a, 12f18.5)' )
c     &                'Puff_eff puff depletion', rq_i2,
c     &                 (rqp(idis), idis=1,numpardis(sp(m)))

      RETURN

      END