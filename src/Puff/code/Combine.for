      SUBROUTINE COMBINE

C----------------------------------------------------------------------- 
c
c     COMBINE
c
c     Date:              April 4, 1996
c     Updated:           February 15, 2000
c
c     Description:       Puff consolidation subroutine from RATCHET.
c
c     Required modules:  None
c
C-----------------------------------------------------------------------

      IMPLICIT      NONE

      INCLUDE       'parm.inc' 
      INCLUDE       'const.inc' 
      INCLUDE       'depos.inc'
      INCLUDE       'nuc_data.inc'
      INCLUDE       'puffs.inc'

      REAL          av_sigy, sep

      INTEGER       i, j, k, n

      IF ( tpuffs .EQ. 1 ) RETURN

      i = tpuffs
      j = tpuffs - 1

  100 CONTINUE

      IF ( mf(i) .EQ. 0 ) GOTO 120
      
c  skip to next puff      
      
      DO WHILE ( (sp(i) .NE. sp(j)) .AND. (j .GT. 1) )
         j = j - 1
      ENDDO
              
              
      IF ( (sp(i) .EQ. sp(j)) .AND. (mf(j) .EQ. 1) )  THEN

c  puffs from same source and both active
 
         sep = delxy * SQRT ( (xp(i) - xp(j))**2 +
     &                        (yp(i) - yp(j))**2 )
         av_sigy = (sigmay(i) + sigmay(j)) / 2

         IF ( (sep / av_sigy) .LT. cln_crit )  THEN

c  puffs close enough together to be merged... 

            mf(j) = 0 

            xp(i)     = 0.5 * (xp(i) + xp(j))
            yp(i)     = 0.5 * (yp(i) + yp(j))
            zp(i)     = 0.5 * (zp(i) + zp(j))
            age(i)    = 0.5 * (age(i) + age(j))
            dxs(i)    = 0.5 * (dxs(i) + dxs(j))
            dys(i)    = 0.5 * (dys(i) + dys(j))
            sigyn(i)  = 0.5 * (sigyn(i) + sigyn(j))
            sigzn(i)  = 0.5 * (sigzn(i) + sigzn(j))
            hts(i)    = 0.5 * (hts(i) + hts(j))
            vts(i)    = 0.5 * (vts(i) + vts(j))
            sigmaz(i) = 0.5 * (sigmaz(i) + sigmaz(j))
            sigmay(i) = 0.5 * (sigmay(i) + sigmay(j))

            DO n = 1, nnucs
               DO k = 1,3
                  qp(n,i,k) = qp(n,i,k) + qp(n,j,k)
               ENDDO
            ENDDO

            DO k = 1, numpardis(sp(i))
               qpf(k,i) = 0.5 * (qpf(k,i) + qpf(k,j))
            ENDDO   

         ENDIF
      ENDIF

  120 CONTINUE

      i = i - 1
      j = i - 1

      IF ( i .GT. 1 )  GOTO 100

      RETURN    
      
      END
