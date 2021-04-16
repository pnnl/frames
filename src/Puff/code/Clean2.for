      SUBROUTINE CLEAN2
C---------------------------------------------------------------------------
c
c     CLEAN2
c     
c     Date:              April 4, 1996
c     Updated:           February 15, 2000    
c
c     Description:       Eliminates inactive puffs (RATCHET).
c
c     Required modules:  None 
c
C---------------------------------------------------------------------------

      IMPLICIT     NONE

      INCLUDE      'parm.inc'
      INCLUDE      'depos.inc'
      INCLUDE      'nuc_data.inc'
      INCLUDE      'puffs.inc'

      INTEGER      i, j, k, ltpuffs, n, np

      ltpuffs = tpuffs

      IF ( tpuffs .EQ. 1 ) THEN
         IF ( mf(1) .EQ. 1 ) THEN
            RETURN
         ELSE
d           WRITE(25,240) 1,0
            tpuffs = 0
            GOTO 250
         ENDIF
      ENDIF

c  drop puffs 
 
      i = 1
      j = 1

200   CONTINUE

      IF ( mf(i) .EQ. 0 ) GOTO 220
      mf(j) = mf(i)
      DO n = 1,nnucs
         DO k = 1,3
            qp(n,j,k) = qp(n,i,k)
         ENDDO
      ENDDO
      
      DO k = 1, numpardis(sp(i))
         qpf(k,j) = qpf(k,i)
      ENDDO
      
      sigmay(j) = sigmay(i)
      sigmaz(j) = sigmaz(i)
      sigzn(j) = sigzn(i)
      sigyn(j) = sigyn(i)
      hts(j) = hts(i)
      vts(j) = vts(i)
      sp(j) = sp(i)
      xp(j) = xp(i)
      yp(j) = yp(i)
      zp(j) = zp(i)
      dxs(j) = dxs(i)
      dys(j) = dys(i)
      age(j) = age(i)
      j = j + 1
220   CONTINUE

      i = i + 1
      IF ( i .LE. tpuffs )  GOTO 200

d     WRITE ( 25,240 ) tpuffs - (j-1), j-1
d 240 FORMAT ( 5X, 'CLEAN CALLED --> ', I4, ' DROPPED',
d    &  4X, I4, ' REMAIN ON THE GRID' )

      tpuffs = j - 1
  250 CONTINUE
  
      np = tpuffs + 1
      DO i = np, ltpuffs
         mf(i) = 0
         DO n = 1,nnucs
            DO k = 1,3
               qp(n,i,k) = 0.0
            ENDDO
         ENDDO
         DO k = 1, numpardis(sp(i))
            qpf(k,i) = 0.0
         ENDDO
         sigmay(i) = 0
         sigmaz(i) = 0
         sigzn(i) = 0
         sigyn(i) = 0
         hts(i) = 0
         vts(i) = 0
         sp(i) = 0
         xp(i) = 0
         yp(i) = 0
         zp(i) = 0
         dxs(i) = 0
         dys(i) = 0
         age(i) = 0
      ENDDO

      RETURN
      
      END
