      SUBROUTINE TESTM ( nhr )

C-----------------------------------------------------------------------
c
c     TESTM
c     
c     Date:              April 4, 1996
c
c     Description:       Subroutine to print additional puff information 
c                        for QA purposes.
c
c     Required modules:
c
c          Subroutines:  NONE
c
c            Functions:  MINDT
c
C----------------------------------------------------------------------- 

      IMPLICIT     NONE

      INCLUDE      'parm.inc'  
      INCLUDE      'const.inc'
      INCLUDE      'depos.inc'
cn     INCLUDE      'nuc_data.inc'
      INCLUDE      'puffs.inc'

      REAL         xmove, ymove
 
      INTEGER      i, idis, nhr
cn     INTEGER      n

  100 FORMAT (10X,'Hour: ',i5)
  110 FORMAT (1X,2X,5X,16(3X,I2,2X))
  115 FORMAT (1X,I2,':',4X,16(1X,F6.3))
      

      DO i = 1, tpuffs

         WRITE ( 7,100 ) nhr
         WRITE ( 7,140 )
  140    FORMAT ( /1X, '      PUFF  SRC  MF  AGE   GRID   GRID     Z',
     &      6X,'DELT X', 4X, 'DELT Y', 5X, ' SIGMA Y     SIGMA Z' )
         WRITE ( 7,150 )
  150    FORMAT ( 1X, '       NO', 20X, 'X', 6X, 'Y' )

         xmove = dxs(i) * delxy 
         ymove = dys(i) * delxy 

         WRITE ( 7,170 ) i, sp(i), mf(i), age(i), xp(i), yp(i),
     &                zp(i), xmove, ymove, sigmay(i), sigmaz(i) 
  170    FORMAT ( 6X,I3,5X,I1,3X,I1,1X,F7.1,1X,F6.2,1X,F6.2,
     &      2X,F6.3,2F10.2,2X,2(2X,F10.1) )

cn        WRITE ( 7,* ) 'qp:'     
cn        WRITE ( 7,180) (qp(n,i), n=1,nnucs)
cn 180    FORMAT( 1p 9(e10.3) )

         WRITE ( 7, * ) 'qpf:'
         WRITE ( 7, 190 ) (qpf(idis,i), idis = 1, numpardis(sp(i)))
  190    FORMAT( 1p, 8(e10.3) )
         
         WRITE ( 7,* )
      
      ENDDO 

      RETURN      
      
      END
