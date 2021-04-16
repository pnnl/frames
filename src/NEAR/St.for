
      PROGRAM ST
C
C   TEST PROGRAM FOR SETELAW to extract element and atomic weight for one decay
Chain.
c
      INCLUDE 'RADIN.CMN'
      CHARACTER*12 RNAMES(9)
      INTEGER NPRG
C
      RNAMES(1) = 'H3'
      RNAMES(2) = 'I131'
      RNAMES(3) = 'PU239'
      RNAMES(4) = 'TE131M'
      RNAMES(5) = 'C14'
      RNAMES(6) = 'U238'
      RNAMES(7) = 'h 3'
      NPRG = 7
      CALL SETELAW(RNAMES,NPRG)
      DO IN = 1,NPRG
        WRITE(*,100) RNAMES(IN),ELTT(IN),AWT(IN)
 100    FORMAT(' Input: ',A12,' Output: ',A2,A6)
      end do
      end


