C--------------------------------------------------------------------------
C
C     SHINE.INC                                        
C     
C     Date:  11/9/98 
C
C     Description:  Information used in cloudshine calculations 
C
C     Included in modules:  BLOCK DATA, CLOSESHN, CPLUME, DIFDEP, 
C                           DOSVDIST, INIT, MODPRMIN, PGPUFSHN, PUFFCS,
C                           PUFSHINE, RESOUT, SHINEPRP, WCSPLUME 
C
C--------------------------------------------------------------------------


      REAL    cylzd(4), DrSInf(MaxNucs), gsdf(MaxNucs),
     &        rdx(20), rdxl(20), rdy(20,MaxNucs,2), rdyc(20), 
     &        vexpz(4), z(4)
      REAL    chgmod, dd, dpufchi,  sigy, zi1, zi2 

      INTEGER mndx(10), ndx(MaxNucs,2), ndxt(2) 
      INTEGER maxn, nr, ntheta, rindx, timdx, tindx

      COMMON /SHINE/ 
     &        cylzd, DrSInf, gsdf,
     &        rdx, rdxl, rdy, rdyc, 
     &        vexpz, z,
     &        chgmod, dd, dpufchi, sigy, zi1, zi2,
     &        mndx, 
     &        ndx, maxn, ndxt, nr, ntheta, rindx, timdx, tindx