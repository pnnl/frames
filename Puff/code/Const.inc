C--------------------------------------------------------------------------
C
C     CONST.INC
C     
C     Date: 10/4/95
C
C     Description:  Constant include file
C
C     Included in modules:  BLOCK DATA, CLEAN, CLOSESHN, COMBINE, DIFDEP, 
C                           DOSVDIST, GRIDIN, INIT, INTPMET, MODPRMIN, 
C                           POLARIN, PRE_PUFF, PREPLUME, PUFF_EFF, 
C                           PUFFCS, PUFFM, PUFFR, RESOUT, SPECIN, SPECRAS,
C                           TADMOD2, TESTM, TIMESTEP, VERDIST, WCSPLUME,
C                           WNDPLUME   
C
C--------------------------------------------------------------------------

      REAL        chimin, cln_crit, delxy, dtr, far_west, far_south, 
     &            gscale, pi, twopi, wndmin, xsmax, xsmin, ysmax, ysmin,
     &            meandia, meanden    
      
ct     REAL        HSecInDOSE(3)

      INTEGER     iopdta, numx, numy, nph 
      
      LOGICAL     cln_flg

      COMMON /CONST/
     &            chimin, cln_crit, delxy, dtr, far_west, far_south, 
     &            gscale, pi, twopi, wndmin, xsmax, xsmin, ysmax, ysmin,
     &            iopdta, numx, numy, nph,
     &            cln_flg, meandia, meanden
ct    &            ,HSecInDose      
