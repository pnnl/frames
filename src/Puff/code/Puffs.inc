C--------------------------------------------------------------------------
C
C     PUFFS.INC
C     
C     Date: 2/15/00
C
C     Description:  Puff parameter include file
C
C     Included in modules: BLOCK DATA, CLEAN, CLEAN2, CLOSIN, COMBINE, 
C                          CPLUME, DIFDEP, DOSVDIST, MODPRMIN, PRE_PUFF,
C                          PREPLUME, PUFF_EFF, PUFFCS, PUFFM, PUFFR, 
C                          SPECIN, TADMOD2, TESTM, TIMESTEP, VERDIST, 
C                          WCSPLUME, WNDPLUME  
C
C--------------------------------------------------------------------------

      REAL     age(MXPUFF), dxs(MXPUFF), dys(MXPUFF),
     &         hts(MXPUFF), qp(MaxNucs,MxPuff,3), qpf(MaxPBins,MxPuff),
     &         sigmay(MXPUFF), sigmaz(MXPUFF), sigyn(MXPUFF), 
     &         sigzn(MXPUFF), vts(MXPUFF), xp(MXPUFF), yp(MXPUFF), 
     &         zp(MXPUFF)
      REAL     radcnst, sy_cnst
        
      INTEGER  fareast, farnorth, farsouth, farwest
      INTEGER  mf(MXPUFF), sp(MXPUFF)
      INTEGER  tpuffs
            
       COMMON / PUFFS /
     &         age, mf, sp, tpuffs,
     &         radcnst, sy_cnst,
     &         hts, vts, sigmay, sigmaz, sigyn, sigzn, 
     &         xp, yp, zp, dxs, dys, qp, qpf, 
     &         fareast, farnorth, farsouth, farwest
