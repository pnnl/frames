C-----------------------------------------------------------------------
C
C     Depos.INC
C
C     Date:    8/26/96
C
C     Description:   Contains the common block variables for doing
C                    deposition
C
C-----------------------------------------------------------------------
      INTEGER  numpardis(MaxSrcs)
      
      REAL     massfrac(MaxSrcs,MaxPBins), setvel(MaxSrcs,MaxPBins),
     &         dv_p(MaxPBins), rtx(2)
      
      REAL     dv_I2, wcdt_I2, wcdt_p
      
      COMMON /depos_puf/
     &         numpardis,
     &         massfrac, setvel,
     &         dv_p, rtx, 
     &         dv_I2, wcdt_I2, wcdt_p 