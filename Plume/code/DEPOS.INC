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
     &         dv_p(MaxPBins), rtx(2), q_dpl_p(MaxPBins), 
     &         oq_dpl_p(MaxPBins)
      
      REAL     dv_I2, wcoef_I2, wcoef_p, q_dpl_I2, oq_dpl_I2
      
      COMMON /depos_puf/
     &         numpardis,
     &         massfrac, setvel,
     &         dv_p, rtx, q_dpl_p, oq_dpl_p, 
     &         dv_I2, wcoef_I2, wcoef_p, q_dpl_I2, oq_dpl_I2     
     