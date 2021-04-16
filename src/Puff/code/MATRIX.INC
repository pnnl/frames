C--------------------------------------------------------------------------
C
C     MATRIX.INC
C     
C     Date: 10/4/95
C
C     Description:  Grid matrix and Polar Grid array include file
C
C     Included in modules:  BLOCK DATA,CLOSIN, CPLUME, PUFF_EFF, RESOUT, 
C                           WNDPLUME
C
C--------------------------------------------------------------------------

      REAL  cg_drq(Max_X,Max_Y), cg_drq_p(MaxPBins,Max_X,Max_Y),  
     &      cg_weq(Max_X,Max_Y), cg_weq_p(MaxPBins,Max_X,Max_Y),
     &      cg_eoq(MaxNucs,Max_X,Max_Y),  
     &      cg_xoq(2,Max_X,Max_Y), cg_xoq_p(MaxPBins,Max_X,Max_Y), 
     &      cg_dry(MaxNucs,Max_X,Max_Y),
     &      cg_wet(MaxNucs,Max_X,Max_Y),  
     &      cg_shine(Max_X,Max_Y)

     

      COMMON / MATRIX / 
                      
     &      cg_xoq, cg_eoq, cg_drq, cg_weq, cg_dry, cg_xoq_p, cg_weq_p, 
     &      cg_drq_p, cg_wet, cg_shine
                      
                      