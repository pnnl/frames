ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Parm.INC
c
c     Parameter used in MultSrc to define the bounds of the arrays.
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      INTEGER  MaxGamma, MaxNucs, MaxRadii, MaxRecp, MaxSrcs, MaxPBins
      
      PARAMETER( MaxRadii = 10 )
      PARAMETER( MaxRecp = 36*MaxRadii ) 
      PARAMETER( MaxSrcs = 5 )
      PARAMETER( MaxNucs = 9 )
      PARAMETER( MaxGamma = 10 )
      PARAMETER( MaxPBins = 8 )
      