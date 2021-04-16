C*******************************************************************
C
C     PARM.INC
C     
C     Date: 3/29/96
C
C     Description:  Model dimension parameters
C
C*******************************************************************
      
      INTEGER     Max_X, Max_Y, MaxPBins,
     &            MaxGamma, MaxNucs, MaxSrcs 
     
      INTEGER*2   MxPuff

      PARAMETER ( MxPuff = 500 )
      PARAMETER ( Max_X = 50 )
      PARAMETER ( Max_Y = 50 )
      PARAMETER ( MaxSrcs = 3 )
      PARAMETER ( MaxGamma = 10 )
      PARAMETER ( MaxNucs = 9 )
      PARAMETER ( MaxPBins = 8 )
