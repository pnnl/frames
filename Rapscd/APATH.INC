C----------------------------------------------------------------------
C
C     APATH.INC
C
C     Date:    8/16/96
C
C     Description:  Common Block Variables for FRMCONC
C
C----------------------------------------------------------------------
      INTEGER  ity(MaxType)
      INTEGER  icall, ndist, numc, nsc, nty, nain
      
      REAL     airc(NumDir,MaxDist,MaxType), 
     &         airb(NumDir,MaxDist,MaxType),
     &         surc(NumDir,MaxDist,MaxType),
c     &         avd(4,6),qnn(8),xkm(MaxDist)! 20Mar01/JGD/Case 6 logic
     &         avd(5,6),qnn(8),xkm(MaxDist)
      REAL     xc1, xb1, xc, xb, xmin, xmax, xmid
      
      COMMON /AIRCON /
     &        ity,
     &        icall, ndist, numc, nsc, nty, nain,
     &        airc,
     &        airb,
     &        surc,
     &        avd, qnn, xkm, 
     &        xc1, xb1, xc, xb, xmin, xmax, xmid
