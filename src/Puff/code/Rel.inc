C-----------------------------------------------------------------------
C
C     REL.INC
C     
C     Date:                 February 15, 2000
C     Rev.  14 Dec 2001     BAN to dimension ng_, rg_, and part_fract
C
C     Description:          Release data include file
C
C     Included in modules:  BLOCK DATA, DIFDEP, PREPLUME, PUFFR, 
C                           TADMOD2, WNDPLUME 
C
C-----------------------------------------------------------------------

      REAL    rstack(MaxSrcs), 
     &        sflow(MaxSrcs), SigYIn(MaxSrcs), SigZin(MaxSrcs),
     &        stempc(MaxSrcs), xsrc(MaxSrcs), ysrc(MaxSrcs), 
     &        zsourc(MaxSrcs), RelRate(MaxSrcs,MaxNucs),
     &        ng_fract(MaxNucs), rg_fract(MaxNucs), part_fract(MaxNucs)
      
      INTEGER nsrc
      
      LOGICAL dorise(MaxSrcs)                       
      
      COMMON /REL/  
     &        rstack, 
     &        sflow, sigyin, sigzin, stempc, 
     &        xsrc,ysrc,zsourc, RelRate, ng_fract,rg_fract,part_fract,
     &        nsrc, 
     &        dorise
    
