ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     SRCREC.INC
c
c     Contains:  The receptor and source information
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      REAL     Area(36,MaxSrcs), bldhgt(36,MaxSrcs), bldwth(36,MaxSrcs),
     .         etemp(MaxSrcs), recradii(MaxRadii), Src_Ang(MaxSrcs),
     .         Src_Rad(MaxSrcs), Src_Var1(MaxSrcs), St_Flow(MaxSrcs),
     .         St_Rad(MaxSrcs), strang(MaxRecp,MaxSrcs), 
     .         strdist(MaxRecp,MaxSrcs), RelRate(MaxSrcs,MaxNucs),
     .         ng_fract(MaxNucs), rg_fract(MaxNucs), part_fract(MaxNucs)
     
C      REAL     ptrad, rechgt, strtang, meandia, meanden, ng_fract,
C     &         rg_fract, part_fract
      REAL     ptrad, rechgt, strtang, meandia, meanden 
	      
      INTEGER  SrcTyp(MaxSrcs), stridx(MaxRecp,MaxSrcs), 
     .         strsec(MaxRecp,MaxSrcs)
     
      INTEGER  inum, numradii, numsrc, recnum
      
      LOGICAL  BIDFlg(MaxSrcs), Hgwndflg(MaxSrcs), ISCwakflg(MaxSrcs),
     .         Lwwndflg(MaxSrcs), PlmFlg(MaxSrcs), SSlowbnd(MaxSrcs)
     
      COMMON /srcrec/
     .         Area, bldhgt, bldwth,
     .         etemp, recradii, Src_Ang,
     .         Src_Rad, Src_Var1, St_Flow,
     .         St_Rad, strang, 
     .         strdist, RelRate,
     .         ng_fract, rg_fract, part_fract,
     .         ptrad, rechgt, strtang, meandia, meanden,
     .         SrcTyp, stridx, 
     .         strsec,
     .         inum, numradii, numsrc, recnum,
     .         BIDFlg, Hgwndflg, ISCwakflg,
     .         Lwwndflg, PlmFlg, SSlowbnd