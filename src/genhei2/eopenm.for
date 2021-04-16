*-----------------------------------------------------------------------
*
      subroutine openem
*
*-----------------------------------------------------------------------
*     routine:  openem
*     author:   k.f. eckerman
*     date:     10/23/93
*     purpose:  open index and dose coefficient files.
*
C
C    5 NOV 8  BAN   ADDED SEGMENT FOR WATER IMMERSION
C    22 Nov 2001 BAN  CHANGED TO FGR13 FORMATTING
C------------------------------------------------------------------------
      character*64 fpath
      character*12 target
      character*8 prog
      include 'iolist.cmn'
      open(olog, file = 'df_read.log')
C      prog = 'dcfpak'
        PROG='FGR13PAK'
*
      target = 'index.ndx'
      call fileini(fpath, target, nlen, olog, 41, prog)
      open(idex, file=fpath, access='direct', recl=nlen, form=
     :    'formatted', status='old')
*
      target = 'ingestsf.dfs'
      call fileini(fpath, target, nlen, olog, 41, prog)
      open(i30, file=fpath, access='direct', recl=nlen, form=
     :    'formatted', status='old')
*
      target = 'inhalesf.dfs'
      call fileini(fpath, target, nlen, olog, 41, prog)
      open(i31, file=fpath, access='direct', recl=nlen, form=
     :    'formatted', status='old')
*
      target = 'submrsin.dfs'
      call fileini(fpath, target, nlen, olog, 41, prog)
      open(i32, file=fpath, access='direct', recl=nlen, form=
     :    'formatted', status='old')
*
      target='grsurf00.dfs'
      call fileini(fpath, target, nlen, olog, 41, prog)
      open(i33, file=fpath, access='direct', recl=nlen, form=
     :    'formatted', status='old')
*
      target = 'grvolinf.dfs'
      call fileini(fpath, target, nlen, olog, 41, prog)
      open(i34, file=fpath, access='direct', recl=nlen, form=
     :    'formatted', status='old')
*
      target = 'external.rsk'
      call fileini(fpath, target, nlen, olog, 41, prog)
      open(i35, file=fpath, access='direct', recl=nlen, form=
     :    'formatted', status='old')
*
      target = 'ingest.rsk'
      call fileini(fpath, target, nlen, olog, 41, prog)
      open(i36, file=fpath, access='direct', recl=nlen, form=
     :    'formatted', status='old')
*
      target = 'inhale.rsk'
      call fileini(fpath, target, nlen, olog, 41, prog)
      open(i37, file=fpath, access='direct', recl=nlen, form=
     :    'formatted', status='old')
*
      target = 'WATERIMM.DFS'
      call fileini(fpath, target, nlen, olog, 41, prog)
      open(IDEXW, file=fpath, access='direct', recl=nlen, form=
     :    'formatted', status='old')
*
      return
      end
