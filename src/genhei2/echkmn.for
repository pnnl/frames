*-----------------------------------------------------------------------
*
      subroutine chekmn( nuke )
*
*-----------------------------------------------------------------------
*     routine: chekmn
*     author:  M. Cristy
*     date:    05/05/93, revised 8/16/93
*     purpose: if "m" isomer is requested, checks whether "n" isomer
*              also exists.
*
      character*7 nuke, nukem, nuken
      character*6 thalfm, thalfn
      character*1 meta
      dimension nukem(3), nuken(3), thalfm(3), thalfn(3)
      data nukem /'Ir-190m', 'Sb-124m', 'Tb-156m'/
      data nuken /'Ir-190n', 'Sb-124n', 'Tb-156n'/
      data thalfm/'1.2h', '93s',   '24.4h'/
      data thalfn/'3.1h', '20.2m', '5.0h'/
      data nnuke /3/
      do 20 i = 1, nnuke
        if (nuke .eq. nukem(i)) then
          write(*, 9110) nuke, nukem(i), thalfm(i), nuken(i), thalfn(i)
   10     write(*, 9120) nukem(i), nuken(i)
          read(*,'(a1)') meta
          if (meta.eq.' ' .or. meta.eq.'m' .or. meta.eq.'M') then
              return
            elseif (meta.eq.'n' .or. meta.eq.'N') then
              nuke = nuke(1:6) // 'n'
              return
            else
              goto 10
          endif
        endif
   20 continue
      return
 9110 format(4x,'Nuclide ',a,' has 2 metastable isomers:'/20x,
     :a,' with halflife ',a/16x,'and ', a,' with halflife ',a)
 9120 format(4x,'Input <Enter> to accept ',a,', or input "n" for ',a,
     :': ',/)
      end
