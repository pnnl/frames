*-----------------------------------------------------------------------
*
      subroutine chekab( nuke )
*
*-----------------------------------------------------------------------
*     routine:  chekab
*     author:   M. Cristy
*     date:     05/05/93
*     purpose:  if nuclide not found, checks whether "a" & "b" isomers
*               exist
*
      character*7 nuke, nukeab, nukea, nukeb, check
      character*6 thalf1, thalf2
      parameter (nnuke = 9)
      dimension nukeab(nnuke), nukea(nnuke), nukeb(nnuke),
     :          thalf1(nnuke), thalf2(nnuke)
      data nukeab /'Eu-150 ', 'In-110 ', 'Ir-186 ', 'Nb-89  ',
     .'Np-236 ', 'Re-182 ', 'Sb-120 ', 'Sb-128 ', 'Ta-178 '/
      data nukea /'Eu-150a', 'In-110a', 'Ir-186a', 'Nb-89a ',
     .'Np-236a', 'Re-182a', 'Sb-120a', 'Sb-128a', 'Ta-178a'/
      data nukeb /'Eu-150b', 'In-110b', 'Ir-186b', 'Nb-89b ',
     .'Np-236b', 'Re-182b', 'Sb-120b', 'Sb-128b', 'Ta-178b'/
      data thalf1/'12.62h', '69.1m', '15.8h', '66m',  '115E3y',
     .'12.7h', '15.89m', '10.4m', '9.31m'/
      data thalf2/'34.2y',  '4.9h',  '1.75h', '122m', '22.5h',
     .'64.0h', '5.76d',  '9.01h', '2.2h'/
      do i = 1, nnuke
        if (nuke .eq. nukeab(i)) then
          write(*,9110) nuke, nukea(i), thalf1(i),nukeb(i),thalf2(i)
          write(*,'(a\)')' Input nuclide or <Enter> to quit)-> '
          read(*,'(bn, a7)') nuke
          nuke = check( nuke )
          if (len_trim(nuke) .eq. 0) stop
          return
        endif
      end do
      return
 9110 format( 4x,'Nuclide ',a,'has 2 isomers:'/20x,a,' with halflife ',a
     ./16x,'and ',a,' with halflife ',a/4x,
     .'Re-input entire name with appropriate "a" or "b" designation',/)
      end
