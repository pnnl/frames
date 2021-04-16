*-----------------------------------------------------------------------------*
*
*     3. Function Routines
*
*-----------------------------------------------------------------------
*
      character*(*) function check(nuke)
*
*-----------------------------------------------------------------------
*     function: check
*     author:   r.j. westfall
*     date:     07/20/89
*     purpose:  convert chemical symbol in nuclide name to proper
*               notation, e.g.; Kr-85m, etc.
*
      character*(*) nuke
      character*7 ltrim
*
*     remove any leading blanks from nuke
*
      nuke = ltrim( nuke )
*
*     ensure first character is upper case.
*
      if (nuke(:1) .ge. 'a' .and. nuke(:1) .le. 'z')
     :    nuke = char(ichar(nuke(:1)) - 32) // nuke(2:7)
*
*     ensure second character, if present, is lower case.
*
      if (nuke(2:2) .ge. 'A' .and. nuke(2:2) .le. 'Z')
     :    nuke = nuke(:1) // char(ichar(nuke(2:2)) + 32) // nuke(3:7)
*
*     ensure metastable notation, if present, is lower case.
*
      do j = 4, 7
         if (nuke(j:j) .ge. 'A' .and. nuke(j:j) .le. 'Z')
     :   nuke = nuke(:j-1) // char(ichar(nuke(j:j)) + 32) // nuke(j+1:)
      end do
      check = nuke
      end
