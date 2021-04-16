*-----------------------------------------------------------------------
*
      subroutine nukeok (nuke, ok)
*
*-----------------------------------------------------------------------
      character*7 nuke, check
      logical ok
*
*     function check ensure proper format of user input
*
      nuke = check( nuke )
      call chekmn( nuke )
*
*     find the nuclide in the index file
*
      ipt = ibinry( nuke )
      if (ipt .eq. 0) then
         call chekab( nuke )
         ipt = ibinry( nuke )
      end if
*
*     if pointer ipt is zero, the nuclide is not in the index file
*     else we have a valid nuclide to process
*
      if (ipt .eq. 0) then
         ok = .false.
      else
         ok = .true.
      end if
      return
      end
