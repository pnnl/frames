*-----------------------------------------------------------------------------*
*                                                                             *
      double precision function timest (t, ix)
*                                                                             *
*-----------------------------------------------------------------------------* 
*     author:   k. f. eckerman
*     date:     10/04/94
*     purpose:  function returns time in days given time string t and its
*               units ix.
*     function arguments.
      character*2 ix
      character*8 t
      double precision tp
*
      read(t,'(E8.0)') tp
      if (ix .eq. 'us') then
         tp = tp / 8.64d+10
      elseif (ix .eq. 'ms') then
         tp = tp / 8.64d+07
      elseif (ix .eq. 's ') then
         tp = tp / 8.64d+04
      elseif (ix .eq. 'm ') then
         tp = tp / 1.44d+03
      elseif (ix .eq. 'h ') then
         tp = tp / 24.d0
      elseif (ix .eq. 'y ') then
         tp = tp * 365.25d0
      endif
      timest = tp
      return
      end 
