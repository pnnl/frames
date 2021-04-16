*-----------------------------------------------------------------------
*
      integer function ibinry ( target )
*
*-----------------------------------------------------------------------
*     function: ibinry
*     author:   k.f. eckerman
*     date:     06/20/88
*     purpose:  locate record sort by target key
*
      integer nstart, nlast, left, right, try
      character*7 target, a1
      logical first
      save first, nstart, nlast
      include 'iolist.cmn'
*
*     initialization.
*
      data first /.true./
      if (first) then
         read (idex, '(2i4)', rec = 1) nstart, nlast
         first = .false.
      end if
      left = nstart
      right = nlast
*
*     begin attempts to find target.
*
   10 try = int((left + right) / 2)
      read (idex, '(a7)', rec = try) a1
      if (a1 .lt. target) then
         left = try + 1
      elseif (a1 .gt. target) then
         right = try - 1
      else
         ibinry = try
         return
      end if
*
*     continue search unless left > right then set ibinry to zero and
*     let calling deal with the unidentified target.
*
      if (left .lt. right + 1) then
         goto 10
      else
         ibinry = 0
         return
      end if
      end
