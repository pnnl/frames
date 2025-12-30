*-----------------------------------------------------------------------------*
*                                                                             *
      character*(*) function lcase (a)
*                                                                             *
*-----------------------------------------------------------------------------*
*     author:   k. f. eckerman
*     date:     10/04/94
*     purpose:  convert character variable a to lower case.
*
      character*(*) a
      lcase = a
      do i = 1, len_trim(lcase)
         ix = ichar(lcase(i:i))
         if (ix .gt. 64 .and. ix .lt. 91) then 
            lcase(i:i) = char(ix + 32)
         end if
      end do
      return
      end
