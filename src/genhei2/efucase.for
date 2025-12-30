*-----------------------------------------------------------------------------*
*                                                                             *
      character*(*) function ucase (a)
*                                                                             *
*-----------------------------------------------------------------------------*
*     author:   k. f. eckerman
*     date:     10/04/94
*     purpose:  convert character variable a to upper case.
*
      character*(*) a
      ucase = a
      do i = 1, len_trim(ucase)
         ix = ichar(ucase(i:i))
         if (ix .gt. 96 .and. ix .lt. 123) then
            ucase(i:i) = char(ix - 32)
         end if
      end do
      return
      end
