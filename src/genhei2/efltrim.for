*-----------------------------------------------------------------------
*
      character*(*) function ltrim(a)
*
*-----------------------------------------------------------------------
*     function: ltrim
*     author:  K.F. Eckerman
*     date:    10/23/93
*     purpose: trim leading blanks from string a
*
      character*(*) a
      logical ok
      ok = .false.
   10 if (a(1:1) .ne. ' ') ok = .true.
      if (.not. ok) then
         n = len_trim(a) - 1
         do i = 1, n
            a(i:i) = a(i+1:i+1)
         end do
         a(n+1:n+1) = ' '
         goto 10
      end if
      ltrim = a
      return
      end
