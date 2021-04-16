*----------------------------------------------------------------------*
*                                                                      *
      integer function icutoff(eat, ebt, egt, igamma, nspec)
*                                                                      *
*----------------------------------------------------------------------*
      dimension eat(*), ebt(*), egt(*)
      ea = eat(nspec)
      eb = ebt(nspec)
      eg = egt(nspec)
      if (ea .gt. 0. .and. igamma .eq. 0) then
         do i = nspec-1, 1, -1
            if (eat(i) .lt. 0.99 * ea) then
               ia = i + 1
               goto 15
            end if
         end do
         ia = 1
      else
         ia = 0
      end if
   15 if (eb .gt. 0.) then
         do i = nspec-1, 1, -1
            if (ebt(i) .lt. 0.99 * eb) then
               ib = i + 1
               goto 25
            end if
         end do
         ib = 1
      else
         ib = 0
      end if
   25 if (eg .gt. 0.) then
         do i = nspec-1, 1, -1
            if (egt(i) .lt. 0.99 * eg) then
               ig = i + 1
               goto 35
            end if
         end do
         ig = 1
      else
         ig = 0
      end if
   35 icutoff = max0(ig, max0(ia, ib))
      return 
      end
