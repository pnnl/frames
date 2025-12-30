*----------------------------------------------------------------------*
*                                                                      *
      subroutine birch(imem, t, rx1, rx2)
*                                                                      *
*----------------------------------------------------------------------*
*
      include 'pakparm.cmn'
      include 'dcfpak.cmn'
      integer mpath, max 
      common/calcul/ max(mspec), mpath(mspec, mspec) 
*     call variables.
      integer imem
*     local variables.
      integer i, j, mark, jpath, ipath, nmem, m
      double precision zkt, zk, b, b0, an1, an2, x1, x2, zerod
      dimension b(mspec), b0(mspec), zkt(mspec), zk(mspec), mark(mspec), 
     :          jpath(mspec), IPATH(mspec)
      parameter(zero = 0.0, zerod = 0.0d0)
*
*     Trace the pathway backwards from Imem to decide which elements
*     of the Mpath matrix to choose.
*
      rx1 = zero
      rx2 = zero
      x1 = zerod
      x2 = zerod
      do i = 1, nspec
        mark(i) = 1
        b(i) = dble(branch(i, i))
      end do
   31 nmem = 1
      jpath(1) = imem
*
      if (max(imem) .eq. 0) goto 35
   33   imem = mpath(mark(imem), imem)
        nmem = nmem + 1
        jpath(nmem) = imem
        if (max(imem) .gt. 0) goto 33
   35 do i = 1, nmem
       IPATH(i) = jpath(nmem - i + 1)
      end do
      imem = IPATH(nmem)
      do i = 1, nmem
         b0(i) = b(IPATH(i))    
         zkt(i) = lmr(IPATH(i))
         if (i .lt. nmem) then 
            zk(i) =  dble(branch(IPATH(i), IPATH(i + 1))) * zkt(i)
         else
            zk(i) = zkt(i)
         end if
      end do
      call batman(b0, zk, zkt, an1, an2, t, nmem)
      x1 = x1 + an1
      x2 = x2 + an2
   60 do 80 i = 1, nmem
        b(IPATH(i)) = zerod
        if (i .gt. 1) then
          if (mark(IPATH(i)) .ne. max(IPATH(i))) then
            m = IPATH(i)
            mark(m) = mark(m) + 1
            do j = 1, m - 1
              mark(j) = 1
              b(j) = dble(branch(j, j))
            end do
            goto 31
          end if
        end if
   80 continue
      imem = IPATH(nmem)
      rx1 = sngl(x1)
      rx2 = sngl(x2)
      return
      end
