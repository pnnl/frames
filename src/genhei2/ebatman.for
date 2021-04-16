*----------------------------------------------------------------------*
*                                                                      *
      subroutine batman(b0, zk, zkt, an1, an2, t, n)
*                                                                      *
*----------------------------------------------------------------------*
*
      include 'pakparm.cmn'
*
*     call variables.
      double precision b0, zk, zkt, an1, an2, zero
      integer n
      dimension b0(mspec), zkt(mspec), zk(mspec)
*     local variables.
      double precision s1, s2, ss1, ss2, prod, expfun, expf1
      integer i, j, k
      parameter (zero=0.0d0)
      include 'iolist.cmn'
      an1 = zero
      an2 = zero
      do 50 i = 1, n
        if (b0(i) .ne. zero) then
           s1 = zero
           s2 = zero
           ss1 = zero
           ss2 = zero
           do j = i, n
              prod = zkt(n) / zk(n) * zk(j) / zkt(i)
              do k = i, n
                 if (k .ne. j) prod = prod * zk(k) / (zkt(k) - zkt(j))
              end do
              if (prod .lt. zero) then
                 s1 = s1 + dabs(prod) * expfun(-zkt(j) * dble(t))
                 ss1 = ss1 + dabs(prod) * expf1(zkt(j), dble(t))
              else
                 s2 = s2 + prod * expfun(-zkt(j) * dble(t))
                 ss2 = ss2 + prod * expf1(zkt(j), dble(t))
              end if
           end do
*
*          only positive values are retained; negatives are zero
*
           if (s2 .gt. s1)   an1 = an1 + b0(i) * (s2 - s1)
           if (ss2 .gt. ss1) an2 = an2 + b0(i) * (ss2 - ss1)
        end if
*
   50 continue
*
      return
      end
