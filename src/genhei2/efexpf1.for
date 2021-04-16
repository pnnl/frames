*-----------------------------------------------------------------------------* 
*                                                                             *
      double precision function expf1 (lm, t) 
*                                                                             *
*-----------------------------------------------------------------------------* 
*     author:   k. f. eckerman
*     date:     10/04/94
*     purpose:  routine to compute [1.0 - exp(-lm * t)] / lm. 
*
      double precision lm, t, lmt, one, two, expfun, eps
      logical first
      parameter (one = 1.0d0, two = 2.0d0)
      data first/ .true./
      if (first) then
         eps = one
  10     eps = eps / two
         if (eps + one .gt. one) goto 10
         eps = dsqrt(eps)
         first = .false.
      end if
      lmt = lm * t
      if (lmt .lt. eps) then
         expf1 = t * (one - lmt / two)
      else
         expf1 = (one - expfun(-lmt)) / lm
      end if 
      return
      end 
