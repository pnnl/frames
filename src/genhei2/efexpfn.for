*-----------------------------------------------------------------------------*
*                                                                             *
      double precision function expfun (t) 
*                                                                             *
*-----------------------------------------------------------------------------*
*     author:   k. f. eckerman
*     date:     10/04/94
*     purpose:  routine to compute exp (t). 
*  
      double precision t, zero, upval 
      parameter(zero = 0.0d0, upval = 180.d0)
      if (t .lt. -upval) then
         expfun = zero 
      else
         expfun = dexp(t)
      end if
      return 
      end 
