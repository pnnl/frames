c ------------------------------------------------------------------------------
      subroutine indecr(iyr,ijul,ihr,idelt,ihrmin,ihrmax,idiag)
c ------------------------------------------------------------------------------
c
c --- PMERGE   Version: 1.2       Level: 901130                    INDECR
c ---          J. Scire, SRC
c
c --- Increment or decrement a date/time by "IDELT" hours
c --- (-24 <= IDELT <= 24)
c --- Allows specification of 0-23 or 1-24 hour clock
c
c --- INPUTS:
c               IYR - integer    - Input Year
c              IJUL - integer    - Input Julian day
c               IHR - integer    - Input hour (ihrmin <= IHR <= ihrmax)
c             IDELT - integer    - Change in time (hours) -- must be
c                                  between -24 to +24, inclusive
c            IHRMIN - integer    - Minimum hour (i.e., either  0 or  1)
c            IHRMAX - integer    - Maximum hour (i.e., either 23 or 24)
c
c --- OUTPUT:
c               IYR - integer    - Year after change of "IDELT" hours
c              IJUL - integer    - Julian day after change of "IDELT" hours
c               IHR - integer    - Hour after change of "IDELT" hours
c
c --- INDECR called by:  PREAD
c --- INDECR calls:      none
c ------------------------------------------------------------------------------
      io6=idiag
c
      if(iabs(idelt).gt.24)then
         write(io6,10)'IDELT',iyr,ijul,ihr,idelt,ihrmin,ihrmax
10       format(/1x,'ERROR in subr. INDECR -- invalid "',a,'" -- ',
     1   ' iyr,ijul,ihr,idelt,ihrmin,ihrmax = ',6i10)
         stop 1
      endif
      if(ihr.lt.ihrmin.or.ihr.gt.ihrmax)then
         write(io6,10)'IHR',iyr,ijul,ihr,idelt,ihrmin,ihrmax
         stop 1
      endif
c
      if(idelt.lt.0)then
c ---    idelt is negative
         ihr=ihr+idelt
         if(ihr.lt.ihrmin)then
            ihr=ihr+24
            ijul=ijul-1
            if(ijul.lt.1)then
               iyr=iyr-1
               if(mod(iyr,4).eq.0)then
                  ijul=366
               else
                  ijul=365
               endif
            endif
         endif
      else
c ---    idelt is positive or zero
         ihr=ihr+idelt
         if(ihr.gt.ihrmax)then
            ihr=ihr-24
            ijul=ijul+1
            if(mod(iyr,4).eq.0)then
               ndays=366
            else
               ndays=365
            endif
            if(ijul.gt.ndays)then
               ijul=1
               iyr=iyr+1
            endif
         endif
      endif
c
      return
      end

