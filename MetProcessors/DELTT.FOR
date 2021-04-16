c-----------------------------------------------------------------------
      subroutine deltt(j1yr,j1jul,j1hr,j2yr,j2jul,j2hr,jleng)
c-----------------------------------------------------------------------
c
c --- PMERGE   Version: 1.2       Level: 901130                    DELTT
c ---          J. Scire, SRC
c
c --- Compute the difference (in hours) between two dates & times
c ---    (time #2 - time #1)
c
c --- INPUTS:
c              J1YR - integer    - Year of date/time #1
c             J1JUL - integer    - Julian day of date/time #1
c              J1HR - integer    - Hour of date/time #1
c              J2YR - integer    - Year of date/time #2
c             J2JUL - integer    - Julian day of date/time #2
c              J2HR - integer    - Hour of date/time #2
c
c --- OUTPUT:
c             JLENG - integer    - Difference (#2 - #1) in hours
c
c --- DELTT called by:  PREAD
c --- DELTT calls:      none
c-----------------------------------------------------------------------
c
      jmin=min0(j1yr,j2yr)
c
c --- find the number of hours between Jan. 1 of the "base" year and
c --- the first date/hour
      if(j1yr.eq.jmin)then
         j1=0
      else
         j1=0
         j1yrm1=j1yr-1
         do 10 i=jmin,j1yrm1
         if(mod(i,4).eq.0)then
            j1=j1+8784
         else
            j1=j1+8760
         endif
10       continue
      endif
      j1=j1+(j1jul-1)*24+j1hr
c
c --- find the number of hours between Jan. 1 of the "base" year and
c --- the second date/hour
      if(j2yr.eq.jmin)then
         j2=0
      else
         j2=0
         j2yrm1=j2yr-1
         do 20 i=jmin,j2yrm1
         if(mod(i,4).eq.0)then
            j2=j2+8784
         else
            j2=j2+8760
         endif
20       continue
      endif
      j2=j2+(j2jul-1)*24+j2hr
c
c --- compute the time difference (in hours)
      jleng=j2-j1
c
      return
      end
