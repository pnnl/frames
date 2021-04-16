c ------------------------------------------------------------------------------
      SUBROUTINE UNCDP( io, ndate, maxap, k, pmm, icode, idiag )
c ------------------------------------------------------------------------------

c
c*******sample call *********************************
c            NDATE = IYEAR*100000+JULIAN*100+IHOUR  *
c            CALL UNCDP(INPPT,NDATE,24,1,PMM,ICODE) *
c****************************************************
c
c --- PMERGE   Version: 1.2       Level: 901130                    UNCDP
c ---          J. Scire, SRC
C*
C*
C*    Modified: Jayant A. Hardikar, PES
C*              Made it relatively generic
C*              2/15/95
C*
c
c --- Determine the precipitation rate for a given date/hour by:
c        (a) reading a character variable storing a previously read
c            record
c        (b) reading a precipitation record from a TD-3240 file
c        (c) resolving an accumulation period
c        (d) resolving a missing data period
c
c --- INPUTS:
c                IO - integer    - Fortran unit no. of precip. input
c                                  file
c             NDATE - integer    - Coded date/time field (YYJJJHH) of
c                                  current hour
c             MAXAP - integer    - Maximum allowed length (hrs) of an
c                                  accumulation period
c                 K - integer    - Array index of precip. arrays
c                                  (K = station no.)
c --- OUTPUT:
c               PMM - real       - Precipitation rate (mm/hr)
c                                  (missing value indicator = 9999.)
c             ICODE - integer    - Data status code:
c                                  1 = valid hourly value,
c                                  2 = valid accumulation period,
c                                  3 = missing data flag (labeled missing),
c                                  4 = missing due to excessive length
c                                      of accumulation period,
c                                  5 = missing data before first valid
c                                      record in file
c                                  6 = missing data after last valid
c                                      record in file
c
c --- UNCDP called by:  READPP
c --- UNCDP calls:      PREAD
c
c --- IFLAG -- Flag indicating precip. data status:
c              IFLAG = -99 if this is the first pass for this station
c              IFLAG =   0 if date/hr of first precip. record has not
c                          been reached yet (data is assumed missing
c                          up to date/hr of first valid record)
c              IFLAG =  +1 if current date/hr > date/hr of first record
c                          (precip. rate is assumed = 0.0 between time
c                          of valid records)
c ------------------------------------------------------------------------------
      parameter(maxps=1)
c
      real pmmsav(maxps)
      integer icodsv(maxps),ibdat(maxps),iedat(maxps),iflag(maxps)
      integer iprev(maxps)
      character*42 cdat(maxps)
c
      data icodsv/maxps*0/,ibdat/maxps*0/,iedat/maxps*0/
      data iflag/maxps*-99/,cdat/maxps*' '/
      data iprev/maxps*0/

c     Set the unit log file unit number, io6, to idiag
      io6=idiag
c
c --- determine if current date/hr is within range previously stored
c --- pmmsav array
10    CONTINUE
      if(ndate.lt.ibdat(k))then
c
c ---    Date/hr between valid records -- precip. rate = 0.0
         pmm=0.0
         icode=1
         return
      else if(ndate.le.iedat(k))then
c
c ---    Current date/hr is within period of validity of pmmsav
         pmm=pmmsav(k)
         icode=icodsv(k)
         return
      endif
c
c --- Current date/hr is after end of period of validity of pmmsav
25    CONTINUE
      CALL PREAD( io, ndate, maxap, iflag(k), cdat(k), iprev(k),
     1            icodsv(k), ibdat(k), iedat(k), pmmsav(k), idiag )
      GO TO 10
      
      END
