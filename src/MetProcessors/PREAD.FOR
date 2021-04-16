c ------------------------------------------------------------------------------
      subroutine pread(io,ndate,maxap,iflag,cdat,iprev,
     1 icode,ibdat,iedat,pmmsav,idiag)
c ------------------------------------------------------------------------------
c
c --- PMERGE   Version: 1.2       Level: 921022                    PREAD
c ---          J. Scire, SRC
c
c --- Read a precipitation record -- if necessary, read a second record
c --- to resolve a missing data or accumulation period
c
c --- INPUTS:
c                IO - integer    - Fortran unit no. of precip. input
c                                  file
c             NDATE - integer    - Coded date/time field (YYJJJHH) of
c                                  current hour
c             MAXAP - integer    - Maximum allowed length (hrs) of an
c                                  accumulation period
c             IFLAG - integer    - Flag indicating precip. data status:
c                                  IFLAG = -99 if this is the first pass
c                                              for this station
c                                  IFLAG =   0 if date/hr of first
c                                              precip. record has not
c                                              been reached yet (data is
c                                              assumed missing up to
c                                              date/hr of first valid
c                                              record)
c                                  IFLAG =  +1 if current date/hr >
c                                              date/hr of first record
c                                              (precip. rate is assumed
c                                              = 0.0 between time of
c                                              valid records)
c              CDAT - char.*42   - A character string to store a TD-3240
c                                  data record (an input only if IFLAG = 0)
c             IPREV - integer    - Coded date/time field (YYJJJHH) of
c                                  previously read TD-3240 record
c
c --- OUTPUT:
c             ICODE - integer    - Data status code:
c                                  1 = valid hourly value,
c                                  2 = valid accumulation period,
c                                  3 = missing data flag (labeled missing)
c                                  4 = missing due to excessive length
c                                      of accumulation period
c                                  5 = missing data before first valid
c                                      record in file
c                                  6 = missing data after last valid
c                                      record in file
c             IBDAT - integer    - Beginning date/time of data (YYJJJHH)
c             IEDAT - integer    - Ending date/time of data (YYJJJHH)
c            PMMSAV - real       - Precipitation rate (mm/hr)
c                                  (missing value indicator = 9999.)
c              CDAT - char.*42   - A character string storing a TD-3240
c                                  data record (an output only if
c                                  IFLAG = -99)
c             IPREV - integer    - Updated coded date/time field of
c                                  last TD-3240 record read
c
c --- PREAD called by:  UNCDP
c --- PREAD calls:   JULIAN
c                    INDECR
c ------------------------------------------------------------------------------
c
      character*42 cdat
      character*1 cflag,cflag2
      cflag=' '
      cflag2=' '
      io6=idiag

      if(iflag.le.0)then
c
c ---    If first time through for this station, read TD-3240 record &
c ---    store in character string (CDAT)
         if(iflag.eq.-99)then
            read(io,18)cdat
18          format(a42)
            iflag=0
c
c ---       extract station id, date/hr
            read(cdat,20)idsta,iyr,imo,iday,ihr
20          format(3x,i6,10x,i2,i2,i4,3x,i2,2x,i6,a1)
            call julian(iyr,imo,iday,ijul,0)
            idate=iyr*100000+ijul*100+ihr
            iprev=idate
c
c
c ---       all data up to time of first record in file is considered
c ---       as zero
            icode=5
            ibdat=0
c ---       subtract one hour from yr/Julian day/hr
            call indecr(iyr,ijul,ihr,-1,1,24,idiag )
            iedat=iyr*100000+ijul*100+ihr
            pmmsav=0.
            return
         endif
c
c ---    extract data from a previously read character string
         read(cdat,20,end=995)idsta,iyr,imo,iday,ihr,ihinch,cflag
         iflag=1
         call julian(iyr,imo,iday,ijul,0)
         idate=iyr*100000+ijul*100+ihr
c

      else
c
c ---    read a new record
         read(io,20,end=995)idsta,iyr,imo,iday,ihr,ihinch,cflag
         call julian(iyr,imo,iday,ijul,0)
         idate=iyr*100000+ijul*100+ihr
c
c
c ---    check if date/hr of record is out of order
         if(idate.gt.iprev)then
            iprev=idate
         else
            go to 1040
         endif
      endif
c
      if(cflag.eq.' ' .or. cflag.eq.'E'  .or. cflag .eq. 'e')then
c
c ---    valid hourly data value -- convert to mm/hr
         icode=1
         ibdat=idate
         iedat=idate
         pmmsav=0.254*float(ihinch)
         return
      else if(cflag.eq.'A' .or.  cflag .eq. 'a')then
c
c ---    beginning of accumulation period -- read next record
c ---    with ending date/time of accumulation period & accum. amount
         read(io,20,end=996)jdsta,jyr,jmo,jday,jhr,jhinch,cflag2
         call julian(jyr,jmo,jday,jjul,0)
         jdate=jyr*100000+jjul*100+jhr
         if(jdate.gt.iprev)then
            iprev=jdate
         else
            go to 1050
         endif
c
         if(cflag2.ne.'A' .and. cflag2 .ne. 'a')then
c
c ---       ERROR -- unpaired accumulation period
            go to 1000
         else
c
c ---       paired accumulation records -- resolve accumulation
c ---       period precip. rate
            call deltt(iyr,ijul,ihr,jyr,jjul,jhr,idelt)
            nhrs=idelt+1
c
c ---       if length of the accumulation period exceeds max.
c ---       allowed, consider data as missing
            if(nhrs.gt.maxap)then
               icode=4
               ibdat=idate
               iedat=jdate
               pmmsav=9999.
               return
            endif
c
c ---       valid accumulation period -- resolve & save results
            icode=2
            ibdat=idate
            iedat=jdate
            pmmsav=0.254*float(jhinch)/float(nhrs)
            return
         endif
c
      else if(cflag.eq.'M' .or. cflag.eq.'D' .or.
     &        cflag.eq.'m' .or. cflag.eq.'d')then
c ---    beginning of missing or deleted data period -- read next record
c ---    with ending date/time of missing or deleted data period
         read(io,20,end=998)jdsta,jyr,jmo,jday,jhr,jhinch,cflag2
         call julian(jyr,jmo,jday,jjul,0)
         jdate=jyr*100000+jjul*100+jhr
         if(jdate.gt.iprev)then
            iprev=jdate
         else
            go to 1050
         endif
c
c **     if(cflag2.ne.'M' .and. cflag2 .ne. 'm')then
         if(cflag2.ne.cflag)then
c
c ---       ERROR -- unpaired missing or deleted data records
            go to 1030
         else
c
c ---       paired missing data records
            icode=3
            ibdat=idate
            iedat=jdate
            pmmsav=9999.
            return
         endif
      endif
c
c --- invalid precipitation flag encountered
      write(io6,990)idsta,ndate,io,iyr,imo,iday,ihr,cflag,cflag2
990   format(/1x,'Error in subr. PREAD -- invalid ',
     1 'precipitation flag encountered'//1x,'ID = ',i6,3x,
     2 //1x,'Requested date/hr (YYJJJHH) = ',i7,3x,'io = ',i5
     3 //1x,'Yr: ',i2,2x,'Month: ',i2,2x,'Day: ',i2,
     4 2x,'Hr: ',i2//1x,'CFLAG = ',a1,3x,'CFLAG2 = ',a1)
c      CALL ERRHDL('E','0000',2)
c
c --- end of file encountered
995   continue
      icode=6
      ibdat=0
      iedat=9999999
      pmmsav=0.
      return
c
c --- end of file encountered -- unpaired accumulation period
996   continue
      write(io6,997)idsta,ndate,io,iyr,imo,iday,ihr,cflag
997   format(/1x,'Error in subr. PREAD -- unpaired ',
     1 'accumulation period'//1x,'ID = ',i6,3x,
     2 //1x,'Requested date/hr (YYJJJHH) = ',i7,3x,'io = ',i5
     3 //1x,'1st record: ',2x,'Yr: ',i2,2x,'Month: ',i2,2x,
     4 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1
     5 //1x,'2nd record: ',2x,'END OF FILE REACHED')
c      CALL ERRHDL('E','0000',2)
c
c --- end of file encountered -- unpaired missing or deleted period
998   continue
      write(io6,999)idsta,ndate,io,iyr,imo,iday,ihr,cflag
999   format(/1x,'Error in subr. PREAD -- unpaired ',
     1 'missing or deleted data period'//1x,'ID = ',i6,3x,
     2 //1x,'Requested date/hr (YYJJJHH) = ',i7,3x,'io = ',i5
     3 //1x,'1st record: ',2x,'Yr: ',i2,2x,'Month: ',i2,2x,
     4 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1
     5 //1x,'2nd record: ',2x,'END OF FILE REACHED')
c      CALL ERRHDL('E','0000',2)
c
c --- write error message -- unpaired accumulation period
1000  continue
      write(io6,1002)idsta,ndate,io,iyr,imo,iday,ihr,cflag,
     1 jyr,jmo,jday,jhr,cflag2
1002  format(/1x,'Error in subr. PREAD -- unpaired ',
     1 'accumulation period'//1x,'ID = ',i6,3x,
     2 //1x,'Requested date/hr (YYJJJHH) = ',i7,3x,'io = ',i5
     3 //1x,'1st record: ',2x,'Yr: ',i2,2x,'Month: ',i2,2x,
     4 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1
     5 //1x,'2nd record: ',2x,'Yr: ',i2,2x,'Month: ',i2,2x,
     6 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1)
c      CALL ERRHDL('E','0000',2)
c
c --- write error message -- unpaired missing data period
1030  continue
      write(io6,1032)idsta,ndate,io,iyr,imo,iday,ihr,cflag,
     1 jyr,jmo,jday,jhr,cflag2
1032  format(/1x,'Error in subr. PREAD -- unpaired ',
     1 'missing or deleted data period'//1x,3x,
     2 //1x,'Requested date/hr (YYJJJHH) = ',i7,3x,'io = ',i5
     3 //1x,'1st record: ',2x,'Yr: ',i2,2x,'Month: ',i2,2x,
     4 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1
     5 //1x,'2nd record: ',2x,'Yr: ',i2,2x,'Month: ',i2,2x,
     6 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1)
c      CALL ERRHDL('E','0000',2)
c
c --- write error message -- invalid date/hr ( <= previous value)
1040  continue
      write(io6,1042)idsta,ndate,io,iyr,imo,iday,ihr,cflag,
     1 idate,iprev
1042  format(/1x,'Error in subr. PREAD -- invalid date/hr ( <= ',
     1 'previous value)'//1x,'ID = ',i6,3x,
     2 //1x,'Requested date/hr (YYJJJHH) = ',i7,3x,'io = ',i5
     3 //1x,'Yr: ',i2,2x,'Month: ',i2,2x,
     4 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1
     5 //1x,'Date/hr (YYJJJHH) = ',i10,'  (Current record)'
     6 //1x,'Date/hr (YYJJJHH) = ',i10,' (Previous record)')
c      CALL ERRHDL('E','0000',2)
c
1050  continue
      write(io6,1042)jdsta,ndate,io,jyr,jmo,jday,jhr,cflag,
     1 jdate,iprev
c      CALL ERRHDL('E','0000',2)

      end

