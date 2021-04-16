      SUBROUTINE JULIAN(IYR,IMO,IDY,JDAY,ITYPE)
C***********************************************************************
C*    JULIAN Utility Module
C*
C*    PURPOSE:    Converts dates between Julian and mo/day format
C*
C*    INPUTS:     Year, month, day   OR  Julian day and year
C*                ITYPE: 0 - CONVERT IMO/IDY TO JDAY
C*                       1 - CONVERT JDAY TO IMO/IDY
C*
C*    OUTPUTS:    Julian day and year   OR  year, month, day
C*        
C*    CALLED FROM: Various subprograms
C***********************************************************************

C*    Variable Declarations
      dimension iofset(13,2)
  
      data iofset/
     $ 0,31,59,90,120,151,181,212,243,273,304,334,365,
     $ 0,31,60,91,121,152,182,213,244,274,305,335,366/
C
C**** SEE IF WE ARE IN A LEAP YEAR
C
      if(mod(iyr,4).eq.0) then
         ileap = 2
      else
         ileap = 1
      endif
C
C**** SEE WHICH DIRECTION WE ARE GOING
C
      if(itype.eq.0) then
         jday = iofset(imo,ileap) + idy
      else
         do 10 j=2,13
            if(jday.le.iofset(j,ileap)) then
               imo = j-1
               idy = jday - iofset(imo,ileap)
               go to 999
            endif
   10    continue
      endif
  999 continue
      return
      end
