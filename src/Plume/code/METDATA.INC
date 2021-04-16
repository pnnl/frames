ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     METDATA.INC
c
c     Contains:  The meteo data and other parameters
c
c     Revision 30 March 2006 BAN to add MINWIND
c                                                                       
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      REAL*8   rndseed
      
      REAL     calmdis(36)
      
      REAL     atemp, mixhgt, molinv, prate, relspd,  
     &         ustr, wght, wnddir, wndmin, z0, minwind 
     
      INTEGER  ptyp, sigparm, stab, wndsec, iyr, imo, ida, ihr
                                           
      LOGICAL  usercalm
                                           
      COMMON /metdata/
     &         rndseed,
     &         calmdis, 
     &         atemp, mixhgt, molinv, prate, relspd,   
     &         ustr, wght, wnddir, wndmin, z0,  
     &         ptyp, sigparm, stab, wndsec, iyr, imo, ida, ihr,
     &         usercalm, minwind  
      