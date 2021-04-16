CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     
C     HRLY.INC
C
C     Description:  The common block variables for HRLYPROC
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      REAL*8   rndseed      

      REAL     afv(24), fvr(24), temp(24), wspeed(24), precip(24),
     &         ustr(24), mixhgt(24), p3240(24), wndspd(24)
     
      REAL     sunris, sunset, lat, long, z0meas, z0appl, amhgt
      
      INTEGER  idir(24), prpcode(24), isyr(24), ismo(24), isdy(24),
     &         ishr(24), icover(24), iceil(24), iprec(5,24), 
     &         kspeed(24), kst(24), idvar(21), iwxcod(9,24)
      
      INTEGER  iotd, timzon, iosfc, lwd, iout, idiag, nvars
      
      LOGICAL  recok(24)
      LOGICAL  td32flg
      
      CHARACTER*5 acd144(12,24)
      CHARACTER*1 td32form
	  CHARACTER*1 tilde
      CHARACTER*6 sfctyp
      CHARACTER*9 jsamsn(26,24)
      CHARACTER*80 outfile, sfcfile, td32file
      
      COMMON /HRLY/
     &         rndseed, 
     &         afv, fvr, temp, wspeed, precip,
     &         ustr, mixhgt, p3240, wndspd,
     &         sunris, sunset, lat, long, z0meas, z0appl, amhgt,
     &         idir, prpcode, isyr, ismo, isdy,
     &         ishr, icover, iceil, iprec, 
     &         kspeed, kst, idvar,  iwxcod,
     &         iotd, timzon, iosfc, lwd, iout, idiag, nvars,
     &         recok,
     &         td32flg,
     &         acd144,
     &         td32form,tilde,
     &         sfctyp,   
     &         jsamsn,
     &         outfile, sfcfile, td32file