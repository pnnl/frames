C--------------------------------------------------------------------------
C
C     MET_DATA.INC
C     
C     Date:  10/5/95
C
C     Description:  Meteorological grid include file
C
C     Included in modules:  CHKMET, CLOSIN, CPLUME, DATRD, DATWR, DIFDEP,
C                           DOSVDIST, FCTRD, GRIDIN, INTPMET, MODPRMIN, 
C                           PRE_PUFF, PREPLUME, PUFF_EFF, PUFFM, PUFFR, 
C                           SPECIN, SWITCH, TADMOD2, TIMESTEP, VERDIST,
C                           WCSPLUME, WNDPLUME     
C
C--------------------------------------------------------------------------


      REAL     Gz0(Max_X,Max_Y)
      
      REAL     flow_vec, ldepth, mixhgt, prate, spd10, temp, 
     &         ustr, z0sta
      
      INTEGER  pcode, stab, iyr, imo, ida, ihr 

      COMMON /MET_DATA/ 
     &         Gz0,
     &         flow_vec, ldepth, mixhgt, prate, spd10, temp, 
     &         ustr, z0sta,
     &         pcode, stab, iyr, imo, ida, ihr