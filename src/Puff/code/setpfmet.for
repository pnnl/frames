      SUBROUTINE  SETPFMET( newmet, amhgt, acute, PrgStat )
C-----------------------------------------------------------------------
C
C     SETPFMET.FOR
C
C     Date:    2/23/98
C
C     Description:   Sets the meteorological variables used in
C                    puff model to the values in structure newmet
C
C----------------------------------------------------------------------- 
      IMPLICIT       NONE

      INCLUDE        'parm.inc'
      INCLUDE        'met_data.inc'      
      INCLUDE        'metinput.inc'

      RECORD         /metinput/ newmet
      
      REAL*4         mol, INVMOL, USTAR, amhgt

      CHARACTER*50   PrgStat

      LOGICAL        acute
      
      iyr = newmet.year
      imo = newmet.month
      ida = newmet.day
      ihr = newmet.hour
      stab = newmet.stab
      flow_vec = newmet.wnddir
      mol = INVMOL(stab,z0sta)
      ustr = USTAR(amhgt,newmet.wndspd,z0sta,mol,stab,acute,PrgStat)

      IF( PrgStat .NE. ' ' ) RETURN
      
      temp = newmet.tempc
      mixhgt = newmet.mixhgt
      pcode = newmet.pcode
      prate = newmet.prate
      
      RETURN
      
      END