C-----------------------------------------------------------------------
C
C     METINPUT.INC
C
C     Date: 2/24/98
C
C     Description:   Contains the Structure for Meteorological 
C                    Input Variables
C
C-----------------------------------------------------------------------
      
      STRUCTURE /MetInput/
         INTEGER  year
         INTEGER  month
         INTEGER  day
         INTEGER  hour
         INTEGER  stab
         REAL     wnddir
         REAL     wndspd
         REAL     ustar
         REAL     tempk
         REAL     tempc
         REAL     mixhgt
         INTEGER  pcode
         REAL     prate
         REAL     wght
         LOGICAL  missmet
         LOGICAL  jfdmet
         LOGICAL  end
      END STRUCTURE
      