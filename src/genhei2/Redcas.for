C  Dose/Risk                Version Date:  15-Jun-1998
C----------------------------------------------------------------------
C
      SUBROUTINE REDCAS (SETDATA,NLINES)
C
C     This module reads the receptor input data set from the GID file.
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Creation Date:     15-Jun-98  BA Napier
C     Last Modification: 15-Jun-98  BA Napier
C     
C-----------------------------------------------------------------------
C
C     15 Jun 98   BAN  Establish as input for GENII dose/risk module
C      8 NOV 98   BAN  ADDED ABILITY TO READ F1, AMAD, ETC.
C      3 DEC 98   BAN  Converted 1/2/3 to F/M/S per UI input change
C     10 JAN 00   BAN  CORRECTION TO SOILT & SURCM INPUTS
C     24 NOV 01   BAN  ALLOWED V & G INHALATION CLASSES
C-----------------------------------------------------------------------
C
C  SETDATA(1000)  REAL U  Array of input images from the GID file
C  NLINES         INT  U  Number of lines in SETDATA
C
C------------------------------------------------------------------------
C
      INCLUDE 'ALLPAR.CMN'
c      INCLUDE 'DEVICE.CMN'
      INCLUDE 'OPT.CMN'
	INCLUDE 'RADATA.CMN'
	INCLUDE 'FGR13D.CMN'
C
      INTEGER GETINT
	CHARACTER*32 GETSTR
      REAL GETREAL
      LOGICAL GETLOG
      CHARACTER*80 SETDATA(1000)
	character*1 sol
C	DATA HECONINC /0.06/, HECONFAT /0.05/
c	DATA SOILT /0.15/, SLDN /224./
C
C---- Set default conditions -------------------------------------------
C     
      IZ = 0
C
C---- Read input options and general parameters ------------------------
C
      HEINC =  GETLOG(SETDATA,NLINES,'HEINC         ',IZ,IZ,IZ,IZ,IZ,IZ)
      HEFAT =  GETLOG(SETDATA,NLINES,'HEFAT         ',IZ,IZ,IZ,IZ,IZ,IZ)
      HECEDE = GETLOG(SETDATA,NLINES,'HECEDE        ',IZ,IZ,IZ,IZ,IZ,IZ)
	FGR13 =  GETLOG(SETDATA,NLINES,'FGR13         ',IZ,IZ,IZ,IZ,IZ,IZ)
C
	IHEAST = GETINT(SETDATA,NLINES,'IHEAST        ',IZ,IZ,IZ,IZ,IZ,IZ)
C
	HECONINC=GETREAL(SETDATA,NLINES,'HECONINC      ',IZ,IZ,IZ,IZ,IZ,   	
     . IZ)
	HECONFAT=GETREAL(SETDATA,NLINES,'HECONFAT      ',IZ,IZ,IZ,IZ,IZ,      
     . IZ)
	SOLT = GETREAL(SETDATA,NLINES,'SOILT         ',IZ,IZ,IZ,IZ,IZ,IZ)
	IF (SOLT .NE. 0.0) SOILT = SOLT
	SLD  = GETREAL(SETDATA,NLINES,'SLDN          ',IZ,IZ,IZ,IZ,IZ,IZ)
	IF (SLD .NE. 0.0) SLDN = SLD
C
C--------FGR13 RADIONUCLIDE DESCRIPTORS
C
	NUMCON = GETINT(SETDATA,NLINES,'NUMCON        ',IZ,IZ,IZ,IZ,IZ,IZ)
	DO J=1,NUMCON
	CASID(J)=GETSTR(SETDATA,NLINES,'CASID         ',J, IZ,IZ,IZ,IZ,IZ)

C	F1(J)  =GETREAL(SETDATA,NLINES,'F1            ',J, IZ,IZ,IZ,IZ,IZ)
	SOL = GETSTR(SETDATA, NLINES, 'SOLUBIL       ',J, IZ,IZ,IZ
     .                     ,IZ,IZ)
C	AMAD(J)=GETREAL(SETDATA,NLINES,'AMAD          ',J, IZ,IZ,IZ,IZ,IZ)
	if (sol .eq. "1") then 
	  solubil(j) = "F"
	else if (sol .eq. "2") then 
	  solubil(j) = "M"
	else if (sol .eq. "3") then 
	  solubil(j) = "S"
	else if (sol .eq. "4") then 
	  solubil(j) = "V"
	else if (sol .eq. "5") then 
	  solubil(j) = "G"
	else 
	  solubil(j) = " "
	end if
	END DO
C
C----- end of input ----------------------------------------------------------
C
      RETURN
C
C---- End of Module REDCAS ---------------------------------------------------
C
      END

