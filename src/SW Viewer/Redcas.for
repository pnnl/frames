C  neshaps                Version Date:  15-JAn-2003
C----------------------------------------------------------------------
C
      SUBROUTINE REDCAS(SETDATA,NLINES, FacNam, FacStrt, FacCity, UsrNam 
     . ,METHOD, PFilNam, IremSv, Ipthnuc)
C
C     This module reads the receptor input data set from the GID file.
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Creation Date:     15-Jun-98  BA Napier
C     Last Modification: 15-JAN-03  BA Napier
C     
C-----------------------------------------------------------------------
C
C     15 JAn 03   BAN  Establish as input for GENII NESHAPS module
C-----------------------------------------------------------------------
C
C  SETDATA(1000)  REAL U  Array of input images from the GID file
C  NLINES         INT  U  Number of lines in SETDATA
c
C
C------------------------------------------------------------------------
C
C
      INTEGER GETINT, NLINES
	CHARACTER*80 GETSTR
      CHARACTER*120 SETDATA(1000)
	CHARACTER*80 FacNam, FacStrt, FacCity, UsrNam, PFilNam
C---- Set default conditions -------------------------------------------
C     
      IZ = 0
C
C---- Read input options and general parameters ------------------------
C
      IremSv = getint(setdata,nlines,'reportunits   ',iz,iz,iz,iz,iz,iz)
	Ipthnuc= getint(setdata,nlines,'printdetails  ',iz,iz,iz,iz,iz,iz)
      METHOD = GETINT(SETDATA,NLINES,'exposuretype  ',IZ,IZ,IZ,IZ,IZ,IZ)
      IF(METHOD .NE. 2)THEN
      FacNam = GETSTR(SETDATA,NLINES,'FacName       ',IZ,IZ,IZ,IZ,IZ,IZ)
      FacStrt= GETSTR(SETDATA,NLINES,'FacAddress    ',IZ,IZ,IZ,IZ,IZ,IZ)
	FacCity= GETSTR(SETDATA,NLINES,'FacCityState  ',IZ,IZ,IZ,IZ,IZ,IZ)
	UsrNam = GETSTR(SETDATA,NLINES,'FacUserName   ',IZ,IZ,IZ,IZ,IZ,IZ)
	IF(METHOD .EQ. 1)THEN
	PFilNam= GETSTR(SETDATA,NLINES,'filename      ',IZ,IZ,IZ,IZ,IZ,IZ)
	END IF
	END IF
C
C----- end of input ----------------------------------------------------------
C
      RETURN
C
C---- End of Module REDCAS ---------------------------------------------------
C
      END

