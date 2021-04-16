C  neshaps                Version Date:  15-JAn-2003
C----------------------------------------------------------------------
C
      SUBROUTINE REDCAS(SETDATA,NLINES, FacNam, FacStrt, FacCity, UsrNam 
     . ,PFilNam, METHOD, IremSv, Ipthnuc, FOODFILE, ifodprd)
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
c     6 Sept 2012 BAN  Resized PFilNam to 80
C-----------------------------------------------------------------------
C
C  SETDATA(1000)  REAL U  Array of input images from the GID file
C  NLINES         INT  U  Number of lines in SETDATA
c
C
C------------------------------------------------------------------------
C
C
      INTEGER GETINT, NLINES, iremsv,ipthnuc,ifodprd
	CHARACTER*80 GETSTR, FOODFILE, PFilNam
      CHARACTER*120 SETDATA(1000)
	CHARACTER*32 FacNam, FacStrt, FacCity, UsrNam
C---- Set default conditions -------------------------------------------
C     
      IZ = 0
C
C---- Read input options and general parameters ------------------------
C
      IremSv = getint(setdata,nlines,'IremSv        ',iz,iz,iz,iz,iz,iz)
	Ipthnuc= getint(setdata,nlines,'Ipthnuc       ',iz,iz,iz,iz,iz,iz)
	Ifodprd=getint(setdata,nlines,'Ifodprd       ',iz,iz,iz,iz,iz,iz)
      METHOD = GETINT(SETDATA,NLINES,'METHOD        ',IZ,IZ,IZ,IZ,IZ,IZ)
      IF(METHOD .NE. 0)THEN
      FacNam = GETSTR(SETDATA,NLINES,'FacNam        ',IZ,IZ,IZ,IZ,IZ,IZ)
      FacStrt= GETSTR(SETDATA,NLINES,'FacStrt       ',IZ,IZ,IZ,IZ,IZ,IZ)
	FacCity= GETSTR(SETDATA,NLINES,'FacCity       ',IZ,IZ,IZ,IZ,IZ,IZ)
	UsrNam = GETSTR(SETDATA,NLINES,'UsrNam        ',IZ,IZ,IZ,IZ,IZ,IZ)
	IF(METHOD .EQ. 2)THEN
	PFilNam= GETSTR(SETDATA,NLINES,'PFilNam       ',IZ,IZ,IZ,IZ,IZ,IZ)
	end if
	if (ifodprd .ne. 0) then
	FOODFILE=GETSTR(SETDATA,NLINES,'FOODFILE      ',IZ,IZ,IZ,IZ,IZ,IZ)
	END IF
	END IF
C

C
C----- end of input ----------------------------------------------------------
C
      RETURN
C
C---- End of Module REDCAS ---------------------------------------------------
C
      END

