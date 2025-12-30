C    HEALTH.FOR                     Version Date: 05-Jun-98                  *
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.     *
C*****************************************************************************
C                                                                            *
C                     PROGRAM  HEALTH                                        *
C                                                                            *
C  Program HEALTH controls calculation of DOSE AND RISK for specified        * 
C  receptor locations.  Input and output use files from the Framework        *
C  control program definition (.GID and RIF) with output to file HIF.        *
C                                                                            *
C  Written by:       BA NAPIER                                               *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    05-JUN-98                                               *
C  Last Modified:    05-JUN-98      BAN                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: HEALTH  main program
C     Called by: None
C     Calls: 
C     Common blocks referenced: 
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     CONID     S/U     CHR    LOCAL     Contaminant id from FUI set in GID
C     CID       S/U     CHR    SWINFO    Contaminant id from WFF file
C     GIDFNM     U      CHR    FNAMES    File prefix for .GID file
C     LINMAX     U      CHR    PARAMETER Maximum number of lines allowed in 
C                                        the interal array SETDATA for data
C                                        sets taken from the GID file
C     NERR       U      INT    DEVICE    Logical unit for ERR file
C     NERROR    S/U     INT              Error counter
C     NGID       U      INT    DEVICE    Logical unit for GID file
C     NRECPT     U      INT    FNAMES    Index number of receptor to use 
C     NSITE      U      INT    FNAMES    Index number of site to be used
C     NUMEXP     U      INT    EXINFO    Number of exposure location to be used
C     NHIF       U      INT    DEVICE    Logical unit for HIF file (output
C     NRIF       U      INT    DEVICE    Logical unit for RIF file (input)
C     RUNFNM     U      CHR    FNAMES    File name for run files and data set
C                                        name in GID
C     SETDATA    S      CHR    GID file  Storage array for GID data set 
C
C==== Modification History ===================================================
C     Date     Who  Modification Description
C     -------- ---  ----------------------------------------------------------
C   05-Jun-98  BAN  Initial programming started
c   29-Oct-01  BAN  Added conName to read from FUI
C   19 Nov 2003 BAN Changed HeiRcp, HeiRcpNum to HeiSrcName, HeiSrcNum to fit FRAMES 1.4
C
C==== SUBROUTINE CALL ========================================================
C
      PROGRAM HEALTH
C
C---- Include Statements for Parameter and Common Declarations ---------------
C      
C      INCLUDE 'AFLAGS.CMN'
C      INCLUDE 'AGES.CMN'
	INCLUDE 'ALLPAR.CMN'
C      INCLUDE 'ANMPAR.CMN'
C      INCLUDE 'CONIN.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'RCPINFO.CMN'
      INCLUDE 'FNAMES.CMN'
C      INCLUDE 'FODPAR.CMN'
C      INCLUDE 'FLUX.CMN'
C      INCLUDE 'NUCNAM.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'RADATA.CMN'
	INCLUDE 'SETF.CMN'
C      INCLUDE 'SOLPAR.CMN'
C      INCLUDE 'TIMES.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
	real ta1, ta2
      INTEGER GETINT, heiMAX, HEISNUM, NEXPSRC(10)
      INTEGER EXPNUM, TPATH, indxage, INDX2
      CHARACTER*12 RNAMES,RNAME
      CHARACTER*32 GETSTR, HEISRCNA(10), EXPNAM
      CHARACTER*32 heiNAM    
      CHARACTER*80 SETDATA(LINEMAX)
      CHARACTER*14 FUI, MEDTYPE
      CHARACTER*1 B, C
      LOGICAL FILTER
      LOGICAL FOUND, FOUNDM, FOUNDR, ACUTE
	LOGICAL SEQI, FGR11
c SOME STUFF TO COMPILE IT WITH AHAZ BOTTOM END
	LOGICAL  WARN,FOUNDE
	CHARACTER*20 CONAME
	CHARACTER*22 EXNAME, EXPTYPE
	CHARACTER*7 EU, STYPE
	CHARACTER*32 ELOC
	CHARACTER*4 RISK, DOSE
	CHARACTER*6 UT, UR
	CHARACTER*22 CANCER, INTAKE, RADOSE, CONINT, OUTYPE, CONAMEP
	CHARACTER*16 RADOUT(4)
	CHARACTER*12 EXROUTE, CONIDP
	CHARACTER*13 TYPES(6)
	CHARACTER*9 ONAMES(24),CNAMES(15),allsite
	character*10 TBODY
	INTEGER CTYPE
	DATA RADOUT(1) /'cancer incidence'/
	DATA RADOUT(2) /'fatal cancer    '/
	DATA RADOUT(3) /'                '/
	DATA RADOUT(4) /'radiation dose  '/
	DATA DOSE /'Sv  '/
	DATA TYPES(1)/'Aquifer      '/
	DATA TYPES(2)/'Surface water'/
	DATA TYPES(3)/'Air          '/
	DATA TYPES(4)/'Air          '/
	DATA TYPES(5)/'Soil         '/
	DATA TYPES(6)/'Foods        '/
	DATA INTAKE /'intake                '/
	data RADOSE /'radiation dose        '/
	DATA CONINT /'concentration         '/
	DATA NDOSE /2/
	DATA NO/16/
C---- Data Statements --------------------------------------------------------
C
      DATA FUI/'FUI           '/
      DATA IZ/0/
	DATA B/' '/
	DATA TBODY/'total body'/
	DATA ALLSITE/'all sites'/
	DATA ONAMES/'Adrenals ','Bld Wall ','B Surface','Brain    ',  
     .             'Breasts  ','Esophagus','St Wall  ','SI Wall  ',
     .             'ULI Wall ','LLI Wall ','Kidneys  ','Liver    ',
     .             'Lungs    ','Muscle   ','Ovaries  ','Pancreas ',
     .             'R Marrow ','Skin     ','Spleen   ','Testes   ',
     .             'Thymus   ','Thyroid  ','Uterus   ','Effective'/
	DATA CNAMES/'Esophagus','Stomach  ','Colon    ','Liver    ',
     .             'Lung     ','Bone     ','Skin     ','Breast   ',
     .             'Ovary    ','Bladder  ','Kidneys  ','Thyroid  ',
     .             'Leukemia ','Residual ','Total    '/


C
C---- Initialize parameter values --------------------------------------------
C
C
C---- Get file names ---------------------------------------------------------
C
      CALL GETNAM
      NGIDNM = INDEX(GIDFNM,B) - 1
      IF(NGIDNM.LE.0) NGIDNM = 8
      NRUNAM = INDEX(RUNFNM,B) - 1
      IF(NRUNAM.LE.0) NRUNAM = 128
C
C----- Open files for this run -----------------------------------------------
C  
C-------- Global Input Data file (.GID) 
C 
      OPEN (UNIT=NGID,FILE=GIDFNM(1:NGIDNM)//'.GID',STATUS='OLD')
C
C
C-------- Run Error Message file (.ERR)
C
      OPEN (UNIT=NERR,FILE=RUNFNM(1:NRUNAM)//'.ERR',STATUS='UNKNOWN')
C
C-------- Run Output listing file (.HLS)
C
      OPEN (UNIT=NhLS,FILE=RUNFNM(1:NRUNAM)//'.HLS',STATUS='UNKNOWN')
C
C-------- Health component PDCF listing file (.HIF) -----------------------
C
      OPEN (UNIT=NHIF,FILE=RUNFNM(1:NRUNAM)//'.HIF',STATUS='UNKNOWN')
C
C----- Write run argument values to HLS report file --------------------------
C
      WRITE(NHLS,111) GIDFNM,RUNFNM,NSITE,NUMhei,GLFNAME
 111  FORMAT(' GIDFNM  =',A70/
     .       ' RUNFNM  =',A70/
     .       ' NSITE   =',I3/
     .       ' NUMhei  =',I3/
     .       ' GLFNAME =',A32)
C
C----- Get Framework User Interface data from GID file -----------------------     
C
      FILTER = .TRUE.
      CALL GETSET(NGID,NERR,FUI,NLINES,SETDATA,NSITE,FILTER)
      FILTER = .FALSE.
C
C----- Was the FUI data set found? -------------------------------------------
C
      IF(NLINES.LE.0) THEN
        WRITE(NERR,2211) NLINES
 2211   FORMAT(' The FUI data set was not found, NLINES =',I5)
        NERROR = NERROR + 1
        GO TO 999
      ENDIF
C
C----- Get name of "site" (e.g. SCENARIO 1)-----------------------------------
C
      IS = NSITE                 ! Number of sites from ID file
	I1 = 1                     ! FRAMES has a second index (unused??)
      SITNAM = GETSTR(SETDATA,NLINES,'SiteName      ',IS,IZ,IZ,IZ,IZ,IZ)
      conName =GETSTR(SETDATA,NLINES,'conName       ',IS,I1,IZ,IZ,IZ,IZ)
	WRITE(NHLS,112) SITNAM, conName
 112  FORMAT(' SiteName = ',A32, 'Database name =', A)
C
C----- Get number of HEI Glyphs---------------------------------------
C
C      
      HEIMAX = GETINT(SETDATA,NLINES,'HeiNum        ',IS,IZ,IZ,IZ,IZ,IZ)
      IF(HEIMAX.LT.NUMHei.OR.NUMHei.LE.0) THEN
        WRITE(NERR,2212) NUMHei, HEIMAX
 2212   FORMAT(' Error in specification of NUMBER OF receptorS'/
     .         ' Value given: ',I3,'  Maximum allowed: ',I3)
        NERROR = NERROR + 1
        GO TO 999
      ENDIF
C
C----- Get name of THIS HEALTH IMPACT GLYPH ----------------------------
	IH = NUMHEI
      HEINAM = GETSTR(SETDATA,NLINES,'HeiName       ',IS,IH,IZ,IZ,IZ,IZ)
      WRITE(NHLS,113) HEINAM
 113  FORMAT(' HeiName  = ',A32)
C
C----- Get number of receptor glyphs feeding into this one -------------
C
C      HEISNUM=GETINT(SETDATA,NLINES,'HeiRcpNum     ',IS,IH,IZ,IZ,IZ,IZ)
      HEISNUM=GETINT(SETDATA,NLINES,'HeiSrcNum     ',IS,IH,IZ,IZ,IZ,IZ)
      WRITE(NHLS,116) HEISNUM
 116  FORMAT(' HeiRcpNum  = ',I3)
      IF(HEISNUM.LE.0) THEN
        WRITE(NERR,2213) HEISNUM
 2213   FORMAT(' Error in specification of number of receptor GLYPHS '/
     .         ' Value must be greater than zero.  Found: ',I3)
        NERROR = NERROR + 1
        GO TO 999
      ENDIF
C
C----- Identify NAMES for the receptor Glyphs -----------------------
C
      DO IT = 1,HeiSNUM
C        HeiSRCNA(IT)=GETSTR(SETDATA,NLINES,'HEIRCP        ',IS,IH,IT,IZ,
        HeiSRCNA(IT)=GETSTR(SETDATA,NLINES,'HEISrcName    ',IS,IH,IT,IZ,
     .                IZ,IZ)
      END DO
C
C
C----- Get data set for this glyph ------------------------------------
C
      CALL GETSET(NGID,NERR,HEINAM,NLINES,SETDATA,NSITE,FILTER)
      WRITE(NHLS,1115) HEINAM,NLINES
 1115 FORMAT(' Health risk data set sought in GID: ',A16,' lines =',i4)
C
C----- Was the Run data set found? -------------------------------------------
C
      IF(NLINES.LE.0) THEN
        WRITE(NERR,2214) NLINES
 2214   FORMAT(' The Run data set was not found, NLINES =',I5)
         NERROR = NERROR + 1
        GO TO 999
      ENDIF
C
C----- Call REDCAS to read parameter values for this run ---------------------
C
      CALL REDCAS(SETDATA,NLINES)
C
C----- Call HIFHEAD to WRITE heading information to the HIF PDCF file -------
c
	nr=1
       CALL HIFHEAD(NR,HEISNUM,HEISRCNA)
C
C----- Evaluate each RECEPTOR source for this HEALTH GLYPH----
C
      DO IR = 1,HEISNUM
        NUMNUC = 0
C
C-------- Open run RECEPTOR INTAKE file (.RIF) ---------------------------
C
      OPEN (UNIT=NRIF,FILE=GIDFNM(1:NGIDNM)//'.RIF',STATUS='UNKNOWN')
C
C----- read beginning of RIF file and position to desired data set -----------
C
        CALL MARKIN(HEIsrcNa(ir),foundm)
          IF(.NOT.FOUNDM) THEN
             IERR = IERR + 1
             GO TO 999
          ENDIF
c
c----- Write lines describing special conditions for this run ------------
c
            IF(IHEAST.LE.0) THEN
              NHEOUT = 0
              IF (FGR13) THEN
			  WRITE(nhls,21040)
	        ELSE
	          WRITE(nhls,21041)
	        END IF
              IF(HEINC) THEN
                WRITE(nhls,2108)
                NHEOUT = 1
              ENDIF
              IF(HEFAT) THEN
                WRITE(nhls,2110)
                NHEOUT = NHEOUT + 1
              ENDIF
              IF(HECEDE) THEN
                WRITE(nhls,2109)
                NHEOUT = NHEOUT + 1
              ENDIF
            ELSE IF (FGR13 .LE. 0) THEN
              WRITE(nhls,2105)
              HEINC = .TRUE.
              HEFAT = .FALSE.
              HEFSH = .FALSE.
C              HECEDE= .FALSE.
              IF(HEINC) WRITE(NHLS,2108)
              WRITE(NHLS,2112)
              NHEOUT = 1
              IF(HECEDE) NHEOUT = NHEOUT + 1
            ENDIF
C
C----- Output option format statements ---------------------------------------
C
21040   FORMAT(' Radiation dose factors from EPA Federal Guidance ',
     .         ' Reports No. 11 and 13 are used.')
21041   FORMAT(' Radiation dose factors from EPA Federal Guidance ',
     .         ' Reports No. 11 and 12 are used.')
 2105   FORMAT(' Slope factors from EPA HEAST reports are used for',
     .         ' radiation risk calculations, when available.')
 2109   FORMAT(' Radionuclide consequenses will be reported as ',
     .           'radiation dose (Sv).')
 2108   FORMAT(' Radionuclide consequenses will be reported as ',
     .           'cancer incidence risk.')
 2110   FORMAT(' Radionuclide consequenses will be reported as ',
     .           'cancer fatality risk.')
 2112   FORMAT(' Because EPA HEAST slope factors have been selected,'/
     .         '   the only risk reported will be cancer incidence.',
     .         '   ALL OTHER RISK OUTPUTS HAVE BEEN DISABLED.')
C
C---- Read header lines for this set -----------------------------------------
C
        READ(NRIF,*) NLNS
        DO IL = 1,NLNS
           READ(NRIF,*) C
        END DO
        C = C
C
C---- Read number of data sets to consider -----------------------------------
C
c       READ(NRIF,*) NSETS,STYPE
       READ(NRIF,*) NSETS
c       NS = LENWORD(STYPE,7)
c       WRITE(NHIF,7003) NSETS,STYPE(1:NS)
c 7003  FORMAT(I3,',"',A,'",')
c       IF(NSETS.LE.0) THEN
c         WRITE(NERR,2001) NSETS
c 2001    FORMAT(' Error in RIF file, number of data sets bad =',i5)
c         IERR = IERR + 1
c         GO TO 999
c       ENDIF
       WRITE(NHIF,7003) NSETS
 7003  FORMAT(I3,',')
       IF(NSETS.LE.0) THEN
         WRITE(NERR,2001) NSETS
 2001    FORMAT(' Error in RIF file, number of data sets bad =',i5)
         IERR = IERR + 1
         GO TO 999
       ENDIF
c       IF(STYPE.NE.'chronic') THEN
c          WRITE(NERR,2002) STYPE
c 2002     FORMAT(' Error in RIF file, type not chronic: ',A)
c          IERR = IERR + 1
c          GO TO 999
c       ENDIF
C
C---- Loop on data sets in RIF file ------------------------------------------
C
 88   continue
      DO IS = 1,NSETS
C
C---- Read first line of RIF data set and test for inclusion -----------------
C
        READ(NRIF,*,end=995) STYPE,ELOC,MEDTYPE,NPOINTS,NAGES,NCONST
        NS = LENWORD(STYPE,7)
        NE = LENWORD(ELOC,32)
        NM = LENWORD(MEDTYPE,14)
          IF(STYPE.NE.'chronic') THEN
            WRITE(NERR,2002) STYPE
 2002       FORMAT(' Error in RIF file, type not chronic: ',A)
            IERR = IERR + 1
            GO TO 999
          ENDIF
C
	   IF ((HEINC .OR. HECEDE .OR. HEFAT) .AND. (.NOT. FGR13)) THEN
            NORG=1
	      NCAN=1
         ELSE IF (FGR13) THEN
	      NCAN=15
	      NORG=24
         END IF
C
        WRITE(NHIF,7004) ELOC(1:NE),MEDTYPE(1:NM),NPOINTS,NAGES,NCONST,
     .                   NCAN,NORG
 7004   FORMAT('"chronic","',A,'","',A,'",'I3,',',I2,',',I3,
     .         ',',I3,',',I3,',')
C
C----WRITE HARD-WIRED ORGAN AND CANCER SITE NAMES HERE
C----THIS IS BEFORE THEY ARE READ FROM ECKERMAN'S FILES -ASSUMED CONSTANT!
C
	   IF ((HEINC .OR. HECEDE .OR. HEFAT) .AND. (.NOT. FGR13)) THEN
            WRITE(NHIF,7121)ALLSITE
	      WRITE(NHIF,7122)TBODY
         ELSE IF (FGR13) THEN
	      WRITE(NHIF,7123) (CNAMES(I), I=1,15)
	      WRITE(NHIF,7124) (ONAMES(I), I=1,24)
         END IF
 7121   FORMAT('"',A9,'",')
 7122   FORMAT('"',A10,'",')
 7123   FORMAT('"',A9,'","',A7,'","',A5,'","',A5,'","',A4,'","',A4,
     .       '","',A4,'","',A6,'","',A5,'","',A7,'","',A7,'","',A7,'","',
     .             A8,'","',A8,'","',A5,'",')
 7124   FORMAT('"',A8,'","',A8,'","',A9,'","',A5,'","',A7,'","',A9,
     .       '","',A7,'","',A7,'","',A8,'","',A8,'","',A7,'","',A5,'","',
     .             A5,'","',A6,'","',A7,'","',A8,'","',A8,'","',A4,'","',
     .             A6,'","',A6,'","',A6,'","',A7,'","',A6,'","',A9,'",')
C
C---- Identify transport medium type, MEDTYPE --------------------------------
C
        TPATH = 0
        DO IT = 1,6
           IF(SEQI(MEDTYPE,TYPES(IT),12)) THEN
              TPATH = IT
           END IF
        END DO
        IF(TPATH.EQ.4) TPATH = 3
C
C----- Write error message for bad medium type -------------------------------
C
        IF(TPATH.LE.0) THEN
           WRITE(NERR,2005) MEDTYPE
 2005      FORMAT(' Error in RIF, medium type not recognized: ',a)
           IERR = IERR +1
           GO TO 999
        ENDIF
C
C---- Read coordinate data ---------------------------------------------------
C
        DO IP = 1,NPOINTS
          READ(NRIF,*) EXPX(IP),UT,EXPY(IP),UR
          WRITE(NHIF,7000) EXPX(IP),EXPY(IP)
 7000     FORMAT(F9.1,',"km",',f9.1,',"km",')
        END DO
C
C---- read age group break points for current age group ----------------------
C
	  DO IAGE = 1, NAGES
          READ(NRIF,*) TA1,TA2
          WRITE(NHIF,7006) TA1,TA2
 7006     FORMAT(F5.0,',',F5.0,',"yr",')
          DELTIM = TA2 - TA1
C----  ALGORITHM FOR AGE GROUP INDEX
	    CALL AGEidx(TA1, TA2, INDXAGE, INDX2)
C
C---- Do for each constituent in RIF file ------------------------------------
C
          DO IPOL = 1,NCONST
C
C---- Read constituent name, progeny and number of time periods ----
C
            READ(NRIF,*) CONAME,CONID,NPRG,NTIMES
C
	      IF ((HEINC .OR. HECEDE .OR. HEFAT) .AND. (.NOT. FGR13))
     .    CALL OLD(CONAME,NPRG,NTIMES,DELTIM,NHEOUT,NPOINTS,TPATH)
C
	      IF (FGR13) CALL NEW (CONAME, NPRG, NTIMES, INDXAGE, NHEOUT, 
     .	                   NPOINTS, TPATH,deltim, INDX2)
C	      IF (FGR11) CALL NEW2 (...)
C
          END DO  ! Loop on constituents
	  END DO  ! LOOP ON AGE GROUPS
      END DO  !  Loop on number of sets in RIF file, IS
C
      CLOSE(NRIF)
 50   CONTINUE  ! Loop on number of receptors for health GLYPH
C
C
995   ENDFILE (nhls)
      CLOSE (nhls)
	ENDFILE (NERR)
      IF (NERROR .LE. 0) THEN
	  CLOSE (NERR, STATUS = 'DELETE')
	ELSE
	  CLOSE (NERR)
	ENDIF
	ENDFILE (NHIF)
	CLOSE (NHIF)
   85 CONTINUE
      GO TO 999
C
	ENDDO
  999 CONTINUE
      STOP
      END
C=================== END OF MODULE HEALTH ====================================
