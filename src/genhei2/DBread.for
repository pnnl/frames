C Subroutine to obtain dose conversion factors from database portion of GID file, "con#"
C
C
      SUBROUTINE DBread (CONNID,IERR)
C     This module reads the database data set from the GID file.
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Creation Date:     29-Oct-2001  BA Napier
C     Last Modification: 3-Nov-2002   BA Napier
C     
C-----------------------------------------------------------------------
C
C     15 Jun 98   BAN  Establish as input for GENII dose/risk module
C     3 Nov 2002  BAN  Select maximum DF from Database.  CHANGE THIS LATER!!
C ---------
C
C  SETDATA(linemax)  REAL U  Array of input images from the GID file
C  NLINES            INT  U  Number of lines in SETDATA
C  conName           Label for GID section with data
C
C------------------------------------------------------------------------
C   PARMTR.PAR HAS linemax
C   DEVICE.CMN HAS NGID
C   DFACTR.CMN HAS DFA, DFG, DSH, DIMR, DEX
C   FNAMES.CMN HAS NSITE
C   RCOINFO HAS conName
	INCLUDE 'PARMTR.PAR'
	INCLUDE 'DEVICE.CMN'
	INCLUDE 'DFACTR.CMN'
	INCLUDE 'FNAMES.CMN'
	INCLUDE 'RCPINFO.CMN'
C
      INTEGER GETINT
	CHARACTER*32 GETSTR
	CHARACTER*12 CAS
	CHARACTER*12 CONNID
      REAL GETREAL
      LOGICAL GETLOG, FILTER
      CHARACTER*80 SETDATA(LINEMAX)

	IERR = 0
C      
C----- Zero toxicity parameters  ------------
C      
      CPFH = 0.0
      CPFG = 0.0
      CPFS = 0.0
      DFG = 0.0
      DFA = 0.0
	DFA1 = 0.0
	DFA2 = 0.0
	DFA3 = 0.0
	DFG1 = 0.0
	DFG2 = 0.0
      DIMR= 0.0 
      DSH = 0.0
      DEX = 0.0

C
C---- Start of Analysis
C    MOVESET finds the location of the first record on the set, and the 
C    number of lines to be read (NLINES)
C
      IF (MOVESET(NGID,conName,NLINES)) THEN
C      
C----- Set name was found in the .GID file.  Read a data set ---------------
C
        IF(NLINES .LE. 0. OR. NLINES.GT. linemax) THEN   ! Test NLINES for range
          WRITE(NERR,100) NLINES
  100     FORMAT(' Error in con# specification of NLINES: ',I5)
          IERR = IERR+1
	    GO TO 20
        ELSE    
C---- Set default conditions -------------------------------------------
C     
      IZ = 0
	Ic = NSITE
	FILTER = .FALSE.
C
C---- Read input parameters --------------------------------------------
C	
	CALL GETSET(NGID, NERR, CONNAME, NLINES, SETDATA, NSITE, FILTER)
C
C ------ Determine which pollutant we are dealing with, set indices
C
      NUMCN=GETINT(SETDATA,NLINES,'NUMCON        ',NSITE,IZ,IZ,IZ,IZ,IZ)
	DO I = 1, NUMCN
      CAS=GETSTR(SETDATA,NLINES,'FSCASID       ',NSITE,I,IZ,IZ,IZ,IZ)
      ND=GETINT(SETDATA,NLINES,'NDS           ',NSITE,I,IZ,IZ,IZ,IZ)
	  IF(CAS.EQ.CONNID(1:12)) THEN
	  IN=I
	  IJ=0
	  GO TO 10
	  END IF
	  IF (ND. GT. 0) THEN
	  DO J = 1, ND
         IJ = J
         CAS=GETSTR(SETDATA,NLINES,'FSCASID       ',NSITE,I,IJ,IZ,IZ,IZ)
         ND=GETINT(SETDATA,NLINES,'NDS           ',NSITE,I,IJ,IZ,IZ,IZ)
         IF(CAS.EQ.CONNID(1:12)) THEN
	   IN=I
	   GO TO 10
	   END IF
	  END DO
	  END IF
      END DO
	GO TO 20
C
C  Set properties for PARENT OR progeny------------------------------------
C----- Set dose factor for radionuclides ------
   10 Continue  
C
C  Right now we are using the maximum of all possibilities
C
	DFA1=GETREAL(SETDATA,NLINES,'CLDFAD        ',IC,IN,IJ,IZ,IZ,IZ)
	DFA2=GETREAL(SETDATA,NLINES,'CLDFAW        ',IC,IN,IJ,IZ,IZ,IZ)
	DFA3=GETREAL(SETDATA,NLINES,'CLDFAY        ',IC,IN,IJ,IZ,IZ,IZ)
      DFA = DFA1
C
C     icrp-30 CORRECTION FOR NEWTRIT HT CONVERSION TO HTO
	IF (CAS .EQ. 'H3EL') DFA = DFA*641.
C
	If (DFA2 .gt. DFA) DFA=DFA2
      IF (DFA3 .gt. DFA) DFA=DFA3
	DFG1=GETREAL(SETDATA,NLINES,'CLRDFGS       ',IC,IN,IJ,IZ,IZ,IZ)
      DFG2=GETREAL(SETDATA,NLINES,'CLRDFGI       ',IC,IN,IJ,IZ,IZ,IZ)
      DFG = DFG1
	If (DFG2 .gt. DFG) DFG=DFG2
	DEX=GETREAL(SETDATA,NLINES,'CLDEX         ',IC,IN,IJ,IZ,IZ,IZ)
	DIMR=GETREAL(SETDATA,NLINES,'CLDIMR        ',IC,IN,IJ,IZ,IZ,IZ)
      DSH=GETREAL(SETDATA,NLINES,'CLDSH         ',IC,IN,IJ,IZ,IZ,IZ)                          
C
C----- Use slope factors for radionuclides 
C        
      CPFH = GETREAL(SETDATA,NLINES,'CLsfh         ',IC,IN,IJ,IZ,IZ,IZ)
      CPFG = GETREAL(SETDATA,NLINES,'CLsfg         ',IC,IN,IJ,IZ,IZ,IZ)
      CPFS = GETREAL(SETDATA,NLINES,'CLSFEX        ',IC,IN,IJ,IZ,IZ,IZ)
      FONE = 1.0
	FONE = GETREAL(SETDATA,NLINES,'CLFONEI       ',IC,IN,IJ,IZ,IZ,IZ)
C
      END IF
      END IF
   20 CONTINUE
	RETURN
	END