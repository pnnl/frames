C Subroutine to obtain dose conversion factors from database portion of GID file, "con#"
C
C
      SUBROUTINE DBreadA (CHNAM,NOCHM)
C     This module reads the database data set from the GID file.
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Creation Date:     30-Oct-2001  BA Napier
C     Last Modification: 30-Oct-2001  BA Napier
C     
C-----------------------------------------------------------------------
C
C     30 Oct 2001   BAN  Establish as input for GENII chronic module
C     11 Sept 2002  BAN  Catch zero soil half-times for leach rate
C ---------
C
C  SETDATA(linemax)  REAL U  Array of input images from the GID file
C  NLINES            INT  U  Number of lines in SETDATA
C  conName           Label for GID section with data
C
C------------------------------------------------------------------------
C   PARMTR.PAR HAS linemax, lenchain
C   DEVICE.CMN HAS NGID
C   FNAMES.CMN HAS NSITE
C   EXINFO.CMN HAS conName
	INCLUDE 'PARMTR.PAR'
	INCLUDE 'DEVICE.CMN'
	INCLUDE 'FNAMES.CMN'
	INCLUDE 'EXINFO.CMN'
	INCLUDE 'LEACH.CMN'
	INCLUDE 'FODPAR.CMN'
	INCLUDE 'SOLPAR.CMN'
	INCLUDE	'ANMPAR.CMN'
	INCLUDE 'AQUPAR.CMN'
	INCLUDE 'OPT.CMN'
C
      INTEGER GETINT, IG
	CHARACTER*32 GETSTR
	CHARACTER*12 CAS
	CHARACTER*12 CHNAM(chainlen)
      REAL GETREAL
      LOGICAL GETLOG, FILTER
      CHARACTER*80 SETDATA(LINEMAX)
C
	IERR = 0
C      
C----- Zero parameters  ------------
      Do IN = 1, NoChM
          DPVL(IN) = 0.
          IF(LCHOPT.LE.1) LEACHR(IN) = 0.
          BVI(1,IN) = 0.
          BVI(2,IN) = 0.
          BVI(3,IN) = 0.
          BVI(4,IN) = 0.
          FMI(1,IN) = 0.
          FMI(2,IN) = 0.
          FMI(3,IN) = 0.
          FMI(4,IN) = 0.
          FMI(5,IN) = 0.
          FMI(6,IN) = 0.
          BIOACF(1,IN) = 0.
          BIOACF(2,IN) = 0.
          BIOACF(3,IN) = 0.
          BIOACF(4,IN) = 0.
          DWCLEN(IN) = 0  
      END DO
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
C
C---- Identify parent in CON# data set in GID  ----------------------
C     Read number of constituents (parents) in GID exposure data set
C
      NUMCON = GETINT(SETDATA,NLINES,'NumCon          ',Ic,IZ,IZ,IZ,
     .                IZ,IZ)
      DO IN = 1,NUMCON
C
C---- Get current parent name and test against input name, CHNAM(1) ---------
C
          CAS = GETSTR(SETDATA,NLINES,'FSCASID       ',Ic,In,IZ,IZ,
     .                IZ,IZ)
        IF(CAS.EQ.CHNAM(1)) THEN   ! found parent in list
C
C---- Get number of progeny for this parent ----------------------------------
C
          NDTS = GETINT(SETDATA,NLINES,'NDS           ',Ic,In,IZ,IZ,
     .                 IZ,IZ)
C
C---- Get transfer factors for parent --------------------------------------------
C
          DPVL(1)=GETREAL(SETDATA,NLINES,'CLVD          ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          IF(LCHOPT.LE.1) LEACHR(1) = GETREAL(SETDATA,NLINES,
     .                               'CLSHALF       ',IC,IN,IZ,IZ,IZ,IZ)
	    IF((LCHOPT .LE. 1) .and. (LEACHR(1) .gt. 0.0)) 
     .                                LEACHR(1) = 0.693/(LEACHR(1)/365.)
          BVI(1,1) = GETREAL(SETDATA,NLINES,'CLBVLV        ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          BVI(2,1) = GETREAL(SETDATA,NLINES,'CLBVOV        ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          BVI(3,1) = GETREAL(SETDATA,NLINES,'CLBVFR        ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          BVI(4,1) = GETREAL(SETDATA,NLINES,'CLBVCL        ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          FMI(1,1) = GETREAL(SETDATA,NLINES,'CLFMT         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          FMI(2,1) = GETREAL(SETDATA,NLINES,'CLFPL         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          FMI(3,1) = GETREAL(SETDATA,NLINES,'CLFMK         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          FMI(4,1) = GETREAL(SETDATA,NLINES,'CLFEG         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          FMI(5,1) = GETREAL(SETDATA,NLINES,'CLFMT         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          FMI(6,1) = GETREAL(SETDATA,NLINES,'CLFMK         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          IF (ISALT) THEN
		BIOACF(1,1) = GETREAL(SETDATA,NLINES,'CLBMF         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          BIOACF(2,1) = GETREAL(SETDATA,NLINES,'CLBMM         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          BIOACF(3,1) = GETREAL(SETDATA,NLINES,'CLBMI         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          BIOACF(4,1) = GETREAL(SETDATA,NLINES,'CLBMP        ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
	    ELSE
          BIOACF(1,1) = GETREAL(SETDATA,NLINES,'CLBFF         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          BIOACF(2,1) = GETREAL(SETDATA,NLINES,'CLBFM         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          BIOACF(3,1) = GETREAL(SETDATA,NLINES,'CLBFI         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          BIOACF(4,1) = GETREAL(SETDATA,NLINES,'CLBFP         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
	    END IF
          DWCLEN(1) = GETREAL(SETDATA,NLINES,'CLWPF         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
C
C---- Get PARAMETERS for progeny, if any ------------------------------------
C
          IF(NoChM.GT.1) THEN
C
C---- Search all progeny for chain members from master list ------------------
C
            DO 10 IP = 2,NoChM
              IF(NDTS.GT.0) THEN
C
C---- Find current progeny in list from GID file -----------------------------
C
               DO IGN = 1,NDTS
                 IG = IGN
         CAS = GETSTR(SETDATA,NLINES,'FSCASID       ',Ic,IN,IG,IZ,IZ,IZ)
                 IF(CAS.EQ.CHNAM(IP)) THEN
C
C----- If progeny found in GID, then transfer data ----------
C
          DPVL(IP)=GETREAL(SETDATA,NLINES,'CLVD          ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          IF(LCHOPT.LE.1) LEACHR(IP) = GETREAL(SETDATA,NLINES,
     .                               'CLSHALF       ',IC,IN,IG,IZ,IZ,IZ)
	    IF((LCHOPT .LE. 1) .and. (LEACHR(IP) .gt. 0.0))
     .                              LEACHR(IP) = 0.693/(LEACHR(IP)/365.)
          BVI(1,IP) = GETREAL(SETDATA,NLINES,'CLBVLV        ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          BVI(2,IP) = GETREAL(SETDATA,NLINES,'CLBVOV        ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          BVI(3,IP) = GETREAL(SETDATA,NLINES,'CLBVFR        ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          BVI(4,IP) = GETREAL(SETDATA,NLINES,'CLBVCL        ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          FMI(1,IP) = GETREAL(SETDATA,NLINES,'CLFMT         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          FMI(2,IP) = GETREAL(SETDATA,NLINES,'CLFPL         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          FMI(3,IP) = GETREAL(SETDATA,NLINES,'CLFMK         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          FMI(4,IP) = GETREAL(SETDATA,NLINES,'CLFEG         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          FMI(5,IP) = GETREAL(SETDATA,NLINES,'CLFMT         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          FMI(6,IP) = GETREAL(SETDATA,NLINES,'CLFMK         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          IF (ISALT) THEN
		BIOACF(1,IP) = GETREAL(SETDATA,NLINES,'CLBMF         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          BIOACF(2,IP) = GETREAL(SETDATA,NLINES,'CLBMM         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          BIOACF(3,IP) = GETREAL(SETDATA,NLINES,'CLBMI         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          BIOACF(4,IP) = GETREAL(SETDATA,NLINES,'CLBMP        ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
	    ELSE
          BIOACF(1,IP) = GETREAL(SETDATA,NLINES,'CLBFF         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          BIOACF(2,IP) = GETREAL(SETDATA,NLINES,'CLBFM         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          BIOACF(3,IP) = GETREAL(SETDATA,NLINES,'CLBFI         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          BIOACF(4,IP) = GETREAL(SETDATA,NLINES,'CLBFP         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
	    END IF
          DWCLEN(IP) = GETREAL(SETDATA,NLINES,'CLWPF         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
C
C
                 ENDIF
               END DO
              ENd IF
  10        CONTINUE
          END IF  !   IF progeny
C
        ENDIF  ! If on matching CON to CHNAM(1)
C
      END DO  ! End of Parent constituent loop 
      end if    ! on NLINES test              
	end if   ! on MOVESET test
   20 CONTINUE
	RETURN
	END