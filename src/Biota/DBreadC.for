C Subroutine to obtain dose conversion factors from database portion of GID file, "con#"
C
C
      SUBROUTINE DBreadC (CHNAM,NOCHM)
C     This module reads the database data set from the GID file.
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Creation Date:     30-Oct-2001  BA Napier
C     Last Modification: 11-Sept-2002  BA Napier
C     
C-----------------------------------------------------------------------
C
C     30 Oct 2001   BAN  Establish as input for GENII chronic module
C     11 Sept 2002  BAN  Catch zero soil half-times from Database
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
	INCLUDE 'BIOTDF.CMN'
C
      INTEGER GETINT, IG
	CHARACTER*32 GETSTR
	CHARACTER*12 CAS
	CHARACTER*12 CHNAM(lenchain)
      REAL GETREAL
      LOGICAL GETLOG, FILTER
      CHARACTER*80 SETDATA(LINEMAX)
C
	IERR = 0
C      
C----- Zero parameters  ------------
      Do IN = 1, NoChM
          DPVL(IN) = 0.
	    ALGDF(IN) = 0.
	    CRUDF(IN) = 0.
	    FISDF(IN) = 0.
	    MOLDF(IN) = 0.
	    TMAMDF(IN) = 0.
	    TBIRDF(IN) = 0.
	    TEGGDF(IN) = 0.
	    TPLADF(IN) = 0.
	    RMAMDF(IN) = 0.
	    RBIRDF(IN) = 0.
	    CLDEX(IN)  = 0.
	    CLDIMR(IN) = 0.
	    CLDSH(IN)  = 0.
          IF(LCHOPT.LE.1) LEACHR(IN) = 0.
          BVI(1,IN) = 0.
          FMI(1,IN) = 0.
          FMI(2,IN) = 0.
          FMI(3,IN) = 0.
          FMI(4,IN) = 0.
          FMI(5,IN) = 0.
          BIOACF(1,IN) = 0.
          BIOACF(2,IN) = 0.
          BIOACF(3,IN) = 0.
          BIOACF(4,IN) = 0.
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
	FILTER = .TRUE.
C
C---- Read input parameters --------------------------------------------
C	
	CALL GETSET(NGID, NERR, CONNAME, NLINES, SETDATA, NSITE, FILTER)
C
      FILTER = .FALSE.
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
          FMI(1,1) = GETREAL(SETDATA,NLINES,'CLFMT         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          FMI(2,1) = GETREAL(SETDATA,NLINES,'CLFmt         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          FMI(3,1) = GETREAL(SETDATA,NLINES,'CLFpl         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          FMI(4,1) = GETREAL(SETDATA,NLINES,'CLFPL         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          FMI(5,1) = GETREAL(SETDATA,NLINES,'CLFEG         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          TMAMDF(1) = GETREAL(SETDATA,NLINES,'TMAMDF        ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          TPLADF(1) = GETREAL(SETDATA,NLINES,'TPLADF        ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          TBIRDF(1) = GETREAL(SETDATA,NLINES,'TBIRDF        ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          TEGGDF(1) = GETREAL(SETDATA,NLINES,'TEGGDF        ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          RMAMDF(1) = GETREAL(SETDATA,NLINES,'RMAMDF        ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          RBIRDF(1) = GETREAL(SETDATA,NLINES,'RBIRDF        ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          CLDEX(1) = GETREAL(SETDATA,NLINES,'CLDEX         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          CLDIMR(1) = GETREAL(SETDATA,NLINES,'CLDIMR        ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          CLDSH(1) = GETREAL(SETDATA,NLINES,'CLDSH         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          IF (ISALT) THEN
		BIOACF(1,1) = GETREAL(SETDATA,NLINES,'CLBMF         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          BIOACF(2,1) = GETREAL(SETDATA,NLINES,'CLBMM         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          BIOACF(3,1) = GETREAL(SETDATA,NLINES,'CLBMI         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          BIOACF(4,1) = GETREAL(SETDATA,NLINES,'CLBMP         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          ALGDF(1) = GETREAL(SETDATA,NLINES,'MALGDF         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          CRUDF(1) = GETREAL(SETDATA,NLINES,'MCRUDF         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          FISDF(1) = GETREAL(SETDATA,NLINES,'MFISDF         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          MOLDF(1) = GETREAL(SETDATA,NLINES,'MMOLDF         ',
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
          ALGDF(1) = GETREAL(SETDATA,NLINES,'FALGDF         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          CRUDF(1) = GETREAL(SETDATA,NLINES,'FCRUDF         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          FISDF(1) = GETREAL(SETDATA,NLINES,'FFISDF         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
          MOLDF(1) = GETREAL(SETDATA,NLINES,'FMOLDF         ',
     .                                                IC,IN,IZ,IZ,IZ,IZ)
	    END IF
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
          FMI(1,IP) = GETREAL(SETDATA,NLINES,'CLFMT         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          FMI(2,IP) = GETREAL(SETDATA,NLINES,'CLFmt         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          FMI(3,IP) = GETREAL(SETDATA,NLINES,'CLFpl         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          FMI(4,IP) = GETREAL(SETDATA,NLINES,'CLFPL         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          FMI(5,IP) = GETREAL(SETDATA,NLINES,'CLFEG         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          TMAMDF(IP) = GETREAL(SETDATA,NLINES,'TMAMDF         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          TPLADF(IP) = GETREAL(SETDATA,NLINES,'TPLADF         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          TBIRDF(IP) = GETREAL(SETDATA,NLINES,'TBIRDF         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          TEGGDF(IP) = GETREAL(SETDATA,NLINES,'TEGGDF         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          RMAMDF(IP) = GETREAL(SETDATA,NLINES,'RMAMDF         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          RBIRDF(IP) = GETREAL(SETDATA,NLINES,'RBIRDF         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          CLDEX(IP) = GETREAL(SETDATA,NLINES,'CLDEX         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          CLDIMR(IP) = GETREAL(SETDATA,NLINES,'CLDIMR        ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          CLDSH(IP) = GETREAL(SETDATA,NLINES,'CLDSH         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          IF (ISALT) THEN
		BIOACF(1,IP) = GETREAL(SETDATA,NLINES,'CLBMF         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          BIOACF(2,IP) = GETREAL(SETDATA,NLINES,'CLBMM         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          BIOACF(3,IP) = GETREAL(SETDATA,NLINES,'CLBMI         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          BIOACF(4,IP) = GETREAL(SETDATA,NLINES,'CLBMP         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          ALGDF(IP) = GETREAL(SETDATA,NLINES,'MALGDF         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          CRUDF(IP) = GETREAL(SETDATA,NLINES,'MCRUDF         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          FISDF(IP) = GETREAL(SETDATA,NLINES,'MFISDF         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          MOLDF(IP) = GETREAL(SETDATA,NLINES,'MMOLDF         ',
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
          ALGDF(IP) = GETREAL(SETDATA,NLINES,'FALGDF         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          CRUDF(IP) = GETREAL(SETDATA,NLINES,'FCRUDF         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          FISDF(IP) = GETREAL(SETDATA,NLINES,'FFISDF         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
          MOLDF(IP) = GETREAL(SETDATA,NLINES,'FMOLDF         ',
     .                                                IC,IN,IG,IZ,IZ,IZ)
	    END IF
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
C
C---- Set leach rate constants for this decay chain --------------------------
C
      IF(LCHOPT.EQ.2.OR.LCHOPT.EQ.3) THEN
C       
        DO IP = 1,NOCHM
           LEACHR(IP) = ALEACH(IP)
        END DO
      ENDIF
   20 CONTINUE
	RETURN
	END