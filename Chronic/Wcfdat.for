C  WCFDAT.FOR   EXPOS                Version Date: 6-May-98
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE WCFDAT                                *
C                                                                            *
C  Subroutine WCFDAT reads data for a radionuclide chain from the WCF file   *
C             Water concentrations are read and interpolated for each year   *
C             of interest for the analysis                                   *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    26-Dec-96                                               *
C  Last Modified:    6-May-98     BAN                                        *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EDUP/EXPOS
C     Called by: EXPOS
C     Calls: AVGCON
C     Common blocks referenced: TIMES
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     WATC(100,9) S    REAL    Argument  Water concentration values for each
C                                        year for the analysis by chain member
C     RNAMES(9)   S    CHAR    Argument  Names of radionuclide chain members
C                                        read from the WCF file
C
C==== Modification History ===================================================
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C   26-Dec-96      DLS  Initial programming started
C   6-May-98       BAN  Corrected character of H3N and C14N
C   4-June-01      BAN  Skip non-radionuclides in water processing
C   7-June-01      BAN  Use water progeny start/end times in averaging
C   9-Nov-2001     BAN  Weakened the C14 and H3 flags
C   19-Jun-2006    BAN  Allow zero time-pair instances in input files
C   10-Dec-09      BAN  Added nerror=nerror+1 line to GW error test
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE WCFDAT(CONID,CONPID,NUMCON,NDS,RNAMES,NPROG,NICP,ISKIP)
C
C---- Include statements -----------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'CONIN.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'TIMES.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      CHARACTER*8 TU,CONU
      CHARACTER*12 RNAMES(LENCHAIN),CNAMID,CONID(100),
     .             CONPID(100,LENCHAIN),chnam(lenchain),
     .             PNAMES(LENCHAIN) 
      CHARACTER*20 CNAMIN
      CHARACTER*3 C14N,H3N
      DATA C14N/'C14'/, H3N/'H3 '/
      INTEGER NDS(100),NUMCON
      REAL REL(100),TIME(100)
      REAL BQPI,MLL, UTF
      LOGICAL FOUND
      LOGICAL SEQI, ISKIP
C
C---- Data Statements --------------------------------------------------------
C
      DATA BQPI/0.037/, MLL/1000./
C
C---- Start of Analysis
C
      UTF = BQPI * MLL
	nopairs = 0
	noppairs = 0
C
C---- Determine number of time periods for analysis --------------------------
C
      NT = INT(NTKEND)
      NTIMES = NT
      IBEFR=INT(BEFIRR)
      IF(BEFIRR.GT.0) NTIMES = NTIMES + IBEFR
      IF(NTIMES.GT.100) THEN
        WRITE(NERR,1000) NTIMES
 1000   FORMAT(' Error in definition of number of time period for ',
     .         'waterborne conenctration input.'/
     .         '   Maximum value is 100, but input requires:',i5)
      NERROR = NERROR + 1
      RETURN
      ENDIF
C
C---- Read name of radionuclide/number of progeny/ number of time points -----
C
      READ(NWCF,*) CNAMIN,CNAMID,TU,CONU,NTIN,NPROG
      TU = TU
      CONU = CONU
      CNAMIN = CNAMIN
      CNAMID = CNAMID
c
c---- Test value for number of time periods/write error message --------------
c
      IF(NTIN.LT.1.OR.NTIN.GT.100) THEN
        WRITE(NERR,100) RNAMES(1),NTIN
 100    FORMAT(' Error in WCF data input: incorrect number of time '
     .    'data points for ',A12/'    Value should be between 1 and ',
     .    '100.  Value found: ',I4)
	  nopairs = 1
	    nerror=Nerror+1
        if(ntin .gt. 100) GO TO 999
      ENDIF
C
C---- Test input parent name against GID master list -------------------------
C
      FOUND = .FALSE.
	C14 = .FALSE.
	H3 = .FALSE.
      DO IC = 1,NUMCON
         IF(CNAMID.EQ.CONID(IC)) THEN
            FOUND = .TRUE.
            NICP = IC
            IF(SEQI(CNAMID, C14N, 3)) C14 = .TRUE.
            IF(SEQI(CNAMID, H3N, 2)) H3 = .TRUE.
         ENDIF
      END DO
      IF(.NOT.FOUND) THEN
          WRITE(NELS,2012) CNAMID   
 2012     FORMAT(' Error in WCF radionuclide name: not in master ',
     .    'list.'/'   Name found was: ',A12,' Analysis not performed')
C         Added logic to skip non-radioactive chemicals
C         
          WRITE(NEPF,1111) TRIM(CNAMIN), TRIM(CNAMID)
 1111     FORMAT('"',A,'","',A,'",0,0')
          ISKIP = .TRUE.
C
          RNAMES(1) = CNAMID
      ENDIF
C
C----- Call RMDGET to read chain decay data and names of valid progeny -------
C
      IF(FOUND) THEN
      NP = NDS(NICP)
      DO IP = 1,NP
        PNAMES(IP) = CONPID(NICP,IP)
      ENDDO
C      CALL RMDGET(CONID(NICP),CONPID(NICP,1),NDS(NICP),RNAMES,ICROS,
      CALL RMDGET(CONID(NICP),PNAMES,NDS(NICP),RNAMES,ICROS,
     .            NOCHM,chnam)
      ENDIF
C
C----- Read time/water concentration data pairs ------------------------------
c
      if (nopairs .eq. 0) then
      DO IT = 1,NTIN
        READ(NWCF,*) TIME(IT),REL(IT)
      END DO
	endif
c
c---- Evaluate average water concentration for each year necessary -----------
C
      IF(FOUND) THEN
         TSTRT1 = TIME(1)
         TEND1 = TIME(NTIN)
         TREL = 1.
         ntrel = int(trel)
         INUC = 1
         T1 = BEFORE
         START1 = T1 - FLOAT(BEFIRR)
C
         if(nopairs .eq. 0) then
         DO IT = 1,NTIMES
           START = START1 + FLOAT(IT-1)
           WATC(IT,INUC) = UTF*AVGCON(START,NTREL,TSTRT1,TEND1,REL,TIME,
     .                             IERR)
           NERROR = NERROR + IERR
         END DO
	   else
	   do it = 1,ntimes
	     watc(it,inuc) = 0.0
	   enddo
	   endif
      ENDIF
C
C----- If progeny, then read data for progeny
C----- Read name of progeny/ number of time points ---------------------------
C
      IF(NPROG.GT.0) THEN
        DO IP = 1,NPROG
          READ(NWCF,*) CNAMIN,CNAMID,TU,CONU,NTIN
C
c---- Test value for number of time periods/write error message --------------
c
          IF(NTIN.LT.1.OR.NTIN.GT.100) THEN
            WRITE(NERR,100) CNAMID,NTIN
	      noppairs = 1
            If (ntin .gt. 100) NERROR = NERROR + 1
            if (ntin .gt. 100) GO TO 999
          ENDIF
C
C----- Test progeny names against RMDLIB list for inclusion in analysis ------
C
          IF(FOUND) THEN
            FOUND = .FALSE.
            DO IC = 2,NOCHM
               IF(SEQI(CNAMID,RNAMES(IC),12))  THEN
                 FOUND = .TRUE.
                 NIC = IC
                 GO TO 20
               ENDIF
            END DO  
20          CONTINUE
            IF(.NOT.FOUND) THEN
                WRITE(NELS,2013) CNAMID
 2013     FORMAT(' Error in WCF progeny rad name: not in master ',
     .    'list.'/'   Name found was: ',A12,' Not included in chain.')
            ENDIF
          ENDIF
C
C----- Read time/water concentration data pairs ------------------------------
c
          if (noppairs .eq. 0) then
          DO IT = 1,NTIN
            READ(NWCF,*) TIME(IT),REL(IT)
          END DO
	    endif
c
c---- Evaluate average water concentration for each year necessary -----------
C
          IF (FOUND) THEN
          TSTRTP = TIME(1)
          TENDP = TIME(NTIN)
          TREL = 1.
          NTREL = INT(TREL)
	       if (noppairs .eq. 0) then
             DO IT = 1,NTIMES
                START = START1 + FLOAT(IT-1)
                WATC(IT,NIC) = UTF*AVGCON(START,NTREL,TSTRTP,TENDP,
     .                          REL,TIME,IERR)
                NERROR = NERROR + IERR
             END DO
	       else
	       do it = 1,ntimes
	          watc(it,nic) = 0.0
	       enddo
	       endif
          ENDIF
c
        END DO
      ENDIF
 999  RETURN
C----- END OF MODULE WCFDAT -------------------------------------------------
      END

