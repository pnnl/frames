C    EXPOS: ATOIN.FOR                Version Date:  15-Apr-06              
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C      SUBROUTINE ATOIN                                                      *
C                                                                            *
C  Subroutine ATOIN reads heading  data from the ATO file.                   *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:     08-Jan-97                                              *
C  Last Modified:     15-Apr-06  MAS                                         *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EXPOS  main program
C     Called by: EXPOS
C     Calls: HEADINA
C     Common blocks referenced: DEVICE, FNAMES
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     NATO       U      INT    DEVICE    Logical unit for ATO file
C==== Modification History ===================================================
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     8-Jan-97      DLS  Initial programming started
C     7-May-97      DLS  Added error count increment (NERROR)
C     15-May-97     DLS  Revised for consistency with current ATO file format
C     30-Mar-98     BAN  Re-revised ATO format (STRTMN not available)
C     15-Apr-06     MAS  Added time to accident start (JHOUR)
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE ATOIN(SRCNAME,NUMCON,FOUNDA)
C
C---- Include Statements for Parameter and Common Declarations ---------------
C      
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'FNAMES.CMN'
      INCLUDE 'EXPLOC.CMN'
      INCLUDE 'FLUX.CMN'
	INCLUDE 'OPT.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      CHARACTER*12 CIDIN
      CHARACTER*32 SRCNAME, SETNAM(10), XXall
      CHARACTER*8 ACUTE, CHRONIC
C      CHARACTER*10 CTYPE
      CHARACTER*6 GRID,POINTS
      CHARACTER*20  CH
      REAL TY
      CHARACTER*8 TU
C      CHARACTER*14 MEDTYPE
      CHARACTER*20 CNAMIN
      CHARACTER*100 LINE
      INTEGER NSETS, NFLUX, NUMCON, TPATH
      INTEGER STRTYR,STRTMO,STRTDA,STRTHR,STRTMN
      LOGICAL FOUNDA ! skip flag for media in .ATO
      LOGICAL SEQI
C
C---- Data Statements --------------------------------------------------------
C
      DATA ACUTE/'acute   '/,CHRONIC/'chronic '/
      DATA GRID /'grid  '/,  POINTS /'points'/
	DATA XXall/'all'/
c
C---- Initialize parameter values --------------------------------------------
C
      NUMCON = 0
      FOUNDA = .FALSE.
      TPATH = 3
C
C-----  Read heading information from Water Flux File ------------------------
C
      CALL HEADIN(TPATH,NSETS)
C
C----- Loop on the number of media in the ATO file ---------------------------
C
      FOUNDA = .FALSE.
      DO ISET = 1,NSETS
c
        READ(NATO,*) NFLUX,SETNAM(ISET)
C
C----- Test set name against desired set name (SRCNAME input argurement) -----
C
        IF(SEQI(SETNAM(ISET),SRCNAME,32) .or. 
     .                SEQI(SETNAM(ISET),XXall,32)) THEN    
          FOUNDA = .TRUE.             ! Found set.  read heading parameters
c
C------ Read flux type descriptive parameters --------------------------------
c
          DO IX = 1,NFLUX
            READ(NATO,*) FLXNAM(IX),PRADUS(IX),PRADU(IX),PARDEN(IX),
     .                   PDENU(IX)
          END DO
c
c------ Read data set type descriptive parameters ----------------------------
c
          READ(NATO,100) LINE
 100      FORMAT(A100)
          READ(LINE,*) ATYPE,CTYPE,RECTYPE,NUMCON
c
c-------- File is now positioned at the start of the first constituent -------
c         Return to calling program.  Data will be read by ATODAT subroutine
c
          IF(.NOT.SEQI(ATYPE,ACUTE,8)) THEN
            WRITE(NERR,1004) ATYPE
 1004       FORMAT(' Error in ATO data set type.  Must be acute.'/
     .             '    Found: ',A10)
            NERROR = NERROR + 1
            GO TO 999
          ENDIF
          READ(LINE,*) ATYPE,CTYPE,RECTYPE,NUMCON,STRTYR,STRTMO,
C     .                 STRTDA,STRTHR,STRTMN
     .                 STRTDA,STRTHR
C          WRITE(NELS,2000) STRTYR,STRTMO,STRTDA,STRTHR,STRTMN
          WRITE(NELS,2000) STRTYR,STRTMO,STRTDA,STRTHR
 2000     FORMAT(' Acute atmospheric analysis data found for start',
     .     ' time:'/
     .     '      Year - ',I4,', Month - ',I2,', Day - ',I3,', Hour - ',
C     .     I2,', Minute - ',I2)
     .     I2)
C
C------- Calculates number of hours (JHOUR) in year up to accident month, day, and hour.
C	   The calculated hours do not include February 29th during leap years
C	   giving a maximum of 8760 hours.
C
		IF (STRTMO.EQ.1) THEN
		  JHOUR = ((STRTDA-1) * 24) + STRTHR
		ELSEIF (STRTMO.EQ.2) THEN
		  IF (STRTDA.EQ.29) THEN
			JHOUR = (744) + ((STRTDA-1) * 24) + STRTHR - 24		! "- 24"  February 29th not counted
		  ELSE
			JHOUR = (744) + ((STRTDA-1) * 24) + STRTHR			! 744 hours to February 1st
		  ENDIF
		ELSEIF (STRTMO.EQ.3) THEN
		  JHOUR = (1416) + ((STRTDA-1) * 24) + STRTHR			! 1416 hours to March 1st
		ELSEIF (STRTMO.EQ.4) THEN
		  JHOUR = (2160) + ((STRTDA-1) * 24) + STRTHR			! 2160 hours to April 1st
		ELSEIF (STRTMO.EQ.5) THEN
		  JHOUR = (2880) + ((STRTDA-1) * 24) + STRTHR			! 2880 hours to May 1st
		ELSEIF (STRTMO.EQ.6) THEN
		  JHOUR = (3624) + ((STRTDA-1) * 24) + STRTHR			! 3624 hours to June 1st
		ELSEIF (STRTMO.EQ.7) THEN
		  JHOUR = (4344) + ((STRTDA-1) * 24) + STRTHR			! 4344 hours to July 1st
		ELSEIF (STRTMO.EQ.8) THEN
		  JHOUR = (5088) + ((STRTDA-1) * 24) + STRTHR			! 5088 hours to August 1st
		ELSEIF (STRTMO.EQ.9) THEN
		  JHOUR = (5832) + ((STRTDA-1) * 24) + STRTHR			! 5832 hours to September 1st
		ELSEIF (STRTMO.EQ.10) THEN
		  JHOUR = (6552) + ((STRTDA-1) * 24) + STRTHR			! 6552 hours to October 1st
		ELSEIF (STRTMO.EQ.11) THEN
		  JHOUR = (7296) + ((STRTDA-1) * 24) + STRTHR			! 7296 hours to November 1st
		ELSEIF (STRTMO.EQ.12) THEN
		  JHOUR = (8016) + ((STRTDA-1) * 24) + STRTHR			! 8016 hours to December 1st
		ENDIF

          RETURN
        ELSE  ! Not desired data set.  Read past the set and search for others
          FOUNDA = .FALSE.
          IF(ISET.EQ.NSETS) THEN
            WRITE(NERR,1000) SRCNAME
 1000       FORMAT(' Error reading ATO file.  Data set not found ',A32/
     .             '   The following sets were found:')
            NERROR = NERROR + 1
            DO IS = 1,NSETS
              WRITE(NERR,1002) IS,SETNAM(IS)
 1002       FORMAT('   Set number',i2,' name is ',a32)
            END DO
            RETURN
          ENDIF
C
C------ Skip past this data set and position to top of next ------------------
C
C------ Skip flux type descriptive parameters --------------------------------
c
          DO IX = 1,NFLUX
            READ(NATO,*) FLXNAM(IX),PRADUS(IX),PRADU(IX),PARDEN(IX),
     .                   PDENU(IX)
          END DO
c
c------ Read data set type descriptive parameters ----------------------------
c
          READ(NATO,*) ATYPE,CTYPE,RECTYPE,NUMCON
c
C-------- Test data set type for "acute" or "chronic" for skiping data -------
c
          IF(SEQI(ATYPE,ACUTE,8)) THEN
c
c-------- Skip past acute data set -------------------------------------------
c 
            DO IN = 1,NUMCON
              READ(NATO,*) CNAMIN,CIDIN,NTM,NPRG
              DO IT = 1, NTM
                READ(NATO,*) TY, TU, NOPS
                DO IO = 1,NOPS
                  READ(NATO,*) CH,CH,CH,CH,NAX1,CH,NAX2,CH
                  IF(SEQI(RECTYPE,GRID,6)) THEN
                     READ(NATO,*) X
                     DO IX = 1,NAX2
                       READ(NATO,*) Y
                     END DO
                  ELSE IF (SEQI(RECTYPE,POINTS,6)) THEN
                     READ(NATO,*) CH   ! Line with receptor point names
                     READ(NATO,*) X    ! Line with point x-axis values
                     READ(NATO,*) Y    ! Line with point y-axis values
                     READ(NATO,*) X    ! Extra line to skip (-99)
                  ENDIF
                END DO
              END DO
C
C---- Skip acute data for progeny, if any ------------------------------------
C
              IF(NPRG.GT.0) THEN
                DO INP = 1,NPRG
                  READ(NATO,*) CNAMIN,CIDIN,NTM
                  DO IT = 1, NTM
                    READ(NATO,*) TY, TU, NOPS
                    DO IO = 1,NOPS
                      READ(NATO,*) CH,CH,CH,CH,NAX1,CH,NAX2
                       IF(SEQI(RECTYPE,GRID,6)) THEN
                          READ(NATO,*) X
                          DO IX = 1,NAX2
                            READ(NATO,*) Y
                          END DO
                       ELSE IF (SEQI(RECTYPE,POINTS,6)) THEN
                         READ(NATO,*) CH   ! Line with receptor point names
                         READ(NATO,*) X    ! Line with point x-axis  values
                         READ(NATO,*) Y    ! Line with point y-axis values
                         READ(NATO,*) X    ! Extra line to skip (-99)
                       END IF
                    END DO    ! End of do on IO to NOPS
                  END DO    ! End of do on IT to NTM
                END DO   ! End of DO on progeny
              ENDIF
            END DO    ! End of do on IN to NUMCON
          ELSEIF(SEQI(ATYPE,CHRONIC,8)) THEN
c
c-------- Skip past chronic data set -----------------------------------------
c
            DO IN = 1,NUMCON
              READ(NATO,*) CNAMIN,CIDIN,NTM,NPRG
              CIDIN = CIDIN
              CNAMIN = CNAMIN
              DO IT = 1, NTM
                READ(NATO,*) TY, TU, NOPS
                TY = TY
                TU = TU
                DO IO = 1,NOPS
                  READ(NATO,*) CH,CH,CH,CH,NAX1,CH,NAX2
                  IF(SEQI(RECTYPE,GRID,6)) THEN
                     READ(NATO,*) X
                     X = X
                     DO IX = 1,NAX2
                       READ(NATO,*) Y
                     END DO
                     Y = Y
                  ELSE IF (SEQI(RECTYPE,POINTS,6)) THEN
                     READ(NATO,*) CH   ! Line with receptor point names
                     READ(NATO,*) X    ! Line with point x-axis values
                     READ(NATO,*) Y    ! Line with point y-axis values
                     READ(NATO,*) X    ! Extra line to skip (-99)
                  ELSE    ! Error in RECTYPE
                    WRITE(NERR,1003) RECTYPE
                    NERROR = NERROR + 1
 1003       FORMAT(' Error in ATO file grid/points specification: ',A20)
                    RETURN
                  END IF
                END DO    ! End of do on IO to NOPS
                CH = CH
                NAX1 = NAX1
              END DO    ! End of do on IT to NTM
C
C---------- Repeat for any progeny
c
              IF(NPRG.GT.0) THEN
                DO INP = 1,NPRG
                  READ(NATO,*) CNAMIN,CIDIN,NTM
                  DO IT = 1, NTM
                    READ(NATO,*) TY, TU, NOPS
                    DO IO = 1,NOPS
                      READ(NATO,*) CH,CH,CH,CH,NAX1,CH,NAX2
                       IF(SEQI(RECTYPE,GRID,6)) THEN
                          READ(NATO,*) X
                          DO IX = 1,NAX2
                            READ(NATO,*) Y
                          END DO
                       ELSE IF (SEQI(RECTYPE,POINTS,6)) THEN
                         READ(NATO,*) CH   ! Line with receptor point names
                         READ(NATO,*) X    ! Line with point x-axis  values
                         READ(NATO,*) Y    ! Line with point y-axis values
                         READ(NATO,*) X    ! Extra line to skip (-99)
                       END IF
                    END DO    ! End of do on IO to NOPS
                  END DO    ! End of do on IT to NTM
                END DO   ! End of DO on progeny
              ENDIF
            END DO    ! End of do on IN to NUMCON
          ELSE
            WRITE(NERR,1001) ATYPE
            NERROR = NERROR + 1
 1001       FORMAT(' Error in ATO file type specification: ',a32/
     .             '     Should be "acute" or "chronic".')
            RETURN
          ENDIF
        ENDIF
c
      END DO        ! End of DO Loop on data sets, ISET to NSET

 999    RETURN
C
C----- END OF MODULE ATOIN --------------------------------------------------
C
      END

