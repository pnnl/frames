C    EXPOS: ATOIN.FOR                Version Date:  15-May-97               
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
C  Creation Date:     8-Jan-97                                               *
C  Last Modified:     15-May-97     DLS                                      *
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
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE ATOIN(SRCNAME,NUMCON,FOUNDA)
C
C---- Include Statements for Parameter and Common Declarations ---------------
C      
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'FNAMES.CMN'
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'EXPLOC.CMN'
      INCLUDE 'FLUX.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      CHARACTER*12 CIDIN
      CHARACTER*32 SRCNAME, SETNAM(10)
      CHARACTER*8 ACUTE, CHRONIC
C      CHARACTER*10 CTYPE
      CHARACTER*6 GRID,POINTS
      CHARACTER*20  CH
      REAL TY
      CHARACTER*8 TU
C      CHARACTER*14 MEDTYPE
      CHARACTER*20 CNAMIN
      INTEGER NSETS, NFLUX, NUMCON, TPATH
      LOGICAL FOUNDA ! skip flag for media in .ATO
C
C---- Data Statements --------------------------------------------------------
C
      DATA ACUTE/'acute   '/,CHRONIC/'chronic '/
      DATA GRID /'grid  '/,  POINTS /'points'/
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
        IF(SETNAM(ISET).EQ.SRCNAME) THEN    
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
          READ(NATO,*) ATYPE,CTYPE,RECTYPE,NUMCON
c
c-------- File is now positioned at the start of the first constituent -------
c         Return to calling program.  Data will be read by ATODAT subroutine
c
          IF(ATYPE.NE.CHRONIC) THEN
            WRITE(NERR,1004) ATYPE
 1004       FORMAT(' Error in ATO data set type.  Must be chronic.'/
     .             '    Found: ',A10)
            NERROR = NERROR + 1
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
          IF(ATYPE.EQ.ACUTE) THEN
c
c-------- Skip past acute data set -------------------------------------------
c 
            DO IN = 1,NUMCON
              READ(NATO,*) CNAMIN,CIDIN,NTM,NPRG
              DO IT = 1, NTM
                READ(NATO,*) TY, TU, NOPS
                DO IO = 1,NOPS
                  READ(NATO,*) CH,CH,CH,CH,NAX1,CH,NAX2,CH
                  IF(RECTYPE.EQ.GRID) THEN
                     READ(NATO,*) X
                     DO IX = 1,NAX2
                       READ(NATO,*) Y
                     END DO
                  ELSE IF (RECTYPE.EQ.POINTS) THEN
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
                       IF(RECTYPE.EQ.GRID) THEN
                          READ(NATO,*) X
                          DO IX = 1,NAX2
                            READ(NATO,*) Y
                          END DO
                       ELSE IF (RECTYPE.EQ.POINTS) THEN
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
          ELSEIF(ATYPE.EQ.CHRONIC) THEN
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
                  IF(RECTYPE.EQ.GRID) THEN
                     READ(NATO,*) X
                     X = X
                     DO IX = 1,NAX2
                       READ(NATO,*) Y
                     END DO
                     Y = Y
                  ELSE IF (RECTYPE.EQ.POINTS) THEN
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
                       IF(RECTYPE.EQ.GRID) THEN
                          READ(NATO,*) X
                          DO IX = 1,NAX2
                            READ(NATO,*) Y
                          END DO
                       ELSE IF (RECTYPE.EQ.POINTS) THEN
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

        RETURN
C
C----- END OF MODULE ATOIN --------------------------------------------------
C
      END

