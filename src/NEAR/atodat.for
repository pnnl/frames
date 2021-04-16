C    EXPOS: ATODAT.FOR               Version Date:  22-May-97              
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C      SUBROUTINE ATODAT                                                     *
C                                                                            *
C  Subroutine ATODAT reads concentration data from the ATO file.              *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:     9-Jan-97                                               *
C  Last Modified:     22-May-97     DLS                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EXPOS  main program
C     Called by: EXPOS
C     Calls: none    
C     Common blocks referenced: DEVICE, EXPLOC, FLUX
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
C     15-May-97     DLS  Revised input for points and grid for current ATO format
C     22-May-97     DLS  Completed input for "Points"
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE ATODAT(RNAMES,NPRG,NPOINTS)
C
C---- Include Statements for Parameter and Common Declarations ---------------
C      
      INCLUDE 'CONIN.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'FNAMES.CMN'
      INCLUDE 'AIRINFO.CMN'
C     INCLUDE 'PARMTR.PAR'
      INCLUDE 'EXPLOC.CMN'
      INCLUDE 'FLUX.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      CHARACTER*12 CIDIN, RNAMES(9)
      CHARACTER*8 CHRONIC
      CHARACTER*10 OU, AU, POLAR
      CHARACTER*3 TEST
      CHARACTER*5 WETDRY
      CHARACTER*6 GRID,POINTS
      CHARACTER*20  CH,ONAME,OTYPE
      REAL TY,  VALIN(10), TIMP(10), VTIN(10,5,10,36)
      CHARACTER*8 TU
      CHARACTER*20 CNAMIN
      LOGICAL TSKIP, FOUNDT, INTERP
C
C---- Data Statements --------------------------------------------------------
C
C      DATA ACUTE/'acute   '/,CHRONIC/'chronic '/
      DATA CHRONIC/'chronic '/
      DATA GRID /'grid  '/,  POINTS /'points'/
      DATA POLAR/'polar     '/
      DATA ITONE/1/   ! One year integrating period value
c
C---- Initialize parameter values --------------------------------------------
c
c---- Upon entry, the file is positioned at the start of data for the --------
C     next constituent parent.  
C
      FOUNDT = .FALSE.
C
C-------- Test data set type for "acute" or "chronic" for reading data -------
c
      IF(ATYPE.NE.CHRONIC) THEN  ! error if not chronic caught in ATOIN
        WRITE(NERR,1000) ATYPE
 1000   FORMAT(' Error in ATODAT, bad data set type.  Must be ',
     .      'chronic.  Found: ',a20)
        NERROR = NERROR + 1
      ELSE
C
C------- Initialize arrays for atmospheric concentration parameters ----------
c
        DO IN = 1,9
          DO IX1 = 1,10
            DO IX2 = 1,36
              AIRCIN(IN,IX1,IX2) = 0.0
              DEPDRY(IN,IX1,IX2) = 0.0
              DEPWET(IN,IX1,IX2) = 0.0
              DEPTOT(IN,IX1,IX2) = 0.0
              EXTDOS(IN,IX1,IX2) = 0.0
            END DO
          END DO
        END DO
c
c-------- Read chronic data set ----------------------------------------------
c     First read name of parent, number of time periods, and number of progeny
c
        READ(NATO,*) CNAMIN,CIDIN,NTM,NPRG
        RNAMES(1) = CIDIN
        CNAMIN = CNAMIN
        NTMX = NTM
        IF(NTM.EQ.1) THEN
           INTERP = .FALSE.
        ELSE
           INTERP = .TRUE.
           IF(NTM.GT.10) NTMX = NTM
        ENDIF
C
C----- For each time period read time and number of input parameter
C      types included for this contaminant
C
        DO IT = 1, NTMX
            READ(NATO,*) TIMP(IT), TU, NOPS
            TIMP(IT) = TIMP(IT)
            TU = TU
            TSKIP = .TRUE.
            IF(.NOT.INTERP) THEN
              FOUNDT = .TRUE.
              TSKIP = .FALSE.
            ENDIF
C
C----- For each input parameter type to read
C
            DO IO = 1,NOPS
              READ(NATO,*) ONAME,OTYPE,WETDRY,OU,NAX1,CH,NAX2,AU
              IF(RECTYPE.EQ.GRID) THEN          ! For grid input structure
                 NPOINTS = NAX1*NAX2
                 READ(NATO,*) (AUX1(I),I=1,NAX1)
                 DO IX = 1,NAX2
                   ix2 = ix
                   READ(NATO,*) AUX2(IX),(VALIN(I),I=1,NAX1)
                   IF(.NOT.TSKIP) THEN
                     TEST = WETDRY(1:3)
                     IF(TEST.EQ.'dry') THEN
                       IF(.NOT.INTERP) THEN
                         CALL AIRSET(DEPDRY,VALIN,1,NAX1,IX2)
                       ELSE
                         CALL AIRSAVE(VTIN,IT,1,NAX1,IX2)
                       ENDIF
                     ELSE IF(TEST.EQ.'wet') THEN
                       IF(.NOT.INTERP) THEN
                         CALL AIRSET(DEPWET,VALIN,1,NAX1,IX2)
                       ELSE
                         CALL AIRSAVE(VTIN,IT,2,NAX1,IX2)
                       ENDIF
                     ELSE IF(TEST.EQ.'tot') THEN
                       IF(.NOT.INTERP) THEN
                         CALL AIRSET(DEPTOT,VALIN,1,NAX1,IX2)
                       ELSE
                         CALL AIRSAVE(VTIN,IT,3,NAX1,IX2)
                       ENDIF
                     ELSE
                       TEST = OTYPE(1:3)
                       IF(TEST.EQ.'Gas'.OR.TEST.EQ.'Par') THEN
                         IF(.NOT.INTERP) THEN
                           CALL AIRSET(AIRCIN,VALIN,1,NAX1,IX2)
                         ELSE
                           CALL AIRSAVE(VTIN,IT,4,NAX1,IX2)
                         ENDIF
                       ELSE IF(TEST.EQ.' ') THEN
                         IF(.NOT.INTERP) THEN
                           CALL AIRSET(EXTDOS,VALIN,1,NAX1,IX2)
                         ELSE
                           CALL AIRSAVE(VTIN,IT,5,NAX1,IX2)
                         ENDIF
                       ELSE  ! Atmospheric output paramater type not known
                         WRITE(NERR,1001) ONAME,OTYPE
 1001     FORMAT(' Error in ATO data input, quantity name not known: ',
     .             A20,1X,A3)
                       ENDIF
                     ENDIF
C
C--- If polar grid, calculate cartesian coordinates --------------------------
C
                    DO IX1 = 1,NAX1
                     INDEX = IX1 + (IX2-1)*NAX1
                     IF(CTYPE.EQ.POLAR) THEN  ! For polar convert to x,y
                       CALL GETXY(AUX1(IX1),AUX2(IX2),X,Y)
                       EXPXA(INDEX) = X
                       EXPYA(INDEX) = Y
                     ELSE     ! For cartesian use given values
                       EXPXA(INDEX) = AUX1(IX1)
                       EXPYA(INDEX) = AUX2(IX2)
                     ENDIF
                    END DO
                   ENDIF   ! End if on skipping processing of data
                 END DO
C
C-------- End of input for "grid" input method -------------------------------
C         If points, read input data
C
              ELSE IF (RECTYPE.EQ.POINTS) THEN   ! For points input structure
                NPOINTS = NAX1
                READ(NATO,*) CH     ! Line with receptor point names
                READ(NATO,*) (AUX1(I),I=1,NAX1) ! Line with point x-axis values
                READ(NATO,*) (AUX2(I),I=1,NAX2) ! Line with point y-axis values
                READ(NATO,*) ICH,(VALIN(I),I=1,NPOINTS)  ! Extra line to skip (-99)
                ICH = ICH
                IX2 = 1
                  IF(.NOT.TSKIP) THEN
                    TEST = WETDRY(1:3)
                    IF(TEST.EQ.'dry') THEN
                       IF(.NOT.INTERP) THEN
                         CALL AIRSET(DEPDRY,VALIN,1,NAX1,IX2)
                       ELSE
                         CALL AIRSAVE(VTIN,IT,1,NAX1,IX2)
                       ENDIF
                    ELSE IF(TEST.EQ.'wet') THEN
                      IF(.NOT.INTERP) THEN
                        CALL AIRSET(DEPWET,VALIN,1,NAX1,IX2)
                      ELSE
                        CALL AIRSAVE(VTIN,IT,2,NAX1,IX2)
                      ENDIF
                    ELSE IF(TEST.EQ.'tot') THEN
                      IF(.NOT.INTERP) THEN
                        CALL AIRSET(DEPTOT,VALIN,1,NAX1,IX2)
                      ELSE
                        CALL AIRSAVE(VTIN,IT,3,NAX1,IX2)
                      ENDIF
                    ELSE
                      TEST = OTYPE(1:3)
                      IF(TEST.EQ.'Gas'.OR.TEST.EQ.'Par') THEN
                        IF(.NOT.INTERP) THEN
                          CALL AIRSET(AIRCIN,VALIN,1,NAX1,IX2)
                        ELSE
                          CALL AIRSAVE(VTIN,IT,4,NAX1,IX2)
                        ENDIF
                      ELSE IF(TEST.EQ.' ') THEN
                        IF(.NOT.INTERP) THEN
                          CALL AIRSET(EXTDOS,VALIN,1,NAX1,IX2)
                        ELSE
                          CALL AIRSAVE(VTIN,IT,5,NAX1,IX2)
                        ENDIF
                      ELSE  ! Atmospheric output paramater type not known
                        WRITE(NERR,1001) ONAME,OTYPE
                      ENDIF
                    ENDIF
C
C--- If polar grid, calculate cartesian coordinates --------------------------
C
                   IX2 = 1
                   DO IX1 = 1,NAX1
                     INDEX = IX1 + (IX2-1)*NAX1
                     IF(CTYPE.EQ.POLAR) THEN  ! For polar convert to x,y
                       CALL GETXY(AUX1(IX1),AUX2(IX2),X,Y)
                       EXPXA(INDEX) = X
                       EXPYA(INDEX) = Y
                     ELSE     ! For cartesian use given values
                       EXPXA(INDEX) = AUX1(IX1)
                       EXPYA(INDEX) = AUX2(IX2)
                     ENDIF
                   END DO
                  ENDIF   ! End if on skipping processing of data
              END IF
C
            END DO    ! End of do on IO to NOPS
            CH = CH
            NAX1 = NAX1
          END DO    ! End of do on IT to NTM
C
C---- If interpolation is necessary do now for parent ------------------------
C
          IF(INTERP.AND..NOT.TSKIP) THEN
            NCM = 1
            CALL VINTERP(VTIN,TIMP,NCM,NTMX,OUTYPE,NAX1,NAX2)
          END IF
C
C---------- Repeat for any progeny
c
          IF(NPRG.GT.0) THEN
            DO INP = 1,NPRG
              READ(NATO,*) CNAMIN,CIDIN,NTMP
              IF(NTM.NE.NTMP) THEN
                 WRITE(NERR,1002) NTM,NTMP
 1002            FORMAT(' Error in ATO file, time points for parent ',
     .                  'and progeny differ.', 2I3)
                 NERROR = NERROR + 1
                 GO TO 999
              ENDIF
              RNAMES(INP+1) = CIDIN
              IPP = INP + 1
              DO IT = 1, NTM
                READ(NATO,*) TY, TU, NOPS
                TSKIP = .TRUE.
                IF(INT(TY).EQ.ITONE) TSKIP = .FALSE.
                DO IO = 1,NOPS
                  READ(NATO,*) ONAME,OTYPE,WETDRY,OU,NAX1,CH,NAX2,AU
                    IF(RECTYPE.EQ.GRID) THEN    ! Grid input structure
                      READ(NATO,*) (AUX1(I),I=1,NAX1)
                      DO IX = 1,NAX2
                        IX2 = IX
                        READ(NATO,*) AUX2(IX),(VALIN(I),I=1,NAX1)
                        IF(.NOT.TSKIP) THEN
                          TEST = WETDRY(1:3)
                          IF(TEST.EQ.'dry') THEN
                            IF(.NOT.INTERP) THEN
                              CALL AIRSET(DEPDRY,VALIN,IPP,NAX1,IX2)
                            ELSE
                              CALL AIRSAVE(VTIN,IT,1,NAX1,IX2)
                            ENDIF
                          ELSE IF(TEST.EQ.'wet') THEN
                            IF(.NOT.INTERP) THEN
                              CALL AIRSET(DEPWET,VALIN,IPP,NAX1,IX2)
                            ELSE
                              CALL AIRSAVE(VTIN,IT,2,NAX1,IX2)
                            ENDIF
                          ELSE IF(TEST.EQ.'tot') THEN
                            IF(.NOT.INTERP) THEN
                              CALL AIRSET(DEPTOT,VALIN,IPP,NAX1,IX2)
                            ELSE
                              CALL AIRSAVE(VTIN,IT,3,NAX1,IX2)
                            ENDIF
                          ELSE
                            TEST = OTYPE(1:3)
                            IF(TEST.EQ.'Gas'.OR.TEST.EQ.'Par') THEN
                              IF(.NOT.INTERP) THEN
                                CALL AIRSET(AIRCIN,VALIN,IPP,NAX1,IX2)
                              ELSE
                                CALL AIRSAVE(VTIN,IT,4,NAX1,IX2)
                              ENDIF
                            ELSE IF(TEST.EQ.' ') THEN
                              IF(.NOT.INTERP) THEN
                                CALL AIRSET(EXTDOS,VALIN,IPP,NAX1,IX2)
                              ELSE
                                CALL AIRSAVE(VTIN,IT,5,NAX1,IX2)
                              ENDIF
                            ELSE  ! Atmospheric output paramater type not known
                              WRITE(NERR,1001) ONAME,OTYPE
                            ENDIF
                          ENDIF
                        ENDIF
                      END DO
                    ELSE IF (RECTYPE.EQ.POINTS) THEN  ! Points input structure
                NPOINTS = NAX1
                READ(NATO,*) CH     ! Line with receptor point names
                READ(NATO,*) (AUX1(I),I=1,NAX1) ! Line with point x-axis values
                READ(NATO,*) (AUX2(I),I=1,NAX2) ! Line with point y-axis values
                READ(NATO,*) ICH,(VALIN(I),I=1,NPOINTS)  ! Extra line to skip (-99)
                IX2 = 1
                  IF(.NOT.TSKIP) THEN
                   TEST = WETDRY(1:3)
                   IF(TEST.EQ.'dry') THEN
                     IF(.NOT.INTERP) THEN
                       CALL AIRSET(DEPDRY,VALIN,IPP,NAX1,IX2)
                     ELSE
                       CALL AIRSAVE(VTIN,IT,1,NAX1,IX2)
                     ENDIF
                   ELSE IF(TEST.EQ.'wet') THEN
                     IF(.NOT.INTERP) THEN
                       CALL AIRSET(DEPWET,VALIN,IPP,NAX1,IX2)
                     ELSE
                       CALL AIRSAVE(VTIN,IT,2,NAX1,IX2)
                     ENDIF
                   ELSE IF(TEST.EQ.'tot') THEN
                     IF(.NOT.INTERP) THEN
                       CALL AIRSET(DEPTOT,VALIN,IPP,NAX1,IX2)
                     ELSE
                       CALL AIRSAVE(VTIN,IT,3,NAX1,IX2)
                     ENDIF
                   ELSE
                     TEST = OTYPE(1:3)
                     IF(TEST.EQ.'Gas'.OR.TEST.EQ.'Par') THEN
                       IF(.NOT.INTERP) THEN
                         CALL AIRSET(AIRCIN,VALIN,IPP,NAX1,IX2)
                       ELSE
                         CALL AIRSAVE(VTIN,IT,4,NAX1,IX2)
                       ENDIF
                     ELSE IF(TEST.EQ.' ') THEN
                       IF(.NOT.INTERP) THEN
                         CALL AIRSET(EXTDOS,VALIN,IPP,NAX1,IX2)
                       ELSE
                         CALL AIRSAVE(VTIN,IT,5,NAX1,IX2)
                       ENDIF
                     ELSE  ! Atmospheric output paramater type not known
                       WRITE(NERR,1001) ONAME,OTYPE
                     ENDIF
                   END IF
                  ENDIF      ! if not skip
                 ENDIF     ! If rectype = grid/points
                END DO    ! End of do on IO to NOPS
              END DO    ! End of do on IT to NTM
C
C---- If interpolation necessary do now for progeny ---------------------------
C
              IF(INTERP.AND..NOT.TSKIP) THEN
                NCM = IPP
                CALL VINTERP(VTIN,TIMP,NCM,NTMX,OUTYPE,NAX1,NAX2)
              END IF
            END DO   ! End of DO on progeny
C
          ENDIF  ! if progeny
      ENDIF   ! if chronic
      TY = TY
      AUX2(1) = AUX2(1)
      X = X
      Y = Y
      AU = AU
      OU = OU
      IF(.NOT.FOUNDT) THEN
        WRITE(NERR,2000)
 2000   FORMAT(' Error in ATO input time integration periods; '/
     .         '   a 1 year period is required, but was not found.')
        NERROR = NERROR + 1
      ENDIF
999   RETURN
C
C----- END OF MODULE ATODAT -------------------------------------------------
C
      END

