C   ACTUE:  ATODAT.FOR               Version Date:  11-May-98      
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C      SUBROUTINE ATODAT                                                     *
C                                                                            *
C  Subroutine ATODAT reads concentration data from the ATO file for acute    *
C             release                                                        *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:     9-Jan-97                                               *
C  Last Modified:     11-May-98     BAN                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: ACUTE               
C     Called by: ACUTE
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
C    Date     Who Modification Description
C  ---------  --- ------------------------------------------------------
C  08-Jan-97  DLS Initial programming started
C  15-May-97  DLS Revised input for points and grid for current ATO format
C  22-May-97  DLS Completed input for "Points"
C  06-Nov-97  DLS Revised for acute release input
C  11-May-98  BAN Update for 41x41 arrays
C  24 May 2000 BAN Revise for Ramsdell's naming conventions (TEST)
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE ATODAT(RNAMES,NPRG,NPOINTS)
C
C---- Include Statements for Parameter and Common Declarations ---------------
C      
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'TIMES.CMN'
      INCLUDE 'CONIN.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'FNAMES.CMN'
      INCLUDE 'AIRINFO.CMN'
      INCLUDE 'EXPLOC.CMN'
      INCLUDE 'FLUX.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      CHARACTER*12 CIDIN, RNAMES(9)
      CHARACTER*8 ACUTE  
      CHARACTER*10 OU, AU, POLAR
      CHARACTER*3 TEST
      CHARACTER*5 WETDRY
      CHARACTER*6 GRID,POINTS
      CHARACTER*20  CH,ONAME,OTYPE
      REAL TY,  VALIN(36), TIMP(36)
      CHARACTER*8 TU
      CHARACTER*20 CNAMIN
      LOGICAL TSKIP, PICTIM
      LOGICAL SEQI
C
C---- Data Statements --------------------------------------------------------
C
      DATA ACUTE/'acute   '/
      DATA GRID /'grid  '/,  POINTS /'points'/
      DATA POLAR/'polar     '/
c
C---- Initialize parameter values --------------------------------------------
c
c---- Upon entry, the file is positioned at the start of data for the --------
C     next constituent parent.  
C
C-------- Test data set type for "acute" or "chronic" for reading data -------
c
      IF(.NOT.SEQI(ATYPE,ACUTE,8)) THEN  ! error if not acute input
        WRITE(NERR,1000) ATYPE
 1000   FORMAT(' Error in ATODAT, bad data set type.  Must be ',
     .      'acute.  Found: ',a20)
        NERROR = NERROR + 1
      ELSE
C
C------- Initialize arrays for atmospheric concentration parameters ----------
c
        DO IN = 1,9
          DO IX1 = 1,41
            DO IX2 = 1,41
               AIRCIN(IN,IX1,IX2) = 0.0
               DEPDRY(IN,IX1,IX2) = 0.0
               DEPWET(IN,IX1,IX2) = 0.0
               DEPTOT(IN,IX1,IX2) = 0.0
               EXTDOS(IN,IX1,IX2) = 0.0
            END DO
          END DO
        END DO
c
c-------- Read acute data set ------------------------------------------------
c     First read name of parent, number of time periods, and number of progeny
c
        READ(NATO,*) CNAMIN,CIDIN,NTM,NPRG
        RNAMES(1) = CIDIN
        CNAMIN = CNAMIN
        TSKIP = .FALSE.
        PICTIM = .FALSE.
        IF(NTM.EQ.1) PICTIM= .TRUE.
C
C----- For each time period read time and number of input parameter
C      types included for this contaminant
C
        DO IT = 1, NTM
            READ(NATO,*) TIMP(IT), TU, NOPS
            TIM = TIMP(IT)
            TU = TU
            IF(.NOT.PICTIM) THEN
              IF(TIM.GT.0.9*ACUTIM.AND.TIM.LT.1.1*ACUTIM) PICTIM=.TRUE.
            ENDIF
C
C----- For each input parameter type to read
C
            DO IO = 1,NOPS
              READ(NATO,*) ONAME,OTYPE,WETDRY,OU,NAX1,CH,NAX2,AU
              IF(SEQI(RECTYPE,GRID,6)) THEN          ! For grid input structure
                 NPOINTS = NAX1*NAX2
                 READ(NATO,*) (AUX1(I),I=1,NAX1)
                 DO IX = 1,NAX2
                   ix2 = ix
                   READ(NATO,*) AUX2(IX),(VALIN(I),I=1,NAX1)
                   IF(.NOT.TSKIP) THEN
                     TEST = WETDRY(1:3)
C                     IF(TEST.EQ.'dry') THEN
	               IF(SEQI(TEST,'DRY',3))THEN
                       IF(PICTIM) THEN
                         CALL AIRSET(DEPDRY,VALIN,1,NAX1,IX2)
                       ENDIF
C                     ELSE IF(TEST.EQ.'wet') THEN
	               ELSE IF(SEQI(TEST,'WET',3))THEN
                       IF(PICTIM) THEN
                         CALL AIRSET(DEPWET,VALIN,1,NAX1,IX2)
                       ENDIF
C                     ELSE IF(TEST.EQ.'tot') THEN
	               ELSE IF(SEQI(TEST,'TOT',3))THEN
                       IF(PICTIM) THEN
c                         CALL AIRSET(DEPTOT,VALIN,1,NAX1,IX2)
                         CALL AIRSET(DEPdry,VALIN,1,NAX1,IX2)
                       ENDIF
                     ELSE
                       TEST = ONAME(1:3)
C                       IF(TEST.EQ.'Air') THEN
	                 IF(SEQI(TEST,'AIR',3))THEN
                         IF(PICTIM) THEN
                           CALL AIRSET(AIRCIN,VALIN,1,NAX1,IX2)
                         ENDIF
C                       ELSE IF(TEST.EQ.'Ext') THEN
	                 ELSE IF(SEQI(TEST,'EXT',3))THEN
                         IF(PICTIM) THEN
                           CALL AIRSET(EXTDOS,VALIN,1,NAX1,IX2)
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
                     IF(SEQI(CTYPE,POLAR,6)) THEN  ! For polar convert to x,y
                       CALL GETXY(AUX1(IX1),AUX2(IX2),X,Y)
                       EXPXA(INDEX) = X*0.001
                       EXPYA(INDEX) = Y*0.001
                     ELSE     ! For cartesian use given values
                       EXPXA(INDEX) = AUX1(IX1)*0.001
                       EXPYA(INDEX) = AUX2(IX2)*0.001
                     ENDIF
                    CALL XYPOINT(NPOINTS)
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
C                    IF(TEST.EQ.'dry') THEN
	              IF(SEQI(TEST,'DRY',3))THEN
                       IF(PICTIM) THEN
                         CALL AIRSET(DEPDRY,VALIN,1,NAX1,IX2)
                       ENDIF
C                    ELSE IF(TEST.EQ.'wet') THEN
	              ELSE IF(SEQI(TEST,'WET',3))THEN
                      IF(PICTIM) THEN
                        CALL AIRSET(DEPWET,VALIN,1,NAX1,IX2)
                      ENDIF
C                    ELSE IF(TEST.EQ.'tot') THEN
	              ELSE IF(SEQI(TEST,'TOT',3))THEN
                      IF(PICTIM) THEN
c                        CALL AIRSET(DEPTOT,VALIN,1,NAX1,IX2)
                        CALL AIRSET(DEPdry,VALIN,1,NAX1,IX2)
                      ENDIF
                    ELSE
                      TEST = ONAME(1:3)
C                      IF(TEST.EQ.'Air') THEN
	                IF(SEQI(TEST,'AIR',3))THEN
                        IF(PICTIM) THEN
                          CALL AIRSET(AIRCIN,VALIN,1,NAX1,IX2)
                        ENDIF
C                      ELSE IF(TEST.EQ.'Ext') THEN
	                ELSE IF(SEQI(TEST,'EXT',3))THEN
                        IF(PICTIM) THEN
                          CALL AIRSET(EXTDOS,VALIN,1,NAX1,IX2)
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
                     IF(SEQI(CTYPE,POLAR,6)) THEN  ! For polar convert to x,y
                       CALL GETXY(AUX1(IX1),AUX2(IX2),X,Y)
                       EXPXA(INDEX) = X
                       EXPYA(INDEX) = Y
                     ELSE     ! For cartesian use given values
                       EXPXA(INDEX) = AUX1(IX1)
                       EXPYA(INDEX) = AUX2(IX2)
                     ENDIF
                     CALL XYPOINT(NPOINTS)
                   END DO
                  ENDIF   ! End if on skipping processing of data
              END IF
C
            END DO    ! End of do on IO to NOPS
            CH = CH
            NAX1 = NAX1
            IF(PICTIM) TSKIP = .TRUE.
          END DO    ! End of do on IT to NTM
C
C---------- Repeat for any progeny -------------------------------------------
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
              TIM = TIMP(IT)
              TU = TU
              TSKIP = .FALSE.
              PICTIM = .FALSE.
              IF(NTM.EQ.1) PICTIM= .TRUE.
C
              DO IT = 1, NTM
                READ(NATO,*) TY, TU, NOPS
                IF(.NOT.PICTIM) THEN
                IF(TIM.GT.0.9*ACUTIM.AND.TIM.LT.1.1*ACUTIM)
     .             PICTIM=.TRUE.
                ENDIF
                DO IO = 1,NOPS
                  READ(NATO,*) ONAME,OTYPE,WETDRY,OU,NAX1,CH,NAX2,AU
                    IF(SEQI(RECTYPE,GRID,6)) THEN    ! Grid input structure
                      READ(NATO,*) (AUX1(I),I=1,NAX1)
                      DO IX = 1,NAX2
                        IX2 = IX
                        READ(NATO,*) AUX2(IX),(VALIN(I),I=1,NAX1)
                        IF(.NOT.TSKIP) THEN
                          TEST = WETDRY(1:3)
C                          IF(TEST.EQ.'dry') THEN
	                    IF(SEQI(TEST,'DRY',3))THEN
                            IF(PICTIM) THEN
                              CALL AIRSET(DEPDRY,VALIN,IPP,NAX1,IX2)
                            ENDIF
C                          ELSE IF(TEST.EQ.'wet') THEN
	                    ELSE IF(SEQI(TEST,'WET',3))THEN
                            IF(PICTIM) THEN
                              CALL AIRSET(DEPWET,VALIN,IPP,NAX1,IX2)
                            ENDIF
C                          ELSE IF(TEST.EQ.'tot') THEN
	                    ELSE IF(SEQI(TEST,'TOT',3))THEN
                            IF(PICTIM) THEN
c                              CALL AIRSET(DEPTOT,VALIN,IPP,NAX1,IX2)
                              CALL AIRSET(DEPdry,VALIN,IPP,NAX1,IX2)
                            ENDIF
                          ELSE
                            TEST = ONAME(1:3)
C                            IF(TEST.EQ.'Air') THEN
  	                      IF(SEQI(TEST,'DRY',3))THEN
                              IF(PICTIM) THEN
                                CALL AIRSET(AIRCIN,VALIN,IPP,NAX1,IX2)
                              ENDIF
C                            ELSE IF(TEST.EQ.'Ext') THEN
	                      ELSE IF(SEQI(TEST,'EXT',3))THEN
                              IF(PICTIM) THEN
                                CALL AIRSET(EXTDOS,VALIN,IPP,NAX1,IX2)
                              ENDIF
                            ELSE  ! Atmospheric output paramater type not known
                              WRITE(NERR,1001) ONAME,OTYPE
                            ENDIF
                          ENDIF
                        ENDIF
                      END DO
                    ELSE IF (SEQI(RECTYPE,POINTS,6)) THEN  ! Points input structure
                NPOINTS = NAX1
                READ(NATO,*) CH     ! Line with receptor point names
                READ(NATO,*) (AUX1(I),I=1,NAX1) ! Line with point x-axis values
                READ(NATO,*) (AUX2(I),I=1,NAX2) ! Line with point y-axis values
                READ(NATO,*) ICH,(VALIN(I),I=1,NPOINTS)  ! Extra line to skip (-99)
                IX2 = 1
                  IF(.NOT.TSKIP) THEN
                   TEST = WETDRY(1:3)
C                   IF(TEST.EQ.'dry') THEN
	             IF(SEQI(TEST,'DRY',3))THEN
                     IF(PICTIM) THEN
                       CALL AIRSET(DEPDRY,VALIN,IPP,NAX1,IX2)
                     ENDIF
C                   ELSE IF(TEST.EQ.'wet') THEN
	             ELSE IF(SEQI(TEST,'WET',3))THEN
                     IF(PICTIM) THEN
                       CALL AIRSET(DEPWET,VALIN,IPP,NAX1,IX2)
                     ENDIF
C                   ELSE IF(TEST.EQ.'tot') THEN
	             ELSE IF(SEQI(TEST,'TOT',3))THEN
                     IF(PICTIM) THEN
c                       CALL AIRSET(DEPTOT,VALIN,IPP,NAX1,IX2)
                       CALL AIRSET(DEPdry,VALIN,IPP,NAX1,IX2)
                     ENDIF
                   ELSE
                     TEST = ONAME(1:3)
C                     IF(TEST.EQ.'Air') THEN
	               IF(SEQI(TEST,'AIR',3))THEN
                       IF(PICTIM) THEN
                         CALL AIRSET(AIRCIN,VALIN,IPP,NAX1,IX2)
                       ENDIF
C                     ELSE IF(TEST.EQ.'Ext') THEN
	               ELSE IF(SEQI(TEST,'EXT',3))THEN
                       IF(PICTIM) THEN
                         CALL AIRSET(EXTDOS,VALIN,IPP,NAX1,IX2)
                       ENDIF
                     ELSE  ! Atmospheric output paramater type not known
                       WRITE(NERR,1001) ONAME,OTYPE
                     ENDIF
                   END IF
                  ENDIF      ! if not skip
                 ENDIF     ! If rectype = grid/points
                END DO    ! End of do on IO to NOPS
                IF(PICTIM) TSKIP = .TRUE.
              END DO    ! End of do on IT to NTM
C
C---- If interpolation necessary do now for progeny ---------------------------
C
C             IF(INTERP.AND..NOT.TSKIP) THEN
C               NCM = IPP
C               CALL VINTERP(VTIN,TIMP,NCM,NTM,OUTYPE,NAX1,NAX2)
C             END IF

           END DO   ! End of DO on progeny
C
          ENDIF  ! if progeny
      ENDIF   ! if chronic
c  BAN commented out the following lines 28 Aug 2012
c      TY = TY
c      AUX2(1) = AUX2(1)
c      X = X
c      Y = Y
c      AU = AU
c      OU = OU
999   RETURN
C
C----- END OF MODULE ATODAT -------------------------------------------------
C
      END

