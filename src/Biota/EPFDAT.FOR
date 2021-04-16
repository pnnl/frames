C   BTFDAT.FOR EXPOS                Version Date: 22-May-97               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE BTFDAT                                *
C                                                                            *
C  Subroutine BTFDAT  writes data for a parent(&progeny) for each time period*
C       and each exposure pathway                                            *
C                                                                            *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    07-Jan-97                                               *
C  Last Modified:    22-May-97      DLS                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EDUP/EXPOS
C     Called by: EXPOS
C     Calls: NONE  
C     Common blocks referenced: NONE
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     LNAMES(9)   U   CHAR*12    ARG     Name of each chain member
C     NUMNUC      U    INT       ARG     Number of chain members in current chain
C     TPATH       U    INT       ARG     Transport path index (1-GW, 2-SW, 3-air
C                                        4-Soil
C     NPOINTS     U    INT       ARG     Number of location points for data output
c                                        For GW, SW, and Soil = 1.  For air = 1
C                                        for MI, = grid points for population.
C==== Modification History ===================================================
C     Date     Who  Modification Description
C     -------- ---  ------------------------------------------------------
C   07-Jan-97  DLS  Initial programming started
C   15-May-97  DLS  Added TPATH and NPOINTS to argument list, revised output
C                   to be consistent with current BTF format.
C   22-May-97  DLS  Moved exposure pathway names/units to common EXPNAM
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE BTFDAT(INUC,LNAMES,NUMNUC,TPATH,NPOINTS)
C
C---- Include statements -----------------------------------------------------
c
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'EXPALL.CMN'
      INCLUDE 'EXINFO.CMN'
      INCLUDE 'EXPNAM.CMN'
      INCLUDE 'AIRINFO.CMN'
      INCLUDE 'FLUX.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'TIMES.CMN'
	INCLUDE 'BIOTDF.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      REAL EXPVAL(28)
      INTEGER NUMNUC,TPATH
      CHARACTER*12 LNAMES(LENCHAIN),EXPROUT(28)
      CHARACTER*20 EXPLABL(28)
      CHARACTER*7 EXPUNIT(28)
      INTEGER NLAB(28),NROUT(28),NPUN(28)
	LOGICAL BPRINT(28)
C
C---- Data Statements --------------------------------------------------------
C
      DATA EMIN/1.E-25/
	DATA BPRINT/.FALSE.,.FALSE.,.FALSE.,.TRUE.,
     . .FALSE.,.FALSE.,.FALSE.,.TRUE.,.TRUE.,.TRUE.,
     . .TRUE.,.TRUE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     . .FALSE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,.FALSE.,
     . .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE./
C
C---- Write coordinate data -------------------------------------------------
C    (Write only for first radionuclide, ipol = 1)
C
      IF(INUC.EQ.1) THEN
        IF(TPATH.NE.3) THEN            !Write exposure location coordinates
          WRITE(NBTF,400) EXPX,EXPY    ! for all except air.  One point
 400      FORMAT(F9.1,',"km",',F9.1,',"km",')
        ELSE        ! For air, loop on number of points
          DO IX = 1,NPOINTS
            WRITE(NBTF,400) EXPXA(IX),EXPYA(IX)
          END DO
        END IF
      ENDIF
C
C---- Write line of: parent name, progeny, time periods -------
C
      NL = LENWORD(LNAMES(1),12)
      IF(NUMNUC.LE.0) NUMNUC = 1
      WRITE(NBTF,100) LNAMES(1)(1:NL), LNAMES(1)(1:NL), NUMNUC-1, NTIME
 100  FORMAT('"',A,'","',A,'",',I2,',',I3,',')
c
c----- For each time period, write parent media concentrations, the progeny --
c      media concentrations for all exposure pathways with positive conc.
C
      DO IT = 1,NTIME
        START = BEFORE + FLOAT(IT-1)
        TREL = 1.
        DO IC = 1,NUMNUC
          NC = LENWORD(LNAMES(IC),12)
          IOUT = 0
          DO IP = 1,28
C
C----- Set values for output for selected pathways and non-zero concentrations-
C
            IF(PATH(IP) .AND. BPRINT(IP)) THEN
              IOUT = IOUT + 1
              EXPLABL(IOUT) = EXPLAB(IP)
              NLAB(IOUT)    = NEXPLAB(IP)
              EXPROUT(IOUT) = EXPRUT(IP)
              NROUT(IOUT)   = NEXPRUT(IP)
              EXPUNIT(IOUT) = EXPUN(IP)
              NPUN(IOUT)    = NEXPUN(IP)
c
c              if(EXPOST(IT,IC,IP) .lt. emin) EXPOST(IT,IC,IP)=0.0
c
c              EXPVAL(IOUT) = EXPOST(IT,IC,IP)
C
C TURN CONCENTRATIONS INTO BIOTA DOSES HERE:
C
              IF (IP .EQ. 4) EXPVAL(IOUT) = EXPOST(IT,IC,IP)*TPLADF(IC)      !  Terr. Plant
     .          + EXPOST(IT,IC,1)*CLDEX(IC)*24./0.037                        ! +air
     .          + EXPOST(IT,IC,3)*CLDSH(IC)*24.*225./0.037                   ! +soil
              IF (IP .EQ. 8) EXPVAL(IOUT) = EXPOST(IT,IC,IP)*TMAMDF(IC)      !  Terr. Mammal
     .          + EXPOST(IT,IC,1)*CLDEX(IC)*24./0.037                        ! +air
     .          + EXPOST(IT,IC,3)*CLDSH(IC)*24.*225./0.037                   ! +soil
	        IF (IP .EQ. 9) EXPVAL(IOUT) = EXPOST(IT,IC,IP)*RMAMDF(IC)      !  Rip. Mammal
     .          + EXPOST(IT,IC,1)*CLDEX(IC)*24/0.037                         ! +air
     .          + EXPOST(IT,IC,16)*CLDSH(IC)*RATIME(3)/365.*15./0.037        ! +shore
     .    + EXPOST(IT,IC,3)*CLDSH(IC)*(8760.- RATIME(3))/365.*225./0.037     ! +SOIL
     .          + EXPOST(IT,IC,14)*CLDIMR(IC)*RATIME(1)/365.                 ! +swim      
	        IF (IP .EQ.10) EXPVAL(IOUT) = EXPOST(IT,IC,IP)*TBIRDF(IC)      !  Terr. Bird
     .          + EXPOST(IT,IC,1)*CLDEX(IC) *24./0.037                       ! +air
     .          + EXPOST(IT,IC,3)*CLDSH(IC)*24.*225./0.037                   ! +soil
	        IF (IP .EQ.11) EXPVAL(IOUT) = EXPOST(IT,IC,IP)*TBIRDF(IC)      !  Rip.  Bird
     .          + EXPOST(IT,IC,1)*CLDEX(IC)*24./0.037                        ! +air
     .          + EXPOST(IT,IC,16)*CLDSH(IC)*RATIME(4)/365.*15./0.037        ! +shore
     .    + EXPOST(IT,IC,3)*CLDSH(IC)*(8760.- RATIME(4))/365.*225./0.037     ! +soil 
     .          + EXPOST(IT,IC,14)*CLDIMR(IC)*RATIME(2)/365.                 ! +swim
 	        IF (IP .EQ.12) EXPVAL(IOUT) = EXPOST(IT,IC,IP)*TEGGDF(IC)      !  bird  egg
     .          + EXPOST(IT,IC,1)*CLDEX(IC)*24./0.037                        ! +air
     .          + EXPOST(IT,IC,3)*CLDSH(IC)*24.*225./0.037                   ! +soil   
  	        IF (IP .EQ.18) EXPVAL(IOUT) = EXPOST(IT,IC,IP)*FISDF(IC) +     !  F or M Fish
     .          + EXPOST(IT,IC,16)*CLDSH(IC)*12.*15./0.037                   !  1/2SED 
     .          + EXPOST(IT,IC,14)*CLDIMR(IC)*24.                            ! +swim              
  	        IF (IP .EQ.19) EXPVAL(IOUT) = EXPOST(IT,IC,IP)*MOLDF(IC) +     !  F or M Mollusc
     .          + EXPOST(IT,IC,16)*CLDSH(IC)*24.*15./0.037                   ! + SED 
     .          + EXPOST(IT,IC,14)*CLDIMR(IC)*24.                            ! +swim
              IF (IP .EQ.20) EXPVAL(IOUT) = EXPOST(IT,IC,IP)*CRUDF(IC) +     !  F or M Crustacea
     .          + EXPOST(IT,IC,16)*CLDSH(IC)*24.*15/0.037                    ! + SED 
   	        IF (IP .EQ.21) EXPVAL(IOUT) = EXPOST(IT,IC,IP)*ALGDF(IC)       !  F or M Algae
     .          + EXPOST(IT,IC,14)*CLDIMR(IC)*24.                            !  swim
C 
            ENDIF
          END DO
c
C----- Write concentrations for parent at each time --------------------------
c
            IF(IC.EQ.1) THEN
              WRITE(NBTF,300) START,TREL,IOUT
 300   FORMAT(F5.1,',"yr",',F5.1,',"yr",',I3,',')
            ELSE
              WRITE(NBTF,100) LNAMES(IC)(1:NC),LNAMES(IC)(1:NC),IOUT
            ENDIF
            IF(IOUT.GT.0) THEN  ! write results
              DO IO = 1,IOUT
                WRITE(NBTF,200) EXPLABL(IO)(1:NLAB(IO)),
     .                          EXPROUT(IO)(1:NROUT(IO)),
     .                          EXPUNIT(IO)(1:NPUN(IO))
                WRITE(NBTF,201) EXPVAL(IO)
 200   FORMAT('"',A,'","',A,'","',A,'"')
 201   FORMAT(1PE10.3,',')
              END DO
            END IF
        END DO  ! End of loop on chain members
      END DO    ! End of loop on time periods
      RETURN
C
C----- END OF MODULE BTFDAT -------------------------------------------------
C
      END

