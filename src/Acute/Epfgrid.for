C   EPFGRID.FOR ACUTE               Version Date: 11-May-98  BAN            
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE EPFGRID                               *
C                                                                            *
C  Subroutine EPFDAT  writes data for a parent(&progeny) for one location for*
C       air releases for each exposure pathway                               *
C                                                                            *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    07-Jan-97                                               *
C  Last Modified:    11-May-98     BAN                                     *
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
C                                                                        
C
C==== Modification History ===================================================
C   Date      Who  Modification Description
C  ---------  ---  ------------------------------------------------------
C  07-Jan-97  DLS  Initial programming started
C  21-May-97  DLS  Revised to read from temporary file ATP
C  22-May-97  DLS  Moved exposure pathways names/units to common EXPNAM
C  18-Nov-97  DLS  Initial modification for use with ACUTE
C  29-Jan-98  DLS  Modification to print acute and chronic pathways in correct
C                  order for 1st year.
C  11-May-98  BAN  Update for 41x41 arrays
C  27-May-98  BAN  Fixed indexing on ONEPOINT locations
c  17 April 06 BAN  Expanded EXPOUT to 15 to include Indoor Air
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE EPFGRID(LNAMES,NUMNUC,ICALL)
C
C---- Include statements -----------------------------------------------------
c
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'CONIN.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'EXPALL.CMN'
      INCLUDE 'EXPNAM.CMN'
      INCLUDE 'EXINFO.CMN'
      INCLUDE 'AIRINFO.CMN'
      INCLUDE 'FLUX.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'TIMES.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      REAL EXPOUT(15,41,41),TYR(70), CVALIN(15)
      INTEGER NUMNUC, APTIN, CMIN
      CHARACTER*12 LNAMES(9)
      CHARACTER*2 EXPUNSV
C
C---- Data Statements --------------------------------------------------------
C
       DATA EMIN/1.E-24/
       DATA EXPUNSV/'Sv'/
C
C---- Write coordinate data --------------------------------------------------
C
      IF(ONEPOINT) THEN
        IF(ICALL.LE.0) THEN
          WRITE(NEPF,600) EXPX,EXPY
C          NX2 = 1 + (NXY - 1)/NAX2 : ban/dls 5/27/98
          NX2 = 1 + (NXY - 1)/NAX1
          NX1 = NXY - (NX2 - 1) * NAX1
          ICALL = ICALL + 1
        ENDIF
cCCc  ADDED ban 27 AUG 2012       
          NX2 = 1 + (NXY - 1)/NAX1
          NX1 = NXY - (NX2 - 1) * NAX1
      ELSE
        NPNTS = NAX1*NAX2
        IF(ICALL.LE.0) THEN
          DO IX = 1,NPNTS
            WRITE(NEPF,600) EXPXA(IX),EXPYA(IX)
600         FORMAT(F9.3,',"km",',F9.3,',"km",')
          END DO
          ICALL = ICALL + 1
        ENDIF
      ENDIF   ! ONEPOINT
C
C---- Read first line of temporary file ATP to get number of time periods ----
C
      REWIND(NATP)
      READ(NATP,1000) NTIME,IT,TYR(1)
 1000 FORMAT(2I3,F8.1,2I3)
      REWIND(NATP)
C
C---- Loop on number of time periods and extract data from ATP file for each time
C  The first data set in the ATP file has all data for the acute period plus
C  the first year chronic pathways.  This data set is read twice (ITN = 1 and 2).
C  The first time, data for acute pathways (air external and air inhalation) are
C  saved and printed to the EPF file.  The second time data for the chronic 1st
C  year pathways are saved and printed (all other).  For subsequent time periods
C  only chronic pathways are present, which are saved and printed.
C
      DO ITN = 1,NTIME+1
        IT = ITN - 1
        IF(IT.LE.0) IT = 1
C
C----  Count pathways to include for this time period -------------------------
C  Loop indices are set for either the acute period (1,2) or for chronic
C  periods (3-12 and 25, 28).
C
        NPA = 0
        TREL = 1.
        IF(ITN.EQ.1) THEN                ! First period, acute
          DO IP = 1,2
            IF(PATH(IP)) NPA = NPA + 1
	      If(IP.eq.2 .and. PATH(22)) NPA=NPA+1
          END DO
          TREL = ACUTIM
        ELSE                 ! After first period, chronic
          DO IP = 3,12
            IF(PATH(IP)) NPA = NPA + 1
          END DO
          IF(PATH(25)) NPA = NPA + 1
          IF(PATH(28)) NPA = NPA + 1
        ENDIF
C   minor output change here BAN 18 Dec 2001
	   tout = tyr(it)
         If(itn .eq. 2) tout = acutim
        IF(ITN.GT.1)  WRITE(NEPF,300) tout,TREL,NPA
 300      FORMAT(1PE10.2,',"yr",',1PE10.2,',"yr",',I3,',')
C
C---- Loop on number of chain members for current parent ---------------------
C
        DO IC = 1,NUMNUC
          NL = LENWORD(LNAMES(IC),12)
C
C---- Write first line of data set: parent name, progeny, time periods -------
C    
        IF(IC.EQ.1.AND.ITN.EQ.1) THEN
          WRITE(NEPF,100) LNAMES(1)(1:NL),LNAMES(1)(1:NL),NUMNUC-1,
     .                    NTIME+1
 100      FORMAT('"',A,'","',A,'",',I2,',',I2,',')
          WRITE(NEPF,300) TYR(1),TREL,NPA
        ENDIF
C
C----- Initialize output accumulation array to zero for current time and rad.--
C
          IF(ONEPOINT) THEN
            DO IP = 1,15
              EXPOUT(IP,1,1) = 0.0
            END DO
          ELSE
            DO IX1 = 1,NAX1
              DO IX2 = 1,NAX2
                DO IP = 1,15
                  EXPOUT(IP,IX1,IX2) = 0.0
                END DO
              END DO
            END DO
          ENDIF
C
C-----  Rewind temporary file to start reading and extracting information -----
C
          REWIND(NATP)
 10       READ(NATP,1000,END=50) ININ,ITIN,TYR(ITIN),APTIN,CMIN
          IF(ITIN.EQ.IT) THEN
            DO IN = CMIN,NUMNUC
              READ(NATP,1001) ININ,(CVALIN(I),I=1,15)
 1001         FORMAT(I3,15E10.2)
              IF(ININ.EQ.IC) THEN
                DO IP = 1,15
                  IF(ONEPOINT) THEN
                    IF(IP.EQ.1.AND.FINITE) THEN
                      IF(APTIN.EQ.1) EXPOUT(IP,1,1) =
     .          EXPOUT(IP,1,1) + CVALIN(IP) * EXTDOS(CMIN,NX1,NX2)
                    ELSE
                      IF(APTIN.EQ.1) EXPOUT(IP,1,1) =
     .          EXPOUT(IP,1,1) + CVALIN(IP) * AIRCIN(CMIN,NX1,NX2)
                      IF(APTIN.EQ.2) EXPOUT(IP,1,1) =
     .          EXPOUT(IP,1,1) + CVALIN(IP) * DEPDRY(CMIN,NX1,NX2)
                      IF(APTIN.EQ.3) EXPOUT(IP,1,1) =
     .          EXPOUT(IP,1,1) + CVALIN(IP) * DEPWET(CMIN,NX1,NX2)
                    END IF
                  ELSE
                    DO IX1 = 1,NAX1
                      DO IX2 = 1,NAX2
                        IF(IP.EQ.1.AND.FINITE) THEN
                          IF(APTIN.EQ.1) EXPOUT(IP,IX1,IX2) =
     .          EXPOUT(IP,IX1,IX2) + CVALIN(IP) * EXTDOS(CMIN,IX1,IX2)
                        ELSE
                          IF(APTIN.EQ.1) EXPOUT(IP,IX1,IX2) =
     .          EXPOUT(IP,IX1,IX2) + CVALIN(IP) * AIRCIN(CMIN,IX1,IX2)
                          IF(APTIN.EQ.2) EXPOUT(IP,IX1,IX2) =
     .          EXPOUT(IP,IX1,IX2) + CVALIN(IP) * DEPDRY(CMIN,IX1,IX2)
                          IF(APTIN.EQ.3) EXPOUT(IP,IX1,IX2) =
     .          EXPOUT(IP,IX1,IX2) + CVALIN(IP) * DEPWET(CMIN,IX1,IX2)
                        END IF
                      END DO
                    END DO
                  ENDIF    ! ONEPOINT
                END DO
              ENDIF
            END DO
          ELSE
            DO IN = CMIN,NUMNUC
              READ(NATP,1001) ININ
            END DO
          ENDIF
          GO TO 10
C
C----- Done reading ATP file, write data set for current chain member and time
C
 50       CONTINUE
          IF(IC.GT.1) THEN
            WRITE(NEPF,100) LNAMES(IC)(1:NL),LNAMES(IC)(1:NL),NPA
          ENDIF
C 
          NPS = 1
          IF(ITN.GT.1) NPS = 3
          NPX = 28
          IF(ITN.EQ.1) NPX = 3
          DO IP = NPS,NPX
	      IPX=IP
            IPT = 0
c            IF(PATH(IP)) THEN
c              IF(IP.LE.12) IPT = IP
c	        IF(IP.EQ.3 .AND. NPS .EQ. 1) IPT = 15
c	        IF(IP.EQ.3 .AND. NPS .EQ. 1) IPX = 22
c              IF(IP.EQ.25) IPT = 13
c              IF(IP.EQ.28) IPT = 14
c  Logic revised 28 Sept 2006 BAN
			If(nps .eq. 1) then
				If(path(ip)) then
					if(ip .le. 12)ipt=ip
	            endif
	            if((ip .eq. 3) .and. (path(22)))ipt=15
	            if((ip .eq. 3) .and. (path(22)))ipx=22
	            if((ip .eq. 3) .and. (.not. path(22))) ipt=0
	        elseif(nps .eq. 3) then
				if(path(ip)) then
					if(ip .le. 12)ipt=ip
	                if(ip .eq. 25)ipt=13
	                if(ip .eq. 28)ipt=14
	            endif
	        endif
c
c
c
            IF(IPT.GT.0) THEN
c
c----- Write line with exposure pathway names and units ----------------------
c
              IF(IPT.EQ.1.AND.FINITE) THEN
                WRITE(NEPF,200) EXPLAB(IPX)(1:NEXPLAB(IPX)),
     .                          EXPRUT(IPX)(1:NEXPRUT(IPX)),
     .                          EXPUNSV
              ELSE
                WRITE(NEPF,200) EXPLAB(IPX)(1:NEXPLAB(IPX)),
     .                          EXPRUT(IPX)(1:NEXPRUT(IPX)),
     .                          EXPUN(IPX)(1:NEXPUN(IPX))
 200   FORMAT('"',A,'","',A,'","',A,'",')
              ENDIF
C
C---- Test output value against minimum value --------------------------------
C
             IF(ONEPOINT) THEN
               IF(EXPOUT(IPT,1,1).LT.EMIN)EXPOUT(IPT,1,1)=0.
              WRITE(NEPF,500) EXPOUT(IPT,1,1)
             ELSE
              DO IX1 = 1,NAX1
                DO IX2 = 1,NAX2
                 IF(EXPOUT(IPT,IX1,IX2).LT.EMIN)EXPOUT(IPT,IX1,IX2)=0.
                END DO
              END DO
C
C---- Write a line of results for all points in the grid ---------------------
C
              WRITE(NEPF,500) ((EXPOUT(IPT,I,j),I=1,NAX1),j=1,nax2)
             ENDIF
 500          FORMAT(1PE10.2,',',359(E10.2,','))
            ENDIF
c           ENDIF
          END DO   ! Loop on pathways for printing
        END DO   ! End of loop on chain members
C
      END DO   ! End of loop on integrating time periods 
C
      REWIND(NATP)
      RETURN
C----- END OF MODULE EPFGRID -------------------------------------------------
      END

