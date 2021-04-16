C   NESHAPS:     OUTriv.FOR             Version Date: 20 Aug 09               
C   Copyright 2009 by Battelle Memorial Institute.  
C                            All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE OUTRIV                                *
C                                                                            *
C  Subroutine OUTRIV Writes the input SW  CONTROLS(s) to the *.EPA file      *
C                                                                            *
C  Written by:       Bruce Napier                                            *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:   20 Aug 09     BAN                                        *
C  Last Modified:   20 Aug 09     BAN                                        *
c                   24 Apr 12     BAN  cleaned up output formats             *
C                                                                            *
C*****************************************************************************
c
C
C==== Modular Organization ===================================================
C
C     Module of: GENII/NESHAPS
C     Called by: NESHAPS
C     Calls: NONE
C     Common blocks referenced: DATABLKS
C
      SUBROUTINE OUTRIV
      INCLUDE 'DATABLKS.CMN'
	INCLUDE 'NESHAPS.CMN'
	include 'swpar.cmn'
	LOGICAL FILTER
	CHARACTER*120 SETDATA(10000), getstr
	Character*23 rmodl(4), pmodl(3)
	INTEGER NLINES, numexp, rmod, pyrs, getint
      data rmodl/'Acute River','Chronic River','Chronic Near-shore Lake'
     .,'Chronic Flow Dilution'/
	data pmodl/'Plug Flow','Completely Mixed','Partially Mixed'/
C
C----- Get data from GID file -----------------------     
C
      FILTER = .FALSE.
      CALL GETSET(NGID,NERR,RIVNAME,NLINES,SETDATA,NSITE,FILTER)
C
C----- Was the riv data set found? -------------------------------------------
C
      IF(NLINES.LE.0) THEN
        WRITE(NERR,2211) NLINES
 2211   FORMAT(' The src data set was not found, NLINES =',I5)
        NERROR = NERROR + 1
        GO TO 999
      ENDIF
C
      WRITE(NEPA,1) RIVNAME
1     FORMAT(//,'---- SUMMARY OF SURFACE WATER TRANSPORT INPUTS FOR ',
     .'MODULE ',A32,'--------------------------------')
	IZ=0
	ipond=GETint(SETDATA,NLINES,'GNipond       ',IZ,IZ,IZ,IZ,IZ,IZ)
	if (ipond .ne. 0) then
	  pvol=GETreal(SETDATA,NLINES,'GNpondvol      ',IZ,IZ,IZ,IZ,IZ,IZ)
	  pout=GETreal(SETDATA,NLINES,'GNpondout      ',IZ,IZ,IZ,IZ,IZ,IZ)
	  pdis=GETreal(SETDATA,NLINES,'GNponddis      ',IZ,IZ,IZ,IZ,IZ,IZ)
	  pyrs=GETreal(SETDATA,NLINES,'GNpondyrs      ',IZ,IZ,IZ,IZ,IZ,IZ)
	end if
C
      If (ipond .gt. 0)Write(NEPA,2) pmodl(ipond)
2     format('  RECIRCULATION POND TYPE APPLIED: ',A16)
      IF (IPOND .EQ. 1)WRITE(NEPA,3)PVOL, POUT
3     FORMAT('     COOLING POND VOLUME, M^3: ',T60, 1PE8.2,/,
     . '     COOLING POND BLOWDOWN RATE, M^3/SEC: ',T60, 1PE8.2)
      IF (IPOND .EQ. 2)WRITE(NEPA,4)PVOL, POUT, PYRS
4     FORMAT('     COOLING POND VOLUME, M^3: ',T60, 1PE8.2,/,
     . '     COOLING POND BLOWDOWN RATE, M^3/SEC: ',T60, 1PE8.2,/,
     . '     PRIOR YEARS OF COOLING POND OPERATION, YR:'T60, I3)
      IF (IPOND .EQ. 3)WRITE(NEPA,5)PVOL, POUT, PDIS
5     FORMAT('     COOLING POND VOLUME, M^3: ',T60, 1PE8.2,/,
     . '     COOLING POND BLOWDOWN RATE, M^3/SEC: ',T60, 1PE8.2,/,
     . '     DISCHARGE RATE TO POND, M^3/SEC:',T60, 1PE8.2)
C
	numexp=GETint(SETDATA,NLINES,'GNSWExpNum     ',IZ,IZ,IZ,IZ,IZ,IZ)
	rmod=GETint(SETDATA,NLINES,'GNSWTYPE      ',IZ,IZ,IZ,IZ,IZ,IZ)
C
	WRITE(NEPA,6)rmodl(rmod+1)
6     FORMAT('  TYPE OF RIVER MODEL USED: ',a23)
C
C---- Test for acute or chronic surface water model --------------------------
C
      IF(rmod.EQ.0) THEN                        ! Start acute
C                 
C----- Get parameters for the acute river model -----------------------------
C
          IL = numexp
            IZ = 0
            SWTT(IL)= GETREAL(SETDATA,NLINES,'GNSWTT        ',IL,IZ,
     .                          IZ,IZ,IZ,IZ)
            SWFLOW(IL) = 0.0
            SWFLOW(IL) = GETREAL(SETDATA,NLINES,'GNSWDSCHG     ',IL,IZ,
     .                          IZ,IZ,IZ,IZ)
	write(nepa,10)swflow(il), swtt(il)
C
10    format('     RIVER FLOW RATE, M^3/SEC',T60, 1PE8.2,/,
     . '     TRAVEL TIME TO INLET LOCATION, HR: ',T60, 1PE8.2)
c Start chronic input
C
      ELSE IF ((rmod .eq. 1) .or. (rmod .eq. 3)) then
C----- River width (SWIDTH), depth (SWDPTH), velocity (SWFLOW) --------------
C      downstream distance (SWLSX), cross-stream distance (SWOSY)
C
          IL = numexp
          IF(rmod.EQ.1) THEN
C  - read data for chronic river model
            SWIDTH(IL)=GETREAL(SETDATA,NLINES,'GNSWIDTH      ',IL,IZ,
     .                       IZ,IZ,IZ,IZ)
            SWDPTH(IL)=GETREAL(SETDATA,NLINES,'GNSWDPTH      ',IL,IZ,
     .                          IZ,IZ,IZ,IZ)
            SWFLOW(IL)=GETREAL(SETDATA,NLINES,'GNSWVELOC     ',IL,IZ,
     .                          IZ,IZ,IZ,IZ)
            SWLSX(IL)= GETREAL(SETDATA,NLINES,'GNSWLSX       ',IL,IZ,
     .                          IZ,IZ,IZ,IZ)
            SWOSY(IL)= GETREAL(SETDATA,NLINES,'GNSWOSY       ',IL,IZ,
     .                          IZ,IZ,IZ,IZ)
            SWTT(IL)= GETREAL(SETDATA,NLINES,'GNSWTT        ',IL,IZ,
     .                          IZ,IZ,IZ,IZ)
	write(nepa,7)swidth(il), swdpth(il), swflow(il), swlsx(il), 
     .            swosy(il), swtt(il)
C
7     format('     RIVER WIDTH, M: ',T60, 1PE8.2,/,
     . '     RIVER DEPTH, M: ',T60, 1PE8.2,/,
     . '     RIVER VELOCITY, M/SEC:',T60, 1PE8.2,/,
     . '     DOWNSTREAM DISTANCE TO INLET, M',T60, 1PE8.2,/,
     . '     OFFSHORE DISTANCE TO INLET, M',T60, 1PE8.2,/,
     . '     DOWNSTREAM TRAVEL TIME, HR',T60, 1PE8.2)
          ELSE IF(rmod.EQ.3) THEN
C  - read data for flow dilution model
            SWFLOW(IL)=GETREAL(SETDATA,NLINES,'GNSWDSCHG     ',IL,IZ,
     .                          IZ,IZ,IZ,IZ)
            SWTT(IL)= GETREAL(SETDATA,NLINES,'GNSWTT        ',IL,IZ,
     .                          IZ,IZ,IZ,IZ)
            TREL = GETREAL(SETDATA,NLINES,'GNSWTREL      ',IZ,IZ,IZ,
     .                     IZ,IZ,IZ)
	write(nepa,8)swflow(il), swtt(il), trel
C
8     format('     RIVER FLOW RATE, M^3/SEC',T60, 1PE8.2,/,
     . '     TRAVEL TIME TO INLET LOCATION, HR: ',T60, 1PE8.2,/,
     . '     RELEASE PERIOD, YR:',T60, 1PE8.2)
          ENDIF
C
        ELSE IF (rmod .eq. 2) THEN
C  chronic near-shore lake model input parameters
C  
C----- Shore source depth (SWDZ), depth (SWDPTH), velocity (SWFLOW) ---------
C      downstream distance (SWLSX), cross-stream distance (SWOSY)
C
          IL = numexp
            SWDPTH(IL)=GETREAL(SETDATA,NLINES,'GNSWDPTH      ',IL,IZ,
     .                          IZ,IZ,IZ,IZ)
            SWFLOW(IL)=GETREAL(SETDATA,NLINES,'GNSWVELOC     ',IL,IZ,
     .                          IZ,IZ,IZ,IZ)
            SWLSX(IL)= GETREAL(SETDATA,NLINES,'GNSWLSX       ',IL,IZ,
     .                          IZ,IZ,IZ,IZ)
            SWOSY(IL)= GETREAL(SETDATA,NLINES,'GNSWOSY       ',IL,IZ,
     .                          IZ,IZ,IZ,IZ)
            SWTT(IL)= GETREAL(SETDATA,NLINES,'GNSWTT        ',IL,IZ,
     .                          IZ,IZ,IZ,IZ)
            SWDZ=GETREAL(SETDATA,NLINES,'GNSWZDEP      ',IZ,IZ, 
     .                       IZ,IZ,IZ,IZ)
            TREL = GETREAL(SETDATA,NLINES,'GNSWTREL      ',IZ,IZ,IZ,
     .                     IZ,IZ,IZ)
      write(nepa,9)swdpth(il), swflow(il), swlsx(il), swosy(il),
     .             swtt(il), swdz, trel
C
9     format(('     LAKE DEPTH, M: ',T60, 1PE8.2,/,
     . '     CURRENT VELOCITY, M/SEC:',T60, 1PE8.2,/,
     . '     DOWNSTREAM DISTANCE TO INLET, M',T60, 1PE8.2,/,
     . '     OFFSHORE DISTANCE TO INLET, M',T60, 1PE8.2,/,
     . '     DOWNSTREAM TRAVEL TIME, HR',T60, 1PE8.2,/,
     . '     DEPTH OF INLET, M',T60,1PE8.2,/,
     . '     DURATION OF RELEASE, YR',T60,1PE8.2))
        ENDIF
C
999   CONTINUE
      RETURN
	END