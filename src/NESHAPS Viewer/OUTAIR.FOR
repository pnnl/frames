C   NESHAPS:     OUTAIR.FOR             Version Date: 2 Feb-03               
C   Copyright 2003 by Battelle Memorial Institute.  
C                            All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE OUTair                                *
C                                                                            *
C  Subroutine OUTSRC Writes the input AIR CONTROLS(s) to the *.EPA file      *
C                                                                            *
C  Written by:       Bruce Napier                                            *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    2 Feb 03     BAN                                        *
C  Last Modified:    2 Feb 03     BAN                                        *
C                    24 APR 12    BAN   ADDED SOME FORMATTING                *
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
      SUBROUTINE OUTAIR
      INCLUDE 'DATABLKS.CMN'
	INCLUDE 'NESHAPS.CMN'
	LOGICAL FILTER
	CHARACTER*80 TEST, GETSTR, NAME(5)
	CHARACTER*120 SETDATA(5000)
	INTEGER NLINES
C
      DATA NAME/'BRIGGS OPEN COUNTRY','PASQUILL-GIFFORD (ISC)',
     . 'PASQUILL-GIFFORD (NRC)','BRIGGS URBAN','TURBULENCE STATS'/

C
C----- Get data from GID file -----------------------     
C
      FILTER = .FALSE.
      CALL GETSET(NGID,NERR,airNAME,NLINES,SETDATA,NSITE,FILTER)
C
C----- Was the AIR data set found? -------------------------------------------
C
      IF(NLINES.LE.0) THEN
        WRITE(NERR,2211) NLINES
 2211   FORMAT(' The src data set was not found, NLINES =',I5)
        NERROR = NERROR + 1
        GO TO 999
      ENDIF
C
      WRITE(NEPA,2) airNAME
2     FORMAT(//,'---- SUMMARY OF ATMOSPHERIC TRANSPORT RESULTS FOR ',
     .'MODULE ',A32,'------------------')
      IS=1
	IZ=0
	TEST=GETSTR(SETDATA,NLINES,'ARMETFILE     ',IS,IS,IZ,IZ,IZ,IZ)
	WRITE(NEPA,1)TEST
1     FORMAT('METEOROLOGICAL DATA TAKEN FROM FILE: ',a32,/)

	Sigpar  =   GETREAL(SETDATA,NLINES,'ARSIGPARM     ',IS,IS,IZ,
     .                  IZ,IZ,IZ)
	RISEMINSPD= GETREAL(SETDATA,NLINES,'ARMINRISESPD  ',IS,IS,IZ,
     .                  IZ,IZ,IZ)
	SIGYMINSHIFT=GETREAL(SETDATA,NLINES,'ARMINSIGYSHIFT',IS,IS,
     .                  IZ,IZ,IZ,IZ)
	TRANSR1=GETREAL(SETDATA,NLINES,'ARTRANSRESIST ',IS,IS,1,IZ,IZ,IZ)
	TRANSR2=GETREAL(SETDATA,NLINES,'ARTRANSRESIST ',IS,IS,2,IZ,IZ,IZ) 
	WINDMIN=GETREAL(SETDATA,NLINES,'ARMINWIND     ',IS,IS,IZ,IZ,IZ,IZ)  
	WRITE(NEPA,3) INT(SIGPAR), NAME(INT(SIGPAR))
3     FORMAT(' DISPERSION PARAMETERIZATION ',I1,2X,A80)
      WRITE(NEPA,4) RISEMINSPD,SIGYMINSHIFT,TRANSR1,TRANSR2,WINDMIN
4     FORMAT(' MINIMUM WIND SPEED DURING PLUME RISE',T60,F7.2,' M/S',/,
     .  ' SIGMA TO SHIFT TO SEMI-INFINITE CLOUD SHINE',T60,F7.2,' M',/,
     .  ' TRANSFER RESISTENCE FOR IODINE',T60,F7.2,' S/M',/,
     .  ' TRANSFER RESISTENCE FOR PARTICLES',T60,F7.2,' S/M',/,
     .  ' MAXIMUM WIND SPEED FOR CALM',T60,F7.2,' M/S',/,
     . 107('-'))
C
999   CONTINUE
      RETURN
	END