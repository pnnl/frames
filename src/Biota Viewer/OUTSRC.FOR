C   NESHAPS:     OUTSRC.FOR             Version Date: 2 Feb-03               
C   Copyright 2003 by Battelle Memorial Institute.  
C                            All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE OUTSRC                                *
C                                                                            *
C  Subroutine OUTSRC Writes the input source term(s) to the *.EPA file       *
C                                                                            *
C  Written by:       Bruce Napier                                            *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    2 Feb 03     BAN                                        *
C  Last Modified:    5 Feb 08     BAN                                        *
C                                                                            *
C*****************************************************************************
C
C---- Modular Organization ---------------------------------------------------
C
C     Module of: GENII/NESHAPS
C     Called by: NESHAPS
C     Calls: NONE
C     Common blocks referenced: DATABLKS
C
C  Change History
C
C      19 Nov 2003  BAN  Modify for index (I3) change in FRAMES Known Source Module
C       5 Feb 2008  BAN  Capture and ignore "NOT FOUND" nuclides, minor cleanup
c
C
C-----------------------------------------------------------------------------
      SUBROUTINE OUTSRC
      INCLUDE 'DATABLKS.CMN'
	INCLUDE 'NESHAPS.CMN'
	LOGICAL FILTER, FT(4), GETLOG, skip, seqi
	CHARACTER*80 TEST, GETSTR 
	character*32 CASID, CASIDp, look
	CHARACTER*120 SETDATA(5000)
	INTEGER NLINES
	REAL*8 ONE, TWO, THREE, FOUR, FIVE, SIX, GETREAL
	DIMENSION RADIUS(4), DENSIT(4), CFLUX(4), DEPFRAC(4)
C
C----- Get data from GID file -----------------------     
C
      DO ISRC = 1, srcNUM
      FILTER = .FALSE.
      CALL GETSET(NGID,NERR,srcNAME(ISRC),NLINES,SETDATA,NSITE,FILTER)
C
C----- Was the src data set found? -------------------------------------------
C
      IF(NLINES.LE.0) THEN
        WRITE(NERR,2211) NLINES
 2211   FORMAT(' The src data set was not found, NLINES =',I5)
        NERROR = NERROR + 1
        GO TO 999
      ENDIF
C
      I3=3
C   Figure out index from the "dataset' parameter
C
      Itest=0
	iz=0
	do j=1,6
	look=getstr(setdata,nlines,'dataset       ',itest,iz,iz,iz,iz,iz)
	if(look .eq. 'aff:Air') i3=itest
	itest = itest+1
	end do
C
C
C
 	TEST=GETSTR(SETDATA,NLINES,'media         ',I3,IZ,IZ,IZ,IZ,IZ)
	WRITE(NEPA,1)srcNAME(ISRC),TEST
1     FORMAT(/,'---- SUMMARY OF INPUT DATA FOR ATMOSPHERIC RELEASES --',
     .'-----------------------------------------------------'/,
     .'FOR LOCATION ',A10,' THE RELEASE IS ',a32)
      ONE = GETREAL(SETDATA,NLINES,'one           ',I3,IZ,IZ,IZ,IZ,IZ)
      TWO = GETREAL(SETDATA,NLINES,'two           ',I3,IZ,IZ,IZ,IZ,IZ)
      THREE=GETREAL(SETDATA,NLINES,'three         ',I3,IZ,IZ,IZ,IZ,IZ)
      FOUR= GETREAL(SETDATA,NLINES,'four          ',I3,IZ,IZ,IZ,IZ,IZ)
      FIVE= GETREAL(SETDATA,NLINES,'five          ',I3,IZ,IZ,IZ,IZ,IZ)
      SIX = GETREAL(SETDATA,NLINES,'six           ',I3,IZ,IZ,IZ,IZ,IZ)					         '
C
      write(NEPA,2)ONE, TWO, THREE, FOUR, FIVE, SIX
2     FORMAT('   EXIT AREA, M2:',T60,1PE8.2,/,
     .'   EXIT HEIGHT, M: ',T60, 1PE8.2,/,
     .'   HEIGHT OF ADJACENT BUILDING, M:',T60,1PE8.2,/,
     .'   EXIT VELOCITY, M/S:',T60,1PE8.2,/,
     .'   EXIT TEMPERATURE, DEGREE C:',T60,1PE8.2,/,
     .'   AMBIENT TEMPERATURE, DEGREE C;',T60,1PE8.2,/)
C
      IFLXCNT = 0
      DO IFLX = 1,4
	  FT(IFLX) = GETLOG(SETDATA,NLINES,'Fluxtypes     ',IFLX-1,IZ,IZ,
     .                     IZ,IZ,IZ)
	  IF (FT(IFLX)) THEN
	    IFLXCNT = IFLXCNT+1
	    RADIUS(IFLX)= GETREAL(SETDATA,NLINES,'Radius        ',IFLX-1,
     .                     IZ,IZ,IZ,IZ,IZ)
	    DENSIT(IFLX)= GETREAL(SETDATA,NLINES,'Density       ',IFLX,IZ,
     .                     IZ,IZ,IZ,IZ)
	    DEPFRAC(IFLX)=GETREAL(SETDATA,NLINES,'reactivefrac  ',IFLX,IZ,
     .                     IZ,IZ,IZ,IZ)
	  END IF
	END DO ! ON IFLX
	DO ICNT = 1,4
	IF (FT(ICNT) .AND. (ICNT .EQ. 1)) THEN
	WRITE (NEPA,4) DEPFRAC(1)
4     FORMAT('   RELEASE MEDIUM 1 IS A GAS WITH NON-DEPOSITING FRACTION'
     .,' ',1PE8.2)
      ELSE IF (FT(ICNT) .AND. (ICNT .GT.1)) THEN
	WRITE (NEPA,3) ICNT,RADIUS(ICNT)
3     FORMAT('   RELEASE PARTICLE ',I1,' HAS RADIUS ',1PE8.2,' um')
	END IF
	END DO ! ON ICNT
C     READ AND PROCESS NUCLIDE INPUT INVENTORIES (NOTE: I3 = AIR)
C     Start with Parents
      WRITE(NEPA,7)
7     FORMAT(/,8X,'RADIONUCLIDE    TIME      GAS    PARTICLE 1  ',
     .'PARTICLE 2',
     .'  PARTICLE 3',/,8X,'RELEASE RATE    (yr)     (pCi/yr)   (pCi/yr)'
     .,'   (pCi/yr)  ',
     .' (pCi/yr)',/,8X,'------------   -------   --------   --------   '
     .,'--------   ---------')
C      I3 = 3
	DO INC = 1, NUMCON
	IT1=0
	CASID = GETSTR(SETDATA,NLINES,'casid         ',I3,INC,IZ,
     .                  IZ,IZ,IZ)
	WRITE(NEPA,5) casid
5     format('          ',a8)
1111  CONTINUE
      IT1=IT1+1
      CTIME = GETREAL(SETDATA,NLINES,'ctime         ',I3,INC,IZ,
     .                       IT1,IZ,IZ)
	  IF(((CTIME .NE. 0.0) .AND. (IT1 .GT. 1)) .OR. (IT1 .EQ. 1)) 
     .    THEN
	    DO IFT = 1,4
c	CFLUX(IFT) = GETREAL(SETDATA,NLINES,'cflux         ',I3,INC,IZ,
c     .                       IT1,IFT,IZ)
	CFLUX(IFT) = GETREAL(SETDATA,NLINES,'cval          ',I3,INC,IZ,
     .                       IT1,IFT,IZ)
	    END DO
	    write(nepa,6) ctime,(cflux(ift), ift=1,4)
6         format(19x, 5(3x,1pe8.2))
	    GO TO 1111
	  ELSE
	    GO TO 2222
	  END IF
2222    CONTINUE
C     Now do progeny, inside of parent loop
        do ipg = 1,nds(inc)
	IT2=0
	CASIDp = GETSTR(SETDATA,NLINES,'casid         ',I3,INC,ipg,
     .                  IZ,IZ,IZ)
	skip = .false.
	if (seqi(casidp,'NOT FOUND',9)) skip = .true.
	if (.not. skip) then
	WRITE(NEPA,5) casidp
1112  CONTINUE
      IT2=IT2+1
      CTIME = GETREAL(SETDATA,NLINES,'ctime         ',I3,INC,ipg,
     .                       IT1,IZ,IZ)
	  IF(((CTIME .NE. 0.0) .AND. (IT2 .GT. 1)) .OR. (IT2 .EQ. 1)) 
     .    THEN
	    DO IFT = 1,4
c	CFLUX(IFT) = GETREAL(SETDATA,NLINES,'cflux         ',I3,INC,ipg,
c     .                       IT1,IFT,IZ)
	CFLUX(IFT) = GETREAL(SETDATA,NLINES,'cval          ',I3,INC,ipg,
     .                       IT1,IFT,IZ)
	    END DO
	    write(nepa,6) ctime,(cflux(ift), ift=1,4)
	    GO TO 1112
	  ELSE
	    GO TO 2221
	  END IF
2221    CONTINUE
      endif  ! on skip
      end do ! on IPG
      END DO ! ON NUMCON
C
      END DO ! ON srcNUM DATA BLOCKS
C
999   CONTINUE
      RETURN
	END