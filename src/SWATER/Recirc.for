C   RECIRC.FOR                          Version Date: 17-AUG-2009            *
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.     *
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE RECIRC                                *
C                                                                            *
C  Subroutine RECIRC evaluates water concentration at a location for one     *
C             decay chain for release TO IMPOUNDMENTS                        *
C                                                                            *
C  Written by:       Bruce Napier                                            *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    17-Aug-09  BAN                                          *
C  Last Modified:    17-Aug-09  BAN                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: GENII/SWATER
C     Called by: SWCHRON
C     Calls: NONE
C     Common blocks referenced: SWPAR, SWCON
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     IPOND      U     INTEGER           TYPE OF POND 0=NONE, 1=PLUG FLOW, 
C                                        2=FULLY MIXED, 3=PARTIALLY MIXED
C     PONDVOL    U     REAL              POND VOLUME, M3
C     PONDOUT    U     REAL              POND BLOWDOWN RATE, M3/SEC
C     PONDDIS    U     REAL              PLANT DISCHARGE RATE, M3/SEC
C     PONDYRS    U     REAL              YEARS OF PRIOR POND OPERATION
C     QSWR       S     REAL              'EQUIVALENT' SOURCE TERM EXITING POND                                                 
C                                                                             
C-----------------------------------------------------------------------------
C     
C==== Modification History ===================================================
C
C   Date      Who  Modification Description
C  ---------  ---  ------------------------------------------------------------
C  17-Aug-09  BAN  Add NRC Recirculation models
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE RECIRC
C
C==== COMMON Block Definitions ===============================================
C
        include 'SWPAR.CMN'
        INCLUDE 'SWCON.CMN'
	  INCLUDE 'DECAY.CMN'
C
C==== DIMENSION Statements ===================================================
C
      REAL DUMMY(9), sum0(9), sum1(9), sum2(9)
C 
      dummy = 0.0
C  NO RECIRCULATION; USE SOURCE DIRECTLY
      IF (IPOND .EQ. 0) RETURN
C
C  PLUG FLOW; TRAVEL TIME DECAY
      IF (IPOND .EQ. 1) THEN
	  PONDDK = PONDVOL/PONDOUT/86400.
	  CALL CHAIN(PONDDK, DUMMY, QSW, QSWR, 0)
C
C  FULLY MIXED, INTEGRAL DECAY
      ELSE IF (IPOND .EQ. 2) THEN
C      
        DO I=1,9
	    DUMMY(I) = PONDOUT/PONDVOL*86400.
	    QSWR(I) = QSW(I)/PONDVOL*PONDOUT*86400.
	  ENDDO
	  CALL CHAIN(PONDYRS*365.25, DUMMY, QSWR, QSWR, 1)
	  DUMMY = 0.0
C  
C  PARTIALLY MIXED, COMPLEX SERIES EXPANSION
      ELSE IF (IPOND .EQ. 3) THEN
        sum0 = 0.0
	  sum1 = 0.0
	  sum2 = 0.0
	  TIMPND = PONDVOL/(PONDDIS + PONDOUT)/86400.
        do i = 1, nuc
	    sum1(i) = qsw(i)*pondout/(ponddis + pondout)
	  end do
	  call chain (timpnd, dummy, sum1, sum1, 0)
	do j=1,50
	  do i = 1, nuc
	     sum2(i) = ponddis/(ponddis +pondout)*(sum1(i) - sum0(i))
	  end do
	  call chain(timpnd, dummy, sum2, sum2, 0)
	  do i = 1, nuc
	     sum2(i) = sum2(i) + sum1(i)
	     sum0(i) = sum1(i)
	     sum1(i) = sum2(i)
	  end do
	  if (((sum1(1) - sum0(1))/sum1(1)) .lt. 0.00001) go to 99
	end do
   99 continue
        DO J = 1, NUC
	    QSWR(J) = sum2(J)
	  ENDDO      
      END IF
	RETURN
	END
