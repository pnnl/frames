C     MEPAS RADCON:S_TERM.FOR             Version Date: 05-15-1998
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.
C*****************************************************************************
C                                                                            *
C                             SUBROUTINE S_TERM                              *
C                                                                            *
C  S_TERM is a subroutine called by LEACHV that computes the decrease in the *
C  source term for the groundwater transport pathway when the flux in known  *
C  (ISOURCE = 2 and ISTYPE = 1 or 2).  As the source term decreases with     *
C  time, the release rate from the contaminated zone will decease;           *
C   approaching zero.                                                        *
C                                                                            *
C  Written by:       John W Buck                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    02/03/92                                                *
C  Last Modified:    05/15/98 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCONPC
C     Called by: LEACHV
C     Calls to:  None
C=============================================================================
C     RADCONPC --
C                |-> SOURCE --
C                             |-> LEACHV -- 
C                                          |-> S_TERM
C=============================================================================
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- ----- ------   -------------  --------------------------------
C
C
C
C
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     02/21/92     JWB  Common blocks converted to include statements
C     07/01/92     JLS  Open unit 16 and remove time conversion to sec.
C     02/03/93     JWB  RAAS modification for ISS
C     02/08/93     JWB  Added Mass Balance option to read in computed fluxes
C     05/16/94     JPM  Removed lines that added another flux point on the
C                       end to account for the tail of the flux curve.
C     11/17/95     JPM  Removed the lines that set the first time and flux
C                       both equal to zero.  Was causing mass balance problems
C                       via the connection between RADCON and the Source Term
C                       Code.
C     11/11/96     JPM  Changed units from Ci to pCi (PH1-E006).
C     05/15/98     PDM  Made extensive changes to allow for a variable number
C                       of infiltration rates (see RAD-E054 for details).
C
C==== SUBROUTINE Statement ===================================================
C
      SUBROUTINE S_TERM(istype,LEACH_RATE,WASTE_NAME,TLIFE)
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'INTER.WTN'
      INCLUDE 'INDEX9.WTN'
      INCLUDE 'INIT.WTN'
      INCLUDE 'INDEX3.WTN'
      INCLUDE 'COM21.WTN'
      INCLUDE 'extflux.WTN'
      INCLUDE 'CAP.WTN'
C
C==== Variable Declarations ==================================================
C                    In Situ Solidification Variables
C
      CHARACTER CHAR_KEY*15,WASTE_NAME*15
      character CON_NAME(MAXCON)*20
      character CAS_ID(MAXCON)*12
      REAL*4 PI,WASTE_VOL,col_SAREA,N_TIME,RATIO
      REAL*4 TLIFE(MAXCON)
      REAL*4 L_RATE
      REAL*4 RATIO_LIM,LEACH_RATE
      REAL*4 TOT_TIME,NN_TIME,new_inv,col_vol
      REAL*4 INVEN(MAXNTI,MAXCON),CON_SOL(MAXNTI,MAXCON)
      REAL*4 CON_DIFF(MAXCON)
      INTEGER*2 I,J,POL_MARKER,FILE_MARKER,lim_flag
      INTEGER*4 CON_NUM,col_num
C
C                    Mass Balance Variables
C
      real*4    leachflux, runflux, runoff, infilt, oldinf
      real*4    new_inven(MAXNTI,MAXCON)
      integer*4 ind_lch, ind_run,wmsfil, infcnt
C
C==== DATA Definitions =======================================================
C
      DATA PI          /3.141592654/
      DATA RATIO_LIM   /1.0E-04/
      DATA POL_MARKER  /-88/
      DATA FILE_MARKER /-99/
C
C*****************************************************************************
C     THE INVENTORY OF EACH CONSTITUENT WILL DECREASE IN THE SOIL WITH TIME
C     BECAUSE OF LEACHING TO GROUNDWATER.
C     VOLATILIZATION, SURFACE RUNOFF, AND SUFRACE WIND SUSPENSION WILL BE
C     ADDED IN A LATER VERSION OF RADCONCP.
C*****************************************************************************
C     INPUT PARAMETERS FOR COMPUTING DEDUCTION OF SOURCE TERM FOR WASTE FORMS
C*****************************************************************************
C
      open (unit=15, file=filenm//'.wms', status='OLD',iostat=wmsfil)
      if (wmsfil .NE. 0) goto 905
      READ(15,100) CHAR_KEY
100   FORMAT(A15)
      IF( CHAR_KEY .EQ. ' WASTE FORM' ) THEN
	      if( isssfl .eq. 0 ) then
		OPEN (UNIT=16, FILE=FILENM//'.SLS', STATUS='UNKNOWN')
		L_RATE = LEACH_RATE * 365.25
	      endif
	    BACKSPACE( 15 )
	    READ(15,110) WASTE_NAME,WASTE_VOL,col_SAREA,N_TIME,col_vol,
     +               CON_NUM
110   FORMAT(15X,A15,3E10.2,I5)
	    READ(15,100) CHAR_KEY
	      IF( CHAR_KEY .EQ. 'DIFFUSION COEFF' ) THEN
		BACKSPACE( 15 )
		DO I = 1, CON_NUM
		  READ(15,120)CON_NAME(I),CAS_ID(I),CON_DIFF(I)
120   FORMAT(20X,A20,A12,E10.2)
		  con_diff(i) = con_diff(i) * 3.16E+07
		END DO
	      END IF
C*****************************************************************************
C   Compute the number of grout columns (integer number)
C                      JWB and LMB - 02/03/93
C*****************************************************************************
	    col_num = (waste_vol/col_vol) + 0.5
CKJC        write(*,934) col_num
934     format(' S-TERM --> col_num = ',i5)
C*****************************************************************************
C     THE FOLLOWING RELEASE RATE EQUATION IS BASED ON THE EFFECTIVE DIFFUSION
C     COEFFICIENT FORMULA FOR A SEMI-INFINITE CYLINDRICAL SOLID FORM FROM
C     (WHYATT ET AL. 1991) GROUT PA REPORT.
C*****************************************************************************
	    DO I = 1, NUMCON
	      NN_TIME = N_TIME
	      RATIO = RATIO_LIM
	      J = 0
	      TOT_TIME = 0.0
	      SUM_INVEN = 0.0
	      WRITE(16,129)
129       FORMAT(/'  POL  NUM   CINVEN   NEW INVEN   TIME      PNT    ',
     +           ' DIFFUSION ',/,11X,'(g OR pCi)  (g OR pCi)',
     +            ' (Years) (g OR pCi/Yr) (cm/Yr)' )
	      DO WHILE( (RATIO .GE. RATIO_LIM) .AND.
     +                        (TOT_TIME .LT. (TFINAL-1.0)) )
		J = J + 1
		IF( J .EQ. 101 ) THEN
			NN_TIME = NN_TIME * 10.0
		ELSE IF( J .EQ. 191 ) THEN
			NN_TIME = NN_TIME * 100.0
		END IF
		IF( J .EQ. 1 ) THEN
		  new_inv = CINVEN(I)
		  INVEN(J,I) = CINVEN(I)
		ELSE
		  INVEN(J,I) = CINVEN(I) - SUM_INVEN
		  RATIO = INVEN(J,I) / CINVEN(I)
		END IF
C*****************************************************************************
C  If release is solubility limited, change initial inventory for diffusion
C     controlled release for next time step (unitl diffusion controlled)
C  Else release rate is diffusion controlled and initial inventory is constant
C                      JWB and LMB - 02/03/93
C*****************************************************************************
		if( lim_flag .eq. 1 ) new_inv = INVEN(J,I)
		TOT_TIME = TOT_TIME + NN_TIME
		TIME(J,I) = ((SQRT(TOT_TIME) +
     1                    SQRT(TOT_TIME-NN_TIME))/2.0)**2.0
		PNT(J,I) = col_num*(SQRT(CON_DIFF(I)/PI/TIME(J,I))*
     1                    new_inv*col_SAREA/col_VOL)
		CON_SOL(J,I) = PNT(J,I) / (col_SAREA*L_RATE)
C*****************************************************************************
C   If release is solubility limited (CON_SOL > SOLLIM)  lim_flag =1
C        and recompute flux (PNT)
C   If release is diffusion limited (CON_SOL < SOLLIM)  lim_flag =0
C                      JWB and LMB - 02/03/93
C*****************************************************************************
		IF( CON_SOL(J,I) .GT. SOLLIM(I) ) THEN
		  PNT(J,I) = SOLLIM(I)*col_SAREA*L_RATE
		  lim_flag = 1
		else
		  lim_flag = 0
		END IF

		SUM_INVEN = SUM_INVEN + PNT(J,I) * NN_TIME
		WRITE(16,130) I,J,CINVEN(I),INVEN(J,I),TIME(J,I),
     +                   PNT(J,I),CON_DIFF(I)
130   FORMAT(I5,I5,1P,5E10.2)
	      END DO
	    NUM(I) = J
c           TIME(1,I) = 0.0
	    TLIFE(I) = TIME(NUM(I),I)
	    WRITE(16,135) POL_MARKER
135   FORMAT(I5)
	    END DO
	    WRITE(16,135) FILE_MARKER
	    CLOSE(16)
      ELSE
	  IF( CHAR_KEY .EQ. ' MASS BALANCE' ) THEN
C****** Read in indexes and flow rates for leaching and runoff ********
	    read(15,901) ind_lch, ind_run, num_con, leach_rate, runoff
901     format(3i5,2e10.3)
	    runoff = runoff * 24.0
C****** Loop to read in constituent name, casid and inventory ********
	    do i = 1, num_con
	      read(15,903) con_name(i),cas_id(i),cinven(i)
903       format(a20,a12,e10.2)
cjpm      time(1,i)=0.0
cjpm      pnt(1,i)=0.0
	      j = 1
	      index = 0
	      infcnt = 0
	      infilt = -1.0
C****** Read the various fluxes (g or pCi/yr) ********
c
c pdm Skip over that part of the *.wms file that has already been
c pdm   used (i.e., for prior infiltration rates). time(j,i) and flux
c pdm   at termination of loop correspond to the first time with the
c pdm   new infiltration rate (i.e., tremed(incr-1) for incr>1)
c
	      do while( (index .ge. 0) .and. (incr .gt. infcnt) )
		oldinf = infilt
		read(15,904) index,time(j,i),new_inven(j,i),
     +                    leachflux, runflux, infilt
		if (infilt .ne. oldinf) infcnt = infcnt + 1
	      enddo
C
C If index is less than zero, then records for this constituent in the WMS
C file end before the next infiltration rate is found.  In that case,
C set variables for zero mass flux for 1 year.  JPM
C
	      if (index.lt.0) then
		num(i) = 2
		pnt(1,i) = 0.0
		pnt(2,i) = 0.0
		time(1,i) = 0.0
		time(1,i) = 1.0
		tlife(i) = 1.0
	      else
c
c pdm Read in that part of the *.wms file that corresponds to the
c pdm   new infiltration rate. 
c
		oldinf = infilt
		do while( (index .ge. 0) .AND. (infilt .eq. oldinf) )
		  if( istype .lt. 3 ) then
			pnt(j,i) = leachflux
		  else
			pnt(j,i) = runflux
		  endif
c pdm             oldinf = infilt
		  j = j + 1
		  read(15,904) index,time(j,i),new_inven(j,i),
     +                    leachflux, runflux, infilt
		enddo
c
c pdm This if statement added to include in the current simulation
c pdm   all time up to the point at which the infiltration rate changes.
c pdm   Also set NUM = j or j-1 depending on whether all fluxes for the
c pdm   current constituent have been read.
		if (index .ge. 0) then
		  if( istype .lt. 3 ) then
		      pnt(j,i) = leachflux
		  else
		      pnt(j,i) = runflux
		  endif
		  num(i) = j
		else
		  num(i) = j-1
		endif
c
c pdm Read through *.wms file to get to next constituent
c        
		do while( index .ge. 0 )
		  read(15,'(i5)') index
c pdm             read(15,904) index,time(j,i),new_inven(j,i),
c pdm     +                    leachflux, runflux, infilt
		enddo
C
C***********************************************************************
C     Set TLIFE; set leach_rate to cm/day  -  JPM 05/16/95
C***********************************************************************
C
c pdm           num(i) = j-1
		tlife(i) = time(num(i),i) - time(1,i)
		if (incr.ge.2) then
		  subtr = time(1,i)
		  do kk = 1,num(i)
		    time(kk,i) = time(kk,i) - subtr
		  enddo
		endif
	      endif
	      leach_rate = oldinf * 24.0

CKJC      write(*,*) time(1,i),time(num(i),i),tlife(i)

904       format(i5,5e10.3)
	    enddo
	  end if
      END IF

      CLOSE(15)
905   RETURN
      END
	
