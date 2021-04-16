      SUBROUTINE ATOREAD(iop, NDIST, DIST, NDIR, DIR, airname)
C     A SUBROUTINE TO READ THE INITIAL PARTS OF THE ATO FILE AND GET THE 
C     GRID DEFINITION
C
C     B.A. NAPIER
C     26 DECEMBER 2001
C
c     rev. 28 Dec 2001 BAN to duplicate ATO file in EPA output
c     rev. 7 Feb 2002  BAN to remember to put in print loop over nuclides
c     rev 3 Dec 2009   BAN to allow zero-entry nuclides to be first (xq files)
c
C
      INCLUDE 'NESHAPS.CMN'
      DIMENSION DIST(41), DIR(41)
	CHARACTER*1 DUM
	CHARACTER*8 CHRONAC,SQRAD,ANUKE,ASOURCE
      CHARACTER*10 OU, AU, POLAR
      CHARACTER*5 WETDRY
      CHARACTER*6 GRID,POINTS,RGRID
      CHARACTER*20  CH,ONAME,OTYPE,PARTICLE
      REAL TY,  VALIN(10), TIMP(10), AUX1(10), AUX2(10)
      CHARACTER*8 TU, UNIT1, UNIT2
      CHARACTER*20 CNAMIN, CIDIN
	CHARACTER*32 airname
      LOGICAL SEQI
C
C---- Data Statements --------------------------------------------------------
C
      DATA GRID /'grid  '/,  POINTS /'points'/
      DATA POLAR/'polar     '/

c
C
      if (iop .eq. 1) then
123   READ(NATO,*) ASOURCE, NLINES
c
      if (seqi(asource,airname,5))then
c
	READ(NATO,*)NHEADA
	DO I = 1, NHEADA
	  READ(NATO,'(A1)')DUM
	END DO
	READ(NATO,*)NASETS
	READ(NATO,*)NGP
	DO I = 1,NGP
	  READ(NATO,'(A1)')DUM
	END DO
	READ(NATO,*)CHRONAC,SQRAD,RGRID,NUKEA
321	READ(NATO,*)DUM, ANUKE,NTIMES,NPROG
	READ(NATO,*)NTIMES,DUM,NBLOCKS
c   a minor fix for zero blocks BAN 3 Dec 2009
      if (nblocks .gt. 0) then
	  READ(NATO,*)DUM,DUM,DUM,DUM,NDIST,DUM,NDIR,DUM
	  READ(NATO,*)(DIST(I),I = 1, NDIST)
	  DO I = 1, NDIR
	    READ(NATO,*) DIR(I)
	  END DO
	else
	  go to 321
	end if
c
      rewind(nato)
	else
	do i=1,nlines
	read(nato,*)dum
	end do
	go to 123
	end if
	end if
C

      if(iop .eq. 2) then
124      READ(NATO,*) ASOURCE, NLINES
c
      if (seqi(asource,airname,5))then
c
	READ(NATO,*)NHEADA
	DO I = 1, NHEADA
	  READ(NATO,'(A1)')DUM
	END DO
C
C
      WRITE(NEPA,111)
  111 FORMAT(/,107('-'),/,'MIRROR OF ATMOSPHERIC TRANSPORT RESULT (FROM'
     .,' ATO FILE)')
	READ(NATO,*)NASETS
      DO ISET = 1, NASETS
	READ(NATO,*)NGP, DUM
	DO I = 1,NGP
	  READ(NATO,*)PARTICLE,DIAM,UNIT1,DENS,UNIT2
        WRITE(NEPA,112)PARTICLE,DIAM,UNIT1,DENS,UNIT2
  112   FORMAT(A10, 1PE10.1,1X,A6,1PE10.1,1X,A6)
	END DO
	READ(NATO,*)CHRONAC,SQRAD,RGRID,NUKEA
	WRITE(NEPA,113)CHRONAC,SQRAD,RGRID,NUKEA
  113 FORMAT('THE RELEASE WAS ',A7,' INTO A ',2A8,' WITH',I3,' PARENTS')
c
      do ink = 1,nukea
c-------- Read chronic data set ----------------------------------------------
c     First read name of parent, number of time periods, and number of progeny
c
      READ(NATO,*) CNAMIN,CIDIN,NTM,NPRG
      WRITE(NEPA,114) CNAMIN,CIDIN,NTM,NPRG
  114 FORMAT(A15,A8,2I3)
C
C----- For each time period read time and number of input parameter
C      types included for this contaminant
C
      DO IT = 1, NTM
        READ(NATO,*) TIMP(IT), TU, NOPS
        WRITE(NEPA,115) TIMP(IT), TU
  115   FORMAT('TIME PERIOD',F8.4,1X,A7)
C
C----- For each input parameter type to read
C
          DO IO = 1,NOPS
            READ(NATO,*) ONAME,OTYPE,WETDRY,OU,NAX1,CH,NAX2,AU
            WRITE(NEPA,116) ONAME,OTYPE,WETDRY,OU,NAX1,CH,NAX2,AU
  116       FORMAT(A20,A5,A6,A10,I3,1X,A5,I3,1X,A5)
            IF(SEQI(RGRID,GRID,6)) THEN          ! For grid input structure
              READ(NATO,*) (AUX1(I),I=1,NAX1)
              WRITE(NEPA,117) (AUX1(I),I=1,NAX1)
  117         FORMAT(10X,10F10.1)
              DO IX = 1,NAX2
                READ(NATO,*) AUX2(IX),(VALIN(I),I=1,NAX1)
                WRITE(NEPA,118) AUX2(IX),(VALIN(I),I=1,NAX1)
  118           FORMAT(F10.1,10(1PE10.1))
	        END DO
C
C-------- End of input for "grid" input method -------------------------------
C         If points, read input data
C
            ELSE IF (SEQI(RGRID,POINTS,6)) THEN   ! For points input structure
              NPOINTS = NAX1
              READ(NATO,*) CH    ! Line with receptor point names
			WRITE(NEPA,*)CH 
              READ(NATO,*) (AUX1(I),I=1,NAX1) ! Line with point x-axis values
              WRITE(NEPA,*)(AUX1(I),I=1,NAX1)
              READ(NATO,*) (AUX2(I),I=1,NAX2) ! Line with point y-axis values
	        WRITE(NEPA,*)(AUX2(I),I=1,NAX2)
              READ(NATO,*) ICH,(VALIN(I),I=1,NPOINTS) ! Extra line to skip (-99)
	        WRITE(NEPA,*)ICH,(VALIN(I),I=1,NPOINTS)
            END IF
C
          END DO    ! End of do on IO to NOPS
      END DO    ! End of do on IT to NTM

C
C---------- Repeat for any progeny
c
      IF(NPRG.GT.0) THEN
        DO INP = 1,NPRG
          READ(NATO,*) CNAMIN,CIDIN,NTMP
          WRITE(NEPA,119) CNAMIN,CIDIN,NTMP
  119     FORMAT(/,A15,A8,I3)
          DO IT = 1, NTM
            READ(NATO,*) TY, TU, NOPS
            WRITE(NEPA,120) TY, TU
  120       FORMAT('TIME PERIOD',F8.4,1X,A7)
            DO IO = 1,NOPS
              READ(NATO,*) ONAME,OTYPE,WETDRY,OU,NAX1,CH,NAX2,AU
              WRITE(NEPA,119) CNAMIN,CIDIN,NTMP
			WRITE(NEPA,116) ONAME,OTYPE,WETDRY,OU,NAX1,CH,NAX2,AU
              IF(SEQI(RGRID,GRID,6)) THEN    ! Grid input structure
                READ(NATO,*) (AUX1(I),I=1,NAX1)
                WRITE(NEPA,117) (AUX1(I),I=1,NAX1)
                DO IX = 1,NAX2
                  READ(NATO,*) AUX2(IX),(VALIN(I),I=1,NAX1)
                  WRITE(NEPA,118) AUX2(IX),(VALIN(I),I=1,NAX1)
                END DO
              ELSE IF (SEQI(RGRID,POINTS,6)) THEN  ! Points input structure
                NPOINTS = NAX1
                READ(NATO,*) CH     ! Line with receptor point names
			  WRITE(NEPA,*)CH 
                READ(NATO,*) (AUX1(I),I=1,NAX1) ! Line with point x-axis values
                WRITE(NEPA,*)(AUX1(I),I=1,NAX1)
                READ(NATO,*) (AUX2(I),I=1,NAX2) ! Line with point y-axis values
	          WRITE(NEPA,*)(AUX2(I),I=1,NAX2)
                READ(NATO,*) ICH,(VALIN(I),I=1,NPOINTS)  ! Extra line to skip (-99)
	          WRITE(NEPA,*)ICH,(VALIN(I),I=1,NPOINTS)
	        END IF
            END DO    ! End of do on IO to NOPS
          END DO    ! End of do on IT to NTM
        END DO   ! End of DO on progeny
      ENDIF  ! if progeny
	end do ! nukea loop
	END DO ! NASETS
	else
	do i=1,nlines
	read(nato,*)dum
	end do
	go to 124
	end if
	END IF ! IOP = 2
C
      RETURN
	END
