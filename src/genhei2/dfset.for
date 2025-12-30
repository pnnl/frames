      SUBROUTINE DFSET(CONID,F1,CLASS,AMAD)
C-----------------------------------------------------------------------
C  
C	ROUTINE TO SET FGR13 DOSE/RISK FACTORS INTO ARRAYS
c
c
c     written by B.A. Napier
c     This subroutine connects the data files prepared by Kieth Eckerman
c     with the structure required by GENII Version 2 in FRAMES
c
c-------------------------------------------------------------------------
c
c     Initial programming  27 October 1998
c
c     latest revision;
C        BAN   27 Oct 98    Installation in NEW routine
C        BAN    5 NOV 98    INCLUDE SURFACE WATER DFs
C        BAN   18 Nov 02    allow selection of OBT
C        BAN   15 Feb 2003  Correct morb/route interchange for risk factors
c
c-------------------------------------------------------------------------
      include 'pakparm.CMN'
      include 'dcfpak.cmn'
      include 'batch.cmn'
      include 'iolist.cmn'
	INCLUDE 'FACTORS.CMN'
c
c  df(chain member, pathway, organ)
c
      character*1 class
      character*7 nuke, HYPHEN
	CHARACTER*7 CONID
      reaL heing(4),heinh(5),exdose(7),conext(7),thlf(25)
      real AM(25),DFO(9)
      logical IPTH(9),OK,IMP(25),reseter1, reseter2
      data daylim/0.375/,flim/0.1/
c      data amad/1.0/,CLASS/' '/
C
      al2 = alog(2.)
      do ip = 1,9
        IPTH(ip) = .true.
      end do
C
      CALL OPENEM()
c
c
c   get dose factors for current radionuclide, including external dose factors
c
       reseter1 = .false.
	 reseter2 = .false.
	NUKE = HYPHEN(CONID)
c
c   In this call, all ingestion DFs and RFs are set to the FIRST set
c   Only here, do we allow another for the OBT form of H3
c   Inhalation forms are adjustable through the CLASS input
c
      jban = 1
      if (nuke .eq. 'H-3EL') then
	 nuke = 'H-3'
	 reseter1 = .true.
      end if
      if (nuke .eq. 'OB-T') then
	 nuke = 'H-3'
	 reseter2 = .true.
	 jban = 2
      end if
C
	CALL NUKEOK(NUKE,OK)

C
        CALL DOSECOF(NUKE,F1,CLASS,AMAD,IPTH, jban)
c
c   identify implicit progeny and add in dose contributionsc
c   Determine which radionuclides should be considered implicit
c
      if (reseter1) nuke = 'H-3EL'
      if (reseter2) nuke = 'OB-T'
	jban = 1

        if(nucnam(nspec).eq.'Sf     ') nspec = nspec - 1
        imp(1) = .true.
        am(1) = 1.0
        thlf(1) = AL2/LMR(1)
        do in = 2,nspec
          am(in) = 0.0
          imp(in) = .false.
         do ib = 1,nbr(in)
          thlf(in) = AL2/LMR(IN)
          if(thlf(in).lt.daylim) then         ! may be implicit
            if(thlf(in).lt.FLIM*THLF(1)) then ! is implicit if precursor is imp.
              IPC = IBR(IB,IN)
              if(imp(IPC)) then                ! definitely implicit
                imp(in) = .true.
                am(in) = am(in) + branch(ipc,in)*am(ipc)
              end if
            end if
          end if
         end do
       end do
c
c   Add contribution from implicit progeny to ingestion and inhalation dose
c
      DO IORG = 1, MORG
        DO IAGE = 1, MAGE
          DFGSUM(IAGE,IORG) = DFING(IAGE,1,IORG)
          DFHSUM(IAGE,IORG) = DFINH(IAGE,1,IORG)
C
C     CORRECTION FACTOR FOR INHALATION OF HTO WITH HT, AS IN NEWTRIT
C      NOTE: IF WE EVER INCLUDE H3 SKIN ABSORPTION, ADD 1.5 MULTIPLIER
C      ALSO: THIS REALLY OUGHT TO BE A FUNCTION OF ABSHUM
C
	IF((nuke .EQ. 'H-3EL') .AND. (CLASS .EQ. 'G'
     .    .or. CLASS .EQ. 'V')) THEN
	   DFHSUM(IAGE,IORG) = DFHSUM(IAGE,IORG)*641.
	END IF
C
C
C
            IF(NSPEC.GT.1) THEN   ! Generate dose factors summed over implicits
              DO IN = 2,NSPEC
              if(imp(in)) then
c     include this radionuclide as an implicit progeny
              DFGSUM(IAGE,IORG) = DFGSUM(IAGE,IORG) +
     .                              AM(IN)*DFING(IAGE,IN,IORG)
              DFHSUM(IAGE,IORG) = DFHSUM(IAGE,IORG) +
     .                              AM(IN)*DFINH(IAGE,IN,IORG)
              end if
              END DO
            END IF
        END DO
C
        DO IR = 1,MEXT
          DFXSUM(IR,IORG) = DFEXT(1,IR,IORG)
        END DO
        IF (NSPEC .GT. 1) THEN
          DO IN = 2,NSPEC
            IF (IMP(IN)) THEN
              DO IR = 1,MEXT
                DFXSUM(IR,IORG) = DFXSUM(IR,IORG) +
     .                              AM(IN) * DFEXT(IN,IR,IORG)
              END DO
            END IF
          END DO
        END IF
C
C----ADDED 5 NOV
	  DFWSUM(IORG) = DFEXTW(1,IORG)
	  IF (NSPEC .GT. 1) THEN
	    DO IN = 2,NSPEC
           IF (IMP(IN)) DFWSUM(IORG)=DFWSUM(IORG)+AM(IN)*DFEXTW(IN,IORG)
	    END DO
	  END IF
c
      END DO
c
c  Now do RISK FACTORS
c
C  Already know implicits from ingestion analysis.  Use to get total RF for
c  this set.
c
	DO JMORB = 1,2
C  EXTERNAL RFs
        Do iage = 1,5
	  DO JEXT = 1, MEXT
	    DO JSITE = 1, MCAN
		RFXSUM(iage,JSITE,JEXT,JMORB)= RFEXTD(iage,1,JSITE,JEXT,JMORB)
	      DO IN = 2, NSPEC
	        IF (IMP(IN)) THEN
	          RFXSUM(iage,JSITE,JEXT,JMORB) = 
     .          RFXSUM(iage,JSITE,JEXT,JMORB) +
     .          AM(IN) * RFEXTD(iage,IN,JSITE,JEXT,JMORB)
	        END IF
            END DO
          END DO 
        END DO
	  End do
C INHALATION RFs
C
C  ADDED 5 RISK AGE GROUPS
        DO IAGE = 1,5
        DO JSITE = 1, MCAN
	  RFHSUM(IAGE,JSITE,JMORB) = RFINHD(IAGE,1,JSITE,JMORB)
C
C     CORRECTION FACTOR FOR INHALATION OF HTO WITH HT, AS IN NEWTRIT
C       NOTE: IF WE EVER ADD H3 SKIN ABSORPTION, ADD 1.5 MULTIPLIER
C       ALSO: THIS REALLY OUGHT TO BE A FUNCTION OF ABSHUM
C
	IF((nuke .EQ. 'H-3EL') .AND. (CLASS .EQ. 'G'
     .    .OR. CLASS .EQ. 'V')) THEN
	   RFHSUM(IAGE,JSITE,JMORB) = RFHSUM(IAGE,JSITE,JMORB)*641.
	END IF
C
C
C
	    DO IN = 2, NSPEC
	      IF (IMP(IN)) THEN
	        RFHSUM(IAGE,JSITE,JMORB) = RFHSUM(IAGE,JSITE,JMORB) + 
     .        AM(IN) * RFINHD(IAGE,IN, JSITE, JMORB)
	      END IF
	    END DO
	  END DO	 
C  INGESTION RFs
        DO JROUTE = 1,2
	    DO JSITE = 1, MCAN
C		RFGSUM(JSITE,JMORB,JROUTE) = RFINGD(1,JSITE,JMORB,JROUTE) 
		RFGSUM(IAGE,JSITE,JMORB,JROUTE) = 
     .              RFINGD(IAGE,1,JSITE,Jroute,Jmorb)
	      DO IN = 2, NSPEC
	        IF (IMP(IN)) THEN
	          RFGSUM(IAGE,JSITE,JMORB,JROUTE) = 
     .                  RFGSUM(IAGE,JSITE,JMORB,JROUTE)+
C     .          AM(IN) * RFINGD(IN, JSITE, JMORB, JROUTE)
     .          AM(IN) * RFINGD(IAGE,IN, JSITE, Jroute, Jmorb)
	        END IF
	      END DO
          END DO
        END DO
	  END DO
	END DO
 999  RETURN
      END
