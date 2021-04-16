C----------------------------------- VERSION: 25-Feb-1998 --------------
C
      SUBROUTINE EXPOSR (TYR,tpath,RELFLG)
C
C     This module controls environmental exposure calculations for a 
C     radionuclide chain.
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Reviewed and Approved: 
C     Last Modified:  25-Feb-1998  BAN
C-----------------------------------------------------------------------
C   MODIFICATION HISTORY
C  25-Feb-1998  BAN  Added calculation of inhalation dose for resuspension
C-----------------------------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'AFLAGS.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'EXPALL.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'TIMES.CMN'
      INCLUDE 'FLUX.CMN'
C      INCLUDE 'NUCNAM.CMN'
c      CHARACTER*6 GRID
      REAL TYR
      INTEGER TPATH
      LOGICAL RELFLG
c
C----- Calculate uptake rate from terrestrial foods, animal products ---------
c
        IF (C14 .OR. H3) THEN
          CALL CANDH
        ELSE
          IF (TFOOD) CALL CRPCAL(RELFLG,TPATH)
          IF (ANFOOD) CALL ANMCAL(RELFLG,TPATH)
        ENDIF
C     ENDIF
c
C----- Calculate uptake from drinking water ----------------------------------
c
      IF ((TPATH.LE.2.AND.DRINK) .or. SHINDR) CALL DRKCAL
c
C----- Calculate uptake from aquatic food ingestion --------------------------
c
      IF (TPATH.EQ.2.AND.AQFOOD) CALL AQUCAL
c
C----- Calculate inhalation rate ---------------------------------------------
c
      IF (INHAL .OR. SLINH) CALL INHCAL
c
C----- Calculate external exposure rates and inadvertent soil ingestion ------
c
      IF (AIREXT .OR. GROUND .OR. SLING .OR. SLDRML .OR. RECRE) 
     .  CALL EXTCAL(TYR,TPATH)
c
C----- Transfer to exposure medium concentration array -----------------------
c
      DO 201 IP = 1, NPATH
        DO 202 IN = 1, NONUC
          EXPOST(ITIME,IN,IP) = EXPOS(IN,IP)
  202   CONTINUE
c        IF(TPATH.EQ.3.AND.RECTYPE.EQ.GRID) THEN
         IF(TPATH.EQ.3) THEN
          IF(IP.LE.12.OR.IP.EQ.22.OR.IP.EQ.25.OR.IP.EQ.28) THEN
            IPT = IP
            IF(IP.EQ.25) IPT = 13
	      IF(IP.EQ.22) IPT = 15
            IF(IP.EQ.28) IPT = 14
            DO IN = CMEM,NONUC
              UEXP(IN,IPT) = EXPOS(IN,IP)
            END DO
          END IF
        END IF
  201 CONTINUE
C
C----- Write exposure results to temporary file for this time period ---------
C
      IF(TPATH.EQ.3) THEN
        DO IN = CMEM,NONUC
         WRITE(NATP,1000) IN,(UEXP(IN,I),I=1,15)
        END DO
 1000   FORMAT(I3,1P15E10.3)
      ENDIF
C
      IF(TPATH.EQ.3.AND.DEBUG) WRITE(NELS,100) APTYPE,CMEM,(UEXP(I,15),
     .                I=CMEM,NONUC)
 100  FORMAT(' In exposr, UEXP values for APTYPE and CMEM: ',2i3/
     .       1p14e10.2)
      RETURN
C
C----- END OF SUBROUTINE EXPOSR -----------------------------------------------
C
      END
