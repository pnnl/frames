C------------------------------------------------24-Mar-1998------------
C
      SUBROUTINE EXPOSR (TPATH)
C
C     This module controls environmental exposure calculations for a 
C     radionuclide chain.
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Last Modification:  24-Mar-98 BAN
C     Reviewed and Approved: 12-Sept-88  BA Napier
C
C-----------------------------------------------------------------------
C   24-Mar-98  BAN  Add flags to DRKCAL and EXTCAL calls for new pathways
C   17 April 06 BAN  increased UEXP to 15 to include indoor air
C-----------------------------------------------------------------------
C
C     ISEAS    - Season index for acute cases, Set to 4 for autumn
C
C-----------------------------------------------------------------------

      INCLUDE 'PARMTR.PAR'
      INCLUDE 'CONC.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'EXPALL.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'TIMES.CMN'
      INCLUDE 'AFLAGS.CMN'
C
C-----  Type statements ------------------------------------------------------
C
      INTEGER TPATH
C
C      ISEAS = 4
      IF (ITIMT .EQ. 1) THEN
C
C-----  Food uptake from acute release ---------------------------------------
C
        IF (C14 .OR. H3) THEN
          CALL CANDH
        ELSE
          IF (TFOOD) CALL ACUTEC (TPATH)
          IF (ANFOOD) CALL ACUTEA ()
        ENDIF

      ELSE
C
C----- Calculate uptake rate from terrestrial foods, animal products ---------
C
        IF (C14 .OR. H3) THEN
          CALL CANDH
        ELSE
          IF (TFOOD) CALL CRPCAL
          IF (ANFOOD) CALL ANMCAL
        ENDIF
      ENDIF
C
C-----Calculate uptake from drinking water -----------------------------------
C
      IF ((DRINK .OR. SHINDR .OR. SHING .OR. SHDRML).AND.TPATH.EQ.2)
     .    CALL DRKCAL
C
C----- Calculate uptake from aquatic food ingestion --------------------------
C
      IF (AQFOOD.AND.TPATH.EQ.2) CALL AQUCAL
C
C----- Calculate inhalation rate ---------------------------------------------
C  (soil test added 26-Feb-98)
C
      IF (INHAL .OR. SLINH) CALL INHCAL
C
C---- Calculate external exposure rates and inadvertent soil ingestion -------
C
      IF (AIREXT .OR. FINITE .OR. GROUND .OR. SLING .OR. RECRE 
     .  .OR. SLDRML .OR. SRDRML) CALL EXTCAL
C
C----- Transfer to exposure rate array ---------------------------------------
C
      DO IP = 1, NPATH
        DO IN = 1, NONUC
          EXPOST(ITIME,IN,IP) = EXPOS(IN,IP)
        END DO
          IF(TPATH.EQ.3) THEN
            IF(IP.LE.12.OR. IP.EQ.22 .OR. IP.EQ.25.OR.IP.EQ.28) THEN
              IPT = IP
	        IF(IP.EQ.22) IPT = 15
              IF(IP.EQ.25) IPT = 13
              IF(IP.EQ.28) IPT = 14
              DO IN = CMEM,NONUC
                UEXP(IN,IPT) = EXPOS(IN,IP)
              END DO
            END IF
          END IF
        END DO  
C      END DO
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
C-----------------------------------------------------------------------
      END
