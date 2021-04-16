C----------------------------------------------------------------------
C
      SUBROUTINE RITMED (RELFLG)
C
C     This module prints a media report for the residential location.
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Last Modification: 26-Feb-88  RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C     
C-----------------------------------------------------------------------
C
C     AIRC     - Air concentration or population-weighted air 
C                concentration used, UoA/m3
C     FIRST    - Logical set if this is the first call to this subroutine 
C     RELFLG   - Flag set if this is during the release period
C
C-----------------------------------------------------------------------

      INCLUDE 'RAD.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'TIMES.CMN'
      INCLUDE 'EXPALL.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'SWPAR.CMN' 
      INCLUDE 'SOLPAR.CMN'

      REAL AIRC
      LOGICAL FIRST, RELFLG
      DATA FIRST /.TRUE./

C      IF (ACUTE) FIRST = .FALSE.
      LUN = 21

      IF (FIRST) THEN
        IF (SWAT .AND. MIXFLG .NE. 0) WRITE (LUN,16) MIXR

        WRITE (LUN, 12) UNIT1(IUNIT), UNIT1(IUNIT), UNIT1(IUNIT), 
     .                  UNIT1(IUNIT), UNIT1(IUNIT), UNIT1(IUNIT)
        FIRST = .FALSE.
      ENDIF

      IF (RELFLG) THEN
        DO 132 IN = 1, NONUC
          AIRC = ARPCON(IN) * YRSEC
C          IF (ACUTE) THEN
C           WRITE (LUN,15) ELT(IN), AW(IN), ITIME, RESSOL(IN)
C          ELSE
            WRITE (LUN,14) ELT(IN), AW(IN), ITIME, AIRC, RESSOL(IN),
     .                     DSRES(IN), GWCON(IN), SWCON(IN)
C          ENDIF
  132   CONTINUE
      ENDIF

      RETURN

C---- Format Statements ------------------------------------------------

   12 FORMAT (//
     .        '                  ------Residential------        ',
     .        '         Irr.'/
     .        ' Radio-                   Surface Deep    Ground ',
     .        ' Surface Surface'/ 
     .        ' nuclide  Year    Air     Soil    Soil    Water   Water',
     .        '   Water'/
     .        '                  ',A3,'/m3  ',A3,'/m2  ',A3,'/m3  ',A3,
     .        '/L   ',A3,'/L   ',A3,'/L'/     
     .        ' -------- ------- ------- ------- ------- ------- ',
     .        '------- -------')

   14 FORMAT (' ', A2, A6, I8, 1P6E8.1)
C   15 FORMAT (' ', A2, A6, I8, 16X, 1PE8.1)
   16 FORMAT (//' Surface water mixing ratio: ',1PE8.1E2//)

C-----------------------------------------------------------------------
      END



