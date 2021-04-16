C---------------------------------------------------------------------
C
      SUBROUTINE HEADNG (LUN)
C
C     This subroutine prints page heading for quality assurance and
C     results reports.
C   
C     Module of Programs ENVIN, INTDF, ENV, and DOSE of the GENII
C     Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System
C
C     Last Modification:  27-Jun-90 RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C
C----------------------------------------------------------------------
C
C     LUN      - Logical unit device 
C     IOS      - Control integer set to a positive number if a file 
C                error occurrs
C     IPN      - Page number counter of primary output file
C     CALPRG   - Name of calling program, based on logical unit number
C     PART     - Character part designator, A or B to control page
C                numbering of ENVIN and DOSE
C
C----------------------------------------------------------------------

      INCLUDE 'TITL.CMN'
      INCLUDE 'DAYPC.CMN'

      INTEGER LUN, IOS, IPN
      CHARACTER FF*1, CALPRG*6, PART*2
      LOGICAL FIRST
      DATA IPN /0/, FIRST /.TRUE./

      FF = CHAR(12)
      IF (LUN .GT. 0) IPN = IPN + 1

      IF (LUN .EQ. 15) THEN
        CALPRG = 'INTDF '
      ELSEIF (LUN .EQ. 36) THEN
        CALPRG = 'EXTDF '
      ELSE
        CALPRG = 'GENII '
      ENDIF

      IF (LUN .EQ. 6) THEN
        PART = 'A.'
      ELSEIF (LUN .EQ. 28) THEN
        PART = 'B.'
      ELSEIF (LUN .EQ. 14) THEN
        PART = 'C.'
      ELSE
        PART = '  ' 
      ENDIF

C     Skip if this is not the first call in ENV-- 
      IF (LUN .EQ. 28 .AND. .NOT. FIRST) GOTO 100
 
      WRITE (LUN,1,ERR=98,IOSTAT=IOS) FF, CALPRG, TITLS(5)(1:60),
     .                                TITLS(5)(61:80)
      WRITE (LUN,2,ERR=98) TODAY, CLOCK, PART, IPN
      WRITE (LUN,3,ERR=98)

      FIRST = .FALSE.
 
100   CONTINUE
      RETURN

C---- Error Messages ---------------------------------------------------

   98 CALL FILERR (2, LUN, 'In HEADNG')
      RETURN

C---- Format Statements -----------------------------------------------

    1 FORMAT(A1,1X,78('-'),/,25X,A6,
     .       ' Dose Calculation Program'/,
     .       28X, '(Version 1.473 27-Jun-90)',//,
     .       ' Case title:   ',A60/15X,A20)
    2 FORMAT(' Executed on:  ',A8,' at ',A8,34X,'Page ',A2,I3)
    3 FORMAT (1X,78('-')) 

C----------------------------------------------------------------------
      END   

