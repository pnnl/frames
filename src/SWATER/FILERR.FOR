C--------------------------------------- VERSION: 28-May-1997 ----------
C
c      SUBROUTINE FILERR (ICODE, LUN, MESSAG)
C
C     This subroutine prints fatal error messages associated with 
C     data files and halts program execution
C
C     General Module of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System
C
C     GENII Master Version:  2-Dec-88  RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C
C     Last Modification:  28-May-DLS
C
C-----------------------------------------------------------------------
C
      SUBROUTINE FILERR (ICODE, LUN, MESSAG)
C
C---- Include Files ----------------------------------------------------------
C
      INCLUDE 'FILES.CMN' 
      INCLUDE 'DEVICE.CMN'
C
C---- Type and Dimension statements ------------------------------------------
C
      CHARACTER LINE*70, FACE*1
      INTEGER ICODE, LUN
      CHARACTER MESSAG*20, ERROR(4)*50
C
C---- Data Statements --------------------------------------------------------
C
      DATA ERROR /' Error opening file named',
     .            ' Premature end-of-file encountered in',
     .            ' Error occurred while reading file',
     .            ' File out of order or other indexing problem' / 
C
C----- Start of Analysis -----------------------------------------------------
C
      FACE = CHAR(2)
C
      DO 100 I = 1, 70
        WRITE (LINE(I:I),'(A)') FACE
  100 CONTINUE
C
      IF (ICODE .EQ. 0) THEN
        WRITE (NERR,*) 'FILERR:  Error code = 0.'
      ELSEIF (LUN .EQ. 0) THEN
        WRITE (NERR,*) 'FILERR:  Logical unit number = 0.'
      ELSE
        WRITE (NERR,2) LINE, ERROR(ICODE), FILN(LUN), LUN, MESSAG
    2   FORMAT (////' FILERR:  ',A70,/A50, A20/
     .              ' ',T10, 'Assigned to LUN: ',I3/
     .              ' ',T10, 'Message:         ',A20////)
      ENDIF
C
      NERROR = NERROR + 1
      RETURN
C
C------ END OF MODULE FILERR -------------------------------------------------
C
      END
