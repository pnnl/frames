      SUBROUTINE QASET
C*....SUBROUTINE QASET
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine sets some information for Quality Assurance
C*    purposes.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C***********************************************************************
C
      INCLUDE 'QA.ins'
C
C *** Set when QA procedures have not been followed or the code has
C     not been formally tested
C
      QA   = .FALSE.
      VERN = 'PNL-XXXX'
      VDAT = '00/00/00'
C
C *** Set when QA procedures have been followed and code tested
C       VERN will contain the number of the verification report
C       VDAT will contain the date of release of the verification report
C
C      QA   = .TRUE.
C      VERN = 'PNL-XXXX'
C      VDAT = '00/00/00'
C
      RETURN
      END
