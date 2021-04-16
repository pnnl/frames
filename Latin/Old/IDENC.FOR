      SUBROUTINE IDENC
C*....SUBROUTINE IDENC
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine sets the program name, version number and
C*    modification date.  In addition, it gets the system date and time
C*    and generates identification variables for this run of the code.
C*
C*
C*  Operating system:
C*
C*    This subroutine is designed for the IBM PC using the Lahey
C*    FORTRAN 77 compiler.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C***********************************************************************
C
      INCLUDE 'IDENC.ins'
C
C *** User name
C
      USRNAM = 'Anonymous User'
C
C *** Program name and version number
C
      PRGNAM = 'LATIN   '
      PRGVER = '1.11.2  '
C
C *** Program date
C
      PRGDAT = '13 Jul 1993'
C
C *** System time in the (CHARACTER) form hh:mm:ss
C
      CALL TIME( SYSTIM )
C
C *** System date in the (CHARACTER) form mm-dd-yy
C
      CALL DATE( SYSDAT )
C
C *** Run identification number (Digit 1 is the the leftmost digit)
C       Digits  1- 2 = Year
C       Digits  3- 4 = Month
C       Digits  5- 6 = Day (1 to 31)
C       Digits  7- 8 = Hour (0 to 23)
C       Digits  9-10 = Minute (0 to 59)
C       Digits 11-12 = Second (0 to 59)
C
      CRUNID( 1: 2) = SYSDAT(7:8)
      CRUNID( 3: 4) = SYSDAT(1:2)
      CRUNID( 5: 6) = SYSDAT(4:5)
      CRUNID( 7: 8) = SYSTIM(1:2)
      CRUNID( 9:10) = SYSTIM(4:5)
      CRUNID(11:12) = SYSTIM(7:8)
C
C *** Fill any blanks with zero's
C
      DO 10 I = 1, 12
        IF( CRUNID(I:I) .EQ. ' ' ) CRUNID(I:I) = '0'
   10 CONTINUE
C
      RETURN
      END
