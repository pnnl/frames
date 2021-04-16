      SUBROUTINE WRITHEAD( oio, firstline, outfile, metfile )
C----------------------------------------------------------------------
C     WRITEHEAD.FOR
C
C     Christian J Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Date:    11/19/96
C
C     Description:   This code writes the header information for the
C                    ATO file.
C
C----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER*2  dd(3), tt(4)
      INTEGER    oio

      CHARACTER*11  rdate, rtime  
      CHARACTER*72  firstline
      CHARACTER*50  outfile, metfile

c     Write out number of line of header

      WRITE( oio, '(i1,a1)') 6,','

c     Write out first line

      WRITE( oio, '(a)')
     &'"=============================================================="'
      
      WRITE( oio, '(a)') firstline
      
c     Get time of run

      rdate = '  /  /     '
      rtime = '  :  :  .  '

      CALL GETDAT(dd(1),dd(2),dd(3))
      CALL GETTIM(tt(1),tt(2),tt(3),tt(4))

      WRITE(rdate(1:2),'(i2)')dd(3)
      WRITE(rdate(4:5),'(i2)')dd(2)
      WRITE(rdate(7:10),'(i4)')dd(1) 

      WRITE(rtime(1:2),'(i2)')tt(1)
      WRITE(rtime(4:5),'(i2)')tt(2)
      WRITE(rtime(7:8),'(i2)')tt(3)
      WRITE(rtime(10:11),'(i2)')tt(4)

      WRITE(oio,'(5a)')
     &'"Run Performed:","',rdate,'","',rtime,'",'

      WRITE(oio,'(4a)')
     &'"Output Filename:",','"',outfile,'",' 
      WRITE(oio,'(4a)') 
     &'"Meteorological File used:",', '"', metfile, '",'
      
      WRITE(oio,'(a)')
     &'"=============================================================="'
     
      RETURN
      
      END      