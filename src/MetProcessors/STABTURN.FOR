      SUBROUTINE STABTURN( ihr, iday )
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     STABTURN
C
C     Christian J Fosmire
C     Pacific Northwest National Laboratories
C     P.O. Box 999
C     Richland, WA 99352
C
C     Created:    1/16/96
C
C     Description:  Estimates the atmospheric stability using the 
C       method proposed by Turner(1964).  Adapted from TURNER subroutine
C       used in HEDR project.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE

      INCLUDE  'hrly.inc'
            
      REAL     SOLALT
      REAL     solang
      
      INTEGER  sclass(7,9)
      INTEGER  iclhgt, iday, ihr, isky, ispd, rindx, sindx, windx
      
      DATA sclass  / 1, 1, 2, 3, 4, 6, 7,
     &               1, 2, 2, 3, 4, 6, 7, 
     &               1, 2, 3, 4, 4, 5, 6, 
     &               2, 2, 3, 4, 4, 5, 6, 
     &               2, 2, 3, 4, 4, 4, 5, 
     &               2, 3, 3, 4, 4, 4, 5, 
     &               3, 3, 4, 4, 4, 4, 5, 
     &               3, 3, 4, 4, 4, 4, 4,
     &               3, 4, 4, 4, 4, 4, 4  /
     
      iclhgt = iceil(ihr)
      isky = icover(ihr)
      ispd = kspeed(ihr)

c     Check if can calculate stability

      IF ( ispd.gt.90 ) THEN
         kst(ihr) = 9
         RETURN
      ENDIF      
      
c     Check for low overcast (net radiation = 0) 

      IF ( isky.eq.10 .AND. iclhgt.lt.7000 ) THEN
         kst(ihr) = 4
         RETURN
      ENDIF
      
C     Check whether night or day

      IF ( ihr.LT.sunris .OR. ihr.GT.sunset ) THEN
      
c        Night time

         IF ( isky.LE.4 ) THEN
            rindx = 7
         ELSE
            IF( prpcode(ihr).GT.0 .AND. prpcode(ihr).LE.6 ) THEN
               rindx = 5
            ELSE
               rindx = 6
            ENDIF
         ENDIF
      
      ELSE
      
c        DAY -- initial estimate

c        Calculate solar elevation angle for this hour

c         write(*,*) 'iday, ihr, lat = ', iday, ihr, lat

         solang = SOLALT( iday, ihr, lat )
         
         IF( solang.LE.15.0 ) THEN
            sindx = 1
         ELSEIF( solang.LE.35.0 ) THEN
            sindx = 2
         ELSEIF( solang.LE.60.0 ) THEN
            sindx = 3
         ELSE
            sindx = 4
         ENDIF
         
c        Modify sindx for cloud cover

         IF( isky.GT.5 .AND. isky.LE.10 ) THEN
            IF ( iclhgt.LT.7000 ) THEN
               sindx = sindx - 2
            ELSEIF ( iclhgt.LT.16000 .OR. isky.EQ.10 ) THEN
               sindx = sindx - 1
            ENDIF
            IF( sindx.LT.1 ) sindx = 1
         ELSEIF( prpcode(ihr).GT.0 .AND. prpcode(ihr).LE.6 ) THEN
            sindx = sindx - 2
            IF ( sindx.LT.0 ) sindx = 0
         ENDIF
         
         rindx = 5 - sindx
         
      ENDIF
      
c     Determine wind speed index

      IF ( ispd.GE.12 ) THEN
         windx = 9
      ELSEIF ( ispd.LT.2 ) THEN
         windx = 1
      ELSEIF ( ispd.LT.4 ) THEN
         windx = 2
      ELSEIF ( ispd.LT.6 ) THEN
         windx = 3
      ELSEIF ( ispd.LT.7 ) THEN
         windx = 4
      ELSEIF ( ispd.LT.8 ) THEN
         windx = 5
      ELSEIF ( ispd.LT.10 ) THEN
         windx = 6
      ELSEIF ( ispd.LT.11 ) THEN
         windx = 7        
      ELSE
         windx = 8
      ENDIF
      
c     Determine Turner Stability Class

      kst(ihr) = sclass(rindx,windx)
      
      RETURN
      
      END              