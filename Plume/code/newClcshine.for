      SUBROUTINE newCLCSHINE( indx, qold, q_line, sigin, arearad,  
     &                     IsSector, IsArea, wght )
C-----------------------------------------------------------------------
C     newCLCSHINE
C
C     J.V. Ramsdell
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Created:       August 10, 1999
C     Updated:       August 24, 1999
C
C     Description:   Calculate the cloud shine for the Gaussian 
C                    Plume Equation 
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      INCLUDE  'parm.inc' 
      INCLUDE  'nuc_data.inc' 
      INCLUDE  'srcrec.inc'
      INCLUDE  'shine.inc'
      INCLUDE  'pl_shine.inc'   
      INCLUDE  'sigin.inc'
      INCLUDE  'output.inc'
      
      INTEGER  i, indx, n
      
      REAL     q_line(MaxNucs), qold(MaxNucs)

      REAL     arearad, dose,  pi, rdyt, sector_width, wght
      
      LOGICAL  IsSector, IsArea, close_in
      
      pi = 3.14159               

c     If receptor within the area, then can't calculate
      
      IF (IsArea .AND. (sigin.dist .LT. arearad) ) THEN
         shine_hr(indx) = -1
         shine_ch(indx) = -1
         RETURN
      ENDIF

c      WRITE (25, '(a)' ) ' newClcShine '                      

c     Check if need to Calculate Cloud Shine

      IF ( ndxt(2) .LE. 1 ) RETURN

C  Compute shine dose vs distance 

      maxn = 0
      DO n = 1, nnucs
	   maxn =MAX( ndxt(2), ndx(n,2) )
      ENDDO  

c      WRITE (25, '(a)' ) ' Infinite line shine, newClcShine '

c      DO i = 1,maxn 
c         print ('(i5,f7.0,18(1pe10.2))' ), 
c     .       i, rdx(i), (rdy(i,n,2),n=1,nnucs)
c      ENDDO

C  Combine the relative dose rates

      DO i = 1,20

         rdyt = 0.0
         DO n = 1,nnucs
            IF ( i .GT. ndx(n,2) ) CYCLE
            rdyt = rdyt + rdy(i,n,2) * q_line(n)
         ENDDO   

         IF ( rdyt .GT. 0.0 ) THEN
            rdyc(i) = ALOG( rdyt )
         ELSE
            rdyc(i) = -100.0
         ENDIF 

      ENDDO

c      IF ( indx .EQ. 12 ) THEN
c         DO i = 1,maxn 
c            WRITE (25, '(i5,f7.0,20(1pe10.2))' ) 
c     &          i, rdx(i), (rdy(i,n,2),q_line(n),n=1,nnucs), rdyc(i)
c         ENDDO
c      ENDIF      

      close_in = .false.

      IF( IsSector) THEN
          sector_width = 
     &         AMAX1(4.0*sigin.sigy,2*pi*sigin.dist/FLOAT(recnum))
         IF( IsArea ) THEN
            sector_width = AMAX1(sector_width,2*arearad)
         ENDIF
         IF ( (sector_width / 4.0) .LE. chgmod ) close_in = .true.  
      ELSE
         sector_width = 0.0         
         IF ( sigin.sigy .LE. chgmod ) close_in = .true.  
      ENDIF

      IF ( close_in ) THEN
         CALL PlumeShn1( indx, sector_width, sigin, dose )
      ELSE 
         CALL PlumeShn2( indx, sector_width, sigin, qold, dose )
      ENDIF

c     Dose is in Sv/s - convert to Sv by multipling by time
      
      shine_hr(indx) = dose * 3600.0
      shine_ch(indx) = shine_ch(indx) + shine_hr(indx) * wght
     
      RETURN

      END
             