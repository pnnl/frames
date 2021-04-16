      SUBROUTINE CLCSHINE( indx, qold, sigin, arearad, IsSector, 
     &                     IsArea, wght )
C-----------------------------------------------------------------------
C     CLCSHINE
C
C     Christian J Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Created:    November 10, 1998
C
C     Description:   Calculate the cloud shine for the Gaussian 
C                    Plume Equation at receptor indx
C
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
      
      REAL     qold(MaxNucs)

      REAL     arearad, dose,  pi, rdyt, sector_width, wght
      
      LOGICAL  IsSector, IsArea
      
      pi = 3.14159               

c     If receptor within the area, then can't calculate
      
      IF (IsArea .AND. sigin.dist .LT. arearad) THEN
         shine_hr(indx) = -1
         shine_ch(indx) = -1
         RETURN
      ENDIF
                      
c     Check if need to Calculate Cloud Shine

      IF ( ndxt(2) .LE. 1 ) RETURN

C  Compute shine dose vs distance 

      maxn = 0
      DO n = 1, nnucs
	   maxn =MAX( ndxt(2), ndx(n,2) )
      ENDDO  

C  Combine the relative dose rates

      DO i = 1,20

         rdyt = 0.0
         DO n = 1,nnucs
            IF ( i .GT. ndx(n,2) ) CYCLE
            rdyt = rdyt + rdy(i,n,2) * qold(n)
         ENDDO   

         IF ( rdyt .GT. 0.0 ) THEN
            rdyc(i) = ALOG( rdyt )
         ELSE
            rdyc(i) = -100.0
         ENDIF 

      ENDDO
      
      IF( IsSector) THEN
      
         sector_width = 
     &         AMAX1(4.0*sigin.sigy,2*pi*sigin.dist/FLOAT(recnum))
      
         IF( IsArea ) THEN
            sector_width = AMAX1(sector_width,2*arearad)
         ENDIF
      
         IF ( sector_width .LE. chgmod ) THEN
            CALL SAPlShn1( sector_width, sigin, qold, dose )       
         ELSE 
            CALL SAPlShn2( sector_width, sigin, qold, dose ) 
         ENDIF
      
      ELSE
         
         IF ( sigin.sigy .LE. chgmod ) THEN
            CALL CLPlShn1( sigin, qold, dose )
         ELSE
            CALL CLPlShn2( sigin, qold, dose )
         ENDIF
      
      ENDIF

c     Dose is in Sv/s - convert to Sv by multipling by time
      
      shine_hr(indx) = dose * 3600.0
      shine_ch(indx) = shine_ch(indx) + shine_hr(indx) * wght
     
      RETURN

      END
             