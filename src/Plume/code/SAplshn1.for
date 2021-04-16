
      SUBROUTINE SAPlShn1( sector_width, sigin, qold, dose )  
      
C---------------------------------------------------------------------------
c
c     SAPlShn1                                        
c     
c     Date:              November 10, 1998
c
c     Description:       computes the dose from a sector-average plume, for
c                        plumes that are sufficently narrow that there are
c                        edge effects. 
c
c     Required modules:
c
c          Subroutines:  None
c
c            Functions:  None
c
C---------------------------------------------------------------------------
      
      IMPLICIT       NONE
      
      INCLUDE        'parm.inc'
	INCLUDE        'nuc_data.inc'
      INCLUDE        'shine.inc'
      INCLUDE        'sigin.inc'  
      
      REAL           ShnInterp   
              
      REAL           dose, fin_line_cf, pi, rho, sector_width, sigzlim,
     &               udepth, wt, xoq 

      REAL           htfact(5), ldist(10), lht(10), qold(MaxNucs)
      
      INTEGER        in, iy, iz, nhoriz, nvert

      DATA   htfact / 0.127, 0.385, 0.675, 1.037, 1.645 /

      pi = 3.14159

      nhoriz = 10
      IF ( (sector_width/2.0) .LT. 20.0 ) 
     &      nhoriz = INT( sector_width / 2.0 ) 
 
      DO iy = 1,nhoriz
         ldist(iy) = ((sector_width / 2.0) / FLOAT(nhoriz) ) * 
     &           ( FLOAT(2*iy - 1) / FLOAT( 2*nhoriz ) )
      ENDDO
 
      sigzlim = 1.2 * AMAX1( sigin.relhgt, sigin.mixhgt )

      udepth = 0.0
      IF ( sigin.sigz .GE. sigzlim ) udepth = sigzlim

c  *** represent the vertical distribution of mass using 10 slabs, each with 10% 
c      of the activity.  Normalized centers of activities of the slabs (distance 
c      above and below plume centerline, divided by sigz) are given in htfact. 

      nvert = 10
      IF (  udepth .LT. 400.0 ) THEN

         DO iz = 1,5
            lht(iz) = sigin.relhgt - sigin.sigz * htfact(6-iz)
            lht(11-iz) = sigin.relhgt + sigin.sigz * htfact(6-iz)
         ENDDO
   
c *** check for reflection at ground and top of mixing layer

         DO iz = 1,5
            IF ( lht(iz) .LT. 0.0 ) lht(iz) = -lht(iz)  
            IF ( lht(11-iz) .GT. sigin.mixhgt ) 
     &           lht(iz) = 2.0 * sigin.mixhgt - lht(iz)
         ENDDO

      ELSE        ! Uniform distribution in the vertical

         DO iz = 1,10
            lht(iz)  = udepth *  FLOAT(2*iz-1) / 10.0 
         ENDDO
      
      ENDIF       ! Done determining slab heights
      
      wt = 1.0 / FLOAT( nhoriz * nvert )  

c *** calculate dose from lines

      dose = 0.0
	DO iy = 1,nhoriz
	   DO iz = 1,nvert
	      rho = SQRT( ldist(iy)**2 + (lht(iz)-1.0)**2 )
	      rho = AMAX1( rho, 1.0 )
	      fin_line_cf = (1.0 + sigin.dist/SQRT(sigin.dist**2+rho**2))
     &                     / 2.0
            dose = dose + 
     &          ShnInterp( maxn, rho, rdxl, rdyc ) * wt * fin_line_cf
         ENDDO
      ENDDO

      RETURN
      
      END 
