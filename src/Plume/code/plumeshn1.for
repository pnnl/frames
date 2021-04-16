
      SUBROUTINE PlumeShn1( indx, sector_width, sigin, dose )  
      
C---------------------------------------------------------------------------
c
c     PlumeShn1                                        
c     
c     Date:              August 18, 1999
c                        February 22, 2000  
c
c     Description:       computes the dose from a plume near the release 
c                        point
c
c     Required modules:
c
c          Subroutines:  None
c
c            Functions:  ShnInterp
c
C---------------------------------------------------------------------------
      
      IMPLICIT       NONE
      
      INCLUDE        'parm.inc'
	INCLUDE        'nuc_data.inc'
      INCLUDE        'shine.inc'
      INCLUDE        'sigin.inc'  
      
      REAL           ShnInterp   
              
      REAL           dose, fin_line_cf, pi, rho, sector_width, sigzlim,
     &               udepth, wt, line_shine 

      REAL           htfact(5), ldist(10), lht(10), wdthfact(10)
      
      INTEGER        iy, iz, nhoriz, nvert, indx

      DATA           htfact   / 0.127, 0.385, 0.675, 1.037, 1.645 /
	DATA           wdthfact / 0.063, 0.186, 0.319, 0.453, 0.597,
     &                          0.755, 0.934, 1.15,  1.44, 1.96 /

      pi = 3.14159

      nhoriz = 10

      IF ( sector_width .GT. 0.0 ) THEN
         IF ( sector_width .LT. 40. ) nhoriz = INT( sector_width / 4.0 ) 
         DO iy = 1,nhoriz
             ldist(iy) = (sector_width / 2.0) * 
     &                   FLOAT(2*iy-1) / FLOAT(2*nhoriz) 
         ENDDO
      ELSE
         DO iy = 1,nhoriz
            ldist(iy) = sigin.sigy * wdthfact(iy)
         ENDDO
      ENDIF 

      sigzlim = 1.2 * AMAX1( sigin.relhgt, sigin.mixhgt )

      udepth = 0.0
      IF ( sigin.sigz .GE. sigzlim ) udepth = sigzlim
      
c  *** represent the vertical distribution of mass using 10 layers, each with 10% 
c      of the activity.  Normalized centers of activities of the layers (distance 
c      above and below plume centerline, divided by sigz) are given in htfact. 

      nvert = 10
      IF (  udepth .LE. 0.0 ) THEN  ! not uniform concentration distribution

         DO iz = 1,5
            lht(iz) = sigin.relhgt - sigin.sigz * htfact(6-iz)
            lht(11-iz) = sigin.relhgt + sigin.sigz * htfact(6-iz)
         ENDDO
   
c *** check for reflection at ground and top of mixing layer

         DO iz = 1,5
            IF ( lht(iz) .LT. 0.0 ) lht(iz) = -lht(iz)  
            IF ( lht(11-iz) .GT. sigin.mixhgt ) 
     &           lht(11-iz) = 2.0 * sigin.mixhgt - lht(11-iz)
         ENDDO

      ELSE        ! Uniform distribution in the vertical

         DO iz = 1,10
            lht(iz)  = udepth *  FLOAT(2*iz-1) / 20.0 
         ENDDO
      
      ENDIF       ! Done determining slab heights
      
      wt = 1.0 / FLOAT( nhoriz * nvert ) 

c *** calculate dose from lines

c      WRITE ( 25, '(a)') '  Finite line shine, plumeshn1'
      dose = 0.0
	DO iy = 1,nhoriz
	   DO iz = 1,nvert
	      rho = SQRT( ldist(iy)**2 + (lht(iz)-1.0)**2 )
	      rho = AMAX1( rho, 1.0 )
	      fin_line_cf = (1.0 + sigin.dist/SQRT(sigin.dist**2+rho**2))
     &                     / 2.0
c            IF ( indx .EQ. 12 ) THEN
c               line_shine = ShnInterp( maxn, rho, rdxl, rdyc ) * wt 
c     &                      * fin_line_cf
c               dose = dose + line_shine
c               WRITE (25,'(2i4,3f8.2,2f7.4, 2(1pe10.2))') iy, iz,  
c     &            ldist(iy), lht(iz), rho, wt, fin_line_cf, line_shine,
c     &            dose 
c            ELSE
               dose = dose + 
     &          ShnInterp( maxn, rho, rdxl, rdyc ) * wt * fin_line_cf
c            ENDIF
         ENDDO
      ENDDO

      RETURN
      
      END 
