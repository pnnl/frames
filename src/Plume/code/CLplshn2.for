
      SUBROUTINE CLPlShn2( sigin, qold, dose )  
      
C---------------------------------------------------------------------------
c
c     CLPlShn2                                        
c     
c     Date:              November 16, 1998
c
c     Description:       computes the dose from a sector-average plume 
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

      REAL           VERTTERM
              
      REAL           df, dose, k, mu, pi, rho, sector_width, sigzlim,
     &               udepth, vterm, xoq 

      REAL           htfact(5), lht(10), qold(MaxNucs)
      
      INTEGER        in, iz

      DATA   htfact / 0.127, 0.385, 0.675, 1.037, 1.645 /

      pi = 3.14159

      sigzlim = 1.2 * AMAX1( sigin.relhgt, sigin.mixhgt )

      udepth = 0.0
      IF ( sigin.sigz .GE. sigzlim ) udepth = sigzlim

      vterm = VERTTERM( sigin.relhgt, 1.0, sigin.mixhgt, sigin.sigz)
      
c  ***  try for quick exit because semi-infinite cloud dose factor is ok

	IF ( (sigin.sigz .GT. 300.0) .OR. (udepth .GT. 400) ) THEN

	   IF ( udepth .GT. 400.0 ) THEN
            xoq = 1.0 / 
     &       ( SQRT(2.0*pi) * sigin.sigy * sigin.wndspd * udepth )
         ELSE   
            xoq = 1.0 * vterm / 
     &       ( 2 * pi * sigin.sigy * sigin.sigz * sigin.wndspd  )
	   ENDIF

         dose = 0.0
         DO in = 1, nnucs
  	      dose = dose + xoq * DrSinf(in) * qold(in)
         ENDDO

         RETURN
      ENDIF

c  *** represent the vertical distribution of mass using 10 slabs, each with 10% 
c      of the activity.  Normalized centers of activities of the slabs (distance 
c      above and below plume centerline, divided by sigz) are given in htfact. 

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

c *** calculate the buildin and attenuation for the slab height to a 
c     receptor height of 1 m, assume an average gamma energy of 0.7 Mev.
c     Note that the minimum distance from the slab to the receptor is 
c     set at 1 m.

	k = 1.4
	mu = 1.0e-2

      df = 0.0
      DO iz = 1,10
	   rho = AMAX1( lht(iz)-1.0, 1.0 )
	   df = df + (1 + k * mu * rho ) * EXP( - mu * rho )
      ENDDO
	df = df / 10.0

c *** note xoq is vertically integrated through the depth of the plume here
  
      xoq = 1.0 / 
     &  ( SQRT( 2.0 * pi ) * sigin.sigy * sigin.wndspd ) 

      dose = 0.0
      DO in = 1, nnucs
        dose = dose + xoq * gsdf(in) * qold(in) * df
      ENDDO

      RETURN
      
      END 
