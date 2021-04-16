
      SUBROUTINE PlumeShn2( indx, sector_width, sigin, qold, dose )  
      
C---------------------------------------------------------------------------
c
c     PlumeShn2                                        
c     
c     Date:              August 18, 1999
c     Updated:           February 22, 2000    
c
c     Description:       computes the dose at long distances from source 
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
      
      INTEGER        in, iz, indx

      DATA   htfact / 0.127, 0.385, 0.675, 1.037, 1.645 /

      pi = 3.14159

      udepth = 0.0
      sigzlim = 1.2 * AMAX1( sigin.relhgt, sigin.mixhgt )
      IF ( sigin.sigz .GE. sigzlim ) udepth = sigzlim

      vterm = VERTTERM( sigin.relhgt, 1.0, sigin.mixhgt, sigin.sigz)
c      CALL VERTTERM( sigin.relhgt, 1.0, sigin.mixhgt, sigin.sigz,vterm)
      
c  ***  try for quick exit because semi-infinite cloud dose factor is ok

	IF ( (sigin.sigz .GT. 300.0) .OR. (udepth .GT. 400) ) THEN

         IF ( sector_width .GT. 0.0 ) THEN   ! sector-average model

            IF ( udepth .GT. 400.0 ) THEN
               xoq = 1.0 / 
     &           ( sector_width * sigin.wndspd * udepth )
            ELSE   
               xoq = 1.0 * vterm / 
     &           (SQRT(2*pi) * sector_width * sigin.sigz * sigin.wndspd)
	      ENDIF

         ELSE                                ! center-line model

            IF ( udepth .GT. 400.0 ) THEN
               xoq = 1.0 / 
     &           ( SQRT(2.0*pi) * sigin.sigy * sigin.wndspd * udepth )
            ELSE   
               xoq = 1.0 * vterm / 
     &            ( 2 * pi * sigin.sigy * sigin.sigz * sigin.wndspd  )
	      ENDIF

         ENDIF

         dose = 0.0
         DO in = 1, nnucs
  	      dose = dose + xoq * DrSinf(in) * qold(in)
         ENDDO

         RETURN

      ELSE IF ( udepth .LE. 0.0 ) THEN

c  *** represent the vertical distribution of mass using 10 slabs, each with 10% 
c      of the activity.  Normalized centers of activities of the slabs (distance 
c      above and below plume centerline, divided by sigz) are given in htfact. 

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

c *** calculate the buildin and attenuation for the layer height to a 
c     receptor height of 1 m, assume an average gamma energy of 0.7 Mev.
c     Note that the minimum distance from the layer to the receptor is 
c     set at 1 m.

	k = 1.4
	mu = 1.0e-2

      df = 0.0
      DO iz = 1,10
	   rho = AMAX1( lht(iz)-1.0, 1.0 )
	   df = df + (1 + k * mu * rho ) * EXP( - mu * rho )
      ENDDO

c *** note xoq is vertically integrated through the depth of the plume here

      IF ( sector_width .GT. 0.0 ) THEN      ! sector-average model
         xoq = 1.0 / 
     &      ( sector_width * sigin.wndspd ) 
      ELSE                                   ! center-line model
         xoq = 1.0 / 
     &      ( SQRT( 2.0*pi ) * sigin.sigy * sigin.wndspd )
      ENDIF 

c      WRITE (25, '(a)') ' Cloud shine, slab model,  PlumeShn2' 
      dose = 0.0
      DO in = 1, nnucs
        dose = dose + 0.1 * xoq * qold(in) * DrSinf(in) / 241.2 * df
c        WRITE ( 25,'(0pi5,5(1pe10.2))') in, xoq, qold(in), DrSinf(in), 
c     &                               df, dose
      ENDDO

      RETURN
      
      END 
