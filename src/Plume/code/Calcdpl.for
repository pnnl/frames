      SUBROUTINE CALCDPL( isrc, odist, dist, ohe, he, osigz, sigz, 
     &                    crtdist, mixhgt, udepth, wndspd )
c-----------------------------------------------------------------------
c     CALCDPL.FOR
c
c     Date:          May 30, 1996
c
c     Description:   Calculates depeletion from dry depostion using
c                    a trapzoidal rule to estimate the integral
c
c     Required Modules:     
c        Subroutines: NONE
c        Functions:   VERTTERM
c
c-----------------------------------------------------------------------
      IMPLICIT    NONE

      INCLUDE     'parm.inc'
      INCLUDE     'depos.inc'

      REAL        VERTTERM
      REAL        crtdist, depdpl1, depdpl2, dist, dist1, dist2,
     &            he, llim, mixhgt, odist, ohe, osigz, sigz, 
     &            soln, udepth, ulim, vert1, vert2, wndspd            
      
      INTEGER     i, isrc, nterm      
     
c     If distance is less than odist, no depletion

      IF ( dist .LT. odist ) THEN
         q_dpl_p = oq_dpl_p
         q_dpl_I2 = oq_dpl_I2
         RETURN
      ENDIF
      
c  if both distances are beyond the critical time (where have
c      uniform mixing) then don't have to integrate
     
      IF( (dist .GE. crtdist) .AND. (odist .GE. crtdist) ) THEN

         dist2 = dist
         dist1 = odist
         ulim = 0.
         llim = 0.
      
      ELSE IF( (dist .GE. crtdist) .AND. (odist .LT. crtdist) ) THEN

c     If new distance is greater than or equal to critical distance, 
c     but the old distance isn't, then integrate up to 
c     the critical distance (crtdist) and use the uniformly distributed 
c     case from the crtdist to dist            

         dist2 = dist
         dist1 = crtdist
         ulim = crtdist
         llim = odist

      ELSE

c     If new distance and old distance are less than critical distance,
c     then integrate 
               
         dist2 = 0.
         dist1 = 0.
         ulim = dist
         llim = odist
        
      ENDIF

d      WRITE(25,*) 'ulim = ',ulim,' llim = ', llim
      
c     Have to integrate (approx. the integral)      

      IF ( (ulim .NE. 0) .OR. (llim .NE. 0) ) THEN
         
         nterm = 1
                  
         DO i = 1, nterm

c     Calculate G(z)/sigz at old distance

c     If odist is the first point (set to zero)

            IF (osigz .GT. 0 ) THEN

               vert2 = VERTTERM( ohe, 0.0, mixhgt, osigz ) / osigz
c               CALL VERTTERM( ohe, 0.0, mixhgt, osigz, vert2 )
c               vert2 = vert2 / osigz
            ELSE
               
               vert2 = 0.0
               
            ENDIF      

c     If ulim = crtdist, then can replace G(z)/sigz with uniform
                           
            IF( ulim .EQ. crtdist ) THEN
         
               vert1 = SQRT(2 * 3.14159) / udepth
               
            ELSE
               
               vert1 = VERTTERM( he, 0.0, mixhgt, sigz ) / sigz
c               CALL VERTTERM( he, 0.0, mixhgt, sigz, vert1 ) 
c                vert1 = vert1 / sigz
     
            ENDIF

c     Use the geometric mean except if old value is zero
            
            IF( vert2 .EQ. 0 ) THEN
               soln = (vert1) * (ulim - llim) / 2.0
            ELSE
               soln = SQRT(vert1 * vert2) * (ulim - llim)
            ENDIF
                  
         ENDDO
      
      ELSE
      
         soln = 0.0
      
      ENDIF                                       
      
d      WRITE(25,*) 'soln = ',soln
      
c  Calculate the depleted release rates
      
      depdpl1 = -dv_I2 * soln / (SQRT(2.0 * 3.14159) * wndspd)
      depdpl2 = -dv_I2 * (dist2 - dist1) / (wndspd * udepth)

      q_dpl_I2 = oq_dpl_I2 * EXP(depdpl1) * EXP(depdpl2)
      
      DO i = 1, numpardis(isrc)
         depdpl1 = -dv_p(i) * soln / ( SQRT(2.0 * 3.14159) * wndspd )
         depdpl2 = -dv_p(i) * (dist2-dist1) / (wndspd * udepth )
         
         q_dpl_p(i) = oq_dpl_p(i) * EXP(depdpl1) * EXP(depdpl2) 
      
      ENDDO

d     WRITE(25,*) 'q_dpl_p = ',q_dpl_p 

c   Reset the old values
      
      odist = dist
      osigz = sigz
      oq_dpl_I2 = q_dpl_I2
      oq_dpl_p = q_dpl_p
      
      RETURN
      
      END