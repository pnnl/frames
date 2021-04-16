
      SUBROUTINE PUFFCS ( m, pufcx, pufcy, sy, rshine )

C-----------------------------------------------------------------------
c            
c     PUFFCS
c
c     Date:              March 19, 1998   
c
c     Description:       This subroutine does accumulation of the shine doses
c                        doses on the Cartesian grid.
c 
c     Required modules:
c
c          Subroutines:  None
c
c            Functions:  SHINEINT
c       
C-----------------------------------------------------------------------
      
      IMPLICIT      NONE

      INCLUDE       'parm.inc'
      INCLUDE       'const.inc'
      INCLUDE       'difter.inc' 
      INCLUDE       'matrix.inc'
      INCLUDE       'puffs.inc'
      INCLUDE       'shine.inc'
      INCLUDE       'pf_shine.inc'
       
      REAL          SHINEINT 

      REAL          deldose, gndist, hexp, pufcx, pufcy, rshine, sy 

      INTEGER       east, i, j, m, north, south, west    
          
c  check puff position

      IF ( ( (xp(m)+rshine) .LT. 1.0 ) .OR. 
     &     ( (xp(m)-rshine) .GT. numx ) .OR.
     &     ( (yp(m)+rshine) .LT. 1.0 ) .OR.
     &     ( (yp(m)-rshine) .GT. numy ) ) RETURN  ! no shine doses

c   calculate the grid points near puff  

      west = NINT(pufcx - 1.2 * rshine)
      east = NINT(pufcx + 1.2 * rshine)
      south = NINT(pufcy - 1.2 * rshine)
      north = NINT(pufcy + 1.2 * rshine)
      IF ( west  .LT. 1 )      west = 1
      IF ( east  .GT. numx ) east = numx
      IF ( south .LT. 1 )      south = 1
      IF ( north .GT. numy ) north = numy

c  accumulate dose increments
  
      DO j = south, north
         DO i = west, east

            gndist = SQRT( (pufcx-i)**2 + (pufcy-j)**2 ) * delxy / 2. 

            IF ( sy .GE. chgmod ) THEN

c  use semi-inifinite cloud approximation              

               hexp = EXP ( -0.5 * (gndist/sy)**2 )
               deldose = drate0 * hexp * dt
               cg_shine(i,j) = cg_shine(i,j) + deldose
            ELSE

c  use finite plume approximation 
              
               IF ( gndist .LE. pdx( mdist ) ) 
     &            cg_shine(i,j) = cg_shine(i,j) + 
     &                SHINEINT ( mdist, gndist, pdx, pdy ) 
     &                          * dt               
            ENDIF  
            
         ENDDO   !  east-west  (i)
      ENDDO      !  north-south  (j)


      RETURN
     
      END
      
