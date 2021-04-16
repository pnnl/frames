C---------------------------------------------------------------------------
c
c     PUFFM
c
c     Date:          August 17, 1995
c     Updated:       March 14, 2000 
c
c     Description:   Calculates movement of each puff for each
c                    advection step.
c
c     Required modules:
c
c          Subroutines:  None
c
c            Functions:  PROFILE, USTAR
c
C---------------------------------------------------------------------------

      SUBROUTINE PUFFM( m, acute, PrgStat )

      IMPLICIT       NONE
      
      INCLUDE       'parm.inc'
      INCLUDE       'const.inc'
      INCLUDE       'met_data.inc'
      INCLUDE       'puffs.inc'
      
      REAL          CORRUSTR
      REAL          INVMOL
      REAL          PROFILE

      REAL          corx(2), cory(2), dxsfc(2), dysfc(2), spd10a(2), 
     &              xg(2), yg(2)

      REAL          fac, mol, spd, spdavg, ucomp, uospd, ustr_g, 
     &              vcomp, vospd, z0 
                  
      INTEGER       i, ii, j, jj, k, m, nx, ny 
      
      CHARACTER*50  PrgStat

      LOGICAL       acute   

      fac = 3600.0 / (nph * delxy)
      
      xg(1) = xp(m)
      yg(1) = yp(m)

      uospd = sin(flow_vec*dtr)
      vospd = cos(flow_vec*dtr)
      
      spd10a = 0
      

      DO k = 1,2

C  DETERMINE THE TRANSPORT SPEED COMPONENTS BASED ON THE SURFACE
C  WIND SPEED
       
         IF ( (xg(k) .GT. 1.0) .AND. (xg(k) .LT. numx) .AND.
     &        (yg(k) .GT. 1.0) .AND. (yg(k) .LT. numy) ) THEN

C  PUFF POSITION IS INSIDE DOMAIN

            i = IFIX( xg(k) )
            j = IFIX( yg(k) )
            nx = 1
            ny = 1            
         
         
         ELSE IF ( (xg(k) .LT. 1.0) .OR. (xg(k) .GT. numx)  .OR.
     +             (yg(k) .LT. 1.0) .OR. (yg(k) .GT. numy) ) THEN

C  PUFF POSITION IS OUTSIDE DOMAIN

            IF ( xg(k) .LT. 1.0 ) THEN
               i = 1
               IF ( yg(k) .LT. 1.0 ) THEN
                  j = 1
               ELSE IF ( yg(k) .GT. numy ) THEN
                  j = numy
               ELSE
                  j = IFIX ( yg(k) + 0.49999 )
               ENDIF
            ELSE IF ( xg(k) .GT. numx ) THEN
               i = numx
               IF ( yg(k) .LT. 1.0) THEN
                  j = 1
               ELSE IF ( yg(k) .GT. numy) THEN
                  j = numy
               ELSE
                  j = IFIX ( yg(k) + 0.49999 )
               ENDIF
            ELSE IF ( yg(k) .LT. 1.0) THEN
               j = 1
               i = IFIX ( xg(k) + 0.49999 )
            ELSE
               j = numy
               i = IFIX( xg(k) + 0.49999 )
            ENDIF
            
            nx = 0
            ny = 0            
           
         ELSE

C  PUFF POSITION IS ON AN EDGE OF THE DOMAIN

            IF ( (xg(k) .EQ. 1.0) .OR. (xg(k) .EQ. numx) ) THEN
               i = IFIX( xg(k) )
               j = IFIX( yg(k) )
               IF ( (yg(k) .EQ. 1.0) .OR. (yg(k) .EQ. numy) ) THEN
                  
                  nx = 0
                  ny = 0
      
               ELSE
                  nx = 0
                  ny = 1                  
               
               ENDIF
            ELSE
               i = IFIX ( xg(k) )
               j = IFIX ( yg(k) )
               
               nx = 1
               ny = 0
               
            ENDIF

         ENDIF
         
         IF( nx .GT. 0 ) THEN
            corx(2) = xg(k) - i
            corx(1) = 1 - corx(2)
         ELSE
            corx(1) = 1
         ENDIF
         
         IF( ny .GT. 0 ) THEN
            cory(2) = yg(k) - j
            cory(1) = 1 - cory(2)
         ELSE
            cory(1) = 1
         ENDIF
         
         spdavg = 0
         
         DO ii = 0, nx

            DO jj = 0, ny
            
               z0 = Gz0(i+ii,j+jj)
               mol = INVMOL( stab, z0 )
               ustr_g = CORRUSTR( ustr,z0sta,z0,stab,acute,PrgStat )
               IF( PrgStat .NE. ' ' ) RETURN

               IF( zp(m) .GT. 12 ) THEN
                  spd = PROFILE( z0,ustr_g,mol,stab,zp(m),acute,PrgStat)
                  IF( PrgStat .NE. ' ' ) RETURN
               ELSE
                  spd = PROFILE( z0,ustr_g,mol,stab,10.0,acute,PrgStat )
                  IF( PrgStat .NE. ' ' ) RETURN
               ENDIF
                 
               spd10 = PROFILE( z0,ustr_g,mol,stab,10.0,acute,PrgStat )
               IF( PrgStat .NE. ' ' ) RETURN

               spdavg = spdavg + corx(ii+1)*cory(jj+1)*spd
               spd10a(k) = spd10a(k) + corx(ii+1)*cory(jj+1)*spd10

            ENDDO

         ENDDO

         ucomp = spdavg*uospd
         vcomp = spdavg*vospd
                            
         IF ( spdavg .GT. 0 ) THEN

            dxsfc(k) = ucomp * fac
            dysfc(k) = vcomp * fac

         ELSE
         
            dxsfc(k) = 0
            dysfc(k) = 0
          
         ENDIF
         
         IF ( k .EQ. 2 )  CYCLE
         xg(k+1) = xg(k) + dxsfc(k)
         yg(k+1) = yg(k) + dysfc(k)
         
      ENDDO
      
      dxs(m) = (dxsfc(1) + dxsfc(2)) / 2.0
      dys(m) = (dysfc(1) + dysfc(2)) / 2.0


      spd10 = (spd10a(1)*spd10a(1) + spd10a(2)*spd10a(2) )

      spd10 = 0.5 * SQRT ( spd10 )    

      RETURN
      
      END