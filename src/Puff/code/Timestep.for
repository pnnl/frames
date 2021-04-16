
      SUBROUTINE  TIMESTEP ( m, nsi, symin, szmin, acute, PrgStat )                             
C---------------------------------------------------------------------------
c
c     TIMESTEP
c
c     Date:        August 18, 1995
c     Updated:     September 14, 1999
c                  March 14, 2000  
c
c     Description:   Subroutine determines the time step to be used
c                    for integration of air concentrations, surface
c                    contamination, and cloud shine doses for the
c                    Cartesian grid.  The time step varies as a function
c                    of puff dimensions and wind speed. 
c
c     Required modules:
c       
c          Subroutines:  NSIG, WSIG
c
c            Functions:  MIX_INTP, PUFFSIGZ, PUFSIGY, TURBSIGV, TURBSIGW,
c                        USTAR
c                        
C---------------------------------------------------------------------------

      IMPLICIT     NONE
      
      INCLUDE      'parm.inc'
      INCLUDE      'const.inc'
      INCLUDE      'difter.inc'
      INCLUDE      'met_data.inc'
      INCLUDE      'puffs.inc'

      REAL         CORRUSTR
      REAL         INVMOL
      REAL         PUFFSIGZ
      REAL         PUFSIGY
      REAL         TURBSIGV
      REAL         TURBSIGW 

      REAL         denom, dsmtr, dsmtri, moi_puf, pspd, symin, szmin,
     &             testdist, ttime, tsyn,  tszn, twsy2, twsz2, ustr_g, 
     &             z0
 
      INTEGER       nsia(12)
  
      INTEGER       i, ix, iy, m, nsi
                   
      CHARACTER*50  PrgStat

      LOGICAL       acute             
                   
      DATA          nsia /   1,   2,   3,   4,   5,   6, 
     &                      10,  12,  15,  20,  30,  60  /

c  ix, iy are coordinates of Cartesian grid node nearest puff center

      ix = xp(m) + 0.5
      iy = yp(m) + 0.5 
      IF ( ix .LT. 1 ) THEN
         ix = 1
      ELSE IF ( ix .GT. numx ) THEN
         ix = numx
      ENDIF
      IF ( iy .LT. 1) THEN
         iy = 1
      ELSE IF ( iy .GT. numy ) THEN
         iy = numy
      ENDIF

c  meteorological conditions for interval

      z0 = gz0(ix,iy)
      moi_puf = INVMOL( stab, z0 )
      ustr_g = CORRUSTR( ustr, z0sta, z0, stab, acute, PrgStat )
      IF( PrgStat .NE. ' ' ) RETURN

      dsmtr = SQRT( dxs(m)**2 + dys(m)**2 ) * delxy

      pspd = dsmtr / (3600.0 / nph)    ! Puff Speed in m/s

      testdist = delxy/4.         ! Test Distance in m
      
      tszn = sigzn(m)
      tsyn = sigyn(m)
      twsy2 = 0.0
      twsz2 = 0.0
      
      IF ( (age(m) * pspd * 60) .LT. testdist ) THEN

c  Ttime is used to avoid many small steps when puff is small and not near
c  any node 

         IF ( pspd .GT. 1 ) THEN
            TTime = testdist / pspd
         ELSE
            ttime = testdist
         ENDIF

         IF ( sigparm .LT. 5 ) THEN
         
c  use sigma based on distance

            CALL CLCPFSIG( testdist, stab, sigzlim, tszn, tsyn, sigparm)

         ELSE
         
c  use Turbulent statistics sigmas
         
            sigv = TURBSIGV ( stab, ustr_g, zp(m), ldepth, moi_puf, 
     &                        acute, PrgStat )
            IF ( PrgStat.NE.' ' ) RETURN
            
            sigw = TURBSIGW ( stab, ustr_g, zp(m), ldepth, moi_puf, 
     &                        sigv, acute, PrgStat )
            IF ( PrgStat.NE.' ' ) RETURN
            
            tsyn = PUFSIGY ( ttime/60, sy_cnst, sigv, ttime, tsyn )
            tszn = PUFFSIGZ ( sigw, stab, ttime, ttime, zp(m), ldepth,
     &                        tszn, sigzlim )
         ENDIF                     
         
         denom = tsyn
         
c     BUILDING WAKE NEEDS TO BE DONE
c         IF ( hts(m) .GT. 0.0 ) 
c     &      CALL WSIG ( ttime, hts(m), vts(m), twsy2, twsz2 )
c         denom = SQRT ( tsyn**2 + twsy2 )

      ELSE
      
         denom = sigmay(m)
      
      ENDIF   ! IF( age * pspd * 60 < testdist )

      symin = denom
      szmin = SQRT( tszn**2 + twsz2 )

      i = 1
      dsmtri = dsmtr / nsia(1)
      DO WHILE ( (i .LT. iopdta) .AND. (dsmtri/sigmay(m) .GE. 1.1) )
         i = i + 1
         dsmtri = dsmtr / nsia(i)
      ENDDO

c  dt = integration time step in sec
    
      dt = 3600 / FLOAT( nph * nsia(i) ) 
      nsi = nsia(i)
      
      RETURN
      
      END