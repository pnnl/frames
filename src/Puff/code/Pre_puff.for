
      SUBROUTINE PRE_PUFF ( m, nsi, acute, PrgStat )                                        
C-----------------------------------------------------------------------
c
c     PRE_PUFF
c 
c     Date:              April 1, 1996
c     Updated;           March 18, 2000
c
c     Description:       Set the meteorological conditions 
c                        at the puff center, calculate the deposition 
c                        velocity and washout coefficients, and determine
c                        intial values of sigmay and sigmaz.
c
c     Required modules: 
c
c          Subroutines:  NSIG, WSIG
c
c            Functions:  ddepvel, mix_intp,  pufsigy, puffsigz,
c                        turbsigv, turbsigw,   ustar,  WDEP_i2, wdeppart
c                       
c
C-----------------------------------------------------------------------

      IMPLICIT      NONE
      
      INCLUDE       'parm.inc'
      INCLUDE       'const.inc'
      INCLUDE       'depos.inc' 
      INCLUDE       'difter.inc'
      INCLUDE       'met_data.inc'
      INCLUDE       'puffs.inc'

      REAL          CORRUSTR
      REAL          DDEPVEL
      REAL          INVMOL
      REAL          PUFFSIGZ
      REAL          PUFSIGY
      REAL          TURBSIGV
      REAL          TURBSIGW
      REAL          WDEP_I2
      REAL          WDEPPART
      
      REAL          adelx, dsmtr, dsmtri, dum, moi_puf, pspd, 
     &              tempsz, ttime, ustr_g, wcoef_I2, wcoef_p,
     &              wsigy2, wsigz2,  z0
    
      INTEGER       i, ix, iy, m, nsi
      
      CHARACTER*50  PrgStat 

      LOGICAL       acute    
             
      age(m) = age(m) + dt
      
      ix = xp(m) + 0.5
      IF ( ix .LT. 1 ) THEN
         ix = 1
      ELSE IF ( ix .GT. numx ) THEN
         ix = numx
      ENDIF

      iy = yp(m) + 0.5
      IF ( iy .LT. 1) THEN
         iy = 1
      ELSE IF ( iy .GT. numy ) THEN
         iy = numy
      ENDIF

      z0 = gz0(ix,iy)
      moi_puf = INVMOL( stab, z0 )
      ustr_g = CORRUSTR( ustr, z0sta, z0, stab, acute, PrgStat )
      IF( PrgStat .NE. ' ' ) RETURN
       
c  calculate deposition velocity 
 
      dv_I2 = DDEPVEL ( ustr_g, spd10, rtx(1), 0.0 )
      DO i = 1, numpardis(sp(m))
         dv_p(i) = DDEPVEL( ustr_g, spd10, rtx(2), setvel(sp(m),i) )
      ENDDO

c  Calculate washout coefficients

      wcoef_I2 = 0.0
      wcoef_p = 0.0 
      IF ( (pcode .GE. 1) .AND. (pcode .LE. 6)) THEN
         wcoef_I2 = WDEP_I2( pcode, prate,acute,PrgStat )
         wcoef_p = WDEPPART( pcode, prate, acute,PrgStat )
      ENDIF
      
      IF ( PrgStat .NE. ' ' ) RETURN
   
      wcdt_I2 = wcoef_I2 * dt 
      wcdt_p = wcoef_p * dt                  

      IF ( sigparm .LT. 5 ) THEN

c  use distance dependent diffusion coefficients
          
         dsmtr = SQRT ( dxs(m)**2 + dys(m)**2 ) * delxy
         dsmtri = dsmtr / nsi
         pspd = dsmtr / (3600 / nph)
         tempsz = sigzn(m)

         IF ( pspd .LE. 0.4 ) THEN

c  Note: dt is in seconds

            sigyn(m) = sigyn(m) + (700.0 / 60.0) * dt 
            adelx = 0.4 * dt
            
            IF( zp(m) .LT. ldepth ) THEN            
               CALL CLCPFSIG( adelx,stab,sigzlim,tempsz,dum,sigparm )
            ELSE
               CALL CLCPFSIG( adelx,7,sigzlim,tempsz,dum,sigparm )
            ENDIF
            sigzn(m) = tempsz

         ELSE
            
            CALL CLCPFSIG( dsmtri,stab,sigzlim,tempsz,sigyn(m),sigparm )
            IF( zp(m) .GT. ldepth ) THEN
               tempsz = sigzn(m)
               CALL CLCPFSIG( dsmtri, 7, sigzlim, tempsz, dum,
     &                        sigparm )
            ENDIF    
            sigzn(m) = tempsz
            
         ENDIF ! IF ( pspd .LE. 0.4 ) THEN

c  BUILDING WAKE STUFF...NEEDS TO BE DONE>>>>
c
         wsigy2 = 0.0
         wsigz2 = 0.0
c            IF ( hts(m) .GT. 0.0 ) THEN          
c               TTime = 60.0 * age(m)
c               CALL WSIG( TTime, hts(m), vts(m), wsigy2, wsigz2 )
c            ENDIF
c            
         sigysq = sigyn(m)**2 + wsigy2
         sigmay(m)  = SQRT ( sigysq )
         sigzsq = sigzn(m)**2 + wsigz2
         sigmaz(m) = SQRT ( sigzsq)
            
      ELSE       

c  use RATCHET diffusion cofficients

         sigv = TURBSIGV ( stab,ustr,zp(m),ldepth,moi_puf,acute,PrgStat)
         IF ( PrgStat .NE. ' ' ) RETURN
         
         sigw = TURBSIGW ( stab,ustr,zp(m),ldepth,moi_puf,sigv,acute,
     &                     PrgStat )
         IF ( PrgStat .NE. ' ' ) RETURN
   
         sigmay(m) = PUFSIGY ( age(m), SY_CNST, sigv, dt, sigmay(m))
         sigmaz(m) = PUFFSIGZ ( sigw, stab, age(m), dt, zp(m), ldepth,
     &                       sigmaz(m), sigzlim)
         sigysq = sigmay(m)**2
         sigzsq = sigmaz(m)**2
         
      ENDIF      ! Choice of diffusion coefficients

      hsgsq = (-0.5 * (delxy) ** 2) / sigysq

      RETURN
      
      END