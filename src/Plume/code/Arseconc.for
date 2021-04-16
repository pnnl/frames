      REAL FUNCTION ARSECONC( conc_inp )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     AreaConc.For
c     Christian J Fosmire
c     Pacific Northwest Lab
c     P.O. Box 999
c     Richland, WA 99352
c
c     Created: 6/26/95
c
c     Description:  This is the function calculates the sector avg area 
c      concentration.  All the required inputs are found in the 
c      structure conc_inp.  If the downwind distance is much larger 
c      than the radius of the area (i.e., half the side of the square), 
c      then the integration can be approximated by a point src.  
c      For the integrated and pt src concentration to be within ~ 1% of
c      each other, the downwind distance must be 5 times larger than
c      the radius of the square.
c
c     Subroutine: CalcSig, INTSIMP
c     Functions:  ARSECINT (external), PTSECONC
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      
      INCLUDE  'conc_inp.inc'
      INCLUDE  'ptsrcin.inc'
      INCLUDE  'sigin.inc'
      
      REAL ARSECINT
      EXTERNAL ARSECINT

      REAL  PTSECONC       
      REAL  arg1, chgratio, ftol, llim, pi, soln, tol, ulim 
      
      INTEGER iflag, nmax
      
      pi = 3.14159

c     parameters for the integration routine
      
      tol = -1.0
      ftol = 1.0E-3
      nmax = 20
      
c     Based on the number of recptors - determine when can integrate by point
c     The switch occurs for x/a = 5 for 16 receptors and 11.4 for 36.  

      IF( conc_inp.numrec .EQ. 36 ) THEN
         chgratio = 11.4
      ELSE
         chgratio = 5.0
      ENDIF      
      
c     Check if can approximate the integral by the point src

      IF( conc_inp.dwdist .GE. chgratio * conc_inp.lenside ) THEN

c     Do approximation

         ptsrcin.q_src = conc_inp.q_area*4 * (conc_inp.lenside**2)
         ptsrcin.wndspd = conc_inp.wndspd
         ptsrcin.mixhgt = conc_inp.mix_hgt
         ptsrcin.relhgt = conc_inp.rel_hgt
         ptsrcin.rechgt = conc_inp.rec_hgt
         ptsrcin.udepth = conc_inp.udepth 
         ptsrcin.dwdist = conc_inp.dwdist
         ptsrcin.numrec = conc_inp.numrec
          
         sigin.relhgt = conc_inp.rel_hgt
         sigin.mixhgt = conc_inp.mix_hgt
         sigin.sigopt = conc_inp.sigopt
         sigin.stab = conc_inp.stab
         sigin.dist = conc_inp.dwdist
         sigin.dodist = .false.
         sigin.wndspd = conc_inp.wndspd
         sigin.sigw = conc_inp.sigw
         sigin.sigv = conc_inp.sigv

         CALL CALCSIG( sigin )
          
         ptsrcin.sigy = sigin.sigy
         ptsrcin.sigz = sigin.sigz
          
         ARSECONC =  PTSECONC( ptsrcin )
                    
      ELSE

c     Do the integration
c     Set the lower limit
          
         llim = -conc_inp.lenside

c     If downwind distance is beyond the area source then the upper
c      limit is the radius of the source else it's the downwind distance

         IF( ABS(conc_inp.dwdist) .LT. ABS(conc_inp.lenside) ) THEN
            ulim = conc_inp.dwdist
         ELSE
            ulim = conc_inp.lenside  
         ENDIF

c     Call Routine to do Simpon's Integration of area integrand

         CALL INTSIMP( llim, ulim, ARSECINT, tol, ftol, nmax, 
     &                 conc_inp, soln, iflag )

         IF( iflag .NE. 0 ) THEN 
            WRITE(25,*) 'ERROR--Too many steps required to integrate'
            STOP
         ENDIF
c     Calculate the area concentration

c     *Never used as reach point source before get past intermediate region
c         r16 = 16*conc_inp.lenside/pi

         arg1 = conc_inp.q_area / (SQRT(2 * pi) * conc_inp.wndspd)

c         IF( ABS(conc_inp.dwdist).ge.r16 ) THEN
c            arg1 = arg1*r16/conc_inp.dwdist
c         ENDIF

         arseconc = arg1 * soln

      ENDIF

      RETURN               
      
      END  