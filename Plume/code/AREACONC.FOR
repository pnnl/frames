      REAL FUNCTION AREACONC( conc_inp )
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     AreaConc.For
c     Christian J Fosmire
c     Pacific Northwest Lab
c     P.O. Box 999
c     Richland, WA 99352
c
c     Created: 4/24/95
c
c     Description:  This is the function calculates the avg area 
c      concentration.  All the required inputs are found in the 
c      structure conc_inp.  If the down wind distance is much larger 
c      than the radius of the square (i.e., half the side of the square), 
c      then the integration can be approximated without integrating.  
c      For the integrated and nonintegrated concentration to be within 
C      ~ 1% of each other, the downwind distance must be 10 times larger
c      than the radius of the square.
c
c     Subroutines: None
c     Functions:   AREAINT, INTSIMP
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      
      INCLUDE  'conc_inp.inc'
      
      REAL AREAINT
      EXTERNAL AREAINT
  
      REAL     arg1, ftol, llim, pi, soln, tol, ulim
      
      INTEGER  iflag, nmax

      pi = 3.14159

c     parameters for the integration routine
      
      tol = -1.0
      ftol = 1.0E-3
      nmax = 20

c     Check if can approximate the integral without integration

      IF( conc_inp.dwdist .GE. 10.0 * conc_inp.lenside ) THEN

c     Do approximation

         arg1 = 2.0 * conc_inp.lenside * conc_inp.q_area / SQRT(2 * pi)
         arg1 = arg1 / (conc_inp.wndspd)
         
         AREACONC = arg1 * AREAINT(0,conc_inp)
      
      ELSE

c     Do the integration:
c     Set the lower limit
          
         llim = -conc_inp.lenside

c     If downwind distance is beyond the area source then the upper
c      limit is the radius of the source else it's the downwind distance

         IF( ABS(conc_inp.dwdist) .LT. ABS(conc_inp.lenside)) THEN

            ulim = conc_inp.dwdist

         ELSE

            ulim = conc_inp.lenside  

         ENDIF

c     Call Routine to do Simpon's Integration of area integrand

         CALL INTSIMP( llim, ulim, AREAINT, tol, ftol, nmax, conc_inp,
     &                  soln, iflag )

         IF( iflag .NE. 0 ) THEN 
            WRITE(25,*) 'ERROR--Too many steps required to integrate'
            write(25,*) 'Limits are l u', llim, ulim
            write(25,*) 'spd = ', conc_inp.wndspd
            write(25,*) 'q_ar = ', conc_inp.q_area
            write(25,*) 'rel_hgt = ', conc_inp.rel_hgt
            write(25,*) 'rec_hgt = ', conc_inp.rec_hgt
            write(25,*) 'x = ', conc_inp.dwdist
            write(25,*) 'y = ', conc_inp.cwdist
            write(25,*) 'zmin = ', conc_inp.zmin
            write(25,*) 'lenside = ', conc_inp.lenside
            write(25,*) 'mixhgt = ',conc_inp.mix_hgt
            write(25,*) 'minsigz = ', conc_inp.minsigz
            write(25,*) 'udepth = ', conc_inp.udepth
            write(25,*) 'sigv = ', conc_inp.sigv
            write(25,*) 'sigw = ', conc_inp.sigw
            write(25,*) 'stab = ', conc_inp.stab
            write(25,*) 'sigopt = ', conc_inp.sigopt
            STOP
         ENDIF
         
c     Calculate the area concentration
          
         arg1 = conc_inp.q_area / (SQRT(2 * pi) * conc_inp.wndspd)
         
         AREACONC = arg1 * soln
     
      ENDIF

      RETURN               
      
      END  