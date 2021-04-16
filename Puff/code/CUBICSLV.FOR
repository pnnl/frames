      SUBROUTINE CUBICSLV( a1, a2, a3, a4, soln, numsoln )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     CubicSlv.For
c     Christian J Fosmire
c     Pacific Northwest Laboratories
c     P.O. Box 999
c     Richland, WA 99352
c
c     Created: 6/28/95
c
c     Description:  Calculates the three roots to a cubic equation given
c      by (a1y^3 + a2y^2 + a3y + a4 = 0).  The soln is calculated by
c      first getting the equation in the form of (y^3 + py^2 + qy +r = 0)
c      and reducing it to (x^3 + ax + b = 0) where y = x - p/3.
c      The solutions are x = A + B, -(A+B)/2 + i(A-B)SQRT(3)/2, and
c      (A+B)/2 - i(A-B)SQRT(3)/2.
c       
c      where A = (-(b/2) + SQRT(b^2/4 + a^3/27))^(1/3)
c            B = (-(b/2) - SQRT(b^2/4 + a^3/27))^(1/3)
c
c      The real soln are sent back in the array soln.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT    NONE
      
      REAL        soln(3)
      REAL        a, a1, a2, a3, a4, b, c, p, q, r, rterm1, rterm2, 
     &            soln1, soln2
      
      INTEGER     i, numsoln
      
      COMPLEX     root(3)
      COMPLEX     AA, BB, sqrm3, term1, term2
      
      IF( a1.eq.0 ) THEN
         WRITE(*,*)'ERROR--First term is equal to zero'
      ENDIF

c     convert coefficients so equation is in form y^3+py^2+qy+r =0
      
      p = a2/a1
      q = a3/a1
      r = a4/a1

c     convert coefficient so equation is in form x^3 + ax + b = 0
      
      a = (1./3.)*(3*q - p*p)
      b = (1./27.)*(2*p*p*p - 9*p*q + 27*r)
      
      c = b*b/4. + a*a*a/27.

      IF( c.le.0 ) THEN

c     if c < 0 then have three real roots
      
         term1 = cmplx(-0.5*b,0)
         term2 = csqrt(cmplx(c,0.0))
      
      
         AA = (term1 + term2)**(1./3.)
         BB = (term1 - term2)**(1./3.)
      
      
         sqrm3 = csqrt(cmplx(-3.0,0))

c     Solve the three roots
      
         root(1) = AA + BB
         root(2) = -(AA+BB)/2 + (AA-BB)*SQRM3/2
         root(3) = -(AA+BB)/2 - (AA-BB)*SQRM3/2


c     All three roots are real so send them all back

         numsoln = 3
         DO i = 1,3
            soln(i) = REAL(root(i)) -p/3.
         ENDDO
      
      ELSE

c     if c > 0 then only one root.  

         rterm1 = -0.5*b + SQRT(c)
         rterm2 = -0.5*b - SQRT(c)
        
         soln1 = SIGN(ABS(rterm1)**(1./3.),rterm1)
         soln2 = SIGN(ABS(rterm2)**(1./3.),rterm2)
        
         numsoln = 1
         soln(1) = soln1+soln2
         
      ENDIF
      
      RETURN
      END