      SUBROUTINE TRAPZD( FUNC, llim, ulim, nstep, conc_inp, soln )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     TRAPZD.FOR
c     Christian J Fosmire
c     Pacific Northwest Laboratories
c     P.O. Box 999
c     Richland, WA 99352
C
C     Created: 3/30/95
c
c     Description:  This is the extended trapoziodal rule found in the 
c       NUMERICAL RECIPES.  Basically is computes the Nth stage of 
c       refinement of an extended trapoziodal rule.                 
c
c     Required Modules: NONE
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      
      INCLUDE  'conc_inp.inc'
      
      EXTERNAL FUNC
      REAL     FUNC

      REAL     del, llim, soln, sum, tnm, ulim, x  
      
      INTEGER  j, nstep, it
  
c     Check to see if using just the end points

      IF( nstep .EQ. 1 ) THEN

         soln = FUNC( llim, conc_inp ) + FUNC( ulim, conc_inp )
         soln = 0.5 * (ulim - llim) * soln
      
      ELSE

c     calculate the number of points to add 

         it = 2**(nstep-2)
         tnm = it

c     calculate the spacing of the points

         del = (ulim - llim) / tnm

c     this is the first new point

         x = llim + 0.5 * del
         sum = 0.

c     calculate the function at the new points

         DO j = 1, it
            sum = sum + FUNC( x, conc_inp )
            x = x + del
         ENDDO

c     add to the previous solution to get the new solution

         soln = 0.5 * (soln + (ulim - llim) * sum / tnm)
      
      ENDIF
      
      RETURN
      
      END