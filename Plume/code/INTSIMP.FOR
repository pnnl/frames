      SUBROUTINE INTSIMP( llim, ulim, func, tol, ftol, maxiter, 
     &                    conc_inp, soln, iflag )
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     IntSimp.FOR
c     Christian J J Fosmire
c     Pacific Northwest Lab
c     P O Box 999
c     Richland, WA 99352
c
c     Created: 3/29/95
c
c     Description:  This is a generic integration routine that is based 
c       on the NUMERICAL RECIPES subroutine QSIMP.  It calculates the 
c       integral of function FUNC from the limits of A to B with a 
c       tolerance of "TOL", a fractional tolerance of "FTOL" and maximum 
c       iterations of "MAXITER".  The program will return the solution 
c       "SOLN" when the difference between succesive solutions are 
c       smaller than TOL or when the fractional difference is smaller 
c       than FTOL or when maximum iterations have been computed.  Either
c       tolerance can be diabled by setting it to -1.  IFLAG denotes 
c       whether the solution has converged (=0), violated the total 
c       tolerance (=1), violated the fractional tolerance (=2), or 
c       violated both tolerances (=3)  
c
c     Subroutines:  TRAPZD
c     Functions:    NONE
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT    NONE          
      
      INCLUDE     'conc_inp.inc'
      
      EXTERNAL    func

      REAL        ftol, llim, nsoln, osoln, soln, tol, ulim   
      INTEGER     iflag, j, maxiter

      DO j = 1, maxiter
         iflag = 0

c     call extended trapziodal routine

         CALL TRAPZD( func, llim, ulim, j, conc_inp, nsoln ) 

c     Don't compute during first time through

         IF( j .GT. 1 ) THEN

c     This computes Simpson's rule

            soln=(4. * nsoln - osoln) / 3.

c     Check if value is within a total tolerance

            IF( tol .GT. 0 ) THEN
               IF( ABS(soln - osoln) .GT. tol ) THEN
                  iflag = iflag + 1
               ENDIF
            ENDIF

c     Check if value is within the fractional tolerance

            IF( ftol .GT. 0 ) THEN
               IF( ABS(soln-osoln) .GT. ftol*0.5*ABS(soln+osoln) ) THEN
                  iflag = iflag + 2
               ENDIF
            ENDIF

c     If within both the fractional and total tolerance, then solved

            IF( iflag .EQ. 0 ) RETURN
         
         ENDIF

c     Set the newest value to the old value

         osoln = nsoln
      
      ENDDO
      
      RETURN        
      
      END