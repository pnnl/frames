      REAL FUNCTION ERF1( x )
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     ERF1.for
c     Christian J Fosmire
c     Pacific Northwest Lab
c     P.O. Box 999
c     Richland, WA 99352
c
c     Created: 3/31/95
c
c     Description:  This function returns the erf function for x.  It
c       computes the complementary error function (erfc) and take 1-erfc
c       to get erf (this is part of the definition).  The erfc code is
c       from the Numerical Recipes book.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc  
      IMPLICIT NONE
      
      REAL     erfc, t, x, z
      
      z = ABS(x)
      t = 1. / (1. + 0.5 * z)
      
      erfc = t * EXP(-z*z-1.26551223+t*(1.00002368+t*(.37409196+t*
     +(.09678418+t*(-.18628806+t*(.27886807+t*(-1.13520398+t*
     +(1.48851587+t*(-.82215223+t*.17087277)))))))))
      
      IF( x .LT. 0. ) erfc = 2. - erfc
      
      ERF1 = 1 - erfc
      
      RETURN
      
      END