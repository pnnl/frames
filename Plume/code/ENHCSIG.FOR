      SUBROUTINE ENHCSIG( wkflg, menflg, sigopt, u, area, sigysq, 
     &                    sigzsq, x )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     EnhcSig
c     Christian J Fosmire
c     Pacific Northwest Lab
c     P O Box 999
c     Richland, WA 99352
c
c     Created: 6/23/95
c
c     Description:  This code computes the enhanced diffusion
c       coefficients as described by Ramsdell and Fosmire (1995).  
c       This include enhancements due to low wind speed (meander) and
c       high wind speed building wakes. 
c
c     Required Modules: None
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      
      REAL     Ay1(5), Ay2(5), Az1(5), Az2(5)
      REAL     area, func, sigysq, sigy1, sigy2, sigzsq, sigz1, sigz2,
     &         u, x
      
      INTEGER  sigopt
      
      LOGICAL  menflg, wkflg

      DATA Ay1 /5.98E5, 2.23E5, 6.89E5, 6.53E5, 2.07E5/
      DATA Az1 /2.75E3, 3.38E2, 2.35E3, 2.51E3, 1.19E3/
      DATA Ay2 /0.00E-0, 0.00E-0, 0.00E-0, 0.00E-0, 0.00E-0/
      DATA Az2 /0.00E-0, 0.00E-0, 0.00E-0, 0.00E-0, 0.00E-0/

c  Initialize variables
      
      sigy1 = 0
      sigz1 = 0      
      sigy2 = 0
      sigz2 = 0
      
c  Calculate low wind speed sigs (i.e., meander)
      
      IF( menflg ) THEN
         sigy1 = Ay1(sigopt)*(1 - (1 + x/(1000.*u)) * EXP(-x/(1000.*u)))
         sigz1 = Az1(sigopt)*(1 - (1 + x/(100.*u)) * EXP(-x/(100.*u)))
      ENDIF
      
c  If building wake, compute building wake diffusion
      
      IF( wkflg .AND. (area .GT. 0) ) THEN
         func = 1 - (1 + x/(10.*SQRT(area))) * EXP(-x/(10.*SQRT(area)))
         sigy2 = Ay2(sigopt) * u*u * area * func
         sigz2 = Az2(sigopt) * u*u * area * func
      ENDIF
      
      sigysq = sigy1 + sigy2
      sigzsq = sigz1 + sigz2
      
      RETURN          
      
      END              