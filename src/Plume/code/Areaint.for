      REAL FUNCTION AREAINT( x, conc_inp )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     AreaInt.For
c     Christian J Fosmire
c     Pacific Northwest Lab
c     P.O. Box 999
c     Richland, WA 99352
c
c     Created: 3/30/95
c
c     Description:  This function is the integrand of 
c       the area concentration integral.
c  
c       Inputs are:
c             x      = the point at which the function is being 
c                       evaluated (m)
c             conc_inp = a stucture that contains the necessary
c                         source and meteorological parameters
c
c     Subroutines:  CalcSig  
c     Functions:    ERF1, VERTTERM  
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      IMPLICIT    NONE
      
      INCLUDE     'conc_inp.inc'
      INCLUDE     'sigin.inc'
      
      REAL        ERF1
      REAL        VERTTERM
      REAL        a, h_e, horzdisp, mixhgt, sigy, sigz, udepth, 
     &            vertdisp, wndspd, x, y, z 
      
c     Get various parameters from structure
      
      h_e = conc_inp.rel_hgt
      z = conc_inp.rec_hgt
      a = conc_inp.lenside
      y = conc_inp.cwdist
      wndspd = conc_inp.wndspd
      mixhgt = conc_inp.mix_hgt

c     compute the uniform vertical disperison depth

      udepth = conc_inp.udepth
      
c     if h_e and z are zero then our function tends to infinite
c      as dist tend to zero.  This is unrealistic and a minimum 
c      height is used to prevent this.
 
      IF( (h_e .EQ. 0) .AND. (z .EQ. 0) ) z = conc_inp.zmin

c     Compute sigy and sigz

      sigin.sigopt = conc_inp.sigopt
      sigin.stab = conc_inp.stab
      sigin.dist = conc_inp.dwdist - x
      sigin.relhgt = h_e
      sigin.mixhgt = mixhgt
      sigin.dodist = .false.
      sigin.wndspd = wndspd
      sigin.sigv = conc_inp.sigv
      sigin.sigw = conc_inp.sigw

      CALL CALCSIG( sigin )
      
      sigy = sigin.sigy
      sigz = sigin.sigz

c     Check if sigz is too small...if so, then set function to zero      

      IF( sigz .LT. conc_inp.minsigz ) THEN
         areaint = 0.0

      ELSE                 

c     Calculate the vertical dispersion term
       
         vertdisp = VERTTERM( h_e, z, mixhgt, sigz ) 
c         CALL VERTTERM( h_e, z, mixhgt, sigz, vertdisp ) 

c     Calculate the horizontial (crosswind) dispersion term      

         horzdisp = 0.0
         horzdisp = ERF1((a + y)/(SQRT(2.) * sigy))
         horzdisp = horzdisp + ERF1((a - y)/(SQRT(2.) * sigy))
         horzdisp = 0.5 * horzdisp
         
         IF( horzdisp .LT. 1E-7 ) THEN
            horzdisp = 0
         ENDIF
      
c     Calculate the integrand of the area concentration      
c     note if sigz greater than mixing height...have to assume
c     uniform distribution...Note multiple areaint by 1/SQRT(2*pi) so
c     have to multiple by it during the uniform distribution

         IF( sigz .GE. 1.2 * udepth ) THEN
            areaint = (SQRT(2.0 * 3.14159) / udepth) * horzdisp
         ELSE
            areaint = (1.0 / sigz) * vertdisp * horzdisp
         ENDIF  
      ENDIF

      RETURN                               
      
      END  