      REAL FUNCTION ARSECINT( x, conc_inp )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     ArSecInt.For
c     Christian J Fosmire
c     Pacific Northwest Lab
c     P.O. Box 999
c     Richland, WA 99352
c
c     Created: 4/24/95
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
c     Subroutine:  CalcSig  
c     Functions:   VERTTERM       
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  BAN 11/30/2009
c  a change suggested by RAMSDELL for sigz .gt. udepth
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      
      INCLUDE  'conc_inp.inc'
      INCLUDE  'sigin.inc'
      
      REAL     VERTTERM
      REAL     a, h_e, mixhgt, sigz, udepth, vertdisp, wndspd, x, z 
  
      h_e = conc_inp.rel_hgt
      z = conc_inp.rec_hgt
      a = conc_inp.lenside
      wndspd = conc_inp.wndspd
      mixhgt = conc_inp.mix_hgt

c     compute the uniform mixed depth
  
      udepth = conc_inp.udepth

c     if h_e and z are zero then our function tends to infinite
c      as dist tends towards zero.  This is unrealistic and a minimum 
c      height is used to prevent this.
 
      IF( (h_e .EQ. 0) .AND. (z .EQ. 0) ) z = conc_inp.zmin

      sigin.sigopt = conc_inp.sigopt
      sigin.stab = conc_inp.stab
      sigin.dist = conc_inp.dwdist - x
      sigin.relhgt = h_e
      sigin.mixhgt = mixhgt
      sigin.dodist = .false.
      sigin.wndspd = conc_inp.wndspd
      sigin.sigv = conc_inp.sigv
      sigin.sigw = conc_inp.sigw      
      
      CALL CALCSIG( sigin )

      sigz = sigin.sigz

c     Check if sigz is too small...if so, then set function to zero      

      IF( sigz .LT. conc_inp.minsigz ) THEN

         arsecint = 0.0

      ELSE                 

c     Calculate the vertical dispersion term

         vertdisp = VERTTERM( h_e, z, conc_inp.mix_hgt, sigz )
c         CALL VERTTERM( h_e, z, conc_inp.mix_hgt, sigz, vertdisp )

         IF( sigz .LE. 1.2 * udepth ) THEN
            arsecint = (1.0 / sigz) * vertdisp
         ELSE
cccc  this change made at JVR's suggestion:
	      arsecint = 2./udepth
c            WRITE(*,*) 'ERROR---cannot integrate if sigz >= udepth'
c            STOP
         ENDIF  

      ENDIF
      
      RETURN
      
      END  