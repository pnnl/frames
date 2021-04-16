      REAL FUNCTION PNTCONC( ptsrcin, horzdisp, vertdisp )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     PNTCONC.FOR
c     Christian J Fosmire
c     Pacific Northwest Lab
c     P O Box 999
c     Richland, WA 99352
c
c     Created: 4/5/95
c     Updated: 8/24/99  J.V. Ramsdell
c
c     Description:    This function calculate the concentration from
c      a point source given the following:
c
c      q_src   = the release rate (Ci/s or g/s)
c      wndspd  = the wind speed (m/s)
c      sigy    = the horizontial dispersion coefficient (m)
c      sigz    = the vertical dispersion coefficient (m)
c      y       = the cross-wind distance from the receptor to the
c                 source (m)
c      mix_hgt = the mixing layer depth (m)
c      h_e     = the release height (m)
c      z       = the height of the receptor (m)
c
c     Subroutines:  None
c     FUNCTIONS:    VERTTERM
C
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT    NONE         
      
      INCLUDE     'ptsrcin.inc'
      
      REAL        VERTTERM
      REAL        exparg, h_e, horzdisp, mix_hgt, pi, q_src, sigy, 
     &            sigz, udepth, vertdisp, wndspd, y, z      

c     get input variables from sturcture
        
      q_src = ptsrcin.q_src
      wndspd = ptsrcin.wndspd
      h_e = ptsrcin.relhgt
      z = ptsrcin.rechgt
      mix_hgt = ptsrcin.mixhgt
      y = ptsrcin.cwdist
      udepth = ptsrcin.udepth
      sigy = ptsrcin.sigy
      sigz = ptsrcin.sigz
            
      pi = 3.14159     
      
c     Calculate the Vertical Terms      
      
      vertdisp = VERTTERM( h_e, z, mix_hgt, sigz )
 
c      WRITE (25, '(a, 5f10.4)') 'PntConc ',h_e,z,mix_hgt,sigz,vertdisp

c     Calculate the horizontial terms

      exparg = 0.5 * (y / sigy)**2
      
      IF( exparg .LT. 1E-3 ) THEN
         horzdisp = 1.0
      ELSE IF( exparg .LT. 15.0 ) THEN
         horzdisp = EXP(-exparg)
      ELSE 
         horzdisp = 0.0
      ENDIF

c      WRITE (25, '(a, 5f9.4)') 'PntConc ',y,sigy,exparg,horzdisp

c     Calculate Concentration

c     Note: if sigy or sigz < 1e-7 then concentration is zero
      
      IF( (sigy. LT. 1E-7) .OR. (sigz .LT. 1E-7) ) THEN
         PNTCONC = 0.0
      ELSE IF( (sigz .GE. 1.2*udepth) .AND. (h_e. LE. mix_hgt) ) THEN
         PNTCONC = (q_src/(SQRT(2.0*pi)*wndspd*sigy*udepth))*horzdisp
      ELSE  
         PNTCONC = (q_src/(2.0*pi*wndspd*sigy*sigz))*horzdisp*vertdisp

      ENDIF
      
      RETURN              
      
      END            