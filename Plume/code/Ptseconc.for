      REAL FUNCTION PTSECONC( ptsrcin )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     PNTCONC.FOR
c     Christian J Fosmire
c     Pacific Northwest Lab
c     P O Box 999
c     Richland, WA 99352
c
c     Created: 4/5/95
c
c     Description:    This function calculate the concentration from
c      a sector averaged point source given the following:
c
c      q_src   = the release rate (Ci/s or g/s)
c      wndspd  = the wind speed (m/s)
c      sigz    = the vertical dispersion coefficient (m)
c      mix_hgt = the mixing layer depth (m)
c      h_e     = the release height (m)
c      z       = the height of the receptor (m)
c
c     Subroutines:  None     
c     Functions:    VERTTERM
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE 

      INCLUDE  'ptsrcin.inc'
      
      REAL     VERTTERM
      REAL     dist, h_e, mix_hgt, pi, q_src, secwdth, sigy, sigz,
     &         udepth, vertdisp, wndspd, z

      pi = 3.14159     

c     get input variables from sturcture
        
      q_src = ptsrcin.q_src
      wndspd = ptsrcin.wndspd
      h_e = ptsrcin.relhgt
      z = ptsrcin.rechgt
      mix_hgt = ptsrcin.mixhgt
      dist = ptsrcin.dwdist
      udepth = ptsrcin.udepth
      sigy = ptsrcin.sigy
      sigz = ptsrcin.sigz
      
c     The width of the sector is either 2*pi*x/16 or 4*sigy depending
c     upon which one is larger...this prevents an overestimation for
c     at short distance for unstable stability class
      
      secwdth = AMAX1(2*pi*dist/FLOAT(ptsrcin.numrec),4.0*sigy)
      
c     Calculate the vertical terms
      
      IF( sigz .GE. 1.2 * udepth ) THEN
     
c     if sigz greater than uniform depth, then assume uniform depth     
     
         PTSECONC = q_src / (wndspd * secwdth * udepth)
      
      ELSE IF( sigz .LT. 1E-7 ) THEN

c     if sigz is too small...then concentration is zero
         
         PTSECONC = 0
      
      ELSE

c     Calculate the vertical dispersion term and compute concentration

         vertdisp = VERTTERM( h_e, z, mix_hgt, sigz )

c         CALL VERTTERM( h_e,z,mix_hgt,sigz, vertdisp ) 
        
         PTSECONC = q_src * vertdisp / 
     &              (SQRT(2 * pi) * sigz * wndspd * secwdth)
      
      ENDIF
      
      RETURN        
      
      END            