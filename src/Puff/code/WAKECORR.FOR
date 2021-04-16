      SUBROUTINE WAKECORR ( area, dist, ubar, dsy, dsz )

C---------------------------------------------------------------------------
c
c     WAKECORR
c
c     Date:              August 23, 1995
C
C     Description:  WAKECORR computes adjustments to the NRC diffusion
c                   coefficients to account for building wakes.  These
c                   adjustments, described in PNL-10286 (Ramsdell 1995), 
c                   are most significant in high wind speed conditions.  
c                   The numerical coefficients in the model are based on 
c                   turbulence and wake diffusion data.  
c
c     Required modules:  None
c       
C---------------------------------------------------------------------------

      IMPLICIT   NONE

      REAL       area, dist, dsy, dsz, hts, tt, ubar, vts 

C  Horizontal time scale for diffusion, in seconds  ==>  hts
    
      hts = 10 * SQRT ( area ) / ubar

C  Vertical time scale for diffusion, in seconds ==>  vts

      vts = hts 

C  Travel time = dist / ubar  ==> tt

      tt = dist / ubar

C  Compute adjustments to NRC diffusion coefficients

      dsy = 
     &   SQRT ( 5.24E-2 * ubar**2 * area * 
     &          (1.0 - (1.0 + tt/hts) * EXP ( -tt/hts )) ) 

      dsz = 
     &   SQRT ( 1.17E-2 * ubar**2 * area * 
     &          (1.0 - (1.0 + tt/vts) * EXP ( -tt/vts )) )

      RETURN                                             
      
      END
