
      SUBROUTINE WSIG ( Ttime, hts, vts, wsigy2, wsigz2 )

C---------------------------------------------------------------------------
c
c     WSIG                                          
c
c     Date:              August 16, 1995
c
c     Description:       Computes Sigma YW and ZW for building wake (1990)
c                        model.
c
c     Required modules:  None
c
C---------------------------------------------------------------------------

      IMPLICIT    NONE

      REAL        c, hts, Ttime, vts, wsigy2, wsigz2

      c = 0.5

C  Compute Wake diffusion Coefficients

      wsigy2 =  c * hts**2 * 
     &           ( 1 -  (1 + Ttime/hts) * EXP ( -Ttime/hts ) )
      wsigz2 =  c * vts**2 * 
     &           ( 1 -  (1 + Ttime/vts) * EXP ( -Ttime/vts ) )

      RETURN     
      
      END

