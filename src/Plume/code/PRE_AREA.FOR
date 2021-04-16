      SUBROUTINE  PRE_AREA( sigin, conc_inp, crtdist, isrc )
C-----------------------------------------------------------------------
C     PRE_AREA
C
C     Christian J Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Date:       7/26/96
C
C     Description:   Set up the structures and calculate the 
C                    critical distance                                  
C
C     Subroutines:   CALCSIG
C     Functions:     PROFILE, TURBSIGV, TURBSIGW
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      INCLUDE 'parm.inc'
      INCLUDE 'metdata.inc'
      INCLUDE 'srcrec.inc'
      INCLUDE 'conc_inp.inc'
      INCLUDE 'sigin.inc'
      
      REAL     PROFILE
      REAL     TURBSIGV
      REAL     TURBSIGW
      
      REAL     crtdist, sigv, sigw, sigz     
      INTEGER  isrc
      
c     For an area source...release winds are that at 10 meters

      relspd = PROFILE( z0, ustr, molinv, stab, 10.0 )

c     If using turbulent statistics, calculate sigv and sigw

      sigv = TURBSIGV( stab, ustr, 10.0, mixhgt, molinv )
      sigw = TURBSIGW( stab, ustr, 10.0, mixhgt, molinv, sigv )
      
      conc_inp.sigv = sigv
      conc_inp.sigw = sigw
      
      
c     Put all information into structure
c     Note:  The release rate is 1/Area
      
      conc_inp.wndspd = relspd
      conc_inp.q_area = 1 / Src_Var1(isrc)
      conc_inp.rel_hgt = 0
      conc_inp.rec_hgt = 1
      conc_inp.cwdist = 0
      conc_inp.zmin = 1
      conc_inp.lenside = SQRT(Src_Var1(isrc)) / 2.0          
      conc_inp.mix_hgt = mixhgt
      conc_inp.minsigz = 0.01
      conc_inp.stab = stab
      conc_inp.sigopt = sigparm
      conc_inp.udepth = mixhgt

c     Calculate at what distance, crtdist, sigz = 1.2*udepth
c     udepth is always the mixing height as release height is zero

      sigz = 1.2 * conc_inp.udepth
             
      sigin.sigopt = sigparm
      sigin.stab = stab
      sigin.sigz = sigz
      sigin.relhgt = conc_inp.rel_hgt
      sigin.mixhgt = mixhgt
      sigin.dodist = .TRUE.
      sigin.wndspd = relspd
      sigin.sigv = sigv
      sigin.sigw = sigw
                                    
      CALL CALCSIG( sigin )
             
      crtdist = sigin.dist


      RETURN
      
      END