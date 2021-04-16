      SUBROUTINE VARBINT
C***********************************************************************
C     VARBINT.FOR 
C
C     Christian Fosmire
c     Pacific Northwest National Lab
c     P.O. Box 999
c     Richland, WA 99352
c     
c     Created:  1/15/96
c
c     Description:  Initializes the input and output data
C
C***********************************************************************
      IMPLICIT NONE
      
      INCLUDE  'hrly.inc'      
      
      kst     = 9
      icover  = 99
      idir    = 999
      iceil   = 9999

      wspeed  = 99.0
      afv     = 999.0
      fvr     = 999.0
      temp    = 9999.0
      precip  = 9999.0
      p3240   = 9999.0
      prpcode = 9
      ustr    = -9.0
      mixhgt  = -999.0
      recok   = .TRUE.      
      
      RETURN
      
      END      