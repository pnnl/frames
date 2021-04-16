      SUBROUTINE RESETOUT
C-----------------------------------------------------------------------
C     RESETOUT
C
C     Date:  10/16/97
C
C     Description:   Reset the output values to zero
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      INCLUDE  'parm.inc'
      INCLUDE  'output.inc'
      
      EOQ_ch = 0
      EOQ_hr = 0
      Dry_ch = 0
      Dry_hr = 0
      Wet_ch = 0
      Wet_hr = 0
      Shine_ch = 0
      Shine_hr = 0
      tot_wght = 0
      
      RETURN
      
      END