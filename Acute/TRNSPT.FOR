C----------------------------------------------------------------------
C
      SUBROUTINE TRNSPT ()
C
C     This module controls calculation of yearly radionuclide migration 
C     through the environment.
C     For ACUTE releases, there are no more depositions from any source
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Last Modification: 23-Mar-98  BAN
C     Reviewed and Approved:             BA Napier
C     
C-----------------------------------------------------------------------
C     23-Mar-98  BAN  Vast simplification for ACUTE
C-----------------------------------------------------------------------
c 
C----- For times beyond the first year consider harvest removal --------------
C
      CALL DKHARV
C
C  Note:  this subroutine is more complex in Chronic and Near Field cases
C
      RETURN
C
C----- END OF SUBROUTINE TRNSPT ----------------------------------------------
C
      END

