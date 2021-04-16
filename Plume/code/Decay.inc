C  -- DECAY.FTC
c  This COMMON BLOCK is part of the TABLES software package for generation of
c  dosimetry factors for the Nuclear Regulatory Commission project on
c  Residual Radioactive Contamination from Decommissioning.
c
c  This common block contains chain decay parameters for one chain
c
c  Developed by:   Pacific Northwest Laboratory
c                  Occupational & Environmental Health Protection Section
c                  P. O. Box 999
c                  Richland, WA 99352
c
c         Software Design Description:  D.L. Strenge
c         Initial Coding:  D. A. Baker
c         Software Custodian: ...
c         Initial preparation: 11 Dec 90
c         Initial testing complete: (date)
c
c         Modification History:
c    Date  Change #  Summary of Modification                     Changed by
c  ------- --------  ------------------------------------------- ----------
c
c --------------------------------------------------------------------------
c
c    Modules referencing this COMMON Block:
c
c        AGPF, CHAIN
c
c --------------------------------------------------------------------------
c   Definition of COMMON BLOCKS
c
      COMMON /DECAY/  DK(2,9), AL(9), NUC, IFRM(2,9)
c
      REAL*8 DK, AL
      INTEGER IFRM
      INTEGER NUC                                                       !
c
c --------------------------------------------------------------------------
c
c  Parameters in COMMON BLOCK
c  Name (dim)       Type                     Purpose
c  -------------   --------  --------------------------------------------
c    NCHAIN         Parameter Number of chain members
c    NUC            Integer   Number of radionuclides in the current chain
c                             representation, maximum value is 9.
c
c    IFRM(2,9)      Integer   Index of the parents (if any) for each chain
c                             member, with two (2) parents allowed for each of
c                             nine (9) chain members.
c
c    DK(2,9)        Real      Fraction of each parent decays that result in
c                             production of the chain member, with index
c                             values corresponding to those defined for IFRM
c                             above.
c
c    AL(9)          Real      Radiological decay constant for each member of
c                             the chain, d-1.
c
c------------  End of file  DECAY.FTC  -----------------------------------
