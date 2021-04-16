C     MEPAS RADCON: RISKCONC.FOR            Version Date: 06-09-98
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                         SUBROUTINE RISKCONC                                *
C                                                                            *
C  Subroutine RISKCONC writes out the concentrations at the time risk        *
C  computations begin to the *.WLS file.                                     *
C                                                                            *
C  Written by:       John P. McDonald                                        *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    04/28/97                                                *
C  Last Modified:    06/09/98 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of:  RADCON
C     Called by:  RADCONPC
C     Calls to: None
C     Common blocks referenced:  
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- ------ ------  ------------- ---------------------------------
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     06/09/98     JPM  Making IVS a character variable type (PH1-E016).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE RISKCONC(NUMCON,NDS,TIMAX,NTIMES,SMASS,IVS,TDIFF,CS,
     +                    TOTTIM,UNITS)
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'MAXIMUM.WTN'
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION NDS(MAXCON),TIMAX(MAXCON),SMASS(MAXCON),IVS(MAXCON)
      DIMENSION TDIFF(MAXCON),TOTTIM(MAXNTI,MAXCON),CS(MAXNTI,MAXCON)
C
C==== Variable Declarations ==================================================
C
      REAL*4 TDIFF,TOTTIM,SMASS,TIMAX,UNITS,CS
      CHARACTER*18 IVS
      INTEGER*4 NDS,NUMCON,NTIMES,M1,M3,II
C
C==== DATA Statement =========================================================
C
C     None
C
C*****************************************************************************
C     THE CONCENTRATIONS FOR THE YEAR IN WHICH RISK CALCULATIONS BEGIN
C     (WS-TRISK) ARE PRINTED OUT HERE.
C     IF II IS 1 THEN ONLY ONE CONCENTRATION IS PRINTED OUT BUT IF II > 1,    
C     THEN THE CONCENTRATION JUST BEFORE AND AFTER THE START DATE FOR RISK IS
C     PRINTED OUT.    JWB  1-26-1989  JPM 07-29-94
C*****************************************************************************
C
      write(6,787)
787   format(1X,78('*'),/,' Times are given as years since the start',
     1' of risk calculations (WS-TRISK)'/)
      M3=1
      M1=1
      DO WHILE (M1.LE.NUMCON)
        II = 1
        DO WHILE ((TDIFF(M1)-TOTTIM(II,M1)).GT.0.0.AND.II.LE.NTIMES+1)
          II = II + 1
        ENDDO
        IF(TIMAX(M1).LE.0.0) THEN
C
C***********************************************************************
C     Determine if concentrations are zero because the source term flux
C     for the parent is zero.  JPM  12/22/94
C***********************************************************************
C
          IF (SMASS(M1).LE.0.0) THEN
            WRITE (6,185) IVS(M3)
185         FORMAT(1X,'NOTE for ',A18,':  Concentration',
     1               ' is Zero because the mass flux from the',/,
     2               ' source for this constituent (or the parent'
     3               ' constituent) is Zero.')
          ELSE
            WRITE(6,149) IVS(M3)
149         FORMAT(1X,'NOTE for ',A18,':  Constituent is',
     1                ' past the receptor by the end of TDIFF')
          ENDIF
        ELSE                
          IF (II.EQ.1) THEN
            write(6,788) ivs(m3),cs(ii,m3)*units
788         format(1x,'Concentration for ',A18,' when risk '
     1      ,'calculations begin (0.0 yrs) is:'/
     2      ,10X,1PE10.3,' (g/ml or pCi/ml)'/)
          ELSE
            WRITE(6,789) IVS(M3),CS(II-1,M3)*units,
     1      TOTTIM(II-1,M1)-tdiff(m1),CS(II,M3)*units,
     2      TOTTIM(II,M1)-tdiff(m1)
789         format(1x,'Concentration for ',A18,' when risk '
     1      ,'calculations begin (0.0 yrs) is between:'/
     2      ,10X,1PE10.3,' (g/ml or pCi/ml) at ', 0PF6.1,
     3      ' years',/,10X,1PE10.3,' (g/ml or pCi/ml) at ',
     4      0PF6.1,' years'/)
          ENDIF
        ENDIF
        M1 = M1 + 1
        M3 = M3 + NDS(M1-1)
      ENDDO
      RETURN
      END
