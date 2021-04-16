C     MEPAS RADCON: FLXTABLE.FOR            Version Date: 08-24-1998
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE FLXTABLE                              *
C                                                                            *
C  Subroutine FLXTABLE writes the table of new constituent fluxes and new    *
C  TLIFE's to the *.WLS file, after these values have been recomputed by     *
C  subroutines MASS and INPUTM.                                              *
C                                                                            *
C  Written by:       John P. McDonald                                        *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    04/17/97 (Converted to PC)                              *
C  Last Modified:    08/24/98 JPM                                            *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: MASS,INPUTM
C     Calls to:  none
C     Common blocks referenced: MAXIMUM
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- -----  ------  -------------  --------------------------------
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     06/09/98     JPM  Making IV a character variable type (PH1-E016).
C     08/24/98     JPM  Corrected a bug in shifting the TIME array back to
C                       a zero starting time (PH1-P017).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE FLXTABLE (EXT_FLUX,PNT,TIME,NUM,NUMCON,IV,TLIFE,FILENM)
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'MAXIMUM.WTN'
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION TLIFE(MAXCON),NUM(MAXCON)
      DIMENSION TIME(MAXNTI,MAXCON),PNT(MAXNTI,MAXCON)
C
C==== Variable Declarations ==================================================
C
      INTEGER*2 EXT_FLUX,NUMCON
      CHARACTER*8 FILENM
      CHARACTER*18 IV(MAXCON,12)
C
C==== DATA Statements ========================================================
C
C     none
C
      WRITE(6,164)
  164 FORMAT (/,'New Constituent Flux (g/yr or pCi/yr) and Time ',
     +'(Years)',
     1        ' at the Release Unit',//,
     2        2(' Constituent # ',4X,'Flux',6X,'Time',3X)/)
      if( EXT_FLUX .eq. 0 ) then
        write(6,593)
        write(6,594) filenm
      else
        do i = 1, numcon
          temp = min( 15, num(i) )
          WRITE(6,163) (I,J,PNT(J,I),TIME(J,I),J=1,temp)
          DO J=2,NUM(I)
            TIME(J,I)=TIME(J,I)-TIME(1,I)
          enddo
          TIME(1,I) = 0.0
        enddo
      endif
      WRITE(6,11)
   11 FORMAT (/,'The New Duration of Time that Constituents are',
     1        ' Released from the Release Unit',/)
      WRITE(6,2)
    2 FORMAT ('    #      Constituent         New TLIFE (Years)',/)
      DO I=1,NUMCON
        WRITE(6,3) I,IV(I,1),TLIFE(I)
    3   FORMAT (I5,7x,A18,13x,1P,E10.3)
      ENDDO
593   format('**** ',
     1       'Contaminant Fluxes Obtained from External Source',
     1       ' ****',/)
594   format(' **** ',
     1       'See File ',a18,'.WMS for Flux Values',
     1       ' ****',/)
163   FORMAT (2(4x,2I5,3x,1P,E10.2,1x,1p,E10.3))
      RETURN
      END
