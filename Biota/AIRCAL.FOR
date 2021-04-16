C--------------------------------------- VERSION: 22-May-1997 ---------
C
C     SUBROUTINE AIRCAL
C
C     This module calculates air concentrations.  
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Last Modification: 27-Jun-90  RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C
C-----------------------------------------------------------------------
C
C     AFRAC    - Fraction of grid area in current sector
C     AREAT    - Total area in grid
C     DKAIR() -  Air source term decayed for transit, for the current 
C                exposure pathway, distance and direction
C     T        - Time in years passed to CHAIN
C
C------------------------------------------------------------------------
C
      SUBROUTINE AIRCAL
C
C---- Include Statments ------------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'AIRPAR.CMN'
      INCLUDE 'ANMPAR.CMN'
      INCLUDE 'AQUPAR.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'FODPAR.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
C
C---- Basic air concentration input ------------------------------------
C
        DO 103 IN = 1, NONUC
          ARPCON(IN) = AIRCON(IN)
  103   CONTINUE
C
C---- Set air concentration for terrestrial food crops -----------------
C
        IF (TFOOD) THEN 
          DO 108 ITF = 1, NTF
            IF (TFD(ITF)) THEN
              DO 105 IN = 1, NONUC
                ARFCON(IN,ITF) = AIRCON(IN)
  105         CONTINUE  
            ENDIF
  108     CONTINUE
        ENDIF
C
C---- Set air concentration for animal products -----------------------
C
        IF (ANFOOD .or. tfood) THEN
          DO 106 IAN = 1, NAN
            IF (ANF(IAN)) THEN
              DO 109 IN = 1, NONUC
                ARF2CN(IN,IAN) = AIRCON(IN)
  109         CONTINUE
            ENDIF
  106     CONTINUE
        ENDIF        
C
       RETURN
C
C----- END OF MODULE AIRCAL --------------------------------------------
C
      END

