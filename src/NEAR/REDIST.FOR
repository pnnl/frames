C------------------------------------ Version: 08-Sep-97 ---------------------
C
C     SUBROUTINE REDIST
C
C     This subroutine calculates surface soil concentration due to
C     manual redistribution of the deep soil to the surface (e.g.,
C     excavation for a basement or foundation).  This redistribution
C     occurs in the first year of the exposure period.
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Last Modification: 22-Mar-88 RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C       
C---- Modification History ---------------------------------------------------
C   Date      Who    Modification Summary
C  ---------  ---  -----------------------------------------------------------
C  08-Sep-97  DLS  Initial modification of old GENII
C
C---- Subroutine Call Statement ----------------------------------------------
C
      SUBROUTINE REDIST
C    
C---- Include Statements -----------------------------------------------------
C
      INCLUDE 'RAD.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'SOLPAR.CMN'
      INCLUDE 'OPT.CMN'
C    
C---- Start of Analysis ------------------------------------------------------
C
      DO 172 IN = 1, NONUC
C
C-----  Residential/resuspension soil ----------------------------------------
C
        RESSOL(IN) = RESSOL(IN) + DSRES(IN) * MANULR
C
C-----  Terrestrial food pathway ---------------------------------------------
C
        DO 170 ITF = 1, NTF
          IF (TFD(ITF)) SOLCON(IN,ITF) = SOLCON(IN,ITF) + 
     .                                   DSCON(IN,ITF) * MANULR 
  170   CONTINUE
C         
C----   Animal food pathway --------------------------------------------------
C
        DO 174 IAN = 1, NAN
          IF (ANF(IAN)) SL2CON(IN,IAN) = SL2CON(IN,IAN) + 
     .                                   DS2CON(IN,IAN) * MANULR 
          IF (ANF(IAN) .AND. IAN .EQ. 1) SL2CON(IN,5) = SL2CON(IN,5) + 
     .                                   DS2CON(IN,5) * MANULR 
          IF (ANF(IAN) .AND. IAN .EQ. 3) SL2CON(IN,6) = SL2CON(IN,6) + 
     .                                   DS2CON(IN,6) * MANULR 
  174   CONTINUE
  172 CONTINUE   ! End of chain member loop
C
      RETURN
      END
C
C----- End of Module REDIST --------------------------------------------------
C
