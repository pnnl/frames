C     MEPAS RADCON: MULREC.FOR            Version Date: 06-10-1998
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE MULREC                                *
C                                                                            *
C  Subroutine MULREC calculates the concentration hydrographs for multiple   *
C  receptors along a river where the transport scenarios are the same.       *
C  The contaminant concentration is diluted as it travels downstream due to  *
C  the additional volume added by river tributaries.  Subroutine MULREC      *
C  is called by the main program.                                            *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    06/10/98 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of:  RADCON
C     Called by:  RADCOND
C     Calls to:   None
C     Common blocks referenced: CTIME, LIST, INDEX1, INDEX2, INDEX3, INDEX4,
C                               INDEX5, INDEX6, INDEX8, DUMMY, MAXIMUM,
C                               COM21, LIST, INTER, ERROR
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import   Parameter Description
C     --------- ------ ------- --------------- -------------------------------
C     CS         S/U    REAL    COMMON - EXP   CONTAMINANT CONCENTRATION AT   
C                                              RECEPTOR LOCATION; CS UPDATED  
C                                              AT EVERY NEW LOCATION          
C     CMAX       USED   REAL    ARG - IMPORT   PASSED FROM RADCOND AS CONCMAX;
C                                              MAX CONTAMINANT CONCENTRATION  
C     CON        USED   REAL    ARG - IMPORT   DECAYED INITIAL CONCENTRATIONS
C                                              AT THE SOURCE FOR EACH TIMESTEP
C                                              FOR EACH PARENT
C     IMULR      USED  INTEGER  COMMON - IMP   NUMBER OF 70 YR AVERAGES
C                                              WRITTEN TO *.WAT FILE
C     MTOT       SET   INTEGER  LOCAL          TOTAL NUMBER OF CONSTITUENTS   
C                                              INCLUDING DAUGHTER PRODUCTS    
C     NFLAG      SET   INT      ARG - EXPORT   INDICATES WHEN COMPUTED
C                                              CONCENTRATIONS EXCEED INITIAL
C                                              SOURCE CONCENTRATIONS
C     QNEW       SET   REAL     COMMON - EXP   DOWNSTREAM DISCHARGE AT NEW    
C                                              RECEPTOR LOCATION              
C     QOLD       SET   REAL     LOCAL          DOWNSTREAM DISCHARGE AT        
C                                              PREVIOUS RECEPTOR LOCATION     
C     XLNEW      SET   REAL     LOCAL          FRACTION OF SOURCE PLUME
C                                              UPSTREAM OF CURRENT RECEPTOR.
C     XLOLD      USED  REAL     ARG - IMP/EXP  FRACTION OF SOURCE PLUME
C                SET                           UPSTREAM OF PREVIOUS RECEPTOR
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     01/26/89     JWB  Maximum concentration for each constituent computed   
C     03/26/89     JWB  Decay-product book-keeping changed and CSMAX added    
C     06/27/91     JGD  Changes to allow nonsequential order of receptors and 
C                       some output text changed.                             
C     11/26/91     JWB  Added required comments and header for MEPAS QA plan  
C     01/28/94     KJC  Added lines so no stop is used ERRORSIG is set instead
C     04/20/94     GW   Changed the computation of concentrations
C                  AdH  with multiple receptors.
C     04/26/94     JPM  Corrected error in writing records out to the *.POL
C                  AdH  file.  Code was writing out the same number of records
C                       to the *.POL file as it was writing to the *.WAT file.
C                       Created a new loop on NTIMES.
C     05/19/94     JPM  To compute concentrations for the current receptor,
C                       this module modifies the concentrations for the
C                       previous receptor.  However, the algorithm was always
C                       using the mixing length and the discharge at the first
C                       receptor, instead of the previous receptor.  Removed
C                       the conditional on QOLD = QNEW so the discharge at the
C                       previous receptor is saved before QNEW is read in, and
C                       added a statement at the end of this module to save
C                       XMIXNEW in XMIX.
C     05/23/94     JPM  Placed a check on XMIXNEW so it is not set larger than
C                       the width of the river.
C     05/23/94     JPM  New formulation for multiple river receptors. Will now
C                       take into account XL for the previous receptor when
C                       calculating concentrations for the current receptor.
C                       The variable XL was changed to an array on parent
C                       constituents, so the values for the previous receptor
C                       could be saved.
C     05/31/94  JPM/AdH Calling COUPLE to compute and write out the 70yr
C                       average concentrations to the *.WAT file for multiple
C                       river receptors.  COUPLE has been modified to allow
C                       for many more than just 200 records.
C     06/01/94     JPM  Replaced DMAX with CO.  DMAX is not being used anymore.
C     06/02/94     JPM  Warning message about exceeding the initial
C                       concentration at the source is only written when the
C                       initial concentration is a dissolved concentration
C                       (ICONC(M1) = 3).
C     06/03/94     JPM  Writing a warning message to the *.WLS file if the
C                       maximum concentration exceeds the solubility limit.
C     06/10/94     JPM  Using X2 to redefine the distance from the center of
C                       the plume entering the river for receptors within the
C                       plume.  Using X2 in the mixing length calculation so
C                       it matches the mixing length computation in SSWAP.
C     07/13/94     JPM  Added NFLAG - indicates when the warning message about
C                       exceeding initial source concentrations should be
C                       written.  Also added CON - this arrays holds the
C                       decayed initial concentrations for each timestep for
C                       each parent.  Revised the text of the warning message
C                       that is written if the maximum concentration exceeds
C                       the initial source concentration.
C     07/22/94     JPM  Changed XLOLD, XLNEW, and ALEN from arrays to single
C                       variables.  These parameters are not contaminant
C                       specific.
C     07/29/94     JPM  Removed IYR from the argument list - not writing the
C                       calendar year for the max concentration in the *.WLS
C                       file since it is incorrect if WS-TRISK is not equal to
C                       the current date.
C     08/23/94     JPM  Removed the lines that write to the maximum
C                       constituent table.  Replaced with a call to MAXTABLE.
C                       Added CASN1 to the argument list - receiving from
C                       RADCOND and passing to MAXTABLE.  Setting TDIFF to
C                       zero before calling MAXTABLE, since TDIFF is already
C                       subtracted from the TIME array.
C     07/28/95     JPM  Writing the error message to the screen and the *.WRN
C                       file in addition to the *.WLS file.
C     11/11/96     JPM  Changed units from Ci to pCi (PH1-E006).
C     04/15/97     JPM  Removed the call to subroutine COUPLE.  No longer
C                       needed since concentrations are passed to AHAZ using
C                       the *.WCF file (PH1-E007).
C     04/18/97     JPM  Added a call to OUTPUT replacing the former call to
C                       COUPLE (PH1-P007).
C     04/28/97     JPM  Made subroutine compatible with permuted decay
C                       products (PH1-E014).
C     06/09/98     JPM  Making CASN1 a character variable type (PH1-E016).
C     06/10/98     JPM  Passing JSCE, NUMWU and NUMFLX to subroutine
C                       OUTPUT (PH1-E017).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE MULREC (XLOLD,CMAX,CO,CON,NFLAG,CASN1,LOCNAM)
C
C==== COMMON Block Definitions ===============================================
C
CKJC  Added for dll compilation
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'DUMMY.WTN'
      INCLUDE 'ERROR.WTN'
      INCLUDE 'INDEX1.WTN'
      INCLUDE 'INDEX2.WTN'
      INCLUDE 'INDEX3.WTN'
      INCLUDE 'INDEX4.WTN'
      INCLUDE 'INDEX5.WTN'
      INCLUDE 'INDEX6.WTN'
      INCLUDE 'INDEX8.WTN'
      INCLUDE 'INDEX9.WTN'
      INCLUDE 'COM21.WTN'
      INCLUDE 'CTIME.WTN'
      INCLUDE 'LIST.WTN'
      INCLUDE 'INTER.WTN'
      INCLUDE 'EXTFLUX.WTN'
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION CMAX(MAXCON),CTIM(MAXCON)
      DIMENSION CO(MAXCON),NFLAG(MAXCON)
      DIMENSION CON(MAXNTI,MAXCON)
C
C==== Variable Declaration ===================================================
C
      CHARACTER*18 CASN1(MAXCON,12)
      CHARACTER*21 LOCNAM
	CHARACTER*15 RIVX,RIVY,RIVZ
C
C==== DATA Statements ========================================================
C
C     None
C
      MED = 5
      IJK=-88
      IMFLAG=IMFLAG+1
      WRITE (9,996) IMFLAG
996   FORMAT (///2X,'SURFACE WATER RECEPTOR # ',I5//)
      WRITE(9,237)
237   FORMAT (T6,'#',T9,'TIME',T15,'NAME',T35,
     1        'TRAVEL',T43,'CONC/UNITS',T56,'UNITS'/T9,'INC.',
     2        T36,'TIME',T44,'(G/ML) OR'/,T36,
     3        '(YR)',T45,'(pCI/ML)'/)
      IF (LOCMED .EQ. 3) GO TO 20
      WRITE(6,1) LOCMED,NUMREC,IMFLAG
      WRITE(11,1) LOCMED,NUMREC,IMFLAG
      CALL MESSAGE ()
    1 FORMAT (
     1 /2x,'ERROR --> LOCMED = ',I2,' NUMREC = ',I2,' IMFLAG = ',I2,
     2 /2x,'LOCMED must equal 3 for multiple receptors along a river')
CKJC  Added for dll compilation
      ERRORSIG=.TRUE.
      goto 9999
CKJC      STOP
20    QOLD=QNEW
C*****************************************************************************
C     THIS CHANGE CAPTURES FIRST DISCHARGE RATE AND DOES NOT ASSUME QQOLD 
C     IS INITIALLY ZERO.  JGD 06/27/91
C*****************************************************************************
      IF (QQOLD.LE.0.0) qqold = qnew
  991 FORMAT (2X,3I5)
C*****************************************************************************
C     READ NEW DOWNSTREAM DISCHARGE AT NEXT DOWNSTREAM RECEPTOR
C*****************************************************************************
      READ(5,2) QNEW,H1,B1,X3
2     FORMAT (8E10.3)
      QNEW=QNEW*365.25
      U1=QNEW/H1/B1
C*****************************************************************************
C     CALCULATING NEW SOURCE-TERM LENGTH BASED ON FRACTION OF WASTE
C     SITE UPSTREAM OF RECEPTOR.  ALSO CALCULATE THE MIXING LENGTH FOR THE
C     CURRENT RECEPTOR.  IF THE RECEPTOR IS WITHIN THE PLUME ENTERING THE
C     RIVER, REDIFINE THE DISTANCE TO THE RECEPTOR FROM THE CENTER OF THE
C     PLUME (X2) FOR USE IN COMPUTING THE MIXING LENGTH.
C*****************************************************************************
      X2=X3
      XLNEW=1.
      AL1=ALEN
      AL2=ALEN/2.
      IF (X3 .LT. AL2) THEN
        XLNEW=(X3+AL2)/AL1
        X2=(X3+AL2)/2
      ENDIF
      XMIXNEW=SQRT(0.03*H1*X2)
      IF (XMIXNEW .GT. B1) XMIXNEW = B1
      WRITE (6,291) IMFLAG,QNEW,H1,B1,U1,X3,XMIX,XMIXNEW
291   FORMAT (////2X,'SURFACE WATER RECEPTOR LOCATION',I5/
     +T10,'DISCHARGE                            = ',1PE10.3,2X,'CM3/YR'/
     +T10,'DEPTH                                = ',1PE10.3,2X,'CM'/
     +T10,'WIDTH                                = ',1PE10.3,2X,'CM'/
     +T10,'FLOW VELOCITY                        = ',1PE10.3,2X,'CM/YR'/
     +T10,'DOWNSTREAM DISTANCE                  = ',1PE10.3,2X,'CM'/
     +T10,'MIXING DISTANCE AT PREVIOUS RECEPTOR = ',1PE10.3,2X,'CM'/
     +T10,'MIXING DISTANCE AT THIS RECEPTOR     = ',1PE10.3,2X,'CM'//)
      M1 = 1
      M3 = 1
      DO WHILE (M1.LE.NUMCON)
        if( isourc .eq. 4 ) then
          cmax(m3) = (co(m3)/units) / (qnew/qqold)
        endif
C*****************************************************************************
C     CMAX2 IS THE MAXIMUM CONCENTRATION FOR THE GIVEN CONSTITUENT.  
C                              JWB 3-26-89
C*****************************************************************************
        TMAX2=0
        CMAX2=0
        DO I=1,NTIMES+1
          CS(I,M3)=CS(I,M3)*QOLD*XLNEW*XMIX/QNEW/XMIXNEW/XLOLD
          PNT(I,M1) = CS(I,M3)
          IF (CS(I,M3).GT.CON(I,M1).AND.ICONC(M1).EQ.3) THEN
            NFLAG(M1) = 1
            MFLG = MFLG + 1
          ENDIF
          IF (CMAX2 .LT. CS(I,M3)) THEN
            CMAX2 = CS(I,M3)
            TMAX2 = TOTTIM(I,M1) - TDIFF(M1)
          ENDIF
          WRITE(9,995) M3,I,IVS(M3),TOTTIM(I,M1),CS(I,M3),UNITS
995       FORMAT (2X,2I5,2X,A18,10X,1P9E10.3)
        ENDDO
        XTRAV = 0.
        XDIFF = 0.
        CALL MAXTABLE(TMAX2,CMAX2,CS,TOTTIM,XTRAV,M1,M3,NFLAG(M1),
     +  NTIMES,IVS(M3),TDIFF(M1),NUMCON,CASN1(M1,1),SOLLIM(M1),CO(M1),
     +  UNITS,MED,ISOURC)
        M1 = M1 + 1
        M3 = M3 + NDS(M1-1)
      ENDDO
      CALL OUTPUT(FILENM,5,NUMCON,IVS,CASN1,NTIMES+1,NDS,TIM,
     +            CS,-99.,-99.,-99.,-99.,-99.,-99.,-99.,-99.,-99.,
     +            UNITS,NUMSCE,.FALSE.,.FALSE.,LOCNAM,JSCE,NUMWU,NUMFLX,
     +            KTYPE,RIVX,RIVY,RIVZ)
      WRITE(9,'(/,'' END OF RECEPTOR '',I2,'' RUN'',//)')IMFLAG
      WRITE(6,781)
781   FORMAT(/1X,78('*'))
C
******************************************************************************
C     Save the mixing length and the fraction of the source plume upstream
C     of the receptor (XL) to use for the next receptor.  JPM  05/23/94
C*****************************************************************************
C
      XMIX = XMIXNEW
      XLOLD = XLNEW
9999  RETURN
      END
