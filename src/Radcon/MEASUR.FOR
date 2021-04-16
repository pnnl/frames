C     MEPAS RADCON: MEASUR.FOR            Version Date: 04-28-1997
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE MEASUR                                *
C                                                                            *
C  Subroutine MEASUR uses measured receptor contaminant concentrations in    *
C  the HPI calculations instead of modeling the transport process and        *
C  estimating the receptor concentrations.  It is called by the main         *
C  program.                                                                  *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    04/28/97 - JPM                                          *
C                                                                            *
C                                                                            *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: RADCONPC
C     Calls to: DEACY, GATHER, WLIST, ORIGIN
C     Common blocks referenced:  DUMMY, INTER, INIT, TIME, LIST, INDEX1,
C                                INDEX2, INDEX3, INDEX4, INDEX6, INDEX7,
C                                INDEX9, COM21
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Name
C     --------- ----- ------   -------------- --------------------------------
C     AL        USED   REAL    COMMON - IMP   HALF LIFE FOR CONTAMINANT
C     CO        USED   REAL    COMMON - IMP   MEASURED CONCENTRATION AT
C                                             RECEPTOR
C     IUNIT     SET    INTEGER COMMON - EXP   INDEX FOR UNITS ON CONCENTRATION
C     MSOURC    SET    INTEGER LOCAL          INDEX FOR RECEPTOR TYPE         
C     ND        USED   INTEGER COMMON - IMP   INDEX FOR TOTAL # OF CONSTITUENT
C                                             CHAIN, PARENT & DECAY- PRODUCTS 
C     TLIFE     USED   REAL    COMMON - IMP   TOTAL TIME OF RELEASE           
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     06/27/91     JGD  Correct PNT computation and IYR to call to ORIGIN
C     11/25/91     JWB  Added required comments and header for MEPAS QA plan  
C     10/22/92     BLH  Moved calc of QNEW*365.25 and QOLD*365.25 below the
C                       printout that is in units/day
C     01/29/93     GW   Modified to read in monitoring well data and estimate
C                       concentrations at receptor well(s)                    
C     01/28/94     KJC  Added lines so no stop is used ERRORSIG is set instead
C     05/23/94     JPM  The variable XL is the fraction of the source plume
C                       upstream from a river receptor.  It has been changed
C                       to an array due to changes in the formulation for
C                       concentrations at multiple river receptors.  XL is
C                       declared as a local array here.
C     07/14/94     JPM  Fixed the argument lists in the calls to DECAY and
C                       ORIGIN.  Previous modifications resulted in additional
C                       parameters being added to the argument lists of these
C                       two routines, but the call statements in this routine
C                       (MEASUR) were not updated.
C     07/22/94     JPM  Changed XL from an array to a single variable.  This
C                       parameter is not contaminant specific.
C     07/25/94     JPM  Passing the array of decay constants (AL) to ORIGIN.
C     07/29/94     JPM  Removed IYR - no longer needed by ORIGIN.
C     08/04/94     JPM  Estimated concentrations for a river receptor are now
C                       stored in CO just like for a well.  Max concentrations
C                       are found by ORIGIN in the CO array.
C     08/26/94     JPM  RATIO array is now in the argument list for ORIGIN.
C                       Just using a junk array here so the argument lists
C                       match.
C     07/28/95     JPM  Writing error message to the screen and the *.WRN file
C                       in addition to the *.WLS file.
C     08/03/95     JPM  Passing XJUNK into ORIGIN a total of 4 times due to a
C                       change in the argument list of ORIGIN.
C     11/11/96     JPM  Changed units from Ci to pCi (PH1-E006).
C     04/28/97     JPM  Passing a junk real array to subroutine ORIGIN in
C                       place of TIMAX (PH1-E014).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE MEASUR (TLIFE,CO,UNITNM,xl,MSOURC,QOLD,NFLAG)
C
C==== COMMON Block Definitions ===============================================
C
CKJC  Added for dll compilation
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'ERROR.WTN'
      INCLUDE 'INDEX1.WTN'
      INCLUDE 'INDEX2.WTN'
      INCLUDE 'INDEX3.WTN'
      INCLUDE 'INDEX4.WTN'
      INCLUDE 'INDEX6.WTN'
      INCLUDE 'INDEX7.WTN'
      INCLUDE 'INDEX9.WTN'
      INCLUDE 'COM21.WTN'
      INCLUDE 'CTIME.WTN'
      INCLUDE 'DUMMY.WTN'
      INCLUDE 'INIT.WTN'
      INCLUDE 'LIST.WTN'
      INCLUDE 'INTER.WTN'
      INCLUDE 'MEDIA.WTN'
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION CO(MAXCON),UNITNM(6),TLIFE(MAXCON),CONCMAX(MAXCON),
     +          CONCTIM(MAXCON),NFLAG(MAXCON),XJUNK(MAXCON)
C
C==== Variable Declarations ==================================================
C
      REAL*4 MDIST, RDIST
C
C==== DATA Statements ========================================================
C
C     None
C
      READ(5,1) MSOURC,IUNIT
    1 FORMAT (2I5)
C
C*****************************************************************************
C    Added check on MSOUCR (must be less than 4)  1/29/93 by GW and JWB
C*****************************************************************************
      If( Msourc.gt.3 ) THEN 
        Write(6,771) MSOURC
        Write(11,771) MSOURC
        CALL MESSAGE ()
771     FORMAT (
     1  /2x,'ERROR --> MSOURC = ',I2,
     2  /2x,'MEASUR: MSOURC must be less than 4')
CKJC  Added for dll compilation
        ERRORSIG=.TRUE.
        goto 9999
CKJC        STOP 'MEASUR --> MSOURC must be less that 4'
      ENDIF
C
      KMED=1
      QOLD=1.
      QNEW=1.
      NMED=1
      XL=1.
      do m1 = 1, numcon
        num(m1) = 2
        XJUNK(M1) = 0.
      enddo
      NTIMES=2
C
C*****************************************************************************
C     WRITE STATEMENTS
C*****************************************************************************
      WRITE(6,211) ISTYPE,ISOURC,IPOND,IDECAY,IDDSW,NUMCON,
     1             num(1),tlife(1)
  211 FORMAT(/,
     1 ' Source-Term Type (ISTYPE)',T47,'= ',I6/
     2 ' Source-Term Boundary Condition (ISOURC)',T47,'= ',I6/
     3 ' Liquid Impoundment Index (IPOND)',T47,'= ',I6/
     4 ' Source-Term Decay Index (IDECAY)',T47,'= ',I6/
     5 ' Direct Discharge Surface Water Index (IDDSW)',T47,'= ',I6/
     6 ' Number of Parent Constituents (NUMCON)',T47,'= ',I6/
     7 ' Number of Concentration Input Values (NUM)',T47,'= ',I6/
     8 ' Duration of Release (TLIFE)',T47,'= ',G10.2,' years'/)
C
      WRITE(6,411) NMED,NTIMES
  411 FORMAT(
     1 ' Number of Media (NMED)',T47,'= ',I6/
     2 ' Number of Discrete Time Steps (NTIMES)',T47,'= ',I6/)
C
      WRITE(6,201) KMED,MSOURC
  201 FORMAT(/,
     1 ' Medium # ',I2,' Measured Receptor Type (MSOURC) ',I5)
  729 IF (IUNIT .NE. 1) GO TO 731
      UNITS=1.0E-18
      GO TO 724
  731 IF (IUNIT .NE. 2) GO TO 732
      UNITS=1.0E-12
      GO TO 724
  732 IF (IUNIT .NE. 3) GO TO 733
      UNITS=1.0E-06
      GO TO 724
  733 IF (IUNIT .NE. 4) GO TO 734
      UNITS=1.0E-03
      GO TO 724
  734 IF (IUNIT .NE. 5) GO TO 735
      UNITS=1.0E-02
      GO TO 724
  735 IF (IUNIT .EQ. 6) GO TO 736
CKJC  Added for dll compilation
      ERRORSIG=.TRUE.
      goto 9999
CKJC      STOP
  736 UNITS=1.0E+03
  724 CONTINUE
C
      WRITE(6,4602) UNITNM(IUNIT),UNITS
 4602 FORMAT(' The Units of This Program are in ',A4,' (',1PE10.2,')',
     1 ' Curies or Grams'/)
C
      WRITE(6,621)
  621 FORMAT (/43x,'Surface',/,
     1 ' Constituent  CAS ID  Half-Life  Initial',
     2 ' Equilibrium  Solubility  Constituent',/,23x,'(Years)',
     3 '     Conc.    Coeff.      limit     Inventory'/,
     4 44x,'(ml/g)      (g/ml)    (g OR pCi)'/)
C
      DO 501 M1=1,NUMCON
      IMULR(M1)=2
      ND=NDS(M1)
      WRITE(6,206) IV(M1,1),CASN1(M1,1),AL(M1,1),CO(M1),eqcoef(M1),
     1             sollim(M1),CINVEN(M1)
  206 FORMAT (1X,A18,4x,A18,1P,E9.2,2x,E9.2,1x,E9.2,3x,E10.2,2x,E10.2)
      IF (ND .LE. 1) GO TO 501
      DO 502 N=2,ND
  502 WRITE(6,277) IV(M1,N),CASN1(M1,N),AL(M1,N)
  277 FORMAT (1X,A18,4x,A18,1P,E9.2)
  501 CONTINUE
C*****************************************************************************
C    Variable CRATIO added to compare monitoring and receptor distances       
C                         1/29/93 - GW
C*****************************************************************************
      cratio=1.
      GO TO (551,552,553), MSOURC
  551 MED(KMED) = 3
      WRITE(6,561)
C*****************************************************************************
C    Read in Variables MDIST, RDIST, and H1 for esitmating concentrations at
C    receptor well base on monitoring well data  1/29/93 - GW                 
C*****************************************************************************
      READ(5,461) MDIST,RDIST,H1
  461 FORMAT (8E10.3)
      HMIXM=7.62E-3*MDIST
      HMIXR=7.62E-3*RDIST
      IF ((H1 .GE. HMIXM) .AND. (H1 .GE. HMIXR)) THEN
        CRATIO=(MDIST/RDIST)**2.
      ELSE
        IF ((H1 .LE. HMIXM) .AND. (H1 .LE. HMIXR)) THEN
          CRATIO=MDIST/RDIST
        ELSE
          IF ((H1 .GE. HMIXM) .AND. (H1 .LE. HMIXR)) THEN
            CRATIO=7.62E-3*MDIST**2./H1/RDIST
          ELSE
            CRATIO=MDIST*H1/7.62E-3/RDIST**2.
          ENDIF
        ENDIF
      ENDIF
      WRITE(6,462) H1,HMIXM,HMIXR,CRATIO,MDIST,RDIST
  462 FORMAT (//2X,'Aquifer Depth (cm) = ',1pe10.3/
     c          2X,'Monitoring Well Mixing Depth (cm) = ',1pe10.3/
     c          2X,'Receptor Well Mixing Depth (cm) = ',1pe10.3/
     c          2X,'Monitoring/Receptor Well Ratio = ',1pe10.3/
     c          2X,'Distance to Monitoring Well (cm) = ',1pe10.3/
     c          2X,'Distance to Receptor Well (cm) = ',1pe10.3//)
c
      GO TO 557
  552 MED(KMED) = 5
      WRITE(6,562)
       GO TO 557
  553 MED(KMED) = 7
      WRITE(6,563)
  561 FORMAT (/,' Known Concentrations at a Well')
  562 FORMAT (/,' Known Concentrations at a River')
  563 FORMAT (/,' Known Concentrations at a Holding Pond or Wetland')
  557 CONTINUE
C
C*****************************************************************************
C     IF RIVER, THEN CHECK FOR MULTIPLE RECEPTORS ON RIVER
C*****************************************************************************
      IF (MSOURC .NE. 2) GO TO 120
      READ(5,2) NUMREC,QOLD,QNEW
    2 FORMAT (I5,2E10.3)
      WRITE(6,236) NUMREC,QOLD,QNEW
  236 FORMAT(
     1' Number of River Receptor Locations (NUMREC)',T52,'= ',I6/
     2' Discharge Where Concentrations are Measured (QOLD)',T52,'= '
     3       ,1PE10.3,' cm**3/day'/
     4' Downstream Discharge at First Receptor (QNEW)',T52,'= '
     5       ,1PE10.3,' cm**3/day')
      qold = qold * 365.25
      qnew = qnew * 365.25
  120 CONTINUE
C*****************************************************************************
C     INITIALIZE PARAMETERS
C*****************************************************************************
      DO 110 M1=1,NUMCON
      TRAVEL(M1)=0.
      TSTRT1(M1)=0.
      TEND1(M1)=TLIFE(M1)
      STRT(M1)=0.
      TSTRT2(M1)=0.
C*****************************************************************************
C     Computes new concentration at receptor based on CRATIO (when MSOURC=1)
C     or QOLD and QNEW (when MSOURC=2) - JPM 08/04/94
C*****************************************************************************
      CO(M1)=CO(M1)*CRATIO*QOLD/QNEW
C*****************************************************************************
C     STORE MEASURED CONCENTRATIONS (CO) IN PNT
C*****************************************************************************
      NUMP1=NUM(M1)+1
      DO 100 JJ=1,NUMP1
      TIME(JJ,M1)=0.
      PNT(JJ,M1)= CO(M1)/UNITS
  100 CONTINUE
      TIME(NUM(M1),M1)=TLIFE(M1)
  110 TIME(NUMP1,M1)=TLIFE(M1)+1.
C*****************************************************************************
C     CHECK TO SEE IF CONTAMINANTS DECAY AT THE SOURCE, THEN EXPAND SOURCE    
C     TERM SO THAT THERE ARE AT LEAST NTIMES SOURCE-TERM INPUT DATA POINTS
C*****************************************************************************
      ICOUNT=0
      DO 56 JJ=1,NUMCON
      IF (NDS(JJ) .GT. 1) ICOUNT=ICOUNT+1
   56 CONTINUE
      IF (ICOUNT .EQ. 0) GO TO 50
      WRITE(6,332) ICOUNT
      WRITE(11,332) ICOUNT
      CALL MESSAGE ()
  332 FORMAT (
     1 /2x,'ERROR --> ',I2,' Constituents have Decay Products',
     2 /2x,'When ISOURC=4 decay products are not allowed')
CKJC  Added for dll compilation
      ERRORSIG=.TRUE.
      goto 9999
CKJC      STOP
   50 CONTINUE
C****************************************************************************
C     TRANSFER SOURCE-TERM (I.E., RECEPTOR) INPUT TO RECEPTOR OUTPUT
C****************************************************************************
      DO 111 N=1,ND
      DO 112 M1=1,NUMCON
      NUM(M1)=IMULR(M1)
  112 AL(M1,N)=ALOG(2.)/AL(M1,N)
  111 CONTINUE
      DO 200 IT=1,NTIMES+1
      ICNT=0
      DO 210 M1=1,NUMCON
      ND=NDS(M1)
      T2(M1)=TIME(IT,M1)
C*****************************************************************************
C     DECAY/DEGRADATION IS NOT REQUIRED BECAUSE THESE CALCULATIONS ARE        
C     ASSOCIATED WITH MEASURED ENVIRONMENTAL CONTAMINANT LEVELS               
C*****************************************************************************
      C=PNT(IT,M1)
      CALL DECAY (TIME(IT,M1),AL,CDA,ND,C,M1,TRAVEL,MED(KMED),
     1            CO,TPK,IDECAY,ICONC(M1))
      DO 334 L=1,ND
  334 CALL WLIST (IV(M1,L),CASN1(M1,L),CDA(L),ICNT,M1)
  210 CONTINUE
      CALL GATHER (ICNT)
  200 CONTINUE
      CALL ORIGIN (CS,IVS,CASN2,ICNT,M1,UNITS,CONCMAX,
     1             CONCTIM,NDS,co,CS_SPEC,NFLAG,AL,XJUNK,XJUNK,
     2             XJUNK,XJUNK,XJUNK)
  222 CONTINUE
 9999 RETURN
      END
