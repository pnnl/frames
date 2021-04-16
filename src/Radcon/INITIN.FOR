C     MEPAS RADCON: INITIN.FOR            Version Date: 04-28-1997
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE INITIN                                *
C                                                                            *
C  Subroutine INITIN reads the initial data input.  It is called by RADCON.  *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    04/28/97 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: RADCONPC
C     Calls to:  SUBROUTINES MASS, PNTTIM and FLXTABLE
C     Common blocks referenced: INTER, INIT, TIME, MEDIA, CONC1, INDEX1,
C                               INDEX2, INDEX3, INDEX4, INDEX8, INDEX9,
C                               EXTFLUX
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- -----  ------  -------------  --------------------------------
C     AL1       SET    REAL    COMMON - EXP   LENGTH OF WASTE SITE            
C     AN5       USED   REAL    ARG - IMPORT   WASTE UNIT MOISTURE CAPACITY,   
C                                             SAME AS WUMC                    
C     B1        SET    REAL    ARG - EXPORT   WIDTH OF WASTE SITE             
C     CLEN      SET    REAL    ARG - EXPORT   LENGTH OF WASTE SITE            
C     ISTYPE    USED   INTEGER COMMON - IMP   INDEX FOR SOURCE TERM TYPE      
C     NTIMES    SET    INTEGER COMMON - EXP   NUMBER OF INTEGRATION TIME STEPS
C                                             SET TO 40                       
C     R2        USED   REAL    COMMON - IMP   WASTE UNIT BULK DENSITY, SAME   
C                                             AS WUBD                         
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     06/27/91     JGD  Output text revised
C     11/25/91     JWB  Added required comments and header for MEPAS QA plan  
C     11/14/29     JWB  ISCONF variable put in CONTAM but left in INITIN to
C                       assure that old *.WIN files will still work
C     01/31/93     JWB  Include statement EXTFLUX added to provide filename of
C                       external file that contains conatamint fluxes (mass
C                       balance or other codes)
C     11/03/93     JPM  Changed the statements that write the contaminant
C                       fluxes to the .WLS file to accommodate an odd number
C                       of fluxes
C     01/27/94     KJC  Added lines so no stop is used ERRORSIG is set instead
C     04/15/94     JPM  HRA-EIS Time Specific Modification - added lines to
C                       write the input information to the *.WLS file
C                       if this modification is invoked (NTIMSPEC > 0).
C                       Also, aborts the specific time computation if the last
C                       medium is not a saturated zone.  NTIMSPEC is described
C                       in INDEX1.WTN  JPM/GW
C     06/01/94     JPM  Subroutine CONTAM no longer sets CO(I) to 1E+30
C                       if the user did not provide an initial concentration.
C                       This routine (INITIN) wrote out -99.9 for the initial
C                       concentration if CO(I) was GT 1E29.  Changed this
C                       conditional to LE 0.0.
C     06/02/94     JPM  No longer reading in release unit dimensions for
C                       point sources.
C     07/22/94     JPM  Changed ALEN from an array to a single variable.  This
C                       parameter is not contaminant specific.
C     07/28/95     JPM  Writing error messages to the screen and the *.WRN
C                       file in addition to the *.WLS file.  Writing warning
C                       messages to the *.WRN file only.
C     10/26/95     JPM  Reading in ZTOP and ZBOT for ISCONF = 5, yz-area source
C     07/10/96     JPM  Reading in ZTOP and ZBOT for ISCONF = 6, volume source
C                       in addition to the length (AL1) and width (B1)
C                       (RAD-E048).
C     11/11/96     JPM  Changed units from Ci to pCi (PH1-E006).
C     04/17/97     JPM  Moved writing the new constituent fluxes and TLIFE
C                       table to subroutine FLXTABLE.  This table needs to be
C                       written from INITIN and INPUTM (PH1-P004).
C     04/28/97     JPM  Not writing decay product information to the table in
C                       the *.WLS file - it duplicates information already
C                       being written for the decay product as a parent and
C                       will just confuse users (PH1-E014).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE INITIN (NDS,AL,ND,UNITS,UNITNM,B1,ALEN,BLEN,
     1                   BWIDTH,CO,SEDFLX,AN5,R2,CLEN,I,ERROR,TLIFE,
     2                   ZTOP,ZBOT)
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
      INCLUDE 'INDEX8.WTN'
      INCLUDE 'INDEX9.WTN'
      INCLUDE 'CONC1.WTN'
      INCLUDE 'CTIME.WTN'
      INCLUDE 'INIT.WTN'
      INCLUDE 'INTER.WTN'
      INCLUDE 'MEDIA.WTN'
      INCLUDE 'extflux.WTN'
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION NDS(MAXCON),AL(MAXCON,12),UNITNM(6)
      DIMENSION CO(MAXCON),TLIFE(MAXCON)
C
C--- Variable Declarations --------------------------------------------------
C
C
C==== DATA Statements ========================================================
C
C     None
C
C*****************************************************************************
C     READ SOURCE-TERM CONFIGURATION, NUMBER OF MEDIA AND NUMBER OF
C     INTEGRATION TIME STEPS
C*****************************************************************************
      B1=1.
      READ(5,601) ISCONF,NMED,IUNIT,NTIMES,ERROR
  601 FORMAT (4I5,E10.3)
1     FORMAT (16I5)
      WRITE(6,200) ISCONF,NMED,NTIMES
  200 FORMAT(
     1'Source-Term Configuration (ISCONF)',t43,'=',i3,/,
     2'Number of Media (NMED)',T43,'=',I3,/
     3'Number of Integration Time Steps (NTIMES)',T43,'=',I3,/)
      I10=30
      IF (NMED .LE. I10) GO TO 34
C
      WRITE(6,35) NMED,I10
      WRITE(11,35) NMED,I10
      CALL MESSAGE ()
   35 FORMAT (
     1 /2x,'ERROR --> This run specifies ',I3,' media and this',
     2 /2x,'code only handles up to ',I3,' media')
CKJC  Added for dll compilation
      ERRORSIG=.TRUE.
      goto 9999
CKJC      STOP
C*****************************************************************************
C     READ THE MEDIA TYPES IN ORDER 
C*****************************************************************************
   34 READ(5,1) (MED(K), K=1,NMED)
C
      DO 202 K=1,NMED
      WRITE(6,201) K,MED(K)
  202 CONTINUE
  201 FORMAT(
     1'Medium # ',I2,' Equals Medium Type (MED) # ',I2)
C
C*****************************************************************************
C     HRA-EIS Time Specific Modification - if this modification is invoked,
C     check to make sure the last medium is the saturated zone.  If so,
C     write out a message and print the specific times to the *.WLS file.
C     If not, write out a warning message saying that specific time
C     calculations are only done on receptors in the saturated zone, then
C     reset NTIMSPEC (described in INDEX1.WTN) to zero.  JPM/GW 04/15/94
C*****************************************************************************
C
      IF (NTIMSPEC.GT.0) THEN
           WRITE (6,738) NTIMSPEC
           IF(MED(NMED).EQ.3) THEN
                WRITE (6,739)
                WRITE (6,740) (TIMSPEC(I),I=1,NTIMSPEC)
           ELSE
                WRITE (11,741) MED(NMED)
                NTIMSPEC=0
           ENDIF
      ENDIF
738   FORMAT (/,'Number of Specific Times (NTIMSPEC)',T43,'=',I3)
739   FORMAT (/,'Concentrations in the saturated zone will be computed',
     1' only at',/,'the following specific times:',//,T10,'Time(years)',
     2/,T10,'-----------')
740   FORMAT (T10,1PE10.3)
741   FORMAT (
     1 /2x,'WARNING --> MED(NMED) = ',I3,
     2 /2x,'Concentrations at specific times are only computed for a',
     3 /2x,'receptor in the saturated zone (MED(NMED) = 3)'/)
C
      IF (NMED .EQ. 1) GO TO 21
      DO 23 K=1,NMED-1
      IF (MED(K) .NE. 3) GO TO 23
      WRITE(6,22)
      WRITE(11,22)
      CALL MESSAGE ()
   22 FORMAT (
     1 /2x,'ERROR --> Flux Calculations can not occur at Medium',
     2 /2x,'Type 3 (GROUNDWATER WELL)')
CKJC  Added for dll compilation
      ERRORSIG=.TRUE.
      goto 9999
CKJC      STOP
   23 CONTINUE
   21 IF ((ISTYPE .EQ. 3) .OR. (ISTYPE .EQ. 5)) GO TO 522
C*****************************************************************************
C     FOR LINE OR AREA SOURCES, READ THE RELEASE UNIT DIMENSIONS
C*****************************************************************************
      IF (ISCONF.GE.2.AND.ISCONF.LE.4) THEN
        READ(5,2001) AL1,B1
        WRITE(6,204) AL1,B1
      ELSE IF (ISCONF.EQ.5) THEN
        READ(5,2001) B1,ZTOP,ZBOT
        WRITE(6,205) B1,ZTOP,ZBOT
      ELSE IF (ISCONF.EQ.6) THEN
        READ(5,2001) AL1,B1,ZTOP,ZBOT
        WRITE(6,207) AL1,B1,ZTOP,ZBOT
      ELSE
        AL1 = 0.
        B1 = 0.
      ENDIF
  204 FORMAT(/,
     1'Length of Release Unit (AL1 OR CLEN)                = ',1P,E10.2,
     2      ' cm',/,
     3'Width of Release Unit (B1)                          = ',E10.2,
     4      ' cm',/)
  205 FORMAT(/,
     1'Width of Release Unit (B1)                          = ',1P,E10.2,
     2      ' cm',/,
     3'Depth to top of Release Unit (ZTOP)                 = ',E10.2,
     4      ' cm',/,
     5'Depth to bottom of Release Unit (ZBOT)              = ',E10.2,
     6      ' cm',/)
  207 FORMAT(/,
     1'Length of Release Unit (AL1 OR CLEN)                = ',1P,E10.2,
     2      ' cm',/,
     3'Width of Release Unit (B1)                          = ',E10.2,
     4      ' cm',/,
     5'Depth to top of Release Unit (ZTOP)                 = ',E10.2,
     6      ' cm',/,
     7'Depth to bottom of Release Unit (ZBOT)              = ',E10.2,
     8      ' cm',/)
      CLEN=AL1
      GO TO 523
  522 IF (ISOURC .GT. 1) GO TO 523
      READ(5,2001) CLEN,B1
      WRITE(6,204) CLEN,B1
  523 IF (AL1 .LE. 0.) AL1=1.
      IF (B1 .LE. 0.) B1=1.
      IF (CLEN .LE. 0.) CLEN=1.
 2001 FORMAT (8E10.3)
      sedrate = sedflx * clen * b1
      IF ((ISOURC .EQ. 1) .AND. (ISTYPE .GT. 2))
     1   WRITE(6,101) PNT(1,1),sedrate
      IF ((ISOURC .EQ. 1) .AND. (ISTYPE .LE. 2))
     1   WRITE(6,102) PNT(1,1)
  101 FORMAT ('The Average-Annual OVERLAND Water Flow Rate         = ',
     1              1P,E10.2,' cm/yr',/,
     2        'The Average-Annual OVERLAND Sediment Soil Loss      = ',
     3              E10.2,' g/yr',/)
  102 FORMAT ('The Average-Annual Infiltration Rate                = ',
     1              1P,E10.2,' cm/yr',/)
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
      WRITE(6,4602) UNITNM(IUNIT),UNITS
 4602 FORMAT ('Water concentration units are: ',A4,
     1        ' (',1PE9.3,')',' pCi OR g'/)
      I10=MAXCON
      K=0
      WRITE(6,621)
  621 FORMAT (43x,'Surface',/,
     1 'Constituent   CAS ID  Half-Life  Initial',
     2 ' Equilibrium  Solubility  Constituent',/,23x,'(Years)',
     3 '     Conc.    Coeff.      limit     Inventory'/,
     4 44x,'(ml/g)      (g/ml)    (g OR pCi)'/)
      ALEN=AL1
      DO 501 I=1 ,NUMCON
      aa = co(i)
      if( aa .LE. 0.0 ) aa = -99.9
      WRITE(6,206) IV(I,1),CASN1(I,1),AL(I,1),aa,EQCOEF(I),
     1             SOLLIM(I),CINVEN(I)
  206 FORMAT (A8,5x,A18,1P,E9.2,2x,E9.2,1x,E9.2,3x,E10.2,2x,E10.2)
      ND=NDS(I)
      IF (ISOURC .NE. 1) GO TO 107
      IF ((ISTYPE .NE. 4) .AND. (ISTYPE .NE. 6)) GO TO 105
C******************************************************************
C HOLDING POND CALCULATIONS
C ISOURC=1 AND ISTYPE=4 OR 6
C******************************************************************
      ILEACH=3
      CALL PNTTIM (FPSUM,TLIFE,ILEACH,CO,AN5,R2,SEDFLX,
     +             BLEN,BWIDTH,UNITS,AL1,B1,CLEN,I)
CKJC  Added for dll compilation
      if(ERRORSIG) goto 9999
      GO TO 107
  105 CONTINUE
C******************************************************************
C GROUNDWATER PERCOLATION CALCULATIONS
C ISOURC=1 AND ISTYPE=1 OR 2
C OVERLAND RUNOFF CALCULATIONS
C ISOURC=1 AND ISTYPE=3 OR 5
C******************************************************************
      ILEACH=4
      CALL PNTTIM (FPSUM,TLIFE,ILEACH,CO,AN5,R2,SEDFLX,
     +             BLEN,BWIDTH,UNITS,AL1,B1,CLEN,I)
CKJC  Added for dll compilation
      if (ERRORSIG) goto 9999
  107 CONTINUE
      IF (ND .LE. 1) GO TO 777
CJPM      DO 502 N=2,ND
CJPM  502 WRITE(6,206) IV(I,N),CASN1(I,N),AL(I,N)
  777 K=K+ND
      TSTRT1(I)=0.
      TSTRT3(I)=0.
      STRT(I)=0.
      T2(I)=0.
      TRAVEL(I)=TIME(1,I)
      TPEAK(I)=0.
      TSTRT1(I)=TIME(1,I)
      TEND1(I)=TIME(NUM(I),I)
  501 CONTINUE
      WRITE(6,162)
  162 FORMAT (//,'Constituent Flux (g/yr or pCi/yr) and Time (Years)',
     1        ' at the Release Unit',//,
     2        2(' Constituent # ',4X,'Flux',6X,'Time',5X)/)
C*******************************************************************
C   If external flux exists (EXT_FLUX=0), then write filename
C   Else, write fluxes out  JWB - 01/31/93
C*******************************************************************
      DO I=1,NUMCON
        if( EXT_FLUX .eq. 0 ) then
          write(6,593)
593       format('**** ',
     1           'Contaminant Fluxes Obtained from External Source',
     1           ' ****',/)
          write(6,594) filenm
594       format(' **** ',
     1           'See File ',a8,'.MAS for Flux Values',
     1           ' ****',/)
        else
          temp = min( 15, num(i) )
          WRITE(6,163) (I,J,PNT(J,I),TIME(J,I),J=1,temp)
  163     FORMAT (2(4x,2I5,3x,1P,E10.2,1x,1p,E10.3))
        endif
      enddo
C*******************************************************************
C THIS SUBROUTINE CHECKS TO ENSURE THAT MORE MASS AT THE SOURCE
C THAN THAT WHICH IS AVAILABLE IS NOT BEING RELEASED INTO THE
C ENVIRONMENT
C*******************************************************************
      CALL MASS (TLIFE)
      CALL FLXTABLE (EXT_FLUX,PNT,TIME,NUM,NUMCON,IV,TLIFE,FILENM)
      IF (numcon .LE. I10) GO TO 31
      WRITE(6,32) I10,numcon
      WRITE(11,32) I10,numcon
      CALL MESSAGE ()
   32 FORMAT (
     1 /2x,'ERROR --> Too many constituents for a single run:',
     2 /2x,'This version can handle only up to ',I2,'constituents',
     3 /2x,'and this run specifies ',I2,'constituents')
CKJC  Added for dll compilation
      ERRORSIG=.TRUE.
      goto 9999
CKJC      STOP
   31 CONTINUE
      RETURN
 9999 END
