C     MEPAS RADCON: PNTTIM.FOR            Version Date: 02-28-1993
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.
C*****************************************************************************
C                                                                            *
C                       SUBROUTINE PNTTIM                                    *
C                                                                            *
C  Subroutine PNTTIM calculates the source flux term (PNT) and time (TIME),  *
C  depending on the control parameters for the leach rate (ILEACH).  PNTTIM  *
C  is called by subroutines INITIN and LEACHV and calls subroutine CONC.     *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    01/27/94 - KJC                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of:  RADCON
C     Called by:  SUBROUTINES INITIN and LEACHV
C     Calls to:   SUBROUTINE CONC
C     Common blocks referenced: INTER, CONC1, INDEX2, INDEX3, isv
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- -----  ------  -------------  --------------------------------
C     AL1       USED   REAL    ARG - IMPORT   LENGTH OF WASTE UNIT
C     B1        USED   REAL    ARG - IMPORT   WIDTH OF SITE ASSOCIATED WITH
C                                             OVERLAND RUNOFF
C     BLEN      USED   REAL    ARG - IMPORT   OVERLAND FLOW DISTANCE FROM
C                                             UPPER BOUNDARY OF BASIN TO
C                                             HOLDING POND
C     BWIDTH    USED   REAL    ARG - IMPORT   OVERLAND FLOW WIDTH OF BASIN
C     CO        USED   REAL    ARG - IMPORT   DISSOLVED PORE WATER
C                                             CONCENTRATION
C     CP        USED   REAL    ARG - IMPORT   CONCENTRATION ON SEDIMENT
C     ILEACH    USED   REAL    ARG - IMPORT   INDEX ON DEFINING PNT TYPE;
C                                             = 1; STORES OVERLAND FLOW IN PNT
C                                                  FOR ISOURC=1 AND ISTYPE>2;
C                                                  OR STORES INFILTRATION RATE
C                                                  IF ISOURC=1 AND ISTYPE>3
C                                             = 2; STORES KNOWN SOURCE TERM
C                                                  FLUXES IN PNT FOR ISOURC=2
C                                             = 3; HOLDING POND CONCENTRATIONS
C                                                  STORED IN PNT (ISTYPE=4,6)
C                                             = 4; STORES OVERLAND RUNOFF
C                                                  FLUXES FOR ISTYPE=3 OR 5
C     PNT       SET    REAL    COMMON - EXP   EITHER SOURCE TERM FLUXES OF
C                                             STORAGE OF PERCOLATION FOR
C                                             FURTHER FLUX SOURCE TERM
C                                             CALCULATIONS
C     SEDFLX    SET    REAL    ARG - EXPORT   SEDIMENT FLUX FROM OVERLAND FLOW
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/26/91     JWB  Added required comments and header for MEPAS QA plan
C     03/11/92     JWB  COMMON blocks converted to INCLUDE statements
C     03/12/92     JWB  INCLUDE statement 'INDEX1.WTN' added for ISTYPE value.
C                       ILEACH=1 goes to 90 (not 60) to include sedflx in the
C                       PNT calulation.
C     08/16/92     JWB  Changed GO TO 90 back to GO TO 60 for ILEACH=1
C     02/28/93     KJC  Include ISV and Tech added for RAAS glass_pnt computed
C                       and compared with original pnt
C     01/27/94     KJC  Added lines so no stop is used ERRORSIG is set instead
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE PNTTIM (FPSUM,TLIFE,ILEACH,CO,AN5,R2,SEDFLX,BLEN,
     1                   BWIDTH,UNITS,AL1,B1,CLEN,III)
C
C==== COMMON Block Definitions ===============================================
C

CKJC  Added for dll compilation
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'ERROR.WTN'
      INCLUDE 'INDEX1.WTN'
      INCLUDE 'INDEX2.WTN'
      INCLUDE 'INDEX3.WTN'
      INCLUDE 'CONC1.WTN'
      INCLUDE 'INTER.WTN'
      include 'tech.wtn'
      include 'isv.wtn'
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION CO(MAXCON), TLIFE(MAXCON)
C
C==== Variable Declarations ==================================================
C
C     None
C
C==== DATA Statements ========================================================
C
C     None
C
      GO TO (10,20,40,40), ILEACH
C*****************************************************************************
C     STORES THE AVERAGE-ANNUAL OVERLAND FLOW VOLUME IN PNT FOR ISOURC=1 AND
C     ISTYPE>2; STORES THE AVERAGE-ANNUAL LANDFILL INFILTRATION RATE IN PNT
C     FOR ISOURC=1 AND ISTYPE<3
C*****************************************************************************
   10 DO 30 I=1,NUMCON
      PNT(1,I)=FPSUM
      PNT(2,I)=FPSUM
      TIME(1,I)=0.
   30 TIME(2,I)=TLIFE(I)
C*****************************************************************************
C     CHANGED GO TO STATMENT JWB - 11/16/92
C     PREVIOUSLY --> GO TO 90
C     NOW        --> GO TO 60
C*****************************************************************************
      GO TO 60
C
C*****************************************************************************
C     STORES KNOWN SOURCE-TERM FLUXES IN PNT FOR ISOURC=2
C*****************************************************************************
   20 DO 50 I=1,NUMCON
   50 READ(5,147) (PNT(J,I),TIME(J,I), J=1,NUM(I))
  147 FORMAT (8E10.3)
      GO TO 60
C
   40 CALL CONC (CO,AN5,R2,III)
      GO TO (70,70,80,90), ILEACH
CKJC  Added for dll compilation
   70 ERRORSIG=.TRUE.
      GOTO 60
CKJC   70 STOP
C*****************************************************************************
C     HOLDING POND CALCULATIONS; IF ISOURC=1 AND ISTYPE=4 OR 6
C*****************************************************************************
   80 DO 106 IJ=1,NUM(III)
  106 PNT(IJ,III)=(PNT(IJ,III)/BLEN/BWIDTH*CO(III)+SEDFLX*CP)
     1            /UNITS*AL1*B1/PNT(IJ,III)
      GO TO 60
C
C*****************************************************************************
C     GROUNDWATER PERCOLATION CALCULATIONS; ISOURC=1 AND ISTYPE=1 OR 2
C
C     OVERLAND RUNOFF CALCULATIONS; ISOURC=1 AND ISTYPE=3 OR 5
C*****************************************************************************
   90 IF (ISTYPE .le. 2) then
        SEDFLX=0.
        CP=0.
      endif
      DO IJ=1,NUM(III)
C*****************************************************************************
C  glass_pnt     is the flux rate of contaminants from ISV waste form
C  glass_lr      is the leach rate of ISV glass
C  rass_cs       is the contaminant iii concentration
C  waste_vol*0.6 is the surface area of the ISV waste form
C  tech_type     is the current technology being considered
C
C  if glass_pnt is less then pnt, then pnt equals glass_pnt
C  else pnt is equal to the original flux rate computed by the code
C  this assure that pnt is always less than or equal to original pnt
C*****************************************************************************
        PNT(IJ,III)=(PNT(IJ,III)*CO(III)+SEDFLX*CP)*CLEN*B1
        if( tech_type .eq. 47 ) then
          glass_pnt(ij,iii) = glass_lr*raas_cs(iii)*waste_vol*0.6
c JLS   What is the use of this assignment?!
c JLS          ppp = pnt(ij,iii)
          if( pnt(ij,iii) .gt. glass_pnt(ij,iii) ) then
            pnt(ij,iii) = glass_pnt(ij,iii)
          endif
        endif
      enddo
   60 RETURN
      END

