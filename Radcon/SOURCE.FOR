C     MEPAS RADCON: SOURCE.FOR            Version Date: 11-06-1996
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                          SUBROUTINE SOURCE                                 *
C                                                                            *
C  Subroutine SOURCE is designed to obtain input source-term information.    *
C  It is called from the main program and calls the subroutine LEACHV to     *
C  calculate the leaching rate and source term flux.                         *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    11/06/96 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: RADCONPC
C     Calls to:  SUBROUTINE LEACHV
C     Common blocks referenced: INTER, VLEA1, CONC1, TIME, INDEX1, INDEX2
C                               INDEX3
C
C==== Significant Parameter Designation and Descriptions =====================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Name
C     --------- -----  -------- -------------- -------------------------------
C     ISOURC    USED  INTEGER  COMMON - IMP   INDEX FOR SOURCE TERM FLUX
C     ISTYPE    USED  INTEGER  COMMON - IMP   INDEX FOR SOURCE TERM TYPE
C     NUMCON    USED  INTEGER  COMMON - IMP   NUMBER OF PARENT CONSTITUENTS
C     PNT       SET   REAL     COMMON - EXP   FLUX OF WATER EITHER AS ANNUAL
C                                             INFILTRATION OR OVERLAND FLOW,
C                                             DEPENDING ON ISOURC AND ISTYPE
C     VLEACH    SET   REAL     COMMON - EXP   LEACHING RATE AT SOURCE, ALSO
C                                             KNOWN AS VARIABLE U
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/26/91     JWB  Added required comments and header for MEPAS QA plan
C     09/28/92     JLS  Initialized VLECH1 to zero
C     01/27/94     KJC  Added lines so no stop is used ERRORSIG is set instead
C     04/20/94     JPM  Bug fix - subtracted 1 from I before returning - see
C                       note below.
C     07/28/95     JPM  Writing error message to the screen and the *.WRN file
C                       in addition to the *.WLS file.
C     11/06/96     JPM  Setting TLIFE equal to the last flux time minus the
C                       first flux time.  TLIFE as read from the *.WIN file
C                       will not always equal the last flux time less the
C                       first flux time due to rounding (PH1-P003).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE SOURCE (VLEACH,AL1,BLEN,BWIDTH,TLIFE,SEDFLX,CO,AN5,R2,
     1                   UNITS,B1,CLEN,GAMMA,I)
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
      INCLUDE 'CTIME.WTN'
      INCLUDE 'INTER.WTN'
      INCLUDE 'VLEA1.WTN'
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION CO(MAXCON),TLIFE(MAXCON)
C
C==== Variable Declarations ==================================================
C
C     None
C
C==== DATA Statements ========================================================
C
C     None
C
      VLEACH=0.
      VLECH1=0.
      AL1=1.
      DO 20 I=1,NUMCON
   20 NUM(I)=2
      ILEACH=0
C*****************************************************************************
C     COMPUTATION OF THE LEACHING RATE INCLUDED IN THIS SUBROUTINE IS THE 
C     COMPUTATION OF THE CONTAMINANT FLUX SOURCE TERM (SEE SUBROUTINE PNTTIM)
C*****************************************************************************
      CALL LEACHV (ILEACH,VLEACH,BLEN,BWIDTH,TLIFE,SEDFLX,CO,
     1             AN5,R2,UNITS,B1,CLEN,I)
CKJC  Added for dll compilation
      if (ERRORSIG) goto 9999
      DO I = 1,NUMCON
        TLIFE(I)=TIME(NUM(I),I)-TIME(1,I)
      ENDDO
      WRITE(6,200) ISTYPE,ISOURC,IPOND,IDECAY,IDDSW,NUMCON
  200 FORMAT(
     1       ' Source-Term Type (ISTYPE)                     = ',I2/
     2       ' Source-Term Flux Boundary Conditions (ISOURC) = ',I2/
     3       ' Liquid Impoundment Index (IPOND)              = ',I2/
     4       ' Source-Term Decay Index (IDECAY)              = ',I2/
     5       ' Direct Discharge Surface Water Index (IDDSW)  = ',I2/
     6       ' Number of Parent Constituents (NUMCON)        = ',I2/)
C
C******************************************************************************
C     The loop index I was being left at the value NUMCON+1.  I is passed
C     back to RADCOND (as M1) and is subsequently used in the call to INITIN
C     as ERROR(M1).  When the maximum allowable number of constituents
C     (MAXCON) was being run on, a subscript out of range error was occurring,
C     because ERROR is dimensioned out to MAXCON, and M1 is 1+ this number.
C     So, subtract 1 from I before returning to RADCOND - JPM
C******************************************************************************
C
      I=I-1
      RETURN
 9999 END
