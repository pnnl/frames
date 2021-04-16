C     MEPAS RADCON: CHECKS.FOR            Version Date: 07-10-1996
C     Copyright 1989 by Battelle Memorial Institute.  All rights reserved.    
C*****************************************************************************
C                                                                            *
C                       SUBROUTINE CHECKS                                    *
C                                                                            *
C  Subroutine CHECKS reviews the input control parameters to ensure that the *
C  scenario can be modeled.  It is called from subroutine CONTAM.            *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    07/10/96 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCONPC
C     Called by:  SUBROUTINE CONTAM
C     Calls to: None
C     Common blocks referenced: INDEX1, INDEX2, INDEX4
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter  Set/               Location
C     Name       Used     Type      Export/Import  Parameter Description
C     ---------  -----   --------   -------------  ---------------------------
C     IDDSW      USED     INTEGER   COMMON - IMP   INDEX FOR A KNOWN DIRECT
C                                                  DISCHARGE OR OVERLAND FLOW
C                                                  TO A SURFACE WATER         
C                                                     = 0; NO DIRECT DISCHARGE
C                                                          TO SURFACE WATER   
C                                                     = 1; KNOWN DIRECT DIS-  
C                                                          CHARGE TO RIVER    
C     IPOND      USED     INTEGER   COMMON - IMP   INDEX FOR LIQUID           
C                                                  IMPOUNDMENT AS A SOURCE    
C                                                       = 1;DESIGNATED PONDED 
C                                                           SITE              
C                                                       = 2;NOT A DESIGNATED  
C                                                           PONDED SITE       
C     ISOURC     USED     INTEGER   COMMON - IMP   INDEX FOR SOURCE TERM FLUX 
C                                                       = 1;INTERNALLY        
C                                                           COMPUTED SOURCE   
C                                                           FLUXES            
C                                                       = 2;KNOWN SOURCE FLUX 
C                                                       = 3;KNOWN SOURCE FLUX 
C                                                           & PRECIPITATION   
C                                                           GENERATED LEACHATE
C                                                       = 4;MEASURED          
C                                                           CONCENTRATIONS AT 
C                                                           RECEPTOR          
C     ISTYPE     USED     INTEGER   COMMON - IMP   INDEX ON SOURCE TERM TYPE  
C     J          USED     INTEGER   COMMON - IMP   INDEX FOR SCENARIO NUMBER
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description                              
C     -------      ---  ------------------------------------------------------
C     06/27/91     JGD  MINOR FORMAT AND SPELLING CHANGES                     
C     11/22/91     JWB  Added required comments and header for MEPAS QA plan  
C     11/14/92     JWB  Revised the list of scenarios accepted by RADCON      
C     12/02/92     JWB  Revised the list of scenarios accepted by RADCON      
C     01/27/94     KJC  Added lines so no stop is used ERRORSIG is set instead
C     06/10/94     JPM  Revised that statement that writes out the conditions
C                       under which modeling is performed.  It didn't match
C                       the conditions above.
C     07/28/95     JPM  Writing the error message to the *.WRN file in
C                       addition to the *.WLS file.  Added a shorter message
C                       for screen output.
C     10/26/95     JPM  Allowing ISCONF = 5 (yz-area source) when ISTYPE = 2
C                       and ISOURC = 2.
C     07/10/96     JPM  Allowing ISCONF = 6 (volume source) when ISTYPE = 2
C                       and ISOURC = 2 (RAD-E048).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE CHECKS 
C 
C==== COMMON Block Definitions ===============================================
C
CKJC  Added for dll compilation
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'ERROR.WTN'
      INCLUDE 'INDEX1.WTN'
      INCLUDE 'INDEX2.WTN'
      INCLUDE 'INDEX4.WTN'
C
C==== DIMENSION Statements ===================================================
C
C     None
C
C==== Variable Declarations ==================================================
C
C     None
C
C==== DATA Statements ========================================================
C
C     None
C
C*****************************************************************************
C                 ACCEPTABLE SCENARIOS ARE CHECKED (23 CASES)                           
C*****************************************************************************
C                          ISTYPE = 1 CASES                                   
C*****************************************************************************
      IF( ISTYPE.EQ.1 .AND. ISOURC.EQ.1 .AND. IPOND.EQ.1 .AND.
     1    ISCONF.EQ.4) RETURN
      IF( ISTYPE.EQ.1 .AND. ISOURC.EQ.1 .AND. IPOND.EQ.2 .AND.
     1    ISCONF.EQ.4) RETURN
      IF( ISTYPE.EQ.1 .AND. ISOURC.EQ.2 .AND. IPOND.EQ.1 .AND.
     1    ISCONF.LE.4) RETURN
      IF( ISTYPE.EQ.1 .AND. ISOURC.EQ.2 .AND. IPOND.EQ.2 .AND.
     1    ISCONF.LE.4) RETURN
      IF( ISTYPE.EQ.1 .AND. ISOURC.EQ.3 .AND. IPOND.EQ.2 .AND.
     1    ISCONF.LE.4) RETURN
      IF( ISTYPE.EQ.1 .AND. ISOURC.EQ.4 ) RETURN
C*****************************************************************************
C                          ISTYPE = 2 CASES                                   
C*****************************************************************************
      IF( ISTYPE.EQ.2 .AND. ISOURC.EQ.1 .AND. IPOND.EQ.1 .AND.
     1    ISCONF.EQ.4) RETURN
      IF( ISTYPE.EQ.2 .AND. ISOURC.EQ.1 .AND. IPOND.EQ.2 .AND.
     1    ISCONF.EQ.4) RETURN
      IF( ISTYPE.EQ.2 .AND. ISOURC.EQ.2 .AND. IPOND.EQ.1 .AND.
     1    ISCONF.LE.4) RETURN
      IF( ISTYPE.EQ.2 .AND. ISOURC.EQ.2 .AND. IPOND.EQ.2 .AND.
     1    ISCONF.LE.6) RETURN
      IF( ISTYPE.EQ.2 .AND. ISOURC.EQ.3 .AND. IPOND.EQ.2 .AND.
     1    ISCONF.EQ.4) RETURN
      IF( ISTYPE.EQ.2 .AND. ISOURC.EQ.4 ) RETURN
C*****************************************************************************
C                          ISTYPE = 3 CASES                                   
C*****************************************************************************
      IF( ISTYPE.EQ.3 .AND. ISOURC.EQ.1 .AND. IPOND.EQ.2 .AND.
     1    IDDSW.EQ.0) RETURN
      IF( ISTYPE.EQ.3 .AND. ISOURC.EQ.2 .AND. IPOND.EQ.2 .AND.
     1    IDDSW.EQ.0) RETURN
      IF( ISTYPE.EQ.3 .AND. ISOURC.EQ.2 .AND. IPOND.EQ.2 .AND.
     1    IDDSW.EQ.1) RETURN
      IF( ISTYPE.EQ.3 .AND. ISOURC.EQ.4 ) RETURN
C*****************************************************************************
C     FORMATS FOR SCENARIOS THAT CAN NOT BE MODELED IN CURRENT MODEL
C*****************************************************************************
      WRITE(6,1) JSCE,ISTYPE,ISOURC,IPOND,ISCONF,IDDSW
      WRITE(11,1) JSCE,ISTYPE,ISOURC,IPOND,ISCONF,IDDSW
      CALL MESSAGE ()
1     FORMAT (
     1' Modeling is performed only under the following conditions',/,
     2'    ISTYPE=1,2   ISOURC=1   IPOND=1,2 ISCONF=4',/,
     3'    ISTYPE=1     ISOURC=2   IPOND=1,2 ISCONF=1,2,3,4',/,
     4'    ISTYPE=1     ISOURC=3   IPOND=2   ISCONF=1,2,3,4',/,
     5'    ISTYPE=2     ISOURC=2   IPOND=1,2 ISCONF=1,2,3,4',/,
     Z'    ISTYPE=2     ISOURC=2   IPOND=2   ISCONF=5',/,
     6'    ISTYPE=2     ISOURC=3   IPOND=2   ISCONF=4',/,
     7'    ISTYPE=3     ISOURC=1,2 IPOND=2   IDDSW=0',/,
     8'    ISTYPE=3     ISOURC=2   IPOND=2   IDDSW=1',/,
     9'    ISTYPE=1,2,3 ISOURC=4',//,
     A' The indices in this modeling exercise are as follows:'/,
     B'    SCENARIO # =        ',I2,/
     C'    ISTYPE =            ',I2,/
     D'    ISOURC =            ',I2,/
     E'    IPOND =             ',I2,/
     F'    ISCONF =            ',I2,/
     G'    IDDSW =             ',I2,/)
CKJC  Added for dll compilation
      ERRORSIG=.TRUE.
CKJC      STOP
C
      END
