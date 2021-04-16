C     MEPAS RADCON: DUMMYV.FOR            Version Date: 06-10-1998
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                            SUBROUTINE DUMMYV                               *
C                                                                            *
C  Subroutine DUMMYV assigns dummy variables based on media type.  It is     *
C  called by the main program.                                               *
C                                                                            *
C  Written by :      Gene Whelan                                             *
C                    Battelle Pacific Northwest Laboratories                 *
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
C     Called by:  RADCONPC
C     Calls to: None
C     Common blocks referenced:  DUMMY, INIT, MEDIA, INPUT, INDEX1, INDEX2,   
C                                INDEX3, INDEX4                               
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter  Set/           Location
C     Name       Used   Type    Export/Import  Parameter Description
C     --------- ------ -------- -------------  -------------------------------
C     ALEN       SET    REAL    COMMON - EXP   LENGTH OF SOURCE TERM AT RIVER 
C     E1         SET    REAL    COMMON - EXP   DISPERSION IN THE X-DIRECTION  
C     E2         SET    REAL    COMMON - EXP   DISPERSION IN THE Y-DIRECTION  
C     E3         SET    REAL    COMMON - EXP   DISPERSION IN THE Z-DIRECTION  
C     C2         SET    REAL    COMMON - EXP   STORAGE FOR OTHER SUBROUTINES  
C     C3         SET    REAL    COMMON - EXP   STORAGE FOR OTHER SUBROUTINES  
C     C4         SET    REAL    COMMON - EXP   STORAGE FOR OTHER SUBROUTINES  
C     C5         SET    REAL    COMMON - EXP   STORAGE FOR OTHER SUBROUTINES  
C     C9         SET    REAL    COMMON - EXP   STORAGE FOR OTHER SUBROUTINES  
C     MED        USED   INTEGER COMMON - EXP   INDEX FOR MEDIA TYPE           
C     U          USED   REAL    COMMON - IMP   INPUT GROUNDWATER VELOCITY     
C     U1         SET    REAL    COMMON - EXP   RETARDED GROUNDWATER VELOCITY  
C     X2         SET    REAL    COMMON - EXP   SAME AS X3 OR IF RECEPTOR IS IN
C                                              FLUX ZONE; 1/2 DISTANCE FROM   
C                                              FIRST RECEPTOR TO UPSTREAM FLUX
C     X3         USED   REAL    ARG - IMPORT   DISTANCE DOWNSTREAM FROM SOURCE
C                                              CENTER TO RECEPTOR             
C     XL         SET    REAL    ARG - EXPORT   FRACTION OF FLUX BOUNDARY UP-  
C                                              STREAM FROM CURRENT RECEPTOR IN
C                                              FLUX ZONE.
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/25/91     JWB  Added required comments and header for MEPAS QA Plan  
C     05/23/94     JPM  XL has been changed to an array for use by the new
C                       formulation for concentrations at multiple river
C                       receptors.
C     07/22/94     JPM  Changed XL and ALEN from arrays to just variables.
C                       These parameters are not contaminant specific.
C     10/30/96     JPM  Made changes allowing MED = 6 to indicate flux
C                       computations in a river (PH1-E003).
C     04/15/97     JPM  Incorporated changes made for the new wetlands
C                       volatilization technique (RAD-E053).  No longer
C                       setting AL1 and ALEN to 1 for a wetland, since AL1
C                       and B1 are read in by INPUTM for the wetland
C                       environment (PH1-E007).
C     06/10/98     JPM  Writing the dispersion coefficients to the WLS
C                       file (PH1-E018).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE DUMMYV (C,X3,XL)
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'INDEX1.WTN'
      INCLUDE 'INDEX2.WTN'
      INCLUDE 'INDEX3.WTN'
      INCLUDE 'INDEX4.WTN'
      INCLUDE 'DUMMY.WTN'
      INCLUDE 'INIT.WTN'
      INCLUDE 'INPUT.WTN'
      INCLUDE 'MEDIA.WTN'
C
C==== DIMENSION Statements ===================================================
C
C
C==== Variable Declarations ==================================================
C
C     None
C
C==== DATA Statements ========================================================
C
C     None
C
      C=0.
      ALAM=0.
      ARG=0.
      ND=1
      IF (MED(KMED) .EQ. 7) GO TO 40
   30 CONTINUE
      AL1=ALEN
      AL2=AL1/2.
      B2=B1/2.
      IF (MED(KMED).NE.5.AND.MED(KMED).NE.6) GO TO 40
      XL=1.
      X2=X3
      IF (X3 .GE. AL2) GO TO 40
C*****************************************************************************
C     XL REPRESENTS THE FRACTION OF THE SOURCE TERM CONTRIBUTING TO THE       
C     CONTAMINATION AT THE RECEPTOR                                           
C*****************************************************************************
      XL=(X3+AL2)/AL1
      X2=(X3+AL2)/2.
      AL1=2.*X2
      AL2=X2
   40 CONTINUE
      IF (KMED .EQ. NMED) ND=NDS(M1)
      IF (NDS(M1) .EQ. 1) ALAM=AL(M1,1)
      IF (MED(KMED).EQ.5.OR.MED(KMED).EQ.6) GO TO 10
      U1=U/A(M1)
C*****************************************************************************
C     HYDRODYNAMIC DISPERSION = MECHANICAL DISPERSION AND MOLECULAR           
C     DIFFUSION (EQUAL TO APPROXIMATELY 0.52 CM**2/D = 190 CM**2/YR           
C*****************************************************************************
      DIFMOL=190.
      E1=(A1*U+DIFMOL)/A(M1)
      E2=(A2*U+DIFMOL)/A(M1)
      E3=(A3*U+DIFMOL)/A(M1)
      IF (MED(KMED).LT.5.AND.IT.EQ.1.AND.M1.EQ.1) WRITE (6,80) E1*A(M1)
      IF (MED(KMED).GT.2.AND.IT.EQ.1.AND.M1.EQ.1) WRITE (6,85) E2*A(M1)
      IF ((MED(KMED).EQ.3.OR.MED(KMED).EQ.4).AND.IT.EQ.1.AND.M1.EQ.1)
     +WRITE (6,90) E3*A(M1)
   80 FORMAT (/' Longitudinal Dispersion Coefficient = ',1P,1E10.3,
     +' cm^2/yr')
   85 FORMAT (' Lateral Dispersion Coefficient =      ',1P,1E10.3,
     +' cm^2/yr')
   90 FORMAT (' Vertical Dispersion Coefficient =     ',1P,1E10.3,
     +' cm^2/yr')
   10 ALPHA=1./AN5/A(M1)
      C2=SQRT(4*E2)
      C3=SQRT(4*E1)
      C4=X2+AL2
      C5=X2-AL2
      C9=1/(SQRT(4*3.1415926))
   20 CONTINUE
      RETURN
      END
