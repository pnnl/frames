C     MEPAS RADCON: WETLND.FOR            Version Date: 03-03-1997
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                        SUBROUTINE WETLND                                   *
C                                                                            *
C  Subroutine WETLND calculates concentrations in and contaminant fluxes     *
C  existing in a wetland.  It is called from the main program and calls      *
C  subroutines FUNCTION (function EXPF), INTERP, and LIMITS.                 *
C                                                                            *
C  Written by:       CH2M HILL of Seattle, Washington                        *
C                                                                            *
C  Modified by:      Gene Whelan                                             *
C                    Battelle Northwest Laboratories                         *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    03/03/97 - JPM/MVA                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: RADCONPC
C     Calls to:  SUBROUTINES FUNCTION, INTERP, and LIMITS
C     Common blocks referenced: INIT, WETLND, INDEX3
C
C---- Significant Parameter Designation and Descriptions ------------
C
C     Parameter      Set/           Location
C     Name           Used   Type    Export/Import  Parameter Description
C     ------------- ------ -------- -------------- ----------------------
C     GW_MASS       Set    Real     Argument       Summation of the mass
C                                                  lost to the groundwater
C                                                  pathway
C     HLC           Set    Real     Common         Henry's Law Constant
C     SW_MASS       Set    Real     Argument       Summation of the mass
C                                                  lost to the surface
C                                                  water pathway
C     VOL_MASS      Set    Real     Argument       Summation of the mass
C                                                  lost to the volatilization
C                                                  pathway
C     DEC_MAS       Set    Real     Argument       Summation of the mass
C                                                  lost to the decay pathway 
C     VTYPE         Set    Integer  Common         Type of volatilization
C                                                  1 = sediment-controlled
C                                                  2 = controlled by the water
C                                                  phase conc of contaminant 
C     TAIR          Set    Real     Common         annual average air temp (K)
C     SS            Set    Real     Common         susp sed conc in wetl(g/ml)
C     DEPTH         Set    Real     Common         depth of watercolumn in
C                                                  wetland (cm)
C     KOV           Set    Real     Argument       overall mass transfer coeff
C                                                  from a liq phase to the
C                                                  atmosphere for contaminant
C                                                  (cm/s)
C     KLIQ          Set    Real     Argument       Liq-phase mass transfer
C                                                  coeff for contaminant at
C                                                  a pond surface (cm/s)
C     KHI           Set    Real     Argument       Modified HLC for contaminant
C                                                  (unitless)
C     KGAS          Set    Real     Argument       Gas-phase mass transfer
C                                                  coeff for contaminant at a
C                                                  pond surface (cm/s)
C     RI            Set    Real     Argument       Retardation factor for
C                                                  contaminant (unitless)
C     DW            Set    Real     Argument       Molecular diffusivity in 
C                                                  water (cm2/s)
C     BENTHIC       Set    Integer  Common         (Yes=0 and No=1) For Benthic
C                                                  organisms in the water
C     PT            Set    Real     Argument       Total porosity (Unitless)
C     SBULK         Set    Real     Common         Soil bulk density (g/cm3)
C     KWS           Set    Real     Argument       Sediment-side mass-transfer
C                                                  coefficient at the sediment-
C                                                  water interface (cm/s)
C     HS            Set    Real     Common         depth of contaminated
C                                                  sediment (cm)
C     AREA          Set    Real     Common         area of waterbody (cm2)
C
C---- Constants used in calculations ------------------------------------------
C
C     VA            Set    Real     Argument       volumetric air content
C                                                  of soil (unitless)
C     R             Set    Real     Argument       Boltzmann gas constant
C                                                  (atm m^3)/(mole K)
C     MWO2          Set    Real     Argument       molecular weight of oxygen
C                                                  (g/mole)
C     KO2           Set    Real     Argument       ind liq-phase mass transfer
C                                                  coeff for O2 at 25 C (cm/s)
C     MWH2O         Set    Real     Argument       molecular weight of water
C                                                  (g/mole)
C     KH2O          Set    Real     Argument       ind gas-phase mass transfer
C                                                  coeff for H2O at 25 C (cm/s)
C     HLCMIN        Set    Real     Argument       min Henry's Law Const for
C                                                  volatile constituents
C                                                  (atm m^3/mole)
C     PS            Set    Real     Argument       particle density (g/cm3)
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/26/91     JWB  Added required comments and header for MEPAS QA plan  
C     01/28/94     KJC  Added lines so no stop is used ERRORSIG is set instead
C     08/01/95     JPM  Summing the total mass lost to volatization and
C                       groundwater for a wetland mass balance table written
C                       to the *.WLS file.
C     08/25/95     JPM  Using the Henry's Law Constant to make sure that
C                       non-volatile constituents are not allowed to
C                       volatilize.
C     09/18/96     JPM  Adjusting for decay at the source if the wetlands is
C                       the first medium and IDECAY = 1 or 3.  Also setting
C                       the degradation rate to zero if IDECAY = 3 (decay at
C                       source only) (RAD-P065).
C     10/01/96     JPM  Added HW and ISET to the subroutine call and the
C                       argument list for LIMITS (RAD-E049).
C     03/03/97 MVA/JPM  Changed volatilization to make it consistent with other
C                       MEPAS pathways. Implemented sediment-controlled 
C                       volatilization and volatilization controlled by
C                       contaminant water phase concentration (RAD-E053).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE WETLND (T2,C,NDS,UNITS,AL,GAMMA,T3,DTT,MED,M1,IT,KMED,
     1                   T33,GW_MASS,VOL_MAS,SW_MASS,HW,ISET)
C
C==== COMMON Definitions =====================================================
C
CKJC  Added for dll compilation
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'ERROR.WTN'
      INCLUDE 'INDEX2.WTN'
      INCLUDE 'INDEX3.WTN'
      INCLUDE 'INIT.WTN'
      INCLUDE 'WETLAN.WTN'
      INCLUDE 'EXTFLUX.WTN'
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION CMASS(2),NDS(MAXCON),T2(MAXCON),WK(MAXCON),SK(MAXCON),
     1          AL(MAXCON,12),MED(15),GW_MASS(MAXCON),VOL_MAS(MAXCON),
     1          SW_MASS(MAXCON),DEC_MAS(MAXCON)   
C
C==== Variable Declarations ==================================================
C
      DOUBLE PRECISION CASID
C
      Real KOV,KLIQ,KHI,KGAS,RI,VA,R,MWO2,KO2,MWH2O,KH2O,HLCMIN,PS,PT,
     1     DW, KWS
C
C==== DATA Statements ========================================================
C
C     None
C
      IF (IT .EQ. 1) THEN
	IF (M1 .EQ. 1) THEN
	  OPEN (UNIT=12,FILE=FILENM//'.CHM',STATUS='OLD',IOSTAT=IOS)
	  IF (IOS.NE.0) THEN
	    WRITE (6,36) FILENM
	    WRITE (11,36) FILENM
	    CALL MESSAGE ()
	    ERRORSIG = .TRUE.
	    GOTO 9999
	  ENDIF
	ENDIF
36      FORMAT (/
     + /2X,'ERROR --> CANNOT OPEN FILE ',A8,'.CHM',
     + /2X,'To properly simulate the wetlands medium, the Henry''s Law',
     + /2x,'Constant is required for each parent constituent.  These',
     + /2x,'are obtained from the *.CHM file, which could not be',
     + /2x,'opened.  This error will occur whenever the *.CHM file is',
     + /2x,'not located in the same directory as the waterborne input',
     + /2x,'file (*.WIN file).  It may also occur if you are running',
     + /2x,'RADCON directly and bypassing the shell.')
C
C*****************************************************************************
C     READ IN NECESSARY PARAMETERS
C        CONC0 IN G/ML OR CI/ML, CMW IN G/MOLE, WK IN 1/DAY, SK IN 1/DAY,
C        SKD IN ML/G, AND BCF IN ML/G
C*****************************************************************************
C
	READ(5,1) CONC0(M1),CMW(M1),SKD(M1),BCF(M1)
1       FORMAT (10X,6E10.3)
	READ (12,1001,IOSTAT=IOS)
1001    FORMAT (80X)
	READ (12,1002,IOSTAT=IOS) CASID,HLC(M1)
1002    FORMAT (A8,/,40X,E10.0,/,/,/,/,/,/,/)
cmap	DO WHILE (IOS.EQ.0.AND.CASID.NE.CASN1(M1,1))
	DO WHILE (IOS.EQ.0)
	  READ (12,1002,IOSTAT=IOS) CASID,HLC(M1)
	END DO
	IF (IOS.NE.0) THEN
	  WRITE (6,43) IV(M1,1),CASN1(M1,1),FILENM
	  WRITE (11,43) IV(M1,1),CASN1(M1,1),FILENM
	  CALL MESSAGE ()
	  ERRORSIG = .TRUE.
	  GOTO 9999
	ENDIF
43      FORMAT (/
     + /2x,'ERROR --> ',A8,' (CAS=',A8,') WAS NOT FOUND IN ',A8,'.CHM',
     + /2X,'To properly simulate the wetlands medium, the Henry''s Law',
     + /2x,'Constant (HLC) is required for each parent constituent.',
     + /2x,'The HLC for the above constituent was not found in the',
     + /2x,'*.CHM file.')
	IF (M1.EQ.NUMCON) THEN
	  CLOSE (12)
	ELSE
	  REWIND (12)
	ENDIF
	IF (IDECAY.EQ.3) THEN
	  WK(M1) = 1.0E-20
	  SK(M1) = 1.0E-20
	ELSE
	  WK(M1)=AL(M1,1)/365.25
	  SK(M1)=AL(M1,1)/365.25
	ENDIF
	WRITE(6,4) M1,IV(M1,1),CONC0(M1),CMW(M1),SKD(M1),BCF(M1)
        IF (M1.EQ.NUMCON) THEN
          WRITE(6,5)
          DO III = 1,NUMCON
            WRITE(6,7) III,IV(III,1),HLC(III)
          ENDDO
        ENDIF
4       FORMAT (I5,T9,A8,T22,1PE10.3,T37,E10.3,T50,E10.3,T67,E10.3)
5       FORMAT (//T2,'CONTAM',T11,'NAME',T24,'HENRY''S',/,
     1  T4,'#',T21,'LAW CONSTANT',/,T20,'((ATM M^3)/MOLE)',/)
7       FORMAT (I5,T9,A8,T22,1PE10.3)
	CMASS0=CONC0(M1)*VOLW
	UNITTS=1./UNITS
C ****************************************************************************
C    SET CONSTANTS NEEDED FOR VOLATILIZATION 
C ****************************************************************************
	HLCMIN=1.0E-07
	PS=2.65
	IF (CMW(M1).GT.0.0.AND.HLC(M1).GT.HLCMIN.AND.VTYPE.EQ.2) THEN
	  VA=0.0
	  R=8.20578e-05
	  MWO2=32.0
	  KO2=2.2E-03
	  MWH2O=18.0
	  KH2O=1.4
	ENDIF
      ENDIF
C*****************************************************************************
C     IF CONC0 IS NEGATIVE, THEN WILL USE THAT CONCENTRATION FOR ENTIRE 
C     SIMULATION PERIOD AND JUST HAVE TO CALCULATE FLUX OUT
C*****************************************************************************
      IF (CONC0(M1) .LT. 0.) GO TO 120
C*****************************************************************************
C     CALCULATE FLUX OUT AND CONCENTRATION IN WETLAND AT EACH TIME STEP
C*****************************************************************************
C
      IF (NPRINT .LT. 1) GO TO 20
      WRITE(6,8) M1,IV(M1,1),CONC0(M1),CMW(M1),WK(M1),SK(M1),
     1           SKD(M1),BCF(M1)
    8 FORMAT (/I5,7X,A8,5X,1PE10.3,5X,1PE10.3,3X,1PE10.3,
     1        6X,1PE10.3,5X,1PE10.3,7X,1PE10.3)
   20 CONTINUE
C
      A=0.
      DECAY=0.
C
C***********************************************************************
C     Compute the volatilization rate (per day) if a molecular weight 
C     greater than zero was read in from the *.WIN file, and the Henry's 
C     Law Constant for this constitiuent is greater than 1.0E-07 
C     atm^3/mole. Otherwise, set the volatilization rate equal to zero.
C     Volatilization is either sediment-controlled (VTYPE=1) or controlled
C     by the water phase conc. of the contaminants (VTYPE=2)
C***********************************************************************
C
      IF (CMW(M1).GT.0.0.AND.HLC(M1).GT.HLCMIN) THEN
	IF (VTYPE.EQ.1) THEN
	  PT=1.0-(SBULK/PS)
	  DW=0.00022*(CMW(M1)**(-0.6666667))
	    IF (BENTHIC.EQ.0) THEN
	      DW=DW*100.0
	    ENDIF
	  KWS=DW*(PT**1.33)/HS
	  VOLITA=KWS*AREA/VOLW
	  VOLITA=VOLITA*86400.
	ENDIF
	IF (VTYPE.EQ.2) THEN
	  PT=1.0-(SS/PS)
C          IF (M1.eq.2) write (*,*) 'HLC=',HLC(M1)
C          IF (M1.eq.2) write (*,*) 'PT=',PT
	  KLIQ=((MWO2/CMW(M1))**0.5)*(TAIR/298.)*KO2
C          IF (M1.eq.2) write (*,*) 'KLIQ=',KLIQ
	  KGAS=((MWH2O/CMW(M1))**0.335)*((TAIR/298.)**1.005)*KH2O
C          IF (M1.eq.2) write (*,*) 'KGAS=',KGAS
	  KHI=HLC(M1)/(R*TAIR)
C          IF (M1.eq.2) write (*,*) 'KHI=',KHI
	  KOV=1/((1/KLIQ)+(1/(KHI*KGAS)))
C          IF (M1.eq.2) write (*,*) 'KOV=',KOV
	  RI=1+((VA*KHI)+(SS*SKD(M1)))/PT
C          IF (M1.eq.2) write (*,*) 'RI=',RI
	  VOLITA=KOV/(PT*RI*DEPTH)
	  VOLITA=VOLITA*86400.
C          if (m1.eq.2) write (*,*) 'volita =',volita
	ENDIF
      ELSE
	VOLITA=0.
      ENDIF
C
      IF (NDS(M1) .LE. 1) DECAY=WK(M1)+
     1    SK(M1)*SEDM*SKD(M1)/VOLW
      OUT=(QOUT+QGW)/VOLW
      CLOSS=VOLITA+DECAY+OUT
c      if (m1.eq.2) write (*,*) 'closs=',closs
      FACTOR=1.0+SEDM*SKD(M1)/VOLW+BIOM*BCF(M1)/VOLW
      A=CLOSS/FACTOR
c      if (m1.eq.2) write (*,*) 'A=',a
C
      P9=A*365.25
      OUTFLOW = QOUT + QGW
      CALL LIMITS (GAMMA,T3,DTT,MED,T33,HW,ISET)
CKJC  Added for dll compilation
      IF (ERRORSIG) goto 9999
      CALL INTERP(T2(M1),PNT,M1)
C
C*****************************************************************************
C     IF THE WETLAND IS THE FIRST MEDIUM, THEN ADJUST THE INPUT FLUX FOR DECAY
C     AT THE SOURCE IF NECESSARY
C*****************************************************************************
C
      IF (KMED.EQ.1.AND.(IDECAY.EQ.1.OR.IDECAY.EQ.3)) THEN
	PNT = PNT * EXP(-AL(M1,1)*T2(M1))
      ENDIF
C
      EAT=EXPF(-A*DT*365.25)
c      if (m1.eq.2) write (*,*) 'eat=',eat
      CFLUXI=PNT/365.25
      IF (IT .EQ. 1) THEN
        CMASS(1)=CMASS0*EAT+CFLUXI*(1.-EAT)/CLOSS
        C=0.
        CM=CMASS(1)*UNITTS
      ELSE
C*****************************************************************************
C     CONVERT PNT TO CI/DAY OR G/DAY
C*****************************************************************************
        CFLUXI=PNT/365.25
        CMASS(2)=CMASS(1)*EAT+CFLUXI*(1.-EAT)/CLOSS
        IF (CMASS(2) .LT. 0.) CMASS(2)=0.
C*****************************************************************************
C     DETERMINE FLUX AND CONCENTRATION
C*****************************************************************************
        CM=CMASS(2)*UNITTS
      ENDIF
c      if (m1.eq.2) write (*,*) 'cmass(2)=',cmass(2)
C*****************************************************************************
C     CONCENTRATION IN G/ML OR CI/ML, FLUX OUT TO SURFACE WATER IN G/YR OR 
C     CI/YR, FLUX OUT TO GROUNDWATER IN G/YR OR CI/YR, NOTE THAT ALL OF THE 
C     VALUES ARE MULTIPLIED BY UNITTS
C*****************************************************************************
      C=CM/VOLW
      IF (KMED .NE. NMED) THEN
	IF (MED(KMED+1).EQ.5.OR.MED(KMED+1).EQ.7) THEN
	  C=CM*QOUT*365.25/VOLW
	ELSE
	  C=CM*QGW*365.25/VOLW
	END IF
      END IF
C
C***********************************************************************
C     Sum the mass lost to groundwater, surface water, volatization and decay
C***********************************************************************
C
      GW_MASS(M1) = GW_MASS(M1) + CM*UNITS * QGW * 365.25 / VOLW * DT
      SW_MASS(M1) = SW_MASS(M1) + CM*UNITS * QOUT * 365.25 / VOLW * DT
      VOL_MAS(M1) = VOL_MAS(M1) + CM*UNITS * VOLITA * 365.25 * DT
      DEC_MAS(M1) = DEC_MAS(M1) + CM*UNITS * DECAY * 365.25 * DT
C
      IF (NPRINT .LT. 1) GO TO 30
      WRITE(6,6) IT,M1,T2(M1),PNT,C,CM,QOUT,VOLW,CMASS(1),CMASS(2),
     1           UNITTS,EAT,CFLUXI,DECAY,OUT,CLOSS,FACTOR,VOLITA
    6 FORMAT (2I5,8E10.3/10X,8E10.3)
   30 CONTINUE
      IF (IT .GE. 2) CMASS(1)=CMASS(2)
  110 CONTINUE
  115 CONTINUE
      RETURN
  120 CONTINUE
C*****************************************************************************
C     USE CONSTANT CONCENTRATION; CONCENTRATION OUT IN G/ML AND FLUX OUT TO
C     SURFACE WATER OR GROUNDWATER IN G/YR.  NOTE THAT VALUES ARE MULTIPLIED 
C     BY UNITTS
C*****************************************************************************
      IF (KMED .NE. NMED) THEN
	IF (MED(KMED+1).EQ.5.OR.MED(KMED+1).EQ.7) THEN
	  C=-CONC0(M1)*QOUT*365.25*UNITTS
	ELSE
	  C=-CONC0(M1)*QGW*365.25*UNITTS
	END IF
      ELSE
	C=-CONC0(M1)*UNITTS
      END IF
C
  130 CONTINUE
  140 CONTINUE
 9999 RETURN
      END
