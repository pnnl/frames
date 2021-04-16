C     MEPAS RADCON: INPUTM.FOR            Version Date: 08-24-2006
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE INPUTM                                *
C                                                                            *
C  Subroutine INPUTM reads input data by medium type and performs several    *
C  computations depending on the media type and situation.  It is called     *
C  from the main program and calls subroutine CONC.                          *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99325                                      *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    08/24/06 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: RADCONPC
C     Calls to:  SUBROUTINE CONC
C     Common blocks referenced: INTER, TIME, MEDIA, INPUT, WETLAN, INDEX1,
C                               INDEX2, INDEX3, INDEX4, INDEX6, INDEX7,
C                               INDEX8, INDEX9, COM21
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- ------ ------  --------------  -------------------------------
C******************** FOR THE PARTIALLY SATURATED ZONE ***********************
C     A1        SET    REAL    COMMON - EXP   X-DIRECTION DISPERSIVITY
C     A2        SET    REAL    COMMON - EXP   SOIL TYPE COEFFICIENT
C     AN4       SET    REAL    COMMON - EXP   TOTAL POROSITY
C     AN5       SET    REAL    COMMON - EXP   FIELD CAPACITY
C     H1        SET    REAL    ARG - EXPORT   THICKNESS OF UNSATURATED LAYER
C     R2        SET    REAL    COMMON - EXP   BULK DENSITY
C     X2        SET    REAL    ARG - EXPORT   PERMEABILITY
C     U         SET    REAL    COMMON - EXP   LEACH RATE
C     VFLUX     SET    REAL    COMMON - EXP   VADOSE ZONE WATER FLUX
C*********************** FOR THE SATURATED ZONE ******************************
C     A1        SET    REAL    COMMON - EXP   X-DIRECTION DISPERSIVITY
C     A2        SET    REAL    COMMON - EXP   Y-DIRECTION DISPERSIVITY
C     A3        SET    REAL    COMMON - EXP   Z-DIRECTION DISPERSIVITY
C     AN4       SET    REAL    COMMON - EXP   TOTAL POROSITY
C     AN5       SET    REAL    COMMON - EXP   EFFECTIVE POROSITY
C     ANGLE     SET    REAL                   The angle in degrees between
C                                             the centerline and a line from
C                                             the center of the source directly
C                                             to the receptor.
C     DARCYV    SET    REAL    LOCAL          DARCY VELOCITY
C     HC        SET    REAL    LOCAL          LATERAL HYDRAULIC CONDUCTIVITY
C     HW        SET    REAL    ARG - EXPORT   DEPTH OF SITE IN WATERTABLE
C     H1        SET    REAL    ARG - EXPORT   AQUIFER THICKNESS
C     U         SET    REAL    COMMON - EXP   PORE-WATER VELOCITY
C     X2        SET    REAL                   x-component of distance to the
C                                             receptor downgradient from the
C                                             center of the source
C     Y2        SET    REAL                   y-component of distance to the
C                                             receptor - this is the length
C                                             of the perpendicular from the
C                                             centerline to the receptor
C     Z1        SET    REAL    COMMON - EXP   Depth below water table at which
C                                             concentrations will be computed
C************************* FOR RIVERINE ENVIRONMENT **************************
C     B1        SET    REAL    ARG - EXPORT   WIDTH OF RIVER
C     E1        SET    REAL    ARG - EXPORT   LONGITUDINAL DISPERSIVITY
C     E2        SET    REAL    ARG - EXPORT   LATERAL DISPERSIVITY
C     H1        SET    REAL    ARG - EXPORT   DEPTH OF RIVER
C     NUMREC    SET    INTEGER COMMON - EXP   # OF RIVER RECEPTOR LOCATIONS
C     QNEW      SET    REAL    COMMON - EXP   DOWNSTREAM DISCHARGE
C     U1        SET    REAL    ARG - EXPORT   RIVER FLOW VELOCITY
C     X3        SET    REAL    ARG - EXPORT   DISTANCE DOWNSTREAM FROM CENTER
C                                             OF LINE SOURCE
C     RIVX      SET    CHAR    ARG - EXPORT   LOCATION X-COORDINATE OF RIVER
C                                             RECEPTOR (METERS)
C     RIVY      SET    CHAR    ARG - EXPORT   LOCATION Y-COORDINATE OF RIVER
C                                             RECEPTOR (METERS)
C     RIVZ      SET    CHAR    ARG - EXPORT   DEPTH BELOW WATER OF RIVER
C                                             RECEPTOR (METERS)
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     03/24/89     JWB  Modification tosave last time for each media.
C                       Summation of PSZ and SZ was modified
C     11/25/91     JWB  Added required comments and header for MEPAS QA plan
C     09/28/92     JLS  Use Darcian Velocity to calculate moisture content
C     01/25/93     JWB  Modified to reflect CAP changes from JLS
C     02/10/93     JWB  Corrected the calcualtion of initial concentration CO
C                       for known flux (ISOURC=2), VLEACH needed to multipled
C                       by 365.0 to get it to correct units (cm/yr)
C     03/02/93     JWB  Multiple aquifer modification (PNTFRAC)
C     05/18/93     JLS  Cap infiltration cannot exceed K_sat of the clay layer
C     01/27/94     KJC  Added lines so no stop is used ERRORSIG is set instead
C     04/15/94     AdH  Added lines to read in Z1 (depth of calculated
C                       concentration below the water table) and write out
C                       to *.WLS file.  Also added a message explaining under
C                       what conditions an approximate solution is used.
C                       JPM/CS/GW
C     04/18/94     JPM  Modified to calculate vadose zone water flux (VFLUX)
C                       for near-field dilution
C     04/26/94     JPM  Changed the parameter that X2 is input as.  It used to
C                       be the distance directly from the center of the 
C                       source to the receptor.  It now represents only the
C                       x-component of the distance to the receptor.  JPM
C     05/27/94     JPM  Modified the SZ groundwater velocity.  The velocity
C                       read from the input file is now a darcy velocity
C                       (DARCYV).  It is then converted into the pore-water
C                       velocity (U).
C     06/02/94     JPM  Using the COMP_CO array for the code computed initial
C                       concentration.  Code computed initial concentrations
C                       are used only if the user does not specify an initial
C                       concentration.  If the user did not enter and the code
C                       did not compute an initial concentration, then CO
C                       is set to 1E+30.  When ISOURC = 3, IPOND = 2,
C                       ISTYPE = 1 or 2, and the user has entered a total
C                       concentration for the initial concentration, use the
C                       code computed initial concentration.  See note below.
C     06/02/94      CS  Estimated peak travel time for vadose zone and 
C                       saturated zone, and write out in WLS file.
C     06/03/94     JPM  Writing a warning message to the *.WLS file if the
C                       initial dissolved concentration at the source exceeds
C                       the solubility limit.
C     06/03/94     JPM  Changing the units for Kd in the table headings from
C                       CC/GM to ML/G for consistency.
C     06/07/94     JPM  After the initial concentration is computed by the
C                       code, if initial concentration is still <= 0.0, then
C                       use the solubility limit if it is greater than zero
C                       and less than 1E-15 (to avoid overflow errors).
C     06/15/94     JPM  A typo was found in the computation of estimated peak
C                       travel times for the partially saturated zone.
C     07/21/94     JPM  Changed the output description of X2 for the SZ to
C                       "Centerline Downgradient Distance from Center of
C                       Source"
C     09/01/94     JPM  Using equation 5.68 in Whelan et al., 1987 for the
C                       estimated time to peak and using equation 5.66 for
C                       the advective travel time.
C     09/12/94     JPM  Added a statement suggesting that a dissolved
C                       concentration be entered when the initial concentration
C                       remains a total concentration.  Also writing out a
C                       message notifying the user when the solubility limit
C                       is being used for the initial concentration.
C     09/14/94     JPM  Reconfigured the handling of initial concentration.
C                       If the code can compute the initial concentration, that
C                       value is used always.  Otherwise, the user entered CO
C                       is used.  If the user did not enter a CO, then the
C                       solubility limit is used if it is reasonable.  Else,
C                       CO is set real high to be non-limiting.  Simplified
C                       the logic in which CO is computed by the code.
C     09/22/94     JPM  Code was crashing on the computation of advective
C                       travel time when u=0.
C     07/28/95     JPM  Removed the message referring to Yeh (1981) and the
C                       approximate saturated zone solution, that was written
C                       to the *.WLS file.
C     07/31/95     JPM  Writing warning messages to the *.WRN file only.
C                       Writing error messages to the screen
C                       and *.WRN file in addition to the *.WLS file.
C                       Also, added a warning message to alert the user when
C                       the cumulative time to peak in a saturated zone is
C                       greater than TFINAL.
C     08/01/95     JPM  Reformatting table headers for the wetland medium.
C     10/27/95     JPM  Computing the initial source concentration when
C                       ISCONF = 5 (yz area source).
C     05/06/96     JPM  Made multiple saturated zone changes as follows:
C                       1) writing to *.WLS new medium description for a SZ
C                       at another SZ, 2) only writing out width (B1), length
C                       (AL1), and penetration depth (HW) if ISCONF <> 5.
C     05/08/96     JPM  Removed the lines that set the y- and z-dispersivities
C                       (A2 and A3, respectively) equal to 1/10 the
C                       x-dispersivity (A1) when A2 and A3 are <= zero.  This
C                       allows us to enter zero dispersivities and thus to
C                       approximate plug flow (with molecular diffusion)
C                       (RAD-E047).
C     10/15/96     JPM  Near-field dilution changes:  1) added ZTOP and ZBOT
C                       to the subroutine argument list, 2) setting ZTOP and
C                       ZBOT for ISTYPE = 1 conditions based on the water flux
C                       from the PSZ, and 3) redefining the source width for
C                       the saturated zone if necessary to accommodate the
C                       water flux from the vadose zone (RAD-E052).
C     10/18/96     JPM  Made changes so medium type 2 (MED=2) is handled the
C                       same as medium type 1.  MED=2 indicates a
C                       concentration computation in the vadose zone.  Change
C                       being made for the Phase 1 MEPAS waterborne executable
C                       for the framework (PH1-E001).
C     10/30/96     JPM  Made changes allowing MED = 6 to indicate flux
C                       computations in a river (PH1-E003).
C     11/11/96     JPM  Changed units from Ci to pCi (PH1-E006).
C     04/15/97     JPM  Incorporated changes made for the new wetlands
C                       volatilization technique (RAD-E053).  New input
C                       parameters for wetland volatilization were added:
C                       SBULK, HS, TAIR, SS, DEPTH, AREA, VTYPE, and BENTHIC.
C                       Removed unit conversions on VOLW, QOUT, QGW, SEDM, and
C                       BIOM - we were converting units here and then again
C                       in subroutine WETLND.  The units read in are now used
C                       in the wetland computations (PH1-E007).
C     04/17/97     JPM  Calling subroutine FLXTABLE to write out the new
C                       fluxes and TLIFE's after the call to subroutine MASS.
C                       MASS is called after fluxes are recomputed if the
C                       source penetrates the aquifer (PH1-P004).
C     05/01/97     JPM  Multiplying TOTMAS by PNTFRAC (PH1-P011).
C     05/15/98     PDM  Commented out the include statement for CAP.WTN and
C                       the statements that computed the third infiltration
C                       rate from the first two (RAD-E054).
C     06/09/98     JPM  Making IV and CASN1 character variable types
C                       (PH1-E016).
C     06/11/98     JPM  Writing the lateral dispersion coefficient (E2) for
C                       surface water to the WLS file (PH1-E018).
C     06/16/98     JPM  Writing the decay constants (AL) to the WLS file
C                       (PH1-E019).
C     09/29/04     JPM  Made changes to prevent a math error when the
C                       infiltration rate in the vadose zone is zero.  Also,
C                       aborting the run if the water flux in the vadose zone
C                       or into the aquifer is zero, but the mass flux is
C                       greater than zero (PH1-P026).
C     08/24/06     JPM  Reading the river receptor location coordinates for
C                       MED(KMED) = 5 and passing them back to the main
C                       program for output to the WCF file (PH1-E021).
C     
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE INPUTM (X2,ND,NDS,AL,AL1,B1,IV,CASN1,H1,U1,E1,E2,X3,
     1                   CO,HW,VLEACH,WUMC,WUBD,UNITS,tlife,TP_TOTAL,
     1                   ZTOP,ZBOT,TOTMAS,RIVX,RIVY,RIVZ)
C
C---- COMMON Block Definitions -----------------------------------------------
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
      INCLUDE 'INDEX8.WTN'
      INCLUDE 'INDEX9.WTN'
      INCLUDE 'CTIME.WTN'
      INCLUDE 'INPUT.WTN'
      INCLUDE 'INTER.WTN'
      INCLUDE 'MEDIA.WTN'
      INCLUDE 'WETLAN.WTN'
      INCLUDE 'VLEA1.WTN'
c pdm INCLUDE 'CAP.WTN'
      INCLUDE 'welldist.WTN'
      INCLUDE 'DILUTION.WTN'
      INCLUDE 'COM21.WTN'
      INCLUDE 'extflux.wtn'
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION NDS(MAXCON),AL(MAXCON,12),IV(MAXCON,12),CO(MAXCON)
      DIMENSION CASN1(MAXCON,12),tlife(MAXCON),TP_TOTAL(MAXCON)
      DIMENSION TOTMAS(MAXCON)
C
C==== Variable Declarations ==================================================
C
      CHARACTER*18 IV,CASN1
      CHARACTER*15 RIVX,RIVY,RIVZ
C
C==== DATA Statements ========================================================
C
C     None
C
      HW=0.
      IF (KMED.EQ.1) VFLUX=0.
C*****************************************************************************
C     WRITE STATEMENTS DESCRIBING THE MEDIA CHOSEN
C*****************************************************************************
      GO TO (551,551,553,554,555,555,558), MED(KMED)
  551 WRITE(6,561)
      WRITE(11,561)
      GO TO 557
  552 WRITE(6,562)
      WRITE(11,562)
      GO TO 557
  553 IF (MED(KMED+1).EQ.4.OR.MED(KMED+1).EQ.3) THEN
        WRITE (6,560)
        WRITE (11,560)
      ELSE
        WRITE(6,563)
        WRITE(11,563)
      ENDIF
      GO TO 557
  554 IF (MED(KMED+1).EQ.4.OR.MED(KMED+1).EQ.3) THEN
        WRITE (6,560)
        WRITE (11,560)
      ELSE
        WRITE(6,564)
        WRITE(11,564)
      ENDIF
      GO TO 557
  555 WRITE(6,565)
      WRITE(11,565)
      GO TO 557
  556 WRITE(6,566)
      WRITE(11,566)
      GO TO 557
  558 WRITE(6,559)
      WRITE(11,559)
  561 FORMAT (' PARTIALLY SATURATED ZONE'//)
  562 FORMAT (' SYNTHETIC LINER (CURRENTLY NOT ADDRESSED)'//)
  563 FORMAT (' SATURATED ZONE AT A WELL'//)
  560 FORMAT (' SATURATED ZONE'//)
  564 FORMAT (' SATURATED ZONE AT A RIVER'//)
  565 FORMAT (' SURFACE WATER ENVIRONMENT'//)
  566 FORMAT (' HOLDING POND'//)
  559 FORMAT (' WETLAND ENVIRONMENT'//)
C*****************************************************************************
C     READ DATA DEPENDING ON MEDIA TYPE
C*****************************************************************************
  557 GO TO (10,10,30,30,40,40,80), MED(KMED)
C*****************************************************************************
C     FOR THE PARTIALLY SATURATED ZONE, SUBROUTINE IMPUTM CALCULATES A
C     MOISTURE CONTENT BASED ON EQUATION 5.11 FROM WHELAN ET AL. 1987.
C     THE LEACH RATE IS THEN COMPARED WITH THE SOIL TRANSMISSION RATE,
C     AND THE APPROPRIATE RATE IS USED IN FUTURE CALCULATIONS.  IF THE
C     UNSATURATED ZONE IS THE FIRST MEDIUM TYPE, SUBROUTINE INPUTM ALSO
C     CALCULATES THE INITIAL CONCENTRATION AT THE SOURCE.
C*****************************************************************************
   10 READ(5,1) H1,R2,AN4,AN5,A1,X2,A2
      IF( Kmed.EQ.1 ) THEN
        IF( ISOURC.EQ.1 ) THEN
          IF( IPOND.NE.1 ) VLEACH=VLECH1
        ENDIF
        IF( ISOURC.EQ.3 ) VLEACH=VLECH1+VLEACH
      ENDIF
      U=VLEACH
CKJC  VLEACH is already in cm/dy.
CKJC  IF (KMED .NE. 1) U=U/365.25
      AN4=AN4/100.
      AN5=AN5/100.
C*****************************************************************************
C JLS If we are using the cap (incr=2) and the darcy velocity exceeds the
C   saturated hydrolic conductivity of the cap, set the infiltration to the
C   hydrolic conductivity of the clay layer.
C*****************************************************************************
c JLS      if( (incr .eq. 2) .AND. (vleach .gt. clay_ks) ) vleach = clay_ks
c jls  the infiltration is adjusted in the source term code.
C*****************************************************************************
C Save vleach for both runs to calculate the vleach after cap failure JLS
C only coded to interpolate between the first two values (ie. cap)
C*****************************************************************************
c pdm flxdns(incr) = vleach
c pdm if(incr .eq. 3) vleach = ( flxdns(1) - flxdns(2) )*fractf +
c pdm+                              flxdns(2)
CKJC      WRITE(6,202) H1,A1,R2,AN4,AN5,X2,U,A2
      WRITE(6,202) H1,A1,R2,AN4,AN5,X2,VLEACH,A2
  202 FORMAT(/,
     1' Thickness of Unsaturated Layer (H1) = ',1PE10.2,' cm'/
     2' X-Direction Dispersivity (A1)       = ',E10.2,' cm'/
     3' Bulk Density (R2)                   = ',E10.2,' g/cm**3'/
     4' Total Porosity (AN4)                = ',E10.2,' (fraction)',/
     5' Field Capacity (AN5)                = ',E10.2,' (fraction)',/
     6' Permeability (X2)                   = ',E10.2,' cm/day'/
     7' Darcy Velocity (VLEACH)             = ',E10.2,' cm/day'/
     8' Soil-Type Coefficient (A2)          = ',E10.2//)
1     FORMAT (8E10.3)
C*****************************************************************************
C     Write out a warning message if the infiltration rate is equal to zero
C     (PH1-P026).
C*****************************************************************************
      IF (VLEACH.EQ.0.0) THEN
        WRITE (11,783) VLEACH
783     FORMAT (
     1  /2X,'WARNING --> VLEACH = ',1PE10.3,
     2  /2x,'The infiltration rate (VLEACH) is equal to zero.',
     3  /2x,'This means that computed fluxes/concentrations will',
     4  /2x,'be equal to zero.')
      ENDIF
C*****************************************************************************
C THIS SECTION IS BASED ON:
C     CLAPP, R.B., AND G.M. HORNBERGER. 1978. "EMPIRICAL EQUATIONS FOR
C     SOME SOIL HYDRAULIC PROPERTIES". WATER RESOURCES RESEARCH. VOL 14,
C     NO. 4, AUGUST, PP. 601-604.
C THIS SECTION HAS BEEN UPDATED TO INCLUDE CHANGES IN MOISTURE
C CONTENT (X2=SAT HYD CON IN CM/DAY, U=DEEP-DRAINAGE, LEACH, OR
C PERCOLATION RATE) IN CM/DAY (NOTE THAT THE HYDRAULIC CONDUCTIVITY (KU)
C EQUALS THE DEEP-DRAINAGE RATE TIMES THE MOISTURE CONTENT (I.E.,
C KU=U*RAN), AN4=TOTAL POROSITY IN %, AN5=FIELD CAPACITY IN % OR NEW
C MOISTURE CONTENT IN TENTHS, RAN=CALCULATED MOISTURE CONTENT, A2=SOIL
C TYPE COEFFICIENT (SAND=4.05, CLAY=11.04)), A3=MAXIMUM PORE WATER
C VELOCITY (I.E., A3=KS/N)
C
C THETA=N*(KU/KS)**(1/(2*B+3))  ===  RAN=AN4*(U*RAN/X2)**(1./(2*A2+3))
C*****************************************************************************
      A2=1./(2.*A2+3.)
C******************** Darcy velocity changes *************************
      if( vleach.le.x2 ) ran = (an4*(vleach/x2)**a2)
      if( ran.gt.an4 .or. vleach.gt.x2 ) then
        ran = an4
        vleach = x2
      endif
      if( ran.lt.an5 ) ran = an5
      u = vleach/ran
      x2 = h1
      an5 = ran
      a2 = 0.0

      WRITE(6,203) AN5*100.0,U
  203 FORMAT(/,' Moisture Content (AN5)   = ',F6.2,' %',/
     1         ' Pore water velocity (U)  = ',1p,E10.2,' cm/day',/)

      IF( U. LE. 0.0 ) NTIMES = 0
C
C***********************************************************************
C     Compute the annual water flux from the vadose zone if an
C     area source is used (ISCONF = 4):
C
C     Water flux = Infiltration rate * field capacity * waste site area
C
C***********************************************************************
C
      IF (ISCONF.EQ.4) VFLUX = U*AN5*AL1*B1*365.25
      GO TO 70
C*****************************************************************************
C     FOR THE SATURATED ZONE, INPUTM GENERATES RETARDATION FACTORS
C     BASED ON EQUATION 5.5 FROM WHELAN ET AL. 1987.  IF THE
C     SATURATED ZONE IS THE FIRST MEDIUM TYPE, SUBROUTINE INPUTM ALSO
C     CALCULATES THE INITIAL CONCENTRATION AT THE SOURCE.  THE CALCULATIONS
C     ARE DEPENDENT ON THE SOURCE CONFIGURATION (HW = 0, OR HW > O).
C*****************************************************************************
   30 READ(5,1) DARCYV,H1,R2,AN4,AN5,A1,A2,A3,X2,HW,mdist,y2,Z1
      IF (Z1 .LE. 0.) Z1=0.
C*****************************************************************************
C     CONVERT X2 AND ANGLE INTO X-COMPONENT AND Y-COMPONENT OF DISTANCE FROM
C     SOURCE TO RECEPTOR
C*****************************************************************************
      if (y2 .eq. 0.) then
        ANGLE=0.
      else
        radian = atan(y2/x2)
        ANGLE  = radian /(3.141593/180.0)
      endif
C      write(*,134) x2,y2,radian,angle
C134   format(' INPUTM --> X2,Y2,RADIAN,ANGLE = ',1p,4e10.2)
      AN4=AN4/100.
      AN5=AN5/100.
      if( kmed.eq.1 ) then
        if( isourc.eq.1 .and. ipond.ne.1) vleach = vlech1
        if( isourc.eq.3 ) vleach = vlech1 + vleach
      endif
C*****************************************************************************
C     SUBROUTINE MOUND WAS REMOVED FROM CODE UNTIL IT CAN BE FULLY TESTS
C             CALL MOUND (VLEACH,AL1,B1,AN5,X2,U,HC,H1)
C*****************************************************************************
      U = DARCYV / AN5
C*****************************************************************************
C     Compute the dimensions of the volume source for dilution
C*****************************************************************************
      if (istype.eq.1.and.isconf.eq.4) then
        isconf = 6
        ztop = 0.0
        zbot = vflux/(darcyv*365.25*b1)
        if (zbot.gt.h1) then
          write (11,363)
363       format (2x/
     +    'WARNING -->  For the saturated zone, the source width is',/
     +    '             being increased to accommodate the water flux',/
     +    '             from the vadose zone.'//)
          ZBOT = h1
          b1 = vflux/(darcyv*365.25*h1)
        endif
        write (6,362) b1,ztop,zbot
362     format (2x/
     +  ' To account for the water flux from the vadose zone, a'/,
     +  ' volume source is being used for the saturated zone'//,
     +  ' Dimensions:'//,
     +  ' Width (B1)                 = ',1pe10.3,' cm'/,
     +  ' Upper z-coordinate (ZTOP)  = ',e10.3,' cm'/,
     +  ' Lower z-coordiante (ZBOT)  = ',e10.3,' cm')
      endif
C*****************************************************************************
C     COMPUTATIONS AND PRINT STATEMENTS FOR BOTH UNSATURATED AND SATURATED
C     ZONE COMPUTATIONS, CONTENT DEPENDS ON IF THIS IS THE FIRST MEDIA
C*****************************************************************************
   70 READ(5,1) (AK(I), I=1,NUMCON)
C*****************************************************************************
C     SATURATED ZONE RAN WAS REDEFINED TO 1.0 TO CORRESPOND TO OTHER CODES
C     PREVIOUSLY --> RAN=AN4/AN5
C*****************************************************************************
      RAN = 1.0
      B3=R2/AN5
      IF (KMED .EQ. 1) THEN
        IF (MED(KMED) .LE. 4) THEN
          WRITE(6,187)
  187   FORMAT (//,T55,'CENTERLINE',T69,'CENTERLINE',/T55,'ESTIMATED',
     1  T69,'ADVECTIVE',/T2,'NO',T6,'PARENT',T17,'HALFLIFE',T32,'KD',
     2  T40,'RETARDATION',T54,'TIME TO PEAK',T68,'TRAVEL TIME',
     3  /T18,'YEARS',T31,'ML/G',T42,'FACTOR',T57,'YEARS',T71,'YEARS'/)
        ELSE             
          WRITE(6,188)
  188   FORMAT (//,T2,'NO',T6,'PARENT',T19,'HALFLIFE',T34,
     1  'KD',T44,'RETARDATION'/T20,'YEARS',T33,'ML/G',T46,
     2  'FACTOR'/)
        ENDIF
      ENDIF
      IF (KMED .NE. 1) THEN
        IF (MED(KMED) .LE. 4) THEN
          WRITE(6,107)
  107   FORMAT (//,T55,'CENTERLINE',T69,'CENTERLINE',/T55,'ESTIMATED',
     1  T69,'ADVECTIVE',/T2,'NO',T6,'PARENT',T16,'DECAY RATE',T32,'KD',
     2  T40,'RETARDATION',T54,'TIME TO PEAK',T68,'TRAVEL TIME',
     3  /T18,'YEARS',T31,'ML/G',T42,'FACTOR',T57,'YEARS',T71,'YEARS'/)
        ELSE
          WRITE(6,108)
  108   FORMAT (//,T2,'NO',T6,'PARENT',T18,'DECAY RATE',T34,
     1  'KD',T44,'RETARDATION'/T19,'1/YEARS',T33,'ML/G',T46,
     2  'FACTOR'/)
        ENDIF
      ENDIF
C*****************************************************************************
C     GENERATE RETARDATION FACTORS AND COMPUTE TRAVEL TIMES
C*****************************************************************************
C
C     If this is a partially saturated zone, the travel distance is the
C     thickness of the layer (H1), else the travel distance is the centerline
C     downgradient distance (X2).
C
      IF (MED(KMED).LE.2) THEN
        XDIST = H1
      ELSE
        XDIST = X2
      ENDIF
      DO I=1,NUMCON
C
C****************************************************************************
C     For the vadose zone (MED(KMED)<=2), check to see if the infiltration
C     rate is zero (VLEACH=0.0) and the mass entering this layer is not zero
C     (CINVEN(I)>1E-25).  For the saturated zone (MED(KMED)>2), check to see
C     if the infiltration rate from the vadose zone is zero (ZTOP=ZBOT) and
C     the mass entering the aquifer is not zero (CINVEN(I)>1E-25).  Both of
C     these cases are invalid - for mass to enter the medium, water must
C     enter also - so abort the run.  Write a warning message for the
C     saturated zone if the water flux from the vadose zone is zero and
C     the mass entering the aquifer is also zero.  PH1-P026
C****************************************************************************
C
      IF (MED(KMED).LE.2.AND.VLEACH.EQ.0.0.AND.CINVEN(I).GT.1.0E-25)
     +THEN
        WRITE (6,491) IV(I,1),VLEACH,CINVEN(I)
        WRITE (11,491) IV(I,1),VLEACH,CINVEN(I)
        ERRORSIG = .TRUE.
      ENDIF
      IF (MED(KMED).GT.2.AND.ZBOT-ZTOP.EQ.0.0.AND.CINVEN(I).GT.1.0E-25)
     +THEN
        WRITE (6,492) IV(I,1),ZTOP,ZBOT,CINVEN(I)
        WRITE (11,492) IV(I,1),ZTOP,ZBOT,CINVEN(I)
        ERRORSIG = .TRUE.
      ELSE IF (MED(KMED).GT.2.AND.ZBOT-ZTOP.EQ.0.0) THEN
        WRITE (11,493) ZTOP,ZBOT
      ENDIF
491     format(
     C   /2x,'ERROR --> ',A18,' VLEACH = ',1PE10.3,' CINVEN = ',E10.3,
     C   /2x,'The infiltration rate (VLEACH) is zero, but the mass',
     C   /2x,'of contaminant entering this layer (CINVEN) is not zero.')
492     format(
     C   /2x,'ERROR --> ',A18,' ZTOP = ',1PE10.3,' ZBOT = ',E10.3,
     C   /2X,'          CINVEN = ',E10.3,
     C   /2x,'The infiltration rate from the vadose zone is zero (i.e.',
     C   /2x,'ZTOP=ZBOT), but the mass of contaminant entering the',
     C   /2x,'aquifer (CINVEN) is not zero.')
493     FORMAT (
     1  /2X,'WARNING --> ZTOP = ',1PE10.3,' ZBOT = ',E10.3
     2  /2x,'The infiltration rate from the vadose zone is equal to'
     3  /2x,'zero causing the source thickness in the aquifer to be'
     4  /2x,'zero (i.e., ZTOP = ZBOT).  This means that computed'
     5  /2x,'fluxes/concentrations will be equal to zero.')        
C
C     Compute the retardation factor (A)
C
        A(I)=1.0+B3*AK(I)
C
C     Compute the dispersion coefficient in the x-direction
C
        D_STAR = (A1*U*365.25+190)/A(I)
C
C     Consider decay only if decay is occurring in the environment (IDECAY
C     = 1 or 2).  If this is the first medium, then the AL array is
C     halflives, else it is decay constants.  Need decay constants to
C     compute travel time.
C
        IF (IDECAY.LT.3) THEN
          IF (KMED.EQ.1) THEN
            ALAM = 0.6931472/AL(I,1)
          ELSE
            ALAM = AL(I,1)
          ENDIF
        ELSE
          ALAM = 1.0E-21
        ENDIF
C
C     If this is a PSZ or an SZ, compute the peak travel time (PTTIME) using
C     equation 5.68 in Whelan, et al., 1987, and compute the advective travel
C     time (ATTIME) using equation 5.66.  Else, set PTTIME equal to -99.9
C
        IF (MED(KMED).LE.4) THEN
          PTTIME=(SQRT((U*365.25*XDIST/A(I))**2+4*ALAM*D_STAR*XDIST**2+
     C    D_STAR**2)-D_STAR)/(4*ALAM*D_STAR+(U*365.25/A(I))**2)
          TP_TOTAL(I) = TP_TOTAL(I) + PTTIME
          IF (U.EQ.0.0) THEN
            ATTIME = -9999.
          ELSE
            ATTIME=XDIST*A(I)/(U*365.25)
          ENDIF
          IF (MED(KMED).EQ.3.OR.MED(KMED).EQ.4) THEN
            IF ((TFINAL.NE.10001.0).AND.(TP_TOTAL(I).GT.TFINAL)) THEN
              WRITE (11,496) TP_TOTAL(I),TFINAL,IV(I,1)
496         FORMAT (
     1      /2X,'WARNING --> TP_TOTAL = ',1PE10.3,' TFINAL = ',1E10.3,
     2      /2x,'The cumulative time to peak (i.e., the sum of the',
     3      /2x,'times to peak for all previous media and the current',
     4      /2x,'medium) (TP_TOTAL) for ',A18,' is greater than TFINAL')
            ENDIF
          ENDIF
        ELSE
          PTTIME=-99.9
        ENDIF
C
        IF (PTTIME .LT. 0.0) THEN
          WRITE(6, 109) I,IV(I,1),AL(I,1),AK(I),A(I)
        ELSE
          WRITE(6,109) I,IV(I,1),AL(I,1),AK(I),A(I),PTTIME,ATTIME
        END IF
      END DO
      IF (ERRORSIG) THEN
        CALL MESSAGE ()
        GOTO 9999
      ENDIF
  109 FORMAT (T2,I2,T6,A18,T16,1P1E10.3,T26,2E12.3,T50,2E14.3)
      IF (MED(KMED).LE.2) GO TO 226
      WRITE(6,200) DARCYV,U,H1,A1,A2,A3,R2,AN4,AN5,X2,y2,angle,Z1
      IF (ISCONF.NE.5) WRITE(6,201) AL1,B1,HW
  200 FORMAT(/,
     1' Darcy Velocity (DARCYV)                         = ',1PE10.2,
     2        ' cm/day'/
     3' Pore-water Velocity (U)                         = ',1PE10.2,
     4        ' cm/day'/
     5' Aquifer Thickness (H1)                          = ',E10.2,' cm'/
     6' X-Direction Dispersivity (A1)                   = ',E10.2,' cm'/
     7' Y-Direction Dispersivity (A2)                   = ',E10.2,' cm'/
     8' Z-Direction Dispersivity (A3)                   = ',E10.2,' cm'/
     9' Bulk Density (R2)                               = ',E10.2,
     *        ' g/cm**3'/
     1' Total Porosity (AN4)                            = ',E10.2,
     2        ' (fraction)',/
     3' Effective Porosity (AN5)                        = ',E10.2,
     4        ' (fraction)',/
     8' Centerline Downgradient Distance from'/
     9' Center of Source (X2)                           = ',E10.2,' cm'/
     *' Perpendicular Distance Off Flow Centerline (Y2) = ',E10.2,' cm'/
     1' Angle Between Flow Direction and Receptor'/
     2' Direction (ANGLE)                               = ',E10.2,
     3' deg',/
     4' Depth Below Watertable of Calculated',/
     5' Concentration at Receptor (Z1)                  = ',E10.2,' cm') 
  201 FORMAT(
     1' Length of Burial Area (AL1)                     = ',1PE10.2,
     2  ' cm'/
     3' Width of Burial Area (B1)                       = ',E10.2,' cm'/
     4' Depth of Release Unit Below Water Table (HW)    = ',E10.2,' cm')
C
C  FOR THE DEGENERATE CASE OF U .LE. 0.0
C  SET NTIMES = 0 AS A FLAG
226   IF( U. LE. 0.0 ) NTIMES = 0
      U=U*365.25
      if( kmed.eq.1 ) then
        if( hw.gt.0.0 .and. isconf.lt.4 ) then
          write(6,663) isconf,hw
          write(11,663) isconf,hw
          CALL MESSAGE ()
663       format(
     C    /2x,'ERROR --> ISCONF = ',I5,' HW = ',1PE10.2,
     C    /2x,'If HW is greater than zero, ISCONF must equal 4')
CKJC  Added for dll compilation
          ERRORSIG=.TRUE.
          GOTO 9999
CKJC          stop ' INPUTM --> ISCONF is not 4 when HW > 0.0'
        endif
      endif
      goto 50
C*****************************************************************************
C     FOR SURFACE WATER COMPUTATIONS, THE BASIC INPUT PARAMETERS ARE CHECKED
C     AND LATERAL DISPERSION (E2) IS CALCULATED BASED ON EQUATION 6.8
C     FROM WHELAN ET AL. 1987.                                                
C*****************************************************************************
   40 CONTINUE
      READ(5,11) NUMREC,U1,H1,B1,X3,QNEW,RIVX,RIVY,RIVZ
   11 FORMAT (I5,5E10.3/3A15)
c     June 27, 1991/next two lines moved up to before print/jgd
      QNEW=QNEW*365.25
      U1=U1*365.25
      E1=0.25*U1*B1**2./H1
      E2=0.06*H1*U1
      WRITE(6,206) NUMREC,U1,H1,B1,QNEW,X3,E2
  206 FORMAT(/,
     1' Number of River Receptor Locations (NUMREC)    = ',I10,/
     2' Flow Velocity (U1)                             = ',1PE10.2,
     3         ' cm/year'/
     4' Depth of Flow (H1)                             = ',E10.2,' cm'/
     5' Width of River (B1)                            = ',E10.2,' cm'/
     6' Downstream Discharge at First Receptor (QNEW)  = ',E10.2,
     7 ' cm**3/year'/
     8' Distance Downstream from Center of Source (X3) = ',E10.2,
     9 ' cm'/,
     1' Lateral Dispersion Coefficient (E2)            = ',E10.2,
     2 ' cm**2/year'//)
      WRITE(6,207) RIVX(1:LEN_TRIM(RIVX)),RIVY(1:LEN_TRIM(RIVY)),
     +             RIVZ(1:LEN_TRIM(RIVZ))
  207 FORMAT(/,
     1' Receptor Location Easting (RIVX)               = ',A,' m'/
     2' Receptor Location Northing (RIVY)              = ',A,' m'/
     3' Receptor Location Depth Below Water (RIVZ)     = ',A,' m'/)
      IF (QNEW .LE. 0.) QNEW=U1*B1*H1
      IF (NUMREC .LE. 0) NUMREC=1
      DO 41 I=1,NUMCON
   41 A(I)=1.
      RAN=1.
      B3=0.
      AN4=1.
      AN5=1.
      R2=0.
      GO TO 50
C*****************************************************************************
C     READ IN THE WETLAND DATA
C*****************************************************************************
   80 CONTINUE
      READ(5,82) VOLW,QOUT,QGW,SEDM,BIOM,SBULK,AL1,B1
   82 FORMAT (8E10.3)
      READ(5,89) VTYPE
   89 FORMAT (I5)
      VLEACH=QGW/(B1*AL1)
      AREA = AL1*B1
      WRITE(6,83) VOLW,QOUT,QGW,SEDM,BIOM,SBULK,AL1,B1,VLEACH
   83 FORMAT(/,1P
     +' Water column volume (VOLW)                = ',E10.2,' cm**3'/
     +' Surface water discharge rate (QOUT)       = ',E10.2,
     +      ' cm**3/day'/
     +' Groundwater discharge rate (QGW)          = ',E10.2,
     +      ' cm**3/day'/
     +' Sediment Mass (SEDM)                      = ',E10.2,' g'/
     +' Biomass (BIOM)                            = ',E10.2,' g'/
     +' Soil Bulk Density (SBULK)                 = ',E10.2, 
     +      ' g/cm**3'/
     +' Length of wetland water column area (AL1) = ',E10.2,' cm'/
     +' Width of wetland water column area (B1)   = ',E10.2,' cm'/
     +' Infiltration rate to groundwater          = ',E10.2,
     +      ' cm/day'/)
      IF (VTYPE.EQ.1) THEN
        READ(5,85) HS,BENTHIC
   85 FORMAT (E10.3,I5)
        WRITE(6,86) HS,BENTHIC
   86 FORMAT (/,
     +' Volatilization is sediment-controlled (VTYPE=1)'//
     +' Depth of contaminated sediment (HS)       = ',1PE10.2,' cm'/ 
     +' Benthic organisms (YES=0, NO=1)           = ',I5/)
      END IF  
      IF (VTYPE.EQ.2) THEN
        READ(5,87) TAIR,SS,DEPTH
   87 FORMAT (3E10.3)
        WRITE(6,88) TAIR,SS,DEPTH
   88 FORMAT (/,
     1' Volatilization is water-phase controlled (VTYPE=2)'/,/
     2' Annual average air temperature (TAIR)  = ',1PE10.2,' K'/
     3' Suspended sediment concentration (SS)  = ',E10.2,' g/cm**3'/
     4' Depth of wetland water column (DEPTH)  = ',E10.2,' cm'/)
      END IF
      DO 884 I=1,NUMCON
  884 A(I)=1.
      RAN=1.
      B3=0.
      AN4=1.
      AN5=1.
      R2=0.
C*****************************************************************************
C COMPUTATIONS BY CONTAMINANT FOR ALL MEDIA TYPES
C*****************************************************************************
   50 CONTINUE
      IF (KMED .NE. 1) GO TO 319
C*****************************************************************************
C    CALCULATE INITIAL GROUNDWATER CONCENTRATIONS AT THE SOURCE FOR VARIOUS   
C    NON-POINT SOURCE CONDITIONS (ISCONF <> 1)
C*****************************************************************************
      IF (ISCONF .GT. 1) THEN
        IF ((ISTYPE.EQ.1.AND.(ISOURC.EQ.2.OR.ISOURC.EQ.3))
     +  .OR.(ISOURC.EQ.3.AND.ISTYPE.EQ.2.AND.IHW.EQ.0)) THEN
          DO JJ=1,NUMCON
            COSAVE=0.0
            DO IJ=1,NUM(JJ)
              IF (VLEACH.EQ.0.0) THEN
                COSAVE = -9999.
              ELSE
                CO(JJ)=PNT(IJ,JJ)/AL1/B1/(VLEACH*365.0)
                IF (CO(JJ) .GT. COSAVE) COSAVE=CO(JJ)
              ENDIF
            ENDDO
            CO(JJ)=COSAVE
            ICONC(JJ)=3
          ENDDO
        ENDIF
      ENDIF
  621 FORMAT (/43x,'Surface',/,
     1        ' Constituent   CAS ID  Half-Life  Initial',
     2        ' Equilibrium  Solubility  Constituent',/,23x,'(Years)',
     3        '      Conc.    Coeff.      limit     Inventory'/,
     4        44x,'(ml/g)      (g/ml)    (g OR pCi)'/)
C****************************************************************************
C     RECALCULATE THE CONTAMINANT MASS FLUX FOR WASTE UNITS THAT ARE         
C     PARTIALLY BELOW THE WATER TABLE SURFACE IN THE GROUNDWATER ENVIRONMENT:
C     ISTYPE=2, ISOURC=1, IPOND=2, ISCONF=4, AND IHW>0.
C****************************************************************************
      IF( ISTYPE.EQ.2 .AND. ISOURC.EQ.1 .AND. IPOND.EQ.2 .AND.
     1    ISCONF.EQ.4 .AND. HW.GT.0.0 ) THEN
        FLUX=U*AN5*B1*HW
        DO JJ=1,NUMCON
          DO IJ=1,NUM(JJ)
            PNT(IJ,JJ)=PNT(IJ,JJ)+FLUX*CO(JJ)
          ENDDO
        ENDDO
        CALL MASS(TLIFE)
        WRITE (6,306)
306     FORMAT (/' Contaminant fluxes are being recomputed because the'/
     +         , ' source is partially below the water table:'/)
        CALL FLXTABLE (EXT_FLUX,PNT,TIME,NUM,NUMCON,IV,TLIFE,FILENM)
      ENDIF
      JJ=0
      DO 318 I=1,NUMCON
        FLUX = PNT(1,I)
C*****************************************************************************
C     CHECK TO SEE IF THE SOURCE TERM IS UNIFORMLY DISTRIBUTED (STEADY STATE)
C*****************************************************************************
        DO 158 JK=1,NUM(I)
          IF (FLUX .EQ. PNT(JK,I)) GO TO 158
          JJ=JJ+1
158     CONTINUE
        IF (JJ .GT. 0) ISTEDY(I)=1
C****************************************************************************
C     CALCULATE INITIAL DISSOLVED CONCENTRATIONS AT THE SOURCE
C****************************************************************************
        IF( IPOND.EQ.2 ) THEN
          IF( ISTYPE.EQ.1 ) THEN
            IF( ISOURC.EQ.1 .AND. ISCONF.EQ.4 ) 
     1          CALL CONC(CO,WUMC,WUBD,I)
            IF( ISOURC.GE.2 .AND. ISCONF.LT.4 )
     1          CALL CONC(CO,WUMC,WUBD,I)
          else IF( ISTYPE.EQ.2 ) THEN
            IF( ISOURC.LE.2 .AND. ISCONF.GE.4 )
     1          CALL CONC(CO,WUMC,WUBD,I)
            IF( ISOURC.EQ.2 .AND. ISCONF.LT.4 )
     1          CALL CONC(CO,WUMC,WUBD,I)
          ENDIF
        ENDIF
CKJC  Added for dll compilation
        if (ERRORSIG) goto 9999
  318 CONTINUE
  319 CONTINUE
C*****************************************************************************
C     IF THE INITIAL CONCENTRATION IS <= ZERO, THEN THE CODE DID NOT COMPUTE
C     CO AND THE USER HAS NOT ENTERED A CO.  IF THE INITIAL CONCENTRATION
C     IS NOT A DISSOLVED CONCENTRATION (ICONC <> 3), THEN THE USER HAS
C     ENTERED A TOTAL CONCENTRATION AND THE CODE CANNOT CONVERT TO A DISSOLVED
C     CONCENTRATION.  IN EITHER OF THESE CASES, TRY TO USE THE SOLUBILITY
C     LIMIT FOR CO.  IF NO SOLUBILITY LIMIT IS ENTERED OR IF THE SOLUBILITY
C     IS UNREALISTIC, SET CO REAL HIGH TO BE NON-LIMITING.
C******************************************************************************
      IF (KMED.EQ.1) THEN
        DO M1 = 1,NUMCON
          IF (CO(M1).LE.0.0.OR.ICONC(M1).NE.3) THEN
            IF (SOLLIM(M1).GT.0.0.AND.SOLLIM(M1).LT.10.0) THEN
              CO(M1) = SOLLIM(M1)
              WRITE (11,543) M1
            ELSE
              CO(M1) = 1.0E+30
              WRITE (11,393) M1
            ENDIF
            ICONC(M1) = 3
          ENDIF
        ENDDO
543   FORMAT (
     +2X/,'WARNING --> Contaminant Number = ',I2,
     +2X/,'The solubility limit is being used for the initial ',
     +'concentration.',/)
393   FORMAT (
     +2X/,'WARNING --> Contaminant Number = ',I2,
     +2X/,'The initial concentration at the source for this',
     +2x/,'contaminant is being set to 1E+30, and thus will not',
     +2X/,'be a constraint on concentrations at the receptor.'/)
C*****************************************************************************
C     IF THE INITIAL CONCENTRATION IS GREATER THAN THE SOLUBILITY LIMIT,
C     WRITE A WARNING MESSAGE.
C*****************************************************************************
        DO I = 1,NUMCON
          IF (CO(I).GT.SOLLIM(I).AND.CO(I).LT.1.0E+30) THEN
            WRITE (11,394) I,SOLLIM(I)
          ENDIF
        ENDDO
        WRITE(6,621)
        DO I = 1,NUMCON
          WRITE(6,885) IV(I,1),CASN1(I,1),AL(I,1),co(i),EQCOEF(I),
     1             SOLLIM(I),CINVEN(I)
  885     FORMAT (A18,5x,A18,1P,E9.2,2x,E9.2,1x,E9.2,3x,E10.2,2x,E10.2)
          IF (CO(I).LT.1.0E+30) CO(I)=CO(I)/UNITS
        ENDDO
      ENDIF
394   FORMAT (
     C /2X,'WARNING --> Contaminant Number = ',I2,
     + /2x,'The initial concentration at the source for this',
     + /2x,'contaminant EXCEEDS the solubility limit of ',1PE10.3,
     C /2x,'(g/mL)'/)
C******************************************************************************
C     TIMEX IS USED TO SAVE THE LAST TIME IN EACH MEDIA.  FOR SOME REASON THIS 
C     LAST TIME HAS BEEN GETTING LOST AND REDUCING THE TOTAL TIME OF THE RUN.  
C     JWB 3-24-89
C******************************************************************************
      DO 20 I=1 ,NUMCON
      TIMEX=TIME(NUM(I),I)
       IF (KMED .NE. 1) THEN
        TSTRT3(I)=TSTRT3(I)+TSTRT2(I)
        TIMEX=TIME(NUM(I)+1,I)
       ENDIF
      TSTRT2(I)=0.
      TSTRT1(I)=TIME(1,I)
      TEND1(I)=TIMEX + TSTRT3(I)
      IF (NPRINT .LE. 1) GO TO 209
      WRITE(6,357) I,TSTRT1(I),TEND1(I)
  357 FORMAT (/' Constituent = ',I2/'Source-Term Starting Time = ',
     1   1PE10.2,/,' Source-Term Ending Time = ',E10.2//)
  209 CONTINUE
C**********************************************************************
C     CALCULATION OF THE DECAY COEFFICIENT FOR FIRST MEDIUM ONLY
C**********************************************************************
      ND=NDS(I)
      IF (KMED .NE. 1) GO TO 20
      DO 111 N=1,ND
  111 AL(I,N)=ALOG(2.0)/AL(I,N)
      IF (I.EQ.1) WRITE (6,'(//)')
      WRITE (6,371) IV(I,1),CASN1(I,1),AL(I,1)
  371 FORMAT (1X,A18,' (',A18,'): DECAY CONSTANT = ',1PE10.3,' 1/YR')
   20 CONTINUE
   21 CONTINUE
      IF (KMED .NE. 1) GO TO 668
C*****************************************************************************
C     Fluxes modified based on fraction to aquifer (PNTFRAC)  JWB - 01/31/93  
C*****************************************************************************
      if((ISTYPE.gt.2).or.(pntfrac.eq.0.)) PNTFRAC = 1.0
      do I = 1, NUMCON
        TOTMAS(I) = TOTMAS(I) * PNTFRAC
        do JK = 1, NUM(I)
          PNT(JK,I) = PNT(JK,I) * PNTFRAC
        enddo
      enddo
C*****************************************************************************
C     FOR WETLAND MEDIUM (MED(K) = 7)
C*****************************************************************************
  668 IF (MED(KMED) .EQ. 7) WRITE(6,84)
   84 FORMAT (//T2,'CONTAM',T11,'NAME',T24,'INITIAL',T38,'MOLECULAR',
     1        T50,'EQUILIBRIUM',T64,'BIOCONCENTRATION',/,T4,'#',T21,
     2        'CONCENTRATION',T39,'WEIGHT',T50,'COEFFICIENT',T69,
     3        'FACTOR',/,T20,'(pCI/ML OR G/ML)',T38,'(G/MOLE)',
     4        T52,'(ML/G)',T69,'(ML/G)'/)
 9999 RETURN
      END
