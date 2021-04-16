C   INTAKER.FOR INTAKE              Version Date: 12-Sept-2002               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE INTAKER                               *
C                                                                            *
C  Subroutine INTAKER calculates intake rates from exposure concentrations   *
C                                                                            *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    10-Jan-97    DL Strenge                                 *
C  Last Modified:    12-Sept-2002 BAN                                        *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EDUP/INTAKE
C     Called by: EPFRIF
C     Calls: NONE  
C     Common blocks referenced:        
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C  Name      Used   Type    Location  Parameter Description
C  --------- -----  ------  --------- -------------------------------------
C  RNAME      U    Char*12  Name of paraent radionuclide             
C  PNAME      U    Char*12  Name of progeny radionuclide, if any
C  IPAR       U      Int    Seach type flag: 0 for parent, >0 for progeny
C  NRAD      S/U     Int    Position of parent in GID master list
C  NPROG     S/U     Int    Position of progeny in GID master list
C  NPOINTS    U      Int    Number of points for which results are needed
C  IAGE       U      Int    Age group index to be evaluated on this call
C==== Modification History ===================================================
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C   10-Jun-97      DLS  Initial programming
C   6-May-98       BAN  Updated to 41x41
C   12-Sept-2002   BAN  Revised Rn-222 progeny equilibrium factor adjustment
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE INTAKER(NPATH,NRAD,NPROG,EXPCON,EXPINT,NPOINTS,IAGE,
     .  RNAME)
C
C---- Include statements -----------------------------------------------------
c
      INCLUDE 'AGES.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'PATHFAC.CMN'
      INCLUDE 'RADATA.CMN'
	INCLUDE 'ALLPAR.CMN'
	INCLUDE 'RADON.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      INTEGER NRAD, NPROG, NPATH, NPOINTS
      REAL EXPCON(1681),EXPINT(1681,5)
      REAL RFACTOR
	CHARACTER RNAME*12
	LOGICAL SEQI
C
C---- Data Statements --------------------------------------------------------
C
C     NONE
C-----------------------------------------------------------------------------
C
C---- First set radionuclide specific factor, RFACTOR, for dermal pathways ---
C
      IF(NPATH.LE.0.OR.NPATH.GT.28) GO TO 998
      RFACTOR = 1.0
C  This stuff is not currently used.
      IF(NPATH.EQ.23) THEN          ! Showering dermal contact
         RFACTOR = PERMK(NRAD,NPROG+1)
      ELSEIF(NPATH.EQ.25) THEN      ! Soil dermal contact
         RFACTOR = ABSFAC(NRAD,NPROG+1)
      ELSEIF(NPATH.EQ.26) THEN      ! Swimming dermal contact
         RFACTOR = PERMK(NRAD,NPROG+1)
      ELSEIF(NPATH.EQ.27) THEN      ! Shoreline sediment dermal contact.
         RFACTOR = ABSFAC(NRAD,NPROG+1)
      ENDIF
C  Use the same type of logic to incorporate the radon progeny equilibrium
C   for indoor air
      IF ((NPATH.EQ.22) .AND. ((RNAME.EQ.'PB214') .or. 
     .    (RNAME .eq. 'BI214'))) RFACTOR = EQFRAC(IAGE)
C
C
      IA = IAGE
        DO IP = 1,NPOINTS
          EXPINT(IP,IA) = RFACTOR * EXPCON(IP) * PFACTOR(NPATH,IA)
C
C -------  Working Level Trap ------------------
C
	IF (NPATH .EQ. 2)THEN
	  RF = RFACTOR
	ELSE
	  RF = 0.0
	END IF
      IF(SEQI(RNAME,'RN222',5)) WL(IAGE, IP) = WL(IAGE,IP) + 
     .               EXPCON(IP)*RF*0.104*2.67E-4
      IF(SEQI(RNAME,'PB214',5)) WL(IAGE,IP) = WL(IAGE,IP) + 
     .               EXPCON(IP)*RF*0.514*2.67E-4
      IF(SEQI(RNAME,'BI214',5)) WL(IAGE,IP) = WL(IAGE,IP) + 
     .               EXPCON(IP)*RF*0.382*2.67E-4
C
C -------  Working Level Month Trap ------------------
C
      If(NPATH .EQ. 2)then
C Outdoors
      IF(SEQI(RNAME,'RN222',5)) WLM(IAGE, IP) = WLM(IAGE,IP) + 
     .               EXPCON(IP)*RF*0.104*2.67E-4 *
     .               TINH(IA) * FRINH(IA) * 24.0 / 170.0 ! 170 hours/WL-month
      IF(SEQI(RNAME,'PB214',5)) WLM(IAGE,IP) = WLM(IAGE,IP) + 
     .               EXPCON(IP)*RF*0.514*2.67E-4 *
     .               TINH(IA) * FRINH(IA) * 24.0 / 170.0 ! 170 hours/WL-month
      IF(SEQI(RNAME,'BI214',5)) WLM(IAGE,IP) = WLM(IAGE,IP) + 
     .               EXPCON(IP)*RF*0.382*2.67E-4 *
     .               TINH(IA) * FRINH(IA) * 24.0 / 170.0 ! 170 hours/WL-month
C Indoors
      else if (NPATH .eq. 22) then
      IF(SEQI(RNAME,'RN222',5)) WLM(IAGE, IP) = WLM(IAGE,IP) + 
     .               EXPCON(IP)*RF*0.104*2.67E-4 *
     .               TINDRH(IA) * FRINDR(IA) * 24.0 / 170.0 ! 170 hours/WL-month
      IF(SEQI(RNAME,'PB214',5)) WLM(IAGE,IP) = WLM(IAGE,IP) + 
     .               EXPCON(IP)*RF*0.514*2.67E-4 *
     .               TINDRH(IA) * FRINDR(IA) * 24.0 / 170.0 ! 170 hours/WL-month
      IF(SEQI(RNAME,'BI214',5)) WLM(IAGE,IP) = WLM(IAGE,IP) + 
     .               EXPCON(IP)*RF*0.382*2.67E-4 *
     .               TINDRH(IA) * FRINDR(IA) * 24.0 / 170.0 ! 170 hours/WL-month
	end if
        END DO   ! End loop on location points
      GO TO 999
 998  WRITE(NERR,100) NPATH
 100  FORMAT(' Exposure pathway index out of range in INTAKER:',I3/
     .       '     Index must be between 1 and 28')
C
 999  RETURN
C
C----- END OF MODULE INTAKER ------------------------------------------------
C
      END

