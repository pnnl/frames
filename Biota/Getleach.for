C   GETLEACH  GENII/EXPOS              Version Date: 10-Jul-97               
C   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE GETLEACH                              *
C                                                                            *
C  Subroutine GETLEACH extracts data from the .GID file and calculates       *
C                    leach rate constants for agricultural soil.             *
C                                                                            *
C  Written by:       Dennis Strenge                                          *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    28-Nov-95                                               *
C  Last Modified:    10-Jul-97      DLS                                       *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EDUP/EXPOS
C     Called by: SUBROUTINE EXPOS                 
C     Calls: SUBROUTINE GET.. ROUTINES FOR READING .GID FILES
C     Common blocks referenced: LEACH,DEVICE,EXINFO
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C   SETDATA      U     CHAR*80 Argument  GID exposure data set
C   NLINES       U     INT     Argument  Number of lines used in GID set
C   LNAMES(9)    U     CHAR*12 Argument  Names of chain members from RMDLIB
C   NOCHM        U     INT     Argument  Number of chain members
C   ALEACH(9)   S/U    REAL*8  LEACH     Leach rate constant (1/days)
C   SOILKD(9)   S/U    REAL*4  LEACH     Kd for agricultural soil (ml/g)
C   THICK       S/U    REAL*4  LEACH     Soil thickness (cm)
C   MOISTC      S/U    REAL*4  LEACH     Soil moisture content (fraction)
C   BULKD       S/U    REAL*4  LEACH     Soil bulk density (g/cm3)
C   VLEACH      S/U    REAL*4  LEACH     total infiltration rate (cm/yr)
C==== Modification History ===================================================
C
C     Date      Who  Modification Description
C     --------  ---  ------------------------------------------------------
C   18-Dec-95   DLS  Removed NUC = 1 (set in PDATIN)
C   08-Jan-96   DLS  Allowed Kd = 0.0 to be used
C   01-Apr-97   DLS  Added printout of Kd with Leach Rate Constant
C   16-Apr-97   DLS  Changed PRM parameter name from NUMCON to NUMALLCON.
C   12-Jun-97   DLS  Modifed from MEPAS version for use in GENII/EDUP
C   10-Jul-97   DLS  Added LNAMES to call list for checking/printing chain
C                    members in GID data set
C   15 Oct 2004 BAN  MOISTC is not in percent, but ml/cm3
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE GETLEACH(SETDATA,NLINES,LNAMES,NOCHM)
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'LEACH.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'EXINFO.CMN'
C
C==== DIMENSION Statements ===================================================
C
C
C==== Variable Declarations ==================================================
C
      INTEGER GETINT
      CHARACTER*12 LNAMES(LENCHAIN), CON
      CHARACTER*32 GETSTR
      CHARACTER*80 SETDATA(LINEMAX)
      LOGICAL FOUND
C
C==== DATA Statements ========================================================
C
      DATA IZ/0/
C      DATA C365/365.25/
C
C---- Get leach rate selection flag ------------------------------------------
C
      LCHOPT=GETINT(SETDATA,NLINES,"LEACHOPTION   ",IZ,IZ,IZ,IZ,IZ,IZ)
      NPRG = NOCHM - 1
C
C----- LCHOPT = 1, No calculations, use values from ENVLIB -------------------
C
      IF(LCHOPT.LE.1.OR.LCHOPT.GT.3) THEN
          GO TO 999
      ENDIF
      IF (LCHOPT.EQ.2) THEN
C
C---- Get parameter for agricultural soil ------------------------------------
C
      VAL = GETREAL(SETDATA,NLINES,"BULKD         ",IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.) BULKD = VAL
      VAL = GETREAL(SETDATA,NLINES,"MOISTC        ",IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.) MOISTC = VAL
      VAL = GETREAL(SETDATA,NLINES,"THICK         ",IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.) THICK = VAL
      VAL = GETREAL(SETDATA,NLINES,"VLEACH        ",IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.) VLEACH = VAL
C
      write(NELS,3000) LNAMES(1),VLEACh,thick,moistc,bulkd
 3000 format(/' The following data was used to calculate leaching rate'
     . ' constants for pollutant: ',A8/
     . ' Total infiltration rate (cm/yr): ',F10.1/
     . ' Agricultural soil thickness (cm):',F10.1/
     . ' Soil moisture content (ml/cm3): ',F10.1/
     . ' Soil bulk density (g/cm3):       ',F10.2)
      ENDIF
C
C---- Identify parent in exposure location GID data set ----------------------
C     Read number of constituents (paraents) in GID exposure data set
C
      NUMCON = GETINT(SETDATA,NLINES,'NumCon          ',IZ,IZ,IZ,IZ,
     .                IZ,IZ)
      DO IN = 1,NUMCON
C
C---- Get current parent name and test against input name, LNAMES(1) ---------
C
          CON = GETSTR(SETDATA,NLINES,'CASID         ',IN,IZ,IZ,IZ,
     .                IZ,IZ)
        IF(CON.EQ.LNAMES(1)) THEN   ! found parent in list
C
C---- Get number of progeny for this parent ----------------------------------
C
          NDTS = GETINT(SETDATA,NLINES,'NDS           ',IN,IZ,IZ,IZ,
     .                 IZ,IZ)
C
C---- Get leach rates for parent --------------------------------------------
C     For LCHOPT = 2, read Kd values and calculate leach rate constant
C     for LCHOPT = 3, read leach rate constants directly
C
          IF(LCHOPT.EQ.2) THEN

            SOILKD(1) = 0.0
            SOILKD(1) = GETREAL(SETDATA,NLINES,"SOILKD        ",IN,IZ,
     .                  IZ,IZ,IZ,IZ)
            AL = VLEACH/ ( THICK*MOISTC*  (  1.+
     .                      ( BULKD*SOILKD(1)/MOISTC) )  )
C     .                      ( BULKD*SOILKD(1)/MOISTC) )  ) / C365
          ELSE  ! LCHOPT = 3
            AL = GETREAL(SETDATA,NLINES,"LEACHR        ",IN,IZ,
     .                  IZ,IZ,IZ,IZ)
          ENDIF
          ALEACH(1) = AL
C
C---- Get leach rates for progeny, if any ------------------------------------
C
          IF(NOCHM.GT.1) THEN
C
C---- Search all progeny for chain members from master list ------------------
C
            DO 10 IP = 2,NOCHM
              IF(NDTS.GT.0) THEN
               FOUND = .FALSE.
               AL = 0.0
C
C---- Find current progeny in list from GID file -----------------------------
C
               DO IGN = 1,NDTS
                 IGX = IGN
                 CON = GETSTR(SETDATA,NLINES,'CASID         ',IN,IGX,
     .                         IZ,IZ,IZ,IZ)
                 IF(CON.EQ.LNAMES(IP)) THEN
C
C----- If progeny found in GID, then read Kd or leach rate constant ----------
C
                   FOUND = .TRUE.
                   IF(LCHOPT.EQ.2) THEN
                   SOILKD(IP) = 0.0
                   SOILKD(IP) = GETREAL(SETDATA,NLINES,'SOILKD        ',
     .                                  IN,IGX,IZ,IZ,IZ,IZ)
                     IF(SOILKD(IP).GT.0.) THEN
                       AL = VLEACH/ ( THICK*MOISTC*  (  1.+
     .                        ( BULKD*SOILKD(IP)/MOISTC) )  ) 
                     ENDIF
                   ELSE
                     AL = GETREAL(SETDATA,NLINES,"LEACHR        ",IN,
     .                            IGX,IZ,IZ,IZ,IZ)
                   ENDIF
                   ALEACH(IP) = AL
                 ENDIF
               END DO
C
C---- If progeny not found in GID, set Kd or leach rate to parent value ------
C
               IF(.NOT.FOUND.OR.AL.LE.0.) THEN
                SOILKD(IP) = SOILKD(1)
                ALEACH(IP) = ALEACH(1)
               ENDIF
              ELSE  ! Progeny not in master list, use parent soilkd
                SOILKD(IP) = SOILKD(1)
                ALEACH(IP) = ALEACH(1)
              ENDIF
  10        CONTINUE
          END IF  !   IF progeny
C
C---- Write leach rate constants to output listing ---------------------------
C
          write(NELS,3001) LNAMES(1),aleach(1),soilkd(1)
 3001     format(1x,a12,' leach constant (d-1):',1pe10.3,'.  Soil Kd =',
     .         e10.3)
          DO IP = 1,NPRG
            write(NELS,3001) LNAMES(IP+1),aleach(IP+1),soilkd(IP+1)
          END DO
        ENDIF  ! If on matching CON to LNAMES(1)
C
      END DO  ! End of Parent constituent loop 
C
 999  RETURN
C
C================ END OF MODULE GETLEACH ===================================
C
      END
