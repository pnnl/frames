!     Last change:  DLS  23 Nov 2005   10:31 am
!     Code updates, code cleanup, compile in Compaq Fortran:  JGD  April 2010
!     Updated decay.ftn and device.ftn files - added missing parameters:  JGD  April 2010 
!     Decay of chemicals is not operational in this version -- many applicable lines are commented out
!
MODULE Errors_Mod
!!! Module Errors_Mod +++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains variables related to writing error messages to the
!    report file.  The messages are written using the routine PRTERR.
!
!  History:
!
!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!
!  Code Block:
!
      INTEGER :: IRPT ! Unit number for writing the error messages
      INTEGER, PARAMETER :: MAXMES = 5 ! Maximum number of lines in an error message
      CHARACTER(LEN=72), DIMENSION(MAXMES) :: MESSAG ! Vector of message lines
!
!!! End Module Errors_Mod ++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE
!
MODULE Decay_Chem_Input_Mod
!!! Module Decay_Chem_Input_Mod +++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  Purpose:
!
!    This module contains input (from CSM section of GID) parameters describing chemical
!    chain decay for selected chemicals.  These values are analyzed to establish the chain
!    decay parameters needed for the ChemChain subroutine.
!
!   *********************************
!
!   NOTE: THIS MODULE IS NOT CURRENTLY USED
!         CHEMCHAIN IS NOT CURRENTLY USED
!         Chemical decay is performed using CHAIN as for radionuclides with the logical
!         parameter CHEM set .true. for chemicals (to inhibit division by lambda)
!
!   *********************************
!
!  Description:
!
!    Chains are defined in the GID by decay levels. Each level is defined as a set which
!    includes 1 parent and up to 2 decay/degradation products.  A maximum of 20 levels (20 sets)
!    is allowed in MEPAS Exposure Module.
!
!
!  History:
!
!    Dennis Strenge : 01 Nov 2004 : Version 1.0
!
!  Code Block:
!
      CHARACTER(LEN=12), DIMENSION(20) :: PARENT   ! Names of parents in each decay level set.
      CHARACTER(LEN=12), DIMENSION(2,20) :: DPNAME ! Decay product name(s) for each decay level set.
      INTEGER, DIMENSION(20) :: NDPS               ! Number of decay products in each decay level set (0,1,2)
      REAL(KIND=4), DIMENSION(20) :: PHALF         ! Half life of parent (20) in each decay level set.
      REAL(KIND=4), DIMENSION(2,20) :: DHALF       ! Half life of decay/degradation product for each decay level set.
      REAL(KIND=4), DIMENSION(2,20) :: DFRAC       ! Decay fraction (from parent to progeny) for each product
                                                   ! in each decay level set.
!
!!! End Decay_Chem_Input_Mod ++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE
!
MODULE Crop_Data_Mod
!!! Module Crop_Data_Mod +++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  Purpose:
!
!    This module contains intermediate results for evaluation of contributions to crop
!    pathways by: deposition onto leaves, uptake from roots, animal drinking, and animal
!    soil intake.
!
!  History:
!
!    Dennis Strenge : 29 Jan 2003 : Version 1.0
!
!  Code Block:
!
      REAL(KIND=4), DIMENSION(4,4) :: CROP_FACTORS   ! product concentration from each
                                                     ! route contributing to contamination
!    Dimension 1 is for Crop 1, Crop 2, Animal meat, animal milk
!    Dimension 2 is for: 1 - Deposition to leaves (or animal feed intake plant)
!                        2 - Root uptake by plant
!                        3 - Animals drinking water intake
!                        4 - Animal soil ingestion
!
!!! End Crop_Data_Mod ++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE
MODULE Decay_Chem_Mod
!!! Module Decay_Chem_Mod +++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  Version Date: 28-Oct-2004
!  This data module is part of the MEPAS software package.
!
!  This data module contains chain decay parameters for one chemical decay chain
!
!  Developed by:   Pacific Northwest Laboratory
!                  Occupational & Environmental Health Protection Section
!                  P. O. Box 999
!                  Richland, WA 99352
!
!         Software Design Description:  D.L. Strenge
!         Initial preparation: 28-Oct-04
!         Initial testing complete:
!
!         Modification History:
!    Date  Change by Summary of Modification
!  ------- --------- ------------------------------------------------------
!
!
! --------------------------------------------------------------------------
!
!  Code Block:
!
      INTEGER, DIMENSION(3) :: NUCC                ! Number of chemical chain members, by media
      INTEGER, DIMENSION(2,20,3) :: IFRMC          ! Source chain member for Branching fractions by media
      INTEGER, DIMENSION(20,3) :: ICROSSC          ! Cross index to master list location, by media
      REAL(KIND=8), DIMENSION(2,20,3) :: DKC       ! Decay fraction, by media
      REAL(KIND=8), DIMENSION(20,3) :: ALC         ! Decay rate constant for each chemical chain member, by media
      CHARACTER(LEN=4), DIMENSION(20,3) :: CONNAMC ! Chemical names, by media
!
!     Media defined for chemical decay chains are:
!     array position 1:  air
!     array position 2:  soil
!     array position 3:  water
! --------------------------------------------------------------------------
!
!  Parameters in COMMON BLOCK
!  Name (dim)       Type                     Purpose
!  -------------   --------  --------------------------------------------
!    Each parameter is also dimensioned by media for chemicals
!
!    NUCC            Integer   Number of radionuclides in the current chain
!                             representation, maximum value is 9.
!
!    IFRMC(2,20)     Integer   Index of the parents (if any) for each chain
!                             member, with two (2) parents allowed for each of
!                             nine (20) chain members.
!
!    DKC(2,20)       Real      Fraction of each parent decays that result in
!                             production of the chain member, with index
!                             values corresponding to those defined for IFRM
!                             above.
!
!    ALC(20)         Real      Radiological decay constant for each member of
!                             the chain, d-1.
!
!!! End Decay_Cem_Mod ++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE
!
!   MEPAS AHAZ: Chem_Data_Read.F95             Version Date:  03-Nov-2004
!==== SUBROUTINE CALL ========================================================
!
    SUBROUTINE CHEM_DATA_READ(SETDATA,NLINES,IPDAT,TPATH,CHEM,IERR)
!
!   Copyright 1989-2004 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE Chem_Data_Read                        *
!                                                                            *
!  Subroutine Chem_Data_Read reads the exposure pathway parameters from the  *
!                    CONi section of the GID file for one parent constituent *
!                    plus associated progeny                                 *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    01 Nov 04 (From AHAZX PDATIN version 03-Nov-04          *
!  Last Modified:                                                            *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZXALL
!     Called by: SUBROUTINE AHAZX, main program
!     Calls: SUBROUTINES BFCAL, BICAL, BVCAL, FEMCAL, FMICAL
!     Common blocks referenced: PSET1, PSET3, DEVICE, TITLS, KLASS
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     variable    S    CHAR    Argument  nnnnnnnnnnnnnnnnnnnnnn
!                 U    INT     Internal
!                 S/U  REAL    Common
!                      DBLE    External
!
!     ARGUMENT LIST:
!
!    SETDATA CHAR    Images of GID file section CONi (i = icon for chemical database)
!    NLINES  INT     Number of lines in SETDAT to consider
!    IPDATF  INT     Print flag for constituent data (>0 if not previously printed)
!    IERR    INT     Error message flag
!
!==== Modification History ===================================================
!
!   Date      Who  Modification Description
!  --------   ---  ------------------------------------------------------
!  03-Nov-04  DLS  Created from PDATIN for AHAZX
!
!==== COMMON Block Definitions ===============================================
!
!     Define all parameters used in this module
!
!      IMPLICIT NONE
!
      include 'AQPATH.FTN'
      include 'PSET1.FTN'
      include 'PSET3.FTN'
      include 'DERMDAT.FTN'
      include 'DEVICE.FTN'
      include 'TITLS.FTN'
      include 'KLASS.FTN'
      include 'PFLAGS.FTN'
      include 'RISKCM.FTN'
      include 'COUPLE.FTN'
      include 'DECAY.FTN'
!
!
!==== DIMENSION Statements ===================================================
!
      DIMENSION CKOC(20),       SOL(20),VAP(20), lll(20)
      DIMENSION CAS(3),           CASID(3,20),ffname(17,20)
!      dimension airhalf(20), swhalf(20), gwhalf(20), soilhalf(20)
!
!==== Argument Declarations ==================================================
!     CHARACTER*80 SETDATA(LINEMAX)
!
      CHARACTER(LEN=*), DIMENSION(*) :: SETDATA
      INTEGER :: NLINES         ! Number of lines of GID file data included in array SETDATA
      INTEGER :: IPDAT          ! Print flag, > 0 indicates parameters for this constituent are
                                ! to be printed to the ELS file.
      INTEGER :: TPATH          ! Medium flag: 1 - GW, 2 - SW, 3 - Air, 5 - soil
      INTEGER :: IERR           ! Error flag, > 0 indicates error condition on RETURN
!
!---  Internal parameters
!
      INTEGER, DIMENSION(20) :: ICM
      INTEGER :: IPARENT
      INTEGER :: KTYPE
      INTEGER :: LEVEL
      INTEGER, DIMENSION(20) :: NDSLEVEL   ! number of constituents defined for each level
      INTEGER, DIMENSION(20) :: NDSPROG    ! number of decay/degradation products defined for each constituent
      INTEGER, DIMENSION(20,20) :: LEVELNUMCM  ! Cross index between level and input chain members
      INTEGER :: NUMCM
      CHARACTER(LEN=12), DIMENSION(20) :: FSMEMBER
      CHARACTER(LEN=12) :: PARENTID
      CHARACTER(LEN=12) :: FSTEST
      CHARACTER(LEN=80), DIMENSION(20,2) :: FSNA
      CHARACTER(LEN=12), DIMENSION(20,20) :: FSIDLEVEL
      CHARACTER(LEN=12), DIMENSION(20) :: FSPID
      CHARACTER(LEN=80), DIMENSION(20) :: FSPNA
      REAL(KIND=4), DIMENSION(20,2) :: FSFRACTION
      REAL(KIND=4), DIMENSION(20,20) :: FRACTIONLEVEL
      REAL(KIND=8) :: AL2
      REAL(KIND=8) :: HALF
      LOGICAL :: FOUNDMEMBER
      INTEGER :: FSTCHR
!
!==== Variable Declarations ==================================================
!
      CHARACTER*4 CAS, CASID, ffname
      CHARACTER(LEN=80) :: PNAME
      CHARACTER(LEN=80) :: fname
      CHARACTER*4 RN1, RN2
      CHARACTER*1 B
!
!     GID variable names
!
      CHARACTER(LEN=12) :: FSCASID         ! Name of current constituent
      CHARACTER(LEN=12), DIMENSION(20,2) :: FSID ! Names of each chain member (parent in position 1)
!
!     Function type definitions
!
      LOGICAL :: SEQI                ! function for comparing two strings case-insensitive
      CHARACTER(LEN=80) :: GETSTR            ! Function for extracting a character parameter value from GID data
      REAL(KIND=4) :: GETREAL        ! Function for extracting a real parameter value from GID data
      INTEGER GETINT                 ! Function for extracting an integer parameter value from GID data
      !
!==== DATA Statements ========================================================
!
      DATA B/' '/
      DATA DFLAM/1.E-8/
      DATA RN1/'RN22'/,RN2/'2   '/
!
!----- Start of Calculations --------------------------------------------------
!
!----- Initialize index values
!
      NF = 0
      LEVEL = 0
      NUMCM = 0
      NUC = 1                 ! set chain length to 1 (no decay/degradation products)
      NUCC = 1
!
!---- Initialize index values
!

!
!---- Initialize arrays -------------------------------------------------------
!
      DO IL = 1,20
         FSMEMBER(IL) = ' '
         FSPID(IL)    = ' '
         FSPNA(IL)    = ' '
         NDSLEVEL(IL) = 0
         NDSPROG(IL)  = 0
         DO IN = 1,20
            FSIDLEVEL(IL,IN) = ' '
            LEVELNUMCM(IL,IN) = 0
            FRACTIONLEVEL(IL,IN) = 0.0
         END DO
         DO IN = 1,2
            FSNA(IL,IN) = ' '
            FSID(IL,IN) = ' '
            FSFRACTION(IL,IN) = 0.0
         END DO
      END DO
      PARENTID = ' '
      FSTEST   = ' '
!
! ***********************************************************************
!
!  Find parent constituent in Chemical Database.  Statement 10 is return
!  point for incrementing search in Chemical database.
!
! ***********************************************************************
!
!----- Get number of constituents included in the analysis
!
       WRITE(NELS,'(a,2i5)') ' In Chem_Data_Read, number of data lines is: ',NLINES
       WRITE(NELS,'(a,a,2i5)') ' Line 1 is: ',SETDATA(1)
       NUMCON = GETINT(SETDATA,NLINES,'NUMCON        ',SITNUM,IZ,IZ,IZ,IZ,IZ)
!
!   If number of constituents in the chemical database is < 1, error
!
      IF(NUMCON.LT.1) GO TO 997
!
!   Loop on number of constituents in GID file, searching for current constituent (POLID(i),i=1,3)
!
      DO IP = 1,NUMCON
!
         FSCASID = GETSTR(SETDATA,NLINES,'FSCASID       ',SITNUM,IP,IZ,IZ,IZ,IZ)
         WRITE(NELS,'(a,a,2i5)') ' Searching for parent of chain, FSCASID, NUMCON, LEVEL: ',FScasID, numcON, LEVEL
!
         IF(.NOT.SEQI(FSCASID(1:4),POLID(1,1),4)) GO TO 10
         IF(.NOT.SEQI(FSCASID(5:8),POLID(2,1),4)) GO TO 10
         IF(.NOT.SEQI(FSCASID(9:12),POLID(3,1),4)) GO TO 10
!
!----    Constituent found, transfer to read parameters for this parent
! 
         FSPID(1) = FSCASID
         FSPNA(1) = GETSTR(SETDATA,NLINES,'FSCNAME       ',SITNUM,IP,IZ,IZ,IZ,IZ)
         ICM(1) = IP
         IPARENT = IP
!         write(*,*) 'ip,ic=',ip,ic
         IF(IC.gt.0) ICDB(IC) = IP  ! temporary patch --why is ic 0 sometimes? jgd
         WRITE(NELS,'(a,a,i5,a,a)') ' Found parent, constituent number: ',FSCASID, IP,' Name', fspna(1)
         GO TO 12
!
  10     CONTINUE
!
      END DO
!
!---- At this point, the constituent was not found in the GID CONi section.
!     Write error message and return.
!
     GO TO 996        ! error message on constituent not found, RETURN
!
!---- Continue point when constituent was found
!
  12 CONTINUE
!
      NUC = 1                 ! set chain length to 1 (no decay/degradation products)
      NDSPROG(1) = 0
      IF(NDS.LE.0) GO TO 15
      NUMCM = 1
      LEVEL = 1
!
!---- Read decay/degradation products for the parent (index = IPARENT)
!
         IDS = 1
         FSID(NUMCM,1) = GETSTR(SETDATA,NLINES,'FSCASID       ',SITNUM,ICM(1),IDS,IZ,IZ,IZ)
         WRITE(NELS,'(a,a,2i5)') ' Read 1st progeny of parent, chain: ',FSID(numcm,1), numcm
         FSNA(NUMCM,1) = GETSTR(SETDATA,NLINES,'FSCNAME       ',SITNUM,NUMCM,IDS,IZ,IZ,IZ)
         FSFRACTION(NUMCM,1) = GETREAL(SETDATA,NLINES,'FSFRACTION    ',SITNUM,ICM(1),IDS,IZ,IZ,IZ)
         NDSPROG(NUMCM) = 1
         NDSLEVEL(LEVEL) = NDSLEVEL(LEVEL) + 1
         FSIDLEVEL(LEVEL,NDSLEVEL(LEVEL)) = FSID(NUMCM,1)
         FRACTIONLEVEL(LEVEL,NDSLEVEL(LEVEL)) = FSFRACTION(NUMCM,1)
         LEVELNUMCM(LEVEL,NDSLEVEL(LEVEL)) = NUMCM
         WRITE(NELS,'(a,a,2i5)') ' Found 1st progeny of parent, chain, level numbers: ',FSID(numcm,1), numcm,level
         WRITE(NELS,'(a,a,2i5)') ' Save as FSIDLEVEL(LEVEL,NDSLEVEL(LEVEL) = ',FSIDLEVEL(LEVEL,NDSLEVEL(LEVEL)), LEVEL, &
                                          NDSLEVEL(LEVEL)
!
!---- Test fraction to see if another decay/degradation product is to be read
!
        IF(FSFRACTION(1,1).LT.1.00.and.NDS.EQ.2) THEN
           !IDS = 2
           FSID(NUMCM,2) = GETSTR(SETDATA,NLINES,'SSCASID       ',SITNUM,ICM(1),IDS,IZ,IZ,IZ)
           FSNA(NUMCM,2) = GETSTR(SETDATA,NLINES,'SSCNAME       ',SITNUM,ICM(1),IDS,IZ,IZ,IZ)
           FSFRACTION(NUMCM,2) = GETREAL(SETDATA,NLINES,'SSFRACTION    ',SITNUM,ICM(1),IDS,IZ,IZ,IZ)
           NDSPROG(NUMCM) = 2
           NDSLEVEL(LEVEL) = NDSLEVEL(LEVEL) + 1
           FSIDLEVEL(LEVEL,NDSLEVEL(LEVEL)) = FSID(NUMCM,2)
           FRACTIONLEVEL(LEVEL,NDSLEVEL(LEVEL)) = FSFRACTION(NUMCM,2)
           LEVELNUMCM(LEVEL,NDSLEVEL(LEVEL)) = NUMCM
         WRITE(NELS,'(a,a,2i5)') ' Found 2nd progeny of parent, chain, level numbers: ',FSID(numcm,2), numcm,level
         WRITE(*,'(a,a,2i5)') ' Found 2nd progeny of parent, chain, level numbers: ',FSID(numcm,2), numcm,level
        ENDIF
!
!---- Evaluate each new member on this level.  Find the decay/degradation product(s) in the constituent list
!
!       Set number of decay/degradation produces to search for
!
 11   NUMFIND = NDSLEVEL(LEVEL)
      LEVEL = LEVEL + 1
      NDSLEVEL(LEVEL) = 0
      WRITE(NELS,'(a,3i5)') ' Start search for level progeny, NUMFIND, LEVEL, NUMCM: ',NUMFIND, LEVEL, NUMCM
!
!---- Loop on number to search for
!
      DO IDECAY = 1, NUMFIND
!
!        Loop on number of constituents in GID file, searching for current constituent (POLID(i),i=1,3)
!
         DO IP = 1,NUMCON
!
            FSCASID = GETSTR(SETDATA,NLINES,'FSCASID       ',SITNUM,IP,IZ,IZ,IZ,IZ)
!
            IF(.NOT.SEQI(FSCASID,FSIDLEVEL(LEVEL-1,IDECAY),12)) GO TO 13
!
!----    Constituent found
!
            NUMCM = NUMCM + 1
            NDSPROG(NUMCM) = 0
            FSPID(NUMCM) = FSCASID
            FSPNA(NUMCM) = GETSTR(SETDATA,NLINES,'FSCNAME       ',SITNUM,IP,IZ,IZ,IZ,IZ)
            ICM(NUMCM) = IP
            LEVELNUMCM(LEVEL,idecay) = NUMCM
!            WRITE(NELS,'(a,a,2i5)') ' Found progeny, constituent number, level index: ',FSCASID, IP, IDECAY
!
!---- Read number of decay/degradation products for this constituent decay/degradation product as a parent.
!
            NDS = GETINT(SETDATA,NLINES,'NDS           ',SITNUM,IP,IZ,IZ,IZ,IZ)
!
            IF(NDS.LE.0) GO TO 14
!
!---- Read decay/degradation products for the parent (index = IPARENT)
!
            IDS = 1
            FSID(NUMCM,1) = GETSTR(SETDATA,NLINES,'FSCASID       ',SITNUM,IP,IDS,IZ,IZ,IZ)
            FSNA(NUMCM,1) = GETSTR(SETDATA,NLINES,'FSCNAME       ',SITNUM,IP,IDS,IZ,IZ,IZ)
            FSFRACTION(NUMCM,1) = GETREAL(SETDATA,NLINES,'FSFRACTION    ',SITNUM,IP,IDS,IZ,IZ,IZ)
            NDSPROG(NUMCM) = 1
            NDSLEVEL(LEVEL) = NDSLEVEL(LEVEL) + 1
            FSIDLEVEL(LEVEL,NDSLEVEL(LEVEL)) = FSID(NUMCM,1)
            FRACTIONLEVEL(LEVEL,NDSLEVEL(LEVEL)) = FSFRACTION(NUMCM,1)
            LEVELNUMCM(LEVEL,NDSLEVEL(LEVEL)) = NUMCM
!
!---- Test fraction to see if another decay/degradation product is to be read
!
            IF(FSFRACTION(NUMCM,1).LT.1.00.and.NDS.EQ.2) THEN
!              IDS = 2
              FSID(NUMCM,2) = GETSTR(SETDATA,NLINES,'SSCASID       ',SITNUM,IP,IDS,IZ,IZ,IZ)
              FSNA(NUMCM,2) = GETSTR(SETDATA,NLINES,'SSCNAME       ',SITNUM,IP,IDS,IZ,IZ,IZ)
              FSFRACTION(NUMCM,2) = GETREAL(SETDATA,NLINES,'SSFRACTION    ',SITNUM,IP,IDS,IZ,IZ,IZ)
              NDSPROG(NUMCM) = 2
              NDSLEVEL(LEVEL) = NDSLEVEL(LEVEL) + 1
              FSIDLEVEL(LEVEL,NDSLEVEL(LEVEL)) = FSID(NUMCM,2)
              FRACTIONLEVEL(LEVEL,NDSLEVEL(LEVEL)) = FSFRACTION(NUMCM,2)
              LEVELNUMCM(LEVEL,NDSLEVEL(LEVEL)) = NUMCM
            ENDIF
!
            GO TO 14
!
  13        CONTINUE
!
         END DO
!
!---- At this point, the constituent was not found in the GID CONi section.
!     Write error message and return.
!
         GO TO 996        ! error message on constituent not found, RETURN
!
!---- Continue point when constituent was found
!
  14     CONTINUE
!
      END DO
!
!---- Determine next action. If still have progeny to identify then return to 11 to find rest of progeny
!     Otherwise, analyze decay data to establish decay chains
!
      IF(NDSLEVEL(LEVEL).GT.0) GO TO 11

!---- Continuation point for data input.  At this point the decay/degradation chain
!     has been identified, and the number of chain members is established (NUC).
!     Now the data for all chain members can be read.
!
!---- If there are no decay/degradation products, then read data for single parent and return to calling program.
!
  15  CONTINUE
!
!---- Set parent
!
!      WRITE(NELS,'(A,2I5)') ' LEVEL and NUMCM ',LEVEL, NUMCM
      IF(LEVEL.EQ.NUMCM) THEN
!
!---     Set parameters for straight chain decay/degradation
!
         NUC = NUMCM
         IF(NUC.LE.0) NUC = 1
         AL2 = DLOG(2.D0)
         WRITE(NELS,'(A,I4)') ' Setting decay parameters for chain with members:',NUC
         FSMEMBER(1) = FSPID(1)
         DO IC = 1,NUC
            IP = ICM(IC)
            POLID(1,IC) = FSPID(IC)(1:4)
            POLID(2,IC) = FSPID(IC)(5:8)
            POLID(3,IC) = FSPID(IC)(9:12)
            FSMEMBER(IC) = FSPID(IC)
            ICDB(IC) = IP
            WRITE(NELS,'(A,I5,A,A,A,I5)') '  Chain member:',ic,' is ',FSPID(IC),' DB position is: ',ICDB(IP)
            IF(IC.GT.1) THEN
               WRITE(NELS,'(A,F8.3,A,I5)') '  Decay fraction is: ',FSFRACTION(IC-1,1),' from member: ',IC-1
               IFRM(1,IC) = IC-1
               IFRM(2,IC) = 0
               DK(1,IC) = FSFRACTION(IC-1,1)
               DK(2,IC) = 0.0
            ENDIF
         END DO
!
!----  Set parameters for chains with branching
!
      ELSE
!
!----  Set parameters for parent and first level progeny -----------------------
!
! ***************
!  NEED HALFLIFE FOR ALL CHAIN MEMBERS FROM GID FILE
! ***************
        FSMEMBER(1) = FSPID(1)
        NUC = 2
        ICDB(1) = ICM(1)
        ICDB(2) = ICM(2)
        POLID(1,NUC) = FSID(1,1)(1:4)
        POLID(2,NUC) = FSID(1,1)(5:8)
        POLID(3,NUC) = FSID(1,1)(9:12)
        FSMEMBER(NUC) = FSID(1,1)
        IFRM(1,NUC) = 1
        IFRM(2,NUC) = 0
        DK(1,NUC) = FSFRACTION(1,1)
        DK(2,NUC) = 0.0
        IF(NDSLEVEL(1).EQ.2) THEN
           NUC = 3
           ICDB(3) = ICM(3)
           POLID(1,NUC) = FSID(1,2)(1:4)
           POLID(2,NUC) = FSID(1,2)(5:8)
           POLID(3,NUC) = FSID(1,2)(9:12)
           FSMEMBER(NUC) = FSID(1,2)
           IFRM(1,NUC) = 1
           IFRM(2,NUC) = 0
           DK(1,NUC) = FSFRACTION(1,2)
           DK(2,NUC) = 0.0
        ENDIF
        NLAST = NUC
!
!----  Set parameters for remaining levels ------------------------------------
!
       IF(NDSLEVEL(2).LE.0) GO TO 40
       DO IL = 2,LEVEL
          DO NL = 1, NDSLEVEL(IL)
!
!----        Set name of parent for current progeny (NL) on this level (IL)----
!
             NUMTEST = LEVELNUMCM(IL,NL)
             PARENTID = FSPID(NUMTEST)
!
!----        Loop on previously defined chain members to find current progeny
!            This test is to see if the current progeny is already in the chain
!
             FOUNDMEMBER = .FALSE.
             DO IPR = 1,NUC
                FSTEST = FSIDLEVEL(IL,NL)
                IF(SEQI(FSIDLEVEL(IL,NL),FSMEMBER(IPR),12)) THEN
                   IF(IFRM(2,IPR).GT.0) GO TO 995    ! Error condition, three parents
                   FOUNDMEMBER = .TRUE.
                   MEMPOSITION = IPR
                   DO INP = 1,NUC
                      IF(SEQI(PARENTID,FSMEMBER(INP),12)) THEN
                         IPRNT = INP
                         GO TO 20
                      ENDIF
                   END DO
                   GO TO 994   ! Parent not found in previous list of chain members
                ENDIF
             END DO
  20         CONTINUE
             IF(FOUNDMEMBER) THEN
                IFRM(2,MEMPOSITION) = IPRNT
                DK(2,MEMPOSITION) = FRACTIONLEVEL(IL,NL)
             ELSE        ! New member of chain, set parameters
!
!---            Find parent position in chain
!
                DO IPR = 1, NLAST
                    IF(SEQI(PARENTID,FSMEMBER(IPR),12)) THEN
                      NUC = NUC + 1
                      FSMEMBER(NUC) = FSIDLEVEL(IL,NL)
!
!----                 Find position of chain member in input master list from chemical database
!
                      DO IP = 1,NUMCON
                         FSCASID = GETSTR(SETDATA,NLINES,'FSCASID       ',SITNUM,IP,IZ,IZ,IZ,IZ)
                         IF(SEQI(FSCASID,FSMEMBER(NUC),12)) ICDB(NUC) = IP
                      END DO
!
!----                 Set name and decay data for this chain member
!
                      POLID(1,NUC)  = FSMEMBER(NUC)(1:4)
                      POLID(2,NUC)  = FSMEMBER(NUC)(5:8)
                      POLID(3,NUC)  = FSMEMBER(NUC)(9:12)
                      IFRM(1,NUC) = IPR
                      IFRM(2,NUC) = 0
                      DK(1,NUC) = FRACTIONLEVEL(IL,NL)
                      DK(2,NUC) = 0.0
                      GO TO 30
                   ENDIF
                END DO
                GO TO 994    ! error parent not found in previous list of chain members
 30             CONTINUE
             ENDIF      ! end if on current progeny being previously defined
!
          END DO    ! end loop on number of progeny defined in this level
!
          NLAST = NUC
        END DO       ! end loop on number of levels to evaluate
!
!---- End If on straight chain versus branched chain
!
      ENDIF
!
!----  Get half life values for each chain member -----------------------------
!
      AL2 = DLOG(2.D0)
      DO IC = 1,NUC
         IPR = ICDB(IC)
         WRITE(NELS,'(A,I5,A,I5)') ' Reading decay half life for chain member: ',ic,' DB position ',IPR
         IF(TPATH.EQ.1) THEN
            HALF = GETREAL(SETDATA,NLINES,'CLGHALF       ',SITNUM,IPR,IZ,IZ,IZ,IZ)
         ELSEIF(TPATH.EQ.2) THEN
            HALF = GETREAL(SETDATA,NLINES,'CLWHALF       ',SITNUM,IPR,IZ,IZ,IZ,IZ)
         ELSEIF(TPATH.EQ.3) THEN
            HALF = GETREAL(SETDATA,NLINES,'CLTHALF       ',SITNUM,IPR,IZ,IZ,IZ,IZ)
         ELSEIF(TPATH.EQ.5) THEN
            HALF = GETREAL(SETDATA,NLINES,'CLSHALF       ',SITNUM,IPR,IZ,IZ,IZ,IZ)
         ELSE
            IERR = IERR + 1
            WRITE(NERR,'(A,I5)') ' Error in chem_data_read, path index invalid: ',TPATH
            GO TO 900
         ENDIF
         IF(HALF.LE.0.) THEN
            AL(IC) = DBLE(DFLAM)
            WRITE(NELS,'(A,1P,E10.3)') '  Decay rate constant set to default value: ',DFLAM
         ELSE
            AL(IC) = AL2/(DBLE(HALF))
            WRITE(NELS,'(A,1P,E10.3)') '  Decay half life (days) is: ',HALF
            WRITE(NELS,'(A,1P,D10.3)') '  Decay constant is: ',AL(IC)
         ENDIF
!
      END DO
!
!---- End setting decay/degradation chain data --------------------------------
!
 40   CONTINUE
!
!---- Write decay chain parameters to ELS file for testing --------------------
!
      DO IN = 1,NUC
         HALF = AL2/AL(IN)
      END DO
!
!----- Set chemical database parameters for each chain member
!
      DO 300 IP = 1,NUC
!
! ************************************************************************
!      Find current chain member in chemical database
! ************************************************************************
!
!---- Set position of current chain member in master input list in GID file ---
!
          INP = ICDB(IP)
!
!---- Get parameter values from GID file for this chain member ----------------
!
!---- Read constituent type (CLKTYPE), 1 = radionuclide, > 0 = chemical
!
         KTYPE = GETINT(SETDATA,NLINES,'CLKTYPE       ',SITNUM,INP,IZ,IZ,IZ,IZ)
!
!---- Parent constituent found in GID Chemical Database section.  Check type.
!
         IF(KTYPE.LT.0.OR.KTYPE.GT.5) GO TO 998
         MTYPE(IP) = 1                              ! Revised to use new definition of KTYPE
         IF(KTYPE.LT.1) MTYPE(IP) = 2               ! KTYPE = 0 for chemicals, KTYPE = 1 for radionuclides
!
!---- Set chemical flag for parent (used for entire chain) --------------------
!
         IF(IP.EQ.1) THEN
            CHEM = .FALSE.
            IF(MTYPE(IP).GT.1) CHEM = .TRUE.
         ENDIF
!
!---- PNAME
         PNAME = GETSTR(SETDATA,NLINES,'FSCNAME       ',SITNUM,INP,IZ,IZ,IZ,IZ)
         FNAME = GETSTR(SETDATA,NLINES,'FSCNAME       ',SITNUM,INP,IZ,IZ,IZ,IZ)
!
!---- RWM
!
         RWM = GETREAL(SETDATA,NLINES,'CLWM          ',SITNUM,INP,IZ,IZ,IZ,IZ)
!
!---- Images of initial read from CHEMLIB.DAT ---------------------------------
!
      RVAP = GETREAL(SETDATA,NLINES,'CLVAP         ',SITNUM,INP,IZ,IZ,IZ,IZ)
!
!---- RHLC
!     CLHCL
      RHLC = GETREAL(SETDATA,NLINES,'CLHLC         ',SITNUM,INP,IZ,IZ,IZ,IZ)
!
!---- RSOL
!     CLSOL
      RSOL = GETREAL(SETDATA,NLINES,'CLSOL         ',SITNUM,INP,IZ,IZ,IZ,IZ)
!
!---- RKOC
!     CLKOC
      RKOC = GETREAL(SETDATA,NLINES,'CLKOC         ',SITNUM,INP,IZ,IZ,IZ,IZ)
!
!---- RCKOW
!     CLKOW
      RCKOW = GETREAL(SETDATA,NLINES,'CLKOW         ',SITNUM,INP,IZ,IZ,IZ,IZ)
!
!---- RBFF
!     CLBFF
      RBFF = GETREAL(SETDATA,NLINES,'CLBFF         ',SITNUM,INP,IZ,IZ,IZ,IZ)
!
!---- RBFI
!     CLBFI
      RBFI = GETREAL(SETDATA,NLINES,'CLBFI         ',SITNUM,INP,IZ,IZ,IZ,IZ)
!
!---- RBFV
!     CLBVLV
      RBFV = GETREAL(SETDATA,NLINES,'CLBVLV        ',SITNUM,INP,IZ,IZ,IZ,IZ)
!
!---- RBMT
!     CLFMT
      RBMT = GETREAL(SETDATA,NLINES,'CLFMT         ',SITNUM,INP,IZ,IZ,IZ,IZ)
!
!---- RBMK
!     CLFMK
      RBMK = GETREAL(SETDATA,NLINES,'CLFMK         ',SITNUM,INP,IZ,IZ,IZ,IZ)
!
!---- RWPF
!     CLWPF
      RWPF = GETREAL(SETDATA,NLINES,'CLWPF         ',SITNUM,INP,IZ,IZ,IZ,IZ)
      IF(RWPF.LE.0.) REPF = 1
!
!---- RVDP
!     CLVD
      RVDP = GETREAL(SETDATA,NLINES,'CLVD          ',SITNUM,INP,IZ,IZ,IZ,IZ)
!
!---- KRCLS
!     CLCLASS
      KRCLS = GETINT(SETDATA,NLINES,'CLCLASS       ',SITNUM,INP,IZ,IZ,IZ,IZ)
!
! ************************************************************************
!
!   PROCESS DATA FOR CURRENT CHAIN MEMBER
!
! ************************************************************************
 101  CONTINUE
      ANAME(1,IP)=PNAME(1:4)
      ANAME(2,IP)=PNAME(5:8)
      ANAME(3,IP)=PNAME(9:12)
      ANAME(4,IP)=PNAME(13:16)
      ANAME(5,IP)=PNAME(17:20)
      do 55 il=1,17
        fstchr = 1 + 4*(il-1)
        lstchr = 4 + 4*(il-1)
        ffname(il,ip) = fname(fstchr:lstchr)
 55   continue
!      WRITE(NELS,'(A,A/a,4a)') ' Name from GID :',fname,' Name set in chem_Data_Read: ',(ffname(il,ip),il=1,4)

      DO 60 IL=1,3
       CASID(IL,IP)=POLID(IL,IP)
 60   CONTINUE
      WM(IP)=RWM                ! Molecular weight
      CKOW(IP) = RCKOW          ! Kow
      BFF(IP)=RBFF*FISH_TISSUE  ! Water to finfish bioaccumulation factor   ! 26-Apr-01
      BFI(IP)=RBFI              ! Water to shellfish bioaccumulation factor
! -- Soil-to-plant transfer factor.  Convert from wet weight to dry weight basis
!                                  This is the original MEPAS assumption
      BFV(IP)=RBFV * 0.25       ! Soil to plant transfer factor, based on 75% water in plant
      BMT(IP)=RBMT              ! Feed to Meat transfer factor
      BMK(IP)=RBMK              ! Feed to Milk transfer
      WPF(IP)=RWPF              ! Water treatment purification factor
      VDEP(IP)=RVDP             ! Deposition velocity
      HLC(IP)=RHLC              ! Henry's Law constant
      IF(KRCLS.LE.2) KRCLS = 3  ! Fix deposition to a minimum of 3
      ICLS(IP)=KRCLS            ! Atmospheric class
!
!----- Set factors for Andelman indoor inhalation model implemented
!      through the shower inhalation pathway for volatile chemicals
!      and Ra222.
!
      ANDF(IP) = 0.0            ! Default for radionuclides and chemicals
      IF(ANDFO.GT.0.) ANDF(IP) = ANDFO
      IF(MTYPE(IP).GT.1) THEN   ! Chemicals
        IF(RHLC.GT.1.E-5.AND.RWM.LT.200.) ANDF(IP) = ANDFC  ! Set to default
      ELSE
        IF(POLID(1,IP).EQ.RN1.AND.POLID(2,IP).EQ.RN2) THEN
          ANDF(IP) = ANDFR
        ENDIF
      ENDIF
!
!----- Set deposition velocity if not given in chemlib.  Evaluate only if
!      values for Kow and HLC are given.
!
      IF(CKOW(IP).LE.0.) GO TO 80
      IF(RVDP.LE.0.) THEN
        IF(RHLC.GT.0.) VDEP(IP) = DEPV(RHLC,RCKOW)
      ENDIF
!
!----- Estimate biota transfer factors is values are not given in chemlib.
!      Use correlations with Kow.
!
      IF(RBFF.LE.0.) BFF(IP)=BFCAL(RCKOW)*FISH_TISSUE   ! 26-Apr-01
      IF(RBFI.LE.0.) BFI(IP)=BICAL(RCKOW)
      IF(RBFV.LE.0.) BFV(IP)=BVCAL(RCKOW)
      IF(RBMT.LE.0.) BMT(IP)=FMECAL(RCKOW)
      IF(RBMK.LE.0.) BMK(IP)=FMICAL(RCKOW)
  80  CONTINUE
      NF=NF+1
      IF(NF.GE.NUC) GO TO 2000
!
!---- End of loop on chain members for reading data from GID section CONi -----
!
  300 CONTINUE
!
! ************************************************************************
!    End of search loop for chain members
! ************************************************************************
      GO TO 2000
!
!---- Error condition while setting decay chains:
!
 994  WRITE(NELS,'(A,A)') ' Parent not found for chain member: ',FSTEST,' Searching for: ',PARENTID
      WRITE(NERR,'(A,A)') ' Parent not found for chain member: ',FSTEST,' Searching for: ',PARENTID
      GO TO 900
!
!---- Error condition while setting decay chains: three parents for one chain member
!     maximum allowed is 2
!
 995  WRITE(NELS,'(A,A)') ' More than two parents defined for a chain member:',FSTEST
      WRITE(NERR,'(A,A)') ' More than two parents defined for a chain member:',FSTEST
      GO TO 900
!
!-----  ALL DESIRED POLLUTANTS WERE NOT FOUND IN THE DATA BASE. ---------------
!
 996  WRITE(nels,1011) POLID(1:3,1)
      IF(IP.GT.0.AND.IP.LT.20) WRITE(NELS,1091) POLID(1:3,IP)
      WRITE(nerr,1011) POLID(1:3,1)                            ! write to error message file
 1011 FORMAT(/' Data base error, all constituents not found in GID'/'Parent is ',3a)
      IF(IP.GT.0.AND.IP.LT.20) WRITE(Nerr,1091) POLID(1:3,IP)  ! write to error message file
      IERR = IERR + 1
      GO TO 900
!
!-----  Invalid value for CLKTYPE ---------------------------------------------
!
  998 WRITE(nels,1012) ktype,FSCASID
      WRITE(nerr,1012) ktype,FSCASID       ! write to error message file
 1012 FORMAT(/' Data base error - invalid value for CLKTYPE =',I4,/  &
              ' Parent name is ',A)
      IERR = IERR + 1
      GO TO 900
  997 WRITE(nels,1014) NUMCON
      WRITE(nerr,1014) NUMCON        ! write to error message file
 1014 FORMAT(/' DATA BASE ERROR - NO CONSTITUENTS SPECIFIED IN GID, NUMCON =',I5)
      IERR = IERR + 1
      GO TO 900
 2000 CONTINUE
      npol = NUC
      WRITE(nels,2040)
2040  FORMAT(//'Num Constituent Names',' ------------------------------------------',&
             ' CASID  ')
      do 5999 ip = 1,npol
       WRITE(nels,1111) ip,(fFNAME(I,ip),I=1,15),(CASID(I,IP),I=1,3)
 1111   FORMAT(I2,2x,15A4,1x,3A4)
 5999 continue
!
!----- Print reports of library data if IPDAT > 0.
!
      IF(IPDAT.GT.0) THEN
      if( npol .le. 4 ) then
        newpol = 1
        iii = npol
      else if( npol .gt. 4 .and. npol .le. 8 ) then
        newpol = 2
        iii = npol - 4
      else if( npol .gt. 8 .and. npol .le. 12 ) then
       newpol = 3
       iii = npol - 8
      else if( npol .gt. 12 .and. npol .le. 16 ) then
       newpol = 4
       iii = npol - 12
      else if( npol .gt. 16 ) then
        newpol = 5
       iii = npol - 16
      end if
      do 2999 ichart = 1, newpol
       jj = newpol - ichart
       if ( jj .gt. 0.0 ) then
         nnn = ((ichart - 1) * 4) + 1
         jjj = 4 * ichart
       else
         nnn = ((ichart -1) * 4) + 1
         jjj = iii + ((ichart -1) * 4)
       end if
!-----
!----- writing out the constituent names
!-----
      do 4999 nml = nnn, jjj
        lll(nml) = nml
 4999   continue
        kkl = jjj - nnn
       if( ichart .eq. 1) then
         WRITE(nels,1001) 
       else
         WRITE(nels,1122) 
       endif
       if( kkl .eq. 0 ) then
         write(nels,3002) (lll(nml), nml=nnn,jjj)
         write(nels,'(41(''-''))')
 3002     format( '  Constituent   Parameter   ',5x,                 &
                    ('Constituent '),                                &
                   /,'   Parameter      Units         ',(5x,I2,5x) )
       else if( kkl .eq. 1 ) then
         write(nels,3003) (lll(nml), nml=nnn,jjj)
         write(nels,'(53(''-''))')
 3003     format( '  Constituent   Parameter   ',5x,                 &
                   2('Constituent '),                                &
                   /,'   Parameter      Units         ',2(5x,I2,5x) )
       else if( kkl .eq. 2 ) then
         write(nels,3004) (lll(nml), nml=nnn,jjj)
         write(nels,'(65(''-''))')
 3004     format( '  Constituent   Parameter   ',5x,                 &
                   3('Constituent '),                                &
                   /,'   Parameter      Units         ',3(5x,I2,5x) )
       else if( kkl .eq. 3 ) then
         write(nels,3005) (lll(nml), nml=nnn,jjj)
         write(nels,'(80(''-''))')
 3005     format( '  Constituent   Parameter   ',5x,                 &
                   4('Constituent '),                                &
                   /,'   Parameter      Units         ',4(5x,I2,5x) )
       end if
!-----
!----- writing out the constituent parameter chart(s)
!-----
       write(nels,2001) ((CASID(I,IP),I=1,2), ip=nnn,jjj)
 2001   format('ID / CAS #:      (None)         ',1p,4(2x,2a4,2x))
       write(nels,2002) (mtype(ip),ip=nnn,jjj)
 2002   format('Toxicity Type:   (None)         ',4(1x,I10,1x))
       write(nels,2003) (wm(IP), ip=nnn,jjj)
 2003   format('Mol. Weight:     g/mole         ',1p,4(1x,E10.3,1x))
       write(nels,2005) (hlc(ip), ip=nnn,jjj)
 2005   format('Henry"s Law Con: atm*m3/mole    ',1p,4(1x,E10.3,1x))
       write(nels,2008) (ckow(ip), ip=nnn,jjj)
 2008   format('Oct Wat Part:    (None)         ',1p,4(1x,E10.3,1x))
       write(nels,2018) (bff(ip), ip=nnn,jjj)
 2018   format('Fish Bioaccum:   (None)         ',1p,4(1x,E10.3,1x))
       write(nels,2019) (bfi(ip), ip=nnn,jjj)
 2019   format('Shell Bioaccum:  (None)         ',1p,4(1x,E10.3,1x))
       write(nels,2020) (bfv(ip), ip=nnn,jjj)
 2020   format('Veg Bioaccum:    (None)         ',1p,4(1x,E10.3,1x))
       write(nels,2021) (bmt(ip), ip=nnn,jjj)
 2021   format('Meat Trans Fact: day/Kg         ',1p,4(1x,E10.3,1x))
       write(nels,2022) (bmk(ip), ip=nnn,jjj)
 2022   format('Milk Trans Fact: day/L          ',1p,4(1x,E10.3,1x))
       write(nels,2023) (wpf(ip), ip=nnn,jjj)
 2023   format('Water Pur Fact:  (None)         ',1p,4(1x,E10.3,1x))
       write(nels,2024) (vdep(ip), ip=nnn,jjj)
 2024   format('Dep Velocity:    m/sec          ',1p,4(1x,E10.3,1x))
       write(nels,2025) (icls(ip), ip=nnn,jjj)
 2025   format('Atmos Dep Class: (None)         ',4(1x,I10,1x))   ! 5-Oct-94
       write(nels,2125) (ANDF(ip), ip=nnn,jjj)
 2125   format('Andelman Factor: (L/m^3)         ',1p,4(1x,E10.3,1x))
       write(nels,'(38(''*''),/,''KEY for Constituent Table'')')
       do 3999 ijk = nnn, jjj
         write(nels,2035)  ijk, (aname(i,ijk),i=1,5)
 2035     format('Constituent ',I2,' = ',5a4)
 3999   continue
 2999 continue
!
      ENDIF
 900  CONTINUE
      RETURN
!
!----- Format Statements -------------------------------------------------
!
 1000 FORMAT(I4,A80)
 1001 FORMAT(a1,14X,'Summary of Data from MEPAS Constituent Database'  &
            ,/,4x,A76,                                                 &
      /,10x,'This Database Contains Entries for ',I4,' Constituents'/)
 1122 FORMAT(a1,11X,'Summary of Data from MEPAS Constituent Database', &
                  ' (Cont)',/,4x,A76,                                  &
      /,10x,'This Database Contains Entries for ',I4,' Constituents'/)
 1016 FORMAT(/'POLLUTANT NAME            CP INH.     CP ING. ',        &
        '   RFD INH.    RFD ING.   ')
 1021 format(26x,'Kg*day/mg    Kg*day/mg   mg/Kg/day   mg/Kg/day')
 1020 format(24x,' 1/days       1/days      1/days      1/days',       &
       '      rem/pCi     rem/pCi')
 1006 FORMAT(1X,5A4,2X,1PE10.1,2X,5(E10.1,2X))
 1017 FORMAT(1X,5A4,3A4,I3,2X,1PE10.1,2X,5(E10.1,2X))
 1007 FORMAT(1X,5A4,1PE10.1,3X,4(E10.1,2X),5E10.1)
 1008 FORMAT(1X,5A4,1PE10.1,6E10.1,2X,I3)
 1091 FORMAT(' Progeny not found is ',3a)
 1015 FORMAT(/,' POLLUTANT NAME       ID/CAS NO.  TYPE    MOL. WT.  ',    &
        ' VAP PRES',                                                    &
       '    HENRYS C    WAT. SOL.      KOC         KOW')
 1019 FORMAT(41X,' g/mole     mm - Hg   atm-m3/mole     mg/L        L/g',&
       '    DIMENSIONLESS')
!
!================= END OF MODULE CHAIN_DATA_READ =============================
!
      END
!   MEPAS HAZ2: BFCAL.FOR             Version Date: 21-Jan-1994
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! *****************************************************************************
!                                                                            *
!                           FUNCTION BFCAL                                   *
!                                                                            *
!  FUNCTION BFCAL (Description of module from existing text)                 *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    01/19/89 (Converted to PC)                              *
!  Last Modified:    21-Jan-1994   DLS                                       *
!                                                                            *
! *****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: SUBROUTINE pdatin
!     Calls: NONE
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     28-JUL-92    DLS  Modification of heading information
!
!==== SUBROUTINE CALL ========================================================
!
      FUNCTION BFCAL(CKOW) 
!
!==== COMMON Block Definitions ===============================================
!
!     NONE
!
!==== DIMENSION Statements ===================================================
!
!     NONE
!
!==== Variable Declarations ==================================================
!
!     NONE
!
!==== DATA Statements ========================================================
!
!     NONE
!
! ****** BFCAL ESTIMATES WATER-TO-FISH BIOCONCENTRATION FACTOR, BF      *******C
!   CKOW IS THE OCTANOL-WATER PARTITION COEFFICIENT
!   Bintein, DeVillers, and Carcher, 1993, SAR and QSAR in Environmental
!   Research Vol. 1, pp 29-39
!   Correlation model changed 21-Jan-1994
      T1 = 0.910 * ALOG10(CKOW)
      T2 = 1.975 * ALOG10(6.8E-7 * CKOW + 1)
      T3 = 0.786
      BFLOG = T1 - T2 - T3
      BFCAL = 10.**BFLOG
      RETURN
!====== END OF MODULE BFCAL ===================================================
      END
!   MEPAS HAZ2: BICAL.FOR             Version Date: 11-24-1991
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           FUNCTION BICAL                                   *
!                                                                            *
!  FUNCTION BICAL (Description of module from existing text)                 *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    01/19/89 (Converted to PC)                              *
!  Last Modified:    30-July-1992  DLS                                       *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: SUBROUTINE...
!     Calls: NONE
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     28-JUL-92    DLS  Modification of heading information
!
!==== SUBROUTINE CALL ========================================================
!
      FUNCTION BICAL(CKOW)
!
!==== COMMON Block Definitions ===============================================
!
!     NONE
!
!==== DIMENSION Statements ===================================================
!
!     NONE
!
!==== Variable Declarations ==================================================
!
!     NONE
!
!==== DATA Statements ========================================================
!
!     NONE
!
! ****** BICAL ESTIMATES WATER-TO-INVERTEBRATE FACTOR, BI               *******C
!
!   CKOW IS THE OCTANOL-WATER PARTITION COEFFICIENT
!  CORRELATION FROM SOUTHWORTH (1978) eNVIRON. SCI. TECHNOL. 12.1062+
      BILOG = 0.819 * ALOG10(CKOW) - 1.146
      BICAL = 10.**BILOG
      RETURN
!===================== END OF MODULE BICAL ===================================
      END
!   MEPAS HAZ2: BVCAL.FOR             Version Date: 11-24-1991
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           FUNCTION BVCAL                                   *
!                                                                            *
!  FUNCTION BVCAL (Description of module from existing text)                 *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    01/19/89 (Converted to PC)                              *
!  Last Modified:    30-July-1992  DLS                                       *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: NONE
!     Calls: NONE
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     28-JUL-92    DLS  Modification of heading information
!
!==== SUBROUTINE CALL ========================================================
!
      FUNCTION BVCAL(CKOW)
!
!==== COMMON Block Definitions ===============================================
!
!     NONE
!
!==== DIMENSION Statements ===================================================
!
!     NONE
!
!==== Variable Declarations ==================================================
!
!     NONE
!
!==== DATA Statements ========================================================
!
!     NONE
!
! ****** BVCAL ESTIMATES SOIL-TO-PLANT UPTAKE FACTOR, BV, WET PLANT WT. ***C
!   CKOW IS THE OCTANOL-WATER PARTITION COEFFICIENT
!  FROM TRAVIS AND ARMS (1988) ENVIRON. SCI. TECHNOL., V22, #3, P271-274
      BVLOG = 0.986 - 0.578 * ALOG10(CKOW)
      BVCAL = 10.**BVLOG
      RETURN
      END
!========== END OF MODULE BVKAL ============================================
!   MEPAS HAZ2: DEPV.FOR             Version Date: 11-24-1991
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           FUNCTION DEPV                                    *
!                                                                            *
!  FUNCTION DEPV FUNCTION TO EVALUATE DEPOSITION VELOCITY FOR ORGANIC        *
!                CHEMICALS                                                   *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    01/19/89 (Converted to PC)                              *
!  Last Modified:    30-July-1992  DLS                                       *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: NONE
!     Calls: NONE
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!
!     28-JUL-92    DLS  Modification of heading information
!==== SUBROUTINE CALL ========================================================
!
      FUNCTION DEPV(HC,KOW)
!
!==== COMMON Block Definitions ===============================================
!
!     NONE
!
!==== DIMENSION Statements ===================================================
!
!     NONE
!
!==== Variable Declarations ==================================================
!
!     NONE
!
!==== DATA Statements ========================================================
!
!     NONE
!
!
!
      REAL KOW
!
!  EVALUATE EFFECTIV DEPOSTION VELOCITY (M/SEC)
!  Assumptions:  plant is 90% water and 10% organic
!                biomass is 2 kg/m2
!                air column is 1000 meters high
!  Empirical correction added 10/6/88 to better predict known values
!  correction factor = 1/(Vd/1E-3)**.25, >=1.
!
      REL = 0.9 + 0.1 * KOW
      DEN = HC * 1000. + 4.8E-5 * REL
      VD = 0.436681 * 1.1E-7 * REL / DEN
      CF = 1./(VD*1000.)**0.25
      IF(CF.LT.1.) CF = 1.0
      DEPV = CF * VD
!
      RETURN
      END
!========== END OF MODULE DEPV =======================================
!  MEPAS HAZ2: EXFCT.FOR             Version Date: 11-24-1991
!  Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ***************************************************************************
!                                                                           *
!                          FUNCTION EXFCT                                   *
!                                                                           *
! FUNCTION EXFCT Derived from a module of LADTAP II, LATEST                 *
!                  MODIFICATION - FEBRUARY 4, 1985                          *
!                  FUNCTION EXFCT EVALUATES THE EXPRESSION 1.-EXP(-X)       *
!                                                                           *
! Written by:       Dennis Strenge                                          *
!                   Battelle Pacific Northwest Laboratories                 *
!                   P.O. Box 999                                            *
!                   Richland, WA 99352                                      *
!                                                                           *
! Creation Date:    01/19/89 (Converted to PC)                              *
! Last Modified:    30-July-1992  DLS                                       *
!                                                                           *
! ***************************************************************************
!
!=== Modular Organization ===================================================
!
!    Module of: MEPAS/HAZ2
!    Called by: SUBROUTINE RISKF
!    Calls: NONE
!    Common blocks referenced: NONE
!
!=== Significant Parameter Designation and Description ======================
!
!    Parameter Set/
!    Name      Used   Type    Location  Parameter Description
!    --------- -----  ------  --------- -------------------------------------
!    EXFCT     S              FUNCTION  FUNCTION VALUE RETURNED TO
!                                       CALLING PROGRAM
!    X         U              ARGUMENT  ARGUMENT FOR EVALUATION OF THE
!                                       FUNCTION, DIMENSIONLESS
!
!=== Modification History ===================================================
!
!    Date         Who  Modification Description
!    --------     ---  ------------------------------------------------------
!    28-JUL-92    DLS  Modification of heading information
!
!=== SUBROUTINE CALL ========================================================
!
      FUNCTION EXFCT (X) 
!
!==== COMMON Block Definitions ===============================================
!
!     NONE
!
!==== DIMENSION Statements ===================================================
!
!     NONE
!
!==== Variable Declarations ==================================================
!
!     NONE
!
!==== DATA Statements ========================================================
!
!     NONE
!
!
!----- START CALCULATIONS, SET DEFAULT FUNCTION VALUE
!
      EXFCT=1.                                                      
!
!     ---- FOR ARGUMENT >= 30. RETURN A VALUE OF 1.0
!
      IF (X.GE.30.) RETURN                                          
      IF (X.LE.0.01) GO TO 1                                        
!
!     ---- ARGUMENT IS BETWEEN 0.01 AND 30., EXP MAY BE USED
!
      EXFCT=1.0-EXP(-X)                                             
      RETURN                                                        
!
!     ---- FOR ARGUMENT <= 0.01 USE APPROXIMATION FOR 1-EXP(-X)
!
  1   IF(X.LT.1.E-6) THEN
        EXFCT = X
      ELSE
        X2=X*X                                                      
        X3=X*X2                                                     
        X4=X*X3                                                     
        EXFCT=X+X3/6.0-X2/2.0-X4/24.0                               
      ENDIF
      RETURN                                                        
!
!============ END OF MODULE EXFCT ============================================
      END                                                           

!   MEPAS HAZ2: FMECAL.FOR             Version Date: 11-24-1991
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           FUNCTION FMECAL                                  *
!                                                                            *
!  FUNCTION FMECAL (Description of module from existing text)                *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    01/19/89 (Converted to PC)                              *
!  Last Modified:    30-July-1992  DLS                                       *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: NONE
!     Calls: NONE
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     28-JUL-92    DLS  Modification of heading information
!
!==== SUBROUTINE CALL ========================================================
!
      FUNCTION FMECAL(CKOW)
!
!==== COMMON Block Definitions ===============================================
!
!     NONE
!
!==== DIMENSION Statements ===================================================
!
!     NONE
!
!==== Variable Declarations ==================================================
!
!     NONE
!
!==== DATA Statements ========================================================
!
!     NONE
!
!
! ****** FMECAL ESTIMATES ANIMAL FEED-TO-MEAT UPTAKE FACTOR, FME ******C
!        (WET MEAT/WET FEED)
!  CKOW IS THE OCTANOL-WATER PARTITION COEFFICIENT
!  FROM TRAVIS AND ARMS (1988) ENVIRON. SCI. TECHNOL., V22, #3, P271-274.
      FMELOG = -7.60 + ALOG10(CKOW)
      FMECAL = 10.**FMELOG
      RETURN
      END
!========================== END OF MODULE FMECAL =============================C
!  MEPAS HAZ2: FMICAL.FOR             Version Date: 11-24-1991
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           FUNCTION FMICAL                                  *
!                                                                            *
!  FUNCTION FMICAL (Description of module from existing text)                *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    01/19/89 (Converted to PC)                              *
!  Last Modified:    30-July-1992  DLS                                       *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: NONE
!     Calls: NONE
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     28-JUL-92    DLS  Modification of heading information
!
!==== SUBROUTINE CALL ========================================================
!
      FUNCTION FMICAL(CKOW)
!
!==== COMMON Block Definitions ===============================================
!
!     NONE
!
!==== DIMENSION Statements ===================================================
!
!     NONE
!
!==== Variable Declarations ==================================================
!
!     NONE
!
!==== DATA Statements ========================================================
!
!     NONE
!
!
! ****** FMICAL ESTIMATES ANIMAL FEED-TO-MILK UPTAKE FACTOR, FMI ******C
!  CKOW IS THE OCTANOL-WATER PARTITION COEFFICIENT
!  FROM TRAVIS AND ARMS (1980) ENVIRON. SCI. TECHNOL. V22, #3, P172-274.
      FMILOG = -8.10 +  ALOG10(CKOW)
      FMICAL = 10.**FMILOG
      RETURN
      END
!================= END OF MODULE FMICAL ======================================
!   MEPAS HAZSIF: GROUND.FOR             Version Date: 09-Apr-97
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE GROUND                                *
!                                                                            *
!  Subroutine GROUND controls exposure pathway calculations for the          *
!                    groundwater transport pathway.  GROUND returns hazard   *
!                    potential index values for the current usage location   *
!                    for each pollutant.                                     *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    09/19/85 (Converted to PC)                              *
!  Last Modified:    09-Apr-1997       DLS                                   *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZ
!     Called by: SUBROUTINE EXPOS
!     Calls: SUBROUTINES CROPS, DRINK, RISKF
!     Common blocks referenced: AGCLAS, GWPATH, PSET1, PTHUSE,
!              RISK, DEVICE, MASBAL
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     variable    S    CHAR    Argument  nnnnnnnnnnnnnnnnnnnnnn
!                 U    INT     Internal
!                 S/U  REAL    Common
!                      DBLE    External
!    IPOL         U    Int     Argument  position of chain member
!==== Modification History ===================================================
!
!    Date     Who  Modification Description
!  ---------  --- ------------------------------------------------------
!  07-Nov-95  DLS  Created from HAZ2 version of GROUND.FOR
!  02-Apr-10  JGD  Code cleaned

!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE GROUND(IT,IPOL,CW,CS)
!
!==== COMMON Block Definitions ===============================================
!
        USE Crop_Data_Mod
        INCLUDE 'CRPATH.FTN'
        include 'AGCLAS.FTN'
        include 'GWPATH.FTN'
        include 'PFLAGS.FTN'
        include 'PSET1.FTN'
        INCLUDE 'PSET3.FTN'
        include 'PTHUSE.FTN'
        include 'DEVICE.FTN'
        INCLUDE 'DERMDAT.FTN'
        INCLUDE 'ETIMES.FTN'
        INCLUDE 'CRLOC.FTN'
        include 'tsoil.ftn'
        INCLUDE 'DECAY.FTN'
        INCLUDE 'DEL.FTN'
!
!==== DIMENSION Statements ===================================================
!
      DOUBLE PRECISION, DIMENSION (20) :: CW, CS, APF, AB
      DIMENSION :: CWD(20) , CDKW(20)
      DIMENSION :: exout(12)
      REAL(KIND=4), DIMENSION(16) :: CROPOUT
      INTEGER :: MEDIUM
!     POSITION IN EXOUT(1) = DRINKING WATER
!                      (2) = SHOWER DERMAL
!                      (3) = SHOWER INGESTION
!                      (4) = LEAFY VEG. INGESTION
!                      (5) = OTHER VEG. INGESTION
!                      (6) = MEAT INGESTION
!                      (7) = MILK INGESTION
!                      (8) = SHOWER INHALATION
!                      (9) = SOIL INGESTION
!                      (10) = SOIL DERMAL
!                      (11) = SOIL RESUSPENSION INHALATION
!                      (12) = SOIL EXTERNAL
!==== Variable Declarations ==================================================
!
!     NONE
!
!==== DATA Statements ========================================================
!     The value for CNST is 1 / (RT) = 1 / (8.2E-5 * 310)
!
      DATA CNST/3.93E1/
      DATA C10E3/1.E3/     ! Constant added for shower inhalation model
      DATA ALPL/0.049511/  ! plant weathering rate constant, 1/d
!
!--- Start of Calculations ----------------------------------------------
!
        IPATH = 1
        IP = IPOL             ! IPOL is ALWAYS = 1 in current MEPAS implementation
        CWA = CW(1)
        CWS = CS(1)
        MEDIUM = 3            ! Medium 3 is water for groundwater media calculations
!        IF(MTYPE(IPOL).GT.1) THEN         ! CHEMICALS      ! ** Re-activate for Chem Decay
!           NUC = NUCC(MEDIUM)                              ! ** Re-activate for Chem Decay
!        ELSE                                               ! ** Re-activate for Chem Decay
!           NUC = NUC                      ! RADIONUCLIDES  ! ** Re-activate for Chem Decay
!        ENDIF                                              ! ** Re-activate for Chem Decay
!
!---   Initialize ingestion intake array -------------------------------
!
       DO I = 1,20
         AB(I) = DBLE(ALPL)
         CWD(I) = CW(I)
         DO IE = 1,25
           DEL(IE,I) = 0.
         END DO
       END DO
!---   Initialize new output array for temporary results to ELS file.
       DO I = 1,12
          EXOUT(I) = 0.
       END DO
!
!--- Calculate ingestion uptake from drinking water for this pollutant --
!   include contribution from ingestion and inhalation while showering -
      IF(KGDR.GT.0) THEN
        ITYPE = 1
        CALL DRINK(IPATH,IPOL,CWD,CDKW,CHEM)
!
!----- Test for shower ingestion pathway --------------------------------
!                 **!**  modify for chemical decay
        IF(KEXPTH(3).GT.0.AND.KGDR.LT.3) THEN
!          IF(MTYPE(IPOL).LE.1) THEN         ! ** For chemical decay, deactivate IF, use same code
            DO IE = 1,NUC                   ! ** for chemicals and radionuclides
              DEL(3,IE) = CDKW(IE)                             ! Inadvertent ingest.
            END DO
!          ELSE                              ! ** deactivate
!            DEL(3,1) = CDKW(1)              ! ** deactivate       ! Inadvertent ingest.
!            EXOUT(3) = DEL(3,1)             ! ** deactivate
!          ENDIF                             ! ** deactivate
!
        ENDIF
!
!----- Test for dermal absorption while showering ----------------------
!                 **!**  modify for chemical decay
        IF(KEXPTH(2).GT.0.AND.KGDR.LT.3) THEN
!          IF(MTYPE(IPOL).LE.1) THEN         ! ** For chemical decay, deactivate IF, use same code
            DO IE = 1,NUC                   ! ** for chemicals and radionuclides
              DEL(2,IE) = CDKW(IE)                                    
            END DO
!          ELSE                              ! ** deactivate
!            DEL(2,1) = CDKW(1)              ! ** deactivate
!            EXOUT(2) = DEL(2,1)             ! ** deactivate
!          ENDIF                             ! ** deactivate
        ENDIF
!
!----- Test for inhalation while showering for volatile chemicals ---------
!      Pathway 17 is "Showering inhalation"
!      Pathway 25 is "Indoor inhalation", EPA Andelman model
!      Note: parent Henry's Law Constant (HLC) used for all chain members
!
         IF(KEXPTH(17).GT.0.AND.KGDR.LT.3) THEN  ! Original MEPAS shower inh. model
            HC=HLC(IPOL)
            IF(HC.GT.2.4E-3) HC=2.4E-3
!            IF(MTYPE(IPOL).LE.1) THEN              ! ** deactivate! **  modify for chemical decay
             DO IE = 1,NUC
              DEL(17,IE)=CDKW(IE)*HC*CNST*C10E3    !Rads       C10E3 added 27-Mar-2000
             END DO
!            ELSE                                   ! ** deactivate
!              DEL(17,1) = CDKW(1)*HC*CNST*C10E3    ! ** deactivate!Chemicals C10E3 added 27-Mar-2000
!              EXOUT(8) = DEL(17,1)                 ! ** deactivate
!            ENDIF                                  ! ** deactivate
        ENDIF
!
!------ Test for indoor inhalation (KEXPTH(17)= 2) ---------------------------
!       Note: chain member Andelman factor (ANDF) used for each chain member
!
        IF(KEXPTH(25).GT.0.AND.KGDR.LT.3) THEN    ! EPA Andelman indoor inhalation model
!            IF(MTYPE(IPOL).LE.1) THEN             ! ** deactivate! **!   modify for chemical decay
               DO IE = 1,NUC 
                 IF(ANDF(IE).GT.0.) DEL(25,IE) = CDKW(IE) * ANDF(IE) !Rads
               END DO
!            ELSE                                  ! ** deactivate
!              IF(ANDF(IPOL).GT.0.) THEN           ! ** deactivate
!                 DEL(25,1) = CDKW(1) * ANDF(IPOL) ! ** deactivate!Chemicals
!                 EXOUT(8) = DEL(25,1)             ! ** deactivate
!              ENDIF                               ! ** deactivate
!            ENDIF                                 ! ** deactivate
        ENDIF
!
!----- Test for drinking water ingestion ----------------------------------
!
          IF(KEXPTH(1).GT.0) THEN
            IF(KGDR.GT.1) THEN
 !             IF(MTYPE(IPOL).LE.1) THEN         ! ** deactivate, chem decay same as rad decay
                DO IE = 1,NUC
                  DEL(1,IE)=CDKW(IE)
                END DO
 !             ELSE                                ! ** deactivate
 !               DEL(1,1) = CDKW(1)                ! ** deactivate
!                EXOUT(1) = DEL(1,1)               ! ** deactivate
!              ENDIF                               ! ** deactivate
            ENDIF
         ENDIF
      ENDIF
!
!--- Calculate ingestion uptake from irrigated crops for each pollutant --
!
!      WRITE(6,*)it,cw(1),cs(1),apf(1),ab(1)
      IF(KGIR.NE.0) THEN
        I4=4
        IAD=KGIR
        do IC = 1,4
           DO Ix = 1,4
              CROP_FACTORS(IC,IX) = 0.0
           END DO
        end do
        DO 40 IC = I4,7
!
!----- Test exposure pathway flags for food ingestion -------------------
!
          IF(KEXPTH(IC).LE.0) GO TO 40
            EXOUT(9) = CWS/DEN
            IC3 = IC - 3
            APF(1) = 1.0D0
            AB(1) =  DBLE(ALPL)
            IF(MTYPE(IPOL).GT.1) AB(1) = DBLE(ALPL) + ALAM(1)
            IF(NUC.GT.1) THEN
              DO IE = 2,NUC
                AB(IE) =  DBLE(ALPL)
                APF(IE) = 0.0D0
                IF(MTYPE(IPOL).GT.1) AB(IE) = DBLE(ALPL) + ALAM(IE)
              END DO
            END IF
            INTGRL = 1                            ! AB is plant loss constant (14 day half time)
!            IF(MTYPE(IPOL).GT.1) AL(1) = ALAM(1)  ! **!   modify for chemical decay
            TIM = TGRW(IC3,1)         ! TIM in units of days for CHAIN
            tirr = 365.25 * firr(1)   ! Irrigation period in days
            IF(tim.gt.tirr) then  ! if irrigation occurs less than growing period
              tim = tirr          ! then adjust deposition time to equal irrigation
            endif
            IF(tim.gt.0.) then    ! do crop calculation only if irrigation occurs
! **
! **  do decay for radionuclides and chemicals
! **
                 CALL CHAIN(TIM,AB,APF,APF,INTGRL,CHEM)          ! Updated 15 November 2004, DL Strenge
                 DO IE = 1,NUC
                   APFACT(IE) = APF(IE)  ! APF units are days
                 END DO
!                 IF(MTYPE(IPOL).LE.1) THEN   ! Radionuclides ! **! (delete line)  modify for chemical decay
                   DO IE = 1,NUC
                     CALL LAMAX(IPOL,IE,ISMX,IGMX)
                     CWS = CS(IE)
                     CALL CROPS(IPATH,IPOL,IAD,IC3,CWA,CWS,CWS,CROP,IPOL,IE,ISMX,IGMX)
                     DEL(IC,IE)= CROP
                   END DO
!                 ELSE      ! Chemicals  ! **! (delete line)  modify for chemical decay, make like rads
!                   CALL CROPS(IPATH,IPOL,IAD,IC3,CWA,CWS,CWS,CROP,IPOL,1,IPOL,IPOL)  ! **! (delete line)
!                   DEL(IC,IP)=CROP                                                   ! **! (delete line)
!                   EXOUT(IC) = CROP                                                  ! **! (delete line)
!                 ENDIF                        ! **! (delete line)
            else    ! IF TIM = 0, SET RESULTS TO ZERO (NO IRRIGATION)
              DO IE = 1,NUC
                DEL(IC,IE) = 0.0
              END DO
            endif
   40    CONTINUE
      ENDIF
!
!--- Add analysis for soil related pathways here
!
!--- Write results of media concentrations to ELS file in csv format
!
!      WRITE(nels,100) (exout(i),i=1,12),(CROP_FACTORS(1,I),I=1,4),(CROP_FACTORS(2,I),I=1,4), &
!                     (CROP_FACTORS(3,I),I=1,4),(CROP_FACTORS(4,I),I=1,4)
! 100  FORMAT(1P,E10.2,',',40(E10.2,','))
      DUROUT = DGWED
      CALL EPFDAT()
!
      RETURN
!=================== END OF MODULE GROUND ===============================
      END
!   MEPAS AHAZ: EXPOS.FOR             Version Date: 08-Apr-97
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE EXPOS                                 *
!                                                                            *
!  Subroutine EXPOS controls calculation of hazard potential index values    *
!                   for one transport pathway and one usage location per     *
!                   call.  Values are calculated for one pollutant.          *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    04/01/86 (Converted to PC)                              *
!  Last Modified:    08-Apr-97 DLS                                           *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZ
!     Called by: SUBROUTINE HAZ
!     Calls: SUBROUTINES SRETEN, GROUND, SURFWT, ATMOS
!     Common blocks referenced: DEVICE, PSET1, PSET3, TSOIL
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     variable    S    CHAR    Argument  nnnnnnnnnnnnnnnnnnnnnn
!                 U    INT     Internal
!                 S/U  REAL    Common
!                      DBLE    External
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!    02-Apr-10  JGD  Code cleaned
!
!
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE EXPOS(IPATH,ILOC,IPOL,ITIM,CW,CA,CS,DELT,IERR)
!
!==== COMMON Block Definitions ===============================================
!
        include 'ATPATH.FTN'
        include 'GWPATH.FTN'
        include 'SWPATH.FTN'
        include 'SLPATH.FTN'
        include 'DEVICE.FTN'
        include 'TSOIL.FTN'
        include 'PSET1.FTN'
        include 'PSET3.FTN'
        include 'DECAY.FTN'
        include 'LEACH.FTN'
!
!==== DIMENSION Statements ===================================================
!
      DIMENSION TFS(20), AB(20)
!
!==== Variable Declarations ==================================================
!
      DOUBLE PRECISION, DIMENSION (20) :: CW, CA, CS
!
!==== DATA Statements ========================================================
!
      DATA C365/365.25/
!
!   START OF CALCULATIONS
!   INITIALIZE PARAMETERS
!
!
      TFS(1) = 1.0
      AB(1) = 0.0
      DO IE = 2,20
        TFS(IE) = 0.0
        AB(IE)  = 0.0
      END DO
!
!   TEST INDEX VALUES
!      NPER:  1 <= NPER <= 1500
      IF(ITIM.LT.1.OR.ITIM.GT.14286) GO TO 197
!      IPATH:  1 <= IPATH <= 7
      IF(IPATH.LT.1.OR.IPATH.GT.7) GO TO 197
!      IPOL:  1 <= IPOL <= 20
      IF(IPOL.LT.1.OR.IPOL.GT.20) GO TO 197
!
!--- For water and air pathways evaluate soil retention factor TFSOIL for
!    each pollutant and progeny, using decay constant for soil, SLAM, and
!    leach rate constant for agricultural soil, ALEACH.
!
!  Set exposure duration for SRETEN evaluations by transport route
! (replace use of DELT with TEX)
!
          TEX = DELT
          IF(IPATH.EQ.1) TEX = DGWED
          IF(IPATH.EQ.2) TEX = DSWED
          IF(IPATH.EQ.3) TEX = ATED
          IF(IPATH.EQ.4) TEX = ATED
          IF(IPATH.LE.4) THEN
!
!---  Set soil retention factors for each chain member.  Not needed for measured soil.
!
            DO IE=1,NUC                                   !   For chemicals use CHEMLIB and
               AB(IE) = ALEACH(IE)                        !   add ALEACH and SLAM for each
               IF(MTYPE(IE).GT.1) AB(IE) = ALEACH(IE) + SLAM(IE)
            END DO                                        !   chain member (as is done now
            IF(TEX.GE.0.) CALL SRETEN(TEX,AB(1),TFS)      !   for the chemical as a parent)
            DO IE = 1,NUC                                 !
               TFSOIL(IE) = TFS(IE)                       !
               IF(IPATH.EQ.3.OR.IPATH.EQ.4) THEN          !
                  ASOIL(IE) = TFS(IE)/C365                !
               ENDIF                                      !
            END DO                                        !
!
!            IF(MTYPE(1).EQ.1) THEN      ! **               !   modify for chemical decay
!              DO IE=1,NUC                                  !   For chemicals use CHEMLIB and
!                AB(IE) = ALEACH(IE)                        !   add ALEACH and SLAM for each
!              END DO                                       !   chain member (as is done now
!              IF(TEX.GE.0.) CALL SRETEN(TEX,AB(1),TFS)     !   for the chemical as a parent)
!              DO IE = 1,NUC                                !
!                TFSOIL(IE) = TFS(IE)                       !
!                IF(IPATH.EQ.3.OR.IPATH.EQ.4) THEN          !
!                  ASOIL(IE) = TFS(IE)/C365                 !
!                ENDIF                                      !
!              END DO                                       !
!            ELSE                                           !
!              AB(1) = ALEACH(1) + slam(1)                  !
!              IF(TEX.GE.0.) CALL SRETEN(TEX,AB(1),TFS)     !
!              TFSOIL(1) = TFS(1)                           !
!              ASOIL(1) = TFS(1)/C365                       !
!            ENDIF                       ! **               !   modify for chemical decay
!
          ENDIF
!
!----- EVALUATE EXPOSURE (INTAKE) FOR CURRENT USAGE LOCATION PER INDIVIDUAL
!
!----- GROUND WATER TRANSPORT PATHWAY: IPATH = 1
!
      IF(IPATH.EQ.1) CALL GROUND(ITIM,IPOL,CW,CS)
!
!----- SURFACE WATER TRANSPORT PATHWAY: IPATH = 2
!
      IF(IPATH.EQ.2) CALL SURFWT(ITIM,IPOL,CW,CS,TEX)
!
!----- REGIONAL ATMOSPEHERIC TRANSPORT PATHWAY: IPATH = 3
!
      IF(IPATH.EQ.3) CALL ATMOS(ITIM,IPOL,CW,CA,CS)
!
!----- IF LOCAL SOIL INTAKE PATHWAY: IPATH = 5
!
      IF(IPATH.EQ.5) THEN
         IF(SOILMOD) THEN                 ! Original option, use MEPAS soil model
            CALL SOILP(ITIM,IPOL,CS)
         ELSE                             ! New option, use SCF data vs time
            CALL SOILPT(ITIM,IPOL,CS)
         ENDIF
      ENDIF
!
!----- INGESTION OF MEASURED FOODS:  IPATH = 6
!
!      IF(IPATH.EQ.6) CALL FOODIN(ITIM,IPOL,CA)
!
!----- DIRECT EXTERNAL DOSE WITH MEASURED DOSE RATES: IPATH = 7
!
!      IF(IPATH.EQ.7) CALL DIRDOS(ITIM)
!
      RETURN
!
!----- PRINT ERROR MESSAGE FOR BAD PARAMETER VALUE
!
  197 WRITE(6,1000) IPATH,IPOL
      IERR = 1
      RETURN
!
!----- Format Statement -------------------------------------------------------
!
 1000 FORMAT(1H1,'Error in Subroutine EXPOS,'/&
                 '  Bad value for IPATH, OR IPOL, =',3I10)
!
!=============== END OF MODULE EXPOS ===========================================
!
      END
!   MEPAS HAZ2: ARETEN.FOR             Version Date: 11-24-1991
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE ARETEN                                *
!                                                                            *
!  Function ARETEN calculates the soil accumulation factor for the air       *
!      exposure pathway.  The factor is to include the total uptake by       *
!      ingestion over a 70 year period from one year of deposition.          *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    09/24/85 (Converted to PC)                              *
!  Last Modified:    30-July-1992  DLS                                       *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: SUBROUTINE EXPOS
!     Calls: NONE
!     Common blocks referenced:
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter      Set/
!     Name           Used  Type  Location  Parameter Description
!     -------------- ----  ----  --------  ------------------------------
!
!     AFS             S    Real  Argument  Soil retention factor
!
!     ALD             U    Real  Argument  Physical decay constant for the
!                                         current pollutant, days-1.  This
!                                         is the input value.
!     ALY            S/U   Real  Internal  Same as ALD in units of year-1.
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     02-Apr-10    JGD  Code cleaned
!
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE ARETEN(ALD,AFS)
!
!==== COMMON Block Definitions ===============================================
!
!     NONE
!
!==== DIMENSION Statements ===================================================
!
!     NONE
!
!==== Variable Declarations ==================================================
!
!     NONE
!
!==== DATA Statements ========================================================
!
!     NONE
!
!
!--- Start of calculations ----------------------------------------------
!
      AR = 1.0
      ALY = ALD * 365.25
!
!--- Loop over the remaining 69 years -----------------------------------
!
      DO 10 N = 1,69
      ARG = ALY * FLOAT(N)
      IF(ARG.GT.16) GO TO 20
      AR = AR + EXP(-ARG)
   10 CONTINUE
   20 AFS = AR / 70.
      RETURN
!
!======== END OF MODULE ARETEN ===========================================
      END
!
!   MEPAS HAZ2: GDK.FOR             Version Date: 11-24-1991
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE GDK                                   *
!                                                                            *
!  Subroutine GDK calculates the deposition decay factor for a period of T   *
!                 days and returns a decay factor for each pollutant in      *
!                 array GDKFAC.                                              *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    09/19/91 (Converted to PC)                              *
!  Last Modified:    30-July-1992  DLS                                       *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: Main Program
!     Calls: NONE
!     Common blocks referenced: PSET1, PSET3, DEVICE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     GDKFAC(ip)  S    REAL    Argument  Decay correction factor for dep-
!                                        osition to ground for a period of
!                                        T days.  Units are years.
!
!     NPOL        U    INT.    PSET1     Number of pollutant considered.
!
!     SLAM(ip)    U    REAL    PSET3     Decay constant for each pollutant
!                                        in soil, 1/days.
!
!     T           U    REAL    Argument  Time period over which deposition is
!                                        accumulated and decayed, days.
!
!     YPD         S/U  REAL    Internal  Units conversion factor, year/day.
!
!                 U    INT     Internal
!                 S/U  REAL    Common
!                      DBLE    External
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!    02-Apr-10     JGD  Code cleaned
!
!
!
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE GDK(T,GDKFAC)
!
!==== COMMON Block Definitions ===============================================
!
        include 'PSET1.FTN'
        include 'PSET3.FTN'
        include 'DEVICE.FTN'
!
!==== DIMENSION Statements ===================================================
!
      DIMENSION GDKFAC(20)
!
!==== Variable Declarations ==================================================
!
!    NONE
!
!==== DATA Statements ========================================================
!
!   Number of terms in exponential approximation
      DATA NTERMS/11/
!   Argument test limit
      DATA XXX/0.001/
!   Years per day
      DATA YPD/0.00273785/
!
!----- Start calculations. Initialize daily intake array ----------------
!
!   Loop over number of pollutants in input list.
!
      DO 1001 IP = 1, NPOL
!
!   Test for non-zero value of SLAM(IP) to avoid division by zero.  If zero,
!   then set GDKFAC to zero and write warning message.
        IF(SLAM(IP).LE.0.) THEN
          GDKFAC(IP) = 0.0
          WRITE(nels,1000) IP, SLAM(IP)
 1000     FORMAT(' Invalid value for soil decay constant in GDK',&
                 ' for pollutant',I3,1PE10.3)
        ENDIF
!
!  Test magnitude of argument to determine method of analysis.
!
        ARG = T * SLAM(IP)
        IF(ARG.LT.XXX) THEN
!
!  For small arguments, use special equation to avoid loss of
!  significant figures.
!
          SUM = 0.5
          DENOM = 2.0
          DO 1002 IT = 3,NTERMS
            DENOM = DENOM*FLOAT(IT)
            FAC = -1./DENOM
            SUM = SUM + FAC * ARG
1002      Continue
           GDKFAC(IP) = SUM * T * YPD
        ELSE
!
!  For larger arguments, use standard equation.
          OME = 1. - EXP(-ARG)
          TERM = OME/ARG
          GDKFAC(IP) = (1.- TERM)/SLAM(IP) * YPD
        ENDIF
!
!  End of pollutant loop.
1001  Continue
      RETURN
      END
!
!================= END OF MODULE GDK =====================================
!
!   MEPAS HAZ2: SOILP.FOR             Version Date: 08-Jan-96
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE SOILP                                 *
!                                                                            *
!  Subroutine SOILP This subroutine calculates HPIL values for ingestion of  *
!                   local soil for pathway 5.                                *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    01/19/89 (Converted to PC)                              *
!  Last Modified:    08-Jan-96  DLS                                          *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: NONE
!     Calls: SUBROUTINES RISKF, MAXIN
!     Common blocks referenced: RISK, SLPATH
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!    02-Apr-10     JGD  Code cleaned
!
!
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE SOILP(IT,IPOL,CS)
!
!==== COMMON Block Definitions ===============================================
!
      include  'DERMDAT.FTN'
      INCLUDE  'DEVICE.FTN'
      INCLUDE  'ETIMES.FTN'
      include  'PFLAGS.FTN'
      INCLUDE  'PSET1.FTN'
      INCLUDE  'PSET3.FTN'
      INCLUDE  'PTHUSE.FTN'
      include  'SLPATH.FTN'
      INCLUDE  'LEACH.FTN'
      include  'decay.ftn'
      INCLUDE  'DEL.FTN'
!
!==== DIMENSION Statements ===================================================
!
      DIMENSION :: EX1C(20)
      DOUBLE PRECISION, DIMENSION (20) :: AOU,CS
      DOUBLE PRECISION, DIMENSION (20) :: AB
!
!==== Variable Declarations ==================================================
!
!     NONE
!
!==== DATA Statements ========================================================
!
      DATA C365/365.25/,C10E3/1.E3/
!
      IPATH = 5
      DO IC = 1,20
        DO IE = 1,24
           DEL(IE,IC) = 0.0
        END DO
      END DO
      CZ = 0.0
      IP = IPOL
      AB(1) = 0.0D0
      DO IE = 2,20
        AB(IE)  = 0.0D0
      END DO
!!
!----- Set time-integral factor to account for loss from soil layer by
!      decay and volatilization during exposure duration.
!
!  For chemical pollutants and radionuclides without progeny, use the initial
!  expression for decay accumulation
!
      DO IE = 1,NUC
         EX1C(IE) = 0.0
      END DO
! ** Updated 15 November 2004, DL Strenge
!      IF(MTYPE(IP).NE.1.OR.NUC.LE.1) THEN      ! **                  !   modify for chemical decay
!        ARG = SMED * (SLAM(1)+ALEACH(1)) * C365   ! 4-Jan-96         !
!        EX1 = 1./ ARG                                  ! 24-Oct-94   !
!        IF(ARG.GT.50.) ARG = 0.                                      !
!        IF(ARG.GT.0.) EX1 = EXFCT(ARG)/ ARG            ! 24-Oct-94   !
!        EX1C(1) = EX1 * CS(1)                                        !
!      ELSE     ! For radionuclides with progeny                      !
!
!---  Set soil retention factors for each chain member.
!
            DO IE=1,NUC                                   !   For chemicals use CHEMLIB and
               AB(IE) = DBLE(ALEACH(IE))                        !   add ALEACH and SLAM for each
               IF(MTYPE(IE).GT.1) AB(IE) = DBLE(ALEACH(IE) + SLAM(IE))
            END DO                                        !   chain member (as is done now
!
!---- Evaluate time integral of soil concentration with leaching and physical (for chemicals)
!
      INTGRL = 1                                                   !
      TIM = SMED * C365              ! TIM units are days          !
      WRITE(NELS,*) ' In SOILP, TIM,AB(1),ALEACH(1),SLAM(1)=', TIM,AB(1),ALEACH(1),SLAM(1)
      CALL CHAIN(TIM,AB,CS,AOU,INTGRL,CHEM)                         !  for radionuclides and chemicals
!      CALL CHAIN(TIM,ALEACH,CS,AOU,INTGRL)                         !  for radionuclides and chemicals
      EX1 = AOU(1)/(SMED*C365)                                     !
      EX1C(1) = EX1                                                !
      DO IE = 2,NUC                                                !
         EX1C(IE) = AOU(IE)/(SMED*C365)                             !
      END DO                                                       !
      WRITE(NELS,*) ' In SOILP, AOU(1),CS(1), EX1C(1)=', AOU(1), CS(1), EX1C(1)
!
!----- Daily intake from inadvertent soil ingestion ----------------------
!
      IF(KEXPTH(14).GT.0) THEN
!        IF(MTYPE(1).LE.1) THEN         ! Radionuclides
          DEL(14,1) = EX1C(1)
          IF(NUC.GT.1) THEN   ! Add progeny contribution
            DO IE = 2,NUC
              DEL(14,IE) =  EX1C(IE)
            END DO
          ENDIF
!        ELSE
!          DEL(14,1) = EX1C(1)
!        ENDIF
      ENDIF
!
!----- Daily intake from contact with soil, dermal absorption ------------
!
      IF(KEXPTH(15).GT.0) THEN
!
         DEL(15,1) =  EX1C(1)
         IF(NUC.GT.1) THEN   ! Add progeny contribution
            DO IE = 2,NUC
               DEL(15,IE) =   EX1C(IE)
            END DO
         ENDIF
!       ELSE                          ! Chemicals
!         DEL(15,1) = EX1C(1)
!       ENDIF
      ENDIF
!
!----- Evaluate soil resuspension inhalation, mass loading model
!
      IF(KEXPTH(19).GT.0) THEN
!
          DEL(19,1) = EX1C(1) * CML
          IF(NUC.GT.1) THEN   ! Add progeny contributions
            DO IE = 2,NUC
              DEL(19,IE) = EX1C(IE) * CML
            END DO
         ENDIF
!
      ENDIF
!
!----- Evaluate soil external dose ------------------------------
!
      IF(KEXPTH(23).GT.0.AND.MTYPE(IP).EQ.1) THEN
!
        IF(MTYPE(IPOL).LE.1) THEN
          DEL(23,1) = EX1C(1) 
          IF(NUC.GT.1) THEN        ! Add progeny contributions
            DO IE = 2,NUC
              DEL(23,IE) =  EX1C(IE)
            END DO
          ENDIF
        ENDIF
      ENDIF
!
!----- Evaluate agricultural food ingestion, positions 5 - 8 in SIF arrays
!
      IAD = 1
      DO ICRP = 1,4  ! Loop on the four food pathways
       IF(KEXPTH(ICRP+3).GT.0) THEN   !ICRP+3 is index in exposure path list
          DO IE = 1,NUC
            CSL = 1.0
            IPL = IE
            CALL LAMAX(IP,IE,ISMX,IGMX)
            CALL CROPS(IPATH,IPL,IAD,ICRP,CSL,CZ,CZ,CROP,IPOL,IE,ISMX,IGMX)
              DEL(ICRP+3,IE) = CROP * EX1C(IE)
          END DO
       ENDIF
      END DO
!
!---- Call EPFDAT to write output to EPF file --------------------------------
!
       DUROUT = SMED
       CALL EPFDAT()
!
!--- Write results of media concentrations to ELS file in csv format
!
!      WRITE(nels,100) (DEL(I,1),i=1,23),EX1C(1),CS(1)
! 100  FORMAT(1P,E10.2,',',40(E10.2,','))


      RETURN
      END
!=================== END OF MODULE SOILP ==============================
!
!   MEPAS HAZ2: SOILPT.FOR             Version Date: 08-Jan-96
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE SOILPT                                *
!                                                                            *
!  Subroutine SOILPT This subroutine calculates pathway concentrations       *
!                   for soil vs time input data from SCF                     *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    07-May-2002 for soil vs time analysis                   *
!  Last Modified:    07-May-2002 DLS                                         *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: NONE
!     Calls: SUBROUTINES RISKF, MAXIN
!     Common blocks referenced: SLPATH
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!    02-Apr-10     JGD  Code cleaned
!
!
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE SOILPT(IT,IPOL,CS)
!
!==== COMMON Block Definitions ===============================================
!
      include  'DERMDAT.FTN'
      INCLUDE  'ETIMES.FTN'
      include  'PFLAGS.FTN'
      INCLUDE  'PSET1.FTN'
      INCLUDE  'PSET3.FTN'
      INCLUDE  'PTHUSE.FTN'
      include  'SLPATH.FTN'
      include  'decay.ftn'
      INCLUDE  'DEL.FTN'
!
!==== DIMENSION Statements ===================================================
!
      REAL(KIND=8), DIMENSION(20) :: CS
!
!==== Variable Declarations ==================================================
!
!     NONE
!
!==== DATA Statements ========================================================
!
      DATA C365/365.25/,C10E3/1.E3/
!
      IPATH = 5
      DO IC = 1,20
        DO IE = 1,24
           DEL(IE,IC) = 0.0
        END DO
      END DO
      CZ = 0.0
      IP = IPOL
!
!---- Set pollutant type index values
!
!      CALL GETYPE(MTYPE(IPOL),MTING,MTINH)  ! Not used.
!
!----- Daily intake from inadvertent soil ingestion ----------------------
!
      IF(KEXPTH(14).GT.0) THEN
        IF(MTYPE(1).LE.1) THEN         ! Radionuclides
          DEL(14,1) = CS(1)
          IF(NUC.GT.1) THEN   ! Add progeny contribution
            DO IE = 2,NUC
              DEL(14,IE) =  CS(IE)
            END DO
          ENDIF
        ELSE
          DEL(14,1) = CS(1)
        ENDIF
      ENDIF
!
!----- Daily intake from contact with soil, dermal absorption ------------
!
      IF(KEXPTH(15).GT.0) THEN
!     Evaluate absorption per event per cm2 of skin exposed (
!          units = (mg or pCi per cm2 per event)
!     Test pollutant type (radionuclide or chemical)
       IF(MTYPE(IP).LE.1) THEN       ! Radionuclides
         DEL(15,1) = CS(1)
         IF(NUC.GT.1) THEN   ! Add progeny contribution
           DO IE = 2,NUC
             DEL(15,IE) =   CS(IE)
           END DO
         ENDIF
       ELSE                          ! Chemicals
         DEL(15,1) = CS(1)
       ENDIF
      ENDIF
!
!----- Evaluate soil resuspension inhalation, mass loading model
!
      IF(KEXPTH(19).GT.0) THEN
        IF(MTYPE(IPOL).LE.1) THEN
          DEL(19,1) = CS(1) * CML
          IF(NUC.GT.1) THEN   ! Add progeny contributions
            DO IE = 2,NUC
              DEL(19,IE) = CS(IE) * CML
            END DO
         ENDIF
        ELSE
          DEL(19,1) = CS(1) * CML
        ENDIF
      ENDIF
!
!----- Evaluate soil external dose ------------------------------
!
      IF(KEXPTH(23).GT.0.AND.MTYPE(IP).EQ.1) THEN
!
        IF(MTYPE(IPOL).LE.1) THEN        
          DEL(23,1) = CS(1)
          IF(NUC.GT.1) THEN        ! Add progeny contributions
            DO IE = 2,NUC
              DEL(23,IE) =  CS(IE)
            END DO
          ENDIF
        ENDIF
      ENDIF
!
!----- Evaluate agricultural food ingestion, positions 5 - 8 in SIF arrays
!
      IAD = 1
      DO ICRP = 1,4  ! Loop on the four food pathways
       IF(KEXPTH(ICRP+3).GT.0) THEN   !ICRP+3 is index in exposure path list
        IF(MTYPE(IP).LE.1) THEN    !ICRP+4 is index in MS path list, SIF
          DO IE = 1,NUC
            CSL = 1.0
            IPL = IE
            ISMX=IE
            IGMX=IE
            CALL CROPS(IPATH,IPL,IAD,ICRP,CSL,CZ,CZ,CROP,IPOL,IE,ISMX,IGMX)
              DEL(ICRP+3,IE) = CROP * CS(IE)
          END DO
!
        ELSE   ! Chemicals
          CSL = 1.0
          CALL CROPS(IPATH,IP,IAD,ICRP,CSL,CZ,CZ,CROP,IPOL,1,IPOL,IPOL)
            DEL(ICRP+3,1) = CROP * CS(1)
        ENDIF
       ENDIF
      END DO
!
!---- Call EPFDAT to write output to EPF file --------------------------------
!
       DUROUT = SMED
       CALL EPFDAT()
!
      RETURN
      END
!=================== END OF MODULE SOILPT ==============================
!   MEPAS AHAZ: CROPS.FOR             Version Date: 03-Jun-96
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE CROPS                                 *
!                                                                            *
!  Subroutine CROPS calculates plant or animal concentrations from air or    *
!             water contamination.                                           *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    09/20/85 (Converted to PC)                              *
!  Last Modified:    03-Jun-1996  DLS                                        *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: SUBROUTINE ATMOS, OVRLND, SURFWT
!     Calls: SUBROUTINES EXFCT
!     Common blocks referenced: BACKGD, PSET3, CRPATH, CRLOC, H3, C14, TSOIL, APATH, KLASS
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter      Set/
!     Name           Used   Type  Location  Parameter Description
!     -------------- ----   ----  --------  ------------------------------
!     CIRR             U    REAL  C-CRPATH  Irrigation rate, L/m2 per mo.
!     CONC             U    REAL  Argument  Concentration in water (mg/L or
!                                           pCi/L) or air (mg/m3 or Ci/m3).
!                                       or initial soil conc (mg/kg or pCi/kg)
!     CONS             U    REAL  Argument  Average soil concentration over
!                                           the growing period (mg/kg or pCi/kg)
!     CROP             S    REAL  Argument  Calculated concentration of each
!                                           pollutant in current crop, mg/Kg or
!                                           pCi/Kg.
!     KAW              U    INT   Internal  Index for air or water pathway;
!                                           1 for air and 0 for water, and 2
!                                           for measured soil
!     ILOC             U    INT   Argument  Index of current location.
!     IPOL             U    INT   Argument  Index of current pollutant.
!     ICRP             U    INT   Argument  Crop index: 1-lfy veg; 2-veg; 3-meat;
!                                           and 4-milk.
!     TGRW             U    INT   C-CRPATH  Growing period for site, days.
!     IPAR             U    INT   Argument  Index of parent in master list
!     IE               U    INT   Argument  Index of current radionuclide in
!                                           progeny list for parent IPAR
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!    02-Apr-10     JGD  Code cleaned
!
!
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE CROPS(IPATH,IPOL,IAD,ICRP,CONC,CONS,CSOIL,CROP,IPAR,IE,ISMX,IGMX)
!
!==== COMMON Block Definitions ===============================================
!
        USE Crop_Data_Mod
        include 'PSET3.FTN'
        include 'CRPATH.FTN'
        include 'CRLOC.FTN'
        include 'H3.FTN'
        include 'C14.FTN'
        include 'TSOIL.FTN'
        include 'APATH.FTN'
        include 'KLASS.FTN'
        include 'airdat.ftn'   ! Added 18-Mar-94 to provide ICLASS(IP)
!
!==== DIMENSION Statements ===================================================
!
      DIMENSION FHP(4),FHM(2),FCP(4),FCM(2) 
      DIMENSION KCLSS(7),ICP(4) 
!
!==== Variable Declarations ==================================================
!
!     NONE
!
!==== DATA Statements ========================================================
!
       DATA KCLSS/1,2,3,4,4,3,4/ 
       DATA ICP/3,4,4,3/ 
!   Data for carbon-14 model, ALC14 is soil carbon removal rate constant
!      PC14 is concentration of carbon in air, FCP is fraction of plant type
!      that s carbon,
       DATA ALC14/.00219 / 
       DATA PC14/1.6E-4/ 
       DATA FCP/0.09, 0.09, 0.40, 0.09/ 
       DATA FCM/0.24, 0.07/ 
       DATA FCW/2.E-5/ 
       DATA C30/30./
       DATA C365/365.25/
!
      ALPL = 0.049511 
      HUMID =0.008 
!
      FHP(1) = 0.1        ! Fractional hydrogen values
      FHP(2) = 0.1 
      FHP(3) = 0.068 
      FHP(4) = 0.1 
!
      FHM(1) = 0.1
      FHM(2) = 0.11 
!
!--- Set constants for equations
!
      IPAR = IPOL
      T2 = 365.25 
      IP = IPOL                        
      AIN=0. 
      CPL = 0. 
      ARG = GLAM(IGMX)*TCRP(ICRP) 
      EX3 = 1.0 
      IF(ARG.GT.0.) EX3=EXP(-ARG) 
      KAW = 0                ! For groundwater and surface water
      IF(IPATH.EQ.4) KAW=1   ! For air
      IF(IPATH.EQ.5) KAW=2   ! For measured soil pathway
      IF(IAD.LT.0) GO TO 50 
      CN=1.0 
      CN1 = 0.0
      IF(KAW.EQ.0) CN = CIRR(IPATH) / C30 
      IF(KAW.LE.1) CN1 = CN * TRN(ICRP) * RET / YLD(ICRP) 
!
!--- Perform calculations for each pollutant
!
      TGR = TGRW(ICRP,IPATH) 
!
!--- If pollutant is tritium use special model
!
      IF(TRITM) THEN 
        IF(KAW.LE.1) THEN
          CPL = 9.0 * CONC * FHP(ICRP) 
        ELSE
          CPL = 0.0
        ENDIF
        IF(KAW.EQ.1) CPL = CPL / HUMID 
      ELSE 
!
!--- If pollutant is carbon-14 use special model
!
        IF(CARBON) THEN 
          IF(KAW.LE.0) THEN 
!---      Carbon-14 model for irrigation
            tirr = 365.25 * firr(ipath)      !  to maximum of growing period or
            IF(tgr.gt.tirr) tgr = tirr       !  irrigation duration
            ARG = ALC14 * TGR 
            EX4 = EXFCT(ARG) 
            CPL = 0.333 * CONC * CIRR(IPATH) * EX4 * FCP(ICRP) / (DEN * ALC14)
            CPL = CPL + CSOIL * 10. * FCP(ICRP)/DEN
          ELSE IF( KAW.EQ.1) THEN
!---      Carbon-14 model for air
            CPL = CONC * FCP(ICRP) / PC14        ! air-to-plant
            CPL = CPL + 10.*FCP(ICRP)*CONS*TFSOIL(IE)/(DEN*C365)    ! air-to-soil-to-plant
            CPL = CPL + CSOIL * 10. * FCP(ICRP) / DEN ! previous soil to plant
          ELSE
            CPL = CONC * 10. * FCP(ICRP)  ! Carbon-14 uptake from soil
          ENDIF 
        ELSE
!
!--- Do calculations for other pollutants
!
        BFVI = BFV(IE) 
        CN2 = CN * BFVI / DEN         ! C365 not needed for water
        IF(KAW.EQ.1) CN2 = CN2 / C365 ! C365 added for change in TFSOIL
!
!--- Set exponential term for accumulation on plant surfaces
!    (Use decay constant for air, ALAM)
!
         EX1 = APFACT(IE)  ! apply air-to-plant factor for progeny
!
!--- Set exponential term for accumulation in soil
!--- (Use decay constant for soil, SLAM)
!
        CSL = 0.0
        IF(KAW.EQ.0) then                         !
          T2 = TGR                                !  set irrigation deposition time
          tirr = 365.25 * firr(ipath)             !  to maximum of growing period or
          IF(tgr.gt.tirr) T2 = tirr               !  irrigation duration
        endif                                     !
        ARG = SLAM(ISMX) * T2 
        EX2 = TGR 
        IF(ARG.GT.0.) EX2 = EXFCT(ARG) / SLAM(ISMX) 
        IF(KAW.EQ.1) THEN   ! air pathway
!
!--- Set deposition velocity for crop/pollutant types
!
          IF(ICLASS(IP).GT.0) THEN
            ICLSS = KCLSS(ICLASS(IP)) 
          ELSE
            ICLSS = KCLSS(ICLS(IP)) 
          ENDIF
          VDEFF = AVD(ICLSS,ICP(ICRP))
!--
          IF(ICLSS.EQ.4) THEN
            IF(VDEP(IP).GT.0.) THEN 
              VDEFF = 1./(1./VDEFF + 1./VDEP(IP)) 
            ELSE 
              VDEFF = 0.0 
            ENDIF 
!
          ENDIF 
          CPL = CONC*CN1*EX1*VDEFF*86400. + CONS*CN2*TFSOIL(IE) + CSOIL*BFVI/DEN
          CSL = CONS*TFSOIL(IE)/(DEN*C365) + CSOIL/DEN
        ELSE IF (KAW.EQ.0) THEN   ! water
!
!  FIRR added 21-Nov-95, fraction of year irrigation occurs.
!  average soil concentration from previous years added 21-Nov-95
!
          EX2 = TFSOIL(IE) * FIRR(IPATH) 
          CROPLEAF = CONC * (CN1*EX1)
          CROPROOT = CONC * (CN2 * EX2) + CONS * BFVI/DEN
          CPL = CONC*(CN1*EX1+CN2*EX2) + CONS*BFVI/DEN
          CSL = CONC*CN*TFSOIL(IE)*FIRR(IPATH)/(DEN)+CONS/DEN
        ELSE     ! Measured Soil
          CPL = CONC * BFVI
          CROPROOT = CPL
          CSL = CONC 
!
        ENDIF 
!
       ENDIF 
      ENDIF 
!
!--- Calculate plant concentration at harvest
!
!--- Set exponential term for decay between harvest and consumption
!   (Use decay constant for water, GLAM)
!
      IF(ICRP.GT.2) GO TO 50 
!
!--- Calculate crop concentration (end of calculation for crops)
!
        CROP=CPL*EX3
        CROP_FACTORS(ICRP,1) = CROPLEAF*EX3
        CROP_FACTORS(ICRP,2) = CROPROOT*EX3
      GO TO 100 
!
!--- Calculate animal product concentration using crop concentration at
!      harvest.
!
   50 UM = UMT    ! Meat animal parameters
      UW=WMT 
      US=USL(1)
      IF(ICRP.EQ.3) GO TO 80 
      UM = UMK    ! Milk animal parameters
      UW=WMK 
      US = USL(2)
!
!--- If current pollutant is tritium, use special model for animal product
!
  80  IF(TRITM) THEN 
!
!--- Calculate animal intake of tritium
!
        AIN = CPL * UM * FFC(ICRP-2)    ! added contaminated feed fraction
!
!--- If water pathway include tritium in animal drinking water
!                                       ! added contaminated water fraction
        IF(KAW.EQ.0.AND.IAD.LT.2) AIN = AIN + CONC * UW * FWC(ICRP-2)
!
!--- Calculate total hydrogen intake by animal
!
        HIN = FHP(ICRP) * UM + UW/9.0 
        CROP = AIN * FHM(ICRP-2) / HIN 
!
!--- If current pollutant is carbon-14, use special model for animal product
!
      ELSE 
       IF(CARBON) THEN
!--- For air releases:
        IF(KAW.EQ.1) THEN 
          CROP = CPL * FFC(ICRP-2) * FCM(ICRP-2) / FCP(ICRP)
!         CROP = CPL *               FCM(ICRP-2) / FCP(ICRP)
        ELSE IF (KAW.EQ.0) THEN
!--- For water releases:
          IF(IAD.LT.0) THEN 
!---    Animal exposed by water only
            CROP = UW * FCM(ICRP-2) * CONC * FWC(ICRP-2)/ (FCP(ICRP)*UM + FCW*UW)
!            CROP = FCM(ICRP-2) * CONC / (FCP(ICRP)*UM + FCW*UW)
          ELSE 
            IF(IAD.EQ.1) THEN 
!---    Animal exposed by feed and water
   CROP = FCM(ICRP-2)*(CPL*UM*FfC(ICRP-2)+CONC*UW*FWC(ICRP-2))/(FCP(ICRP)*UM+FCW*UW)
            ELSE 
!---    Animal exposed by feed only
              CROP = CPL * FCM(ICRP-2) *UM*FFC(ICRP-2) / (FCP(ICRP)*UM+FCW*UW)
            ENDIF 
          ENDIF 
        ELSE
!--- For measured soil
          CROP = CPL * FCM(ICRP-2) *UM*FFC(ICRP-2) / (FCP(ICRP)*UM+FCW*UW)
        ENDIF
!
!--- Do calculation for non-tritium pollutants
!
       ELSE
        BMTK=BMT(IE) 
        IF(ICRP.EQ.4) BMTK=BMK(IE) 
!
!--- Calculate contribution from animal feed (air and water pathways)
!
        BMTKFACTOR = BMTK * FFC(ICRP-2) * EX3
!
        CROPLEAF = BMTKFACTOR * CROPLEAF * UM
        CROP_FACTORS(ICRP,1) = CROPLEAF
!
        CROPROOT = BMTKFACTOR * CROPROOT * UM
        CROP_FACTORS(ICRP,2) = CROPROOT
!
        CROPSOIL = BMTKFACTOR * CSL * US
        CROP_FACTORS(ICRP,4) = CROPSOIL
        CROP = BMTK*FFC(ICRP-2)*EX3 * (CPL*UM + CSL*US)
!
!--- Add contribution from animal drinking water (water pathway only)
!

         IF(KAW.EQ.0.AND.IAD.LT.2.) THEN
           If (IE.EQ.1) THEN
              CROPWATER = CONC*UW*BMTK*FWC(ICRP-2)*EX3
              CROP=CROP + CROPWATER
              CROP_FACTORS(ICRP,3) = CROPWATER
           ENDIF
         ENDIF
!
!--- End of animal product calculations
!
       ENDIF
      ENDIF 
  100 CONTINUE 
      RETURN
      END 
!============ END OF MODULE CROPS ========================================
!   MEPAS HAZ2: DRINK.FOR             Version Date: 11-24-1991
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE DRINK                                 *
!                                                                            *
!  Subroutine DRINK calculates drinking water concentrations                 *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    09/23/85 (Converted to PC)                              *
!  Last Modified:    30-July-1992  DLS                                       *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: SUBROUTINE GROUND, OVRLND, SURFWT
!     Calls: NONE
!     Common blocks referenced: DEVICE, DWPATH, PSET3
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter      Set/
!     Name           Used  Type  Location  Parameter Description
!     -------------- ----  ----  --------  ------------------------------
!     GLAM(20)        U    Real  C-PSET3   Decay constant for each pollutant,
!                                          days-1.
!     CDKW            S    Real  Argument  Calculated concentration in drinking
!                                          water, mg/L or pCi/L.
!     CWAT            U    Real  Argument  Concentration in source water for
!                                          drinking water supply system, mg/L
!                                          pCi/L.
!     ILOC            U    Int   Argument  Index for current usage location.
!     LTRTL(10)       U    Int   C-DWPATH  Flag for water treatment: <=0, none;
!                                          >0, use WPF.
!     NPOL            U    Int   Argument  Number of pollutants for current run.
!     TWAT(10)        U    Real  C-DWPATH  Holdup time between water intake
!                                          plant and consumption, days.
!     WPF(20)         U    Real  C-PSET3   Water treatment purification factor.
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!    02-Apr-10     JGD  Code cleaned
!
!  
!
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE DRINK(IPATH,IPOL,CWAT,CDKW,CHEM)
!
!==== COMMON Block Definitions ===============================================
!
        include 'DEVICE.FTN'
        include 'PSET3.FTN'
        include 'DWPATH.FTN'
        INCLUDE 'DECAY.FTN'
!
!     Define all parameters used in this module
!
!      IMPLICIT NONE
!
!==== DIMENSION Statements ===================================================
!
      REAL(KIND=4), DIMENSION(20) ::  CWAT, CDKW
      REAL(KIND=8), DIMENSION(20) :: AB, CWIN, CWOUT
      INTEGER :: IP, IPATH, IPOL, I, INTGRL
      REAL(KIND=4) :: TRT, TWATL
!
!==== Variable Declarations ==================================================
!
!     NONE
!
!==== DATA Statements ========================================================
!
!     NONE
!
!--- Start of Calculations ---------------------------------------------
!
      TWATL = TWAT(IPATH)
!
!--- Perform calculations for each pollutant ---------------------------
!
      IP = IPOL
!
!--- Set water purification treatment factor ---------------------------
!
        TRT = 1.0
        IF(LTRTL(IPATH).GT.0) TRT = WPF(IP)
!
!--- Calculate water concentration, including decay in distribution system.
!
!        IF(NUC.GT.1) THEN                                       ! ** deactivate
          DO I = 1,NUC
             CWIN(I) = DBLE(CWAT(I))
          END DO
          INTGRL = 0
          IF(CHEM) THEN  !XX                            ! ** activity for chemical degradation
              DO I = 1,NUC
                 AB(I) = DBLE(GLAM(I))
              END DO
          ELSE                                                 ! ** activity for chemical degradation
             DO I = 1,NUC
               AB(I) = 0.0D0
             END DO
          ENDIF                                                ! ** activity for chemical degradation
          CALL CHAIN(TWATL,AB,CWIN,CWOUT,INTGRL,CHEM)
          DO I = 1,NUC
            TRT = 1.0
            IF(LTRTL(IPATH).GT.0) TRT = WPF(I)
            CDKW(I)=CWOUT(I)*TRT
          END DO
!        ELSE                                                    ! ** deactivate
!          CDKW(1)=CWAT(1)*TRT*EXP(-GLAM(IP)*TWATL)              ! ** deactivate
!        END IF                                                  ! ** deactivate
! ** Updated 15 November 2004, DL Strenge
  100 CONTINUE
      RETURN
!============== END OF MODULE DRINK =====================================
      END
!
!   MEPAS HAZ2: AQUAT.FOR             Version Date: 07-Dec-96
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE AQUAT                                 *
!                                                                            *
!  Subroutine AQUAT calculates the concentration of each pollutant in fish   *
!                   and invertebrates for the current usage location.        *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    08/20/85 (Converted to PC)                              *
!  Last Modified:    07-Dec-96     DLS                                       *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: SUBROUTINE SURFWT
!     Calls: SUBROUTINES (NONE)
!     Common blocks referenced: AQPATH, PSET3
!
!==== Significant Parameter Designation and Description ======================
!
!    Parameter      Set/
!    Name           Used  Type  Location  Parameter Description
!    -------------- ----  ----  --------  ------------------------------
!    variable        S    CHAR  Argument  nnnnnnnnnnnnnnnnnnnnnn
!                    U    INT   Internal
!                    S/U  REAL  Common
!                         DBLE  External
!    CAQF            S    REAL  Argument Concentration of pollutant
!                                        in fish, mg/Kg or Ci/Kg
!    CAQI            S    REAL  Argument Concentration of pollutant
!                                        in invertebrates, mg/Kg or Ci/Kg
!    CWAT            U    REAL  Argument Pollutant water concentrations,
!                                         mg/L or Ci/L
!    ILOC            U    INT   Argument Index of current usage location
!    NPOL            U    INT   Argument Number of pollutant to consider
!    TFSH(10)        U    REAL  C-AQPATH Time between harvest and consumption
!
!                                        for fish for each location, days
!    TFSHL          S/U   REAL  Internal Time between harvest and consumption
!                                        for fish for current location, days
!
!    TINV(10)        U    REAL  C-AQPATH Time between harvest and consumption
!                                        for invertebrates, days
!
!    TINVL          S/U   REAL  Internal Time between harvest and consumption
!                                        for invertebrates for current
!                                        location, days
!
!    GLAM             U   REAL   PSET3   Decay constant in fish products after
!                                        harvest until consumption, days-1
!
!==== Modification History ===================================================
!
!    Date     Who  Modification Description
!  ---------  ---  ------------------------------------------------------
!    02-Apr-10     JGD  Code cleaned
!
!  
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE AQUAT(IPOL,CWAT,CAQF,CAQI) 
!
!==== COMMON Block Definitions ===============================================
!
      include 'PSET3.FTN'
      include 'AQPATH.FTN'
      INCLUDE 'DECAY.FTN'
!
!==== DIMENSION Statements ===================================================
!
      DOUBLE PRECISION, DIMENSION (20) :: DKF,DKI
      DOUBLE PRECISION, DIMENSION (20) :: CWAT,CAQF,CAQI,AB
!
!==== Variable Declarations ==================================================
!
!     NONE
!
!==== DATA Statements ========================================================
!
!     NONE
!
!--- Set time from harvest to consumption for current location
!
      TFSHL=TFSH
      TINVL=TINV
!
!--- Perform calculation for each pollutant
!
      IP = IPOL
!
!---    Calculate decay in water for time from harvest to consumption
!
! ** Updated 15 November 2004, DL Strenge
!      IF(NUC.GT.1) THEN  ! Chemical decay logic
      INTGRL = 0
      DO IP = 1,NUC
         AB(IP) = 0.0D0
      END DO
      CALL CHAIN(TFSHL,AB,CWAT,DKF,INTGRL,CHEM)
      CALL CHAIN(TINVL,AB,CWAT,DKI,INTGRL,CHEM)
!      ELSE
!        DKF(1) = CWAT(1)*EXP(-GLAM(IP)*TFSHL)
!        DKI(1) = CWAT(1)*EXP(-GLAM(IP)*TINVL)
!      ENDIF
! ** Updated 15 November 2004, DL Strenge
!
!---    Calculate concentration in fish and invertebrates
!
      DO IP = 1,NUC
        CAQF(IP) = BFF(IP)*DKF(IP)
        CAQI(IP) = BFI(IP)*DKI(IP)
      END DO
      RETURN
!
!====== END OF MODULE AQUAT==============================================
!
      END
!   MEPAS HAZ2: SLINPT.FOR             Version Date: 11-Feb-97
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE SLINPT                                *
!                                                                            *
!  Subroutine SLINPT This subroutine read the input parameters for the       *
!                    exposure pathway of direct ingestion of soil, based on   *
!                    measured soil concentrations in the vicinity of a site. *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    01/19/89 (Converted to PC)                              *
!  Last Modified:    11-Feb-97 DLS                                           *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: NONE
!     Calls: NONE
!     Common blocks referenced: SLPATH, LOCNAM, DEVICE, PSET1
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!    02-Apr-10     JGD  Code cleaned
!
!  
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE SLINPT(SETDATA,NLINES)
!
!==== COMMON Block Definitions ===============================================
!
        include 'SLPATH.FTN'
        include 'DEVICE.FTN'
        INCLUDE 'PARMTR.PAR'
!
!==== DIMENSION Statements ===================================================
!
!==== Variable Declarations ==================================================
!
      CHARACTER*(*) SETDATA(LINEMAX)
      INTEGER NLINES
!
!==== DATA Statements ========================================================
!
       DATA IZ/0/
!
!---- Read parameters from GID data set --------------------------------------
!
      ED = GETREAL(SETDATA,NLINES,'SMED          ',IZ,IZ,IZ,IZ,IZ,IZ)
        SMED = 70.
        IF(ED.GT.0.) SMED = ED
       RETURN
      END
!
!   MEPAS HAZ2: BLKDAT.FOR             Version Date: 02-Mar-1998
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           BLOCK DATA BLKDAT                                *
!                                                                            *
!  BLOCK DATA BLKDAT This data block contains default parameter              *
!                    values for RAPS.                                         *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    02/08/86 (Converted to PC)                              *
!  Last Modified:    02-Mar-1998  DLS                                        *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: NONE
!     Calls: NONE
!     Common blocks referenced: CRLOC, AGCLAS, AQCLAS, DEVICE, RECLAS, PTHUSE, CRPATH, RISKCM
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter      Set/
!     Name           Used  Type  Location  Parameter Description
!     -------------- ----  ----  --------  ------------------------------
!     variable        S    CHAR  Argument  nnnnnnnnnnnnnnnnnnnnnn
!                     U    INT   Internal
!                     S/U  REAL  Common
!                          DBLE  External
!
!==== Modification History ===================================================
!
!  Date       Who  Modification Description
!  --------   ---  ------------------------------------------------------


!==== SUBROUTINE CALL ========================================================
!
      BLOCK DATA BLKDAT
!
!==== COMMON Block Definitions ===============================================
!
  include 'CRLOC.FTN'
  include 'AGCLAS.FTN'
  include 'AQCLAS.FTN'
  include 'AQPATH.FTN'
    include 'APATH.FTN'
  include 'DEVICE.FTN'
  include 'DWPATH.FTN'
    include 'PTHUSE.FTN'
  INCLUDE 'CRPATH.FTN'
  INCLUDE 'RISKCM.FTN'
  INCLUDE 'DERMDAT.FTN'
  INCLUDE 'LEACH.FTN'
!
!==== DIMENSION Statements ===================================================
!
!     NONE
!
!==== Variable Declarations ==================================================
!
!     NONE
!
!==== DATA Statements ========================================================
!
!----- Parameter definitions for COMMON Block AGCLAS ---------------------
!
      DATA NAGC/30/
!
!----- Parameter definitions for COMMON Block AQCLAS ---------------------
!
      DATA NAQC/10/
!
!-----  Parameter definitions for COMMON Block APATH --------------------
!
      DATA (AVD(I,1),I=1,4)/8.38E-3,1.76E-3,4.30E-5,5.06E-3/
      DATA (AVD(I,2),I=1,4)/1.82E-2,6.70E-3,3.16E-4,1.75E-2/
      DATA (AVD(I,3),I=1,4)/2.44E-2,9.98E-3,4.60E-4,2.56E-2/
      DATA (AVD(I,4),I=1,4)/2.89E-2,1.23E-2,5.57E-4,3.13E-2/
      DATA (AVD(I,5),I=1,4)/5.03E-2,2.42E-2,1.01E-3,5.84E-2/
      DATA (AVD(I,6),I=1,4)/7.95E-2,4.12E-2,1.66E-3,9.39E-2/
!
!-----  Parameter definitions for COMMON Block AQPATH --------------------
!
      DATA TFSH/10./
      DATA TINV/10./
!
!----- Parameter definitions for COMMON Block CRPATH ---------------------
!
      DATA YLD/2.0, 2.0, 0.7, 0.7/
      DATA TCRP/2.,60.,20.,4./
      DATA RET/0.25/,DEN/240./
      DATA UMT,UMK,WMT,WMK/68.,55.,50.,60./
      DATA TRN/1.0,0.1,0.1,1.0/
      DATA FFC/1.0,1.0/,FWC/1.0,1.0/  
      DATA USL/0.8,0.7/     ! Animal soil intake, kg/d, meat and milk
!
!----- Logical I/O unit definitions for COMMON Block DEVICE --------------
!
      DATA NIN,NELS,NWCF,NDBLU,NATO,NPIN/5,6,12,10,2,8/
      DATA NFAC,NTMP,INUNIT,txt,NEPF/4,9,16,15,17/
      DATA NSIF,NERR,NGID,NSCF,NFOOD/18,19,20,21,22/
      DATA NATM,NETM,NWRN/23,24,25/
!
!----- Logical I/O unit definitions for COMMON Block HPORNL  --------------
!
!----- Parameter definitions for COMMON Block DWPATH
!
      DATA TWAT/3*1.0/
      DATA LTRTL/3*0/
!
!----- Parameter definitions for COMMON Block CRLOC ----------------------
!
      DATA (TGRW(I,1),I=1,4)/60.,60.,30.,30./
      DATA (TGRW(I,2),I=1,4)/60.,60.,30.,30./
      DATA (TGRW(I,3),I=1,4)/60.,60.,30.,30./
      DATA (TGRW(I,4),I=1,4)/60.,60.,30.,30./
      DATA (TGRW(I,5),I=1,4)/60.,60.,30.,30./
      DATA FIRR/1.0,1.0,0.0/  ! 1-GW, 2-not used, 3-SW
!
!----- Parameter definitions for COMMON Block PTHUSE ---------------------
!
      DATA CML/1.E-8/             ! Mass loading factor, kg/m3
!
!----- Parameter definitions for COMMON Block RISKCOM---------------------
!
      DATA C365/365.25/,TCOM/70./
!
!----- Parameter definitions for COMMON Block DERMDAT --------------------
!
      DATA ANDFC/0.5/,ANDFR/0.1/     ! Andelman factors for chems/radon222
      DATA TSS/0.04/,  RHOSS/1.5/    ! Shoreline thickness m, and density
      DATA TMS/0.04/,  RHOMS/1.5/    ! Measured soil thickness & density
      DATA TAS/0.04/,  RHOAS/1.5/    ! Atmospheric deposition, soil thickness
!                                      and density
!----- Parameter definitions for COMMON Block LEACH
!
      DATA THICK/15./
      DATA MOISTC/.3/
      DATA BULKD/1.5/
      DATA VLEACH/5./
!
!============ END OF MODULE BLKDAT =========================================
      END
!   MEPAS AHAZ: SURFWT.FOR             Version Date: 19-Jun-97
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE SURFWT                                *
!                                                                            *
!  Subroutine SURFWT controls exposure pathway calculations for the surface  *
!                    water transport pathway. SURFWT returns hazard potential*
!                    index values for the current usage location and each    *
!                    pollutant.                                              *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    09/23/85 (Converted to PC)                              *
!  Last Modified:    19-Jun-1997      DLS                                    *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: SUBROUTINE EXPOS
!     Calls: SUBROUTINES AQUAT, CROPS, DRINK, RISKF
!     Common blocks referenced: DEVICE, PSET1, PSET2, PSET3, PTHUSE, AGCLAS,
!                               AQCLAS, RECLAS, RISK, SWPATH
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     IT          U    INT     Argument  Index on time period
!     ILOC        U    INT     Argument  Location index
!     IPOL        U    INT     Argument  Pollutant index (always 1)
!     CW(20)      U  REAL*8    Argument  Water concentration for chain members
!     CS(20)      U  REAL*8    Argument  Soil concentration for chain members
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE SURFWT (IT,IPOL,CW,CS,DELT)
!
!==== COMMON Block Definitions ===============================================
!
    USE CROP_DATA_MOD
  include 'DEVICE.FTN'
  include 'PFLAGS.FTN'
    INCLUDE 'ETIMES.FTN'
  include 'PSET1.FTN'
  include 'PSET3.FTN'
  include 'PTHUSE.FTN'
  include 'AGCLAS.FTN'
  include 'AQCLAS.FTN'
  include 'SWPATH.FTN'
  include 'DERMDAT.FTN'
  include 'TSOIL.FTN'
  INCLUDE 'CRLOC.FTN'
  INCLUDE 'DECAY.FTN'
    INCLUDE 'DEL.FTN'
!
!==== DIMENSION Statements ===================================================
!
      DIMENSION AWKG(20)
!
!==== Variable Declarations ==================================================
!
      REAL CWD(20),CDKW(20)
      REAL exout(25)
      DOUBLE PRECISION, DIMENSION (20) :: CW,CS,CAQF,CAQI
      DOUBLE PRECISION, DIMENSION (20) :: AM, AB, AWOUT
      DOUBLE PRECISION, DIMENSION (20) :: apf, abb
!
!==== DATA Statements ========================================================
!
      DATA CNST/3.93E1/
      DATA CONST/100./
      DATA CONVM3/1.E-3/
      DATA C10E3/1.E3/        ! Constant for MEPAS shower inhalation model
      DATA C365/365.25/
      DATA ALPL/0.049511/
!
!--- Start of calculations ----------------------------------------------
!
      IPATH = 2
      IP=IPOL
      IT = IT
      IF(IP.GT.1) IP = 1
      CWA = CW(1)
      CWS = CS(1)
!
!--- Initialize intake array --------------------------------------------
!
      DO I = 1,20
  ABB(I) = DBLE(ALPL)
  AB(I)  = 0.0D0
  CWD(I) = CW(I)
        DO IE = 1,25
    DEL(IE,I) = 0.0
  END DO
      END DO
!---   Initialize new output array for temporary results to ELS file.
       DO I = 1,25
          EXOUT(I) = 0.
       END DO
!
!----- Set pollutant type index values
!
!      CALL GETYPE(MTYPE(IPOL),MTING,MTINH)    ! Not used.
!
!--- Calculate ingestion uptake from drinking water ---------------------
!
      IF(KSDR.GT.0) THEN
!
        CALL DRINK(IPATH,IPOL,CWD,CDKW,CHEM)
!
!----- Test exposure pathway flag for drinking water ingestion
!
          IF(KEXPTH(1).GT.0) THEN               ! old logic removed 24-Oct-05 DL Strenge
!          IF(KEXPTH(1).GT.0.AND.KSDR.GT.1) THEN
!      IF(MTYPE(IPOL).LE.1) THEN             ! Radionuclides
        DO IE = 1,NUC
                DEL(1,IE) = CDKW(IE) 
        END DO
!      ELSE                                ! Chemicals
!              DEL(1,1) = CDKW(1)
!              EXOUT(1) = DEL(1,1)
!      ENDIF
    ENDIF
!
!----- Test exposure pathway flag for ingestion while showering ----------
!
        IF(KEXPTH(3).GT.0) THEN                  ! old logic removed 24-Oct-05 DL Strenge
      DO IE = 1,NUC
              DEL(3,IE) = CDKW(IE) 
      END DO
  ENDIF
!
!----- Test exposure pathway flag for dermal absorption while showering ----
!
        IF(KEXPTH(2).GT.0) THEN                  ! old logic removed 24-Oct-05 DL Strenge
!        IF(KEXPTH(2).GT.0.AND.KSDR.LT.3) THEN
!    IF(MTYPE(IPOL).LE.1) THEN
      DO IE = 1,NUC
              DEL(2,IE) = CDKW(IE) 
      END DO
!    ELSE                                                     !FIX
!            DEL(2,1) = CDKW(1)
!            EXOUT(2) = DEL(2,1)
!    ENDIF
  ENDIF
!
!----- Test exposure pathway flag for inhalation intake while showering -----
!
        IF(KEXPTH(17).GT.0) THEN                   ! old logic removed 24-Oct-05 DL Strenge
      HC=HLC(IPOL)
      IF(HC.GT.2.4E-3) HC=2.4E-3
!      IF(MTYPE(IPOL).LE.1) THEN
        DO IE = 1,NUC
                DEL(17,IE) = CDKW(IE)*HC*CNST*C10E3   ! rads   C10E3 added 27-Mar-2000
        END DO
!      ELSE
!              DEL(17,1) = CDKW(1)*HC*CNST*C10E3   ! chemicals  C10E3 added 27-Mar-2000
!              EXOUT(17) = DEL(17,1)
!      ENDIF
        ENDIF
!
!---- Indoor inhalation  -----------------------------------------------------
!
          IF(KEXPTH(25).GT.0) THEN                     ! old logic removed 24-Oct-05 DL Strenge
!      IF(MTYPE(IPOL).LE.1) THEN
        DO IE = 1,NUC
    IF(ANDF(IE).GT.0.) THEN
                  DEL(25,IE) = CDKW(IE)*ANDF(IE)
    ENDIF
        END DO
!      ELSE
!        IF(ANDF(IPOL).GT.0.) THEN
!                 DEL(25,1) = CDKW(1)*ANDF(IPOL)
!                 EXOUT(25) = DEL(25,1)
!        ENDIF
!      ENDIF
    ENDIF
  ENDIF
!
!--- Calculate ingestion uptake from aquatic pathways --------------------
!
      IF(KSAQ.GT.0) THEN
!
!----- Test exposure pathway flag for finfish/shellfish ingestion --------
!
        CALL AQUAT(IPOL,CW,CAQF,CAQI)
!
!----- Finfish, path 8 -------------------------------------------------------
!
        IF(KEXPTH(8).GT.0) THEN
           DO IE = 1,NUC
              DEL(8,IE) = CAQF(IE)
!              EXOUT(8) = DEL(8,1)
           END DO
        ENDIF
!
!----- Shellfish, path 9 -----------------------------------------------------
!
         IF(KEXPTH(9).GT.0) THEN
            DO IE = 1,NUC
               DEL(9,IE) = CAQI(IE)
!               EXOUT(9) = DEL(9,1)
            END DO
         ENDIF
      ENDIF
!
!--- Calculate ingestion uptake from irrigated farm products ------------
!
      IF(KSIR.NE.0) THEN
  I4=4
        IAD=KSIR
        DO IC = 1,4
           DO I = 1,4
              CROP_FACTORS(IC,I) = 0.0
           END DO
        END DO
  DO 35 IC = I4,7
!
!----- Test exposure pathway flags for inclusion of food ingestion -------
!
    IF(KEXPTH(IC).LE.0) GO TO 35
              EXOUT(15) = CS(1)
        IC3=IC-3
        APF(1) = 1.0D0
              ABB(1) = DBLE(ALPL)
        IF(MTYPE(IPOL).GT.1) ABB(1) = DBLE(ALPL) + ALAM(1)
              IF(NUC.GT.1) THEN
    DO IE = 2,NUC
            ABB(IE) =  DBLE(ALPL)
                  APF(IE) = 0.0D0
                  IF(MTYPE(IPOL).GT.1) ABB(IE) = DBLE(ALPL) + ALAM(IE)
    END DO
        ENDIF
        INTGRL = 1
!
! ** Updated 08 December 2004, DL Strenge
!      IF(MTYPE(IPOL).GT.1) AL(1) = ALAM(1)
        TIM = TGRW(IC3,2)        ! Time input to SRETEN in years
              tirr = 365.25 * firr(2) ! Irrigation period in days
              IF(tim.gt.tirr) then  ! if irrigation occurs less than growing period
                tim = tirr          ! then adjust deposition time to equal irrigation
              endif
            IF(tim.gt.0.) then    ! do crop calculation only if irrigation occurs
        CALL CHAIN(TIM,ABB,APF,APF,INTGRL,CHEM)
        DO IE = 1,NUC
     APFACT(IE) = APF(IE)  ! APF units are days
        END DO
!             IF(MTYPE(IPOL).LE.1) THEN
              DO IE = 1,NUC                ! Radionuclides
                 CALL LAMAX(IP,IE,ISMX,IGMX)
                 CWS = CS(IE)
                 CALL CROPS(IPATH,IPOL,IAD,IC3,CWA,CWS,CWS,CROP,IPOL,IE,ISMX,IGMX)
                 DEL(IC,IE) = CROP
        END DO
!            ELSE
!               CALL CROPS(IPATH,IP,IAD,IC3,CWA,CWS,CWS,CROP,IP,1,IP,IP)
!               DEL(IC,1) = CROP
!               EXOUT(ic) = DEL(ic,1)
!            ENDIF
! ** Updated 15 November 2004, DL Strenge
           else      ! IF TIM = 0, SET RESULTS TO ZERO (NO IRRIGATION)
            DO IE = 1,NUC
              DEL(IC,IE) = 0.0
            END DO
           endif
   35   CONTINUE
      ENDIF
!
!--- Calculate equivalent exposure from recreational activities ---------
!
      IF(KSR.GT.0) THEN 
!
!----- Test exposure pathway flag for inclusion of swimming dermal intake
!
  IF(KEXPTH(11).GT.0) THEN
! ** Updated 15 November 2004, DL Strenge
!    IF(MTYPE(IPOL).GT.1) THEN                       ! Chemicals
!            DEL(11,1)=CWA
!            EXOUT(11) = DEL(11,1)
!    ELSE
            DEL(11,1) = CWA
!    ENDIF
! ** Updated 15 November 2004, DL Strenge
  ENDIF
!
!----- Test exposure pathway flag for ingestion while swimming -----------
!
  IF(KEXPTH(10).GT.0) THEN
! ** Updated 15 November 2004, DL Strenge
!    IF(MTYPE(IPOL).GT.1) THEN
!            DEL(10,1)=CWA
!            EXOUT(10) = DEL(10,1)
!    ELSE
            DEL(10,1)=CWA
!    ENDIF
! ** Updated 15 November 2004, DL Strenge
  ENDIF
!
!----- Test exposure pathway flag for external dose while swimming -------
!
  IF(KEXPTH(20).GT.0) THEN
    IF(MTYPE(IPOL).LE.1) THEN              ! Radionuclides only
            DEL(20,1) = CWA                    ! Average concentration
    ENDIF
       ENDIF
!
!----- Test exposure pathway flag for external dose while boating --------
!
  IF(KEXPTH(21).GT.0) THEN
    IF(MTYPE(IPOL).LE.1) THEN
            DEL(21,1) = CWA               ! Not Time-integral
    ENDIF
       ENDIF
!
!----- Start calculation for shoreline pathways ------------------------
!
!      Set shoreline factor for midpoint of 70 year period
!
       IOK=0
       IF(KEXPTH(12).GT.0) IOK=IOK+1
       IF(KEXPTH(13).GT.0) IOK=IOK+1
       IF(KEXPTH(22).GT.0) IOK=IOK+1
       if(IOK.GT.0) THEN
  TIMS = C365 * DELT
!
!   Evaluate chain decay for all pollutants
!   Note:  the sediment equation includes only radiological or chemical decay
!
    AM(1) = CWA*CONST*ALOG(2.)
    AB(1) = 0.0D0
          IF(NUC.GT.1) THEN
      DO IE=2,NUC
        AM(IE) = 0.0D0
        AB(IE) = 0.0D0
      END DO
          ENDIF
     INTGRL = 1
          CALL CHAIN(TIMS,AB,AM,AWOUT,INTGRL,CHEM) 
    !    CSEKG = CONVM3 / (TSS*RHOSS)
    CSEKG = CONVM3 / (TSS*RHOSS) / TIMS    ! Change 23-Nov-05
    DO IE=1,NUC
            AWKG(IE) = AWOUT(IE) * CSEKG 
    END DO
      ENDIF
!
!----- Test exposure pathway flag for external dose on shoreline ---------
!
  IF(KEXPTH(22).GT.0) THEN
    IF(MTYPE(IPOL).LE.1) THEN    ! Radionuclides only
!!**      IF(NUC.GT.1) THEN
              DO IE = 1,NUC
                DEL(22,IE) = AWKG(IE)          ! Not time-integral
!               DEL(22,IE) = AWOUT(IE)          ! Not time-integral
        END DO
!!**      ELSE
!!**          DEL(22,1) = CSKG
!             DEL(22,1) = CSED
!!**      ENDIF
    ENDIF
  ENDIF
!
!----- Test exposure pathway flag for shoreline ingestion -----------------
!
  IF(KEXPTH(13).GT.0) THEN
! ** Updated 15 November 2004, DL Strenge
!    IF(MTYPE(IPOL).LE.1) THEN
!!**      IF(NUC.GT.1) THEN
        DO IE = 1,NUC
                DEL(13,IE) = AWKG(IE) 
        END DO
!!**      ELSE
!!**              DEL(13,1) = CSKG
!!**      ENDIF
!    ELSE
!            DEL(13,1) = CSKG
!            EXOUT(13) = DEL(13,1)
!    ENDIF
! ** Updated 15 November 2004, DL Strenge
  ENDIF
!
!----- Test exposure pathway flag for shoreline dermal contact ------------
!
  IF(KEXPTH(12).GT.0) THEN
! ** Updated 15 November 2004, DL Strenge
!    IF(MTYPE(IPOL).LE.1) THEN
!!**     IF(NUC.GT.1) THEN
        DO IE = 1,NUC
                DEL(12,IE) = AWKG(IE) 
        END DO
!!**     ELSE
!!**              DEL(12,1) = CSKG
!!**     ENDIF
!    ELSE
!            DEL(12,1) = CSKG
!            EXOUT(12) = DEL(12,1)
!    ENDIF
  ENDIF
      ENDIF     ! End of surface water recreational activity pathways
      DUROUT = DSWED
      CALL EPFDAT()
!
      RETURN
!================ END OF MODULE SURFWT ======================================
      END
!   MEPAS HAZ2: USEDAT.FOR             Version Date: 25-Jun-1997
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE USEDAT                                *
!                                                                            *
!  Subroutine USEDAT controls input of pathway and usage location data.      *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    04/01/86 (Converted to PC)                              *
!  Last Modified:    25-Jun. -1997  DLS                                      *
!                                                                            *
! *****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: SUBROUTINE FUNCTION ....
!     Calls: FUNCTIONS...
!            SUBROUTINES....
!     Common blocks referenced: DEVICE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!  IPATH      U    INT    Transport pathway index
!  SETDATA()  U   CHAR*80 Data set from GID for extraction of information
!  NLINES     U    INT    Number of lines of data in SETDATA
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!
!
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE USEDAT(IPATH,SETDATA,NLINES)
!
!==== COMMON Block Definitions ===============================================
!
        include 'DEVICE.FTN'
        INCLUDE 'PARMTR.PAR'
        INCLUDE 'PFLAGS.FTN'
!
!==== DIMENSION Statements ===================================================
!
!    NONE
!
!==== Variable Declarations ==================================================
!
      CHARACTER*(*) SETDATA(LINEMAX)
      INTEGER IPATH,NLINES,GETINT,IZ
!
!==== DATA Statements ========================================================
!
      DATA IZ/0/
!
!     IPATH is the index on the pathway for which data is to be provided in
!     this call.
!
!----- Test argument parameters for proper range ----------------------------C
!
      IF(IPATH.LT.1.OR.IPATH.GT.7) THEN
        WRITE(NERR,1000) IPATH
 1000   FORMAT('Error in USEDAT: BAD VALUE OF IPATH = ',I5)
        GO TO 999
      ENDIF
!
!----- Get exposure pathway flags from GID file
!
!
!----- Read input record type 2, exposure pathway selection flags
!
         DO IP = 1,25
            KEXPTH(IP) = GETINT(SETDATA,NLINES,'KEXPTH        ',IP,IPATH,IZ,IZ,IZ,IZ)
         END DO
!
!----- Call appropriate subroutines for input of usage location data    -----C
!          Groundwater exposure data - GWINPT
!          Surface water exposure data - SWINPT
!          Atmospheric exposure data - ATINPT
!
      IF( IPATH.EQ.1) then
        CALL GWINPT(SETDATA,NLINES)
      else IF(IPATH.EQ.2) then
        CALL SWINPT(SETDATA,NLINES)
      else IF( IPATH.EQ.3) then
        CALL ATINPR(SETDATA,NLINES)
        CALL ATINPA(SETDATA,NLINES)
      else IF( IPATH.EQ.5) then
        CALL SLINPT(SETDATA,NLINES)
      endif
!
 999  RETURN
!================== END OF MODULE USEDAT ====================================
      END
!   MEPAS HAZ2: SWINPT.FOR             Version Date: 11-Feb-1997
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE SWINPT                                *
!                                                                            *
!  Subroutine SWINPT This module reads records to define the usage practices *
!                    at each usage location for overland water pathways      *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    02/28/86 (Converted to PC)                              *
!  Last Modified:    11-Feb-1997  DLS                                        *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: SUBROUTINE USEDAT
!     Calls: NONE
!     Common blocks referenced: AQCLAS, PROD, RECLAS, SWPATH, AGCLAS, DEVICE,
!                               LOCNAM, CRLOC, AQPATH, PTHUSE, DWPATH
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     variable    S    CHAR    Argument  nnnnnnnnnnnnnnnnnnnnnn
!                 U    INT     Internal
!                 S/U  REAL    Common
!                      DBLE    External
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!
!
!
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE SWINPT(SETDATA,NLINES)
!
!==== COMMON Block Definitions ===============================================
!
        include 'AQCLAS.FTN'
        include 'PFLAGS.FTN'
        include 'SWPATH.FTN'
        include 'AGCLAS.FTN'
        include 'DEVICE.FTN'
        include 'LOCNAM.FTN'
        include 'crloc.FTN'
        include 'aqpath.FTN'
        include 'pthuse.FTN'
        include 'dwpath.FTN'
        include 'dermdat.FTN'
        INCLUDE 'PARMTR.PAR'
!
!==== DIMENSION Statements ===================================================
!
       CHARACTER*(*) SETDATA(LINEMAX)
!
!==== Variable Declarations ==================================================
!
      character*4 cltrtl
      INTEGER NLINES, GETINT
!
!==== DATA Statements ========================================================
!
      DATA IZ/0/
      DATA ED/0./
!
!----- Start calculations.  Initialize error index and data arrays. -------
!
      IERR  = 0
      KSDR  = 0
      KSAQ  = 0
      KSIR  = 0
      KSR   = 0
      DSWED = 0.
!
!----- Read one record for each location, Record Type 6c --------------------
!
      IDR = GETINT(SETDATA,NLINES,'KSDR          ',IZ,IZ,IZ,IZ,IZ,IZ)
      IAQ = GETINT(SETDATA,NLINES,'KSAQ          ',IZ,IZ,IZ,IZ,IZ,IZ)
      IIR = GETINT(SETDATA,NLINES,'KSIR          ',IZ,IZ,IZ,IZ,IZ,IZ)
      ISR = GETINT(SETDATA,NLINES,'KSR           ',IZ,IZ,IZ,IZ,IZ,IZ)
      DSWED =GETREAL(SETDATA,NLINES,'DSWED         ',IZ,IZ,IZ,IZ,IZ,IZ)
!
!----- Test value given for exposure duration -------------------------------
!
        IF(DSWED.LE.0.) THEN   ! Exposure duration must be positive
          WRITE(NERR,'(A,F10.4)') ' Surface Water Exposure Duration incorrect, must be positive: ',DSWED
          IERR=IERR+1
        ENDIF
!
!----- Test value given for agricultural index ------------------------------
!
        IF(IIR.GT.NAGC) THEN
          WRITE(NERR,1000) IIR,NAGC
          IERR=IERR+1
        ENDIF
!
!----- Test value given for aquatic food index ------------------------------
!
        IF(IAQ.LT.0.OR.IAQ.GT.NAQC) THEN
          WRITE(nerr,1002) IAQ,NAQC
          IERR=IERR+1
        ENDIF
!
!----- Set values into arrays ----------------------------------------------
!
        IF(IERR.GT.0) GO TO 10
        KSDR  =IDR
        KSAQ  =IAQ
        KSIR  =IIR
        KSR   =ISR
   10 CONTINUE
!-----
        if( ltrtl(2) .eq. 0 ) cltrtl = ' No '
        if( ltrtl(2) .eq. 1 ) cltrtl = ' Yes'
!-----
!----- modify usage location printout --- JWB - 01/18/90
!-----
          WRITE(nels,2000) char(12)
 2000     FORMAT(a1,20x,'USAGE LOCATION DATA FOR SURFACE WATER PATHWAY')
!-----
!----- writing out the usage location parameter chart(s)
!
        write(nels,2001) ksdr
 2001   format('Drink Index:  (None)',1x,I10)
        write(nels,2003)  ksr
 2003   format('Rec Index:    (None)',1x,I10)
        write(nels,2005) ksaq
 2005   format('Aquatic Index:(None)',1x,I10)
        write(nels,2008) ksir
 2008   format('Irr Index:    (None)',1x,I10)
        write(nels,3009) twat(2)
 3009   format('H20 Trans Time:  d  ',1p,1x,E10.3)
        write(nels,3010)  cltrtl
 3010   format('Water Treated:yes/no  ',1p,4x,a4)
        if( ltrtl(2) .eq. 1 ) WRITE(nels,'(a)') ' The default water purification factor is 1.00 (no removal)'
!
            write(nels,2101) dswed
 2101       format('Exposure Duration: y',1p,1x,E10.3)
          IF(KEXPTH(8).GT.0) THEN
            write(nels,3012) tfsh
 3012       format('Fish Time Delay:days',1p,1x,E10.3)
          ENDIF
          IF(KEXPTH(9).GT.0) THEN
            write(nels,3013) tinv
 3013       format('Inv time delay: days',1p,1x,E10.3)
          ENDIF
       iok=0 
       IF(KEXPTH(4).GT.0.OR.KEXPTH(5).GT.0) iok=iok+1
       IF(KEXPTH(6).GT.0.OR.KEXPTH(7).GT.0) iok=iok+1
       IF(iok.gt.0) then        
        write(nels,3008) cirr(2)
 3008   format('Irr. Rate:    L/mo  ',1p,1x,E10.3)
        ENDIF
!
          write(nels,'(79(''=''))')
!
      RETURN
!
!----- Format statements ----------------------------------------------------

!
  100 FORMAT(5A4,4I5,4E10.2/8E10.2)
 1000 FORMAT('ERROR IN AGRICULTURAL CLASS INDEX IN SWINPT,'/  &
      ' VALUE GIVEN =',I4,', THE MAXIMUM ALLOWED IS ',I3)
 1001 FORMAT('END-0F-FILE ENCOUNTERED ON UNIT ',I3,' WHILE READING' &
       /' SURFACE WATER TRANSPORT USAGE LOCATION DATA')
 1002 FORMAT('ERROR IN AQUATIC FOOD CLASS INDEX IN SWINPT,'/    &
      ' VALUE GIVEN =',I4,', THE MAXIMUM ALLOWED IS ',I3)
 1003 FORMAT('ERROR IN RECREATIONAL CLASS INDEX IN SWINPT,'/  &
      ' VALUE GIVEN =',I4,', THE MAXIMUM ALLOWED IS ',I3)
!========================== END OF MODULE SWINPT ============================

      END
!  AIRSET.FOR   EXPOS                Version Date: 10-Jan-97
!   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE AIRSET                                *
!                                                                            *
!  Subroutine AIRSET puts values into arrays for input from the ATO file     *
!                                                                            *
!  Written by:       DL Strenge                                              *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    10-Jan-97                                               *
!  Last Modified:    10-Jan-97      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: EDUP/EXPOS
!     Called by: ATODAT
!     Calls: NONE
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     ARRAY     S/U    REAL    Argument  Array of parameter values for output
!     VALIN      U     REAL    Argument  Array of input values
!     ICM        U     INT     Argument  Position of chain member of interest
!     NX1        U     INT     Argument  Number of positions to be filled
!     IX2        U     INT     Argument  Position of data for second co-ordinate
!     XFAC       U     real    argument  Units correction factor 1.0 - y/y, 3.15e7 s/yr
!==== Modification History ===================================================
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!  
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE AIRSET(NPROD,TOTAL,VALIN,ICM,NX1,IX2,XFAC)
!
!----- Include statements ----------------------------------------------------
!
      INCLUDE 'CONIN.FTN'
!
!---- Variable Type Declarations ---------------------------------------------
!
      LOGICAL TOTAL
      INTEGER NX1,IX2,ICM,NPROD
      REAL VALIN(20), XFAC
!
!---- Start of Analysis
!
      DO IX = 1,NX1
         IF(NPROD.EQ.1) THEN        
            AIRCIN(ICM,IX,IX2) = AIRCIN(ICM,IX,IX2) + VALIN(IX)
         ELSE
            IF(TOTAL) THEN
               DEPTOT(ICM,IX,IX2) = VALIN(IX) * XFAC
            ELSE
               DEPTOT(ICM,IX,IX2) = DEPTOT(ICM,IX,IX2) + VALIN(IX) * XFAC
            ENDIF
         ENDIF
      END DO
      RETURN
!----- END OF MODULE AIRSET -------------------------------------------------
      END

!  AIRSAVE.FOR  AHAZX                Version Date: 24-Jun-97
!   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE AIRSAVE                               *
!                                                                            *
!  Subroutine AIRSAVE puts values into arrays for input from the ATO file,   *
!          for interpolation over time
!                                                                            *
!  Written by:       DL Strenge                                              *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    24-Jun-97                                               *
!  Last Modified:    24-Jun-97      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZX
!     Called by: ATODAT
!     Calls: NONE
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     ARRAY     S/U    REAL    Argument  Array of parameter values for output
!     VTIN       U     REAL    Argument  Array of input values
!     ICM        U     INT     Argument  Position of chain member of interest
!     NX1        U     INT     Argument  Number of positions to be filled
!     IX2        U     INT     Argument  Position of data for second co-ordinate
!     XFAC       U     REAL    Argument  Units conversion factor: 1.0 - yr/yr, 3.15e7 s/yr
!==== Modification History ===================================================
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!  
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE AIRSAVE(VALIN,IOP,TOTAL,NX1,IX2,XFAC)
!
!---- Include statements -----------------------------------------------------
!
      INCLUDE 'VTIN.FTN'
      include 'device.ftn'
!
!---- Variable Type Declarations ---------------------------------------------
!
      LOGICAL TOTAL
      INTEGER NX1,IX2,IOP
      REAL VALIN(20),XFAC
!
!---- Start of Analysis
!
!
!-----  test input array for negative values
!
      do ix = 1,nx1
        if(valin(ix).lt.0.) then
           write(nerr,100) ix,valin(ix)
 100  format(' negative valin ',i4,1pe10.2)
        endif
      end do
!
      DO IX = 1,NX1
        IF(IOP.EQ.1) THEN
          VTIN(IOP,IX,IX2)=VTIN(IOP,IX,IX2)+VALIN(IX)
        ELSE
          IF(TOTAL) THEN
             VTIN(IOP,IX,IX2) = VALIN(IX) * XFAC
          ELSE
             VTIN(IOP,IX,IX2)=VTIN(IOP,IX,IX2)+VALIN(IX) * XFAC
          ENDIF
        END IF
      END DO
      RETURN
!
!----- END OF MODULE AIRSAVE ------------------------------------------------
!
      END

!   MEPAS HAZ2: GWINPT.FOR             Version Date: 11-Feb-97
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE GWINPT                                *
!                                                                            *
!  Subroutine GWINPT This module reads records to define the usage practices *
!                    at each usage location for ground water pathways.       *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    02/28/86 (Converted to PC)                              *
!  Last Modified:    11-Feb-1997     DLS                                     *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: SUBROUTINE USEDAT
!     Calls: NONE
!     Common blocks referenced: GWPATH, AGCLAS, DEVICE, LOCNAME,
!      PROD, CRLOC, AQPATH, PTHUSE, DWPATH
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     variable    S    CHAR    Argument  nnnnnnnnnnnnnnnnnnnnnn
!                 U    INT     Internal
!                 S/U  REAL    Common
!                      DBLE    External
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     --- ------------------------------------------------------
!     
!==== SUBROUTINE CALL  ========================================================
!
      SUBROUTINE GWINPT(SETDATA,NLINES)
!
!==== COMMON Block Definitions ===============================================
!
        include 'GWPATH.FTN'
        include 'AGCLAS.FTN'
        include 'DEVICE.FTN'
        include 'PFLAGS.FTN'
        include 'LOCNAM.FTN'
        include 'crloc.FTN'
        include 'aqpath.FTN'
        include 'pthuse.FTN'
        include 'dwpath.FTN'
        include 'dermdat.FTN'
        INCLUDE 'PARMTR.PAR'
!
!==== DIMENSION Statements ===================================================
!
       CHARACTER*(*) SETDATA(LINEMAX)
!
!==== Variable Declarations ==================================================
!
      character*4 cltrtl
      INTEGER NLINES
      INTEGER GETINT
!
!==== DATA Statements ========================================================
!
      DATA IZ/0/
!
!----- Start calculations.  Initialize error index and data arrays. -------
!
      IERR=0
      KGDR=0
      KGIR=0
!
!----- Read one record for each location, Record Type 6a --------------------
!
      IDR = GETINT(SETDATA,NLINES,'KGDR          ',IZ,IZ,IZ,IZ,IZ,IZ)
      IIR = GETINT(SETDATA,NLINES,'KGIR          ',IZ,IZ,IZ,IZ,IZ,IZ)
      ED = GETREAL(SETDATA,NLINES,'DGWED         ',IZ,IZ,IZ,IZ,IZ,IZ)
!
!----- Test value given for exposure duration -------------------------------
!
        IF(ED.LE.0.) THEN   ! Exposure duration must be positive
          WRITE(NERR,'(A,F10.4)') ' Groundwater Exposure Duration incorrect, must be positive: ',DGWED
          IERR=IERR+1
        ENDIF
        DGWED = ED
!
!----- Test value given for agricultural index ------------------------------
!
        IF(IIR.GT.NAGC) THEN
          WRITE(NERR,1000) IIR,NAGC
          IERR=IERR+1
          GO TO 999
        ENDIF
!
!----- Set values into arrays ----------------------------------------------
!
        KGDR=IDR
        KGIR=IIR
!
!-----
!
        if( ltrtl(1) .eq. 0 ) cltrtl = ' No '
        if( ltrtl(1) .eq. 1 ) cltrtl = ' Yes'
!-----
      WRITE(NELS,2000) char(12)
 2000 FORMAT(a1,20x,'USAGE LOCATION DATA FOR GROUNDWATER PATHWAY')
!-----
!----- writing out the usage location parameter chart(s)
!-----
        write(NELS,2001) kgdr
 2001   format('Drink Index:  (None)',1x,I10)
        write(NELS,2003) kgir
 2003   format('Irr Index:    (None)',1x,I10)
        write(NELS,2101) dgwed
 2101   format('Exposure Duration: y',1p,1x,E10.3)
        write(NELS,2008) cirr(1)
 2008   format('Irr. Rate: L/mo/m2  ',1p,1x,E10.3)
        write(NELS,2012) tgrw(1,1)
 2012   format('Harvest Time: days  ',1p,1x,E10.3)
        write(NELS,2009) twat(1)
 2009   format('Transit Time: days  ',1p,1x,E10.3)
        write(NELS,2010) cltrtl
 2010   format('Water Treated:yes/no  ',4x,a4)
        if( ltrtl(1) .eq. 1 ) WRITE(nels,'(a)') ' The default water purification factor is 1.00 (no removal)'
 2999 continue
!
 999   RETURN
!
!----- Format Statements ---------------------------------------------------
!
  100 FORMAT(5A4,2I5,9E10.2)
 1000     FORMAT('ERROR IN AGRICULTURAL CLASS INDEX IN GWINPT,'/&
          ' VALUE GIVEN =',I4,', THE MAXIMUM ALLOWED IS ',I3)
 1001 FORMAT('END-0F-FILE ENCOUNTERED ON UNIT ',I3,' WHILE READING'&
       /' GROUNDWATER TRANSPORT USAGE LOCATION DATA')
!===================== END OF MODULE GWINPT ==================================
      END
!   MEPAS HAZSIF: GETYPE.FOR             Version Date: 13-Dec-93
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE GETYPE                                *
!                                                                            *
!  Subroutine GETYPE returns pollutant type integers, MTING, and MTINH       *
!                    for a pollutant with type index MTYPIN                  *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    13-Dec-93                                               *
!  Last Modified:    13-Dec-1993       DLS                                   *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ
!     Called by: SUBROUTINE GROUND, SURFWT, ATMOS, FOODIN, SOILP
!     Calls: SUBROUTINES NONE
!     Common blocks referenced: NONE
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     MTYPIN     U     INT     Argument  Type index for the pollutant
!     MTING      S     INT     Argument  Pollutant type for ingestion
!     MTINH      S     INT     Argument  Pollutant type for inhalation
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     --- ------------------------------------------------------
!
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE GETYPE(MTYPIN,MTING,MTINH)
!
!==== COMMON Block Definitions ===============================================
!
!       NONE
!
!==== DIMENSION Statements ===================================================
!
!     NONE
!
!==== Variable Declarations ==================================================
!
!     NONE
!
!==== DATA Statements ========================================================
!
!--- Start of Calculations ----------------------------------------------
!
!----- Set Pollutant type index values for ingestion and inhalation
!
       MTING = 3         ! Radionuclides
       MTINH = 3         ! Radionuclides
       IF(MTYPIN.EQ.2) THEN
         MTING = 2
         MTINH = 2
       ELSE IF(MTYPIN.EQ.3) THEN
         MTING = 1
         MTINH = 2
       ELSE IF(MTYPIN.EQ.4) THEN
         MTING = 2
         MTINH = 1
       ELSE IF(MTYPIN.EQ.5) THEN
         MTING = 1
         MTINH = 1
       END IF
!
      RETURN
!=================== END OF MODULE GETYPE ===============================
      END
!   MEPAS HAZ2: DRINPT.FOR             Version Date: 11-Feb-97
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE DRINPT                                *
!                                                                            *
!  Subroutine DRINPT This subroutine reads the input parameters for the      *
!                    exposure pathway of direct radiation dose based on      *
!                    measured dose rates as selected locations the vicinity  *
!                    of a site.                                              *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    01/19/89 (Converted to PC)                              *
!  Last Modified:    11-Feb-1997  DLS                                        *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: NONE
!     Calls: NONE
!     Common blocks referenced: DRPATH, LOCNAM, DEVICE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE DRINPT(SETDATA,NLINES)
!
!==== COMMON Block Definitions ===============================================
!
        include 'DRPATH.FTN'
        include 'DEVICE.FTN'
        include 'PARMTR.PAR'
!
!==== DIMENSION Statements ===================================================
!
!     NONE
!
!==== Variable Declarations ==================================================
!
      CHARACTER*(*) SETDATA(LINEMAX)
!
!==== DATA Statements ========================================================
!
        DRHALF = 0.
      ED = GETREAL(SETDATA,NLINES,'DRED          ',IZ,IZ,IZ,IZ,IZ,IZ)
      THDR = GETREAL(SETDATA,NLINES,'TDIRD         ',IZ,IZ,IZ,IZ,IZ,IZ)
!
        IF(ED.LE.0.) ED = 70.
        DRED = ED
        IF(THDR.LE.0.) THDR = 1.E8
        DRHALF = ALOG(2.)/THDR
!
        WRITE(nels,200) DRATE,DRED,THDR
 200  FORMAT(' Direct radiation rate ',1P,E10.2,' rad/hr'/  &
             ' Exposure duration     ',E10.2,' days'/       &
             ' Radiation half life   ',E10.2,' days')
  10  CONTINUE
      RETURN
      END
!============= END OF MODULE DRINPT ======================================
!   MEPAS HAZ2: CHAIN.FOR             Version Date: 07-Nov-95
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE CHAIN                                 *
!                                                                            *
!  Subroutine CHAIN  evaluate decay chain activities, time-integrals of      *
!                    activity, or activity after constant deposition for a   *
!                    chain of radioactive pollutants.
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    20-Apr-1994                                             *
!  Last Modified:    07-Nov-95      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: SUBROUTINE (several)
!     Calls: SUBROUTINE EXFCT
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!  Name        Type             Purpose
!  ---------   ------    ---------------------------------------------------
!   T           Real     Time over which decay calculation is to be performed,
!                        in units consistent with AB and radiological decay.
!   AB(20)      Real     Non-radiological loss-rate constant for each member,
!                        inverse time units.
!   AM(20)      Real     Initial activity of each chain member, activity
!                        units.
!   AO(20)      Real     Final activity (or time integral) of each nuclide in
!                        the decay chain, activity units ( or activity-days)
!   INTGRL      Integer  Control flag set 0 to calculate decay for time T,
!                        set to 1 to calculate time integral over T,
!                        set to 2 to calculate double time integral over T.
!   CHEM        Logical  Flag to indicate if the decay calculation is to be
!                        evaluated for a chemical.  If so, the conversion from
!                        activity to mass units is not performed.
!
!==== Modification History ===================================================
!
!     Date         Who      Modification Description
!     --------   ---------  ------------------------------------------------------
!
!     12-Nov-04  DL Strenge Added CHEM flag to allow evaluation of chemical decay
!
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE CHAIN (T, AB, AM, AO, INTGRL,CHEM)
!
!==== COMMON Block Definitions ===============================================
!
      INCLUDE 'DECAY.FTN'
!
!==== DIMENSION Statements ===================================================
!
      DOUBLE PRECISION, DIMENSION (210) ::  A
      DOUBLE PRECISION, DIMENSION (20) :: AMD, EXPO, ABD
      DOUBLE PRECISION :: SUMPR, ASUM
      DOUBLE PRECISION :: ARG, TERM
      DOUBLE PRECISION, DIMENSION (20) :: AM, AO, AB
!      LOGICAL :: CHEM                                       ! 12-Nov-2004 DL Strenge
!
!==== Variable Declarations ==================================================
!
!    None
!
!==== DATA Statements ========================================================
!
!    None
!   Initialize chain parameters
!
      DO 113 IJK = 1, NUC
!   Initialize exponential term
        EXPO(IJK) = 0.0D0 
!   Calculate effective removal constant, sum of radiological and
!     non-radiological constants
        ABD(IJK) = AB(IJK) + AL(IJK)
!
!---- Divide by lambda (radionuclides only) to convert activity to atom units
!
        IF(CHEM) THEN      !XX                                 ! 12-Nov-2004 DL Strenge
           AMD(IJK) = AM(IJK)                         ! 12-Nov-2004 DL Strenge
        ELSE                                                ! 12-Nov-2004 DL Strenge
           IF (AL(IJK) .NE. 0.0D0) AMD(IJK) = AM(IJK) / AL(IJK)
         ENDIF                                               ! 12-Nov-2004 DL Strenge
  113 CONTINUE
!
!   Initialize coefficient array to zero--
      N2N = NUC * (NUC-1) / 2 + NUC                     
!
      DO 100 IJK = 1, N2N 
        A(IJK) = 0.0D0 
  100 CONTINUE 
!
!     Do loop on chain members,  Maximum = NUC
      DO 5 J = 1, NUC                                                       
!
!       Calculate exponential for current nuclide
        ARG=-ABD(J) * DBLE(T)
!
        IF (INTGRL .GT. 0)    THEN 
!    Test for error in argument value, must be negative or zero
          IF (ARG .GT. 0.0D0) THEN
          ELSE
!    For integral form: (1 - DEXP (ARG) ) / AB,  For INTGRL > 0
            IF (-ARG .GT. 50.0D0) THEN
              EXPO(J) = 1.0D0 / ABD(J) 
            ELSEIF (-ARG .GT. 0.001D0) THEN
              EXPO(J) = (1.0D0 - DEXP(ARG)) / ABD(J) 
            ELSE
              FX = -(DLOG10(-ARG)) 
              I = 10 - IFIX(FX) 
              IF (I .LT. 2)  I=2 
              TERM = DBLE(T)
              EXPO(J) = -ARG / ABD(J) 
              DO 13 IT = 2,I 
                TERM = (TERM*ARG) / DBLE(IT) 
                EXPO(J) = EXPO(J) + TERM 
  13          CONTINUE 
            ENDIF 
          ENDIF 
        ELSE                                                             
!    For standard form: EXP(ARG), for INTGRL = 0
          IF (-ARG .GT. 50.0) THEN 
            EXPO(J) = 0.0D0 
          ELSE 
            EXPO(J) = DEXP (ARG)                                         
          ENDIF 
        ENDIF                                                            
        IF(INTGRL.GT.1) THEN
!    Double integral form: (T-(1-DEXP(ARG))/AB)AB,  For INTGRL > 1
!    (Above EXPO(J) values may be used in this evaluation)
            IF (-ARG .GT. 50.0) THEN 
              EXPO(J) = (T- 1.D0 / ABD(J) ) / ABD(J)
            ELSEIF (-ARG .GT. 0.001) THEN 
              EXPO(J) = (T - EXPO(J)) / ABD(J) 
            ELSE 
              FX = -(DLOG10(-ARG)) 
              I = 11 - IFIX(FX) 
              IF (I .LT. 3)  I=3 
              TERM = T*T/2.0D0
              EXPO(J) = TERM
              DO 14 IT = 3,I 
                TERM = (TERM*ARG) / DBLE(IT) 
                EXPO(J) = EXPO(J) + TERM 
  14          CONTINUE 
            ENDIF 
        ENDIF 
!
!   Set starting index for term array A
        JJ = J * (J-1) / 2                                               
!
!   Set number of daughters following current radionuclide
!     (chain position minus one)
        J1 = J - 1                                                        
!   Calculate coefficient contributions for daughters (if any)
        IF(J1 .GT. 0)   THEN                                              
!   Set maximum number of parents to consider for current radionuclide (<=2)
          IMAX = MIN0 (J1, 2)                                             
          DO 3 M = 1, J1                                                  
            DO 2 L = M, J1                                                
              DO 1 I = 1, IMAX                                            
!
!     coefficient calculation
                IF (IFRM(I,J) .EQ. L)    THEN                             
                  A(M+JJ) = A(M+JJ) + DK(I,J) * AL(L) * A(M+L * (L-1)/2)  
                ENDIF                                                     
!
    1         CONTINUE                                                    
    2       CONTINUE                                                      
!   Calculate final contribution to coefficient for current chain member
            A(M+JJ) = A(M+JJ) / (ABD(J) - ABD(M))                         
!
    3     CONTINUE                                                        
!
        ENDIF   
!
!
        ASUM = 0.0D0 
        IF (J1 .EQ. 0) GO TO 11 
          DO 12 IRAP = 1, J1 
            JK = JJ + IRAP 
            ASUM = ASUM + A(JK) 
   12     CONTINUE 
   11   CONTINUE
!   Calculate last coefficient for current chain member
        A(J+JJ) = AMD(J) - ASUM  
        SUMPR = 0.0D0 
        J2 = J 
        DO 8884 IN = 1, J2 
          JK = JJ + IN 
          SUMPR = SUMPR + EXPO(IN) * A(JK) 
 8884   CONTINUE 
!
!----   Calculate activity of current chain member ----------------------------
!       Multiply by lambda (Radionuclides only) and return to single precision
!
        IF(CHEM) THEN                           ! 12-Nov-2004 DL Strenge
           AO(J) = SUMPR                        ! 12-Nov-2004 DL Strenge
        !   AO(J) = SNGL (SUMPR)                ! 12-Nov-2004 DL Strenge
        ELSE                                    ! 12-Nov-2004 DL Strenge
           AO(J) = SUMPR * AL(J)
        !   AO(J) = SNGL (SUMPR * AL(J))
        ENDIF                                   ! 12-Nov-2004 DL Strenge
!
    5 CONTINUE                                                            
      RETURN
!----------------  CHAIN.FOR  -----------------------------------------c
      END
!   MEPAS HAZ2: CHEMCHAIN.FOR             Version Date: 27-Oct-2004
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE CHEMCHAIN                             *
!                                                                            *
!  ****************************                                              *
!                                                                            *
!  NOTE: THIS MODULE IS NOT CURRENTLY USED.  DECAY FOR CHEMICALS IS          *
!        PERFORMED USING CHAIN                                               *
!                                                                            *
!  ****************************                                              *
!                                                                            *
!  Subroutine CHAIN  evaluate decay chain activities, time-integrals of      *
!                    activity, or activity after constant deposition for a   *
!                    chain of chemical pollutants.                           *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    27-Oct-2004 from CHAIN for radionuclides                *
!  Last Modified:    27-Oct-2004    DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: SUBROUTINE (several)
!     Calls: SUBROUTINE EXFCT
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!  Name        Type             Purpose
!  ---------   ------    ---------------------------------------------------
!   T           Real     Time over which decay calculation is to be performed,
!                        in units consistent with AB and chemical decay.
!   AB(20)      Real     Loss-rate constant for each member,
!                        inverse time units.
!   AM(20)      Real     Initial amount of each chain member, mass units.
!
!   AO(20)      Real     Final amount (or time integral) of each chemical in
!                        the decay chain, mass units ( or mass-days)
!   INTGRL      Integer  Control flag set 0 to calculate decay for time T,
!                        set to 1 to calculate time integral over T,
!                        set to 2 to calculate double time integral over T.
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!
!
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE CHEMCHAIN (T, AB, AM, AO, INTGRL, MEDIUM)
!
!==== DATA Block Definitions ===============================================
!
      USE Decay_Chem_Mod
!
!     Allow no implicit parameter definitions
!
      IMPLICIT NONE
!
!
!==== DIMENSION Statements ===================================================
!
      DOUBLE PRECISION, DIMENSION (210) ::  A
      DOUBLE PRECISION, DIMENSION (20) :: AMD, EXPO, ABD
      DOUBLE PRECISION :: SUMPR, ASUM
      DOUBLE PRECISION :: ARG, TERM, FX
      DOUBLE PRECISION, DIMENSION (20) :: AM, AO, AB
      REAL(KIND=4) :: T,FY
      INTEGER :: INTGRL, MEDIUM
      INTEGER :: NUC                 ! Number of chain members for current medium
      INTEGER :: IJK, J, L, M        ! Loop indices
      INTEGER :: N2N                 ! Array size calculated for coefficients
      INTEGER :: JJ, J1, J2, JK      ! Count integers
      INTEGER :: IRAP, IMAX, IN, I, IT ! Count integers
!
!==== Variable Declarations ==================================================
!
!    None
!
!==== DATA Statements ========================================================
!
!    None
!   Initialize chain parameters
!
      NUC = NUCC(MEDIUM)
      DO 113 IJK = 1, NUC
!   Initialize exponential term
        EXPO(IJK) = 0.0D0 
!   Calculate effective removal constant, sum of decay and
!     physical loss constants
        ABD(IJK) = DBLE (AB(IJK) + ALC(IJK,MEDIUM))
!   Divide by lambda to convert activity to atom units (NOT FOR CHEMICALS)
!!        IF (AL(IJK) .NE. 0.0) AMD(IJK) = DBLE (AM(IJK) / AL(IJK))
  113 CONTINUE
!
!   Initialize coefficient array to zero--
      N2N = NUC * (NUC-1) / 2 + NUC
!
      DO 100 IJK = 1, N2N 
        A(IJK) = 0.0D0 
  100 CONTINUE 
!
!     Do loop on chain members,  Maximum = NUC
      DO 5 J = 1, NUC
!
!       Calculate exponential for current nuclide
        ARG=-ABD(J) * T                                                     
!
        IF (INTGRL .GT. 0)    THEN 
!    Test for error in argument value, must be negative or zero
          IF (ARG .GT. 0.0) THEN  
          ELSE
!    For integral form: (1 - DEXP (ARG) ) / AB,  For INTGRL > 0
            IF (-ARG .GT. 50.0) THEN 
              EXPO(J) = 1.0D0 / ABD(J) 
            ELSEIF (-ARG .GT. 0.001) THEN 
              EXPO(J) = (1.0D0 - DEXP(ARG)) / ABD(J) 
            ELSE
              FX = -(DLOG10(-ARG)) 
              FY=FX
              I = 10 - IFIX(FY) 
              IF (I .LT. 2)  I=2 
              TERM = T 
              EXPO(J) = -ARG / ABD(J) 
              DO 13 IT = 2,I
                TERM = (TERM*ARG) / DBLE(IT) 
                EXPO(J) = EXPO(J) + TERM 
  13          CONTINUE 
            ENDIF 
          ENDIF 
        ELSE                                                             
!    For standard form: EXP(ARG), for INTGRL = 0
          IF (-ARG .GT. 50.0) THEN 
            EXPO(J) = 0.0D0 
          ELSE 
            EXPO(J) = DEXP (ARG)                                         
          ENDIF 
        ENDIF                                                            
        IF(INTGRL.GT.1) THEN
!    Double integral form: (T-(1-DEXP(ARG))/AB)AB,  For INTGRL > 1
!    (Above EXPO(J) values may be used in this evaluation)
            IF (-ARG .GT. 50.0) THEN 
              EXPO(J) = (T- 1.D0 / ABD(J) ) / ABD(J)
            ELSEIF (-ARG .GT. 0.001) THEN 
              EXPO(J) = (T - EXPO(J)) / ABD(J) 
            ELSE 
              FX = -(DLOG10(-ARG))
              FY = FX
              I = 11 - IFIX(FY) 
              IF (I .LT. 3)  I=3 
              TERM = T*T/2.0D0
              EXPO(J) = TERM
              DO 14 IT = 3,I 
                TERM = (TERM*ARG) / DBLE(IT) 
                EXPO(J) = EXPO(J) + TERM 
  14          CONTINUE 
            ENDIF 
        ENDIF 
!
!   Set starting index for term array A
        JJ = J * (J-1) / 2                                               
!
!   Set number of daughters following current radionuclide
!     (chain position minus one)
        J1 = J - 1                                                        
!   Calculate coefficient contributions for daughters (if any)
        IF(J1 .GT. 0)   THEN                                              
!   Set maximum number of parents to consider for current radionuclide (<=2)
          IMAX = MIN0 (J1, 2)                                             
          DO 3 M = 1, J1                                                  
            DO 2 L = M, J1                                                
              DO 1 I = 1, IMAX                                            
!
!     coefficient calculation
                IF (IFRMC(I,J,MEDIUM) .EQ. L)    THEN
                  A(M+JJ) = A(M+JJ) + DKC(I,J,MEDIUM) * ALC(L,MEDIUM) * A(M+L * (L-1)/2)
                ENDIF                                                     
!
    1         CONTINUE                                                    
    2       CONTINUE                                                      
!   Calculate final contribution to coefficient for current chain member
            A(M+JJ) = A(M+JJ) / (ABD(J) - ABD(M))                         
!
    3     CONTINUE                                                        
!
        ENDIF   
!
!
        ASUM = 0.0D0 
        IF (J1 .EQ. 0) GO TO 11 
          DO 12 IRAP = 1, J1 
            JK = JJ + IRAP 
            ASUM = ASUM + A(JK) 
   12     CONTINUE 
   11   CONTINUE
!   Calculate last coefficient for current chain member
        A(J+JJ) = AMD(J) - ASUM  
        SUMPR = 0.0D0 
        J2 = J 
        DO 8884 IN = 1, J2 
          JK = JJ + IN 
          SUMPR = SUMPR + EXPO(IN) * A(JK) 
 8884   CONTINUE 
!   Calculate amount of current chain member
!       Return to single precision
        AO(J) = SNGL (SUMPR)
!        AO(J) = SNGL (SUMPR * AL(J))
!
    5 CONTINUE                                                            
      RETURN
!----------------  CHEMCHAIN.FOR  -----------------------------------------c
      END
!   MEPAS AHAZ: LAMAX.FOR             Version Date: 18-Dec-95               
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************C
!
!  Subroutine LAMAX  finds the pollutant index for the maximum soil and
!           groundwater environmental half times for use in CROPS                             *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    07-Nov-95  (From HAZ2 version)                          *
!  Last Modified:    18-Dec-95      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZ
!     Called by: GROUND, ATMOS, SWINPT
!     Calls: None
!     Common blocks referenced: DECAY, PSET3
!
!==== Significant Parameter Designation and Description ======================
!
!  Parameters in argument list
!  Name        Type             Purpose
!  ---------   ------    ---------------------------------------------------
!---------------------------------------------------------------------------
!
!  Major Local Parameters:
!     Name    Type                  Purpose
!   -------  ------  ---------------------------------------------------
!    IPIN      INT      Parent index for chain to consider
!    IEIN      INT      Progeny position in chain
!    IPSX      INT      Output index for pollutant for which to use SLAM
!    IPGX      INT      Output index for pollutant for which to use GLAM
! --------------------------------------------------------------------------
!
!==== Modification History ===================================================
!
!     Date       Who  Modification Description
!     --------   ---  --------------------------------------------------------
!   
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE LAMAX(IPIN,IEIN,IPSX,IPGX)
!
!==== COMMON Block Definitions ===============================================
!
      INCLUDE 'DECAY.FTN'
      INCLUDE 'PSET3.FTN'
!
!==== DIMENSION Statements ===================================================
!
!     None
!
!==== Variable Declarations ==================================================
!
      INTEGER IPIN,IEIN,IPSX,IPGX
!
!==== DATA Statements ========================================================
!
!     None
! ****************************************************************************
!
      IPGX = IPIN
      IPSX = IPIN
      IF(NUC.GT.1.AND.IEIN.GT.1) THEN
        JIPIN = IEIN
        IPSX = JIPIN
        IPGX = JIPIN
        SLMX = SLAM(JIPIN)
        GLMX = GLAM(JIPIN)
        DO IE = 1, IEIN-1
          JTP = IE
          SLTX = SLAM(JTP)
          IF(SLMX.GT.SLTX) THEN
            SLMX = SLTX
            IPSX = JTP
          ENDIF
          GLTX = GLAM(JTP)
          IF(GLMX.GT.GLTX) THEN
            GLMX = GLTX
            IPGX = JTP
          ENDIF
        END DO
      ENDIF
      RETURN
      END        
!-------------  End file LAMAX.FOR -------------------------
!   MEPAS AHAZ: ATMOS.FOR              Version Date: 24-Jun-1997             *
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.     *
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE ATMOS                                 *
!                                                                            *
!  Subroutine ATMOS  controls exposure pathway calculations for the          *
!             atmospheric transport pathways.                                *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    09/05/85 (Converted to PC)                              *
!  Last Modified:    24-Jun-1997  DLS                                       *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ
!     Called by: SUBROUTINE EXPOS
!     Calls: NONE
!     Common blocks referenced: AGCLAS, AIRDAT, ATPATH, DEVICE, PSET2, PTHUSE, RISK
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter      Set/
!     Name           Used  Type  Location  Parameter Description
!     -------------- ----  ----  --------  ------------------------------
!     CSOIL(20)       U  REAL*8  Argument  Soil concentration from previous
!                                          periods deposition and accumulation
!     CA(20)          U  REAL*8  Argument  Average air concentration for the in-
!                                          halation pathway
!     CS(20)          U  REAL*8  Argument  Average soil concentration for the
!                                          soil ingestion and resuspension paths
!
!==== Modification History ===================================================
!
!     Date      Who  Modification Description
!     --------  ---  ---------------------------------------------------------
!
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE ATMOS(IT,IPOL,CSOIL,CA,CS)
!
!==== COMMON Block Definitions ===============================================
!
      include 'AGCLAS.FTN'
      include 'AIRDAT.FTN'
      include 'ATPATH.FTN'
      include 'DECAY.FTN'  
      include 'DERMDAT.FTN'
      include 'DEVICE.FTN'
      include 'klass.FTN'
      include 'PFLAGS.FTN'
      include 'PSET1.FTN'
      include 'PSET3.FTN'
      include 'PTHUSE.FTN'
      include 'TSOIL.FTN'
      include 'CRLOC.FTN'  
      INCLUDE 'DEL.FTN'
      INCLUDE 'IXY.FTN'
!
!==== DIMENSION Statements ===================================================
!
      DOUBLE PRECISION, DIMENSION (20) :: CSOIL, CA, CS, AB
      REAL CAA,CSA,CSL
      REAL, DIMENSION (10) :: DOUT
      DOUBLE PRECISION, DIMENSION (20) ::  APF
      REAL DCUT
!
!==== Variable Declarations ==================================================
!
!     NONE
!
!==== DATA Statements ========================================================
!
      DATA ALPL/0.049511/
      DATA C10EM3/1.E-3/
      DATA DCUT/1.E-30/
!
!----- Start calculations. Initialize daily intake array ----------------
!
      IP=1
      IPATH=4
      CAA = CA(1)
      CSA = CS(1)
      CSL = CSOIL(1)
      DO IN=1,NUC
        AB(IN) = DBLE(ALPL)
        DO IE = 1,24
          DEL(IE,IN) = 0.0
        END DO
      END DO
!
!      CALL GETYPE(MTYPE(IP),MTING,MTINH)   ! Not used.
!
!----- If crops included calculate intake -------------------------------
!
      IAD=0
      IF(KAAG.GT.0) THEN
!
      DO 10 IC=4,7
!
!----- Test exposure pathway flags for crops to be included
!
        IF(KEXPTH(IC).LE.0) GO TO 10
          IC3 = IC-3
          APF(1) = 1.D0
!
!--- calculate radionuclide ingrowth on plant surfaces during one season
!
          IF(NUC.GT.1) THEN
            DO IE = 2,NUC
            APF(IE) = 0.D0
            END DO
          END IF
          INTGRL = 1
!          IF(MTYPE(1).GT.1) AL(1) = ALAM(1)    ! ** Deactivated - 15 November 2004, DL Strenge
          TIM = TGRW(IC3,3)
          CALL CHAIN(TIM,AB,APF,APF,INTGRL,CHEM)
          DO IE = 1,NUC         
            APFACT(IE) = APF(IE)
          END DO                
!          IF(MTYPE(IP).LE.1) THEN    ! Radionuclides  ! ** Deactivated - 15 November 2004, DL Strenge
          DO IE = 1,NUC
             CALL LAMAX(IP,IE,ISMX,IGMX)
             CSL = CSOIL(IE)
             CALL CROPS(IPATH,IP,IAD,IC3,CAA,CSA,CSL,CROP,IP,IE,ISMX,IGMX)
             DEL(IC,IE) = CROP
          END DO
!          ELSE
!            CALL CROPS(IPATH,IP,IAD,IC3,CAA,CSA,CSL,CROP,IPOL,1,IP,IP)
!              DEL(IC,1) = CROP
!          ENDIF
! ** Updated 15 November 2004, DL Strenge
   10   CONTINUE
!
      ENDIF
!
!----- If people live at this location, calculate inhalation, external,
!      soil ingestion and soil dermal absorption
!
      IF(KAIN.GT.0) THEN     ! This flag indicates people live here
!
!----- Dose from ingestion of deposition onto soil -------------------
!----- Test exposure pathway flag to include ingestion of soil (14)
!
      IE = 1
      IF(KEXPTH(14).GT.0) THEN
        SFAC = C10EM3/(TAS*RHOAS)
        DO IE = 1,NUC
           DEL(14,IE) = ( ASOIL(IE)*CS(1)+CSOIL(IE) ) * SFAC
        END DO
! ** Updated 15 November 2004, DL Strenge
      ENDIF
      WRITE(nels,*) ' In ATMOS, ASOIL(1),CS(1),CSOIL(IE) ',ASOIL(1),CS(1),CSOIL(1),SFAC
!
!----- Dose from dermal contact with deposition onto soil
!      (New intake added November 1992)
!
      IF(KEXPTH(15).GT.0) THEN
!
!-- Test pollutant type (radionuclide or chemical)
!
        SFAC = C10EM3/(TAS*RHOAS)
! ** Updated 15 November 2004, DL Strenge
          DO IE = 1,NUC
              DEL(15,IE) = (ASOIL(IE)*CS(1)+CSOIL(IE)) * SFAC
          END DO
! ** Updated 15 November 2004, DL Strenge
      ENDIF          ! End of If on KEXPTH(15), soil dermal absorption
!
!----- Test exposure pathway flag to include air inhalation
!----- Air inhalation pathway (18)
!
      IF(KEXPTH(18).GT.0) THEN
! ** Updated 15 November 2004, DL Strenge
         DO IE = 1,NUC
            DEL(18,IE)= CA(IE)
         END DO
! ** Updated 15 November 2004, DL Strenge
      ENDIF
!
!----- Dose from inhalation of resuspended soil ------------------------
!      (New pathway added October 1993)
!
      IF(KEXPTH(19).GT.0) THEN
! ** Updated 15 November 2004, DL Strenge
!        IF(MTYPE(1).LE.1) THEN                ! Radionuclides
          DO IE = 1,NUC
              DEL(19,IE) = (ASOIL(IE)*CS(1)+CSOIL(IE)) * RESFAC
          END DO
!        ELSE                       ! Chemicals
!            DEL(19,1) = (ASOIL(1)*CS(1) + CSOIL(1) ) * RESFAC
!        ENDIF
! ** Updated 15 November 2004, DL Strenge
      ENDIF
!
!----- External dose from deposited radionuclides --------------------
!----- Test exposure pathway flag to include external dose from soil
!
      IF(KEXPTH(23).GT.0) THEN
        SFAC = C10EM3/(TAS*RHOAS)
        IF(MTYPE(1).EQ.1) THEN         ! Radionuclides only
          DO IE = 1,NUC
              DEL(23,IE) = (ASOIL(IE) * CS(1)+CSOIL(IE)) * SFAC
          END DO
        ENDIF
      ENDIF
!
!----- Air external pathway (24).  Time-integral of air conc. ----------------
!      Evaluated as average concentration times exposure duration (yr)
!
      IF(KEXPTH(24).GT.0) THEN                   ! Radionuclides only
        IF(MTYPE(1).EQ.1) THEN
          DO IE = 1,NUC
            DEL(24,IE) = CA(IE) 
          END DO
        ENDIF
      ENDIF
!
      ENDIF            ! End of IF on KAIN
!
!---- load array for output for each radionuclide ----------------------------
!
      DO IN = 1,NUC
        DOUT(1) = DEL(4,IN)
        DOUT(2) = DEL(5,IN)
        DOUT(3) = DEL(6,IN)
        DOUT(4) = DEL(7,IN)
        DOUT(5) = DEL(14,IN)
        DOUT(6) = DEL(15,IN)
        DOUT(7) = DEL(18,IN)
        DOUT(8) = DEL(19,IN)
        DOUT(9) = DEL(23,IN)
        DOUT(10)= DEL(24,IN)
        DO JJ = 1,10
          IF(DOUT(JJ).LE.DCUT) DOUT(JJ) = 0.0
        END DO
!
!----  Write one line for this chain member
!
        WRITE(NETM,1000) IN,IT,IX1,IX2,DOUT
 1000   FORMAT(I3,I5,2I3,1P,10E10.3)
      END DO
!
      RETURN
!============ END OF MODULE ATMOSR ============================================
      END
!   MEPAS HAZ2: BLKREAD.FOR             Version Date: 21-Nov-1995
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE BLKREAD                               *
!                                                                            *
!  Subroutine BLKREAD This data block allows changes to default              *
!                     parameter values.                                      *
!                                                                            *
!  Written by:       Dennis Strenge/J.G. Droppo                              *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    11/03/91 (Converted to PC)                              *
!  Last Modified:    21-Nov-1995  DLS                                        *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: NONE
!     Calls: NONE
!     Common blocks referenced: CRLOC, AGCLAS, AQCLAS, DEVICE, RECLAS, PTHUSE, CRPATH, RISKCM
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter      Set/
!     Name           Used  Type  Location  Parameter Description
!     -------------- ----  ----  --------  ------------------------------
!     variable        S    CHAR  Argument  nnnnnnnnnnnnnnnnnnnnnn
!                     U    INT   Internal
!                     S/U  REAL  Common
!                          DBLE  External
!
!==== Modification History ===================================================
!
!   Date      Who  Modification Description
!  --------   ---  ------------------------------------------------------
!
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE  BLKREAD(SETDATA,NLINES,IERR)
!
!==== COMMON Block Definitions ===============================================
!
        INCLUDE 'PARMTR.PAR'
        include 'AGCLAS.FTN'
        include 'AQCLAS.FTN'
        include 'AQPATH.FTN'
        INCLUDE 'COUPLE.FTN'
        include 'CRLOC.FTN'
        include 'CRPATH.FTN'
        INCLUDE 'DERMDAT.FTN'
        include 'DEVICE.FTN'
        INCLUDE 'DRPATH.FTN'
        include 'DWPATH.FTN'
        include 'PTHUSE.FTN'
        INCLUDE 'RISKCM.FTN'
!
!==== DIMENSION Statements ===================================================
!
!==== Variable Declarations ==================================================
!
      CHARACTER*(*) SETDATA(LINEMAX)
      INTEGER :: GETINT
      INTEGER :: IERR
!
!==== DATA Statements ========================================================
!
       DATA IZ/0/
!
!    Write heading for report of changed data
!
      WRITE(nels,300) 
 300  FORMAT(/' Default parameter summary ')
!
!         Parameter definitions for COMMON Block CRPATH
!
        DO IP = 1,4
          VAL = GETREAL(SETDATA,NLINES,'YLD           ',IP,IZ,IZ,IZ,IZ,IZ)
          IF(VAL.GT.0.) YLD(IP) = VAL
        END DO
          WRITE(nels,203) YLD
 203      FORMAT(' YLD(1)    ',F10.2,' Leafy Vegetable Yield',  &
          ' (kg/m2)'/                                           &
          ' YLD(2)    ',F10.2,' Other Vegetable Yield (kg/m2)'/ &
          ' YLD(3)    ',F10.2,' Beef Feed Yield (kg/m2)'/       &
          ' YLD(4)    ',F10.2,' Milk Cow Feed Yield (kg/m2)')
!
!
!-----    Parameter definitions for COMMON Block AQPATH
!         Shellfish and crustacea consumption delay time
!
           TFSH = GETREAL(SETDATA,NLINES,'TFSH          ',IZ,IZ,IZ,IZ,IZ,IZ)
           WRITE(nels,211) TFSH
 211       FORMAT(' TFSH(1)  ',F10.2,' Finfish intake delay (d)')
           TINV = GETREAL(SETDATA,NLINES,'TINV          ',IZ,IZ,IZ,IZ,IZ,IZ)
           WRITE(nels,213) TINV
 213       FORMAT(' TINV(1)  ',F10.2,' Shellfish intake delay ','(d)')
           VAL = GETREAL(SETDATA,NLINES,'TISSUE        ',IZ,IZ,IZ,IZ,IZ,IZ)     ! 26-Apr-01 DLS
           FISH_TISSUE = 1.0                                                    ! 26-Apr-01 DLS
           IF(VAL.GT.0.) FISH_TISSUE = VAL                                      ! 26-Apr-01 DLS
           WRITE(nels,212) FISH_TISSUE
 212       FORMAT(' FISH_TISSUE',F10.2,' Finfish BAF tissue correction')
!
!---- Read food product delay prior to ingestion -----------------------------
!
           DO IP = 1,4
           VAL  = GETREAL(SETDATA,NLINES,'TCRP          ',IP,IZ,IZ,IZ,IZ,IZ)
           IF(VAL.GT.0.0) TCRP(IP) = VAL
           ENDDO 
           WRITE(nels,215) (TCRP(I),I=1,4)
 215       FORMAT(' TCRP(1)  ',F10.2,' Leafy vegetable delay',     &
           ' time to ingestion (d)'/                               &
           ' TCRP(2)  ',F10.2,' Other vegetable delay time ',      &
           'to ingestion (d)'/                                     &
           ' TCRP(3)  ',F10.2,' Meat delay time to ingestion (d)'/ &
           ' TCRP(4)  ',F10.2,' Milk delay time to ingestion (d)')
!
!----- Read animal feed and water intake rates -------------------------------
!
           UMT = GETREAL(SETDATA,NLINES,'UMT           ',IZ,IZ,IZ,IZ,IZ,IZ)
           UMK = GETREAL(SETDATA,NLINES,'UMK           ',IZ,IZ,IZ,IZ,IZ,IZ)
           WMT = GETREAL(SETDATA,NLINES,'WMT           ',IZ,IZ,IZ,IZ,IZ,IZ)
           WMK = GETREAL(SETDATA,NLINES,'WMK           ',IZ,IZ,IZ,IZ,IZ,IZ)
           WRITE(nels,216)  UMT,UMK,WMT,WMK
 216       FORMAT(' UMT      ',F10.2,' Meat animal feed intake',  &
           ' kg/d'/                                               &
           ' UMK      ',F10.2,' Milk animal feed intake kg/d'/    &
           ' WMT      ',F10.2,' Meat animal water intake L/d'/    &
           ' WMK      ',F10.2,' Milk animal water intake L/d')
!
!----- Read agricultural soil areal density and plant retention fraction -----
!
        VAL = GETREAL(SETDATA,NLINES,'DEN           ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.) DEN = VAL
        VAL = GETREAL(SETDATA,NLINES,'RET           ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.) RET = VAL
        WRITE(nels,217) DEN,RET         
 217       FORMAT(' DEN      ',F10.2,' Agricultural areal soil',  &
           ' density, kg/m2'/                                     &
           ' RET      ',F10.2,' Retention fraction on plants')
!
!----- Read translocation factors --------------------------------------------
!
           DO IP = 1,4
           TRN(IP) = GETREAL(SETDATA,NLINES,'TRN           ',IP,IZ,IZ,IZ,IZ,IZ)
           END DO           
           WRITE(nels,218)  (TRN(I),I=1,4)  
 218       FORMAT(' TRN(1)   ',F10.2,' Leafy vegetable ',         &
           ' translocation factor'/                               &
           ' TRN(2)   ',F10.2,' Other vegetable translocation',   &
           ' factor'/                                             &
           ' TRN(3)   ',F10.2,' Meat animal feed translocation',  &
           ' factor'/                                             &
           ' TRN(4)   ',F10.2,' Milk animal feed translocation',  &
           ' factor')
!
!----- Read feed and water fractions contaminated for each animal -------------
!
        DO IP = 1,2
           FFC(IP) = GETREAL(SETDATA,NLINES,'FFC           ',IP,IZ,IZ,IZ,IZ,IZ)
           FWC(IP) = GETREAL(SETDATA,NLINES,'FWC           ',IP,IZ,IZ,IZ,IZ,IZ)
        END DO
           WRITE(nels,219) FFC(1), FFC(2), FWC(1), FWC(2)
 219       FORMAT(' FFC(1)   ',F10.2,' Fraction of feed contam',   &
           'inated, meat animals'/                                 &
           ' FFC(2)   ',F10.2,' Fraction of feed contaminated,',    &
           ' milk animals'/                                        &
           ' FWC(1)   ',F10.2,' Fraction of water contaminated,',   &
           ' meat animals'/                                        &
           ' FWC(2)   ',F10.2,' Fraction of water contaminated,',   &
           ' milk animals')
!
!      Parameter definitions for COMMON Block CRLOC
!----- Read irrigation rates for groundwater ---------------------------------
!
!
        IP = 1
        CIRR(1) = GETREAL(SETDATA,NLINES,'CIRR          ',IP,IZ,IZ,IZ,IZ,IZ)
           WRITE(nels,220)  CIRR(1)
 220       FORMAT(' CIRR-1   ',F10.2,' GW irrigation rate',' (L/m2/mo)')
!
!----- Read irrigation rates for groundwater ---------------------------------
!
!
        IP = 2
        CIRR(2) = GETREAL(SETDATA,NLINES,'CIRR          ',IP,IZ,IZ,IZ,IZ,IZ)
           WRITE(nels,221)  CIRR(2)
 221       FORMAT(' CIRR-2   ',F10.2,' SW irrigation rate',' (L/m2/mo)')
!
!----- Read animal soil intake rates -----------------------------------------
!
       DO IP = 1,2
         USL(IP) = GETREAL(SETDATA,NLINES,'USL           ',IP,IZ,IZ,IZ,IZ,IZ)
       END DO
           WRITE(nels,222) USL(1), USL(2)
 222       FORMAT(' USL(1)   ',F10.2,' Meat animal soil intake', &
           ' rate (kg/d)'/                                       &
           ' USL(2)   ',F10.2,' Milk animal soil intake rate',    &
           ' (kg/d)')
!
!----- Read fraction of irrigation water that is contaminated ----------------
!
        IP = 1
        FIRR(1) = GETREAL(SETDATA,NLINES,'FIRR          ',IP,IZ,IZ,IZ,IZ,IZ)
        IP = 2
        FIRR(2) = GETREAL(SETDATA,NLINES,'FIRR          ',IP,IZ,IZ,IZ,IZ,IZ)
          WRITE(nels,226)  FIRR(1),FIRR(2)
 226      FORMAT(' FIRR-1   ',F10.3,' GW irrigation fraction'/    &
           ' FIRR-2   ',F10.3,' SW irrigation fraction')
!
!------ Read growing periods for plants and feed crops -----------------------
!
        DO IT = 1,5
          DO IP =1,4
            TGRW(IP,IT) = GETREAL(SETDATA,NLINES,'TGRW          ',IP,IT,IZ,IZ,IZ,IZ)
          END DO
          IF(IT.EQ.1) THEN
            WRITE(nels,230)  (TGRW(I,1),I=1,4)
 230        FORMAT(' TGRW-1,1 ',F10.2,' GW leafy vegetable ,',  &
            ' growing period (d)'/                              &
            ' TGRW-2,1 ',F10.2,' GW other vegetation growing',  &
            ' period (d)'/                                      &
            ' TGRW-3,1 ',F10.2,' GW meat animal feed growing',  &
            ' period (d)'/                                      &
            ' TGRW-4,1 ',F10.2,' GW milk animal feed growing',  &
            ' period (d)')
          ELSE IF(IT.EQ.2) THEN
           WRITE(nels,234) (TGRW(I,2),I=1,4)  
 234       FORMAT(' TGRW-1,2 ',F10.2,' SW leafy vegetable ,',   &
           ' growing period (d)'/                               &
           ' TGRW-2,2 ',F10.2,' SW other vegetation growing',   &
           ' period (d)'/                                       &
           ' TGRW-3,2 ',F10.2,' SW meat animal feed growing',   &
           ' period (d)'/                                       &
           ' TGRW-4,2 ',F10.2,' SW milk animal feed growing',   &
           ' period (d)')
          ELSE IF(IT.EQ.3) THEN
           WRITE(nels,236)  (TGRW(I,3),I=1,4)  
 236       FORMAT(' TGRW-1,3 ',F10.2,' AT leafy vegetable ,',   &
           ' growing period (d)'/                               &
           ' TGRW-2,3 ',F10.2,' AT other vegetation growing',   &
           ' period (d)'/                                       &
           ' TGRW-3,3 ',F10.2,' AT meat animal feed growing',   &
           ' period (d)'/                                       &
           ' TGRW-4,3 ',F10.2,' AT milk animal feed growing',   &
           ' period (d)')
          ELSE IF(IT.EQ.4) THEN
!          ELSE IF(IT.EQ.5) THEN   ! not sure which is correct for MS at this location
           WRITE(nels,237)  (TGRW(I,4),I=1,4)  
 237       FORMAT(' TGRW-1,4 ',F10.2,' MS leafy vegetable ,',   &
           ' growing period (d)'/                               &
           ' TGRW-2,4 ',F10.2,' MS other vegetation growing',   &
           ' period (d)'/                                       &
           ' TGRW-3,4 ',F10.2,' MS meat animal feed growing',   &
           ' period (d)'/                                       &
           ' TGRW-4,4 ',F10.2,' MS milk animal feed growing',   &
           ' period (d)')
          ENDIF
        END DO
!
!---- Read water purification plant control flag (>0 treatment of DW) --------
!
        IP = 1             ! Ground water
        LTRTL(1) = GETINT(SETDATA,NLINES,'LTRTL         ',IP,IZ,IZ,IZ,IZ,IZ)
        WRITE(nels,238)  LTRTL(1)
 238    FORMAT(' LTRTL-1  ',I10,' GW treatment flag,')
        IP = 2             ! Surface water
        LTRTL(2) = GETINT(SETDATA,NLINES,'LTRTL         ',IP,IZ,IZ,IZ,IZ,IZ)
        WRITE(nels,242) LTRTL(2)
 242    FORMAT(' LTRTL-2  ',I10,' SW treatment flag,')
!
!----- Read domestic water distribution system delay time --------------------
!
        IP = 1
        TWAT(1) = GETREAL(SETDATA,NLINES,'TWTR          ',IP,IZ,IZ,IZ,IZ,IZ)
           WRITE(nels,244)  TWAT(1)
 244    FORMAT(' TWTR-1,1 ',F10.2,' GW delivery time,')
        IP = 2
        TWAT(2) = GETREAL(SETDATA,NLINES,'TWTR          ',IP,IZ,IZ,IZ,IZ,IZ)
        WRITE(nels,248) TWAT(2)
 248    FORMAT(' TWTR-1,2 ',F10.2,' SW delivery time,')
!
!----- Read indoor air volatilization factor (Andelman factors) --------------
!
          ANDFC = GETREAL(SETDATA,NLINES,'ANDFC         ',IZ,IZ,IZ,IZ,IZ,IZ)
          ANDFR = GETREAL(SETDATA,NLINES,'ANDFR         ',IZ,IZ,IZ,IZ,IZ,IZ)
          ANDFO = GETREAL(SETDATA,NLINES,'ANDFO         ',IZ,IZ,IZ,IZ,IZ,IZ)
          WRITE(nels,268) ANDFC,ANDFR,ANDFO
 268      FORMAT(' ANDFC    ',1PE10.1,' Andelman factor for',         &
          ' chemicals (L/m^3)'/                                        &
          ' ANDFR    ',E10.1,' Andelman factor for Rn222 (L/m^3)'/     &
          ' ANDFO    ',E10.1,' Andelman factor for other pollutants', &
          ' (L/m^3)')
!
!----- read soil and sediment thickness and density --------------------------
!
        VAL = GETREAL(SETDATA,NLINES,'TSS           ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.0) TSS = VAL
        VAL = GETREAL(SETDATA,NLINES,'RHOSS         ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.0) RHOSS  = VAL
        VAL = GETREAL(SETDATA,NLINES,'TAS           ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.0) TAS = VAL
        VAL = GETREAL(SETDATA,NLINES,'RHOAS         ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.0) RHOAS  = VAL
          WRITE(nels,270)  TSS, RHOSS, TAS, RHOAS
 270      FORMAT(' TSS      ',1PE10.1,' Shoreline sediment',   &
          ' thickness (m)'/                                    &
          ' RHOSS    ',E10.1,' Shoreline sediment density,',   &
          ' (g/cm3)'/                                          &
          ' TAS      ',1PE10.1,' Soil thickness, atmospheric', &
          ' (m)'/                                              &
          ' RHOAS    ',E10.1,' Soil density, atmospheric ,',   &
          ' (g/cm3)')
!
!----- Read resuspension factor for soil inhalation pathway ------------------
!
        VAL = GETREAL(SETDATA,NLINES,'RESFAC        ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.0) then
           RESFAC = VAL
           WRITE(nels,271) RESFAC
 271       FORMAT(' RESFAC   ',1PE10.1,' Resuspension factor,',  &
           ' (m-1)')
        ELSE
           IERR = IERR + 1
           WRITE(NERR,'(A,1P,E10.3)') ' Bad value for resuspension factor: must be > 0, found: ',RESFAC
        ENDIF
!
!----- Read soil thickness and density for measured soil pathway -------------
!      and mass loading
!
        VAL = GETREAL(SETDATA,NLINES,'TMS           ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.0) TMS = VAL
        VAL = GETREAL(SETDATA,NLINES,'RHOMS         ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.0) RHOMS = VAL
        VAL = GETREAL(SETDATA,NLINES,'CML           ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.0) CML = VAL
        WRITE(nels,273) CML   
 273    FORMAT(' CML      ',1PE10.1,' Mass loading factor,',     &
        ' (kg/m^3)')
!
!---- End of input -----------------------------------------------------------
!
      RETURN
!
!================ END OF MODULE BLKREAD ====================================
!
      END
!   MEPAS AHAZ: SRETEN.FOR             Version Date: 01-Jan-97
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE SRETEN                                *
!                                                                            *
!  Subroutine SRETEN estimates the soil retention factor for a period of T   *
!                    years for a decay constant AL.                          *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    07/11/95 (Converted from HAZ2 version)                  *
!  Last Modified:    01-Jan-97      DLS                                       *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZ
!     Called by: SUBROUTINE EXPOS, GROUND, SURFWT
!     Calls: SUBROUTINE EXFCT
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     T          U     REAL    Argument  Time (years) for evaluation
!     ALIN(20)   U     REAL    Argument  loss rate constant (1/days) for parent
!     SRF       S/U    REAL    Argument  calculated accumulation factor
!     C365      S/U    REAL    Internal  units conversion (days/year)
!==== Modification History ===================================================
!
!     Date      Who  Modification Description
!     --------  ---  ---------------------------------------------------------
! 
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE SRETEN(T,ALIN,SRF)
!
!==== COMMON Block Definitions ===============================================
!
      INCLUDE 'DECAY.FTN'
      INCLUDE 'PSET1.FTN'
!
!==== DIMENSION Statements ===================================================
!
      REAL SRF(20),TIM,ALIN(20)
      DOUBLE PRECISION, DIMENSION (20) :: SRFOUT, AB, AM
!
!==== Variable Declarations ==================================================
!
      INTEGER INTGRL
!
!==== DATA Statements ========================================================
!
      DATA C365/365.25/
!
!   Set parameters used for all pollutants
!
      IF(T.LE.0.) THEN     ! Check time period, set output to 1 if t <= 0.
        SRF(1) = 0.0       ! No previous deposition
        IF(NUC.GT.1) THEN
          DO IE = 2,NUC
            SRF(IE) = 0.0
          END DO
        ENDIF
      ELSE                 ! Calculate for time = TIM days
        AM(1) = 1.D0
        TIM = T *C365
        INTGRL = 2          ! Deposition integral at constant rate
! ** Updated 15 November 2004, DL Strenge
!        IF(MTYPE(1).EQ.1) THEN   ! Radionuclides
          AB(1) = DBLE(ALIN(1))
          IF(NUC.GT.1) THEN   ! Chain with progeny
            DO IE = 2,NUC     ! Set parameters into decay parameter arrays
              AM(IE) = 0.D0
              AB(IE) = DBLE(ALIN(IE))
            END DO
          ENDIF
          CALL CHAIN(TIM,AB,AM,SRFOUT,INTGRL,CHEM)
          SRF(1) = SRFOUT(1)/TIM
          IF(NUC.GT.1) THEN
            DO IE = 2,NUC
              SRF(IE) = SRFOUT(IE)/TIM
            END DO
          ENDIF
!        ELSE   ! Chemicals
!          AB(1) = 0.D0
!          AL(1) = DBLE(ALIN(1))
!          CALL CHAIN(TIM,AB,AM,SRFOUT,INTGRL)
!          SRF(1) = SRFOUT(1)/TIM
!        ENDIF
! ** Updated 15 November 2004, DL Strenge
      ENDIF
      RETURN
!================ END OF MODULE SRETEN =====================================
      END
!   MEPAS AHAZ: AIRREG.FOR          Version Date:  08-Apr-97
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE AIRREG                                *
!                                                                            *
!  This subroutine controls regional air analysis as a function of time for  *
!  one pollutant at a time.  The input is the concentration time data.       *
!  Output is generated by calls to other subroutines.                        *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    14-Dec-1995 (New for 100 year update)                   *
!  Last Modified:    08-Apr-1997  DLS                                        *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZ
!     Called by: AHAZ
!     Calls: AVGCON
!     Common blocks referenced: DEVICE, PFLAGS
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!      NYEARS     U    INT     Argument  Number of years in each time period
!      NPERDS     U    INT     Argument  Number of time periods to evaluate
! CATIME(100,20,1 U    Real    Argument  Array of air concentration values
! CATIME(100,20,2 U    Real    Argument  Array of air deposition rate values
!  TIMP (20,10)   S    Real    VTIN      Array of time points for air data
!  NTIMP(20)      S    Int     VTIN      Number of time points for air data
!      TSTART     S    Real    Argument  Start of air release rate times
!      TEND       S    Real    Argument  End of air release rate times
!      NCREL(7)   U  Logical   Argument  Class release flag (T if releases)
!      IERR       S    Int     Argument  Error flag for file read errors
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!
!                      deposition and accumulation, TEXD
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE AIRREG(SETDATA,NLINES,NYEARS,NPERDS,CATIME,TSTART,INTERP,IERR)
!
!==== COMMON Block Definitions ===============================================
!
        INCLUDE 'ATPATH.FTN'
        INCLUDE 'PARMTR.PAR'
        INCLUDE 'DEVICE.FTN'
        INCLUDE 'DECAY.FTN'
        INCLUDE 'PSET1.FTN'
        INCLUDE 'PSET3.FTN'
        INCLUDE 'CRLOC.FTN'
        INCLUDE 'LEACH.FTN'
        INCLUDE 'AIRDAT.FTN'
        INCLUDE 'KLASS.FTN'
        INCLUDE 'VTIN.FTN'
!
!==== DIMENSION Statements ===================================================
!
      DIMENSION CATIME(1000,20,2)
!     REAL(KIND=4), ALLOCATABLE :: CATIME(:,:,:)
!      DIMENSION CATIME(100,20,2)
!
!==== Variable Declarations ==================================================
!
      INTEGER IERR
      REAL TSTART
      DOUBLE PRECISION, DIMENSION (20) :: CSOILI,DPRATI,CSOILP
      DOUBLE PRECISION, DIMENSION (20) :: CSLI, CSLP
      DOUBLE PRECISION, DIMENSION (20) :: CAI, CSI
      DOUBLE PRECISION, DIMENSION (20) :: CSDI
      DOUBLE PRECISION, DIMENSION (20) :: TLOSS
      LOGICAL INTERP
      CHARACTER*(*) SETDATA(LINEMAX)
!
!==== DATA Statements ========================================================
!
      DATA C365/365.25/
!
!==== INITIALIZATIONS ========================================================
!
      IPATH = 3   ! Air Regional 
      IERR = 0
      TIM = TSTART
      DELT = FLOAT(NYEARS)
      TDAYS = DELT*C365
      TEXD = ATED*C365
      NYTEXD = ATED
      IF(NYTEXD.LE.0) NYTEXD = 1
!      IF(MTYPE(1).GT.1) AL(1) = SLAM(1)
      DO IN = 1,20
        DPRATI(IN) = 0.D0
        CSLI(IN) = 0.D0
        CSLP(IN) = 0.D0
        TLOSS(IN) = ALEACH(IN) + DBLE(SLAM(IN))
      END DO
!
!==== START OF ANALYSIS ======================================================
!
!---- Loop over time periods for this data set -------------------------------
!
        DO ITIM = 1,NPERDS
!
!---- For first time period, set concentrations to zero ----------------------
!
          IF(ITIM.EQ.1) THEN
            DO IN = 1,NUC
              CSOILI(IN) = 0.D0
              CSOILP(IN) = 0.D0
            END DO
          END IF
!
!----  Get average concentrations for next time period -----------------------
!
          CR = 0.
          DO IN = 1,NUC
            CAI(IN) = 0.D0
            CSI(IN) = 0.D0
          END DO
          DO IN = 1,NUC
!
!---- first set air concentration for chain member and exposure time ---------
!
              IF(INTERP) THEN
                INX = IN
                IT1 = ITIM-1
                TIM = TSTART + FLOAT(IT1)*DELT
                CAI(IN) = SETAIRC(TIM,NYTEXD,CATIME(1,1,1),TIMP(1,1),1,INX,IERR)
                IF(IERR.GT.0) GO TO 999
                CSI(IN) = SETAIRC(TIM,NYTEXD,CATIME(1,1,1),TIMP(1,1),2,INX,IERR)
                IF(IERR.GT.0) GO TO 999
              ELSE
                CAI(IN) = CATIME(1,IN,1)
                CSI(IN) = CATIME(1,IN,2)
              ENDIF
!
          END DO  ! Loop on chain members
          IF(NUC.GT.1) THEN
            DO IN = 1,NUC
              ICLASS(IN) = ICLS(IN)
            END DO
          ENDIF
!
!---- Call EXPOS to evaluate health impacts for current time period ----------
!
          IPOL = 1
          ILOC = 1    ! 1 causes calculation of individual risk
!
!---- First call EXPOS to evaluate individual risk ---------------------------
!
          CALL EXPOS(IPATH,ILOC,IPOL,ITIM,CSOILI,CAI,CSI,DELT,IERR)
          IF(IERR.GT.0) GO TO 998
!
!---- Get average deposition over time increment -----------------------------
!
          CR = 0.
          DO IN = 1,NUC
            CAI(IN) = 0.D0
            CSI(IN) = 0.D0
          END DO
          DO IN = 1,NUC
              INX = IN
              IF(INTERP) THEN
                CR = SETAIRC(TIM,NYEARS,CATIME(1,1,1),TIMP(1,1),2,INX,IERR)
                IF(IERR.GT.0) GO TO 999
                CSI(IN) = CR
              ELSE
                CSI(IN) = CATIME(1,IN,2)
              ENDIF
         END DO
          TIM = TIM + DELT
!
!---- First set contribution from amount present at end of previous period ---
           IF(ITIM.GT.1) THEN
             INTGRL = 0
             CALL CHAIN(TDAYS,TLOSS,CSLI,CSLI,INTGRL,CHEM)   ! Individual
             CALL CHAIN(TDAYS,TLOSS,CSLP,CSLP,INTGRL,CHEM)   ! Population
           END IF
!
!---- Next add amount deposited from air during the period -------------------
!
           DPRATI(1) = CSI(1)/C365
           INTGRL = 1
           CALL CHAIN(TDAYS,TLOSS,DPRATI,CSDI,INTGRL,CHEM)
!         WRITE(NELS,'(A,1P,2E10.3)') ' Amount deposited from air during period:',TEXD,CSDI(1)
           DO IN = 1,NUC
             CSLI(IN) = CSLI(IN)+CSDI(IN)
           END DO
!
!---- Next calculate average concentration in soil over next exposure period -
!
           CALL CHAIN(TEXD,TLOSS,CSLI,CSOILI,INTGRL,CHEM)
           CALL CHAIN(TEXD,TLOSS,CSLP,CSOILP,INTGRL,CHEM)
           DO IN = 1,NUC
             CSOILI(IN) = CSOILI(IN) / TEXD
             CSOILP(IN) = CSOILP(IN) / TEXD
           END DO
!        WRITE(NELS,'(A,1P,2E10.3)') ' Average concentration in soil over next period:',TEXD,CSoili(1)
!
        END DO  ! End of loop on time periods
!
!==== N0RMAL RETURN ==========================================================
!
      RETURN
!
!==== PROCESS READ ERROR CONDITIONS ==========================================
!
998   WRITE(NERR,400) ITIM
 400  FORMAT(' Error in EXPOS call in AIRREG during time period ',I6)
      IERR = IERR + 1
      RETURN
999   WRITE(NERR,300) ITIM
 300  FORMAT(' Error in retrieving air release rates from AVGCON ',  &
             'in AIRREG'/' during time period ',I6)
      IERR = IERR + 1
      RETURN
      END
!
!=================== END OF MODULE AIRREG ====================================
!
!   MEPAS AHAZX: AHAZX.FOR             Version Date:  14-Oct-98
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           MAIN PROGRAM                                     *
!                                                                            *
!  Main Program AHAZ Annual HAZard analysis evaluation program               *
!                 The main program HAZ controls the calculation of human     *
!                 health impacts  for each pollutant                         *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    01-Nov-1995 (Converted from HAZ2)                       *
!  Last Modified:    14-Oct-1998  DLS                                        *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZ
!     Called by: NONE
!     Calls: SUBROUTINES PDATIN, USEDAT, EXPOS, GDK
!     Common blocks referenced: BLKDAT, BACKGD, DEVICE, H3, C14, SWPATH,
!                               HPDAT, PSET1, AIRDAT, ATPATH, TITLS, COUPLE,
!                               LOCNAM, SLPATH, FDPATH, PSET3, POS,
!                               RISKCM, PSET2
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     variable    S    CHAR    Argument  nnnnnnnnnnnnnnnnnnnnnn
!                 U    INT     Internal
!                 S/U  REAL    Common
!                      DBLE    External
!     GDKFAC(20)  -    REAL    Internal  Deposition decay correction factor
!                                        for each pollutant (air pathway), yr.
!     IPDAT       -    int     Internal  Flag to print library data.
!     KNPATH(7)   -    INT     Internal  Number of usage locations for each
!                                        transport pathway
!     NIN         s/u  INT     C-DEVICE  Logical unit for input records.
!     NPOL        S/U  INT     C-PSET1   Number of pollutants for current run.
!     TRITM       S/U  LOGIC   C-H3      Logical flag to indicate if current
!                                        pollutant is tritium
!     CARBON      S/U  LOGIC   C-C14     Logical flag to indicate if current
!                                        pollutant is carbon-14
!
!==== Modification History ===================================================
!
!    Date     Who  Modification Description
!  ---------  ---  ------------------------------------------------------
!   02Apr10   JGD  Code cleanup
!
!==== SUBROUTINE CALL ========================================================
!
      PROGRAM AHAZX
!
! *** Global variables
!
      USE Errors_Mod
!
!
!==== COMMON Block Definitions ===============================================
!
!
        INCLUDE 'PARMTR.PAR'
        INCLUDE 'DEVICE.FTN'
        INCLUDE 'H3.FTN'
        INCLUDE 'C14.FTN'
        INCLUDE 'CONIN.FTN'
        INCLUDE 'SWPATH.FTN'
        INCLUDE 'PFLAGS.FTN'
        INCLUDE 'PSET1.FTN'
        INCLUDE 'PSET3.FTN'
        INCLUDE 'AIRDAT.FTN'
        INCLUDE 'ATPATH.FTN'
        INCLUDE 'TITLS.FTN'
        INCLUDE 'COUPLE.FTN'
        INCLUDE 'LOCNAM.FTN'
        INCLUDE 'SLPATH.FTN'
        INCLUDE 'FDPATH.FTN'
        INCLUDE 'FLUX.FTN'
        INCLUDE 'RISKCM.FTN'
        INCLUDE 'TIMES.FTN'
        INCLUDE 'ETIMES.FTN'
        INCLUDE 'DECAY.FTN'
        INCLUDE 'IXY.FTN'
        INCLUDE 'VTIN.FTN'
!
!==== DIMENSION Statements ===================================================
!
      DIMENSION KNPATH(7)
      REAL(KIND=4), DIMENSION(20) :: CSOIL
      REAL(KIND=4), DIMENSION(20) :: CFOOD
      REAL(KIND=4), ALLOCATABLE :: CWTIME(:,:),TIMES(:,:)
      DIMENSION TRELST(20), TEND(20)
      DIMENSION CATIME(1000,20,2)
!
!==== Variable Declarations ==================================================
!
      CHARACTER*80 GETSTR, EXPNAM
      CHARACTER*32 EXPSRCNA(20), EXPS(20)
      CHARACTER*120 SETDATA(LINEMAX)
      CHARACTER*14 EXPTYPE(20), EXPT(20)
      CHARACTER*14 FUI
      CHARACTER*14 CSM
      CHARACTER*4  H3NAME,CAS(3),C14NAM,THONAM
      CHARACTER*30 EPATH(25)    ! Exposure pathway titles
      CHARACTER*1  B
      CHARACTER*14 RECGW,RECSW,CONID,RNAMES(20)
      CHARACTER*14 RECAIR, RECSOIL,          RECMS
      CHARACTER*32 RECNAM, MODNAME
      CHARACTER*10 MODID                              ! Search ID for contaminant database ID
      INTEGER MODSRCNUM                               ! Number of source modules connected to exp module
      CHARACTER*8  UTIME,UCON
      INTEGER ICONNUM                                 ! Index of Contaminant database ID
      CHARACTER*5 MODSRCTYPE                          ! Source type, search for "con"
      CHARACTER*3 CON                                 ! Source type for testing = "con"
      CHARACTER*14 CONI                               ! Source type for finding database data in GID
      LOGICAL DRONLY, NEW
      LOGICAL FILTER, FOUNDM, INTERP, FOUNDA
      CHARACTER*8 MMDDYY,HHMMSSHH
      CHARACTER (LEN=10) :: DATE, TIME, ZONE
      CHARACTER (LEN=3) :: CMONTH(12),CMO
      CHARACTER*64 Path
      INTEGER :: DT(8)
      INTEGER :: NTIMX
      INTEGER :: NTIMCP
      INTEGER MAXPER, GETINT, TPATH
      LOGICAL SEQI                  ! function for comparing two strings case-insensitive
      LOGICAL GETLOG                 ! Function for extracting a logical parameter value from GID data
      CHARACTER(LEN=400) :: CLINE    ! Command line arguments
!     CALLER is the name of this module, for printing error messages, using PRTERR()
      CHARACTER(LEN=9) :: CALLER = 'AHAZX' ! Name of this routine
      REAL(KIND=4), DIMENSION(20) :: airhalf, swhalf, gwhalf, soilhalf
!
!==== DATA Statements ========================================================
!
      DATA AL2/0.69314718/
      DATA CMONTH/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'/
      DATA FUI/'FUI           '/
      DATA CSM/'CSM           '/
      DATA CON/'con'/
      DATA CONI/'              '/
      DATA IZ/0/
      DATA RECGW  /'Aquifer       '/
      DATA RECSW  /'Surface water '/
      DATA RECAIR /'Air           '/
      DATA RECSOIL/'Soil          '/
      DATA RECMS  /'Measured Soil '/
      DATA B/' '/
      DATA H3NAME/'H3  '/
      DATA THONAM/'THO '/
      DATA C14NAM/'C14 '/
      DATA MAXPER/500/
      DATA DFLAM/1.E-8/
      DATA EPATH(1)/'Drinking water ingestion      '/
      DATA EPATH(2)/'Shower: dermal                '/
      DATA EPATH(3)/'Shower: ingestion             '/
      DATA EPATH(4)/'Leafy vegetable ingestion     '/
      DATA EPATH(5)/'Other vegetable ingestion     '/
      DATA EPATH(6)/'Meat ingestion                '/
      DATA EPATH(7)/'Milk ingestion                '/
      DATA EPATH(8)/'Fin fish ingestion            '/
      DATA EPATH(9)/'Shell fish ingestion          '/
      DATA EPATH(10)/'Swimming: water ingestion     '/
      DATA EPATH(11)/'Swimming: dermal absorption   '/
      DATA EPATH(12)/'Shoreline: dermal             '/
      DATA EPATH(13)/'Shoreline: ingestion          '/
      DATA EPATH(14)/'Soil ingestion                '/
      DATA EPATH(15)/'Soil dermal                   '/
      DATA EPATH(16)/'Special food intake           '/
      DATA EPATH(17)/'Showering inhalation          '/
      DATA EPATH(18)/'Air inhalation                '/
      DATA EPATH(19)/'Soil resuspension inhalation  '/
      DATA EPATH(20)/'Swimming: external            '/
      DATA EPATH(21)/'Boating: external             '/
      DATA EPATH(22)/'Shoreline: external           '/
      DATA EPATH(23)/'Soil: external                '/
      DATA EPATH(24)/'Air: external                 '/
      DATA EPATH(25)/'Indoor Air Inhalation         '/
!
      VERN = ' VERSION 06Apr2010   '
!
!----- Get FACILITY AND WASTE UNIT (file)NAMES ------------------------
!
!
      Path='RMDLIBM.DAT'
!
!---  Read the command line to get parameters for the run
!     GETCL returns command line entries after the program name
!     GETCL is a Lahey Fortran 90 language extension
!!
      ICOMP=2 ! Digital/Compaq FORTRAN
      IF(ICOMP.EQ.1)THEN ! Lahey Fortran 90
        IERR = 0
!        CALL GETCL(CLINE)    ! activate for Lahey Fortran 90
!        READ(CLINE,*,ERR=5,END=5) GIDFNM,HINFNM,SITNUM,EXPNUM,MODNAME
      ELSE  ! COMPAQ FORTRAN
        call GetArg(1,GIDFNM,status)
        if(status.lt.0) GO TO 5  
        call GetArg(2,HINFNM,status) 
        if(status.lt.0) GO TO 5  
        call GetArg(3,CLINE,status)
        if(status.lt.0) GO TO 5  
        READ(CLINE,'(I5)') SITNUM
        call GetArg(4,CLINE,status)                                                         
        if(status.lt.0) GO TO 5 
        READ(CLINE,'(I5)') EXPNUM
        call GetArg(5,MODNAME,status)
        if(status.lt.0) GO TO 5  
     ENDIF
      GO TO 6
 5    ierr = 1
      WRITE(nerr,*) 'Error reading command line'
      GO TO 999
  6   continue
      WRITE(*,*) TRIM(GIDFNM),TRIM(HINFNM),SITNUM,EXPNUM,TRIM(MODNAME)
!
!---- open error message file
!
      open(UNIT=NERR,STATUS='UNKNOWN',FILE=TRIM(HINFNM)//'.ERR')
!
!---- open warning message file
!
      open(UNIT=NWRN,STATUS='UNKNOWN',ACCESS='APPEND',FILE=TRIM(HINFNM)//'.WRN')
!
!     set output unit for PRTERR function to NERR
!
      IRPT = NERR
!
!---- Open Global Input Data (GID) file and find CSM section -----------------
!
      FILTER = .TRUE.
      OPEN (UNIT=NGID, FILE=TRIM(GIDFNM)//'.GID',STATUS='OLD')
!
!  Open and read from the CSM section to get the identifier for the contaminant data (instead of FUI section)
!
      CALL GETSET(NGID,NERR,CSM,NLINES,SETDATA,SITNUM,FILTER)
!
!       Get number of modules in this case for this site
!
        NUMMOD = GETINT(SETDATA,NLINES,'Nummod        ',SITNUM,IZ,IZ,IZ,IZ,IZ)
!
!  Test value for nummod, must be 1 or greater
!
        IF(NUMMOD.LE.0) THEN
           WRITE(nerr,'(a,I5)') ' Bad value for number of modules found in CSM section of GID file - stop',NUMMOD
           ierr = ierr + 1
           GO TO 999
        ENDIF
!
!     Loop on number of modules defined to find the contaminant module name
!
        DO IMOD = 1,NUMMOD
!
!          Get module name for current module
!
           MODID = GETSTR(SETDATA,NLINES,'ModId         ',SITNUM,IMOD,IZ,IZ,IZ,IZ)
!
!          Test to see if this module is the current module name (this MEPAS exposure module)
!
           IF(SEQI(MODID,MODNAME,5)) THEN
!
!             Get number of source modules attached to this exposure module, including DB module
!
              MODSRCNUM = GETINT(SETDATA,NLINES,'ModSrcNum     ',SITNUM,IMOD,IZ,IZ,IZ,IZ)
!
!             Loop on number of source modules to find "con" module
!
              DO ISRC = 1, MODSRCNUM
!
!                Get type of source module
!
                 MODSRCTYPE = GETSTR(SETDATA,NLINES,'ModSrcType    ',SITNUM,IMOD,ISRC,IZ,IZ,IZ)
!
!                Check to see if current source module is of type "con"
!
                 IF(SEQI(MODSRCTYPE,CON,3)) THEN
!                Get type of source module
!
                 CONI = '     '
                 CONI     = GETSTR(SETDATA,NLINES,'ModSrcID      ',SITNUM,IMOD,ISRC,IZ,IZ,IZ)
                    ICONNUM = ISRC
                    GO TO 7
                 ENDIF
              END DO    ! end loop in number of source modules attached to this exposure module
           ENDIF
        END DO          ! end loop on number of modules in the case
!
!  Error condition at this point.  Contaminant database ID not found
!
      WRITE(nerr,'(a)') ' Contaminant module ID not found in CSM section of GID file - stop'
      IERR = IERR + 1
      GO TO 999
   7  CONTINUE
!
!   Get "FUI" section of GID file.
!
      FILTER = .TRUE.
      CALL GETSET(NGID,NERR,FUI,NLINES,SETDATA,SITNUM,FILTER)
      FILTER = .FALSE.
!
!  Set flags for tritium and carbon-14
!
        H3NAME = 'H3  '
        C14NAM = 'C14 '
!
!---- Open output EPF file for results from this analysis --------------------
!
      OPEN (UNIT=NEPF, FILE=TRIM(HINFNM)//'.EPF',STATUS='UNKNOWN')
!
!---- Open output listing file, ELS ------------------------------------------
!
      OPEN (UNIT=NELS, FILE=TRIM(HINFNM)//'.ELS', STATUS='UNKNOWN',IOSTAT=IOS)
!
!----- Report header for output listing file ---------------------------------
!
      WRITE (NELS,'(''Multimedia Environmental Pollutant Assessment'','' System (MEPAS)'')')
      WRITE (NELS,'(''Annual Exposure Version'')')
      WRITE (NELS,'(''MEPAS Human Health Analysis Input Summary'',2x,a19)')VERN
      WRITE (NELS,'(''Pacific Northwest Laboratory'')')
      WRITE (NELS,'(''Richland, WA 99352'')')
      WRITE (NELS,'(''Developed for the U.S. Department of Energy'')')
!
      CALL DATE_AND_TIME(DATE,TIME,ZONE,DT)
      IYR   = DT(1)
      IMO   = DT(2)
      IDAY  = DT(3)
      CMO   = CMONTH(IMO)
      IHR   = DT(5)
      IMINT = DT(6)
      ISEC  = DT(7)
      WRITE(nEls,1111) IDAY,CMO,IYR,IHR,IMINT,ISEC
1111  FORMAT(' Run Date and Time: ',I2,'-',A3,'-',I4,3X,I2,':',I2,':',I2)
      WRITE(*,8900) VERN
 8900 FORMAT(' MEPAS Chronic Exposure Module: ',a)
!
!---- Read exposure source information from GID FUI data set -----------------
!
       IST = SITNUM
       IEX = EXPNUM
!
!---- Get number of exposure sources to consider -----------------------------
!
       NUMEXP = GETINT(SETDATA,NLINES,'ExpNum        ',IST,IZ,IZ,IZ,IZ,IZ)
       EXPNAM = GETSTR(SETDATA,NLINES,'ExpName       ',IST,IEX,IZ,IZ,IZ,IZ)
       EXPX   = GETREAL(SETDATA,NLINES,'ExpX          ',IST,IEX,IZ,IZ,IZ,IZ)
       EXPY   = GETREAL(SETDATA,NLINES,'ExpY          ',IST,IEX,IZ,IZ,IZ,IZ)
       RECNAM = EXPNAM(1:20)
       IF(NUMEXP.LT.IEX.OR.NUMEXP.LE.0) THEN
          WRITE(NERR,4000) IEX,NUMEXP
 4000     FORMAT(' Error in specification of exposure location numbers'/ &
                 ' Input index: ',I3,' Value from GID: ',i3)
          IERR = IERR + 1
          GO TO 999
       END IF
       NEXPTYPE=GETINT(SETDATA,NLINES,'ExpTypeNum    ',IST,IEX,IZ,IZ,IZ,IZ)
       IF(NEXPTYPE.LE.0) THEN
         WRITE(NERR,4004) NEXPTYPE
 4004    FORMAT(' Error in number of exposure types in GID'/    &
                ' Must be > 0, found ',I3)
         IERR = IERR + 1
         GO TO 999
       ENDIF
       DO IT = 1, NEXPTYPE
          EXPTYPE(IT) = GETSTR(SETDATA,NLINES,'ExpType       ',IST,IEX,IT,IZ,IZ,IZ)
          EXPSRCNA(IT)= GETSTR(SETDATA,NLINES,'ExpSRCName    ',IST,IEX,IT,IZ,IZ,IZ)
       END DO
!
!----- Find section in GID for exposure location parameter input --------------
!
      FILTER = .FALSE.
      CALL GETSET(NGID,NERR,MODNAME,NLINES,SETDATA,SITNUM,FILTER)
!
!----- Read default values from GID ------------------------------------------
!
      IERR=0
      CALL BLKREAD(SETDATA,NLINES,IERR)
      IF(IERR.GT.0) GO TO 999
!
! ***************************************************************************
!   Get start time for risk analysis, relative to release time
! ***************************************************************************
!
        CALL GETSTART(SETDATA,NLINES,TSTART)
!
!----- Read input record type 3, control parameters ------------------
!
        IERR=0
        NYEARS = GETINT(SETDATA,NLINES,'NYEARS        ',IZ,IZ,IZ,IZ,IZ,IZ)
        MAXTIM = GETINT(SETDATA,NLINES,'MAXTIM        ',IZ,IZ,IZ,IZ,IZ,IZ)
        DELT = FLOAT(NYEARS)
!
!----- Read Record Type 3, number of locations for each pathway  --------
!----- Values must be between 0 and 10 for water pathways and 0 and 30 --
!----- for the air pathway.  0 indicates pathway not included
!
        DO IP = 1,7
           KNPATH(IP) = GETINT(SETDATA,NLINES,'KNPATH        ',IP,IZ,IZ,IZ,IZ,IZ)
        END DO
!
!----- Write names of transport pathways included -----------------------
!
        write (nels,8802)
 8802   FORMAT(' The following transport routes are available:')
        IF(KNPATH(1).GT.0) THEN
          WRITE(NELS,*) ' - ',RECGW
        ELSEIF(KNPATH(2).GT.0) THEN
          WRITE(NELS,*) ' - ',RECSW
        ELSEIF(KNPATH(3).GT.0) THEN
          WRITE(NELS,*) ' - ',RECAIR
        ELSEIF(KNPATH(5).GT.0) THEN
          WRITE(NELS,*) ' - ',RECMS
!        ELSEIF(KNPATH(6).GT.0) THEN
!          WRITE(NELS,*) ' - ',RECFOOD
        ENDIF
!
!----- Test pathway selection values, print error message ---------------
!
        DRONLY = .FALSE.
        NUMPTH = 0
        DO 15 IPATH=1,7
          KMAX=10
          KK=KNPATH(IPATH)
          IF(KK.GT.0) NUMPTH = NUMPTH + 1        ! count the number of paths
          IF(IPATH.EQ.4.OR.IPATH.GT.5) KMAX=30   ! set maximum usage locations
          IF(KK.LT.0.OR.KK.GT.KMAX) THEN
            WRITE(NERR,1000) IPATH,KK            ! path index out of range
            IERR=IERR+1
          ENDIF
   15   CONTINUE
        IF(IERR.GT.0) GO TO 999
!
!   Test parameter MAXTIM and print to output file
!
          IF(MAXTIM.LE.0) MAXTIM = 10000
          IF(MAXTIM.GT.500000) MAXTIM = 100000
          WRITE(NELS,2208) MAXTIM
 2208     FORMAT(' The analysis will be limited to a total time period' &
                ,' of',I6,' years')
          IF(NYEARS.LE.0) NYEARS = 70
          WRITE(NELS,2209) NYEARS
 2209     FORMAT(' Each exposure time increment is',I5,' years.')
          IF(TSTART.GT.0.) THEN
            WRITE(NELS,2210) TSTART
 2210       FORMAT(' The risk start time is',F10.4,' years after',     &
            ' the event start time.')
          ELSE
            WRITE(NELS,2211)
 2211       FORMAT(' The risk start time is the same as the event',    &
                   ' start time.')
          ENDIF
!
!---- write conditions for indoor inhalation for GW and SW --------------
!
!
   20 CONTINUE
!
! ****************************************************************************
!---- Calculate number of time periods from maximum time and time period length
! ****************************************************************************
!
      NSTART = INT(TSTART)
      NPERDS = (MAXTIM-NSTART)/NYEARS + 1
      IF(NPERDS.LE.0) NPERDS = 1
      IF(NPERDS.GT.MAXPER) NPERDS = MAXPER
!
!---- If water pathways are defined, then open WCF file ----------------------
!
       IF(KNPATH(1).GT.0.OR.KNPATH(2).GT.0) THEN
         WRITE(NELS,'(A,5i5)') ' AT OPEN .WCF, KNPATH = ',KNPATH(1),KNPATH(2),KNPATH(3),KNPATH(4),KNPATH(5)
         OPEN (NWCF, FILE=TRIM(GIDFNM)//'.WCF', STATUS='OLD',IOSTAT=IOS)
         if( ios .ne. 0 ) then
          IOTEST = MOD(IOS,256)
          write(NERR,*) ios
          if( IOTEST .eq. 71 ) then
            write(NERR,'(/,11x,'' WCF FILE NOT FOUND !!!!'')')
            write(NERR,*) char(007)
          else
           write(NERR,'(/,11x,'' ERROR IN *.WCF FILE !!!!'',I10)')IOTEST
          endif
          ierr = ierr + 1
          go to 999
        endif
      ENDIF
!
!---- If air pathways are defined, then open ATO file ------------------------
!
        IF(KNPATH(3).GT.0.OR.KNPATH(4).GT.0) THEN
           WRITE(NELS,'(A,5i5)') ' AT OPEN .ATO, KNPATH = ',KNPATH(1),KNPATH(2),KNPATH(3),KNPATH(4),KNPATH(5)
           OPEN (NATO, FILE=TRIM(GIDFNM)//'.ATO', STATUS='OLD')
        ENDIF
!
!---- If measured soil pathways are defined, then open SCF file --------------
!
        IF(KNPATH(5).GT.0.) THEN
           WRITE(NELS,'(A,5i5)') ' AT OPEN .SCF, KNPATH = ',KNPATH(1),KNPATH(2),KNPATH(3),KNPATH(4),KNPATH(5)
           OPEN (NSCF, FILE=TRIM(GIDFNM)//'.SCF', STATUS='OLD')
        ENDIF
!
!---- NEW STUFF TO FAKE OUT FOLLOWING LOOP
!
      DO IR = 1, NEXPTYPE
        EXPS(IR) = EXPSRCNA(IR)
        EXPT(IR) = EXPTYPE(IR)
        NEXP = NEXPTYPE
        WRITE(NELS,*) IR,trim(EXPSRCNA(IR)),'\',trim(EXPTYPE(IR))
      END DO
!
      NEXPTYPE = 0
      WRITE(nels,2401) ' Starting loop on NEXP =',nexp
2401  FORMAT(a,1x,i5)
2402  FORMAT(A,3I5)
    
      DO IR = 1, NEXP
        IF(KNPATH(1).GT.0) THEN
          WRITE(NELS,2402) ' CALL IDMARKER, IR, KNPATH1 =,',IR,KNPATH(1)
          CALL IDMARKER(NEXPTYPE,1,EXPS(IR),EXPNAM,EXPSRCNA,EXPTYPE)
        ENDIF
        IF(KNPATH(2).GT.0) THEN
          WRITE(NELS,2402) ' CALL IDMARKER, IR, KNPATH2 =,',IR,KNPATH(2)
          CALL IDMARKER(NEXPTYPE,2,EXPS(IR),EXPNAM,EXPSRCNA,EXPTYPE)
        ENDIF
        IF(KNPATH(3).GT.0.OR.KNPATH(4).GT.0) THEN
          WRITE(NELS,2402) ' CALL IDMARKER, IR,KNPATH3,4 =,',IR,KNPATH(3),KNPATH(4)
          CALL IDMARKER(NEXPTYPE,3,EXPS(IR),EXPNAM,EXPSRCNA,EXPTYPE)
        ENDIF
        IF(KNPATH(5).GT.0.) THEN
          WRITE(NELS,2402) ' CALL IDMARKER, IR,KNPATH(5) =,',IR,KNPATH(5)
          CALL IDMARKER(NEXPTYPE,5,EXPS(IR),EXPNAM,EXPSRCNA,EXPTYPE)
        ENDIF
      END DO
      
      WRITE(NELS,2402) ' END LOOP ON NEXP, NEXPTYPE',NEXPTYPE
      DO IR = 1, NEXPTYPE
        WRITE(NELS,2403) 'TYPES',IR,EXPSRCNA(IR),EXPTYPE(IR)
 2403   FORMAT(A,I5,1x,A,2X,A)
      END DO
!
!---- Write information to output EPF file -----------------------------------
!
!      WRITE(NELS,2403) ' NEXT CALL EPFHEAD, NEXPTYPE, ', NEXPTYPE
      CALL EPFHEAD(NEXPTYPE,EXPTYPE,EXPSRCNA,IERR)
      IF(IERR.GT.0) GO TO 999
!
! ****************************************************************************
!
!---- Loop on number of sources for this exposure location -------------------
!
! ****************************************************************************
!
      DO 35 IXL = 1,NEXPTYPE
!
!----- Find section in GID for exposure location parameter input --------------
!
         FILTER = .FALSE.
         CALL GETSET(NGID,NERR,MODNAME,NLINES,SETDATA,SITNUM,FILTER)
!
!---- Determine transport type for this exposure source ----------------------
!
        TPATH = 0
        IF(SEQI(EXPTYPE(IXL),RECGW,LEN(TRIM(RECGW)))) THEN
          TPATH = 1
          NPOINTS = 1
          CALL USEDAT(TPATH,SETDATA,NLINES)
          WRITE(NELS,106) RECGW
          DO I = 1,25
            IF(KEXPTH(I).GT.0) WRITE(NELS,105) I,EPATH(I)
          END DO
          IF(KEXPTH(17).EQ.1) WRITE(NELS,2120)
          IF(KEXPTH(25).EQ.1) WRITE(NELS,2121)
 2120     FORMAT(' Indoor VOC inhalation evaluated using MEPAS shower ', &
                 'model')
 2121     FORMAT(' Indoor VOC inhalation evaluated using EPA/Andelman ', &
                 'model')
          GO TO 25
        ELSE IF(SEQI(EXPTYPE(IXL),RECSW,LEN(TRIM(RECSW)))) THEN
          TPATH = 2
          NPOINTS = 1
          CALL USEDAT(TPATH,SETDATA,NLINES)
          WRITE(NELS,106) RECSW
 106      FORMAT(/'Summary of Pathways Allowed for ',A,/  &
               '  Path No.   Description '/                       &
               ' ----------  -----------------------------')
          DO I = 1,25
            IF(KEXPTH(I).GT.0) WRITE(NELS,105) I,EPATH(I)
          END DO
 105      FORMAT(I7,5x,A)
          IF(KEXPTH(17).EQ.1) WRITE(NELS,2120)
          IF(KEXPTH(25).EQ.1) WRITE(NELS,2121)
          GO TO 25
        ELSE IF(SEQI(EXPTYPE(IXL),RECAIR,LEN(TRIM(RECAIR)))) THEN
          TPATH = 3
          CALL USEDAT(TPATH,SETDATA,NLINES)
          WRITE(NELS,106) RECAIR
          DO I = 1,25
            IF(KEXPTH(I).GT.0) WRITE(NELS,105) I,EPATH(I)
          END DO
          GO TO 26
        ELSE IF(SEQI(EXPTYPE(IXL),RECSOIL,LEN(TRIM(RECSOIL)))) THEN
          TPATH = 5
          NPOINTS = 1
          CALL USEDAT(TPATH,SETDATA,NLINES)
          WRITE(NELS,106) RECMS
          DO I = 1,25
            IF(KEXPTH(I).GT.0) WRITE(NELS,105) I,EPATH(I)
          END DO
          GO TO 27
        ENDIF
        IF(TPATH.LE.0) THEN
           WRITE(NELS,4001) EXPTYPE(IXL)
 4001      FORMAT(' Exposure source type not recognized - skip ',A)
          GO TO  35
        ENDIF
!
!---- Find data set in WCF ---------------------------------------------------
!     Use MARKER = EXPSRCNA(IXL)
!
 25       WRITE(nels,*) ' TPATH = ',TPATH,'IXL =',IXL, 'EXPSRCNA(IXL) = ',EXPSRCNA(IXL)
    CALL MARKIN(TPATH,EXPSRCNA(IXL),FOUNDM)
        IF(.NOT.FOUNDM) THEN
          WRITE(NERR,4002) EXPSRCNA(IXL)
 4002     FORMAT(' Error in WCF file.  Water source not found:',a)
          IERR = IERR + 1
          GO TO 999
        ENDIF
!
!---- Call WCFIN to read number of media and find NAME for this run ----------
!
        CALL WCFIN(TPATH,MODNAME,NCONST,EXPSRCNA(IXL))
        NLOCS = 1
        LOCREC = 1
        CALL EPFDSET(TPATH,EXPSRCNA(IXL),NPOINTS,NCONST)
!
!----- Now have the water type (GW or SW) and usage location is identified.
!
         DO 30 IC = 1,NCONST
            READ(NWCF,*) CONNAM(1),CONID,UTIME,UCON,NTIMC,NPROG
            utime = utime
            ucon = ucon
!
!---- Test input value for number of time points (NTIMC), must be positive to allocate space
!
       IF(NTIMC.GT.2000) THEN
          IERR = 2
          MESSAG(1) = ' Error in number of time points in NWCF file, must be <= 2000, found: '
          WRITE(messag(1)(72:),*) NTIMC
          CALL PRTERR(IERR,CALLER,1)
          GO TO 90
       ENDIF
!
!---- Test input value for number of time points (NTIMC), must be positive to allocate space
!
       IF(NTIMC.LE.0) THEN
          IERR = 2
          MESSAG(1) = ' Error in number of time points in NWCF file, must be positive, found: '
          WRITE(messag(1)(72:),*) NTIMC
          CALL PRTERR(IERR,CALLER,1)
          GO TO 90
       ENDIF
!
!---- Allocate space for water concentration data
!
       IF(ALLOCATED(CWTIME)) DEALLOCATE(CWTIME)
       ALLOCATE( CWTIME(2000,20), STAT=IERA )
       IF( IERA .NE. 0 ) THEN
          IERR = 3
          MESSAG(1) = 'ALLOCATE function on the vector CWTIME failed'
          MESSAG(2) = 'System error code returned is '
          WRITE(MESSAG(2)(31:),*) IERA
          CALL PRTERR( IERR, CALLER, 2 )
          GO TO 90
       END IF
       IF(ALLOCATED(TIMES))  DEALLOCATE(TIMES)
       ALLOCATE(TIMES(2000,20), STAT=IERA )
       IF( IERA .NE. 0 ) THEN
          IERR = 3
          MESSAG(1) = 'ALLOCATE function on the vector TIMES failed'
          MESSAG(2) = 'System error code returned is '
          WRITE(MESSAG(2)(31:),*) IERA
          CALL PRTERR( IERR, CALLER, 2 )
          GO TO 90
       END IF
       NTIMX = NTIMC
!
!----- Identify current pollutant from water file
!      Call NEWPOL to determine if this pollutant has been read previously
!      and data values printed.
!
           CAS(1) = CONID(1:4)
           CAS(2) = CONID(5:8)
           CAS(3) = CONID(9:12) 
           CALL NEWPOL(CAS,NEW)
           IPDATF = 0
           IF(NEW) IPDATF = 1
           POLID(1,1) = CAS(1)
           POLID(2,1) = CAS(2)
           POLID(3,1) = CAS(3)
!
!----- Call GETSET to load GID file information from chemical database use CONI for set name.
!
      FILTER = .TRUE.         ! To include data for current site number NUMSIT  ! Activate for Chem decay
      CALL GETSET(NGID,NERR,CONI,NLINES,SETDATA,SITNUM,FILTER)                  ! Activate for Chem decay
!
!----- Read physical data from GID, contaminant data section for each chain member
!
          IPDATF = 1
          CALL CHEM_DATA_READ(SETDATA,NLINES,IPDATF,TPATH,CHEM,IERR)    ! ** Activate for Chem decay & reading from GID-CONi
!
!----- Find section in GID for exposure location parameter input --------------
!
!----- Call GETSET to load GID file information from exposure module set name.
!
      FILTER = .FALSE.         ! To include data for current site number NUMSIT  ! Activate for Chem decay
      CALL GETSET(NGID,NERR,MODNAME,NLINES,SETDATA,SITNUM,FILTER)                  ! Activate for Chem decay
!
!!!!         IF(CHEM) THEN
            DO INP = 1,NUC
!
!---- Set position of current chain member in master input list in GID file ---
!
               IP = ICDB(INP)
!
!---- RTHALF, physical loss half life in air ----------------------------------
!     CLTPHALF
               RTHALF = GETREAL(SETDATA,NLINES,'ECTHALF       ',IP,IZ,IZ,IZ,IZ,IZ)
!               RTHALF = GETREAL(SETDATA,NLINES,'CLTPHALF      ',SITNUM,IP,IZ,IZ,IZ,IZ)
!
!---- RGHALF, physical loss half life in groundwater --------------------------
!     CLGPHALF
               RGHALF = GETREAL(SETDATA,NLINES,'ECGHALF       ',IP,IZ,IZ,IZ,IZ,IZ)
!               RGHALF = GETREAL(SETDATA,NLINES,'CLGPHALF      ',SITNUM,IP,IZ,IZ,IZ,IZ)
!
!---- RWHALF, physical loss half life in surface water ------------------------
!     CLWPHALF
               RWHALF = GETREAL(SETDATA,NLINES,'ECWHALF       ',IP,IZ,IZ,IZ,IZ,IZ)
!               RWHALF = GETREAL(SETDATA,NLINES,'CLWPHALF      ',SITNUM,IP,IZ,IZ,IZ,IZ)
!
!---- RSHALF, physical loss half life in soil ---------------------------------
!     CLSPHALF
               RSHALF = GETREAL(SETDATA,NLINES,'ECSHALF       ',IP,IZ,IZ,IZ,IZ,IZ)
!               RSHALF = GETREAL(SETDATA,NLINES,'CLSPHALF      ',SITNUM,IP,IZ,IZ,IZ,IZ)
!
!----- Set environmental (or radiological) half time and decay constants
!
            WLAM(INP)=DFLAM
            GLAM(INP)=DFLAM
            SLAM(INP)=DFLAM
! -- RTHALF is constituent half-life in air
            if( rthalf .LE. 0.0 ) then
               rthalf = 3.7E+9   ! default maximum half life
               WRITE(NELS,'(A)') ' Constituent loss rate half time in air set to default maximum: 3.7E9 days'
            endif
            ALAM(INP)=AL2/RTHALF
            airhalf(iNp) = rthalf
! -- RWHALF is constituent half-life in surface water
            if( rwhalf .LE. 0.0 ) then
               rwhalf = 3.7E+9   ! default maximum half life
               WRITE(NELS,'(A)') ' Constituent loss rate half time in surface water set to default maximum: 3.7E9 days'
            endif
            WLAM(INP) = AL2/RWHALF
            swhalf(INp) = rwhalf
            THALF(INP)=RWHALF
! -- RGHALF is constituent half-life in Groundwater
            if( rghalf .LE. 0.0 ) then
               rghalf = 3.7E+9   ! default maximum half life
               WRITE(NELS,'(A)') ' Constituent loss rate half time in groundwater set to default maximum: 3.7E9 days'
            endif
            GLAM(INP)=AL2/RGHALF                     ! ground water loss rate const.
            gwhalf(iNp) = rghalf
! -- RSHALF is constituent half-life in soil
            if( rshalf .LE. 0.0 ) then
               rshalf = 3.7E+9   ! default maximum half life
               WRITE(NELS,'(A)') ' Constituent loss rate half time in soil set to default maximum: 3.7E9 days'
            endif
            soilhalf(iNp) = rshalf
            SLAM(INP)=AL2/RSHALF       ! soil loss rate const.
!
            WRITE(NELS,'(A,i3,A,3a)')  ' Physical loss data for chain member ',INp,'  ',(polid(i,inp),i=1,3)
            write(nels,2009) airhalf(iNp),ALAM(INP)
 2009       format('   Half Time, decay constant    - Air:  days       ',1p,4(1x,E10.3,1x))
            write(nels,2010) gwhalf(iNp),GLAM(INP)
 2010       format('   Half Time, decay constant     - GW:  days       ',1p,4(1x,E10.3,1x))
            write(nels,2011) swhalf(iNp),WLAM(INP)
 2011       format('   Half Time, decay constant     - SW:  days       ',1p,4(1x,E10.3,1x))
            write(nels,2012) soilhalf(iNp),SLAM(INP)
 2012       format('   Half Time, decay constant   - Soil:  days       ',1p,4(1x,E10.3,1x))
            END DO
!
!!!!         ENDIF
!
           TRITM = .FALSE.
           IF(POLID(1,1).EQ.H3NAME) TRITM = .TRUE.
           IF(POLID(1,1).EQ.THONAM) TRITM = .TRUE.
           CARBON = .FALSE.
           IF(POLID(1,1).EQ.C14NAM) CARBON = .TRUE.
!
!----- Set unit conversion factors for radionuclides, water transport
!
           XMF = 1.0
           IF (MTYPE(1).EQ.1) THEN
             XMF = XMF * 1.E3 * 0.03700   ! from pCi/ml to Bq/L
           ENDIF
!
!----- Set unit conversion factors for chemicals, water transport
!
           IF (MTYPE(1).gt.1) THEN
!
!-----  For surface water with positive transit time, do loss calculation ----
             TDK = 1.0
             XMF = XMF * 1.E6 * TDK        ! from g/ml to mg/L
           ENDIF
           IEP = 1
!          CALL GETCON(IEP,NTIMC,XMF,CWTIME,TIMES,TRELST,TEND,IERR)
           CALL GETCON(IEP,NTIMC,XMF,CWTIME,TIMES,TRELST,TEND,NTIMX,IERR)
           IF(IERR.GT.0) THEN
           WRITE(NERR,2303) EXPTYPE(IXL),RECNAM
 2303         FORMAT(' Error while reading water concentration data', &
              ' for RECTYPE = ',A20,' RECNAM = ',A20)
              GO TO 999
           ENDIF
!
!---- Read data for progeny (if any) and process as for the parent -----------
!
!          WRITE(nels,'(A,I4)') ' Reading from WCF, number of PROGENY IS: ',NPROG
            IF(NPROG.GT.0) THEN
              DO IP = 1,NPROG
                ICROSS(IP) = 0
!                READ(NWCF,*) CONNAM(IP+1),CONID,UTIME,UCON,NTIMC
                READ(NWCF,*) CONNAM(IP+1),CONID,UTIME,UCON,NTIMCP
!                WRITE(nels,'(A,I3,a,a)') ' Name of progeny ',IP,' is: ',conid
!
!----- Identify current progeny pollutant from water file
!      Call NEWPOL to determine if this pollutant has been read previously
!      and data values printed.  (Only test if IPDAT > 0).
!
                DO IE = 1,NUC
                   IF(CONID(1:4).EQ.POLID(1,IE).AND.CONID(5:8).EQ.POLID(2,IE)) THEN
                      ICROSS(IP) = IE
                   END IF
                END DO
                IF(ICROSS(IP).LE.0) THEN
                   WRITE(NERR,1234) CONID
 1234              FORMAT(' Error in progeny name in WCF, not found ', &
                          'in master list: ',A)
                   ierr = ierr + 1
                   go to 999
                ENDIF
!
                IEP = ICROSS(IP)
                CALL GETCON(IEP,NTIMCP,XMF,CWTIME,TIMES,TRELST,TEND,NTIMX,IERR)
                IF(IERR.GT.0) THEN
                 WRITE(NERR,2334) EXPTYPE(IXL),RECNAM
 2334            FORMAT(' Error while reading water concentration data', &
                  ' for RECTYPE ',A20,' RECNAM ',A12)
                 GO TO 999
                ENDIF
!
            END DO  ! End of loop on progeny for current constituent
          END IF    ! End of IF construct for progeny
!
          CALL ACCUM(SETDATA,NLINES,NYEARS,TPATH,LOCREC,NPERDS,CWTIME,  &
           TIMES,TSTART,TRELST,TEND,IERR)
  30     CONTINUE  ! End loop on constituents
         GO TO 35
!
! ****************************************************************************
!  End of waterborne transport analyses
! ****************************************************************************
!      For atmospheric pathways
! ****************************************************************************
!----- Begin loop over pollutants to be included at the site
!
!---- Find data set in ATO ---------------------------------------------------
!     Use MARKER = EXPSRCNA(IXL)
!
 26     CALL MARKIN(TPATH,EXPSRCNA(IXL),FOUNDM)
        IF(.NOT.FOUNDM) THEN
           WRITE(NERR,4003) EXPSRCNA(IXL)
 4003      FORMAT(' Error in ATO file, exposure source name not found'/'   for ',A)
           IERR = IERR + 1
           GO TO 999
        ENDIF
!
!----- Set exposure duration for output to EPF file. -------------------------
!
        DUROUT = ATED
!
!---- Call ATOIN to read heading information from current data set -----------
!
        CALL ATOIN(EXPSRCNA(IXL),NCONST,FOUNDA)
!
!---- Now have source identified and number of constituents ------------------
!
        DO IN = 1,NCONST
          INC = IN
!
!---- Read pollutant name
          READ(NATO,*) CONNAM(1),CONID,NTIMA,NPRG
!
!----- Identify current pollutant from atmospheric release rate file
!      Call NEWPOL to determine if this pollutant has been read previously
!      and data values printed.
!
           CAS(1) = CONID(1:4)
           CAS(2) = CONID(5:8)
           CAS(3) = CONID(9:12) 
           CALL NEWPOL(CAS,NEW)
           IPDATF = 0
           IF(NEW) IPDATF = 1
           POLID(1,1) = CAS(1)
           POLID(2,1) = CAS(2)
           POLID(3,1) = CAS(3)
!
!----- Read physical data from GID, contaminant data section for each chain member
!
!----- Call GETSET to load GID file information from chemical database use CONI for set name.
!
       FILTER = .TRUE.         ! To include data for current site number NUMSIT  ! Activate for Chem decay
       CALL GETSET(NGID,NERR,CONI,NLINES,SETDATA,SITNUM,FILTER)                  ! Activate for Chem decay
!
       IPDATF = 1
       CALL CHEM_DATA_READ(SETDATA,NLINES,IPDATF,TPATH,CHEM,IERR)    ! ** Activate for Chem decay & reading from GID-CONi
!
!
!----- Call GETSET to load GID file information from exposure module set name.
!
      FILTER = .FALSE.         ! To include data for current site number NUMSIT  ! Activate for Chem decay
      CALL GETSET(NGID,NERR,MODNAME,NLINES,SETDATA,SITNUM,FILTER)                  ! Activate for Chem decay
         IF(CHEM) THEN
            DO INP = 1,NUC
!
!---- Set position of current chain member in master input list in GID file ---
!
              IP = ICDB(INP)
!
!---- RTHALF, physical loss half life in air ----------------------------------
!     CLTPHALF
               RTHALF = GETREAL(SETDATA,NLINES,'ECTHALF       ',IP,IZ,IZ,IZ,IZ,IZ)
!
!---- RGHALF, physical loss half life in groundwater --------------------------
!     CLGPHALF
               RGHALF = GETREAL(SETDATA,NLINES,'ECGHALF       ',IP,IZ,IZ,IZ,IZ,IZ)
!
!---- RWHALF, physical loss half life in surface water ------------------------
!     CLWPHALF
               RWHALF = GETREAL(SETDATA,NLINES,'ECWHALF       ',IP,IZ,IZ,IZ,IZ,IZ)
!
!---- RSHALF, physical loss half life in soil ---------------------------------
!     CLSPHALF
               RSHALF = GETREAL(SETDATA,NLINES,'ECSHALF       ',IP,IZ,IZ,IZ,IZ,IZ)
!
!----- Set environmental (or radiological) half time and decay constants
!
            WLAM(INP)=DFLAM
            GLAM(INP)=DFLAM
            SLAM(INP)=DFLAM
! -- RTHALF is constituent half-life in air
            if( rthalf .LE. 0.0 ) then
               rthalf = 3.7E+9   ! default maximum half life
               WRITE(NELS,'(A)') ' Constituent loss rate half time in air set to default maximum: 3.7E9 days'
            endif
            ALAM(INP)=AL2/RTHALF
            airhalf(iNp) = rthalf
! -- RWHALF is constituent half-life in surface water
            if( rwhalf .LE. 0.0 ) then
               rwhalf = 3.7E+9   ! default maximum half life
               WRITE(NELS,'(A)') ' Constituent loss rate half time in surface water set to default maximum: 3.7E9 days'
            endif
            WLAM(INP) = AL2/RWHALF
            swhalf(INp) = rwhalf
            THALF(INP)=RWHALF
! -- RGHALF is constituent half-life in Groundwater
            if( rghalf .LE. 0.0 ) then
               rghalf = 3.7E+9   ! default maximum half life
               WRITE(NELS,'(A)') ' Constituent loss rate half time in groundwater set to default maximum: 3.7E9 days'
            endif
            GLAM(INP)=AL2/RGHALF                     ! ground water loss rate const.
            gwhalf(iNp) = rghalf
! -- RSHALF is constituent half-life in soil
            if( rshalf .LE. 0.0 ) then
               rshalf = 3.7E+9   ! default maximum half life
               WRITE(NELS,'(A)') ' Constituent loss rate half time in soil set to default maximum: 3.7E9 days'
            endif
            soilhalf(iNp) = rshalf
            SLAM(INP)=AL2/RSHALF       ! soil loss rate const.
!
            WRITE(NELS,'(A,i3,A,3a)')  ' Physical loss data for chain member ',INp,'  ',(polid(i,inp),i=1,3)
            write(nels,2009) airhalf(iNp)
            write(nels,2010) gwhalf(iNp)
            write(nels,2011) swhalf(iNp)
            write(nels,2012) soilhalf(iNp)
            END DO
         ENDIF
!
!----- Check for special radionuclides tritium and carbon 14
!
           TRITM = .FALSE.
           IF(POLID(1,1).EQ.H3NAME) TRITM = .TRUE.
           IF(POLID(1,1).EQ.THONAM) TRITM = .TRUE.
           CARBON = .FALSE.
           IF(POLID(1,1).EQ.C14NAM) CARBON = .TRUE.
!
!----- Set unit conversion factors for radionuclides, atmospheric input
!      No conversion necessary because input is in Bq
            XMF = 1.0
!
!----- Set unit conversion factors for chemicals, atmospheric input
!
            IF (MTYPE(1).gt.1) THEN
              XMF = 1.E6     ! convert from kg to mg
            ENDIF
!
!----  Open temporary file for atmospheric data ------------------------------
!
       OPEN (UNIT=NATM, FILE=TRIM(HINFNM)//'.ATM',STATUS='UNKNOWN')
!
!---- Call ATODAT to read and evaluate air concentration for all chain members -
!
            RNAMES(1) = CONID
            CALL ATODAT(RNAMES,NPRG,NPOINTS,NTIMA,INTERP,IERR)
            IF(IERR.GT.0) THEN
            WRITE(NERR,2333) CONID
 2333         FORMAT(' Error while reading air release rate data',  &
              ' for pollutant ',A12)
              GO TO 999
           ENDIF
!
!---- Rewind temporary file containing air and deposition values for ---------
!     the current constituent and chain members
!
           REWIND(NATM)
!
!---- Open temporary file for exposure concentration results -----------------
!
      OPEN (UNIT=NETM, FILE=TRIM(HINFNM)//'.ETM',STATUS='UNKNOWN')
!
!----- Call GETSET to load GID file information from exposure module set name.
!
      FILTER = .FALSE.         ! To include data for current site number NUMSIT  ! Activate for Chem decay
      CALL GETSET(NGID,NERR,MODNAME,NLINES,SETDATA,SITNUM,FILTER)                  ! Activate for Chem decay
!
!---- Get leach rate constants for this pollutant ----------------------------
!
      CALL GETLEACH(SETDATA,NLINES)
          IF(ONEPOINT) THEN
            NX2 = 1 + (NXY - 1) / NAX1
            NX1 = NXY - (NX2 - 1) * NAX1
            IF(INTERP) THEN
               CALL ATMRED(NX1,NX2,CATIME,XMF)
            ELSE
! Added tests on inclusion of AIR and Deposition in analysis
               IF(DOAIR) THEN
                  DO ICM = 1,NUC
                     CATIME(1,ICM,1) = AIRCIN(ICM,NX1,NX2)
                  END DO
               ELSE
                  DO ICM = 1,NUC
                     CATIME(1,ICM,1) = 0.0
                  END DO
               ENDIF
               IF(DODEP) THEN
                  DO ICM = 1,NUC
                     CATIME(1,ICM,2) = DEPTOT(ICM,NX1,NX2)
                  END DO
               ELSE
                  DO ICM = 1,NUC
                     CATIME(1,ICM,2) = 0.0
                  END DO
               ENDIF
! end of added testing
            ENDIF
            ix1 = nx1
            ix2 = nx2
!!!!            WRITE(NELS,*) ' CALLING AIRREG'
            CALL AIRREG(SETDATA,NLINES,NYEARS,NPERDS,CATIME,TSTART,  &
                              INTERP,IERR)
!!!!              TRELST,TEND,INTERP,IERR)
!!!            WRITE(NELS,*) ' RETURN FROM AIRREG'
            REWIND(NATM)
          ELSE
!
!---- Loop on axis 1 points --------------------------------------------------
!
           DO IX1 = 1,NAX1
!
!---- Loop on axis 2 points --------------------------------------------------
!
             DO IX2 = 1,NAX2
!
!---- Call ATMRED to extract concentration/time data for current point (x,y)
!     and all chain members
!
               IF(INTERP) THEN
                 CALL ATMRED(IX1,IX2,CATIME,XMF)
               ELSE
! Added tests on inclusion of AIR and Deposition in analysis
                 IF(DOAIR) THEN
                   DO ICM = 1,NUC
                     CATIME(1,ICM,1) = AIRCIN(ICM,IX1,IX2)
                   END do
                 ELSE
                   DO ICM = 1,NUC
                     CATIME(1,ICM,1) = 0.0
                   END DO
                 ENDIF
                 IF(DODEP) THEN
                   DO ICM = 1,NUC
                     CATIME(1,ICM,2) = DEPTOT(ICM,IX1,IX2)
                   END DO
                 ELSE
                   DO ICM = 1,NUC
                     CATIME(1,ICM,2) = 0.0
                  END DO
                 ENDIF
               ENDIF
! End of added test
               CALL AIRREG(SETDATA,NLINES,NYEARS,NPERDS,CATIME,TSTART,  &
                               INTERP,IERR)
               REWIND(NATM)
             END DO   ! Loop on axis 2, IX2
           END DO     ! Loop on axis 1, IX1
           ENDIF  ! If ONEPOINT
           REWIND(NETM)
!
!---- Call ETMRED to reread and process results and write to EPF file ---------
!
          CALL ETMRED(NPERDS,INC,RECNAM,NCONST)
          REWIND(NETM)
        END DO
        CLOSE(NATM)
        CLOSE(NETM)
        GO TO 35
!
! ****************************************************************************
!      END IF  ! End of atmospheric section
! ****************************************************************************
!  Pathway for measured soil contamination exposure pathways
! ****************************************************************************
!
 27   CALL MARKIN(TPATH,EXPSRCNA(IXL),FOUNDM)
      IF(.NOT.FOUNDM) THEN
        WRITE(NERR,4005) IXL,EXPSRCNA(IXL)
 4005   FORMAT(' Error reading SCF for source ',I3/   &
               ' looking for source name: ',A)
      ENDIF
!
! *************************************************************************
!  Get soil model selection flag SOILMOD, True for MEPAS model, False for
!      use of time varying soil concentration data in SCF file
! *************************************************************************
!

      SOILMOD = .TRUE.
      SOILMOD = GETLOG(SETDATA,NLINES,'SOILMOD       ',IZ,IZ,IZ,IZ,IZ,IZ)
      IF(SOILMOD) THEN
          WRITE(NELS,'(A)') ' MEPAS soil model will be used to evaluate time varying soil concentrations.'
      ELSE
          WRITE(NELS,'(A)') ' Time varying soil concentration data from SCF file will be used.'
      ENDIF
!
! *************************************************************************
!  Restart Point for Next Receptor Location for measured soil
! *************************************************************************
!
      CALL SCFIN(TPATH,MODNAME,NCONST,IERR,EXPSRCNA(IXL))
      IF(IERR.GT.0) GO TO 999
      CALL EPFDSET(TPATH,EXPSRCNA(IXL),NPOINTS,NCONST)
! ****************************************************************************
!  Read next pollutant from soil file, NSCF
! ****************************************************************************
          DO 50 IC = 1,NCONST
            READ(NSCF,*) CONNAM(1),CONID,UTIME,UCON,NTIMC,NPROG
!
!---- Test input value for number of time points (NTIMC), must be positive to allocate space
!
            IF(NTIMC.LE.0) THEN
               IERR = 1
               MESSAG(1) = ' Error in number of time points in NSCF file, must be positive, found: '
               WRITE(messag(1)(72:),*) NTIMC
               WRITE(nels,*) ' Error in number of time points in NSCF file, must be positive, found: '
               WRITE(nels,*) ntimc
               CALL PRTERR(IERR,CALLER,1)
               GO TO 90
            ENDIF
!
!---- Test input value for number of time points (NTIMC), must be positive to allocate space
!
            IF(NTIMC.GT.2000) THEN
               IERR = 1
               MESSAG(1) = ' Error in number of time points in NSCF file, must be <= 2000, found: '
               WRITE(messag(1)(72:),*) NTIMC
               WRITE(nels,*) ' Error in number of time points in NSCF file, must be <= 2000, found: '
               WRITE(nels,*) ntimc
               CALL PRTERR(IERR,CALLER,1)
               GO TO 90
            ENDIF
!
!---- Allocate space for SOIL concentration data
!
            IF(ALLOCATED(CWTIME)) DEALLOCATE(CWTIME)
            ALLOCATE( CWTIME(2000,20), STAT=IERA )
                IF( IERA .NE. 0 ) THEN
                  IERR = 3
                  MESSAG(1) = 'ALLOCATE function on the vector CWTIME failed'
                  MESSAG(2) = 'System error code returned is '
                  WRITE(MESSAG(2)(31:),*) IERA
                  WRITE(nels,*) 'ALLOCATE function on the vector CWTIME failed'
                  WRITE(nels,*) 'System error code returned is '
                  CALL PRTERR( IERR, CALLER, 2 )
                  GO TO 90
                END IF
            IF(ALLOCATED(TIMES))  DEALLOCATE(TIMES)
            ALLOCATE(TIMES(2000,20), STAT=IERA )
                IF( IERA .NE. 0 ) THEN
                  IERR = 3
                  MESSAG(1) = 'ALLOCATE function on the vector TIMES failed'
                  MESSAG(2) = 'System error code returned is '
                  WRITE(MESSAG(2)(31:),*) IERA
                  WRITE(nels,*) 'ALLOCATE function on the vector TIMES failed'
                  WRITE(nels,*) 'System error code returned is '
                  CALL PRTERR( IERR, CALLER, 2 )
                  GO TO 90
                END IF
            NTIMX = NTIMC
!
! ****************************************************************************
!   Identify current pollutant from soil file
!   Call NEWPOL to determine if this pollutant has been read previously
!   and data values printed.
! ****************************************************************************
           CAS(1) = CONID(1:4)
           CAS(2) = CONID(5:8)
           CAS(3) = CONID(9:12) 
           CALL NEWPOL(CAS,NEW)
           IPDATF = 0
           IF(NEW) IPDATF = 1
           POLID(1,1) = CAS(1)
           POLID(2,1) = CAS(2)
           POLID(3,1) = CAS(3)
!
!           CALL PDATIN(IPDATF,Path,IERR)
!           WRITE(NELS,*) ' IERR FLAG at return from PDATIN', IERR
!           IF(IERR.GT.0) GO TO 999
!
!
!----- Call GETSET to load GID file information from chemical database use CONI for set name.
!
      FILTER = .TRUE.         ! To include data for current site number NUMSIT  ! Activate for Chem decay
      CALL GETSET(NGID,NERR,CONI,NLINES,SETDATA,SITNUM,FILTER)                  ! Activate for Chem decay
!
!----- Read physical data from GID, contaminant data section for each chain member
!
          IPDATF = 1
          CALL CHEM_DATA_READ(SETDATA,NLINES,IPDATF,TPATH,CHEM,IERR)    ! ** Activate for Chem decay & reading from GID-CONi
!
!          CALL PDATIN(IPDATF,Path,IERR)         ! ** deactivate when reading constituent data from CONi
!
!----- Find section in GID for exposure location parameter input --------------
!
!----- Call GETSET to load GID file information from exposure module set name.
!
      FILTER = .FALSE.         ! To include data for current site number NUMSIT  ! Activate for Chem decay
      CALL GETSET(NGID,NERR,MODNAME,NLINES,SETDATA,SITNUM,FILTER)                  ! Activate for Chem decay
!
         IF(CHEM) THEN  !XX
            DO INP = 1,NUC
!
!---- Set position of current chain member in master input list in GID file ---
!
          IP = ICDB(INP)
!
!---- RTHALF, physical loss half life in air ----------------------------------
!     CLTPHALF
               RTHALF = GETREAL(SETDATA,NLINES,'ECTHALF       ',IP,IZ,IZ,IZ,IZ,IZ)
!
!---- RGHALF, physical loss half life in groundwater --------------------------
!     CLGPHALF
               RGHALF = GETREAL(SETDATA,NLINES,'ECGHALF       ',IP,IZ,IZ,IZ,IZ,IZ)
!
!---- RWHALF, physical loss half life in surface water ------------------------
!     CLWPHALF
               RWHALF = GETREAL(SETDATA,NLINES,'ECWHALF       ',IP,IZ,IZ,IZ,IZ,IZ)
!
!---- RSHALF, physical loss half life in soil ---------------------------------
!     CLSPHALF
               RSHALF = GETREAL(SETDATA,NLINES,'ECSHALF       ',IP,IZ,IZ,IZ,IZ,IZ)
!
!----- Set environmental (or radiological) half time and decay constants
!
            WLAM(INP)=DFLAM
            GLAM(INP)=DFLAM
            SLAM(INP)=DFLAM
! -- RTHALF is constituent half-life in air
            if( rthalf .LE. 0.0 ) THEN   ! default maximum half life
               rthalf = 3.7E+9   ! default maximum half life
               WRITE(NELS,'(A)') ' Constituent loss rate half time in air set to default maximum: 3.7E9 days'
            ENDIF
            ALAM(INP)=AL2/RTHALF
            airhalf(iNp) = rthalf
! -- RWHALF is constituent half-life in surface water
            if( rwhalf .LE. 0.0 ) THEN  ! default maximum half life
               rwhalf = 3.7E+9   ! default maximum half life
               WRITE(NELS,'(A)') ' Constituent loss rate half time in surface water set to default maximum: 3.7E9 days'
            endif
            WLAM(INP) = AL2/RWHALF
            swhalf(INp) = rwhalf
            THALF(INP)=RWHALF
! -- RGHALF is constituent half-life in Groundwater
            if( rghalf .LE. 0.0 ) THEN
               rghalf = 3.7E+9   ! default maximum half life
               WRITE(NELS,'(A)') ' Constituent loss rate half time in groundwater set to default maximum: 3.7E9 days'
            endif
            GLAM(INP)=AL2/RGHALF                     ! ground water loss rate const.
            gwhalf(iNp) = rghalf
! -- RSHALF is constituent half-life in soil
            if( rshalf .LE. 0.0 ) THEN
               rshalf = 3.7E+9   ! default maximum half life
               WRITE(NELS,'(A)') ' Constituent loss rate half time in soil set to default maximum: 3.7E9 days'
            endif
            soilhalf(iNp) = rshalf
            SLAM(INP)=AL2/RSHALF       ! soil loss rate const.
!
            WRITE(NELS,'(A,i3,A,3a)')  ' Physical loss data for chain member ',INp,'  ',(polid(i,inp),i=1,3)
            write(nels,2009) airhalf(iNp)
            write(nels,2010) gwhalf(iNp)
            write(nels,2011) swhalf(iNp)
            write(nels,2012) soilhalf(iNp)
            END DO
         ENDIF
!
           TRITM = .FALSE.
           IF(POLID(1,1).EQ.H3NAME) TRITM = .TRUE.
           IF(POLID(1,1).EQ.THONAM) TRITM = .TRUE.
           CARBON = .FALSE.
           IF(POLID(1,1).EQ.C14NAM) CARBON = .TRUE.
!
!---- Initialize soil concentration array for current decay chain
!
          DO ICC = 1,20
             CSOIL(ICC) = 0.0
          END DO
!
!---- Set source unit conversion factor, XMF ---------------------------------
!
           IF(MTYPE(1).GT.1) THEN
             XMF = 1.0              ! For chemicals, mg to mg
!             XMF = 1.E3            ! For chemicals, g to mg
           ELSE
             XMF = 0.037           ! Rads, pCi to Bq
           ENDIF
           IEP = 1
!
! For time varying soil concentration input, call SCFTIME to save all data
!
           IF(SOILMOD) THEN
              CALL SCFDAT(IEP,NTIMC,XMF,CSOIL,IERR)              ! Original option, MEPAS soil model
           ELSE
              CALL SCFTIME(IEP,NTIMC,XMF,CWTIME,TIMES,TRELST,TEND,NTIMX,IERR) ! New option, use SCF data
           ENDIF
           IF(IERR.GT.0) THEN
              WRITE(NERR,5236)
 5236         FORMAT(' Error reading SCF file for parent')
              GO TO 999
           ENDIF
!
!---- Read data for progeny (if any) and process as for the parent -----------
!
           IF(NPROG.GT.0) THEN
             DO IP = 1,NPROG
               ICROSS(IP) = 0
               READ(NSCF,*) CONNAM(ip+1),CONID,UTIME,UCON,NTIMCP
!
!----- Identify current progeny pollutant from water file
!
               DO IE = 1,NUC
                 IF(CONID(1:4).EQ.POLID(1,IE).AND.CONID(5:8).EQ.POLID(2,IE)) THEN
                   ICROSS(IP) = IE
                 ENDIF
               END DO
               IF(ICROSS(IP).LE.0) THEN
                 WRITE(NERR,5234) CONID
 5234            FORMAT(' Error in progeny name in SCF file,'   &
                        /' not in master list, found: ',A)
                 IERR = IERR + 1
                 GO TO 999
               ENDIF
               IEP = ICROSS(IP)
           IF(SOILMOD) THEN
              CALL SCFDAT(IEP,NTIMCP,XMF,CSOIL,IERR)                     ! Original option, MEPAS soil model
           ELSE
              CALL SCFTIME(IEP,NTIMCP,XMF,CWTIME,TIMES,TRELST,TEND,NTIMX,IERR) ! New option, use SCF data
           ENDIF
!
               IF(IERR.GT.0) THEN
                 WRITE(NERR,5235) IP
 5235            FORMAT(' Error reading SCF file for progeny',I3)
                 GO TO 999
               ENDIF
              END DO  ! End of loop on progeny for current constituent
            END IF    ! End of IF construct for progeny
!
! For original MEPAS soil model, call SOILAC to do analysis
!
            IF(SOILMOD) THEN                         ! Original option, MEPAS soil model
               CALL SOILAC(SETDATA,NLINES,NYEARS,LOCREC,NPERDS,CSOIL,TSTART,IERR)
!
! For use of time varying soil concentration input, call SOILTIME to do analysis
!
            ELSE                                     ! New option, use SCF data vs time
               CALL SOILTIME(SETDATA,NLINES,NYEARS,LOCREC,NPERDS,CWTIME,TIMES,TSTART,TRELST,TEND,IERR)
!
            ENDIF
            IF(IERR.GT.0) GO TO 999
  50     CONTINUE  ! End loop on constituents
      REWIND(NSCF)
      GO TO 35
! ****************************************************************************
!  End of measured soil contamination analysis
! ****************************************************************************
!
 35   END DO  ! Loop on exposure location sources: IXL
!
!----- Close files, end of analysis ------------------------------------------
!
      CLOSE (NATO)
      ENDFILE (NELS)
      CLOSE (NELS)
      ENDFILE (NERR)
      LENERR = 0
      INQUIRE (UNIT=NERR,IOSTAT  = LENERR)
!
      IF(LENERR.GT.0) THEN
        CLOSE (NERR)
        CLOSE (NATM)
        CLOSE (NETM)
      ELSE
        CLOSE (NERR,STATUS="DELETE")
        CLOSE (NATM,STATUS="DELETE")
        CLOSE (NETM,STATUS="DELETE")
      ENDIF
      CLOSE (NWRN)
!
   85 CONTINUE
!
      CLOSE (NWCF)
!
      GOTO 999
!
!
!----- Print error messages for input end-of-file errors
!
   97 WRITE(NELS,1003) NWCF
      WRITE(*,1003) NWCF
      GO TO 999
!
!----- Format Statements ------------------------------------------------
!
  100 FORMAT(7I3,I5)
  102 FORMAT(I3,I6,5I3,I6)
  104 FORMAT(6I3,i6)
  101 FORMAT(1X,5I3)
  103 FORMAT(25I2)
  200 FORMAT(3A4,E10.3,I3,I3)
  300 FORMAT(E10.2,I5,2E10.2)
  310 FORMAT(2X,3I5,E10.3)
  320 FORMAT(2X,2I5,2X,10X,2A4,2X,4E10.3)
  400 FORMAT(2X,I5,37X,E10.1)
  500 FORMAT(A80)
 1000 FORMAT(' ERROR IN RECORD TYPE 3, PATHWAY ',I2,' HAS BAD',      &
       ' VALUE FOR NUMBER OF USAGE LOCATIONS.  VALUE READ =',I3)
 1002 FORMAT(' END-OF-FILE ENCOUNTERED ON UNIT',I3,' WHILE READING ' &
      /' AIR AND SOIL CONCENTRATION DATA - STOP')
 1003 FORMAT(' END-OF-FILE ENCOUNTERED ON UNIT',I3,' WHILE READING ' &
      /' WATER CONCENTRATION DATA - STOP')
 1004 FORMAT(' END-OF-FILE ENCOUNTERED ON UNIT',I3,' WHILE READING ' &
      /' RECORD TYPE 3 - STOP')
 1005 FORMAT(' END-OF-FILE ENCOUNDTERD ON UNIT',I3,' WHILE READING ' &
      /' RECORD TYPES 1 AND 2 - STOP')
 1006     FORMAT(' ERROR IN VALUE GIVEN FOR NUMBER OF TIME PERIODS,',&
                 ' NPER =',I4)
 1007 FORMAT(' ERROR READING WATER FILE FOR POLLUTANT ',I5,2X,2A4)
 1008 FORMAT(' .WIN WASTE UNIT NUMBER DOES NOT MATCH ORDER, EXPECTED',&
       I3,', BUT FOUND',I3)
 1009 FORMAT(' END-OF-FILE ENCOUNTERED ON UNIT',I3,' WHILE READING '  &
      /' AIR RELEASE RATE DATA - STOP')
!
! *************************************************************************C
!
  90  STOP
!
  999 CONTINUE
      CLOSE (NATO)
!      ENDFILE (NELS)  ! comment out for digital Fortran
      CLOSE (NELS)
!      ENDFILE (NERR)   ! comment out for digital Fortran
      LENERR = 0
      INQUIRE (UNIT=NERR,IOSTAT  = LENERR)
!
      IF(LENERR.GT.0) THEN
        CLOSE (NERR)
        CLOSE (NATM)
        CLOSE (NETM)
      ELSE
        CLOSE (NERR,STATUS="DELETE")
        CLOSE (NATM,STATUS="DELETE")
        CLOSE (NETM,STATUS="DELETE")
      ENDIF
      CLOSE (NWRN)
!
      END
!=================== END OF MODULE AHAZX ====================================
!   MEPAS AHAZ: RMCHAIN.FOR             Version Date: 24-Apr-97
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ***************************************************************************C
!
!  Subroutine RMCHAIN reads radionuclide decay chain data and stores values  *
!                    common block DECAY  arrays.                             *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    03-Nov-95                                               *
!  Last Modified:    24-Apr-97      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZ
!     Called by: PDATIN
!     Calls: None
!     Common blocks referenced: DECAY, DEVICE
!
!==== Significant Parameter Designation and Description ======================
!
!  Parameters in argument list
!  Name        Type             Purpose
! ---------   ------    ---------------------------------------------------
!   LURMD      Int*2     Logical unit for input of radionuclide chain decay
!                        data.  Either DOSCAL.IN or master RMDLIB.DAT.
!---------------------------------------------------------------------------
!
!  Major Local Parameters:
!     Name          Type                  Purpose
!   ------------   ------  ---------------------------------------------------
!   NUC            Int.     Number of chain members
!   DIN(2)        Char*4    Name of radionuclide for which data is sought.
! --------------------------------------------------------------------------
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!  
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE RMCHAIN(DIN,Path,IERR)
      CHARACTER*64 Path
!
!==== COMMON Block Definitions ===============================================
!
      INCLUDE 'DECAY.FTN'
      INCLUDE 'DEVICE.FTN'
      INCLUDE 'PSET1.FTN'
      INCLUDE 'TITLS.FTN'
!
!==== DIMENSION Statements ===================================================
!
      DOUBLE PRECISION :: AL2
      CHARACTER*4 DIN(3), DN(3), BLANK
      CHARACTER*3 CEND
      INTEGER IE, NIN, IERR
      LOGICAL SAVE
      DATA CEND/'   '/
      DATA LURMD/25/
      DATA BLANK/'    '/
      DATA DN(3)/'    '/
!
!==== Variable Declarations ==================================================
!
!     None
!
!==== DATA Statements ========================================================
!
!     None
! ****************************************************************************
!
!  OPEN RADIONUCLIDE DATA INPUT FILE ON UNIT LURMD
!
       OPEN (LURMD,FILE=Path,FORM='FORMATTED',STATUS='OLD') 
!
      AL2 = DLOG(2.D0)
        AL(1) = 0.0
      DO IE=2,20
        AL(IE) = 0.0
        POLID(1,IE) = BLANK
        POLID(2,IE) = BLANK
        POLID(3,IE) = BLANK
        IFRM(1,IE)= 0
        DK(1,IE)  = 0.0
        IFRM(2,IE)= 0
        DK(2,IE)  = 0.0
      END DO
      SAVE = .FALSE.
!
!  Read title lines from RMDLIB.DAT
!
      READ(LURMD,1000) CTITLE
      READ(LURMD,1020) 
 1000 FORMAT(A80)
 1020 FORMAT(9X,2I3)
!
!  Read first data line for next parent entry
!
  5     READ(LURMD,1002,END=999) (DN(J),J=1,2), NOCHM, NIMPIN
 1002   FORMAT(2A4,1X,2I3)
!
!   Test for end of RMDLIB.DAT file
!
      IF(NOCHM.GT.20) GO TO 55
!
!  Read second data line for current parent, decay data.
!
        READ(LURMD,1003) (DN(J),J=1,2), HALF
 1003   FORMAT(19X,2A4,E10.2) 
!
!  Is current radionuclide the desired radionuclide?
!
        IF(DN(1).EQ.DIN(1).AND.DN(2).EQ.DIN(2)) THEN
          SAVE = .TRUE.           ! Found
          POLID(1,1) = DN(1)
          POLID(2,1) = DN(2)
          POLID(3,1) = DN(3)
          NUC = NOCHM
        ELSE
          SAVE = .FALSE.          ! Not found
        ENDIF
!
!  If half life is positive, then calculate radiological decay constant
!  and desired radionuclide is found.
!
        IF(SAVE) THEN
          IF(HALF.GT.0.) THEN
            AL(1) = AL2/HALF
          ELSE
            WRITE(neRR,3000) HALF
 3000       FORMAT(' ERROR IN RMDLIB, NON-POSITIVE HALF LIFE FOR ', &
            'PARENT RADIONUCIDE',1PE12.4)
            IERR = IERR + 1
          ENDIF
        ENDIF
!
         IF(NIMPIN.GT.0) THEN      !  Implicit daughters
            DO 10 ID = 1, NIMPIN
!
!              Skip record for each implicit daughter
!
              READ(LURMD,1004) 
 1004         FORMAT(19X,2A4,14X,F7.5) 
!
  10        CONTINUE
!
         END IF
!
         IF(NOCHM.GT.1) THEN       !  Explicit daughters
            DO IE = 2, NOCHM
!
!               Read record for explicit daughter
!
              IF(SAVE) THEN      ! Read and save data
                READ(LURMD,1005) (POLID(J,IE),J=1,2), HALF,   &
                  IFRM(1,IE), DK(1,IE), IFRM(2,IE), DK(2,IE)
 1005             FORMAT(19X,2A4,E10.3,2X,I2,F7.5,I2,F7.5) 
                IF(HALF.GT.0.) THEN
                  AL(IE) = AL2/HALF
                ELSE
                  WRITE(nels,3000) HALF
                ENDIF
              ELSE               ! Skip data
                READ(LURMD,1005)  
              ENDIF
!
            END DO
!
         END IF
        IF(SAVE) GO TO 55
      GO TO 5
!
 999  WRITE(nels,3001) DIN
      WRITE(nerr,3001) DIN
 3001 FORMAT(' Radionuclide not found in RMDLIB decay data file ',3A4)
      IERR = IERR + 1
  55  IF(.NOT.SAVE) THEN
         WRITE(nerr,3001) DIN
         IERR = IERR + 1
      ENDIF
      CLOSE(LURMD)
      RETURN
      END        
!-------------  End file RMCHAIN.FOR ---------------------------------------
!   MEPAS AHAZ: NEWPOL.FOR             Version Date: 07-Nov-95
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE NEWPOL                                *
!                                                                            *
!  Subroutine NEWPOL test to see if current pollutant has been evaluated     *
!                    previously in the same run.                             *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    07/11/95 (Converted from HAZ2 version)                  *
!  Last Modified:    07-Nov-95      DLS                                       *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZ
!     Called by: SUBROUTINE AHAZ
!     Calls: NONE
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     CAS(3)     U     char*4  Argument  Name of pollutant to be tested
!     NEW        S     LOGIC.  Argument  Logical flag, TRUE if new pollutant
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE NEWPOL(CAS,NEW)
!
!==== COMMON Block Definitions ===============================================
!
!==== DIMENSION Statements ===================================================
!
      CHARACTER*4 CAS(3),OLDCAS(3,200)
!
!==== Variable Declarations ==================================================
!
      LOGICAL NEW
      INTEGER NPOL
!
!==== DATA Statements ========================================================
!
      DATA NPOL/0/
!
      NEW = .TRUE.
      IF(NPOL.LE.0) THEN
        OLDCAS(1,1) = CAS(1)
        OLDCAS(2,1) = CAS(2)
        OLDCAS(3,1) = CAS(3)
        NPOL = 1
      ELSE
        DO 100 IP = 1,NPOL
          IF(CAS(1).NE.OLDCAS(1,IP)) GO TO 100
          IF(CAS(2).NE.OLDCAS(2,IP)) GO TO 100
          IF(CAS(3).NE.OLDCAS(3,IP)) GO TO 100
          NEW = .FALSE.
          GO TO 200
 100    CONTINUE
        NPOL = NPOL + 1
        OLDCAS(1,NPOL) = CAS(1)
        OLDCAS(2,NPOL) = CAS(2)
        OLDCAS(3,NPOL) = CAS(3)
      ENDIF
 200  RETURN
!================ END OF MODULE NEWPOL =====================================
      END
!   MEPAS AHAZ: GETCON.FOR             Version Date:  21-Nov-95
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE GETCON                                *
!                                                                            *
!  This subroutine reads one set of water concentration vs. time data from   *
!                 the general water PDCF in free field format.               *
!                 The number of time periods is input.                       *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    21-Nov-1995 (New for 100 year update)                   *
!  Last Modified:    21 Nov 1995  DLS                                        *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZ
!     Called by: AHAZ
!     Calls: NONE
!     Common blocks referenced: DEVICE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!      IE         U    INT     Argument  Chain member index, 1=parent
!      NTIMC      U    INT     Argument  Number of data lines to read
!      XMF        U    Real    Argument  Units conversion factor
!      CWTIME     S    Real    Argument  Array of water concentration values
!      TIMES      S    Real    Argument  Array of time points for water data
!      TRELST     S    Real    Argument  Start of water concentration times
!      TEND       S    Real    Argument  End of water concentration times
!      IERR       S    Int     Argument  Error flag for file read errors
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!  
!
!==== SUBROUTINE CALL ========================================================
!
!      SUBROUTINE GETCON(IE,NTIMC,XMF,CWTIME,TIMES,TRELST,TEND,IERR)
      SUBROUTINE GETCON(IE,NTIMC,XMF,CWTIME,TIMES,TRELST,TEND,NTIMX,IERR)
!
!==== COMMON Block Definitions ===============================================
!
      INCLUDE 'DEVICE.FTN'
!
!==== DIMENSION Statements ===================================================
!
      DOUBLE PRECISION cwdp
      REAL(KIND=4), DIMENSION(2000,20) :: CWTIME, TIMES
      REAL(KIND=4), DIMENSION(*) :: TRELST,TEND
!
!==== Variable Declarations ==================================================
!
      INTEGER NTIMC,IERR,NTIMX
!      REAL TRELST,TEND
!
!==== DATA Statements ========================================================
!
!==== INITIALIZATIONS ========================================================
!
      IERR = 0
!      DO ITIM = 1,100
      DO ITIM = 1,NTIMX
         TIMES(ITIM,IE)  = 0.0
         CWTIME(ITIM,IE) = 0.0
      END DO
!
!==== START OF ANALYSIS ======================================================
!
!  Test value of NTIMC for range
!
      IF(NTIMC.LE.0.OR.NTIMC.GT.NTIMX+1000) THEN
        IERR = IERR + 1
        WRITE(NERR,200) NTIMC,NTIMX
 200    FORMAT(' NTIMC out of range in GETCON =',I6,' NTIMX (Max) = ',I6)
        RETURN
      ENDIF
      DO ITIM = 1,NTIMC
        READ(NWCF,*,END=998,ERR=999) TIMES(ITIM,IE), CWDP
        IF(CWDP.LT.1.E-25) THEN
          CWTIME(ITIM,IE) = 0.0
        ELSE
          CWTIME(ITIM,IE) = CWDP
        ENDIF
        CWTIME(ITIM,IE) = CWTIME(ITIM,IE) * XMF
        WRITE(nels,*) ' ITIM, IE, TIMES(ITIM,IE), cwtime(ITIME,ie), xmf',  ITIM,ie,TIMES(ITIM,IE),cwtime(itim,ie),xmf
      END DO
      TRELST(IE) = TIMES(1,IE)
      TEND(IE) = TIMES(NTIMC,IE)
!
!==== N0RMAL RETURN ==========================================================
!
      RETURN
!
!==== PROCESS READ ERROR CONDITIONS ==========================================
!
998   WRITE(NERR,100) ITIM
 100  FORMAT(' End of file on water concentration input in GETCON',  &
            /' for time period ',I6)
      IERR = IERR + 1
      RETURN
999   WRITE(NERR,300) ITIM
 300  FORMAT(' Error in reading water concentration file in GETCON', &
            /' during time period ',I6)
      IERR = IERR + 1
      RETURN
      END
!=================== END OF MODULE GETCON ===================================
!   MEPAS AHAZ: SCFTIME.FOR             Version Date:  07-May-2002
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE SCFTIME                               *
!                                                                            *
!  This subroutine reads one set of soil concentration vs. time data from    *
!                 the general soil PDCF in free field format.                *
!                 The number of time periods is input.                       *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    07-May-2002 (New for soil time-varying input use        *
!  Last Modified:    07-May-2002  DLS                                        *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZ
!     Called by: AHAZX
!     Calls: NONE
!     Common blocks referenced: DEVICE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!      IE         U    INT     Argument  Chain member index, 1=parent
!      NTIMC      U    INT     Argument  Number of data lines to read
!      XMF        U    Real    Argument  Units conversion factor
!      CWTIME     S    Real    Argument  Array of soil concentration values
!      TIMES      S    Real    Argument  Array of time points for water data
!      TRELST     S    Real    Argument  Start of soil concentration times
!      TEND       S    Real    Argument  End of soil concentration times
!      IERR       S    Int     Argument  Error flag for file read errors
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!   
!                       
!==== SUBROUTINE CALL ========================================================
!
!     SUBROUTINE SCFTIME(IE,NTIMC,XMF,CWTIME,TIMES,TRELST,TEND,IERR)
      SUBROUTINE SCFTIME(IE,NTIMC,XMF,CWTIME,TIMES,TRELST,TEND,NTIMX,IERR)
!
!==== COMMON Block Definitions ===============================================
!
      INCLUDE 'DEVICE.FTN'
!
!==== DIMENSION Statements ===================================================
!

      REAL(KIND=4), DIMENSION (2000,20) :: CWTIME,TIMES
      REAL(KIND=4), DIMENSION (*) :: TRELST,TEND
      REAL(KIND=8) :: cwdp
!
!==== Variable Declarations ==================================================
!
      INTEGER :: NTIMC,IERR,NTIMX,IE
      REAL :: XMF
!
!==== DATA Statements ========================================================
!
!==== INITIALIZATIONS ========================================================
!
!      WRITE(nels,*) ' In SCFTIME for ntimc,ntimx',ntimc,ntimx
      IERR = 0
      DO ITIM = 1,NTIMC
         TIMES(ITIM,IE)  = 0.0
         CWTIME(ITIM,IE) = 0.0
      END DO
!
!==== START OF ANALYSIS ======================================================
!
!  Test value of NTIMC for range
!
      IF(NTIMC.LE.0.OR.NTIMC.GT.NTIMX+1000) THEN
       IERR = IERR + 1
        WRITE(NERR,200) NTIMC,NTIMX
 200    FORMAT(' NTIMC out of range in SCFTIME =',I6,/' NTIMX (Max) = ',I6)
        RETURN
      ENDIF
      DO ITIM = 1,NTIMC
        READ(NSCF,*,END=998,ERR=999) TIMES(ITIM,IE), CWDP
        IF(CWDP.LT.1.E-25) THEN
          CWTIME(ITIM,IE) = 0.0
        ELSE
          CWTIME(ITIM,IE) = CWDP
        ENDIF
        CWTIME(ITIM,IE) = CWTIME(ITIM,IE) * XMF
      END DO
      TRELST(IE) = TIMES(1,IE)
      TEND(IE) = TIMES(NTIMC,IE)
!
!==== N0RMAL RETURN ==========================================================
!
      RETURN
!
!==== PROCESS READ ERROR CONDITIONS ==========================================
!
998   WRITE(NERR,100) ITIM
 100  FORMAT(' End of file on soil concentration input in SCFTIME',  &
            /' for time period ',I6)
      IERR = IERR + 1
      RETURN
999   WRITE(NERR,300) ITIM
 300  FORMAT(' Error in reading soil concentration file in SCFTIME', &
            /' during time period ',I6)
      IERR = IERR + 1
      RETURN
      END
!=================== END OF MODULE SCFTIME ===================================
!   MEPAS AHAZ: ACCUM.FOR             Version Date:  10-Sep-97
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE ACCUM                                 *
!                                                                            *
!  This subroutine controls analysis as a function of time for one           *
!  pollutant at a time.  The input is the concentration time data.           *
!  Output is generated by calls to other subroutines.                        *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    21-Nov-1995 (New for 100 year update)                   *
!  Last Modified:    10-Sep-1997  DLS                                        *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZ
!     Called by: AHAZ
!     Calls: AVGCON
!     Common blocks referenced: DEVICE, PFLAGS
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!      NYEARS     U    INT     Argument  Number of years in each time period
!      IPATH      U    INT     Argument  Transport pathway index (1-GW etc)
!      LOCREC     U    INT     Argument  Usage location index
!
!      NPERDS     U    INT     Argument  Number of time periods to evaluate
!      CWTIME     S    Real    Argument  Array of water concentration values
!      TIMES      S    Real    Argument  Array of time points for water data
!      TSTART     S    Real    Argument  Start of water concentration times
!  TRELST(20)     U    Real    Argument  Start of current period
!  TEND(20)       S    Real    Argument  End of water concentration times
!      IERR       S    Int     Argument  Error flag for file read errors
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!  
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE ACCUM(SETDATA,NLINES,NYEARS,IPATH,LOCREC,NPERDS, &
          CWTIME,TIMES,TSTART,TRELST,TEND,IERR)
!
!==== COMMON Block Definitions ===============================================
!
        INCLUDE 'PARMTR.PAR'
        INCLUDE 'GWPATH.FTN'
        INCLUDE 'SWPATH.FTN'
        INCLUDE 'DEVICE.FTN'
        INCLUDE 'DECAY.FTN'
        INCLUDE 'PSET1.FTN'
        INCLUDE 'PSET3.FTN'
        INCLUDE 'CRLOC.FTN'
        INCLUDE 'LEACH.FTN'
        INCLUDE 'ETIMES.FTN'
!
!==== DIMENSION Statements ===================================================
!
      DIMENSION CWTIME(2000,20),TIMES(2000,20)
!
!==== Variable Declarations ==================================================
!
      INTEGER IERR
      REAL TRELST(20),TEND(20)
      DOUBLE PRECISION, DIMENSION (20) :: WLM, CW, CSOIL
      DOUBLE PRECISION, DIMENSION (20) :: DPRATE, CA
      DOUBLE PRECISION, DIMENSION (20) :: CSD, CEND, ALOSS
      CHARACTER*(*) SETDATA(LINEMAX)
      CHARACTER*8 NAMOUT
!
!==== DATA Statements ========================================================
!
      DATA C365/365.25/
      DATA C30/30.0/
!
!==== INITIALIZATIONS ========================================================
!
      IT = IPATH
      IERR = 0
      TIM = TSTART
      DELT = FLOAT(NYEARS)
      TDAYS = DELT*C365
      IF(IPATH.EQ.1) THEN
        TEXD = DGWED*C365
        DELTEX = DGWED
        NYTEX = INT(DGWED)
      ENDIF
      IF(IPATH.EQ.2) THEN
        TEXD = DSWED*C365
        DELTEX = DSWED
        NYTEX = INT(DSWED)
      ENDIF
      DO IN = 2,20
        DPRATE(IN) = 0.D0
      END DO
!
!==== START OF ANALYSIS ======================================================
!
!---- Get leach rate constants for this pollutant ----------------------------
!
      CALL GETLEACH(SETDATA,NLINES)
!
!---- Write first line for current parent into EPF file ----------------------
!
      NAMOUT(1:4) = POLID(1,1)
      NAMOUT(5:8) = POLID(2,1)
      NC = LEN_TRIM(NAMOUT)
      NP = LEN_TRIM(CONNAM(1))
      WRITE(NEPF,1000) CONNAM(1)(1:NP),NAMOUT(1:NC),NUC-1,NPERDS
 1000 FORMAT('"',A,'","',A,'",',I2,',',I4,',')
!
!---- Loop over time periods for this data set -------------------------------
!
      DO IT = 1,10
         DO IN = 1,NUC
!         WRITE(NELS,*) ' IT, IN, TIMES(IT,IN),CWTIME(IT,IN) ',IT,IN,TIMES(IT,IN),CWTIME(IT,IN)
         END DO
      END DO
!!!      WRITE(nels,*) ' Start time period loop for NPERDS',i7
      DO ITIM = 1,NPERDS
!
!---- For first time period, set concentrations to zero ----------------------
!
        IF(ITIM.EQ.1) THEN
          DO IN = 1,20
            CSOIL(IN) = 0.D0
            CEND(IN)  = 0.D0
            CW(IN)    = 0.D0
          END DO
          DO IN = 1,NUC
            ALOSS(IN) = ALEACH(IN)
            IF(MTYPE(IN).NE.1) ALOSS(IN) = ALEACH(IN) + DBLE(SLAM(IN))
          END DO
        END IF
!
!---- Set time parameters for writing to EPF file ----------------------------
!
        DURTIM = FLOAT(NYEARS)
        CTIME = TSTART + (ITIM - 1)*DURTIM
!
!----  Get average concentrations for next time period -----------------------
!
        DO IN = 1,NUC
!          WRITE(nels,*) ' IN, CWTIME(ITIM,in) IN ', ITIM, IN, CWTIME(ITIM,IN)
!          IEP = ICROSS(IN)
          CW(IN) = AVGCON(CTIME,NYTEX,TRELST(IN),TEND(IN),CWTIME(1,IN), &
                        TIMES(1,IN),IERR)
          IF(IERR.GT.0) GO TO 999
!          WRITE(nels,*) ' ITIM, IN, CW(in) OUT', ITIM, IN, CW(IN)
        END DO
!
!---- Call EXPOS to evaluate health impacts for current time period ----------
!
           IPOL = 1
           CALL EXPOS(IPATH,LOCREC,IPOL,ITIM,CW,CA,CSOIL,DELTEX,IERR)
           IF(IERR.GT.0) GO TO 998
!
!---- Calculate soil concentration at start of next period from
!     deposition in current period.
!
          DO IN = 1,NUC
            CW(IN)    = 0.D0
          END DO
!
!----  Get average concentrations for next time period -----------------------
!
          DO IN = 1,NUC
             CW(IN) = AVGCON(CTIME,NYEARS,TRELST(IN),TEND(IN),  &
                      CWTIME(1,IN),TIMES(1,IN),IERR)
             IF(IERR.GT.0) GO TO 999
          END DO
!
!---- First set contribution from amount present at end of previous period ---
!
          TIM = TIM + DELT
          IF(ITIM.GT.1) THEN
            INTGRL = 0
            CALL CHAIN(TDAYS,ALOSS,CEND,CEND,INTGRL,CHEM)
          END IF
!
!---- Next add amount deposited during the time increment from irrigation --------------
!
          DO IN = 1,NUC
            DPRATE(IN) = CW(IN) * CIRR(IPATH) * FIRR(IPATH)/C30
          END DO
          INTGRL = 1
          CALL CHAIN(TDAYS,ALOSS,DPRATE,CSD,INTGRL,CHEM) 
          DO IN = 1,NUC
            CEND(IN) = CEND(IN)+CSD(IN)
          END DO
!
!---- Next calculate average soil concentration over the next period
!
          CALL CHAIN(TEXD,ALOSS,CEND,CSOIL,INTGRL,CHEM)
          DO IN = 1,NUC
            CSOIL(IN) = CSOIL(IN)/TEXD
          END DO
      END DO  ! End of loop on time periods
!
!==== N0RMAL RETURN ==========================================================
!
      RETURN
!
!==== PROCESS READ ERROR CONDITIONS ==========================================
!
998   WRITE(NERR,400) ITIM
 400  FORMAT(' Error in EXPOS call in ACCUM during time period ',I6)
      IERR = IERR + 1
      RETURN
999   WRITE(NERR,300) ITIM
 300  FORMAT(' Error in retrieving water concentration from AVGCON ',  &
             'in ACCUM'/' during time period ',I6)
      IERR = IERR + 1
      RETURN
      END
!=================== END OF MODULE ACCUM ====================================
!
!   MEPAS AHAZ: SOILTIME.FOR          Version Date:  07-May-2002
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE SOILTIME                              *
!                                                                            *
!  This subroutine controls analysis as a function of time for one           *
!  pollutant at a time.  The input is the concentration time data.           *
!  Output is generated by calls to other subroutines.                        *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    07-May-2002 (New for use of soil vs time data           *
!  Last Modified:    07-May-2002  DLS                                        *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZ
!     Called by: AHAZ
!     Calls: AVGCON
!     Common blocks referenced: DEVICE, PFLAGS
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!      NYEARS     U    INT     Argument  Number of years in each time period
!      IPATH      U    INT     Argument  Transport pathway index (5-SOIL)
!      LOCREC     U    INT     Argument  Usage location index
!
!      NPERDS     U    INT     Argument  Number of time periods to evaluate
!      CWTIME     S    Real    Argument  Array of soil concentration values
!      TIMES      S    Real    Argument  Array of time points for soil data
!      TSTART     S    Real    Argument  Start of soil concentration times
!  TRELST(20)     U    Real    Argument  Start of current period
!  TEND(20)       S    Real    Argument  End of soil concentration times
!      IERR       S    Int     Argument  Error flag for file read errors
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
! 
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE SOILTIME(SETDATA,NLINES,NYEARS,LOCREC,NPERDS, &
          CWTIME,TIMES,TSTART,TRELST,TEND,IERR)
!           CALL SOILTIME(SETDATA,NLINES,NYEARS,LOCREC,NPERDS, &
!         CWTIME,TIMES,TSTART,TRELST,TEND,IERR)!
!==== COMMON Block Definitions ===============================================
!
        INCLUDE 'PARMTR.PAR'
        INCLUDE 'CRLOC.FTN'
        INCLUDE 'DEVICE.FTN'
        INCLUDE 'DECAY.FTN'
        INCLUDE 'PSET1.FTN'
        INCLUDE 'PSET3.FTN'
        INCLUDE 'SLPATH.FTN'
        INCLUDE 'ETIMES.FTN'
!
!==== DIMENSION Statements ===================================================
!
      DIMENSION CWTIME(2000,20),TIMES(2000,20)
!
!==== Variable Declarations ==================================================
!
      INTEGER IERR
      REAL TRELST(20),TEND(20)
      REAL(KIND=8), DIMENSION (20) :: WLM, CS, CSOIL
      REAL(KIND=8), DIMENSION (20) :: DPRATE, CA
      REAL(KIND=8), DIMENSION (20) :: CSD, CEND, ALOSS
      CHARACTER*(*) SETDATA(LINEMAX)
      CHARACTER*8 NAMOUT
!
!==== DATA Statements ========================================================
!
      DATA C365/365.25/
      DATA C30/30.0/
!
!==== INITIALIZATIONS ========================================================
!
      IT = 5
      IERR = 0
      TIM = TSTART
      DELT = FLOAT(NYEARS)
      TDAYS = DELT*C365
      TEXD = SMED*C365
      DELTEX = SMED
      NYTEX = INT(SMED)
      DO IN = 2,20
        DPRATE(IN) = 0.D0
      END DO
!
!==== START OF ANALYSIS ======================================================
!
!---- Write first line for current parent into EPF file ----------------------
!
      NAMOUT(1:4) = POLID(1,1)
      NAMOUT(5:8) = POLID(2,1)
      NC = LEN_TRIM(NAMOUT)
      NP = LEN_TRIM(CONNAM(1))
      WRITE(NEPF,1000) CONNAM(1)(1:NP),NAMOUT(1:NC),NUC-1,NPERDS
 1000 FORMAT('"',A,'","',A,'",',I2,',',I4,',')
!
!---- Loop over time periods for this data set -------------------------------
!
      DO ITIM = 1,NPERDS
!
!---- Set time parameters for writing to EPF file ----------------------------
!
        DURTIM = FLOAT(NYEARS)
        CTIME = TSTART + (ITIM - 1)*DURTIM
!
!----  Get average concentrations for next time period -----------------------
!
        DO IN = 1,NUC
          CS(IN) = AVGCON(CTIME,NYTEX,TRELST(IN),TEND(IN),CWTIME(1,IN), &
                        TIMES(1,IN),IERR)
          IF(IERR.GT.0) GO TO 999
        END DO
!
!---- Call EXPOS to evaluate health impacts for current time period ----------
!
           IPOL = 1
           CALL EXPOS(IT,LOCREC,IPOL,ITIM,CA,CA,CS,DELTEX,IERR)
           IF(IERR.GT.0) GO TO 998
!
      END DO  ! End of loop on time periods
!
!==== N0RMAL RETURN ==========================================================
!
      RETURN
!
!==== PROCESS READ ERROR CONDITIONS ==========================================
!
998   WRITE(NERR,400) ITIM
 400  FORMAT(' Error in EXPOS call in SOILTIME during time period ',I6)
      IERR = IERR + 1
      RETURN
999   WRITE(NERR,300) ITIM
 300  FORMAT(' Error in retrieving soil concentration from AVGCON ',  &
             'in SOILTIME'/' during time period ',I6)
      IERR = IERR + 1
      RETURN
      END
!=================== END OF MODULE SOILTIME ====================================
!
!
!   MEPAS HAZ2: ATINPA.FOR             Version Date: 25-Jun-1997
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE ATINPA                                *
!                                                                            *
!  Subroutine ATINPA This module reads records to define the usage           *
!             practices at each atmospheric agricultural usage location      *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    02/28/86 (Converted to PC)                              *
!  Last Modified:    25-Jun-1997   DLS                                       *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: SUBROUTINE USEDAT
!     Calls: NONE
!     Common blocks referenced: AIRDAT, ATPATH, AGCLAS, DEVICE, LOCNAM, PSET1, PROD
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter      Set/
!     Name           Used  Type  Location  Parameter Description
!     -------------- ----  ----  --------  ------------------------------
!     variable        S    CHAR  Argument  nnnnnnnnnnnnnnnnnnnnnn
!                     U    INT   Internal
!                     S/U  REAL  Common
!                          DBLE  External
!
!==== Modification History ===================================================
!
!   Date      Who  Modification Description
!  ---------  ---  -----------------------------------------------------------
!  
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE ATINPA(SETDATA,NLINES) 
!
!==== COMMON Block Definitions ===============================================
!
      INCLUDE 'AGCLAS.FTN'
      include 'ATPATH.FTN'
      INCLUDE 'DEVICE.FTN'
      INCLUDE 'PARMTR.PAR'
!
!==== DIMENSION Statements ===================================================
!
      CHARACTER*(*) SETDATA(LINEMAX)
!
!==== Variable Declarations ==================================================
!
      INTEGER NLINES, GETINT
!
!==== DATA Statements ========================================================
!
      DATA IZ/0/
!
!----- Start calculations.  Initialize error index. -------------------------
!
      IERR=0
!
!---- Initialize parameters -------------------------------------------------
!
!----- Read a record for agricultural input flag, IAG -----------------------
!
      IAG = GETINT(SETDATA,NLINES,'KAAG          ',IZ,IZ,IZ,IZ,IZ,IZ)
      ED  = GETREAL(SETDATA,NLINES,'ATED          ',IZ,IZ,IZ,IZ,IZ,IZ)
!
!---- process data for agricultural locations
!
      KAAG = 0
!
!----- Test value given for agricultural index ------------------------------
!
      IF(IAG.LT.0.OR.IAG.GT.NAGC) THEN
!!!          WRITE(nels,3000) IAG,NAGC
          IERR=IERR+1
          GO TO 11
      ENDIF
      KAAG = IAG
!
!----- Read data for agricultural usage locations
!
      WRITE(nels,2000) char(12)
 2000 FORMAT(1a,18x,'Summary of Atmospheric Usage Location Data'/)
      IF(ED.LE.0.) ED = 70.
      ATED = ED
!
!----- writing out the usage location parameter chart(s)
!
          write(nels,2101) ATED
 2101     format('Exposure Duration: y',1p,1x,E10.3)
!
 11   RETURN
!
!----- Write error message for end-of-file on input record read -------------
!
!----- Format Statements ----------------------------------------------------
!
 3000 FORMAT('ERROR IN AGRICULTURAL CLASS INDEX IN ATINPT,'/,   &
          ' VALUE GIVEN =',I4,', THE MAXIMUM ALLOWED IS ',I3)
!
!============== END OF MODULE ATINP2 ========================================
!
      END 
!
!     MEPAS RADCON: AVGCON.FOR            Version Date: 20-Nov-1995
!     Copyright 1989 by Battelle Memorial Institute. All rights reserved.
! ****************************************************************************
!                                                                            *
!                         FUNCTION AVGCON                                    *
!                                                                            *
!  FUNCTION AVGCON finds the average concentration between times T0 and      *
!  T1 by integrating between these two times and dividing by the input.      *
!  time period, ntimes.                                                      *
!  The two concentration values C0 and C1 are obtained through linear        *
!  interpolation of the source (PNT(J)) and time (TIM(J)) table.             *
!  This function is called from the subroutine HAZ.                          *
!                                                                            *
!  Written by:       James Stroh                                             *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    08/30/93                                                *
!  Last Modified:    20-Nov-1995 DLS (from FUNCTION AVG70Y)                  *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: HAZ
!     Called by: SUBROUTINE HAZ
!     Calls to: NONE
!     Common blocks referenced: INTER, CTIME, INDEX3
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/           Location
!     Name      Used   Type    Export/Import  Parameter Description
!     --------- -----  ------  -------------- --------------------------------
!     T0        USED   REAL    ARG - IMPORT   Start time for the integral
!
!     T1        USED   REAL    LOCAL          End time for the integral
!
! (   M1        USED   INT     ARG - IMPORT   Parent number) Use Deleted
!
!     NYR       USED   INT     ARG            Number of years in averaging
!                                             period (years).  20-Nov-1995
!
!    PNT        USED   REAL    ARG            Concentration array
!
!    TIME       USED   REAL    ARG            Time points corresponding to
!                                             concentration values (years)
!
!     CO        USED   REAL    LOCAL          Concentration corresponding to
!                                             T0
!     C1        USED   REAL    LOCAL          Concentration corresponding to
!                                             T1
!     TEND1     USED   REAL    COMMON - IMP   END OF concentration data
!
!     TSTRT1    USED   REAL    COMMON - IMP   START OF concentration data
!
!     JSTART    USED   INT     LOCAL          Starting node for conc. and time
!                                             pair.
!     JEND      USED   INT     LOCAL          Ending node for conc. and time
!                                             pair.
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     11/02/93     JPM  Corrected "subscript out of range" error that occurred
!                       when TEND1 and TSTRT1 were an even multiple of 70.
!
!     20-Nov-95    DLS  Modified program to use variable time period, instead
!                       of the fixed 70 year time period.
!
!==== FUNCTION CALL ========================================================
!
      FUNCTION AVGCON (T0,NYR,TSTRT1,TEND1,PNT,TIME,IERRF)
!
!----COMMON Block Definitions ------------------------------------------------
!    None
!
!==== DIMENSION Statements ===================================================
!
      DIMENSION PNT(2000),TIME(2000)
!
!==== Variable Declarations ==================================================
!
      REAL T1,C0, C1, Area
      INTEGER J, JSTART, JEND, NYR
!
!==== DATA Statements ========================================================
!
!     None
!
! If the starting time is out of range, return a zero concentration
!
      IERRF = 0
      AVGCON = 0.0
      ZFAC = 1.0
      IF (                    T0 .GE. TEND1 ) THEN
      AVGCON = 0.0 
      RETURN
      END if
! *************************************************************
      IF(NYR.LE.0) GO TO 999
      TYR = FLOAT(NYR)
      T1 = T0 + TYR
      IF(T1.LE.TSTRT1) THEN
         RETURN
      ENDIF
!
! If the end point of the integral is greater than TEND1, reset the end point
! to equal TEND1 (minus some small delta so that the interp routine is ok).
! Also reset the time period.
!
      TYRN = TYR
      IF ( T1 .GE. TEND1 )  THEN
        T1 = TEND1 * 0.9999999
        TYRN = TEND1 - T0
      ENDIF
!
! Find the data points which surround the start time
!
      J = 2
      DO WHILE ( TIME(J) .LE. T0 )
          J = J+1
      END DO
!  If start time is less than 1st data time point, then reset start time
!  to first point time
      IF(T0.LT.TSTRT1) THEN
!         ZFAC  = (TYR - (TSTRT1 - T0))/TYR
         C0 = PNT(1)
         JSTART = 1
         J = 1
      ELSE
! Interpolate to find the starting concentration for the integration
         C0 = ( (pnt(j) - pnt(j-1))/(time(j) - time(j-1)) )  &
              * (T0 - time(j-1)) + pnt(j-1)
         JSTART = J
      ENDIF
!
! Find the data points which surround the end time
! (Let J start from where JSTART left off)
!
      DO WHILE ( TIME(J) .LE. T1 )
          J = J + 1
      END DO
!
! Interpolate to find the ending concentration for the integration
      C1 = ( (pnt(j) - pnt(j-1))/(time(j) - time(j-1)) )   &
          * (T1 - time(j-1)) + pnt(j-1)
      JEND = J
!
!  Find the area under the curve for the 70 year interval.
!       Area of a trapazoid:  A = 1/2 * dt * ( F(0) + F(1) )
!
      j = JSTART
      if( JSTART .LT. JEND ) then
          Area = 0.5 * ( time(j) - T0 ) * ( PNT(j) + C0 )
          j = j+1
          Do while ( j .lt. JEND )
              Area = Area + 0.5 * ( time(j) - time(j-1) )    &
                                * ( PNT(j)  + PNT(j-1) )
              j = j+1
          end do
          Area = Area + 0.5*( T1 - time(j-1)) * ( C1 + PNT(j-1) )
      else
          Area = TYRN/2. * (C0 + C1)
      end if
!
! Divide the area by the time interval to get the average concentration
!
      AVGCON = Area * ZFAC / TYR
      RETURN
 999  CONTINUE
      IERRF = 1
      RETURN
      END
!
!   MEPAS AHAZ: GETLEACH.FOR           Version Date: 18-Aug-97
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE GETLEACH                              *
!                                                                            *
!  Subroutine GETLEACH extracts data from the .PRM file and calculates       *
!                    leach rate constants for agricultural soil.             *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    28-Nov-95                                               *
!  Last Modified:    18-Aug-97      DLS                                       *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZX
!     Called by: SUBROUTINE ACCUM
!     Calls: SUBROUTINE GET.. ROUTINES FOR READING .PRM FILES
!     Common blocks referenced: DECAY,LEACH,PSET1,DEVICE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     POLID      U     CHAR    PSET1     Pollutant names
!   ALEACH(20)  S/U    REAL*8  LEACH     Leach rate constant (1/days)
!   SOILKD(20)  S/U    REAL*4  LEACH     Kd for agricultural soil (ml/g)
!   THICK       S/U    REAL*4  LEACH     Soil thickness (cm)
!   MOISTC      S/U    REAL*4  LEACH     Soil moisture content (fraction)
!   BULKD       S/U    REAL*4  LEACH     Soil bulk density (g/cm3)
!   VLEACH      S/U    REAL*4  LEACH     total infiltration rate (cm/yr)
!==== Modification History ===================================================
!
!    Date     Who  Modification Description
!  ---------  ---  ------------------------------------------------------
!  
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE GETLEACH(SETDATA,NLINES)
!
!==== COMMON Block Definitions ===============================================
!
      INCLUDE 'DECAY.FTN'
      INCLUDE 'PSET1.FTN'
      INCLUDE 'LEACH.FTN'
      INCLUDE 'DEVICE.FTN'
      INCLUDE 'COUPLE.FTN'
      INCLUDE 'PARMTR.PAR'
!
!==== DIMENSION Statements ===================================================
!
!
!==== Variable Declarations ==================================================
!
      CHARACTER*(*) SETDATA(LINEMAX)
      CHARACTER*14 KDNAME,BLKDNAM,MOISTNAM,THICKNAM,VINNAM
      CHARACTER*12 NAM,CASNAM
      CHARACTER*80 GETSTR
      INTEGER GETINT,LECHOPT
      LOGICAL FOUND
!
!==== DATA Statements ========================================================
!
      DATA   KDNAME/'SOILKD        '/
      DATA  BLKDNAM/'BULKD         '/
      DATA MOISTNAM/'MOIST         '/
      DATA THICKNAM/'THICK         '/
      DATA   VINNAM/'LEACV         '/
      DATA IZ/0/
      DATA C365/365.25/
!
!---- Get option flag for evaluation of leach rate constant ------------------
!
      LECHOPT = GETINT(SETDATA,NLINES,'LEACHOPTION   ',IZ,IZ,IZ,IZ,IZ,IZ)
      IF(LECHOPT.EQ.1) THEN  ! User input of Kd and soil properties to calculate leach rates
!
!---- Get parameter for agricultural soil ------------------------------------
!
      VAL = GETREAL(SETDATA,NLINES,BLKDNAM,IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.) BULKD = VAL
      VAL = GETREAL(SETDATA,NLINES,MOISTNAM,IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.) MOISTC = VAL
      VAL = GETREAL(SETDATA,NLINES,THICKNAM,IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.) THICK = VAL
      VAL = GETREAL(SETDATA,NLINES,VINNAM,IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GE.0.) VLEACH = VAL                           ! Allow zero value
!
      ENDIF
!
!---- Get Kd or leach rate constant value for each chain member. -------------
!     For leachoption = 0, read leach rate constant,
!     for leachoption = 1, read Kd and calculate leach rate constants.
!---- Get number of pollutants in .PRM file ----------------------------------
!
      NPOLLS = GETINT(SETDATA,NLINES,'NUMCON        ',IZ,IZ,IZ,IZ,IZ,IZ)
      IF(lechopt.eq.0) then
        WRITE(nels,3003) (polid(i,1),i=1,2)
 3003   FORMAT(/' Leach rate information for pollutant: ',2a4)
      else
        write(nels,3000) (polid(i,1),i=1,2),vleach,thick,moistc*100.,bulkd
 3000   format(/' The following data was used to calculate leaching rate',  &
         ' constants for pollutant: ',2a4/                                 &
         ' Total infiltration rate (cm/yr): ',F10.1/                       &
         ' Agricultural soil thickness (cm):',F10.1/                       &
         ' Soil moisture content (percent): ',F10.1/                       &
         ' Soil bulk density (g/cm3):       ',F10.2)
      endif
      DO IN = 1,NUC
         FOUND = .FALSE.
         SOILKD(IN) = 0.0
         ALEACH(IN) = 0.0
         CASNAM(1:4)  = POLID(1,IN)
         CASNAM(5:8)  = POLID(2,IN)
         CASNAM(9:12) = POLID(3,IN)
!
! ** Updated 15 November 2004, DL Strenge
!    Revised to use decay chain members extracted from CONi section of GID, with branching
!    NUC = number of chain members
!    Position in CONi section is ICDB(i)
!
         IP = ICDB(IN)
!
!----- Match name from GID to name of current pollutant.  When found, read
!      leach rate OR Kd for the pollutant
!
         IF(LECHOPT.EQ.0) THEN  ! Input leach rate constants
            VAL=GETREAL(SETDATA,NLINES,'SOILLR        ',IP,IZ,IZ,IZ,IZ,IZ)
            IF(VAL.GE.0.) ALEACH(IN) = VAL / C365
         ELSE     ! User input Kd, calculate leach rate constants
            VAL=GETREAL(SETDATA,NLINES,KDNAME,IP,IZ,IZ,IZ,IZ,IZ)
            IF(VAL.GE.0.) SOILKD(IN) = VAL
            ALEACH(IN) = VLEACH/(THICK*MOISTC*(1.+( BULKD*SOILKD(IN)/MOISTC)))
            ALEACH(IN) = ALEACH(IN) / C365
         ENDIF
!              FOUND = .TRUE.
!            ENDIF
!
! ** Updated 15 November 2004, DL Strenge
!    Revised to use decay chain members extracted from CONi section of GID, with branching
!    NUC = number of chain members
!    Position in CONi section is ICDB(i)
!
!            NPR = GETINT(SETDATA,NLINES,'NDS           ',IP,IZ,IZ,IZ,IZ,IZ)
!           IF(NPR.GT.0) THEN
!            DO IN = 1,NUC
!               IPR = ICDB(IN)
!               DO IPR = 1,NPR
!                 NAM = GETSTR(SETDATA,NLINES,'CASID         ',IP,IPR,IZ,IZ,IZ,IZ)
!                 IF(NAM.EQ.CASNAM) THEN
!               IF(LECHOPT.EQ.0) THEN  ! Input leach rate constants
!                  VAL=GETREAL(SETDATA,NLINES,'SOILLR        ',IP,IPR,IZ,IZ,IZ,IZ)
!                  IF(VAL.GE.0.) ALEACH(IN) = VAL / C365
!               ELSE     ! User input Kd, calculate leach rate constants
!                  VAL=GETREAL(SETDATA,NLINES,KDNAME,IP,IPR,IZ,IZ,IZ,IZ)
!                  IF(VAL.GE.0.) SOILKD(IN) = VAL
!                  ALEACH(IN) = VLEACH/(THICK*MOISTC*(1.+(BULKD*SOILKD(IN)/MOISTC)))
!                  ALEACH(IN) = ALEACH(IN) / C365
!               ENDIF
!                  FOUND = .TRUE.
!               ENDIF
!
!              ENDDO
!            END DO
!           ENDIF
!          ENDIF
!  10     CONTINUE
         IF(lechopt.eq.0) then
            write(nels,3002) casnam,aleach(in)
 3002       FORMAT(1x,a12,' user supplied leach constant (d-1):',1pe10.3)
         else
            write(nels,3001) casnam,aleach(in),soilkd(in)
 3001       format(1x,a12,' leach constant (d-1):',1pe10.3,'.  Soil Kd =',e10.3)
         endif
!
      END DO
      RETURN
!
!================ END OF MODULE GETLEACH ===================================
!
      END
!   GETINT.FOR                       Version Date: 29-Oct-96
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE GETINT                                *
!                                                                            *
!  Subroutine GETINT extracts an integer value from the .GID file            *
!                                                                            *
!  Written by:       Karl Castleton                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    16-Nov-93                                               *
!  Last Modified:    29-Oct-96      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ
!     Called by: SUBROUTINE GETINTAK
!     Calls: MOVETO
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     SETDATA    U      CHAR   Argument  Array of line images for extraction
!                                        of an integer parameter
!     ***THE ARRAY MUST ALREADY BE FILED***
!     NAME       U      CHAR   Argument  Name of parameter sought
!     C1         U      INT    Argument  CONSTITUENT COUNTER
!     C2         U      INT    Argument  MEDIA COUNTER FOR GROUND WATER PSZ AND SZ
!     C3         U      INT    Argument  LOCATION COUNTER
!     C4         U      INT    Argument  FLUX COUNTER
!     C5         U      INT    Argument  MONTH COUNTER
!     C6         U      INT    Argument  MISCELANEOUS COUNTER
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!    
!==== SUBROUTINE CALL ========================================================
!
      INTEGER FUNCTION GETINT(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,C6)
!
!---- Variable Type Declarations ---------------------------------------------
!
      include 'parmtr.par'
      INTEGER C1,C2,C3,C4,C5,C6,NLINES
      CHARACTER*(*) NAME
      CHARACTER*(*) SETDATA(linemax)
      LOGICAL MOVETO
      INTEGER T1,T2,T3,T4,T5,T6,REF
      CHARACTER UNITS*8,USUNTS*8,TNAME*14
      INTEGER VALUE
!
!---- Start of Analysis
!    MOVETO finds the record having the desired parameter value
!
      IF (MOVETO(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,C6,LINE)) THEN
        READ(SETDATA(LINE),*) TNAME,T1,T2,T3,T4,T5,T6,REF,USUNTS,UNITS,VALUE
!
        GETINT=VALUE
      ELSE
        GETINT=0
      END IF
      END
!
!   MEPAS HAZ2: SEQ.FOR                Version Date: 06-Dec-95
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE SEQ                                   *
!                                                                            *
!  Subroutine SEQ is used by functions to extract values from the .PRM file  *
!                    values if a value was found in the .PRM file.           *
! THIS CHECKS TO SEE IF THE NAMES OF THE PARAMETERS MATCH. RETURNS
! A .TRUE. IF THEY DO MATCH .FALSE. IF THEY DON'T. IT COMPARES ONLY
! THE FIRST 14 CHARACTERS AND STOPS AT THE FIRST SPACE IT FINDS
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    29-Nov-95                                               *
!  Last Modified:    04-Dec-95      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ
!     Called by: SUBROUTINE HAZ
!     Calls: SUBROUTINE GET.. ROUTINES FOR READING .PRM FILES
!     Common blocks referenced: PTHUSE,DERMDAT,DRPATH,DEVICE,COUPLE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!    NAME1       U      STR    Argument  Name of parameter from .PRM file
!    NAME2       U      STR    Argument  Name of parameter searching for
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
! 
!==== SUBROUTINE CALL ========================================================
!
      LOGICAL FUNCTION SEQ(NAME1,NAME2)
!
!---- Variable type declarations ---------------------------------------------
!
      CHARACTER*14 NAME1,NAME2
      LOGICAL TEMP
      INTEGER POS
!
      TEMP=.TRUE.
      POS=1
 320  IF (NAME1(POS:POS) .EQ. NAME2(POS:POS)) THEN
      POS=POS+1
      ELSE
      TEMP=.FALSE.
      END IF
      IF (POS .LE. 14 .AND. TEMP .AND. NAME1(POS:POS) .GT. ' '  &
      .AND. NAME2(POS:POS) .GT. ' ') THEN
      GOTO 320
      END IF
      SEQ=TEMP
      END
!   MEPAS AHAZX: GETSTART.FOR           Version Date: 08-Jul-97
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE GETSTART                              *
!                                                                            *
!  Subroutine GETSTART extracts dates from the .GID file and calculates      *
!                    the start time for the risk calculation                 *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    29-Nov-95                                               *
!  Last Modified:    08-Jul-97      DLS                                       *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZ
!     Called by: SUBROUTINE AHAZX
!     Calls: SUBROUTINE GET.. ROUTINES FOR READING .PRM FILES
!     Common blocks referenced: DECAY,LEACH,PSET1,DEVICE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!   SETDATA()   U     char*80  Argument  GID data set for extraction of info.
!   NLINES      U      INT     Argument  Number of lines of info in the GID
!                                        data set
!    TSTART     S      REAL*4  Argument  Start time for risk calculation
!                                        relative to start of release
!==== Modification History ===================================================
!
!    Date      Who  Modification Description
!  ---------   ---  ------------------------------------------------------
! 
!              
! 
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE GETSTART(SETDATA,NLINES,TSTART)
!
!==== COMMON Block Definitions ===============================================
!
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'DEVICE.FTN'
      INCLUDE 'COUPLE.FTN'
!
!==== DIMENSION Statements ===================================================
!
!
!==== Variable Declarations ==================================================
!
      REAL VAL
      CHARACTER*(*) SETDATA(LINEMAX)
!
!==== DATA Statements ========================================================
!
      DATA IZ/0/
!
!---- Get date of start of risk from parameter file
!
      VAL = GETREAL(SETDATA,NLINES,'TEXPOS        ',IZ,IZ,IZ,IZ,IZ,IZ)
      IF(VAL.LT.0.) THEN 
        WRITE(nels,200)
 200    FORMAT(' The risk start time was not found in PRM file.  ', &
               'The start time is set to 0.')
        TSTART = 0.0
        RETURN
      ELSE
        TSTART = VAL
      ENDIF
      RETURN
!================ END OF MODULE GETSTART ===================================
      END
!   MEPAS HAZ2: ATINPR.FOR             Version Date: 25-Jun-1997
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE ATINPR                                *
!                                                                            *
!  Subroutine ATINPR This module reads records to define the usage           *
!             practices at each usage location for airborne regional         *
!             exposure pathways (individual and population.                  *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    18-Dec-95 (from ATINP2)                                 *
!  Last Modified:    25-Jun-1997  DLS                                        *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: SUBROUTINE USEDAT
!     Calls: NONE
!     Common blocks referenced: AIRDAT, ATPATH, AGCLAS, DEVICE, LOCNAM, PSET1, PROD
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter      Set/
!     Name           Used  Type  Location  Parameter Description
!     -------------- ----  ----  --------  ------------------------------
!     variable        S    CHAR  Argument  nnnnnnnnnnnnnnnnnnnnnn
!                     U    INT   Internal
!                     S/U  REAL  Common
!                          DBLE  External
!
!==== Modification History ===================================================
!
!     Date           Who  Modification Description
!     --------       ---  ------------------------------------------------------
!  
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE ATINPR(SETDATA,NLINES) 
!
!==== COMMON Block Definitions ===============================================
!
      include 'ATPATH.FTN'
      INCLUDE 'PARMTR.PAR'
      include 'PFLAGS.FTN'
!
!==== DIMENSION Statements ===================================================
!
       CHARACTER*(*) SETDATA(LINEMAX)
!
!==== Variable Declarations ==================================================
!
      INTEGER GETINT,IZ
!
!==== DATA Statements ========================================================
!
      DATA IZ/0/
!----- Start calculations. ---------------------------------------------------
!
!----- Read a record for regional air data -----------------------------------
!
      IIN =  GETINT(SETDATA,NLINES,'KAIN          ',IZ,IZ,IZ,IZ,IZ,IZ)
!
      KAIN = IIN
      IF(KAIN.LE.0.AND.KEXPTH(18).GT.0) KAIN = 1
      IF(KAIN.LE.0.AND.KEXPTH(24).GT.0) KAIN = 1
      RETURN
!
!============== END OF MODULE ATINPR ========================================
      END 
!
!   EPFDAT.FOR EXPOS                Version Date: 07-Jan-97
!   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE EPFDAT                                *
!                                                                            *
!  Subroutine EPFDAT  writes data for a parent(&progeny) for each time period*
!       and each exposure pathway                                            *
!                                                                            *
!                                                                            *
!  Written by:       Dl Strenge                                              *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    07-Jan-97                                               *
!  Last Modified:    07-Jan-97      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: EDUP/EXPOS
!     Called by: EXPOS
!     Calls: NONE
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!
!
!==== Modification History ===================================================
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!   07-Jan-97      DLS  Initial programming started
!
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE EPFDAT()
!
!---- Include statements -----------------------------------------------------
!
      INCLUDE 'DEVICE.FTN'
      INCLUDE 'ETIMES.FTN'
      INCLUDE 'PSET1.FTN'
      INCLUDE 'PFLAGS.FTN'
      INCLUDE 'DECAY.FTN'
      INCLUDE 'DEL.FTN'
!
!---- Variable Type Declarations ---------------------------------------------
!
      REAL EXPVAL(25)
      CHARACTER*12 EXPROUT(20),EXPRUT(25)
      CHARACTER*20 EXPLABL(20),EXPLAB(25)
      CHARACTER*7 EXPUNIT(20), EXPUNR(25), EXPUNC(25)
      CHARACTER*8 NAMOUT
      INTEGER NLABL(20),NEXPLAB(25)
      INTEGER NROUT(20),NEXPRUT(25)
      INTEGER NUNIT(20),NEXPUNR(25), NEXPUNC(25)
!
!---- Data Statements --------------------------------------------------------
!
      DATA EMIN/1.E-25/
      DATA EXPLAB/'Water               ',  &
                  'Shower              ',  &
                  'Shower              ',  &
                  'Leafy vegetables    ',  &
                  'Other vegetables    ',  &
                  'Meat                ',  &
                  'Milk                ',  &
                  'Fish                ',  &
                  'Crustacea           ',  &
                  'Swimming            ',  &
                  'Swimming            ',  &
                  'Shoreline           ',  &
                  'Shoreline           ',  &
                  'Soil                ',  &
                  'Soil                ',  &
                  'Food                ',  &
                  'Shower              ',  &
                  'Air                 ',  &
                  'Soil                ',  &
                  'Swimming            ',  &
                  'Boating             ',  &
                  'Shoreline           ',  &
                  'Soil                ',  &
                  'Air                 ',  &
                  'Indoor air          '/
      DATA EXPRUT/'ingestion   ',    &
                  'dermal      ',    &
                  'ingestion   ',    &
                  'ingestion   ',    &
                  'ingestion   ',    &
                  'ingestion   ',    &
                  'ingestion   ',    &
                  'ingestion   ',    &
                  'ingestion   ',    &
                  'ingestion   ',    &
                  'dermal      ',    &
                  'dermal      ',    &
                  'ingestion   ',    &
                  'ingestion   ',    &
                  'dermal      ',    &
                  'ingestion   ',    &
                  'inhalation  ',    &
                  'inhalation  ',    &
                  'inhalation  ',    &
                  'external    ',    &
                  'external    ',    &
                  'external    ',    &
                  'external    ',    &
                  'external    ',    &
                  'inhalation  '/
      DATA EXPUNR/'Bq/L   ',     &  ! water ingestion
                  'Bq/L   ',     &  ! shower dermal
                  'Bq/L   ',     &  ! shower ingestion
                  'Bq/kg  ',     &  ! leafy vegetables
                  'Bq/kg  ',     &  ! other vegetables
                  'Bq/kg  ',     &  ! Meat
                  'Bq/L   ',     &  ! Milk
                  'Bq/kg  ',     &  ! Fish
                  'Bq/kg  ',     &  ! Crustacea
                  'Bq/L   ',     &  ! swimming ingestion
                  'Bq/L   ',     &  ! swimming dermal
                  'Bq/kg  ',     &  ! shoreline dermal
                  'Bq/kg  ',     &  ! shoreline ingestion
                  'Bq/kg  ',     &  ! soil ingestion
                  'Bq/kg  ',     &  ! soil dermal
                  'Bq/kg  ',     &  ! foods
                  'Bq/m^3 ',     &  ! shower inhalation ! ^ added 2/2001
                  'Bq/m^3 ',     &  ! air inhalation    ! ^ added 2/2001
                  'Bq/m^3 ',     &  ! soil inhalation   ! ^ added 2/2001
                  'Bq/L   ',     &  ! swimming external
                  'Bq/L   ',     &  ! boating external
                  'Bq/kg  ',     &  ! shoreline external
                  'Bq/kg  ',     &  ! soil external
                  'Bq/m^3 ',     &  ! air external      ! ^ added 2/2001
                  'Bq/m^3 '/        ! indoor inhalation ! ^ added 2/2001
      DATA EXPUNC/'mg/L   ',     &  ! water ingestion
                  'mg/L   ',     &  ! shower dermal
                  'mg/L   ',     &  ! shower ingestion
                  'mg/kg  ',     &  ! leafy vegetables
                  'mg/kg  ',     &  ! other vegetables
                  'mg/kg  ',     &  ! Meat
                  'mg/L   ',     &  ! Milk
                  'mg/kg  ',     &  ! Fish
                  'mg/kg  ',     &  ! Crustacea
                  'mg/L   ',     &  ! swimming ingestion
                  'mg/L   ',     &  ! swimming dermal
                  'mg/kg  ',     &  ! shoreline dermal
                  'mg/kg  ',     &  ! shoreline ingestion
                  'mg/kg  ',     &  ! soil ingestion
                  'mg/kg  ',     &  ! soil dermal
                  'mg/kg  ',     &  ! foods
                  'mg/m^3 ',     &  ! shower inhalation  ! ^ added 2/2001
                  'mg/m^3 ',     &  ! air inhalation     ! ^ added 2/2001
                  'mg/m^3 ',     &  ! soil inhalation    ! ^ added 2/2001
                  'N/A    ',     &  ! swimming external
                  'N/A    ',     &  ! boating external
                  'N/A    ',     &  ! shoreline external
                  'N/A    ',     &  ! soil external
                  'N/A    ',     &  ! air external
                  'mg/m^3 '/        ! indoor inhalation  ! ^ added 2/2001
      DATA NEXPLAB/5, 6, 6,16,16, 4, 4 ,4, 9, 8,  &
                   8, 9, 9, 4, 4, 4,10, 3, 4, 8,  &
                   7, 9, 4, 3, 10/
      DATA NEXPRUT/9,6,8*9,6,6,9,9,6,9,3*10,5*8,10/
      DATA NEXPUNR/3*4,3*5,4,2*5,2*4,5*5,3*6,2*4,2*5,2*6/
      DATA NEXPUNC/3*4,3*5,4,2*5,2*4,5*5,3*6,5*4,6/
!
!----- For each time period, write parent media concentrations, the progeny --
!      media concentrations for all exposure pathways with positive conc.
!
!       WRITE(nels,*) ' In EPFDAT for MTYPE(1) = ',MTYPE(1)
        MT = 1
        IF(MTYPE(1).GT.1) MT = 2
        DO IC = 1,NUC
          IOUT = 0
          DO IP = 1,25
!
!----- Set values for output for selected pathways and non-zero concentrations-
!
            IF(KEXPTH(IP).GT.0.AND.DEL(IP,IC).GT.EMIN) THEN
              IF(MT.EQ.1) THEN
                IOUT = IOUT + 1
                EXPLABL(IOUT) = EXPLAB(IP)
                NLABL(IOUT)   = NEXPLAB(IP)
                EXPROUT(IOUT) = EXPRUT(IP)
                NROUT(IOUT)   = NEXPRUT(IP)
                EXPUNIT(IOUT) = EXPUNR(IP)
                NUNIT(IOUT)   = NEXPUNR(IP)
                EXPVAL(IOUT) = DEL(IP,IC)
              ELSEIF(IP.LT.20.OR.IP.EQ.25) THEN
                IOUT = IOUT + 1
                EXPLABL(IOUT) = EXPLAB(IP)
                NLABL(IOUT)   = NEXPLAB(IP)
                EXPROUT(IOUT) = EXPRUT(IP)
                NROUT(IOUT)   = NEXPRUT(IP)
                EXPUNIT(IOUT) = EXPUNC(IP)
                NUNIT(IOUT)   = NEXPUNC(IP)
                EXPVAL(IOUT) = DEL(IP,IC)
              ENDIF
            ENDIF
!            WRITE(nels,*) ' Chain member =',IC, ' Path index = ',ip,' IOUT = ',IOUT, ' RESULT ',DEL(IP,IC)
          END DO
!
!----- Write concentrations for parent at each time --------------------------
!
            IF(IC.EQ.1) THEN
              WRITE(NEPF,300) CTIME,DUROUT,IOUT
 300   FORMAT(F8.1,',"yr",',F5.1,',"yr",',I3,',')
            ELSE
              NAMOUT(1:4) = POLID(1,IC)
              NAMOUT(5:8) = POLID(2,IC)
              NC = LEN_TRIM(NAMOUT)
              WRITE(NEPF,100) NAMOUT(1:NC),NAMOUT(1:NC),IOUT
 100   FORMAT('"',A,'","',A,'",',I3,',')
            ENDIF
            IF(IOUT.GT.0) THEN  ! write results
              DO IO = 1,IOUT
                WRITE(NEPF,200) EXPLABL(IO)(1:NLABL(IO)),   &
                                EXPROUT(IO)(1:NROUT(IO)),   &
                                EXPUNIT(IO)(1:NUNIT(IO))
                WRITE(NEPF,201) EXPVAL(IO)
 200   FORMAT('"',A,'","',A,'","',A,'"')
 201   FORMAT(1PE10.3,',')
              END DO
            END IF
        END DO
!
      RETURN
!
!----- END OF MODULE EPFDAT -------------------------------------------------
!
      END
!
!   MEPAS AHAZ: SOILAC.FOR          Version Date:  04-Jan-96
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE SOILAC                                *
!                                                                            *
!  This subroutine controls analysis as a function of time for one           *
!  pollutant at a time.  The input is the soil concentration at time zero.   *
!  Output is generated by calls to other subroutines.                        *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    04-Jan-1996 (New for 100 year update)                   *
!  Last Modified:    04-Jan-1996  DLS                                        *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZ
!     Called by: AHAZ
!     Calls: CHAIN,EXPOS
!     Common blocks referenced: DEVICE, PFLAGS
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     variable    S    CHAR    Argument  nnnnnnnnnnnnnnnnnnnnnn
!                 U    INT     Internal
!                 S/U  REAL    Common
!                      DBLE    External
!      NYEARS     U    INT     Argument  Number of years in each time period
!      LOCREC     U    INT     Argument  Usage location index
!      NPERDS     U    INT     Argument  Number of time periods to evaluate
!      CSOIL      S    Real    Argument  Array of soil concentration values
!      IERR       S    Int     Argument  Error flag for file read errors
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
! 
!
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE SOILAC(SETDATA,NLINES,NYEARS,LOCREC,NPERDS,CSOIL,TSTART,IERR)
!
!==== COMMON Block Definitions ===============================================
!
        INCLUDE 'PARMTR.PAR'
        INCLUDE 'DEVICE.FTN'
        INCLUDE 'DECAY.FTN'
        INCLUDE 'ETIMES.FTN'
        INCLUDE 'PSET1.FTN'
        INCLUDE 'PSET3.FTN'
        INCLUDE 'SLPATH.FTN'
        INCLUDE 'CRLOC.FTN'
        INCLUDE 'LEACH.FTN'
!
!==== DIMENSION Statements ===================================================
!
      DIMENSION CSOIL(20)
!
!==== Variable Declarations ==================================================
!
      INTEGER IERR
      REAL TSTART
      DOUBLE PRECISION, DIMENSION (20) :: CS, ALOSS
      CHARACTER*(*) SETDATA(LINEMAX)
      CHARACTER*8 NAMOUT
!
!==== DATA Statements ========================================================
!
      DATA C365/365.25/
!
!==== INITIALIZATIONS ========================================================
!
      IPATH = 5
      IERR = 0
      TIM = TSTART
      DELT = FLOAT(NYEARS)
      TDAYS = DELT*C365
!
!==== START OF ANALYSIS ======================================================
!
!---- Get leach rate constants for this pollutant ----------------------------
!
      CALL GETLEACH(SETDATA,NLINES)
!
!---- Write first line for current parent into EPF file ----------------------
!
      NAMOUT(1:4) = POLID(1,1)
      NAMOUT(5:8) = POLID(2,1)
      NC = LEN_TRIM(NAMOUT)
      NP = LEN_TRIM(CONNAM(1))
      WRITE(NEPF,1000) CONNAM(1)(1:NP),NAMOUT(1:NC),NUC-1,NPERDS
 1000 FORMAT('"',A,'","',A,'",',I2,',',I4,',')
!
!---- Loop over time periods for this data set -------------------------------
!
      DO ITIM = 1,NPERDS
!
!---- set start time for output ----------------------------------------------
!
        DURTIM = FLOAT(NYEARS)
        CTIME = TSTART + (ITIM-1)*DURTIM
!
!---- For first time period, decay from time zero to start of risk analysis --
!
        IF(ITIM.EQ.1) THEN
          CS(1) = CSOIL(1)
          ALOSS(1) = ALEACH(1)
          IF(MTYPE(1).NE.1) ALOSS(1) = ALEACH(1) + DBLE(SLAM(1))   ! ** Updated 15 November 2004, DL Strenge
!
          IF(NUC.GT.1) THEN
            DO IN = 2,NUC
              CS(IN) = CSOIL(IN)
              ALOSS(IN) = ALEACH(IN)
              IF(MTYPE(1).NE.1) ALOSS(IN) = ALEACH(IN) + DBLE(SLAM(IN))   ! ** Updated 15 November 2004, DL Strenge
            END DO
          END IF
          IF(TSTART.GT.0.) THEN
            INTGRL= 0
            TST=TSTART*C365
!            IF(MTYPE(1).EQ.1) THEN
!              CALL CHAIN(TST,ALEACH,CS,CS,INTGRL)
!            ELSE
              CALL CHAIN(TST,ALOSS,CS,CS,INTGRL,CHEM)
!            ENDIF
          END IF
!
        END IF
!
!---- Call EXPOS to evaluate health impacts for current time period ----------
!
         IPOL = 1
         CALL EXPOS(IPATH,LOCREC,IPOL,ITIM,CS,CS,CS,DELT,IERR)
         TIM = TIM + DELT
         IF(IERR.GT.0) GO TO 998
!
!---- Calculate soil concentration at start of next period
!---- Set contribution from amount present at start of previous period -------
!
        INTGRL = 0
!        IF(MTYPE(1).EQ.1) THEN
!          CALL CHAIN(TDAYS,ALEACH,CS,CS,INTGRL)
!        ELSE
!          AL(1) = DBLE(SLAM(1))
          CALL CHAIN(TDAYS,ALOSS,CS,CS,INTGRL,CHEM)
!        ENDIF
      END DO  ! End of loop on time periods
!
!==== N0RMAL RETURN ==========================================================
!
      RETURN
!
!==== PROCESS READ ERROR CONDITIONS ==========================================
!
998   WRITE(NERR,400) ITIM
 400  FORMAT(' Error in EXPOS call in SOILAC during time period ',I6)
      IERR = IERR + 1
      RETURN
      END
!=================== END OF MODULE SOILAC ===================================
!
!   MEPAS: SCFIN.FOR                   Version Date:  16-Feb-1998
!   Copyright 1998 by Battelle Memorial Institute.  All Rights reserved.
! ****************************************************************************
!
!                            SUBROUTINE SCFDAT
!
!  Subroutine SCFIN searches the SCF file for the correct data set defined by
!  parameter SRCNAME.  Data sets are searched and skipped until the correct set
!  is found.  When found, the read position is at the start of the data set
!  for the first parent constituent.
!
!  Written by:     DL Strenge
!                  Battelle Pacific Northwest Laboratories
!                  P.O. Box 999
!                  Richland, WA 99352
!
!  Creation Date:  16-Feb-98   by DL Strenge
!  Last Modified:  16-Feb-98   by DL Strenge
!
! ****************************************************************************
!
!===== Modular Organization
!
!   Module of: MEPAS
!   Called by: AHAZX (main program)
!   Calls: HEADIN, SEQI, LENWORD
!
!===== Significant Parameter Designation and Description =====================
!
!  Parameter   Set/
!    Name      Used  Type  Location    Parameter Description
!  ----------  ----  ----  ----------  ---------------------------------------
!  SRCNAME      U    Char  Argument    Name of the data set sought in the
!                                      soil concentrations file (SCF)
!  NUMNUC       S    Int.  Argument    Number of chain members for the current
!                                      parent constituent.
!  IERR         S    Int.  Argument    Error flag, incremented if a read error
!                                      occurs reading from the SCF file.
!
!===== Modification History ==================================================
!   Date     Who Modification Description
!  --------- --- -------------------------------------------------------------
!
!===== Subroutine Call =======================================================
!
      SUBROUTINE SCFIN(TPATH,MODNAME,NUMNUC,IERR,SRCNAME)
!
!---- Include Statements -----------------------------------------------------
!
      INCLUDE 'DEVICE.FTN'
      INCLUDE 'COUPLE.FTN'
!
!----- Variable Type Declarations --------------------------------------------
!
      REAL XD, YD, ZD
      INTEGER IERR,tpath
      INTEGER LENWORD
      LOGICAL SKIP, SEQI
      CHARACTER*3 ALLL
      CHARACTER*6 SOURCE,UX,UY,UZ
      CHARACTER*12 CIDIN,TU,CU
      CHARACTER*14 MEDTYPE
      CHARACTER*32 MEDNAME, MODNAME, SRCNAME
      CHARACTER*20 LOCNAM,CNAMIN
!
!----- Data Statements -------------------------------------------------------
!
      DATA SOURCE/'Soil                '/
      DATA ALLL/'all'/
!
!----- Start of Analysis -----------------------------------------------------
      write(nels,*) 'SCFIN Call <<<<<<<<<<<<<<<<<<<<<<<<<<'
!
      SKIP = .TRUE.   !scfin
!
!----- Read heading information from Soil Concentration file ------------------
!
      CALL HEADIN(TPATH,NMEDIA)
!
!----- Loop on number of media in the SCF file ------------------------------
!
      DO IMED = 1,NMEDIA
!        READ(NSCF,*,ERR=999,END=999) MEDNAM,MEDTYPE,NLOC
!        MEDTYPE = MEDTYPE
!
!-----   Read the first line for the current location data set ---------------
!
!          READ(NSCF,*,ERR=999,END=999) LOCNAM,XD,UX,YD,UY,ZD,UZ,NCON
!  Revised reading of first line of data set (eliminated number of locations)
!
!-----   Read the first line for the current location data set ---------------
!
        READ(NSCF,*,ERR=999,END=999) MEDNAME,MEDTYPE,XD,UX,YD,UY,ZD,UZ,NCON
          XD = XD
          YD = YD
          ZD = ZD
          UX = UX
          UY = UY
          UZ = UZ
!
!-----    Check the current medium and location for desired data set ---------
!
          IF(SEQI(MEDNAME,MODNAME,32).OR.SEQI(MEDNAME,ALLL,3)) THEN
            IF(LEN(TRIM(MEDTYPE)).EQ.4) THEN
               IF(SEQI(MEDTYPE,SOURCE,4)) SKIP = .FALSE.
            ENDIF
          END IF  
!
          IF(SKIP) THEN
!
!----  If not the desired data set, read past the set to the start of the next
!
            DO IC = 1,NCON
              READ(NSCF,*,ERR=999,END=999) CNAMIN,CIDIN,TU,CU,NTIMES,NPG
              CIDIN = CIDIN
              TU = TU
              CU = CU
              CNAMIN = CNAMIN
              DO IT = 1,NTIMES
                READ(NSCF,*,ERR=999,END=999) T
              END DO   ! End of loop on time points
              T = T
              IF(NPG.GT.0) THEN
                DO IP = 1,NPG
                  READ(NSCF,*,ERR=999,END=999) CNAMIN,CIDIN,TU,CU,NTIMES
                  DO IT = 1,NTIMES
                    READ(NSCF,*,ERR=999,END=999) T
                  END DO   ! End loop on times for current progeny
                END DO   ! End loop on progeny
              END IF  ! End IF on progeny
            END DO   ! End of loop on constituents in skipped data set
          ELSE
            NUMNUC = NCON
          END IF   ! End of IF on reading past a data set
          IF(.NOT.SKIP) RETURN
      END DO   ! End of loop on media
!
!---- Write error message, data set not found --------------------------------
!
!      NS = LEN_TRIM(NAME)
      WRITE(NERR,1115) SRCNAME
 !     WRITE(NERR,1115) NAME(1:NS)
 1115 FORMAT(' Error in SCFIN, soil medium set not found: '/  &
             ' for source name ',A)
      IERR = IERR + 1
      RETURN
!
!---- Process error condition on NSCF read -----------------------------------
!
! 999  NS = LEN_TRIM(NAME)
 999  WRITE(NERR,1000) MEDNAME
!      WRITE(NERR,1000) NAME(1:NS)
 1000 FORMAT(' Read error on soil concentration file'/  &
             '  for location ',A)
      IERR = IERR + 1
      RETURN
      END
!
! *** End of Module SCFIN ****************************************************
!

!   MEPAS: SCFDAT.FOR                  Version Date:  16-Feb-1998
!   Copyright 1998 by Battelle Memorial Institute.  All Rights reserved.
! ****************************************************************************
!
!                            SUBROUTINE SCFDAT
!
!  Subroutine SCFDAT reads one set of soil concentration data from the SCF
!  input file for one chain member.  The soil concentration for the first time
!  point is returned as the initial concentration.
!
!  Written by:     DL Strenge
!                  Battelle Pacific Northwest Laboratories
!                  P.O. Box 999
!                  Richland, WA 99352
!
!  Creation Date:  16-Feb-98   by DL Strenge
!  Last Modified:  16-Feb-98   by DL Strenge
!
! ****************************************************************************
!
!===== Modular Organization
!
!   Module of: MEPAS
!   Called by: AHAZX (main program)
!   Calls: None
!
!===== Significant Parameter Designation and Description =====================
!
!  Parameter   Set/
!    Name      Used  Type  Location    Parameter Description
!  ----------  ----  ----  ----------  ---------------------------------------
!  IEP          U    Int.  Argument    Position in chain member for saving the
!                                      soil concentrations read in this call
!  NTIMC        U    Int.  Argument    Number of times points data is to be read
!                                      for, time/soil concentration pairs.
!  XMF          U    Real  Argument    Units conversion factor for input soil
!                                      concentrations (XMF = 1 for rads, 1000
!                                      for chemicals.
!  CSAIL(20)    S    Real  Argument    Array of soil concentrations for a decay
!                                      chain.  One value is filled per call, into
!                                      the position of the current member, IEP.
!  IERR         S    Int.  Argument    Error flag, incremented if a read error
!                                      occurs reading from the SCF file.
!
!===== Modification History ==================================================
!   Date     Who Modification Description
!  --------- --- -------------------------------------------------------------
!
!===== Subroutine Call =======================================================
!
      SUBROUTINE SCFDAT(IEP,NTIMC,XMF,CSOIL,IERR)
!
!---- Include Statements -----------------------------------------------------
!
      INCLUDE 'DEVICE.FTN'
!
!----- Variable Type Declarations --------------------------------------------
!
      REAL CSOIL(20), XMF, TIME, SCON
      INTEGER IEP,NTIMC,IERR
      INTEGER IC,IT
!
!----- Start of Analysis -----------------------------------------------------
!
      DO IT = 1,NTIMC
        READ(NSCF,*,ERR=999,END=999) TIME,SCON
        TIME = TIME
        IF(IT.EQ.1) CSOIL(IEP) = SCON*XMF
      END DO
      RETURN
!
!---- Process error condition on NSCF read -----------------------------------
!
 999  WRITE(NERR,1000) IEP,IT
 1000 FORMAT(' Read error on soil concentration file'/        &
             '  for chain member ',I2,' and time period',I3/  &
             '  (message written in SCFDAT subroutine)')
      IERR = IERR + 1
      RETURN
      END
!
! *** End of Module SCFDAT ***************************************************
!
!
!   EPFDSET.FOR EXPOS                Version Date: 06-May-97
!   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE EPFDSET                               *
!                                                                            *
!  Subroutine EPFHEAD writes data set heading information and data           *
!                                                                            *
!                                                                            *
!  Written by:       Dl Strenge                                              *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    07-Jan-97                                               *
!  Last Modified:    06-May-97      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: EDUP/EXPOS
!     Called by: EXPOS
!     Calls: NONE
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     TPATH      U      Int    Argument  Transport pathway type: 1-GW, 2-SW, 3-air, 4-soil
!     RECNAM     U    Char*32  Argument  Exposure Location Name
!     NPOINTS    U      Int    Argument  Number of exposure points
!     NUMNUC     U      Int    Argument  Number of radionuclide parents
!==== Modification History ===================================================
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!   
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE EPFDSET(TPATH,RECNAM,NPOINTS,NUMNUC)
!
!---- Include statements -----------------------------------------------------
!
      INCLUDE 'DEVICE.FTN'
      INCLUDE 'LOCNAM.FTN'
!
!---- Variable Type Declarations ---------------------------------------------
!
      INTEGER NUMNUC, TPATH
      CHARACTER*20 RECNAM
      CHARACTER*7 AQUIF
      CHARACTER*13 SWATR
      CHARACTER*4 SOILNM
      CHARACTER*5 FOODNM
!
!---- Data Statements --------------------------------------------------------
!
      DATA AQUIF  /'Aquifer'/
      DATA SWATR  /'Surface water'/
      DATA SOILNM /'Soil'/
      DATA FOODNM /'Foods'/
!
!---- Write first line of data set: chronic, polar, num. exp. loc. -----------
!
      NPOINTS = 1
!
!----- For each exposure location: write name, medium, co-ordinates, Number --
!      of constituents (parents)
! --- Groundwater transport ------
!
      NR = LEN_TRIM(RECNAM)
      IF(TPATH.EQ.1) WRITE(NEPF,200) RECNAM(1:NR),AQUIF,NPOINTS,NUMNUC
 200  FORMAT('"chronic","',A,'","',A,'",',I2,',',I2,',')
!
!---- Surface water transport ------
!
      IF(TPATH.EQ.2) WRITE(NEPF,200) RECNAM(1:NR),SWATR,NPOINTS,NUMNUC
!
!---- Soil measured pathway ----------
!
      IF(TPATH.EQ.5) WRITE(NEPF,200) RECNAM(1:NR),SOILNM,NPOINTS,NUMNUC
!
!---- Food measured pathway ----------
!
      IF(TPATH.EQ.6) WRITE(NEPF,200) RECNAM(1:NR),FOODNM,NPOINTS,NUMNUC
!
!---- Write x,y coordinate line (0,0) ---------------------------------------
!
      IF(TPATH.NE.3) THEN
         write(nepf,300)
 300     format('0.,"km",0.,"km"')
      ENDIF
      RETURN
!
!----- END OF MODULE EPFDSET ------------------------------------------------
!
      END
!
!    GETSET.FOR                      Version Date: 28-Oct-96
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE GETSET                                *
!                                                                            *
!  Subroutine GETSET reads a set of lines from the GID file                  *
!                                                                            *
!  Written by:       Dl Strenge                                              *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    28-Oct-96                                               *
!  Last Modified:    28-Oct-96      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: GENERAL USE
!     Called by: ...
!     Calls: NONE
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     ***THE FILES MUST ALREADY BE OPEN***
!     NGID       U      INT    Argument  Logical unit of the .GID file
!     NERR       U      INT    Argument  Logical unit for writing error messages
!     NAME       U     char*14 Argument  Name of data set in GID file to be
!                                        found in this call
!     NLINES    S/U     INT    Argument  Number of lines read in GID data set
!     SETDATA    S     char*80 GID file  Storage array for GID data set
!     NSITE      U      INT    Argument  Site index used to select data lines
!                                        from GID data set when FILTER = .true.
!     FILTER     U      Log    Argument  Logical flag to filter data in GID
!                                        data set while reading
!==== Modification History ===================================================
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE GETSET(NGID,NERR,NAME,NLINES,SETDATA,NSITE,FILTER)
!
!---- Variable Type Declarations ---------------------------------------------
!
      include 'parmtr.par'
      LOGICAL MOVESET, FILTER
      INTEGER NGID,NLINES,NERR,NGOOD
      CHARACTER*(*) SETDATA(linemax)
      CHARACTER*120 SETLINE
      CHARACTER*(*) NAME
      CHARACTER*14 PNAM
!
!---- Data Statements --------------------------------------------------------
!
!
!---- Start of Analysis
!    MOVESET finds the location of the first record on the set, and the
!    number of lines to be read (NLINES)
!
      IF (MOVESET(NGID,NAME,NLINES)) THEN
!
!----- Set name was found in the .GID file.  Read a data set ---------------
!
        IF(NLINES.LE.0.OR.NLINES.GT.linemax) THEN   ! Test NLINES for range
          WRITE(NERR,100) NLINES,linemax
 100      FORMAT(' Error in GID specification of NLINES: ',I5/  &
                 ' Maximum allowed is ',I5)
        ELSE                            ! Read data set
          NGOOD = 0
          DO ILN = 1,NLINES
            IF(FILTER) THEN
              READ(NGID,'(A120)',ERR=300,END=400) SETLINE
              READ(SETLINE,*) PNAM,ISITE
                IF(ISITE.EQ.NSITE) THEN
                   NGOOD = NGOOD + 1
                   SETDATA(NGOOD) = SETLINE
                ENDIF
            ELSE 
              READ(NGID,'(A120)',ERR=300,END=400) SETDATA(ILN)
            ENDIF
          END DO
          IF(FILTER) NLINES = NGOOD
        ENDIF
      ELSE
        WRITE(NERR,105) NAME
 105    FORMAT(' Error in GID read.  Set name not found. ',A14)
      ENDIF
      PNAM = PNAM
      RETURN
 300  WRITE(NERR,101)ILN
 101  FORMAT(' Error in GID set read at line ILN: ',I5)
      RETURN
 400  WRITE(NERR,102) ILN, NLINES
 102  FORMAT(' Error in GID set read. EOF after reading ',I4,' of',I4,' lines')
      RETURN
!----- END OF MODULE GETSET -------------------------------------------------
      END

!  ETMRED.FOR  AHAZX                Version Date: 30-Jun-97
!   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE ETMRED                                *
!                                                                            *
!  Subroutine ETMRED reads the temporary atmospheric exposure output data    *
!             and generates EPF file lines for a chain and all locations     *
!                                                                            *
!  Written by:       DL Strenge                                              *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    30-Jun-97                                               *
!  Last Modified:    30-Jun-97      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZX
!     Called by: AHAZX
!     Calls: NONE
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     ARRAY     S/U    REAL    Argument  Array of parameter values for output
!     VALIN      U     REAL    Argument  Array of input values
!     ICM        U     INT     Argument  Position of chain member of interest
!     NX1        U     INT     Argument  Number of positions to be filled
!     IX2        U     INT     Argument  Position of data for second co-ordinate
!==== Modification History ===================================================
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!   
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE ETMRED(NPERDS,INC,EXPNAM,NCONST)
!
!---- Include Statements ------------------------------------------------------
!
      INCLUDE 'AIRINFO.FTN'
      INCLUDE 'ATPATH.FTN'
      INCLUDE 'DECAY.FTN'
      INCLUDE 'DEVICE.FTN'
      INCLUDE 'FLUX.FTN'
      INCLUDE 'PFLAGS.FTN'
      INCLUDE 'PSET1.FTN'
      INCLUDE 'TIMES.FTN'
      INCLUDE 'VTIN.FTN'
!
!---- Variable Type Declarations ---------------------------------------------
!
      REAL TIME(1000)
      CHARACTER*100 CDIN
      CHARACTER*12 EXPROUT(10)
      CHARACTER*20 EXPLABL(10),expnam
      CHARACTER*7 EXPUNITR(10),EXPUNITC(10),EXPUNIT(10)
      CHARACTER*3 CAIR
      CHARACTER*8 NAMOUT
      INTEGER IKROS(10),NEXPLAB(10),NEXPRUT(10),NEXPUN(10)
      LOGICAL PATHOUT(10)
!
!---- DATA Statements --------------------------------------------------------
!
      DATA CAIR/'Air'/
      DATA IKROS/4,5,6,7,14,15,18,19,23,24/
      DATA EXPLABL/'Leafy vegetables    ',  &
                  'Other vegetables    ',   &
                  'Meat                ',   &
                  'Milk                ',   &
                  'Soil                ',   &
                  'Soil                ',   &
                  'Air                 ',   &
                  'Soil                ',   &
                  'Soil                ',   &
                  'Air                 '/
      DATA NEXPLAB/16,16,4*4,3,4,4,3/
      DATA EXPROUT/'ingestion   ',    &
                  'ingestion   ',     &
                  'ingestion   ',     &
                  'ingestion   ',     &
                  'ingestion   ',     &
                  'dermal      ',     &
                  'inhalation  ',     &
                  'inhalation  ',     &
                  'external    ',     &
                  'external    '/
      DATA NEXPRUT/5*9,6,10,10,8,8/
      DATA EXPUNITR/'Bq/kg  ',    &   ! leafy vegetables
                    'Bq/kg  ',    &   ! other vegetables
                    'Bq/kg  ',    &   ! Meat
                    'Bq/L   ',    &   ! Milk
                    'Bq/kg  ',    &   ! soil ingestion
                    'Bq/kg  ',    &   ! soil dermal
                    'Bq/m^3 ',    &   ! air inhalation  ! ^ added 2/2001
                    'Bq/m^3 ',    &   ! soil inhalation ! ^ added 2/2001
                    'Bq/kg  ',    &   ! soil external
                    'Bq/m^3 '/        ! air external    ! ^ added 2/2001
      DATA EXPUNITC/'mg/kg  ',    &   ! leafy vegetables
                    'mg/kg  ',    &   ! other vegetables
                    'mg/kg  ',    &   ! Meat
                    'mg/L   ',    &   ! Milk
                    'mg/kg  ',    &   ! soil ingestion
                    'mg/kg  ',    &   ! soil dermal
                    'mg/m^3 ',    &   ! air inhalation  ! ^ added 2/2001
                    'mg/m^3 ',    &   ! soil inhalation ! ^ added 2/2001
                    'N/A    ',    &   ! soil external
                    'N/A    '/        ! air external
      DATA NEXPUN/10*5/
!
!---- Calculate the number of lines in the ETM file --------------------------
!
      MT = 1
      IF(MTYPE(1).GT.1) MT = 2
      IF(ONEPOINT) THEN
        NLINES = NUC * NPERDS
      ELSE
        NLINES = NUC * NPERDS * NAX1 * NAX2
      ENDIF
      IF(NLINES.LE.0) THEN
         WRITE(NERR,1000) NUC,NPERDS,NAX1,NAX2
 1000    FORMAT(' Error in reading ETM file, number of lines = 0'/  &
                '   Number of chain members =',I3/                  &
                '   Number of times         =',I5/                  &
                '   Number of axis-1 points =',I3/                  &
                '   Number of axis-2 points =',I3)
         GO TO 999
      END IF
!
!---- Set exposure pathway flag and count for rapid printing of results ------
!
      NOUT = 0
      DO IE = 1,10
         PATHOUT(IE) = .FALSE.
         IF(KEXPTH(IKROS(IE)).GT.0) THEN
           IF(MT.EQ.1) THEN
              PATHOUT(IE) = .TRUE.
              NOUT = NOUT + 1
              EXPUNIT(ie) = EXPUNITR(IE)
              NEXPUN(ie) = LEN_TRIM(EXPUNIT(ie))
           ELSEIF (IE.LT.9) THEN
              PATHOUT(IE) = .TRUE.
              NOUT = NOUT + 1
              EXPUNIT(ie) = EXPUNITC(IE)
              NEXPUN(ie) = LEN_TRIM(EXPUNIT(ie))
           ENDIF
         ENDIF
      END DO
      IF(NOUT.LE.0) THEN
        WRITE(NERR,1001) KEXPTH
 1001   FORMAT(' Error in pathway selection while writing EPF file for', &
               ' atmospheric pathways:',i3/                              &
               '   Pathway index values:',25I2)
        GO TO 999
      ENDIF
!
!----- Set number of points ---------------------------------------------------
!
      IF(ONEPOINT) THEN
        NPOINTS = 1
        NX2 = 1 + (NXY - 1)/NAX1
        NX1 = NXY - (NX2 - 1) * NAX1
      ELSE
        NPOINTS = NAX1*NAX2
      ENDIF
!
!
!---- Write each location and each location coordinate points to EPF file -----
!     if first constituent
!
      IF(INC.EQ.1) THEN
       NE = LEN_TRIM(EXPNAM)
       WRITE(NEPF,204) EXPNAM(1:NE),CAIR,NPOINTS,NCONST
 204   FORMAT('"chronic","',A,'","',A,'",',I4,',',I3,',')
       IF(ONEPOINT) THEN
          WRITE(NEPF,202) EXPX,EXPY
       ELSE
       DO IX2 = 1,NAX2
        DO IX1 = 1,NAX1
          INDEX = IX1 + (IX2-1)*NAX1
          WRITE(NEPF,202) EXPXA(INDEX),EXPYA(INDEX)
 202      FORMAT(F9.2,',"km",',f9.2,',"km",')
        END DO
       END DO
      ENDIF  ! If ONEPOINT
      END IF ! If INC = 1
!
!---- Write first line for constituent parent --------------------------------
!
      NPROG = NUC-1
      NAMOUT(1:4) = POLID(1,1)
      NAMOUT(5:8) = POLID(2,1)
      NC = LEN_TRIM(NAMOUT)
      NCC = LEN_TRIM(CONNAM(1))
      WRITE(NEPF,203) CONNAM(1)(1:NCC),NAMOUT(1:NC),NPROG,NPERDS
 203  FORMAT('"',A,'","',A,'",',I2,',',I5,',')
!
!----  Set time increment data -----------------------------------------------
!
      TIME(1) = TSTART
      DO IT = 2,NPERDS
         TIME(IT) = TIME(IT-1) + DELT
      END DO
!
!---- Loop on time points in input concentration data ------------------------
!
      DO IT = 1,NPERDS
!
!---- Loop on number of chain members ----------------------------------------
!
        DO IN = 1,NUC
!
!------- Loop on number of lines in ETM file and extract values for current --
!        radionuclide (IN) and time period (IT)
!
           REWIND(NETM)
           DO IL = 1,NLINES
              READ(NETM,2001) ININ,ITIN,IX1,IX2,CDIN
 2001         FORMAT(I3,I5,2I3,A)
              IF(ITIN.EQ.IT) THEN
                 IF(ININ.EQ.IN) THEN
                    READ(CDIN,2002) (VTIN(I,IX1,IX2),I=1,10)
 2002               FORMAT(10E10.3)
                 END IF
              END IF
!
           END DO  ! end loop on lines in ETM file
!
!---- Now have data for one chain member and time period, write to EPF -------
!     Test to see if pathway is to be included, KEXPTH(i) and print only
!     those pathways selected.  Cross index to VTIN array is IKROS.
!
!----- Write concentrations for parent at each time --------------------------
!
            IF(IN.EQ.1) THEN
              WRITE(NEPF,300) TIME(IT),ATED,NOUT
 300   FORMAT(F8.1,',"yr",',F7.1,',"yr",',I3,',')
            ELSE
              NAMOUT(1:4) = POLID(1,IN)
              NAMOUT(5:8) = POLID(2,IN)
              NC = LEN_TRIM(NAMOUT)
              NCC = LEN_TRIM(CONNAM(IN))
              WRITE(NEPF,100) CONNAM(IN)(1:NCC),NAMOUT(1:NC),NOUT
 100   FORMAT('"',A,'","',A,'",',I3,',')
            ENDIF
           DO IO = 1,10
              IF(PATHOUT(IO)) THEN
                if(vtin(io,5,11).lt.0.) then
                  write(nerr,1006) io,vtin(io,5,5)
 1006  format(' Negative concentration for pathway ',i2,1pe10.2)
                endif
                WRITE(NEPF,200) EXPLABL(IO)(1:NEXPLAB(IO)),  &
                                EXPROUT(IO)(1:NEXPRUT(IO)),  &
                                EXPUNIT(IO)(1:NEXPUN(IO))
                IF(ONEPOINT) THEN
                  WRITE(NEPF,201)   VTIN(IO,NX1,NX2)
                ELSE
                  WRITE(NEPF,201) ((VTIN(IO,IX1,IX2),IX1=1,NAX1),IX2=1,NAX2)
                ENDIF
 200   FORMAT('"',A,'","',A,'","',A,'"')
 201   FORMAT(360(1PE10.3,','))
              END IF
           END DO  ! End loop on exposure pathways 
         END DO   ! End loop on chain members
!
      END DO  !  end loop on time periods
!
 999  RETURN
!
!----- END OF MODULE ETMRED -------------------------------------------------
!
      END
!
!     MEPAS AHAZX: SETAIRC.FOR            Version Date: 07-Apr-1998
!     Copyright 1989 by Battelle Memorial Institute. All rights reserved.
! ****************************************************************************
!                                                                            *
!                         FUNCTION SETAIRC                                   *
!                                                                            *
!  FUNCTION SETAIRC sets air concentration and deposition rate data for      *
!  a chain member and time period using values from the ATO file.            *
!                                                                            *
!                                                                            *
!  Written by:       DL Strenge                                              *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    28-Aug-97                                               *
!  Last Modified:    07-Apr-98 DL Strenge                                    *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: AHAZX
!     Called by: SUBROUTINE AIRREG
!     Calls to: NONE
!     Common blocks referenced: none
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/           Location
!     Name      Used   Type    Export/Import  Parameter Description
!     --------- -----  ------  -------------- --------------------------------
!     T0        USED   REAL    ARG - IMPORT   Start time for the integral
!
!     T1        USED   REAL    LOCAL          End time for the integral
!
! (   M1        USED   INT     ARG - IMPORT   Parent number) Use Deleted
!
!     NYR       USED   INT     ARG            Number of years in averaging
!                                             period (years).  20-Nov-1995
!
!    PNT        USED   REAL    ARG            Concentration array
!
!    TIME       USED   REAL    ARG            Time points corresponding to
!                                             concentration values (years)
!
!     CO        USED   REAL    LOCAL          Concentration corresponding to
!                                             T0
!     C1        USED   REAL    LOCAL          Concentration corresponding to
!                                             T1
!     TEND1     USED   REAL    COMMON - IMP   END OF concentration data
!
!     TSTRT1    USED   REAL    COMMON - IMP   START OF concentration data
!
!     JSTART    USED   INT     LOCAL          Starting node for conc. and time
!                                             pair.
!     JEND      USED   INT     LOCAL          Ending node for conc. and time
!                                             pair.
!==== Modification History ===================================================
!
!  Date       Who  Modification Description
!  --------   ---  ------------------------------------------------------
!  11/02/93   JPM  Corrected "subscript out of range" error that occurred
!                  when TEND1 and TSTRT1 were an even multiple of 70.
!
!  20-Nov-95  DLS  Modified program to use variable time period, instead
!                  of the fixed 70 year time period.
!
!  07-Apr-98  DLS  Changed logic to treat values as averages over each time
!                  period and to treat "ends" correctly.
!
!  07-Apr-99  DLS  Changed logic to return a zero value if period is before
!                  start of data.
!==== FUNCTION CALL ========================================================
!
      FUNCTION SETAIRC(T0,NYR,PNT,TIME,IA,IN,IERRF)
!
!----COMMON Block Definitions ------------------------------------------------
!
      INCLUDE 'DEVICE.FTN'
!
!==== DIMENSION Statements ===================================================
!
      DIMENSION PNT(1000,20,2),TIME(1000,20)
      DIMENSION DELTIME(1000)
!
!==== Variable Declarations ==================================================
!
      INTEGER NYR
!
!==== DATA Statements ========================================================
!
!     None
!
! If the starting time is out of range, return a zero concentration
!
      IERRF = 0
      SETAIRC = 0.0
      nt = 1
      do i=2,1000
          if(time(i,IN).gt.0.) nt = i
      enddo
      IF (T0 .GT. TIME(nt,in)) THEN
        SETAIRC = 0.0
        RETURN
      END if
!
! *************************************************************
!
      IF(NYR.LE.0) GO TO 999
      TYR = FLOAT(NYR)
      TEND = T0 + TYR
      TEST = TEND + 1.
      IF(TEST.LE.TIME(1,IN)) THEN      !!!
        SETAIRC = 0.0
        RETURN
      ENDIF
      DELTIME(1) = TIME(1,IN)          !!!
      DO IT = 2,1000
         IF(TIME(IT,IN).GT.0.) THEN
            DELTIME(IT) = TIME(IT,IN) - TIME(IT-1,IN)     !!!   !!!
            NTIMES = IT
         ENDIF
      END DO
!
!----- Step through the time periods to find the data to use in the analysis
!
      C = 0.
      TOTIME = 0.0
      DO IT = 1,NTIMES
        TEXYR = DELTIME(IT)
        TT = TIME(IT,IN)         !!!
        IF(TT.GT.T0) THEN
          IF(IT.EQ.1) THEN
            TEXYR = TT - T0
            GO TO 10
          ENDIF
          IF(T0.GT.TIME(IT-1,IN)) THEN          !!!
            IF(TEND.LT.TT) THEN
              TX = TEND
            ELSE
              TX = TT
            ENDIF
            TEXYR = TX - T0
            GO TO 10
          ELSE
            IF(TEND.GE.TT) THEN
              TEXYR = DELTIME(IT)
            ELSE
              TEXYR = TEND - TIME(IT-1,IN)       !!!
            ENDIF
          ENDIF
 10       C = C + PNT(IT,IN,IA) * TEXYR          !!!
          TOTIME = TOTIME + TEXYR
          IF(TOTIME.GE.TYR) GO TO 20
        END IF
      END DO
 20   C = C / TYR
      SETAIRC = C
      RETURN
 999  CONTINUE
      WRITE(NERR,100) NYR, T0
      IERRF = 1
 100  FORMAT(' SETAIRC Error, Bad value for NYR =',I5,' at time ',1PE10.3)
      RETURN
      END
!
!  VINTERP.FOR  EXPOS                Version Date: 24-Jun-97
!   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE VINTERP                               *
!                                                                            *
!  Subroutine VINTERP controls interpolation of air concentration data over  *
!             time for each chain member, quantity, and coordinate point     *
!                                                                            *
!  Written by:       DL Strenge                                              *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    24-Jun-97                                               *
!  Last Modified:    24-Jun-97      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: EDUP/EXPOS
!     Called by: ATODAT
!     Calls: NONE
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     ARRAY     S/U    REAL    Argument  Array of parameter values for output
!     VALIN      U     REAL    Argument  Array of input values
!     ICM        U     INT     Argument  Position of chain member of interest
!     NX1        U     INT     Argument  Number of positions to be filled
!     IX2        U     INT     Argument  Position of data for second co-ordinate
!==== Modification History ===================================================
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!  
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE VINTERP(NCM,IT,TIMIN,NAX1,NAX2)
!
!---- Include Statements ------------------------------------------------------
!
      INCLUDE 'TIMES.FTN'
      INCLUDE 'DEVICE.FTN'
      INCLUDE 'VTIN.FTN'
!
!---- Variable Type Declarations ---------------------------------------------
!
      INTEGER NCM,NAX1,NAX2
!
!---- Start of Analysis
!
       WRITE(NATM,1002) IT,TIMIN
 1002  FORMAT(I3,F10.3)
!
!---- Write for current time, input concentration data ------------------------
!
       IF(DOAIR) THEN
         WRITE(NATM,1001) NCM,IT,((VTIN(1,IX1,IX2),IX2=1,NAX2),IX1=1,NAX1)
       ENDIF
 1001    FORMAT(2I3,1P,360(E9.2))
       IF(DODEP) THEN
         WRITE(NATM,1001) NCM,IT,((VTIN(2,IX1,IX2),IX2=1,NAX2),IX1=1,NAX1)
       ENDIF
!
      RETURN
!
!----- END OF MODULE VINTERP ------------------------------------------------
!
      END
!
!   INTAKE:MARKIN.FOR                      Version Date: 18-Jun-97
!   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                     SUBROUTINE MARKIN                                      *
!                                                                            *
!  Program MARKIN controls finding of the correct marker in the EPF file.    *
!                                                                            *
!  Written by:       Dl Strenge                                              *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    20-Dec-96                                               *
!  Last Modified:    18-Jun-97      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: GENII/INTAKE
!     Called by: INTAKE
!     Calls: NONE
!     Common blocks referenced: DEVICE
!
!==== Significant Parameter Designation and Description ======================
!
!   Parameter Set/
!   Name      Used  Type    Location  Parameter Description
!  --------- ----- -------  --------- -------------------------------------
!   NAME       U   Char*32  Argument  Name of marker being sought
!   FOUNDM     S   Logical  Argument  Flag set true if marker found
!==== Modification History ===================================================
!     Date    Who  Modification Description
!  ---------  ---  ------------------------------------------------------
!  
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE MARKIN(TPATH,NAME,FOUNDM)
!
!---- Include Statements for Parameter and Common Declarations ---------------
!
      INCLUDE 'DEVICE.FTN'
!
!---- Variable Type Declarations ---------------------------------------------
!
      CHARACTER*1 C
      CHARACTER*32 MARK
      CHARACTER*(*) NAME
      LOGICAL FOUNDM, SEQI
      INTEGER TPATH
!
!---- Data Statements --------------------------------------------------------
!
      DATA N32/32/
!
!---- Set input unit for reading ---------------------------------------------
!
      IF(TPATH.LE.2) NUNIT = NWCF
      IF(TPATH.EQ.3) NUNIT = NATO
      IF(TPATH.EQ.5) NUNIT = NSCF
      FOUNDM = .FALSE.
      REWIND(NUNIT)
!
!---- Read a marker line ----------------------------------------------------
!
 10   READ(NUNIT,*,ERR=999,END=999) MARK,NLTEXT  ! Read a marker and number of lines
       IF(.NOT.SEQI(NAME,MARK,N32)) THEN
        DO IL = 1,NLTEXT
          READ(NUNIT,*) C
        END DO
        C = C
        GO TO 10
      END IF
      FOUNDM = .TRUE.
!
      RETURN
!
!---- Error in file read.  Marker name not found. ---------------------------
!
 999  WRITE(NERR,100) NAME,MARK
 100  FORMAT(' Error in reading marker information '/  &
             ' Sought: ',a20,'  Last found: ',a20)
!
!----- END OF MODULE MARKIN -------------------------------------------------
!
      END
!
!  ATMRED.FOR  AHAZX                Version Date: 26-Jun-97
!   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE ATMRED                                *
!                                                                            *
!  Subroutine ATMRED reads the temporary atmospheric concentration data file *
!             time for point of interest for one chain and all time points   *
!                                                                            *
!  Written by:       DL Strenge                                              *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    26-Jun-97                                               *
!  Last Modified:    26-Jun-97      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZX
!     Called by: ATMRED
!     Calls: NONE
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     ICM        U     INT     Argument  Position of chain member of interest
!     IX2        U     INT     Argument  Position of data for second co-ordinate
! CATIME(100,20,2) S   REAL    Argument  Concentration time information
!==== Modification History ===================================================
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
! 
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE ATMRED(IX1,IX2,CATIME,XMF)
!
!---- Include Statements ------------------------------------------------------
!
      INCLUDE 'DECAY.FTN'
      INCLUDE 'DEVICE.FTN'
      INCLUDE 'VTIN.FTN'
!
!---- Variable Type Declarations ---------------------------------------------
!
      INTEGER NCM,NAX1,NAX2
      REAL CATIME(1000,20,2)
!      REAL CATIME(100,20,2)
      CHARACTER*12 CNAME
!
!---- Read first record with array dimensions --------------------------------
!
!       ncn = LEN_TRIM(cname)
       READ(NATM,*) CNAME,ICM,NOCHM,NTM,NAX1,NAX2
!1000   FORMAT('"',a,'",',5i5)
!       WRITE(nels,*) cname,icm,nochm,ntm,nax1,nax2
!
!----- Initialize time point array to zero -----------------------------------
!
      DO IN = 1,20
!        DO IT = 1,100
        DO IT = 1,1000
          TIMP(IT,IN) = 0.0
        END DO
      END DO
      !
!---- Loop on number of chain members ----------------------------------------
!
      DO IN = 1,NOCHM
!
!---- Read parent first line
!
       IEP = 1
       IF(IN.GT.1) THEN
         READ(NATM,*) CNAME,ICM,NCM,NTM,NAX1,NAX2
         IEP = ICROSS(ICM)
         NCM = NCM
!         WRITE(nels,*) cname,icm,iep
       END IF
!
!---- Loop on time points in input concentration data ------------------------
!
         DO IT = 1,NTM
!
!---- Read second record with time points ------------------------------------
!
      READ(NATM,1002) ITIN,TIMP(IT,IEP)
!      READ(NATM,1002) ITIN,ttt
 1002 FORMAT(I3,F10.3)
           IF(IT.NE.ITIN) THEN
             WRITE(NERR,2000) IT,IN,CNAME
 2000        FORMAT(' Error reading intermediate file NATM, time ', &
                    'period index bad (',i3,'). '/                  &
                    ' At chain member ',i2,1x,a12)
             GO TO 999
           ENDIF
! Add logic for reading air (DOAIR) and deposition (DODEP)
           IF(DOAIR) THEN
              READ(NATM,1001) NCM,ITIN,((VTIN(1,I1,I2),I2=1,NAX2),I1=1,NAX1)
           ENDIF
 1001       FORMAT(2I3,1P,360(E9.2))
           IF(DODEP) THEN
              READ(NATM,1001) NCM,ITIN,((VTIN(2,I1,I2),I2=1,NAX2),I1=1,NAX1)
           ENDIF
!
!----- Save data for current point, IX1 IX2 -----------------------------------
!
! Add logic for saving air (DOAIR) and deposition (DODEP)
           IF(DOAIR) THEN
              CATIME(IT,IEP,1) = VTIN(1,IX1,IX2) * XMF
           ENDIF
           IF(DODEP) THEN
              CATIME(IT,IEP,2) = VTIN(2,IX1,IX2) * XMF
           ENDIF
!
         END DO   ! end loop on time periods
!
      END DO  ! End loop on chain members
!
 999  RETURN
!
!----- END OF MODULE ATMRED -------------------------------------------------
!
      END
!   MEPAS: EPFHEAD.FOR                 Version Date:  16-Feb-1998
!   Copyright 1998 by Battelle Memorial Institute.  All Rights reserved.
! ****************************************************************************
!
!                            SUBROUTINE EPFHEAD
!
!  Subroutine EPFHEAD writes a head data set to the EPF output file.
!
!  Written by:     DL Strenge
!                  Battelle Pacific Northwest Laboratories
!                  P.O. Box 999
!                  Richland, WA 99352
!
!  Creation Date:  16-Feb-98   by DL Strenge
!  Last Modified:  16-Feb-98   by DL Strenge
!
! ****************************************************************************
!
!===== Modular Organization
!
!   Module of: MEPAS
!   Called by: AHAZX (main program)
!   Calls: None
!
!===== Significant Parameter Designation and Description =====================
!
!  Parameter   Set/
!    Name      Used  Type  Location    Parameter Description
!  ----------  ----  ----  ----------  ---------------------------------------
!  NEXPTYPE     U    Int.  Argument    Number of exposure types for the current
!                                      analysis.
!  EXPTYPE(20)  U    Char  Argument    Name of each exposure transport type
!  EXPSRCNA(20) U    Char  Argument    Name of sources
!  IERR         S    Int.  Argument    Error flag, incremented if a read error
!                                      occurs reading from the SCF file.
!
!===== Modification History ==================================================
!   Date     Who Modification Description
!  --------- --- -------------------------------------------------------------
!
!===== Subroutine Call =======================================================
!
      SUBROUTINE EPFHEAD(NEXPTYPE,EXPTYPE,EXPSRCNA,IERR)
!
!---- Include Statements -----------------------------------------------------
!
      INCLUDE 'DEVICE.FTN'
!
!----- Variable Type Declarations --------------------------------------------
!
      INTEGER NEXPTYPE, IERR
      CHARACTER*14 EXPTYPE(20),AQUIF,SWATR,AIRNM,SOILNM,FOODNM,USERDEF
      CHARACTER*32 EXPSRCNA(20)
      CHARACTER*4 SOIL
      LOGICAL SEQI
!
!----- Data Statements -------------------------------------------------------
!
      DATA AQUIF /'Aquifer       '/
      DATA SWATR /'Surface water '/
      DATA AIRNM /'Air           '/
      DATA SOILNM/'Source        '/
      DATA FOODNM/'Food          '/
      DATA USERDEF/'User Defined  '/
      DATA SOIL/'Soil'/
!
!----- Start of Analysis -----------------------------------------------------
!
!----- Write line of number of data sets to be included in the EPF file ------
!
      WRITE(NEPF,100) NEXPTYPE
 100  FORMAT(I3,',')
!
!----- Write line for each data set to header --------------------------------
!
      DO IK = 1,NEXPTYPE
        NE = LEN_TRIM(EXPSRCNA(IK))
        IF(SEQI(EXPTYPE(IK),AQUIF,14)) THEN
          WRITE(NEPF,200) AQUIF(1:7),EXPSRCNA(IK)(1:NE)
 200      FORMAT('  Medium type - ',A,' Source ',A)
        ELSEIF(SEQI(EXPTYPE(IK),SWATR,14)) THEN
          WRITE(NEPF,200) SWATR(1:13),EXPSRCNA(IK)(1:NE)
        ELSEIF(SEQI(EXPTYPE(IK),AIRNM,14)) THEN
          WRITE(NEPF,200) AIRNM(1:3),EXPSRCNA(IK)(1:NE)
        ELSEIF(SEQI(EXPTYPE(IK),SOILNM,14)) THEN
          WRITE(NEPF,200) SOIL(1:4),EXPSRCNA(IK)(1:NE)
        ELSEIF(SEQI(EXPTYPE(IK),FOODNM,14)) THEN
          WRITE(NEPF,200) FOODNM(1:4),EXPSRCNA(IK)(1:NE)
        ELSEIF(SEQI(EXPTYPE(IK),USERDEF,14)) THEN                 
          WRITE(NEPF,200) FOODNM(1:4),EXPSRCNA(IK)(1:NE)     
        ELSE
          WRITE(NEPF,200) 'Unknown', EXPSRCNA(IK)(1:NE)
        END IF
      END DO  ! End of loop on exposure types
      WRITE(NEPF,300) NEXPTYPE
 300  FORMAT(I3,',')
!
 998  RETURN
!
      END
!
! *** End of Module SCFDAT ***************************************************
!
!
!   AHAZX:  ATODAT.FOR               Version Date:  10-Nov-97
!   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!      SUBROUTINE ATODAT                                                     *
!                                                                            *
!  Subroutine ATODAT reads concentration data from the ATO file.              *
!                                                                            *
!  Written by:       Dl Strenge                                              *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:     9-Jan-97                                               *
!  Last Modified:     10-Nov-97     DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: AHAZX  main program
!     Called by: AHAZX
!     Calls: none
!     Common blocks referenced: DEVICE, EXPLOC, FLUX
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     NATO       U      INT    DEVICE    Logical unit for ATO file
!  RNAMES        U    CHAR*12 Argument   Names of chain members
!  NPRG          U      INT   Argument   Number of progeny in chain
!  NPOINTS      S/U     INT   Argument   Number of location points to consider
!  NTIME        S/U     INT   Argument   Number of time points
!  INTERP       S/U     LOG   Argument   Flag to indicate interpolation necessary
!  IERR         S/U     INT   Argument   Error flag, >0 if errors found
!==== Modification History ===================================================
!  Date      Who  Modification Description
! --------   ---  ------------------------------------------------------
!  
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE ATODAT(RNAMES,NPRG,NPOINTS,NTIMA,INTERP,IERR)
!
!---- Include Statements for Parameter and Common Declarations ---------------
!
      INCLUDE 'CONIN.FTN'
      INCLUDE 'DEVICE.FTN'
      INCLUDE 'PSET1.FTN'
      INCLUDE 'AIRINFO.FTN'
      INCLUDE 'FLUX.FTN'
      INCLUDE 'DECAY.FTN'
      INCLUDE 'VTIN.FTN'
!
!---- Variable Type Declarations ---------------------------------------------
!
      CHARACTER*12 CIDIN, RNAMES(20)
      CHARACTER*10 OU, AU, POLAR
      CHARACTER*3 TEST,TESTN
      CHARACTER*5 WETDRY
      CHARACTER*6 GRID,POINTS
      CHARACTER*20  CH,ONAME,OTYPE
      REAL TY,  VALIN(20)
      REAL (KIND=8), DIMENSION(20) :: VALIND  ! Temporary array for reading atmospheric values
      CHARACTER*8 TU
      CHARACTER*20 CNAMIN
      LOGICAL TSKIP, INTERP, TOTAL, TOTPRIOR,TMSKIP
      REAL (KIND=4) :: SMALL    ! small value allowed for single precision values
!
!---- Data Statements --------------------------------------------------------
!
      DATA GRID /'grid  '/,  POINTS /'points'/
      DATA POLAR/'polar     '/
!
!---- Initialize parameter values --------------------------------------------
!
!---- Upon entry, the file is positioned at the start of data for the --------
!     next constituent parent.
!
       SMALL = TINY(1.0)
!
!---- Initialize inclusion flags for AIR and DEPOSITION
!
      DOAIR = .FALSE.
      DODEP = .FALSE.
!
!------- Initialize arrays for atmospheric concentration parameters ----------
!
        DO IX1 = 1,20
          DO IX2 = 1,36
             DO IN = 1,20
              AIRCIN(IN,IX1,IX2) = 0.0
              DEPTOT(IN,IX1,IX2) = 0.0
            END DO
                DO IOP = 1,2
                    VTIN(IOP,IX1,IX2) = 0.0
                END DO
          END DO
        END DO
!
!-------- Read chronic data set ----------------------------------------------
!
      NTM = NTIMA
      IF(NTIMA.EQ.1) THEN
         INTERP = .FALSE.
         NTM = NTIMA
         NTIMP(1) = NTIMA
         TMSKIP = .FALSE.
      ELSE
         INTERP = .TRUE.
         IF(NTIMA.GT.1000) THEN
           WRITE(NELS,1001) NTIMA
 1001      FORMAT(' In ATO file, number of times is too large:', i3, &
                ' reset to 1000')
            NTM = 1000
            NTIMP(1) = 1000
            TMSKIP = .TRUE.
         END IF
      ENDIF
!
!----- For each time period read time and number of input parameter
!      types included for this contaminant
!
        XFAC = 1.0
        IPP = 1
        DO IT = 1, NTIMA
            READ(NATO,*) TIMIN, TU, NOPS
            TU = TU
!            TSKIP = .FALSE.
            TMSKIP = .FALSE.
            IF(IT.GT.NTM) THEN
              TMSKIP = .TRUE.
            ELSE
              TIMP(IT,1) = TIMIN
            ENDIF
!
!------- Initialize arrays for atmospheric concentration parameters ----------
!
        DO IX1 = 1,20
          DO IX2 = 1,36
             DO IN = 1,20
              AIRCIN(IN,IX1,IX2) = 0.0
              DEPTOT(IN,IX1,IX2) = 0.0
            END DO
                DO IOP = 1,2
                    VTIN(IOP,IX1,IX2) = 0.0
                END DO
          END DO
        END DO
!
!----- For each input parameter type to read
!
            TOTPRIOR = .FALSE.
            DO IO = 1,NOPS
              READ(NATO,*) ONAME,OTYPE,WETDRY,OU,NAX1,CH,NAX2,AU
!
!---- Test input units for /s or /yr and apply correction to get /yr as necessary
!
              XFAC = 1.0
              do I = 1,10
                 IF(ou(I:I).eq.'s') then
                 XFAC = 3.15e7   ! sec/yr
                 ENDIF
              end do
!
!---- Write first line of ATM file for parent (if interpolation necessary) ---
!
              IF(.NOT.TMSKIP) THEN
                IF(INTERP.AND.IO.EQ.1.AND.IT.EQ.1) THEN
                  ncn = LEN_TRIM(rnames(1))
                  write(NATM,1000) rNAMEs(1)(1:ncn),IPP,NPRG+1,NTM,NAX1,NAX2
1000              FORMAT('"',a,'",',5i5)
!                  WRITE(NATM,*)RNAMES(1),IPP,NPRG+1,NTM,NAX1,NAX2
                ENDIF
              ENDIF
!
!----  Set flag for handling of data for this product type -------------------
!
              TSKIP = .FALSE.    ! new position for initialization
              NPROD = 0
              TOTAL = .FALSE.
              TEST = WETDRY(1:3)
!              TESTO = OTYPE(1:3)
              TESTN = ONAME(1:3)
              IF(TESTN.EQ.'Air') THEN     ! Air Concentration (always sum)
                 NPROD = 1
! set flag for inclusion of air concentration in analysis
                 DOAIR = .TRUE.
! end flag set
              ELSEIF(TOTPRIOR) then  ! Deposition Rate, Prior total found, sum new totals
! set flag for inclusion of deposition rate in analysis
                  DODEP = .TRUE.
! end flag set
                  IF(TEST.EQ.'wet'.OR.TEST.EQ.'dry') THEN
                    NPROD = 0         ! Don't include wet/dry if prior total
                  ELSEIF(TEST.EQ.'tot') THEN
                     NPROD = 2         ! Sum new totals
                     TOTAL = .FALSE.
                  ENDIF
              ELSEIF(TEST.EQ.'tot') then  ! First total found, sum values
                  TOTPRIOR = .TRUE.
                  TOTAL = .TRUE.
                  NPROD = 2
! set flag for inclusion of deposition rate in analysis
                  DODEP = .TRUE.
! end flag set
!
! New test to set data when only wet or dry
              ELSE
                  NPROD = 2
! set flag for inclusion of deposition rate in analysis
                  DODEP = .TRUE.
! end flag set
! End new test
!
              ENDIF
!
              IF(NPROD.LE.0) TSKIP = .TRUE.
              IF(RECTYPE.EQ.GRID) THEN          ! For grid input structure
                 NPOINTS = NAX1*NAX2
                 READ(NATO,*) (AUX1(I),I=1,NAX1)
                 DO IX = 1,NAX2
                   ix2 = ix
                   READ(NATO,*) AUX2(IX),(VALIND(I),I=1,NAX1)
!
                   DO IXX = 1,NAX1
                      IF(VALIND(IXX).LT.SMALL) VALIND(IXX)=SMALL
                      VALIN(IXX) = VALIND(IXX)
                   END DO
                   IF(.NOT.TSKIP.AND..NOT.TMSKIP) THEN
                       IF(.NOT.INTERP) THEN
                         CALL AIRSET(NPROD,TOTAL,VALIN,1,NAX1,IX2,xfac)
                       ELSE
                         CALL AIRSAVE(VALIN,NPROD,TOTAL,NAX1,IX2,XFAC)
                       ENDIF
!
!--- If polar grid, calculate cartesian coordinates --------------------------
!
                    DO IX1 = 1,NAX1
                     INDEX = IX1 + (IX2-1)*NAX1
                     IF(CTYPE.EQ.POLAR) THEN  ! For polar convert to x,y
                       CALL GETXY(AUX1(IX1),AUX2(IX2),X,Y)
                       EXPXA(INDEX) = X*0.001
                       EXPYA(INDEX) = Y*0.001
                     ELSE     ! For cartesian use given values
                       EXPXA(INDEX) = AUX1(IX1)*0.001
                       EXPYA(INDEX) = AUX2(IX2)*0.001
                     ENDIF
                    END DO  ! End Do on axis 1
                   ENDIF   ! End if on skipping processing of data
                 END DO
                 IF(NPOINTS.GT.1) THEN
                    CALL XYPOINT(NPOINTS)
                 ELSE
                    NXY = 1
                    ONEPOINT = .TRUE.
                 ENDIF
!
!-------- End of input for "grid" input method -------------------------------
!         If points, read input data
!
              ELSE IF (RECTYPE.EQ.POINTS) THEN   ! For points input structure
                NPOINTS = NAX1
                READ(NATO,*) CH     ! Line with receptor point names
                READ(NATO,*) (AUX1(I),I=1,NAX1) ! Line with point x-axis values
                READ(NATO,*) (AUX2(I),I=1,NAX1) ! Line with point y-axis values
                READ(NATO,*) ICH,(VALIND(I),I=1,NPOINTS)  ! Extra line to skip (-99)
                ICH = ICH
                IX2 = 1
!
! Debug
!!!                WRITE(NELS,*) ONAME,OTYPE,WETDRY,OU,NAX1,CH,NAX2,AU
! End Debug
                   DO IXX = 1,NPOINTS
                      IF(VALIND(IXX).LT.SMALL) VALIND(IXX)=SMALL
                      VALIN(IXX) = VALIND(IXX)
                   END DO

! Debug
!!!                WRITE(NELS,*) ' DOAIR, DODEP ',DOAIR,DODEP
! End Debug
                  IF(.NOT.TSKIP.AND..NOT.TMSKIP) THEN
                       IF(.NOT.INTERP) THEN
                         CALL AIRSET(NPROD,TOTAL,VALIN,1,NAX1,IX2,XFAC)
                       ELSE
                         CALL AIRSAVE(VALIN,NPROD,TOTAL,NAX1,IX2,XFAC)
                       ENDIF
!
!--- Calculate cartesian coordinates -----------------------------------------
!
                   IX2 = 1
                   DO IX1 = 1,NAX1
                     INDEX = IX1 + (IX2-1)*NAX1
                     IF(CTYPE.EQ.POLAR) THEN  ! For polar convert to x,y
                       CALL GETXY(AUX1(IX1),AUX2(IX2),X,Y)
                       EXPXA(INDEX) = X*0.001
                       EXPYA(INDEX) = Y*0.001
                     ELSE     ! For cartesian use given values
                       EXPXA(INDEX) = AUX1(IX1)*0.001
                       EXPYA(INDEX) = AUX2(IX1)*0.001
                     ENDIF
                   END DO
                   IF(NPOINTS.GT.1) THEN
                      CALL XYPOINT(NPOINTS)
                   ELSE
                      NXY = 1
                      ONEPOINT = .TRUE.
                   ENDIF
                 ENDIF   ! End if on skipping processing of data
              END IF
!
            END DO    ! End of do on IO to NOPS
            CH = CH
!
!---- If interpolation is necessary do now for parent ------------------------
!
          IF(INTERP.AND..NOT.TSKIP.AND..NOT.TMSKIP) THEN
            CALL VINTERP(IPP,IT,TIMIN,NAX1,NAX2)
          END IF
          END DO    ! End of do on IT to NTIMA
!
!---------- Repeat for any progeny
!
          IF(NPRG.GT.0) THEN
            DO INP = 1,NPRG
              IPP = INP + 1
!
!------- Initialize arrays for atmospheric concentration parameters, progeny -
!
              DO IX1 = 1,20
                DO IX2 = 1,36
                  DO IOP = 1,2
                    VTIN(IOP,IX1,IX2) = 0.0
                  END DO
                END DO
              END DO
              READ(NATO,*) CONNAM(INP+1),CIDIN,NTMP
              RNAMES(INP+1) = CIDIN
!
!----- Identify current progeny pollutant from air file ----------------------
!
              IPP = INP + 1
              ICROSS(IPP) = 0
              DO IE = 2,NUC
                 IF(CIDIN(1:4).EQ.POLID(1,IE).AND.CIDIN(5:8).EQ.POLID(2,IE)) THEN
                    ICROSS(IPP) = IE
                    IEP = IE
                 END IF
              END DO
              IF(ICROSS(IPP).LE.0) THEN
                 WRITE(NERR,1334) CIDIN
 1334            FORMAT(' Error in progeny name in ATO, not found ',  &
                       'in master list: ',A)
                 ierr = ierr + 1
                 go to 999
              ENDIF
!
              NTIMP(IEP) = NTMP
              IF(NTMP.GT.1000) NTIMP(IEP) = 1000
!              IF(NTMP.GT.10) NTIMP(IEP) = 10
               ntmpw = ntimp(iep)
              DO IT = 1, NTMP
                READ(NATO,*) TIMIN, TU, NOPS
!                TSKIP = .FALSE.
                TMSKIP = .FALSE.
!                IF(IT.GT.NTM) THEN
                IF(IT.GT.1000) THEN
                  TMSKIP = .TRUE.
                ELSE
                  TIMP(IT,IPP) = TIMIN
                ENDIF
                TOTPRIOR = .FALSE.
                DO IO = 1,NOPS
                  READ(NATO,*) ONAME,OTYPE,WETDRY,OU,NAX1,CH,NAX2,AU
!
!---- Write first line of ATM file for progeny (if interpolation necessary) ---
!
                  IF(.NOT.TMSKIP) THEN
                    IF(INTERP.AND.IO.EQ.1.AND.IT.EQ.1) THEN
                      WRITE(NATM,1000)RNAMES(IPP),IPP,IPP,NTMPW,NAX1,NAX2
!                      WRITE(NATM,1000)RNAMES(IPP),IPP,IPP,NTM,NAX1,NAX2
                    ENDIF
                  ENDIF
!
!----  Set flag for handling of data for this product type -------------------
!
                  NPROD = 0
                  TOTAL = .FALSE.
                  TSKIP = .FALSE.      ! new position
                  TEST = WETDRY(1:3)
!                  TESTO = OTYPE(1:3)
                  TESTN = ONAME(1:3)
                  IF(TESTN.EQ.'Air') THEN     ! Air Concentration (always sum)
                     NPROD = 1
                  ELSEIF(TOTPRIOR) then   ! Deposition Rate, Prior total found, sum new totals
                      IF(TEST.EQ.'wet'.OR.TEST.EQ.'dry') THEN
                         NPROD = 0         ! Don't include wet/dry if prior total
                      ELSEIF(TEST.EQ.'tot') THEN
                         NPROD = 2         ! Sum new totals
                         TOTAL = .FALSE.
                      ENDIF
! set flag for inclusion of deposition rate in analysis
                      DODEP = .TRUE.
! end flag set
                  ELSEIF(TEST.EQ.'tot') then  ! First total found, sum values
                      TOTPRIOR = .TRUE.
                      TOTAL = .TRUE.
                      NPROD = 2
! set flag for inclusion of deposition rate in analysis
                      DODEP = .TRUE.
! end flag set
                  ELSE
                      NPROD = 2
! set flag for inclusion of deposition rate in analysis
                      DODEP = .TRUE.
! end flag set
                  ENDIF
!
                  IF(NPROD.LE.0) TSKIP = .TRUE.
!
                    IF(RECTYPE.EQ.GRID) THEN    ! Grid input structure
                      READ(NATO,*) (AUX1(I),I=1,NAX1)
                      DO IX = 1,NAX2
                        IX2 = IX
                        READ(NATO,*) AUX2(IX),(VALIND(I),I=1,NAX1)
!
                        DO IXX = 1,NAX1
                           IF(VALIND(IXX).LT.SMALL) VALIND(IXX)=SMALL
                           VALIN(IXX) = VALIND(IXX)
                        END DO
!
                        IF(.NOT.TSKIP.AND..NOT.TMSKIP) THEN
                           IF(.NOT.INTERP) THEN
                             CALL AIRSET(NPROD,TOTAL,VALIN,IEP,NAX1,IX2,XFAC)
                           ELSE
                             CALL AIRSAVE(VALIN,NPROD,TOTAL,NAX1,IX2,XFAC)
                           ENDIF
                        ENDIF
                      END DO
                    ELSE IF (RECTYPE.EQ.POINTS) THEN  ! Points input structure
                NPOINTS = NAX1
                READ(NATO,*) CH     ! Line with receptor point names
                READ(NATO,*) (AUX1(I),I=1,NAX1) ! Line with point x-axis values
                READ(NATO,*) (AUX2(I),I=1,NAX2) ! Line with point y-axis values
                READ(NATO,*) ICH,(VALIND(I),I=1,NPOINTS)  ! Extra line to skip (-99)
!
                DO IXX = 1,NPOINTS
                   IF(VALIND(IXX).LT.SMALL) VALIND(IXX)=SMALL
                   VALIN(IXX) = VALIND(IXX)
                END DO
!
                IX2 = 1
                  IF(.NOT.TSKIP.AND..NOT.TMSKIP) THEN
                       IF(.NOT.INTERP) THEN
                         CALL AIRSET(NPROD,TOTAL,VALIN,IEP,NAX1,IX2,XFAC)
                       ELSE
                         CALL AIRSAVE(VALIN,NPROD,TOTAL,NAX1,IX2,XFAC)
                       ENDIF
                  ENDIF      ! if not skip
                 ENDIF     ! If rectype = grid/points
                END DO    ! End of do on IO to NOPS
!
!---- If interpolation necessary do now for progeny ---------------------------
!
                IF(INTERP.AND..NOT.TSKIP.AND..NOT.TMSKIP) THEN
                   CALL VINTERP(IPP,IT,TIMIN,NAX1,NAX2)
                END IF
              END DO    ! End of do on IT to NTM
            END DO   ! End of DO on progeny
!
          ENDIF  ! if progeny
!      TY = TY
!      AUX2(1) = AUX2(1)
!      X = X
!      Y = Y
!      AU = AU
!      OU = OU
      NTIMA = NTMP
!      CNAMIN = CNAMIN
!      ONAME = ONAME
999   RETURN
!
!----- END OF MODULE ATODAT -------------------------------------------------
!
      END
!
!    EXPOS: WCFIN.FOR                Version Date: 29-May-97
!   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!      SUBROUTINE WCFIN                                                      *
!                                                                            *
!  Subroutine WCFIN reads concentration data from the WCF file.              *
!                                                                            *
!  Written by:       Dl Strenge                                              *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    20-Dec-96                                               *
!  Last Modified:    29-May-97      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: EXPOS  main program
!     Called by: EXPOS
!     Calls: HEADIN
!     Common blocks referenced: DEVICE, FNAMES
!
!==== Significant Parameter Designation and Description ======================
!
!  Parameter Set/
!  Name      Used   Type    Location  Parameter Description
!  --------- -----  ------  --------- -------------------------------------
!  NWCF       U      INT    DEVICE    Logical unit for WCF file
!  TPATH      U      INT    Argument  Transport pathway index
!  SRCNAME    U    Char*32  Argument  Name of exposure location source to be
!                                     found in WCF data set
!  NUMNUC     S      INT    Argument  Number of constituents in data set
!==== Modification History ===================================================
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!   
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE WCFIN(TPATH,MODNAME,NUMNUC,SRCNAME)
!
!---- Include Statements for Parameter and Common Declarations ---------------
!
      INCLUDE 'DEVICE.FTN'
      INCLUDE 'COUPLE.FTN'
!
!---- Variable Type Declarations ---------------------------------------------
!
      CHARACTER*3 ALLL
      CHARACTER*12 CIDIN,TU,CU
      CHARACTER*32 MEDNAM,MODNAME,SRCNAME
      CHARACTER*14 AQUIF,SWATR,TYPENAM,MEDTYPE
      CHARACTER*20 CNAMIN 
      INTEGER TPATH,TYPELEN
      LOGICAL SKIP   ! skip flag for media in .WCF
      LOGICAL SEQI

!
!---- Data Statements --------------------------------------------------------
!
       DATA AQUIF/'Aquifer       '/
       DATA SWATR/'Surface water '/
       DATA ALLL/'all'/
!---- Initialize parameter values --------------------------------------------
!
!----- Determine if groundwater or surface water medium is sought ------------
!
        IF(TPATH.EQ.1) THEN
          TYPENAM = TRIM(AQUIF)
          TYPELEN = LEN(TRIM(AQUIF))
        ELSE IF(TPATH.EQ.2) THEN
          TYPENAM = TRIM(SWATR)
          TYPELEN=LEN(TRIM(SWATR))
        ELSE
          WRITE(NERR,1114) TPATH
 1114     FORMAT(' Error in WCFIN water type.  Must be 1) Aquifer or',/  &
                 '       2) Surface water, found TPATH = ',I4)
        ENDIF
!
!-----  Read heading information from Water Flux File ------------------------
!
      CALL HEADIN(TPATH,NMEDIA)
!
!----- Loop on the number of media in the WFF file ---------------------------
!
      DO IMED = 1,NMEDIA
        READ(NWCF,*) MEDNAM,MEDTYPE,NCON
        MEDTYPE = MEDTYPE
        SKIP = .TRUE.
        IF(SEQI(MEDNAM,MODNAME,32).OR.SEQI(MEDNAM,ALLL,3)) THEN
          IF(LEN(TRIM(MEDTYPE)).EQ. TYPELEN) THEN
            IF(SEQI(MEDTYPE,TYPENAM, TYPELEN)) SKIP = .FALSE.
          END IF
        END IF
!
        IF(SKIP) THEN
!
!---- read past radionuclide data for this media -----------------------------
!
        DO IC = 1,NCON
!---- Read identification line for constituent ------------------------------
           READ(NWCF,*) CNAMIN,CIDIN,TU,CU,NTIMES,NPG
           CIDIN=CIDIN
           TU = TU
           CU = CU
           CNAMIN = CNAMIN
!----- Read release data for parent constituent -----------------------------
           DO IT = 1,NTIMES
             READ(NWCF,*) T
           END DO
           T = T
!---- If there are progeny, read data for each progeny ----------------------
           IF(NPG.GT.0) THEN
             DO IP = 1,NPG
!
!---- Read identification line for constituent ------------------------------
!
              READ(NWCF,*) CNAMIN
!----- Read release data for progeny constituent ----------------------------
              DO IT = 1,NTIMES
                READ(NWCF,*) T
              END DO
            END DO
          ENDIF
        END DO
        ELSE     !  Found medium, now at start of concentration data
          NUMNUC = NCON
          RETURN  ! ADDED 2-Sept-2004 per previous version
        ENDIF    !  End of loop on skipping concentration data for unused loc.
!
!----- END OF PROCESSING FOR CURRENT MEDIUM SET IN WCF ----------------------
!
        IF(.NOT.SKIP) RETURN
      END DO
!
!----- Error, medium set not found ------------------------------------------
!
        WRITE(NERR,1115) SRCNAME
 1115   FORMAT(' Error in WCFIN, waterborne medium set not found'/  &
               '      for source name ',A32)
        RETURN
!
!----- END OF MODULE WCFIN --------------------------------------------------
!
      END
!
!    AHAZX: ATOIN.FOR                Version Date:  25-Jun-97
!   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!      SUBROUTINE ATOIN                                                      *
!                                                                            *
!  Subroutine ATOIN reads heading  data from the ATO file.                   *
!                                                                            *
!  Written by:       Dl Strenge                                              *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:     8-Jan-97                                               *
!  Last Modified:     25-Jun-97     DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: AHAZX  main program
!     Called by: AHAZX
!     Calls: HEADIN
!     Common blocks referenced: DEVICE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     NATO       U      INT    DEVICE    Logical unit for ATO file
!   SRCNAME      U     Char*32 Argument  Name of source of atmospheric data
!   NUMCON       U      INT    Argument  Number of constituents in data set
!   FOUNDA       S      LOG    Argument  Flag to indicate if data set was found
!==== Modification History ===================================================
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE ATOIN(SRCNAME,NUMCON,FOUNDA)
!
!---- Include Statements for Parameter and Common Declarations ---------------
!
      INCLUDE 'DEVICE.FTN'
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'FLUX.FTN'
!
!---- Variable Type Declarations ---------------------------------------------
!
      CHARACTER*12 CIDIN
      CHARACTER*32 SRCNAME, SETNAM(10)
      CHARACTER*8 ACUTE, CHRONIC
      CHARACTER*6 GRID,POINTS
      CHARACTER*20  CH
      REAL TY
      CHARACTER*8 TU
      CHARACTER*20 CNAMIN
      INTEGER NSETS, NFLUX, NUMCON, TPATH
      LOGICAL FOUNDA ! skip flag for media in .ATO
      LOGICAL SEQI
!
!---- Data Statements --------------------------------------------------------
!
      DATA ACUTE/'acute   '/,CHRONIC/'chronic '/
      DATA GRID /'grid  '/,  POINTS /'points'/
!
!---- Initialize parameter values --------------------------------------------
!
      NUMCON = 0
      FOUNDA = .FALSE.
      TPATH = 3
!
!-----  Read heading information from air concentration file -----------------
!
      CALL HEADIN(TPATH,NSETS)
!
!----- Loop on the number of media in the ATO file ---------------------------
!
      FOUNDA = .FALSE.
      DO ISET = 1,NSETS
!
        READ(NATO,*) NFLUX,SETNAM(ISET)
!
!----- Test set name against desired set name (SRCNAME input agreement) -----
!
        IF(SEQI(SETNAM(ISET),SRCNAME,32)) THEN
          FOUNDA = .TRUE.             ! Found set.  read heading parameters
!
!------ Read flux type descriptive parameters --------------------------------
!
          DO IX = 1,NFLUX
            READ(NATO,*) FLXNAM(IX),PRADUS(IX),PRADU(IX)
          END DO
!
!------ Read data set type descriptive parameters ----------------------------
!
          READ(NATO,*) ATYPE,CTYPE,RECTYPE,NUMCON
!
!-------- File is now positioned at the start of the first constituent -------
!         Return to calling program.  Data will be read by ATODAT subroutine
!
!          IF(ATYPE.NE.CHRONIC) THEN
         IF(.NOT.SEQI(ATYPE,CHRONIC,7)) THEN        ! Updated 22 Aug 05 for cases insensitive testing.
            WRITE(NERR,1004) ATYPE
 1004       FORMAT(' Error in ATO data set type.  Must be chronic.'/ &
                   '    Found: ',A10)
          ENDIF
          RETURN
        ELSE  ! Not desired data set.  Read past the set and search for others
          FOUNDA = .FALSE.
          IF(ISET.EQ.NSETS) THEN
            WRITE(NERR,1000) SRCNAME
 1000       FORMAT(' Error reading ATO file.  Data set not found ',A32/  &
                   '   The following sets were found:')
            NERROR = NERROR + 1
            DO IS = 1,NSETS
              WRITE(NERR,1002) IS,SETNAM(IS)
 1002       FORMAT('   Set number',i2,' name is ',a32)
            END DO
            RETURN
          ENDIF
!
!------ Skip past this data set and position to top of next ------------------
!
!------ Skip flux type descriptive parameters --------------------------------
!
          DO IX = 1,NFLUX
            READ(NATO,*) FLXNAM(IX),PRADUS(IX),PRADU(IX)
          END DO
!
!------ Read data set type descriptive parameters ----------------------------
!
          READ(NATO,*) ATYPE,CTYPE,RECTYPE,NUMCON
!
!-------- Test data set type for "acute" or "chronic" for skipping data -------
!
          IF(SEQI(ATYPE,ACUTE,5)) THEN              ! Updated 22 Aug 05 for cases insensitive testing.
!
!-------- Skip past acute data set -------------------------------------------
!
            DO IN = 1,NUMCON
              READ(NATO,*) CNAMIN,CIDIN,NTM,NPRG
              DO IT = 1, NTM
                READ(NATO,*) TY, TU, NOPS
                DO IO = 1,NOPS
                  READ(NATO,*) CH,CH,CH,CH,NAX1,CH,NAX2,CH
                  IF(RECTYPE.EQ.GRID) THEN
                     READ(NATO,*) X
                     DO IX = 1,NAX2
                       READ(NATO,*) Y
                     END DO
                  ELSE IF (RECTYPE.EQ.POINTS) THEN
                     READ(NATO,*) CH   ! Line with receptor point names
                     READ(NATO,*) X    ! Line with point x-axis values
                     READ(NATO,*) Y    ! Line with point y-axis values
                     READ(NATO,*) X    ! Extra line to skip (-99)
                  ENDIF
                END DO
              END DO
!
!---- Skip acute data for progeny, if any ------------------------------------
!
              IF(NPRG.GT.0) THEN
                DO INP = 1,NPRG
                  READ(NATO,*) CNAMIN,CIDIN,NTM
                  DO IT = 1, NTM
                    READ(NATO,*) TY, TU, NOPS
                    DO IO = 1,NOPS
                      READ(NATO,*) CH,CH,CH,CH,NAX1,CH,NAX2
                       IF(RECTYPE.EQ.GRID) THEN
                          READ(NATO,*) X
                          DO IX = 1,NAX2
                            READ(NATO,*) Y
                          END DO
                       ELSE IF (RECTYPE.EQ.POINTS) THEN
                         READ(NATO,*) CH   ! Line with receptor point names
                         READ(NATO,*) X    ! Line with point x-axis  values
                         READ(NATO,*) Y    ! Line with point y-axis values
                         READ(NATO,*) X    ! Extra line to skip (-99)
                       END IF
                    END DO    ! End of do on IO to NOPS
                  END DO    ! End of do on IT to NTM
                END DO   ! End of DO on progeny
              ENDIF
            END DO    ! End of do on IN to NUMCON
          ELSEIF(SEQI(ATYPE,CHRONIC,7)) THEN       ! Updated 22 Aug 05 for cases insensitive testing.
!
!-------- Skip past chronic data set -----------------------------------------
!
            DO IN = 1,NUMCON
              READ(NATO,*) CNAMIN,CIDIN,NTM,NPRG
              CIDIN = CIDIN
              CNAMIN = CNAMIN
              DO IT = 1, NTM
                READ(NATO,*) TY, TU, NOPS
                TY = TY
                TU = TU
                DO IO = 1,NOPS
                  READ(NATO,*) CH,CH,CH,CH,NAX1,CH,NAX2
                  IF(RECTYPE.EQ.GRID) THEN
                     READ(NATO,*) X
                     X = X
                     DO IX = 1,NAX2
                       READ(NATO,*) Y
                     END DO
                     Y = Y
                  ELSE IF (RECTYPE.EQ.POINTS) THEN
                     READ(NATO,*) CH   ! Line with receptor point names
                     READ(NATO,*) X    ! Line with point x-axis values
                     READ(NATO,*) Y    ! Line with point y-axis values
                     READ(NATO,*) X    ! Extra line to skip (-99)
                  ELSE    ! Error in RECTYPE
                    WRITE(NERR,1003) RECTYPE
                    NERROR = NERROR + 1
 1003       FORMAT(' Error in ATO file grid/points specification: ',A20)
                    RETURN
                  END IF
                END DO    ! End of do on IO to NOPS
                CH = CH
                NAX1 = NAX1
              END DO    ! End of do on IT to NTM
!
!---------- Repeat for any progeny
!
              IF(NPRG.GT.0) THEN
                DO INP = 1,NPRG
                  READ(NATO,*) CNAMIN,CIDIN,NTM
                  DO IT = 1, NTM
                    READ(NATO,*) TY, TU, NOPS
                    DO IO = 1,NOPS
                      READ(NATO,*) CH,CH,CH,CH,NAX1,CH,NAX2
                       IF(RECTYPE.EQ.GRID) THEN
                          READ(NATO,*) X
                          DO IX = 1,NAX2
                            READ(NATO,*) Y
                          END DO
                       ELSE IF (RECTYPE.EQ.POINTS) THEN
                         READ(NATO,*) CH   ! Line with receptor point names
                         READ(NATO,*) X    ! Line with point x-axis  values
                         READ(NATO,*) Y    ! Line with point y-axis values
                         READ(NATO,*) X    ! Extra line to skip (-99)
                       END IF
                    END DO    ! End of do on IO to NOPS
                  END DO    ! End of do on IT to NTM
                END DO   ! End of DO on progeny
              ENDIF
            END DO    ! End of do on IN to NUMCON
          ELSE
            WRITE(NERR,1001) ATYPE
            NERROR = NERROR + 1
 1001       FORMAT(' Error in ATO file type specification: ',a32/  &
                   '     Should be "acute" or "chronic".')
            RETURN
          ENDIF
        ENDIF
!
      END DO        ! End of DO Loop on data sets, ISET to NSET

        RETURN
!
!----- END OF MODULE ATOIN --------------------------------------------------
!
      END
!
!   MOVESET.FOR                          Version Date: 14-May-97
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                 LOGICAL FUNCTION MOVESET                                   *
!                                                                            *
!  Subroutine MOVESET finds the location of the sought set of parameters in  *
!  the .GID file.                                                            *
!                                                                            *
!  Written by:       Karl Castleton (as function MOVETO)                     *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    16-Nov-93                                               *
!  Last Modified:    14-May-97      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: GENERAL USAGE
!     Called by: SUBROUTINE GETSET
!     Calls: SEQI
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!    NGID        U      INT    Argument  Logical unit of the .GID file
!    NAME        U      CHAR   Argument  Name of SET sought
!    NLINES      U      INT    Argument  Number of lines in set sought
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     
!==== SUBROUTINE CALL ========================================================
!
      LOGICAL FUNCTION MOVESET(NGID,NAME,NLINES)
!
!---- Variable type declarations ---------------------------------------------
!
      INTEGER NGID,NLINES
      CHARACTER*(*) NAME
      LOGICAL SEQI
      CHARACTER TNAME*32
      DATA N14/14/
!
!---- Start of analysis ------------------------------------------------------
!
      TNAME=' '
!
!---- Rewind .GID file to top ------------------------------------------------
!
      REWIND(NGID)
      MOVESET=.FALSE.
!
!---- Read a line from the .GID file (name, index values, and data -----------
!
 300  READ (NGID,*,ERR=303,END=303) TNAME,NLINES
!
!---- Test name and index values against values sought -----------------------
!
      IF (.NOT. SEQI(TNAME,NAME,N14))  THEN
        DO IL = 1,NLINES                        ! New do loop to read
          READ(NGID,*,ERR=303,END=303) TNAME    ! NLINES skipping past
        END DO                                  ! unneeded data set
        GO TO 300
      ELSE !
        MOVESET=.TRUE.
      END IF
 303  END
!
!--------- END OF MODULE MOVESET ---------------------------------------------
!   GETREAL.FOR                        Version Date: 28-Oct-96
!   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE GETREAL                               *
!                                                                            *
!  Subroutine GETREAL   extracts a real value from the SETDATA array         *
!   from the .GID file                                                       *
!                                                                            *
!  Written by:       Karl Castleton                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    16-Nov-93                                               *
!  Last Modified:    28-Oct-96 DLS for Framework Application                 *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: GENERAL USE
!     Called by: CALLING PROGRAM
!     Calls: SUBROUTINE MOVETO
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!    SETDATA     U      CHAR   Argument  Input data array from .GID file
!    ***THE DATA SET MUST ALREADY BE READ FROM GID FILE***
!    NLINES      U      INT    Argument  Number of lines used in SETDATA
!    NAME        U      CHAR   Argument  Name of parameter in .GID file
!    C1          U      INT    Argument  CONSTITUENT COUNTER
!    C2          U      INT    Argument  MEDIA COUNTER FOR GROUND WATER PSZ AND SZ
!    C3          U      INT    Argument  LOCATION COUNTER
!    C4          U      INT    Argument  FLUX COUNTER
!    C5          U      INT    Argument  MONTH COUNTER
!    C6          U      INT    Argument  MISCELANEOUS COUNTER
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     
!==== SUBROUTINE CALL ========================================================
!
      REAL FUNCTION GETREAL(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,C6)
!
!---- Variable Type Declarations ---------------------------------------------
!
      include 'parmtr.par'
      INTEGER C1,C2,C3,C4,C5,C6,NLINES,LINE
      CHARACTER*(*) NAME
      CHARACTER*(*) SETDATA(linemax)
      LOGICAL MOVETO
      INTEGER T1,T2,T3,T4,T5,T6,REF
      CHARACTER UNITS*8,USUNTS*8,TNAME*14
      REAL VALUE
!
!---- Start of analysis
!     Logical function MOVETO finds the location of the record in the
!     parameter file.  If found, MOVETO = .TRUE., otherwise = .FALSE.
!
      IF (MOVETO(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,C6,LINE)) THEN
        READ(SETDATA(LINE),*) TNAME,T1,T2,T3,T4,T5,T6,REF,USUNTS,UNITS,VALUE
        GETREAL=VALUE
      ELSE
        GETREAL=0.0
      END IF
      END

 
!   GETSTR.FOR                         Version Date: 29-Oct-96
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE GETSTR                                *
!                                                                            *
!  Subroutine GETSTR extracts a string parameter value from the .GID file.   *
!                                                                            *
!  Written by:       Karl Castleon                                           *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    16-Nov-95                                               *
!  Last Modified:    29-Oct-96      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: GENERAL USE
!     Called by:
!     Calls: MOVETO, SEQ
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!    SETDATA     U      CHAR   Argument  Array of line images to search for an
!                                        integer parameter
!    NAME        U      CHAR   Argument  Name of parameter sought.
!    C1          U      INT    Argument  CONSTITUENT COUNTER
!    C2          U      INT    Argument  MEDIA COUNTER FOR GROUND WATER PSZ AND SZ
!    C3          U      INT    Argument  LOCATION COUNTER
!    C4          U      INT    Argument  FLUX COUNTER
!    C5          U      INT    Argument  MONTH COUNTER
!    C6          U      INT    Argument  MISCELANEOUS COUNTER
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     06-Dec-95    DLS  Added heading information
!     29-Oct-96    DLS  Converted for use with the Framework program
!==== SUBROUTINE CALL ========================================================
!
      CHARACTER*80 FUNCTION GETSTR(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,C6)
!
!---- Variable Type Declarations ---------------------------------------------
!
      include 'parmtr.par'
      INTEGER C1,C2,C3,C4,C5,C6,NLINES,LINE
      CHARACTER(*) NAME
      CHARACTER*(*) SETDATA(linemax)
      LOGICAL MOVETO
      INTEGER T1,T2,T3,T4,T5,T6,REF
      CHARACTER USUNTS*8,UNITS*8,TNAME*14,VALUE*80
!
!---- Start of analyses ------------------------------------------------------
!
      IF (MOVETO(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,C6,LINE)) THEN
        READ(SETDATA(LINE),*) TNAME,T1,T2,T3,T4,T5,T6,REF,USUNTS,UNITS,VALUE
        GETSTR=VALUE
      ELSE
        GETSTR='NOT FOUND'
      END IF
      END
!   GETLOG.FOR                       Version Date: 12-Nov-96
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE GETLOG                                *
!                                                                            *
!  Subroutine GETLOG extracts a logical  value from the .GID file            *
!                                                                            *
!  Written by:       Karl Castleton/DL Strenge                               *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    16-Nov-93                                               *
!  Last Modified:    12-Nov-96 DLS Generated from GETINT                     *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: general
!     Called by:
!     Calls: MOVETO
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     SETDATA    U      CHAR   Argument  Array of line images for extraction
!                                        of a logical parameter
!     ***THE ARRAY MUST ALREADY BE FILED***
!     NAME       U      CHAR   Argument  Name of parameter sought
!     C1         U      INT    Argument  first counter
!     C2         U      INT    Argument  second counter
!     C3         U      INT    Argument  third counter
!     C4         U      INT    Argument  fourth counter
!     C5         U      INT    Argument  fifth counter
!     C6         U      INT    Argument  sixth counter
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     
!==== SUBROUTINE CALL ========================================================
!
      LOGICAL FUNCTION GETLOG(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,C6)
!
!---- Variable Type Declarations ---------------------------------------------
!
      include 'parmtr.par'
      INTEGER C1,C2,C3,C4,C5,C6,NLINES
      CHARACTER*(*) NAME
      CHARACTER*(*) SETDATA(linemax)
      LOGICAL MOVETO
      INTEGER T1,T2,T3,T4,T5,T6,REF
      CHARACTER UNITS*8,USUNTS*8,TNAME*14
      LOGICAL VALUE
!
!---- Start of Analysis
!    MOVETO finds the record having the desired parameter value
!
      IF (MOVETO(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,C6,LINE)) THEN
        READ(SETDATA(LINE),*) TNAME,T1,T2,T3,T4,T5,T6,REF,USUNTS,UNITS,VALUE
!
        GETLOG=VALUE
      ELSE
        GETLOG=.FALSE.
      END IF
      END
!
!   MOVETO.FOR                              Version Date: 28-Oct-96
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE MOVETO                                *
!                                                                            *
!  Subroutine MOVETO finds the location of the sought parameter in the .GID  *
!                 data set.                                                  *
! THIS ROUTINE MOVES TO THE ARRAY POSITION YOU ARE LOOKING                   *
! A FUNCTION VALUE OF .TRUE. IF THE RECORD IS .FOUND. FALSE IF               *
! IT IS NOT                                                                  *
!                                                                            *
!  Written by:       Karl Castleton                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    16-Nov-93                                               *
!  Last Modified:    28-Oct-96      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: GENERAL
!     Called by: SUBROUTINE GETINT, GETSTR, GETREALNC
!     Calls: SEQ
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!    SETDATA     U      CHAR   Argument  Data set array to search
!    NLINES      U      INT    Argument  Number of lines in data set used
!    NAME        U      CHAR   Argument  Name of the parameter sought
!    C1          U      INT    Argument  CONSTITUENT COUNTER
!    C2          U      INT    Argument  MEDIA COUNTER FOR GROUND WATER PSZ AND SZ
!    C3          U      INT    Argument  LOCATION COUNTER
!    C4          U      INT    Argument  FLUX COUNTER
!    C5          U      INT    Argument  MONTH COUNTER
!    C6          U      INT    Argument  MISCELANEOUS COUNTER
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     
!==== SUBROUTINE CALL ========================================================
!
         LOGICAL FUNCTION MOVETO(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,C6,line)
!
!---- Variable type declarations ---------------------------------------------
!
      include 'parmtr.par'
      INTEGER C1,C2,C3,C4,C5,C6
      CHARACTER*(*) NAME
      CHARACTER*(*) SETDATA(linemax)
      LOGICAL SEQI
      INTEGER T1,T2,T3,T4,T5,T6,Iok
      CHARACTER TNAME*14
      DATA N14/14/
!
!---- Start of analysis ------------------------------------------------------
!
      LINE = 0
      TNAME=' '
      T1=0
      T2=0
      T3=0
      T4=0
      T5=0
      T6=0
!
!---- Initialize output ------------------------------------------------------
!
      ILN = 0
      MOVETO=.FALSE.
!
!---- Extract a line from the SETDATA array (name, index values, and data ----
!
 300  ILN = ILN + 1
      IF(ILN.GT.NLINES) GO TO 303
      READ (SETDATA(ILN),*,ERR=303,END=303) TNAME,T1,T2,T3,T4,T5,T6
!
!---- Test name and index values against values sought -----------------------
!
!      IF ((.NOT. SEQI(TNAME,NAME,N14) ) .OR. C1 .NE.T1 .OR. C2 .NE. T2 &
!        .OR. C3 .NE. T3 .OR. C4 .NE. T4 .OR. C5 .NE. T5                &
!        .OR. C6 .NE. T6) THEN ! jgd
      Iok=0
      IF ((.NOT. SEQI(TNAME,NAME,N14))) Iok=Iok+1
      IF (C1 .NE. T1) Iok=Iok+1
      IF (C2 .NE. T2) Iok=Iok+1
      IF (C3 .NE. T3) Iok=Iok+1 
      IF (C4 .NE. T4) Iok=Iok+1 
      IF (C5 .NE. T5) Iok=Iok+1 
      IF (C6 .NE. T6) Iok=Iok+1
      If (Iok.gt.0) THEN
        GO TO 300
      ELSE !
        LINE = ILN
        MOVETO=.TRUE.
      END IF
 303  END
!    EXPOS:HEADIN.FOR                      Version Date: 15-May-97
!   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                     SUBROUTINE HEADIN                                      *
!                                                                            *
!  Program HEADIN controls reading of heading information from the water     *
!  concentration file (WCF).                                                 *
!                                                                            *
!  Written by:       Dl Strenge                                              *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    20-Dec-96                                               *
!  Last Modified:    15-May-97      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: GENII/EXPOS
!     Called by: EXPOS
!     Calls: NONE
!     Common blocks referenced: EXINFO, DEVICE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!
!==== Modification History ===================================================
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!   
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE HEADIN(TPATH,NMEDIA)
!
!---- Include Statements for Parameter and Common Declarations ---------------
!
      INCLUDE 'DEVICE.FTN'
!
!---- Variable Type Declarations ---------------------------------------------
!
      CHARACTER*80 TEXT
      INTEGER TPATH
!
!---- Data Statements --------------------------------------------------------
!
!---- Set input unit for reading ---------------------------------------------
!
      NUNIT = NWCF
      IF(TPATH.EQ.3.OR.TPATH.EQ.4) NUNIT = NATO
      IF(TPATH.EQ.5) NUNIT = NSCF
!
!---- Read heading text ------------------------------------------------------
!
      READ(NUNIT,*) NTEXT       ! read number of lines of text to read
      IF(NTEXT.GT.0) THEN
        DO IL = 1,NTEXT
          READ(NUNIT,*) TEXT    ! Read a line of text
          WRITE(NELS,101) TEXT   ! Write a line of text to output listing file
 101      FORMAT(1X,A80)
        END DO
      ENDIF
!
!----- Read line with number of media ---------------------------------------
!
      READ(NUNIT,*) NMEDIA
!
      RETURN
!
!----- END OF MODULE HEADIN -------------------------------------------------
!
      END
!
!    EXPOS: GETXY.FOR                Version Date:  7-May-97
!   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!      SUBROUTINE GETXY                                                      *
!                                                                            *
!  Subroutine GETXY converts polar coordinate (R,THETA) to Cartesian (X,Y)   *
!                                                                            *
!  Written by:       Dl Strenge                                              *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:     7-May-97                                               *
!  Last Modified:     7-May-97      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: EXPOS  main program
!     Called by: EXPOS
!     Calls: none
!     Common blocks referenced: none
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     R          U     Real    Arg.      Radius of polar coordinate
!     THETA      U     Real    Arg.      Angle o polar coordinate
!     X          S     Real    Arg.      X-Axis distance of cartesian coord.
!     Y          S     Real    Arg.      Y-Axis distance of cartesian coord.
!==== Modification History ===================================================
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     
!
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE GETXY(R,THETA,X,Y)
!
!---- Include Statements for Parameter and Common Declarations ---------------
!      (None)
!
!---- Variable Type Declarations ---------------------------------------------
!
      REAL R,THETA,X,Y
!
!---- Data Statements --------------------------------------------------------
!
      DATA PI/3.14159265/
!
!---- Polar axis for Theta = 0 is the positive X-axis -------------------------
!
!    X value is radius * cosine(theta)
!    Y value is radius * sine(theta)
!    Theta in degrees is converted to radians by multiplying by 2 pi / 360
!
       TH = 450. - THETA
       IF(TH.GE.360.) TH = TH - 360.
       T = TH * 2 * PI / 360.
       X = R*COS(T)
       Y = R*SIN(T)
       IF(ABS(Y).LT.0.01) Y = 0.
       IF(ABS(X).LT.0.01) X = 0.
!
        RETURN
!
!----- END OF MODULE GETXY --------------------------------------------------
!
      END
!
!   SEQI.FOR                         Version Date: 14-Aug-97
!   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                     LOGICAL FUNCTION SEQI                                  *
!
!  Subroutine to compare two strings in case-insensitive mode
!                                                                            *
!  Written by:       Dl Strenge                                              *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    14-Aug-97                                               *
!  Last Modified:    14-Aug-97      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZ
!     Called by:
!     Calls: NONE
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     A          U      CHAR   Argument  First word to be compared
!     B          U      CHAR   Argument  Second word to be compared
!     NCAR       U      INT    Argument  Number of characters to be tested
!==== Modification History ===================================================
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!  
!==== SUBROUTINE CALL ========================================================
!
      LOGICAL FUNCTION SEQI(A,B,NCAR)
!
!---- Variable Type Declarations ---------------------------------------------
!
      CHARACTER*(*) A,B
      CHARACTER*1 AC,BC
      INTEGER NCAR
      LOGICAL SEQ
!
!---- Loop on number of characters to be compared.  Stop checking when -------
!     two characters do not agree (when both are expressed as capitals
!
      SEQ = .FALSE.
      DO IC = 1,NCAR
        AC = A(IC:IC)
        BC = B(IC:IC)
        IF(AC.GE.'a'.AND.AC.LE.'z') AC = CHAR(ICHAR(AC) - 32)
        IF(BC.GE.'a'.AND.BC.LE.'z') BC = CHAR(ICHAR(BC) - 32)
        IF(AC.NE.BC) GO TO 100
      END DO
!
!----- If control reaches this point then no differences were detected in ----
!      the two words.  Return a 'true' value for the function.
!
      SEQ = .TRUE.
 100  SEQI = SEQ
      RETURN
!
!----- END OF MODULE SEQI ----------------------------------------------------
!
      END
!
!    EXPOS: XYPOINT.FOR              Version Date:  10-Sep-97
!   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!      SUBROUTINE XYPOINT                                                    *
!                                                                            *
!  Subroutine XYPOINT finds the nearest point to a set or (X,Y) values       *
!  The subroutine returns the number of the closest point.                   *
!  Written by:       Dl Strenge                                              *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    10-Sep-97                                               *
!  Last Modified:    10-Sep-97      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: EXPOS  main program
!     Called by: EXPOS
!     Calls: none
!     Common blocks referenced: none
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!  EXPXA(360)    U     Real   AIRINFO    Array of x-axis points
!  EXPYA(360)    U     Real   AIRINFO    Array of y-axis points
!  NPOINTS       U     Int     Arg       Number of x,y data pairs to evaluate
!  NXY           S     Int     Arg       Number of the nearest data pair.
!     X          S     Real    Arg.      X-Axis distance of cartesian coord.
!     Y          S     Real    Arg.      Y-Axis distance of cartesian coord.
!==== Modification History ===================================================
!    Date     Who  Modification Description
!  ---------  ---  ------------------------------------------------------
!  10-Sep-97  DLS  Initial programming started
!
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE XYPOINT(NPOINTS)
!
!---- Include Statements for Parameter and Common Declarations ---------------
!      (None)
      INCLUDE 'AIRINFO.FTN'
      INCLUDE 'FLUX.FTN'
!
!---- Variable Type Declarations ---------------------------------------------
!
      REAL DIST
      INTEGER NPOINTS
!
!---- Start of Analysis ------------------------------------------------------
!
      X = EXPX 
      Y = EXPY
      ONEPOINT = .TRUE.
      DIST = 1000000.
      NXY = 1  ! Default is first location
      IF(X.EQ.0..AND.Y.EQ.0.) GO TO 100
      IF(NPOINTS.GT.1) THEN
         DO IP = 1,NPOINTS
            XX = (EXPXA(IP)-X)**2
            YY = (EXPYA(IP)-Y)**2
            DD = SQRT(XX + YY)
            IF(DD.LT.DIST) THEN
               DIST = DD
               NXY = IP
            ENDIF
         END DO
      END IF
      RETURN
!
 100  ONEPOINT = .FALSE.
      RETURN
!
!----- END OF MODULE XYPOINTS -----------------------------------------------
!
      END
!
      SUBROUTINE PRTERR( IERR, CALLER, MLINES )
!!************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine prints a standard error message to the report file.
!!
!!  Call List Variables:
!!
!!    Variable Type         Description
!!    -------- -----------  ---------------------------------------
!!    IERR     (Integer)    Error number from the calling routine
!!    CALLER   (Character)  Name of the calling routine
!!    MLINES   (Integer)    Number of lines in the error message
!!
!!  Module Variables:
!!
!!    Variable Type         Description
!!    -------- -----------  ---------------------------------------
!!    IRPT_ERR (Integer)    Error number from the calling routine
!!    MESSAG   (Character)  Message vector
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!
!!************************************************************************
!
! *** Global variables
!
      USE Errors_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: IERR
      INTEGER, INTENT(IN) :: MLINES
      CHARACTER(LEN=*), INTENT(IN) :: CALLER
!
! *** Local variables
!
      INTEGER :: I ! Local looping control variable
!
!---- Executable code ---------------------------------------------------
!
! *** Write out the error number and the calling routine
!
      IF( IERR .EQ. 0 ) THEN
        WRITE(IRPT,1000) TRIM(CALLER)
 1000   FORMAT(/'Message originating in routine ',A)
      ELSE
        WRITE(IRPT,1010) IERR, TRIM(CALLER)
 1010   FORMAT(/'Error number ',I4,' encountered in routine ',A)
      END IF
!
! *** Write out the first line of the error message
!
      IF( MLINES .GT. 0 ) THEN
        WRITE(IRPT,1020) TRIM( MESSAG(1) )
 1020   FORMAT('Message: ',A)
      END IF
!
! *** Write out any trailing lines for the message
!
      IF( MLINES .GT. 1 ) THEN
        DO I = 2, MIN(MLINES,MAXMES)
          WRITE(IRPT,1030) TRIM( MESSAG(I) )
 1030   FORMAT(9X,A)
        END DO
      END IF
!
      RETURN
      END SUBROUTINE



! ****************************************************************************
!                                                                            *
!                     SUBROUTINE IdMarker                                    *
!                                                                            *
!  Program MARKIN controls finding of the correct marker in the EPF file.    *
!                                                                            *
!  Written by:       Dl Strenge                                              *
!  Modified by:      MA Pelton                                               *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    20-Dec-96                                               *
!  Last Modified:    18-Jun-97      DLS                                      *
!                                                                            *
! ****************************************************************************
!     Calls: NONE
!     Common blocks referenced: DEVICE
!
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE IDMARKER(NEXPTYPE,TPATH,MNAME,EXPNAM,EXPSRCNA,EXPTYPE)
!
!---- Include Statements for Parameter and Common Declarations ---------------
!
      INCLUDE 'DEVICE.FTN'
!
!---- Variable Type Declarations ---------------------------------------------
!
      CHARACTER*50 C,lintxt
      CHARACTER*30 C1,C2,C3,C4
      CHARACTER*32 MARK,MNAME,FQUAL,FNAME,EXPNAM
      CHARACTER*32 EXPSRCNA(20),NAME
      CHARACTER*14 EXPTYPE(20),QUAL
      LOGICAL FOUNDM, SEQI
      LOGICAL MATCHED
      INTEGER TPATH,NLTEXT,PROG,NCON,N32
!
!---- Data Statements --------------------------------------------------------
!
      DATA N32/32/
!
!---- Set input unit for reading ---------------------------------------------
      write(nels,*) 'IDMARKER Call <<<<<<<<<<<<<<<<<<<<<<<<<<<<'
!
      IF(TPATH.LE.2) NUNIT = NWCF  !idmarker
      IF(TPATH.EQ.3.OR.TPATH.EQ.4) NUNIT = NATO
      IF(TPATH.EQ.5) NUNIT = NSCF
      FOUNDM = .FALSE.
      REWIND(NUNIT)
!!!      WRITE(NELS,*) ' Call to idmarker, nexptype = ',nexptype, ' tpath = ',tpath
!!!      WRITE(nels,*) ' MNAME = ',mname,' expnam = ',trim(expnam)
!
!---- Read a marker line ----------------------------------------------------
!
 10   READ(NUNIT,*,ERR=999,END=998) MARK,NLTEXT  ! Read a marker and number of lines
      IF(.NOT.SEQI(MNAME,MARK,N32)) THEN
        DO IL = 1,NLTEXT
          READ(NUNIT,*) C
        END DO
        GOTO 10
      ELSE    
        IF(TPATH.EQ.3) THEN
          NEXPTYPE = NEXPTYPE + 1
          EXPSRCNA(NEXPTYPE) = MNAME
          EXPTYPE(NEXPTYPE) = 'AIR'
        ELSE
          READ(NUNIT,*) NLTEXT
          DO IL = 1,NLTEXT
            READ(NUNIT,*) C
            WRITE(NELS,*) C
          END DO
          READ(NUNIT,*) NLTEXT
          DO IX = 1,NLTEXT
            MATCHED = .false.
            IF(TPATH.EQ.1) THEN
              READ(NUNIT,*) FNAME, FQUAL, NCON
              IF(LEN(TRIM(FQUAL)).EQ.7)THEN
                IF (SEQI('AQUIFER',FQUAL,7)) THEN
                  MATCHED = .true.
                END IF  
              END IF  
            ENDIF
            IF(TPATH.EQ.2) THEN
              READ(NUNIT,*) FNAME, FQUAL, NCON
              IF(LEN(TRIM(FQUAL)).EQ.13)THEN
                IF (SEQI('SURFACE WATER',FQUAL,13)) THEN
                  MATCHED = .true.
                END IF  
              END IF  
            ENDIF
            IF(TPATH.EQ.5) THEN
               READ(NUNIT,*) FNAME, FQUAL, C, C, C, C, C, C, NCON
               IF(LEN(TRIM(FQUAL)).EQ.4)THEN
                 IF (SEQI('SOIL',FQUAL,4))  THEN
                   MATCHED = .true.
                 END IF  
               END IF  
            ENDIF
            IF (SEQI("ALL",FNAME,3) .AND. MATCHED) THEN
              NEXPTYPE = NEXPTYPE + 1
              EXPSRCNA(NEXPTYPE) = MNAME
              EXPTYPE(NEXPTYPE) = FQUAL
            ELSE
              IF (SEQI(EXPNAM,FNAME,N32) .AND. MATCHED) THEN
                NEXPTYPE = NEXPTYPE + 1
                EXPSRCNA(NEXPTYPE) = MNAME
                EXPTYPE(NEXPTYPE) = FQUAL
              END IF
            ENDIF
            DO JL = 1, NCON
              READ(NUNIT,*) C1,C2,C3,C4,NTIMES,NPROG
              DO KL = 1, NTIMES
                READ(NUNIT,*) C
              END DO
              DO KL = 1, NPROG
                READ(NUNIT,*) C1,C2,C3,C4,NTIMES
                DO ML = 1, NTIMES
                  READ(NUNIT,*) C
                END DO
              END DO
            END DO
          END DO
          RETURN
        END IF  
      END IF
!
!---- Read past header to get dataset qualifier
!
      RETURN
!
!---- Error in file read.  Marker name not found. ---------------------------
!
 998  RETURN
!
 999  WRITE(NERR,100) MNAME,MARK
 100  FORMAT(' Error in reading marker information '/  &
             ' Sought: ',a20,'  Last found: ',a20)
      RETURN
!
!----- END OF MODULE MARKIN -------------------------------------------------
!
      END   !
