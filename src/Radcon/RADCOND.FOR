C     MEPAS RADCON: RADCOND.FOR           Version Date: 08-24-2006
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.
C*****************************************************************************
C                                                                            *
C                    PROGRAM RADCONPC                                        *
C                                                                            *
C  RADCONPC is the main program controlling the computation of               *
C  contaminant transport in the overland, groundwater, and surface           *
C  water environments.  It provides contaminant concentrations to HAZ        *
C  for the calculation of dose and HPI's.                                    *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    08/24/06 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: None
C     Calls to:  SUBROUTINES GETDAT, GETTIM, SCENAR, WUNITS, EXPOSR, CONTAM,
C                MEASUR, INITIN, INPUTM, DUMMYV, WETLND, LIMITS, SSWAP,
C                SSGW, SIMPS, COMPAR, DECAY, GATHER, ORIGIN, COUPLE, MASSBAL,
C                MERGE, OUTPUT
C
C     Common blocks referenced: DUMMY, INTER, INIT, TIME, MEDIA, INPUT, LIST,
C                               CONC1, VLEA1, WETLAN, INDEX1, INDEX2, INDEX3,
C                               INDEX4, INDEX5, INDEX6, INDEX7, INDEX8,
C                               INDEX9, COM21, AND ITE10 , isv , tech
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- ----- ------   -------------  --------------------------------
C
C
C
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     01/15/89     JWB  Variable TFINAL defined for length of simulation
C     03/10/89     JWB  FACIL.ID added to code to get waste unit file name
C     03/26/89     JWB  Decay-product computations changed and added a check
C                       to compare simulated concentrations to initial values
C     06/28/91     JGD  IYR added to measur call variables
C     11/26/91     JWB  Added required comments and header for MEPAS QA plan
C     01/30/92     JWB  SSGW and FUCNTION changes required a new version #
C     02/12/92     JWB  Include statements created and added to subroutines.
C                       Several subroutines had variable used but not defined
C                       and these were modified
C     07/23/92     JWB  Modification to COUPLE changes version date
C     08/10/92     JWB  Eliminated TFINAL definition becasue CONTAM reads it
C     08/12/92     JWB  Version date changed because of modifications to
C                       COUPLE and SSWAP
C     09/28/92     JLS  inserted VLEACH = V , after call to source()
C     11/08/92     JWB  COUPLE modified to compute 70-year conc as before
C     11/15/92     JWB  Modifications to CHECKS, CONC, CONTAM,INITIN, INPUTM,
C                       LEACHV, MASS, XLINE, and YLINE.  These changes are
C                       based on G Whelan response to two identified problems:
C                       point and xline source calcualtions, and Dracian vs
C                       pore water velocities.
C     12/02/92     JWB  Completed modifications for G Whelan responses
C     01/25/93     JWB  Modified to include CAP changes based on JLS mods
C     02/28/93     KJC  Addition of isv and tech
C     01/28/94     KJC  Added lines so no stop is used ERRORSIG is set instead
C     04/07/94     JPM  Changed version date.  Made changes to allow fluxes
C                       to be read from the *.WIN file in LEACHV.
C     04/15/94     JPM  HRA-EIS Time Specific Modification - Added a new loop
C                       outside the NTIMES loop to compute concentrations in
C                       the saturated zone at specific times found in the
C                       *.win file.  Changed the conditional just prior to
C                       the call to COMPAR so TPKSUM(M1) is not changed by
C                       repeated iterations through the new loop.
C                       JPM/GW  04/15/94
C     04/21/94     JPM  Changed argument list in call to MULREC so it matches
C                       the changes in MULREC.  Uncommented the lines that
C                       write information to the screen.  Changed the
C                       version date.
C     04/26/94     JPM  Corrected an error in MULREC regarding the writing
C                       of concentrations of multiple river receptors to the
C                       *.POL file.  Also changed the handling of X2 for a 
C                       saturated zone in INPUTM.  Corrected an error in the
C                       Time Specific Modification in LIMITS and SIMPS so the
C                       code will interpret the user entered specific times
C                       as total times instead of medium times.  Changed the
C                       version date.  Will not archive the 04/21/94 version.
C     05/02/94     JPM  Changed version date for changes to LIMITS, SIMPS, and
C                       CONTAM.
C     05/13/94     JPM  Added a call to the new subroutine MASSBAL.  This
C                       routine computes the remaining mass at the source
C                       when specific times are entered.  Output is to the
C                       *.MAS file.  Statements to open and close this file
C                       were also added.  Changed version date.
C     05/19/94     JPM  Changed the version date for bug fixes in MULREC
C                       and function FUN.
C     05/23/94     JPM  Changed the name of unit 10 from *.MAS to *.MBS.  Was
C                       conflicting with an output file from the source term
C                       code.
C     05/23/94     JPM  XL is the fraction of the source plume flux that is
C                       upstream of a river receptor.  Changed this variable
C                       to an array so the fraction for all the parents are
C                       saved.  Needed by the new formulation for multiple
C                       river receptors.
C     05/24/94     JPM  Changed version date.
C     05/31/94     AdH  Added VI and MSOURC to the call statement to MULREC.
C                       MULREC now call the COUPLE subrouting to get 70 yr
C                       averages for the *.WAT file, and COUPLE needs these
C                       variables.
C     06/02/94     JPM  Added a new array COMP_CO - initial concentrations
C                       at the source computed by the code.  Removed the
C                       initialization of the DMAX array.  DMAX is being
C                       removed from the code.  Passing ICONC to COMPAR and
C                       DECAY.
C     06/03/94     JPM  Removed the statements that open and close the *.MBS
C                       file.  This file will be open and closed in the
C                       MASSBAL subroutine, so it will not be created if no
C                       records are to be written to it.
C     06/03/94     JPM  Removing the page breaks (char(12)) that are written
C                       to the *.WLS file.
C     06/15/94     JPM  XL was not being passed into SSWAP as an array.
C                       Changed version date.
C     07/12/94     JPM  Added the array NFLAG on parent constituents - this
C                       flag is turned on in COMPAR if a computed
C                       concentration exceeds the decayed concentration at the
C                       source.  This flag indicated when the warning message
C                       is written to the *.WLS file.  Also added the CON
C                       array.  This is used by MULREC and SSWAP to store
C                       the decayed initial source concentrations.
C     07/14/94     JPM  Passing NFLAG to MEASUR.
C     07/22/94     JPM  Removed the dimension statement for XL - XL is the
C                       fraction of a source plume upstream of a river
C                       receptor.  It is now just a variable, since it is not
C                       contaminant specific.
C     07/25/94     JPM  Passing the array of decay constants (AL) to ORIGIN
C     07/29/94     JPM  No longer passing IYR to ORIGIN, MEASUR and MULREC.
C                       Calendar years are no longer written to the *.WLS
C                       file since they are not correct if the current date is
C                       not the start date for risk calculations.
C     08/04/94     JPM  Changed version date.
C     08/24/94     JPM  Passing CASN1 into MULREC for use by the new
C                       subroutine MAXTABLE.  Initializing the IPEAK, ISS,
C                       and ITRUNC flags used by MAXTABLE.
C     08/26/94     JPM  Added the array RATIO, which is used to determine if
C                       the instantaneous solution is to be used.  Passing to
C                       SIMPS and ORIGIN.  Need to save the RATIO values for
C                       output by ORIGIN.
C     08/31/94     JPM  Initializing IZERO for use by MAXTABLE.
C     09/06/94     JPM  Initializing LENFLAG for use by FUNCTION and DFUNCTIO
C     09/16/94     JPM  Removed the COMP_CO array - no longer needed by INPUTM.
C     09/22/94     JPM  Changed version date - releasing for use in MEPAS 3.1
C     12/22/94     JPM  Changed version date for three bug fixes in ORIGIN,
C                       COUPLE, and PRECIP, SEDOVR, & LEACH1.
C     02/10/95     JPM  Changed version date for fixes/enhancements to other
C                       modules.  Releasing for updated MEPAS version.
C     07/31/95     JPM  Added a new output file (*.WRN).  Most of the warning
C                       messages will now be written to this file instead of
C                       the *.WLS file.  Also, added TP_TOTAL which holds the
C                       cumulative time to peak for each constituent.
C     08/01/95     JPM  Added the arrays GW_MASS, VOL_MAS and SW_MASS for
C                       summing the mass lost to groundwater and volatization
C                       from the wetland environment.
C     11/17/95     JPM  Adding ZTOP and ZBOT to the call statement to INITIN.
C     11/17/95     JPM  Changed version date for a fix to S_TERM.
C     04/19/96     JPM  Changed version date for a fix to RUNVOL.  RAD-P061
C     05/06/96     JPM  Made the following changes for multiple saturated
C                       zones:  1) setting ISCONF to 5, when MED(KMED) = 4
C                       and MED(KMED-1) = 4 [i.e., the n-th saturated zone
C                       has a groundwater plume as a source, when n > 2],
C                       2) added an initialization of ZTOP and ZBOT to the
C                       top of the scenario loop, 3) writing the number of
C                       the saturated zone to the screen, and 4) calling
C                       subroutine PLUME to compute the source term dimensions
C                       for the next medium to be modeled (RAD-E046).
C     05/08/96     JPM  Changed the version date.
C     09/25/96     JPM  Passing HW and ISET to LIMITS and to WETLND for a
C                       subsequent call to LIMITS (RAD-E049).
C     09/27/96     JPM  Replaced SIMPS with KSIMPS (RAD-E050).
C     10/15/96     JPM  Near-field dilution changes:  1) removed the near-
C                       field dilution ratio (WRATIO), 2) added ZTOP and ZBOT
C                       to the argument list for subroutine INPUTM, and 3)
C                       changed version date (RAD-E052).
C     10/18/96     JPM  Made changes to allow MED=2, which indicates a
C                       concentration computation in the vadose zone.  Change
C                       being made for the phase 1 MEPAS waterborne
C                       executable for the framework (PH1-E001).
C     10/30/96     JPM  Calling MERGE for decay product summation (PH2-E002).
C     10/30/96     JPM  Made changes to allow MED = 6 to indicate flux
C                       computations in a river (PH1-E003).
C     11/04/96     JPM  Converting the times in the TIM array to total time
C                       prior to decay product summation (PH1-P002).
C     11/05/96     JPM  Opening the *.ERR file after FILENM is read from
C                       the FACIL.ID file, then closing and deleting the file
C                       if the code ran normally (PH1-E004).
C     11/07/96     JPM  Calling subroutine OUTPUT to write out the *.WFF and
C                       *.WCF files.  Modified the argument list for
C                       subroutine plume.  No longer creating a *.WAT file
C                       (PH1-E005).
C     11/11/96     JPM  Changed units from Ci to pCi (PH1-E006).
C     04/17/97     JPM  Passing MED(NMED) to subroutine OUTPUT instead of
C                       MED(1).  Calling OUTPUT for ISOURC = 4 (PH1-P006).
C     04/18/97     JPM  Passing the IVS array into subroutine OUTPUT instead
C                       of the IV array.  The IV array is over-written for
C                       multiple river receptors (PH1-P007).
C     04/25/97     JPM  Restored the call to subroutine COMPAR (PH1-E013).
C     04/28/97     JPM  Cleaned up the output to the *.WLS file, given that
C                       decay products are now permuted.  Simplified the
C                       argument list to MULREC, among other simple changes
C                       (PH1-E014).
C     04/29/97     JPM  Passing NTIMES into subroutine OUTPUT when ISOURC = 4,
C                       instead of passing in NTIMES+1 (PH1-P010).
C     05/01/97     JPM  Added TOTMAS to the argument list for INPUTM.  Changed
C                       version date (PH1-P011).
C     05/15/98     JPM  Made changes to allow for a variable number of
C                       multiple infiltration rates (see RAD-E054 for
C                       details).
C     05/21/98     JPM  Changed version date for a bug fix in LIMITS
C                       (RAD-P069).
C     06/10/98     JPM  Moved the initialization of CONCHEDR and FLUXHEDR
C                       to just prior to the NUMWU loop.  Passing JSCE,
C                       NUMWU, and NUMFLX to subroutine OUTPUT (PH1-E017).
C     07/23/98     JPM  Was skipping over setting TPKSUM for river media.
C                       Changed version date and released for FRAMES 1.0
C                       and MEPAS 4.0 (PH1-P016).
C     08/24/98     JPM  Changed version date for a bug fix in FLXTABLE
C                       (PH1-P017).
C     03/23/99     JPM  Changed version date for a bug fix in WRITDATA
C                       (PH1-P018).
C     06/18/99     JPM  Failed to change the version date in WRITHEDR for
C                       the previous change (PH1-P019).
C     08/08/00     JPM  Changed version date for a bug fix in DECAY.  Added
C                       necessary error handling code after the call to
C                       DECAY (PH1-P020).
C     02/27/01     JPM  Added the variable KTYPE (MEPAS contaminant type).
C                       Passing to CONTAM and OUTPUT.  Identifies whether
C                       the parent constituent is a radionuclide or not, and
C                       is used to write the correct units to the WFF and
C                       WCF files (PH1-P021).
C     05/22/01     JPM  Converting the times in the CS_TIME array to total
C                       time prior to calling MERGE and OUTPUT when specific
C                       times are being used.  Changed version date
C                       (PH1-P022).
C     08/13/01     JPM  Changed version date for a bug fix to subroutine
C                       WRITE_HEADER (PH1-P023).
C     08/23/01     JPM  Changed version date for a bug fix in subroutine
C                       DECAY (PH1-P024).
C     11/12/02     JPM  Changed version date for an enhancement in subroutine
C                       OUTPUT (PH1-E018).
C     09/29/04     JPM  Just before the call to subroutine ORIGIN, set NTIMES
C                       equal to 1 if NTIMES has been previously set to zero.
C                       Needed in order to properly write out the results when
C                       the vadose zone infiltration rate is zero (PH1-P026).
C                       Changed version date for changes to subroutine LIMITS
C                       (PH1-P025) and changes to subroutines INPUTM and WLIST
C                       to correctly handle a zero infiltration rate
C                       (PH1-P026).
C     08/24/06     JPM  Add the RIVX, RIVY, and RIVZ variables.  These are the
C                       location coordinates of the river receptor in meters
C                       which are read from the WIN file within subroutine
C                       INPUTM and then are written to the WCF file.  They are
C                       read in and written as character strings because no
C                       computations are performed and this ensures accurate
C                       reproduction of all digits in the WCF file.  Changed
C                       version date (PH1-E021).
C     07/28/10     MAP  Coverted to 32 bit application and compiled with 
C                       Compac Visual Fortran, rearranged dependencies
C                       associated with MAXIMUM.FOR now MAXIMUM.WTN.
C                       Corrected calling errors with S_TERM and OUTPUT
C
C==== PROGRAM Statement ======================================================
C
      integer FUNCTION RADCONS()
CKJC      PROGRAM RADCONPC
C
C==== FUNCTION Definitions ===================================================
C
      EXTERNAL myintr
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
      INCLUDE 'INDEX5.WTN'
      INCLUDE 'INDEX6.WTN'
      INCLUDE 'INDEX7.WTN'
      INCLUDE 'INDEX8.WTN'
      INCLUDE 'INDEX9.WTN'
      INCLUDE 'COM21.WTN'
      INCLUDE 'CONC1.WTN'
      INCLUDE 'CTIME.WTN'
      INCLUDE 'DUMMY.WTN'
      INCLUDE 'INIT.WTN'
      INCLUDE 'LIST.WTN'
      INCLUDE 'INPUT.WTN'
      INCLUDE 'INTER.WTN'
      INCLUDE 'ITE10.WTN'
      INCLUDE 'MEDIA.WTN'
      INCLUDE 'VLEA1.WTN'
      INCLUDE 'WETLAN.WTN'
      INCLUDE 'CAP.WTN'
      include 'extflux.wtn'
      include 'isv.wtn'
      include 'tech.wtn'
      include 'masss.wtn'

C
C==== Variable Declarations ==================================================
C
C     CHARACTER FILENM*8
      CHARACTER RECIN*80,RECTYP,LOCNAM*21
      REAL newcon, newtim, myintr
      LOGICAL FLUXHEDR,CONCHEDR
      REAL*4 TIMAX,CONCTIM,CONCMAX
      INTEGER*4 KTYPE
      CHARACTER*15 RIVX,RIVY,RIVZ
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION UNITNM(6)
CJPM      DIMENSION VI(MAXCON)
      DIMENSION CO(MAXCON)
      DIMENSION CON(MAXNTI,MAXCON)
      DIMENSION TLIFE(MAXCON)
      DIMENSION T33(MAXCON)
      DIMENSION LMON(12)
      DIMENSION ERRMAX(MAXCON)
      DIMENSION ERROR(MAXCON)
      DIMENSION CMAX(MAXCON)
      DIMENSION CTMAX(MAXCON)
      DIMENSION NDAU(MAXCON)
      DIMENSION TPKSUM(MAXCON)
c pdm
      DIMENSION newcon(MAXITR,MAXNTI,MAXCON)
      DIMENSION newtim(MAXITR,MAXNTI,MAXCON)
c      DIMENSION newcon(3,MAXNTI,MAXCON)
c      DIMENSION newtim(3,MAXNTI,MAXCON)
c pdm
      DIMENSION NFLAG(MAXCON),KTYPE(MAXCON)
      DIMENSION RATIO(MAXCON)
      DIMENSION TP_TOTAL(MAXCON),TIMAX(MAXCON)
      DIMENSION GW_MASS(MAXCON),VOL_MAS(MAXCON),SW_MASS(MAXCON)
C
C==== DATA Definitions =======================================================
C
      DATA UNITNM(1),UNITNM(2),UNITNM(3),UNITNM(4),UNITNM(5),UNITNM(6)/
     1     4HATTO,4HPICO,4HMICR,4HMILI,4HCENT,4HKILO/
      DATA LMON/31,28,31,30,31,30,31,31,30,31,30,31/
C
      here=0

      CALL GETDAT( IYR,IMON,IDAY )
      CALL GETTIM( IHR,IMIN,ISEC,I100TH)
C*****************************************************************************
C     GET WASTE UNIT FILENAME FROM FACIL.ID  JWB  3-10-1989
C*****************************************************************************
      OPEN (UNIT=8, FILE='FACIL.ID', STATUS='OLD', IOSTAT=IOS)
    6 READ (8,1001) RECIN
      READ (RECIN,1002) RECTYP
      IF (RECTYP.NE.'1') GOTO 6
      READ (RECIN,1003) NUMWUF
C
      DO 80 NF=1,NUMWUF
C
      READ (8,1004) FILENM
      isssfl=1
c Set ext_flux to zero if *.WMS exists, set to non-zero if error  JLS
      OPEN (UNIT=13,FILE=FILENM//'.ERR',STATUS='UNKNOWN',IOSTAT=IOS)
      open (unit=15, file=filenm//'.wms', status='OLD',iostat=ext_flux)
      close(15)
C*****************************************************************************
C     OPEN FILES FOR FURTHER CALCULATIONS AND STORAGE
C*****************************************************************************
      OPEN (UNIT=5, FILE=FILENM//'.WIN', STATUS='OLD', IOSTAT=IOS)
      OPEN (UNIT=6, FILE=FILENM//'.WLS', STATUS='UNKNOWN',
     1                                   IOSTAT=IOS)
cjpm      OPEN (UNIT=7, FILE=FILENM//'.WAT', STATUS='UNKNOWN',
cjpm     1                                   IOSTAT=IOS)
 1001 FORMAT (A80)
 1002 FORMAT (1X,A1)
 1003 FORMAT (3X,I3)
 1004 FORMAT (3X,A8)
C
      OPEN(UNIT=9,FILE=FILENM//'.POL',STATUS='UNKNOWN',IOSTAT=IOS)
      OPEN (UNIT=11, FILE=FILENM//'.WRN', STATUS='UNKNOWN',
     1                                   IOSTAT=IOS)
C
      YDIST=0.0
      MFLG = 0
C*****************************************************************************
C     CODE VERSION FOR RADCON
C     WRITE(6,'(30(''*''),'' 4-28-90 Version 3.0 '',30(''*''))')
C     WRITE(6,'(1X,27(''*''),''July 2, 1991 VERSION III'',27(''*''))')
C     WRITE(6,'(1X,27(''*''),''1/30/1992 VERSION III'',27(''*''))')
C     WRITE(6,'(1X,27(''*''),''8/10/1992 VERSION III'',27(''*''))')
C     WRITE(6,'(1X,27(''*''),''8/12/1992 VERSION III'',27(''*''))')
C     WRITE(6,'(1X,27(''*''),''11/15/1992 VERSION III'',27(''*''))')
C     WRITE(6,'(1X,27(''*''),''12/02/1992 VERSION III'',27(''*''))')
C     WRITE(6,'(1X,27(''*''),''03/02/1993 VERSION IV'',27(''*''))')
C     WRITE(6,'(1X,24(''*''),''01/28/1994 RAAS VERSION 1.0''24(''*''))')
C     WRITE(6,'(1X,24(''*''),''04/11/1994 VERSION 4.0'',24(''*''))')
C     WRITE(6,'(1X,24(''*''),''04/26/1994 VERSION 4.0'',24(''*''))')
C     WRITE(6,'(1X,24(''*''),''05/02/1994 VERSION 4.0'',24(''*''))')
C     WRITE(6,'(1X,27(''*''),''05/24/1994 VERSION 4.0'',27(''*''))')
C     WRITE(6,'(1X,27(''*''),''06/15/1994 VERSION 4.0'',27(''*''))')
C     WRITE(6,'(1X,27(''*''),''08/04/1994 VERSION 4.0'',27(''*''))')
C     WRITE(6,'(1X,27(''*''),''09/22/1994 VERSION 4.1'',27(''*''))')
C     WRITE(6,'(1X,27(''*''),''12/22/1994 VERSION 4.1'',27(''*''))')
C     WRITE(6,'(1X,27(''*''),''02/10/1995 VERSION 4.1'',27(''*''))')
C     WRITE(6,'(1X,27(''*''),''11/17/1995 VERSION 4.2'',27(''*''))')
C     WRITE(6,'(1X,27(''*''),''04/19/1996 VERSION 4.2'',26(''*''))')
C     WRITE(6,'(1X,28(''*''),''VERSION 05/08/1996'',28(''*''))')
C     WRITE(6,'(1X,28(''*''),''VERSION 10/15/1996'',28(''*''))')
C     WRITE(6,'(1X,28(''*''),''VERSION 05/01/1997'',28(''*''))')
C     WRITE(6,'(1X,28(''*''),''VERSION 05/22/1998'',28(''*''))')
C     WRITE(6,'(1X,28(''*''),''VERSION 07/23/1998'',28(''*''))')
C     WRITE(6,'(1X,28(''*''),''VERSION 08/24/1998'',28(''*''))')
C     WRITE(6,'(1X,28(''*''),''VERSION 03/23/1999'',28(''*''))')
C     WRITE(6,'(1X,28(''*''),''VERSION 06/18/1999'',28(''*''))')
C     WRITE(6,'(1X,28(''*''),''VERSION 08/08/2000'',28(''*''))')
C     WRITE(6,'(1X,28(''*''),''VERSION 02/28/2001'',28(''*''))')
C     WRITE(6,'(1X,28(''*''),''VERSION 05/22/2001'',28(''*''))')
C     WRITE(6,'(1X,28(''*''),''VERSION 08/13/2001'',28(''*''))')
C     WRITE(6,'(1X,28(''*''),''VERSION 08/23/2001'',28(''*''))')
C     WRITE(6,'(1X,28(''*''),''VERSION 11/12/2002'',28(''*''))')
C     WRITE(6,'(1X,28(''*''),''VERSION 09/29/2004'',28(''*''))')
C     WRITE(6,'(1X,28(''*''),''VERSION 08/24/2006'',28(''*''))')
C*****************************************************************************
      WRITE(6,'(1X,28(''*''),''VERSION 07/28/2010'',28(''*''))')
      WRITE(*,'(1X,28(''*''),''VERSION 07/28/2010'',28(''*''))')
      WRITE(6,4500)
 4500 FORMAT (//,10x,'RADCON -- ',
     1  'WATERBORNE TRANSPORT COMPONENT for the Multimedia'/
     2  20x,'Environmental Pollutant Assessment System (MEPAS):'/
     3  20x,'Models movement of radionuclides and other chemicals'/
     4  20x,'in groundwater, surface water, and overland pathways.'//
     5  20X,'Battelle, Pacific Northwest Laboratory (PNL)'/,
     6  20X,'P.O. Box 999, Richland, WA 99352',//
     7  20X,'Developed for the U.S. Department of Energy'//)
      write(6,1357)filenm
1357  format(12x,'Input file for this run is ',a8,'.WIN')
C*****************************************************************************
C     ENTER NUMBER OF WASTE UNITS PER FACILITY (NUMWU)
C pdm   and the number of infiltration rates to use in the simulation (NUMITR)
C*****************************************************************************
      CALL WUNITS
c pdm Added for dll compilation
      if (ERRORSIG) goto 9999
C
      FLUXHEDR = .TRUE.
      CONCHEDR = .TRUE.
      DO 70 M=1,NUMWU
C*****************************************************************************
C     ENTER THE NUMBER OF USAGE LOCATIONS PER TRANSPORT SCENARIO SUMMED
C     PER WASTE UNIT (NUMSCE)
C*****************************************************************************
      CALL SCENAR
      NUMREC=1
      IMFLAG=1
C
      DO 68 JSCE=1,NUMSCE
C
      WRITE(6,4) JSCE,M
      WRITE(11,4) JSCE,M
    4 FORMAT (1X, 78('*'),/9X,' Modeling Parent Contaminant Number ',
     +I3,' for Scenario Number ',I3,/1X,78('*')/)
      IPEAK = 0
      ISS = 0
      ITRUNC = 0
      IMEAS = 0
      IZERO = 0
      ZTOP = 0.0
      ZBOT = 0.0
      DO M1=1,MAXCON
	NFLAG(M1)=0
	TP_TOTAL(M1) = 0.0
	GW_MASS(M1) = 0.0
	VOL_MAS(M1) = 0.0
	SW_MASS(M1) = 0.0
      ENDDO
C
C*****************************************************************************
C     IDENTIFY EXPOSURE LOCATIONS FOR EACH TRANSPORT AND EXPOSURE SCENARIO
C*****************************************************************************
      CALL EXPOSR (JSCE,LOCNAM)
C
C Open Debug file
C
      OPEN (UNIT=28, FILE=FILENM//'.dbg', STATUS='UNKNOWN',
     1    IOSTAT=IOS)
C
C Initialize flags to enter the loop for the first iteration
C
      ISTYPE = 1
      ISOURC = 1
      IPOND  = 2
C
C initialize tcap and tfail for debug purposes JLS
C
c      tcap  = 50.0         Sample data  JLS
c      tfail = 100.0
c      fractf = 0.70
c
c JLS check should be done in the shell
c      if((tcap .le. 0.0) .or. (tfail.le.tcap) .or. (fractf.lt.0.0) .or.
c     +   (fractf.gt.1.0)  .or. (numitr.le.0)) numitr = 1
C
C pdm Start loop here for vadose zone changes JLS
C
c pdm      if (numitr.eq.0) numitr=1
      DO 777 incr=1,numitr
      if ( ((ISTYPE .LT. 3) .AND. (ISOURC .EQ. 2) .AND. (IPOND .EQ. 2))
     +                    .OR.  (incr.eq.1) ) then

      if ( incr .GT. 1 ) then
	close (UNIT=5)

	open (UNIT=5, FILE=FILENM//'.WIN', STATUS='OLD', IOSTAT=IOS)
C
C       Read, same as in wunits.for
C
	read(5,7771) NUMWU
	IF (NUMWU .LE. 0) NUMWU=1
C
C       Read, same as in scenar.for
C
	READ(5,7771) NUMSCE
7771    FORMAT (I5)
	IF (NUMSCE .LE. 0) NUMSCE=1
C
C       Read, same as in exposr.for
C
	READ(5,7772) (IV(I,1), I=1,4)
	READ(5,7772) (IV(I,2), I=1,4)
	READ(5,7772) (IV(I,3), I=1,4)
	READ(5,7772) (IV(I,4), I=1,4)
 7772   FORMAT (8A18)
	READ(5,7773) LOCNUM,LOCMED,locnam
 7773   FORMAT (2I5,5x,a21)
      end if
C*****************************************************************************
C     COMPUTES CONCENTRATION HYDROGRAPHS AT MULTIPLE RECEPTORS IN THE SAME
C     MEDIUM FOR THE SAME TRANSPORT SCENARIO
C*****************************************************************************
      IF (NUMREC .GE. 2) THEN
	CALL MULREC (XL,CMAX,co,CON,NFLAG,CASN1,LOCNAM)
CKJC  Added for dll compilation
	if (ERRORSIG) goto 9999
      endif
      IF (NUMREC .GE. 2) GO TO 69
C*****************************************************************************
C     INPUT CONTAMINANT INFORMATION
C*****************************************************************************
      CALL CONTAM (TLIFE,CO,GAMMA,KTYPE)
CKJC  Added for dll compilation
      if (ERRORSIG) goto 9999
C*****************************************************************************
C    INPUT MEASURED CONCENTRATIONS AT THE RECEPTOR
C*****************************************************************************
      IF (ISOURC .LT. 4) GO TO 221
C*****************************************************************************
C     SPELL CHECKS, CORRECTIONS TO MEASUR.FOR, OUTPUTS;
C     IYR WAS ADDED TO MEASUR CALL VARIABLES - JGD 06/19/91
C*****************************************************************************
      IF (NUMREC .LT. 2) then
	CALL MEASUR(TLIFE,CO,UNITNM,xl,MSOURC,QOLD,NFLAG)
CKJC  Added for dll compilation
	if (ERRORSIG) goto 9999
      endif
      GO TO 222
C*****************************************************************************
C     INPUT SOURCE-TERM INFORMATION
C
C     VARIABLE (U) REPLACED BY (VLEACH) IN CALL TO SOURCE  JWB - 11/14/92
C221   CALL SOURCE (U,AL1,BLEN,BWIDTH,TLIFE,SEDFLX,CO,WUMC,WUBD,
C     1             UNITS,B1,CLEN,GAMMA,M1)
C*****************************************************************************
221   CALL SOURCE (vleach,AL1,BLEN,BWIDTH,TLIFE,SEDFLX,CO,WUMC,WUBD,
     1             UNITS,B1,CLEN,GAMMA,M1)
CKJC  Added for dll compilation
      if (ERRORSIG) goto 9999
C*****************************************************************************
C     INITIAL DATA INPUT BEGINS
C*****************************************************************************
C JLS CALL INITIN (NDS,AL,ND,AL2,B2,UNITS,UNITNM,B1,ALEN,BLEN,BWIDTH,
c JLS removed AL2, B2 from the argument list to match the function definition
      CALL INITIN (NDS,AL,ND,UNITS,UNITNM,B1,ALEN,BLEN,BWIDTH,
     1             CO,SEDFLX,WUMC,WUBD,CLEN,M1,ERROR(M1),TLIFE,
     2             ZTOP,ZBOT)
CKJC  Added for dll compilation
      if (ERRORSIG) goto 9999
C
C*****************************************************************************
C     Call MASSBAL when specific times are entered.  MASSBAL computes the
C     mass remaining at the source at specific times.
C*****************************************************************************
C
      IF(NTIMSPEC.GT.0) CALL MASSBAL (WUMC,WUBD,FILENM)
C
C*****************************************************************************
C     DATA INPUT BY MEDIUM
C        ISET IS USED FOR PRINTING THE MIXING LENGTH
C        INITIALIZE STEADY-STATE INDICES: ISTEDY AND ISSTAT
C*****************************************************************************
      ISET=0
C
      DO 445 I=1,MAXCON
	ISTEDY(I)=0
  445 ISSTAT(I)=0
C
      NPSZ = 0
      NSZ = 0
      DO 94 M1=1,NUMCON
      TPKSUM(M1)=0.
94    CONTINUE
      DO 67 KMED=1,NMED
C*****************************************************************************
C     NPSZ IS A COUNTER FOR PRINTING THE PARTIALLY SATURATED ZONE # TO SCREEN
C     AND NSZ IS A COUNTER FOR THE SATURATED ZONE #
C*****************************************************************************
	IF(MED(KMED).LE.2) THEN
	  NPSZ = NPSZ + 1
	ELSE IF (MED(KMED).EQ.3.OR.MED(KMED).EQ.4) THEN
	  NSZ = NSZ + 1
	ENDIF
C
	DO 91 M1=1,NUMCON
	  ERRMAX(M1) = 0.0
91      CONTINUE
C
C*****************************************************************************
C     If the current medium is a SZ at another SZ or a river (i.e., MED(KMED)
C     =4), and the previous medium was also a SZ at another SZ (i.e.,
C     MED(KMED-1)=4), then set the source configuration to a groundwater plume
C     (i.e., ISCONF=5).  JPM 05/06/96 (RAD-E046)
C*****************************************************************************
C
      IF (KMED.GT.1) THEN
	IF ((MED(KMED).EQ.4.OR.MED(KMED).EQ.3).AND.MED(KMED-1).EQ.4)
     +  THEN
	   ISCONF = 5
	   B1 = ALEN
	ENDIF
      ENDIF
C
      WRITE(6,21) KMED,MED(KMED),ISCONF,ISTYPE
      WRITE(11,21) KMED,MED(KMED),ISCONF,ISTYPE
   21 FORMAT (' Modeling Medium #            ',I2,/,
     1        ' Medium Type (MED)          = ',I2,/,
     2        ' Source Configuration       = ',I2,/,
     3        ' Source Term Discharge Type = ',I2)
C*****************************************************************************
C     TLIFE ADDED TO INPUTM SUBROUTINE  JWB - 11/14/92
C*****************************************************************************
      CALL INPUTM (X2,ND,NDS,AL,AL1,B1,IV,CASN1,H1,U1,E1,E2,X3,CO,HW,
     1             VLEACH,WUMC,WUBD,UNITS,TLIFE,TP_TOTAL,ZTOP,ZBOT,
     2             TOTMAS,RIVX,RIVY,RIVZ)
CKJC  Added for dll compilation
      if (ERRORSIG) goto 9999
C
C*****************************************************************************
C     HRA-EIS Time Specific Modification - Adding a new loop outside of the
C     NTIMES loop, that will execute once for each specific time at which
C     concentrations in the saturated zone will be computed.  This
C     modification is invoked by specifying the specific times in the *.win
C     file (NTIMSPEC > 0) and by having a receptor in the saturated zone
C     (MED(KMED) = 3).  If invoked, reset NTIMES to 1 so only the specific
C     time the user is interesed in is computed.  This shortens run times in
C     the saturated zone when the user only wants the concentrations at
C     specific times.  JPM/GW 04/15/94
C*****************************************************************************
C
      IF ((NTIMSPEC.GT.0).AND.(MED(KMED).EQ.3)) THEN
	   IKOUNT = NTIMSPEC
	   NTIMES = 1
      ELSE
	   IKOUNT = 1
      ENDIF
      DO 248 KOUNT_TS=1,IKOUNT
C
C*****************************************************************************
C     TEMPORAL COMPUTATIONS BEGIN
C*****************************************************************************
      DO 2 IT=1,NTIMES+1
C*****************************************************************************
C                    SCREEN PRINT OF THE DO LOOP I
C*****************************************************************************
	JJJ = JSCE
C        IF( LOCMED .EQ. 3 ) JJJ = LOCNUM
	IF(MED(KMED) .LE. 2 ) THEN
	    WRITE(*,'(A,''CONTAMINANT # '',I2,
     1                '' -- PARTIALLY SATURATED ZONE # '',I2,
     2                '' -- TIMESTEP IS '',I3,'' OF '',I3,\)') 
     3                CHAR(13),JJJ,NPSZ,IT,NTIMES+1
	ELSE IF( MED(KMED) .GT. 2 .AND. MED(KMED) .LT. 5 ) THEN
	    WRITE(*,'(A,''CONTAMINANT # '',I2,
     1                '' -- SATURATED ZONE # '',I2,
     2                ''           -- TIMESTEP IS '',I3,'' OF '',I3,\)')
     3                CHAR(13),JJJ,NSZ,IT,NTIMES+1
	ELSE IF( MED(KMED) .EQ. 5 .OR. MED(KMED) .EQ. 6) THEN
	    WRITE(*,'(A,''CONTAMINANT # '',I2,
     1                '' -- SURFACE WATER              '',
     2                ''   -- TIMESTEP IS '',I3,'' OF '',I3,\)')
     3                CHAR(13),JJJ,IT,NTIMES+1
	ELSE IF( MED(KMED) .EQ. 7 ) THEN
	  WRITE(*,'(A,''CONTAMINANT # '',I2,
     1                '' -- WETLANDS/SWAMP/BOGS        '',
     2                ''   -- TIMESTEP IS '',I3,'' OF '',I3,\)')
     3                CHAR(13),JJJ,IT,NTIMES+1
	ENDIF
      IF (NPRINT .LE. 1) GO TO 164
      IF (KMED .NE. NMED) WRITE(6,165) UNITNM(IUNIT)
      IF (KMED .EQ. NMED) WRITE(6,105) UNITNM(IUNIT)
  165 FORMAT ('Constituent',T19,'F-(',A4,'pCi/yr)',T43,
     1        'TSTRT2 (yr)',T57,'TEND2 (yr)'/)
  105 FORMAT ('Constituent',T19,'C-(',A4,'pCi/ml)',T43,
     1        'TSTRT2 (yr)',T57,'TEND2 (yr)'/)
  164 CONTINUE
      ICNT=0
C*****************************************************************************
C     COMPUTATIONS BY CONTAMINANT BEGIN; NUMCON IS THE NUMBER OF CONSTITUENTS
C*****************************************************************************
      DO 66 M1=1,NUMCON
C*****************************************************************************
C     ASSIGNING DUMMY VARIABLES; NDS IS THE NUMBER OF PARENT-DECAY IN CHAIN
C*****************************************************************************
      NDAU(M1)=NDS(M1)
      IF( NTIMES .EQ. 0 ) GOTO 10
      IF(MED(KMED).LT.7) NDS(M1)=1
      CALL DUMMYV (C,X3,XL)
      IF ((ISTYPE .NE. 4) .AND. (ISTYPE .NE. 6)) GO TO 30
C*****************************************************************************
C     HOLDING POND CALCULATIONS
C*****************************************************************************
      CALL HOLDP (M1,C,AL,TLIFE,NDS)
      GO TO 10
C*****************************************************************************
C    WETLAND CALCULATIONS
C*****************************************************************************
   30 IF (MED(KMED) .NE. 7) GO TO 123
      CALL WETLND (T2,C,NDS,UNITS,AL,GAMMA,T3,DTT,MED,M1,IT,KMED,
     1             T33,GW_MASS,VOL_MAS,SW_MASS,hw,iset)
CKJC  Added for dll compilation
      if (ERRORSIG) goto 9999
      GO TO 10
C*****************************************************************************
C     CALCULATES LIMITS OF INTEGRATION FOR GROUNDWATER PATHWAY
C*****************************************************************************
123   CALL LIMITS (GAMMA,T3,DTT,MED,T33,hw,iset)
CKJC  Added for dll compilation
      if (ERRORSIG) goto 9999
      GO TO (121,121,121,121,122,122,10), MED(KMED)
C*****************************************************************************
C     SURFACE WATER CALCULATIONS
C*****************************************************************************
122   CALL SSWAP (U1,H1,X2,B1,XL,GAMMA,C,ALAM,E2,T2(M1),M1,
     1            TPEAK(M1),UNITS,NDS,CON,NFLAG,CO,MED(KMED))
CKJC  Added for dll compilation
      if (ERRORSIG) goto 9999
      GO TO 10
C*****************************************************************************
C     CALCULATE STEADY-STATE GROUNDWATER CONCENTRATIONS/FLUXES
C*****************************************************************************
  121 CALL SSGW (C,TLIFE)
CKJC  Added for dll compilation
      if (ERRORSIG) goto 9999
C*****************************************************************************
C     TRAPEZOID INTEGRATION
C*****************************************************************************
20    CALL KSIMPS (T3,DTT,C,HW,ISET,ERROR(M1),M1,ERRMAX(M1),RATIO(M1))
CKJC  Added for dll compilation
      if (ERRORSIG) goto 9999
C
C*****************************************************************************
C     COMPARES SOURCE-TERM CONCENTRATIONS WITH SIMULATED CONCENTRATIONS TO
C     ENSURE THAT THE SIMULATED CONCENTRATIONS DO NOT EXCEED THE SOURCE-
C     TERM CONCENTRATIONS.  JWB 3-26-1989
C*****************************************************************************
      IF (MED(KMED).EQ.2.OR.MED(KMED).EQ.3.OR.MED(KMED).EQ.5.OR.
     +   (MED(KMED).EQ.7.AND.KMED.EQ.NMED)) THEN
	CALL COMPAR (M1,C,CO,ICONC,NFLAG,MFLG)
      ENDIF
C*****************************************************************************
C     COMPUTE DECAY PRODUCTS FROM PARENTS.  JWB 3-26-1989
C*****************************************************************************
10    ND=NDAU(M1)
      NDS(M1)=NDAU(M1)
C*****************************************************************************
C     HRA-EIS Time Specific Modification - When the time specific loop is
C     being executed more than once, make sure TPKSUM(M1) is only set the
C     first time through the loop.  JPM/GW 03/25/94
C*****************************************************************************
25    IF ((IT.EQ.1).AND.(KOUNT_TS.EQ.1)) THEN
	   TPKSUM(M1) = TPKSUM(M1) + TPEAK(M1)
      ENDIF
      CALL DECAY (T2(M1),AL,CDA,ND,C,M1,TRAVEL,MED(KMED),CO(M1),
     1            TPKSUM(M1),IDECAY,ICONC(M1))
      if (ERRORSIG) goto 9999
C*****************************************************************************
C     COLLATE CONTAMINANTS
C*****************************************************************************
      DO 344 L=1,ND
344   CALL WLIST (IV(M1,L),CASN1(M1,L),CDA(L),ICNT,M1)
   66 CONTINUE
C*****************************************************************************
C     GATHER AND PRINT CONCENTRATIONS/FLUXES
C*****************************************************************************
      CALL GATHER (ICNT)
    2 CONTINUE
  248 CONTINUE
C*****************************************************************************
C     FOR A SATURATED ZONE AT A RIVER OR AT ANOTHER SATURATED ZONE (MED=4),
C     COMPUTE THE SOURCE TERM DIMENSIONS FOR THE NEXT MEDIUM TO BE MODELED.
C*****************************************************************************
      IF (MED(KMED).EQ.4) THEN
C
C       Multiply the dispersion coefficients by the retardation factors
C       to generate the dispersion coefficients independent of contaminant.
C
	DY = E2*A(M1-1)
	DZ = E3*A(M1-1)
	CALL PLUME (ALEN,ZTOP,ZBOT,DY,DZ,X2,H1,U,B1,ISCONF)
      ENDIF
C*****************************************************************************
C     If NTIMES = 0, then the vadose zone infiltration rate (VLEACH) is zero.
C     When this is the case, the first two records in the CS array were set
C     euqal to zero and the first two times in the TIM array were set equal to
C     zero and TFINAL in subroutine WLIST.  To correctly output these results,
C     NTIMES must be set to 1 (so than NTIMES+1 equals 2, because the results
C     occupy 2 rows in these arrays).  PH1-P026.
C*****************************************************************************
      IF (NTIMES.EQ.0) NTIMES = 1
C*****************************************************************************
C     SHIFT ORIGIN FOR NEXT MEDIUM
C*****************************************************************************
      CALL ORIGIN (CS,IVS,CASN2,ICNT,M1,UNITS,CMAX,CTMAX,NDS,co,
     C             CS_SPEC,NFLAG,AL,RATIO,GW_MASS,VOL_MAS,SW_MASS,TIMAX)
   67 CONTINUE
C*****************************************************************************
C     Convert the times in the TIM array into total time.  For specific
C     times, convert from medium time to total time for the first
C     constituent.  For all other constituents, just use the total times
C     for the first, since they will be identical.  This avoids problems
C     in subroutine MERGE when it tries to decide if the times in each
C     array are equal (PH1-P022).
C*****************************************************************************
      M3 = 0
      DO M1 = 1,NUMCON
	DO MM = 1,NDS(M1)
	  M3 = M3 + 1
	  DO I = 1,NTIMES+1
	    TIM(I,M3) = TIM(I,M3) - STRT(M3) + TRAVEL(M1)
	  ENDDO
          IF (NTIMSPEC.NE.0) THEN
            DO I = 1,NTIMSPEC
              IF (M3.EQ.1) THEN
                CS_TIME(I,M3) = CS_TIME(I,M3) - STRT(M3) + TRAVEL(M1)
              ELSE
                CS_TIME(I,M3) = CS_TIME(I,1)
              ENDIF
            ENDDO
          ENDIF
	ENDDO
      ENDDO
C*****************************************************************************
C     Add the permutated decay products back onto the parent constituents.
C     NDP is the number of decay products for the M1th parent, counting decay
C     products only (PH1-E002).
C*****************************************************************************
      M3 = 0
      DO M1 = 1,NUMCON
	NDP = NDS(M1) - 1
	M3 = M3 + 1
	DO I = 1,NDP,+1
	  M3 = M3 + 1
	  IF (NTIMSPEC.EQ.0) THEN
	    CALL MERGE(TIM,CS,M3,M3+NDP,NTIMES+1)
	  ELSE
	    CALL MERGE(CS_TIME,CS_SPEC,M3,M3+NDP,NTIMSPEC)
	  ENDIF
	ENDDO
      ENDDO
C
C*****************************************************************************
C     Write the results to the *.WFF or *.WCF files
C*****************************************************************************
C
      IF (NTIMSPEC.EQ.0) THEN
	CALL OUTPUT(FILENM,MED(NMED),NUMCON,IVS,CASN1,NTIMES+1,NDS,TIM,
     +            CS,AL1,B1,ZTOP,ZBOT,ALEN,XMIX,H1,AN5,U,UNITS,NUMSCE,
     +            FLUXHEDR,CONCHEDR,LOCNAM,JSCE,NUMWU,NUMFLX,KTYPE,
     +            RIVX,RIVY,RIVZ)
      ELSE
        CALL OUTPUT(FILENM,MED(NMED),NUMCON,IVS,CASN1,NTIMSPEC,NDS,
     +            CS_TIME,CS_SPEC,AL1,B1,ZTOP,ZBOT,ALEN,XMIX,H1,AN5,U,
     +            UNITS,NUMSCE,FLUXHEDR,CONCHEDR,LOCNAM,JSCE,NUMWU,
     +            NUMFLX,KTYPE,RIVX,RIVY,RIVZ)
      ENDIF
      IF(NTIMSPEC.EQ.0) THEN
	IF (MED(NMED).EQ.2.OR.MED(NMED).EQ.3.OR.MED(NMED).EQ.5.OR.
     +  MED(NMED).EQ.7) THEN
	  CALL RISKCONC(NUMCON,NDS,TIMAX,NTIMES,SMASS,IVS,TDIFF,CS,
     +                TOTTIM,UNITS)
	ENDIF
	M1 = 1
	M3 = 1
	DO WHILE (M1.LE.NUMCON)
	  CONCTIM = 0.0
	  CONCMAX = 0.0
	  DO II = 1,NTIMES+1
	    IF (CS(II,M3).GT.CONCMAX) THEN
	      CONCMAX = CS(II,M3)
	      CONCTIM = TIM(II,M3)
	    ENDIF
	  ENDDO
	  CALL MAXTABLE(CONCTIM,CONCMAX,CS,TIM,TRAVEL(M1),M1,
     +    M3,NFLAG(M1),NTIMES,IVS(M3),TDIFF(M1),NUMCON,CASN1(M1,1),
     +    SOLLIM(M1),CO(M1),UNITS,MED(NMED),ISOURC)
	  M1 = M1 + 1
	  M3 = M3 + NDS(M1-1)
	ENDDO
      ELSE
	WRITE(6,781)
	WRITE(6,8034)
	WRITE(6,781)
781     FORMAT(/1X,78('*'))
8034    FORMAT(/10X,'Maximum constituent concentrations are not output',
     1         /10X,'when concentrations are computed only for',
     2         /10X,'specific times.')
      ENDIF
c
c Store the flux and time values for the polutograph for future use.
c
cjpm      M3 = 0
      M3 = 1
      do j=1,NUMCON
	do i=1,NTIMES+1
cjpm          TIME(i,j) = TIME(i,j) + TRAVEL(j)
cjpm          newtim(incr,i,j)  = TIME(i,j)
	  newtim(incr,i,M3) = tim(i,M3)
	  newcon(incr,i,M3) = cs(i,M3)
	end do
cjpm        do k=1,NDS(j)
	M3 = M3 + nds(j)
cjpm          do i=1,NTIMES+1
cjpm            newcon(incr,i,M3) = CS(i,M3)
cjpm          end do
cjpm        end do
      end do

c pdm if last iteration, combine all results into one array of (ntimes+1)
c      if (incr .eq. 3) then
      if (numitr.gt.1 .and. incr .eq. numitr) then
	M3 = 1
	do j=1,NUMCON
c
c Shift all the times
c
cjpm          do i=1,ntimes+1
cjpm            do kk=2,numitr
cjpm              newtim(kk,i,j) = newtim(kk,i,j) + tremed(kk-1)
	  do kk=2,numitr
	    do i=1,ntimes+1
	      newtim(kk,i,m3) = newtim(kk,i,m3) + tremed(kk-1)
	      write(28,'(3I3,6E12.4)')i,j,m3,newtim(1,i,m3),
     +        newcon(1,i,m3),newtim(2,i,m3),newcon(2,i,m3),
     +        newtim(3,i,m3),newcon(3,i,m3)
	    enddo
c            newtim(2,i,j) = newtim(2,i,j) + tcap
c            newtim(3,i,j) = newtim(3,i,j) + tfail
cjpm            write(28,'(3I3,6E12.4)')i,j,m3,newtim(1,i,j),newcon(1,i,j),
cjpm     &      newtim(2,i,j),newcon(2,i,j),newtim(3,i,j),newcon(3,i,j)
	  end do
c
	  tmin = newtim(1,1,m3)
	  cs(1,m3) = newcon(1,1,m3)
	  tmax = newtim(numitr,ntimes+1,m3)
	  cs(ntimes+1,m3) = newcon(numitr,ntimes+1,m3)
	  do kk=2,numitr
	    if (newtim(kk,1,m3).lt.tmin) then
	      tmin = newtim(kk,1,m3)
	      cs(1,m3) = newcon(kk,1,m3)
cjpm            else if (newtim(kk,1,m3).eq.tmin) then
cjpm              cs(1,m3) = cs(1,m3) + newcon(kk,1,m3)
	    endif
	    if (newtim(kk,ntimes+1,m3).gt.tmax) then
	      tmax = newtim(kk,ntimes+1,m3)
	      cs(ntimes+1,m3) = newcon(numitr,ntimes+1,m3)
cjpm            else if (newtim(kk,ntimes+1,m3).eq.tmax) then
cjpm              cs(ntimes+1,m3) = cs(ntimes+1,m3) +
cjpm     &        newcon(numitr,ntimes+1,m3)
	    endif
	  enddo
	  delta = (tmax - tmin)/(ntimes)
c          delta = (newtim(3,ntimes+1,j) - newtim(1,1,j) )/(ntimes)
c
c pdm  Use TIME array for times to print combined solution.
	  do i=1,ntimes+1
cjpm            time(i,j) = (i - 1.0)*delta + newtim(1,1,j)
	    time(i,M3) = (i - 1.0)*delta + tmin
	  end do

cjpm          do k=1,NDS(j)
cjpm            M3 = M3 + 1
cjpm            cs(1,m3)        = newcon(1,1,m3)
cjpm            pnt(1,m3)       = cs(1,m3)
cjpm            cs(ntimes+1,m3) = newcon(numitr,ntimes+1,m3)
c            cs(ntimes+1,m3) = newcon(3,ntimes+1,m3)
cjpm            pnt(ntimes+1,m3)= cs(ntimes+1,m3)
	    do i=2,NTIMES
cjpm              cs(i,m3) = myintr(time(i,j),newtim,newcon,j,m3,ntimes,
	      cs(i,m3) = myintr(time(i,m3),newtim,newcon,m3,m3,ntimes,
     &                          co, numitr)
c     &                          co)
cjpm              pnt(i,m3) = cs(i,m3)
c              write(28,*) i,j,m3,time(i,j),cs(i,m3)
	    end do
	    m3 = m3 + nds(j)
cjpm          end do
	end do
c
c pdm  Write out combined results, overwriting the contents of the *.WCF file.
c pdm  Note: Appropriateness of flux calculation is unknown.
c pdm  Note: User specified times (NTIMSPEC>0) not currently supported with
c pdm        multiple infiltration rates.
c
	CONCHEDR = .TRUE.
	CALL OUTPUT(FILENM,MED(NMED),NUMCON,IVS,CASN1,NTIMES+1,NDS,TIME,
     +            CS,AL1,B1,ZTOP,ZBOT,ALEN,XMIX,H1,AN5,U,UNITS,NUMSCE,
     +            FLUXHEDR,CONCHEDR,LOCNAM,JSCE,NUMWU,NUMFLX,KTYPE,RIVX,
     +            RIVY,RIVZ)
c
      end if
c pdm
C
C 'end if' below, matches 'if' just inside loop 777 ( JLS )
C
      end if
  777 continue
C*****************************************************************************
C     COMPUTES BOUNDARY CONDITIONS FOR THE EXPOSURE COMPONENT
C*****************************************************************************
222   continue
cjpm  222 CALL COUPLE (UNITS,VI,M,NDS,co,MSOURC,QOLD)
      IF (ISOURC.EQ.4) THEN
	CALL OUTPUT(FILENM,MED(NMED),NUMCON,IVS,CASN1,NTIMES,NDS,TIM,
     +              CS,-99.,-99.,-99.,-99.,-99.,-99.,-99.,-99.,-99.,
     +              UNITS,NUMSCE,FLUXHEDR,CONCHEDR,LOCNAM,JSCE,NUMWU,
     +              NUMFLX,KTYPE,RIVX,RIVY,RIVZ)
      ENDIF
C
   69 IF (IMFLAG .NE. NUMREC) GO TO 68
      IMFLAG=1
      NUMREC=1
   68 CONTINUE
      J=-99
cjpm      WRITE(7,11) J
   11 FORMAT (2X,I5)
   70 CONTINUE
   80 CONTINUE
C*****************************************************************************
C     GETTING FINAL TIME AND COMPUTING FINAL RUN TIME AND PRINTING TO SCREEN
C*****************************************************************************
      NYFLAG = 0
      NMFLAG = 0
      CALL GETDAT( JYR,JMON,JDAY )
      IF( JYR .GT. IYR ) THEN
	NYFLAG = 1
	NYR = (JDAY+(31-IDAY))*86400
      ELSE IF( JMON .GT. IMON .AND. NYFLAG .EQ. 0) THEN
	NMFLAG = 1
	IF( MOD(1900-IYR,4) .EQ. 0.0 ) LMON(2) = 29
	NMON = (JDAY+(LMON(IMON)-IDAY))*86400
      ELSE IF( JDAY .GT. IDAY .AND.
     1            NYFLAG .EQ. 0 .OR. NMFLAG .EQ. 0 ) THEN
	NDAY = (JDAY-IDAY)*86400
      ENDIF
      CALL GETTIM( JHR,JMIN,JSEC,J100TH)
      JJHR = JHR*3600
      IIHR = IHR*3600
      JJMIN = JMIN*60
      IIMIN = IMIN*60
      ITIME = IIHR+IIMIN+ISEC
      JTIME = JJHR+JJMIN+JSEC
      NTIME = (JTIME-ITIME)+NYR+NMON+NDAY
      NHOUR=NTIME/3600
      N=NTIME-3600*NHOUR
      NMIN=N/60
      NSEC=N-60*NMIN
      WRITE(*,'(A,'' DATE IS '',I2,''/'',I2,''/'',I4,
     1          '' --- TIME IS '',I2,'':'',I2,'':'',I2,
     2          '' --- RUN TIME IS '',I3,'' HOURS            '',/,
     3          58X,I3,'' MINUTES'',/,58X,I3,'' SECONDS'')')
     4          CHAR(13),JMON,JDAY,JYR,JHR,JMIN,JSEC,NHOUR,NMIN,NSEC
C
      IF( MFLG.GT.0 ) WRITE(*,'('' Be certain to inspect the maximum'',
     1    '' concentrations given in the *.WLS file!'')')
C*****************************************************************************
C     CLOSE ALL OPENED FILES
C*****************************************************************************
      CLOSE (UNIT=13,STATUS = 'DELETE')
 9999 ENDFILE (UNIT=6)
cjpm      ENDFILE (UNIT=7)
      REWIND (UNIT=5)
      REWIND (UNIT=8)

      CLOSE (UNIT=6)
cjpm      CLOSE (UNIT=7)
      CLOSE (UNIT=8)
      CLOSE (UNIT=9)
      CLOSE (UNIT=11)
      CLOSE (UNIT=5)
      CLOSE (UNIT=28)
      if (ERRORSIG) then RADCONS=0
      else RADCONS=1
      Return 
      END
	
