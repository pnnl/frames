C    EXPOS: WCFIN.FOR                Version Date: 3-Jan-03               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C      SUBROUTINE WCFIN                                                      *
C                                                                            *
C  Subroutine WCFIN reads concentration data from the WCF file.              *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    20-Dec-96                                               *
C  Last Modified:    3-Jan-03      BAN                                       *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EXPOS  main program
C     Called by: EXPOS
C     Calls: HEADIN
C     Common blocks referenced: DEVICE, FNAMES
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     NWCF       U      INT    DEVICE    Logical unit for WCF file
C==== Modification History ===================================================
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C   20-Dec-96      DLS  Initial programming started
C   15-May-97      DLS  Made revisions for current WCF format
C   29-May-97      DLS  Redirected screen output to files
C   3-Jan-03       BAN  Minor addition for user input of SETNAME = "All"
c   12/15/09       BAN  Allow progeny to have different number of time periods
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE WCFIN(WTYPE,SRCNAME,NUMNUC)
C
C---- Include Statements for Parameter and Common Declarations ---------------
C      
      INCLUDE 'DEVICE.CMN'
C      INCLUDE 'OPTG.CMN'
      INCLUDE 'FNAMES.CMN'
      INCLUDE 'PARMTR.PAR'
C      INCLUDE 'SWCON.CMN'
C      INCLUDE 'EXINFO.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      CHARACTER*12 CIDIN,TU,CU
      CHARACTER*32 SRCNAME
      CHARACTER*32 MEDNAM, XXall
      CHARACTER*14 MEDTYPE
      CHARACTER*14 AQUIF,SWATR,TYPENAM
      CHARACTER*14 WTYPE
      CHARACTER*20 CNAMIN 
      INTEGER TPATH
      LOGICAL SEQI, SKIP   ! skip flag for media in .WCF
C
C---- Data Statements --------------------------------------------------------
C
C       DATA GWC/'GW'/,SWC/'SW'/
       DATA AQUIF/'Aquifer       '/
       DATA SWATR/'Surface Water '/
	 DATA XXall/'all'/
C---- Initialize parameter values --------------------------------------------
C
C----- Determine if groundwater or surface water medium is sought ------------
C
        IF(SEQI(WTYPE,AQUIF,14)) THEN
C        IF(WTYPE.EQ.AQUIF) THEN
          TYPENAM = AQUIF
          TPATH = 1
        ELSE IF(SEQI(WTYPE,SWATR,14)) THEN
C        ELSE IF(WTYPE.EQ.SWATR) THEN
          TYPENAM = SWATR
          TPATH = 2
        ELSE
          WRITE(NERR,1114) WTYPE
 1114     FORMAT(' Error in WCFIN water type.  Must be Aquifer or',
     .           ' Surface Water, found'
     .    ': ',A14)
        ENDIF
C
C-----  Read heading information from Water Flux File ------------------------
C
      CALL HEADIN(TPATH,NMEDIA)
C
C----- Loop on the number of media in the WFF file ---------------------------
C
      DO IMED = 1,NMEDIA
        READ(NWCF,*) MEDNAM,MEDTYPE,NCON
        WRITE(NELS,200) IMED,MEDNAM,MEDTYPE,NCON
c
 200    FORMAT(' Medium number:',I3,' information.'/'  Location name: ',
     .     A32/' Medium type: ',A14,' Number of constituents (parents) '
     .     'at this location ',I4)
        SKIP = .TRUE.
C        IF(SEQI(MEDNAM,SRCNAME,32)) THEN   ! found media of interest
        IF(SEQI(MEDNAM,SRCNAME,32) .or. SEQI(MEDNAM,XXall,32)) THEN   ! found media of interest
          IF(SEQI(MEDTYPE,TYPENAM,14)) THEN  ! and correct transport medium
C          IF(MEDTYPE.EQ.TYPENAM) THEN  ! and correct transport medium
            SKIP = .FALSE.
            write(NELS,2005) mednam,WTYPE,SRCNAME
 2005       format(' Found source: ',A20,' for ',A13,1X,A20)
          ENDIF
        ENDIF
        IF(SKIP) THEN
c
c---- read past radionuclide data for this media -----------------------------
c
        DO IC = 1,NCON
C---- Read identification line for constituent ------------------------------
           READ(NWCF,*) CNAMIN,CIDIN,TU,CU,NTIMES,NPG
           CIDIN=CIDIN
           TU = TU
           CU = CU
           CNAMIN = CNAMIN
C----- Read release data for parent constituent -----------------------------
           DO IT = 1,NTIMES
             READ(NWCF,*) T
           END DO
           T = T
C---- If there are progeny, read data for each progeny ----------------------
           IF(NPG.GT.0) THEN
             DO IP = 1,NPG
C
C---- Read identification line for constituent ------------------------------
C
              READ(NWCF,*) CNAMIN,CIDIN,TU,CU,mTIMES
C----- Read release data for progeny constituent ----------------------------
              DO IT = 1,mTIMES
                READ(NWCF,*) T
              END DO
            END DO
          ENDIF
        END DO
        ELSE     !  Found medium, now at start of concentration data
C          write(NELS,1113)
C 1113     format(' Found medium
          NUMNUC = NCON
        ENDIF    !  End of loop on skiping concentration data for unused loc.
C
C----- END OF PROCESSING FOR CURRENT MEDIUM SET IN WCF ----------------------
C
        IF(.NOT.SKIP) RETURN
      END DO
C
C----- Error, medium set not found ------------------------------------------
c
        WRITE(NERR,1115) WTYPE,SRCNAME
 1115   FORMAT(' Error in WCFIN, waterborne medium set not found: ',a14/
     .   '   for source name ',A32)
        RETURN
C
C----- END OF MODULE WCFIN --------------------------------------------------
C
      END

