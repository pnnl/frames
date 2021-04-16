C    ACUTE: WCFIN.FOR                Version Date: 9-Jan-03               
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
C  Last Modified:    9-Jan-03      BAN                                       *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: ACUTE              
C     Called by: ACUTE
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
C     Date    Who  Modification Description
C  ---------  ---  ------------------------------------------------------
C  20-Dec-96  DLS  Initial programming started
C  15-May-97  DLS  Made revisions for current WCF format
C  29-May-97  DLS  Redirected screen output to files
C  29-Jul-97  DLS  Revised heading and comments for use with ACUTE
C  3-Jan-03   BAN  Minor revision for user input of MEDNAME = 'all'
c  12/15/09   BAN  Allow progeny to have different number of time periods
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE WCFIN(TPATH,SRCNAME,NUMNUC)
C
C---- Include Statements for Parameter and Common Declarations ---------------
C      
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'FNAMES.CMN'
      INCLUDE 'PARMTR.PAR'
C
C---- Variable Type Declarations ---------------------------------------------
C
      CHARACTER*12 CIDIN,TU,CU
      CHARACTER*32 SRCNAME
      CHARACTER*32 MEDNAM, XXall
      CHARACTER*14 MEDTYPE
      CHARACTER*14 SWATR,TYPENAM
C     CHARACTER*14 AQUIF,SWATR,TYPENAM
C      CHARACTER*14 WTYPE
      CHARACTER*20 CNAMIN 
      INTEGER TPATH
      LOGICAL SEQI
      LOGICAL SKIP   ! skip flag for media in .WCF
C
C---- Data Statements --------------------------------------------------------
C
c       DATA AQUIF/'Aquifer       '/
       DATA SWATR/'Surface Water '/
       DATA N32/32/, N14/14/
	 DATA XXall/'all'/
C
C----- Determine if surface water medium is sought ---------------------------
C
        IF(TPATH.EQ.2) THEN
          TYPENAM = SWATR
        ELSE
          WRITE(NERR,1114) TPATH
 1114     FORMAT(' Error in TPATH water type.  Must be Surface Water',
     .           ' (2), found',I3)
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
 200    FORMAT(' Medium number:',I3,' information.'/'  Location name: ',
     .     A32/' Medium type: ',A14,' Number of constituents (parents) '
     .     'at this location ',I4)
        SKIP = .TRUE.
C        IF(SEQI(MEDNAM,SRCNAME,N32)) THEN   ! found media of interest
        IF(SEQI(MEDNAM,SRCNAME,N32) .or. SEQI(MEDNAM,XXall,32)) THEN   ! found media of interest
          IF(SEQI(MEDTYPE,TYPENAM,N14)) THEN  ! and correct transport medium
            SKIP = .FALSE.
            write(NELS,2005) mednam,TPATH,SRCNAME
 2005       format(' Found source: ',A20,' for TPATH ',I2,1X,A20)
          ENDIF
        ENDIF
        IF(SKIP) THEN
C
C---- Read past radionuclide data for this media -----------------------------
C
        DO IC = 1,NCON
C
C---- Read identification line for constituent ------------------------------
C
           READ(NWCF,*) CNAMIN,CIDIN,TU,CU,NTIMES,NPG
           CIDIN=CIDIN
           TU = TU
           CU = CU
           CNAMIN = CNAMIN
C
C----- Read release data for parent constituent -----------------------------
C
           DO IT = 1,NTIMES
             READ(NWCF,*) T
           END DO
           T = T
C
C---- If there are progeny, read data for each progeny ----------------------
C
           IF(NPG.GT.0) THEN
             DO IP = 1,NPG
C
C---- Read identification line for constituent ------------------------------
C
              READ(NWCF,*) CNAMIN,CIDIN,TU,CU,mTIMES
C
C----- Read release data for progeny constituent ----------------------------
C
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
      WRITE(NERR,1115) TPATH,SRCNAME
 1115 FORMAT(' Error in WCFIN, waterborne medium set not found '
     .       ' for TPATH =',I2,' for source name ',A)
      RETURN
      END
C
C----- END OF MODULE WCFIN --------------------------------------------------
C

