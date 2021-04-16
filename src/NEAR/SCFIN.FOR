C    EXPOS: SCFIN.FOR                Version Date: 31-Aug-97               
C   Copyright 1997 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C      SUBROUTINE SCFIN                                                      *
C                                                                            *
C  Subroutine SCFIN reads concentration data from the SCF file.              *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    31-Aug-96                                               *
C  Last Modified:    31-Aug-97      DLS                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: NEAR               
C     Called by: NEAR 
C     Calls: HEADIN
C     Common blocks referenced: DEVICE, FNAMES
C
C==== Significant Parameter Designation and Description ======================
C
C Parameter Set/
C  Name     Used Type     Location   Parameter Description
C  ------- ----- -------  --------  -------------------------------------
C  NSCF     U    INT      DEVICE    Logical unit for WCF file
C  STYPE    S    INT      Argument  Soil type: 1 - surface, 2 - deep, or
C                                        3 - waste package
C  SRCNAME  U    CHAR*32  Argument  Name of media source
C  NUMNUC   S    INT      Argument  Number of contaminants in medium source 
C==== Modification History ===================================================
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C   31-Aug-97      DLS  Initial programming started
C   20 Nov 2003    BAN  Revise format for update to FRAMES specifications for SCF
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE SCFIN(STYPE,EXPNAM,NUMNUC)
C
C---- Include Statements for Parameter and Common Declarations ---------------
C      
      INCLUDE 'CONIN.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'FNAMES.CMN'
      INCLUDE 'PARMTR.PAR'
C
C---- Variable Type Declarations ---------------------------------------------
C
C      REAL AX
      CHARACTER*6 AU,SOURCE
      CHARACTER*12 CIDIN,TU,CU
      CHARACTER*32 EXPNAM
      CHARACTER*32 MEDNAM,LOCNAM, ALLNAME,SOILNAME, SEDNAME
      CHARACTER*14 MEDTYPE
      CHARACTER*14 SOILNAM,DEEPNAM,WASTENAM
      CHARACTER*20 CNAMIN 
      INTEGER STYPE
      LOGICAL SEQI   ! logical function to compare n characters of strings
C                      A and B,  = SEQI(A,B,N)
      LOGICAL SKIP   ! skip flag for media in .WCF
C
C---- Data Statements --------------------------------------------------------
C
        DATA SOILNAM /'Surface Soil  '/
        DATA DEEPNAM /'Deep Soil     '/
        DATA WASTENAM/'Waste Package '/
        DATA SOURCE  /'Source'/
	  DATA ALLNAME /'all'/
	  DATA SOILNAME /'Soil'/
	  DATA SEDNAME /'Sediment'/
C
C---- Initialize parameter values --------------------------------------------
C
C-----  Read heading information from Soil Flux File -------------------------
C
      CALL HEADIN(NMEDIA)
C
C----- Loop on the number of media in the SCF file ---------------------------
C
      DO IMED = 1,NMEDIA
C        READ(NSCF,*) MEDNAM,MEDTYPE,NLOC
C        WRITE(NELS,200) IMED,MEDNAM,MEDTYPE,NCON
C 200    FORMAT(' Medium number:',I3,' information.'/'  Location name: ',
C     .     A32/' Medium type: ',A14,' Number of constituents (parents) '
C     .     'at this location ',I4)
        READ(NSCF,*) MEDNAM,MEDTYPE,XX,AU,YY,AU,ZZ,AU,NCON
        WRITE(NELS,200) IMED,MEDNAM,MEDTYPE,NCON
 200    FORMAT(' Medium number:',I3,' information.'/'  Location name: ',
     .     A32/' Medium type: ',A14,' Number of constituents (parents) '
     .     'at this location ',I4)
C
C---- Loop on number of locations for this medium ----------------------------
C  *** the ability to have more than one 'location' went away with the new specs ***
C
C        DO IL = 1,NLOC
C
C---- Read line giving media location type (e.g., source, pasture, etc) ------
C     The type must be "source" or skip
C
C          READ(NSCF,*) LOCNAM,XX,AU,YY,AU,ZZ,AU,NCON
          SKIP = .TRUE.
          VOLWP = XX*YY*ZZ
          IF(VOLWP.LE.0.) VOLWP = 1.0
          AU = AU
          NM = LENWORD(MEDNAM,32)
c          IF(SEQI(MEDNAM,SRCNAME,32).AND.SEQI(LOCNAM,SOURCE,6)) THEN   ! found media of interest
c
c  Updated specs say type must be "soil" or "sediment" and source must 
c  refer to either 'all' or 'the target(this module name)'
c
          IF((SEQI(MEDNAM,ALLNAME,32) .OR. SEQI(MEDNAM,EXPNAM,32))
     ..AND. (SEQI(MEDTYPE,SOILNAME,4) .OR. SEQI(MEDTYPE,SEDNAME,7)))THEN   ! found media of interest
            IF(STYPE.EQ.1) THEN  ! and surface soil medium
              SKIP = .FALSE.
              write(NELS,2005) SOILNAM(1:12),mednam(1:NM)
 2005         format(' Found ',A,' source for ',A)
            ELSEIF(STYPE.EQ.2) THEN  ! and deep soil medium
              write(NELS,2005) DEEPNAM(1:9),mednam(1:NM)
              SKIP = .FALSE.
            ELSEIF(STYPE.EQ.3) THEN  ! and waste package medium
              write(NELS,2005) WASTENAM(1:13),mednam(1:NM)
              SKIP = .FALSE.
            ENDIF
          ENDIF
          IF(SKIP) THEN
c
c---- read past unused data for this media ----------------------------------
c
          DO IC = 1,NCON
C
C---- Read identification line for constituent ------------------------------
C
           READ(NSCF,*) CNAMIN,CIDIN,TU,CU,NTIMES,NPG
           CIDIN=CIDIN
           TU = TU
           CU = CU
           CNAMIN = CNAMIN
C----- Read concentration data for parent constituent -----------------------
           DO IT = 1,NTIMES
             READ(NSCF,*) T
           END DO
           T = T
C---- If there are progeny, read data for each progeny ----------------------
           IF(NPG.GT.0) THEN
             DO IP = 1,NPG
C
C---- Read identification line for constituent ------------------------------
C
              READ(NSCF,*) CNAMIN
C----- Read concentration data for progeny constituent ----------------------
              DO IT = 1,NTIMES
                READ(NSCF,*) T
              END DO
            END DO
          ENDIF
        END DO
        ELSE     !  Found medium, now at start of concentration data
          NUMNUC = NCON
        ENDIF    !  End of loop on skiping concentration data for unused loc.
C
C----- END OF PROCESSING FOR CURRENT MEDIUM SET IN SCF ----------------------
C
        IF(.NOT.SKIP) RETURN
C       END DO
      END DO
C
C----- Error, medium set not found ------------------------------------------
c
        NS = LENWORD(EXPNAM,32)
        WRITE(NERR,1115) EXPNAM(1:ns)
 1115   FORMAT(' Error in SCFIN, soil medium set not recognized: '/
     .   ' for source name ',A)
        RETURN
C
C----- END OF MODULE SCFIN --------------------------------------------------
C
      END

