C   ACUTE:      RMDGET.FOR             Version Date: 25-mAR-98              
C   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
C****************************************************************************C                             
C
C  Subroutine RMDGET  reads radionuclide decay chain data and stores values  *
C                    common block DECAY  arrays.                             *
C                                                                            *
C  Written by:       Dennis Strenge                                          *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    03-Nov-95                                               *
C  Last Modified:    25-Mar-98  BAN                                    *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: GENII/SWATER
C     Called by: SWATER
C     Calls: None
C     Common blocks referenced: DECAY, DEVICE
C
C==== Significant Parameter Designation and Description ======================
C
c  Parameters in argument list
c  Name        Type             Purpose
c  ---------   ------    ---------------------------------------------------
c   LURMD      Int*2     Logical unit for input of radionuclide chain decay
c                        data.  Either DOSCAL.IN or master RMDLIB.DAT.
c---------------------------------------------------------------------------
c
c  Major Local Parameters:
c     Name          Type                  Purpose
c   ------------   ------  ---------------------------------------------------
C   DIN           Char*12   Name of radionuclide for which data is sought.
C
c --------------------------------------------------------------------------
c
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     30-Oct-96    DLS  Revised nuclide name to Char*12
C  29-Jul-97  DLS  revised comments for ACUTE component
C  25-mAR-98  BAN  Fixed day/yr units on AL for progeny
c  30 oCT 2001 BAN Added NoChM, ChNam to call list to transfer to DBreadA
c   5 Dec 06   BAN Capture branched chain members/order list
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE RMDGET(DIN,DINP,NPROG,DOUT,ICROS,NOCHM,chnam)
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'OPT.CMN'
      INCLUDE 'AQUPAR.CMN'
      INCLUDE 'DECAY.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'DEVICE.CMN'
C
C==== TYPE/DIMENSION Statements ==============================================
C
      REAL*8 AL2
      CHARACTER*12 DIN(9),DINP(9),DOUT(9),DN, BLANK, CHNAM(chainlen)
      CHARACTER*80 CTITLE
C      INTEGER MAXEX, MAXIMP
      CHARACTER*2 ENAME
      INTEGER IE,ICROS(9)
      LOGICAL SAVE
      DATA BLANK/'            '/
      DATA DN/'            '/
C
C==== Variable Declarations ==================================================
C
c     None
C
C==== DATA Statements ========================================================
C
C     None
c
      AL2 = ALOG(2.)
        AL(1) = 0.0
        ICROS(1) = 0
        DOUT(1) = BLANK
        ANDFAC(1) = 0.
      DO IE=2,9
        ANDFAC(IE) = 0.
        ICROS(IE) = 0
        AL(IE) = 0.0
        CHNAM(IE) = BLANK
        DOUT(IE) = BLANK
        IFRM(1,IE)= 0
        DK(1,IE)  = 0.0
        IFRM(2,IE)= 0
        DK(2,IE)  = 0.0
      END DO
      SAVE = .FALSE.
C
C---- Set Andelman factor for parent -----------------------------------------
C
c      IF(SHINDR) THEN
        ENAME = DIN(1)(1:2)
        IF(ENAME.EQ.'RN') THEN
          ANDFAC(1) = ANDKRN
        ELSE
          ANDFAC(1) = ANDKR
        ENDIF
c      ENDIF
C
C-----  Read title lines from RMDLIB.DAT -------------------------------------
C
      READ(NRMD,1000) CTITLE
      IF(SAVE) WRITE(NERR,1000) CTITLE
      READ(NRMD,1020) 
 1000 FORMAT(A80)
 1020 FORMAT(9X,2I3)
C
C-----  Read first data line for next parent entry ----------------------------
C
  5     READ(NRMD,1002,END=999) DN, NOCHM, NIMPIN
 1002   FORMAT(A8,1X,2I3)
C
C----- Test for end of RMDLIB.DAT file ---------------------------------------
C
      IF(NOCHM.GT.9) GO TO 55
C
C----- Read second data line for current parent, decay data. -----------------
C
        READ(NRMD,1003) DN, HALF
 1003   FORMAT(19X,A8,E10.2) 
C
C-----  Is current radionuclide the desired radionuclide? --------------------
C
        IF(DN.EQ.DIN(1)) THEN
          SAVE = .TRUE.           ! Found
          CHNAM(1) = DN
          ICROS(1) = 1
          NUC = NOCHM
          NONUC = NOCHM
          DOUT(1) = DN
        ELSE
          SAVE = .FALSE.          ! Not found
        ENDIF
C
C- If half life is positive, then calculate radiological decay constant ------
C  and desired radionuclide is found.
C
        IF(SAVE) THEN
          IF(HALF.GT.0.) THEN
            AL(1) = DAYYR*AL2/HALF
          ELSE
            WRITE(NERR,3000) HALF
 3000       FORMAT(' ERROR IN RMDLIB, NON-POSITIVE HALF LIFE FOR ',
     .      'PARENT RADIONUCIDE',1PE12.4)
          ENDIF
        ENDIF
C
         IF(NIMPIN.GT.0) THEN      !  Implicit daughters
            DO 10 ID = 1, NIMPIN
C
C-----         Skip record for each implicit daughter ------------------------
C            
              READ(NRMD,1004) 
 1004         FORMAT(19X,2A4,14X,F7.5) 
C
  10        CONTINUE
C
         END IF
C
         IF(NOCHM.GT.1) THEN       !  Explicit daughters
            DO IE = 2, NOCHM
C
C-----          Read record for explicit daughter ----------------------------
C
              IF(SAVE) THEN      ! Read and save data
                READ(NRMD,1005) CHNAM(IE), HALF,
     .            IFRM(1,IE), DK(1,IE), IFRM(2,IE),
     .            DK(2,IE)
 1005             FORMAT(19X,A8,E10.3,2X,I2,F7.5,I2,F7.5) 
c                  IF(SHINDR) THEN
C
C---- Set Andelman factor for progeny ----------------------------------------
C
                    ENAME = DINP(IE)(1:2)
                    IF(ENAME.EQ.'RN') THEN
                      ANDFAC(IE) = ANDKRN
                    ELSE
                      ANDFAC(IE) = ANDKR
c                    ENDIF
                  ENDIF
                IF(HALF.GT.0.) THEN
                  AL(IE) = DAYYR*AL2/HALF
                ELSE
                  WRITE(NERR,3000) HALF
                ENDIF
C
C- find position of explicit chain member in input list, set cross index -----
C
                IF(NPROG.GT.0) THEN
c                  DO IP = 1,NPROG  : changed 05 Dec 06 BAN
                  do ip = 1, nochm - 1
                    IF(DINP(IP).EQ.CHNAM(IE)) THEN
                      DOUT(IP+1) = CHNAM(IE)
                      ICROS(IE) = IP+1
                    ENDIF
                  END DO
                  IF(ICROS(IE).LE.0) THEN
                    WRITE(NELS,1001) IE,CHNAM(IE),(din(i),i=1,nprog+1)
 1001      FORMAT(' Warning, explicit chain member ',i2,', ',a8,
     .            ' not in input chain list'/3x,9a8)
                  ENDIF
                ENDIF
              ELSE               ! Skip data
                READ(NRMD,1005)  
              ENDIF
C
            END DO
C
         END IF
        IF(SAVE) GO TO 55
      GO TO 5
C
C----- Write error message, parent not found in RMDLIB.DAT ------------------
C
 999  WRITE(NERR,3001) DN
 3001 FORMAT(' POLLUTANT NOT FOUND ',A12)
C
C----- Rewind RMDLIB.DAT to be ready for next call --------------------------
C
  55  REWIND(NRMD)
C
C----- Normal return --------------------------------------------------------
C
c
c  ---  get all lists in the same order: BAN 05 Dec 06
c
        do i=1,nochm
	    dinp(i) = chnam(i)
	    dout(i) = chnam(i)
	  end do
c
      RETURN
      END        
C
C---------- End of file RMDGET.FOR ------------------------------------------
C
