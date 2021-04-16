C   SWATER:     RMDGET.FOR             Version Date: 25 mAR 1998              
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
C  Last Modified:    25 Mar 98  ban                                          *
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
c    Name      Type             Purpose
c  ---------   ------    ---------------------------------------------------
c   LRMD       Int*2     Logical unit for input of radionuclide chain decay
c                        data.  
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
C    Date     Who  Modification Description
C  ---------  ---  -----------------------------------------------------------
C  30-Oct-96  DLS  Revised nuclide name to Char*12
C  25-Feb-97  DLS  Added common block OPT
C  30-May-97  DLS  Added N32 and N12 for variable word checking in SEQ
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE RMDGET(DIN,INUC,nocm)
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'DECAY.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'SWINFO.CMN'
C
C==== DIMENSION Statements ===================================================
C
      REAL*8 AL2
      CHARACTER*12 DIN, DN, BLANK
      CHARACTER*32 B32
      CHARACTER*80 CTITLE
      INTEGER IE,N32,N12
      LOGICAL SAVE
      LOGICAL SEQ
      EXTERNAL SEQ
      DATA BLANK/'            '/
      DATA B32/'                                '/
      DATA DN/'            '/
      DATA N32/32/,N12/12/
C
C==== Variable Declarations ==================================================
C
c     None
C
C==== DATA Statements ========================================================
C
C     None
c*****************************************************************************
C
c
      NOCM = 0
      AL2 = dLOG(2.0d0)
        AL(1) = 0.0
      DO IE=2,20
        AL(IE) = 0.0
        CHNAM(IE) = BLANK
        IFRM(1,IE)= 0
        DK(1,IE)  = 0.0
        IFRM(2,IE)= 0
        DK(2,IE)  = 0.0
      END DO
      SAVE = .FALSE.
C
C  Read title lines from RMDLIB.DAT
C
      READ(NRMD,1000) CTITLE
      IF(SAVE) WRITE(NSLS,1000) CTITLE
      READ(NRMD,1020) 
 1000 FORMAT(A80)
 1020 FORMAT(9X,2I3)
C
C  Read first data line for next parent entry
C
  5     READ(NRMD,1002,END=999) DN, NOCHM, NIMPIN
 1002   FORMAT(A8,1X,2I3)
C
C   Test for end of RMDLIB.DAT file
C
      IF(NOCHM.GT.20) GO TO 55
C
C  Read second data line for current parent, decay data.
C
        READ(NRMD,1003) DN, HALF
 1003   FORMAT(19X,A8,E10.2) 
C
C  Is current radionuclide the desired radionuclide?
C
        IF(DN.EQ.DIN) THEN
          SAVE = .TRUE.           ! Found
          CHNAM(1) = DN
          NUC = NOCHM
          nocm = nochm - 1
        ELSE
          SAVE = .FALSE.          ! Not found
        ENDIF
C
C  If half life is positive, then calculate radiological decay constant
C  and desired radionuclide is found.
C
        IF(SAVE) THEN
          IF(HALF.GT.0.) THEN
            AL(1) = AL2/HALF
          ELSE
            WRITE(NERR,3000) HALF
            NERROR = NERROR + 1
 3000       FORMAT(' ERROR IN RMDLIB, NON-POSITIVE HALF LIFE FOR ',
     .      'PARENT RADIONUCIDE',1PE12.4)
          ENDIF
        ENDIF
C
         IF(NIMPIN.GT.0) THEN      !  Implicit daughters
            DO 10 ID = 1, NIMPIN
c
C              Skip record for each implicit daughter
c            
              READ(NRMD,1004) 
 1004         FORMAT(19X,2A4,14X,F7.5) 
C
  10        CONTINUE
c
         END IF
c
         IF(NOCHM.GT.1) THEN       !  Explicit daughters
            DO IE = 2, NOCHM
c
C               Read record for explicit daughter
c
              IF(SAVE) THEN      ! Read and save data
                READ(NRMD,1005) CHNAM(IE), HALF,
     .            IFRM(1,IE), DK(1,IE), IFRM(2,IE),
     .            DK(2,IE)
 1005             FORMAT(19X,A8,E10.3,2X,I2,F7.5,I2,F7.5) 
              IF(.NOT.SEQ(CNAM(IE,INUC),B32,N32))
     .                        CNAM(IE,INUC)=CHNAM(IE)
              IF(.NOT.SEQ(CID(IE,INUC),BLANK,N12))
     .                        CID(IE,INUC) = CHNAM(IE)
                IF(HALF.GT.0.) THEN
                  AL(IE) = AL2/HALF
                ELSE
                  WRITE(NERR,3000) HALF
                  NERROR = NERROR + 1
                ENDIF
              ELSE               ! Skip data
                READ(NRMD,1005)  
              ENDIF
c
            END DO
c
         END IF
        IF(SAVE) GO TO 55
      GO TO 5
c
 999  WRITE(NERR,3001) DN
      NERROR = NERROR + 1
 3001 FORMAT(' POLLUTANT NOT FOUND ',A12)
  55  REWIND(NRMD)
      RETURN
      END        
C-------------  End file RMREAD.FOR -------------------------
