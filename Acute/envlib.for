C---- ACUTE: ENVLIB ------------------ Version: 19-mAR-98 ---------------
C
C     SUBROUTINE ENVLIB
C
C     This subroutine handles reading of environmental data libraries and
C     storing of parameter values for radionuclides considered in this 
C     case.
C
C     Module of ENVIN of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     GENII Initial version:  4-Aug-88  RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C     Last Revision:         19-mAR-98   ban
C
C--- Parameter Description ---------------------------------------------------
C
C     A        - Input mass number
C     BIOAC1() - Input bioaccumulation factor
C     DUM      - Character input dummy variable 
C     DWIN     - Input drinking water cleanup factor
C     E        - Input element symbol
C     FTRC()   - Input food transfer factor
C     GAMMA()  - Input energy per disintegration 
C     IOFF     - Column offset to select fresh or salt water 
C                bioaccumulation factors, first 4 are salt water
C     LECHIN   - Input leaching rate
C     NC       - Index of the current radionuclide considered for this
C                case
C
C-----  Modification History -------------------------------------------------
C   Date      Who  Description of Modifications
C  ---------  ---  -----------------------------------------------------------
C  29-Jul-97  DLS  Heading and comments modified for use with ACUTE
c  19-MAR-98  BAN  Correct storage of transfer factors - delete ENVPAR.CMN
C
C-----------------------------------------------------------------------------
C
C---- SUBROUTINE CALL --------------------------------------------------------
C
      SUBROUTINE ENVLIB(TPATH)
C
C----- Include statements ----------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'anmpar.CMN'
      INCLUDE 'fodpar.CMN' 
      INCLUDE 'NUCNAM.CMN'
      INCLUDE 'SOLPAR.CMN'
      INCLUDE 'LEACH.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'AQUPAR.CMN'
C
C----- Type/dimension Statements ---------------------------------------------
C
      REAL BIOAC1(8), FTRC(8), DWIN, LECHIN
      INTEGER IOFF, NC
      CHARACTER E*2
      INTEGER TPATH
C
C----- Data Statements -------------------------------------------------------
C
      DATA IOFF /0/
C
C---- Read food transfer coefficients, unit LUN = 8 --------------------------
C
      NC=1
      IFLG1=0
  120 CONTINUE
      LUN = 8
      CLOSE (LUN)
      CALL OPNFIL (4,0,LUN)
C
      READ (LUN,1000,ERR=98,END=99) DUM
      READ (LUN,1000,ERR=98,END=99) DUM
      dum = dum
  121 CONTINUE    
C
        READ (LUN,1000,ERR=98,END=123) E, DVEL, (FTRC(I),I=1,8), 
     .                                 LECHIN
C
        IF (E .EQ. ELT(NC)) THEN
          DPVL(NC) = DVEL
          IF(LCHOPT.LE.1) THEN
            LEACHR(NC) = LECHIN
          ENDIF
          DO 127 ITF = 1, NTF
            BVI(ITF,NC) = FTRC(ITF) 
  127     CONTINUE
          FMI(1,NC) = FTRC(5)
          FMI(2,NC) = FTRC(6)
          FMI(3,NC) = FTRC(7)
          FMI(4,NC) = FTRC(8)
          FMI(5,NC) = FTRC(5)
          FMI(6,NC) = FTRC(7)
          NC = NC+1
          IFLG1 = 0
          IF (NC .GT. NUCTOT) GOTO 125
        ENDIF
        GOTO 121
C
  123   CONTINUE
        IF (IFLG1 .EQ. 0) THEN
          IFLG1 = 1
        ELSE
C         No information this radionuclide
          IFLG1 = 0
          NC = NC + 1
        ENDIF
C
      IF (NC .LE. NUCTOT) GOTO 120
  125 CLOSE (LUN)
C
C---- Read bioaccumulation factor library-------------------------------
C     Only read if current analysis is for surface water, TPATH = 2
C
      IF(TPATH.EQ.2) THEN
         IF (ISALT) THEN
            IOFF = 0
         ELSE
            IOFF = 4
         ENDIF
         NC=1
         IFLG1=0
C
  140    CONTINUE
         LUN = 9
         CLOSE (LUN)
         CALL OPNFIL (4,0,LUN)
         READ (LUN,1000,ERR=98,END=99) DUM
C
  141    CONTINUE    
C
         READ (LUN,1010,ERR=98,END=143) E, (BIOAC1(I),I=1,8), DWIN
C
         IF (E .EQ. ELT(NC)) THEN
            DO IAQ = 1, NAQ
               BIOACF(IAQ,NC) = BIOAC1(IAQ+IOFF)
            END DO  
            DWCLEN(NC) = DWIN          
            NC = NC+1
            IFLG1 = 0
            IF (NC .GT. NUCTOT) GOTO 145
         ENDIF
         GO TO 141
C
  143    CONTINUE
         IF (IFLG1 .EQ. 0) THEN
           IFLG1 = 1
         ELSE
C         No information this radionuclide
           IFLG1 = 0
           NC = NC + 1
         ENDIF
C
         IF (NC .LE. NUCTOT) GOTO 140
  145    CLOSE (LUN)
      ENDIF
C
      RETURN
C
C---- Error Messages ---------------------------------------------------
C
   98 CALL FILERR (2, LUN, 'In ENVLIB')
      NERROR = NERROR + 1
   99 CALL FILERR (3, LUN, 'In ENVLIB')
      NERROR = NERROR +1
      RETURN
C
C---- Format Statements ------------------------------------------------
C
 1000 FORMAT (A2,1X, 10E8.0) 
 1010 FORMAT (A2,8F9.1,F6.1) 
 1020 FORMAT (1X, A2, A6, 2X, 6F8.0)
      END
C
C---- End of Module ENVLIB ---------------------------------------------------
C
