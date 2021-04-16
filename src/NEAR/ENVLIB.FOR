C-----------------------------------------------------------------------------
C
      SUBROUTINE ENVLIB(NOCHM)
C
C     This subroutine handles reading of environmental data libraries and
C     storing of parameter values for radionuclides considered in this 
C     case.
C
C     Module of ENVIN of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Version of 4-Aug-88  RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C
C-----------------------------------------------------------------------
C
C     A        - Input mass number
C     BIOAC1() - Input bioaccumulation factor
C     DUM      - Character input dummy variable 
C     DWIN     - Input drinking water cleanup factor
C     E        - Input element symbol
C     FTRC()   - Input food transfer factor
C     GAMMA()  - Input energy per disintegration 
C     IOFF     - Column offset to select fresh or salt water 
C                bioaccumulation factors
C     LECHIN   - Input leaching rate
C     NC       - Index of the current radionuclide considered for this
C                case
C
C====== Modification History ============================================
C     Date         Who  Modification Description
C   ----------     ---  ------------------------------------------------------
C  20 May 97       DLS  Added text heading information.
C-----------------------------------------------------------------------------
      INCLUDE 'RAD.CMN'
C      INCLUDE 'ENVPAR.CMN'
c      INCLUDE 'NUCNAM.CMN'
      INCLUDE 'FODPAR.CMN'
      INCLUDE 'SOLPAR.CMN'
      INCLUDE 'ANMPAR.CMN'
      INCLUDE 'AQUPAR.CMN'
      INCLUDE 'AIRPAR.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'LEACH.CMN'
      INCLUDE 'EXINFO.CMN'
C
      REAL BIOAC1(8), FTRC(8), DWIN, LECHIN
c      REAL BIOAC1(8), FTRC(8), DWIN, LECHIN, GAMMA(6)
      INTEGER IOFF, NC
      CHARACTER E*2, DUM*8
c      CHARACTER E*2, A*6, DUM*8
      DATA IOFF /0/
C
C---- Read food transfer coefficients---------------------------------------
C
      NC=1
      IFLG1=0

  120 CONTINUE
      LUN = 8
      CLOSE (LUN)
      CALL OPNFIL (4,0,LUN)

      READ (LUN,1000,ERR=98,END=99) DUM
      READ (LUN,1000,ERR=98,END=99) DUM
      DUM = DUM

  121 CONTINUE    

        READ (LUN,1000,ERR=98,END=123) E, DVEL, (FTRC(I),I=1,8), 
     .                                 LECHIN

        IF (E .EQ. ELT(NC)) THEN
          DPVL(NC) = DVEL
          IF(LCHOPT.LE.1) LEACHR(NC) = LECHIN
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
C
C---- Read bioaccumulation factor library-------------------------------
C

      IF (ISALT) THEN
        IOFF = 0
      ELSE
        IOFF = 4
      ENDIF

      NC=1
      IFLG1=0

  140 CONTINUE
      LUN = 9
      CLOSE (LUN)
      CALL OPNFIL (4,0,LUN)
      READ (LUN,1000,ERR=98,END=99) DUM

  141 CONTINUE    

        READ (LUN,1010,ERR=98,END=143) E, (BIOAC1(I),I=1,8), DWIN

        IF (E .EQ. ELT(NC)) THEN
          DO 144 IAQ = 1, NAQ
            BIOACF(IAQ,NC) = BIOAC1(IAQ+IOFF)
  144     CONTINUE
          DWCLEN(NC) = DWIN          
          NC = NC+1
          IFLG1 = 0
          IF (NC .GT. NUCTOT) GOTO 145
        ENDIF
        GOTO 141

  143   CONTINUE
          IF (IFLG1 .EQ. 0) THEN
          IFLG1 = 1
        ELSE
C         No information this radionuclide
          IFLG1 = 0
          NC = NC + 1
        ENDIF

      IF (NC .LE. NUCTOT) GOTO 140
  145 CLOSE (LUN)

C---- Read gamma energies by group -------------------------------------

      NC=1
      IFLG1=0
c
c  160 CONTINUE
c      LUN = 38
c      CLOSE (LUN)
c      CALL OPNFIL (4,0,LUN)
c
c  161 CONTINUE    
c
c        READ (LUN,1020,ERR=98,END=163) E, A, (GAMMA(I),I=1,6)
c
c        IF (E .EQ. ELT(NC) .AND. A .EQ. AW(NC)) THEN
c          DO 167 I = 1, 6
c            EDIS(I,NC) = GAMMA(I) 
c  167     CONTINUE
c
c          NC = NC+1
c          IFLG1 = 0
c          IF (NC .GT. NUCTOT) GOTO 165
c        ENDIF
c        GOTO 161
c
c  163   CONTINUE
c        IF (IFLG1 .EQ. 0) THEN
c          IFLG1 = 1
c        ELSE
C         No information this radionuclide
c          IFLG1 = 0
c          NC = NC + 1
c        ENDIF
C
c      IF (NC .LE. NUCTOT) GOTO 160
c  165 CLOSE (LUN)
C
C---- Set leach rate constants for this decay chain --------------------------
C
      IF(LCHOPT.EQ.2.OR.LCHOPT.EQ.3) THEN
C        NPRG = NPROG(INUC)
        DO IP = 1,NOCHM
           LEACHR(IP) = ALEACH(IP)
        END DO
      ENDIF
      RETURN

C---- Error Messages ---------------------------------------------------

   98 CALL FILERR (2, LUN, 'In ENVLIB')
      RETURN
   99 CALL FILERR (3, LUN, 'In ENVLIB')
      RETURN
C---- Format Statements ------------------------------------------------

 1000 FORMAT (A2,1X, 10E8.0) 
 1010 FORMAT (A2,8F9.1,F6.1) 
 1020 FORMAT (1X, A2, A6, 2X, 6F8.0)

C-----------------------------------------------------------------------
      END
