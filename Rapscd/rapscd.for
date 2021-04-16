Copyright 1994 - 2001 Battelle Memorial Institute.  All Rights Reserved.
C     This is a sub code for rapscd
      Program rapscdstub
c$debug
        call rapscd()
      end
Copyright 1989-2001 Battelle Memorial Institute.  All Rights Reserved.
      SUBROUTINE RAPSCD()
C      IMPLICIT NONE
      INCLUDE 'ATMOS.INA'
      INCLUDE 'DPSTN.INA'
      INCLUDE 'DRYD.INA'
C
C     RAPSCD 042194 BATTELLE PNL, RICHLAND WA
C
C     MODIFICATION LOG/VERSION 
c     Comment Table of RAPSCD I/O Unit Numbers 
c     Number  Variable  I/O  File Extension
c       1  
c       2       IS       I   SOU
c       3       IM       I   MET
c       4        -       O   AIR
c       5        -       -   Screen Default?
c       6      IOL       O   ALS
c       7        -       O   ACF    
c       8        -       I   ID
c       9       IT       I   TOP, BLD
c      96       io       O   AFF
c
c
      INTEGER WS,WD,ST 
      INTEGER souleng, metleng, jfdleng
      INTEGER*2 JHR,JMIN,JSEC,J100TH

      REAL     ar_siz, ar_zfac

      CHARACTER*30 FILSOU,FILMET,FILTOP,FILJFD,filacf
      CHARACTER*20 DDZ
      CHARACTER*20 VERN,SN
      character*12 SYSDATE
      character*10 SYSTIME
      COMMON /VRN/ VERN
      CHARACTER RECIN*80,RECTYP,SOUFNM*8,METFNM*8,NAME*8
      DIMENSION AV4(6),RSIZ1(12,7,16),RSIZ2(12,7,16),
     +RSIY1(12,7,16),RSIY2(12,7,16),AIRCC(12)

      LOGICAL LCHL
C
          VERN = '(8 May-2001) RAPSCD'
c         VERN = '() RAPSCD'
c         VERN = '() RAPSCD'
c         VERN = '() RAPSCD'
c         VERN = '() RAPSCD'
C
C     RAPSCD.FOR
C
C
C          RAPS/MEPAS CLIMATOLOGICAL DEPOSITION PROGRAM
C          --------------------------------------------
C
C       RAPSCD Developed by J.G.Droppo and J.Buck 1987/1988
C       UPDATED JULY 1983 JG DROPPO, JAN 1985, MAY-JULY 1986
C       AUGUST 1986/JGD, RAPS 1987, MEPAS 1988
C       UPDATED March 20, 2001 /JGD/
C        1. Use particle radii from AFF file
C        2. Change ICLASS 6 to compute with old class 3 radius
C        3. Old revision notes deleted
c        4. iprt=3 logic for max conc removed
c       May 08, 2001 Index fix/JGD
c                    Comment line fix in coinit/JGD
c                    New Version Compile Date /JGD
c
c
C     -------------------------------------------------------------
C     ICLASS =   SURFACE EMISSION CONTAMINANT CLASS (1 TO 7)  (I)
C           0 -  initialize arrays
C           1 -  particles (default radius = 7.5um)
C           2 -  particles (default radius = 3.0um)
C           3 -  particles (default radius = 0.3um)
C           4 -  gas with moderate deposition rate
C           5 -  non-depositing gas
C           6 -  gas emission/particle deposition
c                (default radius=0.3m), and also
C                ambient particle deposition !JGD March 15, 2001
C           7 -  gas with fast deposition rate (zero surface resistance)
C     RATE  =   EMISSION FACTOR OF CONTAMINANT IN ICLASS       (F)
C                   1 -  g/kg (pCi/g) in surface soil
C                   2 -  g/kg (pCi/g) in surface soil
C                   3 -  g/kg (pCi/g) in surface soil
C                   4 -  g/s (pCi/s) of gas
C                   5 -  g/s (pCi/s) of gas
C                   6 -  g/s (pCi/s) of gas
C                   7 -  g/s (pCi/s) of gas
C----------------------------------------------------------------
C*********************************************************************
c       zlim, upper limit for zo dispersion influences is set to 80cm
c       representing a relatively rough woodland forest
C
C
C       DEFINE LOGICAL UNITS AND FILE NAMES
C
      CALL GETTIM (JHR,JMIN,JSEC,J100TH) 
        zlim=80.
        IS=2
        IM=3
        IT=9
        IEOF=0
        IOL=6

C
        OPEN (UNIT=8,FILE='FACIL.ID',STATUS='OLD')
10      READ (8,6) RECIN
             READ (RECIN,7) RECTYP
             IF (RECTYP.NE.'2') GOTO 10
c
        READ (RECIN,8) NUMWU
c
6       FORMAT (A80)
7       FORMAT (1X,A1)
8       FORMAT (3X,I3)
c

C     MAIN LOOP FOR EACH RELEASE UNIT
C
      DO 300 IW=1,NUMWU
C

C     WASTE TO RELEASE April 5, 1990 JGD
      READ (8,6) RECIN
      READ (RECIN,9) SOUFNM, METFNM
9     FORMAT (3X,A8,2X,A8)

         souleng = LEN_TRIM(SOUFNM)
         FILSOU = SOUFNM(1:souleng)//'.SOU'
         metleng = LEN_TRIM(METFNM)
         FILMET = METFNM(1:metleng)//'.MET'
         FILTOP = METFNM(1:metleng)//'.TOP'
         FILBLD = METFNM(1:metleng)//'.BLD'
         FILJFD = METFNM(1:metleng)//'.JFD'
 
C
         OPEN(IOL,FILE=SOUFNM(1:souleng)//'.ALS',STATUS='UNKNOWN')
         OPEN(4,FILE=SOUFNM(1:souleng)//'.AIR',STATUS='UNKNOWN')
c
c        Test file with raw air conc. output
         if(iprt.eq.9) then
           FILACF = SOUFNM(1:souleng)//'.ACF'
           OPEN(7,FILE=FILACF,STATUS='UNKNOWN')
         endif

        CALL HDR1(VERN,IOL,sysdate,systime)
        write(iol,*) 'Run Name = ',soufnm
c
C       Initialize Variables...
C
C           DEFINE WIND HEIGHT FOR EQUALITY AT ALL SITES
            HSTD=200.
c           FDISP=1/(2*PI)^.5; FWID=2*PI/16; FQ=8./PI*FWID
            FDISP=.39894
            FWID=.392699
            FQ=2.546479*FWID
            sr2=1/sqrt(2.)
            afac=1.-sr2
            IOPT=1
            IBLD=0
C           DEFINE PASQUILL ZO AND HEIGHT
            ZPAS=10.
            HBASE=2.0
            HPAS=HBASE
            NUMC=0
            NDIST=12
            NTY=7
            nts=1
            CALL coINIT
            CALL SETINIT
            GASSCV=0.0
            CALL AFFREAD(IOL,SOUFNM,RSG,RDS,GASRES,GASSCV)
c           !March 11,2001 JGD read AFF file
            IEOF=0
C           DEFINE DEPOSITION REFERENCE HEIGHT
c           Depost. height is set to 10 m to be consistent with AIRDRY
            IZ = 10
C
C       Read emission unit data...
C
        OPEN(IS,FILE=FILSOU,STATUS='OLD')
        CALL RDSCE(IS,SOILF1,SOILF2,ICP1,ICG2,IEOF,SOUFNM,souleng)
c        if(iprt.eq.0) iprt=4 ! March 15, 2001 JGD
        IF(NSR.NE.9) THEN
             SN='DISPERSION'
             OPEN(IM,FILE=FILMET,STATUS='OLD')
             CALL RDMET(IM,IEOF)
             CALL RDHTZ(IM,IEOF)
             READ(IM,'(15X,A8)') FILJFD
             jfdleng = LEN_TRIM(FILJFD)
             CLOSE(IM)   
             OPEN(IM,FILE=FILJFD(1:jfdleng)//'.JFD',STATUS='OLD')
             CALL RDJFD(IM,IEOF)
             CLOSE(IM)
             OPEN(IT,FILE=FILTOP,STATUS='OLD',IOSTAT=IER)
             NUMC=0
             NDIST=12
             IF(IER.NE.0) THEN
                 WRITE(IOL,*) ' RAPSCD ERROR / FILE NOT FOUND: ',FILTOP
                 WRITE(  6,*) ' RAPSCD ERROR / FILE NOT FOUND: ',FILTOP
                 goto 9999
             ELSE
                 WRITE(IOL,*) ' TERRAIN INPUT FILE: ',FILTOP
                 CALL RDTOP(IT)
             ENDIF
             CALL INDIS
             close(it)
             OPEN(IT,FILE=FILBLD,STATUS='OLD',IOSTAT=IER)
             CALL RDBLD(IT,IER)
             close(it)
                 BHGT=2.439
                 BFLT=1.000
             CALL PPTFR(PFC)
c            switch channel dir by 180 deg to matchc wind direction arrays
             ACHAN=ACHAN+8
             IF(ACHAN.GT.16) ACHAN=ACHAN-16
             ICLL=ACHAN+8
             IF(ICLL.GT.16) ICLL=ICLL-16
             NDIST=12
             WRITE(6,*)
C            SET INITIAL DISPERSION LENGTHS, ARWX AND ARWZ
             IF (NSR.EQ.0.OR.NSR.EQ.2) THEN
C               POINT/STACK RELEASE...
                ARWZ=RSTK*RSTK
                ARWX=0.0
             ELSE
C               AREA RELEASE...
C               CHANGE TO RADIUS/April 5, 1990/JGD
                ARWXP=SQRT(AREAS/3.1415)
                ARWXG=SQRT(AREAG/3.1415)
                ARWZ=0.0
             ENDIF
          ELSE
             SN='AIR-AS-SOU'
             PNAME=SOUFNM//'     '
C            Use variables from SETINIT
c            Jgd/February 8, 1998
c             XMIN=.1
C             XMAX=75.
          ENDIF
C         DEFINE OUTPUT PRODUCT INDEX
          IF(NSR.EQ.0.OR.NSR.EQ.2) ILC(3)=1
          IF(NSR.EQ.1.OR.NSR.EQ.3) ILC(1)=1
          NAME= SOUFNM
C     WRITE HEADER TO DATA OUTPUT FILE...
C
      WRITE(4,*) 'SOU LABL=',PNAME,SN
      WRITE(4,'(4I5,5X,A,1X,A,A)') NDIST, NUMC, NSR, NTY, VERN,
     1 SYSDATE, SYSTIME
       WRITE(4,1001) XMIN,XMID,XMAX,AREAS,AREAG,RAREA
       WRITE(4,1001) (QNN(IR),IR=nts,NTY)
C
C     START MAIN COMPUTATION LOOP...
C
C     DEPOSITION TO SURFACE TYPES
      CALL AIRDRY
      WRITE(6,*) '      AVERAGE DEPOSITION VELOCITY ARRAY, cm/s'
      WRITE(6,*)
     +'D'
      WRITE(6,*)'P          Bare  Low          Resi-  '
      WRITE(6,*)'#  Water   Land  Grass  Wheat dential Trees'
      DO 40 J = 1,6
C     DEP V =  1  / ( AIR RESISTANCE + SURFACE RESISTANCE)
c40    AV4(J) = 1./  (  1./AVD(4,J)   +     RES_GAS       ) 
40    AV4(J) = 1./  (  1./AVD(5,J)   +     RES_GAS       )!20MAR01/JGD
      WRITE(6,1002) ' 1', (100.*AVD( 1,J),J=1,6),
     & ' PartRad=',RDS(1),'u, ',RSG(1),' cm^3/g'
      WRITE(6,1002) ' 2', (100.*AVD( 2,J),J=1,6),
     & ' PartRad=',RDS(2),'u, ',RSG(2),' cm^3/g'
      WRITE(6,1002) ' 3', (100.*AVD( 3,J),J=1,6),
     & ' PartRad=',RDS(3),'u, ',RSG(3),' cm^3/g'
      WRITE(6,1002) ' 4', (100.*AV4(J),J=1,6),  ' Gas, Reactive '
      WRITE(6,1002) ' 5', (0.0,J=1,6),          ' Gas, Nondepositing'
c      WRITE(6,1002) ' 6 ', (100.*AVD(3,J),J=1,6),' Gas as Particle'
c      WRITE(6,1002) ' 7', (100.*AVD(4,J),J=1,6),' Gas Maximum Rate'
c      20 Mar 01 /JGD/ logic for case 6 added
      WRITE(6,1002) ' 6', (100.*AVD(4,J),J=1,6),
     & ' Ambient Part./Gas as Part.'
      WRITE(6,1002) ' 7', (100.*AVD(5,J),J=1,6),' Gas Maximum Rate'
      WRITE(6,*) ' '
c      WRITE(4,'(1X,6G10.3)') ((AVD(I,J),J=1,6),I=1,4)
      WRITE(4,'(1X,6G10.3)') ((AVD(I,J),J=1,6),I=1,5)
C
C     CONCENTRATION/DEPOSITION PATTERNS
c     rtot is for debuging purposes
      rtot=0.0
      DO 250 IR=nts,NTY
      IF(NSR.NE.9) THEN
c     IF(IR.EQ.6) GOTO 243! March 15, 2001 JGD compute ambient particle
        do 230 iii = 1,673
           cmm(iii,1)=0.0
           cmm(iii,2)=0.0
           cmm(iii,3)=0.0
230        continue
        QN=QNN(IR)
        RADIUS=RDS(IR)*.0001
        RHOP=RSG(IR)
        CALL INIT
        IF (IR.LT.4) THEN
           ARWX=ARWXP
        ELSE
           ARWX=ARWXG
        ENDIF
C       compute ns and ew lengths
        XEW=2.*ARWX/(1+RAREA)
        XNS=XEW*RAREA
        RFRT=0.0
        TM = TDAYS*24.*3600.

        AL=COWET(ALCO,RAINRT,IR)
        IF(IR.eq.4) then  ! March 15, 2001 JGD Update to use GASSCV if given
          IF(GASSCV.gt.0.0) AL=GASSCV
        endif

        NSEAS=NSEAS+1
        if(iprt.eq.7) write(6,*)
     +         ' st,IDS,x(IDS),ywid,chi,chix,fdry,fdryx'
        if(iprt.eq.6) write(6,*)
     +  ' st,IDS,x(IDS),arw,ywid,siz,uave,zfac,chip,fdry,ywid'
        if(iprt.eq.5) write(6,*)' wd,st,ws,IDS,uave,u(ws),',
     +  'siz,ywid,siy,zro,chi,chip,fdry,',
     +   'zfac,tmet,tpas,RF,have,hcom,hmix'
c
        DO 80 ISTV=1,7
        IF (ISTV.LE.6) THEN
          ST=ISTV
        ELSE
          ST=6
        ENDIF
        IF(ST.GT.4) THEN
            HMIX=5000.
        ELSE
            HMIX=(HMSTAB+(HMUNST-HMSTAB)*(4.-ST)/3.)*.8
        ENDIF
        IST=ST
        CALL CHANNEL(IR, IST, ARWX, ARWZ, RHOP, RADIUS)
        DO 80 WD=1,16
C       COMPUTE RADIUS FOR CURRENT DIRECTION
        CALL DIRRAD(WD,XNS,XEW,ARW)
        DO 80 WS=1,7
            IF(RF(ISTV,WS,WD).LE.0.0) THEN
                 NULCASE=0
            ELSE
                 NULCASE=1
            ENDIF
            IF(NULCASE.EQ.0.AND.WS.EQ.1) NULCASE=3
            IF(NULCASE.EQ.0) GOTO 80
C
C           COMPUTE MEASURED WIND PROFILE PARAMETERS...
C
            ZRO=ZOS
C           Limit used for Zo corrections
C           ZRO is anemon. local roughness length
            IF (ZRO.GT.ZLIM) ZRO=ZLIM
            IF (ZRO.LE.0.0) THEN
C           OVER WATER
                 CALL UWAT(ST,U(WS),ANHGT,USTAR,ZRO,ZDISP)
            ELSE
C           OVER LAND
                 CALL ULAN(ST,U(WS),ANHGT,USTAR,ZRO,ZDISP)
            ENDIF
            CALL UBAR(ST,WSMES,HSTD,USTAR,ZRO,ZDISP)
            CALL UBAR(ST,USMES,HPAS,USTAR,ZRO,ZDISP)
C
            IF (NSR.EQ.0.OR.NSR.EQ.2) THEN
C           POINT SOURCE RELEASE
              H=HSTK+
     &           FNPLMRS(st, rstk, vstk, u(ws), tstk, tmean )
     
              IF(H.LT.2.) H=HPAS
            ELSE
C           AREA GROUND LEVEL RELEASE
               H=HPAS
            ENDIF
            IF(H.GT.HSTD) THEN
                HMAS=HSTD
            ELSE
                HMAS=H
            ENDIF
C           COMPUTE EQUIVL PASQUILL WIND PARAMETERS
            CALL ULAN(ST,WSMES,HSTD,USPAS,ZPAS,ZDPAS)
            CALL UBAR(ST,UPAS,HMAS,USPAS,ZPAS,ZDPAS)

c           Calculate Sigma Z and Reflection Term at edge of Area 

c           if arw less than 1 meter use ARWZ (point source radius)
c           or 100.0 if ARWZ is less than 1 meter               
            IF( ARW .LT. 1.0 ) THEN
               IF( ARWZ .LT. 1.0 ) THEN
                  TMET = 100.0 / upas
               ELSE
                  TMET = ARWZ / upas 
               ENDIF
            ELSE         
               TMET = ARW/UPAS
            ENDIF
            CALL SIGZT(TMET,ST,UPAS,AR_SIZ,SIY)
            HCOM = HBASE
            CALL REFLEC(-5,5,AR_SIZ,HMAS,HCOM,HMIX,AR_ZFAC)

C
C           DEFINE CHANNEL STATUS, LCHL
            LCHL=.FALSE.
            IF( st .GT. 4 .AND. u(ws) .LT. 6 ) LCHL = .TRUE.
            IF(XCHAN.LE.0.0.OR.YCHAN.LE.0.0) LCHL=.FALSE.
            IF (LCHL) THEN
               XCDIS=XCD(WD,ACHAN,ARW)
            ELSE
               XCDIS=0.0
            ENDIF
            DO 70 IDS=1,NDIST
C           KDS IS INPUT DATA DISTANCE INDEX (1 TO 6)
            CALL INDEXD(X(IDS),KDS)
            IF(ICNL(IDS).EQ.3) LCHL=.FALSE.
            IF(H.LE.HBASE) THEN
C            GROUND LEVEL RELEASE
               HCUR=HBASE
               HAVE=HBASE
             ELSE
C            ELEVATED RELEASE
C            COMPUTE PLUME HEIGHT OVER LOCAL TERRAIN
C            (BASED ON HANNA ET AL 1982, P 85)
                H2=H/2.0
                IF(ST.LE.4) THEN
C               UNSTABLE AND NEUTRAL CONDITIONS...
                    IF(HTER(WD,KDS).GT.0.0) THEN
                         IF(H2.GT.HTER(WD,KDS)) THEN
                              HCUR = H - HTER(WD,KDS)
                         ELSE
                              HCUR = H - H2
                         ENDIF
                    ELSE
                         HCUR=H
                    ENDIF
                    IF(HCUR.LT.2.) HCUR=2.0
                    HAVE=HCUR
                    IF(ST.EQ.3.AND.WS.EQ.3) THEN
                      IF(WD.EQ.13) HUSTL = H
                    ENDIF
                ELSE  
C               STABLE CONDITIONS
                    IF(IDS.GT.1) HLAS=HCUR
                    IF(HTER(WD,KDS).GT.0.0) THEN
                         HCUR=H-HTER(WD,KDS)
                    ELSE
                         HCUR=H
                    ENDIF
C                   HCUR FOLLOWS TERRAIN AFTER IMPINGEMENT
                    IF(HCUR.LT.2.) HCUR=2.0
                    IF(IDS.EQ.1) THEN
                        HAVE=HCUR
                        HLAS=HCUR
                    ELSE
C                       HCUR NEVER INCREASES IN VALUE
                        IF(HCUR.GT.HLAS) HCUR=HLAS
C                       COMPUTE DISTANCE WEIGHTED AVERAGE HEIGHT
                        HAVE = HCUR
                    ENDIF
                ENDIF
                    IF(ST.EQ.5.AND.WS.EQ.3) THEN
                      IF(WD.EQ.13) HSTBL = H
                    ENDIF
C
             ENDIF
C
C                COMPUTE WIND PROFILE PARAMETERS...
C
                 ZRO=Z0(WD,KDS)
C                Use limit for Zo corrections
C                Here ZRO is local roughness length at computation area
                 IF (ZRO.GT.ZLIM) ZRO=ZLIM
                 IF (ZRO.EQ.0.0) THEN
C                OVER WATER
                      CALL UWAT(ST,WSMES,HSTD,USTAR,ZRO,ZDISP)
                 ELSE
C                OVER LAND
                      CALL ULAN(ST,WSMES,HSTD,USTAR,ZRO,ZDISP)
                 ENDIF
C                HMAS IS HEIGHT FOR TOTAL PLUME MOVEMENT
                 CALL UBAR(ST,UPLM,HMAS,USTAR,ZRO,ZDISP)
                 CALL UBAR(ST,UPLP,HMAS,USPAS,ZPAS,ZDPAS)
C
C                COMPUTE DISPERSION VALUES...
C                COMPUTE TIME OF TRAVEL OF PLUME
                    IF(IDS.EQ.1) THEN
                        TMET=X(1)/UPLM
                        TPAS=X(1)/UPLP
                        HX2=HMAS-HMIX/2.0
                     ELSE
                        xdelt=x(IDS)-x(IDS-1)
                        TMET=TMET+xdelt/UPLM
                        TPAS=TPAS+XDELT/UPLP
                     ENDIF
                     UAVE=X(IDS)/TMET
                     UAVP=X(IDS)/TPAS
c
777              FORMAT(A,2I2,5G10.3)
C
           CALL SIGZT(TMET,ST,UPAS,SIZ,SIY)
C          STORE SIGZ AND ZO CORR RATIO FOR OUTPUT
           IF(WS.EQ.2) THEN
           IF(IR.EQ.1) THEN
C               COMPUTE NET DISPERSION CORRECTION RATIO
                CALL SIGZT(TPAS,ST,UPAS,RSIZZ,rsiyy)
                RSIZ1(IDS,ST,WD)=SIZ/RSIZZ
                RSIZ2(IDS,ST,WD)=UAVE/UAVP
                RSIY1(IDS,ST,WD)=SIY/RSIYY
                RSIY2(IDS,ST,WD)=UAVE/UAVP
           ENDIF
           ENDIF
           IF(NSR.EQ.0.OR.NSR.EQ.2) then
                SIZ=SQRT(SIZ*SIZ+ARWZ)
                SIY=SQRT(SIY*SIY+ARWZ)
            ENDIF
C
C          COMPUTE HEIGHT OF PLUME MASS CENTER...
C
c          Keep wind speed height same with distance
c          HMAS=HMIX/2.+HX2*HMIX/(HMIX+0.50*SIZ)
           IF(HMAS.GT.HSTD) HMAS=HSTD

           HCOM = HBASE
           IF(HCOM.LT.HBASE) HCOM=HBASE
           IF(HAVE.GT.HMIX) HMIX = HAVE
           CALL REFLEC(-5,5,SIZ,HAVE,HCOM,HMIX,ZFAC)
C
C          DRY REMOVAL AS FUNCTION OF DISTANCE...
C
           IF(IR.EQ.5) THEN
             FDRY=1.0
             FWET=1.0
             VD=0.0
             VS=0.0
           ELSEIF (ids.lt.i100m) then
             FDRY=1.0
             FWET=1.0
             VD=0.0
             VS=0.0
           ELSE
             UWS=U(WS)
             CALL DRYVEL(UWS,WD,KDS,ST,IZ,RHOP,RADIUS,ZRO)
             IF(X(IDS).LE.ARW) THEN
                 DREM=100.0/AR_SIZ*AR_ZFAC
             ELSE
                 IF( ids .NE. 1 ) THEN
c                     IF( X(IDS-1) .LE. ARWX ) THEN
c                        xdelt = x(ids) - arwx + 100.0
                     IF( X(IDS-1) .LE. ARW ) THEN
                        xdelt = x(ids) - arw + 100.0
                     ELSE
                        xdelt=x(IDS)-x(IDS-1)
                     ENDIF
                 
                     DREM=DREM+XDELT/SIZ*ZFAC
                 ELSE
                     DREM = X(ids)/SIZ*ZFAC
                 ENDIF
             ENDIF
             FDRY=EXP(-.7979*DREM*VD/UAVE)
           ENDIF

C          DEFINE DISPERSION WIDTH
C          WITH OFFSET FOR CHANNEL FLOW
c           YWID=X(IDS)-XCDIS
c            IF(YWID.LT.0.0) THEN
C           SWITCH DIRECTION
            IF(X(IDS).LT.ARW) THEN
               IF(XCDIS+X(IDS).GT.0) THEN 
                 ICL=ACHAN
               ELSE
                 ICL=ICLL
              ENDIF
            ELSE
                 ICL=ACHAN
            ENDIF
C
            YWID=ABS(YWID)+YCHAN/FWID
            IF(YWID.LT.XMIN) YWID=XMIN
                YWID=X(IDS)*FWID
c            ENDIF
C
C           COMPUTE OUTDOOR SECTOR AVERAGE AIR CONC
                CHI = FDISP*FDRY/UAVE/YWID/SIZ*ZFAC
C           COMPUTE OUTDOOR PEAK AIR CONC
                CHIP = FDRY/(3.1415*UAVE*SIY*SIZ)*ZFAC
            if(iprt.eq.6) then
              if(ir.eq.3.or.ir.eq.5) then
              if(st.eq.4)
     +          write(6,'(2I2,5F9.1,f8.3,3g10.3)')
     +          st,IDS,x(IDS),arw,ywid,siz,uave,zfac,chip,fdry
              endif
            endif
            if(iprt.eq.7) then
              if(ir.eq.1.or.ir.eq.4) then
              if(st.eq.4) then
                chix=chi/Fdry
                if(x(IDS).lt.arw) then
                   fdryx=1.-arw*chix*ywid*vd
                else
                   if(IDS.eq.1) then
                      fdryx=1-x(1)*chix*ywid*vd
                   else
                      if(x(IDS-1).lt.arw) then
                          xdelt=x(IDS)-arw
                      else
                          xdelt=x(IDS)-x(IDS-1)
                      endif
                   fdryx=fdryx*(1.-xdelt*chix*ywid*vd)
                   endif
                endif
                if(fdryx.lt.0.) fdryx=0.0
                write(6,'(2I2,4F9.1,g9.3,2f8.6)')
     +          st,IDS,x(IDS),ywid,arw,chi,chix,fdry,fdryx
              endif
              endif
            endif
            if(iprt.eq.5) then
              if (st.eq.4.and.IDS.eq.7) then
                write(6,'(1X,4i1,12g8.2,f8.3,3f8.0)')
     +          wd,st,ws,IDS,uave,u(ws), siz,ywid,siy,zro,chi,chip,
     +          fdry,zfac,tmet,tpas,RF(ISTV,WS,WD),have,hcom,hmix
              endif
            endif
C
C           COMPUTE INDOOR AIR CONC
            IF(ZFAC.LE.0) THEN
                CHB=FDRY/UAVE/YWID/BLDHGT
            ELSE
                BAC = SIZ/FDISP/ZFAC
                IF(BLDHGT.GE.BAC) THEN
                    CHB=CHI*BAC/BLDHGT*BLDFLT(IR)
                ELSE
                    CHB=CHI*BLDFLT(IR)
                ENDIF
            ENDIF
C
C           WASHOUT FOR NEUTRAL STABILITY AND FIRST 4 SPEED CLASSES...
            IF(IR.EQ.5) GOTO 50
            IF(ST.NE.4) GOTO 55
            IF(WS.GT.5) GOTO 55
C                    CONCENTRATION WITH WET AND DRY DEPOSITION...
                     FWET=EXP(-AL*X(IDS)/UAVE)
C                    AMOUNT OF  WET DEPOSITION ...
C                    IF THERE ARE NO "D" CLASS CASES, THEN PFC WILL BE
C                    ZERO AND NO WET DEPOSITION WILL OCCUR
                     WO=AL*FWET*FQ/(UAVE*YWID)
                     RFR=PFC*RF(ISTV,WS,WD)
                     RFDC=RF(ISTV,WS,WD)-RFR
                     RFRT=RFRT+RFR
                GOTO 60
50                   FDRY=1.0
55                   RFDC=RF(ISTV,WS,WD)
                     RFR=0.0
                     CHIR=0.0
                     FWET=1.0
60              CONTINUE
            IF(LCHL) THEN
C              CHANNEL FLOW
               ACON(ICL,IDS)=ACON(ICL,IDS)+CA(IDS)*RFDC*FD(IDS)
               ACOB(ICL,IDS)=ACOB(ICL,IDS)+CB(IDS)*RFDC*FD(IDS)
               ADRY(ICL,IDS)=ADRY(ICL,IDS)+CD(IDS)*RFDC*FD(IDS)
               IF(ICNL(IDS).EQ.2) THEN
                ACON(WD,IDS)=ACON(WD,IDS)+CHI*RFDC*(1.-FD(IDS))
                ACOB(WD,IDS)=ACOB(WD,IDS)+CHB*RFDC*(1.-FD(IDS))
                ADRY(WD,IDS)=ADRY(WD,IDS)+CHI*RFDC*VD*(1.-FD(IDS))
C               July 4, 1993/JGD
                chip=CM(IDS)
               ENDIF
              ELSE
C              NO CHANNEL FLOW
               ACON(WD,IDS)=ACON(WD,IDS)+CHI*(RFDC+RFR*FWET)
               ACOB(WD,IDS)=ACOB(WD,IDS)+CHB*(RFDC+RFR*FWET)
               AWET(WD,IDS)=AWET(WD,IDS)+RFR*WO
               ADRY(WD,IDS)=ADRY(WD,IDS)+CHI*(RFDC+RFR*FWET)*VD
             ENDIF
c           endif
c          lines added for debug use /allows stop for rfdc>0.0
           if(rfdc.gt.0.0) then
             rtot=rtot+rfdc
           endif
C          SECTION TO BUILD ACUTE CONC PROB. DISTB.
           if(xmin.gt.0.0) then
              I100m = 1
           else
              I100m = 3
           endif
           if (RF(ISTV,WS,WD).gt.0.0) then
               IF (H.EQ.HPAS) THEN
C                 GROUND LEVEL RELEASE
                  IF (IDS.EQ.I100m) THEN
C                    100m - FIRST DISTANCE for acute
C                    PUT CHIP INTO CMM
                     iiis=1
                     do 64 iii = 1,673
c                       check for no value (=0.0)
                        if(cmm(iii,1).gt.0.0) then
c                          check for position 
                           if (chip.lt.cmm(iii,1)) iiis=iii+1
                        endif
64                   continue
                     IF(IIIS.LE.673) THEN
                        do 65 iii=673,iiis+1,-1
                           cmm(iii,1)=cmm(iii-1,1)
                           cmm(iii,2)=cmm(iii-1,2)
65                         continue
C                       PUT CURRENT CHIP INTO ARRAY
                        cmm(iiis,1) = chip
                        cmm(iiis,2) = RF(ISTV,WS,WD)
                     ENDIF
                  ENDIF
               ENDIF
c
c           Store Concentations as function of Distance

            AIRCC(IDS)=CHIP
C           SECTION TO STORE MAXIMUM CONCENTRATIONS
            IF(ISTV.EQ.7) THEN
C             ONLY CONSIDER "G" IF "G" DATA ARE NONZERO
c              IF(GSUM.EQ.0.0) CHIP=0.0
C             DO NOT CONSIDER "G" 
               CHIP=0.0
            ENDIF
            IF (IDS.GE.I100m) THEN
              IF (CHIMAX(WD) .LE. CHIP) THEN
C                FOR EACH DIRECTION
C                REPLACE STORED VALUES IF NEW CONC IS LARGER
                 CHIMAX(WD)= CHIP
                 RMWS(WD)= UAVE
                 IMST(WD)= ST
                 IMWS(WD)= WS
                 IMDS(WD)= IDS
                 RMZO(WD)= ZRO
              ENDIF
              IF ( CHIMAX(17) .LT. CHIP ) THEN
C                ALL CASE VALUE
C                REPLACE STORED VALUES IF NEW CONC IS LARGER
                 CHIMAX(17)= CHIP
                 RMWS(17)= RMWS(WD)
                 IMST(17)= ST
                 IMWS(17)= WS
                 IMDS(17)= IDS
                 RMZO(17)= ZRO
              ENDIF
            ENDIF
            chip=0.0
            endif
70          CONTINUE
            if(iprt.eq.9) then
c             Write out concentrations at
c             end of distance Loop/JGD/Feb. 6, 1998
              Write(7,*) IR,ISTV,WD,WS,rfdc,(aircc(ids),IDS=1,NDIST)
            endif
c
80          CONTINUE
        DO 90 WD=1,16
        DO 90 IDS=1,NDIST
C           DEFINE WET, DRY, AND TOTAL DEPOSITION
            IF(IR.EQ.5) THEN
                 AWET(WD,IDS)=0.0
                 ADRY(WD,IDS)=0.0
                 ATOT(WD,IDS)=0.0
            ELSE
                 AWET(WD,IDS)=AWET(WD,IDS)*TM
                 ADRY(WD,IDS)=ADRY(WD,IDS)*TM
                 ATOT(WD,IDS)=AWET(WD,IDS)+ADRY(WD,IDS)
            ENDIF
90    CONTINUE

      IF(ILC(IR).LT.8) THEN
       IPL=1
       WRITE(6,*)
       WRITE(6,*)
       WRITE(6,*)
       WRITE(6,9500) 'Atmospheric Computation for Deposition Case ',IR
       WRITE(IOL,*)
       IF(IR.LE.3) THEN
          WRITE(IOL,8000) 'Particles with radius = ',RDS(IR),' um'
       ELSE IF(IR.EQ.4) THEN
          WRITE(IOL,8000) 'Gas with moderate deposition rate'
       ELSE IF(IR.EQ.5) THEN
          WRITE(IOL,8000) 'Non-depositing gas'
       ELSE IF(IR.EQ.6) THEN
          WRITE(IOL,8000) 'Ambient Particulates & Gas on Particles'
       ELSE IF(IR.EQ.7) THEN
          WRITE(IOL,8000) 'Fast depositing gas'
       ENDIF
       IF(IR.LT.4.AND.ILC(IR).EQ.1) THEN
        WRITE(IOL,*) 'Site = ',SOUFNM,', ','Met  = ',METFNM
        IF(NSR.EQ.0.OR.NSR.EQ.2) THEN
          DDZ='Release Height (m) ='
          DDX=HSTK
          WRITE(IOL,8000) 'Plume Height-C (m) = ',HUSTL
          WRITE(IOL,8000) 'Plume Height-E (m) = ',HSTBL
        ELSE
          DDZ='Release Areas  (m2) = '
          DDX=AREAS
        ENDIF
        WRITE(IOL,8000) DDZ,DDX
       ENDIF
c       IF(IPRT.GE.3) THEN  ! March 15, 2001 JGD
         WRITE(IOL,*)' '
         WRITE(IOL,*)
     +  ' Acute exposure outputs are listed in the following two'
         WRITE(IOL,*)
     +  ' tables for review.  The acute exposure data in these'
         WRITE(IOL,*)
     + ' two tables should be considered preliminary in nature.'
         WRITE(IOL,*)' '
         WRITE(IOL,*)
         WRITE(IOL,*)
     +  'Table of maximium acute normalized air concentrations based on'
         WRITE(IOL,*)
     +  'site data and an assumed range of possible wind speeds:'
         WRITE(IOL,*)
         WRITE(IOL,*)
     +  '---------- Acute Exposure Data -------------------------------'
         WRITE(IOL,*)
     +  ' Direction  Maximum              Stability  Wind   Wind  Local'
         WRITE(IOL,*)
     +  '  Sector     X/Q       Distance   Class     Speed  Speed   Zo '
         WRITE(IOL,*)
     +  '    -        (s/m3)      (m)        #        m/s   Group  (cm)'
c        
9001     format (2x,a4,4x,1pe10.2,0pf10.0,3x,i5,f11.1,i5,5x,g7.2)
         do wd=1,16
           if(imds(wd).gt.0) then ! may8, changed (16) to (wd) jgd
              write (iol,9001)
     +        di(wd),chimax(wd),X(imds(wd)),imst(wd),Rmws(wd),imws(wd),
     +        rmzo(wd)
           endif ! imds
         enddo ! wd 
         IF( imds(17) .GT. 0 ) THEN
               write (iol,9001)
     +    ' Max', chimax(17),(X(imds(17))),imst(17),Rmws(17),imws(17),
     &     rmzo(17)
         ENDIF
c       ENDIF ! iprt   ! March 15, 2001 JGD
C
       chimax(17) = 0.0
       imst(17)   = 0
       Rmws(17)   = 0.0
       WRITE(IOL,*)

c      SECTION TO DEFINE 95% AND 50% ACUTE CONCENTRATIONS
       IF(H.EQ.HPAS) THEN
C         ONLY DONE FOR GROUND LEVEL RELEASES IN THIS VERSION
          cmm(1,3)= cmm(1,2)
          ITFND=0
          WRITE(IOL,*)
     +   'Probability distribution table of acute normalized air'
          WRITE(IOL,*)
     +   'concentrations based on site characteristics and wind-'
          WRITE(IOL,*)
     +   'stability joint frequency data:' 
         WRITE(IOL,*)
          do 220 iii = 1,672 ! 20Mar01/JGD/iprt logic removed from loop
            cmm(iii+1,3)= cmm(iii+1,2) + cmm(iii,3)
            if (ITFND.eq.0) then
              if(cmm(iii+1,3).gt.0.05) then
                 write(iol,'(A,1pe10.2,A)')
     +           ' Near Surface Release  95th % Acute X/Q at 100m = ',
     +           cmm(iii,1),' s/m3'
                 ITFND=1
              endif
            elseif (ITFND.eq.1) then
              if(cmm(iii+1,3).gt.0.10) then
                 write(iol,'(A,1pe10.2,A)')
     +           '                       90th % Acute X/Q at 100m = ',
     +           cmm(iii,1),' s/m3'
                 ITFND=2
              endif
            elseif (ITFND.eq.2) then
              if(cmm(iii+1,3).gt.0.50) then
                 write(iol,'(A,1pe10.2,A)')
     +           '                       50th % Acute X/Q at 100m = ',
     +           cmm(iii,1),' s/m3'
                 ITFND=3
              endif
            elseif (ITFND.eq.3) then
              if(cmm(iii+1,3).gt.0.90) then
                 write(iol,'(A,1pe10.2,A)')
     +           '                       10th % Acute X/Q at 100m = ',
     +           cmm(iii,1),' s/m3'
                 ITFND=4
              endif
            elseif (ITFND.eq.4) then
              if(cmm(iii+1,3).gt.0.95) then
                 write(iol,'(A,1pe10.2,A)')
     +           '                        5th % Acute X/Q at 100m = ',
     +           cmm(iii,1),' s/m3'
                 ITFND=5
               endif
            endif
220       continue

         IF(IPRT.EQ.5)
     +     write (iol,'(1x,i3,1x,1pe10.2,0pg10.3,f10.5)')
     +     (iii,(cmm(iii,jjj),jjj=1,3),iii=1,673)
         ENDIF
         WRITE(IOL,*) ' '
         WRITE(IOL,*) ' '
         WRITE(IOL,*)
     +   'NORMALIZED CHRONIC OUTDOOR AIR CONCENTRATIONS (s/m3)'
      ELSE
         IPL=0
      ENDIF
      CALL OUTRES(ACON,IOL,IPL)
      DO 240 IDS=1,NDIST
240         WRITE(4,6000)IR+10,(ACON(WD,IDS),WD=9,16),
     +                   IDS,  (ACON(WD,IDS),WD=1,8)
      IF(IBLD.GT.0) THEN
         WRITE(IOL,*)
         WRITE(IOL,*)
         WRITE(IOL,*) 'NORMALIZED INDOOR AIR CONCENTRATIONS (s/m3)'
        ENDIF
        CALL OUTRES(ACOB,IOL,IBLD)
        DO 241 IDS=1,NDIST
241         WRITE(4,6000)IR+20,(ACOB(WD,IDS),WD=9,16),
     +                   IDS,  (ACOB(WD,IDS),WD=1,8)
       IF (IPL.EQ.1) THEN
        WRITE(IOL,*)
        WRITE(IOL,*)
        WRITE(IOL,9000) 'WET'
       ENDIF
       CALL OUTRES(AWET,IOL,IPL)
       IF (IPL.EQ.1) THEN
        WRITE(IOL,*)
        WRITE(IOL,*)
        WRITE(IOL,9000) 'DRY'
       ENDIF
       CALL OUTRES(ADRY,IOL,IPL)
       IF (IPL.EQ.1) THEN
        WRITE(IOL,*)
        WRITE(IOL,*)
c        WRITE(IOL,9000) 'TOTAL'
       ENDIF
       CALL OUTRES(ATOT,IOL,0)
       IF(IR.EQ.5) GOTO 243
        DO 242 IDS=1,NDIST
242        WRITE(4,6000)IR+30,(ATOT(WD,IDS),WD=9,16),
     +     IDS,  (ATOT(WD,IDS),WD=1, 8)
243     CONTINUE
C
      ELSE ! IR Pollutant type loop
C
C       AIR AS A SOURCE
C       SET CONC TO LOG10(1)
C
        DVL=-99.
        IF (IR.EQ.ICP1) THEN
           IF(SOILF1.GT.0.0) DVL=ALOG10(SOILF1)
        ENDIF
        IF (IR.EQ.ICG2) THEN
           IF(SOILF2.GT.0.0) DVL=ALOG10(SOILF2)
        ENDIF
        DO 244 IDS=1,NDIST
        DO 244 WD=1,8
           ACON(WD,IDS)=0.0
           ACOB(WD,IDS)=-99.0
           ATOT(WD,IDS)=DVL
244        CONTINUE
        DO 245 IDS=1,NDIST
245         WRITE(4,6000)IR+10,(ACON(WD,IDS),WD=1,8),
     +                   IDS,  (ACON(WD,IDS),WD=1,8)
        DO 246 IDS=1,NDIST
246         WRITE(4,6000)IR+20,(ACOB(WD,IDS),WD=1,8),
     +                   IDS,  (ACOB(WD,IDS),WD=1,8)
        IF(IR.EQ.5) GOTO 248
            DO 247 IDS=1,NDIST
247             WRITE(4,6000)IR+30,(ATOT(WD,IDS),WD=1,8),
     +                       IDS,  (ATOT(WD,IDS),WD=1,8)
248     CONTINUE
      ENDIF ! IR Pollutant type loop
250         CONTINUE
c
            IF(IPRT.EQ.5) THEN
            WRITE(6,9600)
            write (6,*) ' '
            Write(6,'(A)') ' Dispersion Coefficient (sigma-z)       '
            Write(6,'(A)') ' Terrain Adjustment Factors             '
            DO 252 WD=1,16
            WRITE(6,'(6X,7(A,8X))')
     +      'A','B','C','D','E','F','X(m)  Dir.'
              DO 251 IDS=1,NDIST
251             WRITE(6,'(1X,6F9.3,F10.2,I3)') RSIZ1(IDS,1,WD),
     +          RSIZ1(IDS,2,WD),RSIZ1(IDS,3,WD),RSIZ1(IDS,4,WD),
     +          RSIZ1(IDS,5,WD),RSIZ1(IDS,6,WD),x(IDS),WD
              write (6,*) ' '
252         CONTINUE
            write (6,*) ' '
            WRITE(6,9600)
            write (6,*) ' '
            Write(6,'(A)') ' Wind Speed Adjustment Factors Resulting'
            Write(6,'(A)') ' From Height and Roughness Functions    '
c
            DO 254 WD=1,16
              WRITE(6,'(6X,7(A,8X))')
     +        'A','B','C','D','E','F','X(m)  Dir.'
              DO 253 IDS=1,NDIST
                WRITE(6,'(1X,6F9.3,F10.2,I3)') RSIZ2(IDS,1,WD),
     +          RSIZ2(IDS,2,WD),RSIZ2(IDS,3,WD),RSIZ2(IDS,4,WD),
     +          RSIZ2(IDS,5,WD),RSIZ2(IDS,6,WD),x(IDS),WD
c                DO  253 WS=1,6 July 5, 1993/jgd correction
                DO  253 ST=1,6
                RSIZ1(IDS,ST,WD)=RSIZ2(IDS,ST,WD)*RSIZ1(IDS,ST,WD)
253             continue
                  write (6,*) ' '
254         CONTINUE
            WRITE(6,9600)
            write (6,*) ' '
            Write(6,'(A)') ' Net Dispersion Adjustment Factors'
            DO 256 WD=1,16
            WRITE(6,'(6X,7(A,8X))')
     +      'A','B','C','D','E','F','X(m)  Dir.'
              DO 255 IDS=1,NDIST
255             WRITE(6,'(1X,6F9.3,F10.2,I3)') RSIZ1(IDS,1,WD),
     +          RSIZ1(IDS,2,WD),RSIZ1(IDS,3,WD),RSIZ1(IDS,4,WD),
     +          RSIZ1(IDS,5,WD),RSIZ1(IDS,6,WD),x(IDS),WD
              write (6,*) ' '
256         CONTINUE
            write (6,*) ' '
            ENDIF
            write (6,*) ' '
260         WRITE(6,9500) 'End Data FILE # ',IEOF
300   CONTINUE
C
      CLOSE (UNIT=8)
      close (4)
      close (iol)
      CALL GETTIM (JHR,JMIN,JSEC,J100TH) 
C **** FORMAT  STATEMENTS ***
C *********************************************************************
 1000 FORMAT (16(2X,A3))
 1001 FORMAT (8G10.3)
c 1002 FORMAT (A,6F9.3,A)
 1002 FORMAT (A,6F7.3,A,f7.2,a,f5.2,a) ! March 19, 2001
 1100 FORMAT (A30)
 1200 FORMAT (1X,3I2,' VD ',6F9.3,', VS ',1X,F9.3)
 1300 FORMAT (I1)
 2000 FORMAT (I5)
 2500 FORMAT (2A4,A2,3F10.0,E10.1)
 2600 FORMAT (20A4)
 2800 FORMAT (1X,20A4)
 3000 FORMAT (6F10.2)
 4000 FORMAT (2A4,A2,4F10.0)
 5000 FORMAT (2A4,A2,3F10.0)
 6000 FORMAT (1X,I2,8F7.3)
 6100 FORMAT (16F5.4)
 7000 FORMAT (7X,6F7.5)
 8000 FORMAT(1H ,A,G10.3,A)
 9000 FORMAT (1H1,'NORMALIZED ANNUAL ',A,' DEPOSITION (s/m**2/year)')
 9500 FORMAT (1H1,A,I3)
 9600 FORMAT(1H1)
 9999 END
C
C     RDSCE/RAPSCD 070493JGD BATTELLE PNL, RICHLAND WA
C
C     Routine Change for Framework, August 7, 1996 CJF
      SUBROUTINE RDSCE(IS,SOILF1,SOILF2,ICP1,ICG2,IEOF,
     &                 SOUFNM, souleng)
C=======================================================|
C       AUTHORS JW BUCK/JG DROPPO
C       READS STACK/AREA SOURCE PARAMETERS AND COMPUTES
C       PARTICULATE EMISSIONS FOR AREA SOURCE TYPES
C======================================================|
      include 'ATMOS.INA'
C

      CHARACTER*4 POLID(3)
c     Next Line Added, August 7, 1996 CJF
      CHARACTER*8 SOUFNM 
c     Next Line added, June 9, 1997 CJF
      INTEGER     souleng  
c     Add Next two lines, December 11, 1996 CJF
      CHARACTER*33 glypnm
      CHARACTER*1 dum
      INTEGER ICL(2)
C
C       LINE 1:
C       NSR   = SOURCE TYPE, 0 = POINT/STACK RELEASE
C                            1 TO 8 = AREA RELEASE
C                            9 = AIR AS A SOURCE
C       NPOL  = NUMBER OF POLLUTANTS
C       RELPAR= AREA PART RELEASE/STACK RELEASE
C       RELGAS= AREA GAS RELEASE/STACK RELEASE
C       VOLAT = AREA VOLATIZATION TYPE / STACK NOT USED
C       EBACK = BACK CALCULATION RUN
C       LINE 2:
C       PNAME = TEXT LABEL LINE FROM SOURCE FILE
C------------------------------------------|
C  NSR = 0 (POINT SOURCE) UNITS  VARIABLE  |
C==========================================|
C
C  EXIT VELOCITY . . . . . M/S    VSTK   (EV)
C  EXIT RADIUS . . . . . .  M     RSTK   (RAD)
C  STACK HEIGHT  . . . . .  M     HSTK   (SH)
C  EXIT TEMPERATURE  . .  DEG K   TSTK   (ET)
C  AMBIENT TEMPERATURE    DEG K   TAMB   (new)
C  BUILDING HEIGHT          M     BSTK   (new)
C
C----------------------------------------------------------------|
C        NSR = 1 (AREA SOURCE)                           VARIABLE|
C================================================================|
C  INPUTS FOR ENTIRE AREA:
C  SDAT(1) = TOTAL CONTAMINATED AREA  . . . . .  M**2    AREAS
C  SDAT(2) = RATIO OF NS AND EW AREA WIDTH  . . . -      RAREA78
C  SDAT(3) = MEAN ANNUAL WIND SPEED . . . . . .  M/S     VSFC
C  SDAT(4) = FASTEST MILE OF WIND . . . . . . .  M/S     FASTMI
C  SDAT(5) = PERCENT SAND IN TOP SOIL . . . . .   %      SAND
C  SDAT(6) = CLIMATOLOGICAL SUSPENSION FACTOR     -      CPR
C  SDAT(7) = PRECIPITATION - EVAPORATION INDEX    -      PEI
C
C                              SOIL                      PAVED  UNPAVED
C  INPUTS FOR                  BARE   GRASS-  TREES-     ROAD-  ROAD-
C                              AREA   SHRUBS  BUILDINGS  WAY    WAY
C
C SURFACE CONTROL PARAMETER     01     02      03        04     05
C
C FRAC.TOTAL AREA         -    FBARE  FGRAS   FTREE     -      -     1
C ROUGHNESS HEIGHT        CM   ZBARE  ZGRAS   ZTREE     -      -     2
C FRAC.VEGETATION COVER   -    VEGF   VEGG    VEGT      -      -     3
C FRAC.WET SOIL           -    FWETB  FWETG   FWETT     -      -     4
C # OF DISTURBANCES      #/MO  DBARE  DGRAS   DTREE     -      -     5
C SFC COVER CORR. FACTOR  -    CORRLC  -       -        -      -     6
C % NOT COVERED BY CRUST  %    FCRUST  -       -        -      -     7
C # OF R/TRIPS PER MONTH #/MO    -     -       -      RTRIPP  RTRIP  1
C # VEHICLES PER R/TRIP   #      -     -       -      VNUMP   VNUM   2
C DISTANCE/ROUND TRIP .   M      -     -       -      VDISTP  VDIST  3
C SILT CONTENT OF SOIL    %      -     -        -     SILTP   SILT   4
C VEHICLE AVERAGE SPEED. KM/HR   -     -       -      VSPEEDP VSPEED 5
C AVERAGE VEHICLE WEIGHT .Mg     -     -       -      VWGHTP  VWGHT  6
C # OF WHEELS ON VEHICLE  #      -     -       -      WHEELP  WHEELS 7
C
C
C================================================================|
C
        READ (IS,40,END=999) IPRT,NSR,NPOL,NRELPAR,NRELGAS,NVCASE
C
C================================================================|   ! BLH
C     Read around Pollutant information destined for
C     Interface with other models

      IF(NSR.EQ.7) THEN

c        Read in Glyph name for FRAMES
c        Skip the pollutant information
         DO i = 1, NPOL
            READ(IS,'(a1)')dum
         ENDDO
c        Read in Glyph Name
         READ( IS, '(a)' )glypnm   
         CALL RDAFF( SOUFNM, souleng, glypnm )
         RETURN
      ENDIF

      IF(NSR.NE.9) THEN
         NVOL=0
         NRES=0
         DO 5 I=1,NPOL
         READ (IS,46,END=999) POLID,ICL,TSTART,RATE
         IF (NSR.EQ.1) THEN
C        AREA RELEASE/CHECK TO SEE IF MODEL IS TO COMPUTE RATES...
           IF(ICL(2).GT.1) THEN
C            GAS RELEASES
             IF (ICL(1).GE.4.AND.ICL(1).LE.9) NVOL=NVOL+1
           ENDIF
           IF(ICL(2).EQ.1.OR.ICL(2).EQ.3) THEN
C            PARTICULATE RELEASES
             IF (ICL(1).GE.0.AND.ICL(1).LE.3) NRES=NRES+1
             IF (ICL(1).GE.7.AND.ICL(1).LE.9) NRES=NRES+1
C            DON'T COMPUTE EMISSIONS IF MULTIMEDIA /JGD February 13, 1993
             IF (NRELPAR.EQ.4) NRES=0
           ENDIF
         ENDIF
5        CONTINUE
         READ (IS,43,END=999) PNAME
         WRITE(6,*) PNAME
C
C        DEFINE DEFAULT DEPOSITION PARAMETERS;
C        V = Deposition Velocity, and ALCO = Scavenging Efficiency
           V=.01
c           ALCO=0.9
           If(ALCO.EQ.0.0) ALCO = 0.9 ! March 15, 2001 JGD
C
C        SET DEFAULT EMISSION RATES
         DO 15 I = 1,7
            QNN(I)=1.0
15       CONTINUE
      ENDIF
      IF( NSR.EQ.0.OR.NSR.EQ.2) THEN
C     POINT SOURCE
        READ (IS,44,END=999)ISFC,VSTK,RSTK,HSTK,TSTK,TAMB,BSTK
        IF (ISFC.EQ.6) THEN
C         MEAN TEMPERATURE FOR PLUME RISE COMPUTATION
          IF(TAMB.EQ.0.0) THEN
            TMEAN=291.
          ELSE
            IF(TAMB.LT.250.) TAMB=TAMB+273.15
            TMEAN=TAMB
          ENDIF
          IF(TAMB.GT.TSTK) TMEAN=TSTK
          WRITE(6,42) 'POINT RELEASE:'
C         CHECK FOR BUILDING WAKE
          BST=BSTK/.75
          IF(HSTK.LT.BST) THEN
          WRITE(6,42) ' Building Downwash Occurs (H=',HSTK,'m)'
          WRITE(6,42) ' Note: Following Stack Parameters have '
          WRITE(6,42) ' been reset to account for building wake'
C           ASSUMED NO RISE AND BUILDING WAKE
            VSTK=0.0
            IF(BSTK.GT.4.) THEN
              HSTK=BSTK/2.0
            ELSE
              HSTK=2.0
            ENDIF
            RSTK=BSTK*.375
            TSTK=TMEAN
          ENDIF
          WRITE(6,42) ' Stack/Vent Height  = ',HSTK,' m '
          WRITE(6,42) ' Building Height    = ',BSTK,' m '
          WRITE(6,42) ' Release Exit Radius=',RSTK,' m '
          WRITE(6,42) ' Exit Velocity      = ',VSTK,' m/s'
          WRITE(6,42) ' Exit Temperature   = ',TSTK,'deg K'
          WRITE(6,42) ' Average Air Temper.= ',TMEAN,'deg K'
          IF(NRELPAR.EQ.4) THEN
             WRITE(6,42) ' Emission back calculation run.'
          ELSE
             WRITE(6,42) ' Emission input run.'
          ENDIF
        ENDIF
      ELSEIF (NSR.GE.1.AND.NSR.LE.8) THEN
C     AREA EMISSION
      CALL SUSPEN(IS,ISFC,R10,NRES)
      IF(NRES.GT.0) THEN
          QNN(1)=R10
          QNN(2)=QNN(1)
          QNN(3)=QNN(1)
          WRITE(6,42)'   TOTAL PARTICULATE= ',QNN(1)*1.E-6,' g/s '
      ELSE
          QNN(1)=1.
          QNN(2)=QNN(1)
          QNN(3)=QNN(1)
c         MULTIMEDIA UPDATE /JGD February 13, 1993
          IF (NRELPAR.NE.4) THEN
           WRITE(6,42)'   Particulate emission rates not computed'
          ENDIF
      ENDIF
C
      ELSEIF(NSR.EQ.9) THEN
C     AIR AS A SOURCE
C
      WRITE(6,*) 'Air as a source - assumes uniform patterns. '
C
      READ (IS,47,END=999)
     + POLID,ICL,TSTART,CINAIR,CINSOIL,SOILF1,SOILF2,ICP1,ICG2
C
      CLOSE(IS)
C
      ELSE
          WRITE(6,*) 'ERROR, VALUE OF NSR NOT ALLOWED',NSR
      ENDIF
      WRITE(6,42) ' '
      RETURN
999   IEOF=IS
      RETURN
 40   FORMAT (2I1,8I2)
 42   FORMAT (1X,A,G9.3,A)
 43   FORMAT (A40)
 44   FORMAT (I2,8G10.3)
 46   FORMAT (3A4,1X,2I1,1X,2E10.3)
 47   FORMAT (3A4,1X,2I1,1X,5E10.3,2I4)
      END
C
C
C     SUSPEN/RAPSCD 042194JGD BATTELLE PNL, RICHLAND WA
C
      SUBROUTINE SUSPEN(IS,ISFC,R10,NRES)
C
C=======================================================|
C       AUTHORS JW BUCK/JG DROPPO
C       COMPUTES PARTICULATE EMISSIONS FOR AREA SOURCE
C======================================================|
C
      include 'ATMOS.INA'
          character*80  sdats
C
C         SUMMARY OF UPDATES:
C
C
C
C     SET EMISSIONS TO ZERO
      R10=0.0
      NRES=0
C
C     SDAT INPUT SECTION
10    READ (IS,40,END=90) sdats
      READ (sdats,41) ISFC
      if (isfc.lt.6) READ (sdats,41) ISFC,(SDAT(K),K=1,9)
   40 FORMAT (a80)
   41 FORMAT (I2,9G8.0)
      IF(ISFC.EQ.9) GOTO 90
      IF(ISFC.EQ.0) THEN
C        START NEW AREA
         AREAS= SDAT(1)
         RAREA= SDAT(2)
         AREAG= SDAT(8)
         WRITE(6,42) 'SOURCE EMISSION AREAS'
         WRITE(6,42) '   Gaseous Area     = ',AREAG,'  m2 '
         WRITE(6,42) '   Particulate Area = ',AREAS,'  m2 '
         WRITE(6,42) '   Area Ratio       = ',RAREA
         IF (RAREA.GT.1000.) RAREA=1000.
         IF (RAREA.LT.0.00001) RAREA=0.00001
         IF(NRELPAR.EQ.2) THEN
              VSFC = SDAT(3)
              FASTMI=SDAT(4)
              SAND = SDAT(5)
              CPR=1.0
              PEI  = SDAT(7)
              WRITE(6,42) '   Mean Wind Speed  = ',VSFC, ' m/s'
              WRITE(6,42) '   Fastest Wind     = ',FASTMI,' m/s'
              WRITE(6,42) '   Percent Sand     = ',SAND, '  % '
C             WRITE(6,42) '   Climate Factor   = ',CPR
              WRITE(6,42) '   Prec-Evap Index  = ',PEI
         ELSEIF(NRELPAR.EQ.1) THEN
               WRITE(6,42) '   Input particulate emissions'
         ELSEIF(NRELPAR.EQ.3) THEN
               WRITE(6,42) '   Backcalculate particulate emissions'
         ELSEIF(NRELPAR.EQ.4) THEN
               WRITE(6,42) '   Multimedia emission rates computed'
         ENDIF
      ELSEIF(NRELPAR.EQ.4) THEN
            DUMM=0.0
      endif
      if(nrelpar.eq.2) then
        IF (ISFC.GE.1.AND.ISFC.LE.3) THEN
              IF(AREAS.LE.0.0) THEN
                AREAS=1.0
                WRITE(6,*) ' AREA=0 ERROR in sou file.'
                WRITE(6,*) 'Particulate area = 1 m2 assumed.'
              ENDIF
              IF(ISFC.EQ.1) WRITE(6,*) '   OPEN AREA --'
              IF(ISFC.EQ.2) WRITE(6,*) '   GRASS/SHRUB AREA --'
              IF(ISFC.EQ.3) WRITE(6,*) '   TREES/BUILDINGS AREA --'
C             AREA SUSPENSION
              FRAC   = SDAT(1)
              ZOSFC  = SDAT(2)
              VEGF   = SDAT(3)
              WETFR  = SDAT(4)
              DISN   = SDAT(5)
              WRITE(6,42)'   Area Fraction    = ',FRAC
              WRITE(6,42)'   Roughness Length = ',ZOSFC,' cm'
              WRITE(6,42)'   Vegetation Cover = ',VEGF
              WRITE(6,42)'   Number of Distrb = ',DISN,' #/mo'
              IF(WETFR.NE.0)
     +          WRITE(6,42)'   Fraction Area Wet= ',WETFR
                IF(ISFC.NE.1) THEN
                  CORRLC = 1.0
                  FCRUST = 0.0
              ELSE
                  CORRLC = SDAT(6)
                  FCRUST = 1.0 - SDAT(7)
                  WRITE(6,42) '   Fraction Crusted = ',FCRUST
                  WRITE(6,42) '   Sfc Corr Factor  = ',CORRLC
              ENDIF
C
C               COMPUTATION SECTION
C
                USTARC=EXP(.4118428*LOG(0.01056 * SAND + 0.05)
     +            +4.167173)*CORRLC/100.
C              (BASED ON FIGURE 3-4 COWHERD ET AL. 1984)
                WRITE(6,42) '   Friction Velocity= ',USTARC,' m/s '
                IF(WETFR.GE.1.) THEN
C                  WET SURFACE - NO RESUSPENSION
                   WRITE(6,42)'   Wet Surface-No Particulate Suspension'
c                  add next line/jgd/February 1, 1995
                   nres=nres+1
                ELSE
C                  CHECK ROUGHNESS LENGTH
                   IF(ZOSFC.LE.0.0.OR.ZOSFC.GT.400.) THEN
                     WRITE(6,*)'   Roughness Length of 10. cm assumed'
                     ZOSFC=10.
                   ENDIF
C                  ASSUME 0.1 % BARE FOR VEGF = 1.0
                   IF(VEGF.EQ.1.0) THEN
                       VEGF=.999
                       WRITE(6,42) '   Assumed Veg Frac = ',VEGF
                   ENDIF
C                  COMPUTE CRITICAL VELOCITY
                   UCRIT=USTARC*LOG(700./ZOSFC)/.4
                   WRITE(6,42) '   Critical Speed   = ',UCRIT,' m/s'
C                  DEFINE EROSION TYPE
                   IF (USTARC.GT.0.75) FCRUST = 1.0
                   IF (FCRUST.GT.0.0) THEN
C                      LIMITED
                       IF( FASTMI .GE. UCRIT ) THEN
                         PU = 6.7 * (FASTMI - UCRIT)
                       ELSE
                         PU = 0.0
                       ENDIF
                       WRITE(6,42) '   Erosion Potential= ',PU,' g/m2'
                       ETEN=0.83*(DISN*PU*(1.-VEGF))/(PEI/50.)**2
                       ETEN=FRAC*FCRUST*(ETEN*AREAS/CPR)*.2778
                       WRITE(6,42)
     +                 '   Erosion (Limited)= ',ETEN*1.E-6,' g/s'
C                      March 23, 1990/CORRECTED E-6 FORMAT/JGD
                       R10=R10+ETEN
                       NRES=NRES+1
                   ENDIF
                   IF (FCRUST.LT.1.0) THEN
C                      UNLIMITED
                       FX=.886*UCRIT/VSFC
                       FX=FXX(FX)
                       ETEN=0.036*(1.-VEGF)*(1.-FCRUST)
     +                       *(VSFC/UCRIT)**(3)*FX
                       ETEN = FRAC*ETEN*AREAS/CPR*277.8
                       WRITE(6,42)
     +                 '   Erosion (Unlim)  = ',ETEN*1.E-6,' g/s'
                       R10=R10+ETEN
                       NRES=NRES+1
                   ENDIF
                ENDIF
        ELSEIF(ISFC.GE.4.AND.ISFC.LE.5) THEN
                IF(ISFC.EQ.4) WRITE(6,*) '   PAVED ROADWAY --'
                IF(ISFC.EQ.5) WRITE(6,*) '   UNPAVED ROADWAY --'
                RTRIP  = SDAT(1)
                VNUM   = SDAT(2)
                VDIST  = SDAT(3)
                SILT   = SDAT(4)
                VSPEED = SDAT(5)
                VWGHT  = SDAT(6)
                WHEELS = SDAT(7)
                WRITE(6,42)'   Roundtrips        = ',RTRIP ,' #/mo'
                WRITE(6,42)'   Vehicle/Roundtrip = ',VNUM  ,'  #'
                WRITE(6,42)'   Distance/Roundtrip= ',VDIST ,'  m'
                WRITE(6,42)'   Silt Content      = ',SILT  ,'  %'
                WRITE(6,42)'   Vehicle Speed     = ',VSPEED,' km/hr'
                WRITE(6,42)'   Vehicle Weight    = ',VWGHT ,'  Mg'
                WRITE(6,42)'   Num. of Wheels    = ',WHEELS,' #/vehicle'
C
                ETEN=0.85*(SILT/10.)*((VSPEED/24.)**0.8)*
     1            ((VWGHT/7.)**0.3)*((WHEELS/6.)**1.2)*
     2            ((365.-DPRECIP)/365.)*RTRIP*VNUM*VDIST*0.012*31.71
C               PAVED ROAD ASSUMED TO BE 1% OF UNPAVED ROAD EMISSION
C               IF(ISFC.EQ.5) ETEN = ETEN*.01 / jgd October 30, 1992
                IF(ISFC.EQ.4) ETEN = ETEN*.01
                WRITE(6,42) '   Roadway Emission  = ',ETEN*1.E-6,' g/s'
                R10 = ETEN+R10
                NRES=NRES+1
        ENDIF
      endif
      GOTO 10
90    CONTINUE
      RETURN
 42   FORMAT (1X,A,G9.3,A)
      END
C
      SUBROUTINE HDR1(VERN,iol,sysdate,systime)
      CHARACTER*20 VERN
      CHARACTER*10 SYSTIME
      CHARACTER*12 SYSDATE
        SYSDATE='          '
        SYSTIME='          '
        CALL DATTIM (SYSDATE, SYSTIME)
        WRITE(IOL,*)'MEPAS ATMOSPHERIC MODEL OUTPUT ',VERN,
     1  '  ',SYSDATE,' ',SYSTIME
      WRITE(IOL,*)' '
      WRITE(IOL,*)
     +' This version uses sigma-z and sigma-y dispersion curves for '
      WRITE(IOL,*)
     +' Pasquill types A to F recommended by Briggs (F. A. Gifford, '
      WRITE(IOL,*)
     +' Turbulent Diffusion Typing Schemes: A Review, Nucl. Saf. 17(1):'
      WRITE(IOL,*)
     +' 74 January-February 1976).  The 7th type, G, is run as type F.'
      WRITE(IOL,*)' '
C
      RETURN
      END
C 
C     AIRDRY/RAPSCD 072188 JG DROPPO, BATTELLE PNL, RICHLAND WA 
C 
      SUBROUTINE AIRDRY 
C 
C     Description: atmospheric resistances are computed for different 
C     surface roughness values.  These resistances are output in main 
C     program in "-.AIR" file for computation of flux rates over specific 
C     surfaces in other RAPS/MEPAS components. 
C
C     Update Log:
c     February 5, 1998/jgd/correct surface roughness in Vd computation
c
c
      include 'ATMOS.INA' 
      include 'DPSTN.INA' 
      include 'DRYD.INA' 
      INTEGER WS,WD,ST 
      dimension zs(6) 
      data zs/0.0, 1., 3.,  5.,  20.,  50./ 
C 
C     THIS SUBROUTINE RETURNS AVERAGE ATMOSPHERIC RESISTANCE VALUES 
C     FOR SIX SURFACE TYPES WITH THE FOLLOWING ROUGHNESS LENGTHS: 
C 
C     1. WATER       -   0 CM (COMPUTED BY MODEL) 
C     2. BARE SOIL   -   1 CM 
C     3. GRASSLAND   -   3 CM 
C     4. WHEAT FIELD -   5 CM 
C     5. RESIDENTIAL  -  20 CM 
C     6. TREES       -  50 CM 
C 
C     
      HMAS=10. 
      IZ=10 
      HSTD=3. 
      ZDISP=0.0 
      NTY=6 
C  -- LOOP OVER SURFACE TYPES 
c     IR changed to IRSUR to avoid confusion with IR's use as
c     contamiant type elsewhere in the code/jgd/Feb 5 1998
C      DO 50 IR=1,NTY 
      DO 50 IRSUR=1,NTY 
c      DO 10 I =1,4! 20 Mar 02 /JGD/
      DO 10 I =1,5 
10      AVD(I,IRSUR)=0.0 
C 
        IF(NSR.LT.9) THEN 
        DO 80 ST=1,6 
        DO 80 WD=1,16 
        DO 80 WS=1,7 
            ZRO=ZS(IRSUR) 
            UWS=U(WS) 
C        -- SKIP ZERO CASES 
            IF(RF(ST,WS,WD).LE.0.0) GOTO 80 
C 
C             -- WIND PROFILE PARAMETERS FOR SELECTED SURFACE 
                 IF (ZRO.EQ.0.0) THEN 
C                OVER WATER 
                      CALL UWAT(ST,UWS,HSTD,USTAR,ZRO,ZDISP) 
                 ELSE 
C                OVER LAND 
                      CALL ULAN(ST,UWS,HSTD,USTAR,ZRO,ZDISP) 
                 ENDIF 
C 
C             -- COMPUTE WIND SPEED AT HMAS 
                 CALL UBAR(ST,UPLM,HMAS,USTAR,ZRO,ZDISP) 
C 
C             -- ATMOSPHERIC DRY DEPOSITION 
                 CALL RAIR(ZRO,ST,UPLM,RIS,IZ,USTAR) 
                 IF(RIS.GT.0.0) THEN 
                      AVD(5,IRSUR)=AVD(5,IRSUR)+RF(ST,WS,WD)/RIS 
                 ENDIF 
                 DO 40 I=1,4
                     IF(I.LT.4) THEN   ! 20 Mar 01 /JGD /case 6 logic
                       RHOP=RSG(I) 
                       R=RDS(I)*.0001
                     ELSE
                       RHOP=RSG(6)
                       R=RDS(6)*.0001
                     ENDIF

c                      debug loop
cx                     write(*,*) ' CALL VDVS(RHOP,R,USTAR,ZRO)'
cx                     write(*,*) ' RHOP    ',RHOP
cx                     write(*,*) ' R       ',R
cx                     write(*,*) ' USTAR   ',USTAR
cx                     write(*,*) ' ZRO     ',ZRO
cx                     write(*,*) ' Itype   ',I
cx                     write(*,*) ' WS,U(WS)',WS,U(WS)
cx                     write(*,*) ' WDirect  ',WD
cx                     write(*,*) ' STabilty',ST
cx                     pause

                     CALL VDVS(RHOP,R,USTAR,ZRO) 
                     VDS=VD-VS 
                     IF(VDS.GT.0.0) THEN 
                        VDS=1./VDS+RIS 
                     ELSE 
                        VDS=RIS 
                     ENDIF 
                     IF(VDS.GT.0.0) THEN 
                        VDS=(1./VDS+VS) 
                     ELSE 
                        VDS=VS 
                     ENDIF
                 AVD(I,IRSUR) = AVD(I,IRSUR)+RF(ST,WS,WD)*VDS 
40               CONTINUE 
80          CONTINUE 
C 
        ELSE 
C 
C       AIR AS A SOURCE 
C       USE TYPICAL CONDITIONS 
C 
C       -- LOOP OVER SURFACE TYPES 
           UPLM=3.0 
           ST=4 
           ZRO=ZS(IRSUR) 
           ANHGT=10. 
           ZDISP=0.0 
           IF(ZRO.LE.0.0) THEN
              CALL UWAT(ST,UPLM,ANHGT,USTAR,ZRO,ZDISP) 
           ELSE
              CALL ULAN(ST,UPLM,ANHGT,USTAR,ZRO,ZDISP) 
           ENDIF
           CALL UBAR(ST,UPLM,HMAS,USTAR,ZRO,ZDISP) 
           CALL RAIR(ZRO,ST,UPLM,RIS,IZ,USTAR) 
c           IF(RIS.GT.0.0) THEN 
c                AVD(4,IRSUR)=1./RIS 
c           ELSE 
c                AVD(4,IRSUR)=.02
c           ENDIF 
           IF(RIS.GT.0.0) THEN ! 20Mar01/JGD/Case 6 Logic
                AVD(5,IRSUR)=1./RIS 
           ELSE 
                AVD(5,IRSUR)=.02
           ENDIF 
           DO 90 I=1,4
               IF(I.LT.4) then
                 RHOP=RSG(I) 
                 R=RDS(I)*.0001
               ELSE
                 RHOP=RSG(6) 
                 R=RDS(6)*.0001
               ENDIF
               CALL VDVS(RHOP,R,USTAR,ZRO) 
               VDS=VD-VS 
               IF(VDS.GT.0.0) THEN 
                  VDS=1./VDS+RIS 
               ELSE 
                  VDS=RIS 
               ENDIF 
               IF(VDS.GT.0.0) THEN 
                  AVD(I,IRSUR) = 1./VDS+VS 
               ELSE 
                  AVD(I,IRSUR) = VS 
               ENDIF 
90         CONTINUE 
        ENDIF 
50    CONTINUE 
      RETURN 
      END 
                                     
