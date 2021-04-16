c$DEBUG 
C  
C     CHANNEL TRANSPORT AND DISPERSION SUBROUTINE  
C  
C     February 13, 1988 JGD / Updated Version March 11, 2001  
C  
      SUBROUTINE CHANNEL( IR, IST, ARWX, ARWZ, RHOP, RADIUS)
C  
C     DISPERSION AND DEPOSITION VALUES ARE RETURNED IN CA,CB,CD AND  
C     TYPE OF VALUE IS IN ICNL (CHANNEL=1,TRANSITION=2,OUTSIDE=3)  
C  
       INCLUDE 'ATMOS.INA'  
       INCLUDE 'DPSTN.INA'  
       INCLUDE 'DRYD.INA'  
C         
c           added ar_siz and ar_zfac /September 16, 1998
      REAL  ar_siz, ar_zfac

            IWD=ACHAN  
            IWS=2  
            FWID=.392699  
            FDISP=.39894  
            SIZ=0.0  
            ZCHAN2=2.*ZCHAN  
            XCHAN2=2.*XCHAN  
            XEW = 2*ARWX/(1+RAREA)
            XNS = XEW*RAREA
            CALL DIRRAD(IWD,XNS,XEW,ARW)
C  
c           Calculate sigma z and reflection term at area edge
c           for use in inner area depletion 
            
            UCHAN = 2.0
c           IF ARW is less than 1 meter set to 100 (this is a punt)

            IF( ARW .LT. 1.0 ) THEN
               TMET = 100.0/uchan
            ELSE
               TMET = ARW/UCHAN         
            ENDIF
            CALL SIGZT(TMET,IST,UCHAN,AR_SIZ,SIY)  
            HCOM = 2.0
            HAVE = 2.0                             
            HMIX = 5000.0
            CALL REFLEC(-5,5,AR_SIZ,HAVE,HCOM,HMIX,AR_ZFAC)
            
            DO 70 IDS=1,NDIST  
             CM(IDS)=0.0  
             CALL INDEXD(X(IDS),KDS) 
             IF(XCHAN.GT.0.OR.YCHAN.GT.0.) THEN  
              YWID=X(IDS)*FWID  
              IF(IDS.EQ.1) THEN  
                ICNL(IDS)=1  
                IF(YWID.GT.YCHAN) YWID=YCHAN  
              ELSE  
C               CHANNEL ENDS WHEN PLUME IS 2*HEIGHT  
                IF(ICNL(IDS-1).EQ.1) THEN  
                  IF(SIZ.GE.ZCHAN2) THEN  
                    ICNL(IDS)=2  
                    XCHAN=X(IDS)  
                    XCHAN2=XCHAN*2.  
                  ELSE IF(X(IDS).LT.XCHAN) THEN  
                    ICNL(IDS)=1  
                    IF(YWID.GT.YCHAN) YWID=YCHAN  
                  ELSE  
                    ICNL(IDS)=2  
                    XCH=X(IDS)-XCHAN+YCHAN/FWID  
                    YWID=XCH*FWID  
                  ENDIF  
                ELSE IF(ICNL(IDS-1).EQ.2) THEN  
                  IF(X(IDS).LE.XCHAN2) THEN  
                    ICNL(IDS)=2  
                    XCH=X(IDS)-XCHAN+YCHAN/FWID  
                    YWID=XCH*FWID  
                  ELSE  
                    ICNL(IDS)=3  
                  ENDIF  
                ELSE IF(ICNL(IDS-1).EQ.3) THEN  
                  ICNL(IDS)=3  
                ENDIF  
              ENDIF  
             ELSE  
              ICNL(IDS)=3  
             ENDIF  
             IF(ICNL(IDS).EQ.3) THEN  
C               OUTSIDE OF CHANNEL  
                CA(IDS)=0.0  
                CB(IDS)=0.0  
                CD(IDS)=0.0  
                FD(IDS)=0.0  
             ELSE  
                IF(ICNL(IDS).EQ.2) THEN  
                    FD(IDS)=2.-X(IDS)/XCHAN  
                    IF(FD(IDS).LT.0.) FD(IDS)=0.  
                    IF(FD(IDS).GT.1.) FD(IDS)=1.  
                ELSE  
                    FD(IDS)=1.0  
                ENDIF  
  
C               CHANNEL COMPUTATION  
                HCUR=2.0  
                HAVE=2.0  
                UCHAN=2.0  
                HMIX = 5000.
                HCOM = 2.0


C               COMPUTE DISPERSION VALUES...  
C  
C               COMPUTE TIME OF TRAVEL OF PLUME IN CHANNEL  
                   If(IDS.eq.1) then
                     TMET = X(1)/UCHAN
                   Else
                       xdelt = X(IDS) - X(IDS-1)
                     TMET = TMET + xdelt/Uchan
                   Endif
                IF (TMET.NE.0.0) THEN  
                  CALL SIGZT(TMET,IST,UCHAN,SIZ,SIY)  
                ELSE  
                  SIZ=0.0  
                ENDIF  
                IF(NSR.EQ.0.OR.NSR.EQ.2) THEN  
                  SIZ=SQRT(SIZ*SIZ+ARWZ)  
                ENDIF  
               IF(HAVE.GT.HMIX) HMIX = HAVE
               CALL REFLEC(-5,5,SIZ,HAVE,HCOM,HMIX,ZFAC)
C  
C               DRY REMOVAL AS FUNCTION OF DISTANCE...  
C  
                IF(IR.EQ.5) THEN  
                    FDRY=1.0  
                    FWET=1.0  
                    VD=0.0  
                    VS=0.0  
                ELSE  
                     IZ = 10
                    zoch = Z0(iwd,kds)
                    CALL DRYVEL(UCHAN,IWD,KDS,IST,IZ,RHOP,RADIUS,ZOCH)  
                 IF(X(IDS).LE.ARWX) THEN
                    DREM=100.0/AR_SIZ*AR_ZFAC
                 ELSE
                    IF( ids .NE. 1 ) THEN
                        IF( X(IDS-1) .LE. ARWX ) THEN
                           xdelt = x(ids) - arwx + 100.0
                        ELSE
                           xdelt=x(IDS)-x(IDS-1)
                        ENDIF
                 
                        DREM=DREM+XDELT/SIZ*ZFAC
                    ELSE
                        DREM = X(ids)/SIZ*ZFAC
                    ENDIF
                 ENDIF
                 FDRY=EXP(-.7979*DREM*VD/UCHAN)
              ENDIF

C  
C               COMPUTE OUTDOOR AIR CONC  
                   CHI = FDISP*FDRY/UCHAN/YWID/SIZ*ZFAC  
            CHIP = FDRY/(3.1415*UCHAN*SIY*SIZ)*ZFAC  
                IF(ZFAC.LE.0) THEN  
                    CHB=FDRY/UCHAN/YWID/BLDHGT  
                ELSE  
                    BAC = SIZ/FDISP/ZFAC  
                    IF(BLDHGT.GE.BAC) THEN  
                       CHB=CHI*BAC/BLDHGT*BLDFLT(IR)  
                    ELSE  
                       CHB=CHI*BLDFLT(IR)  
                    ENDIF  
                ENDIF    
C  
C               CONCENTRATIONS...  
                CA(IDS)=CHI  
                CB(IDS)=CHB  
                CD(IDS)=CHI*VD  
C               July 4, 1993/JGD ACUTE CONC  
                CM(IDS)=CHIP  
             ENDIF  
70          CONTINUE  
            DREM=0.0  
            RETURN  
            END  
      FUNCTION FXX(FX) 
C     COWHERD ET AL 1984 FUNCTION FROM FIG. 4-3 AND PAGE B-2 
C     JGD 3/87 
      IF (FX.LT..5) THEN 
         F=1.91 
      ELSE IF(FX.LT.1.0) THEN 
         F=1.9-(FX-.5)*.6 
      ELSE IF(FX.LT.2.0) THEN 
         F=1.6-(FX-1.)*1.3 
      ELSE IF(FX.GE.2.0) THEN 
         F=0.18*(8.*FX*FX*FX+12.*FX)*EXP(-FX*FX) 
      ENDIF 
      IF(F.LT.0.0) F = 0.0 
      FXX=F 
      END 
C 
      SUBROUTINE DIRRAD(IN,DNSX,DEWX,XRAD) 
C     CODE RETURNS NEW RADIUS, XRAD 
C     JGD 4-05-90 
      DIMENSION RNX(8) 
      DATA RNX/0.0,0.25,0.50,0.75,1.00,0.75,0.50,0.25/ 
      IX=IN 
      IF(IX.GT.8) IX=IX-8 
      FAC=RNX(IX) 
      XRAD=DNSX*FAC+DEWX*(1.-FAC) 
      RETURN 
      END 
C 
C     RAPSCD/CHANNEL OFFSET DISTANCE FUNCTION 
C     February 13, 1988, JGD 
        
       FUNCTION XCD(IWD,ACHAN,ARW) 
        DIR=ACHAN-IWD 
        IF(DIR.GT.16.) DIR=DIR-16. 
        IF(DIR.LT.1.)  DIR=DIR+16. 
        DIR=(2.*3.1415/16.)*DIR 
       XCD = COS(DIR)*ARW
      RETURN 
      END 
C 
C     RDBLD/RAPSCD 0101588JGD BATTELLE PNL, RICHLAND WA 
C 
      SUBROUTINE RDBLD(IT,IER) 
       INCLUDE 'ATMOS.INA' 
C 
C     BLDHGT = HEIGHT OF THE BUILDING INTERIOR (M) 
C     BLDFLT = FILTRATION PASS FRACTIONS FOR THE SEVEN CONTAMINANT CLASSES 
C 
      BLDFLT(1) =.10 
      BLDFLT(2) =.50 
      BLDFLT(3) =.90 
      BLDFLT(4) =.50 
      BLDFLT(5) =1.0 
      BLDFLT(6) =.90 
      BLDFLT(7) =.10 
      BLDHGT    = 2.44 
      IF(IER.NE.0) THEN 
           WRITE(6,*) 
C          WRITE(6,*) ' DEFAULT BUILDING PARAMETERS' 
c          WRITE(6,*) ' Indoor concentrations based on single story' 
c          WRITE(6,*) ' building with minimum filtration' 
      ELSE 
          WRITE(6,*) ' Indoor parameter file used ',FILBLD 
          READ(IT,*,END=10,ERR=10) BLDHGT 
          READ(IT,*,END=10,ERR=10) BLDFLT 
10        CONTINUE 
      ENDIF 
C      WRITE(6,*) ' Bld hgt(M)=',BLDHGT 
C      WRITE(6,*) ' Bld filter pass fractions = ',BLDFLT 
      RETURN 
      END 
C 
C     RDMET/RAPSCD 021493 BATTELLE PNL, RICHLAND WA 
C 
        SUBROUTINE RDMET(IM,IEOF) 
C       VERSION 091887 JGD 
       INCLUDE 'ATMOS.INA' 
        DIMENSION UX(6) 
        TDAYS=365. 
        PNAME='                                                  ' 
        READ (IM,5000,END=999) PNAME 
        READ (IM,5001,END=999) HMSTAB,HMUNST,DPRECIP,TSDNUM,ANNRAIN 
C         HMSTAB  =  Mixing Height stable,m 
C         HMUNST  =  Mixing Height unstable,m 
C         DPRECIP =  Number of precip. days > 0.01",#/yr 
C         NUMTSD  =  Average number of thunderstorms per year,#/yr 
C         ANNRAIN =  Ave Total Precip. for run period, inches(->cm) 
        WRITE (6,5200) PNAME,HMSTAB,HMUNST,DPRECIP,TSDNUM,ANNRAIN 
        ANNRAIN = ANNRAIN*2.54 
        IF( dprecip .GT. 0 ) THEN
           RAINRT = 0.6 * ( TSDNUM / DPRECIP ) + 0.1 
        ELSE
           RAINRT = 0.0
        ENDIF
        READ (IM,5100,END=999) (UX(JD),JD=1,6) 
        IF (UX(1).GT.0.0) THEN 
C       CHANGE DEFAULT WIND VALUES... 
             DO 100 JD=2,7 
100              U(JD)=UX(JD-1) 
        ENDIF 
        RETURN 
999     IEOF=IM 
        RETURN 
 5000 FORMAT (A40) 
 5001 FORMAT (2F10.0,3E10.1) 
 5100 FORMAT (8F10.3) 
 5200 FORMAT (' METEOROLOGICAL DATA: ',A40/ 
     +        '    Mixing Heights   = ',2F10.0,' m'/ 
     +        '    Num Ppt Days     = ', F10.2,' #/yr'/ 
     +        '    Num of Thundst   = ', F10.2,' #/yr'/ 
     +        '    Total Ann Ppt    = ', F10.1,' inches/yr') 
        END 
C 
C     RDHTZ/RAPSCD 021493/JGD BATTELLE PNL, RICHLAND WA 
C 
        SUBROUTINE RDHTZ(IM,IEOF) 
       INCLUDE 'ATMOS.INA' 
       INCLUDE 'DRYD.INA' 
       INCLUDE 'DPSTN.INA' 
C 
C****** Reads site parameters 
             READ(IM,6100,END=999) 
     1       ANHGT, ZOS 
             IF(ANHGT.NE.0.0) THEN 
                IF(ANHGT.LT.0.0) THEN 
                     WRITE(6,*) '   Default Anemometer Height' 
                     ANHGT=-1.*ANHGT 
                ENDIF 
                IF(ANHGT.LT.1.0) ANHGT=1.0 
                IF(ZOS.LT.0.0) THEN 
                     WRITE(6,*) '   Default Anemometer Zo' 
                     ZOS=-1.*ZOS 
                ENDIF 
                WRITE(6,500) ANHGT,ZOS 
             ENDIF 
500          FORMAT( 
     +          '    Wind Sensor Ht   = ',F10.1,' m'/ 
     +          '    Roughness Length = ',G10.3,' cm') 
        RETURN 
999     IEOF=IM 
        RETURN 
 6000 FORMAT (2X,I2,2X,8F8.3/6X,8F8.3) 
 6100 FORMAT (16F5.2) 
        END 
C 
C      HMX/RAPSCD 101887 BATTELLE PNL, RICHLAND WA 
C 
       SUBROUTINE HMX(KS,HGT,WHGT) 
       DIMENSION HMAX(7) 
C      SET LIMIT TO HEIGHT: RAPSCD/101787/JGDROPPO 
       DATA HMAX/4*200.,50.,50.,50./ 
       IF(HMAX(KS).LT.HGT) THEN 
           WHGT=HMAX(KS) 
       ELSE 
           WHGT=HGT 
       ENDIF 
       RETURN 
       END 
C 
C 
C     SIGZT/RAPSCD 021991 BATTELLE PNL, RICHLAND WA 
C 
      SUBROUTINE SIGZT(TMET,IST,WS,SIZ,SIY) 
C 
C Formulation is based on Briggs (1973) as given in Hanna, Briggs, and 
C Hosker (1982), Handbook on Atmospheric Diffusion, DOE/TIC-11223, U. S. 
C Department of Energy (page 30).  Values are extrapolations for range 10 to 
C 50 km. 
C 
C     July 3, 1993/jgd/sigy added 
C 
C     COMPUTE EQUIVALENT PASQUILL DISTANCE 
      XIII=WS*TMET 
C 
      GO TO (1,2,3,4,5,6,7) IST 
    1 SIZ=0.2*XIII 
      SIY=0.22*XIII*SQRT(1.+0.0001*XIII) 
      GO TO 8 
    2 SIZ=0.12*XIII 
      SIY=0.16*XIII*SQRT(1.+0.0001*XIII) 
      GO TO 8 
    3 SIZ=0.08*XIII/SQRT(1.+XIII*2.E-4) 
      SIY=0.11*XIII*SQRT(1.+0.0001*XIII) 
      GO TO 8 
    4 SIZ=0.06*XIII/SQRT(1.+XIII*1.5E-3) 
      SIY=0.08*XIII*SQRT(1.+0.0001*XIII) 
      GO TO 8 
    5 SIZ=0.03*XIII/(1.+XIII*3.E-4) 
      SIY=0.06*XIII*SQRT(1.+0.0001*XIII) 
      GO TO 8 
    6 SIZ=0.016*XIII/(1.+XIII*3.E-4) 
      SIY=0.04*XIII*SQRT(1.+0.0001*XIII) 
      GO TO 8 
c   Class G parameters based on E-F progression 
C    /jgd February 19, 1991 
    7 SIZ=0.0085*XIII/(1.+XIII*3.E-4) 
      SIY=0.027*XIII*SQRT(1.+0.0001*XIII) 
    8 CONTINUE 
      RETURN 
      END 
C 
C 
C 
C     CVAL/RAPSCD 021991 BATTELLE PNL, RICHLAND WA 
C     
      SUBROUTINE CVAL(CPC,OVRL,ZHGT) 
C 
C     Formulation is based on Paulson (1970) as given in 
C     Numerical Prediction and Dynamic Meteorology, 2nd Edition, 
c     Halter and Williams, (1980), John Wiley and Sons, New York. 
C     JGD  February 19, 1991 
C 
C     Figure 8-4 on page 281 
C     LIMIT CALLS TO Z GE .1 METERS 
      IF (ZHGT.LT.0.1) THEN 
        HGT=0.1 
      ELSE 
        HGT=ZHGT 
      ENDIF 
      CPC=OVRL*HGT 
      IF (OVRL.LT.0) THEN 
C       UNSTABLE 
        CPC=-1.*SQRT(-1.*CPC) 
      ELSE 
C       STABLE 
        CPC=CPC*4.6 
      ENDIF 
C     DONT ALLOW EXTRAPOLATION 
C     BEYOND REFERENCE FIGURE 
      IF (CPC.LT.-1.6) CPC=-1.6 
C      IF (CPC.GT.4.6)  CPC= 4.6 
      RETURN 
      END 
C 
C 
C 
C      UBAR/RAPSCD 012992 BATTELLE PNL, RICHLAND WA 
C 
       SUBROUTINE UBAR(KS,UNEW,HGT,USTAR,ZOIC,ZDISP) 
C 
C         SUBPROGRAM ABSTRACT:  UBAR / VER 10-20-1987 jgd 
C         UBAR IS A SUBROUTINE FOR COMPUTING THE WIND SPEED AS A 
C         FUNCTION OF HEIGHT OVER LAND OR WATER SURFACES.  THIS 
C         ROUTINE COMPUTES WIND (UNEW) AT HEIGHT (WHGT) FOR 
C         STABILITY (KS) FOR THE INPUT VALUES OF USTAR AND ZO 
C         JG DROPPO, BATTELLE, PACIFIC NORTHWEST LABORATORY, RICHLAND, WA. 
C 
      DIMENSION CPM(3) 
C      DATA CPM/3.586122,1.597256,0.457900/ 
       DATA CPM/0.4579,0.4579,0.4579/ 
C     "C" VALUE USED FOR A & B JGD February 8, 1991 
C     CPM IS SUM OF TERMS FOR CENTRAL UNSTABLE CONDITIONS 
      IF(ZOIC.GT.20000.) THEN 
        ZOC=20000. 
      ELSE 
        ZOC=ZOIC 
      ENDIF 
      ZO=ZOC/100. 
      CALL MONIN( ZOC, KS, OVRL) 
      CALL HMX(KS,HGT,WHGT) 
      WHGT=WHGT+ZO 
      IF(WHGT.LT.ZDISP) WHGT=ZDISP 
C     Compute new wind speed 
      IF (KS .LT.4) THEN 
C     UNSTABLE 
            TEST=(ALOG(WHGT/ZO)-CPM(KS)) 
            IF(TEST.LT.0.001) TEST=0.001 
            UNEW= USTAR/.4*TEST 
C           UNEW= USTAR/.4*(ALOG(WHGT/ZO)-CPM(KS)) 
      ELSE 
           UNEW = (USTAR/.4)*(ALOG(WHGT/ZO)+5.0*WHGT*OVRL) 
      ENDIF 
      RETURN 
      END 
C 
C     UWAT/RAPSCD 012992 BATTELLE PNL, RICHLAND WA 
C 
      SUBROUTINE UWAT(KS,U,HGT,USTAR,ZOC,ZDISP) 
C 
C         SUBPROGRAM ABSTRACT:  UWAT / VER 10-20-1987 JGD 
C         USTAR AND ZO OVER AN OPEN WATER SURFACE ARE COMPUTED 
C         FOR STABILITY (KS) AND WIND SPEED (U) AT HEIGHT (WHGT). 
C         JG DROPPO, BATTELLE, PACIFIC NORTHWEST LABORATORY, RICHLAND, WA. 
C 
C    NOTE: U is wind speed at 10m. 
C 
      DIMENSION CPM(3) 
      DATA CPM/0.4579,0.4579,0.4579/ 
C     CPM IS SUM OF SIMILARITY TERMS FOR CENTRAL CONDITIONS 
      CALL HMX(KS,HGT,HHGT) 
      NIR=20 
C     Define initial values for turbulence parameters 
      ZDISP=0.0 
      USTAR=U*(.0012)**.5 
      ZO = .0144*USTAR*USTAR/9.8 
      ZOC=ZO*100. 
      IDIF=10. 
      CALL MONIN( ZOC, KS, OVRL) 
C     Compute wind speed parameters 
      IF (KS .LT.4) THEN 
C     UNSTABLE 
            RMAX =EXP(CPM(KS)+.4) 
            DO 265 I = 1,NIR 
              IF(IDIF.LE.1) GOTO 265 
              ZO = .0144*USTAR*USTAR/9.8 
              ZDISP=0.0 
              WHGT=HHGT+ZO 
              RATIO=WHGT/ZO 
              IF(RATIO.LT.RMAX) THEN 
C                  USTAR WILL BE GT U; ASSUME NEAR UNIFORM WINDS 
C                  AND ADJUST HEIGHT SO THAT USTAR IS LE U 
                   ZDISP=ZO*RMAX 
                   WHGT=ZDISP 
              ENDIF 
              UNEW= USTAR/.4*(ALOG(WHGT/ZO)-CPM(KS)) 
              STAR= USTAR*(1+(U-UNEW)/(U+UNEW)*2) 
              IF (STAR.LE.0.0) THEN 
                   USTAR=USTAR*0.9 
              ELSE 
                   USTAR=STAR 
              ENDIF 
              IDIF=ABS((U-UNEW)*10000.) 
265        CONTINUE 
           ZOC=ZO*100. 
      ELSE 
C     NEUTRAL AND STABLE 
           DO 320 I=1,NIR 
              IF(IDIF.LE.1) GOTO 320 
              ZO = .0144*USTAR*USTAR/9.8 
              ZOC=ZO*100. 
              WHGT=HHGT+ZO 
              CALL MONIN( ZOC, KS, OVRL) 
              UNEW = (USTAR/.4)*(ALOG(WHGT/ZO)+5.0*WHGT*OVRL) 
              STAR= USTAR*(1+(U-UNEW)/(U+UNEW)*2) 
              IF (STAR.LE.0.0) THEN 
                   USTAR=USTAR*0.9 
              ELSE 
                   USTAR=STAR 
              ENDIF 
              IDIF=ABS((U-UNEW)*1000.) 
320        CONTINUE 
       ENDIF 
       RETURN 
       END 
C 
C      ULAN/RAPSCD 012992 BATTELLE PNL, RICHLAND WA 
C 
       SUBROUTINE ULAN(KS,U,HGT,USTAR,ZOIC,ZDISP) 
C 
C         SUBPROGRAM ABSTRACT:  ULAN / VER 10-20-1987 
C         USTAR FOR A LAND SURFACE IS COMPUTED FOR ZO, 
C         STABILITY (KS), AND WIND SPEED (U) AT HEIGHT (WHGT). 
C         JG DROPPO, BATTELLE, PACIFIC NORTHWEST LABORATORY, RICHLAND, WA. 
C 
      DIMENSION CPM(3) 
      DATA CPM/0.4579,0.4579,0.4579/ 
C     "C" VALUE USED FOR A & B JGD February 8, 1991 
C     CPM IS SUM OF SIMILARITY TERMS FOR CENTRAL CONDITIONS 
      ZDISP = 0.0 
      IF(ZOIC.GT.20000.) THEN 
        ZOC=20000. 
      ELSE 
        ZOC=ZOIC 
      ENDIF 
      ZO=ZOC/100. 
      CALL MONIN( ZOC, KS, OVRL) 
      ZO=ZOC/100. 
      CALL HMX(KS,HGT,HHGT) 
      WHGT=HHGT+ZO 
C     Compute wind speed parameters 
      IF (KS .LT.4) THEN 
C          UNSTABLE 
           RMAX =EXP(CPM(KS)+.4) 
           RATIO=WHGT/ZO 
           IF(RATIO.LT.RMAX) THEN 
C               USTAR WILL BE GT U; ASSUME NEAR UNIFORM WINDS 
C               AND ADJUST HEIGHT SO THAT USTAR IS LE U 
                ZDISP=ZO*RMAX 
                WHGT=ZDISP 
           ENDIF 
           USTAR= U*.4/(ALOG(WHGT/ZO)-CPM(KS)) 
       ELSE 
C          NEUTRAL AND STABLE 
           CALL MONIN( ZOC, KS, OVRL) 
           USTAR=U*.4/(ALOG(WHGT/ZO)+5.0*WHGT*OVRL) 
       ENDIF 
       RETURN 
       END 
C 
******** DATTIM.FOR **************************************************** 
*  DATTIM/RAPSCD 101887 BATTELLE PNL, RICHLAND WA 
* 
*  Routine gives RAPSCD access to PC system date and time 
*  in character format via MS FORTRAN 4.0 character format. 
*  JG Droppo VER-020698. 
*
c  Update Log:
c   Feb 4, 1998 /jgd/ year 2000 update
c
c
c 
************************************************************************ 
      SUBROUTINE DATTIM ( RDATE, RTIME ) 
c      CHARACTER*10  RDATE, RTIME 
      CHARACTER*12  RDATE
      CHARACTER*10  RTIME 
      INTEGER*2 JYR,JMON,JDAY,JHR,JMIN,JSEC,J100TH 
      CALL GETDAT (JYR,JMON,JDAY) 
c      Feb 4, 1998/jgd/2000 year update
c      JYR=JYR-(JYR/100)*100 
c      WRITE (RDATE,'(I2.2,1H/,I2.2,1H/,I2.2,2H  )') JMON,JDAY,JYR 
      WRITE (RDATE,'(I2.2,1H/,I2.2,1H/,I4.4,2H  )') JMON,JDAY,JYR 
      CALL GETTIM (JHR,JMIN,JSEC,J100TH) 
      WRITE (RTIME,'(I2.2,1H:,I2.2,1H:,I2.2,2H  )') JHR,JMIN,JSEC 
      RETURN 
      END 
C 
        FUNCTION cowet(ALCO,RAINRT,IR) 
C       122388JGD 
C****   Computes wet deposition scavenging coeff.**** 
          DIMENSION SCOR(7) 
          DATA SCOR/1.,1.,1.,1.,1.,1.,1./ 
C         SCOR - ALLOWS SCAVENGING RATE TO VARY WITH TYPE OF MATERIAL 
          COWE = 0          . 
          IF (RAINRT.GT.0.) COWE=1.43*SCOR(IR)*ALCO*(RAINRT*10.)**.75 
c             LIMIT RANGE OF COWET (/HR) 
          IF (COWE.LT.0.8) COWE = 0.8 
          IF (COWE.GT.4.0) COWE = 4.0 
          COWE=COWE/3600. 
          IF(IR.EQ.1) THEN 
            WRITE(6,100) ' Scavenging Coefficient (s-1) = ',COWE 
            WRITE(6,100) ' Scavenging Efficiency (frac) = ',ALCO 
            WRITE(6,100) ' Rain rate (mm/h)             = ',RAINRT*10. 
  100       FORMAT (A,G10.3) 
          ENDIF 
          COWET=COWE 
        RETURN 
        END 
C 
C     INIT/RAPSCD 102687 BATTELLE PNL, RICHLAND WA 
C 
        SUBROUTINE INIT 
       INCLUDE 'ATMOS.INA' 
          DO 29 I=1,16 
          DO 29 J=1,NDIST 
             ACON(I,J)=0.0 
             ACOB(I,J)=0.0 
             AWET(I,J)=0.0 
             ADRY(I,J)=0.0 
29           ATOT(I,J)=0.0 
        RETURN 
        END 
C 
C     COINIT/RAPSCD 102387 BATTELLE PNL, RICHLAND WA 
C 
        SUBROUTINE coinit 
C       VERSION 102386-JGD / COMPLEX TERRAIN ADDED/010288-JGD ILC ADDED 
c       February 8, 1998/jgd/internal distance def moved to SETINT
c       comment removed from line/jgd/may 8, 2001 
c
       INCLUDE 'ATMOS.INA' 
       INCLUDE 'DPSTN.INA' 
       INCLUDE 'DRYD.INA' 
C     DEFINE COMMON CONSTANTS (MS FORTRAN VERSION) 
      ILC(1)=0 
      ILC(2)=0 
      ILC(3)=0 
      ILC(4)=1 
      ILC(5)=0 
      ILC(6)=0 
      ILC(7)=0 
C     WIND SPEED GROUPS IN M/S 
      U(1)=0.50 
      U(2)=0.50 
      U(3)=1.52 
      U(4)=3.02 
      U(5)=4.92 
      U(6)=7.26 
      U(7)=10.28 
      RSG(1)=1.0  ! comment removed from this line/jgd/may 8, 2001 
      RSG(2)=1.0 
      RSG(3)=1.0 
      RSG(4)=1.0 
      RSG(5)=1.0 
      RSG(6)=1.0 
      RSG(7)=1.0 
      DI(1)='  S' 
      DI(2)='SSW' 
      DI(3)=' SW' 
      DI(4)='WSW' 
      DI(5)='  W' 
      DI(6)='WNW' 
      DI(7)=' NW' 
      DI(8)='NNW' 
      DI(9)='  N' 
      DI(10)='NNE' 
      DI(11)=' NE' 
      DI(12)='ENE' 
      DI(13)='  E' 
      DI(14)='ESE' 
      DI(15)=' SE' 
      DI(16)='SSE' 
      VIR(1)=5.55 
      VIR(2)=10.0 
      VIR(3)=12.5 
      VIR(4)=20.0 
      VIR(5)=33.3 
      VIR(6)=66.7 
      VIR(7)=130. 
      DO 40 I=1,7 
40    QNN(I)=1. 
      NTY=7
C      SET TERRAIN HEIGHTS TO ZERO 
C 
          DO 2 JD=1,16 
          DO 2 IDD=1,6 
2             HTER(JD,IDD)=0.0 
      RETURN 
      END 
C 
      SUBROUTINE OUTRES(A,IOL,IPLOT) 
C     OUTRES- A SUBROUTINE TO OUTPUT RESULTS 
C     RAPSCD 101887 BATTELLE PNL, RICHLAND WA 
C     VERSION 110488 JGD 
       INCLUDE  'ATMOS.INA' 
      DIMENSION A(16,12),SUM(12) 
c          jgd/February 8, 1998 
           if(xmin.gt.0.0) then
             i100m=1
           else
             i100m=3
           endif
      IF(IPLOT.GT.0) 
     +      WRITE (IOL,100) (X(i100m)/1000.),(X(7)/1000.), 
     +      (X(10)/1000.),(X(11)/1000.),(X(12)/1000.) 
C 
      IF(IPLOT.GT.0) THEN 
C 
        DO 10 M=1,12 
   10      SUM(M) =0. 
        DO 30 K=1,16 
           DO 20 M=1,12 
   20          SUM(M) = SUM(M)+A(K,M)
c          jgd/February 8, 1998 
           WRITE(IOL,200) 
     +     DI(K),A(K,I100m),A(K,7),A(K,10),A(K,11),A(K,12)
   30      CONTINUE 
        DO 40 M=1,12 
   40      SUM(M) = SUM(M)/16. 
c          jgd/February 8, 1998 
             WRITE (IOL,200)
     +       'AVE',SUM(I100m),SUM(7),SUM(10),SUM(11),SUM(12) 
C 
      ENDIF 
      DO 60 K=1,16 
           KR=K+8 
           IF (KR.GT.16) KR=KR-16 
           DO 50 M=1,NDIST 
                IF (A(K,M).LE.0.)  THEN 
                     A(K,M)=-99. 
                ELSE 
                     A(K,M) = ALOG10(A(K,M)) 
                ENDIF 
   50      CONTINUE 
   60 CONTINUE 
      RETURN 
  100 FORMAT (/' DIRECTION' T35 'DISTANCE (km)'/6X,5G14.3/) 
  200 FORMAT (3X,A3,1P,5E14.2,0P) 
      END 
C     DRYVEL/RAPSCD 101887 BATTELLE PNL, RICHLAND WA 
C 
        SUBROUTINE DRYVEL(UWS,JWD,JDS,JST,IZ,RHOP,R,ZRO) 
C       VERSION 100386 JGD 
c       CHARACTER*1 LAB(11)/jgd/July 27, 1994 
       INCLUDE 'ATMOS.INA' 
       INCLUDE 'DRYD.INA' 
       INCLUDE 'DPSTN.INA' 
C       Compute atmospheric resistance, RAIR 
        IF(JST.LE.6)THEN 
                IST=JST 
        ELSE 
                IST=6 
        ENDIF 
        ZZERO=ZRO 
        CALL RAIR(ZZERO,IST,UWS,RIS,IZ,UR) 
        IF(R.LE.0.0) THEN 
C               GASEOUS POLLUTANT FLUX 
C               uses input array of surface resistances in s/m 
                IF(R.EQ.0.) THEN 
C               USE SURFACE RESISTANCES 
                   VD=1.0/(SURR(JWD,JDS)+RIS) 
                   VS=0.0 
                ELSE 
                   IF (R.EQ.-0.001) THEN 
C                  NO DEPOSITION 
                      VD=0.0 
                      VS=0.0 
                   ELSE 
C                  USE ZERO SURFACE RESISTANCE 
                      VD=1.0/RIS 
                      VS=0.0 
                   ENDIF 
                ENDIF 
        ELSE 
C               PARTICULATE POLLUTANT FLUX 
C               uses Sehmel's curves 
                CALL VDVS(RHOP,R,UR,ZZERO) 
                VDS=VD-VS 
                IF(VDS.LE.0) THEN 
                        VD=VS 
                ELSE 
                        VD=1.0/(1./VDS+RIS)+VS 
                ENDIF 
        ENDIF 
10              CONTINUE 
        RETURN 
        END 
C 
C     RAIR/RAPSCD 101887 BATTELLE PNL, RICHLAND WA 
C 
        SUBROUTINE RAIR(ZZERO,ISTABL,U,RA,IZ,UR) 
C       This computes the atmospheric resistance to dry deposition 
C       based on analogy with heat transport. JGDROPPO 10/82 
C               DH=INVERSE MONIN-OBUKHOV LENGTH, 1/m 
C               ZTA=REF HEIGHT*DH, non-dimensional 
C               RATIO=RATIO OF MOMENTUM AND HEAT TRANSPORT RATES 
C               RA=ATMOSPHERIC RESISTANCE, s/m 
C               U=WIND SPEED AT 'IZ' M HEIGHT, m/s 
C               ZZERO=LOCAL ROUGHNESS LENGTH, cm 
C               UR=FRICTION VELOCITY/ , m/s 
       INCLUDE 'DPSTN.INA' 
        CALL MONIN(ZZERO,ISTABL,DH) 
C       Compute Momentum Resistance 
C       Compute correction to get Heat Resistance 
C       COMPUTATION HT IS IZ PLUS SURFACE DISPLACEMENT HT (20*ZO) 
C       JGD 9-14-86 
        ZHT=IZ*100.+ZZERO*20. 
        ZTA=ZHT*DH/100. 
        IF(ZTA.LE.0) THEN 
C               UNSTABLE CONDITIONS 
                X=(1.-15.*ZTA)**.25 
                UR=U*.35/(ALOG(ZHT/ZZERO)-(ALOG((1+X*X)/2.) 
     1          +2.*ALOG((1+X)/2.0)-2.*ATAN(X)+1.5708)) 
                RA=U/UR/UR 
                RATIO=1.35*SQRT(1.-9*ZTA)/(1.-15.*ZTA)**.25 
        ELSE 
C               STABLE CONDITIONS 
                UR=U*.35/(ALOG(ZHT/ZZERO)+4.7*ZTA) 
                RA=U/UR/UR 
                IF(ZTA.LT.0.5) THEN 
                        RATIO=1.35-ZTA*.30 
                ELSE 
                        RATIO=1.2 
                ENDIF 
        ENDIF 
        RA=RA/RATIO 
        RETURN 
        END 
C 
C     VDVS/RAPSCD 101887 BATTELLE PNL, RICHLAND WA 
C 
C       Deposition Velocity obtained from Tom Horst, 5/9/83 
C       Based on George Sehmel's curves for Vd at 0.1 cm 
C       Height 
C 
        SUBROUTINE VDVS(RHOP,R,USTARM,ZO) 
       INCLUDE 'DPSTN.INA' 
        REAL MILKAN,INT3 
        DATA VERR,C1,C2,C3,C4,C5,C6/1.E-4,-1.1205E-3,0.12009,-2.0444E-4, 
     +                              2.9734E-7,-1.2782E-10,-.2121/ 
C 
C       ROUTINE TO CALCULATE VD=DEPOSITION VELOCITY AND VS=SETTLING VELOCITY 
C       IN M/S.   R=PARTICLE RADIUS IN CM,   USTARM=FRICTION VELOCITY IN M/S 
C       ZOM=ZO IN CM,  RHOP=PARTICLE SPECIFIC GRAVITY. 
C 
cx        write(*,*) ' DebugInfo: call to vdvs with'
cx        write(*,*) ' RHOP  =',RHOP
cx        write(*,*) ' R     =',R
cx        write(*,*) ' USTARM=',USTARM
cx        write(*,*) ' ZO    =',ZO
cx        pause

        ZOM = ZO/100.
        USTAR=USTARM*100. 
        A2 = R*R 
C       P = ATMOSPHERIC PRESSURE = 76 CM OF MERCURY 
        PA = 76.*R 
        MILKAN = 1. + 1.E-4/PA*(6.32 + 2.01*EXP(-2190.*PA)) 
C 
C       SETTLING VEL = 2/9*RHOP*G/VISCOSITY*A2*(A CORRECTION) 
        VS = 1.225E6*RHOP*A2 
        IF(R.LE.5.E-4) THEN 
C 
C               SETTLING VELOCITY FOR SMALL PARTICLES 
                VS = VS*MILKAN/100. 
        ELSE 
C 
C               SETTLING VELOCITY FOR LARGE PARTICLES 
                VT2 = VS 
   16             VT1 = VT2 
                  RE = 13.483*R*VT1 
                  CD=1.+(C1+C2*RE+C3*RE*RE+C4*RE**3+C5*RE**4)*RE**C6 
                  VT2 = VS/CD 
                  IF(ABS((VT2-VT1)/VT2).GT.VERR) GO TO 16 
                VS = VT2/100. 
        ENDIF 
C       CALCULATE DEPOSITION VELOCITY AT ONE CM HEIGHT 
C       D = BROWNIAN DIFFUSIVITY = KT/(6*P*PVISCOSITY*R)*MILKAN 
        D = 1.2174E-11/R*MILKAN 
C       SC = SCHMIDT NUMBER = KINEMATIC VISCOSITY/D 
        SC = 0.14833/D 
C       TPLUS = 2/9*RHOP/VISCOSITY*A2*USTAR2/KINEMATIC VISCOSITY 
C       RHOP = 1.5 GM/CM3 FOR ALL TPLUS 
        TPLUS = 12625.*A2*USTAR*USTAR 
        TPLUS = TPLUS*1.E-8 
        ALT = ALOG(TPLUS) 
        INT3 = EXP(-378.051 + 16.498*ALOG(SC) + ALT*(-11.8178 
     +             -0.28628*ALT + 0.32262*ALOG(2.*R/ZO) 
     +             -0.3385*ALOG(D/ZO/USTAR)) - 12.8044*ALOG(2.*R)) 
        VD = VS/(1.-EXP(-VS/USTARM*INT3)) 
        RETURN 
        END 
C 
C     RAPSCD-INDEXD 012288 JGDROPPO/BATTELLE PNL, RICHLAND WA 
C 
C     REVISION RECORD 
C     June 28, 1993 JGD CHANGED FROM FUNCTION TO SUBROUTINE 
C                   AND MADE INDEX VALUES MATCH GUIDANCE 
C                   (SEE DROPPO ET AL 1989 VOL 2, TABLE 3.3) 
C                   SUPPORTS INPUTS OF ZO'S FOR 4 OR 6 DISTANCES 
C 
C     RETURNS INPUT DATA INDEX (1 TO 6) FOR DISTANCE XN (M) 
      SUBROUTINE INDEXD(XD,IT) 
      DIMENSION X4(4),X6(5) 
       INCLUDE 'ATMOS.INA' 
C     IX CONTAINS THE OUTER BOUND DISTANCES FOR ZO DATA INPUT GROUPS 
      DATA X4/200.,1000.,3000.,10000./ 
      DATA X6/200., 650., 2000., 5000., 15000./ 
      CUR=XD 
      IT=1 
      IF (IZ0.EQ.3) THEN 
C        4 DISTANCE FORMAT 
         DO 10 I=1,4 
            IF(CUR.GT.X4(I)) THEN 
               IT=I+1 
            ENDIF 
10       CONTINUE 
      ELSE 
C        6 DISTANCE FORMAT 
         DO 20 I=1,5 
            IF(CUR.GT.X6(I)) THEN 
               IT=I+1 
            ENDIF 
20       CONTINUE 
      ENDIF 
      END 
C 
C     RDJFD/RAPSCD 021493jgd BATTELLE PNL, RICHLAND WA 
C 
C     February 14, 1993 - UPDATE TO READ WS CLASSES, ANM HGT, AND 
C                         SITE ZO FROM JFD FILE / JGD 
      SUBROUTINE RDJFD(IM,IEOF) 
C 
       INCLUDE 'ATMOS.INA' 
      DIMENSION UX(6) 
      CHARACTER*30 FMT 
C 
C       Inputs wind frequency data for RAPSCD in user definable format. 
C       Distributes calm conditions and normalizes sum of table to 1.0. 
C 
c       July 4, 1993/jgd/gsum added to check for "G" data 
c 
        RFT=0.0 
        IFMT=0 
        WRITE(6,'(1H1)') 
        WRITE(6,*) 'LISTING OF WIND JOINT FREQUENCY INPUT DATA' 
        READ(IM,'(F5.0,A30)') WIL,FMT 
        IF(WIL.EQ.6..OR.WIL.EQ.16.) IFMT=1 
        IF(WIL.EQ.7..OR.WIL.EQ.17.) IFMT=2 
        IF(IFMT.EQ.0) THEN 
             WRITE(6,*) 'Default format for wind data used' 
             BACKSPACE(IM) 
             FMT='(16F5.2)                      ' 
             RFT=0.0 
C 
             DO 302 IST = 1,7 
             WRITE(6,*)' JF, STAB=',IST 
             DO 301 IWS = 2,7 
                  READ(IM,FMT,END=301)(RF(IST,IWS,IWD),IWD=1,16) 
                  WRITE(6,6200) IWS,(RF(IST,IWS,IWD),IWD=1,16) 
301          CONTINUE 
302          CONTINUE 
        ELSE 
            IF (IFMT.EQ.2) THEN 
              READ (IM,5100,END=999) (UX(JD),JD=1,6) 
5100          FORMAT (8F10.3) 
              IF (UX(1).GT.0.0) THEN 
C             CHANGE DEFAULT WIND VALUES... 
                DO 100 JD=2,7 
100               U(JD)=UX(JD-1) 
              ENDIF 
              READ(IM,6100,END=999) ANHGT, ZOS 
6100          FORMAT (16F5.2) 
              IF(ANHGT.LT.0.0) THEN 
                WRITE(6,*) '   Default Anemometer Height' 
                ANHGT=-1.*ANHGT 
              ENDIF 
              IF(ANHGT.LT.1.0) ANHGT=1.0 
2             IF(ZOS.LT.0.0) THEN 
                WRITE(6,*) '   Default Anemometer Zo' 
              ZOS=-1.*ZOS 
              ENDIF 
              WRITE(6,500) ANHGT,ZOS 
500           FORMAT( 
     +   '    Wind Sensor Ht   = ',F10.1,' m'/ 
     +   '    Roughness Length = ',G10.3,' cm') 
              WIL=WIL-1.0 
            ENDIF 
            WRITE(6,*) 'User format for wind data used - ',FMT 
            IF (WIL.EQ.6.) THEN 
                  DO 312 IST = 1,7 
                  WRITE(6,*)' STAB=',IST,' ----- WIND SPEED (M/S) --->' 
                  WRITE(6,'(A,6G10.3)') ' DIR',(U(IWS),IWS=2,7) 
                  DO 311 IWD=1,16 
                      READ(IM,FMT,END=311)(RF(IST,IWS,IWD),IWS = 2,7) 
                      WRITE(6,6300) IWD,(RF(IST,IWS,IWD),IWS = 2,7) 
311               CONTINUE 
312               CONTINUE 
             ELSE 
                  DO 322 IST = 1,7 
                  WRITE(6,*)'WS (STAB=',IST,') -- WIND DIRECTION -->' 
                  DO 321 IWS = 2,7 
                       READ(IM,FMT,END=321)(RF(IST,IWS,IWD),IWD=1,16) 
                       WRITE(6,6200) IWS,(RF(IST,IWS,IWD),IWD=1,16) 
321               CONTINUE 
322               CONTINUE 
             ENDIF 
        ENDIF 
C 
C       ROUTINE TO DISTRIBUTE CALMS - JGD-M 
C 
        READ(IM,FMT,END=330,ERR=330) (RF(IST,1,1),IST=1,7) 
        WRITE(6,*) '   Calm inputs = ',(RF(IST,1,1),IST=1,7) 
        DO 226 IST = 1,7 
             RFT=0.0 
             RVAL=RF(IST,1,1) 
             IF(RVAL.GT.0.0) THEN 
                 DO 224 IWD = 1,16 
                     RFT=RFT + RF(IST,2,IWD) 
224              CONTINUE 
                 DO 225 IWD = 1,16 
                     IF(RFT.GT.0.0) THEN 
C                         DISTRIBUTE USING RATIOS 
                          RF(IST,1,IWD)=RF(IST,2,IWD)/RFT*RVAL 
                     ELSE 
C                         DISTRIBUTE EVENLY 
                          RF(IST,1,IWD)=RVAL/16. 
                     ENDIF 
225              CONTINUE 
             ENDIF 
226     CONTINUE 
C 
C       SUM JF TABLE ******************************************* 
C 
330     RFT=0.0 
        DO 333 IST = 1,7 
        DO 333 IWS = 1,7 
        DO 333 IWD  = 1,16 
333          RFT=RFT+RF(IST,IWS,IWD) 
C 
        WRITE(6,*) ' Sum of JF winds = ', RFT 
c 
c       Normalize joint freq data (rf) if needed **************** 
c       and print all non-zero value if IPRT=5 option is selected 
c 
        IF (RFT.GT.1.01.OR.RFT.LT.0.99) THEN 
           WRITE(6,*) ' WIND DATA TABLE WAS NORMALIZED TO 1.0' 
           RFZ=1 
        ELSE 
           RFZ=0 
        ENDIF 
        DO 340 IST = 1,7 
        DO 340 IWD=1,16 
        DO 340 IWS = 1,7 
           IF (RFZ.GT.0) RF(IST,IWS,IWD)=RF(IST,IWS,IWD)/RFT 
           IF (IPRT.EQ.5) THEN 
              IF(RF(IST,IWS,IWD).GT.0.0) write(6,'(A,3I3,F10.6)')  
     +        ' rf(ist,IWS,iwd)',IWS,iwd,ist,rf(ist,IWS,iwd) 
           ENDIF 
340     continue 
c 
c       See if G class has data (used in acute computation) **** 
c 
        IST = 7 
        gsum=0.0 
        DO 350 IWS = 1,7 
        DO 350 IWD=1,16 
350       gsum=RF(IST,IWS,IWD)+gsum 
        RETURN 
999     IEOF=IM 
        RETURN 
 6200 FORMAT (I2,8G9.3/'  ',8G9.3) 
 6300 FORMAT (I2,6G10.4) 
        END 
C 
C     REFLEC/RAPSCD 071293 BATTELLE PNL, RICHLAND WA 
C 
      SUBROUTINE REFLEC(IL,IU,SZ,HPLM,HCOMP,HMIX,ZFAC) 
C 
C         SUBPROGRAM ABSTRACT:  REFLEC / VER 10-18-1987 JGD 
C         REFLECTION TERMS ARE COMPUTED: 
C             IL & IU = INDEXES RANGE 
C             SZ = SIGMA-Z, 
C             HMIX = HEIGHT OF MIXED LAYER 
C             HCOMP = HEIGHT FOR CONCENTRATION COMPUTATION 
C             HPLM  = AVERAGE PLUME HEIGHT OVER TERRAIN 
C 
C         Factors less than 1.92874E-22 are set to zero. 
C 
C         BASED ON MESOI APPL. OF CSANADY (1973). 
C         JG DROPPO, BATTELLE, PACIFIC NORTHWEST LABORATORY, RICHLAND, WA. 
C          
         ZFAC=0.0 
         DO  200 I=IL,IU 
            TERM1=-.5*((HCOMP-HPLM-2.*I*HMIX)/SZ)**2. 
            IF(TERM1.LT.-50.) GOTO 150 
                ZFAC=ZFAC+EXP(TERM1) 
150         TERM2=-.5*((HCOMP+HPLM-2.*I*HMIX)/SZ)**2. 
            IF(TERM2.LT.-50.) GOTO 200 
                 ZFAC=ZFAC+EXP(TERM2) 
200      CONTINUE 
      RETURN 
      END 
C 
C      MONIN/RAPSCD 012992 BATTELLE PNL, RICHLAND WA 
C 
       SUBROUTINE MONIN(ZZERO,ISTABL,OVRL) 
C      SUBROUTINE MONIN - gives Monin-Obukhov lengths as a function of 
C       stability (ISTABL) and roughness length (ZZERO) in cm. 
C       The 1/L value (OVRL) is returned in 1/m units. 
C       based on D.Golder, Boun.Layer Met.,3:56 (1972). 
C       JGDROPPO,10/82,01/92 
       INCLUDE 'DPSTN.INA' 
        REAL OVRL,SLOPE(6),CONST(6) 
C       DATA SLOPE/.01534,.01022,.00767,0.0,-.00767,-.01278/ 
        SLOPE(1)=.01534 
        SLOPE(2)=.01022 
        SLOPE(3)=.00767 
        SLOPE(4)=0.0 
        SLOPE(5)=-.00767 
        SLOPE(6)=-.01278 
C       DATA CONST/-.14,-.09,-.04,0.0,.04,.08/ 
        CONST(1)=-.14 
        CONST(2)=-.09 
        CONST(3)=-.04 
        CONST(4)=0.0 
        CONST(5)=.04 
        CONST(6)=.08 
        IF (ZZERO.LE.0) THEN 
           ZZERO=.0000001 
C           WRITE(*,*) ' ZZERO LE 0 CHANGED TO .0000001'                ! BLH 
        ENDIF 
        ALOGZ0=ALOG(ZZERO) 
C       July 4, 1993/JGD/"G" STABILITY SET SAME AS "F" 
        IF (ISTABL.EQ.7) THEN 
           OVRL=ALOGZ0*SLOPE(6)+CONST(6) 
        ELSE 
           OVRL=ALOGZ0*SLOPE(ISTABL)+CONST(ISTABL) 
        ENDIF 
        RETURN 
        END 

c     New Plume Rise Subroutine /cjf/ May 19, 1998
      REAL FUNCTION FNPLMRS( ist, stk_rad, wp, wndspd,
     &                       etempk, atempk )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     FNPLMRSE.FOR
c     C.J. Fosmire
c     Pacific Norhtwest Lab
c     P.O.Box 999
c     Richland, WA 99352
c
c     Created: 5/19/98
c
c     Description:  Computes the final plume rise for given input.
c
c     Required Modules: None
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT    NONE
      
      REAL        s0(3)
      REAL        atempk, delt, downwash, dtc, etempk,
     &            fb, pi, plmrsfn, p1, p2, s,  
     &            stk_rad, uc, wndspd, wp, wspd    
      
      INTEGER     ist
      
      LOGICAL     momentum
      
      DATA  s0    / 0.02, 0.035, 0.05 /

c     if wp = 0, then no plume rise
      IF( wp .LE. 0 ) THEN
         FNPLMRS = 0.0
         RETURN
      ENDIF

c     Initialize Variables

      pi = 3.14159
      momentum = .FALSE.
      IF( wndspd .LT. 1 ) THEN
         wspd = 1.0
      ELSE
         wspd = wndspd
      ENDIF
      
C **  Prepare to compute momentum and buoyancy fluxes
 
      delt  = etempk - atempk
      
c**   Compute Buoyancy Flux

      fb = 9.8 * delt * wp * stk_rad*stk_rad / etempk      
      
C **  Compute the downwash correction
             
      downwash = 0.0
      IF( wp .LT. (1.5*wspd)) downwash = 4 * stk_rad * (wp / wspd - 1.5)

C **  Start final plume rise calculations  

      IF( ist .LE. 4 ) THEN         !  Neutral or Unstable

C **  Determine if buoyancy is dominant

         IF( delt .LE. 0.0 ) THEN
            momentum = .TRUE.          
         ELSE
            IF( fb .LT. 55 ) THEN
               dtc = 0.0187 * wp**(1./3.) * etempk / (stk_rad)**(2./3.)
               IF( delt .LE. dtc ) momentum = .TRUE.
            ELSE
               dtc = 0.00456 * (wp*wp / stk_rad)**(1./3.) * etempk
               IF( delt .LE. dtc) momentum = .TRUE.
            ENDIF
         ENDIF

         IF( momentum ) THEN   ! Momentum Rise
            plmrsfn = 6 * stk_rad * ( wp / wspd )
         ELSE                  ! Buoyancy Rise
            IF( fb .LT. 55 ) THEN
               plmrsfn = 1.6 * 49 ** (2./3.) * fb ** 0.75 / wspd
            ELSE
               plmrsfn = 1.6 * 119 ** (2./3.) * fb ** 0.60 / wspd
            ENDIF  
         ENDIF

      ELSE                             !  Stable

         s = 9.8 * s0(ist-4) / atempk 
  
C **  Determine if buoyancy is dominant

         IF( delt .LE. 0.0 ) THEN
            momentum = .TRUE.          
         ELSE
            dtc = 0.019582 * wp * atempk * SQRT( s )
            IF( delt .LE. dtc ) momentum = .TRUE.
         ENDIF

         IF( momentum ) THEN   ! Momentum Rise
            p1 = 6 * stk_rad * ( wp / wspd )
            p2 = 1.5 / s**(1./6.)
     +      * ((wp*wp*stk_rad*stk_rad)* (atempk/etempk)/wspd)**(1./3.)  
            plmrsfn = AMIN1( p1,p2 )
         ELSE                  ! Buoyancy Rise
          
            uc = 0.2746 * fb**0.25 * s**0.125
            IF( wspd .LT. uc ) THEN
               plmrsfn = 4.0 * fb**0.25 / s**0.375
            ELSE
               plmrsfn = 2.6 * ( fb / (wspd * s))**(1./3.)
            ENDIF
         ENDIF       ! MOMENTUM
     
      ENDIF         ! IST
      
      FNPLMRS = plmrsfn + downwash
      
      RETURN      
      
      END

C
C     PPTFR/RAPSCD 101887 BATTELLE PNL, RICHLAND WA
C
        SUBROUTINE PPTFR(PFC)
C       VERSION 060493 JGD
       INCLUDE 'ATMOS.INA'
C       COMPUTE TIME FOR NEUTRAL CONDITIONS
        IST = 4
        RFSM=0.0
        DO 310 IWS = 1,5
        DO 310 JD=1,16
310          RFSM=RF(IST,IWS,JD)+RFSM
C       RFSM IS TOTAL FRACTION OF POSSIBLE PPT TIME
        ISCAV=1
        IF(RFSM.LE.0.0) ISCAV=0
        IF(TDAYS.LE.0.0) ISCAV=0
c       add logic if rainrt = 0/cjf May 22, 1998
        IF(RAINRT .LE. 0 ) ISCAV = 0
        
        IF(ISCAV.GT.0) THEN
          RFSI=ANNRAIN/RAINRT/(TDAYS*24.)
C         RFSI IS FRACTION OF TIME PPT OCCURS BASED ON TOTAL AND AVE RATE
          IF(RFSM.LT.RFSI) THEN
C            INCREASE PPT RATE TO MATCH ANNUAL TOTAL PPT
             RAINRT=ANNRAIN/RFSM/TDAYS/24.
c            next line added/jgd/September 22, 1994
             RFSI=RFSM
          ENDIF
          PFC=RFSI/RFSM
        ELSE
          RFSI=0.0
          PFC=0.0
        ENDIF
        END
c
C 
C     INDIS/RAPSCD 021188 BATTELLE PNL, RICHLAND WA 
C 
        SUBROUTINE INDIS 
C       VERSION 021188-JGD / COMPLEX TERRAIN ADDED 
       INCLUDE 'ATMOS.INA' 
       INCLUDE 'DPSTN.INA' 
       INCLUDE 'DRYD.INA' 
C 
C     DEFINE DISTANCES 
C 
C       COMPUTE DISTANCE CONSTANTS FOR XMAX, XCHAN, AND XMIN . . . 

        XCHKM=XCHAN/1000.
        XMID=XCHKM
C 
        IF(NUMC.LE.0) THEN 
C            NO VALLEY FLOW 
             if (xmin.gt.0.0) then 
               CALL DEFDIST(XMAX,XMIN,NDIST,XB,XC) 
               DO 5 I = 1,NDIST 
                  XKM(I)=XC*EXP(I*1.)+XB 
5                 CONTINUE 
             else
                   CALL disset(xkm)
             endif
             WRITE(6,'(A/4X,6F10.5/6F10.5)' ) 
     +       ' INTERNAL RADIAL GRID (km)',XKM 
        ELSE 
C       VALLEY CHANNEL 
             NUMR=NDIST-NUMC
c             NUMR = NDIST - NUMC + 1
c
c            Channel flow must use limits to set distances
             if(xmin.eq.0.0) xmin=0.001
             if(xmax.eq.0.0) xmax=75. 
c
             CALL DEFDIST(XCHKM,XMIN,NUMC,XB,XC) 
             WRITE(6,*) 
             WRITE(6,*)' WITHIN CHANNEL GRID (km)' 
             DO 6 I = 1,NUMC 
                  XKM(I)=XC*EXP(I*1.)+XB 
                  WRITE(6,'(I4,G10.3)') I,XKM(I) 
 6                CONTINUE 
             WRITE(6,*)' BEYOND CHANNEL GRID (km)' 
             CALL DEFDIST(XMAX,XCHKM,NUMR+1,XB,XC) 
             DO 7 i = 2, numr+1     
                  XKM(numc+i-1) = XC*EXP(I*1.)+XB 
                  WRITE(6,'(I4,G10.3)') I+numc-1,XKM(I+numc-1) 
 7                CONTINUE 
        ENDIF 
        DO 10 IDS=1,NDIST 
10           X(IDS)=1.E+3*XKM(IDS) 
        DO 20 I=1,NDIST-2 
20           DD(I)=0.0
C     return appeared to be missing/jgd/march 11, 2001
      RETURN
      END 
C 
C     RDTOP/RAPSCD 1122388 BATTELLE PNL, RICHLAND WA 
C 
      SUBROUTINE RDTOP(IT) 
       INCLUDE 'DRYD.INA' 
       INCLUDE 'ATMOS.INA' 
      DIMENSION DLST(16),CNVU(2) 
      DATA CNVU/1.0,.3048/ 
C 
C     --IDD-- --DATA TYPE-------------- 
C 
C     17      CHANNEL INPUT PARAMETERS 
C     20      BASE HEIGHT/TERRAIN UNITS 
C     21 - 29 TERRAIN HEIGHTS 6 DIST BY 16 DIRECTIONS 
C     51 - 57 ZO INPUT 6 DIST BY 16 DIRECTIONS 
C     61 - 65 ZO INPUT 4 DIST BY 8 DIRECTIONS 
C     99      END OF INPUT DATA 
c    
c    Modification Log:
c    March 15, 2001 jgd loop added
C 
C     DEFINE DEFAULTS 
           IZD=10 
           ITD=0 
           HSUM=0.0 
           HBASE=0 
           ICONV=1 
c           RES_GAS=100.


           IF (gasres.eq.0.0) then ! March 15, 2001 jgd loop added
             RES_GAS=100.
           else
             res_gas=gasres
           endif

C      DEFAULT Z0 IS 10 CM 
C      DEFAULT SURFACE RESISTANCE FOR GAS IS 100. 
           DO 3 IWS = 1,6 
           DO 3 JD=1,16 
               IF(IZ0.EQ.0) Z0(JD,IWS)=10. 
C              ADD INIT FOR TERRAIN HTS JGD JUNE 29, 1993 
               HTER(JD,IWS)=0.0 
3              SURR(JD,IWS)=RES_GAS 
C 
5     READ(IT,*,END=999) IDD, 
     1(DLST(JD),JD=9,16),(DLST(JD),JD=1,8) 
      IF(IDD.EQ.99) GOTO 999 
      IF(IDD.EQ.17) THEN 
C         CHANNEL INPUT PARAMETERS 
          NUMC=DLST(9) 
c          XCHAN=DLST(10) /jgd /correction/October 18, 1994
c          YCHAN=DLST(11) 
c         conv to m /jgd/October 19, 1994
          XCHAN=DLST(11)*1000.
          YCHAN=DLST(10)*1000.
          ZCHAN=DLST(12)*1000.
          ACHAN=DLST(13) 
          SCHAN=DLST(14) 
          WRITE(6,100)'  LOCAL WIND CHANNEL (Grid = ',DLST(9),')' 
          WRITE(6,100)'    Length = ',XCHAN/1000.,' km' 
          WRITE(6,100)'    Width  = ',YCHAN/1000.,' km' 
          WRITE(6,100)'    Height = ',ZCHAN/1000.,' km' 
          WRITE(6,100)'    Angle  = ',ACHAN,' (index)' 
          WRITE(6,100)'    Slope  = ',SCHAN,' km' 
 100      FORMAT(A,G9.3,A) 
      ELSEIF (IDD.GT.20.AND.IDD.LT.29) THEN 
C         TERRAIN HEIGHTS 
          ITD=1 
          DO 7 JD=1,16 
              HSUM=HSUM+ABS(DLST(JD)) 
              HTER(JD,IDD-20)=DLST(JD) 
7             CONTINUE 
        ELSEIF (IDD.EQ.20.) THEN 
C         EMISSION SITE HEIGHT/TERRAIN UNITS 
          HBASE=DLST(9) 
          HSUM=HSUM+ABS(HBASE) 
          ICONV=INT(DLST(10)) 
      ELSEIF(IDD.GT.50.AND.IDD.LT.57) THEN 
C         SURFACE ROUGHNESS DATA (6X16FORM) 
          DO 10 JD=1,16 
            IF(DLST(JD).LT.0) DLST(JD)=ABS(DLST(JD)) 
            IF(DLST(JD).NE.10.) IZD=0 
            Z0(JD,IDD-50)=DLST(JD) 
10          continue 
      ELSEIF(IDD.GT.60.AND.IDD.LT.67) THEN 
C         SURFACE ROUGHNESS DATA (4X8FORM) 
          DO 20 JD=1,15,2 
            IF(DLST(JD).LT.0) DLST(JD)=ABS(DLST(JD)) 
            IF(DLST(JD).NE.10.) IZD=0 
20          Z0(JD,IDD-60)=DLST(JD) 
            IZ0=3 
      ELSE 
          WRITE(6,*) '   NOTE: TOPOGRAPHY INPUT DATA CODE ',IDD, 
     +               '   NOT USED IN THIS VERSION' 
          WRITE(6,*) 
      ENDIF 
      GOTO 5 
999   IF(ICONV.LT.1.OR.ICONV.GT.2) THEN 
          WRITE(6,*) '   Terrain height input assumed to be in FT' 
          ICONV=2 
      ENDIF 
      IF(IZ0.EQ.3) THEN 
C       FILL Z0 4X8FORM TO 6X16FORM 
c       INNER DISTANCES 
        DO 50 JD=1,15,2 
          DO 50 IWS=1,5 
            IF(JD.LT.15) THEN 
               Z0(JD+1,IWS)=(Z0(JD,IWS)+Z0(JD+2,IWS))/2. 
             ELSE 
               Z0(JD+1,IWS)=(Z0(JD,IWS)+Z0(1,IWS))/2. 
             ENDIF 
50           CONTINUE 
c       INNER/OUTER DISTANCES 
        DO 55 JD=1,16 
          Z0(JD,1)=Z0(JD,2) 
          Z0(JD,6)=Z0(JD,5) 
C         June 30, 1993 JGD SET HT AT FIRST DISTANCE TO BASE HT 
          HTER(JD,1)=HBASE 
C         June 30, 1993 JGD SET HT AT LAST DISTANCE TO PREVIOUS HT 
          HTER(JD,6)=HTER(JD,5) 
55        CONTINUE 
      ENDIF 
      HBASE=HBASE*CNVU(ICONV) 
      WRITE(6,*)'   Base Ht = ',HBASE,' m' 
      IF(ITD.GT.0) THEN 
        IF(HSUM.GT.0.) THEN 
          DO 30 JD = 1,16 
          DO 30 IWS = 1,6 
               HTER(JD,IWS)=HTER(JD,IWS)*CNVU(ICONV)-HBASE 
C              June 30, 1993 JGD SET SMALL HEIGHTS TO ZERO 
30             IF(HTER(JD,IWS).LT.0.01) HTER(JD,IWS)=0.0 
          WRITE(6,*) ' LISTING OF RELATIVE TERRAIN HEIGHTS, m' 
          WRITE(6,*) ' Distance / Direction -->' 
          DO 40 IWS=1,6 
40             WRITE(6,6000) IWS,(HTER(JD,IWS),JD=1,16) 
        ELSE 
          WRITE(6,*) ' FLAT TERRAIN INPUT' 
        ENDIF 
      ENDIF 
      IF(IZD.EQ.0) THEN 
        WRITE(6,*)' REGIONAL SURFACE ROUGHNESS VALUES, cm' 
        WRITE(6,*) ' Distance / Direction -->' 
        DO 60 IWS=1,6 
60        WRITE(6,6000) IWS,(Z0(JD,IWS),JD=1,16) 
      ELSE 
        WRITE(6,*)' ALL REGIONAL SURFACE ROUGHNESS VALUES = 10 cm' 
      ENDIF 
 6000 FORMAT (2X,I2,2X,8G9.3/6X,8G9.3) 
      RETURN 
      END 
        SUBROUTINE DEFDIST(XMAX,XMIN,NUMD,XB,XC) 
        XB=(XMIN*EXP(NUMD-1.)-XMAX)/(EXP(NUMD-1.)-1) 
        XC=(XMAX-XB)/EXP(NUMD*1.) 
        RETURN 
        END 
C 
C     SETINIT/RAPSCD FEB0898 BATTELLE PNNL, RICHLAND WA 
C 
        SUBROUTINE SETINIT 
c       Added file input configeration option for several variables.
c       File input is mainly for testing purposes and is not meant for implementation
c       in the code UI at this time. Created JGD February 8, 1998
c
c       Revision log
c       March 15, 2001 JGD Particle class 6 update
c
       INCLUDE 'ATMOS.INA' 
       INCLUDE 'DPSTN.INA' 
       INCLUDE 'DRYD.INA' 
C 
      dimension rvalues(8)
      character*60 headr
c
C     Set Defaults for run where set file does not exist
c       Internal Computation Distance Limits (km)
          xmax=75.0
          xmin=0.100
          xmid=0.0
C
c       RDS Values:
c
C       Positive Values = Default Particle Radii For Deposition Computation (micron)
c       Negative or Zero Values = Indexes for Gas Deposition (0=default moderate,
c       -1 = no deposition, -2 = no surface resistance)
c
          RDS(1)=7.5
          RDS(2)=3.0 
          RDS(3)=0.3

C       Default settings for gases 
c
          RDS(4)=0.0 
          RDS(5)=-1. 
C         Set ambient particle flux RDS(6)
          RDS(6)=0.3 
          RDS(7)=-2. 
C
c     Read change in defaults fron SET
      OPEN(2,FILE='AIRSET.SET',STATUS='OLD',IOSTAT=IER)
      IF(IER.EQ.0) THEN
c
c       Read Header From File
        read(2,'(a)',err=50,end=50) headr
        write(6,*) 
        write(6,*) 'AIRSET.SET read: ',headr
c
c       Read Values Loop
45      read(2,'(I4,8F8.2)',err=50,end=50) ISET,rvalues
c
c       ISET   parameter indicates:
c
c        1     particle radius (micron) 
c              1 = particle dep. type 1
c              2 = particle dep. type 2
c              3 = particle dep. type 3
c              4 = gas option dep. type 4
c              5 = gas option dep. type 5 
c              6 = ambient particle dep. type 6 !JGD March 15, 2001
c              7 = gas option dep. type 7 
c
c        2     Internal Distance Limits 
c              Xmin = minimium distance (km) / old default is 0.100 km
c                   (=0.0 indicates that new default distances are to be used)
c              Xmid = middle distance (km) / default 0.000 for "not defined"
c              Xmax = maximium distance (km) / default is 75.0 km
c 
        If(iset.eq.1) THEN
c         particles
          If (rvalues(1).gt.0.0) RDS(1)=RVALUES(1)
          If (rvalues(2).gt.0.0) RDS(2)=RVALUES(2)
          If (rvalues(3).gt.0.0) RDS(3)=RVALUES(3)
          If (rvalues(6).gt.0.0) RDS(6)=RVALUES(6)
c         gases (not implemented)
          write(6,*) 'Radii for class 1: ', RDS(1), ' u'
          write(6,*) '                2: ', RDS(2), ' u'
          write(6,*) '                3: ', RDS(3), ' u'
          write(6,*) '                6: ', RDS(6), ' u'
        elseif(iset.eq.2) THEN
          If (rvalues(1).gt.0.0) then
            XMIN = RVALUES(1)
          else
            xmin = 0.0
          endif
          If (rvalues(2).gt.0.0) then
            XMID=RVALUES(2)
          else
            xmid=0.0
          endif
          If (rvalues(3).gt.0.0) then
            XMAX=RVALUES(3)
          else
            xmax=75.
          endif
          write(6,*) 'Internal Computation Distances Read'
          write(6,*) '     Min.: ', xmin, ' km'
          write(6,*) '     Max.: ', xmax, ' km'
          write(6,*) '     Mid.: ', xmid, ' km'
          if(xmin.eq.0.0) then
            write(6,*) 'Used Default Internal Computation Distances'
          endif
        endif
        goto 45
      endif
50    continue
      close(2)
      RETURN 
      END 
C 
C 
C     DISSET/RAPSCD FEB0898 BATTELLE PNNL, RICHLAND WA 
C 
        SUBROUTINE DISSET(xkm)
c       Added subroutine to define new default distances.
c       Distances selected to be consistent with previous default
c       values while adding two closer distances.
c       Created JGD February 8, 1998
c      
      DIMENSION XKM(12)
      XKM(1)=.001
      XKM(2)=.010
      XKM(3)=.100
      XKM(4)=1.238756E-01
      XKM(5)=1.670501E-01
      XKM(6)=2.844107E-01
      XKM(7)=6.034297E-01
      XKM(8)=1.470613
      XKM(9)=3.827863
      XKM(10)=10.235530
      XKM(11)=27.653380
      XKM(12)=75.00000
      RETURN
      END
C
C     New Subroutine Add for Framework, August 7, 1996 CJF
c     Added Glyph Name to subroutine, December 11, 1996 cjf
c     Corrected routine for use in Windows 95 and for
      SUBROUTINE RDAFF( SOUFNM, souleng, Glypnm )
C----------------------------------------------------------------------
C     RdAFF
C    
C     Christian J Fosmire
C     Date: June 9, 1997
C
C     Description:   This routine reads in the variables required by
C                    RAPSCD from the Air Flux File
C    
C----------------------------------------------------------------------
       INCLUDE 'ATMOS.INA'

      INTEGER     io, i, numln, ier, numsets, end
      REAL        area, bstk, bst, tempa, tempe
      CHARACTER*33   glypnm, runname   
      INTEGER     souleng
      CHARACTER*30   filaff
      CHARACTER*8    soufnm
      CHARACTER*1    dum
      CHARACTER*10   type, chktype
c     Add new variable /cjf Septemeber 10, 1997
      LOGICAL        COMPSTR  

c     Open Air Flux File

      io = 96
      filAFF = soufnm(1:souleng) // '.AFF'
      OPEN( io, FILE = FilAFF, STATUS = 'OLD' )

c     Find Correct Source Glyph name

      CALL FINDGLYP(io,glypnm,ier)

      IF( ier .NE. 0 ) THEN
         WRITE(*,*) 'Could not find Glyph Name ', glypnm
         WRITE(*,*) 'In AFF file ', FilAFF
         STOP 1
      ENDIF

c     Read Past Header
      READ( io, *) numln 
      DO i = 1, numln
         READ( io, '(a1)' ) dum
      ENDDO

c     Read in number of air flux rate sets and name (assume only 1)
      READ( io, * ) numsets
      READ( io, '(a)' ) runname  

c     Read Type of Source

      READ( io, '(a)' )type

      CALL READSTR(type,1,chktype,end)

c     Changed logic to use new function
      IF( COMPSTR(chktype,'AREA') ) THEN
         nsr = 1
      ELSEIF( COMPSTR(chktype,'POINT') ) THEN
         nsr = 0
      ELSE
         WRITE(*,*) 'ERROR in read AFF file, undefined source type'
         WRITE(*,*) 'source name is ', type
         STOP 1  
      ENDIF

C        DEFINE DEFAULT DEPOSITION PARAMETERS;
C        V = Deposition Velocity, and ALCO = Scavenging Efficiency

      V=.01
      ALCO=0.9

c     Set Emission Rates to 1

      DO i = 1, 7
         Qnn(i) = 1.0
      ENDDO

c     Read in the exit area of the source

      READ( io, * )area 

c     Define either area source variables or radius of source (point)

      IF( nsr .EQ. 1 ) THEN
         AreaG = area
         AreaS = area
         Rarea = 1.0 
         WRITE(6,42) 'SOURCE EMISSION AREAS'
         WRITE(6,42) '   Gaseous Area     = ',AREAG,'  m2 '
         WRITE(6,42) '   Particulate Area = ',AREAS,'  m2 '
         WRITE(6,42) '   Area Ratio       = ',RAREA
   
      ELSE
         Rstk = SQRT( area / 3.14159 )

c     Read in the rest of the point source variables

         READ( io, * )hstk
         READ( io, * )bstk
         READ( io, * )vstk
         READ( io, * )tempe
         READ( io, * )tempa
         tstk = tempe + 273.15
         tmean = tempa + 273.15
         IF( tmean .GT. tstk ) tmean = tstk

c     Determine in Building Wake Downwash

         bst = bstk / .75

         IF( hstk .LT. bst ) THEN
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

42       FORMAT (1X,A,G9.3,A)

      ENDIF

      CLOSE( io )

      RETURN
      END 

c     Subroutine added /cjf June 9, 1997

      SUBROUTINE FINDGLYP(io, glypnm, ier)
C-----------------------------------------------------------------------
C     FINDGLYP
C
C     Christian Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Date:    6/10/97
C
C     Description:   Finds the correct module in the AFF file.
C                    Glypnm is the name of the module to find.
C                    IO is the unit number of the AFF File and
C                    ier is the error number (0 if no error)
C
C-----------------------------------------------------------------------
      IMPLICIT NONE

      CHARACTER*100  teststr
      CHARACTER*33   glypnm, testglyp  
      CHARACTER*1    dum
      INTEGER        io, ier, i, nextpos, glyplines

      LOGICAL        RightGlyp, COMPSTR

C     Initialize Loop Logical Variable

      RightGlyp = .FALSE.
     
      DO WHILE( .NOT. RightGlyp )

C     Read in data string
         READ( io,'(a)', IOSTAT = ier )teststr
   
         IF ( ier .NE. 0 ) THEN
            RETURN
         ENDIF 

C        Get glyph name and lines from data string

         CALL READSTR(teststr,1,testglyp,nextpos)
         CALL READINT4(teststr,nextpos,glyplines,nextpos)  

C        Determine if glyph name is correct one (skip ahead if not)
   
         IF ( COMPSTR(glypnm,testglyp) ) THEN
            RightGlyp = .TRUE.
            CYCLE
         ELSE
            DO i = 1, glyplines
              READ( io, '(a1)') dum
            ENDDO

         ENDIF
      ENDDO
    
      RETURN

      END 
                         
c     Subroutine added /cjf June 9, 1997                   
      SUBROUTINE READINT4( inpstring, start_pos, int4val, end_pos)
C-----------------------------------------------------------------------
C
C     READINT4
C
C     Christian J Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Date:    6/9/97
C
C     Description:   Gets an integer(*4) value (int4val) from a
C                    character string(inpstring) starting at start_pos.
C                    Integer is assumed to be delimited by a comma.
C                    End_pos is the position of the comma.
C
C-----------------------------------------------------------------------
      IMPLICIT NONE

      CHARACTER*(*) inpstring
      INTEGER*4     int4val, start_pos, end_pos, ier, commpos

      IF( start_pos .GT. LEN(inpstring) ) THEN
         WRITE(*,*) 'Start position ', start_pos, 'past the length of '
         WRITE(*,*) 'input string ', inpstring
         STOP 1
      ENDIF

C     Find the position of the comma

      commpos = SCAN(inpstring(start_pos:),',')

      IF( commpos .EQ. 0 ) THEN
         READ( inpstring(start_pos:),'(i35)',IOSTAT = ier ) int4val
         IF( ier .NE. 0 ) THEN
            WRITE(*,*) 'Error reading integer from ',
     &                  inpstring(start_pos:)
            WRITE(*,*) 'Error number = ', ier
            STOP 1
         ENDIF    
         end_pos = len(inpstring)
      ELSE
         READ( inpstring(start_pos:start_pos + commpos),'(i35)',
     &         IOSTAT = ier ) int4val
         IF( ier .NE. 0 ) THEN
            WRITE(*,*) 'Error reading integer from ',
     &                  inpstring(start_pos:start_pos + commpos)
            WRITE(*,*) 'Error number = ', ier
            STOP 1
         ENDIF
         end_pos = start_pos + commpos
      ENDIF

      RETURN

      END 

c     Subroutine added /cjf June 9, 1997
      SUBROUTINE READREAL( inpstring, start_pos, realval, end_pos )
C-----------------------------------------------------------------------
C     READREAL
C
C     Christian Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Date:    6/9/97
C
C     Description:   Reads a real number (realval) from a string
C                    (inpstring) starting at start_pos.  Number is
C                    assumed to be followed by a comma.  End_pos is
C                    the position after the comma.
C
C-----------------------------------------------------------------------
      IMPLICIT NONE

      CHARACTER*(*)  inpstring
      INTEGER*4      commpos, start_pos, end_pos, ier

      REAL*4         realval

C     Find the comma in inpstring

      IF( start_pos .GT. LEN(inpstring) ) THEN
         WRITE(*,*) 'Start position ', start_pos, 'past the length of '
         WRITE(*,*) 'input string ', inpstring
         STOP 1
      ENDIF
      
      commpos = SCAN(inpstring(start_pos:),',')
      IF( commpos .EQ. 0 ) THEN
         READ(inpstring(start_pos:),'(f30.0)', IOSTAT = ier ) realval
         IF( ier .NE. 0 ) THEN
            WRITE(*,*)
     &            'Error reading real value from ',inpstring(start_pos:)
            WRITE(*,*)
     &            'Error Number - ', ier
            STOP 1
         ENDIF
         end_pos = LEN(inpstring)
      ELSE
         READ(inpstring(start_pos:start_pos+commpos),'(f30.0)',
     &                  IOSTAT = ier) realval
         IF( ier .NE. 0 ) THEN
            WRITE(*,*) 'Error reading real value from ',
     &                  inpstring(start_pos:start_pos + commpos)
            WRITE(*,*)
     &            'Error Number - ', ier
            STOP 1
         ENDIF
                                        
         end_pos = start_pos + commpos
      ENDIF

      RETURN

      END   

c     Subroutine added /cjf June 9, 1997
      SUBROUTINE READSTR( inpstring, start_pos, outstring, end_pos )
C-----------------------------------------------------------------------
C     READSTR
C
C     Christian J Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Date:    4/10/97
C
C     Description:   Grabs a character string (outstring) from a larger
C                    string (inpstring) starting at start_pos. 
C                    The string is assumed to be in double quotes with
C                    a comma following. End_pos is the
C                    postion after the last character in the string
C                    (the double quote or the comma)
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
     
      CHARACTER*(*) inpstring, outstring
     
      INTEGER*4      firstpos, firstpos1, lastpos, commpos, start_pos,
     &               end_pos

      IF( start_pos .GT. LEN(inpstring) ) THEN
         WRITE(*,*) 'Start position ', start_pos, 'past the length of '
         WRITE(*,*) 'input string ', inpstring
         STOP 1
      ENDIF
     
C     Find the position of the first quote or comma
      firstpos = SCAN(inpstring(start_pos:),'"')
      firstpos1 = SCAN(inpstring(start_pos:),',')

      IF( firstpos1 .EQ. 0 .AND. firstpos .EQ. 0 ) THEN
         outstring = ' '
         end_pos = 0
         RETURN
      ENDIF
     
      IF( firstpos1 .GT. 0 .AND. firstpos .EQ. 0 ) THEN
         IF( firstpos1 .GT. 1 ) THEN
            outstring = inpstring(start_pos:start_pos + firstpos1-2)
         ELSE
            outstring = ' '
         ENDIF
         end_pos = start_pos + firstpos1
         RETURN
      ELSEIF( firstpos1 .GT. 0 .AND. firstpos1 .LT. firstpos ) THEN
         IF( firstpos1 .GT. 1 ) THEN
            outstring = inpstring(start_pos:start_pos + firstpos1-2)
         ELSE
            outstring = ' '
         ENDIF
         end_pos = start_pos + firstpos1
         RETURN
      ENDIF
 
      IF( (firstpos .EQ. 0) .OR.
     &    ((firstpos + start_pos+1) .GT. LEN(inpstring)) ) THEN
         outstring = ' '
         end_pos = 0
         RETURN
      ENDIF

C     Find the next quote
      lastpos = SCAN(inpstring(start_pos + firstpos + 1:),'"')
  
      IF( lastpos .EQ. 0 ) THEN 
         outstring = ' '
         end_pos = 0
         RETURN
      ENDIF
  
      IF (start_pos+firstpos .LE. start_pos+firstpos+lastpos-1 ) THEN
         outstring = inpstring(start_pos+firstpos:
     &                      start_pos + firstpos + lastpos-1)
      ELSE
         outstring = ' '
      ENDIF
  
C     Find the comma if any
      commpos = SCAN(inpstring(start_pos+firstpos+lastpos:),',')
      end_pos = start_pos+firstpos + lastpos
      IF( commpos .GT. 0 ) THEN
         end_pos = end_pos + commpos
      ENDIF
  
      RETURN
  
      END  
  
c     Added new subroutine /cjf September 10, 1997 
      LOGICAL FUNCTION COMPSTR ( oldstr, newstr)
C-----------------------------------------------------------------------
C     COMPSTR
C
C     Christian Fosmire
C     Pacific Northwest National Lab
C
C     Date: 9/10/97
C
C     Description:   Determines if two strings are equal without
C                    regard to case (upper or lower)
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
  
      CHARACTER*(*) oldstr, newstr
      INTEGER*4   lenstr, lenstr1, charindex, i, charindex1
      CHARACTER*1 chkchar, chkchar1
  
  
      lenstr = LEN_TRIM(oldstr)
      lenstr1 = LEN_TRIM(newstr)
  
      COMPSTR = .TRUE.
  
c     Check each character in string             
      DO i = 1, MIN0(lenstr,lenstr1)
                        
         READ(oldstr(i:i),'(a1)') chkchar
         READ(newstr(i:i),'(a1)') chkchar1
         charindex = ICHAR(chkchar)
         charindex1 = ICHAR(chkchar1)   

         IF ( charindex .EQ. charindex1 ) THEN
            CYCLE
         ELSE
c     Determine if lower case letter

            IF( charindex .GE. 97 .AND. charindex .LE. 122 ) THEN

c     Convert to upper case letter

               chkchar = CHAR(charindex - 32 )
            ENDIF

            IF( charindex1 .GE. 97 .AND. charindex .LE. 122 ) THEN
               chkchar1 = CHAR(charindex1 - 32 )
            ENDIF

            IF( chkchar .NE. chkchar1 ) THEN
               COMPSTR = .FALSE.
               EXIT
            ENDIF
         ENDIF
      ENDDO    

      RETURN
      END

