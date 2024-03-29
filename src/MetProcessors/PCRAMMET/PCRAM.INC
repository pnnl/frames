      PARAMETER (MAXVAR = 26)
      PARAMETER (MAXERR = 60)

      LOGICAL EOFSFC,COMDLN, DRYFLG,WETFLG,PPTFLG,CALM(24),NOWARN
      CHARACTER*40 INPFIL,OUTFIL,SFCNAM,PPTNAM,MIXNAM
      CHARACTER*80 ERRMSG(MAXERR),ERRCOD(MAXERR)
      CHARACTER*6 OUTTYP,SFCTYP
      CHARACTER*5 VERSON
      REAL XLAT,XLON,PRESS(24),RHO(24)
      INTEGER INSFC,INOPT,INMIX,INPPT, IOUT, INEW, IDIAG
      INTEGER IY3240(24),IM3240(24),ID3240(24)
      INTEGER ISKY,ITZONE, IPREC(5,24),IPCODE(24)
      CHARACTER*22 CITY
      CHARACTER*2 STATE
      INTEGER IDVAR(21),NVARS, KST(24), LWD
      CHARACTER*5 ACD144(12,24)
      CHARACTER*9 JSAMSN(MAXVAR,24)
      REAL AMMIX(3),PMMIX(3),P3240(24),SUNRIS,SUNSET,SOLANG(24)
      CHARACTER*8 PPTFMT
      INTEGER IDMIX(3),IYRMIX(3),IMOMIX(3),IDYMIX(3),IDSFC
      INTEGER ISYR(24),ISMO(24),ISDY(24),ISHR(24)
      REAL WSPEED(24),AFV(24),FVR(24),HLH(2,24),TEMP(24)
      REAL USTAR(24),XMONIN(24),PRECIP(24)
      INTEGER IWXCOD(9,24)
      INTEGER KSPEED(24),ICEIL(24),IDIR(24),ICOVER(24)

      REAL ANEMHT,ZDMEAS,Z0MEAS,ZDAPPL,Z0APPL
      REAL ALBEDO,BOWEN,QF,CSUBG,ELMINM,ELMINA

      REAL   DAYNO,TDAYNO,SIND,COSD,SINTD,COSTD,SIGMA,
     &       FCOVER(24),RSOL(24),RNET(24)
Cjop &       H2,HI,ALFSN,FCOVER(24),RSOL(24),RNET(24)
      REAL   AMM,TEMPZ,DUM,SINLAT,COSLAT,DSIN,DCOS,CONST
      REAL SHF(24)


      COMMON /SUNDAT/ AMM,TEMPZ,DUM,SINLAT,COSLAT,DSIN,DCOS,CONST,
     &                DAYNO,TDAYNO,SIND,COSD,SINTD,COSTD,SIGMA,
     &                ANG,FCOVER,RSOL,RNET
Cjop &                H2,HI,ALFSN,ANG,FCOVER,RSOL,RNET

      COMMON /CHARS/ INPFIL,OUTFIL,SFCNAM,PPTNAM,MIXNAM,
     &               OUTTYP,SFCTYP,CITY,STATE,JSAMSN,
     &               PPTFMT,ACD144,IWXCOD,VERSON,ERRMSG,ERRCOD
     
     
      COMMON /LOGICS/EOFSFC,COMDLN, DRYFLG,WETFLG,PPTFLG,CALM,NOWARN
      COMMON /NUMS1/ INSFC,INOPT,INMIX,INPPT,IOUT,INEW,IDIAG
      COMMON /NUMS2/ XLAT,XLON,ITZONE,
Cjop &        IDVAR,VARS,I3280,
     &        IDVAR,VARS,
     &        AMMIX,PMMIX,P3240,
     &        IDMIX,IYRMIX,IMOMIX,IDYMIX,IDSFC,
     &        ISYR,ISMO,ISDY,ISHR,
Cjop &        IPYR,IPMO,IPDY,
     &        WSPEED,KSPEED,AFV,FVR,HLH,TEMP,
     &        USTAR,XMONIN,PRECIP,IPREC,IPCODE,
     &        ICEIL,IDIR,ICOVER,ISKY,
     &        IY3240,IM3240,ID3240,NVARS,
     &        SHF,PRESS,RHO,KST,LWD,
     &        SUNRIS,SUNSET,SOLANG,
     &        ANEMHT,ZDMEAS,Z0MEAS,ZDAPPL,Z0APPL,
     &        ALBEDO,BOWEN,QF,CSUBG,ELMINM,ELMINA
Cjop &        ANEMHT,ZD,ALBEDO,Z0,ALPHA,QF,CSUBG,ELMIN
