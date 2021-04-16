C-----------------------------------------------------------------------
C
      BLOCK DATA BLOCKD
C
C     This module initializes program parameters.
C
C     Module of Programs of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Version of 14-Sep-99  BAN
C     Reviewed and Approved: 12-Sept-88 BA Napier
c-----------------------------------------------------------------------
C  6 May 98  BAN  Removed duplications with BLKDAT.CMN
C  14 Sep-99  BAN  added animal soil consumption SLCONA
C  4 May 2012 BAN  Set many defaults to zero rather than old human numbers for Biota
C-----------------------------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'AIRPAR.CMN'
      INCLUDE 'ANMPAR.CMN'
c      INCLUDE 'DFPAR.CMN'
      INCLUDE 'EXTPAR.CMN'
      INCLUDE 'FILES.CMN'
      INCLUDE 'FODPAR.CMN'
C      INCLUDE 'LABELS.CMN'
c      INCLUDE 'OPTG.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'SOLPAR.CMN'
      INCLUDE 'SWPAR.CMN'
 
      DATA NAQ /4/,   NTF /1/,   NAN /5/,   NRE /3/, NPATH /28/, 
     .     NMAX /100/

C     Distances and direction--
      DATA NDIST /10/,  NDIR /16/, MINDIS /100.0/
      DATA IDIR/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16/

C     Standard distances--
      DATA X/  805.,  2414.,  4023.,  5632.,  7241.,
     .       12068., 24135., 40255., 56315., 72405./
C
C----- Constants for COMMON Block OPT ------------------------------------
C
      DATA P/3.14159265/, YRSEC/3.1688E-8/, M3L/0.001/, YRHR/1.1408E-4/
      DATA BQPCI/0.037/, PCIBQ/27.027/, M3ML/1.E-6/, L3ML/1.E-3/
      DATA DAYHR/0.0416667/,MMIN/25.4/,DAYMO/30./,TONE/1./

      DATA YRDA /2.74E-3/, AYEAR /365.25/, 
     .     SECYR /3.15E7/, 
     .     HRYR /8766.0/, SECDA / 8.64E4/, DAYYR /365.25/, 
     .     MOYR /12.0/, ONEYR /1.0/, 
     .     ZERO /0.0D0/, LM2IN /25.4/, 
     .     DPVRES /0.001/, DEPFR2 /0.25/, 
     .     LEAFRS /1.0E-9/, WTIM /14.0/, 
     .     SURCM /15.0/, SLDN /224.0/, SSLDN /1500.0/, SOLING /410.0/,
     .     UNITCF /1.E9/, M3CM3 /1.0E-6/, KGG /1.0E-3/, 
     .     ALOG2 /0.6931/, DUMMY /9*0.0/, KGMG /1.0E-6/, 
     .     MEVSV /1.385E-5/, RAIN/1./


C     Inventory units--
      DATA NVU   /3.7E-2, 3.7E+4, 3.7E+7, 3.7E+10, 1.0/
      DATA UNIT1 /'pCi',   'uCi',  'mCi',  ' Ci', ' Bq'/
      DATA UNIT2 /'m3','yr','m2',10*'yr',3*'L',5*'yr','m3','m3',' '/
      DATA UNIT3 /'m2','m3','kg'/
      DATA SVU /1.0, 0.15, 224.0/

C     Inhalation rates--
      DATA RINH /270.0/, RINHA /330.0/

C     Absolute humidity (kg/m3)
      DATA ABSHUM /0.008/

C     Animal consumption, harvest removal, standing biomass--
      DATA CONSUM / 0.0, 0.0, 0.0, 0.0, 0.0/
      DATA DWATER /0.0, 0.0, 0.0, 0.0, 0.0/
      DATA HARVST /.TRUE./
      DATA BIOMAS /0.0/
      DATA BIOMA2 /0.0, 0.0, 0.0, 0.0, 0.0/
      DATA DRYFAC /0.0/
      DATA DRYFA2 /0.0, 0.0, 0.0, 0.0, 0.0/
	DATA SLCONA /0.0, 0.0, 0.0, 0.0, 0.0/  

C     Translocation factors--
      DATA TRANS /1.0/, TRANSA /5*1.0/
c     
      DATA TCWS /25295.0/ 

C     Ingestion of swimming water--
      DATA INGWAT /0.02/

      DATA FILN /'FILENAME.DAT', 49*' '/
      DATA DASHES /'--------','-------  ',8*' -------'/

C----------------------------------------------------------------------
      END   

