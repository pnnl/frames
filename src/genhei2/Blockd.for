C-----------------------------------------------------------------------
C
      BLOCK DATA BLOCKD
C
C     This module initializes program parameters.
C
C     Module of Programs ENVIN, ENV, DOSE, and INTDF 
C                        of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Version of 1-Jun-98  BAN
C
C-----------------------------------------------------------------------

C      INCLUDE 'AIRPAR.CMN'
C      INCLUDE 'ANMPAR.CMN'
C      INCLUDE 'EXTPAR.CMN'
C      INCLUDE 'FILES.CMN'
C      INCLUDE 'FODPAR.CMN'
      INCLUDE 'LABELS.CMN'
      INCLUDE 'OPT.CMN'
	INCLUDE 'ALLPAR.CMN'
C      INCLUDE 'SOLPAR.CMN'
C      INCLUDE 'SWPAR.CMN'
 
      DATA NAQ /4/,   NTF /4/,   NAN /4/,   NRE /3/, NPATH /28/, 
     .     NMAX /100/

C     Distances and direction--
C      DATA NDIST /10/,  NDIR /16/, MINDIS /100.0/
C      DATA IDIR/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16/

C     Standard distances--
C      DATA X/  805.,  2414.,  4023.,  5632.,  7241.,
C     .       12068., 24135., 40255., 56315., 72405./
C
C----- Constants for COMMON Block OPT ------------------------------------
C
      DATA P/3.14159265/, YRSEC/3.1688E-8/, M3L/0.001/, YRHR/1.1408E-4/
      DATA BQPCI/0.037/, PCIBQ/27.027/, M3ML/1.E-6/, L3ML/1.E-3/
      DATA DAYHR/0.0416667/,MMIN/25.4/,DAYMO/30./,TONE/1./
C
      DATA YRDA /2.74E-3/, AYEAR /365.25/, 
     .     SECYR /3.15E7/, 
     .     HRYR /8766.0/, SECDA / 8.64E4/, DAYYR /365.25/, 
     .     MOYR /12.0/, ONEYR /1.0/, 
     .     ZERO /0.0D0/, LM2IN /25.4/, 
     .     DPVRES /0.001/, DEPFR2 /0.25/, 
     .     WTIM /14.0/, 
C     .     SURCM /15.0/, SLDN /224.0/, SSLDN /1500.0/, SOLING /410.0/,
     .     SOILT /0.15/, SLDN /1500./,
     .     UNITCF /1.E9/, M3CM3 /1.0E-6/, KGG /1.0E-3/, 
     .     ALOG2 /0.6931/, DUMMY /9*0.0/, KGMG /1.0E-6/, 
     .     MEVSV /1.385E-5/, LML/0.001/

C     Initiailize inventory flags--
C      DATA RELTRM /.FALSE./,  BASIC /.FALSE./,  DERIVE /.FALSE./

C     Air dispersion conservatism factor defaults to 95% percentile--
C      DATA PRCNTI /2/
C
      DATA SEALAB /'Winter','Spring','Summer','Autumn'/

C     Dose factor type for each exposure pathway--
c      DATA IDF /3,1,3,10*2,3*3,5*2,3,3,0/

C     Inventory units--
      DATA NVU   /3.7E-2, 3.7E+4, 3.7E+7, 3.7E+10, 1.0/
      DATA UNIT1 /'pCi',   'uCi',  'mCi',  ' Ci', ' Bq'/
      DATA UNIT2 /'m3','yr','m2',10*'yr',3*'L',5*'yr','m3','m3',' '/
      DATA UNIT3 /'m2','m3','kg'/
      DATA SVU /1.0, 0.15, 224.0/

C     Inhalation rates--
C      DATA RINH /270.0/, RINHA /330.0/

C     Absolute humidity (kg/m3)
C      DATA ABSHUM /0.008/

C     Animal consumption, harvest removal, standing biomass--
C      DATA CONSUM / 68.0, 0.12, 55.0, 0.12, 68.0, 55.0/
C      DATA DWATER /50.0, 0.3, 60., 0.3/
C      DATA FRACUT /0.0, 0.8, 1.0, 0.8/
C      DATA HARVST /.TRUE./
C      DATA BIOMAS /2.0, 2.0, 3.0, 0.8/
C      DATA BIOMA2 /0.8, 0.8, 1.0, 0.8, 1.0, 1.5/
C      DATA DRYFAC /0.1, 0.25, 0.18, 0.18/
C      DATA DRYFA2 /0.18, 0.18, 0.18, 0.18, 0.2, 0.2/  

C     Translocation factors--
C      DATA TRANS /1.0, 3*0.1/, TRANSA /4*0.1, 2*1.0/
     
C     Biotic transport parameters--
C      DATA YELDBT /0.4, 5.0, 4.0/
C      DATA TOTEXC /9.41E-4, 2*7.48E-4/
C      DATA EXCAV / 1.0, 0.81, 0.19, 0.02, 0.008, 0.002,
C     .             1.0, 0.90, 0.096, 0.006, 0.0005, 0.0005,
C     .             1.0, 0.90, 0.096, 0.006, 0.0005, 0.0005/
C
C     Shorewidth factors for different shoreline types--
C      DATA SHORWI /0.2, 0.3, 0.5, 1.0/,
C     .     TCWS /25295.0/ 

C     Ingestion of swimming water--
C      DATA INGWAT /0.02/

C     Surface dose to EDE conversion for photons in energy groups:
C     (MeV)    0.15   0.4   0.75   1.25   1.75   2.25
c      DATA FC /0.67,  0.66, 0.67,  0.71,  0.75,  0.78/
  
C      DATA FILN /'FILENAME.DAT', 49*' '/
      DATA DASHES /'--------','-------  ',8*' -------'/

C----------------------------------------------------------------------
      END   

