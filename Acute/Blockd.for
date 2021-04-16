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
C     Version of 14-Sep-99   BAN
C     Reviewed and Approved: 12-Sept-88 BA Napier
C
C-----------------------------------------------------------------------

      INCLUDE 'PARMTR.PAR'
      INCLUDE 'AIRPAR.CMN'
      INCLUDE 'ANMPAR.CMN'
c      INCLUDE 'DFPAR.CMN'
      INCLUDE 'EXTPAR.CMN'
      INCLUDE 'FILES.CMN'
      INCLUDE 'FODPAR.CMN'
      INCLUDE 'LABELS.CMN'
c      INCLUDE 'OPTG.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'SOLPAR.CMN'
      INCLUDE 'SWPAR.CMN'
 
      DATA NAQ /4/,   NTF /4/,   NAN /4/,   NRE /3/, NPATH /28/, 
     .     NMAX /100/

C     Distances and direction--
c      DATA NDIST /10/,  NDIR /16/, MINDIS /100.0/
c      DATA IDIR/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16/
c
C     Standard distances--
c      DATA X/  805.,  2414.,  4023.,  5632.,  7241.,
c     .       12068., 24135., 40255., 56315., 72405./
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
c     .     DPVRES /0.001/, DEPFR2 /0.25/, 
     .     LEAFRS /1.0E-9/, WTIM /14.0/, 
c     .     SURCM /15.0/, SLDN /224.0/, SSLDN /1500.0/, 
     .     UNITCF /1.E9/, M3CM3 /1.0E-6/, KGG /1.0E-3/, 
     .     ALOG2 /0.6931/, DUMMY /9*0.0/, KGMG /1.0E-6/, 
     .     MEVSV /1.385E-5/, RAIN/1./
c
C     Exposure labels--
      DATA EXPLAB /'Plume   ','Inhale  ','Sur Soil','Leaf Veg',
     .             'Oth. Veg','Fruit   ','Cereals ','Meat    ',
     .             'Poultry ','Cow Milk','Eggs    ','Soil Ing',
     .             'Swim Ing','Swim Ext','Boating ','Shore   ',
     .             'Water   ','Fish    ','Mollusc ','Crustace',
     .             'Aqu Plnt','Indoor  ','Shwr Drm','Shwr Ing',
     .             'Soil Drm','Swim Drm','Shor Drm','Soil Inh'/
C    .             'Aqu Plnt','Dep Soil','Waste   ','        '/
c
      DATA SEALAB /'Winter','Spring','Summer','Autumn'/
c
C     Dose factor type for each exposure pathway--
c      DATA IDF /3,1,3,10*2,3*3,5*2,3,3,0/
c
C     Inventory units--
c
      DATA NVU   /3.7E-2, 3.7E+4, 3.7E+7, 3.7E+10, 1.0/
      DATA UNIT1 /'pCi',   'uCi',  'mCi',  ' Ci', ' Bq'/
      DATA UNIT2 /'m3','yr','m2',10*'yr',3*'L',5*'yr','m3','m3',' '/
      DATA UNIT3 /'m2','m3','kg'/
      DATA SVU /1.0, 0.15, 224.0/
c
C     Inhalation rates--
      DATA RINH /270.0/, RINHA /330.0/
c
C     Absolute humidity (kg/m3)
      DATA ABSHUM /0.008/
c
C     Animal consumption, harvest removal, standing biomass--
      DATA CONSUM / 68.0, 0.12, 55.0, 0.12, 68.0, 55.0/
      DATA DWATER /50.0, 0.3, 60., 0.3/
      DATA FRACUT /0.0, 0.8, 1.0, 0.8/
      DATA HARVST /.TRUE./
      DATA BIOMAS /2.0, 2.0, 3.0, 0.8/
      DATA BIOMA2 /0.8, 0.8, 1.0, 0.8, 1.0, 1.5/
      DATA DRYFAC /0.1, 0.25, 0.18, 0.18/
      DATA DRYFA2 /0.18, 0.18, 0.18, 0.18, 0.2, 0.2/  
	DATA SLCONA /0.0, 0.0, 0.0, 0.0/
c
C     Translocation factors--
      DATA TRANS /1.0, 3*0.1/, TRANSA /4*0.1, 2*1.0/
c     
C     Biotic transport parameters--
      DATA YELDBT /0.4, 5.0, 4.0/
C      DATA TOTEXC /9.41E-4, 2*7.48E-4/
C      DATA EXCAV / 1.0, 0.81, 0.19, 0.02, 0.008, 0.002,
C     .             1.0, 0.90, 0.096, 0.006, 0.0005, 0.0005,
C     .             1.0, 0.90, 0.096, 0.006, 0.0005, 0.0005/
c
C     Shorewidth factors for different shoreline types--
      DATA SHORWI /0.2, 0.3, 0.5, 1.0/,
     .     TCWS /25295.0/ 
c
C     Ingestion of swimming water--
c      DATA INGWAT /0.02/
c
C     Surface dose to EDE conversion for photons in energy groups:
C     (MeV)    0.15   0.4   0.75   1.25   1.75   2.25
c      DATA FC /0.67,  0.66, 0.67,  0.71,  0.75,  0.78/
c  
      DATA FILN /'FILENAME.DAT', 49*' '/
      DATA DASHES /'--------','-------  ',8*' -------'/
c
C----------------------------------------------------------------------
      END   
