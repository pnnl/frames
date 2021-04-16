CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     JFDPROC.INC
C
C     Contains:  The common block variables for JFDPROC
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      
      REAL     prcpprob(4,5), freq(7,8,18), maxtmp(12), 
     &         mintmp(12), wdir(18), wspd(8), nprcp_prob(5)
      REAL     amhgt, lat, z0appl, z0meas
      
      INTEGER  jdio, ndir, nstab, nwspd, numhr, oio, jfdtype

      LOGICAL  doprcp
            
      COMMON /JFDPROC/
     &         prcpprob, freq, maxtmp,
     &         mintmp, wdir, wspd, nprcp_prob,
     &         amhgt, lat, z0appl, z0meas, 
     &         jdio, ndir, nstab, nwspd, numhr, oio, jfdtype,
     &         doprcp 