C--------------------------------------------------------------------------
C     Files.Inc
C
C     DATE:                 April 2, 1996
C
C     Description:          Contains the common block of filenames
C     
C     Included in Modules:  DATRD, FCTRD, GETFNAME, GRIDIN, INIT, 
C                           MODPRMIN, NUCDATIN, SPECIN, TADMOD2  
C-----------------------------------------------------------------------

      CHARACTER*80 domfile, listfile, nuclfile, 
     &             nucdatfile, obsfile, outfile, rsfile   
     
      COMMON /Files/
     &            domfile, listfile, nuclfile, 
     &            nucdatfile, obsfile, outfile, rsfile  