      SUBROUTINE GETFILES( io, PrgSTat ) 
C-----------------------------------------------------------------------
C     GETFILES
C
C     Christian J Fosmire
C     Pacific Northwest National lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Date:    7/29/96
c              2/9/00  activate debug output
C
C     Description:   This routine reads the various file names from the
C                    listing file                                       
C
C     Required Modules: None
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      INCLUDE  'files.inc'
      
      INTEGER   io
      
      CHARACTER*50   PrgStat

c     Read in name of model variables file
      
c      READ( io, '(a)') modfile 
cd     WRITE(25,*)'Model Variable File is ', modfile
      
c     Read in name of run specification file      
      
      READ( io, * ) rsfile 

c     Read in nuclide file
      
      READ( io, * ) nuclfile
      
c     Read in name of nuclear data file      
      
      READ( io, * ) nucdatfile 

c     Read in name of meteorlogy data file      
                                
      READ( io, * ) metfile 
      
c     Read in name of output file      
      
      READ( io, * ) outfile

c      WRITE(25,*)'Run Spec. File is ', rsfile      
c      WRITE(25,*) 'nuclide file is ', nuclfile
c      WRITE(25,*)'Nuc. Data File is ', nucdatfile       
c      WRITE(25,*)'Met File is ',metfile      
c      WRITE(25,*)'Output file is ', outfile 

      PrgStat = ' '        
      
      RETURN                
      
      END