      SUBROUTINE RdMODVAR( PrgStat )
C-----------------------------------------------------------------------
C
C     RdModVar
C
C     Christian J Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Date:       7/29/96
C
C     Description:   This routine reads in the model variables which
C                    are usually not changed by the user                 
C
C     Required Modules:    NONE
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      INCLUDE  'parm.inc'
      INCLUDE  'files.inc'
      INCLUDE  'metdata.inc'
      INCLUDE  'shine.inc'  
      
      INTEGER        i1, is, im, ih
      
      CHARACTER*50   PrgStat
         

c     Open file containing model variables

      OPEN( 33, File = modfile, STATUS = 'OLD' )
      
c     Read in minimum wind speed for plume rise

      READ( 33, * ) wndmin    

d     WRITE(25,*)'Minimum Wind Speed for Plume Rise is ', wndmin

c  Sigma y for shift to semi-inifinite cloud shine model

      READ ( 33,* ) chgmod
 

d     WRITE(25,*)'The shift for semi-infinite cloud is ', chgmod

      
      CLOSE( 33)

c     Set Random seed

      CALL GETTIM( ih, im, is, i1 )
                  
      rndseed = 1.0D+4 * DBLE(i1) + 1.0D+3 * DBLE(i1*2) + 
     &             1.0D+2 * DBLE(is) + DBLE(im)

      
      RETURN
      
      END      