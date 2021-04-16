      SUBROUTINE READPARM( rio )
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c     READPARM.FOR
c
c     Christian Fosmire
c     Pacific Northwest National Lab
c     P.O. Box 999
c     Richland, WA 99352
c
c     Created:  1/15/96
c
c     Description:  This program reads in the user specificed 
c        parameters.
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE

      INCLUDE  'hrly.inc'
      
      INTEGER  ih, im, is, i1, rio
            
c     Read in the filename of the output file

      READ( rio, '(a80)' ) outfile
      
      WRITE(70, *) 'The output file is ', outfile
      
c     Read in the hourly surface obs file

      READ( rio, '(a80)' ) sfcfile
      
      WRITE(70,*) 'The sfc file is ', sfcfile
      
c     Read in Type of surface data 

      READ( rio, '(a6)' ) sfctyp

      WRITE(70,*) 'The type of sfc data is ',sfctyp

c     Read Station Latitude and Longitude

      READ( rio, * ) lat, long
      
      WRITE(70,*) 'The station latitude and longitude are', lat, long

c     Read in the time zone differnece

      READ( rio, * ) timzon
      
      WRITE(70,*) 'Hours from GMT is ', timzon

c     Read in the surface roughness of measurement site

      READ( rio, * ) z0meas

      WRITE(70,*) 'The surface roughness at meas. site is ', z0meas 
      
c     Read in the surface roughness of application site

      READ( rio, * ) z0appl
      
      WRITE(70,*) 'The surface roughness at appl. site is ', z0appl      
      
c     Read in the anomemeter height

      READ( rio, * ) amhgt

      WRITE(70,*) 'The wind measurement height is ', amhgt
      
c     Read in Flag on whether doing precipitation from TD-3240

      READ( rio, * ) td32flg
      
      WRITE(70,*) 'The flag on whether using TD-3240 is ', td32flg

      IF ( td32flg ) THEN

c     If using TD-3240, then get file name and format of file
      
         READ( rio, '(a80)' ) td32file
         
         WRITE(70,*) 'The TD-3240 file is ', td32file

         READ( rio, '(a1)' ) td32form
         
         IF ( td32form.eq.'F' ) THEN
            WRITE(70,*) 'The TD-3240 file is in fixed format'
         ELSE
            WRITE(70,*) 'The TD-3240 file is in variable format'
         ENDIF

      ENDIF

C     Set random seed for Flow Vector

      CALL GETTIM( ih, im, is, i1 )
                  
      rndseed = 1.0D+4 * DBLE(i1) + 1.0D+3 * DBLE(i1*2) + 
     &             1.0D+2 * DBLE(is) + DBLE(im)

      
      RETURN
      
      END       
      