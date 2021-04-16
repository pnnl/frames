      SUBROUTINE PUFFOUT ( modname, first, numchain, acute, nodata, 
     &                     tothr, metyr, metmo, metda, methr )    
C-----------------------------------------------------------------------
c     PUFFOUT
c
c     Date:                March 20, 2000
C                          10 Nov 2002  BAN: Update numchain from i1 to i3
c                          21 Sep 2004  BAN: update output lables (remove "average")
c                          9 June 2005  BAN: update output lable units (^)
c                          16 June 2005 BAN: revise to "all" particle output
c
c     Description:         This routine writes out the output for puffs
c
c     Required Modules:    None
c
c      BAN   20 April 2012  :  Annualize submersion dosese to a rate for chronic cases
c
C-----------------------------------------------------------------------

      IMPLICIT NONE
      
      INCLUDE  'parm.inc'
      INCLUDE  'const.inc'
      INCLUDE  'files.inc'
      INCLUDE  'matrix.inc'
      INCLUDE  'nuc_data.inc'

      INTEGER  DELFILESQQ
      INTEGER  tothr, metyr, metmo, metda, methr
      INTEGER  delfile, leng
      INTEGER  i, ier, j, k, numout, oio, outtyp, numchain

      REAL     xcent, ycent, yval,CitoBq, RemtoSV
      
      CHARACTER*72   frststr, fmt, fmt1
      CHARACTER*80   modname
      
      LOGICAL  exists, first, acute, nodata  

c      WRITE ( 25, '(a)' ) ' In PuffOut '          
      
      CitoBq = 3.7E10   
      RemToSV = 1E-2
c  had to move this outside of IF  BAN 27 Aug 2012
               oio = 40

c     Check if first chain

      IF ( first ) THEN

c   Delete file if already exist

         INQUIRE( FILE = outfile, EXIST = exists)
          
         IF ( exists ) THEN
            delfile = DELFILESQQ( outfile )
         ENDIF

c  Open the output file
      
         oio = 40
         OPEN ( oio, FILE=outfile, FORM='formatted', STATUS='unknown',
     &          IOSTAT = ier) 

         IF ( ier .NE. 0 ) THEN
            WRITE(*,*)'ERROR Opening Output file ', outfile
            WRITE(*,*)'Error Number - ', ier
            STOP 1
         ENDIF

c     Write Out Header Information
         IF( acute ) THEN
            frststr = 
     &         '"GENII Acute Puff Model Version 2.10.1"'
         ELSE
            frststr = 
     &         '"GENII Chronic Puff Model Version 2.10.1 "'
         ENDIF
         
         CALL WRITHEAD( oio, frststr, outfile, obsfile )      

c     Set the number of data sets to one

         WRITE( oio, '(i1,a1)') 1,','

c     Write out number of flux sets and name of data set
         leng = LEN_TRIM(modname)
c         WRITE( oio, '(i1,a,a,a)') 2,',"',modname(1:leng),'"'
c  Part of the revision below 16 June 2005 BAN
         WRITE( oio, '(i1,a,a,a)') 1,',"',modname(1:leng),'"'
      
c     Write out the flux types...(set parameters to zero)

c         WRITE( oio, '(a,1p,2(e10.3,a))')
c     &   '"Gas 1",', 0.0, ',"um",', 0.0, ',"g/cm^3",'
c         WRITE( oio, '(a,1p,2(e10.3,a))')
c     &   '"Particle 1",', meandia, ',"um",', meanden, ',"g/cm^3",'
c Revised 16 June 2005 BAN
         WRITE( oio, '(a,1p,2(e10.3,a))')
     &   '"All",', meandia, ',"um",', meanden, ',"g/cm^3",'     
c     Write Data type, Co-ordinate type, Receptor Type, and Num Parents
         
         IF( acute ) THEN
            WRITE( oio, '(a,i3,a,i4,a1,3(i2,a1))')
     &   '"acute","cartesian","grid",',numchain, ',', metyr, ',',
     &   metmo, ',', metda, ',', methr, ','
         ELSE
            WRITE( oio, '(a,i3,a)')
     &   '"chronic","cartesian","grid",',numchain,','
         ENDIF
         
         first = .FALSE.
         
      ENDIF
      
c     Set Formats for Output

      WRITE( fmt, 101 ) numx
101   FORMAT( '(1p,',i2,'(e10.3,a1))')

      WRITE( fmt1, 104 ) numx
104   FORMAT( '(1p, e10.3,a1,',i2,'(e10.3,a1))') 

c     Determine center of grid

      xcent = FLOAT(numx + 1)/2.0
      ycent = FLOAT(numy + 1)/2.0

c     Write Output

      DO k = 1, nnucs
      
c     If Parent, write out number of progenies and have external dose
         IF( .NOT. acute ) THEN
            IF( tothr .LE. 0 ) THEN
               nodata = .TRUE.
            ENDIF
         ENDIF
         
         IF ( k .EQ. 1 ) THEN
            WRITE( oio, '(2(a1,a,a2),2(i1,a1))')
     &      '"',NucName(k),'",','"',NucName(k),'",',1,',',nnucs-1,','

            IF( nodata ) THEN
               numout = 0
            ELSE
               numout = 4
            ENDIF            
         ELSE
            WRITE( oio, '(2(a1,a,a2),i1,a1)')
     &      '"',NucName(k),'",','"',NucName(k),'",',1,','
            
            IF( nodata ) THEN
               numout = 0
            ELSE
               numout = 3
            ENDIF
            
         ENDIF
         
c     Write out Time information and number of outputs

         WRITE( oio, '(i1,a,i1,a1)')
     &   1,',"yr",',numout,','
     
         DO outtyp = 1, numout
         
            SELECT CASE(outtyp)
               
               CASE(1) !Air Concentration
               
c                  IF ( deptype(k) .EQ. 0 ) THEN
                     IF( acute ) THEN
                        WRITE( oio, '(a,i2,a,i2,a)')
     &                '"Air Exposure","all","","Bq-s/m^3",',
     &                numx,',"m",',numy,',"m",'
                     ELSE
                        WRITE( oio, '(a,i2,a,i2,a)')
c     &                  '"Average Air Concentration"," ","","Bq/m3",',
     &                  '"Air Concentration","all","","Bq/m^3",',
     &                  numx,',"m",',numy,',"m",'
                     ENDIF
                     
c                  ELSE
c                     IF( acute ) THEN
c                        WRITE( oio, '(a,i2,a,i2,a)')
c     &                '"Air Exposure"," ","","Bq-s/m3",',
c     &                numx,',"m",',numy,',"m",'
c                     ELSE
c                        WRITE( oio, '(a,i2,a,i2,a)')
c     &                  '"Average Air Concentration"," ","","Bq/m3",',
c     &                  numx,',"m",',numy,',"m",'
c                     ENDIF
c                  ENDIF
                  
               CASE(2) !Dry Deposition
               
c                  IF ( deptype(k) .EQ. 0 ) THEN
                    
                     IF( acute ) THEN
                        WRITE( oio, '(a,i2,a,i2,a)')
     &                  '"Total Deposition","all","dry","Bq/m^2",',
     &                  numx,',"m",',numy,',"m",'
                     ELSE
                        WRITE( oio, '(a,i2,a,i2,a)')
c     &                  '"Average Deposition Rate"," ","dry",
     &                  '"Deposition Rate","all","dry",
     &"(Bq/m^2)/s",',numx,',"m",',numy,',"m",'
                     ENDIF
c                  ELSE
                     
c                     IF( acute ) THEN
c                        WRITE( oio, '(a,i2,a,i2,a)')
c     &               '"Total Deposition"," ","dry","Bq/m2",',
c     &               numx,',"m",',numy,',"m",'
c                     ELSE                     
c                        WRITE( oio, '(a,i2,a,i2,a)')
c     &               '"Average Deposition Rate"," ","dry","(Bq/m2)/s",',
c     &               numx,',"m",',numy,',"m",'
c                     ENDIF
c                  ENDIF
           
               CASE(3) !Wet Deposition

c                  IF ( deptype(k) .EQ. 0 ) THEN
                     
                     IF( acute ) THEN
                        WRITE( oio, '(a,i2,a,i2,a)')
     &                  '"Total Deposition","all","wet","Bq/m^2",',
     &                  numx,',"m",',numy,',"m",'
                     ELSE                     
                        WRITE( oio, '(a,i2,a,i2,a)')
c     &               '"Average Deposition Rate"," ","wet","(Bq/m2)/s",',
     &               '"Deposition Rate","all","wet","(Bq/m^2)/s",',
     &                  numx,',"m",',numy,',"m",'
                     ENDIF
c                  ELSE
                     
c                     IF( acute ) THEN
c                        WRITE( oio, '(a,i2,a,i2,a)')
c     &                '"Total Deposition"," ","wet","Bq/m2",',
c     &                  numx,',"m",',numy,',"m",'
c                     ELSE
c                        WRITE( oio, '(a,i2,a,i2,a)')
c     &               '"Average Deposition Rate"," ","wet","(Bq/m2)/s",',
c     &               numx,',"m",',numy,',"m",'
c                     ENDIF
c                  ENDIF
               
               CASE(4) !External Dose
               
                  WRITE( oio, '(4a,i2,a,i2,a)')
     &            '"External Dose",','"all",','" ",','"Sv",',
     &            numx,',"m",',numy,',"m",'
     
            END SELECT
            
            WRITE( oio, fmt ) 
     &      ((FLOAT(i) - xcent)*delxy,',', i = 1, numx)
            
c     Do for each value of y

            DO j = numy, 1, -1
            
               yval = (FLOAT(j) - ycent) * delxy
               
               SELECT CASE( outtyp )
               
                  CASE(1) !Air Concentration
                  
                     IF( acute ) THEN
                        WRITE( oio, fmt1 ) 
     &   yval,',',(cg_eoq(k,i,j)*CitoBq,',', i = 1, numx)
                     ELSE
                        WRITE( oio, fmt1 ) 
     &   yval,',',(cg_eoq(k,i,j)*CitoBq/(tothr*3600.0),',', i = 1, numx)
                     ENDIF              
                     
                  CASE(2) !Dry Deposition
                     
                     IF( acute ) THEN
                        WRITE( oio, fmt1 ) 
     &   yval,',',(cg_dry(k,i,j)*CitoBq,',', i = 1, numx)
                     ELSE
                        WRITE( oio, fmt1 ) 
     &   yval,',',(cg_dry(k,i,j)*CitoBq/(tothr*3600.0),',', i = 1, numx)
                     ENDIF                       
                  CASE(3) !Wet Deposition
                     
                     IF( acute ) THEN
                        WRITE( oio, fmt1 ) 
     &   yval,',',(cg_wet(k,i,j)*CitoBq,',', i = 1, numx)
                     ELSE
                        WRITE( oio, fmt1 ) 
     &   yval,',',(cg_wet(k,i,j)*CitoBq/(tothr*3600.0),',', i = 1, numx)
                     ENDIF
     
                  CASE(4) !External Dose; acute/chronic split BAN 20 April 2012
                     if(acute) then
                     WRITE( oio, fmt1 ) 
     &               yval,',',(cg_shine(i,j),',', i = 1, numx)
	               else
                      WRITE( oio, fmt1 ) 
     &             yval,',',(cg_shine(i,j)*8760./tothr,',', i = 1, numx)
                     endif

               END SELECT
               
            ENDDO !Receptors
         ENDDO !Output Type
      ENDDO ! Nuclides
      
      RETURN 
      
      END                        