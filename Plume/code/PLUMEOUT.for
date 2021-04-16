      SUBROUTINE PLUMEOUT ( numchain, first, model, acute, metyr, metmo, 
     &                      metda, methr, NoData ) 
C-----------------------------------------------------------------------
c     PLUMEOUT
c
c     Date:                3/16/1998
c        Updated           3/18/2000
C                          10 Nov 2002  BAN:  increase i1 to i3 for numchain
c                          9 Jun 2005   BAN:  slight revision in output units
C                          16 Jun 2005  BAN:  Revised ATO header format
c                          20 April 2012 BAN: Annualized submersion doses for chronic cases
c                          24 Aug 2012  BAN:  moved oio definition out of IF(first)
c
c     Description:         This routine writes out the output for plume
c                          models (both acute and chronic)  
c
c     Required Modules:    None
c
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      INCLUDE  'parm.inc'
      INCLUDE  'files.inc'
      INCLUDE  'srcrec.inc'
      INCLUDE  'output.inc'
      INCLUDE  'nuc_data.inc'

      INTEGER  DELFILESQQ
      INTEGER  delfile, numchain, leng      
      INTEGER  i, irec, k, numout, oio, outtyp, step, stop, strt
      INTEGER  metyr, metmo, metda, methr
      
      REAL*4   ang, angdif, CitoBq
      
      CHARACTER*72   fmt, fmt1, firststr
      CHARACTER*(*)   model
      
      LOGICAL  acute, exists, first, NoData      
      
      DATA CitoBq    /3.7e10/
      
C     If first time through
C  BAN had to move oio definition outside of (first) IF for Intel compiler  24 Aug 2012
      oio = 30
      IF( first ) THEN

c     Delete file is exist

         INQUIRE( FILE = outfile, EXIST = exists)
         
         IF ( exists ) THEN
            delfile = DELFILESQQ( outfile )
         ENDIF

c     Open output file

         oio = 30
      
         OPEN( oio, FILE = outfile, STATUS = 'unknown', 
     &         FORM = 'FORMATTED')                           
      
c     First write out the header information
      
         IF( acute ) THEN                                      
            firststr = 
     &   '"GENII Acute Plume Model Version 2.10.1 "'                                      
         ELSE
            firststr = 
     &   '"GENII Chronic Plume Model Version 2.10.1 "'
         ENDIF
                                                 
         CALL WRITHEAD( oio, firststr, outfile, metfile )

c     Set the number of data sets to one
c  Part of the June 16 2005 change below
c         WRITE( oio, '(i1,a1)') 2,','
         WRITE( oio, '(i1,a1)') 1,','

c     Write out number of fluxes                
         leng = LEN_TRIM(model)
         WRITE( oio, '(i1,a2,a,a1)')1,',"',model(1:leng),'"'         

c     Write out the flux types...(set to zero)

c         WRITE( oio, '(a,1p,2(e10.3,a))')
c     &   '"Gas 1",', 0.0, ',"um",', 0.0, ',"g/cm^3",'
c         WRITE( oio, '(a,1p,2(e10.3,a))')
c     &   '"Particle 1",',meandia,',"um",',meanden, ',"g/cm^3",'
C  revised 16 June 2005 BAN to match Mitch's requirements
         WRITE( oio, '(a,1p,2(e10.3,a))')
     &   '"All",',meandia,',"um",',meanden, ',"g/cm^3",'
     
c     Write out Date set type, Co-oridinate type, Receptor Type and Num Parents


         IF( acute ) THEN
            WRITE( oio, '(a,i3,a,i4,a1,3(i2,a1))')
     &   '"acute","polar","grid",',numchain,',', metyr, ',', metmo,
     &   ',', metda, ',', methr, ','
         ELSE
            WRITE( oio, '(a,i3,a)')
     &   '"chronic","polar","grid",',numchain,','
         ENDIF
                                                     
         first = .FALSE.
                                                        
      ENDIF

c     Set Formats for output
      
      WRITE( fmt, 101 ) numradii
101   FORMAT( '(1p,',i2,'(e10.3,a1))')

      WRITE( fmt1, 104) numradii
104   FORMAT( '(f5.1,a1,1p,',i2,'(e10.3,a1))')      

     
c     Write out Output
      
c      WRITE(25,*) 'tot_wght = ', tot_wght

c     Check to make sure data exist for chronic model

      IF( .NOT. acute ) THEN
         IF ( tot_wght .LE. 0 ) THEN
            NoData = .TRUE.
         ENDIF
      ENDIF
      
      DO k = 1, nnucs
         
c     If Parent, write out the number of progenies and external dose

         IF( k .EQ. 1 ) THEN
            WRITE( oio, '(2(a1,a,a2),2(i1,a1))')
     &      '"',NucFulName(k),'",','"',NucName(k),'",',1,',',nnucs-1,','          

            IF ( NoData ) THEN
               numout = 0
            ELSE
               numout = 4
            ENDIF
                     
         ELSE
            WRITE( oio, '(2(a1,a,a2),i1,a1,2(a1,a,a2))')
     &      '"',NucFulName(k),'",','"',NucName(k),'",',1,',',
     &      '"',NucFulName(1),'",','"',NucName(1),'",'
            
            IF ( NoData ) THEN
               numout = 0
            ELSE
               numout = 3
            ENDIF
            
         ENDIF
         
c     Write out time information and number of outputs

         WRITE( oio, '(i1,a,i1,a1)')
     &      1,',"yr",',numout,','

         DO outtyp = 1, numout

            SELECT CASE(outtyp)
         
               CASE(1) ! Air Concentration
               
c                  IF ( deptype(k) .EQ. 0 .OR. deptype(k) .EQ. 1 ) THEN

                     IF ( acute ) THEN
                        WRITE( oio, '(a,i2,a,i2,a)')
     &                     '"Air Exposure","all","","(Bq-s)/m^3",',
     &                     numradii,',"m",',recnum,',"deg",'
                     ELSE
                        WRITE( oio, '(a,i2,a,i2,a)')
     &                     '"Air Concentration","all","","Bq/m^3",',
     &                     numradii,',"m",',recnum,',"deg",'  
                     ENDIF
             
c                  ELSE
                     
c                     IF ( acute ) THEN
c                        WRITE( oio, '(2a,i2,a,i2,a)')
c     &                   '"Air Exposure"," ","",',
c     &                  '"(Bq-s)/m3",',numradii,',"m",',recnum,',"deg",'
c                     ELSE
c                        WRITE( oio, '(2a,i2,a,i2,a)')
c     &                   '"Air Concentration"," ","",',
c     &                   '"Bq/m3",',numradii,',"m",',recnum,',"deg",'
c                     ENDIF
                     
c                  ENDIF
               
               CASE(2) ! Dry Deposition
                 
c                  IF (deptype(k) .EQ. 0 .OR. deptype(k) .EQ. 1 ) THEN
                    
                     IF ( acute ) THEN
                        WRITE( oio, '(a,i2,a,i2,a)')
     &                     '"Total Deposition","all","dry","Bq/m^2",',
     &                     numradii,',"m",',recnum,',"deg",'   
                     ELSE
                        WRITE( oio, '(a,i2,a,i2,a)')
     &                  '"Deposition Rate","all","dry","(Bq/m^2)/s",',
     &                   numradii,',"m",',recnum,',"deg",'                              
                     ENDIF
                           
c                  ELSE
                     
c                     IF ( acute ) THEN
c                        WRITE( oio, '(a,i2,a,i2,a)')
c     &                 '"Total Deposition"," ","dry","Bq/m2",',
c     &                 numradii,',"m",',recnum,',"deg",'                   
c                     ELSE
c                        WRITE( oio, '(a,i2,a,i2,a)')
c     &             '"Deposition Rate"," ","dry","(Bq/m2)/s",',
c     &              numradii,',"m",',recnum,',"deg",'
c                     ENDIF

c                  ENDIF   
                  
               CASE(3) ! Wet Depostion

c                  IF ( deptype(k) .EQ. 0 .OR. deptype(k) .EQ. 1 ) THEN

                     IF( acute ) THEN
                        WRITE( oio, '(a,i2,a,i2,a)')
     &                     '"Total Deposition","all","wet","Bq/m^2",',
     &                     numradii,',"m",',recnum,',"deg",'      
                     ELSE
                        WRITE( oio, '(a,i2,a,i2,a)')
     &                  '"Deposition Rate","all","wet","(Bq/m^2)/s",',
     &                   numradii,',"m",',recnum,',"deg",'
                     ENDIF

c                  ELSE    
                  
c                     IF ( acute ) THEN
c                        WRITE( oio, '(a,i2,a,i2,a)')
c     &                '"Total Deposition"," ","wet","Bq/m2",',
c     &                numradii,',"m",',recnum,',"deg",'       
c                     ELSE
c                        WRITE( oio, '(a,i2,a,i2,a)')
c     &             '"Deposition Rate"," ","wet","(Bq/m2)/s",',
c     &              numradii,',"m",',recnum,',"deg",'
c                     ENDIF
                     
c                  ENDIF
     
               CASE(4) ! External Dose

                  WRITE( oio, '(a,i2,a,i2,a)')
     &               '"External Dose","all"," ","Sv",',
     &               numradii,',"m",',recnum,',"deg",'
     
            END SELECT                      
      
            WRITE( oio, fmt) (RecRadii(i),',', i = 1, numradii )

            angdif = 360.0/FLOAT(recnum)

c     Do for each sector
      
            DO irec = 1, recnum

               strt = irec
               stop = irec + (numradii-1) * recnum
               step = recnum
      
               ang = angdif * FLOAT(irec)
      
               SELECT CASE( outtyp )

                  CASE(1)  !Air Concentration (Time Integeral)
                     
                     IF( acute ) THEN
                     
                        WRITE( oio, fmt1 )
     & ang,',',(eoq_ch(i,k)*CitoBq*3600,',', i = strt, stop, step )
                     
                     ELSE
                        
                        WRITE( oio, fmt1 ) 
     & ang,',',(eoq_ch(i,k)*CitoBq/tot_wght,',', i = strt, stop, step )
                     
                     ENDIF
                     
                  CASE(2)  !Dry Deposition (Total)
                     
                     IF( acute ) THEN
                        
                        WRITE( oio, fmt1 )
     & ang,',',(dry_ch(i,k)*CitoBq*3600,',', i = strt, stop, step )
                     
                     ELSE
                     
                        WRITE( oio, fmt1 )
     & ang,',',(dry_ch(i,k)*CitoBq/tot_wght,',', i = strt, stop, step )
         
                     ENDIF
                     
                  CASE(3)  !Wet Deposition (Total)
                  
                     
                     IF( acute ) THEN
                        
                        WRITE( oio, fmt1 )
     & ang,',',(wet_ch(i,k)*CitoBq*3600,',', i = strt, stop, step )
                     
                     ELSE
                        
                        WRITE( oio, fmt1 )
     & ang,',',(wet_ch(i,k)*CitoBq/tot_wght,',', i = strt, stop, step )
         
                     ENDIF
                     
                  CASE(4)  !External Dose
	                
				  if (acute) then                                          
				   WRITE( oio, fmt1 )
     &                ang,',',(shine_ch(i),',', i = strt, stop, step )
	                else
				   WRITE( oio, fmt1 )
c     &               ang,',',(shine_ch(i),',', i = strt, stop, step )
     &  ang,',',(shine_ch(i)*8760/tot_wght,',', i = strt, stop, step )
	                endif
     
              END SELECT
              
            ENDDO  !Recetors
         ENDDO ! Output Type
      ENDDO  ! Nuclides               
                  
                 
      RETURN

      END     