      SUBROUTINE PLMPerc ( numchain, first, model, acute, metyr, metmo, 
     &                      metda, methr, pass1, noData ) 
C-----------------------------------------------------------------------
c     PLUMEOUT
c
c     Date:                3/16/1998
c        Updated           3/18/2000
C                          10 Nov 2002  BAN:  increase i1 to i3 for numchain
c                          9 Jun 2005   BAN:  slight revision in output units
C                          16 Jun 2005  BAN:  Revised ATO header format
C        Revised           1/21/2008    BAN: To dump long lists in simple format
c
c     Description:         This routine writes out the output for plume
c                          models  
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
	INCLUDE  'metdata.inc'

      INTEGER  DELFILESQQ
      INTEGER  delfile, numchain, leng      
      INTEGER  i, irec, k, numout, oio, outtyp, step, stop, strt
      INTEGER  metyr, metmo, metda, methr
      
      REAL*4   ang, angdif, CitoBq
      
      CHARACTER*72   fmt, fmt1, firststr
      CHARACTER*(*)   model
      
      LOGICAL  acute, exists, first, NoData, PASS1, PASS2      
      
      DATA CitoBq    /3.7e10/
      
C     If first time through
      IF( first ) THEN

c     Delete intermediate files if exist

         INQUIRE( FILE = "store.eq", EXIST = exists) 
         IF ( exists ) THEN
            delfile = DELFILESQQ( "store.eq" )
         ENDIF

c         IF ( exists ) THEN
c           delfile = DELFILESQQ( "store.dd" )
c         ENDIF

c         INQUIRE( FILE = "store.wd", EXIST = exists) 
c         IF ( exists ) THEN
c            delfile = DELFILESQQ( "store.wd" )
c         ENDIF

c         INQUIRE( FILE = "store.ed", EXIST = exists) 
c         IF ( exists ) THEN
c            delfile = DELFILESQQ( "store.ed" )
         ENDIF

c     Open intermediate output files

         oio = 51  
         OPEN( oio, FILE = "store.eq", STATUS = 'unknown', 
     &         FORM = 'FORMATTED')                           
c         oio = 52  
c         OPEN( oio, FILE = "store.dd", STATUS = 'unknown', 
c     &         FORM = 'FORMATTED')                           
c            
c         oio = 53  
c         OPEN( oio, FILE = "store.wd", STATUS = 'unknown', 
c     &         FORM = 'FORMATTED')                           
c         oio = 54  
c         OPEN( oio, FILE = "store.ed", STATUS = 'unknown', 
c     &         FORM = 'FORMATTED')   
c       ENDIF                        


     
c     Write out Output


      DO k = 1, nnucs
c     Set Formats for output
      
      WRITE( fmt, 101 ) numradii
101   FORMAT( '(1p,',i2,'(e10.3,a1))')

      WRITE( fmt1, 104) 2*k, 4*numradii
104   FORMAT( '(',i2,'x,','f5.1,a1,1p,',i2,'(e10.3,a1))')      
         
c     If Parent, write out the number of progenies and external dose

         IF( k .EQ. 1 ) THEN
c            WRITE( 51, '(2(a1,a,a2),2(i1,a1))')
c    &      '"',NucFulName(k),'",','"',NucName(k),'",',1,',',nnucs-1,','          

            IF ( NoData ) THEN
               numout = 0
            ELSE
               numout = 4
            ENDIF
                     
         ELSE
c            WRITE( 51, '(2(a1,a,a2),i1,a1,2(a1,a,a2))')
c     &      '"',NucFulName(k),'",','"',NucName(k),'",',1,',',
c     &      '"',NucFulName(1),'",','"',NucName(1),'",'
            
            IF ( NoData ) THEN
               numout = 0
            ELSE
               numout = 3
            ENDIF
            
         ENDIF
         
c     Write out time information and number of outputs

c         WRITE( oio, '(i1,a,i1,a1)')
c    &      1,',"yr",',numout,','


         DO outtyp = 1, numout
           
            IF ( PASS1 .and. (k .eq. 1)) then

            SELECT CASE(outtyp)
         
               CASE(1) ! Air Concentration
	         rewind(51)
               
                        WRITE( 51, '(a8,a1,2(i2,a1),4a20)')
     &    NucName(k),',',numradii,',',nnucs,',','"Air Exposure",'
     & ,'"Dry Deposition",','"Wet Deposition",','"External Dose"'
             
               
c               CASE(2) ! Dry Deposition
c                    
c                        WRITE( 52, '(a8,a20)')
c     &                     NucName(k),', "Dry Deposition"'   
c                            
c                  
c               CASE(3) ! Wet Depostion
c
c                        WRITE( 53, '(a8,a20)')
c     &                     NucName(k),', "Wet Deposition"'      
c
c     
c               CASE(4) ! External Dose
c
c                  WRITE( 54, '(a8,a17)')
c     &               NucName(k),', "External Dose"'
     
            END SELECT  
		  

c            WRITE( 51, fmt) (RecRadii(i),',', i = 1, numradii )

		  ENDIF ! PASS1                    
      
	            angdif = 360.0/FLOAT(recnum)

c     Do for each sector
      
            DO irec = 1, recnum
               PASS2 = .false.
               strt = irec
               stop = irec + (numradii-1) * recnum
               step = recnum
CCCCCCC - a logic bug here: progeny never grow in soon enough to be non-zero!!!!!
c               if(eoq_ch(strt,k) .gt. 0.0) PASS2 = .true.
	         if(irec .eq. wndsec) PASS2 = .true.
				      
               ang = angdif * FLOAT(irec)
      
               SELECT CASE( outtyp )
   
                  CASE(1)  !Air Concentration (Time Integeral)
 
                     
                     IF( PASS2 ) THEN
                     
                        WRITE( 51, fmt1 )
     & ang,',',(eoq_ch(i,k)*CitoBq*3600,',', i = strt, stop, step ),
     &     (dry_ch(i,k)*CitoBq*3600,',', i = strt, stop, step ),
     &     (wet_ch(i,k)*CitoBq*3600,',', i = strt, stop, step ),
     &     (shine_ch(i),',', i = strt, stop, step )
                     
 
                     ENDIF
                     
                  CASE(2)  !Dry Deposition (Total)
                     
                     IF( PASS2 ) THEN
                        
C                        WRITE( 52, fmt1 )
C    & ang,',',(dry_ch(i,k)*CitoBq*3600,',', i = strt, stop, step )
                     
           
                     ENDIF
                     
                  CASE(3)  !Wet Deposition (Total)
                  
                     
                     IF( PASS2 ) THEN
                        
C                        WRITE( 53, fmt1 )
C     & ang,',',(wet_ch(i,k)*CitoBq*3600,',', i = strt, stop, step )
                     
                     
                     ENDIF
                     
                  CASE(4)  !External Dose


	               IF (PASS2 ) THEN
                     
C                     WRITE( 54, fmt1 )
C     &         ang,',',(shine_ch(i),',', i = strt, stop, step )
	               ENDIF
     
              END SELECT
              
            ENDDO  !Recetors
         ENDDO ! Output Type
      ENDDO  ! Nuclides               
                  
C     If first time through
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
     &   '"GENII Acute 95% Plume Model Version 2.10.1 "'                                      
         ELSE
            firststr = 
     &   '"Something is wrong - check immediately!"'
         ENDIF
                                                 
         CALL WRITHEAD( oio, firststr, outfile, metfile )

c     Set the number of data sets to one

         WRITE( oio, '(i1,a1)') 1,','

c     Write out number of fluxes                
         leng = LEN_TRIM(model)
         WRITE( oio, '(i1,a2,a,a1)')1,',"',model(1:leng),'"'         

c     Write out the flux types...(set to zero)

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
                 
      RETURN

      END     