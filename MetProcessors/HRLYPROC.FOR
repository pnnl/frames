CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     HRLYPROC.FOR
C
C     Christian J Fosmire
C     Pacific Northwest National Lab
C     P.O. BOX 999
C     Richland, WA 99352
C
C     Created:  1/15/95
c
c
c     Description:  This program takes in CD-144 Formatted data and 
c       processes the data so it can be used by the EDUP dispersion
c       programs.

C----------------------------------------------------------------------
C     Modification History
C       1 Oct 2004   BAN  Initial rebuild for SAMSON to CEAM data
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT    NONE
      INCLUDE     'hrly.inc'
      
      REAL        INVMOL
      REAL        MIX_HT
      REAL        PROFILE
      REAL        USTAR
      REAL        fhgt, molappl, molen, molmeas, ustrm, ws100
      
      INTEGER     ip5(5), itemp(9)
      INTEGER     i, iday, ihr, irec, jday, nproc, nread, rio, statin
      
      LOGICAL     eofsfc
      
      CHARACTER*80 rsfile
      
      
c     Open diagnostic file
      
      idiag = 80
      OPEN( UNIT = idiag, FILE = 'DIAGN.DBG', STATUS = 'UNKNOWN')
      

      OPEN( UNIT = 70, FILE = 'HRLYPROC.DBG', STATUS = 'UNKNOWN')

C     Set io numbers

      iout = 45
      iosfc = 25
      iotd = 35

C     Get Name of Run File

      CALL GETARG(1,rsfile,statin)
      IF( statin.gt.0 ) THEN

         rio = 10
         OPEN(UNIT = rio, FILE = rsfile, STATUS = 'old' )
         WRITE(70,*)'Run File is ',rsfile
     
      ELSE
         
         WRITE(*,*) 'Error reading run file'
         STOP 1
      ENDIF

c     Read in the Parameter File

      CALL READPARM( rio )
      
c     Process Input and Output files

      CALL FILES

c     Write the application z0 and anemonometer height to output file
      
      irec = 1
      WRITE( iout,'(f7.3,a1,f5.1)', REC = irec ) z0appl,',',amhgt
      
c     Loop over each day
      
      iday = 0      
      eofsfc = .FALSE.
      
      DO WHILE( .NOT.eofsfc )

         iday = iday + 1


c     Initialize the meteorology variables

         CALL VARBINT                

c     Read in Surface Data

         CALL READSFC( nread, eofsfc )
         
         nproc = nread
         
         IF ( nread.eq.24 ) THEN
            
c     Read in Hourly Precipitation for this day

            CALL READPP
            
c     Fill up the Appropriate Meteorological Arrays

            CALL FILMET
            
c     Get the Juilan Day

            CALL JULIAN( isyr(1), ismo(1), isdy(1), jday, 0 )  
            
c     Get the time of sunrise and sunset

            CALL SUNTIME( jday, lat, long, sunris, sunset )

            IF( sunris.GE.0 .AND. sunset.GE.0 ) THEN
               sunris = sunris - timzon
               sunset = sunset - timzon
            ENDIF
            
c     Begin Loop over all hours in the day

            DO ihr = 1, 24
                  
c     Determine if Hour is 'CALM'
c              ****?????      
c               CALL CHKCLM( ihr )
               
c     Calculate Flow Vector (???)

               CALL FLOVEC( ihr )
               
c     Convert the precp. code

               IF ( sfctyp.EQ.'CD144' .OR. sfctyp.EQ.'SCRAM' ) THEN
                     
                  DO i = 1, 5
                     ip5(i) = iprec(i,ihr)
                  ENDDO
                     
                  CALL PCODES( ip5, prpcode(ihr), precip(ihr) )
     
               ELSEIF( sfctyp.eq.'SAMSON' ) THEN

c     *****Change Later******               
                  DO i = 1, 9
                     itemp(i) = iwxcod(i,ihr)
                  ENDDO
c               
                  CALL WXMAP( itemp, ip5 )
                  CALL PCODES( ip5, prpcode(ihr), precip(ihr) )
               ENDIF

c     Convert all data to appropriate units
               
               CALL UNITS( ihr )

c     Calculate the stability class for this hour
               
               CALL STABTURN( ihr, jday )

c     Calculate ustar and 1/L for this hour

c     If speed or stability are missing, then ustar and
c       mixing height are also missing

               IF ( wspeed(ihr).gt.90 .OR. kst(ihr).GT.7 ) THEN
                  
                  ustr(ihr) = -9 
                  wndspd(ihr) = -9.
                  mixhgt(ihr) = -9999
                  recok(ihr) = .FALSE.                  
                  
               ELSE
                  
                     molmeas = INVMOL( kst(ihr), z0meas )
                     molappl = INVMOL( kst(ihr), z0appl )
                     ustrm = USTAR( amhgt, wspeed(ihr), z0meas, molmeas,
     &                           kst(ihr) )

                  IF( z0meas .NE. z0appl ) THEN

               
c     Estimate the winds at free height assumed to be 100 meters
c     or if stable (kst(ihr)>4), minimum of 3*MO length or 100 meters

                     IF ( kst(ihr).GT.4 ) THEN
                        molen = 1.0/molmeas
                        fhgt = 3.0*molen
                        fhgt = AMIN1(fhgt,100.)
                     ELSE
                        fhgt = 100.
                     ENDIF               
               
               
                     ws100 = PROFILE( z0meas, ustrm, molmeas, kst(ihr),
     &                                fhgt )
                     ustr( ihr ) = USTAR( fhgt, ws100, z0appl, molappl,
     &                                    kst(ihr) )
     
                     wndspd(ihr) = PROFILE( z0appl, ustr(ihr), molappl,
     &                                      kst(ihr), amhgt )
                  
                  ELSE
                     
                     ustr(ihr) = ustrm
                     wndspd(ihr) = wspeed(ihr)
                  
                  ENDIF
                  
c     Calculate mixing height for this hour

                  mixhgt( ihr ) = MIX_HT( ustr(ihr), molappl, kst(ihr),
     &                                 lat )

               ENDIF
               
            ENDDO
            
c     Write out hourly values

            CALL WRITHRLY( irec )
            
         ENDIF
         
      ENDDO
      
      
      END
                                