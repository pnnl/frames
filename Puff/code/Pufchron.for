
      PROGRAM PUFCHRON

C***************************************************************************
C     File:            PUFCHRON
C               
c     Description:   Main code for Chronic Puff Model for Update to GENII
C
C---------------------------------------------------------------------------

      IMPLICIT      NONE

      INCLUDE       'parm.inc' 
      INCLUDE       'const.inc'
      INCLUDE       'files.inc'
      INCLUDE       'nuc_data.inc'
      INCLUDE       'puffs.inc'
      INCLUDE       'rel.inc'  
      INCLUDE       'metinput.inc'
      RECORD        /metinput/ newmet
      
      REAL          amhgt
      
      INTEGER       numargs, NARGS
      INTEGER       iadv, ier, io, isrc, m, metio, n, nhr, 
     &              oio, statin, nio, i, numchain, persist      
      
      INTEGER       metyr, metmo, metda, methr
      
      CHARACTER*1   dum
      CHARACTER*50  PrgStat
      CHARACTER*80  modelname

      LOGICAL       OFF_GRID
      
      LOGICAL       eofmet, eofrun, firstchain, acute, nodata
       
      PrgStat = ' ' 
      acute = .FALSE.
                           
C **  OPEN A LOG FILE

      OPEN (25,FILE='CronPuff.dbg',STATUS='UNKNOWN')

cd     OPEN (49,FILE='tadgrid.dbg',STATUS = 'UNKNOWN') 
ct     OPEN (93,FILE='times.dbg',STATUS = 'UNKNOWN')

c  Get file that contains the various input and output filenames
      
      numargs = NARGS()
      
      IF (numargs .GE. 2 ) THEN
      
         CALL GETARG ( 1, listfile, statin )
         CALL GETARG ( 2, modelname, statin )
         IF ( statin .GT. 0 ) THEN
            io = 10
            OPEN ( io, FILE = listfile, STATUS = 'old', IOSTAT = ier )

c           WRITE ( 25,* )'The file containing the filenames is ',
c     &                   listfile     

            IF ( ier.NE.0 ) THEN
               WRITE( *,* ) 'Error Opening ListFile ', listfile
               WRITE( *,* ) 'Error Number - ', ier
               STOP 1
            ENDIF                                                            
         ELSE
            WRITE( *,* ) 'Missing ListFile Name'
            STOP 1
         ENDIF
      ELSE
      
         WRITE(*,*) 'Not enough arguements passed - Contact PNNL'
         STOP 1
      ENDIF
      
c  Get the various filenames

      CALL GetFName ( io, acute, PrgStat )
      IF( PrgStat .NE. ' ' ) GOTO 9999          
      
c  Read in the user defined model parameters

      CALL RDRSPUFF( acute, metyr, metmo, metda, methr, PrgStat ) 
      IF( PrgStat .NE. ' ' ) GOTO 9999
      
c  Read in the domain parameters

      CALL DOMVARIN( metio, acute, amhgt, PrgStat )
      IF( PrgStat .NE. ' ' ) GOTO 9999

C     Determine the number of chains

      OPEN(unit = nio, File = nuclfile, STATUS = 'OLD', IOSTAT = ier )
      
      IF (ier .NE. 0 ) THEN
         WRITE(*,*) 'Error reading file - ', nuclfile
         WRITE(*,*) 'Error Number - ', ier
         GOTO 9999
      ENDIF
      
      READ( nio, *) numchain

      firstchain = .TRUE.

c     For each chain

      DO i = 1, numchain           
      
         WRITE(*,*) 'Working on Chain ', i
      
         CALL RDNUCLPF( nio, acute, PrgStat )
      
c     Read in the radionuclide and dose calculation data

         CALL NUCDATIN( acute, PrgStat )
         IF( PrgStat .NE. ' ' ) GOTO 9999

c  Open file for TESTM data -- additional info about puffs

cd        OPEN ( 7, FILE='TSTFILE.dbg', STATUS='UNKNOWN')

c  Initilize for chain   

         eofrun = .FALSE.
         eofmet = .FALSE.
         nhr = 0                                          
         persist = 0

         CALL OUTP_INT
         
cd        WRITE ( 25,'(/,A)' ) ' BEGIN SIMULATION'

c  beginning of primary model time loop, process by hour        

         DO WHILE( .NOT.eofrun )

c  Get the next hour met data
      
            IF( .NOT. eofmet ) THEN
               CALL READMET( metio, newmet )

c              Determine if reach end of file

               IF( newmet.end ) THEN
                  eofmet = .TRUE.
                  persist = persist + 1
               ELSE                          
               
                  nhr = nhr + 1             
c                 Check if met data came from JFD

                  IF( newmet.jfdmet ) THEN
                     WRITE(*,*) 'Invalid Meteorological File - ',obsfile
                     WRITE(*,*) 'Meteorological Data created from a JFD'
                     STOP 1
                  ENDIF

c                 If met data missing, assume presistence - add to counter
            
                  IF( newmet.missmet ) THEN
                     persist = persist + 1
                  ELSE
                     persist = 0
                     CALL SETPFMET( newmet, amhgt, acute, PrgStat )
                     IF( PrgStat .NE. ' ' ) GOTO 9999
                  ENDIF
               ENDIF
            ELSE
               persist = persist + 1
            ENDIF
            
            IF( MOD(nhr,100) .EQ. 0 ) THEN
               WRITE(*,*) '   Calculated ',nhr,' hours.'
            ENDIF

c           If persist over 12 hours then turn off all puffs
c           skip all movement around grid
            
            IF( persist .GT. 12 ) THEN                      

c              If no more met than run is finished            
               IF( eofmet ) THEN
                  eofrun = .TRUE.
                  CYCLE
               ELSE
               
                  DO m = 1, tpuffs 
                     mf(i) = 0
                  ENDDO
               ENDIF
            ELSE          

C  Loop over the time step for an hour, nph

               DO iadv = 1, nph
                  
                  IF ( tpuffs .LT. mxpuff ) THEN

c        IF end of file for met, don't release any more puffs

                     IF( .NOT. eofmet ) THEN

c        release puff
c        loop through sources         

                        DO isrc = 1, nsrc 

                           CALL PUFFR ( isrc, acute, PrgStat )
                           IF ( PrgStat .NE. ' ' ) GOTO 9999
                                                
                        ENDDO  ! sources
                     ENDIF
                     
                  ELSE              
            
c  too many puffs         
            
                     WRITE ( PrgStat(1:2),'(a2)' ) 'TM'
                     WRITE ( PrgStat(4:13),'(a10)') 'TADMOD2'
                     WRITE ( PrgStat(15:18), '(i4)' ) 9999
                     WRITE ( PrgStat(31:50),'(a20)') 'Too many puffs' 
                     GOTO 9999               
                  ENDIF

c  Cartesian grid calculations
                
                  DO m = 1, tpuffs

c  skip any puffs that are turned off

                     IF ( mf(m) .EQ. 0 ) CYCLE

                     IF ( OFF_GRID ( xp(m), yp(m), xsmin, xsmax, 
     &                            ysmin, ysmax ) )  THEN          

c  puff is beyond tracking region...turn it off

                        mf(m) = 0

                     ELSE

                        CALL PUFFM ( m, acute, PrgStat )
                        IF ( PrgStat .NE. ' ' ) GOTO 9999 
                                   
                        CALL DIFDEP ( m, acute, PrgStat )
                        IF ( PrgStat .NE. ' ' ) GOTO 9999               

                     ENDIF

                  ENDDO              ! all puffs

               ENDDO                 ! puffs per model time step                         

c  output puff positions, status, and masses 

cd              CALL TESTM( nhr )  

c              Combine puff if necessary

               IF ( Cln_Flg ) THEN
               
                  CALL COMBINE 
                                                   
               ENDIF
            
            ENDIF

c           Clean up puffs on grid
               
            CALL CLEAN2

c           if no more puffs and no more met than run is done
            
            IF( tpuffs .EQ. 0 .AND. eofmet ) THEN
               eofrun = .TRUE.
            ENDIF
      
         ENDDO           ! End of primary loop                                        

c        WRITE OUTPUT
         
         CALL PUFFOUT( modelname, firstchain, numchain, acute, nodata,
     &                 nhr, metyr, metmo, metda, methr )

c        Reset Met File to first met record
      
         REWIND(metio)                     
         READ( metio, '(a1)' ) dum !skip z0
      
      ENDDO

      WRITE ( 25, '(a)' ) ' Normal End for ChronPuf '
         
 9999 CONTINUE

      IF ( PrgStat.NE.' ' ) WRITE(PRGSTAT(1:2),'(a2)')'PF'                       

      IF ( PrgStat.NE.' ' ) THEN
         CALL PRGSTATT ( PrgStat )
         WRITE(*,*) 'Error Occured during Run - See PrgStat.ERR'
         STOP 1
      ENDIF

      CLOSE ( 25 )

      END

C---------------------------------------------------------------------------
c
c     BLOCK DATA
c 
c     Description:       BLOCK DATA initializes variables in COMMON BLOCKS 
c                        used in TADMOD2.
c                                                        
c     Date:              August 11, 1995
c
c     Required Modules:  None
c
C---------------------------------------------------------------------------

      BLOCK DATA
      
      IMPLICIT      NONE
      
      INCLUDE       'parm.inc'
      INCLUDE       'const.inc'
      INCLUDE       'puffs.inc'

C **   INITIALIZE ARRAY

      DATA  mf      / MXPUFF * 0 /
      
C **   GENERAL CONSTANTS

      DATA  pi, twopi, dtr / 3.14159, 6.283185, 0.01745329 / ! dtr = deg to rads

      END
