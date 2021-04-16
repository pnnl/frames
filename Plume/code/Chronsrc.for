
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Chronsrc.For
c     
c     Christian J Fosmire
c     Pacific Northwest Laboratory
c     P.O. Box 999
c     Richland, WA 99352
c
c     Created: 2/13/98
c     Modified:  3/18/00 jvr
c
c     Description:  Chronic Plume Model (main program)
c
c     Calls:  GETFILES, RDMODVAR, RDRSVARA, NUCDATAIN, SRCTOREC, 
C             METEOIN, DEPPRP, CALCPONT, CALCAREA, CHPLMOUT
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   Modification History
c      23 Sept 2004   BAN  added a non-zero rndseed
c      17 Nov 2005    BAN  rndseed setting moved inside nuclide loop
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE

      INCLUDE  'parm.inc'
      INCLUDE  'files.inc'
      INCLUDE  'metdata.inc'
      INCLUDE  'srcrec.inc'          
      INCLUDE  'metinput.inc'
      RECORD   /metinput/ InpMet
   
      REAL       amhgt

      INTEGER  NARGS, numargs
      INTEGER  ier, io, isrc, status, nio, numchain, i

      CHARACTER*50   PrgStat
      CHARACTER*80  modelname
      
      LOGICAL  doSect, newmet, First, firstchain, MissMet, JFDMet,
     &         Acute, NoData    
c      DATA rndseed/123.4/      
c     Open Debug File

      OPEN(25,FILE='chronsrc.dbg',STATUS ='unknown')

c     Initialize the variables       

      acute = .FALSE.

c      WRITE(25,'(1x,a)')'Starting CHRONSRC Program'
      
c     Read command line to get list file name
      
      numargs = NARGS()
      
      IF (numargs.GE.2) THEN
         CALL GETARG( 1, listfile, status )
         CALL GETARG( 2, modelname, status )
      ELSE
         WRITE(*,*) 'Not enough arguements passed - Contact PNNL'
         STOP 1
      ENDIF
      
c     Check if error in reading filename from command line

      IF( status .GT. 0 ) THEN
         io = 20
         OPEN( io, FILE = listfile, STATUS = 'old')      
      ELSE
         WRITE(25,*) 'Error in read input file ',listfile
         GOTO 9999
      ENDIF
      
c      WRITE( 25, * ) 'Input File is ',listfile

c     Get the various File names for list file

      CALL GetFiles( io, PrgStat )      
      
      CLOSE( io )
      
c     Read in Run Specific Parameters

      CALL RdRSVar( PrgStat )

c  Call Routine to calculate the source to receptor distances and
c  angles
                                                                 
      CALL SRCTOREC

c  Open radionuclide input file
      
      nio = 35
      
      OPEN ( unit=nio, File=nuclfile, STATUS='OLD', IOSTAT=ier )
      
      IF (ier .NE. 0 ) THEN
         WRITE(25,*) 'Error reading file - ', nuclfile
         WRITE(25,*) 'Error Number - ', ier
         GO TO 9999
      ENDIF

c     Determine number of chains      

      READ( nio, *) numchain

      firstchain = .TRUE.

c     Read in Nuclear Data
      
      DO i = 1, numchain
c
c  random seed reset for each radionuclide for calm redistribution reproducability
c  BAN 17 Nov 2005
c  
         rndseed = 123.4 
c       
         CALL new_RDNUCLID( nio )      
         CALL NUCDATIN( PrgStat )
         
c         WRITE(25,*) 'Computing for chain ', i 
         
         CALL RESETOUT
         
c     Always doing Sector average

         doSect = .TRUE.
         newmet = .TRUE.
         first = .TRUE.   
         NoData = .FALSE.
      
c  Do while there is new meteorology to process
      
         DO WHILE( newmet )
         
            IF( first ) THEN
               CALL OPMETFIL( metfile, io, z0, amhgt)
               first = .FALSE.                 
            ENDIF
            
            CALL READMET( io, InpMet )
            
            IF( InpMet.End ) THEN
               newmet = .FALSE.
               CYCLE
            ENDIF
            
            IF( InpMet.MissMet ) CYCLE
                        
            CALL SetPlMet( InpMet, amhgt )
                        
c     For each source... 

            DO isrc = 1, numsrc

c  Calculate various deposition values

               CALL DEPPRP( isrc )

c     Check Type of Source
         
               IF( SrcTyp(isrc) .NE. 1 ) THEN

c     Calculate concentration, deposition, etc, for Point Source
               
                  CALL newCALCPONT( isrc, dosect )
         
               ELSE
            
c     Calculate concentration, deposition, etc, for Area Source

                  CALL CALCAREA( isrc, dosect ) 

               ENDIF ! Point or Area Source
            ENDDO ! Sources
         ENDDO ! Met data
      
         CLOSE( io )

c     Write output file
         
         CALL PLUMEOUT( numchain, firstchain, modelname, acute,
     &                  InpMet.year, InpMet.month, InpMet.day, 
     &                  InpMet.Hour, NoData )

      ENDDO ! chains

      WRITE (25, '(a)' ) ' End of ChronSrc '

 9999 CONTINUE
      
      CLOSE( nio )

      END      