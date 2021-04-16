
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Percentile.For
c     
c     Christian J Fosmire
c     Pacific Northwest Laboratory
c     P.O. Box 999
c     Richland, WA 99352
C
C     New coding for cumulative distributions
c     BA Napier
c
c     Created: 1/21/98
c     Updated: 8/24/99
c              2/15/00  add species
c              5/20/00  change nio unit
c     New coding 1/21/2008
c              3/9/2012 as noted in main body to allow missing met data
c
c     Description: Acute Plume Model (Main Program) 
c
c     SUBROUTINE:  GETFILES, RDMODVAR, RDRSVARA, NUCDATAIN, SCTORCAC,
c                  METEOIN, DEPPRP, CALCPONT, CALCAREA, ACPLMOUT
c     Function:    NONE
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      IMPLICIT NONE

      INCLUDE 'parm.inc'
      INCLUDE 'files.inc'
      INCLUDE 'metdata.inc'
      INCLUDE 'srcrec.inc' 
      INCLUDE 'metinput.inc'
      RECORD  /metinput/ MetInp
      
      REAL     amhgt
      
      INTEGER  NARGS, numargs, itestcount
      INTEGER  ier, io, isrc, status, nio, numchain, i, nhr 
      INTEGER  metyr, metmo, metda, methr     

      CHARACTER*50  PrgStat                        
      CHARACTER*80  modelname
      
      LOGICAL  SAMEDATE
      LOGICAL  dosect, newmet, First, firstchain, acute, rightmet, 
     &         nodata, pass1   
            
c     Open Debug File

      OPEN(25,FILE='PERCENTILE.dbg',STATUS ='unknown')
     
c     Read command line to get input file name
      
      numargs = NARGS()
      
      IF ( numargs .GE. 2 ) THEN
      
         CALL GETARG( 1, listfile, status )
         CALL GETARG( 2, modelname, status )
      ELSE
         WRITE(*,*) 'Not enough arguements passed - contact PNNL'
         STOP 1
      ENDIF
      
      IF( status.gt.0 ) THEN
         io = 20
         OPEN(io, FILE = listfile, STATUS = 'old')      
      ELSE
         WRITE(25,*) 'Error in read input file ',listfile
         STOP
      ENDIF
      
c      WRITE( 25, * ) 'Input File is ', listfile

c     Get the various file names from file list

      CALL GetFiles(io, PrgStat )
      
      CLOSE( io )

c     Read in Run Specification Parameters

      CALL RdRSVarA( metyr, metmo, metda, methr, dosect )

c     Call Routine to set source to receptor Distances

      CALL SCTORCAC

C     Determine number of chains
      nio = 31
      OPEN(unit=nio, File=nuclfile, STATUS='OLD', IOSTAT=ier )
      
      IF (ier .NE. 0 ) THEN
         WRITE(25,*) 'Error reading file - ', nuclfile
         WRITE(25,*) 'Error Number - ', ier
         GOTO 9999
      ENDIF
      
      READ( nio, *) numchain

      firstchain = .TRUE.

c     Initialize Flags
      
      newmet = .TRUE.
      first = .TRUE.
      rightmet = .FALSE.
      acute = .TRUE.

C     Read in Nuclear Data

      DO i = 1, numchain
c
c      random seed reset for each radionuclide for calm distribution (same for each so correlation retained)
c      BAN 9 Mar 2012
c
         rndseed = 123.4
c
	PRINT('(a55,i2)'), 'This takes about 30 sec per nuclide. On No.',i
	pass1 = .true.
         
         WRITE (25,*) ' Chain ', i

         CALL New_RDNUCLID( nio )

         CALL NUCDATIN( PrgStat )
         
c     Open Meteorological and get surface roughness

      CALL OPMETFIL( metfile, io, z0, amhgt )

c     Do until data runs out
      nHr=0
      DO WHILE( newmet )
      nhr=nhr+1 
         CALL READMET( io, MetInp )
          
c         IF ( MetInp.JFDmet ) THEN
c            WRITE(*,*) 
c     &         'Invalid Meteorological File - created from JFD!'
c            STOP 1                
c         ENDIF

         IF( MetInp.End ) THEN
	     close (io)
           go to 9998
         ENDIF
            
c

C     If not missing then set met variables

      IF( .NOT. MetInp.MissMet )  CALL SETPLMET( MetInp, amhgt )


C         WRITE(25,*) 'AcuteSrc....Computing for chain ', i
         
      IF( MetInp.MissMet ) THEN
c           BAN added decrement of nhr to allow for missing data
            nhr = nhr - 1            
            NoData = .TRUE.
         
         ELSE
            
            NoData = .FALSE.   
            
            CALL RESETOUT
                    
c     For each source...  

            DO isrc = 1,numsrc

c               WRITE ( 25, '(1x,a)') ' AcuteSrc '
c               WRITE ( 25, '(1x,a,i3)' ) ' Source ', isrc
c               WRITE ( 25, '(1x,a,i3)' ) ' Source Type ', SrcTyp(isrc)

c     Calculate various deposition variables

               CALL DEPPRP(isrc)
      
c     Check Type of Source
         
               IF( SrcTyp(isrc).NE. 1 ) THEN    ! not point source

c     Calculate concentration, deposition, etc, for Point Source
               
                  CALL newCALCPONT( isrc, dosect )
               
               ELSE                             ! area source          

c     Calculate concentration, deposition, etc, for Area Source

                  CALL CALCAREA( isrc, dosect ) 
               
               ENDIF ! Point or Area Source

            ENDDO ! Sources
         
         ENDIF
         
c     Write out the hourly X/Q values
                                                                     
         CALL PLMPerc( numchain, firstchain, modelname, acute, 
     &                  MetInp.year, MetInp.month, MetInp.day, 
     &                  MetInp.hour, pass1, noData )
c
c       BAN 3/9/2012  added "if" here to allow first record(s) to be bad data
c
        IF( .NOT. MetInp.MissMet ) pass1 = .false.

      END DO ! Time loop
 9998 Continue
                      
        CALL ReSort(nhr)

      ENDDO ! Chains

 9999 CONTINUE         

      CLOSE( io )

      WRITE(25,'(1x,a)')'Normal end for PERCENTILE Program'
            
      END      