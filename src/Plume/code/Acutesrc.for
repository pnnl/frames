
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     AcuteSrc.For
c     
c     Christian J Fosmire
c     Pacific Northwest Laboratory
c     P.O. Box 999
c     Richland, WA 99352
c
c     Created: 1/21/98
c     Updated: 8/24/99
c              2/15/00  add species
c              5/20/00  change nio unit
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
      
      INTEGER  NARGS, numargs
      INTEGER  ier, io, isrc, status, nio, numchain, i 
      INTEGER  metyr, metmo, metda, methr     

      CHARACTER*50  PrgStat                        
      CHARACTER*80  modelname
      
      LOGICAL  SAMEDATE
      LOGICAL  dosect, newmet, First, firstchain, acute, rightmet, 
     &         nodata    
            
c     Open Debug File

      OPEN(25,FILE='acutesrc.dbg',STATUS ='unknown')
     
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

c     Open Meteorological and get surface roughness

      CALL OPMETFIL( metfile, io, z0, amhgt )

c     Do until find right date
      
      DO WHILE( .NOT. rightmet )
         
         CALL READMET( io, MetInp )
          
         IF ( MetInp.JFDmet ) THEN
            WRITE(25,*) 
     &         'Invalid Meteorological File - created from JFD!'
            STOP 1                
         ENDIF

         IF( MetInp.End ) THEN
            WRITE(25,*) 'Date Not Found in Meteorological File'
            STOP 1
         ENDIF
            
c        CHECK DATES
         rightmet = SAMEDATE( MetInp.year, MetInp.month, MetInp.day, 
     &                        MetInp.hour, 0, 
     &                        metyr, metmo, metda, methr, 0 )
            
      ENDDO

C     If not missing then set met variables

      IF( .NOT. MetInp.MissMet )  CALL SETPLMET( MetInp, amhgt )


      WRITE(25,*)'Run Spec. File is ', rsfile      
      WRITE(25,*) 'nuclide file is ', nuclfile
      WRITE(25,*)'Nuc. Data File is ', nucdatfile       
      WRITE(25,*)'Met File is ',metfile      
      WRITE(25,*)'Output file is ', outfile 

      WRITE(25,*) 'Before Call New_RDNUCLID '     
      WRITE(25,*) 'The number of chains is ', numchain

C     Read in Nuclear Data

      DO i = 1, numchain
         
         WRITE (25,*) ' Chain ', i

         CALL New_RDNUCLID( nio )

c         WRITE(25,*) 'Before Call NUCDATIN '
c         WRITE(25,*)'Run Spec. File is ', rsfile      
c         WRITE(25,*) 'nuclide file is ', nuclfile
c         WRITE(25,*)'Nuc. Data File is ', nucdatfile       
c         WRITE(25,*)'Met File is ',metfile      
c         WRITE(25,*)'Output file is ', outfile 

         CALL NUCDATIN( PrgStat )
         
         WRITE(25,*) 'AcuteSrc....Computing for chain ', i
         
         IF( MetInp.MissMet ) THEN
            
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
                                                                     
         CALL PLUMEOUT( numchain, firstchain, modelname, acute, 
     &                  MetInp.year, MetInp.month, MetInp.day, 
     &                  MetInp.hour, NoData )
         
      ENDDO ! Chains

 9999 CONTINUE         

      CLOSE( io )

      WRITE(25,'(1x,a)')'Normal end for ACUTESRC Program'
            
      END      