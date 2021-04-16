      SUBROUTINE GetFName ( io, acute, PrgStat )

c-----------------------------------------------------------------------
c
c     GetFName
c
c     Date:             March 14, 2000
c

c     Description:      This subroutine reads in the filenames
c                       for various input and output files
c
c     Required Modules: None
c
c-----------------------------------------------------------------------

      IMPLICIT          NONE

      INCLUDE           'files.inc'
      
      INTEGER           ier, io

      LOGICAL           acute
      
      CHARACTER*2       prgid
      CHARACTER*50      PrgStat

      prgid = 'CR'
      IF ( acute ) prgid = 'AC'   

c      WRITE(25,*)'Subroutine GetFName'

      READ ( io,'(a80)',IOSTAT = ier ) rsfile   ! Run specification file
      IF( ier.NE.0 ) THEN
         WRITE ( PrgStat(1:2),'(a2)'  )    prgid
         WRITE ( PrgStat(4:13),'(a10)' )  'GETFNAME'
         WRITE ( PrgStat(15:18),'(i4)' )   ier
         WRITE ( PrgStat(20:29),'(a10)' ) 'listfile'
         WRITE ( PrgStat(31:50),'(a20)' ) 'rsfile'
         RETURN
      ENDIF
c      WRITE ( 25,* ) 'Run Specification File is ', rsfile
      
      READ( io, '(a80)', IOSTAT = ier ) nuclfile !Nuclide File
      IF ( ier .NE. 0 ) THEN
         WRITE( PrgStat(1:2), '(a2)' )     prgid
         WRITE( PrgStat(4:13), '(a10)' )  'GETFNAME'
         WRITE( PrgStat(15:18), '(i4)' )   ier
         WRITE( PrgStat(20:29), '(a10)' ) 'listfile'
         WRITE( PrgStat(31:50), '(a20)' ) 'nuclfile'
         RETURN
      ENDIF
c      WRITE(25,*) 'Nuclide File is ', nuclfile
      
      READ ( io,'(a80)',IOSTAT = ier ) domfile    ! Domain file
      IF( ier.NE.0 ) THEN
         WRITE ( PrgStat(1:2),'(a2)'  )    prgid
         WRITE ( PrgStat(4:13),'(a10)' )  'GETFNAME'
         WRITE ( PrgStat(15:18),'(i4)' )   ier
         WRITE ( PrgStat(20:29),'(a10)' ) 'listfile'
         WRITE ( PrgStat(31:50),'(a20)' ) 'domfile'
         RETURN
      ENDIF     
c      WRITE ( 25,* )'Domain File is ', domfile

      READ ( io,'(a80)',IOSTAT = ier ) nucdatfile  ! Radionuclide data file
      IF( ier.NE.0 ) THEN
         WRITE ( PrgStat(1:2),'(a2)'  )    prgid
         WRITE ( PrgStat(4:13),'(a10)' )  'GETFNAME'
         WRITE ( PrgStat(15:18),'(i4)' )   ier
         WRITE ( PrgStat(20:29),'(a10)' ) 'listfile'
         WRITE ( PrgStat(31:50),'(a20)' ) 'nucdatfile'
         RETURN
      ENDIF
c      WRITE ( 25,* ) 'Radionuclide Data File is ', nucdatfile
                                                    
      READ ( io,'(a80)',IOSTAT = ier ) obsfile  ! Observed met. data file
      IF( ier.NE.0 ) THEN
         WRITE ( PrgStat(1:2),'(a2)'  )    prgid
         WRITE ( PrgStat(4:13),'(a10)' )  'GETFNAME'
         WRITE ( PrgStat(15:18),'(i4)' )   ier
         WRITE ( PrgStat(20:29),'(a10)' ) 'listfile'
         WRITE ( PrgStat(31:50),'(a20)' ) 'obsfile'
         RETURN
      ENDIF
c      WRITE ( 25,* ) 'Observed Met. Data File is ', obsfile
                                                                                          
      READ ( io,'(a80)',IOSTAT = ier ) outfile      ! Output File
      IF( ier.NE.0 ) THEN
         WRITE ( PrgStat(1:2),'(a2)'  )    prgid
         WRITE ( PrgStat(4:13),'(a10)' )  'GETFNAME'
         WRITE ( PrgStat(15:18),'(i4)' )   ier
         WRITE ( PrgStat(20:29),'(a10)' ) 'listfile'
         WRITE ( PrgStat(31:50),'(a20)' ) 'outfile'
         RETURN
      ENDIF
c      WRITE (25,* ) 'Output File is ', outfile
      
c      WRITE ( 25,* ) 'Finished with GETFNAME'
      
      CLOSE ( io )
      
      RETURN
      
      END             