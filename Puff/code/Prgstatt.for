      SUBROUTINE PRGSTATT( PrgStat )
c-----------------------------------------------------------------------
c     PRGSTATT
c
c     Date:             March 14, 2000
c
c     Description:      This program write to a file the error that has 
c                       occured during the execution of the program
c
c     Required Modules: NONE
c
c-----------------------------------------------------------------------
      IMPLICIT       NONE 
      
      INCLUDE        'files.inc'

      INTEGER        errnum
      
      CHARACTER*2    pgname
      CHARACTER*8    progname
      CHARACTER*10   FilName, subname
      CHARACTER*20   ComStrg
      CHARACTER*50   PrgStat
      CHARACTER*80   sstring

      LOGICAL        prev_error

      DATA           prev_error / .FALSE. /



      IF ( .NOT. prev_error ) THEN

         prev_error = .TRUE.
         WRITE (25,'(a)')  'Opening Error file'
    
c  Open File which will contain error

         OPEN ( 9, FILE = 'PROGSTAT.ERR', STATUS = 'UNKNOWN' )
      
c  Write out name of program producing problem

         READ( PrgStat(1:2),'(a2)' ) pgname
      
         IF ( pgname.EQ.'CR' ) THEN
            progname = 'PUFCHRON'
         ELSEIF( pgname .EQ. 'AC' ) THEN
            progname = 'PUFACUTE'   
         ELSE 
           WRITE( 9, * ) 'Unable to tell which program produced error'
         ENDIF
    
         WRITE( 9, * ) 'The Error Occured running ',progname

      ENDIF      

c  Get the name of the subroutine where the error occured

      READ( PrgStat(4:13), '(a10)' ) subname
      
      WRITE( 9, * ) 'Error in Subroutine ',subname
      
c  Get the error number

      READ( PrgStat(15:18), '(i4)' ) errnum
      
      IF ( errnum .NE. 9999 ) THEN
         READ( PrgStat(20:29), '(a10)' ) FilName

c  Get Name of file missing

         IF ( INDEX(FilName,'listfile').NE.0 ) THEN
            sstring = listfile
         ELSEIF ( INDEX(FilName,'domfile').NE.0 ) THEN
            sstring = domfile
         ELSEIF ( INDEX(FilName,'rsfile').NE.0 ) THEN
            sstring = rsfile
         ELSEIF ( INDEX(FilName,'nuclfile').NE.0 ) THEN
            sstring = nuclfile
         ELSEIF ( INDEX(FilName,'obsfile').NE.0 ) THEN
            sstring = obsfile
         ELSEIF ( INDEX(FilName,'outfile').NE.0 ) THEN
            sstring = outfile
         ELSE
            sstring = 'unknown'//FilName
         ENDIF
 
         WRITE( 9, * )'Error in reading file ',sstring
         READ( PrgStat(31:50), '(a20)' ) ComStrg
         
         IF ( errnum .GT. 0 ) THEN
            WRITE( 9, * ) 'Error Number is ',errnum         
            IF ( ComStrg.NE.' ' ) THEN
               WRITE( 9, * ) 'Error occured reading ', ComStrg
            ENDIF
         ELSEIF ( errnum .LT. 0 ) THEN
            WRITE( 9, * ) 'End of file reached while reading: '
            WRITE( 9, * ) ComStrg
         ELSE
            WRITE( 9, * ) 'Unknown Error'
         ENDIF
      ELSE
         READ( PrgStat(31:50), '(a20)' ) ComStrg
         WRITE( 9, * ) 'Error in ', ComStrg
      ENDIF
      
      RETURN
      END            