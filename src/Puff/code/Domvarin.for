      SUBROUTINE DOMVARIN ( metio, acute, amhgt, PrgStat )
      
c--------------------------------------------------------------------------
c     DOMVARIN
c
c     Date:              March 14, 2000
c
c     Description:       Read in the Domain Variables
c
c     Required Modules:  NONE
c
c--------------------------------------------------------------------------
      IMPLICIT       NONE

      INCLUDE        'parm.inc'
      INCLUDE        'const.inc'
      INCLUDE        'files.inc'
      INCLUDE        'met_data.inc'
      
      REAL*4         amhgt

      INTEGER        i, ier, j, metio

      CHARACTER*2    prgid       
      CHARACTER*50   PrgStat
      CHARACTER*72   fmt

      LOGICAL        exists, acute

c      WRITE ( 25, '(a)' ) 'Subroutine DomVarIn'

      prgid = 'CR'
      IF ( acute ) prgid = 'AC' 

c  Read in the surface roughness at the plant from Met File

      metio = 43
      OPEN( metio, File = obsfile, STATUS = 'OLD', FORM = 'FORMATTED' )
            
      READ( 43, * ) z0sta, amhgt
       
c      WRITE( 25, * ) 'Surface Roughness for met is ', z0sta
c      WRITE( 25, * ) 'Height of Wind Measurement is ', amhgt

c     See if Gz0 file exist
      
      INQUIRE( FILE = domfile, EXIST = exists )
      
      IF( exists ) THEN
      
         OPEN ( 15, FILE=domfile, STATUS='OLD', IOSTAT=ier )

         IF ( ier .NE. 0 ) THEN
            WRITE ( PrgStat(1:2),'(a2)'  )     prgid
            WRITE ( PrgStat(4:13),'(a10)' )   'DOMVARIN'
            WRITE ( PrgStat(15:18),'(i4)' )    ier
            WRITE ( PrgStat(20:29),'(a10)' )  'domfile'
            WRITE ( PrgStat(31:50), '(a20)' ) 'Opening domfile'
         ENDIF                                  
      
         WRITE ( fmt,114 ) numx
      
         DO j = numy, 1, -1
            READ ( 15, fmt, IOSTAT = ier ) (gz0(i,j), i = 1, numx)
         
            IF ( ier.NE.0 ) THEN
               WRITE( PrgStat(1:2), '(a2)' )     prgid
               WRITE( PrgStat(4:13), '(a10)' )  'DOMVARIN'
               WRITE( PrgStat(15:18), '(i4)' )   ier
               WRITE( PrgStat(20:29), '(a10)' ) 'domfile'
               WRITE( PrgStat(31:50), '(a20)' ) 'Variable gz0'
            ENDIF

         ENDDO
      
         CLOSE(15)

  114 FORMAT ( '(',i3,'f10.0)' )

d     WRITE ( 25,'(/a/)' ) 'Surface roughness length for each node'
d     DO j = numx,1,-1
d        WRITE ( 25,40 ) j, (GZ0(j,i),i=1,numy)
d  40    FORMAT ( 1x,i2,2x,51f5.2 )
d     ENDDO
d     WRITE ( 25,45 )  (i,i=1,numx)
d  45 FORMAT ( 4x,51(3x,i2) )
      
      ELSE

c     Set the surface roughness to that of the station
      
         DO i = 1, numx
            Do j = 1, numy
            
               Gz0(i,j) = z0sta
            
            ENDDO
         ENDDO
      ENDIF      

      RETURN
      END