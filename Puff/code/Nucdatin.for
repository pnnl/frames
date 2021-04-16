      SUBROUTINE  NUCDATIN ( acute, PrgStat )
     
C-----------------------------------------------------------------------
c
c     NUCDATIN
c
c     Date:              March 19, 1998
c     Updated:           March 14, 2000
c
c     Description:       Read in the radionuclide data and set up indices
c                        associated with radionuclide order, deposition 
c                        array order, relate daughters to parents  
c
c     Required Modules:  None
c
C-----------------------------------------------------------------------

      IMPLICIT       NONE
      
      INCLUDE        'parm.inc' 
      INCLUDE        'files.inc'
      INCLUDE        'nuc_data.inc' 
      INCLUDE        'shine.inc'
                                                        
      INTEGER        tm_num, i, ii, ier, indx1, indx2, j, nuc

      CHARACTER*2    prgid                                                           
      CHARACTER*8    Nuc_Name
      CHARACTER*50   PrgStat, dataline2
      CHARACTER*120  dataline3   
      CHARACTER*300  dataline, dataline1
      
      LOGICAL        COMPSTR, acute

      prgid = 'CR'
      IF ( acute ) prgid = 'AC'

c      WRITE ( 25, '(a)' ) 'Subroutine NUCDATIN '
c      WRITE ( 25, * ) 'Nuc data file = ', nucdatfile

      dataline3 = ' '

c     Open Datafile containing cloud shine information
                             
      OPEN ( 19, FILE=nucdatfile, STATUS='old', IOSTAT=ier )
      
c     Read in Cloud Shine Distances
                                         
      READ ( 19,*,IOSTAT=ier ) (rdx(i),i=1,20)

      IF( ier.NE.0 ) THEN
         WRITE ( PrgStat(1:2),'(a2)'  )    prgid
         WRITE ( PrgStat(4:13),'(a10)' )  'NUCDATIN'
         WRITE ( PrgStat(15:18),'(i4)' )   ier
         WRITE ( PrgStat(20:29),'(a10)' ) 'nucdatfile'
         WRITE ( PrgStat(31:50),'(a20)' ) 'variable RDX'
      ENDIF

c      WRITE ( 25,'(a)' )  'The distances for normalized shine doses :'
c      WRITE ( 25,'(20(f7.2,1x))' ) (RDX(i), i = 1, 20) 

c     Calculate the log of the distances

      DO i = 1, 20
         IF (rdx(i) .GT. 0.0 ) rdxl(i) = LOG( rdx(i) )
      ENDDO
      
      tm_num = nnucs
      
      DO WHILE( tm_num .NE. 0 ) 
         
         READ(19, '(a)', IOSTAT=ier, END=9000) dataline
      
         IF( ier .GT. 0 ) THEN
            WRITE( PrgStat(1:2),'(a2)')     prgid
            WRITE( PrgStat(4:13),'(a10)')  'NUCDATIN'
            WRITE( PrgStat(15:18),'(i4)')   ier
            WRITE( PrgStat(20:29),'(a10)') 'nucdatfile'
            WRITE( PrgStat(31:50),'(a20)') 'Nuclide Name line'
         ENDIF             

         READ(19, '(a)', IOSTAT = ier, END = 9000) dataline1
      
         IF( ier .GT. 0 ) THEN
            WRITE( PrgStat(1:2),'(a2)')     prgid
            WRITE( PrgStat(4:13),'(a10)')  'NUCDATIN'
            WRITE( PrgStat(15:18),'(i4)')   ier
            WRITE( PrgStat(20:29),'(a10)') 'nucdatfile'
            WRITE( PrgStat(31:50),'(a20)') 'Line dose rate line'
         ENDIF             

         READ(19, '(a)', IOSTAT = ier, END = 9000) dataline2
      
         IF( ier .GT. 0 ) THEN
            WRITE( PrgStat(1:2),'(a2)')     prgid
            WRITE( PrgStat(4:13),'(a10)')  'NUCDATIN'
            WRITE( PrgStat(15:18),'(i4)')   ier
            WRITE( PrgStat(20:29),'(a10)') 'nucdatfile'
            WRITE( PrgStat(31:50),'(a20)') 'Line 3'
         ENDIF             
         
         READ(dataline(1:8),'(a8)') Nuc_Name
         
         DO i = 1, nnucs

c           Determine if Nuclide

            IF( CompStr(Nuc_Name, NucName(i)) ) THEN 

               nuc = i

c              Read normalized point source dose rates               
               indx1 = 14 
               DO j = 1, 20
                  indx2 = indx1 + 10
                  READ(dataline(indx1:indx2),'(f11.0)') rdy(j,nuc,1)
                  indx1 = indx2 + 1
               ENDDO                                              

c              Read in Doses from Line Sources               
               indx1 = 14
               DO j = 1, 20
                  indx2 = indx1 + 10
                  READ(dataline1(indx1:indx2),'(f11.0)') rdy(j,nuc,2)
                  indx1 = indx2 + 1
               ENDDO                          

c              Read in Ground Shine and Semi-Infinite Dose Rate
               READ( dataline2(14:24),'(f11.0)') gsdf(nuc)
               READ( dataline2(25:35),'(f11.0)') DrSinf(nuc)
               
c  determine distance to minimum dose of interest              
              
               ndx(nuc,1) = 0
               IF ( rdy(1,nuc,1) .GT. 0.0 ) THEN
                  ii = 1
                  DO WHILE ( rdy(ii,nuc,1) .GE. 1.0e-20 )
                     ndx(nuc,1) = ii
                     IF ( ii .EQ. 20 ) EXIT
                     ii =  ii + 1
                  ENDDO
                  ndxt(1) = MAX0( ndxt(1),ndx(nuc,1) )
               ENDIF 

               ndx(nuc,2) = 0
               IF ( rdy(1,nuc,2) .GT. 0.0 ) THEN
                  ii = 1
                  DO WHILE ( rdy(ii,nuc,2) .GE. 1.0e-20 )
                     ndx(nuc,2) = ii
                     IF ( ii .EQ. 20 ) EXIT
                     ii =  ii + 1
                  ENDDO
                  ndxt(2) = MAX0( ndxt(2),ndx(nuc,2) )
               ENDIF 

c              Reduce the Number of Nuclides to Find               
               tm_num = tm_num - 1
               EXIT
            ENDIF
         ENDDO
      ENDDO
      
      CLOSE(19)
      
c      WRITE (25,*) 'Cloud Shine Data:'
c      WRITE(25,'(2a9)') 'Name','Dep. Typ'
c      DO i = 1, nnucs
c         WRITE(25,*) NucName(i), ndx(i,1), ndx(i,2)
c         WRITE(25,*)(rdy(j,i,1), j = 1, ndx(i,1))
c         WRITE(25,*)(rdy(j,i,2), j= 1, ndx(i,2))
c      ENDDO
           
      RETURN
                  
 9000 CONTINUE     
      WRITE( PrgStat(1:2), '(a2)' )     prgid
      WRITE( PrgStat(4:13), '(a10)')   'NUCDATIN'
      WRITE( PrgStat(15:18), '(i4)' )   ier
      WRITE( PrgStat(20:29), '(a10)' ) 'nucdatfile'
      WRITE( PrgStat(31:50), '(a20)' ) 'premature eof'

      RETURN

      END                 
                                