C     Copyright 2001 by Battelle Memorial Institute. All rights reserved.
c                    
C     AFFREAD.FOR          Version Date:  March 20, 2001
C  
        SUBROUTINE AFFREAD(IOL,SOUFNM,RSG,RDS,GASRES,GASSCV)
c
C======================================================================
C
C       ATMOSPHERIC PATHWAY COMPONENT - reads AFF file
C       ------------------------------------
C       Written by J.G.Droppo
c
C      April 5, 2001 JGD lover limit to particle radius added
c
      IMPLICIT NONE
      
      INCLUDE  'parm.inc'
      INCLUDE  'apath.inc'
      
      INTEGER  NARGS, IOL, iflux
      INTEGER  naff, numhead, i, numflux, npol,
     &         numparent, irel, souleng, iprog,
     &         numrel, numprog, np, nsou,
     &         ier, next_pos, numarg, parnum
      INTEGER
     & fluxclass [ALLOCATABLE](:),
     & ifluxs [ALLOCATABLE](:)
      REAL RDS(7),RSG(7)
      REAL*4   flux [ALLOCATABLE](:),
     & emission [ALLOCATABLE](:),
     & fdata [ALLOCATABLE](:),
     & partrad [ALLOCATABLE](:),
     & GASRES,GASSCV, reltim
           
      CHARACTER*12 fluxnam [ALLOCATABLE](:)
      CHARACTER SouFnm*8,
     &          dum*1, ParNam*20,
     &          ParID*12,funit*4, unit*8, PrgNam*20,  
     &          PrgID*12, teststr*100, glypnm*33, airglyph*33
      CHARACTER*8
     & uemiss [ALLOCATABLE](:), ufdata [ALLOCATABLE](:),
     & upartrad [ALLOCATABLE](:),ugasres,ugasscv


c     Set Unit Numbers for Files

      NAFF= 5

C     Get Name of Air Glyph from Command Line
      numarg = NARGS()
      IF ( numarg .LE. 1 ) THEN
         WRITE(*,*) 'Error - No Air Glyph Name on CL!'
         STOP 1
      ENDIF
      CALL GETARG(1,airglyph,ier)
      IF( ier .LE. 0 ) THEN
         WRITE(*,*) 'Error reading Air Glyph Name from Command Line!'
         STOP 1
      ENDIF

c     Get Source Glyph Name from Source File
      NSOU = 17
      souleng = LEN_TRIM(soufnm)
      OPEN (UNIT=NSOU, FILE=SOUFNM(1:souleng)//'.SOU', STATUS='OLD')
      READ (NSOU,40, IOSTAT = ier) NPOL
 40   FORMAT (2x,I2) 
      IF( ier .NE. 0 ) THEN
            WRITE(*,*) 'ERROR reading Source File ', 
     &                  soufnm(1:souleng)//'.SOU'
            WRITE(*,*) 'Error Number - ', ier
            STOP 1
      ENDIF           
c     Skip the pollutant information
      DO i = 1, NPOL
            READ(NSOU,'(a1)')dum
      ENDDO
c     Read in Glyph Name
      READ( NSOU, '(a)' )glypnm         
      CLOSE(NSOU)
          
      write(IOL,*) ' '
      write(IOL,*) 'Information From AFF File'
      write(IOL,*) ' airglyph = ',airglyph
        
C.....OPEN AND READ INPUT FILES

         souleng = LEN_TRIM(soufnm)
         OPEN (UNIT=NAFF, FILE=SOUFNM(1:souleng)//'.AFF', STATUS='OLD')         glypnm=airglyph
        
         CALL FINDGLYP(NAFF,glypnm,ier)
         
         IF( ier .NE. 0 ) THEN
            WRITE(*,*) 'Unable to find Glyph Name ',glypnm
            WRITE(*,*) 'In AFF file ', soufnm(1:souleng)//'.AFF'
            STOP 1
         ENDIF
c         write (*,*) ' Glypnm -',glypnm
        
c        Read past header info

         READ( NAFF, * )numhead
         DO i = 1, numhead
            READ( NAFF, '(a1)' )dum
         ENDDO
         
C        Read past number of flux sets and flux names
         READ( NAFF, '(a1)') dum
         READ( NAFF, '(a1)') dum

c        Read past Source Info
         
         DO i = 1, 7
            READ( NAFF, '(a1)' )dum
         ENDDO

c        Read in Number of fluxes

         READ( NAFF, *) numflux
         write(IOL,*) ' number of flux classes = ',numflux
         IF(NUMFLUX.GT.4) THEN
           WRITE(*,*) 'ERROR = max # of MEPAS flux classes exceeded'
         ENDIF
         ALLOCATE( flux(numflux), fluxclass(numflux), fluxnam(numflux), 
     &          ifluxs(numflux),  
     &          emission(numflux), fdata(numflux),
     &          uemiss(numflux), ufdata(numflux), 
     &          partrad(numflux), upartrad(numflux),
     &          STAT = ier )
         
         IF ( ier .NE. 0 ) THEN
            WRITE(*,*) 'Error trying to ALLOCATE flux arrays'
            WRITE(*,*) 'Error number = ', ier
            STOP 1
         ENDIF

c        Determine which flux goes with each type (gas or particles)
         
         DO i = 1, numflux
             ifluxs(i)=0
         ENDDO
         PARNUM=0

         DO i = 1, numflux
            READ( NAFF, '(a)') teststr
            CALL READSTR(teststr,1,fluxnam(i),next_pos)
            write (IOL,*) ' fluxname = ', fluxnam(i)
            IF( INDEX(fluxnam(i),'Gas') .NE. 0 ) THEN
               fluxclass(i) = 0
               CALL READREAL(teststr,next_pos,gasres,next_pos)
               CALL READSTR(teststr,next_pos,ugasres,next_pos)
               CALL READREAL(teststr,next_pos,gasscv,next_pos)
               CALL READSTR(teststr,next_pos,ugasscv,next_pos)
c              zero out and comment out print of gasres and gasscv
c              20Mar2001 JGD
               gasres=0.0
               gasscv=0.0
c                If(gasres.lt.0.0) gasres=0.0
c               write(IOL,*)
c     &          ' gas resistance = ', gasres,' ',ugasres
c               write(IOL,*)
c
c     &          ' gas scav coeff. = ',gasscv,' ',ugasscv
            ELSE
               IF( fluxnam(i)(1:8) .NE. 'Particle' ) THEN
                  WRITE(*,*) 'ERROR reading AFF file'
                  WRITE(*,*) 'Flux Type is not defined correctly'
                  STOP 1
               ENDIF
               PARNUM=PARNUM+1
               CALL READREAL(teststr,next_pos,partrad(i),next_pos)
               CALL READSTR(teststr,next_pos,upartrad(i),next_pos)
               CALL READREAL(teststr,next_pos,fdata(i),next_pos)
               CALL READSTR(teststr,next_pos,ufdata(i),next_pos)
               write (IOL,*)' partrad  = ', partrad(i),' ',upartrad(i)
               write (IOL,*)' fdata    = ', fdata(i),' ', ufdata(i)
               if(PARNUM.lt.4) then
                 rsg(PARNUM) = fdata(i)
                 rds(PARNUM) = partrad(i)
                 if(rsg(PARNUM).lt.01) then
                   wrITE(*,*) 'ERROR in AFF file'
                   WRITE(*,*) 'Particle radius must be at least 0.01 u'
                   STOP 1
                 endif
               else
                 WRITE(*,*) 'ERROR reading AFF file'
                 WRITE(*,*) 'MEPAS has max of 3 Particle Flux Types'
                 STOP 1
               endif
            ENDIF
         ENDDO           
  
c        Read in the number of parent consitutents from AFF file

         READ( NAFF, * ) numparent
         write(IOL,*)' number of parents=',numparent

c        Loop over parents
         DO np = 1, numparent 
            READ (NAFF,'(a)') teststr
c            WRITE(IOL,'(a)')teststr
            CALL READSTR(teststr,1,ParNam,next_pos)
            CALL READSTR(teststr,next_pos,ParID,next_pos)
            CALL READSTR(teststr,next_pos,funit,next_pos)
            CALL READSTR(teststr,next_pos,unit,next_pos)
            CALL READINT4(teststr,next_pos,numrel,next_pos)
            CALL READINT4(teststr,next_pos,numprog,next_pos)
       
            write(IOL,*) parnam,parid

c           Loop over the fluxes
            DO irel = 1, numrel
c           Read time and fluxes from AFF file

               READ( NAFF, * )reltim, (flux(i), i = 1, numflux)
               DO iflux = 1, numflux
                  if (flux(iflux).gt.0.0) then
                     ifluxs(iflux)=1
                  endif
               ENDDO !Number of Fluxes
            ENDDO ! Number of Releases
            
c           Loop over each Progeny
            write(IOL,*)' number of progeny=',numprog
    
            DO iprog = 1, numprog

c           Read Constituent Line from AFF file

               READ (NAFF,'(a)') teststr
               CALL READSTR(teststr,1,PrgNam,next_pos)
               CALL READSTR(teststr,next_pos,PrgID,next_pos)
               CALL READSTR(teststr,next_pos,funit,next_pos)
               CALL READSTR(teststr,next_pos,unit,next_pos)
               CALL READINT4(teststr,next_pos,numrel,next_pos)
               CALL READSTR(teststr,next_pos,ParNam,next_pos)
               CALL READSTR(teststr,next_pos,ParID,next_pos)

               write(IOL,*) prgnam,prgid


c               READ (NAFF,*) PrgNam, PrgID, dum, unit, numrel, ParNam, 
c     &                       ParID

c              Loop over the fluxes
         
               DO irel = 1, numrel

c                 Read time and fluxes from AFF file

                  READ( NAFF, * )reltim, (flux(i), i = 1, numflux)

               
                  DO iflux = 1, numflux
                  if (flux(iflux).gt.0.0) then
                     ifluxs(iflux)=1
                  endif
                  ENDDO !Number of Fluxes
               ENDDO ! Number of Releases
            ENDDO ! Progeny  
         ENDDO  ! Parents
         write(IOL,*)' '
      CLOSE( NAFF )
      end
